unit SoAOS.FMX.POX.Utils;
{
  ...part of (but not exclusivly):

  POX Studio
  - A cross-platform POX resource editor for Siege of Avalon engine based games.
  Copyright (C) 2019  Steffen Nyeland

  License

    This program is free software: you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License along with
    this program.  If not, see <https://www.gnu.org/licenses/>.

  Contributor(s)

    Steffen Nyeland <steffen@nyeland.dk>

  Thanks to

    Dominique Louis and Digital Tome L.P. for making Siege Of Avalon source open.
    Eustace and Rucksacksepp from the SoAmigos forum - for suggestions and ideas.

  You may retrieve the latest version of this file at:

    https://github.com/SteveNew/POXStudio

  Requires

    Delphi 10.3 or later
    A free Community Edition is available from https://www.embarcadero.com

  Description

    Handling of POX files - reading, encoding, drawing, decoding and writing under FMX framework
}

interface

uses
  System.Types, System.Classes, System.Generics.Collections, FMX.Graphics;

type
  PRLEHDR = ^RLEHDR;
  RLEHDR = record
    SrcX : int32;
    SrcY : int32;
    Wdh : DWord;
    Hgh : DWord;
    AdjX : int32;
    AdjY : int32;
    PixFmt : DWord;
    DataPtr : int32;
  end;

  TBMPList = class(TObjectList<TBitmap>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TROMemoryStream = class(TMemoryStream)
  private
    OrigPtr : Pointer;
    OrigSize : LongInt;
  public
    procedure SetMemPointer(Ptr: Pointer; newSize: Longint);
    destructor Destroy; override;
  end;

  TResTypeEnum = ( UR, ST, CC, LC, DS, TT, PR, SC, LL, II, SP );
  TDirectionEnum = ( Frames, NWFrames, NNFrames, NEFrames, EEFrames, SEFrames, SSFrames, SWFrames, WWFrames );
  TLayers = (naked,leg1,boot,feet,leg2,chest1,chest2,arm,belt,chest3,gauntlet,outer,head,helmet,weapon,shield,tabar); // Blitting order.


  function ResTypeFromFile(filename: string): TResTypeEnum;
  function ResTypeFromStream(filestream: TBufferedFileStream): TResTypeEnum;
  procedure decodeRLE(rle: PRLEHDR; rleData: TBufferedFileStream; var bitmap: TBitmap);
  procedure encodeRLE(bitmap: TBitmap; var rle: RLEHDR; var rleData: TMemoryStream);
  procedure LayersFromFile(filename: string; clear: boolean; var bmpList: TBMPList);

implementation

uses
  System.RTTI, System.SysUtils, System.UITypes, System.IniFiles;

function ResTypeFromFile(filename: string): TResTypeEnum;
var
  f : TBufferedFileStream;
  L : DWord;
begin
  f := TBufferedFileStream.Create(fileName, fmOpenRead);
  try
    f.Read( L , SizeOf( L ) );
    if ( L<>$41584F50 ) then // 'POXA'
    begin
      Result := TResTypeEnum.UR;
      Exit;
    end;
    Result := ResTypeFromStream(f);
  finally
    f.Free;
  end;
end;

function ResTypeFromStream(filestream: TBufferedFileStream): TResTypeEnum;
var
  M: array[ 1..2 ] of AnsiChar;
begin
  filestream.Position := 4;
  filestream.Read( M, SizeOf( M ) );
  Result := TRttiEnumerationType.GetValue<TResTypeEnum>(M);
end;

procedure decodeRLE(rle: PRLEHDR; rleData: TBufferedFileStream; var bitmap: TBitmap);
var
  i, x, y : integer;
  c : byte;
  colour: word;
  pxCol: TAlphaColorRec;
  bmpData: TBitmapData;
begin
  pxCol.A := $FF;
  if bitmap.Map(TMapAccess.Write, bmpData) then
  begin
    x := 0;
    y := 0;
    rleData.Read(&c, 1);
    while (c > 0) and (c < 4) do
    begin
      case c of
        1 : begin // colour/pixel data
          rleData.Read(&i, 4);
          while i > 0 do
          begin
            rleData.Read(&colour, 2);
            if rle.PixFmt=1 then
            begin
              pxCol.B := (Colour and $1F) shl 3;
              pxCol.G := ((Colour and $3E0) shr 5) shl 3;
              pxCol.R := ((Colour and $7C00) shr 10) shl 3;
            end;
            if rle.PixFmt=2 then
            begin
              pxCol.B := (Colour and $1F) shl 3;
              pxCol.G := ((Colour and $7E0) shr 5) shl 2;
              pxCol.R := ((Colour and $F800) shr 11) shl 3;
  // Alternatives - but above seems good enough and fastest
  //          r := (r * 527 + 23 ) shr 6; // floor(255/31 * R);
  //          g := (g * 259 + 33 ) shr 6; // floor(255/63 * G);
  //          b := (b * 527 + 23 ) shr 6; // floor(255/31 * B);
            end;
            bmpData.SetPixel(X+rle.AdjX, Y+rle.AdjY, pxCol.Color);
            inc(x);
            dec(i);
          end;
        end;
        2 : begin // add x offset
          rleData.Read(&i, 4);
          i := i div 2;
          inc(x, i);
        end;
        3 : inc(y); // new line, carriage return
      end;
      rleData.Read(&c, 1);
    end;
    bitmap.Unmap(bmpData);
  end;
end;

procedure encodeRLE(bitmap: TBitmap; var rle: RLEHDR; var rleData: TMemoryStream);
var
  colarray: TList<DWORD>;
  casetyp: byte;
  didColor: boolean;
  x, y, i, two: Integer;
  bmpData: TBitmapData;
  transcount, oldx, currentrowlastx, rl : DWORD;
  colour: word;
  pxCol: TAlphaColorRec;
  Wdh : Integer;
  Hgh : Integer;
  AdjX : Integer;
  AdjY : Integer;

  function ColorToBGR565(color: TAlphaColorRec): DWORD;
  var
    r, g, b: byte;
  begin
    r := color.R shr 3;
    g := color.G shr 2;
    b := color.B shr 3;
    Result := (r shl 11)+(g shl 5)+b;
  end;

begin
  Wdh := 0; // Actually less than width+adjX
  Hgh := 0; // Actually less than height+adjY
  AdjX := 9999; //bitmap.Width; // Leftmost non-transparent pixel
  AdjY := 9999; //bitmap.Height; // Topmost non-transparent pixel

  colarray := TList<DWORD>.Create;
  try
    if bitmap.Map(TMapAccess.Read, bmpData) then
    begin
      // Get data dimensions
      for y := 0 to bitmap.Height-1 do
      begin
        for x := 0 to bitmap.Width-1 do
        begin
          pxCol.Color := bmpData.GetPixel(x, y);
          if pxCol.Color <> 0 then
          begin
            if AdjX > x then AdjX := x;
            if AdjY > y then AdjY := y;
            if Wdh < x then
              Wdh := x;
            if Hgh < y then
              Hgh := y;
          end;
        end;
      end;

      rle.SrcX := AdjX; // Not used ?
      rle.SrcY := AdjY; // Not used ?
      rle.Wdh := Wdh - AdjX + 1;
      rle.Hgh := Hgh - AdjY + 1;
      rle.AdjX := AdjX;
      rle.AdjY := AdjY;
      rle.PixFmt := 2;
      rle.DataPtr := int32(rleData.Position); // int32(PChar(@rleData));

      transcount := 0;
      oldx := 0;
      didColor := False;

      for y := AdjY to bitmap.Height-1 do  // should be Hgh, since no relevant data outside.
      begin

        for x := AdjX to bitmap.Width-1 do  // should be Wdh, since no relevant data outside.
        begin
          pxCol.Color := bmpData.GetPixel(x,y);
          if pxCol.Color <> 0 then
          begin
            didColor := true;
            if abs(transcount-oldx) > 0 then
            begin
              two := (transcount-oldx);
              if two < 0 then
                two := two + AdjX - 1;
              casetyp := $02;  // Adjust x
              rl := trunc((two) shl 1);
              rleData.Write(casetyp, 1);
              rleData.Write(rl, 4);
              transcount := 0;
              oldx := 0;
            end;
            colarray.Add(ColorToBGR565(pxCol)); // BGR565
            currentrowlastx := x;
          end
          else
          begin
            if didColor then
            begin
              didColor := False;
              casetyp := $01;
              rl := colarray.Count;
              rleData.Write(casetyp, 1);
              rleData.Write(rl, 4);
              for I := 0 to rl-1 do
              begin
                colour := colarray[I];
                rleData.Write(colour, 2);
              end;
              colarray.Clear;
            end;
            inc(transcount)
          end;
        end;

        if didColor then
        begin
          didColor := False;
          casetyp := $01;
          rl := colarray.Count;
          rleData.Write(casetyp, 1);
          rleData.Write(rl, 4);
          for I := 0 to rl-1 do
          begin
            colour := colarray[I];
            rleData.Write(colour, 2);
          end;
          colarray.Clear;
        end;

        casetyp := $03;
        rleData.Write(casetyp, 1);

        oldx := currentrowlastx;
        transcount := 0;
      end;

      bitmap.Unmap(bmpData);
      casetyp := $00;      // bitmap done
      rleData.Write(casetyp, 1);
    end;

  finally
    colarray.Free;
  end;
end;

procedure LayersFromFile(filename: string; clear: boolean; var bmpList: TBMPList);
var
  f : TBufferedFileStream;
  str : AnsiString;
  BB : Word;
  PicCnt, Size, RLESize, i : DWORD;
//  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
  lpSpr : PRLEHDR;
  L : DWord;
  bitmap : TBitmap;
  resType : TResTypeEnum;
  ini : TMemIniFile;
  imgwth, imghgt: Integer;
  iniData: TStringList;
begin
  if not FileExists(filename) then
    Exit;
  f := TBufferedFileStream.Create(filename, fmOpenRead);
  try
    f.Read( L , SizeOf( L ) );
    if ( L<>$41584F50 ) then // 'POXA'
      Exit;
    resType := ResTypeFromStream(f);
    f.Read( BB, SizeOf( BB ) );
    case resType of
      TResTypeEnum.UR: Exit;
      TResTypeEnum.LC: Exit; // L := f.Size - f.Position;
    else
      f.Read( L, sizeof( L ) );
    end;
    SetLength( str, L );
    f.Read( str[ 1 ], L );
    // Get Image sizes.
    if clear then
    begin
      iniData := TStringList.Create;
      try
        iniData.Text := str;
        ini := TMemIniFile.Create('');
        try
          ini.SetStrings(iniData);
          imgwth := ini.ReadInteger('HEADER', 'ImageWidth', 0);
          imghgt := ini.ReadInteger('HEADER', 'ImageHeight', 0);
        finally
          ini.Free;
        end;
      finally
        iniData.Free;
      end;
    end;
    //
    f.Read( BB, SizeOf( BB ) );
    if BB = $4242 then
    begin
      f.Read( PicCnt, SizeOf( PicCnt ) );
      f.Read( RLESize, SizeOf( RLESize ) );
      Size := PicCnt * SizeOf( RLEHDR );
      GetMem( lpSpr, Size );
      f.Read( lpSpr^, Size );

      p := lpSpr;
      for i := 0 to PicCnt-1 do
      begin
//        p.DataPtr := p.DataPtr + DWORD( RelocOffset );
        if clear then
        begin
          bitmap := TBitmap.Create;
          bitmap.Width := Trunc(imgwth);
          bitmap.Height := Trunc(imghgt);
        end
        else
          bitmap := bmpList[i];
//        bitmap.PixelFormat := TPixelFormat.BGR_565; // pf16bit;  // Should be bgr565
        decodeRLE(p, f, bitmap);  // was digifxConvertRLE( dfx_hnd, p );
        if clear then bmpList.Add(bitmap);
        Inc( p );
      end;
      FreeMem(lpSpr);
    end;
  finally
    f.Free;
  end;
end;

{ TROMemoryStream }

destructor TROMemoryStream.Destroy;
begin
  SetPointer ( OrigPtr, OrigSize );
  inherited;
end;

procedure TROMemoryStream.SetMemPointer(Ptr: Pointer; newSize: Longint);
begin
  OrigPtr := Memory;
  OrigSize := Self.Size;
  SetPointer ( ptr, newSize );
end;

{ TBMPList }

constructor TBMPList.Create;
begin
  inherited Create(True);
end;

destructor TBMPList.Destroy;
var
  i: Integer;
begin
  for i := Self.Count-1 downto 0 do
  begin
    TBitmap(Self[i]).Clear(0);
    TBitMap(Self[i]).FreeHandle;
  end;
  inherited;
end;

end.
