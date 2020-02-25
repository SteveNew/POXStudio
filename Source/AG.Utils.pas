unit AG.Utils;
{
  ...part of (but not exclusivly):

  POX Studio
  - A cross-platform POX resource editor for Siege of Avalon engine based games.
  Copyright (C) 2020  Steffen Nyeland

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

    Handling of AG files - deflating their content into .pox, .bmp or sound files
}

interface

uses
  System.Classes;

  function BufferFind(szSubStr, buf: PAnsiChar; iBufSize: integer): integer;

  // Usage:
  // POX - Deflate('POXA', '.pox', 'C:\Program Files (x86)\Infogrames\Hero X\Resources\sprite.ag', 'c:\temp\sprite\', fcnt);
  // BMP - Deflate('BM6W', '.bmp', 'C:\Program Files (x86)\Infogrames\Hero X\Resources\interface.ag', 'c:\temp\interface\', fcnt);
  // WAV? - Deflate('RIFF', '.wav', 'C:\Program Files (x86)\Infogrames\Hero X\Resources\sound.ag', 'c:\temp\sound\', fcnt);
  procedure Deflate(seperator, extention, agfile, destpath: string; out filesFound: Integer);

  function WriteToPOXFile(var fs: TFileStream; const position, size: Integer; const destpath: string): string;
  function WriteToFile(const extention: string; var fs: TFileStream; const position, size: Integer; const destpath: string): string;

implementation

uses
  System.SysUtils, System.AnsiStrings;

function BufferFind(szSubStr, buf: PAnsiChar; iBufSize: integer): integer;
{ Returns -1 if substring not found,
  or zero-based index into buffer if substring found }
var
  iSubStrLen: integer;
  skip: array [char] of integer;
  found: boolean;
  iMaxSubStrIdx: integer;
  iSubStrIdx: integer;
  iBufIdx: integer;
  iScanSubStr: integer;
  mismatch: boolean;
  iBufScanStart: integer;
  ch: char;
begin
  { Initialisations }
  found := False;
  Result := -1;
  { Check if trivial scan for empty string }
  iSubStrLen := System.AnsiStrings.StrLen(szSubStr);
  if iSubStrLen = 0 then
  begin
    Result := 0;
    Exit
  end;

  iMaxSubStrIdx := iSubStrLen - 1;
  { Initialise the skip table }
  for ch := Low(skip) to High(skip) do skip[ch] := iSubStrLen;
  for iSubStrIdx := 0 to (iMaxSubStrIdx - 1) do
    skip[szSubStr[iSubStrIdx]] := iMaxSubStrIdx - iSubStrIdx;

  { Scan the buffer, starting comparisons at the end of the substring }
  iBufScanStart := iMaxSubStrIdx;
  while (not found) and (iBufScanStart < iBufSize) do
  begin
    iBufIdx := iBufScanStart;
    iScanSubStr := iMaxSubStrIdx;
    repeat
      mismatch := (szSubStr[iScanSubStr] <> buf[iBufIdx]);
      if not mismatch then
        if iScanSubStr > 0 then
        begin // more characters to scan
          Dec(iBufIdx); Dec(iScanSubStr)
        end
      else
        found := True;
    until mismatch or found;
    if found then
      Result := iBufIdx
    else
      iBufScanStart := iBufScanStart + skip[buf[iBufScanStart]];
  end;
end;

procedure Deflate(seperator, extention, agfile, destpath: string; out filesFound: Integer);
const
  BUFSIZE = 8192;
  HeroID = $4F524548;
var
  fstm: TFileStream;
  numread: Longint;
  buffer: array [0..BUFSIZE-1] of AnsiChar;
  szFind: array [0..255] of AnsiChar;
  found: boolean;

  aPos, bPos: Integer;
  cnt2: Integer;

  fileID: Int32;
  fileSize: Int32;
begin
  ForceDirectories(destpath);
  System.AnsiStrings.StrPCopy(szFind, seperator);
  found := False;
  aPos := 8;
  bPos := 0;
  filesFound := 0;
  fstm := TFileStream.Create(agfile, fmOpenRead);
  try
    fstm.Read(fileID, SizeOf(fileID));
    if fileID=HeroID then
    begin
      fstm.Read(fileSize, SizeOf(fileSize));
      repeat
        repeat
          numread := fstm.Read(Buffer, BUFSIZE);
          cnt2 := BufferFind(szFind, Buffer, numread);
          if cnt2 >= 0 then
          begin
            bPos := (fstm.Position - numRead) + cnt2;
            if bPos=8 then
              Continue;
            found := True;
            inc(filesFound);
          end
          else if numread = BUFSIZE then // more to scan
            fstm.Position := fstm.Position - (Length(seperator)-1);
        until found or (numread < BUFSIZE);

        if found then
        begin
          if extention='.pox' then
            WriteToPOXFile(fstm, aPos, bPos-aPos, destPath)
          else
            WriteToFile(extention, fstm, aPos, bPos-aPos, destPath);
          aPos := bPos;
          found := False;
        end;
      until numread=0;

      if filesFound>0 then
      begin
        if extention='.pox' then
          WriteToPOXFile(fstm, aPos, fileSize-aPos, destPath)
        else
          WriteToFile(extention, fstm, aPos, fileSize-aPos, destPath);
      end;
    end;
  finally
    fstm.Free;
  end;
end;

function WriteToPOXFile(var fs: TFileStream; const position, size: Integer; const destpath: string): string;
var
  oPos: Integer;
  dstring: AnsiString;
  s1, s2: Integer;
  f: TMemoryStream;
begin
  oPos := fs.Position;
  fs.Position := position;
  f := TMemoryStream.create;
  try
    f.CopyFrom(fs, size);
    SetString(dstring, PAnsiChar(f.Memory), f.Size);
    s1 := Pos('FileName=', dString)+9;
    s2 := Pos('.gif', dString);
    Result := Copy(dString, s1, s2-s1 )+'_'+position.ToString+'.pox';
    f.SaveToFile(IncludeTrailingPathDelimiter(destpath) + Result);
  finally
    f.Free;
  end;
  fs.Position := oPos;
end;

function WriteToFile(const extention: string; var fs: TFileStream; const position, size: Integer; const destpath: string): string;
var
  oPos: Integer;
  f: TMemoryStream;
begin
  oPos := fs.Position;
  fs.Position := position;
  f := TMemoryStream.create;
  try
    f.CopyFrom(fs, size);
    Result := position.ToString+extention;
    f.SaveToFile(IncludeTrailingPathDelimiter(destpath) + Result);
  finally
    f.Free;
  end;
  fs.Position := oPos;
end;

end.
