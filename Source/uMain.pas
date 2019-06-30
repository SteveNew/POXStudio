unit uMain;
{
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
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Styles, FMX.Objects, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Ani, System.Generics.Collections, FMX.ListBox, FMX.Layouts,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, System.ImageList, FMX.ImgList, FMX.ExtCtrls, FMX.Menus,
  SoAOS.FMX.POX.Utils;

type
  TFrameList = class(TList<Integer>);
  TResTypeFilter = set of TResTypeEnum;

  TCircle = class(FMX.Objects.TCircle)
  private
    FSelectable : Boolean;
    FSelected : Boolean;
    FAvailable : Boolean;
    procedure SetAvailable(const Value: Boolean);
    procedure SetSelected(const Value: Boolean);
  public
    property Selectable: Boolean read FSelectable;
    property Selected: Boolean read FSelected write SetSelected;
    property Available: Boolean read FAvailable write SetAvailable;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPOXObject = class
    ObjectName: string;
    ObjectResType: TResTypeEnum;
    ObjectRelativePath: string;
  end;

  TPOXObjectList = class(TObjectList<TPOXObject>)
  private
    FFilter: TResTypeFilter;
    procedure SetFilter(const Value: TResTypeFilter);
  public
    constructor Create;
    destructor Destroy; override;
    property Filter: TResTypeFilter read FFilter write SetFilter;
  end;

  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    pnlData: TPanel;
    Panel2: TPanel;
    expPOXObjects: TExpander;
    expLayers: TExpander;
    pnlImage: TPanel;
    Splitter2: TSplitter;
    pnlActions: TPanel;
    btnSetPath: TButton;
    btnSave: TButton;
    btnExport: TButton;
    btnImport: TButton;
    btnHelp: TButton;
    btnAbout: TButton;
    languageStore: TLang;
    Image1: TImage;
    layHead: TCircle;
    layChest1: TCircle;
    layChest3: TCircle;
    layLeg1: TCircle;
    layLeg2: TCircle;
    layFeet: TCircle;
    layOuter: TCircle;
    layChest2: TCircle;
    layBelt: TCircle;
    layArm: TCircle;
    layGauntlet: TCircle;
    layMisc1: TCircle;
    layMisc3: TCircle;
    layMisc2: TCircle;
    layWeapon: TCircle;
    layShield: TCircle;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    memINIData: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    imgRLE: TImage;
    sbNW: TSpeedButton;
    sbN: TSpeedButton;
    sbNE: TSpeedButton;
    sbE: TSpeedButton;
    sbStatic: TSpeedButton;
    sbW: TSpeedButton;
    sbSW: TSpeedButton;
    sbS: TSpeedButton;
    sbSE: TSpeedButton;
    rbX2: TRadioButton;
    rbX3: TRadioButton;
    rbX1: TRadioButton;
    sbPlay: TSpeedButton;
    sbPause: TSpeedButton;
    tkbFrames: TTrackBar;
    playFrames: TFloatAnimation;
    lblFrameCnt: TLabel;
    lblFilename: TLabel;
    rbX4: TRadioButton;
    imlResTypes: TImageList;
    imgRT: TImage;
    lvwPOXFiles: TListView;
    lvwPOXFilter: TListView;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnSetPathClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure tkbFramesChange(Sender: TObject);
    procedure sbPauseClick(Sender: TObject);
    procedure sbPlayClick(Sender: TObject);
    procedure directionClick(Sender: TObject);
    procedure rbX2Change(Sender: TObject);

    procedure actionRBChanged(Sender: TObject);
    procedure layerClick(Sender: TObject);
    procedure lvwPOXFilesItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure rbX1Change(Sender: TObject);
    procedure rbX3Change(Sender: TObject);
    procedure rbX4Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure expPOXObjectsExpandedChanging(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure lvwPOXFilterItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure btnAboutClick(Sender: TObject);
  private
    { Private declarations }
    ArtLibPath: String;
    actions: TStringList;
    POXList: TPOXObjectList;
    layersSL: TStringList;

    actionButtons: TObjectList<TRadioButton>;
    currentActionIdx: Integer;
    currentDirection: Byte;
    currentObjectName, currentResTypeName: string;
    currentResType: TResTypeEnum;
//    currentResTypeFilter: TResTypeFilter;
    frameMultiplier: integer;
    validLayers: string;
//    validLayers: Set of TLayers;
//    visibleLayers: Set of TLayers;
    movements: TObjectDictionary<Byte, TFrameList>;
    bmpList: TBMPList;

    procedure zoom(factor: single);
    procedure parseIni(iniData: TStrings);
    procedure updActions;
    procedure updDirections;
    procedure updateTrackBar;
    procedure updateLayers;
    procedure updatePOXLV;
    procedure ClearBMPList;
    procedure UpdateBMPListBasedonVisibleLayers;
  public
    { Public declarations }
    function LoadPOXFile(filename: string): Boolean;
  end;

const
  origPOXExpanderWidth = 220;
  LayeredRelativePath = '\Resources\Engine\LayeredImages\';

  ResTypeStr : array [TResTypeEnum] of string = ('Unknown Resource',
                                                'Static Resource',
                                                'Non-Layered Character Resource',
                                                'Layered Character Resource',
                                                'Door Resource',
                                                'Tile Resource',
                                                'Projectile Resource',
                                                'Cast Resource',
                                                '(Linked) Layer Resource',
                                                'Inventory Resource',
                                                'Resource');

var
  frmMain: TfrmMain;

implementation

uses
  System.IOUtils, System.IniFiles, System.RTTI, System.Threading, System.StrUtils, uAbout;

{$R *.fmx}

procedure TfrmMain.actionRBChanged(Sender: TObject);
begin
  playFrames.Stop;
  currentActionIdx := actions.IndexOf(TRadioButton(Sender).Text);
  updDirections;
  updateTrackBar;
end;

procedure TfrmMain.btnAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.btnExportClick(Sender: TObject);
var
  exportpath : string;
  i: Integer;
begin
  exportpath := TPath.GetPicturesPath;
  if SelectDirectory('Select a directory', exportPath, exportPath) then
  begin
    exportpath := IncludeTrailingPathDelimiter(exportpath);
    for i := 0 to bmpList.Count-1 do
      bmplist[i].SaveToFile( exportpath+currentObjectName+'_frame'+(i+1).ToString+'.bmp' );
    memINIData.Lines.SaveToFile( exportpath+currentObjectName+'.ini' );
  end;
end;

procedure TfrmMain.btnSetPathClick(Sender: TObject);
var
  oldPath: string;
  ab: Integer;
  files: TArray<string>;
  filn: string;
  POX: TPOXObject;
begin
  oldPath := ArtLibPath;
  if SelectDirectory('Select your "ArtLib" location', '', ArtLibPath) then
  begin
    // Populate FilesDictionary<string, POXRec>
    files := TDirectory.GetFiles(ArtLibPath, '*.pox', TSearchOption.soAllDirectories);
    ab := Length(ArtLibPath)+1;
    POXList.Clear;
    for filn in files do
    begin
      POX := TPOXObject.Create;
      POX.ObjectName := TPath.GetFileNameWithoutExtension(filn);
      POX.ObjectRelativePath := copy(filn, ab);
      POX.ObjectResType := ResTypeFromFile(filn);
      POXList.Add(POX);
    end;
    POXList.Filter := [ST, CC, LC, DS, TT, PR, SC, LL, II, SP];

    updatePOXLV;

    expPOXObjects.IsExpanded := True;
  end
  else
    ArtLibPath := oldPath; // SelectDirectory cancel breaks path
end;

procedure TfrmMain.ClearBMPList;
begin
  if Assigned(bmpList) then
    TBMPList(bmpList).Free;
  bmpList := TBMPList.Create;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
 itm: TListViewItem;
 res: TResTypeEnum;
begin
  TStyleManager.TrySetStyleFromResource('TRANSPARENT');

  POXList := TPOXObjectList.Create;
  actions := TStringList.Create;
  actions.Duplicates := dupIgnore;
  actions.Sorted := True;
  actionButtons := TObjectList<TRadioButton>.Create(true);
//  bmpList := TObjectList<TBitmap>.Create();
  movements := TObjectDictionary<Byte, TFrameList>.Create([doOwnsValues]);
  expLayers.IsExpanded := False;
  expPOXObjects.IsExpanded := False;

  layersSL := TStringList.Create;
  layersSL.Duplicates := dupIgnore;
  layersSL.Sorted := True;

  lvwPOXFilter.Items.Clear;
  lvwPOXFilter.BeginUpdate;
  try

    for res := Low(TResTypeEnum) to High(TResTypeEnum) do
    begin
      if res = UR then Continue;

      itm := lvwPOXFilter.Items.Add;
      itm.Text := ResTypeStr[res];
      itm.ImageIndex := ord(res);
      itm.Checked := True;
    end;

  finally
    lvwPOXFilter.EndUpdate;
  end;
  lvwPOXFilter.Visible := False;

end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  layersSL.Free;
  if assigned(bmpList) then
    bmpList.Free;
  actions.Free;
  actionButtons.Free;
  movements.Free;
  POXList.Free;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  btnSetPathClick(Sender);
end;

procedure TfrmMain.Image2Click(Sender: TObject);
begin
  lvwPOXFilter.Visible := not lvwPOXFilter.Visible;
end;

procedure TfrmMain.layerClick(Sender: TObject);
var
  layerMarker: TCircle;
  running: Boolean;
begin
  layerMarker := TCircle(Sender);
  if layerMarker.Selectable then
    layerMarker.Selected := not layerMarker.Selected;
  running := playFrames.Running;
  if running then
    playFrames.StopAtCurrent;
  UpdateBMPListBasedonVisibleLayers;
  if running then
    playFrames.Start;
end;

function TfrmMain.LoadPOXFile(filename: string): Boolean;
var
  f : TBufferedFileStream;
  str : AnsiString;
  BB : Word;
  PicCnt, Size, RLESize, i : DWORD;
  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
  lpSpr : PRLEHDR;
  L : LongWord;
  bitmap : TBitmap;
  s: TSizeF;
begin
  Result := False;
  tkbFrames.Value := 0;
  imgRLE.Bitmap := nil;
  ClearBMPList;
  currentResTypeName := '';
  currentObjectName := TPath.GetFileNameWithoutExtension(filename);
  f := TBufferedFileStream.Create(fileName, fmOpenRead);
  try
    f.Read( L , SizeOf( L ) );
    if ( L<>$41584F50 ) then // 'POXA'
    begin
      ShowMessage('Not a valid "POX (Proprietary Object eXtension)" file.');
      Exit;
    end;
    currentResType := ResTypeFromStream(f);
    f.Read( BB, SizeOf( BB ) );
    case currentResType of
      TResTypeEnum.UR: begin
        ShowMessage('Not a known resource type.');
        Exit;
      end;
      TResTypeEnum.LC: L := f.Size - f.Position;
      else
      f.Read( L, sizeof( L ) );
    end;

    SetLength( str, L );
    f.Read( str[ 1 ], L );

    memINIData.Lines.Text := str;  // IniFile Data
    parseINI(memINIData.Lines);

    f.Read( BB, SizeOf( BB ) );
    if BB = $4242 then
    begin

      f.Read( PicCnt, SizeOf( PicCnt ) );
      f.Read( RLESize, SizeOf( RLESize ) );
      Size := PicCnt * SizeOf( RLEHDR );
      GetMem( lpSpr, Size );
      f.Read( lpSpr^, Size );
      GetMem( lpRLE, RLESize );
      f.Read( lpRLE^, RLESize );

      RelocOffset := PChar( lpRLE - lpSpr.DataPtr );
      p := lpSpr;
      for i := 1 to PicCnt do
      begin
        bitmap := TBitmap.Create;
        bitmap.Width := Trunc(imgRLE.Width);
        bitmap.Height := Trunc(imgRLE.Height);
//        bitmap.PixelFormat := TPixelFormat.BGR_565; // pf16bit;  // Should be bgr565
        p.DataPtr := PChar( p.DataPtr + DWORD( RelocOffset ) );

        decodeRLE(p, RLESize, bitmap);  // was digifxConvertRLE( dfx_hnd, p );

        bmpList.Add(bitmap);
        Inc( p );
      end;
      FreeMem(lpSpr);
      FreeMem(lpRLE);
    end;

  finally
    f.Free;
  end;
  currentResTypeName := ResTypeStr[currentResType];
  lblFilename.Text := currentObjectName+' ['+currentResTypeName+']'; // ExtractFileName(fileName);
//  Image1.Stretch := true;  // to make it as large as Image1
//  Image1.Proportional := true;  // to keep width/height ratio
  imgRT.Bitmap := nil;
  s.Create(32, 32);
  imgRT.Bitmap := imlResTypes.Bitmap(s, ord(currentResType));
  Result := True;
end;

procedure TfrmMain.lvwPOXFilesItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  playFrames.Stop;
  memINIData.Lines.Clear;
  rbX1.IsChecked := True;
  tkbFrames.Value := 0;
  movements.Clear;

  LoadPOXFile(ArtLibPath+AItem.Detail);
  updActions;
  updDirections;
  updateLayers;
  UpdateBMPListBasedonVisibleLayers;
  if bmpList.Count>0 then // LC
    imgRLE.Bitmap := bmpList[0]
  else
    imgRLE.Bitmap := nil;
  btnExport.Enabled := True;
  if (currentResType = II) then
    tkbFrames.Max := bmpList.Count-1;
  if (currentResType = LL) or (currentResType = LC) then
  begin
    expLayers.Enabled := True;
  end
  else
  begin
    expLayers.IsExpanded := False;
    expLayers.Enabled := False;
  end;

  expPOXObjects.IsExpanded := False;
end;

procedure TfrmMain.lvwPOXFilterItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  res: TResTypeEnum;
begin
  res := TResTypeEnum(AItem.ImageIndex);
  if res in POXList.Filter then
    Exclude(POXList.FFilter, res)
  else
    Include(POXList.FFilter, res);
  updatePOXLV;
end;

procedure TfrmMain.parseIni(iniData: TStrings);
var
  ini: TMemIniFile;
  actionStr: string;

  procedure addFrames;
  var
    direction: TDirectionEnum;
    Frames, str: string;
    i, a: integer;
    frameList: TFrameList;
  begin
    for a := 0 to actions.Count-1 do
    begin
      for direction := Low(TDirectionEnum) to High(TDirectionEnum) do
      begin
        Frames := ini.ReadString('Action '+actions[a], TRttiEnumerationType.GetName(direction), '');
        if Frames<>'' then
        begin
          frameList := TFrameList.Create;
          for str in Frames.Split([',']) do
          begin
            if TryStrToInt(str, i) then
              frameList.Add(i);
          end;
          movements.Add(a*10+ord(direction), frameList);
        end;
      end;
    end;
  end;

begin
  ini := TMemIniFile.Create('');
  try
    ini.SetStrings(iniData);
    actionStr := ini.ReadString('HEADER', 'Actions', '');
    actions.Clear;
    actions.AddStrings(actionStr.Split([',']));

    imgRLE.Width := ini.ReadInteger('HEADER', 'ImageWidth', 0);
    imgRLE.Height := ini.ReadInteger('HEADER', 'ImageHeight', 0);
    frameMultiplier := ini.ReadInteger('HEADER','FrameMultiplier', 1);
    // triggerframes
    validLayers := ini.ReadString('HEADER', 'ValidLayers', ''); // LL
    ini.ReadSectionValues('Layers', layersSL); // LC

    addFrames;

  finally
    ini.Free;
  end;
end;

procedure TfrmMain.rbX1Change(Sender: TObject);
begin
  zoom(1.0);
end;

procedure TfrmMain.rbX2Change(Sender: TObject);
begin
  zoom(2.0);
end;

procedure TfrmMain.rbX3Change(Sender: TObject);
begin
  zoom(3.0);
end;

procedure TfrmMain.rbX4Change(Sender: TObject);
begin
  zoom(4.0);
end;

procedure TfrmMain.directionClick(Sender: TObject);
begin
  playFrames.Stop;
  currentDirection := TSpeedButton(Sender).Tag;
  updateTrackBar;
end;

procedure TfrmMain.expPOXObjectsExpandedChanging(Sender: TObject);
begin
  if expPOXObjects.IsExpanded then
  begin
    expPOXObjects.Width := origPOXExpanderWidth;
    lvwPOXFilter.Visible := False;
  end
  else
    expPOXObjects.Width := origPOXExpanderWidth + 200;
end;

procedure TfrmMain.sbPauseClick(Sender: TObject);
begin
  playFrames.StopAtCurrent;
end;

procedure TfrmMain.sbPlayClick(Sender: TObject);
begin
  playFrames.Start;
end;

procedure TfrmMain.tkbFramesChange(Sender: TObject);
var
  frame : Integer;
begin
  if currentResType = II then
    frame := Trunc(tkbFrames.Value+1)
  else
    frame := movements[(currentActionIdx*10)+currentDirection][Trunc(tkbFrames.Value)];
  imgRLE.Bitmap := bmpList[frame-1];
  lblFrameCnt.Text := 'Frame: '+frame.ToString;
end;

procedure TfrmMain.updActions;
var
  rb: TRadioButton;
  a: integer;
begin
  actionButtons.Clear;

  pnlActions.Repaint;
  for a := 0 to actions.Count-1 do
  begin
    rb := TRadioButton.Create(nil);
    rb.Parent := pnlActions;
    rb.Text := actions[a];
    rb.Position.X := 8;
    rb.Position.Y := 24 + ( 18 * a );
    rb.GroupName := 'action';
    rb.OnChange := actionRBChanged;
    actionButtons.add(rb);
  end;
end;

procedure TfrmMain.UpdateBMPListBasedonVisibleLayers;
var
  LayersPath: string;
begin
  if currentResType = LC then
  begin
    // load naked - and rest in a TObjectlist<TLayerEnum, TBmpList>
    //
    if layersSL.Values['naked']<>'' then
    begin
      ClearBMPList;
      LayersFromFile(TPath.ChangeExtension(ArtLibPath+LayeredRelativePath+layersSL.Values['naked'], 'pox'), true, bmpList);
      LayersPath := ExtractFilePath(ArtLibPath+LayeredRelativePath+layersSL.Values['naked']);
      // Add BMPLIst to ObjectList ....
      if (layersSL.Values['head']<>'') then
        LayersFromFile(TPath.ChangeExtension(ArtLibPath+LayeredRelativePath+layersSL.Values['head'], 'pox'), false, bmpList);
      if (layersSL.Values['feet']<>'') then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['feet'], 'pox'), false, bmpList);

      if (layersSL.Values['helmet']<>'') and layHead.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['helmet'], 'pox'), false, bmpList);
      if (layersSL.Values['chest1']<>'') and layChest1.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['chest1'], 'pox'), false, bmpList);
      if (layersSL.Values['chest2']<>'') and layChest2.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['chest2'], 'pox'), false, bmpList);
      if (layersSL.Values['chest3']<>'') and layChest3.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['chest3'], 'pox'), false, bmpList);
      if (layersSL.Values['leg1']<>'') and layLeg1.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['leg1'], 'pox'), false, bmpList);
      if (layersSL.Values['leg2']<>'') and layLeg2.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['leg2'], 'pox'), false, bmpList);
      if (layersSL.Values['boot']<>'') and layFeet.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['boot'], 'pox'), false, bmpList);
      if (layersSL.Values['outer']<>'') and layOuter.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['outer'], 'pox'), false, bmpList);
      if (layersSL.Values['belt']<>'') and layBelt.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['belt'], 'pox'), false, bmpList);
      if (layersSL.Values['arm']<>'') and layArm.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['arm'], 'pox'), false, bmpList);
      if (layersSL.Values['gauntlet']<>'') and layGauntlet.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['gauntlet'], 'pox'), false, bmpList);
      if (layersSL.Values['misc1']<>'') and layMisc1.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['misc1'], 'pox'), false, bmpList);
      if (layersSL.Values['misc2']<>'') and layMisc2.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['misc2'], 'pox'), false, bmpList);
      if (layersSL.Values['misc3']<>'') and layMisc3.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['misc3'], 'pox'), false, bmpList);
      if (layersSL.Values['weapon']<>'') and layWeapon.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['weapon'], 'pox'), false, bmpList);
      if (layersSL.Values['shield']<>'') and layShield.Selected then
        LayersFromFile(TPath.ChangeExtension(LayersPath+layersSL.Values['shield'], 'pox'), false, bmpList);
    end;
  end;
end;

procedure TfrmMain.updateLayers;
begin
  layHead.Available := False;
  layChest1.Available := False;
  layChest2.Available := False;
  layChest3.Available := False;
  layLeg1.Available := False;
  layLeg2.Available := False;
  layFeet.Available := False;
  layOuter.Available := False;
  layBelt.Available := False;
  layArm.Available := False;
  layGauntlet.Available := False;
  layMisc1.Available := False;
  layMisc2.Available := False;
  layMisc3.Available := False;
  layWeapon.Available := False;
  layShield.Available := False;

  if currentResType=LL then  // Show - where can be used - but no handler.
  begin
    layHead.Available := ContainsText(validLayers, 'head');
    layHead.Available := ContainsText(validLayers, 'helmet');
    layChest1.Available := ContainsText(validLayers, 'chest1');
    layChest2.Available := ContainsText(validLayers, 'chest2');
    layChest3.Available := ContainsText(validLayers, 'chest3');
    layLeg1.Available := ContainsText(validLayers, 'leg1');
    layLeg2.Available := ContainsText(validLayers, 'leg2');
    layFeet.Available := ContainsText(validLayers, 'feet');
    layFeet.Available := ContainsText(validLayers, 'boot');
    layOuter.Available := ContainsText(validLayers, 'outer');
    layBelt.Available := ContainsText(validLayers, 'belt');
    layArm.Available := ContainsText(validLayers, 'arm');
    layGauntlet.Available := ContainsText(validLayers, 'gauntlet');
    layMisc1.Available := ContainsText(validLayers, 'misc1');
    layMisc2.Available := ContainsText(validLayers, 'misc2');
    layMisc3.Available := ContainsText(validLayers, 'misc3');
    layWeapon.Available := ContainsText(validLayers, 'weapon');
    layShield.Available := ContainsText(validLayers, 'shield');
//    layTabar.Visible := ContainsText(validLayers, 'tabar');
  end;
  if currentResType=LC then  // Toggle Opacity
  begin
    // layers has a value = visible
//    layHead.Available := layersSL.Values['head']<>'';
    layHead.Selected := layersSL.Values['helmet']<>'';
    layChest1.Selected := layersSL.Values['chest1']<>'';
    layChest2.Selected := layersSL.Values['chest2']<>'';
    layChest3.Selected := layersSL.Values['chest3']<>'';
    layLeg1.Selected := layersSL.Values['leg1']<>'';
    layLeg2.Selected := layersSL.Values['leg2']<>'';
//    layFeet.Available := layersSL.Values['feet']<>'';
    layFeet.Selected := layersSL.Values['boot']<>'';
    layOuter.Selected := layersSL.Values['outer']<>'';
    layBelt.Selected := layersSL.Values['belt']<>'';
    layArm.Selected := layersSL.Values['arm']<>'';
    layGauntlet.Selected := layersSL.Values['gauntlet']<>'';
    layMisc1.Selected := layersSL.Values['misc1']<>'';
    layMisc2.Selected := layersSL.Values['misc2']<>'';
    layMisc3.Selected := layersSL.Values['misc3']<>'';
    layWeapon.Selected := layersSL.Values['weapon']<>'';
    layShield.Selected := layersSL.Values['shield']<>'';
//    layTabar.Visible := layersSL.Values['tabar']<>'';
  end;
end;

procedure TfrmMain.updatePOXLV;
var
  POX: TPOXObject;
  itm: TListViewItem;
begin
  lvwPOXFiles.Items.Clear;
  lvwPOXFiles.BeginUpdate;
  try

    for POX in POXList do
    begin
      if POX.ObjectResType in POXList.Filter then
      begin
        itm := lvwPOXFiles.Items.Add;
        itm.Text := POX.ObjectName;
        itm.Detail := POX.ObjectRelativePath;
        itm.ImageIndex := ord(POX.ObjectResType);
      end;
    end;

  finally
    lvwPOXFiles.EndUpdate;
  end;
end;

procedure TfrmMain.updateTrackBar;
var
  frames: TFrameList;
begin
  tkbFrames.Value := 0;
  if movements.TryGetValue((currentActionIdx*10)+currentDirection, frames) then
  begin
    tkbFrames.Max := frames.Count-1;
    imgRLE.Bitmap := bmpList[frames[Trunc(tkbFrames.Value)]-1];
  end
  else
  begin
    tkbFrames.Max := tkbFrames.Min;
    imgRLE.Bitmap := bmpList[0];
  end;
  playFrames.StartValue := 0;
  playFrames.StopValue := tkbFrames.Max;
  playFrames.Duration := tkbFrames.Max * 0.1 * frameMultiplier; // 100 msec per frame
  sbPlay.Enabled := tkbFrames.Max <> tkbFrames.Min;
end;

procedure TfrmMain.updDirections;
begin
  sbStatic.Enabled := movements.ContainsKey(currentActionIdx*10+0);
  sbNW.Enabled := movements.ContainsKey(currentActionIdx*10+1);
  sbN.Enabled := movements.ContainsKey(currentActionIdx*10+2);
  sbNE.Enabled := movements.ContainsKey(currentActionIdx*10+3);
  sbE.Enabled := movements.ContainsKey(currentActionIdx*10+4);
  sbSE.Enabled := movements.ContainsKey(currentActionIdx*10+5);
  sbS.Enabled := movements.ContainsKey(currentActionIdx*10+6);
  sbSW.Enabled := movements.ContainsKey(currentActionIdx*10+7);
  sbW.Enabled := movements.ContainsKey(currentActionIdx*10+8);
end;

procedure TfrmMain.zoom(factor: single);
begin
  imgRLE.Scale.X := factor;
  imgRLE.Scale.Y := factor;
end;

{ TPOXObjectList }

constructor TPOXObjectList.Create;
begin
  inherited Create(True);
//  FListViewItems := TAppearanceListViewItems.Create(nil);
end;

destructor TPOXObjectList.Destroy;
begin
//  FListViewItems.Free;
  inherited;
end;

procedure TPOXObjectList.SetFilter(const Value: TResTypeFilter);
begin
  FFilter := Value;
end;

{ TLayerCircle }

constructor TCircle.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCircle.Destroy;
begin
  inherited;
end;

procedure TCircle.SetAvailable(const Value: Boolean);
begin
  Visible := Value;
  FAvailable := Value;
  FSelectable := False;
  Fill.Kind := TBrushKind.Solid;
  Stroke.Kind := TBrushKind.None;
end;

procedure TCircle.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
  if FSelected then
    Fill.Kind := TBrushKind.Solid
  else
    Fill.Kind := TBrushKind.None;
  if Value and (not FAvailable) then
  begin
    Available := True;
    Stroke.Kind := TBrushKind.Solid;
  end;
  FSelectable := True;
end;

end.
