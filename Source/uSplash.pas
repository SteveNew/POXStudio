unit uSplash;
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

type
  TfrmSplash = class(TForm)
    Image1: TImage;
    StartupTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Image1Paint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure StartupTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FInitialized: Boolean;
    procedure LoadMainForm;
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

uses
  uMain;

{$R *.fmx}

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
  StartupTimer.Enabled := false;
  StartupTimer.Interval := 2000;
end;

procedure TfrmSplash.Image1Paint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  StartupTimer.Enabled := not FInitialized;
end;

procedure TfrmSplash.LoadMainForm;
var
  form: TForm;
begin
  form := TfrmMain.Create(Application);
  form.Show;
  Application.MainForm := form;
  Close;
end;

procedure TfrmSplash.StartupTimerTimer(Sender: TObject);
begin
  StartupTimer.Enabled := false;
  if not FInitialized then begin
    FInitialized := true;
    LoadMainForm;
  end;
end;

end.
