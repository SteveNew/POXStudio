unit uAbout;
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ScrollBox, FMX.Memo;

type
  TfrmAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.fmx}

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
