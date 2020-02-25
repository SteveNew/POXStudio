program POXStudio;



{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  System.Sysutils,
  SoAOS.FMX.POX.Utils in 'SoAOS.FMX.POX.Utils.pas',
  uAbout in 'uAbout.pas' {frmAbout},
  uMain in 'uMain.pas' {frmMain},
  uSplash in 'uSplash.pas' {frmSplash},
  AG.Utils in 'AG.Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmSplash, frmSplash);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;
end.
