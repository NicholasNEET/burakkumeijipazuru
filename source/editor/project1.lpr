program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainwindow, aboutform, levelinfoform, teleportform, burakku
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMainWindow, frmMainWindow);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmLevelInfo, frmLevelInfo);
  Application.CreateForm(TfrmTeleport, frmTeleport);
  Application.Run;
end.

