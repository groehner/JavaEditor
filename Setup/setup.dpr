program setup;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  Controls,
  SysUtils,
  Dialogs,
  UMemo in 'UMemo.pas' {FMemo},
  fmxutils in 'fmxutils.pas',
  USetup in 'USetup.pas' {FSetup},
  UUpdater in 'UUpdater.pas' {FUpdater},
  UWait in 'UWait.pas' {Fwait};

{$R administrator.res}

// for debugging use Run/Parameters
// get Parameters from procedure TFConfiguration.CallUpdater variable s

begin
  Application.Initialize;
  Application.CreateForm(TFSetup, FSetup);
  Application.CreateForm(TFUpdater, FUpdater);
  Application.CreateForm(TFMemo, FMemo);
  Application.CreateForm(TFwait, Fwait);
  var Param:= paramStr(1);
  if (Param <> '-Update') and (Param <> '-Registry') then begin
    Application.ShowMainForm:= true;
    Application.Run;
  end else begin
    Application.ShowMainForm:= false;
    FWait.Show;
    FWait.Invalidate;
    Application.ProcessMessages;
    Screen.Cursor:= crHourGlass;
    FUpdater.MakeUpdate;
    FWait.Hide;
    Screen.Cursor:= crDefault;
    if FUpdater.Restart then
      if FUpdater.Error then begin
        FUpdater.Show;
        Application.Run;
      end else
        FUpdater.BOKClick(nil);
  end;
end.
