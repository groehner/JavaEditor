unit UDlgMindstormsEV3;

interface

uses
  Forms,
  StdCtrls,
  System.Classes,
  Vcl.Controls;

type
  TFMindstormsEV3Dialog = class(TForm)
    BRun: TButton;
    BClose: TButton;
    BControlCenter: TButton;
    BTerminate: TButton;
    BShutDown: TButton;
    BUpload: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BRunClick(Sender: TObject);
    procedure BControlCenterClick(Sender: TObject);
    procedure BUploadClick(Sender: TObject);
    procedure BTerminateClick(Sender: TObject);
    procedure BShutDownClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  Windows,
  JvGnugettext,
  UConfiguration,
  UJava,
  UJavaCommands;

procedure TFMindstormsEV3Dialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFMindstormsEV3Dialog.BControlCenterClick(Sender: TObject);
begin
  MyJavaCommands.ExecWithoutWait(FConfiguration.LejosVerzeichnis +
    '\bin\ev3control.bat', '', '', SW_HIDE);
end;

procedure TFMindstormsEV3Dialog.BRunClick(Sender: TObject);
begin
  FConfiguration.MindstormsRun := True;
  FJava.MIRunClick(Self);
end;

procedure TFMindstormsEV3Dialog.BShutDownClick(Sender: TObject);
begin
  MyJavaCommands.CallEV3('-s');
end;

procedure TFMindstormsEV3Dialog.BTerminateClick(Sender: TObject);
begin
  MyJavaCommands.CallEV3('-t');
end;

procedure TFMindstormsEV3Dialog.BUploadClick(Sender: TObject);
begin
  FConfiguration.MindstormsRun := False;
  FJava.MIRunClick(Self);
end;

end.
