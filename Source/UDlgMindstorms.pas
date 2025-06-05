unit UDlgMindstorms;

interface

uses
  Forms,
  StdCtrls,
  System.Classes,
  Vcl.Controls;

type
  TFMindstormsDialog = class(TForm)
    GBOptionen: TGroupBox;
    CBVerbose: TCheckBox;
    CBDebug: TCheckBox;
    CBRun: TCheckBox;
    BRun: TButton;
    BClose: TButton;
    BControlCenter: TButton;
    BFlash: TButton;
    BBrowser: TButton;
    BConsoleViewer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BRunClick(Sender: TObject);
    procedure BControlCenterClick(Sender: TObject);
    procedure BFlashClick(Sender: TObject);
    procedure BBrowserClick(Sender: TObject);
    procedure BConsoleViewerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
  public
    procedure Save;
  end;

implementation

{$R *.dfm}

uses
  JvGnugettext,
  UConfiguration,
  UJava,
  UJavaCommands;

procedure TFMindstormsDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  CBVerbose.Checked := FConfiguration.MindstormsVerbose;
  CBDebug.Checked := FConfiguration.MindstormsDebug;
  CBRun.Checked := FConfiguration.MindstormsRun;
end;

procedure TFMindstormsDialog.FormClose(Sender: TObject;
  var AAction: TCloseAction);
begin
  Save;
  AAction := caFree;
end;

procedure TFMindstormsDialog.BBrowserClick(Sender: TObject);
begin
  MyJavaCommands.MindstormsUtility('nxjbrowse.bat');
end;

procedure TFMindstormsDialog.BConsoleViewerClick(Sender: TObject);
begin
  MyJavaCommands.MindstormsUtility('nxjconsoleviewer.bat');
end;

procedure TFMindstormsDialog.BControlCenterClick(Sender: TObject);
begin
  MyJavaCommands.MindstormsUtility('nxjcontrol.bat');
end;

procedure TFMindstormsDialog.BFlashClick(Sender: TObject);
begin
  MyJavaCommands.MindstormsUtility('nxjflashg.bat');
end;

procedure TFMindstormsDialog.BRunClick(Sender: TObject);
begin
  Save;
  FJava.MIRunClick(Self);
end;

procedure TFMindstormsDialog.Save;
begin
  FConfiguration.MindstormsVerbose := CBVerbose.Checked;
  FConfiguration.MindstormsDebug := CBDebug.Checked;
  FConfiguration.MindstormsRun := CBRun.Checked;
  FConfiguration.WriteBoolU('Mindstorms', 'Verbose', CBVerbose.Checked);
  FConfiguration.WriteBoolU('Mindstorms', 'Debug', CBDebug.Checked);
  FConfiguration.WriteBoolU('Mindstorms', 'Run', CBRun.Checked);
end;

end.
