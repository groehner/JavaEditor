unit UMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FMXUtils;

type
  TFMemo = class(TForm)
    MInstallation: TMemo;
    BClose: TButton;
    procedure BCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Installation: Integer;
    procedure Output(s: String);
  end;

var
  FMemo: TFMemo;

implementation

uses USetup;

{$R *.DFM}

procedure TFMemo.Output(s: String);
begin
  MInstallation.Lines.Add(s);
end;

procedure TFMemo.BCloseClick(Sender: TObject);
begin
  case Installation of
    0: ExecuteFile(FSetup.ToDir + 'JavaEditor.exe', '', '', SW_ShowNormal);
    1: ExecuteFile(GetTempDir + 'RunJava.bat', '', '', SW_Hide);
  end;
  FSetup.Close;
end;

procedure TFMemo.FormCreate(Sender: TObject);
begin
  Installation:= 2                                       
end;

end.
