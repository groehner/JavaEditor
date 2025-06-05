unit UDlgMessage;

interface

uses
  Forms,
  StdCtrls,
  ExtCtrls,
  Vcl.Controls,
  Vcl.Graphics,
  System.Classes;

type
  TDlgMessage = class(TForm)
    Image1: TImage;
    LMessage: TLabel;
    Panel1: TPanel;
    BOverwrite: TButton;
    BOpen: TButton;
    BCancel: TButton;
    procedure FormCreate(Sender: TObject);
  public
    procedure SetMessage(const Filename: string);
  end;

implementation

uses
  SysUtils,
  JvGnugettext,
  UStringRessources;

{$R *.dfm}

procedure TDlgMessage.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  Caption := _('Information');
end;

procedure TDlgMessage.SetMessage(const Filename: string);
var
  WMsg, w: Integer;
  Msg: string;
begin
  Msg := ExtractFileName(Filename) + #13#10 + format(_(LNGAlreadyExists), ['']);
  WMsg := Canvas.TextWidth(Msg);
  w := ClientWidth - LMessage.Left - 10;
  if WMsg > w then
    Width := LMessage.Left + WMsg + 10;
  LMessage.Caption := Msg;
end;

end.
