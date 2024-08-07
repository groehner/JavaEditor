unit UDlgMessage;

interface

uses
  Forms, StdCtrls, ExtCtrls, Vcl.Controls, Vcl.Graphics, System.Classes;

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
    procedure setMessage(const filename: string);
  end;

implementation

uses SysUtils, JvGnugettext, UStringRessources;

{$R *.dfm}

procedure TDlgMessage.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  Caption:= _('Information');
end;

procedure TDlgMessage.setMessage(const filename: string);
  var wmsg, w: integer; msg: string;
begin
  msg:= ExtractFilename(Filename) + #13#10 + format(_(LNGAlreadyExists), ['']);
  wmsg:= Canvas.TextWidth(msg);
  w:= ClientWidth - LMessage.Left - 10;
  if wmsg > w then
    width:= LMessage.Left + wmsg + 10;
  LMessage.Caption:= msg;
end;

end.
