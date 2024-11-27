unit UDlgMethodCall;

interface

uses
  Forms, StdCtrls, Vcl.Controls, System.Classes;

type
  TFMethodCallDialog = class(TForm)
    LMethodCall: TLabel;
    LParametervalues: TLabel;
    LResult: TLabel;
    EResult: TEdit;
    BOK: TButton;
    EMethodCall: TEdit;
    EParametervalues: TEdit;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Prepare;
  end;

implementation

uses Graphics, Math, JvGnugettext;

{$R *.dfm}

procedure TFMethodCallDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFMethodCallDialog.BOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFMethodCallDialog.Prepare;
begin
  var aWidth:= 300;
  aWidth:= max(aWidth, Canvas.TextWidth(EMethodCall.Text));
  aWidth:= max(aWidth, Canvas.TextWidth(EParameterValues.Text));
  aWidth:= max(aWidth, Canvas.TextWidth(EResult.Text));
  if aWidth > 300 then aWidth:= aWidth + 10;
  Width := (360 - 300) + aWidth;
  EMethodCall.Width:= aWidth;
  EParameterValues.Width:= aWidth;
  EResult.Width:= aWidth;
  BOK.Left:= 257 + (aWidth - 300);
  ActiveControl:= BOK;
end;

end.
