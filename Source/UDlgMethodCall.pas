unit UDlgMethodCall;

interface

uses
  Forms,
  StdCtrls,
  Vcl.Controls,
  System.Classes;

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

uses
  Math,
  JvGnugettext;

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
  var AWidth := 300;
  AWidth := Max(AWidth, Canvas.TextWidth(EMethodCall.Text));
  AWidth := Max(AWidth, Canvas.TextWidth(EParametervalues.Text));
  AWidth := Max(AWidth, Canvas.TextWidth(EResult.Text));
  if AWidth > 300 then
    AWidth := AWidth + 10;
  Width := (360 - 300) + AWidth;
  EMethodCall.Width := AWidth;
  EParametervalues.Width := AWidth;
  EResult.Width := AWidth;
  BOK.Left := 257 + (AWidth - 300);
  ActiveControl := BOK;
end;

end.
