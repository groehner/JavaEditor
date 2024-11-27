unit UDlgConfirmReplace;

interface

uses
  Windows, Forms, StdCtrls, ExtCtrls, Vcl.Controls, System.Classes;

type
  TFConfirmReplace = class(TForm)
    BYes: TButton;
    LConfirmation: TLabel;
    BNo: TButton;
    BCancel: TButton;
    BReplaceAll: TButton;
    IImage: TImage;
    procedure FormCreate(Sender: TObject);
  public
    procedure PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer;
      const AReplaceText: string);
  end;

implementation

uses SysUtils, JvGnugettext;

{$R *.DFM}

{ TConfirmReplaceDialog }

procedure TFConfirmReplace.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  IImage.Picture.Icon.Handle:= LoadIcon(0, IDI_QUESTION);
end;

procedure TFConfirmReplace.PrepareShow(AEditorRect: TRect;
  X, Y1, Y2: integer; const AReplaceText: string);
var
  nW, nH: integer;
begin
  LConfirmation.Caption := Format(_('"%s" replace?'), [AReplaceText]);
  nW := AEditorRect.Right - AEditorRect.Left;
  nH := AEditorRect.Bottom - AEditorRect.Top;

  if nW <= Width then
    X := AEditorRect.Left - (Width - nW) div 2
  else begin
    if X + Width > AEditorRect.Right then
      X := AEditorRect.Right - Width;
  end;
  if Y2 > AEditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

end.

