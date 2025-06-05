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
    procedure PrepareShow(AEditorRect: TRect; XPos, Y1Pos, Y2Pos: Integer;
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
  XPos, Y1Pos, Y2Pos: Integer; const AReplaceText: string);
var
  Width, Height: Integer;
begin
  LConfirmation.Caption := Format(_('"%s" replace?'), [AReplaceText]);
  Width := AEditorRect.Right - AEditorRect.Left;
  Height := AEditorRect.Bottom - AEditorRect.Top;

  if Width <= Width then
    XPos := AEditorRect.Left - (Width - Width) div 2
  else begin
    if XPos + Width > AEditorRect.Right then
      XPos := AEditorRect.Right - Width;
  end;
  if Y2Pos > AEditorRect.Top + MulDiv(Height, 2, 3) then
    Y2Pos := Y1Pos - Height - 4
  else
    Inc(Y2Pos, 4);
  SetBounds(XPos, Y2Pos, Width, Height);
end;

end.

