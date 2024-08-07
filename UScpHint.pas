unit UScpHint;

interface

uses
  Messages, Classes, Graphics, Controls, Forms, StdCtrls, Menus,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGIconImageCollection, TB2Item, SpTBXItem;

type
  TFScpHint = class(TForm)
    MHint: TMemo;
    PopupMenuHint: TSpTBXPopupMenu;
    MIClose: TSpTBXItem;
    MIFont: TSpTBXItem;
    MICopy: TSpTBXItem;
    vilPopupMenuLight: TVirtualImageList;
    vilPopupMenuDark: TVirtualImageList;
    icMenuHint: TSVGIconImageCollection;
    procedure MICopyClick(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure PopupMenuHintPopup(Sender: TObject);
    procedure MHintKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    WindowOpened: boolean;
    function IsBorderless: Boolean;
    function GetBorderSpace: Integer;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure WM_NCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WM_NCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    TabActiv: boolean;
    procedure SetFont(aFont: TFont);
    procedure SetFontSize(Delta: integer);
    procedure ChangeStyle;
  end;

var
  FScpHint: TFScpHint = nil;

implementation

uses Windows, SysUtils, Dialogs, Clipbrd, Math, UJava, UConfiguration, UUtils;

{$R *.dfm}

procedure TFScpHint.FormCreate(Sender: TObject);
begin
  BorderWidth:= 0;
  StyleElements:= []; // StyleElement [seBorder] prevents resizing!
  Position:= poDefault; // poScreenCenter displaces the window
  WindowOpened:= false;
end;

procedure TFScpHint.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 10;
end;

procedure TFScpHint.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveSettings;
  Action:= caHide;
  FJava.ActiveTool:= -1;
end;

procedure TFScpHint.FormResize(Sender: TObject);
begin
  if Sender = Self then begin
    var H:= FJava.scpJava.Form.Height;
    FJava.scpJava.Form.SetBounds(Left, Top - H + 7, Width, H);
  end;
end;

procedure TFScpHint.FormShow(Sender: TObject);
begin
  FJava.ActiveTool:= 13;  // not 10 because this is called by TSynBaseCompletionProposal.DoShow
  if not WindowOpened then begin
    LoadSettings;
    WindowOpened:= true;
  end;
end;

function TFScpHint.IsBorderless: Boolean;
begin
  Result := BorderStyle in [bsNone, bsToolWindow, bsSizeToolWin];
end;

function TFScpHint.GetBorderSpace: Integer;
begin
  case BorderStyle of
    bsSingle:
      Result :=
        GetSystemMetrics(SM_CYFIXEDFRAME);
    bsDialog, bsToolWindow:
      Result :=
        GetSystemMetrics(SM_CYDLGFRAME);
    bsSizeable, bsSizeToolWin:
      Result :=
        GetSystemMetrics(SM_CYSIZEFRAME) +
        GetSystemMetrics(SM_CXPADDEDBORDER);
    else
      Result := 0;
  end;
end;

procedure TFScpHint.PopupMenuHintPopup(Sender: TObject);
begin
  MICopy.Enabled:= (MHint.SelLength > 0);
end;

procedure TFScpHint.Resize;
begin
  inherited;
  if (WindowState = wsNormal) and (not IsBorderless) then
    Padding.Top := 1
  else
    Padding.Top := 0;
end;

{ MESSAGES }

procedure TFScpHint.WM_NCCalcSize(var Msg: TWMNCCalcSize);
begin
  inherited;
  if BorderStyle = bsNone then exit;
  var CaptionBarHeight := GetSystemMetrics(SM_CYCAPTION);
  if WindowState = wsNormal then
    Inc(CaptionBarHeight, GetBorderSpace);
  Dec(Msg.CalcSize_Params.rgrc[0].Top, CaptionBarHeight);
end;

procedure TFScpHint.WM_NCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
  var ResizeSpace := GetBorderSpace;
  if (WindowState = wsNormal) and
     (BorderStyle in [bsSizeable, bsSizeToolWin]) and
     (Msg.YPos - BoundsRect.Top <= ResizeSpace) then
    begin
      if Msg.XPos - BoundsRect.Left <= 2 * ResizeSpace then
        Msg.Result := HTTOPLEFT
      else if BoundsRect.Right - Msg.XPos <= 2 * ResizeSpace then
        Msg.Result := HTTOPRIGHT
      else
        Msg.Result := HTTOP;
    end;
end;

procedure TFScpHint.SetFont(aFont: TFont);
begin
  MHint.Font.Assign(aFont);
  FJava.scpJava.Form.Font.Assign(aFont);
  if Assigned(FJava.EditorForm) then
    FJava.scpJava.ActivateTimer(FJava.EditorForm.Editor);
end;

procedure TFScpHint.SetFontSize(Delta: integer);
begin
  var Size:= MHint.Font.Size + Delta;
  if Size < 6 then Size:= 6;
  MHint.Font.Size:= Size;
  FJava.scpJava.Form.Font.Size:= Size;
  if Assigned(FJava.EditorForm) then
    FJava.scpJava.ActivateTimer(FJava.EditorForm.Editor);
end;

procedure TFScpHint.SaveSettings;
begin
  FConfiguration.WriteStringU('CodeCompletion', 'FontName', MHint.Font.Name);
  FConfiguration.WriteIntegerU('CodeCompletion', 'FontSize', PPIUnScale(MHint.Font.Size));
  FConfiguration.WriteIntegerU('CodeCompletion', 'Width', PPIUnScale(FJava.scpJava.Width));
  FConfiguration.WriteIntegerU('CodeCompletion', 'Lines', FJava.scpJava.NbLinesInWindow);
  FConfiguration.WriteIntegerU('CodeCompletion', 'Height', PPIUnScale(Height));
end;

procedure TFScpHint.LoadSettings;
begin
  Font.Name:= FConfiguration.ReadStringU('CodeCompletion', 'FontName', 'Consolas');
  Font.Size:= PPIScale(FConfiguration.ReadIntegerU('CodeCompletion', 'FontSize', 10));
  FJava.scpJava.Width:= PPIScale(FConfiguration.ReadIntegerU('CodeCompletion', 'Width', 260));
  FJava.scpJava.NbLinesInWindow:= FConfiguration.ReadIntegerU('CodeCompletion', 'Lines', 8);
  Height:= PPIScale(FConfiguration.ReadIntegerU('CodeCompletion', 'Height', 100));
  SetFont(Font);
  ChangeStyle;
end;

procedure TFScpHint.MHintKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Tab then
    if TabActiv then
      if FJava.scpJava.Form.canFocus then FJava.scpJava.Form.SetFocus
    else
      TabActiv:= true;
end;

procedure TFScpHint.MICloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFScpHint.MICopyClick(Sender: TObject);
begin
  Clipboard.AsText:= MHint.SelText;
end;

procedure TFScpHint.MIFontClick(Sender: TObject);
begin
  with FJava.FDFont do begin
    Options:= [];
    Font.Assign(MHint.Font);
    if Execute then
      SetFont(Font);
  end;
end;

procedure TFScpHint.ChangeStyle;
begin
  if FConfiguration.isDark
    then PopupMenuHint.Images:= vilPopupMenuDark
    else PopupMenuHint.Images:= vilPopupMenuLight;
end;

procedure TFScpHint.CreateParams(var Params: TCreateParams);
begin
  BorderStyle := bsNone;
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
  Params.Style := Params.Style or WS_SIZEBOX;
end;

end.
