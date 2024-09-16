unit UTooltip;

interface

uses
  Windows, Classes, Controls, Forms, ExtCtrls, ComCtrls, SHDocVw, Vcl.ToolWin,
  Vcl.OleCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, SVGIconImageCollection, TB2Item, SpTBXItem, TB2Dock,
  TB2Toolbar;

type
  TFTooltip = class(TForm)
    WebBrowser: TWebBrowser;
    ToolbarTooltip: TSpTBXToolbar;
    TBClose: TSpTBXItem;
    TBGotoSourcecode: TSpTBXItem;
    TBOpenUrl: TSpTBXItem;
    TBBack: TSpTBXItem;
    TBForward: TSpTBXItem;
    TBZoomOut: TSpTBXItem;
    TBZoomIn: TSpTBXItem;
    CloseTooltipTimer: TTimer;
    OpenTooltipTimer: TTimer;
    icTooltip: TSVGIconImageCollection;
    vilToolbarLight: TVirtualImageList;
    vilToolbarDark: TVirtualImageList;
    SpTBXStatusBar: TSpTBXStatusBar;
    procedure TBCloseClick(Sender: TObject);
    procedure TBOpenUrlClick(Sender: TObject);
    procedure TBBackClick(Sender: TObject);
    procedure TBForwardClick(Sender: TObject);
    procedure TBRefreshClick(Sender: TObject);
    procedure TBGotoSourcecodeClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure TBZoomOutClick(Sender: TObject);
    procedure CloseTooltipTimerExecute(Sender: TObject);
    procedure OpenTooltipTimerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
  private
    EditForm: TForm;
    TokenRect: TRect;
    URL: string;
    base: string;
    Line: string;
    Pathname: string;
    procedure Hide;
    procedure SetFontSize(Delta: integer);
    procedure FindAdress(const s: string);
    procedure WebBrowserOnCommandStateChange(Sender: TObject; Command: Integer; Enable: WordBool);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    closemanually: boolean;
    procedure Init(Form: TForm; P: TPoint; Rect: TRect; const Token: string);
    procedure setFile(const aPathname, aLine: string);
    procedure setURL(const aUrl: string);
    function getHead: string;
    procedure ChangeStyle;
  end;

 var FTooltip: TFTooltip = nil;

implementation

uses SysUtils, JvGnugettext, UJava, UConfiguration, UUtils, UEditorForm;

{$R *.dfm}

procedure TFTooltip.Init(Form: TForm; P: TPoint; Rect: TRect; const Token: string);
begin
  // new Tooltip due to browser.history (forward/backward)  ?
  EditForm:= Form;
  setBounds(P.x, P.y, FConfiguration.TooltipWidth, FConfiguration.TooltipHeight);
  Webbrowser.OnCommandStateChange:= WebBrowserOnCommandStateChange;
  TokenRect:= Rect;
  closemanually:= (Token = '# VK_F2 #');
end;

procedure TFTooltip.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do begin
    Style := WS_POPUP or WS_BORDER OR WS_SIZEBOX;
    ExStyle := WS_EX_TOOLWINDOW OR WS_EX_TOPMOST;
  end;
end;

procedure TFTooltip.SetFontSize(Delta: integer);
begin
  FConfiguration.TooltipFontSize:= FConfiguration.TooltipFontSize + Delta;
  if FConfiguration.TooltipFontSize < 6 then FConfiguration.TooltipFontSize:= 6;
  var SL:= TStringList.Create;
  try
    try
      SL.LoadFromFile(FConfiguration.TempDir + 'Tooltip.html');
      SL.Strings[0]:= getHead;
      SL.SaveToFile(FConfiguration.TempDir + 'Tooltip.html');
      TBRefreshClick(Self);
    except
    end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TFTooltip.OpenTooltipTimerExecute(Sender: TObject);
begin
  OpenTooltipTimer.Enabled:= false;
  if Application.Active and (PtInRect(TokenRect, Mouse.CursorPos) or closemanually) then begin
    Show;
    if (EditForm as TFEditForm).Editor.CanFocus then
      (EditForm as TFEditForm).Editor.SetFocus;
    FindAdress(FConfiguration.TempDir + 'Tooltip.html');
    CloseTooltipTimer.Enabled:= not closemanually;
  end else if Visible then
    Hide;
end;

procedure TFTooltip.CloseTooltipTimerExecute(Sender: TObject);
begin
  if Visible and not (IsMouseOverControl(Self) or IsMouseOverControl(EditForm) or PtInRect(TokenRect, Mouse.CursorPos)) then begin
    Hide;
    (EditForm as TFEditForm).LastToken:= '';
  end;
end;

procedure TFTooltip.TBOpenUrlClick(Sender: TObject);
begin
  if Url <> '' then begin
    FJava.callHelp(url);
    Hide;
  end;
end;

procedure TFTooltip.TBGotoSourcecodeClick(Sender: TObject);
begin
  if (Pathname <> '') and (Line <> '0') then begin
    FJava.NewEditor(Pathname, 'T' + Line + ')X1)Y' + Line + ')W1)');
    Hide;
  end;
end;

procedure TFTooltip.TBCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TFTooltip.setURL(const aurl: string);
begin
  url:= ToWeb('', aurl);
  base:= ToWeb('', ExtractFilepath(aurl));
end;

procedure TFTooltip.setFile(const aPathname, aLine: string);
begin
  Pathname:= aPathname;
  Line:= aLine;
end;

function TFTooltip.getHead: string;
begin
  Result:= '<html><head>';
  if url <> '' then
    Result:= Result + '<base href="' + base + '">';
  Result:= Result + '</head><body style="font-size: ' + IntToStr(PPIScale(FConfiguration.TooltipFontSize)) + 'px; background-color: #FFFFB2">';
end;

procedure TFTooltip.Hide;
begin
  OpenTooltipTimer.Enabled:= false;
  CloseTooltipTimer.Enabled:= false;
  FConfiguration.TooltipWidth:=  Width;
  FConfiguration.TooltipHeight:= Height;
  inherited;
end;

procedure TFTooltip.WebBrowserOnCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  if assigned(TBBack) then begin
    case Command of
      CSC_NAVIGATEBACK:    TBBack.Enabled := Enable;
      CSC_NAVIGATEFORWARD: TBForward.Enabled := Enable;
    end;
  end;
end;

procedure TFTooltip.FindAdress(const s: string);
  var Flags: OLEVariant;
begin
  Flags:= 0;
  WebBrowser.Navigate(s, Flags, Flags, Flags, Flags);
end;

procedure TFTooltip.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  Webbrowser.OnCommandStateChange:= nil;
  FJava.ActiveTool:= -1
end;

procedure TFTooltip.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ChangeStyle;
end;

procedure TFTooltip.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 12;
end;

procedure TFTooltip.FormShow(Sender: TObject);
begin
  FJava.ActiveTool:= 12;
end;

procedure TFTooltip.TBBackClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.GoBack;
end;

procedure TFTooltip.TBForwardClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.GoForward;
end;

procedure TFTooltip.TBRefreshClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.Refresh;
end;

procedure TFTooltip.TBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
end;

procedure TFTooltip.TBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
end;

procedure TFTooltip.ChangeStyle;
begin
  if FConfiguration.isDark
    then ToolBarTooltip.Images:= vilToolbarDark
    else ToolBarTooltip.Images:= vilToolbarLight;
end;

end.
