unit UTooltip;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  SHDocVw,
  System.ImageList,
  Vcl.OleCtrls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  TB2Dock,
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
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
  private
    FEditForm: TForm;
    FTokenRect: TRect;
    FUrl: string;
    FBase: string;
    FCloseManually: Boolean;
    FLine: string;
    FPathname: string;
    procedure Hide;
    procedure SetFontSize(Delta: Integer);
    procedure FindAdress(const Str: string);
    procedure WebBrowserOnCommandStateChange(Sender: TObject; Command: Integer;
      Enable: WordBool);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure Init(Form: TForm; Posi: TPoint; Rect: TRect; const Token: string);
    procedure SetFile(const Pathname, Line: string);
    procedure SetURL(const Url: string);
    function GetHead: string;
    procedure ChangeStyle;
    property CloseManually: Boolean read FCloseManually;
  end;

var
  FTooltip: TFTooltip = nil;

implementation

uses
  SysUtils,
  JvGnugettext,
  UJava,
  UConfiguration,
  UUtils,
  UEditorForm;

{$R *.dfm}

procedure TFTooltip.Init(Form: TForm; Posi: TPoint; Rect: TRect;
  const Token: string);
begin
  // new Tooltip due to browser.history (forward/backward)  ?
  FEditForm := Form;
  SetBounds(Posi.X, Posi.Y, FConfiguration.TooltipWidth,
    FConfiguration.TooltipHeight);
  WebBrowser.OnCommandStateChange := WebBrowserOnCommandStateChange;
  FTokenRect := Rect;
  FCloseManually := (Token = '# VK_F2 #');
end;

procedure TFTooltip.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_SIZEBOX;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;
end;

procedure TFTooltip.SetFontSize(Delta: Integer);
begin
  FConfiguration.TooltipFontSize := FConfiguration.TooltipFontSize + Delta;
  if FConfiguration.TooltipFontSize < 6 then
    FConfiguration.TooltipFontSize := 6;
  var
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FConfiguration.TempDir + 'Tooltip.html');
    StringList[0] := GetHead;
    StringList.SaveToFile(FConfiguration.TempDir + 'Tooltip.html');
    TBRefreshClick(Self);
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFTooltip.OpenTooltipTimerExecute(Sender: TObject);
begin
  OpenTooltipTimer.Enabled := False;
  if Application.Active and (PtInRect(FTokenRect, Mouse.CursorPos) or
    FCloseManually) then
  begin
    Show;
    if TFEditForm(FEditForm).Editor.CanFocus then
      TFEditForm(FEditForm).Editor.SetFocus;
    FindAdress(FConfiguration.TempDir + 'Tooltip.html');
    CloseTooltipTimer.Enabled := not FCloseManually;
  end
  else if Visible then
    Hide;
end;

procedure TFTooltip.CloseTooltipTimerExecute(Sender: TObject);
begin
  if Visible and not(IsMouseOverControl(Self) or IsMouseOverControl(FEditForm) or
    PtInRect(FTokenRect, Mouse.CursorPos)) then
  begin
    Hide;
    TFEditForm(FEditForm).LastToken := '';
  end;
end;

procedure TFTooltip.TBOpenUrlClick(Sender: TObject);
begin
  if FUrl <> '' then
  begin
    FJava.CallHelp(FUrl);
    Hide;
  end;
end;

procedure TFTooltip.TBGotoSourcecodeClick(Sender: TObject);
begin
  if (FPathname <> '') and (FLine <> '0') then
  begin
    FJava.NewEditor(FPathname, 'T' + FLine + ')X1)Y' + FLine + ')W1)');
    Hide;
  end;
end;

procedure TFTooltip.TBCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TFTooltip.SetURL(const Url: string);
begin
  FUrl := ToWeb('', Url);
  FBase := ToWeb('', ExtractFilePath(Url));
end;

procedure TFTooltip.SetFile(const Pathname, Line: string);
begin
  Self.FPathname := Pathname;
  Self.FLine := Line;
end;

function TFTooltip.GetHead: string;
begin
  Result := '<html><head>';
  if FUrl <> '' then
    Result := Result + '<FBase href="' + FBase + '">';
  Result := Result + '</head><body style="font-size: ' +
    IntToStr(PPIScale(FConfiguration.TooltipFontSize)) +
    'px; background-color: #FFFFB2">';
end;

procedure TFTooltip.Hide;
begin
  OpenTooltipTimer.Enabled := False;
  CloseTooltipTimer.Enabled := False;
  FConfiguration.TooltipWidth := Width;
  FConfiguration.TooltipHeight := Height;
  inherited;
end;

procedure TFTooltip.WebBrowserOnCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  if Assigned(TBBack) then
  begin
    case Command of
      CSC_NAVIGATEBACK:
        TBBack.Enabled := Enable;
      CSC_NAVIGATEFORWARD:
        TBForward.Enabled := Enable;
    end;
  end;
end;

procedure TFTooltip.FindAdress(const Str: string);
var
  Flags: OleVariant;
begin
  Flags := 0;
  WebBrowser.Navigate(Str, Flags, Flags, Flags, Flags);
end;

procedure TFTooltip.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  WebBrowser.OnCommandStateChange := nil;
  FJava.ActiveTool := -1;
end;

procedure TFTooltip.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ChangeStyle;
end;

procedure TFTooltip.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool := 12;
end;

procedure TFTooltip.FormShow(Sender: TObject);
begin
  FJava.ActiveTool := 12;
end;

procedure TFTooltip.TBBackClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.GoBack;
end;

procedure TFTooltip.TBForwardClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.GoForward;
end;

procedure TFTooltip.TBRefreshClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.Refresh;
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
  if FConfiguration.IsDark then
    ToolbarTooltip.Images := vilToolbarDark
  else
    ToolbarTooltip.Images := vilToolbarLight;
end;

end.
