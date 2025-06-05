unit ULLMSuggestForm;

interface

uses
  Winapi.Messages,
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  SynEdit,
  SVGIconImageCollection;

type
  TSuggestWindow = class(TForm)
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    vilImages: TVirtualImageList;
    spiAccept: TSpTBXItem;
    spiCancel: TSpTBXItem;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    spiAcceptLine: TSpTBXItem;
    spiAcceptWord: TSpTBXItem;
    icImages: TSVGIconImageCollection;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure spiAcceptClick(Sender: TObject);
    procedure spiAcceptLineClick(Sender: TObject);
    procedure spiAcceptWordClick(Sender: TObject);
    procedure spiCancelClick(Sender: TObject);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    FSeSuggest: TSynEdit;
    FEditor: TCustomSynEdit;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure ShowSuggestion(const Suggestion: string; Editor: TCustomSynEdit);

var
  SuggestWindow: TSuggestWindow;

implementation

{$R *.dfm}

uses
  Winapi.Windows,
  System.Types,
  Vcl.Graphics,
  System.Math,
  SynEditTypes,
  SynEditKeyCmds,
  UConfiguration;

procedure TSuggestWindow.FormCreate(Sender: TObject);
begin
  FSeSuggest:= TSynEdit.Create(Self);
  with FSeSuggest do begin
    Left:= 0;
    Top:= 0;
    Align:= alClient;
    Font.Charset:= ANSI_CHARSET;
    Font.Color:= clWindowText;
    Font.Height:= -13;
    Font.Name:= 'Consolas';
    Font.Style:= [];
    Font.Quality:= fqClearTypeNatural;
    TabOrder:= 0;
    UseCodeFolding:= False;
    Gutter.Font.Charset:= DEFAULT_CHARSET;
    Gutter.Font.Color:= clWindowText;
    Gutter.Font.Height:= -11;
    Gutter.Font.Name:= 'Consolas';
    Gutter.Font.Style:= [];
    Gutter.Font.Quality:= fqClearTypeNatural;
    Gutter.Visible:= False;
    RightEdge:= 0;
    Highlighter:= FConfiguration.JavaHighlighter;
  end;
end;

procedure TSuggestWindow.WMNCHitTest(var Message: TWMNCHitTest);
//  Makes the form resizable
var
  Size: Integer;
  Posi: TPoint;
begin
  Size := GetSystemMetrics(SM_CXSIZEFRAME);
  Posi := ScreenToClient(Message.Pos);
  if Posi.Y < Size then
  begin
    if Posi.X < Size then
      Message.Result := HTTOPLEFT
    else if Posi.X > ClientWidth - Size then
      Message.Result := HTTOPRIGHT
    else
      Message.Result := HTTOP;
  end
  else if Posi.Y > ClientHeight - Size then
  begin
    if Posi.X < Size then
      Message.Result := HTBOTTOMLEFT
    else if Posi.X > ClientWidth - Size then
      Message.Result := HTBOTTOMRIGHT
    else
      Message.Result := HTBOTTOM;
  end
  else
  begin
    if Posi.X < Size then
      Message.Result := HTLEFT
    else if Posi.X > ClientWidth - Size then
      Message.Result := HTRIGHT;
  end;
  if Message.Result = 0 then
    inherited;
end;

{ TSuggestWindow }

procedure TSuggestWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    {
      WS_THICKFRAME causes Windows 10 to display a 6 pixel title bar
      Also with VCL Styles and WS_BORDER the window is not resizable
      So we use WS_BORDER and make the window sizeable by handling WM_NCHITTEST
    }
    Style := WS_POPUP or WS_BORDER or WS_CLIPCHILDREN;
    ExStyle := WS_EX_TOOLWINDOW;

    // Only affects the first time you create the handle
    // https://stackoverflow.com/questions/44521877/window-class-style-cs-noclose-does-not-work-after-calling-to-recreatewnd
    WindowClass.style := WindowClass.style or CS_DROPSHADOW;
  end;

end;

procedure TSuggestWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  SuggestWindow := nil;
end;

procedure TSuggestWindow.spiAcceptClick(Sender: TObject);
begin
  FEditor.SelText := FSeSuggest.Text;
  Close;
end;

procedure TSuggestWindow.spiAcceptLineClick(Sender: TObject);
begin
  var Line := FSeSuggest.Lines[0];
  if FSeSuggest.Lines.Count > 1 then
    Line := Line + sLineBreak;
  FSeSuggest.Lines.Delete(0);
  FEditor.SelText := Line;
  if FSeSuggest.Lines.Count = 0 then
    Close;
end;

procedure TSuggestWindow.spiAcceptWordClick(Sender: TObject);
begin
  FSeSuggest.BlockBegin := BufferCoord(0, 0);
  FSeSuggest.ExecuteCommand(ecSelWordRight, ' ', nil);
  var FirstWord := FSeSuggest.SelText;
  FSeSuggest.SelText := '';
  var TrimTrailingSpaces := eoTrimTrailingSpaces in FEditor.Options;
  FEditor.Options := FEditor.Options - [eoTrimTrailingSpaces];
  FEditor.SelText := FirstWord;
  if TrimTrailingSpaces then
    FEditor.Options := FEditor.Options + [eoTrimTrailingSpaces];
  if FSeSuggest.Text = '' then
    Close;
end;

procedure TSuggestWindow.spiCancelClick(Sender: TObject);
begin
  Close;
end;

procedure ShowSuggestion(const Suggestion: string; Editor: TCustomSynEdit);
const
  MaxLines = 10;
  BordersSize = 8;
begin
  if not Assigned(SuggestWindow) then begin
    SuggestWindow := TSuggestWindow.Create(Application.MainForm);
    SuggestWindow.FSeSuggest.Parent:= SuggestWindow;
  end;

  SuggestWindow.FEditor := Editor;
  if Editor.SelAvail then
  begin
    SuggestWindow.spiAcceptWord.Visible := False;
    SuggestWindow.spiAcceptWord.Enabled := False;
    SuggestWindow.spiAcceptLine.Visible := False;
    SuggestWindow.spiAcceptLine.Enabled := False;
  end;

  var Monitor := Screen.MonitorFromWindow(Editor.Handle);
  var BlockBegin := Editor.BlockBegin;
  var DisplayBegin := Editor.BufferToDisplayPos(BlockBegin);
  var Point := Editor.RowColumnToPixels(DisplayBegin);
  var ScreenPoint := Editor.ClientToScreen(Point);

  SuggestWindow.FSeSuggest.Text := Suggestion;
  var LineCount := Min(MaxLines, SuggestWindow.FSeSuggest.Lines.Count);

  SuggestWindow.ScaleForPPI(Editor.CurrentPPI);
  // Window size
  var Height := LineCount * SuggestWindow.FSeSuggest.LineHeight +
    SuggestWindow.SpTBXToolbar.Height + BordersSize;
  var Width := SuggestWindow.FSeSuggest.CharWidth * 80 + BordersSize;
  // Window position
  var Left := Min(ScreenPoint.X, Monitor.Left + Monitor.Width - Width);
  var Top := ScreenPoint.Y - Height;
  if Top < Monitor.Top then
     Top := ScreenPoint.Y + Editor.LineHeight;
  SuggestWindow.SetBounds(Left, Top, Width, Height);
  SuggestWindow.ShowModal;
end;

end.
