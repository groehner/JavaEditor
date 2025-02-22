{ -----------------------------------------------------------------------------
  Unit Name: ULLMChatForm
  Author:    Kiriakos Vlahos, Gerhard Röhner
  Date:      17-May-2024
  Purpose:   The Chat window serves the purpose of interacting with
             Large Language Models (LLMs) withoug leaving PyScripter.
  History:
  -----------------------------------------------------------------------------}

unit ULLMChatForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.UITypes,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ImageList,
  System.Actions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.ComCtrls,
  Vcl.WinXPanels,
  Vcl.WinXCtrls,
  Vcl.ActnList,
  Vcl.AppEvnts,
  Vcl.BaseImageCollection,
  SynEdit,
  SynEditHighlighter,
  SynHighlighterMulti,
  SVGIconImage,
  JvComponentBase,
  JvDockControlForm,
  SpTBXItem,
  SpTBXControls,
  SpTBXDkPanels,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  SpTBXEditors,
  SpTBXSkins,
  UBaseForm,
  UDockForm,
  uLLMSupport,
  SVGIconImageCollection;

type

  // Interposer class to prevent the auto-scrolling when clicking
  TScrollBox = class(Vcl.Forms.TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); override;
  end;

  TLLMChatForm = class(TDockableForm)
    pnlQuestion: TPanel;
    ScrollBox: TScrollBox;
    QAStackPanel: TStackPanel;
    aiBusy: TActivityIndicator;
    ChatActionList: TActionList;
    actChatSave: TAction;
    sbAsk: TSpeedButton;
    AppEvents: TApplicationEvents;
    SpTBXDock: TSpTBXDock;
    SpTBXToolbar: TSpTBXToolbar;
    spiSave: TSpTBXItem;
    SpTBXRightAlignSpacerItem: TSpTBXRightAlignSpacerItem;
    actChatRemove: TAction;
    actChatNew: TAction;
    actChatPrevious: TAction;
    actChatNext: TAction;
    spiNextTopic: TSpTBXItem;
    spiPreviousTopic: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    spiNewTopic: TSpTBXItem;
    spiRemoveTopic: TSpTBXItem;
    actCopyText: TAction;
    actAskQuestion: TAction;
    SynMultiSyn: TSynMultiSyn;
    Splitter: TSpTBXSplitter;
    pmAsk: TSpTBXPopupMenu;
    mnCopy: TSpTBXItem;
    mnPaste: TSpTBXItem;
    pmTextMenu: TSpTBXPopupMenu;
    mnCopyText: TSpTBXItem;
    actTopicTitle: TAction;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    spiTitle: TSpTBXItem;
    actCancelRequest: TAction;
    spiCancel: TTBItem;
    actCopyCode: TAction;
    mnCopyCode: TSpTBXItem;
    actCopyToNewEditor: TAction;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    mnCopyToNewEditor: TSpTBXItem;
    vilImages: TVirtualImageList;
    pAsk: TPanel;
    icMenuAndToolbar: TSVGIconImageCollection;
    vilImagesDark: TVirtualImageList;
    vilImagesLight: TVirtualImageList;
    procedure actChatSaveExecute(Sender: TObject);
    procedure AppEventsMessage(var Msg: tagMsg; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure synQuestionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actAskQuestionExecute(Sender: TObject);
    procedure actCancelRequestExecute(Sender: TObject);
    procedure actChatNewExecute(Sender: TObject);
    procedure actChatNextExecute(Sender: TObject);
    procedure actChatPreviousExecute(Sender: TObject);
    procedure actChatRemoveExecute(Sender: TObject);
    procedure actCopyCodeExecute(Sender: TObject);
    procedure actCopyTextExecute(Sender: TObject);
    procedure actCopyToNewEditorExecute(Sender: TObject);
    procedure actTopicTitleExecute(Sender: TObject);
    procedure ChatActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure pmTextMenuPopup(Sender: TObject);
    procedure synQuestionEnter(Sender: TObject);
    procedure mnCopyClick(Sender: TObject);
    procedure mnPasteClick(Sender: TObject);
  private
    SynQuestion: TSynEdit;
    function GetCodeBlock(Editor: TSynEdit): string;
    procedure DisplayTopicTitle(Title: string);
    procedure PanelQAResize(Sender: TObject);
    procedure DisplayQA(const QA, ImgName: string);
    procedure ClearConversation;
    procedure DisplayActiveChatTopic;
    procedure PythonHighlighterChange(Sender: TObject);
    procedure SetQuestionTextHint;
    procedure OnLLMResponse(Sender: TObject; const Prompt, Answer, Reason: string);
    procedure OnLLMError(Sender: TObject; const Error: string);
  protected
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    LLMChat: TLLMChat;
    procedure ChangeStyle;
  end;

var
  FLLMChatForm: TLLMChatForm;

implementation

{$R *.dfm}

uses
  System.Math,
  System.IOUtils,
  Vcl.Themes,
  Vcl.Clipbrd,
  JvGnugettext,
  UJava,
  UEditorForm,
  UConfiguration;

resourcestring
  SQuestionHintValid = 'Ask me anything';
  SQuestionHintInvalid = 'Chat setup incomplete';

procedure TLLMChatForm.actChatSaveExecute(Sender: TObject);
begin
  var FileName := TPath.Combine(FConfiguration.HomeDir, 'Chat history.json');
  LLMChat.SaveChat(FileName);
end;

procedure TLLMChatForm.FormDestroy(Sender: TObject);
begin
  FConfiguration.JavaHighlighter.UnhookAttrChangeEvent(PythonHighlighterChange);
  LLMChat.Free;
end;

procedure TLLMChatForm.PanelQAResize(Sender: TObject);
begin
  var pnlAnswer := Sender as TSpTBXPanel;
  var synAnswer := pnlAnswer.Controls[1] as TSynEdit;
  var NewHeight := Max(MulDiv(30, CurrentPPI, 96), 2 * Margins.Bottom +
    synAnswer.DisplayRowCount * synAnswer.LineHeight);
  if NewHeight <> pnlAnswer.Height then
    pnlAnswer.Height := NewHeight;
end;

procedure TLLMChatForm.PythonHighlighterChange(Sender: TObject);
begin
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.WhitespaceAttribute.Foreground;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.WhitespaceAttribute.Foreground;
end;

procedure TLLMChatForm.AppEventsMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Msg.message = WM_MOUSEWHEEL then
  begin
    var Window := WindowFromPoint(Msg.pt);
    var WinControl := FindControl(Window);
    if (WinControl is TSynEdit) and string(WinControl.Name).StartsWith('synQA') then
    begin
      SendMessage(WinControl.Parent.Handle, WM_MOUSEWHEEL, Msg.WParam, Msg.LParam);
      Handled := True;
    end;
  end;
end;

procedure TLLMChatForm.ClearConversation;
begin
  while QAStackPanel.ControlCount > 0  do
    QAStackPanel.Controls[QAStackPanel.ControlCount - 1].Free;
end;

procedure TLLMChatForm.DisplayQA(const QA, ImgName: string);
begin
  var PanelQA := TSpTBXPanel.Create(Self);
  with PanelQA do begin
    Name := 'PanelQA' + QAStackPanel.ControlCount.ToString;
    Color := StyleServices.GetSystemColor(clWindow);
    Anchors := [akLeft,akTop,akRight];
    Width := 570;
    Height := 50;
    Anchors := [akLeft, akTop, akRight];
    Borders := False;
    AlignWithMargins := True;
  end;
  var SvgImage := TSVGIconImage.Create(Self);
  with SvgImage do begin
    Left := 0;
    Top := 0;
    Width := 24;
    Height := 24;
    AutoSize := False;
    if TFConfiguration.IsDark then
      ImageList:= vilImagesDark
    else
      ImageList:= vilImagesLight;
    ImageName := ImgName;
    Anchors := [akLeft, akTop];
    FixedColor := StyleServices.GetSystemColor(clWindowText);
    ApplyFixedColorToRootOnly := True;
    Parent := PanelQA;
  end;
  var synQA := TSynEdit.Create(Self);
  with synQA do begin
    Name := 'synQA' + QAStackPanel.ControlCount.ToString;
    Font.Color := StyleServices.GetSystemColor(clWindowText);
    Color := StyleServices.GetSystemColor(clWindow);
    BorderStyle := bsNone;
    Anchors := [akLeft, akRight, akTop, akBottom];
    Options := Options + [eoRightMouseMovesCursor];
    Highlighter := SynMultiSyn;
    Top := 0;
    Left := 30;
    Width := PanelQA.Width - 30;
    Height := PanelQA.Height;
    Font.Name := 'Consolas';
    Font.Size := 10;
    Gutter.Visible := False;
    PopUpMenu := pmTextMenu;
    ScrollBars := ssNone;
    WantTabs := True;
    WantReturns := True;
    Parent := PanelQA;
  end;
  PanelQA.ScaleForPPI(CurrentPPI);
  PanelQA.Parent := QAStackPanel;
  synQA.Text := QA.Trim;
  PanelQA.OnResize :=  PanelQAResize;
  // Resize twice! - The first time the Scrollbox scrollbar may be shown
  PanelQAResize(PanelQA);
  PanelQAResize(PanelQA);
  ScrollBox.VertScrollBar.Position := ScrollBox.VertScrollBar.Range - 1;
end;

procedure TLLMChatForm.DisplayTopicTitle(Title: string);
begin
  if Title = '' then
    Caption := _('Chat')
  else
    Caption := _('Chat') + ' - ' + Title;
end;

procedure TLLMChatForm.DisplayActiveChatTopic;
begin
  ClearConversation;
  DisplayTopicTitle(LLMChat.ActiveTopic.Title);
  QAStackPanel.LockDrawing;
  try
    for var QAItem in LLMChat.ActiveTopic.QAItems do
    begin
      DisplayQA(QAItem.Prompt, 'UserQuestion');
      DisplayQA(QAItem.Answer, 'Assistant2');
    end;
  finally
    QAStackPanel.UnlockDrawing;
  end;
  if SynQuestion.HandleAllocated then
    SynQuestion.SetFocus;
end;

procedure TLLMChatForm.FormCreate(Sender: TObject);
begin
  inherited;
  SynQuestion:= TSynEdit.create(self);
  with SynQuestion do begin
    Parent:= pnlQuestion;
    Top:= 4;
    Left:= 4;
    Align:= alClient;
    Cursor:= crDefault;
    Font.Charset:= ANSI_CHARSET;
    Font.Color:= clWindowText;
    Font.Height:= -13;
    Font.Name:= 'Consolas';
    Font.Style:= [];
    Font.Quality:= fqClearTypeNatural;
    PopupMenu:= pmAsk;
    TabOrder:= 0;
    OnEnter:= synQuestionEnter;
    OnKeyDown:= synQuestionKeyDown;
    UseCodeFolding:= False;
    Gutter.Font.Charset:= DEFAULT_CHARSET;
    Gutter.Font.Color:= clWindowText;
    Gutter.Font.Height:= -11;
    Gutter.Font.Name:= 'Consolas';
    Gutter.Font.Style:= [];
    Gutter.Font.Quality:= fqClearTypeNatural;
    Gutter.Visible:= False;
    HideSelection:= True;
    SynMultiSyn.DefaultHighlighter:= FConfiguration.JavaHighlighter;
    Highlighter:= SynMultiSyn;
    ScrollBars:= ssVertical;
    WordWrap:= True;
    RightEdge:= 0;
  end;

  {$IF CompilerVersion >= 36}
  ScrollBox.UseWheelForScrolling := True;
  {$ENDIF}
  synQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synQuestion.Color := StyleServices.GetSystemColor(clWindow);

  SynMultiSyn.Schemes[0].Highlighter := FConfiguration.JavaHighlighter;
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.IdentifierAttri.Foreground;
  SynMultiSyn.Schemes[1].Highlighter := FConfiguration.JavaHighlighter;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.IdentifierAttri.Foreground;
  FConfiguration.JavaHighlighter.HookAttrChangeEvent(PythonHighlighterChange);
  LLMChat := TLLMChat.Create;
  LLMChat.OnLLMError := OnLLMError;
  LLMChat.OnLLMResponse := OnLLMResponse;

  // Restore settings and history
  var FileName := TPath.Combine(FConfiguration.HomeDir, 'Chat history.json');
  LLMChat.LoadChat(FileName);
  SetQuestionTextHint;
  ChangeStyle;
end;

procedure TLLMChatForm.FormShow(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure
  begin
    DisplayActiveChatTopic;
  end);
end;

function TLLMChatForm.GetCodeBlock(Editor: TSynEdit): string;
var
  Token: String;
  Attri: TSynHighlighterAttributes;
begin
  Result := '';
  var BC := Editor.CaretXY;

  Editor.GetHighlighterAttriAtRowCol(BC, Token, Attri);
  if SynMultiSyn.CurrScheme < 0 then // not inside python code
    Exit;

  var StartLine := BC.Line;
  var EndLine := BC.Line;

  while (StartLine > 1) and  not Editor.Lines[StartLine -1].TrimLeft.StartsWith('```') do
    Dec(StartLine);
  while (EndLine < Editor.Lines.Count) and not Editor.Lines[EndLine -1].TrimLeft.StartsWith('```') do
    Inc(EndLine);

  Result := '';
  for var Line := StartLine + 1 to EndLine - 1 do
  begin
    Result := Result + Editor.Lines[Line - 1];
    if Line < EndLine - 1 then
      Result := Result + sLineBreak;
  end;
end;

procedure TLLMChatForm.mnCopyClick(Sender: TObject);
begin
  SynQuestion.CopyToClipboard;
end;

procedure TLLMChatForm.mnPasteClick(Sender: TObject);
begin
  Synquestion.PasteFromClipboard;
end;

procedure TLLMChatForm.OnLLMError(Sender: TObject; const Error: string);
begin
  MessageDlg(Error, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TLLMChatForm.OnLLMResponse(Sender: TObject; const Prompt,
  Answer, Reason: string);
begin
  DisplayQA(Prompt, Answer);
  synQuestion.Clear;
end;

procedure TLLMChatForm.synQuestionKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if (Shift * [ssShift, ssCtrl] <> []) and  (Key = vkReturn) then
  begin
    actAskQuestion.Execute;
    Key := 0;
  end;
end;

procedure TLLMChatForm.actAskQuestionExecute(Sender: TObject);
begin
  if synQuestion.Text = '' then
    Exit;

  if FConfiguration.LanguageCode = 'de' then
    LLMChat.Ask('Answer in German. ' + synQuestion.Text)
  else
    LLMChat.Ask(synQuestion.Text);
end;

procedure TLLMChatForm.actCancelRequestExecute(Sender: TObject);
begin
  LLMChat.CancelRequest;
end;

procedure TLLMChatForm.actChatNewExecute(Sender: TObject);
begin
  LLMChat.NewTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatNextExecute(Sender: TObject);
begin
  LLMChat.NextTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatPreviousExecute(Sender: TObject);
begin
  LLMChat.PreviousTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actChatRemoveExecute(Sender: TObject);
begin
  LLMChat.RemoveTopic;
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.actCopyCodeExecute(Sender: TObject);
begin
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var Code := GetCodeBlock(Editor);
  if Code <> '' then
    Clipboard.AsText := Code;
end;

procedure TLLMChatForm.actCopyTextExecute(Sender: TObject);
begin
  if pmTextMenu.PopupComponent is TSynEdit then with TSynEdit(pmTextMenu.PopupComponent) do
  begin
    if SelAvail then
      Clipboard.AsText := SelText
    else
      Clipboard.AsText := Text;
  end
  else if pmTextMenu.PopupComponent is TSpTBXLabel then
    Clipboard.AsText := TSpTBXLabel(pmTextMenu.PopupComponent).Caption;
end;

procedure TLLMChatForm.actCopyToNewEditorExecute(Sender: TObject);
begin
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var Code := GetCodeBlock(Editor);

  if Code = '' then
    Exit;

  var EditForm:= TFEditForm(FJava.FormFactory(fkEditor));
  EditForm.Editor.Text := Code;
  EditForm.New(FJava.getFilename('.java'));
end;

procedure TLLMChatForm.actTopicTitleExecute(Sender: TObject);
var
  Title: string;
begin
  Title := LLMChat.ChatTopics[LLMChat.ActiveTopicIndex].Title;
  if InputQuery(_('Topic Title'), _('Enter title:'), Title) then
    LLMChat.ChatTopics[LLMChat.ActiveTopicIndex].Title := Title;
  DisplayTopicTitle(Title);
end;

procedure TLLMChatForm.ChatActionListUpdate(Action: TBasicAction; var Handled:
    Boolean);
begin
  Handled := True;
  actChatNew.Enabled := ScrollBox.ControlCount > 0;
  actChatNext.Enabled := LLMChat.ActiveTopicIndex < High(LLMChat.ChatTopics);
  actChatPrevious.Enabled := LLMChat.ActiveTopicIndex > 0;
  actAskQuestion.Enabled := LLMChat.ValidateSettings = svValid;

  var IsBusy := LLMChat.IsBusy;
  if aiBusy.Animate <> IsBusy then
    aiBusy.Animate := IsBusy;
  actCancelRequest.Visible := IsBusy;
  actCancelRequest.Enabled := IsBusy;
end;

procedure TLLMChatForm.pmTextMenuPopup(Sender: TObject);
var
  Token: String;
  Attri: TSynHighlighterAttributes;
begin
  actCopyCode.Enabled := False;
  actCopyToNewEditor.Enabled := False;
  if not (pmTextMenu.PopupComponent is TSynEdit) then
    Exit;

  var Editor := TSynEdit(pmTextMenu.PopupComponent);
  var BC := Editor.CaretXY;

  Editor.GetHighlighterAttriAtRowCol(BC, Token, Attri);
  actCopyCode.Enabled := (SynMultiSyn.CurrScheme >= 0) and
    not Editor.Lines[BC.Line - 1].StartsWith('```');
  actCopyToNewEditor.Enabled := actCopyCode.Enabled;
end;

procedure TLLMChatForm.SetQuestionTextHint;
begin
  {var Validation := LLMChat.ValidateSettings;

  if Validation = svValid then
    synQuestion.TextHint := SQuestionHintValid
  else
    synQuestion.TextHint := SQuestionHintInvalid + ': ' + LLMChat.ValidationErrMsg(Validation);
  }
  synQuestion.Invalidate;
end;

procedure TLLMChatForm.synQuestionEnter(Sender: TObject);
begin
  // Spell Checking
  // CommandsDataModule.SynSpellCheck.Editor := synQuestion;
end;

procedure TLLMChatForm.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  synQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  synQuestion.Color := StyleServices.GetSystemColor(clWindow);
  {$IF CompilerVersion >= 36}
  aiBusy.IndicatorColor := aicCustom;
  aiBusy.IndicatorCustomColor := StyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
  DisplayActiveChatTopic;
end;

procedure TLLMChatForm.ChangeStyle;
begin
  if TFConfiguration.IsDark then begin
    SpTBXToolbar.Images:= vilImagesDark;
    sbAsk.Images:= vilImagesDark;
  end
  else
  begin
    SpTBXToolbar.Images:= vilImagesLight;
    sbAsk.Images:= vilImagesLight;
  end;
end;

{ TScrollBox }
// To avoid the jumping of the cursor
procedure TScrollBox.AutoScrollInView(AControl: TControl);
begin
end;

end.
