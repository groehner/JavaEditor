﻿unit ULLMChatForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WebView2,
  Winapi.ActiveX,
  System.Classes,
  System.ImageList,
  System.Actions,
  System.RegularExpressions,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.WinXCtrls,
  Vcl.ActnList,
  Vcl.BaseImageCollection,
  Vcl.Edge,
  SynEdit,
  SynEditHighlighter,
  SynHighlighterMulti,
  SpTBXItem,
  SpTBXDkPanels,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  SpTBXSkins,
  MarkdownProcessor,
  SVGIconImageCollection,
  ULLMSupport;

type
  TLLMChatForm = class(TForm)
    pnlQuestion: TPanel;
    aiBusy: TActivityIndicator;
    ChatActionList: TActionList;
    actChatSave: TAction;
    sbAsk: TSpeedButton;
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
    actAskQuestion: TAction;
    SynMultiSyn: TSynMultiSyn;
    Splitter: TSpTBXSplitter;
    pmAsk: TSpTBXPopupMenu;
    mnCopy: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    mnSpelling: TSpTBXSubmenuItem;
    mnPaste: TSpTBXItem;
    actTopicTitle: TAction;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    spiTitle: TSpTBXItem;
    actCancelRequest: TAction;
    spiCancel: TTBItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    EdgeBrowser: TEdgeBrowser;
    actPrint: TAction;
    spiPrint: TTBItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    pnlBrowserCover: TPanel;
    icMenuAndToolbar: TSVGIconImageCollection;
    vilImagesLight: TVirtualImageList;
    vilImagesDark: TVirtualImageList;
    procedure actChatSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure synQuestionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure actAskQuestionExecute(Sender: TObject);
    procedure actCancelRequestExecute(Sender: TObject);
    procedure actChatNewExecute(Sender: TObject);
    procedure actChatNextExecute(Sender: TObject);
    procedure actChatPreviousExecute(Sender: TObject);
    procedure actChatRemoveExecute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure actTopicTitleExecute(Sender: TObject);
    procedure ChatActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure EdgeBrowserCreateWebViewCompleted(Sender: TCustomEdgeBrowser;
        AResult: HRESULT);
    procedure EdgeBrowserNavigationCompleted(Sender: TCustomEdgeBrowser; IsSuccess:
        Boolean; WebErrorStatus: COREWEBVIEW2_WEB_ERROR_STATUS);
    procedure EdgeBrowserWebMessageReceived(Sender: TCustomEdgeBrowser; Args:
        TWebMessageReceivedEventArgs);
    procedure FormShow(Sender: TObject);
    procedure HighlightCheckedImg(Sender: TObject; ACanvas: TCanvas; State:
        TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
        TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
        PaintDefault: Boolean);
    procedure synQuestionEnter(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FDefaultLang: string;
    FBlockCount: Integer;
    FCodeBlocksRE: TRegEx;
    FBrowserReady: Boolean;
    FMarkdownProcessor: TMarkdownProcessor;
    FSynQuestion: TSynEdit;
    FHasFocus: Boolean;
    FLLMChat: TLLMChat;
    procedure CopyToNewEditor(const Code: string);
    procedure ClearConversation;
    procedure DisplayActiveChatTopic;
    procedure DisplayQA(const Prompt, Answer, Reason: string);
    procedure DisplayTopicTitle(Title: string);
    procedure LoadBoilerplate;
    function MarkdownToHTML(const MarkDown: string): string;
    function NavigateToString(Html: string): Boolean;
    procedure PythonHighlighterChange(Sender: TObject);
    procedure SetBrowserColorScheme;
    procedure SetQuestionTextHint;
    procedure StyleWebPage;
    procedure OnLLMResponse(Sender: TObject; const Prompt, Answer, Reason: string);
    procedure OnLLMError(Sender: TObject; const Error: string);
  protected
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMSpSkinChange(var Message: TMessage); message WM_SPSKINCHANGE;
  public
    procedure ChangeStyle;
    property LLMChat: TLLMChat read FLLMChat;
  end;

var
  FLLMChatForm: TLLMChatForm;

implementation

{$R *.dfm}

uses
  System.UITypes,
  System.SysUtils,
  System.IOUtils,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Themes,
  Vcl.Clipbrd,
  MarkdownUtils,
  SynEditKeyCmds,
  JvGnugettext,
  UJava,
  UEditorForm,
  UUtils,
  UStringRessources,
  UConfiguration,
  UBaseForm;

resourcestring
  SQuestionHintValid = 'Ask me anything';
  SQuestionHintInvalid = 'Chat setup incomplete';

{$REGION 'HTML templates'}

const
  Boilerplate = '''
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta Name="viewport" content="Width=device-Width, initial-scale=1.0">
    <title>LLM Chat</title>
%Str
</head>
<body>
    <!-- svgs -->
%Str
    <!-- Title and other Content -->
%Str
    <!-- Container where Q&A will be added -->
    <div id="qa-list">
    </div>
    <!-- Scripts -->
%Str
</body>
</html>
''';

  MainStyleSheetTemplate = '''
    <style>
        ::-webkit-scrollbar {
            width: 6px; /* Initial width of the scrollbar */
            height: 6px; /* Initial height of the scrollbar */
        }
        ::-webkit-scrollbar-track {
            background: %s; /* Background of the scrollbar track */
        }
        ::-webkit-scrollbar-thumb {
            background: %s; /* Color of the scrollbar thumb */
            border-radius: 4px; /* Rounded corners for the scrollbar thumb */
        }
        ::-webkit-scrollbar-thumb:hover {
            background: %s; /* Color of the scrollbar thumb on hover */
        }
        ::-webkit-scrollbar-corner {
            background: %0:s; /* The corner between scrollbars */
        }
        @media print {
            @page {
                @bottom-center {
                    content: "Page " counter(page);
                    }
            }
        }

        body {
            background-color: %0:s;
            color: %3:s;
            font-family: Aptos, Calibri, Arial, sans-serif;
            line-height: 1.4;
            margin: 10px;
        }
        a {
            color: %4:s;
            text-decoration: none;
        }
        a:hover {
            text-decoration: underline;
        }
        blockquote {
          color: gray;
          border-left: 3px solid #ccc;
          padding-left: 10px;
          margin: 10px 0;
          max-height: 400px;
          overflow-y: auto;
        }
    </style>

''';
var
  MainStyleSheet: string;

const
  QAStyleSheet = '''
    <style>
        .qa-container {
            display: flex;
            align-items: flex-start;
            margin-bottom: 5px; /* Space after each question */
            overflow: hidden
        }
        .icon {
            margin-right: 10px;
            width: 24px;
            height: 24px;
            flex-shrink: 0;
            display: flex;
            align-items: center;
        }
        .question {
            font-weight: bold;
            overflow: hidden
        }
        .answer {
            /* margin-bottom: 15px;  Space after each answer */
            overflow: hidden
       }
    </style>

''';

  CodeStyleSheetTemplate = '''
    <style>
        .code-box {
            border: 1px solid #ccc;
            border-radius: 5px;
            margin: 10px 0;
            max-width: 1000px;
            overflow: hidden; /* Ensures the box doesn't overflow */
        }
        .code-header {
            background: %s;
            padding: 5px;
            border-bottom: 1px solid #ccc;
            font-weight: bold;
            color: %s;
        }
        .code-container {
            padding: 5px;
            max-height: 500px; /* Limits the height of the code box */
            overflow-y: auto; /* Adds vertical scrolling */
            font-size: 14px;
            /* the Prism style background. Need to change if you change the style*/
            background: %s;
        }
        .code-header-button {
            float: right;
            background: transparent;
            color: inherit;
            border: none;
            border-radius: 3px;
            cursor: pointer;
            font-size: 12px;
            padding: 0px;
            height: 24px;
            margin-right: 1em;
        }
        .code-header-button:hover {
            background-color: #6F6F6F6F;
        }

        /* prism style begin */
        code[class*=language-],
        pre[class*=language-] {
            color: #ccc;
            background: 0 0;
            font-family: Consolas, 'Courier New', monospace;
            font-size: 1em;
            text-align: left;
            white-space: pre;
            word-spacing: normal;
            word-break: normal;
            word-wrap: normal;
            line-height: 1.5;
            -moz-tab-size: 4;
            -o-tab-size: 4;
            tab-size: 4;
            -webkit-hyphens: none;
            -moz-hyphens: none;
            -ms-hyphens: none;
            hyphens: none
        }

        pre[class*=language-] {
            padding: 1em;
            margin: .5em 0;
            overflow: auto
        }

        :not(pre)>code[class*=language-],
        pre[class*=language-] {
            background: #2d2d2d
        }

        :not(pre)>code[class*=language-] {
            padding: .1em;
            border-radius: .3em;
            white-space: normal
        }

        .token.block-comment,
        .token.cdata,
        .token.comment,
        .token.doctype,
        .token.prolog {
            color: %s
        }

       .token.operator,
       .token.punctuation {
            color: %s
        }

        .token.attr-Name{
            color: %s
        }

        .token.deleted,
        .token.namespace,
        .token.tag {
            color: %s
        }

        .token.function,
        .token.function-Name {
            color: %s
        }

        .token.Boolean,
        .token.number {
            color: %s
        }

        .token.class-Name,
        .token.constant,
        .token.property,
        .token.symbol {
            color: %s
        }

        .token.atrule,
        .token.builtin,
        .token.important,
        .token.keyword,
        .token.selector {
            color: %s
        }

        .token.attr-value,
        .token.Char,
        .token.regex,
        .token.string,
        .token.variable {
            color: %s
        }

        .triple-quoted-string {
            color: %s
        }

        .token.entity,
        .token.url {
            color: %s
        }

        .token.bold,
        .token.important {
            font-weight: 700
        }

        .token.italic {
            font-style: italic
        }

        .token.entity {
            cursor: help
        }

        .token.inserted {
            color: green
        }
        /* prism style end*/


        /* ** modify prism style ** */

        /* inline code */
        :not(pre)>code {
            background-color: #444; /* Darker background for code */
            border-radius: 4px;
            padding: 2px 4px;
            font-family: monospace;
            color: #ffcc00; /* Light color for code text */
        }
        code[class*=language-] {
            color: %s;
        }
        pre[class*=language-] {
            background: %2:s;
            line-height: 1.2;
            padding: 1em;
            margin: 0 0;
            overflow: visible
        }

    </style>

''';
var
  CodeStyleSheet: string;

const
  CodeBlock = '''
      <div class="code-box">
          <div class="code-header">
              %Str
              <button class="code-header-button" title="%Str" onclick="copyCode('code%s')">
                  <svg Width="24" Height="24">
                      <use href="#Copy-icon">
                  </svg>
              </button>
              <button class="code-header-button" title="%Str" onclick="codeToEditor('code%2:s')">
                  <svg Width="24" Height="24">
                      <use href="#new-editor-icon">
                  </svg>
              </button>
          </div>
          <div class="code-container" id="code%2:Str">
              <pre><code class="language-%4:Str">%5:Str</code></pre>
          </div>
      </div>

  ''';

  SvgIcons = '''
    <!-- Define the SVG icon once -->
    <svg style="display: none;">
        <symbol id="question-icon" viewBox="0 0 24 24" fill="none" stroke=currentColor>
            <path opacity="0.8" d="M12 11C14.2091 11 16 9.20914 16 7C16 4.79086 14.2091 3 12 3C9.79086 3 8 4.79086 8 7C8 9.20914 9.79086 11 12 11Z" fill="cornflowerblue" />
            <path d="M20.9531 13V12.995M19 7.4C19.2608 6.58858 20.0366 6 20.9531 6C22.0836 6 23 6.89543 23 8C23 9.60675 21.2825 8.81678 21 10.5M8 15H16C18.2091 15 20 16.7909 20 19V21H4V19C4 16.7909 5.79086 15 8 15ZM16 7C16 9.20914 14.2091 11 12 11C9.79086 11 8 9.20914 8 7C8 4.79086 9.79086 3 12 3C14.2091 3 16 4.79086 16 7Z" stroke-Width="2" stroke-linecap="round" stroke-linejoin="round"/>
        </symbol>

        <symbol id="assistant-icon" viewBox="0 -960 960 960" fill=currentColor>
            <circle r="70" cx="360" cy="-640" fill="#E24444" />
            <circle r="70" cx="600" cy="-640" fill="#E24444" />
            <path d="M160-120v-200q0-33 23.5-56.5T240-400h480q33 0 56.5 23.5T800-320v200H160Zm200-320q-83 0-141.5-58.5T160-640q0-83 58.5-141.5T360-840h240q83 0 141.5 58.5T800-640q0 83-58.5 141.5T600-440H360ZM240-200h480v-120H240v120Zm120-320h240q50 0 85-35t35-85q0-50-35-85t-85-35H360q-50 0-85 35t-35 85q0 50 35 85t85 35Z" />
        </symbol>

        <symbol id="Copy-icon" viewBox="0 0 24 24" fill=currentColor>
        <path d="M19,21H8V7H19M19,5H8A2,2 0 0,0 6,7V21A2,2 0 0,0 8,23H19A2,2 0 0,0 21,21V7A2,2 0 0,0 19,5M16,1H4A2,2 0 0,0 2,3V17H4V3H16V1Z" />
        </symbol>

        <symbol id="new-editor-icon" viewBox="0 0 24 24" fill=currentColor>
          <path d="M14,3V5H17.59L7.76,14.83L9.17,16.24L19,6.41V10H21V3M19,19H5V5H12V3H5C3.89,3 3,3.9 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V12H19V19Z" />
        </symbol>
    </svg>
''';

  JSScripts = '''
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/plugins/autoloader/prism-autoloader.min.js"></script>
    <script>
        // Function to add a Q&A to the page
        function addQA(question, answer) {
            var qaList = document.getElementById('qa-list');

            // Create the question container
            const questionContainer = document.createElement('div');
            questionContainer.classList.add('qa-container');

            // Create the question icon container
            const questionIconDiv = document.createElement('div'); // More descriptive name
            questionIconDiv.classList.add('icon');
            const questionIconSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            questionIconSvg.setAttribute('width', '24');
            questionIconSvg.setAttribute('height', '24');
            const questionUseElement = document.createElementNS('http://www.w3.org/2000/svg', 'use');
            questionUseElement.setAttribute('href', '#question-icon');
            questionIconSvg.appendChild(questionUseElement);
            questionIconDiv.appendChild(questionIconSvg);

            const questionDiv = document.createElement('div');
            questionDiv.classList.add('question');
            questionDiv.innerHTML = question;

            questionContainer.appendChild(questionIconDiv);
            questionContainer.appendChild(questionDiv);


            // Create the answer container
            const answerContainer = document.createElement('div');
            answerContainer.classList.add('qa-container');

            // Create the answer icon container (Corrected variable name!)
            const answerIconDiv = document.createElement('div');
            answerIconDiv.classList.add('icon');
            const answerIconSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
            answerIconSvg.setAttribute('width', '24');
            answerIconSvg.setAttribute('height', '24');
            const answerUseElement = document.createElementNS('http://www.w3.org/2000/svg', 'use');
            answerUseElement.setAttribute('href', '#assistant-icon');
            answerIconSvg.appendChild(answerUseElement);
            answerIconDiv.appendChild(answerIconSvg);

            // Create the answer div
            const answerDiv = document.createElement('div');
            answerDiv.classList.add('answer');
            answerDiv.innerHTML = answer;

            answerContainer.appendChild(answerIconDiv);
            answerContainer.appendChild(answerDiv);

            qaList.appendChild(questionContainer);
            qaList.appendChild(answerContainer);
            qaList.appendChild(document.createElement('hr'));
        }

        function copyCode(id) {
            // Get the code content
            const code = document.getElementById(id).innerText;
             window.chrome.webview.postMessage(code);
        }

        function codeToEditor(id) {
            // Get the code content
            const code = document.getElementById(id).innerText;
             window.chrome.webview.postMessage(String.fromCharCode(31) + code);
        }

        function clearQA() {
            var qaList = document.getElementById('qa-list');
            qaList.replaceChildren();
        }
    </script>
''';

{$ENDREGION 'HTML templates'}

{$REGION 'Utility functions'}

function RemoveCommonIndentation(const Text: string): string;
var
  Trimmed: string;
  MinIndent: Integer;
begin
  // Split the input text into lines
  var Lines := Text.Split([#13#10, #10]);
  if Length(Lines) = 0 then
    Exit(Text.TrimLeft);

  // Find the minimum indentation (number of leading spaces or tabs)
  MinIndent := MaxInt;
  for var Line in Lines do
  begin
    Trimmed := Line.TrimLeft;
    if (Trimmed <> '') and ((Line.Length - Trimmed.Length) < MinIndent) then
      MinIndent := (Line.Length - Trimmed.Length);
  end;

  if MinIndent = 0 then Exit(Text);

  // Remove the common indentation from each line
  for var I := Low(Lines) to High(Lines) do
    Lines[I] := Copy(Lines[I], MinIndent + 1);

  // Combine the lines back into a single string
  Result := string.Join(#13#10, Lines);
end;

{$ENDREGION 'Utility functions'}

procedure TLLMChatForm.actChatSaveExecute(Sender: TObject);
begin
  var FileName := TPath.Combine(FConfiguration.HomeDir, 'Chat history.json');
  LLMChat.SaveChat(FileName);
end;

procedure TLLMChatForm.FormDeactivate(Sender: TObject);
begin
  FHasFocus:= False;
end;

procedure TLLMChatForm.FormDestroy(Sender: TObject);
begin
  FConfiguration.JavaHighlighter.UnhookAttrChangeEvent(PythonHighlighterChange);
  LLMChat.Free;  FMarkdownProcessor.Free;
end;

procedure TLLMChatForm.PythonHighlighterChange(Sender: TObject);
begin
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.WhitespaceAttribute.Foreground;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.WhitespaceAttribute.Foreground;

  if FBrowserReady then
  begin
    StyleWebPage;
    LoadBoilerplate;
  end;
end;

procedure TLLMChatForm.ClearConversation;
begin
  FBlockCount := 0;
  EdgeBrowser.ExecuteScript('clearQA()');
end;

procedure TLLMChatForm.CopyToNewEditor(const Code: string);
begin
  var EditForm:= TFEditForm(FJava.FormFactory(fkEditor));
  EditForm.Editor.Text := Code;
  EditForm.New(FJava.GetFilename('.java'));
end;

procedure TLLMChatForm.DisplayQA(const Prompt, Answer, Reason: string);
const
  QAScriptCode = '''
  var question = `%Str`;
  var answer = `%Str`;
  addQA(question, answer);
  Prism.highlightAll();
  window.scroll(0,100000);
  ''';
  ReasonTemplate = '''
  <details>
  <summary><b>Reasoning</b></summary>
  <blockquote>%Str</blockquote>
  </details>
  <Posi>
  ''';
begin
  if not FBrowserReady then Exit;

  var PromptHtml := MarkdownToHTML(Prompt);
  var AnswerHtml := MarkdownToHTML(Answer);

  if Reason <> '' then
  begin
    var ReasonHtml := MarkdownToHTML(Reason).Trim;
    ReasonHtml := Format(ReasonTemplate, [ReasonHtml]);
    AnswerHtml := ReasonHtml + AnswerHtml;
  end;
  EdgeBrowser.ExecuteScript(Format(QAScriptCode, [PromptHtml, AnswerHtml]));
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

  for var QAItem in LLMChat.ActiveTopic.QAItems do
    DisplayQA(QAItem.Prompt, QAItem.Answer, QAItem.Reason);

  if FSynQuestion.HandleAllocated and FHasFocus then
    FSynQuestion.SetFocus;
end;

procedure TLLMChatForm.FormActivate(Sender: TObject);
begin
  FHasFocus:= True;
end;

procedure TLLMChatForm.FormCreate(Sender: TObject);
const
  CodeRegEx = '```(\w+)?\s*\n([\s\S]*?)\n?```';
begin
  FSynQuestion:= TSynEdit.Create(Self);
  with FSynQuestion do begin
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
    ScrollBars:= ssBoth;
    WordWrap:= True;
    RightEdge:= 0;
    UseCodeFolding := False;
    Options:= [eoAutoIndent,eoDragDropEditing,eoDropFiles,eoEnhanceHomeKey,
      eoEnhanceEndKey,eoGroupUndo,eoKeepCaretX,eoSmartTabDelete,eoTabIndent,
      eoTabsToSpaces,eoShowLigatures];
  end;

  FDefaultLang := 'java';
  FHasFocus := False;
  FCodeBlocksRE := CompiledRegEx(CodeRegEx);
  FMarkdownProcessor := TMarkdownProcessor.CreateDialect(mdCommonMark);

  EdgeBrowser.UserDataFolder := TPath.Combine(FConfiguration.HomeDir,
    'WebView2');

  SynMultiSyn.Schemes[0].Highlighter := FConfiguration.JavaHighlighter;
  SynMultiSyn.Schemes[0].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.IdentifierAttri.Foreground;
  SynMultiSyn.Schemes[1].Highlighter := FConfiguration.JavaHighlighter;
  SynMultiSyn.Schemes[1].MarkerAttri.Foreground :=
    FConfiguration.JavaHighlighter.IdentifierAttri.Foreground;
  FConfiguration.JavaHighlighter.HookAttrChangeEvent(PythonHighlighterChange);
  FLLMChat := TLLMChat.Create;
  FLLMChat.OnLLMError := OnLLMError;
  FLLMChat.OnLLMResponse := OnLLMResponse;

  // Restore settings and history
  var FileName := TPath.Combine(FConfiguration.HomeDir, 'Chat history.json');
  try
    FLLMChat.LoadChat(FileName);
  except
    StyledMessageDlg(_('Could not read the Chat history'), TMsgDlgType.mtError,
      [TMsgDlgBtn.mbOK], 0);
    DeleteFile(FileName);
  end;

  SetQuestionTextHint;
  ChangeStyle;
end;

procedure TLLMChatForm.OnLLMError(Sender: TObject; const Error: string);
begin
  StyledMessageDlg(Error, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
end;

procedure TLLMChatForm.OnLLMResponse(Sender: TObject; const Prompt,
  Answer, Reason: string);
var FPrompt: string;
begin
  if Pos('Answer in German. ', Prompt) > 0 then
    FPrompt:= Copy(Prompt, Length('Answer in German. '), Length(Prompt))
  else
    FPrompt:= Prompt;
  DisplayQA(FPrompt, Answer, Reason);
  FSynQuestion.Clear;
end;

procedure TLLMChatForm.synQuestionKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
begin
  if FSynQuestion.Text = SQuestionHintValid then
    FSynQuestion.Text := '';

  if Key = vkReturn then
  begin
    if Shift * [ssShift, ssCtrl] <> [] then
      FSynQuestion.ExecuteCommand(ecLineBreak, ' ', nil)
    else
      actAskQuestion.Execute;
    Key := 0;
  end;
end;

procedure TLLMChatForm.actAskQuestionExecute(Sender: TObject);
begin
  if FSynQuestion.Text = '' then
    Exit;

  if FConfiguration.LanguageCode = 'de' then
    LLMChat.Ask('Answer in German. ' + FSynQuestion.Text)
  else
    LLMChat.Ask(FSynQuestion.Text);
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

procedure TLLMChatForm.actPrintExecute(Sender: TObject);
begin
  EdgeBrowser.ShowPrintUI(TEdgeBrowser.TPrintUIDialogKind.Browser);
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
  actChatNew.Enabled := FBrowserReady and (Length(LLMChat.ActiveTopic.QAItems) > 0);
  actChatNext.Enabled := FBrowserReady and (LLMChat.ActiveTopicIndex < High(LLMChat.ChatTopics));
  actChatPrevious.Enabled := FBrowserReady and (LLMChat.ActiveTopicIndex > 0);
  actAskQuestion.Enabled := FBrowserReady and (LLMChat.ValidateSettings = svValid);

  var IsBusy := LLMChat.IsBusy;
  if aiBusy.Animate <> IsBusy then
    aiBusy.Animate := IsBusy;
  actCancelRequest.Visible := IsBusy;
  actCancelRequest.Enabled := IsBusy;
end;

procedure TLLMChatForm.EdgeBrowserCreateWebViewCompleted(Sender:
    TCustomEdgeBrowser; AResult: HRESULT);
// Also called when the Browser is recreated (style change)
begin
  if AResult <> S_OK then
    StyledMessageDlg(_(SWebView2Error), mtError, [mbOK], 0)
  else
  begin
    FBrowserReady := True;
    StyleWebPage;
    SetBrowserColorScheme;
    LoadBoilerplate;
  end;
end;

procedure TLLMChatForm.EdgeBrowserNavigationCompleted(Sender:
    TCustomEdgeBrowser; IsSuccess: Boolean; WebErrorStatus:
    COREWEBVIEW2_WEB_ERROR_STATUS);
begin
  //  Called after LoadBoireplate loads the basic Web page
  if not IsSuccess then
  begin
    StyledMessageDlg(_(SWebNavigationError), mtError, [mbOK], 0);
    Exit;
  end;
  FBrowserReady := True;
  DisplayActiveChatTopic;
  pnlBrowserCover.Visible := False;
end;

procedure TLLMChatForm.EdgeBrowserWebMessageReceived(Sender:
    TCustomEdgeBrowser; Args: TWebMessageReceivedEventArgs);
var
  ArgsString: PWideChar;
begin
  Args.ArgsInterface.TryGetWebMessageAsString(ArgsString);
  if ArgsString^ = #31 then
  begin
    Inc(ArgsString);
    CopyToNewEditor(ArgsString);
  end
  else
    Clipboard.AsText := ArgsString;
end;

procedure TLLMChatForm.FormShow(Sender: TObject);
begin
  SetQuestionTextHint;
  if EdgeBrowser.DefaultInterface = nil then
    TThread.ForceQueue(nil, procedure
    begin
      EdgeBrowser.CreateWebView;
    end);
end;

procedure TLLMChatForm.HighlightCheckedImg(Sender: TObject; ACanvas: TCanvas; State:
    TSpTBXSkinStatesType; const PaintStage: TSpTBXPaintStage; var AImageList:
    TCustomImageList; var AImageIndex: Integer; var ARect: TRect; var
    PaintDefault: Boolean);
begin
  if (PaintStage = pstPrePaint) and (Sender as TSpTBXItem).Checked then
  begin
    ACanvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
    ACanvas.FillRect(ARect);
  end;
  PaintDefault := True;
end;

procedure TLLMChatForm.LoadBoilerplate;
// Loads the basic web pages
begin
  NavigateToString(Format(Boilerplate,
    [MainStyleSheet + CodeStyleSheet + QAStyleSheet,
     SvgIcons, '', JSScripts]));
end;

function TLLMChatForm.NavigateToString(Html: string): Boolean;
begin
  if not FBrowserReady then Exit(False);

  FBrowserReady := False;
  EdgeBrowser.NavigateToString(Html);
  Result := True;
end;

function TLLMChatForm.MarkdownToHTML(const MarkDown: string): string;
begin
  Result := '';
  var Matches := FCodeBlocksRE.Matches(MarkDown);
  if Matches.Count > 0 then
  begin
    var CodeEnd := 1;
    for var Match in Matches do
    begin
      var TextBefore := Copy(MarkDown, CodeEnd, Match.Index - CodeEnd);
      if TextBefore <> '' then
        Result := Result + FMarkdownProcessor.process(TextBefore);
      Inc(FBlockCount);
      var Lang := Match.Groups[1].Value;
      var Code := RemoveCommonIndentation(Match.Groups[2].Value);
      Code := HTMLEncode(Code);
      if Lang = 'delphi' then
        Lang := 'pascal';
      var LangId := Lang;
      if Lang = '' then
      begin
        Lang := '&nbsp';
        LangId := FDefaultLang;
      end;
      Result := Result + Format(CodeBlock, [
        Lang,
        SEdCmdCopy,
        FBlockCount.ToString,
        _('Copy code to new editor'),
        LangId,
        Code]);
      CodeEnd := Match.Index + Match.Length;
    end;
    var TextAfter := Copy(MarkDown, CodeEnd);
    if TextAfter <> '' then
      Result := Result + FMarkdownProcessor.process(TextAfter);
  end
  else
    Result := FMarkdownProcessor.process(MarkDown);

  if Result.StartsWith('<p>') then
    Delete(Result, 1, 3);
  // Escape for JavaScript template strings (within backticks)
  Result := Result.Replace('\', '\\');
  Result := Result.Replace('$', '\$');
  Result := Result.Replace('`', '\`');
end;

procedure TLLMChatForm.SetBrowserColorScheme;
var
  Profile: ICoreWebView2Profile;
  Scheme: COREWEBVIEW2_PREFERRED_COLOR_SCHEME;
begin
  if TFConfiguration.IsDark then
    Scheme := COREWEBVIEW2_PREFERRED_COLOR_SCHEME_DARK
  else
    Scheme := COREWEBVIEW2_PREFERRED_COLOR_SCHEME_LIGHT;
  (EdgeBrowser.DefaultInterface as ICoreWebView2_13).Get_Profile(Profile);
  Profile.Set_PreferredColorScheme(Scheme);
end;

procedure TLLMChatForm.SetQuestionTextHint;
begin
  var Validation := LLMChat.ValidateSettings;

  if Validation = svValid then begin
    FSynQuestion.Text := SQuestionHintValid;
    FSynQuestion.CaretX:= Length(SQuestionHintValid) + 1;
  end
  else
    FSynQuestion.Text := SQuestionHintInvalid + ': ' + LLMChat.ValidationErrMsg(Validation);
  FSynQuestion.Invalidate;
end;

procedure TLLMChatForm.StyleWebPage;
var
  LinkColor: TColor;
  TextColor: TColor;
  CodeHeaderBkg, CodeHeaderFg: string;
  ThumbColor, ThumbHoverColor: string;
begin
  if TFConfiguration.IsDark then
  begin
    LinkColor :=  TColors.Lightblue;
    CodeHeaderBkg := '#2d2d2d';
    CodeHeaderFg := '#f4f4f4';
    ThumbColor := '#666';
    ThumbHoverColor := '#888';
  end
  else
  begin
    LinkColor := clBlue;
    CodeHeaderBkg := '#f4f4f4';
    CodeHeaderFg := '#333';
    ThumbColor := '#ccc';
    ThumbHoverColor := '#999';
  end;

  // Style the main sheet
  MainStyleSheet := Format(MainStyleSheetTemplate, [
    ColorToHTML(StyleServices.GetSystemColor(clWindow)),
    ThumbColor,
    ThumbHoverColor,
    ColorToHTML(StyleServices.GetSystemColor(clWindowText)),
    ColorToHTML(LinkColor)]);


  // style the display of code to make it compatible with the PyScripter Editor
    if IsColorDark(FConfiguration.JavaHighlighter.WhitespaceAttribute.Background) then
      TextColor := DarkenColor(clWhite, 10)
    else
      TextColor := LightenColor(clBlack, 10);

    CodeStyleSheet := Format(CodeStyleSheetTemplate,[
        CodeHeaderBkg,
        CodeHeaderFg,
        ColorToHTML(FConfiguration.JavaHighlighter.WhitespaceAttribute.Background),
        ColorToHTML(FConfiguration.JavaHighlighter.CommentAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.SymbolAttri.Foreground),
        ColorToHTML(FConfiguration.HTMLHighlighter.SymbolAttri.Foreground),
        ColorToHTML(FConfiguration.HTMLHighlighter.SymbolAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.CommentAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.NumberAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.CommentAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.KeyAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.StringAttri.Foreground),
        ColorToHTML(FConfiguration.JavaHighlighter.CommentAttri.Foreground),
        ColorToHTML(FConfiguration.HTMLHighlighter.SymbolAttri.Foreground),
        ColorToHTML(TextColor)
      ]);
end;

procedure TLLMChatForm.synQuestionEnter(Sender: TObject);
begin
  // Spell Checking
  // CommandsDataModule.SynSpellCheck.Editor := synQuestion;
end;

procedure TLLMChatForm.WMDestroy(var Message: TWMDestroy);
begin
  // So that it is not automatically recreated
  EdgeBrowser.CloseWebView;
  FBrowserReady := False;
  pnlBrowserCover.Visible := True;
  inherited;
end;

procedure TLLMChatForm.WMSpSkinChange(var Message: TMessage);
begin
  inherited;
  ChangeStyle;
end;

procedure TLLMChatForm.ChangeStyle;
begin
  FSynQuestion.Font.Color := StyleServices.GetSystemColor(clWindowText);
  FSynQuestion.Color := StyleServices.GetSystemColor(clWindow);
  {$IF CompilerVersion >= 36}
  aiBusy.IndicatorColor := aicCustom;
  aiBusy.IndicatorCustomColor := StyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
  if TFConfiguration.IsDark then begin
    SpTBXToolbar.Images := vilImagesDark;
    sbAsk.Images := vilImagesDark;
  end
  else
  begin
    SpTBXToolbar.Images := vilImagesLight;
    sbAsk.Images := vilImagesLight;
  end;
end;

end.

