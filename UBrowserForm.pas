unit UBrowserForm;

// http://www.swissdelphicenter.ch/de/tipsbycomp.php?component=Webbrowser

interface

uses
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, UBaseForm, SHDocVw,
  System.ImageList, Vcl.ImgList, Vcl.OleCtrls, Vcl.ToolWin,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, SVGIconImageCollection;

  { for creation of SHDOCVw_TLB with Delphi/Project/Import Type Library
    install "Microsoft Internet Controls" or
    "Microsoft Browsers Helpers" anyway with TWebbrowser }

{type
  TWebBrowser = class(SHDocVw.TWebBrowser, IOleCommandTarget)
  private
    function QueryStatus(CmdGroup: PGUID; cCmds: Cardinal; prgCmds: POleCmd;
      CmdText: POleCmdText): HRESULT; stdcall;
    function Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
      const vaIn: OleVariant; var vaOut: OleVariant): HRESULT; stdcall;
  end;}

type
  TFBrowser = class(TFForm)
    WebBrowser: TWebBrowser;
    PBrowser: TPanel;
    CBUrls: TComboBox;
    ToolBar: TToolBar;
    TBBack: TToolButton;
    TBForward: TToolButton;
    TBStop: TToolButton;
    TBRefresh: TToolButton;
    TBClose: TToolButton;
    TBFavoritesAdd: TToolButton;
    PTop: TPanel;
    PLeft: TPanel;
    PRight: TPanel;
    Splitter: TSplitter;
    TBShowSource: TToolButton;
    TBFavoritesDelete: TToolButton;
    icBrowser: TSVGIconImageCollection;
    vilBrowserLight: TVirtualImageList;
    vilBrowserDark: TVirtualImageList;

    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBUrlsClick(Sender: TObject);
    procedure CBUrlsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure WebBrowser1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserDownloadBegin(Sender: TObject);
    procedure WebBrowserDownloadComplete(Sender: TObject);
    procedure WebBrowser1DocumentComplete(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowserOnCommandStateChange(Sender: TObject;
      Command: Integer; Enable: WordBool);

    procedure TBCloseClick(Sender: TObject);
    procedure TBShowSourceClick(Sender: TObject);
    procedure TBBackClick(Sender: TObject);
    procedure TBForwardClick(Sender: TObject);
    procedure TBStopClick(Sender: TObject);
    procedure TBRefreshClick(Sender: TObject);
    procedure TBFavoritesAddClick(Sender: TObject);
    procedure TBFavoritesDeleteClick(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure BrowserTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;
  private
    NewWindow: boolean;
    HistoryIndex: Integer;
    HistoryList: TStringList;
    Server: string;
    Zoom: OleVariant;
    procedure InvokeOleCMD (Value1, Value2: Integer);
    procedure ActivateBrowser;  public
    procedure BrowserCopy;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(const Adresse: string; State: string);
    procedure OpenWindow(Sender: TObject); override;
    procedure UpdateState; override;
    procedure Search; override;
    procedure Print; override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    function getFormType: string; override;
    function getState: string; override;
    procedure setState(var s: string); override;
    procedure SetFontSize(Delta: integer); override;
    procedure UploadFilesHttpPost(const URLstring: string; names, values, nFiles, vFiles: array of string);
    procedure ChangeStyle; override;
    procedure DPIChanged; override;
  end;

implementation

uses Windows, SysUtils, Variants, ActiveX, Clipbrd, MSHTML, JvGnugettext,
     UStringRessources, UJava, UUtils, UEditorForm, UConfiguration,
     UJavaCommands, UWindow;

{$R *.DFM}

{

function TWebBrowser.QueryStatus(CmdGroup: PGUID; cCmds: Cardinal;
  prgCmds: POleCmd; CmdText: POleCmdText): HRESULT; stdcall;
begin
  // MSDN notes that a command target must implement this function; E_NOTIMPL is not a
  // valid return value. Be careful to return S_OK, because we notice that context menu
  // of Web page "Add to Favorites..." becomes disabled. Another MSDN document shows an
  // example with default return value OLECMDERR_E_NOTSUPPORTED.
  // http://msdn.microsoft.com/en-us/library/bb165923(v=vs.80).aspx
  Result := OLECMDERR_E_NOTSUPPORTED;
end;

function TWebBrowser.Exec(CmdGroup: PGUID; nCmdID, nCmdexecopt: DWORD;
  const vaIn: OleVariant; var vaOut: OleVariant): HRESULT; stdcall;
  const
  CGID_DocHostCommandHandler: TGUID = (D1: $F38BC242; D2: $B950; D3: $11D1; D4:
    ($89, $18, $00, $C0, $4F, $C2, $C8, $36));

var
  ShowDialog, InterpretScript: Boolean;
begin
  if CmdGroup = nil then
  begin
    Result := OLECMDERR_E_UNKNOWNGROUP;
    Exit;
  end;

  // MSDN notes that a command target must implement this function; E_NOTIMPL is not a
  // valid return value. Be careful to return S_OK, because we notice some unhandled
  // commands behave unexpected with S_OK. We assumed that a return value
  // OLECMDERR_E_NOTSUPPORTED means to use the default behavior.
  Result := OLECMDERR_E_NOTSUPPORTED;

  if IsEqualGUID(CmdGroup^, CGID_DocHostCommandHandler) then
  begin
    // there's a script error in the currently executed script, so
    if nCmdID = OLECMDID_SHOWSCRIPTERROR then
    begin
      ShowDialog := False;
      InterpretScript := False;

      // Implements an event if you want, so that your application is able to choose the way of handling script errors at runtime.
      //if Assigned(OnNotifyScriptError) then
      //  OnNotifyScriptError(Self, ShowDialog, InterpretScript);

      if ShowDialog then
        Result := S_FALSE
      else
        Result := S_OK;
      vaOut := InterpretScript; // Without setting the variable to true, further script execution will be cancelled.
    end;
  end;
end;    }

constructor TFBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 5;
end;

procedure TFBrowser.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  Server:= '';
  HistoryIndex:= -1;
  WebBrowser.Silent:= true;
  OnMouseActivate:= FormMouseActivate;
  HistoryList:= TStringList.Create;
  FConfiguration.ReadStrings('Browser', 'Urls', CBUrls.Items);
  ChangeStyle;
end;

procedure TFBrowser.Open(const Adresse: string; State: string);
begin
  Pathname:= getProtocolAndDomain(Adresse);
  Caption:= Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  Enter(Self);
  NewWindow:= true;
  Webbrowser.OnCommandStateChange:= WebBrowserOnCommandStateChange;
  WebBrowser.Navigate(Adresse);
  SetState(State);
end;

procedure TFBrowser.FormShow(Sender: TObject);
begin
  CBUrls.SelLength:= 0;
  FJava.UpdateMenuItems(Self);
end;

procedure TFBrowser.ActivateBrowser;
begin
  if assigned(WebBrowser) and assigned(WebBrowser.Document) then
    (WebBrowser.Document as IHTMLDocument2).ParentWindow.Focus;
end;

procedure TFBrowser.OpenWindow(Sender: TObject);
begin
  inherited;
  WebBrowser.Update;
  ActivateBrowser;
end;

procedure TFBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeAndNil(HistoryList);
  Action:= caFree;
end;

procedure TFBrowser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Webbrowser.OnCommandStateChange:= nil;
  TThread.ForceQueue(nil, procedure
  begin
    FJava.CloseBrowser;
  end);
end;

procedure TFBrowser.TBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFBrowser.CBUrlsClick(Sender: TObject);
begin
  var s:= CBUrls.Text;
  if FileExists(s) then
    WebBrowser.Navigate(s)
  else if DirectoryExists(ExtractFilePathEx(s))
    then FJava.NewExplorer(ExtractFilePathEx(s), '')
  else if FConfiguration.BlockedInternet then
    WebBrowser.Navigate('about:' + _(LNGBlockedInternet))
  else
    WebBrowser.Navigate(s);
end;

procedure TFBrowser.CBUrlsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Return then
    CBUrlsClick(Self);
end;

procedure TFBrowser.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssAlt] then
    if (Key = VK_RIGHT) and TBForward.Enabled then
      TBForward.Click
    else if (Key = VK_LEFT) and TBBack.Enabled then
      TBBack.Click;
end;

procedure TFBrowser.WebBrowser1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  NewIndex: Integer; p: Integer; s, s1, nURL, document: string; replace: boolean;
begin
  replace:= false;
  s:= URL;
  nURL:= URL;
  if s = ExtractFilePath(FConfiguration.Javabook) + 'search.html' then begin
    s:= toWeb(FConfiguration.BrowserProgram, s);
    myJavaCommands.ExecWithoutWait(FConfiguration.BrowserProgram, s, '', SW_SHOWNORMAL);
    Cancel:= true;
    exit;
  end;
  // only cache documentation
  if IsHttp(s) and (Pos(ExtractFilePathEx(FConfiguration.JavaManual), s) = 1) then begin
    s:= StripHttp(s);
    p:= Pos('/', s);
    if p = 0 then
      Server:= s + '/'
    else begin
      Server:= Copy(s, 1, p);
      document:= Copy(s, p+1, length(s));
    end;
    if document = '' then document:= 'index.html';
    s:= toWindows(FConfiguration.JavaCache + document);
  end else begin
    if Pos('file:///', s) = 1 then begin
      delete(s, 1, 8);
      p:= Pos('#', s);
      if p > 0 then begin
        s1:= copy(s, p, length(s));
        delete(s, p, length(s));
      end else s1:= '';
      s:= ToWindows(s) + s1;
      nURL:= s;
    end else if Pos('file:', s) = 1 then begin // UNC-Namen
      delete(s, 1, 5);
      p:= Pos('#', s);
      if p > 0 then begin
        s1:= copy(s, p, length(s));
        delete(s, p, length(s));
      end else s1:= '';
      s:= ToWindows(s) + s1;
      nURL:= s;
    end;
  end;
  if (Pos(FConfiguration.JavaCache, s) = 1) and not FileExists(s) then begin
    s1:= copy(s, length(FConfiguration.JavaCache) + 1, length(s));
    if Server = '' then Server:= getServer(FConfiguration.JavaManual);
    s:= HttpToWeb('https://' + Server + s1);
    if FConfiguration.GlobalFileExists(s) then
      replace:= true
    else if Server <> getServer(FConfiguration.JavaManual) then begin
      Server:= getServer(FConfiguration.JavaManual);
      s:= HttpToWeb('https://' + Server + s1);
      if FConfiguration.GlobalFileExists(s) then replace:= true;
    end;
  end;
  NewIndex := HistoryList.IndexOf(URL);
  if NewIndex = -1 then begin
    if (HistoryIndex >= 0) and (HistoryIndex < HistoryList.Count - 1) then
      while HistoryList.Count > HistoryIndex do
        HistoryList.Delete(HistoryIndex);
    if not hasJavaExtension(nURL) then
      HistoryIndex := HistoryList.Add(nURL);
  end else
    HistoryIndex := NewIndex;
  if not hasJavaExtension(nURL) or replace then
    CBURLs.Text:= nURL;
  if isHttp(nURL) and FConfiguration.BlockedInternet then
    Cancel:= true;
end;

procedure TFBrowser.TBBackClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.GoBack;
end;

procedure TFBrowser.TBForwardClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.GoForward;
end;

procedure TFBrowser.TBRefreshClick(Sender: TObject);
begin
  if not WebBrowser.Busy
    then WebBrowser.Refresh;
end;

procedure TFBrowser.TBStopClick(Sender: TObject);
begin
  WebBrowser.Stop;
end;

procedure TFBrowser.WebBrowserDownloadBegin(Sender: TObject);
begin
  TBStop.ImageName:= 'AbortOn';
end;

procedure TFBrowser.WebBrowserOnCommandStateChange(Sender: TObject;
  Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK:    TBBack.Enabled := Enable;
    CSC_NAVIGATEFORWARD: TBForward.Enabled := Enable;
  end;
end;

procedure TFBrowser.WebBrowserDownloadComplete(Sender: TObject);
begin
  TBStop.ImageName:= 'AbortOff';
  if NewWindow
    then NewWindow:= false
    else FJava.RenameTabAndWindow(Number, CBUrls.Text);
  Pathname:= getProtocolAndDomain(CBUrls.Text);
  Caption:= Pathname;
  if not FConfiguration.visible then ActivateBrowser;
end;

procedure TFBrowser.TBFavoritesAddClick(Sender: TObject);
begin
  ComboBoxAdd(CBUrls);
  FConfiguration.SaveStrings('Browser', 'Urls', CBUrls.Items);
end;

procedure TFBrowser.TBFavoritesDeleteClick(Sender: TObject);
begin
  ComboBoxDelete2(CBUrls, CBUrls.Text);
  FConfiguration.SaveStrings('Browser', 'Urls', CBUrls.Items);
  CBUrls.Text:= '';
end;

procedure TFBrowser.UpdateState;
  var CMDF: OLECMDF;
      MICu, MICo, MIPa: boolean;
begin
  // This approach prevents the menu from flickering when the timer is active
  inherited;
  MICo:= false;
  if CBUrls.Focused then begin
    MICu:= (CBUrls.SelLength > 0);
    MICo:= (CBUrls.SelLength > 0);
    try
      MIPa:= Clipboard.HasFormat(CF_Text) and (Clipboard.AsText <> '');
    except
      MIPa:= false;
    end;
  end else begin
    try
      CMDF:= WebBrowser.QueryStatusWB(OLECMDID_COPY);
      MICo:= ((OLECMDF_ENABLED and CMDF) <> 0);
    except
    end;
    try
      WebBrowser.QueryStatusWB($FFFFFFFF {-1});
    except;
    end;
    MICu:= false;
    MIPa:= false;
  end;

  with FJava do begin
    SetEnabledMI(MICut, MICu);
    SetEnabledMI(MICopy, MICo);
    SetEnabledMI(MIPaste, MIPa);
  end;
end;

procedure TFBrowser.InvokeOleCMD (Value1, Value2: Integer);
var CmdTarget:   IOleCommandTarget;
    vaIn, vaOut: OleVariant;
begin
  with WebBrowser do begin
    if Document <> nil then
      try
        Document.QueryInterface (IOleCommandTarget, CmdTarget);
        if CmdTarget <> nil then
          try
            CmdTarget.Exec(PGuid (nil), Value1, Value2, VaIn, VaOut);
          finally
            CmdTarget._Release;
          end;
      except
        // Nothing
      end;
  end;
end;

procedure TFBrowser.BrowserCopy;
begin
  InvokeOleCMD(OLECMDID_Copy, OLECMDEXECOPT_DODEFAULT);
end;

procedure TFBrowser.Search;
begin
  InvokeOleCmd(OLECMDID_Find, 0);
end;

procedure TFBrowser.Print;
begin
  InvokeOleCMD(OLECMDID_PRINT, OLECMDEXECOPT_PROMPTUSER);
end;

procedure TFBrowser.PasteFromClipboard;
begin
  if CBUrls.Focused then
    CBUrls.SelText:= Clipboard.AsText;
end;

procedure TFBrowser.CutToClipboard;
begin
  if CBUrls.Focused then begin
    Clipboard.AsText:= CBUrls.SelText;
    CBUrls.SelText:= '';
  end;
end;

procedure TFBrowser.CopyToClipboard;
begin
  if CBUrls.Focused
    then Clipboard.AsText:= CBUrls.SelText
    else BrowserCopy;
end;

procedure TFBrowser.SplitterMoved(Sender: TObject);
begin
  FConfiguration.WriteIntegerU('Browser', 'CBWidth', PLeft.Width);
end;

function TFBrowser.getFormType: string;
begin
  Result:= '%B%';
end;

procedure TFBrowser.TBShowSourceClick(Sender: TObject);
  var s: string; Editor: TFEditForm;
begin
  TBShowSource.Enabled:= false;
  s:= StripHttpParams(CBUrls.Text);
  if FileExists(s) then 
    if FJava.getTDIWindowType(s, '%E%') = nil then begin
      Editor:= TFEditForm(FJava.FormFactory(fkEditor));
      Editor.Open(s, '');
    end else
      FJava.Open(s)
  else begin
    s:= ExtractFileNameEx(CBUrls.Text);
    if not IsHTML(s) then s:= ChangeFileExt(s, '.html');
    try
      Screen.Cursor:= crHourglass;
      if DownloadURL(CBUrls.Text, FConfiguration.TempDir + s)
        then FJava.NewEditor(FConfiguration.TempDir + s, '')
        else ErrorMsg(_(LNGDownloadError));
    finally
      Screen.Cursor:= crDefault;
    end;
  end;
  TBShowSource.Enabled:= true;
end;

function TFBrowser.getState: string;
begin
  Result:= inherited getState + 'W' + IntToStr(PLeft.Width) + ')';
end;

procedure TFBrowser.setState(var s: string);
begin
  inherited SetState(s);
  if copy(s, 1, 1) = 'W'
    then PLeft.Width:= StrToInt(Copy(s, 2, Pos(')', s) - 2))
    else PLeft.Width:= FConfiguration.ReadIntegerU('Browser', 'CBWidth', 200);
end;

procedure TFBrowser.BrowserTimerTimer(Sender: TObject);
begin
  UpdateState;
end;

procedure TFBrowser.WebBrowser1DocumentComplete(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  inherited;
  if FJava.Search then begin
    FJava.Search:= false;
    Search;
    // When not waiting for the search window
    // then the search text doesn't end up there
    // use ProcessMessages to wait
    var Window:= FWindow.GetWindowHandle(_('Search'));
    while Window = 0 do begin
      Application.ProcessMessages;
      Window:= FWindow.GetWindowHandle(_('Search'));
    end;
    SendKeysGlobal(FJava.Searched);
  end;
end;

procedure TFBrowser.SetFontSize(Delta: integer);
  var Zoom1: OleVariant;
begin
  WebBrowser.ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, Zoom1, Zoom);
  Zoom:= Zoom + Delta;
  if Zoom < 0 then Zoom:= 0;
  if Zoom > 4 then Zoom:= 4;
  WebBrowser.ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_PROMPTUSER, Zoom);
end;

procedure TFBrowser.UploadFilesHttpPost(const URLstring: string; names, values, nFiles, vFiles: array of string) ;
 var
   strData, n, v, boundary: string;
   URL: OleVariant;
   Flags: OleVariant;
   PostData: OleVariant;
   Headers: OleVariant;
   idx: Integer;
   ms: TMemoryStream;
   ss: TStringStream;
 begin
   if Length(names) <> Length(values) then
     raise Exception.Create('UploadFilesHttpPost: Names and Values must have the same length.') ;
   if Length(nFiles) <> Length(vFiles) then
     raise Exception.Create('UploadFilesHttpPost: FileNames and FileValues must have the same length.') ;
 
   URL := 'about:blank';
   Flags := NavNoHistory or NavNoReadFromCache or NavNoWriteToCache or NavAllowAutosearch;
   WebBrowser.Navigate2(URL, Flags) ;
   while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do Application.ProcessMessages;
 
   // anything random that WILL NOT occur in the data.
   boundary := '---------------------------123456789';
 
   strData := '';
   for idx := Low(names) to High(names) do
   begin
     n := names[idx];
     v := values[idx];
     strData := strData + '--' + boundary + #13#10 + 'Content-Disposition: form-data; name="' + n + '"' + #13#10#13#10 + v + #13#10;
   end;
 
   for idx := Low(nFiles) to High(nFiles) do
   begin
     n := nFiles[idx];
     v := vFiles[idx];
     strData := strData + '--' + boundary + #13#10 + 'Content-Disposition: form-data; name="' + n + '"; filename="' + v + '"' + #13#10;
     if v = '' then
       strData := strData + 'Content-Transfer-Encoding: binary'#13#10#13#10
     else
     begin
       if (CompareText(ExtractFileExt(v), '.JPG') = 0) or (CompareText(ExtractFileExt(v), '.JPEG') = 0) then
       begin
         strData := strData + 'Content-Type: image/pjpeg'#13#10#13#10;
       end
       else if (CompareText(ExtractFileExt(v), '.PNG') = 0) then
       begin
         strData := strData + 'Content-Type: image/x-png'#13#10#13#10;
       end
       else if (CompareText(ExtractFileExt(v), '.PNG') = 0) then
       begin
         strData := strData + 'Content-Type: image/x-png'#13#10#13#10;
       end
       else if (CompareText(ExtractFileExt(v), '.CSS') = 0) then
       begin
         strData := strData + 'Content-Type: text/css'#13#10#13#10;
       end
       else if (CompareText(ExtractFileExt(v), '.HTML') = 0) then
       begin
       strData := strData + 'Content-Type: text/html'#13#10#13#10;
       end;
       ms := TMemoryStream.Create;
       try
         ms.LoadFromFile(v) ;
         ss := TStringStream.Create('') ;
         try
           ss.CopyFrom(ms, ms.Size) ;
           strData := strData + ss.DataString + #13#10;
         finally
           FreeAndNil(ss);
         end;
       finally
         FreeAndNil(ms);
       end;
     end;
 
     strData := strData + '--' + boundary + '--'#13#10; // FOOTER
   end;
 
   strData := strData + #0;
 
   {2. you must convert a string into variant array of bytes and every character from string is a value in array}
   PostData := VarArrayCreate([0, Length(strData) - 1], varByte) ;
 
   { copy the ordinal value of the character into the PostData array}
   for idx := 1 to Length(strData) do PostData[idx-1] := Ord(strData[idx]) ;
 
   {3. prepare headers which will be sent to remote web-server}
   Headers := 'Content-Type: multipart/form-data; boundary=' + boundary + #13#10;
 
   {4. you must navigate to the URL with your script and send as parameters your array with POST-data and headers}
   URL := URLstring;
   WebBrowser.Navigate2(URL, Flags, EmptyParam, PostData, Headers) ;
   while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do Application.ProcessMessages;
 end;

procedure TFBrowser.ChangeStyle;
begin
  if FConfiguration.isDark
    then ToolBar.Images:= vilBrowserDark
    else ToolBar.Images:= vilBrowserLight;
end;

procedure TFBrowser.DPIChanged;
begin
  CBUrls.Height:= Toolbar.Height;
end;

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;
end.
