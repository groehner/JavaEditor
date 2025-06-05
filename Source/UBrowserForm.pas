unit UBrowserForm;

// http://www.swissdelphicenter.ch/de/tipsbycomp.php?component=Webbrowser

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  SHDocVw,
  System.ImageList,
  Vcl.ImgList,
  Vcl.OleCtrls,
  Vcl.ToolWin,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  UBaseForm;

{ for creation of SHDOCVw_TLB with Delphi/Project/Import Type Library
  install "Microsoft Internet Controls" or
  "Microsoft Browsers Helpers" anyway with TWebbrowser }

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

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CBUrlsClick(Sender: TObject);
    procedure CBUrlsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WebBrowser1BeforeNavigate2(ASender: TObject;
      const PDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserDownloadBegin(Sender: TObject);
    procedure WebBrowserDownloadComplete(Sender: TObject);
    procedure WebBrowser1DocumentComplete(ASender: TObject;
      const PDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowserOnCommandStateChange(Sender: TObject; Command: Integer;
      Enable: WordBool);

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
    FNewWindow: Boolean;
    FHistoryIndex: Integer;
    FHistoryList: TStringList;
    FServer: string;
    FZoom: OleVariant;
    procedure InvokeOleCMD(Value1, Value2: Integer);
    procedure ActivateBrowser;
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
    function GetFormType: string; override;
    function GetState: string; override;
    procedure SetState(var State: string); override;
    procedure SetFontSize(Delta: Integer); override;
    procedure UploadFilesHttpPost(const URLstring: string;
      Names, Values, NFiles, VFiles: array of string);
    procedure ChangeStyle; override;
    procedure DPIChanged; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Variants,
  ActiveX,
  Clipbrd,
  StrUtils,
  MSHTML,
  JvGnugettext,
  UStringRessources,
  UJava,
  UUtils,
  UEditorForm,
  UConfiguration,
  UJavaCommands,
  UWindow;

{$R *.DFM}

constructor TFBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 5;
end;

procedure TFBrowser.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  FServer := '';
  FHistoryIndex := -1;
  WebBrowser.Silent := True;
  OnMouseActivate := FormMouseActivate;
  FHistoryList := TStringList.Create;
  FConfiguration.ReadStrings('Browser', 'Urls', CBUrls.Items);
  ChangeStyle;
end;

procedure TFBrowser.Open(const Adresse: string; State: string);
begin
  Pathname := GetProtocolAndDomain(Adresse);
  Caption := Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  Enter(Self);
  FNewWindow := True;
  WebBrowser.OnCommandStateChange := WebBrowserOnCommandStateChange;
  WebBrowser.Navigate(Adresse);
  SetState(State);
end;

procedure TFBrowser.FormShow(Sender: TObject);
begin
  CBUrls.SelLength := 0;
  FJava.UpdateMenuItems(Self);
end;

procedure TFBrowser.ActivateBrowser;
begin
  if Assigned(WebBrowser) and Assigned(WebBrowser.Document) then
    (WebBrowser.Document as IHTMLDocument2).parentWindow.focus;
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
  FreeAndNil(FHistoryList);
  Action := caFree;
end;

procedure TFBrowser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  WebBrowser.OnCommandStateChange := nil;
  TThread.ForceQueue(nil,
    procedure
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
  var URL := CBUrls.Text;
  if FileExists(URL) then
    WebBrowser.Navigate(URL)
  else if DirectoryExists(ExtractFilePathEx(URL)) then
    FJava.NewExplorer(ExtractFilePathEx(URL), '')
  else if FConfiguration.BlockedInternet then
    WebBrowser.Navigate('about:' + _(LNGBlockedInternet))
  else begin
   if not (ContainsText(URL, 'http;//') or ContainsText(URL, 'https://')) then
   begin
     URL:= 'https://' + URL;
     CBUrls.Text:= URL;
    end;
    WebBrowser.Navigate(URL);
  end;
end;

procedure TFBrowser.CBUrlsKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if Key = VK_RETURN then
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
const PDisp: IDispatch; const URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  NewIndex, Posi: Integer;
  Str1, Str2, NUrl, Document: string;
  Replace: Boolean;
begin
  Replace := False;
  Str1 := URL;
  NUrl := URL;
  if Str1 = ExtractFilePath(FConfiguration.Javabook) + 'search.html' then
  begin
    Str1 := ToWeb(FConfiguration.BrowserProgram, Str1);
    MyJavaCommands.ExecWithoutWait(FConfiguration.BrowserProgram, Str1, '',
      SW_SHOWNORMAL);
    Cancel := True;
    Exit;
  end;
  // only cache documentation
  if IsHTTP(Str1) and (Pos(ExtractFilePathEx(FConfiguration.JavaManual), Str1)
    = 1) then
  begin
    Str1 := StripHttp(Str1);
    Posi := Pos('/', Str1);
    if Posi = 0 then
      FServer := Str1 + '/'
    else
    begin
      FServer := Copy(Str1, 1, Posi);
      Document := Copy(Str1, Posi + 1, Length(Str1));
    end;
    if Document = '' then
      Document := 'index.html';
    Str1 := ToWindows(FConfiguration.JavaCache + Document);
  end
  else
  begin
    if Pos('file:///', Str1) = 1 then
    begin
      Delete(Str1, 1, 8);
      Posi := Pos('#', Str1);
      if Posi > 0 then
      begin
        Str2 := Copy(Str1, Posi, Length(Str1));
        Delete(Str1, Posi, Length(Str1));
      end
      else
        Str2 := '';
      Str1 := ToWindows(Str1) + Str2;
      NUrl := Str1;
    end
    else if Pos('file:', Str1) = 1 then
    begin // UNC-Namen
      Delete(Str1, 1, 5);
      Posi := Pos('#', Str1);
      if Posi > 0 then
      begin
        Str2 := Copy(Str1, Posi, Length(Str1));
        Delete(Str1, Posi, Length(Str1));
      end
      else
        Str2 := '';
      Str1 := ToWindows(Str1) + Str2;
      NUrl := Str1;
    end;
  end;
  if (Pos(FConfiguration.JavaCache, Str1) = 1) and not FileExists(Str1) then
  begin
    Str2 := Copy(Str1, Length(FConfiguration.JavaCache) + 1, Length(Str1));
    if FServer = '' then
      FServer := GetServer(FConfiguration.JavaManual);
    Str1 := HttpToWeb('https://' + FServer + Str2);
    if FConfiguration.GlobalFileExists(Str1) then
      Replace := True
    else if FServer <> GetServer(FConfiguration.JavaManual) then
    begin
      FServer := GetServer(FConfiguration.JavaManual);
      Str1 := HttpToWeb('https://' + FServer + Str2);
      if FConfiguration.GlobalFileExists(Str1) then
        Replace := True;
    end;
  end;
  NewIndex := FHistoryList.IndexOf(URL);
  if NewIndex = -1 then
  begin
    if (FHistoryIndex >= 0) and (FHistoryIndex < FHistoryList.Count - 1) then
      while FHistoryList.Count > FHistoryIndex do
        FHistoryList.Delete(FHistoryIndex);
    if not HasJavaExtension(NUrl) then
      FHistoryIndex := FHistoryList.Add(NUrl);
  end
  else
    FHistoryIndex := NewIndex;
  if not HasJavaExtension(NUrl) or Replace then
    CBUrls.Text := NUrl;
  if IsHTTP(NUrl) and FConfiguration.BlockedInternet then
    Cancel := True;
end;

procedure TFBrowser.TBBackClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.GoBack;
end;

procedure TFBrowser.TBForwardClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.GoForward;
end;

procedure TFBrowser.TBRefreshClick(Sender: TObject);
begin
  if not WebBrowser.Busy then
    WebBrowser.Refresh;
end;

procedure TFBrowser.TBStopClick(Sender: TObject);
begin
  WebBrowser.Stop;
end;

procedure TFBrowser.WebBrowserDownloadBegin(Sender: TObject);
begin
  TBStop.ImageName := 'AbortOn';
end;

procedure TFBrowser.WebBrowserOnCommandStateChange(Sender: TObject;
Command: Integer; Enable: WordBool);
begin
  case Command of
    CSC_NAVIGATEBACK:
      TBBack.Enabled := Enable;
    CSC_NAVIGATEFORWARD:
      TBForward.Enabled := Enable;
  end;
end;

procedure TFBrowser.WebBrowserDownloadComplete(Sender: TObject);
begin
  TBStop.ImageName := 'AbortOff';
  if FNewWindow then
    FNewWindow := False
  else
    FJava.RenameTabAndWindow(Number, CBUrls.Text);
  Pathname := GetProtocolAndDomain(CBUrls.Text);
  Caption := Pathname;
  if not FConfiguration.Visible then
    ActivateBrowser;
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
  CBUrls.Text := '';
end;

procedure TFBrowser.UpdateState;
var
  CMDF: OLECMDF;
  MICu, MICo, MIPa: Boolean;
begin
  // This approach prevents the menu from flickering when the timer is active
  inherited;
  MICo := False;
  if CBUrls.Focused then
  begin
    MICu := (CBUrls.SelLength > 0);
    MICo := (CBUrls.SelLength > 0);
    try
      MIPa := Clipboard.HasFormat(CF_TEXT) and (Clipboard.AsText <> '');
    except
      MIPa := False;
    end;
  end
  else
  begin
    try
      CMDF := WebBrowser.QueryStatusWB(OLECMDID_COPY);
      MICo := ((OLECMDF_ENABLED and CMDF) <> 0);
    except
      on E: Exception do
        OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
          E.Message));
    end;
    try
      WebBrowser.QueryStatusWB($FFFFFFFF { -1 } );
    except
      on E: Exception do
        OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
          E.Message));
    end;
    MICu := False;
    MIPa := False;
  end;

  with FJava do
  begin
    SetEnabledMI(MICut, MICu);
    SetEnabledMI(MICopy, MICo);
    SetEnabledMI(MIPaste, MIPa);
  end;
end;

procedure TFBrowser.InvokeOleCMD(Value1, Value2: Integer);
var
  CmdTarget: IOleCommandTarget;
  VaIn, VaOut: OleVariant;
begin
  with WebBrowser do
  begin
    if Document <> nil then
      try
        Document.QueryInterface(IOleCommandTarget, CmdTarget);
        if Assigned(CmdTarget) then
          try
            CmdTarget.Exec(PGUID(nil), Value1, Value2, VaIn, VaOut);
          finally
            CmdTarget._Release;
          end;
      except
        on E: Exception do
          OutputDebugString(PChar('Exception: ' + E.ClassName + ' - ' +
            E.Message));
      end;
  end;
end;

procedure TFBrowser.BrowserCopy;
begin
  InvokeOleCMD(OLECMDID_COPY, OLECMDEXECOPT_DODEFAULT);
end;

procedure TFBrowser.Search;
begin
  InvokeOleCMD(OLECMDID_FIND, 0);
end;

procedure TFBrowser.Print;
begin
  InvokeOleCMD(OLECMDID_PRINT, OLECMDEXECOPT_PROMPTUSER);
end;

procedure TFBrowser.PasteFromClipboard;
begin
  if CBUrls.Focused then
    CBUrls.SelText := Clipboard.AsText;
end;

procedure TFBrowser.CutToClipboard;
begin
  if CBUrls.Focused then
  begin
    Clipboard.AsText := CBUrls.SelText;
    CBUrls.SelText := '';
  end;
end;

procedure TFBrowser.CopyToClipboard;
begin
  if CBUrls.Focused then
    Clipboard.AsText := CBUrls.SelText
  else
    BrowserCopy;
end;

procedure TFBrowser.SplitterMoved(Sender: TObject);
begin
  FConfiguration.WriteIntegerU('Browser', 'CBWidth', PLeft.Width);
end;

function TFBrowser.GetFormType: string;
begin
  Result := '%B%';
end;

procedure TFBrowser.TBShowSourceClick(Sender: TObject);
var
  URL: string;
  Editor: TFEditForm;
begin
  TBShowSource.Enabled := False;
  URL := StripHttpParams(CBUrls.Text);
  if FileExists(URL) then
    if FJava.GetTDIWindowType(URL, '%E%') = nil then
    begin
      Editor := TFEditForm(FJava.FormFactory(fkEditor));
      Editor.Open(URL, '');
    end
    else
      FJava.Open(URL)
  else
  begin
    URL := ExtractFileNameEx(CBUrls.Text);
    if not IsHTML(URL) then
      URL := ChangeFileExt(URL, '.html');
    try
      Screen.Cursor := crHourGlass;
      if DownloadURL(CBUrls.Text, FConfiguration.TempDir + URL) then
        FJava.NewEditor(FConfiguration.TempDir + URL, '')
      else
        ErrorMsg(_(LNGDownloadError));
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  TBShowSource.Enabled := True;
end;

function TFBrowser.GetState: string;
begin
  Result := inherited GetState + 'W' + IntToStr(PLeft.Width) + ')';
end;

procedure TFBrowser.SetState(var State: string);
begin
  inherited SetState(State);
  if Copy(State, 1, 1) = 'W' then
    PLeft.Width := StrToInt(Copy(State, 2, Pos(')', State) - 2))
  else
    PLeft.Width := FConfiguration.ReadIntegerU('Browser', 'CBWidth', 200);
end;

procedure TFBrowser.BrowserTimerTimer(Sender: TObject);
begin
  UpdateState;
end;

procedure TFBrowser.WebBrowser1DocumentComplete(ASender: TObject;
const PDisp: IDispatch; const URL: OleVariant);
begin
  if FJava.Search then
  begin
    FJava.Search := False;
    Search;
    // When not waiting for the search window
    // then the search text doesn't end up there
    // use ProcessMessages to wait
    var
    Window := FWindow.GetWindowHandle(_('Search'));
    while Window = 0 do
    begin
      Application.ProcessMessages;
      Window := FWindow.GetWindowHandle(_('Search'));
    end;
    SendKeysGlobal(FJava.Searched);
  end;
end;

procedure TFBrowser.SetFontSize(Delta: Integer);
var
  Zoom1: OleVariant;
begin
  WebBrowser.ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_DONTPROMPTUSER, Zoom1, FZoom);
  FZoom := FZoom + Delta;
  if FZoom < 0 then
    FZoom := 0;
  if FZoom > 4 then
    FZoom := 4;
  WebBrowser.ExecWB(OLECMDID_ZOOM, OLECMDEXECOPT_PROMPTUSER, FZoom);
end;

procedure TFBrowser.UploadFilesHttpPost(const URLstring: string;
Names, Values, NFiles, VFiles: array of string);
var
  StrData, StrN, StrV, Boundary: string;
  URL: OleVariant;
  Flags: OleVariant;
  PostData: OleVariant;
  Headers: OleVariant;
  Idx: Integer;
  MemoryStream: TMemoryStream;
  StringStream: TStringStream;
begin
  if Length(Names) <> Length(Values) then
    raise Exception.Create
      ('UploadFilesHttpPost: Names and Values must have the same Length.');
  if Length(NFiles) <> Length(VFiles) then
    raise Exception.Create
      ('UploadFilesHttpPost: FileNames and FileValues must have the same Length.');
  URL := 'about:blank';
  Flags := navNoHistory or navNoReadFromCache or navNoWriteToCache or
    navAllowAutosearch;
  WebBrowser.Navigate2(URL, Flags);
  while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;
  // anything random that WILL NOT occur in the data.
  Boundary := '---------------------------123456789';
  StrData := '';
  for Idx := Low(Names) to High(Names) do
  begin
    StrN := Names[Idx];
    StrV := Values[Idx];
    StrData := StrData + '--' + Boundary + #13#10 +
      'Content-Disposition: form-data; name="' + StrN + '"' + #13#10#13#10 +
      StrV + #13#10;
  end;

  for Idx := Low(NFiles) to High(NFiles) do
  begin
    StrN := NFiles[Idx];
    StrV := VFiles[Idx];
    StrData := StrData + '--' + Boundary + #13#10 +
      'Content-Disposition: form-data; name="' + StrN + '"; filename="' + StrV +
      '"' + #13#10;
    if StrV = '' then
      StrData := StrData + 'Content-Transfer-Encoding: binary'#13#10#13#10
    else
    begin
      if (CompareText(ExtractFileExt(StrV), '.JPG') = 0) or
        (CompareText(ExtractFileExt(StrV), '.JPEG') = 0) then
        StrData := StrData + 'Content-Type: image/pjpeg'#13#10#13#10
      else if (CompareText(ExtractFileExt(StrV), '.PNG') = 0) then
        StrData := StrData + 'Content-Type: image/x-png'#13#10#13#10
      else if (CompareText(ExtractFileExt(StrV), '.PNG') = 0) then
        StrData := StrData + 'Content-Type: image/x-png'#13#10#13#10
      else if (CompareText(ExtractFileExt(StrV), '.CSS') = 0) then
        StrData := StrData + 'Content-Type: text/css'#13#10#13#10
      else if (CompareText(ExtractFileExt(StrV), '.HTML') = 0) then
        StrData := StrData + 'Content-Type: text/html'#13#10#13#10;
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.LoadFromFile(StrV);
        StringStream := TStringStream.Create('');
        try
          StringStream.CopyFrom(MemoryStream, MemoryStream.Size);
          StrData := StrData + StringStream.DataString + #13#10;
        finally
          FreeAndNil(StringStream);
        end;
      finally
        FreeAndNil(MemoryStream);
      end;
    end;
    StrData := StrData + '--' + Boundary + '--'#13#10; // FOOTER
  end;
  StrData := StrData + #0;
  { 2. you must convert a string into variant array of bytes and every character from string is a value in array }
  PostData := VarArrayCreate([0, Length(StrData) - 1], varByte);
  { copy the ordinal value of the character into the PostData array }
  for Idx := 1 to Length(StrData) do
    PostData[Idx - 1] := Ord(StrData[Idx]);
  { 3. prepare headers which will be sent to remote web-FServer }
  Headers := 'Content-Type: multipart/form-data; Boundary=' + Boundary + #13#10;
  { 4. you must navigate to the URL with your script and send as parameters your array with POST-data and headers }
  URL := URLstring;
  WebBrowser.Navigate2(URL, Flags, EmptyParam, PostData, Headers);
  while WebBrowser.ReadyState < READYSTATE_INTERACTIVE do
    Application.ProcessMessages;
end;

procedure TFBrowser.ChangeStyle;
begin
  if FConfiguration.IsDark then
    ToolBar.Images := vilBrowserDark
  else
    ToolBar.Images := vilBrowserLight;
end;

procedure TFBrowser.DPIChanged;
begin
  CBUrls.Height := ToolBar.Height;
end;

initialization

OleInitialize(nil);

finalization

OleUninitialize;

end.
