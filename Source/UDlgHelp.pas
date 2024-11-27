unit UDlgHelp;

interface

uses
  Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGIconImageCollection;

type
  TFHelpDialog = class(TForm)
    PCJDKHelp: TPageControl;
    TVAPITree: TTreeView;
    TVGuideTree: TTreeView;
    TVToolsTree: TTreeView;

    TSApi: TTabSheet;
    TSDocu: TTabSheet;
    TSTools: TTabSheet;
    BClose: TButton;
    BShow: TButton;
    TSSearch: TTabSheet;
    LSearch: TLabel;
    ESearch: TEdit;
    BSearch: TButton;
    LFiles: TLabel;
    LBFiles: TListBox;
    RGSearchOptions: TRadioGroup;
    TSFavorites: TTabSheet;
    LBFavorites: TListBox;
    BDelete: TButton;
    BAddFavorit: TButton;
    BEdit: TButton;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    BNew: TButton;
    icJDKHelp: TSVGIconImageCollection;
    vilJDKHelpLight: TVirtualImageList;
    vilJDKHelpDark: TVirtualImageList;
    procedure TVTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TVTreeDblClick(Sender: TObject);
    procedure TVTreeCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TVTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure BCloseClick(Sender: TObject);
    procedure BShowClick(Sender: TObject);
    procedure BSearchClick(Sender: TObject);
    procedure LBFilesDblClick(Sender: TObject);
    procedure ESearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PCJDKHelpChange(Sender: TObject);
    procedure LBFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RGSearchOptionsClick(Sender: TObject);
    procedure ESearchKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure BAddFavoritClick(Sender: TObject);
    procedure LBFavoritesDblClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    lastSite: string;
    sites: Integer;
    PerformSearch, ready: boolean;
    Favorites: TstringList;  // format "Identifier | URL"
    procedure Investigate(FoundFiles: TstringList; const sought: string);
    procedure SearchFiles(Directory: string; const Sought: string);
    procedure ReadDirectories(TVTree: TTreeView; Node: TTreeNode; const directory: string);
    function getPath(i: integer): string;
    function getFileFromTree(Sender: TObject): string;
    function getClassName(sought: string): string;
    function getTitle(s: string): string;
    function NoChar(ch: Char): boolean;
    function searchIndex(const sought: string): string;
    procedure GlobalSearch(const sought: string);
    procedure ShowFavorites;
    procedure NewFavorit(const Identifier: string; URL: string);
    function HttpToCache(s: string): string;
    procedure ChangeStyle;
  end;

implementation

uses Windows, SysUtils, Clipbrd, Character, JvGnugettext,
     UJava, UTree, UConfiguration, UDlgFavoriten, UUtils;

{$R *.DFM}

procedure TFHelpDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  Favorites:= TStringList.Create;
  RGSearchOptions.ItemIndex := FConfiguration.ReadIntegerU('Program', 'Searchoptions', 0);
  PCJDKHelp.ActivePageIndex:= FConfiguration.ReadIntegerU('Program', 'JDK-Help-Page', 0);
  FConfiguration.ReadFavorites(Favorites);
  ShowFavorites;
  ReadDirectories(TVAPITree, nil, getPath(2));
  ReadDirectories(TVGuideTree, nil, getPath(3));
  ReadDirectories(TVToolsTree, nil, getPath(4));
  ChangeStyle;
end;

procedure TFHelpDialog.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(Favorites);
end;

procedure TFHelpDialog.FormShow(Sender: TObject);
begin
  LBFiles.Clear;
  BSearch.Caption:= _('Search');
end;

{$WARNINGS OFF}
procedure TFHelpDialog.ReadDirectories(TVTree: TTreeView; Node: TTreeNode; const directory: string);
  var SearchRec: TSearchRec; BinTree: TTree;

  procedure ReadDirectories;
  begin
    if (SearchRec.Attr and faDirectory = faDirectory) and
       (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      BinTree.Insert(SearchRec.Name);
  end;

  procedure InsertDirectories;
  begin
    var s:= Trim(BinTree.Inorder) + '  ';
    while Length(s) > 2 do begin
      var p:= Pos('  ', s);
      var leaf:= TVTree.Items.AddChild(Node, copy(s, 1, p-1));
      leaf.ImageIndex:= 0;
      leaf.HasChildren:= true;
      delete(s, 1, p+1);
    end
  end;

  procedure ReadFiles;
  begin
    var s:= SearchRec.Name;
    var p:= pos('.html', s);
    if p = 0 then exit;
    BinTree.Insert(Copy(s, 1, p-1));
  end;

  procedure InsertFiles;
    var leaf: TTreeNode;
        s: string; p: integer;
  begin
    s:= Trim(BinTree.Inorder) + '  ';
    while Length(s) > 2 do begin
      p:= Pos(' ', s);
      leaf:= TVTree.Items.AddChild(Node, copy(s, 1, p-1));
      leaf.ImageIndex:= 2;
      leaf.SelectedIndex:= 2;
      leaf.HasChildren:= false;
      delete(s, 1, p+1);
    end;
  end;

begin
  BinTree:= TTree.Create;
  if FindFirst(directory + '*.*', faDirectory + faReadOnly, SearchRec) = 0 then begin
    ReadDirectories;
    while FindNext(SearchRec) = 0 do
      ReadDirectories;
  end;
  InsertDirectories;
  FindClose(SearchRec);
  BinTree.Delete;
  if FindFirst(directory + '*.*', faReadOnly, SearchRec) = 0 then begin
    ReadFiles;
    while FindNext(SearchRec) = 0 do
      ReadFiles;
  end;
  InsertFiles;
  FindClose(SearchRec);
  BinTree.Destroy;
end;
{$WARNINGS ON}

function TFHelpDialog.getPath(i: integer): string;
  var s: string;
begin
  var Path:= FConfiguration.ReadstringFile('Program', 'Manual', '');
  if Pos('.html', Path) > 0 then Path:= ExtractFilePath(Path);
  Path:= FConfiguration.AddPortableDrive(withoutTrailingSlash(Path));
  if Right(Path, -4) = '\api' then
    Path:= copy(Path, 1, length(path) - 4);
  case i of
    2: s:= Path + '\api\';
    3: begin
         s:= Path + '\guide\';
         if not DirectoryExists(s) then
           s:= Path + '\technotes\guides\';
       end;
    4: begin
         s:= Path + '\tooldocs\';
         if not DirectoryExists(s) then
           s:= Path + '\technotes\tools\';
       end;
  end;
  Result:= HTTPToCache(s);
end;

procedure TFHelpDialog.TVTreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.Count = 0 then begin
    var s:= Node.Text + '\';
    var pNode:= Node;
    while pNode.Parent <> nil do begin
      pNode:= pNode.Parent;
      s:= pNode.Text + '\' + s;
    end;
    ReadDirectories(TTreeView(Sender), Node, getPath(PCJDKhelp.ActivePageIndex)+ s);
  end;
  Node.ImageIndex:= 1;
end;

procedure TFHelpDialog.TVTreeCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  Node.ImageIndex:= 0;
end;

function TFHelpDialog.getFileFromTree(Sender: TObject): string;
begin
  var s:= '';
  var TVTree:= TTreeView(Sender);
  if (TVTree.Selected <> nil) and (TVTree.Selected.ImageIndex=2) then begin
    var pNode:= TVTree.Selected;
    s:= pNode.Text + '.html';
    while pNode.Parent <> nil do begin
      pNode:= pNode.Parent;
      s:= pNode.Text + '\' + s;
    end;
  end;
  Result:= s;
end;

procedure TFHelpDialog.TVTreeDblClick(Sender: TObject);
begin
  var s:= getFileFromTree(Sender);
  if s <> '' then begin
    FJava.CallHelp(false, getPath(PCJDKHelp.ActivePageIndex) + s);
    Close;
  end;  
end;

procedure TFHelpDialog.TVTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  with Node do
    if HasChildren then
      if Expanded
        then SelectedIndex:= 1
        else SelectedIndex:= 0
      else
        SelectedIndex:= 2;
end;

procedure TFHelpDialog.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFHelpDialog.BShowClick(Sender: TObject);
begin
  case PCJDKHelp.ActivePageIndex of
    0: LBFavoritesDblClick(self);
    1: LBFilesDblClick(self);
    2: TVTreeDblClick(TVAPITree);
    3: TVTreeDblClick(TVGuideTree);
    4: TVTreeDblClick(TVToolsTree);
  end;
end;

function TFHelpDialog.NoChar(ch: Char): boolean;
begin
  Result:= not (ch.isLetter or (ch = '_'));
end;

procedure TFHelpDialog.Investigate(FoundFiles: TstringList; const sought: string);
  var s, Filename: string; i, j, p: Integer;
begin
  var SL:= TStringList.Create;
  try
    for i:= 0 to FoundFiles.Count-1 do begin
      Filename:= FoundFiles[i];
      if PerformSearch then begin
        SL.LoadFromFile(Filename);
        s:= SL.Text;
        p:= Pos(sought, s);
        if p > 0 then begin
          j:= p + Length(sought);
          if NoChar(s[j]) and ((p = 1) or NoChar(s[p-1])) then begin
            Inc(sites);
            LBFiles.Items.Add(Filename);
            lastSite:= Filename;
          end;
        end;
        Application.ProcessMessages;
        if (sites >= FConfiguration.MaxSearch) or ready then begin
          ready:= true;
          exit;
        end;
      end else // find search site
        if lastSite = Filename then
          PerformSearch:= true;
    end;
  finally
    FreeAndNil(SL);
  end;
end;

{$WARNINGS OFF}
procedure TFHelpDialog.searchFiles(directory: string; const sought: string);
  var SearchRec: TSearchRec; s: string;
      FoundFiles: TStringList;
begin
  directory:= HTTPToCache(directory);
  if (FindFirst(directory + '\*.*', faReadOnly, SearchRec) = 0) then begin
    FoundFiles:= TstringList.Create;
    FoundFiles.Sorted:= true;
    repeat
      s:= UpperCase(SearchRec.Name);
      if Pos('.HTML', s) + Pos('.HTM', s) = 0 then continue;
      FoundFiles.Add(directory + '\' + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    Investigate(FoundFiles, sought);
    FreeAndNil(FoundFiles);
  end;
  FindClose(SearchRec);
  if ready then exit;
  if FindFirst(directory + '\*.*', faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
         (SearchRec.Attr and faDirectory = faDirectory) then
        searchFiles(directory + '\' + SearchRec.Name, sought);
    until ready or (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;
{$WARNINGS ON}

procedure TFHelpDialog.GlobalSearch(const sought: string);
begin
  sites:= 0;
  PerformSearch:= (lastSite = '');
  ready:= false;
  BSearch.Caption:= Format(_('Cancel search for "%s"'), [sought]);
  case RGSearchOptions.ItemIndex of
    0: searchFiles(FConfiguration.getJavaManual, sought);
    1: searchFiles(withoutTrailingSlash(ExtractFilePath(FConfiguration.Javabook)), sought);
    2: searchFiles(FConfiguration.getJavaManual + '\guide', sought);
    3: searchFiles(FConfiguration.getJavaManual, sought);
    4: searchFiles(FConfiguration.getJavaManual + '\tooldocs', sought);
  end;
  if sites < FConfiguration.MaxSearch
    then BSearch.Caption:= _('Search')
    else BSearch.Caption:= _('Search again');
end;

function TFHelpDialog.getClassName(sought: string): string;
  var i1, i2, p: Integer; Filename, Path, s: string; SL: TstringList;
begin
  Result:= '';
  sought:= '/' + sought + '.html';  // pattern for "sought" in allclasse-frame.html
  Filename:= FConfiguration.getJavaManual + '\allclasses-frame.html';
  if not FileExists(Filename) then
    Filename:= FConfiguration.getJavaManual + '\allclasses-index.html';

  Path:= ExtractFilepath(Filename);
  if FConfiguration.GlobalFileExists(Filename, false) then begin
    SL:= TstringList.Create;
    SL.LoadFromFile(Filename);
    s:= SL.Text;
    p:= Pos(sought, s);
    if p > 0 then begin
      i1:= p;
      i2:= p;
      while (i1 > 0) and (s[i1] <> '"') do dec(i1);
      while (i2 <= length(s)) and (s[i2] <> '"') do inc(i2);
      s:= Path + toWindows(Copy(s, i1+1, i2-i1-1));
      if FConfiguration.GlobalFileExists(s, false) then Result:= s;
    end;
    FreeAndNil(SL);
  end;
end;

function TFHelpDialog.searchIndex(const sought: string): string;
  var i: Integer; IndexFile, HTMLFile: string;

  function searchstring(const IndexFile: string; sought: string; var HTMLFile: string): boolean;
    var i1, i2, p: Integer; s: string; SL: TstringList;
  begin
    SL:= TstringList.Create;
    SL.LoadFromFile(IndexFile);
    sought:= '"><B>' + sought;  // <A HREF="..."><B>sought   </A>
    s:= SL.Text;
    p:= Pos(sought, s);
    while p > 0 do begin
      i1:= p + Length(sought);
      if IsAlpha(s[i1]) then begin
        delete(s, 1, i1);
        p:= Pos(sought, s);
      end else begin
        i2:= p - 1;
        i1:= p - 2;
        while (i1 > 0) and (s[i1] <> '"') do dec(i1);
        HTMLFile:= FConfiguration.getJavaManual + copy(s, i1+3, i2-i1-2);
        if not FConfiguration.GlobalFileExists(HTMLFile, false) then HTMLFile:= '';
        p:= 0;
      end;
    end;
    FreeAndNil(SL);
    Result:= (p > 0);
  end;

begin
  Result:= '';
  i:= Ord(sought[1]) and $1F; // Map upper/lower case letters to 1 to 26
  if i = 31 then i:= 27; // '_' = 27
  IndexFile:= FConfiguration.getJavaManual + '\index-files\index-' + IntToStr(i) + '.html';
  if FConfiguration.GlobalFileExists(IndexFile, false) and
     searchstring(IndexFile, sought, HTMLFile)
  then
    Result:= HTMLFile
end;

procedure TFHelpDialog.BSearchClick(Sender: TObject);
begin
  if BSearch.Caption = _('Cancel search for "%s"') then begin
    ready:= true;
    exit;
  end;
  ESearch.Text:= Trim(ESearch.Text);
  if Length(ESearch.Text) = 0 then exit;
  Screen.Cursor:= crHourglass;
  try
    if BSearch.Caption = _('Search') then begin
      LBFiles.Items.Clear;
      lastSite:= '';
      if RGSearchOptions.ItemIndex = 0 then begin // API search
        var s:= getClassName(ESearch.Text);
        if s <> '' then begin
          LBFiles.Items.Add(s);
          LBFiles.ItemIndex:= 0;
          end
        else begin
          s:= searchIndex(ESearch.Text);
          if s <> '' then begin
            LBFiles.Items.Add(s);
            LBFiles.ItemIndex:= 0;
          end
        end;
      end;  
      if LBFiles.Items.Count = 0
        then GlobalSearch(ESearch.text)
        else BSearch.Caption:= _('Search again');
      end
    else
      GlobalSearch(ESearch.Text);
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFHelpDialog.LBFilesDblClick(Sender: TObject);
begin
  var i:= LBFiles.ItemIndex;
  if i >= 0 then begin
    var s:= LBFiles.Items[i];
    if s <> '' then begin
      FJava.CallHelp(false, s);
      close;
    end;
  end;
end;

procedure TFHelpDialog.ESearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then begin
    BSearchClick(Self);
    if (LBFiles.Items.Count > 0) and LBFiles.CanFocus then
      LBFiles.SetFocus;
  end;
  BSearch.Caption:= _('Search');
end;

procedure TFHelpDialog.ESearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      Ord('C'): Clipboard.AsText:= ESearch.Text;
      Ord('V'): ESearch.Text:= Clipboard.AsText;
    end;
end;

procedure TFHelpDialog.PCJDKHelpChange(Sender: TObject);
begin
  if PCJDKHelp.ActivePage = TSSearch then begin
    if ESearch.canFocus then ESearch.SetFocus;
    BSearch.Caption:= _('Search');
  end;
end;

procedure TFHelpDialog.LBFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_Return then
    LBFilesDblClick(Self);
end;

procedure TFHelpDialog.RGSearchOptionsClick(Sender: TObject);
begin
  BSearch.Caption:= _('Search');
end;

function TFHelpDialog.GetTitle(s: string): string;
  var s1: string; i, p: integer;
      SL: TstringList;
begin
  Result:= '';
  p:= Pos('#', s);
  if p > 0 then begin
    Delete(s, 1, p);
    Result:= s;
    Exit;
  end;
  if FConfiguration.GlobalFileExists(s, false) then
    try
      SL:= TstringList.Create;
      SL.LoadFromFile(s);
      for i:= 0 to SL.Count - 1 do begin
        s1:= SL[i];
        p:= Pos('<TITLE>', UpperCase(s1));
        if p > 0 then begin
          Delete(s1, 1, p+6);
          p:= Pos('</TITLE>', UpperCase(s1));
          if p = 0 then p:= Pos(#10, s1);
          if p = 0 then p:= Pos(#13, s1);
          if p = 0 then p:= Length(s1);
          Delete(s1, p, Length(s1));
          Result:= s1;
          break;
        end;
      end;
      FreeAndNil(SL);
    finally
    end
   else
     Result:= ExtractFileNameEx(s);
end;

procedure TFHelpDialog.BAddFavoritClick(Sender: TObject);
begin
  var s:= '';
  case PCJDKHelp.ActivePageIndex of
    1: if LBFiles.ItemIndex >= 0 then
         s:= LBFiles.Items[LBFiles.ItemIndex];
    2: begin s:= getFileFromTree(TVAPITree);   if s <> '' then s:= getPath(2) + s; end;
    3: begin s:= getFileFromTree(TVGuideTree); if s <> '' then s:= getPath(3) + s; end;
    4: begin s:= getFileFromTree(TVToolsTree); if s <> '' then s:= getPath(4) + s; end;
  end;
  if s <> '' then NewFavorit('', s);
end;

procedure TFHelpDialog.NewFavorit(const Identifier: string; URL: string);
begin
  PCJDKHelp.ActivePageIndex:= 0;
  with TFFavoritenDialog.Create(Self) do begin
    EUrl.Text:= Url;
    if Identifier <> ''
      then EDescription.Text:= Identifier
      else EDescription.Text:= GetTitle(Url);
    if URL = ''
      then ActiveControl:= EUrl
      else ActiveControl:= EDescription;
    if ShowModal = mrOK then begin
      Url:= EDescription.Text + ' | ' + EUrl.Text;
      Favorites.Add(Url);
      FConfiguration.SaveFavorites(Favorites);
      ShowFavorites;
    end;
    Free;
  end;
end;

procedure TFHelpDialog.ShowFavorites;
  var i, p: Integer; s: string;
begin
  LBFavorites.Items.Clear;
  for i:= 0 to Favorites.Count-1 do begin
    s:= Favorites[i];
    p:= Pos(' | ', s);
    LBFavorites.Items.Add(Copy(s, 1, p-1));
  end;
end;

procedure TFHelpDialog.LBFavoritesDblClick(Sender: TObject);
begin
  var i:= LBFavorites.ItemIndex;
  if (-1 < i) and (i < Favorites.Count) then begin
    var s:= Favorites[i];
    i:= Pos(' | ', s);
    s:= Copy(s, i+3, Length(s));
    if s <> '' then begin
      if isHTML(s)
        then FJava.CallHelp(false, s)
        else FJava.Open(s);
      close;
    end;
  end;
end;

procedure TFHelpDialog.BDeleteClick(Sender: TObject);
begin
  var i:= LBFavorites.ItemIndex;
  with LBFavorites do
    if i >= 0 then begin
      Items.Delete(i);
      Favorites.Delete(i);
      FConfiguration.SaveFavorites(Favorites);
    end;
end;

procedure TFHelpDialog.BEditClick(Sender: TObject);
  var s: string; p, i: integer;
begin
  i:= LBFavorites.ItemIndex;
  if (-1 < i) and (i < Favorites.Count) then begin
    s:= Favorites[i];
    p:= Pos(' | ', s);
    s:= Copy(s, p+3, Length(s));
    with TFFavoritenDialog.Create(Self) do begin
      EUrl.Text:= s;
      EDescription.Text:= LBFavorites.Items[i];
      if ShowModal = mrOK then begin
        s:= EDescription.Text + ' | ' + EUrl.Text;
        Favorites[i]:= s;
        FConfiguration.SaveFavorites(Favorites);
        ShowFavorites;
      end;
      Free;
    end;
  end;
end;

procedure TFHelpDialog.SBUpClick(Sender: TObject);
begin
  var i:= LBFavorites.ItemIndex;
  if (0 < i) and (i < Favorites.Count) then begin
    Favorites.Exchange(i, i-1);
    FConfiguration.SaveFavorites(Favorites);
    ShowFavorites;
    LBFavorites.ItemIndex:= i-1;
  end;
end;

procedure TFHelpDialog.SBDownClick(Sender: TObject);
begin
  var i:= LBFavorites.ItemIndex;
  if (-1 < i) and (i < Favorites.Count-1) then begin
    Favorites.Exchange(i, i+1);
    FConfiguration.SaveFavorites(Favorites);
    ShowFavorites;
    LBFavorites.ItemIndex:= i+1;
  end;
end;

function TFHelpDialog.HttpToCache(s: string): string;
begin
  Result:= s;
  var p:= Pos('://', s);
  if p > 0 then begin
    Delete(s, 1, p + 2);
    p:= Pos('/', s);
    delete(s, 1, p-1);
    Result:= ToWindows(FConfiguration.JavaCache + s);
  end;
end;

procedure TFHelpDialog.BNewClick(Sender: TObject);
begin
  NewFavorit('', '');
end;

procedure TFHelpDialog.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  FConfiguration.WriteIntegerU('Program', 'Searchoptions', RGSearchOptions.ItemIndex);
  FConfiguration.WriteIntegerU('Program', 'JDK-Help-Page', PCJDKHelp.ActivePageIndex);
  aAction:= caFree;
end;

procedure TFHelpDialog.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    TVAPITree.Images:= vilJDKHelpDark;
    TVGuideTree.Images:= vilJDKHelpDark;
    TVToolsTree.Images:= vilJDKHelpDark;
  end else begin
    TVAPITree.Images:= vilJDKHelpLight;
    TVGuideTree.Images:= vilJDKHelpLight;
    TVToolsTree.Images:= vilJDKHelpLight;
  end;
end;

end.
