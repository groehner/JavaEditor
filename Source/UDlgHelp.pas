unit UDlgHelp;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Buttons,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
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
    procedure ESearchKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BAddFavoritClick(Sender: TObject);
    procedure LBFavoritesDblClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure BNewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLastSite: string;
    FSites: Integer;
    FPerformSearch, FReady: Boolean;
    FFavorites: TStringList; // format "Identifier | URL"
    procedure Investigate(FoundFiles: TStringList; const Sought: string);
    procedure SearchFiles(Directory: string; const Sought: string);
    procedure ReadDirectories(TVTree: TTreeView; Node: TTreeNode;
      const Directory: string);
    function GetPath(Num: Integer): string;
    function GetFileFromTree(Sender: TObject): string;
    function GetClassName(Sought: string): string;
    function GetTitle(Str: string): string;
    function NoChar(AChar: Char): Boolean;
    function SearchIndex(const Sought: string): string;
    procedure GlobalSearch(const Sought: string);
    procedure ShowFavorites;
    procedure NewFavorit(const Identifier: string; Url: string);
    function HttpToCache(Str: string): string;
    procedure ChangeStyle;
  end;

implementation

uses
  Windows,
  SysUtils,
  Clipbrd,
  Character,
  JvGnugettext,
  UJava,
  UTree,
  UConfiguration,
  UDlgFavorites,
  UUtils;

{$R *.DFM}

procedure TFHelpDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FFavorites := TStringList.Create;
  RGSearchOptions.ItemIndex := FConfiguration.ReadIntegerU('Program',
    'Searchoptions', 0);
  PCJDKHelp.ActivePageIndex := FConfiguration.ReadIntegerU('Program',
    'JDK-Help-Page', 0);
  FConfiguration.ReadFavorites(FFavorites);
  ShowFavorites;
  ReadDirectories(TVAPITree, nil, GetPath(2));
  ReadDirectories(TVGuideTree, nil, GetPath(3));
  ReadDirectories(TVToolsTree, nil, GetPath(4));
  ChangeStyle;
end;

procedure TFHelpDialog.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFavorites);
end;

procedure TFHelpDialog.FormShow(Sender: TObject);
begin
  LBFiles.Clear;
  BSearch.Caption := _('Search');
end;

procedure TFHelpDialog.ReadDirectories(TVTree: TTreeView; Node: TTreeNode;
  const Directory: string);
var
  SearchRec: TSearchRec;
  BinTree: TTree;

  procedure ReadDirectories;
  begin
    if (SearchRec.Attr and faDirectory = faDirectory) and
      (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      BinTree.Insert(SearchRec.Name);
  end;

  procedure InsertDirectories;
  begin
    var
    Str := Trim(BinTree.InOrder) + '  ';
    while Length(Str) > 2 do
    begin
      var
      Posi := Pos('  ', Str);
      var
      Leaf := TVTree.Items.AddChild(Node, Copy(Str, 1, Posi - 1));
      Leaf.ImageIndex := 0;
      Leaf.HasChildren := True;
      Delete(Str, 1, Posi + 1);
    end;
  end;

  procedure ReadFiles;
  begin
    var
    Str := SearchRec.Name;
    var
    Posi := Pos('.html', Str);
    if Posi = 0 then
      Exit;
    BinTree.Insert(Copy(Str, 1, Posi - 1));
  end;

  procedure InsertFiles;
  var
    Leaf: TTreeNode;
    Str: string;
    Posi: Integer;
  begin
    Str := Trim(BinTree.InOrder) + '  ';
    while Length(Str) > 2 do
    begin
      Posi := Pos(' ', Str);
      Leaf := TVTree.Items.AddChild(Node, Copy(Str, 1, Posi - 1));
      Leaf.ImageIndex := 2;
      Leaf.SelectedIndex := 2;
      Leaf.HasChildren := False;
      Delete(Str, 1, Posi + 1);
    end;
  end;

begin
  BinTree := TTree.Create;
  if FindFirst(Directory + '*.*', faDirectory + faReadOnly, SearchRec) = 0 then
  begin
    ReadDirectories;
    while FindNext(SearchRec) = 0 do
      ReadDirectories;
  end;
  InsertDirectories;
  FindClose(SearchRec);
  BinTree.Delete;
  if FindFirst(Directory + '*.*', faReadOnly, SearchRec) = 0 then
  begin
    ReadFiles;
    while FindNext(SearchRec) = 0 do
      ReadFiles;
  end;
  InsertFiles;
  FindClose(SearchRec);
  BinTree.Destroy;
end;

function TFHelpDialog.GetPath(Num: Integer): string;
var
  Str: string;
begin
  var
  Path := FConfiguration.ReadStringFile('Program', 'Manual', '');
  if Pos('.html', Path) > 0 then
    Path := ExtractFilePath(Path);
  Path := FConfiguration.AddPortableDrive(ExcludeTrailingPathDelimiter(Path));
  if Right(Path, -4) = '\api' then
    Path := Copy(Path, 1, Length(Path) - 4);
  case Num of
    2:
      Str := Path + '\api\';
    3:
      begin
        Str := Path + '\guide\';
        if not DirectoryExists(Str) then
          Str := Path + '\technotes\guides\';
      end;
    4:
      begin
        Str := Path + '\tooldocs\';
        if not DirectoryExists(Str) then
          Str := Path + '\technotes\tools\';
      end;
  end;
  Result := HttpToCache(Str);
end;

procedure TFHelpDialog.TVTreeExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if Node.Count = 0 then
  begin
    var
    Str := Node.Text + '\';
    var
    PNode := Node;
    while PNode.Parent <> nil do
    begin
      PNode := PNode.Parent;
      Str := PNode.Text + '\' + Str;
    end;
    ReadDirectories(TTreeView(Sender), Node,
      GetPath(PCJDKHelp.ActivePageIndex) + Str);
  end;
  Node.ImageIndex := 1;
end;

procedure TFHelpDialog.TVTreeCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  Node.ImageIndex := 0;
end;

function TFHelpDialog.GetFileFromTree(Sender: TObject): string;
begin
  var
  Str := '';
  var
  TVTree := TTreeView(Sender);
  if (TVTree.Selected <> nil) and (TVTree.Selected.ImageIndex = 2) then
  begin
    var
    PNode := TVTree.Selected;
    Str := PNode.Text + '.html';
    while PNode.Parent <> nil do
    begin
      PNode := PNode.Parent;
      Str := PNode.Text + '\' + Str;
    end;
  end;
  Result := Str;
end;

procedure TFHelpDialog.TVTreeDblClick(Sender: TObject);
begin
  var
  Str := GetFileFromTree(Sender);
  if Str <> '' then
  begin
    FJava.CallHelp(False, GetPath(PCJDKHelp.ActivePageIndex) + Str);
    Close;
  end;
end;

procedure TFHelpDialog.TVTreeGetSelectedIndex(Sender: TObject; Node: TTreeNode);
begin
  with Node do
    if HasChildren then
      if Expanded then
        SelectedIndex := 1
      else
        SelectedIndex := 0
    else
      SelectedIndex := 2;
end;

procedure TFHelpDialog.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFHelpDialog.BShowClick(Sender: TObject);
begin
  case PCJDKHelp.ActivePageIndex of
    0:
      LBFavoritesDblClick(Self);
    1:
      LBFilesDblClick(Self);
    2:
      TVTreeDblClick(TVAPITree);
    3:
      TVTreeDblClick(TVGuideTree);
    4:
      TVTreeDblClick(TVToolsTree);
  end;
end;

function TFHelpDialog.NoChar(AChar: Char): Boolean;
begin
  Result := not(AChar.IsLetter or (AChar = '_'));
end;

procedure TFHelpDialog.Investigate(FoundFiles: TStringList;
  const Sought: string);
var
  Str, FileName: string;
  Posi: Integer;
begin
  var
  StringList := TStringList.Create;
  try
    for var I := 0 to FoundFiles.Count - 1 do
    begin
      FileName := FoundFiles[I];
      if FPerformSearch then
      begin
        StringList.LoadFromFile(FileName);
        Str := StringList.Text;
        Posi := Pos(Sought, Str);
        if Posi > 0 then
        begin
          if NoChar(Str[Posi + Length(Sought)]) and
            ((Posi = 1) or NoChar(Str[Posi - 1])) then
          begin
            Inc(FSites);
            LBFiles.Items.Add(FileName);
            FLastSite := FileName;
          end;
        end;
        Application.ProcessMessages;
        if (FSites >= FConfiguration.MaxSearch) or FReady then
        begin
          FReady := True;
          Exit;
        end;
      end
      else // find search site
        if FLastSite = FileName then
          FPerformSearch := True;
    end;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TFHelpDialog.SearchFiles(Directory: string; const Sought: string);
var
  SearchRec: TSearchRec;
  Str: string;
  FoundFiles: TStringList;
begin
  Directory := HttpToCache(Directory);
  if (FindFirst(Directory + '\*.*', faReadOnly, SearchRec) = 0) then
  begin
    FoundFiles := TStringList.Create;
    FoundFiles.Sorted := True;
    repeat
      Str := UpperCase(SearchRec.Name);
      if Pos('.HTML', Str) + Pos('.HTM', Str) = 0 then
        Continue;
      FoundFiles.Add(Directory + '\' + SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    Investigate(FoundFiles, Sought);
    FreeAndNil(FoundFiles);
  end;
  FindClose(SearchRec);
  if FReady then
    Exit;
  if FindFirst(Directory + '\*.*', faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
        (SearchRec.Attr and faDirectory = faDirectory) then
        SearchFiles(Directory + '\' + SearchRec.Name, Sought);
    until FReady or (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);
end;

procedure TFHelpDialog.GlobalSearch(const Sought: string);
begin
  FSites := 0;
  FPerformSearch := (FLastSite = '');
  FReady := False;
  BSearch.Caption := Format(_('Cancel search for "%s"'), [Sought]);
  case RGSearchOptions.ItemIndex of
    0:
      SearchFiles(FConfiguration.GetJavaManual, Sought);
    1:
      SearchFiles(ExcludeTrailingPathDelimiter(ExtractFilePath(FConfiguration.Javabook)
        ), Sought);
    2:
      SearchFiles(FConfiguration.GetJavaManual + '\guide', Sought);
    3:
      SearchFiles(FConfiguration.GetJavaManual, Sought);
    4:
      SearchFiles(FConfiguration.GetJavaManual + '\tooldocs', Sought);
  end;
  if FSites < FConfiguration.MaxSearch then
    BSearch.Caption := _('Search')
  else
    BSearch.Caption := _('Search again');
end;

function TFHelpDialog.GetClassName(Sought: string): string;
var
  Int1, Int2, Posi: Integer;
  FileName, Path, Str: string;
  StringList: TStringList;
begin
  Result := '';
  Sought := '/' + Sought + '.html';
  // pattern for "Sought" in allclasse-frame.html
  FileName := FConfiguration.GetJavaManual + '\allclasses-frame.html';
  if not FileExists(FileName) then
    FileName := FConfiguration.GetJavaManual + '\allclasses-index.html';

  Path := ExtractFilePath(FileName);
  if FConfiguration.GlobalFileExists(FileName, False) then
  begin
    StringList := TStringList.Create;
    StringList.LoadFromFile(FileName);
    Str := StringList.Text;
    Posi := Pos(Sought, Str);
    if Posi > 0 then
    begin
      Int1 := Posi;
      Int2 := Posi;
      while (Int1 > 0) and (Str[Int1] <> '"') do
        Dec(Int1);
      while (Int2 <= Length(Str)) and (Str[Int2] <> '"') do
        Inc(Int2);
      Str := Path + ToWindows(Copy(Str, Int1 + 1, Int2 - Int1 - 1));
      if FConfiguration.GlobalFileExists(Str, False) then
        Result := Str;
    end;
    FreeAndNil(StringList);
  end;
end;

function TFHelpDialog.SearchIndex(const Sought: string): string;
var
  Int: Integer;
  IndexFile, HTMLFile: string;

  function SearchString(const IndexFile: string; Sought: string;
    var HTMLFile: string): Boolean;
  var
    Int1, Int2, Posi: Integer;
    Str: string;
    StringList: TStringList;
  begin
    StringList := TStringList.Create;
    StringList.LoadFromFile(IndexFile);
    Sought := '"><B>' + Sought; // <A HREF="..."><B>Sought   </A>
    Str := StringList.Text;
    Posi := Pos(Sought, Str);
    while Posi > 0 do
    begin
      Int1 := Posi + Length(Sought);
      if IsAlpha(Str[Int1]) then
      begin
        Delete(Str, 1, Int1);
        Posi := Pos(Sought, Str);
      end
      else
      begin
        Int2 := Posi - 1;
        Int1 := Posi - 2;
        while (Int1 > 0) and (Str[Int1] <> '"') do
          Dec(Int1);
        HTMLFile := FConfiguration.GetJavaManual + Copy(Str, Int1 + 3,
          Int2 - Int1 - 2);
        if not FConfiguration.GlobalFileExists(HTMLFile, False) then
          HTMLFile := '';
        Posi := 0;
      end;
    end;
    FreeAndNil(StringList);
    Result := (Posi > 0);
  end;

begin
  Result := '';
  Int := Ord(Sought[1]) and $1F; // Map upper/lower case letters to 1 to 26
  if Int = 31 then
    Int := 27; // '_' = 27
  IndexFile := FConfiguration.GetJavaManual + '\index-files\index-' +
    IntToStr(Int) + '.html';
  if FConfiguration.GlobalFileExists(IndexFile, False) and
    SearchString(IndexFile, Sought, HTMLFile) then
    Result := HTMLFile;
end;

procedure TFHelpDialog.BSearchClick(Sender: TObject);
begin
  if BSearch.Caption = _('Cancel search for "%s"') then
  begin
    FReady := True;
    Exit;
  end;
  ESearch.Text := Trim(ESearch.Text);
  if Length(ESearch.Text) = 0 then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    if BSearch.Caption = _('Search') then
    begin
      LBFiles.Items.Clear;
      FLastSite := '';
      if RGSearchOptions.ItemIndex = 0 then
      begin // API search
        var
        Str := GetClassName(ESearch.Text);
        if Str <> '' then
        begin
          LBFiles.Items.Add(Str);
          LBFiles.ItemIndex := 0;
        end
        else
        begin
          Str := SearchIndex(ESearch.Text);
          if Str <> '' then
          begin
            LBFiles.Items.Add(Str);
            LBFiles.ItemIndex := 0;
          end;
        end;
      end;
      if LBFiles.Items.Count = 0 then
        GlobalSearch(ESearch.Text)
      else
        BSearch.Caption := _('Search again');
    end
    else
      GlobalSearch(ESearch.Text);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFHelpDialog.LBFilesDblClick(Sender: TObject);
begin
  var
  Int := LBFiles.ItemIndex;
  if Int >= 0 then
  begin
    var
    Str := LBFiles.Items[Int];
    if Str <> '' then
    begin
      FJava.CallHelp(False, Str);
      Close;
    end;
  end;
end;

procedure TFHelpDialog.ESearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    BSearchClick(Self);
    if (LBFiles.Items.Count > 0) and LBFiles.CanFocus then
      LBFiles.SetFocus;
  end;
  BSearch.Caption := _('Search');
end;

procedure TFHelpDialog.ESearchKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      Ord('C'):
        Clipboard.AsText := ESearch.Text;
      Ord('V'):
        ESearch.Text := Clipboard.AsText;
    end;
end;

procedure TFHelpDialog.PCJDKHelpChange(Sender: TObject);
begin
  if PCJDKHelp.ActivePage = TSSearch then
  begin
    if ESearch.CanFocus then
      ESearch.SetFocus;
    BSearch.Caption := _('Search');
  end;
end;

procedure TFHelpDialog.LBFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    LBFilesDblClick(Self);
end;

procedure TFHelpDialog.RGSearchOptionsClick(Sender: TObject);
begin
  BSearch.Caption := _('Search');
end;

function TFHelpDialog.GetTitle(Str: string): string;
var
  Str1: string;
  Posi: Integer;
  StringList: TStringList;
begin
  Result := '';
  Posi := Pos('#', Str);
  if Posi > 0 then
  begin
    Delete(Str, 1, Posi);
    Result := Str;
    Exit;
  end;
  if FConfiguration.GlobalFileExists(Str, False) then
    try
      StringList := TStringList.Create;
      StringList.LoadFromFile(Str);
      for var I := 0 to StringList.Count - 1 do
      begin
        Str1 := StringList[I];
        Posi := Pos('<TITLE>', UpperCase(Str1));
        if Posi > 0 then
        begin
          Delete(Str1, 1, Posi + 6);
          Posi := Pos('</TITLE>', UpperCase(Str1));
          if Posi = 0 then
            Posi := Pos(#10, Str1);
          if Posi = 0 then
            Posi := Pos(#13, Str1);
          if Posi = 0 then
            Posi := Length(Str1);
          Delete(Str1, Posi, Length(Str1));
          Result := Str1;
          Break;
        end;
      end;
    finally
      FreeAndNil(StringList);
    end
  else
    Result := ExtractFileNameEx(Str);
end;

procedure TFHelpDialog.BAddFavoritClick(Sender: TObject);
begin
  var
  Str := '';
  case PCJDKHelp.ActivePageIndex of
    1:
      if LBFiles.ItemIndex >= 0 then
        Str := LBFiles.Items[LBFiles.ItemIndex];
    2:
      begin
        Str := GetFileFromTree(TVAPITree);
        if Str <> '' then
          Str := GetPath(2) + Str;
      end;
    3:
      begin
        Str := GetFileFromTree(TVGuideTree);
        if Str <> '' then
          Str := GetPath(3) + Str;
      end;
    4:
      begin
        Str := GetFileFromTree(TVToolsTree);
        if Str <> '' then
          Str := GetPath(4) + Str;
      end;
  end;
  if Str <> '' then
    NewFavorit('', Str);
end;

procedure TFHelpDialog.NewFavorit(const Identifier: string; Url: string);
begin
  PCJDKHelp.ActivePageIndex := 0;
  with TFFavoritenDialog.Create(Self) do
  begin
    EUrl.Text := Url;
    if Identifier <> '' then
      EDescription.Text := Identifier
    else
      EDescription.Text := GetTitle(Url);
    if Url = '' then
      ActiveControl := EUrl
    else
      ActiveControl := EDescription;
    if ShowModal = mrOk then
    begin
      Url := EDescription.Text + ' | ' + EUrl.Text;
      FFavorites.Add(Url);
      FConfiguration.SaveFavorites(FFavorites);
      ShowFavorites;
    end;
    Free;
  end;
end;

procedure TFHelpDialog.ShowFavorites;
var
  Posi: Integer;
  Str: string;
begin
  LBFavorites.Items.Clear;
  for var I := 0 to FFavorites.Count - 1 do
  begin
    Str := FFavorites[I];
    Posi := Pos(' | ', Str);
    LBFavorites.Items.Add(Copy(Str, 1, Posi - 1));
  end;
end;

procedure TFHelpDialog.LBFavoritesDblClick(Sender: TObject);
begin
  var
  Int := LBFavorites.ItemIndex;
  if (-1 < Int) and (Int < FFavorites.Count) then
  begin
    var
    Str := FFavorites[Int];
    Int := Pos(' | ', Str);
    Str := Copy(Str, Int + 3, Length(Str));
    if Str <> '' then
    begin
      if IsHTML(Str) then
        FJava.CallHelp(False, Str)
      else
        FJava.Open(Str);
      Close;
    end;
  end;
end;

procedure TFHelpDialog.BDeleteClick(Sender: TObject);
begin
  var
  Int := LBFavorites.ItemIndex;
  with LBFavorites do
    if Int >= 0 then
    begin
      Items.Delete(Int);
      FFavorites.Delete(Int);
      FConfiguration.SaveFavorites(FFavorites);
    end;
end;

procedure TFHelpDialog.BEditClick(Sender: TObject);
var
  Str: string;
  Posi, Int: Integer;
begin
  Int := LBFavorites.ItemIndex;
  if (-1 < Int) and (Int < FFavorites.Count) then
  begin
    Str := FFavorites[Int];
    Posi := Pos(' | ', Str);
    Str := Copy(Str, Posi + 3, Length(Str));
    with TFFavoritenDialog.Create(Self) do
    begin
      EUrl.Text := Str;
      EDescription.Text := LBFavorites.Items[Int];
      if ShowModal = mrOk then
      begin
        Str := EDescription.Text + ' | ' + EUrl.Text;
        FFavorites[Int] := Str;
        FConfiguration.SaveFavorites(FFavorites);
        ShowFavorites;
      end;
      Free;
    end;
  end;
end;

procedure TFHelpDialog.SBUpClick(Sender: TObject);
begin
  var
  Int := LBFavorites.ItemIndex;
  if (0 < Int) and (Int < FFavorites.Count) then
  begin
    FFavorites.Exchange(Int, Int - 1);
    FConfiguration.SaveFavorites(FFavorites);
    ShowFavorites;
    LBFavorites.ItemIndex := Int - 1;
  end;
end;

procedure TFHelpDialog.SBDownClick(Sender: TObject);
begin
  var
  Int := LBFavorites.ItemIndex;
  if (-1 < Int) and (Int < FFavorites.Count - 1) then
  begin
    FFavorites.Exchange(Int, Int + 1);
    FConfiguration.SaveFavorites(FFavorites);
    ShowFavorites;
    LBFavorites.ItemIndex := Int + 1;
  end;
end;

function TFHelpDialog.HttpToCache(Str: string): string;
begin
  Result := Str;
  var
  Posi := Pos('://', Str);
  if Posi > 0 then
  begin
    Delete(Str, 1, Posi + 2);
    Posi := Pos('/', Str);
    Delete(Str, 1, Posi - 1);
    Result := ToWindows(FConfiguration.JavaCache + Str);
  end;
end;

procedure TFHelpDialog.BNewClick(Sender: TObject);
begin
  NewFavorit('', '');
end;

procedure TFHelpDialog.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  FConfiguration.WriteIntegerU('Program', 'Searchoptions',
    RGSearchOptions.ItemIndex);
  FConfiguration.WriteIntegerU('Program', 'JDK-Help-Page',
    PCJDKHelp.ActivePageIndex);
  AAction := caFree;
end;

procedure TFHelpDialog.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    TVAPITree.Images := vilJDKHelpDark;
    TVGuideTree.Images := vilJDKHelpDark;
    TVToolsTree.Images := vilJDKHelpDark;
  end
  else
  begin
    TVAPITree.Images := vilJDKHelpLight;
    TVGuideTree.Images := vilJDKHelpLight;
    TVToolsTree.Images := vilJDKHelpLight;
  end;
end;

end.
