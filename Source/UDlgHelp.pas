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

    TSApi: TTabSheet;
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
    FPerformSearch: Boolean;
    FReady: Boolean;
    FFavorites: TStringList; // format "Identifier | URL"
    procedure Investigate(FoundFiles: TStringList; const Sought: string);
    procedure SearchFiles(Directory: string; const Sought: string);
    procedure ReadDirectories(TVTree: TTreeView; Node: TTreeNode;
      const Directory: string);
    function GetPath(Num: Integer): string;
    function GetFileFromTree(Sender: TObject): string;
    function GetClassName(Sought: string): string;
    function GetTitle(Url: string): string;
    function SearchIndex(const Sought: string): string;
    procedure GlobalSearch(const Sought: string);
    procedure ShowFavorites;
    procedure NewFavorit(const Identifier: string; Url: string);
    function HttpToCache(Str: string): string;
    procedure ChangeStyle;
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.SysUtils,
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
  ReadDirectories(TVAPITree, nil, GetPath(0));
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
  BinTree: TTree;

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

  procedure InsertFiles;
  var
    Leaf: TTreeNode;
    Str: string;
    Posi: Integer;
  begin
    Str := Trim(BinTree.InOrder) + '  ';
    while Length(Str) > 2 do
    begin
      Posi := Pos('  ', Str);
      Leaf := TVTree.Items.AddChild(Node, Copy(Str, 1, Posi - 1));
      Leaf.ImageIndex := 2;
      Leaf.SelectedIndex := 2;
      Leaf.HasChildren := False;
      Delete(Str, 1, Posi + 1);
    end;
  end;

begin
  BinTree := TTree.Create;
  var
  Directories := TDirectory.GetDirectories(Directory, '*');
  for var Dir in Directories do
    BinTree.Insert(ExtractFilename(Dir));
  InsertDirectories;
  BinTree.Delete;

  var
  Filenames := TDirectory.GetFiles(Directory, '*.html');
  for var Filename in Filenames do
    BinTree.Insert(ChangeFileExt(ExtractFilename(Filename), ''));
  InsertFiles;
  BinTree.Free;
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
    0:
      Str := Path + '\api\'; // Api
    1:
      Str := Path + '\'; // All
    2:
      Str := ExtractFilePath(FConfiguration.Javabook);
  end;
  if not DirectoryExists(Str) then
    Str := Path + '\api\';
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
      GetPath(0) + Str);
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
    FJava.CallHelp(False, GetPath(0) + Str);
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
  end;
end;

procedure TFHelpDialog.Investigate(FoundFiles: TStringList;
  const Sought: string);
var
  Str: string;
  Posi: Integer;

  function NoChar(AChar: Char): Boolean;
  begin
    Result := not(AChar.IsLetter or (AChar = '_'));
  end;

  function IsWord(Text: string; Posi, Len: Integer): Boolean;
  begin
    Result:= NoChar(Text[Posi + Len]) and
              ((Posi = 1) or NoChar(Text[Posi - 1]));
  end;

begin
  for var Filename in FoundFiles do
    if FPerformSearch then
    begin
      Str := TFile.ReadAllText(Filename);
      Posi := Pos(Sought, Str);
      if (Posi > 0) and IsWord(Str, Posi, Length(Sought)) then
        begin
          Inc(FSites);
          LBFiles.Items.Add(Filename);
          FLastSite := Filename;
        end;
      Application.ProcessMessages;
      if (FSites >= FConfiguration.MaxSearch) or FReady then
      begin
        FReady := True;
        Exit;
      end;
    end
    else // find search site
      if FLastSite = Filename then
        FPerformSearch := True;
end;

procedure TFHelpDialog.SearchFiles(Directory: string; const Sought: string);
var
  FoundFiles: TStringList;
begin
  Directory := HttpToCache(Directory);
  FoundFiles := TStringList.Create;
  FoundFiles.Sorted := True;
  var
  Filenames := TDirectory.GetFiles(Directory, '*.html',
    TSearchOption.soAllDirectories);
  for var Filename in Filenames do
    FoundFiles.Add(Filename);
  Investigate(FoundFiles, Sought);
  FoundFiles.Free;
end;

procedure TFHelpDialog.GlobalSearch(const Sought: string);
begin
  FSites := 0;
  FPerformSearch := (FLastSite = '');
  FReady := False;
  BSearch.Caption := Format(_('Cancel search for "%s"'), [Sought]);
  case RGSearchOptions.ItemIndex of
    0:
      SearchFiles(GetPath(0), Sought);
    1:
      SearchFiles(GetPath(1), Sought);
    2:
      SearchFiles(GetPath(2), Sought);
  end;
  if FSites < FConfiguration.MaxSearch then
    BSearch.Caption := _('Search')
  else
    BSearch.Caption := _('Search again');
end;

function TFHelpDialog.GetClassName(Sought: string): string;
var
  Int1, Int2, Posi: Integer;
  Filename, Path, Str: string;
  StringList: TStringList;
begin
  Result := '';
  Sought := '/' + Sought + '.html';
  // pattern for "Sought" in allclasse-frame.html
  Filename := FConfiguration.GetJavaManual + '\allclasses-frame.html';
  if not FileExists(Filename) then
    Filename := FConfiguration.GetJavaManual + '\allclasses-index.html';

  Path := ExtractFilePath(Filename);
  if FConfiguration.GlobalFileExists(Filename, False) then
  begin
    StringList := TStringList.Create;
    StringList.LoadFromFile(Filename);
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

  procedure APIIndexSearch;
  begin
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

begin
  if BSearch.Caption = Format(_('Cancel search for "%s"'), [ESearch.Text]) then
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
        APIIndexSearch;

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

function TFHelpDialog.GetTitle(Url: string): string;
var
  Line: string;
  Posi: Integer;

  function InnerGetTitle: string;
  begin
    Delete(Line, 1, Posi + 6);
    Posi := Pos('</TITLE>', UpperCase(Line));
    if Posi = 0 then
      Posi := Pos(#10, Line);
    if Posi = 0 then
      Posi := Pos(#13, Line);
    if Posi = 0 then
      Posi := Length(Line);
    Delete(Line, Posi, Length(Line));
    Result:= Line;
  end;

begin
  Posi := Pos('#', Url);
  if Posi > 0 then
  begin
    Delete(Url, 1, Posi);
    Exit(Url);
  end;
  if FConfiguration.GlobalFileExists(Url, False) then begin
    var StringArr := TFile.ReadAllLines(Url);
    for var I := 0 to Length(StringArr) - 1 do
    begin
      Line := StringArr[I];
      Posi := Pos('<TITLE>', UpperCase(Line));
      if Posi > 0 then
        Exit(InnerGetTitle);
    end;
  end else
    Result := ExtractFileNameEx(Url);
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
          Str := GetPath(0) + Str;
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
    TVAPITree.Images := vilJDKHelpDark
  else
    TVAPITree.Images := vilJDKHelpLight;
end;

end.
