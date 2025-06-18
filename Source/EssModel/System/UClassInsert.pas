unit UClassInsert;

interface

uses
  Zip,
  Classes,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Vcl.Controls;

type
  TFClassInsert = class(TForm)
    PListView: TPanel;
    PButton: TPanel;
    BCancel: TButton;
    BOK: TButton;
    LVFiles: TListView;
    CBArchiv: TComboBox;
    BOpenArchive: TButton;
    ODClassInsert: TOpenDialog;
    RGOptions: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure LVFilesColumnClick(Sender: TObject; Column: TListColumn);
    procedure LVFilesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure BOpenArchiveClick(Sender: TObject);
    procedure CBArchivSelect(Sender: TObject);
    procedure LVFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FaListItem: TListItem;
    FColumnToSort: Integer;
    FJavaClassJar: TZipFile;
    FJavaSrcZip: TZipFile;
    FJavaZipJar: TZipFile;
    FLastSorted: Integer;
    FSortDir: Integer;
  public
    procedure ActivateListView;
    procedure Open(Str: string);
    function GetIndex: Integer;
    function GetFirstSelectedIndex: Integer;
    function GetNextSelectedIndex: Integer;
    procedure GetSelectedFiles(StringList: TStringList);
    procedure AddComboBox(Int: Integer; const Str: string);
    procedure SaveWindow;
    procedure ListViewBeepOff;
    function ExtractSystemClass(AClassname: string): string;
    function OpenOrExtractClass(BoxFiles: TStringList;
      AClassname: string): string;
    function HasSourceInZip(AClassname: string): Integer;
    function HasClassInJar(AClassname: string): Integer;
    procedure ZipClose;
    property AListItem: TListItem read FaListItem;
    property JavaClassJar: TZipFile read FJavaClassJar;
    property JavaSrcZip: TZipFile read FJavaSrcZip;
    property JavaZipJar: TZipFile read FJavaZipJar;
  end;

var
  FClassInsert: TFClassInsert;

implementation

uses
  Registry,
  StrUtils,
  Windows,
  Messages,
  SysUtils,
  JvGnugettext,
  UUtils,
  UConfiguration,
  UStringRessources;

{$R *.dfm}

procedure TFClassInsert.FormCreate(Sender: TObject);
var
  Str, Str1: string;
begin
  TranslateComponent(Self);
  Visible := False;
  FJavaZipJar := TZipFile.Create;
  FJavaSrcZip := nil;
  FJavaClassJar := nil;
  LVFiles.Columns[0].Caption := _(LNGClass);
  LVFiles.Columns[1].Caption := _('Package');
  Str := FConfiguration.ReadStringU('ClassInsert', 'CBArchivItems', '');
  CBArchiv.Items.Text := LoadComboBoxItems(Str);
  Str1 := FConfiguration.ReadStringU('ClassInsert', 'CBArchivText', '');
  Str := FConfiguration.JDKFolder + '\src.zip';
  if not FileExists(Str) then
    Str := FConfiguration.JDKFolder + '\jre\lib\rt.jar';
  if FileExists(Str1) then
    Str := Str1;
  CBArchiv.Text := Str;
  Open(Str);
end;

procedure TFClassInsert.FormShow(Sender: TObject);
begin
  ListViewBeepOff;
  if FileExists(CBArchiv.Text) then
    Open(CBArchiv.Text);
  ActivateListView;
end;

procedure TFClassInsert.Open(Str: string);
var
  JPos, KPos: Integer;
  Str1, Str2: string;
begin
  if not FileExists(Str) then
    Exit;
  LVFiles.Clear;
  LVFiles.SortType := stNone;
  try
    Screen.Cursor := crHourGlass;
    try
      KPos := 0;
      FJavaZipJar.Open(Str, zmRead);
      for var I := 0 to FJavaZipJar.FileCount - 1 do
      { if not FJavaZipJar.Entries.Items[i].IsFolder then } begin
        Str := FJavaZipJar.FileNames[I];
        JPos := LastDelimiter('/', Str);
        Str1 := Copy(Str, JPos + 1, Length(Str));
        Str2 := GetShortType(Str1);
        if (Str2 = 'class') or (Str2 = 'java') then
        begin
          LVFiles.AddItem(ChangeFileExt(Str1, ''), nil);
          Str1 := StringReplace(Copy(Str, 1, JPos - 1), '/', '.',
            [rfReplaceAll]);
          LVFiles.Items[KPos].SubItems.Add(Str1);
          LVFiles.Items[KPos].SubItems.Add(IntToStr(I));
          Inc(KPos);
        end;
      end;
      LVFiles.SortType := stText;
      LVFiles.ItemIndex := 0;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFClassInsert.ActivateListView;
begin
  LVFiles.ClearSelection;
  // Activate LVFiles for keyboard-input
  // See: http://msdn.microsoft.com/en-us/library/ms645607(VS.85).aspx
  PostMessage(LVFiles.Handle, WM_LBUTTONDOWN, MK_LBUTTON, 50 + 32768 * 50);
end;

procedure TFClassInsert.FormDestroy(Sender: TObject);
begin
  FJavaZipJar.Close;
  FreeAndNil(FJavaZipJar);
  if Assigned(FJavaSrcZip) then
  begin
    FJavaSrcZip.Close;
    FreeAndNil(FJavaSrcZip);
  end;
  if Assigned(FJavaClassJar) then
  begin
    FJavaClassJar.Close;
    FreeAndNil(FJavaClassJar);
  end;
end;

procedure TFClassInsert.ListViewBeepOff;
begin
  // http://groups.google.com.au/group/microsoft.public.win32.programmer.gdi/browse_thread/thread/99a2b1f68b47c4d5/681bf4f839f72ee8
  var
  MyRegistry := TRegistry.Create;
  MyRegistry.RootKey := HKEY_CURRENT_USER;
  MyRegistry.Access := KEY_WRITE;
  try
    MyRegistry.OpenKey
      ('AppEvents\Schemes\Apps\.Default\CCSelect\.current\', False);
    MyRegistry.WriteString('', '');
  finally
    MyRegistry.CloseKey;
  end;
  FreeAndNil(MyRegistry);
end;

procedure TFClassInsert.AddComboBox(Int: Integer; const Str: string);
begin
  CBArchiv.Text := Str;
  if CBArchiv.Items.IndexOf(Str) = -1 then
  begin
    CBArchiv.Items.Insert(Int, Str);
    if CBArchiv.Items.Count >= 8 then
      CBArchiv.Items.Delete(7);
  end;
end;

procedure TFClassInsert.BOpenArchiveClick(Sender: TObject);
begin
  ODClassInsert.InitialDir := ExtractFilePath(CBArchiv.Text);
  if ODClassInsert.Execute then
  begin
    if (ExtractFileExt(ODClassInsert.FileName) = '.jar') and
      (Pos(ODClassInsert.FileName, FConfiguration.JavaClasspathUser) = 0) then
      FConfiguration.JavaClasspathUser := FConfiguration.JavaClasspathUser + ';'
        + ODClassInsert.FileName;
    try
      Open(ODClassInsert.FileName);
    except
      on E: Exception do
      begin
        ErrorMsg(E.Message);
        CBArchivSelect(Self);
      end;
    end;
    AddComboBox(0, ODClassInsert.FileName);
  end;
end;

procedure TFClassInsert.CBArchivSelect(Sender: TObject);
begin
  if FileExists(CBArchiv.Text) then
    Open(CBArchiv.Text)
  else
    ErrorMsg(Format(_(LNGFileNotFound), [CBArchiv.Text]));
end;

function TFClassInsert.GetIndex: Integer;
var
  Str: string;
  Int: Integer;
begin
  Result := -1;
  if Assigned(FaListItem) and TryStrToInt(FaListItem.SubItems[1], Result) then
  begin
    Str := FJavaZipJar.FileNames[Result];
    // prefer .java-file if it exists
    if Pos('.class', Str) > 0 then
    begin
      Str := ChangeFileExt(Str, '.java');
      Int := FJavaZipJar.IndexOf(Str);
      if Int > -1 then
        Result := Int;
    end;
  end;
end;

function TFClassInsert.GetFirstSelectedIndex: Integer;
begin
  FaListItem := LVFiles.Selected;
  Result := GetIndex;
end;

function TFClassInsert.GetNextSelectedIndex: Integer;
begin
  FaListItem := LVFiles.GetNextItem(FaListItem, sdBelow, [isSelected]);
  Result := GetIndex;
end;

procedure TFClassInsert.GetSelectedFiles(StringList: TStringList);
var
  CName, Str, SRelative: string;
  Int: Integer;
begin
  Int := GetFirstSelectedIndex;

  while Int > -1 do
  begin
    Str := FJavaZipJar.FileNames[Int];
    CName := ChangeFileExt(ReplaceStr(Str, '/', '.'), '');
    if FConfiguration.IsAPIClassOrInterface(CName) then
      Str := ExtractSystemClass(CName)
    else
    begin
      SRelative := Str;
      Str := FConfiguration.JavaCache + '\' + StringReplace(Str, '/', '\',
        [rfReplaceAll]);
      if not FileExists(Str) then
      begin
        ForceDirectories(ExtractFilePath(Str));
        FJavaZipJar.Extract(Int, FConfiguration.JavaCache);
      end;
    end;
    StringList.Add(Str);
    Int := GetNextSelectedIndex;
  end;
end;

procedure TFClassInsert.LVFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  if FColumnToSort = FLastSorted then
    FSortDir := 1 - FSortDir
  else
    FSortDir := 0;
  FLastSorted := FColumnToSort;
  (Sender as TCustomListView).AlphaSort;
end;

procedure TFClassInsert.LVFilesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  if FColumnToSort = 0 then
  begin
    if FSortDir = 0 then
      Compare := CompareText(Item1.Caption, Item2.Caption)
    else
      Compare := CompareText(Item2.Caption, Item1.Caption);
  end // if FColumnToSort = 0 then
  else
  begin
    var
    ix := FColumnToSort - 1;
    if FSortDir = 0 then
      Compare := CompareText(Item1.SubItems[ix], Item2.SubItems[ix])
    else
      Compare := CompareText(Item2.SubItems[ix], Item1.SubItems[ix]);
  end;
end;

procedure TFClassInsert.LVFilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = 65) then
    LVFiles.SelectAll;
end;

procedure TFClassInsert.SaveWindow;
begin
  FConfiguration.WriteStringU('ClassInsert', 'CBArchivItems',
    SaveComboBoxItems(CBArchiv.Items.Text));
  FConfiguration.WriteStringU('ClassInsert', 'CBArchivText', CBArchiv.Text);
end;

function TFClassInsert.OpenOrExtractClass(BoxFiles: TStringList;
  AClassname: string): string;
begin
  AClassname := WithoutGeneric(AClassname);
  if Pos('.', AClassname) = 0 then
  begin
    for var I := 0 to BoxFiles.Count - 1 do
    begin
      Result := ExtractFilePath(BoxFiles[I]) + '\' + AClassname + '.java';
      if FileExists(Result) then
        Exit;
      Result := ChangeFileExt(Result, '.class');
      if FileExists(Result) then
        Exit;
    end;
    Result := FConfiguration.JavaCache + '\' + AClassname + '.java';
    if FileExists(Result) then
      Exit;
  end;
  Result := '';

  if FConfiguration.IsAPIClassOrInterface(AClassname) then
    Result := ExtractSystemClass(AClassname)
  else
  begin
    AClassname := StringReplace(AClassname, '.', '\', [rfReplaceAll]) + '.java';
    var
    Str := FConfiguration.JavaCache + '\' + AClassname;
    if FileExists(Str) then
      Result := Str;
  end;
end;

function TFClassInsert.ExtractSystemClass(AClassname: string): string;
var
  Int: Integer;
begin
  Result := '';
  AClassname := StringReplace(AClassname, '.', '\', [rfReplaceAll]) + '.java';
  var
  Str := FConfiguration.JavaCache + '\' + AClassname;
  if not FileExists(Str) then
  begin
    ForceDirectories(ExtractFilePath(Str));
    Int := HasSourceInZip(AClassname);
    if Int > -1 then
      FJavaSrcZip.Extract(Int, FConfiguration.JavaCache);
  end;
  if FileExists(Str) then
    Result := Str
  else
  begin
    Str := ChangeFileExt(Str, '.class');
    if not FileExists(Str) then
    begin
      AClassname := ChangeFileExt(AClassname, '.class');
      Int := HasClassInJar(AClassname);
      if Int > -1 then
        FJavaClassJar.Extract(Int, FConfiguration.JavaCache);
    end;
    if FileExists(Str) then
      Result := Str;
  end;
end;

function TFClassInsert.HasSourceInZip(AClassname: string): Integer;
begin
  AClassname := ReplaceStr(AClassname, '\', '/');
  if FJavaSrcZip = nil then
    try
      FJavaSrcZip := TZipFile.Create;
      FJavaSrcZip.Open(FConfiguration.JDKFolder + '\src.zip', zmRead);
    except
      on E: Exception do
      begin
        ErrorMsg(E.Message);
        FJavaSrcZip := nil;
      end;
    end;
  if Assigned(FJavaSrcZip) then
    Result := FJavaSrcZip.IndexOf(AClassname)
  else
    Result := -1;
end;

function TFClassInsert.HasClassInJar(AClassname: string): Integer;
begin
  AClassname := ReplaceStr(AClassname, '\', '/');
  if FJavaClassJar = nil then
    try
      FJavaClassJar := TZipFile.Create;
      // FJavaClassJar.OverwriteAction:= oaOverwriteAll;
      FJavaClassJar.Open(FConfiguration.JDKFolder + '\jre\lib\rt.jar', zmRead);
    except
      on E: Exception do
      begin
        ErrorMsg(E.Message);
        FJavaClassJar := nil;
      end;
    end;
  if Assigned(FJavaClassJar) then
    Result := FJavaClassJar.IndexOf(AClassname)
  else
    Result := -1;
end;

procedure TFClassInsert.ZipClose;
begin
  FJavaZipJar.Close;
  LVFiles.Clear;
end;

end.
