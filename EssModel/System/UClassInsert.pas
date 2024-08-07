unit UClassInsert;

interface

uses
  Zip, Classes, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Vcl.Controls;

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
    ColumnToSort: Integer;
    LastSorted: Integer;
    SortDir: Integer;
  public
    JavaZipJar: TZipFile;
    JavaSrcZip: TZipFile;
    JavaClassJar: TZipFile;
    aListItem: TListItem;
    procedure ActivateListView;
    procedure Open(s: string);
    function GetIndex: integer;
    function GetFirstSelectedIndex: integer;
    function GetNextSelectedIndex: integer;
    procedure GetSelectedFiles(SL: TStringList);
    procedure AddComboBox(i: integer; const s: string);
    procedure SaveWindow;
    procedure ListViewBeepOff;
    function  ExtractSystemClass( aClassname: string): string;
    function OpenOrExtractClass(BoxFiles: TStringList; aClassname: string): string;
    function HasSourceInZip(aClassname: string): integer;
    function HasClassInJar(aClassname: string): integer;
    procedure ZipClose;
  end;

var FClassInsert: TFClassInsert;

implementation

uses Registry, StrUtils, Windows, Messages, SysUtils,
     UUtils, UConfiguration, JvGnugettext, UStringRessources;

{$R *.dfm}

procedure TFClassInsert.FormCreate(Sender: TObject);
  var s, s1: string;
begin
  TranslateComponent(Self);
  Visible:= false;
  JavaZipJar:= TZipFile.Create;
  JavaSrcZip:= nil;
  JavaClassJar:= nil;
  LVFiles.Columns[0].Caption:= _(LNGClass);
  LVFiles.Columns[1].Caption:= _('Package');
  s:= FConfiguration.ReadStringU('ClassInsert', 'CBArchivItems', '');
  CBArchiv.Items.Text:= LoadComboBoxItems(s);
  s1:= FConfiguration.ReadStringU('ClassInsert', 'CBArchivText', '');
  s:= FConfiguration.JDKFolder + '\src.zip';
  if not FileExists(s) then
    s:= FConfiguration.JDKFolder + '\jre\lib\rt.jar';
  if FileExists(s1) then
    s:= s1;
  CBArchiv.Text:= s;
  Open(s);
end;

procedure TFClassInsert.FormShow(Sender: TObject);
begin
  ListViewBeepOff;
  if FileExists(CBArchiv.Text) then
    Open(CBArchiv.Text);
  ActivateListView;
end;

procedure TFClassInsert.Open(s: string);
  var i, j, k: integer; s1, s2: string;
begin
  if not FileExists(s) then exit;
  LVFiles.Clear;
  LVFiles.SortType:= stNone;
  try
    Screen.Cursor:= crHourglass;
    try
      k:= 0;
      JavaZipJar.Open(s, zmRead);
      for i:= 0 to JavaZipJar.FileCount - 1 do
        {if not JavaZipJar.Entries.Items[i].IsFolder then }begin
          s:= JavaZipJar.FileNames[i];
          j:= LastDelimiter('/', s);
          s1:= copy(s, j+1, length(s));
          s2:= GetShortType(s1);
          if (s2 = 'class') or (s2 = 'java') then begin
            LVFiles.AddItem(ChangeFileExt(s1, ''), nil);
            s1:= StringReplace(copy(s, 1, j-1), '/', '.', [rfReplaceAll]);
            LVFiles.Items[k].SubItems.Add(s1);
            LVFiles.Items[k].SubItems.Add(IntToStr(i));
            inc(k);
          end;
        end;
      LVFiles.SortType:= stText;
      LVFiles.ItemIndex:= 0;
    except on e: exception do
      ErrorMsg(e.Message);
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFClassInsert.ActivateListView;
begin
  LVFiles.ClearSelection;
  // Activate LVFiles for keyboard-input
  // See: http://msdn.microsoft.com/en-us/library/ms645607(VS.85).aspx
  PostMessage(LVFiles.Handle, WM_LBUTTONDOWN, MK_LBUTTON, 50 + 32768*50);
end;

procedure TFClassInsert.FormDestroy(Sender: TObject);
begin
  try
    JavaZipJar.Close;
    FreeAndNil(JavaZipJar);
    if assigned(JavaSrcZip) then begin
      JavaSrcZip.Close;
      FreeAndNil(JavaSrcZip);
    end;
    if assigned(JavaClassJar) then begin
      JavaClassJar.Close;
      FreeAndNil(JavaClassJar);
    end;
  except
  end;
end;

procedure TFClassInsert.ListViewBeepOff;
begin
  // http://groups.google.com.au/group/microsoft.public.win32.programmer.gdi/browse_thread/thread/99a2b1f68b47c4d5/681bf4f839f72ee8
  var MyRegistry:= TRegistry.Create;
  MyRegistry.RootKey:= HKEY_CURRENT_USER;
  MyRegistry.Access:= KEY_Write;
  try
    try
      MyRegistry.OpenKey('AppEvents\Schemes\Apps\.Default\CCSelect\.current\', false);
      MyRegistry.WriteString('', '')
    except
    end;
  finally
    MyRegistry.CloseKey;
  end;
  FreeAndNil(MyRegistry);
end;

procedure TFClassInsert.AddComboBox(i: integer; const s: string);
begin
  CBArchiv.Text:= s;
  if CBArchiv.Items.IndexOf(s) = -1 then begin
    try
      CBArchiv.Items.Insert(i, s);
    except
    end;
    if CBArchiv.Items.Count >= 8 then
      CBArchiv.Items.Delete(7);
  end;
end;

procedure TFClassInsert.BOpenArchiveClick(Sender: TObject);
begin
  ODClassInsert.InitialDir:= ExtractFilePath(CBArchiv.Text);
  if ODClassInsert.Execute then begin
    if (ExtractFileExt(ODClassInsert.FileName) = '.jar') and
      (Pos(ODClassInsert.Filename, FConfiguration.JavaClasspathUser) = 0)
    then
      FConfiguration.JavaClasspathUser:=
        FConfiguration.JavaClasspathUser + ';' + ODClassInsert.FileName;
    try
      Open(ODClassInsert.Filename);
    except
      on E: Exception do begin
        ErrorMsg(E.Message);
        CBArchivSelect(Self);
      end;
    end;
    AddComboBox(0, ODClassInsert.Filename);
  end;
end;

procedure TFClassInsert.CBArchivSelect(Sender: TObject);
begin
  if FileExists(CBArchiv.Text)
    then Open(CBArchiv.Text)
    else ErrorMsg(Format(_(LNGFileNotFound), [CBArchiv.Text]));
end;

function TFClassInsert.GetIndex: integer;
  var s: string; i: integer;
begin
  Result:= -1;
  if Assigned(aListItem) and TryStrToInt(aListItem.SubItems[1], Result) then begin
    s:= JavaZipJar.Filenames[Result];
    // prefer .java-file if it exists
    if Pos('.class', s) > 0 then begin
      s:= ChangeFileExt(s, '.java');
      i:= JavaZipJar.IndexOf(s);
      if i > -1 then Result:= i;
    end;
  end;
end;

function TFClassInsert.GetFirstSelectedIndex: integer;
begin
  aListItem:= LVFiles.Selected;
  Result:= GetIndex;
end;

function TFClassInsert.GetNextSelectedIndex: integer;
begin
  aListItem:= LVFiles.GetNextItem(aListItem, sdBelow, [isSelected]);
  Result:= GetIndex;
end;

procedure TFClassInsert.GetSelectedFiles(SL: TStringList);
  var CName, s, srelative: string; i: integer;
begin
  i:= GetFirstSelectedIndex;

  while i > -1 do begin
    s:= JavaZipJar.Filenames[i];
    CName:= ChangeFileExt(ReplaceStr(s, '/', '.'), '');
    if FConfiguration.IsAPIClassOrInterface(CName) then
      s:= ExtractSystemClass(CName)
    else begin
      srelative:= s;
      s:= FConfiguration.JavaCache + '\' + StringReplace(s, '/', '\', [rfReplaceAll]);
      if not FileExists(s) then begin
        ForceDirectories(ExtractFilePath(s));
        JavaZipJar.Extract(i, FConfiguration.JavaCache);
      end;
    end;
    SL.Add(s);
    i:= GetNextSelectedIndex;
  end;
end;

procedure TFClassInsert.LVFilesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  ColumnToSort:= Column.Index;
  if ColumnToSort = LastSorted then
    SortDir:= 1 - SortDir
  else
    SortDir:= 0;
  LastSorted := ColumnToSort;
  (Sender as TCustomListView).AlphaSort;
end;

procedure TFClassInsert.LVFilesCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
begin
  if ColumnToSort = 0 then begin
    if SortDir = 0 then
      Compare := CompareText(Item1.Caption,Item2.Caption)
    else
      Compare := CompareText(Item2.Caption,Item1.Caption);
  end //if ColumnToSort = 0 then
  else
  begin
   var ix := ColumnToSort - 1;
   if SortDir = 0 then
     Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix])
   else
     Compare := CompareText(Item2.SubItems[ix],Item1.SubItems[ix]);
  end;
end;

procedure TFClassInsert.LVFilesKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = 65) then LVFiles.SelectAll;
end;

procedure TFClassInsert.SaveWindow;
begin
  FConfiguration.WriteStringU('ClassInsert', 'CBArchivItems',
    SaveComboBoxItems(CBArchiv.Items.Text));
  FConfiguration.WriteStringU('ClassInsert', 'CBArchivText', CBArchiv.Text);
end;

function TFClassInsert.OpenOrExtractClass(BoxFiles: TStringList; aClassname: string): string;
begin
  aClassname:= WithoutGeneric(aClassname);
  if Pos('.', aClassname) = 0 then begin
    for var i:= 0 to BoxFiles.Count - 1 do begin
      Result:= ExtractFilePath(BoxFiles[i]) + '\' + aClassname + '.java';
      if FileExists(Result) then exit;
      Result:= ChangeFileExt(Result, '.class');
      if FileExists(Result) then exit;
    end;
    Result:= FConfiguration.JavaCache + '\' + aClassname + '.java';
    if FileExists(Result) then exit;
  end;
  Result:= '';

  if FConfiguration.IsAPIClassOrInterface(aClassname) then
    Result:= ExtractSystemClass(aClassname)
  else begin
    aClassname:= StringReplace(aClassname, '.', '\', [rfReplaceAll]) + '.java';
    var s:= FConfiguration.JavaCache + '\' + aClassname;
    if FileExists(s) then Result:= s;
  end;
end;

function TFClassInsert.ExtractSystemClass(aClassname: string): string;
  var i: integer;
begin
  Result:= '';
  aClassname:= StringReplace(aClassname, '.', '\', [rfReplaceAll]) + '.java';
  var s:= FConfiguration.JavaCache + '\' + aClassname;
  if not FileExists(s) then begin
    ForceDirectories(ExtractFilePath(s));
    i:= HasSourceInZip(aClassname);
    if i > -1 then
      JavaSrcZip.Extract(i, FConfiguration.JavaCache);
  end;
  if FileExists(s)
    then Result:= s
  else begin
    s:= ChangeFileExt(s, '.class');
    if not FileExists(s) then begin
      aClassname:= ChangeFileExt(aClassname, '.class');
      i:= HasClassInJar(aClassname);
      if i > -1 then
        JavaClassJar.Extract(i, FConfiguration.JavaCache);
    end;
    if FileExists(s) then Result:= s;
  end;
end;

function TFClassInsert.HasSourceInZip(aClassname: string): integer;
begin
  aClassname:= ReplaceStr(aClassname, '\', '/');
  if JavaSrcZip = nil then
    try
      JavaSrcZip:= TZipFile.Create;
      JavaSrcZip.Open(FConfiguration.JDKFolder + '\src.zip', zmRead);
    except
      on E: Exception do begin
        ErrorMsg(E.Message);
        JavaSrcZip:= nil;
      end;
    end;
  if assigned(JavaSrcZip)
    then Result:= JavaSrcZip.IndexOf(aClassname)
    else Result:= -1;
end;

function TFClassInsert.HasClassInJar(aClassname: string): integer;
begin
  aClassname:= ReplaceStr(aClassname, '\', '/');
  if JavaClassJar = nil then
    try
      JavaClassJar:= TZipFile.Create;
      //JavaClassJar.OverwriteAction:= oaOverwriteAll;
      JavaClassJar.Open(FConfiguration.JDKFolder + '\jre\lib\rt.jar', zmRead);
    except
      on E: Exception do begin
        ErrorMsg(E.Message);
        JavaClassJar:= nil;
      end;
    end;
  if assigned(JavaClassJar)
    then Result:= JavaClassJar.IndexOf(aClassname)
    else Result:= -1;
end;

procedure TFClassInsert.ZipClose;
begin
  JavaZipJar.Close;
  LVFiles.Clear;
end;

end.
