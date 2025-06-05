unit USetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, Registry, ShellLink, FmxUtils, IniFiles,
  {$WARNINGS OFF} FileCtrl {WARNINGS ON};

type
  TFSetup = class(TForm)
    RGRegOrIni: TRadioGroup;
    LHint1: TLabel;
    EUserfolder: TEdit;
    LHint2: TLabel;
    LFolder: TLabel;
    EFolder: TEdit;
    SBOrdner: TSpeedButton;
    SBUserIni: TSpeedButton;
    BInstall: TButton;
    BDeinstall: TButton;
    BClose: TButton;
    CBDesktopSymbol: TCheckBox;
    CBStartmenu: TCheckBox;
    BLanguage: TButton;
    ODSelect: TOpenDialog;
    CBPortableApplication: TCheckBox;
    procedure RGRegOrIniClick(Sender: TObject);
    procedure SBOrdnerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BInstallClick(Sender: TObject);
    procedure BDeinstallClick(Sender: TObject);
    procedure BLanguageClick(Sender: TObject);
    procedure CBPortableApplicationClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Dir: String;
    StartmenuProgramsDirectory: String;
    DesktopFolder: String;
    InitialLanguageCode: String;
    Error: boolean;
    Starting: boolean;
    FolderDialog: TFileOpenDialog;
    ShellLink: TShellLink;
    procedure DeleteRegistry;
    procedure copyAFile(From, _to: string);
    procedure CreateINIFile(filename: string);
    procedure DeleteAFile(comment, filename:String);
    procedure DeleteINIFiles(ToDir: string);
    procedure RegisterJavaFiles(path: string);
    function GetUsername: string;
    function ReplaceUsername(s: String): String;
    function GetUserdataFolder: string;
    function UnPortApp(s: string): string;
    procedure DeleteSetup;
    procedure RemoveDir(Path: string);
    procedure CreateDirectory(dir: string);
  public
    ToDir: String;
  end;

var
  FSetup: TFSetup;

implementation

uses UMemo, ShlObj, ShellApi, System.UITypes, IOUtils, JvGnugettext, UUpdater;

{$R *.DFM}

procedure TFSetup.FormCreate(Sender: TObject);
begin
  InitialLanguageCode:= GetCurrentLanguage;
  TranslateComponent(Self);
  BLanguage.Caption:= 'Language';
  FolderDialog:= TFileOpenDialog.Create(Self);
  FolderDialog.options:= [fdoPickFolders];
  ShellLink:= TShellLink.Create(Self);
  Error:= false;
  Starting:= true;
  StartmenuProgramsDirectory:= GetSpecialFolderPath(CSIDL_COMMON_STARTMENU);
  if StartmenuProgramsDirectory = '' then
    StartmenuProgramsDirectory:= GetSpecialFolderPath(CSIDL_STARTMENU);
  StartmenuProgramsDirectory:= StartmenuProgramsDirectory + '\Programs\';
  DesktopFolder:= GetSpecialFolderPath(CSIDL_COMMON_DESKTOPDIRECTORY);
  if DesktopFolder = '' then
    DesktopFolder:= GetSpecialFolderPath(CSIDL_DESKTOPDIRECTORY);
  DesktopFolder:= IncludeTrailingBackslash(DesktopFolder);
  Dir:= GetSpecialFolderPath(CSIDL_PROGRAM_FILES) + '\JavaEditor\';
  var s:= ParamStr(0);
  if Copy(s, 1, 2) = '\\' then // Server
    Dir:= ExtractFilePath(s);
  EFolder.text:= Dir;
  EUserFolder.Text:= GetUserdataFolder;
end;

procedure TFSetup.RGRegOrIniClick(Sender: TObject);
begin
  if RGRegOrIni.ItemIndex = 0 then begin
    CBPortableApplication.Checked:= false;
    EUserfolder.Enabled:= false;
    EUserfolder.Color  := clMenu;
    SBUserIni.Enabled   := false;
  end else begin
    EUserfolder.Enabled:= true;
    EUserfolder.Color  := clWindow;
    SBUserIni.enabled   := true;
  end
end;

procedure TFSetup.SBOrdnerClick(Sender: TObject);
begin
  if Sender = SBOrdner
    then FolderDialog.DefaultFolder:= Dir
    else FolderDialog.DefaultFolder:= ReplaceUsername(EUserfolder.Text);

  if FolderDialog.Execute then begin
    Dir:= IncludeTrailingBackslash(FolderDialog.Filename);
    if Sender = SBOrdner then begin
      if Pos('JavaEditor', Dir) = 0 then
        Dir:= Dir + 'JavaEditor\';
      EFolder.Text:= Dir;
      if CBPortableApplication.Checked then
        EUserFolder.Text:= Dir + 'Data\'
    end else
      EUserfolder.Text:= Dir;
  end;
end;

function TFSetup.GetUsername: string;
var
  UserName, s1: String;
  i: Integer;
  Laenge: LongWord;
begin
  Laenge:= 100;
  Username:= StringOfChar(' ', Laenge);
  WNetGetUser(Nil, PChar(Username), Laenge);
  s1:= '';
  i:= 1;
  while (Username[i] <> #0) do begin
    s1:= s1 + Username[i];
    inc(i);
  end;
  Result:= s1;
end;

function TFSetup.ReplaceUsername(s: String): String;
begin
  var p:= Pos('%USERNAME%', UpperCase(s));
  if p > 0 then begin
    Delete(s, p, 10);
    Insert(GetUsername, s, p);
  end;
  Result:= s;
end;

procedure TFSetup.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFSetup.CopyAFile(From, _to: string);
  var s: String;
begin
  if UpperCase(From) = UpperCase(_to) then Exit;
  FMemo.Output(Format(_('Copy %s to %s'), [From, _to]));
  if FileExists(_to) and not DeleteFile(_to) then begin
    s:= Format(_('"%s" can not be deleted'), [_to]);
    MessageDlg(s, mtError, [mbOK], 0);
    FMemo.Output(s);
    Error:= true;
    Exit;
  end;
  if not CopyFile(PChar(From), PChar(_to), false) then begin
    s:= Format(_('Error copying to "%s"'), [_to]);
    FMemo.Output(s);
    Error:= true;
  end;
end;

procedure TFSetup.CreateINIFile(filename: string);
begin
  FMemo.Output(_('Generate configuration file:') + ' ' + filename);
  var SL:= TStringList.Create;
  try
    SL.SaveToFile(filename);
  except
    var s:= Format(_('The configuration file %s could not be created!'), [filename]);
    MessageDlg(s, mtError, [mbOk], 0);
    FMemo.Output(s);
  end;
  FreeAndNil(SL);
end;

procedure TFSetup.CreateDirectory(dir: string);
begin
  if not DirectoryExists(dir) then begin
    FMemo.Output(_('Create directory:') + ' ' + dir);
    ForceDirectories(dir);
  end;
end;

procedure TFSetup.BInstallClick(Sender: TObject);
  var FromDir, UserDir, MachineINII: string;
      MyRegistry: TRegistry;
      IniDatei: TIniFile;
begin
  if not (CBPortableApplication.Checked or IsAdmin) and
    (MessageDlg(_('No admin rights. Continue?'), mtWarning, [mbNo, mbYes], 0) <> mrYes) then
    exit;
  if EFolder.Text = '' then begin
    MessageDlg(_('Please specify installation folder.'), mtError, [mbOK], 0);
    exit;
  end;
  FromDir := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));
  ToDir:= IncludeTrailingBackslash(EFolder.Text);
  CreateDirectory(ToDir);
  if not DirectoryExists(ToDir) then begin
    MessageDlg(Format(_('Installation folder %s does not exist.'), [ToDir]), mtError, [mbOK], 0);
    Exit;
  end;

  CreateDirectory(ToDir + 'docs');
  CreateDirectory(ToDir + 'img');
  CreateDirectory(ToDir + 'locale');
  CreateDirectory(ToDir + 'styles');
  CreateDirectory(ToDir + 'templates');

  if CBPortableApplication.Checked then begin
    CreateDirectory(ToDir + 'App\Cache\');
    CreateDirectory(ToDir + 'App\Temp\');
    CreateDirectory(ToDir + 'Java\');
  end;

  if CBPortableApplication.Checked or (RGRegOrIni.ItemIndex = 1) then begin
    if CBPortableApplication.Checked
      then UserDir:= ToDir + 'Data\'
      else UserDir:= ReplaceUsername(IncludeTrailingBackslash(EUserfolder.Text));
    CreateDirectory(UserDir);
    if not DirectoryExists(UserDir) then begin
      MessageDlg(_('Please specify folder for storing custom files.'), mtError, [mbOK], 0);
      exit;
    end;
  end;

  FMemo.Show;
  CopyAFile(FromDir + 'JavaEditor.exe', ToDir + 'JavaEditor.exe');
  if Error then begin
    MessageDlg(Format(_('Error copying to "%s"'), [ToDir]), mtError, [mbOK], 0);
    exit;
  end;
  CopyAFile(FromDir + 'JE2Java.exe', ToDir + 'JE2Java.exe');
  CopyAFile(FromDir + 'JEClasses.jar', ToDir + 'JEClasses.jar');
  CopyAFile(FromDir + 'java.policy.applet', ToDir + 'java.policy.applet');

  TDirectory.Copy(FromDir + '\locale', ToDir + '\locale');
  TDirectory.Copy(FromDir + '\docs', ToDir + '\docs');
  TDirectory.Copy(FromDir + '\templates', ToDir + '\templates');
  TDirectory.Copy(FromDir + '\img', ToDir + '\img');
  TDirectory.Copy(FromDir + '\styles', ToDir + '\styles');
  FMemo.MInstallation.lines.add(FUpdater.Memo.text);

  if DirectoryExists(ToDir + '\languages\') then
    RemoveDir(ToDir + '\languages\');

  CopyAFile(FromDir + 'WebView2Loader.dll', ToDir + 'WebView2Loader.dll');
  CopyAFile(FromDir + 'MicrosoftEdgeWebview2Setup.exe', ToDir + 'MicrosoftEdgeWebview2Setup.exe');
  if IsWebView2RuntimeNeeded() then
    ExecuteFile(FromDir + 'MicrosoftEdgeWebview2Setup.exe', '/silent /install', ToDir, SW_HIDE);

  CopyAFile(FromDir + 'Setup.exe', ToDir + 'Setup.exe');
  ShellLink.ProgramFile:= ToDir + 'JavaEditor.exe';
  if CBDesktopSymbol.Checked then begin
    ShellLink.LinkFile:= DesktopFolder + 'JavaEditor.lnk';
    FMemo.Output(_('Create desktop shortcut') + ' ' + ShellLink.LinkFile);
    ShellLink.Write;
  end;

  if CBStartmenu.Checked then begin
    ShellLink.LinkFile:= StartmenuProgramsDirectory + 'JavaEditor.lnk';
    if not DirectoryExists(StartmenuProgramsDirectory) then
      {$I-} MkDir(StartmenuProgramsDirectory); {$I+}
    FMemo.Output(_('Create entry in the programs menu') + ' ' + ShellLink.LinkFile);
    ShellLink.Write;
  end;

  if not CBPortableApplication.Checked then begin
    FMemo.Output(_('Create additional desktop shortcut for Java-Editor in folder') + ' ' + ToDir);
    ShellLink.LinkFile:= ToDir + 'JavaEditor.lnk';
    ShellLink.Write;
    RegisterJavaFiles(ToDir + 'JavaEditor.exe');
  end;

  if CBPortableApplication.Checked or (RGRegOrIni.ItemIndex = 1) then begin
    MachineINII:= ToDir + 'JEMachine.INI';
    CreateINIFile(MachineINII);
    if CBPortableApplication.Checked
      then EUserfolder.Text:= UserDir
      else EUserfolder.Text:= IncludeTrailingBackslash(EUserfolder.Text);

    FMemo.Output(_('save in it') + ' ' + EUserfolder.Text);
    IniDatei:= TIniFile.Create(MachineINII);
    IniDatei.WriteString('User', 'HomeDir', UnPortApp(EUserfolder.Text));
    IniDatei.WriteString('Java', 'Language', GetCurrentLanguage);
    IniDatei.WriteBool('Java', 'PortableApplication', CBPortableApplication.Checked);
    IniDatei.Free;
  end else begin
    MyRegistry:= TRegistry.Create;
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_WRITE;
      try
        OpenKey('\Software\JavaEditor\Java\', true);
        WriteString('Language', GetCurrentLanguage);
        WriteBool('PortableApplication', false);
      except
      end;
    end;
    MyRegistry.Free;
  end;

  FMemo.Output(_('Save installation flow in') + ' ' + ToDir + 'install.txt');
  try
    FMemo.MInstallation.Lines.SaveToFile(ToDir + 'install.txt');
  except
  end;
  if not Error then
    FMemo.Installation:= 0;

  FMemo.Output(_('Ready'));
end;

procedure TFSetup.DeleteRegistry;
  var Reg: TRegistry;

  procedure DeleteKeyRecursiv(Key: string);
  begin
    var SL:= TStringList.Create;
    with Reg do begin
      if OpenKey(Key, false) then begin
        GetKeyNames(SL);
        for var i:= 0 to SL.Count-1 do
          DeleteKeyRecursiv(Key + '\' + SL.strings[i]);
        SL.Clear;
      end;
      if OpenKey(Key, false) then begin
        GetValueNames(SL);
        for var i:= 0 to SL.Count-1 do
          DeleteValue(Key + '\' + SL.strings[i]);
      end;    
      DeleteKey(Key);
    end;
    SL.Free;
  end;

begin
  Reg:= TRegistry.Create;
  with Reg do begin
    try
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_CURRENT_USER;
      DeleteKeyRecursiv('\Software\JavaEditor');

      RootKey:= HKEY_LOCAL_MACHINE;
      DeleteKeyRecursiv('\Software\JavaEditor');

      RootKey:= HKEY_CLASSES_ROOT;
      if OpenKey('\.java', false) and (ReadString('') = 'JavaEditor')
        then WriteString('', '');
      DeleteKeyRecursiv('\JavaEditor');
    finally
      Reg.Free;
    end;
  end;
end;

procedure TFSetup.DeleteAFile(comment, filename: String);
begin
  if not FileExists(filename) then exit;
  FMemo.Output(_('Delete') + ' ' + comment + ' ' + filename);
  {$W-} var Attribute:= FileGetAttr(filename); {$W+}
  if (Attribute <> -1) and (Attribute and faReadOnly <> 0) then
    FileSetAttr(filename, 0);
  if not DeleteFile(filename) then
    FMemo.Output(Format(_('"%s" can not be deleted'), [filename]));
end;

procedure TFSetup.DeleteINIFiles(ToDir: string);
  var UserName, s1: String;
      p, i: Integer;
      Laenge: LongWord;
      IniDatei: TIniFile;
      MachineINI, UserDir: string;
begin
  ToDir:= IncludeTrailingBackslash(ToDir);
  MachineINI:= ToDir + 'JEMachine.ini';
  if FileExists(MachineINI) then begin
    IniDatei:= TIniFile.Create(MachineINI);
    UserDir:= IniDatei.ReadString('User', 'HomeDir', '<nix>');
    IniDatei.Free;

    p:= Pos('%USERNAME%', Uppercase(UserDir));
    if p > 0 then begin
      Laenge:= 100;
      Username:= StringOfChar(' ', Laenge);
      WNetGetUser(Nil, PChar(Username), Laenge);
      s1:= '';
      i:= 1;
      while (Username[i] <> #0) do begin
        s1:= s1 + Username[i];
        inc(i);
      end;
      Delete(UserDir, p, 10);
      Insert(s1, UserDir, p);
    end;
    UserDir:= IncludeTrailingBackslash(UserDir);
    var s:= _('configuration file');
    DeleteAFile(s, UserDir + 'JEUser.ini');
    DeleteAFile(s, UserDir + 'JEJavaCol.ini');
    DeleteAFile(s, UserDir + 'JEHTMLCol.ini');
    DeleteAFile(s, MachineINI);
  end;
end;

procedure TFSetup.BDeinstallClick(Sender: TObject);
begin
  if not (CBPortableApplication.Checked or IsAdmin) and
    (MessageDlg(_('No admin rights. Continue?'), mtWarning, [mbNo, mbYes], 0) <> mrYes) then
    exit;
  ToDir:= TPath.GetDirectoryName(ParamStr(0));
  FMemo.Installation:= 1;
  FMemo.Show;
  FMemo.MInstallation.Text:= 'Deinstallation'#13#10;
  DeleteAFile(_('desktop shortcut'), DesktopFolder + 'JavaEditor.lnk');
  RemoveDir(StartmenuProgramsDirectory + 'JavaEditor');
  var Dirs:= TDirectory.GetDirectories(ToDir);
  for var i := 0 to Length(Dirs) - 1 do
    RemoveDir(Dirs[i]);
  var Files:= TDirectory.GetFiles(ToDir);
  for var i := 0 to Length(Files) - 1 do
    DeleteAFile('', Files[i]);
  FMemo.Output(_('Delete registry entries'));
  DeleteRegistry;
  FMemo.Output(_('Delete INI files'));
  DeleteINIFiles(ToDir);
  DeleteSetup;
  FMemo.Output(_('Ready'));
end;

procedure TFSetup.BLanguageClick(Sender: TObject);
begin
  if getCurrentLanguage = InitialLanguageCode
    then UseLanguage('en')
    else UseLanguage(InitialLanguageCode);
  RetranslateComponent(self);
  RetranslateComponent(FUpdater);
  BLanguage.Caption:= 'Language';
end;

procedure TFSetup.Button1Click(Sender: TObject);
begin
  FUpdater.MakeUpdate;
end;

procedure TFSetup.CBPortableApplicationClick(Sender: TObject);
begin
  if CBPortableApplication.Checked then begin
    RGRegOrIni.ItemIndex:= 1;
    EUserfolder.Enabled := false;
    EUserfolder.Color   := clMenu;
    SBUserIni.Enabled   := false;
  end else
    RGRegOrIniClick(Self);
  EUserfolder.Text:= GetUserdataFolder;
end;

function TFSetup.GetUserdataFolder: string;
  var s, user: string; p: integer;
begin
  if CBPortableApplication.Checked then
    s:= EFolder.Text + 'Data' + TPath.DirectorySeparatorChar
  else begin
    s:= TPath.GetHomePath + TPath.DirectorySeparatorChar +
          'JavaEditor' + TPath.DirectorySeparatorChar;
    user:= GetUserName;
    p:= Pos(user, s);
    if p > 0 then begin
      Delete(s, p, Length(user));
      Insert('%USERNAME%', s, p);
    end
  end;
  Result:= s;
end;

function TFSetup.UnPortApp(s: string): string;
  var SL1, SL2: TStringList;
      i, j: integer;
begin
  Result:= s;
  if CBPortableApplication.Checked and (s <> '') and
     (Uppercase(copy(ToDir, 1, 2)) = Uppercase(copy(s, 1, 2)))  // same drive
  then begin
    SL1:= Split('\', withoutTrailingSlash(ToDir));
    SL2:= Split('\', withoutTrailingSlash(s));
    i:= 0;
    while (i < SL1.Count) and (i < SL2.Count) and
          (Uppercase(SL1.Strings[i]) = Uppercase(SL2.Strings[i])) do
      inc(i);
    Result:= '';
    j:= i;
    while i < SL1.Count do begin
      Result:= Result + '..\';
      inc(i);
    end;
    while j < SL2.Count do begin
      Result:= Result + SL2.Strings[j] + '\';
      inc(j);
    end;
    Result:= withoutTrailingSlash(Result);
    FreeAndNil(SL1);
    FreeAndNil(SL2);
  end;
end;

procedure TFSetup.RegisterJavaFiles(path: string);
  var Reg: TRegistry; Filename: string;

  procedure WriteToRegistry(Key: String);
  begin
    with Reg do begin
      OpenKey(Key, true);
      WriteString('', 'Java-Editor');
      CloseKey;
      OpenKey(Key + '\DefaultIcon', true);
      WriteString('', HideBlanks(path) + ',0');
      CloseKey;
      OpenKey(Key + '\Shell\Open\command', true);
      WriteString('', HideBlanks(path) + ' "%1"');
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec', true);
      WriteString('', '[FileOpen("%1")]');
      OpenKey('Application', TRUE);
      FileName := ExtractFileName(path);
      FileName := Copy(FileName, 1, Length(FileName)-4);
      WriteString('', Filename);
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec\topic', true);
      WriteString('', 'System');
      CloseKey;
    end;
  end;

begin
  try
    Reg:= TRegistry.Create;
    with Reg do begin
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_LOCAL_MACHINE;
      WriteToRegistry('Software\Classes\JavaEditor');
      RootKey:= HKEY_CURRENT_USER;
      WriteToRegistry('\Software\Classes\JavaEditor');

      OpenKey('\SOFTWARE\Classes\.java', true);
      WriteString('', 'JavaEditor');
      OpenKey('\Software\Classes\.jfm', true);
      WriteString('', 'JavaEditor');
      OpenKey('\Software\Classes\.uml', true);
      WriteString('', 'JavaEditor');
      OpenKey('\Software\Classes\.jep', true);
      WriteString('', 'JavaEditor');
      OpenKey('\Software\Classes\.jsg', true);
      WriteString('', 'JavaEditor');
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFSetup.DeleteSetup;
  var SL: TStringList; Filename, _to: String;
begin
  Filename:= GetTempDir + 'RunJava.bat';
  SL:= TStringList.Create;
  _to:= HideBlanks(ToDir + 'setup.exe');
  SL.Add('echo on');
  SL.Add(':1');
  SL.Add('del ' + _to);
  SL.Add('if exist ' + _to + ' goto 1');
  SL.Add('rmdir ' + HideBlanks(ToDir));
  SL.SaveToFile(Filename);
  FreeAndNil(SL);
  ShellExecute(Application.MainForm.Handle, 'open',
       PChar(FileName), '', '.', SW_HIDE);
end;

procedure TFSetup.RemoveDir(Path: string);
begin
  if TDirectory.Exists(Path) then
    TDirectory.Delete(Path, true);
end;

end.