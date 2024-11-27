unit UUpdater;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Zip, ShlObj, Registry, IniFiles, ExtCtrls;

type
  TFUpdater = class(TForm)
    Memo: TMemo;
    Panel1: TPanel;
    BOK: TButton;
    BRetry: TButton;
    procedure BOKClick(Sender: TObject);
    procedure BRetryClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Source: string;
    Dest: string;
    Version: string;
    UseRegistry: boolean;
    MyRegistry: TRegistry;
    MachineIniFile: TMemIniFile;
    PortableApplication: boolean;
    PortAppDrive: String;
    procedure CopyFromTo(f1, f2: string);
    procedure ExtractTo(f1, f2: string);
    function ExtractZipToDir(Filename, Dir: string): boolean;
    procedure Register_(JavaEditor: string);
    procedure ChangeRegistry(Dest, Source: string);
    procedure ChangeJERegistration(Source: string);
    procedure WriteString(key, name, value: String);
    function ReadString(key, name, default: String): String;
    procedure WriteInteger(key, name: String; value: Integer);
    function  ReadInteger(key, name: String; default: Integer): Integer;
    function UnPortApp(s: string): string;
    procedure FixUpdate;
    procedure CreateDirectories(dir: string);
  public
    Restart: boolean;
    Error: boolean;
    procedure MakeUpdate;
  end;

var
  FUpdater: TFUpdater;

implementation

{$R *.dfm}

Uses ShellApi, IOUtils, FmxUtils, USetup, UWait, System.UITypes,
     JvGnugettext;

{$WARNINGS OFF}

procedure TFUpdater.CopyFromTo(f1, f2: string);
  var Attribute: Integer;
begin
  Attribute:= FileGetAttr(f2);
  if (Attribute <> -1) and (Attribute and faReadOnly <> 0) then
    FileSetAttr(f2, 0);

  if FileExists(f2) and not DeleteFile(PChar(f2)) then
    Memo.Lines.Add(Format(_('"%s" can not be deleted'), [f2]));
  if CopyFile(PChar(f1), PChar(f2), true) then
    Memo.Lines.Add(Format(_('Copy %s to %s'), [f1, f2]))
  else begin
    Memo.Lines.Add('Cannot copy ' + f1);
    Memo.Lines.Add('  to: ' + f2);
    error:= true;
  end;
end;
{$WARNINGS ON}

procedure TFUpdater.ExtractTo(f1, f2: string);
begin
  if ExtractZipToDir(f1, f2) then begin
    Memo.Lines.Add(f1 + ' -> ');
    Memo.Lines.Add('   ' + f2);
  end else begin
    Memo.Lines.Add('Cannot extract ' + f1);
    Memo.Lines.Add('  to: ' + f2);
    error:= true;
  end;
end;

procedure TFUpdater.CreateDirectories(dir: string);
begin
  if not DirectoryExists(dir) then begin
    Memo.Lines.Add('create directory: ' + dir);
    ForceDirectories(dir);
  end;
end;

{$WARNINGS OFF}
function TFUpdater.ExtractZipToDir(Filename, Dir: string): boolean;
  var ZipFile: TZipFile;
begin
  ZipFile:= TZipFile.Create;
  try
    try
      ZipFile.Open(Filename, zmRead);
      ZipFile.ExtractAll(Dir);
      Result:= true;
    finally
      FreeAndNil(ZipFile);
    end;
   except
     on E: Exception do begin
       Result:= false;
       MessageDlg(E.Message, mtError, [mbOk], 0);
     end;
   end;
end;

procedure TFUpdater.BOKClick(Sender: TObject);
begin
  ExecuteFile(Dest + 'JavaEditor.exe', '', Dest, SW_ShowNormal);
  Close;
  FSetup.Close;
end;

procedure TFUpdater.Register_(JavaEditor: string);
  var Reg: TRegistry;

  procedure WriteToRegistry(Key: String);
  begin
    with Reg do begin
      OpenKey(Key, true);
      WriteString('', 'Java-Editor');
      CloseKey;
      OpenKey(Key + '\DefaultIcon', true);
      WriteString('', HideBlanks(JavaEditor) + ',0');
      CloseKey;
      OpenKey(Key + '\Shell\Open\command', true);
      WriteString('', HideBlanks(JavaEditor) + ' "%1"');
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec', true);
      WriteString('', '[FileOpen("%1")]');
      OpenKey('Application', true);
      WriteString('', changeFileExt(ExtractFilename(JavaEditor), ''));
      CloseKey;
      OpenKey(Key + '\Shell\Open\ddeexec\topic', true);
      WriteString('', 'System');
      CloseKey;
    end;
  end;

begin
  Reg:= TRegistry.Create;
  try
    with Reg do begin
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_LOCAL_MACHINE;
      WriteToRegistry('\SOFTWARE\Classes\JavaEditor');
      RootKey:= HKEY_CURRENT_USER;
      WriteToRegistry('\SOFTWARE\Classes\JavaEditor');
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFUpdater.ChangeJERegistration(Source: string);
  var Reg: TRegistry;
begin
  Source:= decodeQuotationMark(Source);
  Reg:= TRegistry.Create;
  try
    with Reg do begin
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_LOCAL_MACHINE;
      OpenKey('\SOFTWARE\Classes\JavaEditor\Shell\Open\command', true);
      WriteString('', Source);
    end;
  finally
    Reg.Free;
  end;
end;

procedure TFUpdater.ChangeRegistry(Dest, Source: string);
  var reg: TRegistry; s1, ext, b: string; p: integer;

  procedure EditAssociation(Extension: string; docreate: boolean);
  begin
    with Reg do begin
      Access:= KEY_ALL_ACCESS;
      RootKey:= HKEY_LOCAL_MACHINE;
      try
        if docreate then begin
          OpenKey('\SOFTWARE\Classes\' + Extension, true);
          WriteString('', 'JavaEditor')
        end else begin
          OpenKey('\SOFTWARE\Classes\' + Extension, false);
          if ReadString('') = 'JavaEditor' then
            WriteString('', '');
        end;
        CloseKey;
        RootKey:= HKEY_CURRENT_USER;
        if docreate then begin
          OpenKey('\SOFTWARE\Classes\' + Extension, true);
          WriteString('', 'JavaEditor');
        end else begin
          OpenKey('\SOFTWARE\Classes\' + Extension, false);
          if ReadString('') = 'JavaEditor' then
            WriteString('', '');
        end;
        CloseKey;
      except
        on E: Exception do
          ShowMessage(E.Message);
      end
    end;
  end;

begin
  Register_(Dest);
  Reg:= TRegistry.Create;
  s1:= UnhideBlanks(Source);
  p:= Pos(' ', s1);
  while p > 0 do begin
    ext:= copy(s1, 1, p-1);
    delete(s1, 1, p);
    p:= Pos(' ', s1);
    b:= copy(s1, 1, p-1);
    delete(s1, 1, p);
    EditAssociation(ext, StrToBool(b));
    Memo.Lines.Add(ext + ' ' + b);
    p:= Pos(' ', s1);
  end;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
  FreeAndNil(Reg);
end;

procedure TFUpdater.BRetryClick(Sender: TObject);
begin
  MakeUpdate;
end;

{$WARNINGS OFF}
procedure TFUpdater.MakeUpdate;
  var Source2, From, To1, Machine, s, Key, Name, Value, kind, p1: string;
       i, p: integer; SL: TStringList;

  procedure FromTo(JavaProgramm: string);
    var FDT1, FDT2: TDateTime;
  begin
    From:= Source + JavaProgramm;
    To1 := Dest + JavaProgramm;
    if not FileExists(To1) or (FileAge(From, FDT1) and FileAge(To1, FDT2) and (FDT1 > FDT2)) then
      CopyFromTo(From, To1);
  end;

  procedure del(Filename: string);
  begin
    To1:= Dest + Filename;
    if FileExists(To1) then begin
      if DeleteFile(To1)
        then Memo.Lines.Add(From + ' deleted.')
        else Memo.Lines.Add('Unable to delete: ' + To1);
    end;
  end;

begin
  //Show;
  //Memo.Clear;
  Restart:= false;
  p1:= ParamStr(1);
  {
  Memo.Lines.Add('Cmd: ' + CmdLine);
  Memo.Lines.Add('Old Version: ' + ParamStr(2));
  Memo.Lines.Add('Source     : ' + ParamStr(3));
  Memo.Lines.Add('Destination: ' + ParamStr(4));
  }

  i:= 1;
  while (i < ParamCount) and (ParamStr(i) <> '-INI') do
    inc(i);
  if i < ParamCount then begin
    Machine:= UnhideBlanks(ParamStr(i+1));
    MachineIniFile:= TMemIniFile.Create(Machine);
    PortableApplication:= MachineIniFile.ReadBool('Java', 'PortableApplication', false);
    PortAppDrive:= ExtractFileDrive(Machine);   // with UNC we get \\Server\Freigabe
    if Pos(':', PortAppDrive) > 0 then
      PortAppDrive:= copy(PortAppDrive, 1, 1);
    UseRegistry:= false;
  end else begin
    MyRegistry:= TRegistry.Create;
    UseRegistry:= true;
    PortableApplication:= false;
  end;

  if ParamStr(1) = '-Registry' then begin
    SL:= Split('#', UnhideBlanks(ParamStr(2)));
    for i:= 0 to SL.Count - 1 do begin
      s:= SL.strings[i];
      if s = '' then continue;
      p:= Pos('|', s);
      Key:= copy(s, 1, p-1);
      delete(s, 1, p);
      p:= Pos('|', s);
      Name:= copy(s, 1, p-1);
      delete(s, 1, p);
      p:= Pos('|', s);
      Value:= copy(s, 1, p-1);
      delete(s, 1, p);
      Kind:= s;
      memo.lines.add('Key: ' + Key + ' name: ' + name + ' Value: ' + Value);
      case Kind[1] of
        's': WriteString(Key, Name, Value);
        'i': WriteInteger(Key, Name, StrToInt(Value));
      end;
    end;
  end else if ParamStr(1) = '-Update' then begin
    FixUpdate;

    Dest   := trim(UnHideBlanks(ParamStr(2)));
    Source := UnHideBlanks(ParamStr(3));
    Source2:= UnHideBlanks(Paramstr(4));

    Error:= false;
    if Source2 <> '' then
      Memo.Lines.Add('Old Version: ' + Source2);
    Memo.Lines.Add('Source     : ' + Source);
    Memo.Lines.Add('Destination: ' + Dest);
    Memo.Lines.Add('');

    if ExtractFilename(Dest) <> 'javaeditor.exe' then
      CreateDirectories(Dest);
    if Pos('jalopy.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Jalopy', 'Jalopy', UnportApp(Dest));
      CopyFromTo(Source2, PChar(Dest + '\myjalopy.xml'));
      WriteString('Jalopy', 'JalopyConfiguration', UnportApp(Dest + '\myjalopy.xml'));
    end
    else if Pos('checkstyle', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Checkstyle', 'Checkstyle', UnPortApp(Dest));
      CopyFromTo(Source2, PChar(Dest + '\mycheckstyle.xml'));
      WriteString('Checkstyle', 'Configurationfile', UnPortApp(Dest + '\mycheckstyle.xml'));
    end
    else if Pos('html.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      ExtractTo(Source2, Dest);
      WriteString('Program', 'Javabook', UnPortApp(Dest + '\html\cover.html'));
    end
    else if Pos('tutorial.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Program', 'Tutorial', UnPortApp(Dest+ '\tutorial.chm'));
    end
    else if Pos('jikes.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Java', 'JikesCompiler', UnPortApp(Dest + '\bin\jikes.exe'));
    end
    else if Pos('LEJOS', Uppercase(Source)) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Mindstorms', 'LejosFolder', UnPortApp(Dest));
      WriteString('Mindstorms', 'LejosUploader', UnPortApp(Dest + '\bin\nxj.bat'));
      WriteString('Mindstorms', 'LejosFlasher', UnPortApp(Dest + '\bin\nxjflashg.bat'));
      WriteString('Mindstorms', 'Manual', UnPortApp(Dest + '\projects\classes\doc\index.html'));
    //  WriteIntegerM('Mindstorms', 'Port', MindstormsPort);
    end
    else if Pos('jad.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Java', 'Disassembler', UnPortApp(Dest +'\jad.exe'));
    end
    else if Pos('j2se8.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Program', 'Manual', UnPortApp(Dest+ '\j2se8.chm'));
    end
    else if Pos('javafx.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('Program', 'ManualFX', UnPortApp(Dest+ '\api\index.html'));
    end
    else if Pos('junitdoc.zip', Source) > 0 then begin
      ExtractTo(Source, Dest);
      WriteString('JUnit', 'JUnitManual', UnPortApp(Dest+ '\api\index.html'));
    end
    else if Pos('JEClassLoader.class', Source) > 0 then
      CopyFromTo(Source, PChar(Dest + '\JEClassLoader.class'))
    else if Pos('jeregistry', source) > 0 then
      ChangeJERegistration(Source2)
    else if Pos('registry', source) > 0 then
      ChangeRegistry(Dest, Source2)
    else begin  // update version
      Restart:= true;
      if Dest = '' then Exit;
      Sleep(500); // let JavaEditor terminate;
      CreateDirectories(Dest + 'locale\');
      CreateDirectories(Dest + 'styles\');
      CreateDirectories(Dest + 'templates\');
      CreateDirectories(Dest + 'docs\');
      CreateDirectories(Dest + 'img\');
      Version:= Source2;

      TDirectory.Copy(Source + '\locale', Dest + '\locale');
      TDirectory.Copy(Source + '\styles', Dest + '\styles');
      TDirectory.Copy(Source + '\templates', Dest + '\templates');
      TDirectory.Copy(Source + '\docs', Dest + '\docs');
      TDirectory.Copy(Source + '\img', Dest + '\img');

      CopyFromTo(Source + 'JavaEditor.exe', Dest + 'JavaEditor.exe');
      CopyFromTo(Source + 'JE2Java.exe', Dest + 'JE2Java.exe');
      CopyFromTo(Source + 'assemble.bat', Dest + 'assemble.bat');
      del('JavaEditor.map');
      del('InOut.class');
      del('InOut.java');
      del('NumberField.class');
      del('NumberField.java');
      del('JNumberField.class');
      del('JNumberField.java');
      del('Turtle$1.class');
      del('Turtle$2.class');
      del('Turtle.class');
      del('Turtle.java');
      del('JEApplets.jar');
      del('JEControl.class');
      del('JEEval.jar');
      del('JEClassLoader.class');
      del('je2java.map');
      del('docs\Turtle.html');
      FromTo('java.policy.applet');
      FromTo('JEClasses.jar');
      FromTo('Setup.exe');
    end;
    try
      Memo.Lines.Add(IncludeTrailingBackslash(ExtractFilePath(Dest)) + 'update.txt');
      Memo.Lines.SaveToFile(IncludeTrailingBackslash(ExtractFilePath(Dest)) + 'update.txt');
    except on E: Exception do
       Memo.Lines.Add(E.Message);
    end;
  end;
end;
{$WARNINGS ON}

procedure TFUpdater.FixUpdate;
  var u1, u2: Integer;
begin
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_ALL_ACCESS;
      u1:= -1;
      u2:= -1;

      RootKey:= HKEY_CURRENT_USER;
      try
        try
          if OpenKey('\Software\JavaEditor\Program', true) then begin
            if u1 <> -1 then WriteInteger('Update', u1);
            if u2 <> -1 then WriteInteger('NextUpdate', u2);
          end;
        finally
        end;
      except
        on E: Exception do
          ShowMessage(e.Message);
      end;
    end
  else begin
    u1:= ReadInteger('Program', 'Update', -1);
    if u1 <> -1 then begin
      MachineIniFile.DeleteKey('Program', 'Update');
      MachineIniFile.DeleteKey('Program', 'NextUpdate');
    end;
  end;
end;

procedure TFUpdater.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFUpdater.WriteString(key, name, value: String);
begin
  if ReadString(key, name, '') = value then exit;
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_WRITE;
      try
        if OpenKey('\Software\JavaEditor\' + key, true)
          then WriteString(name, value);
      finally
        CloseKey;
      end;
    end
  else
    try
      MachineIniFile.WriteString(key, name, value);
    except
    end;
end;

function TFUpdater.ReadString(key, name, default: String): String;
begin
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(Name)
          then Result:= ReadString(name)
          else Result:= Default;
      finally
        CloseKey;
      end;
    end
  else
    Result:= MachineIniFile.ReadString(key, name, default);
end;

procedure TFUpdater.WriteInteger(key, name: String; value: Integer);
begin
  if ReadInteger(key, name, 0) = value then exit;
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_WRITE;
      try
        if OpenKey('\Software\JavaEditor\' + key, true)
          then WriteInteger(name, value);
      finally
        CloseKey;
      end;
    end
  else
    try
      MachineIniFile.WriteInteger(key, name, value)
    except
    end;
end;

function  TFUpdater.ReadInteger(key, name: String; default: Integer): Integer;
begin
  if UseRegistry then
    with MyRegistry do begin
      RootKey:= HKEY_LOCAL_MACHINE;
      Access:= KEY_READ;
      try
        OpenKey('\Software\JavaEditor\' + key, false);
        if ValueExists(Name)
          then Result:= ReadInteger(name)
          else Result:= default;
      finally
        CloseKey;
      end;
    end
  else
    Result:= MachineIniFile.ReadInteger(key, name, default)
end;

function TFUpdater.UnPortApp(s: string): string;
  var SL1, SL2: TStringList;
      i, j: integer;
      Editorfolder: String;
begin
  Result:= s;
  EditorFolder:= ExtractFilePath(ParamStr(0));
  if (s <> '') and (Uppercase(copy(Editorfolder, 1, 2)) = Uppercase(copy(s, 1, 2)))  // same drive
  then begin
    SL1:= Split('\', withoutTrailingSlash(Editorfolder));
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

end.
