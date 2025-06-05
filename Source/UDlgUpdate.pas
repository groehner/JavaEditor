unit UDlgUpdate;

interface

uses
  Forms,
  StdCtrls,
  ComCtrls,
  Vcl.Controls,
  System.Classes;

const
  Server = 'https://www.guipy.de/javaeditor/download/';
  InformationFile = Server + 'version.txt';
{$IFDEF WIN32}
  ZipFile = Server + 'javaeditor.zip';
  Bits = '32 Bit';
{$ENDIF}
{$IFDEF WIN64}
  ZipFile = Server + 'javaeditor64.zip';
  Bits = '64 Bit';
{$ENDIF}

type
  TFUpdateDialog = class(TForm)
    LOldVersion: TLabel;
    EOldVersion: TEdit;
    LNewVersion: TLabel;
    ENewVersion: TEdit;
    Memo: TMemo;
    LChecking: TLabel;
    CBUpdate: TComboBox;
    ProgressBar: TProgressBar;
    BClose: TButton;
    BUpdate: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure BCloseClick(Sender: TObject);
    procedure BUpdateClick(Sender: TObject);
  private
    FSourceDir: string;
    FDestDir: string;
    procedure MakeUpdate;
    function NewVersion: Boolean;
    function ShowVersionDate(Str: string): string;
  public
    procedure CheckAutomatically;
  end;

implementation

uses
  UDlgDownload,
  SysUtils,
  DateUtils,
  IOUtils,
  JvGnugettext,
  UStringRessources,
  UUtils,
  UDlgAbout,
  UConfiguration,
  UJava;

{$R *.dfm}

procedure TFUpdateDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FConfiguration.SetElevationRequiredState(BUpdate);
end;

procedure TFUpdateDialog.FormShow(Sender: TObject);
begin
  FSourceDir := FConfiguration.TempDir + 'javaeditor\';
  FDestDir := FConfiguration.EditorFolder;
  TThread.ForceQueue(nil,
    procedure
    begin
      NewVersion;
    end);
end;

procedure TFUpdateDialog.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  FConfiguration.WriteIntegerU('Program', 'Update', CBUpdate.ItemIndex);
end;

procedure TFUpdateDialog.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFUpdateDialog.BUpdateClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    ForceDirectories(FSourceDir);
    var
    FileName := TPath.Combine(FSourceDir, 'javaeditor.zip');
    with TFDownload.Create(Self) do
    begin
      if GetInetFile(ZipFile, FileName, ProgressBar) then
        if FConfiguration.ExtractZipToDir(FileName,
          ExcludeTrailingPathDelimiter(FSourceDir)) then
          MakeUpdate
        else
          Memo.Lines.Add(_('Error while unpacking'))
      else
        Memo.Lines.Add(_(LNGDownloadError));
      Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFUpdateDialog.MakeUpdate;
var
  Updater, Params, FSVersion: string;
begin
  FConfiguration.ModelToRegistry;
  // do it in advance, because in 32 bit it takes very long
  FJava.MakeUpdate := True;
  Updater := TPath.Combine(FSourceDir, 'setup.exe');
  FSVersion := Copy(UDlgAbout.Version, 1, Pos(',', UDlgAbout.Version) - 1);
  Params := '-Update ' + HideBlanks(FDestDir) + ' ' + HideBlanks(FSourceDir) +
    ' ' + FSVersion;
  if FConfiguration.RunAsAdmin(Handle, Updater, Params) = 33 then
  begin
    Close;
    FJava.Close;
  end;
end;

function TFUpdateDialog.NewVersion: Boolean;
var
  Old, New, Str: string;
begin
  EOldVersion.Text := UDlgAbout.Version + ', ' + TFAbout.GetDate;
  ENewVersion.Text := '';
  Memo.Lines.Clear;
  try
    Screen.Cursor := crHourGlass;
    if DownloadURL(InformationFile, FConfiguration.TempDir + 'Version.txt') then
    begin
      var
      StringList := TStringList.Create;
      try
        StringList.LoadFromFile(FConfiguration.TempDir + 'Version.txt');
        Str := ShowVersionDate(StringList[0]);
        Insert(Bits + ', ', Str, Pos(', ', Str) + 2);
        ENewVersion.Text := Str;
        Memo.Lines.AddStrings(StringList);
        Old := Copy(EOldVersion.Text, 1, Pos(',', EOldVersion.Text) - 1);
        New := Copy(ENewVersion.Text, 1, Pos(',', ENewVersion.Text) - 1);
        if Old = New then
        begin
          ENewVersion.Text := _('The Java-Editor is up to date.');
          Result := False;
        end
        else
          Result := True;
      finally
        FreeAndNil(StringList);
      end;
    end
    else
    begin
      Memo.Lines.Add(_(LNGNoInternetConnection));
      Result := False;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFUpdateDialog.CheckAutomatically;
var
  NextUpdate: Integer;
  ADate: TDateTime;
  AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word;

  procedure NextMonth;
  begin
    Inc(AMonth);
    if AMonth = 13 then
    begin
      Inc(AYear);
      AMonth := 1;
    end;
    AWeekOfMonth := 1;
    ADayOfWeek := 1;
  end;

  procedure NextWeek;
  begin
    ADayOfWeek := 1;
    Inc(AWeekOfMonth);
    if not IsValidDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek) then
      NextMonth;
  end;

  procedure NextDay;
  begin
    Inc(ADayOfWeek);
    if ADayOfWeek = 8 then
      NextWeek;
  end;

begin
  CBUpdate.ItemIndex := FConfiguration.ReadIntegerU('Program', 'Update', 0);
  if CBUpdate.ItemIndex <= 0 then
    Exit;
  NextUpdate := FConfiguration.ReadIntegerU('Program', 'NextUpdate', -1);
  if (Date < NextUpdate) and (CBUpdate.ItemIndex < 4) then
    Exit;
  DecodeDateMonthWeek(Date, AYear, AMonth, AWeekOfMonth, ADayOfWeek);
  case CBUpdate.ItemIndex of
    1:
      NextMonth;
    2:
      NextWeek;
    3:
      NextDay;
  end;
  ADate := EncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
  FConfiguration.WriteIntegerU('Program', 'NextUpdate', Round(ADate));
  if NewVersion then
    Show;
end;

function TFUpdateDialog.ShowVersionDate(Str: string): string;
var
  Day, Month, Year, Posi: Integer;
  Version: string;
begin
  Posi := Pos(',', Str);
  Version := Copy(Str, 1, Posi);
  Delete(Str, 1, Posi);
  Posi := Pos('.', Str);
  Result := Version;
  if TryStrToInt(Copy(Str, 1, Posi - 1), Year) then
  begin
    Delete(Str, 1, Posi);
    Posi := Pos('.', Str);
    if TryStrToInt(Copy(Str, 1, Posi - 1), Month) then
    begin
      Delete(Str, 1, Posi);
      if TryStrToInt(Str, Day) then
        Result := Version + ' ' + DateToStr(EncodeDate(Year, Month, Day));
    end;
  end;
end;

end.
