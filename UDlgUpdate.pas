unit UDlgUpdate;

interface

uses
  Forms, StdCtrls, ComCtrls, Vcl.Controls, System.Classes;

const
  Server  = 'https://www.guipy.de/javaeditor/download/';
  InfFile = Server + 'version.txt';
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
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure BCloseClick(Sender: TObject);
    procedure BUpdateClick(Sender: TObject);
  private
    SourceDir: string;
    DestDir: string;
    procedure MakeUpdate;
    function NewVersion: boolean;
    function ShowVersionDate(s: string): string;
  public
    procedure CheckAutomatically;
  end;

implementation

uses UDlgDownload, SysUtils, DateUtils, IOUtils,
     JvGnugettext, UStringRessources,
     UUtils, UDlgAbout, UConfiguration, UJava;

{$R *.dfm}

procedure TFUpdateDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FConfiguration.SetElevationRequiredState(BUpdate);
end;

procedure TFUpdateDialog.FormShow(Sender: TObject);
begin
  SourceDir:= FConfiguration.TempDir + 'javaeditor\';
  DestDir := FConfiguration.EditorFolder;
  TThread.ForceQueue(nil, procedure
    begin
      NewVersion;
    end);
end;

procedure TFUpdateDialog.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  FConfiguration.WriteIntegerU('Program', 'Update', CBUpdate.ItemIndex);
end;

procedure TFUpdateDialog.BCloseClick(Sender: TObject);
begin
  Close;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure TFUpdateDialog.BUpdateClick(Sender: TObject);
begin
  try
    Screen.Cursor:= crHourglass;
    ForceDirectories(SourceDir);
    var Filename:= TPath.Combine(SourceDir, 'javaeditor.zip');
    with TFDownload.Create(self) do begin
      if GetInetFile(ZipFile, Filename, ProgressBar) then
        if FConfiguration.ExtractZipToDir(Filename, ExcludeTrailingBackslash(SourceDir))
          then MakeUpdate
          else Memo.Lines.Add(_('Error while unpacking'))
      else Memo.Lines.Add(_(LNGDownloadError));
      Free;
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

procedure TFUpdateDialog.MakeUpdate;
  var Updater, Params, sVersion: string;
begin
  FConfiguration.ModelToRegistry; // do it in advance, because in 32 bit it takes very long
  FJava.MakeUpdate:= true;
  Updater:= TPath.Combine(SourceDir, 'setup.exe');
  sVersion:= copy(UDlgAbout.Version, 1, Pos(',', UDlgAbout.Version) - 1);
  Params:= '-Update ' + HideBlanks(DestDir) +  ' ' + HideBlanks(SourceDir) + ' ' + sVersion;
  if FConfiguration.RunAsAdmin(Handle, Updater, Params) = 33 then begin
    Close;
    FJava.Close;
  end;
end;

function TFUpdateDialog.NewVersion: boolean;
  var old, new, s: string; p: integer;
begin
  EOldVersion.Text:= UDlgAbout.Version + ', ' + TFAbout.GetDate;
  ENewVersion.Text:= '';
  Memo.Lines.Clear;
  try
    Screen.Cursor:= crHourglass;
    if DownloadURL(InfFile, FConfiguration.TempDir + 'Version.txt') then begin
      var SL:= TStringList.Create;
      try
        SL.LoadFromFile(FConfiguration.TempDir + 'Version.txt');
        s:= ShowVersionDate(SL[0]);
        p:= pos(', ', s);
        insert(Bits + ', ', s, p + 2);
        ENewVersion.Text:= s;
        Memo.Lines.AddStrings(SL);
        old:= Copy(EOldVersion.Text, 1, Pos(',', EOldVersion.text)-1);
        new:= Copy(ENewVersion.Text, 1, Pos(',', ENewVersion.text)-1);
        if old = new then begin
          ENewVersion.Text:= _('The Java-Editor is up to date.');
          Result:= false;
        end else
          Result:= true;
      finally
        FreeAndNil(SL);
      end;
    end else begin
      Memo.Lines.Add(_(LNGNoInternetConnection));
      Result:= false;
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFUpdateDialog.CheckAutomatically;
  var NextUpdate: Integer;
      aDate: TDateTime;
      AYear, AMonth, AWeekOfMonth, ADayOfWeek: Word;

  procedure NextMonth;
  begin
    inc(AMonth);
    if AMonth = 13 then begin
      inc(AYear);
      AMonth:= 1;
    end;
    AWeekOfMonth:= 1;
    ADayOfWeek:= 1;
  end;

  procedure NextWeek;
  begin
    ADayOfWeek:= 1;
    inc(AWeekOfMonth);
    if not IsValidDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek) then
      NextMonth;
  end;

  procedure NextDay;
  begin
    inc(ADayOfWeek);
    if ADayOfWeek = 8 then
      NextWeek;
  end;

begin
  CBUpdate.ItemIndex:= FConfiguration.ReadIntegerU('Program', 'Update', 0);
  if CBUpdate.ItemIndex <= 0 then exit;
  NextUpdate:= FConfiguration.ReadIntegerU('Program', 'NextUpdate', -1);
  if (Date < NextUpdate) and (CBUpdate.ItemIndex < 4) then
    exit;
  DecodeDateMonthWeek(Date, AYear, AMonth, AWeekOfMonth, ADayOfWeek);
  case CBUpdate.ItemIndex of
    1: NextMonth;
    2: NextWeek;
    3: NextDay;
  end;
  aDate:= EncodeDateMonthWeek(AYear, AMonth, AWeekOfMonth, ADayOfWeek);
  FConfiguration.WriteIntegerU('Program', 'NextUpdate', Round(aDate));
  if NewVersion then
    Show;
end;

function TFUpdateDialog.ShowVersionDate(s: string): string;
  var Day, Month, Year, p: integer;
      Version: string;
begin
  p:= Pos(',', s);
  Version:= copy(s, 1, p);
  delete(s, 1, p);
  p:= Pos('.', s);
  Result:= Version;
  if TryStrToInt(Copy(s, 1, p-1), Year) then begin
    delete(s, 1, p);
    p:= pos('.', s);
    if TryStrToInt(Copy(s, 1, p-1), Month) then begin
      delete(s, 1, p);
      if TryStrToInt(s, Day) then
        Result:= Version + ' ' + DateToStr(EncodeDate(Year, Month, Day));
    end;
  end;
end;

end.
