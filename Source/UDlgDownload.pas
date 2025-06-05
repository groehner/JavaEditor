unit UDlgDownload;

interface

uses
  Classes,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Buttons,
  Vcl.Controls;

type
  TFDownload = class(TForm)
    LUrl: TLabel;
    EUrl: TEdit;
    ProgressBar: TProgressBar;
    TimerDownload: TTimer;
    BCancel: TButton;
    EFile: TEdit;
    LFile: TLabel;
    BDownload: TButton;
    SaveDialog: TSaveDialog;
    SPOpen: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerDownloadTimer(Sender: TObject);
    procedure BDownloadClick(Sender: TObject);
    procedure SPOpenClick(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
  private
    FFileURL: string;
    FFileName: string;
    FCancel: Boolean;
    FDownloadIsOK: Boolean;
  public
    procedure SetUrlAndFile(const Url, Filename: string);
    function GetInetFile(const AFileURL, AFileName: string;
      const Progress: TProgressBar): Boolean;
    function GetDownloadFiles(const Section: string): TStringList;
    property DownloadIsOK: Boolean read FDownloadIsOK;
  end;

implementation

{$R *.dfm}

uses
  Windows,
  SysUtils,
  IniFiles,
  WinInet,
  JvGnugettext,
  UStringRessources,
  UDlgUpdate,
  UConfiguration,
  UUtils;

procedure TFDownload.SetUrlAndFile(const Url, Filename: string);
begin
  FFileURL := Url;
  FFileName := Filename;
  EUrl.Text := Url;
  EUrl.Hint := Url;
  EFile.Text := Filename;
  EFile.Hint := Filename;
end;

procedure TFDownload.SPOpenClick(Sender: TObject);
begin
  SaveDialog.FileName := ExtractFileName(EFile.Text);
  SaveDialog.InitialDir := ExtractFilePath(EFile.Text);
  if SaveDialog.Execute then
  begin
    FFileName := SaveDialog.FileName;
    EFile.Text := FFileName;
    EFile.Hint := FFileName;
  end;
end;

function TFDownload.GetInetFile(const AFileURL, AFileName: string;
  const Progress: TProgressBar): Boolean;
const
  BufferSize = 1024;
var
  HSession, HURL: HINTERNET;
  Buffer: array [0 .. BufferSize + 1] of Byte;
  Code: array [1 .. 20] of Char;
  Codes: string;
  Value: Integer;
  BufferLen, Index, CodeLen: DWORD;
  SAppName: string;
  AFile: TFileStream;
  KnowSize: Boolean;
begin
  Result := False;
  SAppName := ExtractFileName(Application.ExeName);
  HSession := InternetOpen(PChar(SAppName), INTERNET_OPEN_TYPE_PRECONFIG,
    nil, nil, 0);
  try
    try
      HURL := InternetOpenUrl(HSession, PChar(AFileURL), nil, 0,
        INTERNET_FLAG_DONT_CACHE, 0);
      if Assigned(HURL) then
        try
          try
            // get HTTP state
            Index := 0;
            CodeLen := Length(Code);
            if HttpQueryInfo(HURL, HTTP_QUERY_STATUS_CODE, @Code, CodeLen, Index)
            then
            begin
              Codes := string(Code); // 200, 401, 404 or 500
              Codes := Copy(Codes, 1, 3);
              if Codes <> '200' then
                Exit(False);
            end;

            // get file size
            KnowSize := False;
            if Assigned(Progress) then
            begin
              KnowSize := True;
              Index := 0;
              CodeLen := Length(Code);
              HttpQueryInfo(HURL, HTTP_QUERY_CONTENT_LENGTH, @Code,
                CodeLen, Index);
              Codes := string(Code);
              if TryStrToInt(Codes, Value) then
                Progress.Max := Value
              else
                KnowSize := False;
            end;
            BCancel.Enabled := True;

            // get file
            AFile := TFileStream.Create(AFileName,
              fmCreate or fmShareExclusive);
            repeat
              InternetReadFile(HURL, @Buffer, SizeOf(Buffer), BufferLen);
              AFile.Write(Buffer[0], BufferLen);
              if Assigned(Progress) and KnowSize then
                Progress.StepBy(SizeOf(Buffer));
              Application.ProcessMessages;
            until FCancel or (BufferLen = 0);
            FreeAndNil(AFile);

            Result := not FCancel;
          except
            on e: Exception do
              EFile.Text := Format(_(LNGCanNotCreateFile),
                [AFileName, e.Message]);
          end;
        finally
          InternetCloseHandle(HURL);
        end
      else
        ErrorMsg(_(LNGNoInternetConnection) + ' ' +
          SysErrorMessage(GetLastError));
    except
      ErrorMsg(_(LNGNoInternetConnection) + ' ' +
        SysErrorMessage(GetLastError));
    end;
  finally
    InternetCloseHandle(HSession);
    BCancel.Enabled := True;
  end;
end;

procedure TFDownload.FormShow(Sender: TObject);
begin
  FDownloadIsOK := False;
  BCancel.Enabled := False;
  ProgressBar.Position := 0;
  FCancel := False;
end;

procedure TFDownload.TimerDownloadTimer(Sender: TObject);
begin
  TimerDownload.Enabled := False;
  try
    Screen.Cursor := crHourGlass;
    FDownloadIsOK := GetInetFile(FFileURL, FFileName, ProgressBar);
  finally
    Screen.Cursor := crDefault;
  end;
  if DownloadIsOK then
    BCancel.Caption := _('&OK');
  BDownload.Enabled := True;
end;

procedure TFDownload.BCancelClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TFDownload.BDownloadClick(Sender: TObject);
begin
  BDownload.Enabled := False;
  BCancel.Enabled := False;
  FFileURL := EUrl.Text;
  FFileName := EFile.Text;
  var
  Dir := ExtractFilePath(FFileName);
  if not DirectoryExists(Dir) then
    ForceDirectories(Dir);
  TimerDownload.Enabled := True;
end;

procedure TFDownload.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

function TFDownload.GetDownloadFiles(const Section: string): TStringList;
var
  Ini: TMemIniFile;
  Source, Dest: string;
begin
  Source := Server + 'download.txt';
  Dest := FConfiguration.TempDir + 'download.txt';
  Result := TStringList.Create;
  try
    Screen.Cursor := crHourGlass;
    if DownloadFile(Source, Dest) then
    begin
      Ini := TMemIniFile.Create(Dest);
      Ini.ReadSectionValues(Section, Result);
      FreeAndNil(Ini);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
