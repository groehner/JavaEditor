unit UDlgDownload;

interface

uses
  Classes, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Buttons, Vcl.Controls;

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
    fileURL: string;
    fileName: string;
    Cancel: boolean;
  public
    DownloadIsOK: boolean;
    procedure SetUrlAndFile(const u, f: string);
    function GetInetFile(const aFileURL, aFileName: string; const Progress: TProgressBar): boolean;
    function GetDownloadFiles(const Section: string): TStringList;
  end;

implementation

{$R *.dfm}

uses Windows, SysUtils, IniFiles, WinINet, JvGnugettext,
     UStringRessources, UDlgUpdate, UConfiguration, UUtils;

procedure TFDownload.SetUrlAndFile(const u, f: string);
begin
  fileURL:= u;
  fileName:= f;
  EUrl.Text:= u;
  EUrl.Hint:= u;
  EFile.Text:= f;
  EFile.Hint:= f;
end;

procedure TFDownload.SPOpenClick(Sender: TObject);
begin
  SaveDialog.FileName:= ExtractFileName(EFile.Text);
  SaveDialog.InitialDir:= ExtractFilePath(EFile.Text);
  if SaveDialog.Execute then begin
    Filename:= SaveDialog.FileName;
    EFile.Text:= FileName;
    EFile.Hint:= FileName;
  end;
end;

function TFDownload.GetInetFile(const aFileURL, aFileName: string; const Progress: TProgressBar): boolean;
const
  BufferSize = 1024; 
var
  hSession, hURL: HInternet;
  Buffer: array[0..BufferSize+1] of Byte;
  code : array[1..20] of Char;
  codes: string;
  Value: integer;
  BufferLen,
  Index,
  CodeLen: DWord;
  sAppName: string;
  aFile: TFileStream;
  KnowSize: boolean;
begin
  result := false;
  sAppName := ExtractFileName(Application.ExeName) ;
  hSession := InternetOpen(PChar(sAppName), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0) ;
  try
    try
      hURL:= InternetOpenURL(hSession, PChar(aFileURL), nil, 0, INTERNET_FLAG_DONT_CACHE, 0);
      if assigned(hURL) then
        try
          try
            // get HTTP state
            Index:= 0;
            CodeLen:= Length(code);
            if HttpQueryInfo(hURL, HTTP_QUERY_STATUS_CODE, @code, CodeLen, Index) then begin
              codes:= string(code); // 200, 401, 404 or 500
              codes:= copy(codes, 1, 3);
              if codes <> '200' then exit(false);
            end;

            // get file size
            KnowSize:= false;
            if Progress <> nil then begin
              KnowSize:= true;
              Index := 0;
              CodeLen := Length(code);
              HttpQueryInfo(hURL, HTTP_QUERY_CONTENT_LENGTH, @code, codeLen, Index);
              codes:= string(code);
              if TryStrToInt(codes, Value)
                then Progress.Max:= Value
                else KnowSize:= false;
            end;
            BCancel.Enabled:= true;

            // get file
            aFile:= TFileStream.Create(aFileName, fmCreate or fmShareExclusive);
            repeat
              InternetReadFile(hURL, @Buffer, SizeOf(Buffer), BufferLen);
              aFile.Write(Buffer[0], BufferLen);
              if (Progress <> nil) and KnowSize then
                Progress.StepBy(SizeOf(Buffer));
              Application.ProcessMessages;
            until Cancel or (BufferLen = 0);
            FreeAndNil(aFile);

            result:= not Cancel;
          except on e: Exception do
            EFile.Text:= Format(_(LNGCanNotCreateFile), [aFilename, e.Message]);
          end
        finally
          InternetCloseHandle(hURL)
        end
      else
        ErrorMsg(_(LNGNoInternetConnection) + ' ' + SysErrorMessage(GetLastError));
    except
      ErrorMsg(_(LNGNoInternetConnection) + ' ' + SysErrorMessage(GetLastError));
    end;
  finally
    InternetCloseHandle(hSession);
    BCancel.Enabled:= true;
  end;
end;

procedure TFDownload.FormShow(Sender: TObject);
begin
  DownloadIsOK:= false;
  BCancel.Enabled:= false;
  ProgressBar.Position:= 0;
  Cancel:= false;
end;

procedure TFDownload.TimerDownloadTimer(Sender: TObject);
begin
  TimerDownload.Enabled:= false;
  try
    Screen.Cursor:= crHourglass;
    DownloadIsOK:= GetInetFile(fileUrl, fileName, ProgressBar);
  finally
    Screen.Cursor:= crDefault;
  end;
  if DownloadIsOK then BCancel.Caption:= _('&OK');
  BDownload.Enabled:= true;
end;

procedure TFDownload.BCancelClick(Sender: TObject);
begin
  Cancel:= true;
end;

procedure TFDownload.BDownloadClick(Sender: TObject);
begin
  BDownload.Enabled:= false;
  BCancel.Enabled:= false;
  FileUrl:= EUrl.Text;
  FileName:= EFile.Text;
  var dir:= ExtractFilePath(Filename);
  if not DirectoryExists(dir) then
    ForceDirectories(dir);
  TimerDownload.Enabled:= true;
end;

procedure TFDownload.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

function TFDownload.GetDownloadFiles(const Section: string): TStringList;
  var ini: TMemIniFile; Source, Dest: string;
begin
  Source:= Server + 'download.txt';
  Dest  := FConfiguration.TempDir + 'download.txt';
  Result:= TStringList.Create;
  try
    Screen.Cursor:= crHourglass;
    if DownloadFile(Source, Dest) then begin
      Ini:= TMemIniFile.Create(Dest);
      Ini.ReadSectionValues(Section, Result);
      FreeAndNil(Ini);
    end;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

end.

