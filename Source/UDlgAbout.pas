unit UDlgAbout;

interface

uses
  Forms,
  StdCtrls,
  ExtCtrls,
  Vcl.Controls,
  System.Classes, Vcl.Graphics;

const
{$IFDEF WIN32}
  Version = '23.08, 32 Bit';
{$ELSE}
  Version = '23.08, 64 Bit';
{$ENDIF}
  Day = 17;
  Month = 6;
  Year = 2025;

type
  TFAbout = class(TForm)
    GBVersion: TGroupBox;
    ProgramIcon: TImage;
    LProductName: TLabel;
    LNameOfCopyrights: TLabel;
    GBComponents: TGroupBox;
    LEditor: TLabel;
    LEditorValue: TLabel;
    LUML: TLabel;
    LUMLvalue: TLabel;
    BOK: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  public
    class function GetDate: string;
  end;

implementation

{$R *.DFM}

uses
  SysUtils,
  JvGnugettext;

procedure TFAbout.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LProductName.Caption := 'Java-Editor ' + Version + ', ' + GetDate;
end;

class function TFAbout.GetDate: string;
begin
  Result := DateToStr(EncodeDate(Year, Month, Day));
end;

end.
