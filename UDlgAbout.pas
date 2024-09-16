unit UDlgAbout;

interface

uses Forms, StdCtrls, ExtCtrls, Vcl.Graphics, Vcl.Controls, System.Classes;

const
  {$IFDEF WIN32}
  Version = '22.03, 32 Bit';
  {$ELSE}
  Version = '22.03, 64 Bit';
  {$ENDIF}
  DTag  = 16;
  Monat = 8;
  Jahr  = 2024;

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

uses SysUtils, JvGnugettext;

procedure TFAbout.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LProductname.Caption:= 'Java-Editor ' + Version + ', ' + GetDate;
end;

class function TFAbout.GetDate: string;
begin
  Result:= DateToStr(EncodeDate(Jahr, Monat, DTag));
end;

end.

