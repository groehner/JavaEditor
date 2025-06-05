unit UDlgStringsEditor;

interface

uses
  Forms,
  StdCtrls,
  System.Classes,
  Vcl.Controls;

type
  TFStringEditorDialog = class(TForm)
    MStrings: TMemo;
    BOK: TButton;
    BCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FStringEditorDialog: TFStringEditorDialog;

implementation

uses JvGnugettext;

{$R *.DFM}

procedure TFStringEditorDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFStringEditorDialog.FormShow(Sender: TObject);
begin
  PixelsPerInch := Screen.PixelsPerInch;
  if PixelsPerInch <> 96 then
  begin
    Width := (Width * PixelsPerInch) div 96;
    Height := (Height * PixelsPerInch) div 96;
  end;
end;

end.
