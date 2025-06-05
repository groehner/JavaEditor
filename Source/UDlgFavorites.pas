unit UDlgFavorites;

interface

uses
  Classes,
  Forms,
  StdCtrls,
  Vcl.Controls;

type
  TFFavoritenDialog = class(TForm)
    LDescription: TLabel;
    EDescription: TEdit;
    LUrl: TLabel;
    EUrl: TEdit;
    BSave: TButton;
    BCancel: TButton;
    procedure EUrlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  Clipbrd,
  JvGnugettext;

{$R *.DFM}

procedure TFFavoritenDialog.EUrlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = Ord('C')) then
    Clipboard.AsText := EUrl.Text;
end;

procedure TFFavoritenDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

end.
