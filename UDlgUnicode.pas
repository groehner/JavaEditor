unit UDlgUnicode;

interface

uses
  Forms, StdCtrls, Classes, Vcl.Controls;

type
  TFUnicodeDialog = class(TForm)
    LUnicode: TLabel;
    CBUnicode: TComboBox;
    procedure CBUnicodeKeyPress(Sender: TObject; var Key: Char);
    procedure CBUnicodeSelect(Sender: TObject);
    procedure CBUnicodeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  public
    UnicodeChar: WideChar;
  end;

implementation

uses Windows, SysUtils, Clipbrd;

{$R *.dfm}

procedure TFUnicodeDialog.CBUnicodeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    try
      var hexint:= StrToInt('$' + CBUnicode.Text);
      UnicodeChar:= WideChar(hexint);
      CBUnicode.Items.Insert(0, CBUnicode.Text);
      ModalResult:= mrOK;
    except
      Windows.Beep(600, 100);
      ModalResult:= mrNo;
    end
  else if Key = #27 then
    ModalResult:= mrNo
  else if not (CharInSet(Key, ['A'..'F']) or CharInSet(Key, ['a'..'f']) or CharInSet(Key, ['0'..'9']))
          or (length(CBUnicode.Text) = 4) then
    Key:= #0;
end;

procedure TFUnicodeDialog.CBUnicodeSelect(Sender: TObject);
begin
  var hexint:= StrToInt('$' + CBUnicode.Text);
  UnicodeChar:= WideChar(hexint);
  ModalResult:= mrOK;
end;

procedure TFUnicodeDialog.CBUnicodeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = Ord('V')) and (ssCtrl in Shift) then
    CBUnicode.Text:= Clipboard.AsText;
end;

end.
