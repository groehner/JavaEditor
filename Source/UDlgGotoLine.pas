unit UDlgGotoLine;

interface

uses
  Forms, StdCtrls, Vcl.Controls, System.Classes;

type
  TFGotoLineDialog = class(TForm)
    LGoto: TLabel;
    CBLinenumber: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBLinenumberKeyPress(Sender: TObject; var Key: Char);
  public
    Line: Integer;
  end;

implementation

uses JvGnugettext;

{$R *.DFM}

procedure TFGotoLineDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFGotoLineDialog.CBLinenumberKeyPress(Sender: TObject; var Key: Char);
  var code, i, j: Integer;
      s: string;
begin
  if Key = #13 then begin
    s:= CBLinenumber.Text;
    Val(s, Line, code);
    if code <> 0 then exit;
    if CBLinenumber.Items.IndexOf(s) = -1 then begin
      i:= 0;
      while i < CBLinenumber.Items.Count do begin
        val(CBLinenumber.Items[i], j, code);
        if Line < j then break;
        inc(i);
      end;
      CBLinenumber.Items.Insert(i, s);
    end;
    ModalResult:= mrOK;
  end else if Key = #27 then
    ModalResult:= mrNo;
end;

end.
