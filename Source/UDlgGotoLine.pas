unit UDlgGotoLine;

interface

uses
  Forms,
  StdCtrls,
  Vcl.Controls,
  System.Classes;

type
  TFGotoLineDialog = class(TForm)
    LGoto: TLabel;
    CBLinenumber: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBLinenumberKeyPress(Sender: TObject; var Key: Char);
  private
    FLine: Integer;
  public
    property Line: Integer read FLine;
  end;

implementation

uses
  JvGnugettext;

{$R *.DFM}

procedure TFGotoLineDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFGotoLineDialog.CBLinenumberKeyPress(Sender: TObject; var Key: Char);
var
  Code, Idx, Value: Integer;
  Str: string;
begin
  if Key = #13 then
  begin
    Str := CBLinenumber.Text;
    Val(Str, FLine, Code);
    if Code <> 0 then
      Exit;
    if CBLinenumber.Items.IndexOf(Str) = -1 then
    begin
      Idx := 0;
      while Idx < CBLinenumber.Items.Count do
      begin
        Val(CBLinenumber.Items[Idx], Value, Code);
        if Line < Value then
          Break;
        Inc(Idx);
      end;
      CBLinenumber.Items.Insert(Idx, Str);
    end;
    ModalResult := mrOk;
  end
  else if Key = #27 then
    ModalResult := mrNo;
end;

end.
