unit UDlgJarCreate;

interface

uses
  Forms, StdCtrls, CheckLst, Vcl.Controls, System.Classes;

type
  TFJarCreateDialog = class(TForm)
    BNewItem: TButton;
    BSave: TButton;
    BDelete: TButton;
    JarCreateItems: TCheckListBox;
    ENewItem: TEdit;
    procedure BSaveClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BNewItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    JarCreateCurrent: string;
    JarCreateAll: string;
    procedure Init(s: string);
  end;

implementation

uses SysUtils, JvGnugettext;

{$R *.dfm}

procedure TFJarCreateDialog.Init(s: string);
  var item: string; Count, p: integer; ok: boolean;
begin
  JarCreateItems.Clear;
  Count:= 0;
  if s <> '' then
    repeat
      p:= Pos(';', s);
      if p > 0 then begin
        item:= Trim(Copy(s, 1, p-1));
        delete(s, 1, p);
      end else begin
        item:= s;
        s:= '';
      end;
      p:= Pos('#', item);
      if p = 2 then begin
        ok:= item[1] = '1';
        delete(item, 1, 2);
        item:= trim(item);
        if item <> '' then begin
          JarCreateItems.addItem(item, nil);
          JarCreateItems.Checked[Count]:= ok;
          inc(Count);
        end;
      end;
    until s = '';
end;

procedure TFJarCreateDialog.BSaveClick(Sender: TObject);
begin
  var s1:= '';
  var s2:= '';
  for var i:= 0 to JarCreateItems.Count - 1 do begin
    if trim(JarCreateItems.Items[i]) <> '' then
      if JarCreateItems.Checked[i] then begin
        s1:= s1 + JarCreateItems.Items[i] + ' ';
        s2:= s2 + '1#' + JarCreateItems.Items[i] + ';'
      end else
        s2:= s2 + '0#' + JarCreateItems.Items[i] + ';';
  end;
  JarCreateCurrent:= s1;
  JarCreateall:= s2;
end;

procedure TFJarCreateDialog.BDeleteClick(Sender: TObject);
begin
  JarCreateItems.DeleteSelected;
end;

procedure TFJarCreateDialog.BNewItemClick(Sender: TObject);
begin
  var s:= trim(ENewItem.Text);
  if s <> '' then begin
    JarCreateItems.AddItem(s, nil);
    JarCreateItems.Checked[JarCreateItems.Count-1]:= true;
  end;
end;

procedure TFJarCreateDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

end.
