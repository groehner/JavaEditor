unit UDlgJarCreate;

interface

uses
  Forms,
  StdCtrls,
  CheckLst,
  Vcl.Controls,
  System.Classes;

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
  private
    FJarCreateAll: string;
    FJarCreateCurrent: string;
  public
    procedure Init(Str: string);
    property JarCreateAll: string read FJarCreateAll;
    property JarCreateCurrent: string read FJarCreateCurrent;
  end;

implementation

uses
  SysUtils,
  JvGnugettext;

{$R *.dfm}

procedure TFJarCreateDialog.Init(Str: string);
var
  Item: string;
  Count, Posi: Integer;
  ItemOk: Boolean;
begin
  JarCreateItems.Clear;
  Count := 0;
  if Str <> '' then
    repeat
      Posi := Pos(';', Str);
      if Posi > 0 then
      begin
        Item := Trim(Copy(Str, 1, Posi - 1));
        Delete(Str, 1, Posi);
      end
      else
      begin
        Item := Str;
        Str := '';
      end;
      Posi := Pos('#', Item);
      if Posi = 2 then
      begin
        ItemOk := Item[1] = '1';
        Delete(Item, 1, 2);
        Item := Trim(Item);
        if Item <> '' then
        begin
          JarCreateItems.AddItem(Item, nil);
          JarCreateItems.Checked[Count] := ItemOk;
          Inc(Count);
        end;
      end;
    until Str = '';
end;

procedure TFJarCreateDialog.BSaveClick(Sender: TObject);
  var Str1, Str2: string;
begin
  Str1 := '';
  Str2 := '';
  for var I := 0 to JarCreateItems.Count - 1 do
  begin
    if Trim(JarCreateItems.Items[I]) <> '' then
      if JarCreateItems.Checked[I] then
      begin
        Str1 := Str1 + JarCreateItems.Items[I] + ' ';
        Str2 := Str2 + '1#' + JarCreateItems.Items[I] + ';';
      end
      else
        Str2 := Str2 + '0#' + JarCreateItems.Items[I] + ';';
  end;
  FJarCreateCurrent := Str1;
  FJarCreateAll := Str2;
end;

procedure TFJarCreateDialog.BDeleteClick(Sender: TObject);
begin
  JarCreateItems.DeleteSelected;
end;

procedure TFJarCreateDialog.BNewItemClick(Sender: TObject);
begin
  var
  Str := Trim(ENewItem.Text);
  if Str <> '' then
  begin
    JarCreateItems.AddItem(Str, nil);
    JarCreateItems.Checked[JarCreateItems.Count - 1] := True;
  end;
end;

procedure TFJarCreateDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

end.
