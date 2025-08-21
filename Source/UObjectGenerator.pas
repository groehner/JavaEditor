unit UObjectGenerator;

// for creating and editing objects!

interface

uses
  Classes,
  Graphics,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  Grids,
  ValEdit,
  Vcl.Controls,
  UEditorForm;

const TagNormal = 0; TagText = 1; TagBoolean = 3; TagFont = 5; TagNumber = 6;
  TagChar = 7; TagLines = 8; TagIcon = 16;

type

  TIntArray = array [1 .. 50] of Integer;

  TFObjectGenerator = class(TForm)
    StatusBar: TStatusBar;
    BCancel: TButton;
    BFont: TButton;
    BOK: TButton;
    PictureDialog: TOpenDialog;
    FontDialog: TFontDialog;
    ValueListEditor: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure ValueListEditorEditButtonClick(Sender: TObject);
    procedure ValueListEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MIFontClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FPartner: TFEditForm;
    FTags: TIntArray;
    FWindowOpened: Boolean;
    procedure SetPositionRelativTo(AControl: TControl);
    procedure SetWidthAndHeight;
    procedure AddRow(Attribute: string; const Value: string;
      EditStyle: TEditStyle; const Pick: string; Tag_: Integer);
    procedure OpenWindow;
  public
    procedure SetFont(AFont: TFont);
    procedure SaveWindow;
    procedure PrepareEditClass(const Caption, Title, ObjectNameOld: string);
    procedure PrepareEditObjectOrParams(const Caption, Title: string);
    function Edit(Control: TControl; Attributes: TStringList;
      Row: Integer): Boolean;
    property Partner: TFEditForm read FPartner write FPartner;
    property Tags: TIntArray read FTags;
  end;

var FObjectGenerator: TFObjectGenerator = nil;

implementation

uses
  Windows,
  SysUtils,
  Math,
  System.Types,
  UITypes,
  JvGnugettext,
  jni,
  UDlgStringsEditor,
  UJava,
  UJniWrapper1,
  UUtils,
  UStringRessources,
  UConfiguration;

{$R *.DFM}

procedure TFObjectGenerator.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ValueListEditor.Color := clBtnFace;
  FWindowOpened := False;
end;

procedure TFObjectGenerator.FormShow(Sender: TObject);
begin
  if not FWindowOpened then
  begin // form is now scaled
    OpenWindow;
    FWindowOpened := True;
  end;
  FJava.ActiveTool := 9;
  ActiveControl := ValueListEditor;
  ValueListEditor.Canvas.Font.Size := Font.Size;
  ValueListEditor.DefaultRowHeight := ValueListEditor.Canvas.TextHeight
    ('Ag') + 4;
end;

procedure TFObjectGenerator.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
  FJava.ActiveTool := -1;
end;

procedure TFObjectGenerator.OpenWindow;
var Left, Top, Width, Height, Col: Integer;
begin
  // Execution after form has been scaled
  Left := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Left', 300));
  Top := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Top', 200));
  Width := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator',
    'Width', 100));
  Height := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator',
    'Height', 200));
  Left := Min(Left, Screen.Width - 100);
  Top := Min(Top, Screen.Height - 100);
  Width := Min(Width, Screen.Width div 2);
  Height := Min(Height, Screen.Height div 2);
  SetBounds(Left, Top, Width, Height);
  Col := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator',
    'ValueListEditor.ColWidth', Width div 2));
  if Col < Width * 2 div 10 then
    Col := Width * 2 div 10;
  if Col > Width * 8 div 10 then
    Col := Width * 8 div 10;
  ValueListEditor.ColWidths[0] := Col;
  Font.Size := PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator',
    'Font.Size', 12));
  Font.Name := FConfiguration.ReadStringU('ObjectGenerator', 'Font.Name',
    'Segoe UI');
end;

procedure TFObjectGenerator.SaveWindow;
begin
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Left', PPIUnScale(Left));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Top', PPIUnScale(Top));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Width', PPIUnScale(Width));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Height', PPIUnScale(Height));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'ValueListEditor.ColWidth',
    PPIUnScale(ValueListEditor.ColWidths[0]));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Font.Size',
    PPIUnScale(Font.Size));
  FConfiguration.WriteStringU('ObjectGenerator', 'Font.Name', Font.Name);
end;

procedure TFObjectGenerator.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options := [];
  if FJava.FDFont.Execute then
    Font.Assign(FJava.FDFont.Font);
end;

procedure TFObjectGenerator.SetFont(AFont: TFont);
begin
  Font.Assign(AFont);
end;

procedure TFObjectGenerator.PrepareEditClass(const Caption, Title,
  ObjectNameOld: string);
begin
  Self.Caption := Caption;
  ValueListEditor.TitleCaptions.Text := Title;
  for var I := ValueListEditor.RowCount - 1 downto 2 do
    ValueListEditor.DeleteRow(I);
  ValueListEditor.Cells[0, 1] := _(LNGNameOfObject);
  ValueListEditor.Cells[1, 1] := ObjectNameOld;
  FTags[1] := TagText;
end;

procedure TFObjectGenerator.PrepareEditObjectOrParams(const Caption,
  Title: string);
begin
  Self.Caption := Caption;
  ValueListEditor.TitleCaptions.Text := Title;
  for var I := ValueListEditor.RowCount - 1 downto 2 do
    ValueListEditor.DeleteRow(I);
  // it's not possible to delete row[1] now
  ValueListEditor.Cells[0, 1] := 'Delete';
end;

procedure TFObjectGenerator.SetPositionRelativTo(AControl: TControl);
begin
  var
  ScreenPos := AControl.ClientToScreen(Point(0, 0));
  if ScreenPos.Y < Height then
    SetBounds(ScreenPos.X, ScreenPos.Y + AControl.Height, Width, Height)
  else
    SetBounds(ScreenPos.X, ScreenPos.Y - Height, Width, Height);
end;

procedure TFObjectGenerator.SetWidthAndHeight;
var Max1, Max2: Integer; Content: string;
begin
  Max1 := 0;
  Max2 := 0;
  for var I := 0 to ValueListEditor.RowCount - 1 do
  begin
    Content := ValueListEditor.Cells[0, I];
    Max1 := Math.Max(Canvas.TextWidth(Content), Max1);
    Content := ValueListEditor.Cells[1, I];
    Max2 := Math.Max(Canvas.TextWidth(Content), Max2);
  end;
  Max1 := Max1 + 10;
  Max2 := Max2 + 10;
  if ValueListEditor.ColWidths[0] < Max1 then
    ValueListEditor.ColWidths[0] := Max1;
  if ValueListEditor.ColWidths[1] < Max2 then
    ValueListEditor.ColWidths[1] := Max2;
  Width := Max3(PPIScale(Max1 + 3 + Max2),
    PPIScale(Canvas.TextWidth(Caption) + 55), PPIScale(270));
  Height := PPIScale(70) + (ValueListEditor.RowCount + 1) *
    ValueListEditor.DefaultRowHeight;
end;

procedure TFObjectGenerator.ValueListEditorEditButtonClick(Sender: TObject);

  function StyleToStr(Style: TFontStyles): string;
  begin
    var
    Str := '';
    if Style = [fsBold] then
      Str := 'Bold'
    else if Style = [fsItalic] then
      Str := 'Italic'
    else if Style = [fsBold, fsItalic] then
      Str := 'Bold Italic';
    Result := Str;
  end;

begin
  with ValueListEditor do
    case Tags[Row] of
      TagLines:
        with TFStringEditorDialog.Create(Self) do
        begin
          var
          Str := MStrings.Lines.Text;
          if ShowModal = mrCancel then
            MStrings.Lines.Text := Str;
          Free;
        end;
      TagFont:
        if FontDialog.Execute then
          Cells[1, Row] := FontDialog.Font.Name + ' ' +
            IntToStr(FontDialog.Font.Size) + ' ' + StyleToStr(Font.Style);
      TagIcon:
        begin
          PictureDialog.InitialDir := FConfiguration.Sourcepath;
          if PictureDialog.Execute then
            Cells[1, Row] := ExtractFileName(PictureDialog.FileName);
        end;
    end;
end;

procedure TFObjectGenerator.ValueListEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) or (Key = VK_TAB) then
    if ValueListEditor.Row < ValueListEditor.RowCount - 1 then
      ValueListEditor.Row := ValueListEditor.Row + 1
    else if Key = VK_RETURN then
      ModalResult := mrOk;
end;

procedure TFObjectGenerator.AddRow(Attribute: string; const Value: string;
  EditStyle: TEditStyle; const Pick: string; Tag_: Integer);
begin
  if Pos('$', Attribute) > 0 then // enum type?
    Attribute[Pos('$', Attribute)] := '.';
  ValueListEditor.InsertRow(Attribute, Value, True);
  var
  Idx := ValueListEditor.RowCount - 1;
  case EditStyle of
    esPickList:
      ValueListEditor.ItemProps[Idx - 1].PickList.Text := Pick;
    esEllipsis:
      ValueListEditor.ItemProps[Idx - 1].EditStyle := esEllipsis;
  end;
  FTags[Idx] := Tag_;
end;

function TFObjectGenerator.Edit(Control: TControl; Attributes: TStringList;
  Row: Integer): Boolean;
const PickBoolean = 'true'#13#10'false';
begin
  for var I := 0 to Attributes.Count - 1 do
  begin
    var
    AValueObj := TComJavaValue(Attributes.Objects[I]);
    case AValueObj.Kind of
      ntBool:
        AddRow(Attributes[I], AValueObj.AsString, esPickList, PickBoolean,
          TagBoolean);
      ntChar:
        AddRow(Attributes[I], AValueObj.AsStringWithout(''''), esSimple,
          '', TagChar);
      ntByte, ntShort, ntInt, ntLong:
        AddRow(Attributes[I], AValueObj.AsString, esSimple, '', TagNumber);
      ntFloat, ntDouble:
        AddRow(Attributes[I], AValueObj.AsString, esSimple, '', TagNormal);
      ntString:
        AddRow(Attributes[I], AValueObj.AsStringWithout('"'), esSimple,
          '', TagText);
      ntObject, ntBoolArray .. ntObjectArray:
        AddRow(Attributes[I], AValueObj.AsString, esSimple, '', TagText);
    end;
  end;
  if ValueListEditor.Cells[0, 1] = 'Delete' then
    ValueListEditor.DeleteRow(1);
  if Row < ValueListEditor.RowCount then
    ValueListEditor.Row := Row;
  Show;
  SetWidthAndHeight;
  SetPositionRelativTo(Control);
  Hide;
  Result := (ShowModal = mrOk);
  if Assigned(FJava.ActiveTDIChild) then
    FJava.ActiveTDIChild.OpenWindow(Self);
end;

end.
