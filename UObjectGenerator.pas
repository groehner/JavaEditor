unit UObjectGenerator;

// for creating and editing objects!

interface

uses
  Classes, Graphics, Forms, Dialogs, StdCtrls, ComCtrls, Grids, ValEdit,
  UFrmEditor, Vcl.Controls;

const
  TagNormal        =  0;
  TagText          =  1;
  TagBoolean       =  3;
  TagFont          =  5;
  TagNumber        =  6;
  TagChar          =  7;
  TagLines         =  8;
  TagIcon          = 16;

type

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
    procedure ValueListEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MIFontClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    WindowOpened: boolean;
  public
    Tags: array[1..50] of integer;
    Partner: TFEditForm;
    procedure SetFont(aFont: TFont);
    procedure OpenWindow;
    procedure SaveWindow;
    procedure PrepareEditClass(const aCaption, Title, ObjectNameOld: string);
    procedure PrepareEditObjectOrParams(const aCaption, Title: string);
    procedure AddRow(Attribut: string; const Value: string; es: TEditStyle; const Pick: string; _Tag: integer);
    procedure DeleteRow;
    procedure SetRow(i: integer);
    procedure SetColWidths;
    function Edit(Attributes: TStringList; Row: integer): boolean;
  end;

var
  FObjectGenerator: TFObjectGenerator = nil;

implementation

uses
  Windows, SysUtils, Math, TypInfo, UITypes, Types,
  UDlgStringsEditor, UJava, JNI, UJniWrapper1, UUtils,
  JvGnugettext, UStringRessources, UConfiguration;

{$R *.DFM}

procedure TFObjectGenerator.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ValueListEditor.Color:= clBtnFace;
  WindowOpened:= false;
end;

procedure TFObjectGenerator.FormShow(Sender: TObject);
begin
  if not WindowOpened then begin  // form is now scaled
    OpenWindow;
    WindowOpened:= true;
  end;
  FJava.ActiveTool:= 9;
  ActiveControl:= ValueListEditor;
  ValueListEditor.Canvas.Font.Size:= Font.Size;
  ValueListEditor.DefaultRowHeight:= ValueListEditor.Canvas.TextHeight('Ag') + 4;
end;

procedure TFObjectGenerator.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caHide;
  FJava.ActiveTool:= -1;
end;

procedure TFObjectGenerator.OpenWindow;
  var L, T, W, H, C: integer;
begin
  // Execution after form has been scaled
  L:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Left', 300));
  T:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Top', 200));
  W:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Width', 100));
  H:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Height', 200));
  L:= min(L, Screen.Width - 100);
  T:= min(T, Screen.Height - 100);
  W:= min(W, Screen.Width div 2);
  H:= min(H, Screen.Height div 2);
  SetBounds(L, T, W, H);
  C:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'ValueListEditor.ColWidth', W div 2));
  if C < W * 2 div 10 then C:= W * 2 div 10;
  if C > W * 8 div 10 then C:= W * 8 div 10;
  ValueListEditor.ColWidths[0]:= C;
  Font.Size:= PPIScale(FConfiguration.ReadIntegerU('ObjectGenerator', 'Font.Size', 12));
  Font.Name:= FConfiguration.ReadStringU('ObjectGenerator', 'Font.Name', 'Segoe UI');
end;

procedure TFObjectGenerator.SaveWindow;
begin
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Left', PPIUnScale(Left));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Top', PPIUnScale(Top));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Width', PPIUnScale(Width));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Height', PPIUnScale(Height));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'ValueListEditor.ColWidth', PPIUnScale(ValueListEditor.ColWidths[0]));
  FConfiguration.WriteIntegerU('ObjectGenerator', 'Font.Size', PPIUnScale(Font.Size));
  FConfiguration.WriteStringU('ObjectGenerator', 'Font.Name', Font.Name);
end;

procedure TFObjectGenerator.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options:= [];
  if FJava.FDFont.Execute then
    Font.Assign(FJava.FDFont.Font);
end;

procedure TFObjectGenerator.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
end;

procedure TFObjectGenerator.PrepareEditClass(const aCaption, Title, ObjectNameOld: string);
begin
  Self.Caption:= aCaption;
  ValueListEditor.TitleCaptions.Text:= Title;
  for var i:= ValueListEditor.RowCount-1 downto 2 do
    ValueListEditor.DeleteRow(i);
  ValueListEditor.Cells[0, 1]:= _(LNGNameOfObject);
  ValueListEditor.Cells[1, 1]:= ObjectNameOld;
  Tags[1]:= TagText;
end;

procedure TFObjectGenerator.PrepareEditObjectOrParams(const aCaption, Title: string);
begin
  Self.Caption:= aCaption;
  ValueListEditor.TitleCaptions.Text:= Title;
  for var i:= ValueListEditor.RowCount-1 downto 2 do
     ValueListEditor.DeleteRow(i);
  // it's not possible to delete row[1] now
  ValueListEditor.Cells[0, 1]:= 'Delete';
end;

procedure TFObjectGenerator.DeleteRow;
begin
  if ValueListEditor.Cells[0, 1] = 'Delete' then
    ValueListEditor.DeleteRow(1);
end;

procedure TFObjectGenerator.SetRow(i: integer);
begin
  if i < ValueListEditor.RowCount then
    ValueListEditor.Row:= i;
end;

procedure TFObjectGenerator.SetColWidths;
  var i, max1, max2: integer; s: string;
begin
  max1:= 0;
  max2:= 0;
  for i:= 0 to ValueListEditor.RowCount - 1 do begin
    s:= ValueListEditor.Cells[0, i];
    max1:= Math.Max(length(s), max1);
    s:= ValueListEditor.Cells[1, i];
    max2:= Math.max(length(s), max2);
  end;
  max1:= max1*(Font.Size-2);
  max2:= max2*(Font.Size-2);
  max1:= max1 + 10;
  max2:= max2 + 10;
  if Width < max1 + 1 + max2 then
    Width:= max1 + 3 + max2;
  if Width < BCancel.Left + BCancel.Width then
    Width:= BCancel.Left + BCancel.Width;
  if ValueListEditor.ColWidths[0] < max1 then
     ValueListEditor.ColWidths[0]:= max1;
  if ValueListEditor.ColWidths[1] < max2 then
     ValueListEditor.ColWidths[1]:= max2;

  // very strange interaction with Themes/Styles
  Show;
  Hide;
end;

procedure TFObjectGenerator.ValueListEditorEditButtonClick(Sender: TObject);

  function StyleToStr(Style: TFontStyles): string;
  begin
    var s:= '';
    if Style = [fsBold] then s:= 'Bold' else
    if Style = [fsItalic] then s:= 'Italic' else
    if Style = [fsBold, fsItalic] then s:= 'Bold Italic';
    Result:= s;
  end;

begin
  with ValueListEditor do
    case Tags[Row] of
      TagLines:
        with TFStringEditorDialog.Create(Self) do begin
          var s:= MStrings.Lines.Text;
          if ShowModal = mrCancel then
            MStrings.Lines.Text:= s;
          Free;
        end;
      TagFont:
        with FontDialog do
         if Execute then
           Cells[1, Row]:= Font.Name + ' ' +  IntToStr(Font.Size) + ' ' + StyleToStr(Font.Style);
      TagIcon:
        with PictureDialog do begin
          InitialDir:= FConfiguration.SourcePath;
          if Execute then
            Cells[1, Row]:= ExtractFilename(Filename);
        end;
    end;
end;

procedure TFObjectGenerator.ValueListEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Return) or (Key = VK_Tab) then
    if ValueListEditor.Row < ValueListEditor.RowCount-1
      then ValueListEditor.Row:= ValueListEditor.Row + 1
    else if Key = VK_Return then
      ModalResult:= mrOK;
end;

procedure TFObjectGenerator.AddRow(Attribut: string; const Value: string; es: TEditStyle;
                                   const Pick: string; _Tag: integer);
begin
  if Pos('$', Attribut) > 0 then // enum type?
    Attribut[Pos('$', Attribut)]:= '.';
  ValueListEditor.InsertRow(Attribut, Value, true);
  var i:= ValueListEditor.RowCount - 1;
  case es of
    esPickList: ValueListEditor.ItemProps[i-1].PickList.Text:= Pick;
    esEllipsis: ValueListEditor.ItemProps[i-1].EditStyle:= esEllipsis;
  end;
  Tags[i]:= _Tag;
end;

function TFObjectGenerator.Edit(Attributes: TStringList; Row: integer): boolean;
  const PickBoolean = 'true'#13#10'false';
begin
  for var i:= 0 to Attributes.Count - 1 do begin
    var aValueObj:= TComJavaValue(Attributes.objects[i]);
    case aValueObj.Kind of
      ntBool  : AddRow(Attributes[i], aValueObj.AsString, esPickList, PickBoolean, TagBoolean);
      ntChar  : AddRow(Attributes[i], aValueObj.AsStringWithout(''''), esSimple, '', TagChar);
      ntByte,
      ntShort,
      ntInt,
      ntLong  : AddRow(Attributes[i], aValueObj.AsString, esSimple, '', TagNumber);
      ntFloat,
      ntDouble: AddRow(Attributes[i], aValueObj.AsString, esSimple, '', TagNormal);
      ntString: AddRow(Attributes[i], aValueObj.AsStringWithout('"'), esSimple, '', TagText);
      ntObject,
      ntBoolArray..ntObjectArray:
                AddRow(Attributes[i], aValueObj.AsString, esSimple, '', TagText);
    end;
  end;
  DeleteRow;
  SetRow(Row);
  SetColWidths;
  Result:= (ShowModal = mrOK);
  if assigned(FJava.ActiveTDIChild) then
    FJava.ActiveTDIChild.OpenWindow(Self);
end;

end.
