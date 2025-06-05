unit UJUnitTest;

interface

uses
  Classes,
  Controls,
  Forms,
  ComCtrls,
  ExtCtrls,
  Menus,
  TB2Item,
  SpTBXItem,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  UDockForm;

type

  TFJUnitTests = class(TDockableForm)
    PJUnit: TPanel;
    TVJUnitTests: TTreeView;
    PMJUnitTests: TSpTBXPopupMenu;
    MIFont: TSpTBXItem;
    MIDefaulLayout: TSpTBXItem;
    MIClose: TSpTBXItem;
    icJUnit: TSVGIconImageCollection;
    vilJUnitLight: TVirtualImageList;
    vilJUnitDark: TVirtualImageList;

    procedure TVJUnitTestsChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MIFontClick(Sender: TObject);
    procedure MIDefaulLayoutClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure TVJUnitTestsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FPathname: string;
    FUndockLeft: Integer;
    FUndockTop: Integer;
    FWindowOpened: Boolean;
    procedure SaveWindow;
    procedure OpenWindow;
  public
    procedure ShowIt;
    procedure HideIt;
    procedure DeleteData;
    procedure ChangeStyle;
    property Pathname: string read FPathname write FPathname;
    property UndockLeft: Integer read FUndockLeft write FUndockLeft;
    property UndockTop: Integer read FUndockTop write FUndockTop;
  end;

var
  FJUnitTests: TFJUnitTests;

implementation

uses
  SysUtils,
  Math,
  JvGnugettext,
  UConfiguration,
  UUtils,
  UBaseForm,
  UJava,
  UEditorForm;

{$R *.dfm}

procedure TFJUnitTests.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FWindowOpened := False;
end;

procedure TFJUnitTests.FormShow(Sender: TObject);
begin
  if not FWindowOpened then
  begin
    OpenWindow;
    FWindowOpened := True;
  end;
end;

procedure TFJUnitTests.TVJUnitTestsChange(Sender: TObject; Node: TTreeNode);
var
  Line: Integer;
  EditForm: TFEditForm;
  AForm: TFForm;
  IsWrapping: Boolean;
begin
  FJava.SwitchWindowWithSearch(FPathname);
  if FJava.WindowOpened(FPathname, AForm) then
  begin
    EditForm := AForm as TFEditForm;
    FJava.SwitchToWindow(EditForm);
    Line := TInteger(Node.Data).Int;
    IsWrapping := EditForm.Editor.WordWrap;
    with EditForm.Editor do
    begin
      if IsWrapping then
        EditForm.SBWordWrapClick(nil);
      TopLine := Line;
      CaretY := Line;
      CaretX := 1; // Length(Lines[Line-1]) + 1;
      EnsureCursorPosVisible;
      if IsWrapping then
        EditForm.SBWordWrapClick(nil);
    end;
    if EditForm.CanFocus then
      EditForm.SetFocus;
  end;
end;

procedure TFJUnitTests.TVJUnitTestsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    PMJUnitTests.Popup(X + (Sender as TTreeView).ClientOrigin.X - 40,
      Y + (Sender as TTreeView).ClientOrigin.Y - 5);
end;

procedure TFJUnitTests.OpenWindow;
begin
  UndockLeft := PPIScale(FConfiguration.ReadIntegerU('JUnitTest',
    'UndockLeft', 400));
  UndockTop := PPIScale(FConfiguration.ReadIntegerU('JUnitTest',
    'UndockTop', 100));
  UndockWidth := PPIScale(FConfiguration.ReadIntegerU('JUnitTest',
    'UndockWidth', 200));
  UndockHeight := PPIScale(FConfiguration.ReadIntegerU('JUnitTest',
    'UndockHeight', 200));
  UndockLeft := Min(UndockLeft, Screen.DesktopWidth - 50);
  UndockTop := Min(UndockTop, Screen.DesktopHeight - 50);
  ManualFloat(Rect(UndockLeft, UndockTop, UndockLeft + UndockWidth,
    UndockTop + UndockHeight));
  Font.Name := FConfiguration.ReadStringU('JUnitTest', 'Fontname', 'Segoe UI');
  Font.Size := PPIScale(FConfiguration.ReadIntegerU('JUnitTest',
    'Fontsize', 10));
end;

procedure TFJUnitTests.SaveWindow;
begin
  FConfiguration.WriteBoolU('JUnitTest', 'Visible', Visible);
  FConfiguration.WriteBoolU('JUnitTest', 'Floating', Floating);
  if Floating then
  begin
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockLeft', PPIUnScale(Left));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockTop', PPIUnScale(Top));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockWidth', PPIUnScale(Width));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockHeight',
      PPIUnScale(Height));
  end
  else
  begin
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockLeft',
      PPIUnScale(UndockLeft));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockTop',
      PPIUnScale(UndockTop));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockWidth',
      PPIUnScale(UndockWidth));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockHeight',
      PPIUnScale(UndockHeight));
  end;
  FConfiguration.WriteStringU('JUnitTest', 'Fontname', Font.Name);
  FConfiguration.WriteIntegerU('JUnitTest', 'Fontsize', PPIUnScale(Font.Size));
end;

procedure TFJUnitTests.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  SaveWindow;
  DeleteData;
end;

procedure TFJUnitTests.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveWindow;
end;

procedure TFJUnitTests.DeleteData;
var
  TInt: TInteger;
begin
  for var I := 0 to TVJUnitTests.Items.Count - 1 do
  begin
    TInt := TInteger(TVJUnitTests.Items[I].Data);
    FreeAndNil(TInt);
  end;
  TVJUnitTests.Items.Clear;
end;

procedure TFJUnitTests.MICloseClick(Sender: TObject);
begin
  HideIt;
end;

procedure TFJUnitTests.MIDefaulLayoutClick(Sender: TObject);
begin
  FJava.MIDefaultLayoutClick(Self);
end;

procedure TFJUnitTests.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options := [];
  if FJava.FDFont.Execute then
    Font.Assign(FJava.FDFont.Font);
end;

procedure TFJUnitTests.ShowIt;
begin
  FJava.ShowDockableForm(Self);
end;

procedure TFJUnitTests.HideIt;
begin
  Close;
end;

procedure TFJUnitTests.ChangeStyle;
begin
  if FConfiguration.IsDark then
    PMJUnitTests.Images := vilJUnitDark
  else
    PMJUnitTests.Images := vilJUnitLight;
end;

initialization

FJUnitTests := nil;

end.
