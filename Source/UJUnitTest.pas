unit UJUnitTest;

interface

uses
  Classes, Controls, Forms, ComCtrls, ExtCtrls, Menus, uDockForm, TB2Item,
  SpTBXItem, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, SVGIconImageCollection;

type
  TInteger = class
  public
    i: integer;
    constructor create(aI: Integer);
  end;

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
    FUndockLeft: Integer;
    FUndockTop: Integer;
    WindowOpened: boolean;
    procedure SaveWindow;
    procedure OpenWindow;
  public
    Pathname: string;
    procedure ShowIt;
    procedure HideIt;
    procedure DeleteData;
    procedure ChangeStyle;
    property UndockLeft: Integer read FUndockLeft write FUndockLeft;
    property UndockTop: Integer read FUndockTop write FUndockTop;
  end;

var
  FJUnitTests: TFJUnitTests;

implementation

uses
  SysUtils, Graphics, Dialogs, Math,
  JvGnugettext, UConfiguration, UUtils, UJava, UEditorForm, UBaseForm;

{$R *.dfm}

constructor TInteger.create(aI: Integer);
begin
  inherited create;
  self.i:= aI;
end;

procedure TFJUnitTests.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  WindowOpened:= false;
end;

procedure TFJUnitTests.FormShow(Sender: TObject);
begin
  if not WindowOpened then begin
    OpenWindow;
    WindowOpened:= true;
  end;
end;

procedure TFJUnitTests.TVJUnitTestsChange(Sender: TObject; Node: TTreeNode);
  var Line: Integer;
      EditForm: TFEditForm; aForm: TFForm;
      isWrapping: boolean;
begin
  FJava.SwitchWindowWithSearch(Pathname);
  if FJava.WindowOpened(Pathname, aForm) then begin
    EditForm:= aForm as TFEditForm;
    FJava.SwitchToWindow(EditForm);
    Line:= TInteger(Node.Data).i;
    isWrapping:= EditForm.Editor.WordWrap;
    with EditForm.Editor do begin
      if isWrapping then EditForm.SBWordWrapClick(nil);
      Topline:= Line;
      CaretY:= Line;
      CaretX:= 1; // Length(Lines[Line-1]) + 1;
      EnsureCursorPosVisible;
      if isWrapping then EditForm.SBWordWrapClick(nil);
    end;
    if EditForm.CanFocus then
      EditForm.SetFocus;
  end;
end;

procedure TFJUnitTests.TVJUnitTestsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbRight then
    PMJUnitTests.Popup(X+(Sender as TTreeView).ClientOrigin.X-40, Y+(Sender as TTreeView).ClientOrigin.Y-5);
end;

procedure TFJUnitTests.OpenWindow;
begin
  UndockLeft:= PPIScale(FConfiguration.ReadIntegerU('JUnitTest', 'UndockLeft', 400));
  UndockTop:= PPIScale(FConfiguration.ReadIntegerU('JUnitTest', 'UndockTop', 100));
  UndockWidth:= PPIScale(FConfiguration.ReadIntegerU('JUnitTest', 'UndockWidth', 200));
  UndockHeight:= PPIScale(FConfiguration.ReadIntegerU('JUnitTest', 'UndockHeight', 200));
  UndockLeft:= min(UndockLeft, Screen.DesktopWidth - 50);
  UndockTop:= min(UndockTop, Screen.DesktopHeight - 50);
  ManualFloat(Rect(UnDockLeft, UnDockTop, UnDockLeft + UnDockWidth, UnDockTop + UnDockHeight));
  Font.Name:= FConfiguration.ReadStringU('JUnitTest', 'Fontname', 'Segoe UI');
  Font.Size:= PPIScale(FConfiguration.ReadIntegerU('JUnitTest', 'Fontsize', 10));
end;

procedure TFJUnitTests.SaveWindow;
begin
  FConfiguration.WriteBoolU('JUnitTest', 'Visible', Visible);
  FConfiguration.WriteBoolU('JUnitTest', 'Floating', Floating);
  if Floating then begin
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockLeft', PPIUnScale(Left));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockTop',  PPIUnScale(Top));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockWidth', PPIUnScale(Width));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockHeight', PPIUnscale(Height));
    end
  else begin
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockLeft', PPIUnScale(UndockLeft));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockTop',  PPIUnScale(UndockTop));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockWidth', PPIUnScale(UndockWidth));
    FConfiguration.WriteIntegerU('JUnitTest', 'UndockHeight', PPIUnScale(UndockHeight));
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
  var TInt: TInteger;  i: integer;
begin
  for i:= 0 to TVJUnitTests.Items.Count -1 do begin
    TInt:= TInteger(TVJUnitTests.Items[i].Data);
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
  FJava.FDFont.Options:= [];
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
  if FConfiguration.isDark
    then PMJUnitTests.Images:= vilJUnitDark
    else PMJUnitTests.Images:= vilJUnitLight;
end;

end.
