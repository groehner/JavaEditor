unit UBaseForm;

interface

uses
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms;

{ are derived from TFForm (with type identifier)
                        FormTag
    // a ActiveTDIChild
    TFEditForm      %E%   1    FormMouseActivate
    TFUMLForm       %U%   2    FormMouseActivate
    TFGUIForm       %G%   3    FormMouseActivate   but no TDIForm
    TFTextDiff      %D%   4    FormMouseActivate
    TFBrowser       %B%   5    FormMouseActivate
    TFExplorer      %X%   6    FormMouseActivate
    TFStructogram   %S%  11    FormMouseActivate
    TFSequenceForm  %Q%  14    FormMouseActivate

    // a ActiveTool                    FJava.ActiveTool:=
    TFMessages            7      X       nicht modal FormMouseActivate + FormShow  FormClose -1
    TFObjectInspector     8      X       nicht modal FormMouseActivate + FormShow  FormClose -1
    TFObjectGenerator     9      -             modal                     FormShow  FormClose -1
    TFScpHint            10      -       nicht modal FormMouseActivate + FormShow  FormClose -1
    TFTooltip            12      -       nicht modal FormMouseActivate + FormShow  FormClose -1
    TSynBaseCompletionProposalFormEx 13
    TFConfiguration      15      -             modal                     FormShow  FormClose -1
    TFFileStructure      16      X       nicht modal FormMouseActivate + FormShow  FormClose -1

    TFGUIForm            17              nicht modal           because no TDIForm DOPPELT?
}


type

  TFormKind = (fkEditor, fkUML, fkGUI, fkFXGUI, fkTextDiff, fkBrowser, fkExplorer, fkStructogram, fkSequence);

  { TFForm }

  TFForm = class(TForm)
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FModified: Boolean;
    FNumber: Integer;
    FPathname: string;
    FPartner: TFForm;
    FFormTag: Integer;
    FLockEnter: Boolean;
    FHasFocus : Boolean;
    FAlreadySavedAs: Boolean;
    class var FFormNumber: Integer; // class attribute
  protected
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); virtual;
    procedure UpdateState; virtual;
    function GetModified: Boolean; virtual;
    procedure SetModified(Modified: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetOptions; virtual;
    function GetState: string; virtual;
    function GetFormType: string; virtual;
    function IsDefaultFilename: Boolean;
    procedure OpenWindow(Sender: TObject); virtual;
    procedure CollectClasses(StringList: TStringList); virtual;
    procedure ToMainPanel;
    procedure SetActiveControl(AControl: TWinControl);
    function MyActiveControl: TWinControl;
    procedure SaveIn(const Dir: string); virtual;
    function GetSaveAsName: string; virtual;
    procedure Save(WithBackup: Boolean); virtual;
    procedure DoExport; virtual;
    procedure Print; virtual;
    procedure Search; virtual;
    procedure SearchAgain; virtual;
    procedure Replace; virtual;
    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure SetFont(AFont: TFont); virtual;
    function  GetFont: TFont; virtual;
    procedure SetFontSize(Delta: Integer); virtual;
    procedure Enter(Sender: TObject); virtual;
    procedure SetState(var State: string); virtual;
    function GetAllPathnames: TStringList; virtual;
    function GetAllClassnames: TStringList; virtual;
    procedure ChangeStyle; virtual;
    procedure DPIChanged; virtual;

    property Number: Integer read FNumber;
    property HasFocus: Boolean read FHasFocus;
    property Pathname: string read FPathname write FPathname;
    property Partner: TFForm read FPartner write FPartner;
    property FormTag: Integer read FFormTag write FFormTag;
    property LockEnter: Boolean read FLockEnter write FLockEnter;
    property AlreadySavedAs: Boolean read FAlreadySavedAs write FAlreadySavedAs;
    property Modified: Boolean read GetModified write SetModified;
  end;

implementation

uses
  Windows,
  SysUtils,
  JvGnugettext,
  UStringRessources,
  UUtils,
  UJava,
  UConfiguration,
  UTabObject;

{$R *.dfm}

constructor TFForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPartner:= nil;
  Inc(FFormNumber);
  FNumber:= FFormNumber;
  FAlreadySavedAs:= False;
  FLockEnter:= False;
  OnClose:= FormClose;
  OnMouseActivate:= FormMouseActivate;
  ParentFont:= False;
  // this garanties, that the form is scaled to effectiv dpi
  // scaling is not done fore every position, see: procedure TCustomForm.SetWindowToMonitor
  // SetBounds calls TCustomForm.WMDpiChanged
  Position:= poScreenCenter;
end;

procedure TFForm.FormActivate(Sender: TObject);
begin
  FHasFocus := True;
end;

procedure TFForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  try
    FJava.DeleteTabAndWindow(Number);
    if not IsDefaultFilename and (FJava.ProjectFilename = '') then
      FJava.RearrangeFileHistory(Pathname);
  finally
    FJava.TDIFormsList.Remove(Self);
    if Assigned(FJava.myTabBar.SelectedTab) and Assigned(FJava.myTabBar.SelectedTab.Data)
      then FJava.ActiveTDIChild:= TTabObject(FJava.myTabBar.SelectedTab.Data).Form
      else FJava.ActiveTDIChild:= nil;
    AAction:= caFree;
  end;
end;

procedure TFForm.FormDeactivate(Sender: TObject);
begin
  FHasFocus := False;
end;

procedure TFForm.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  if (FJava.ActiveTDIChild <> Self) or (FJava.ActiveTool > 0) then
    Enter(Self);
  MouseActivate:= maActivate;
end;

procedure TFForm.Save(WithBackup: Boolean);
begin
  // in some descendet classes Save isn't needed
end;

procedure TFForm.SaveIn(const Dir: string);
begin
  // nothing to do
end;

function TFForm.GetSaveAsName: string;
begin
  Result:= '';
end;

function TFForm.GetFormType: string;
begin
  Result:= '';
end;

procedure TFForm.Print;
begin
  inherited Print; // because of GUI-Formular-Print
end;

function TFForm.GetState: string;
begin
  Result:= 'W' + IntToStr(Left) + ')' + IntToStr(Top) + ')' +
                 IntToStr(Width) + ')' + IntToStr(Height) + ')' +
                 WindowStateToStr(WindowState) + ')';
end;

procedure TFForm.SetState(var State: string);
  var Posi: Integer;
begin
  if State = '' then Exit;
  if Copy(State, 1, 1) = 'W' then begin
    Posi:= Pos(')', State); Delete(State, 1, Posi);
    Posi:= Pos(')', State); Delete(State, 1, Posi);
    Posi:= Pos(')', State); Delete(State, 1, Posi);
    Posi:= Pos(')', State); Delete(State, 1, Posi);
    Posi:= Pos(')', State);
    WindowState:= StrToWindowState(Copy(State, 1, Posi - 1));
  end;
end;

procedure TFForm.SetFont(AFont: TFont);
begin
  Font.Assign(AFont);
end;

function TFForm.GetFont: TFont;
begin
  Result:= Font;
end;

procedure TFForm.SetFontSize(Delta: Integer);
begin
  Font.Size:= Font.Size + Delta;
  if Font.Size < 6 then Font.Size:= 6;
  SetFont(Font);
end;

procedure TFForm.SetOptions;
begin
  // nothing to do
end;

procedure TFForm.Search;
begin
  // nothing to do
end;

procedure TFForm.SearchAgain;
begin
  // nothing to do
end;

procedure TFForm.Replace;
begin
  // nothing to do
end;

procedure TFForm.CutToClipboard;
begin
  // nothing to do
end;

procedure TFForm.CopyToClipboard;
begin
  // nothing to do
end;

procedure TFForm.PasteFromClipboard;
begin
  // nothing to do
end;

procedure TFForm.Undo;
begin
  // nothing to do
end;

procedure TFForm.Redo;
begin
  // nothing to do
end;

procedure TFForm.OpenWindow(Sender: TObject);
  var Animation: Boolean;
begin
  LockWindow(Self.Handle);
  try
    try
      Animation:= GetAnimation;
      if Animation then
        SetAnimation(False);
      if Parent = FJava.MainPanel then begin
        if Left > FJava.Width - 200 then
          Left:= FJava.Width - 200;
      end;
      if not Visible then begin
        Visible:= True;
        FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
      end;
      if Animation then
        SetAnimation(True);
      BringToFront;
      Enter(Self);
    except
      on E: Exception do
        FConfiguration.Log('OpenWindow: ' + Pathname, E);
    end;
  finally
    UnlockWindow;
  end;
end;

procedure TFForm.Enter(Sender: TObject);
begin
  FJava.ActiveTool:= -1;
  FJava.SetSelectedTabAndWindow(Number);
  FJava.MyTabBarColorize;
  FJava.ActiveTDIChild:= Self;
  FJava.UpdateMenuItems(Self);
  ChangeStyle;
end;

procedure TFForm.UpdateState;
begin
  with FJava do begin
    DisableMI(MIUndo);
    DisableTB(TBUndo);
    DisableMI(MIRedo);
    DisableTB(TBRedo);
    DisableMI(MICopyRTF);
    DisableMI(MICopyHTML);
    DisableMI(MICopyHTMLAsText);
    DisableMI(MICopyNumbered);
    DisableMI(MICopyRtfNumbered);
  end;
end;

procedure TFForm.SetModified(Modified: Boolean);
begin
  if FModified <> Modified then begin
    FModified:= Modified;
    FJava.TabModified(Number, Modified);
  end;
end;

function TFForm.GetModified: Boolean;
begin
  Result:= FModified;
end;

procedure TFForm.ChangeStyle;
begin
  // nothing to do
end;

procedure TFForm.DPIChanged;
begin
  ScaleForCurrentDPI;
end;

procedure TFForm.DoExport;
begin
  // nothing to do
end;

procedure TFForm.CollectClasses(StringList: TStringList);
begin
  // nothing to do
end;

function TFForm.IsDefaultFilename: Boolean;
 var Posi: Integer; Str, Default: string;
begin
  Result:= False;
  if Assigned(FConfiguration) then
    if not FConfiguration.AcceptDefaultname then begin
      Default:= UpperCase(_(LNGFile));
      Str:= UpperCase(ExtractFileName(Pathname));
      if Copy(Str, 1, Length(Default)) = Default then begin
        Delete(Str, 1, Length(Default));
        Posi:= Pos('.', Str);
        Delete(Str, Posi, Length(Str));
        Result:= TryStrToInt(Str, Posi);
        end
      else
        Result:= False;
    end;
end;

function TFForm.GetAllPathnames: TStringList;
begin
  Result:= TStringList.Create;
end;

function TFForm.GetAllClassnames: TStringList;
begin
  Result:= TStringList.Create;
end;

procedure TFForm.WMSize(var Msg: TMessage);
begin
  inherited;
  if Msg.WParam  = SIZE_MAXIMIZED then
    FConfiguration.WindowStateMaximized:= True;
end;

procedure TFForm.ToMainPanel;
begin
  Align:= alClient;
  BorderStyle:= bsNone;
  BorderIcons:= [];
  Parent:= FJava.MainPanel;
end;

function TFForm.MyActiveControl: TWinControl;
// https://de.comp.lang.delphi.misc.narkive.com/AU1p5kiy/activecontrol-nil
begin
  Result:= Application.MainForm.ActiveControl;
end;

procedure TFForm.SetActiveControl(AControl: TWinControl);
begin
  if Assigned(Parent) then
    Application.MainForm.ActiveControl:= AControl;
end;

initialization

  TFForm.FFormNumber:= 0;

end.
