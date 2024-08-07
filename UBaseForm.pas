unit UBaseForm;

interface

uses
  Messages, Classes, Graphics, Controls, Forms;

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
    {$IFNDEF POSIX}
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    {$ENDIF}
  private
    FModified: boolean;
    FFrameType: integer; // 1..8, look in UEditorForm
    class var FormNumber: integer; // class attribute
  protected
    procedure FormClose(Sender: TObject; var aAction: TCloseAction); virtual;
    procedure UpdateState; virtual;
    function GetModified: boolean; virtual;
    procedure ReleaseWindow(aFree: boolean); virtual;
    procedure MIReleaseWindowClick(Sender: TObject); virtual;
    procedure GotoLine(i: integer); virtual;
    procedure SetModified(Modified: boolean); virtual;
  public
    Number: integer;
    Pathname: string;
    Partner: TFForm;
    AlreadySavedAs: boolean;
    FormTag: integer;
    Lockenter: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure SetOptions; virtual;
    function getState: string; virtual;
    function getFormType: string; virtual;
    function DefaultFilename: boolean;
    procedure OpenWindow(Sender: TObject); virtual;
    function FrameTypToString: string;
    procedure CollectClasses(SL: TStringList); virtual;
    function isApplet: boolean;
    function isAWT: boolean;
    function isSwing: boolean;
    function isJavaFX: boolean;
    procedure ToMainPanel;
    procedure SetActiveControl(aControl: TWinControl);
    function myActiveControl: TWinControl;
    procedure SaveIn(const Dir: string); virtual;
    function GetSaveAsName: string; virtual;
    procedure Save(MitBackup: boolean); virtual;
    procedure SaveAs(const Filename: string); virtual;
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
    procedure SetFont(aFont: TFont); virtual;
    function  GetFont: TFont; virtual;
    procedure SetFontSize(Delta: integer); virtual;
    procedure Enter(Sender: TObject); virtual;
    procedure setState(var s: string); virtual;
    function getAllPathnames: TStringList; virtual;
    function getAllClassnames: TStringList; virtual;
    procedure ChangeStyle; virtual;
    procedure DPIChanged; virtual;

  published
    property FrameType: integer read FFrameType write FFrameType;
    property Modified: boolean read getModified write setModified;
  end;

implementation

uses Windows, SysUtils, JvGnugettext, UStringRessources,
     UUtils, UJava, UConfiguration, UTabObject;

{$R *.dfm}

constructor TFForm.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Partner:= nil;
  inc(FormNumber);
  Number:= FormNumber;
  AlreadySavedAs:= false;
  Lockenter:= false;
  onClose:= FormClose;
  onMouseActivate:= FormMouseActivate;
  ParentFont:= false;
  // this garanties, that the form is scaled to effectiv dpi
  // scaling is not done fore every position, see: procedure TCustomForm.SetWindowToMonitor
  // SetBounds calls TCustomForm.WMDpiChanged
  Position:= poScreenCenter;
end;

procedure TFForm.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  try
    FJava.DeleteTabAndWindow(Number);
    if not DefaultFilename and (FJava.Projectfilename = '') then
      FJava.RearrangeFileHistory(Pathname);
  finally
    FJava.TDIFormsList.Remove(Self);
    if assigned(FJava.myTabBar.SelectedTab) and assigned(FJava.myTabBar.SelectedTab.Data)
      then FJava.ActiveTDIChild:= TTabObject(FJava.myTabBar.SelectedTab.Data).Form
      else FJava.ActiveTDIChild:= nil;
    aAction:= caFree;
  end;
end;

procedure TFForm.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  if (FJava.ActiveTDIChild <> Self) or (FJava.ActiveTool > 0) then
    enter(self);
  MouseActivate:= maActivate;
end;

procedure TFForm.ReleaseWindow(aFree: boolean);
begin
end;

procedure TFForm.MIReleaseWindowClick(Sender: TObject);
begin
end;

procedure TFForm.Save(MitBackup: boolean);
begin
end;

procedure TFForm.SaveIn(const Dir: string);
begin
end;

function TFForm.GetSaveAsName: string;
begin
  Result:= '';
end;

function TFForm.GetFormType: string;
begin
  Result:= '';
end;

procedure TFForm.SaveAs(const Filename: string);
begin
end;

procedure TFForm.Print;
begin
  inherited Print; // because of GUI-Formular-Print
end;

function TFForm.isApplet: boolean;
begin
  Result:= (FrameType in [4, 7]);
end;

function TFForm.isAWT: boolean;
begin
  Result:= FrameType in [2, 3, 4];
end;

function TFForm.isSwing: boolean;
begin
  Result:= FrameType in [5, 6, 7];
end;

function TFForm.isJavaFX: boolean;
begin
  Result:= FrameType = 8;
end;

function TFForm.GetState: string;
begin
  Result:= 'W' + IntToStr(Left) + ')' + IntToStr(Top) + ')' +
                 IntToStr(Width) + ')' + IntToStr(Height) + ')' +
                 WindowStateToStr(WindowState) + ')';
  if Parent = nil then Result:= Result + 'nil)';
end;

procedure TFForm.setState(var s: string);
  var l, t, w, h, p: integer; WS: TWindowState;
begin
  if s = '' then exit;
  if copy(s, 1, 1) = 'W' then begin
    p:= Pos(')', s); l:= StrToInt(Copy(s, 2, p-2)); delete(s, 1, p);
    p:= Pos(')', s); t:= StrToInt(Copy(s, 1, p-1)); delete(s, 1, p);
    p:= Pos(')', s); w:= StrToInt(Copy(s, 1, p-1)); delete(s, 1, p);
    p:= Pos(')', s); h:= StrToInt(Copy(s, 1, p-1)); delete(s, 1, p);
    p:= Pos(')', s); WS:= StrToWindowState(Copy(s, 1, p-1)); delete(s, 1, p);
    p:= Pos(')', s);
    if copy(s, 1, p) = 'nil)' then begin
      delete(s, 1, p);
      ReleaseWindow(true);
      SetBounds(l, t, w, h);
    end else
      WindowState:= TWindowState(WS);
  end;
end;

procedure TFForm.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
end;

function TFForm.GetFont: TFont;
begin
  Result:= Font;
end;

procedure TFForm.SetFontSize(Delta: integer);
begin
  Font.Size:= Font.Size + Delta;
  if Font.Size < 6 then Font.Size:= 6;
  setFont(Font);
end;

procedure TFForm.SetOptions;
begin
end;

procedure TFForm.Search;
begin
end;

procedure TFForm.SearchAgain;
begin
end;

procedure TFForm.Replace;
begin
end;

procedure TFForm.CutToClipboard;
begin
end;

procedure TFForm.CopyToClipboard;
begin
end;

procedure TFForm.PasteFromClipboard;
begin
end;

procedure TFForm.Undo;
begin
end;

procedure TFForm.Redo;
begin
end;

procedure TFForm.GotoLine(i: integer);
begin
end;

procedure TFForm.OpenWindow(Sender: TObject);
  var Animation: boolean;
begin
  try
    LockWindow(Self.Handle);
    try
      Animation:= GetAnimation;
      if Animation then
        SetAnimation(false);
      if Parent = FJava.MainPanel then begin
        if Left > FJava.Width - 200 then
          Left:= FJava.Width - 200;
      end;
      if not Visible then begin
        Visible:= true;
        FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
      end;
      if Animation then
        SetAnimation(true);
      BringToFront;
      Enter(Self);
    except on e: Exception do
      FConfiguration.Log('OpenWindow: ' + Pathname, e);
    end;
  finally
    UnlockWindow;
  end;
end;

procedure TFForm.Enter(Sender: TObject);
begin
  FJava.ActiveTool:= -1;
  FJava.SetSelectedTabAndWindow(Number);
  FJava.myTabBarColorize;
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

function TFForm.FrameTypToString: string;
begin
  case FrameType of
    8: Result:= 'Application';
    7: Result:= 'JApplet';
    6: Result:= 'JDialog';
    5: Result:= 'JFrame';
    4: Result:= 'Applet';
    3: Result:= 'Dialog';
    2: Result:= 'Frame';
  else Result:= '';
  end;
end;

procedure TFForm.SetModified(Modified: boolean);
begin
  if FModified <> Modified then begin
    FModified:= Modified;
    FJava.TabModified(Number, Modified);
  end;
end;

function TFForm.GetModified: boolean;
begin
  Result:= FModified;
end;

procedure TFForm.ChangeStyle;
begin
end;

procedure TFForm.DPIChanged;
begin
  ScaleForCurrentDPI;
end;

procedure TFForm.DoExport;
begin
end;

procedure TFForm.CollectClasses(SL: TStringList);
begin
end;

function TFForm.DefaultFilename: boolean;
 var i, p: Integer; s, Default: string;
begin
  Result:= false;
  if assigned(FConfiguration) then
    if not FConfiguration.AcceptDefaultname then begin
      Default:= Uppercase(_(LNGFile));
      s:= Uppercase(ExtractFilename(Pathname));
      if Copy(s, 1, Length(Default)) = Default then begin
        delete(s, 1, Length(Default));
        p:= Pos('.', s);
        delete(s, p,length(s));
        Result:= TryStrToInt(s, i);
        end
      else
        Result:= false;
    end;
end;

function TFForm.getAllPathNames: TStringList;
begin
  Result:= TStringList.Create;
end;

function TFForm.getAllClassNames: TStringList;
begin
  Result:= TStringList.Create;
end;

procedure TFForm.WMSize(var Msg: TMessage);
begin
  inherited;
  if Msg.WParam  = SIZE_MAXIMIZED then
    FConfiguration.WindowStateMaximized:= true;
end;

procedure TFForm.ToMainPanel;
begin
  Align:= alClient;
  BorderStyle:= bsNone;
  BorderIcons:= [];
  Parent:= FJava.MainPanel;
end;

function TFForm.myActiveControl: TWinControl;
// https://de.comp.lang.delphi.misc.narkive.com/AU1p5kiy/activecontrol-nil
begin
  Result:= Application.Mainform.ActiveControl;
end;

procedure TFForm.SetActiveControl(aControl: TWinControl);
begin
  if assigned(Parent) then
    Application.Mainform.ActiveControl:= aControl;
end;

initialization

  TFForm.FormNumber:= 0;


end.
