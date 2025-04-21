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
    FFrameType: Integer; // 1..8, look in UEditorForm
    class var FFormNumber: Integer; // class attribute
  protected
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); virtual;
    procedure UpdateState; virtual;
    function GetModified: Boolean; virtual;
    procedure ReleaseWindow(AFree: Boolean); virtual;
    procedure MIReleaseWindowClick(Sender: TObject); virtual;
    procedure GotoLine(Line: Integer); virtual;
    procedure SetModified(Modified: Boolean); virtual;
    function GetFrameType: Integer; virtual;
    procedure SetFrameType(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetOptions; virtual;
    function GetState: string; virtual;
    function GetFormType: string; virtual;
    function DefaultFilename: Boolean;
    procedure OpenWindow(Sender: TObject); virtual;
    function FrameTypToString: string;
    procedure CollectClasses(StringList: TStringList); virtual;
    function IsApplet: Boolean;
    function IsAWT: Boolean;
    function IsSwing: Boolean;
    function IsJavaFX: Boolean;
    procedure ToMainPanel;
    procedure SetActiveControl(AControl: TWinControl);
    function MyActiveControl: TWinControl;
    procedure SaveIn(const Dir: string); virtual;
    function GetSaveAsName: string; virtual;
    procedure Save(MitBackup: Boolean); virtual;
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
    property FrameType: Integer read GetFrameType write SetFrameType;
    property Modified: Boolean read GetModified write SetModified;
  end;

implementation

uses Windows, SysUtils, JvGnugettext, UStringRessources,
     UUtils, UJava, UConfiguration, UTabObject;

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
    if not DefaultFilename and (FJava.ProjectFilename = '') then
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

procedure TFForm.ReleaseWindow(AFree: Boolean);
begin
end;

procedure TFForm.MIReleaseWindowClick(Sender: TObject);
begin
end;

procedure TFForm.Save(MitBackup: Boolean);
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

function TFForm.IsApplet: Boolean;
begin
  Result:= (FrameType in [4, 7]);
end;

function TFForm.IsAWT: Boolean;
begin
  Result:= FrameType in [2, 3, 4];
end;

function TFForm.IsSwing: Boolean;
begin
  Result:= FrameType in [5, 6, 7];
end;

function TFForm.IsJavaFX: Boolean;
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

procedure TFForm.SetState(var State: string);
  var l, t, w, h, p: Integer; WS: TWindowState;
begin
  if State = '' then Exit;
  if Copy(State, 1, 1) = 'W' then begin
    p:= Pos(')', State); l:= StrToInt(Copy(State, 2, p-2)); Delete(State, 1, p);
    p:= Pos(')', State); t:= StrToInt(Copy(State, 1, p-1)); Delete(State, 1, p);
    p:= Pos(')', State); w:= StrToInt(Copy(State, 1, p-1)); Delete(State, 1, p);
    p:= Pos(')', State); h:= StrToInt(Copy(State, 1, p-1)); Delete(State, 1, p);
    p:= Pos(')', State); WS:= StrToWindowState(Copy(State, 1, p-1)); Delete(State, 1, p);
    p:= Pos(')', State);
    if Copy(State, 1, p) = 'nil)' then begin
      Delete(State, 1, p);
      ReleaseWindow(True);
      SetBounds(l, t, w, h);
    end else
      WindowState:= WS;
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

procedure TFForm.GotoLine(Line: Integer);
begin
end;

procedure TFForm.OpenWindow(Sender: TObject);
  var Animation: Boolean;
begin
  try
    LockWindow(Self.Handle);
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
end;

procedure TFForm.DPIChanged;
begin
  ScaleForCurrentDPI;
end;

procedure TFForm.DoExport;
begin
end;

procedure TFForm.CollectClasses(StringList: TStringList);
begin
end;

function TFForm.DefaultFilename: Boolean;
 var i, p: Integer; s, Default: string;
begin
  Result:= False;
  if Assigned(FConfiguration) then
    if not FConfiguration.AcceptDefaultname then begin
      Default:= UpperCase(_(LNGFile));
      s:= UpperCase(ExtractFileName(Pathname));
      if Copy(s, 1, Length(Default)) = Default then begin
        Delete(s, 1, Length(Default));
        p:= Pos('.', s);
        Delete(s, p, Length(s));
        Result:= TryStrToInt(s, i);
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

function TFForm.GetFrameType: Integer;
begin
  Result:= FFrameType;
end;

procedure TFForm.SetFrameType(Value: Integer);
begin
  FFrameType:= Value;
end;

initialization

  TFForm.FFormNumber:= 0;

end.
