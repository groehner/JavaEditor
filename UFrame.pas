unit UFrame;

interface

uses
  {$IFNDEF POSIX} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs;

{ von TFFrame sind abgeleitet (mit TypKennung)
                        FormTag
    // ein ActiveTDIChild
    TFEditForm      %E%   1
    TFUMLForm       %U%   2
    TFGUIForm       %G%   3
    TFTextDiff      %D%   4
    TFBrowser       %B%   5
    TFExplorer      %X%   6
    TFStructogram   %S%  11
    TFSequenceForm  %Q%  14

    // ein ActiveTool
    TFMessages            7
    TFObjectInspector     8
    TFObjectGenerator     9
    TFHint               10
    TFTooltip            12
    TSynBaseCompletionProposalFormEx 13
    TFConfiguration      15
    TFFileStructure      16
}


type

  { TFFrame }

  TFFrame= class(TFrame)
    {$IFNDEF POSIX} procedure WMSize(var Msg: TMessage); message WM_SIZE; {$ENDIF}
  private
  public
    Pathname: string;
    Number: integer;
    Partner: TFFrame;
    FormModified: boolean;
    AlreadySavedAs: boolean;
    FormTag: integer;
    FFrameType: integer; // 1..7, look in UEditor
    Lockenter: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure FormClose(Sender: TObject; var aAction: TCloseAction); virtual;
    procedure ReleaseWindow(aFree: boolean); virtual;
    procedure MIReleaseWindowClick(Sender: TObject); virtual;
    procedure Save(MitBackup: boolean); virtual;
    procedure SaveIn(const Dir: string); virtual;
    procedure SaveAs(const Filename: string); virtual;
    procedure Print; virtual;
    procedure setState(var s: string); virtual;
    procedure SetFont(aFont: TFont); virtual;
    function  GetFont: TFont; virtual;
    procedure SetFontSize(Delta: integer); virtual;
    procedure SetOptions; virtual;
    procedure Search; virtual;
    procedure SearchAgain; virtual;
    procedure Replace; virtual;
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard; virtual;
    procedure PasteFromClipboard; virtual;
    procedure Undo; virtual;
    procedure Redo; virtual;
    procedure GotoLine(i: integer); virtual;
    function GetSaveAsName: string; virtual;
    function getFormType: string; virtual;
    function getState: string; virtual;
    procedure OpenWindow(Sender: TObject); virtual;
    procedure Enter(Sender: TObject); virtual;
    procedure UpdateState; virtual;
    procedure SetLanguage; virtual;
    procedure SetModified(Modified: boolean); virtual;
    procedure CollectClasses(SL: TStringList); virtual;
    function getAllPathnames: string; virtual;

    function isAWT: boolean;
    function isSwing: boolean;
    function FrameTypToString: string;
    function MouseInWindow: boolean;
    function DefaultFilename: boolean;
    procedure ToMainPanel;
    function myActiveControl: TWinControl;
    procedure SetActiveControl(aControl: TWinControl);

  published
    property FrameType: integer read FFrameType write FFrameType;
  end;

var FormNumber: integer = 0;


implementation

uses UUtils, UJava, UConfiguration, UTabObject, UExceptionStack;

{$IFNDEF LCL}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

constructor TFFrame.Create(AOwner: TComponent);
begin
  inc(FormNumber);
  Number:= FormNumber;
  try
    inherited create(AOwner);
  except
  end;
  AlreadySavedAs:= false;
  Lockenter:= false;
  //FJava.TDIFormsList.Add(Self);
  //FJava.ActiveTDIChild:= Self;
end;

procedure TFFrame.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  FJava.DeleteTabAndWindow(Number);
  if not DefaultFilename and (FJava.Projectfilename = '') then
    FJava.RearrangeFileHistory(Pathname);
  //FJava.TDIFormsList.Remove(Self);
  if assigned(FJava.myTabBar.SelectedTab) and assigned(FJava.myTabBar.SelectedTab.Data)
    then FJava.ActiveTDIChild:= TTabObject(FJava.myTabBar.SelectedTab.Data).Form
    else FJava.ActiveTDIChild:= nil;
  aAction:= caFree;
end;

procedure TFFrame.ReleaseWindow(aFree: boolean);
begin
end;

procedure TFFrame.MIReleaseWindowClick(Sender: TObject);
begin
end;

procedure TFFrame.Save(MitBackup: boolean);
begin
end;

procedure TFFrame.SaveIn(const Dir: string);
begin
end;

function TFFrame.GetSaveAsName: string;
begin
  Result:= '';
end;

function TFFrame.GetFormType: string;
begin
  Result:= '';
end;

procedure TFFrame.SaveAs(const Filename: string);
begin
end;

procedure TFFrame.Print;
begin
//  {$IFNDEF LCL}inherited Print; {$ENDIF} // because of GUI-Formular-Print
end;

function TFFrame.isAWT: boolean;
begin
  Result:= FrameType in [2, 3, 4];
end;

function TFFrame.isSwing: boolean;
begin
  Result:= FrameType in [5, 6, 7];
end;

function TFFrame.GetState: string;
begin
  Result:= 'W' + IntToStr(Left) + ')' + IntToStr(Top) + ')' +
                 IntToStr(Width) + ')' + IntToStr(Height) + ')' ;
                 // +           WindowStateToStr(WindowState) + ')';
  if Parent = nil then Result:= Result + 'nil)';
end;

procedure TFFrame.setState(var s: string);
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
    end
    //else WindowState:= TWindowState(WS);
  end;
end;

procedure TFFrame.SetFont(aFont: TFont);
begin
  Self.Font.Assign(aFont);
end;

function TFFrame.GetFont: TFont;
begin
  Result:= Font;
end;

procedure TFFrame.SetFontSize(Delta: integer);
begin
end;

procedure TFFrame.SetOptions;
begin
end;

procedure TFFrame.Search;
begin
end;

procedure TFFrame.SearchAgain;
begin
end;

procedure TFFrame.Replace;
begin
end;

procedure TFFrame.CutToClipboard;
begin
end;

procedure TFFrame.CopyToClipboard;
begin
end;

procedure TFFrame.PasteFromClipboard;
begin
end;

procedure TFFrame.Undo;
begin
end;

procedure TFFrame.Redo;
begin
end;

procedure TFFrame.GotoLine(i: integer);
begin
end;

procedure TFFrame.OpenWindow(Sender: TObject);
begin
  try
    LockWindow(Self.Handle);
    try
      {$IFNDEF LCL}SetAnimation(false);{$ENDIF}
      if Parent = FJava.MainPanel then begin
        if Left > FJava.Width - 200 then
          Left:= FJava.Width - 200;
      end;
      {$IFNDEF LCL}SetAnimation(true);{$ENDIF}
//      if (FJava.EditorForm = nil) or (FJava.EditorForm <> Self) then        BringToFront;
      Enter(Self);
      //SetFocus;
    except on e: Exception do
      FExceptionStack.LogExceptionSilent(E, 'OpenWindow: ' + Pathname);
    end;
  finally
    UnlockWindow(Self.Handle);
  end;
end;

procedure TFFrame.Enter(Sender: TObject);
begin
  FJava.SetSelectedTabAndWindow(Number);
  FJava.myTabBarColorize;
  //FJava.ActiveTDIChild:= Self;
  FJava.Caption:= 'Java-Editor - ' + Pathname;
  FJava.UpdateMenuItems(Self);
  FJava.ActiveTool:= -1;
end;

procedure TFFrame.UpdateState;
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

procedure TFFrame.SetLanguage;
begin
end;

function TFFrame.FrameTypToString: string;
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

function TFFrame.MouseInWindow: boolean;
  var WinInfo: TWindowInfo;
      CurPos: TPoint;
begin
   FillChar(WinInfo, Sizeof(WinInfo), 0);
   WinInfo.cbSize := Sizeof(WinInfo);
   GetWindowInfo(Handle, WinInfo);
   GetCursorPos(CurPos);
   Result:= Windows.PtInRect(WinInfo.rcClient, CurPos);
end;

procedure TFFrame.SetModified(Modified: boolean);
begin
  if Self.FormModified <> Modified then begin
    Self.FormModified:= Modified;
    FJava.TabModified(Number, Modified);
  end;
end;

procedure TFFrame.CollectClasses(SL: TStringList);
begin
end;

function TFFrame.DefaultFilename: boolean;
 var i, e, p: Integer; s, Default: string;
begin
  Result:= false;
  if assigned(FConfiguration) then
    if not FConfiguration.AcceptDefaultname then begin
      Default:= Uppercase(FConfiguration.LNGFile);
      s:= Uppercase(ExtractFilename(Pathname));
      if Copy(s, 1, Length(Default)) = Default then begin
        delete(s, 1, Length(Default));
        p:= Pos('.', s);
        delete(s, p,length(s));
        val(s, i, e);
        Result:= (e = 0) and (i > 0);
        end
      else
        Result:= false;
    end;
end;

function TFFrame.getAllPathNames: string;
begin
  Result:= '';
end;

procedure TFFrame.WMSize(var Msg: TMessage);
begin
  inherited;
  if Msg.WParam  = SIZE_MAXIMIZED then
    FConfiguration.WindowStateMaximized:= true;
end;

procedure TFFrame.ToMainPanel;
begin
  Align:= alClient;
  Parent:= FJava.MainPanel;
end;

function TFFrame.myActiveControl: TWinControl;
// https://de.comp.lang.delphi.misc.narkive.com/AU1p5kiy/activecontrol-nil
begin
  Result:= Application.Mainform.ActiveControl;
end;

procedure TFFrame.SetActiveControl(aControl: TWinControl);
begin
  if assigned(Parent) then
    Application.Mainform.ActiveControl:= aControl;
end;

end.



