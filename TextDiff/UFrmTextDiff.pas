unit UFrmTextDiff;

// -----------------------------------------------------------------------------
// Application:     TextDiff                                                   .
// Module:          Main                                                       .
// Version:         4.3                                                        .
// Date:            09.07.2022                                                 .
// Target:          Win32, Delphi 7                                            .
// Author:          Angus Johnson - angusj-AT-myrealbox-DOT-com                .
// Copyright:       © 2003-2004 Angus Johnson                                  .
//                  © 2006-2024 Gerhard Röhner                                 .
// -----------------------------------------------------------------------------

interface

uses
  Windows, Graphics, Controls, Forms, ExtCtrls, Menus, ComCtrls, Classes,
  UFrmBaseform, UFrmEditor, UDiff, USynEditExDiff, SynEditTextBuffer,
  System.ImageList, Vcl.ImgList, Vcl.ToolWin, SVGIconImageListBase,
  SVGIconVirtualImageList, Vcl.BaseImageCollection, SVGIconImageCollection,
  Vcl.VirtualImageList, TB2Item, SpTBXItem, SpTBXSkins;

type

  TFTextDiff = class(TFForm)
    TBTextDiff: TToolBar;
    TBView: TToolButton;
    TBNext: TToolButton;
    TBPrev: TToolButton;
    TBDiffsOnly: TToolButton;
    TBCopyBlockLeft: TToolButton;
    TBCopyBlockRight: TToolButton;
    PMain: TPanel;
    Splitter: TSplitter;
    PLeft: TPanel;
    PCaptionLeft: TPanel;
    PRight: TPanel;
    PCaptionRight: TPanel;
    TBCompare: TToolButton;
    TBSourcecode: TToolButton;
    TBClose: TToolButton;
    TBIgnoreCase: TToolButton;
    TBIgnoreBlanks: TToolButton;
    TBParagraph: TToolButton;
    icTextDiff: TSVGIconImageCollection;
    vilTextDiffLight: TVirtualImageList;
    TBZoomOut: TToolButton;
    TBZoomIn: TToolButton;
    StatusBar: TSpTBXStatusBar;
    vilTextDiffDark: TVirtualImageList;
    liDeleted: TSpTBXLabelItem;
    liModified: TSpTBXLabelItem;
    liAdded: TSpTBXLabelItem;
    liEncoding: TSpTBXLabelItem;
    liInsOvr: TSpTBXLabelItem;
    liModifiedProtected: TSpTBXLabelItem;
    liDifferences: TSpTBXLabelItem;
    liLineColumn: TSpTBXLabelItem;
    PopupEditor: TSpTBXPopupMenu;
    MIClose: TSpTBXItem;
    MIPaste: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    MICopy: TSpTBXItem;
    MICut: TSpTBXItem;
    MIRedo: TSpTBXItem;
    MIUndo: TSpTBXItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;

    procedure HorzSplitClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
    procedure PrevClick(Sender: TObject);
    procedure CopyBlockLeftClick(Sender: TObject);
    procedure CopyBlockRightClick(Sender: TObject);
    procedure TBDiffsOnlyClick(Sender: TObject);
    procedure TBCompareClick(Sender: TObject);
    procedure TBSourcecodeClick(Sender: TObject);
    procedure TBCloseClick(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure MICutClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);
    procedure MIUndoClick(Sender: TObject);
    procedure MIRedoClick(Sender: TObject);
    procedure TBIgnoreCaseClick(Sender: TObject);
    procedure TBIgnoreBlanksClick(Sender: TObject);
    procedure TBParagraphClick(Sender: TObject);
    procedure TBZoomOutClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure liDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
  private
    Diff: TDiff;
    Lines1, Lines2: TSynEditStringList;
    addClr, delClr, modClr, DefaultClr: TColor;
    CodeEdit1: TSynEditExDiff;
    CodeEdit2: TSynEditExDiff;
    OnlyDifferences: boolean;
    IgnoreBlanks: boolean;
    IgnoreCase: boolean;
    procedure LinkScroll(IsLinked: boolean);
    procedure DoCompare;
    procedure DoSaveFile(Nr: integer; WithBackup: boolean);
    procedure DoLoadFile(const Filename: string; Nr: integer);
    procedure ChooseFiles(F1: TFEditForm);
    procedure DisplayDiffs;
    procedure SynEditEnter(Sender: TObject);
  public
    Nr: integer;
    FilesCompared: boolean;
    constructor Create(AOwner: TComponent); override;
    procedure New(F1, F2: TFEditForm);
    procedure Open(const Filename: string); overload;
    procedure Open(const Filename1, Filename2: string); overload;
    procedure ShowDiffState;
    procedure SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);

    procedure Search; override;
    procedure SearchAgain; override;
    procedure Replace; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure GotoLine(i: integer); override;
    procedure DoSaveBoth;
    procedure ShowFileNames;
    function GetCodeEdit: TSynEditExDiff;
    procedure SetFilesCompared(Value: boolean);
    procedure Save(WithBackup: boolean); override;
    function  GetFont: TFont; override;
    procedure SetFont(aFont: TFont); override;
    procedure SetFontSize(Delta: integer); override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure ChangeStyle; override;
    function getFormType: string; override;
    procedure DPIChanged; override;
  end;

implementation

uses SysUtils, Themes, Dialogs, SynEdit, JvGnugettext, UStringRessources,
     UJava, UConfiguration, UUtils, UHashUnit;

{$R *.DFM}

constructor TFTextDiff.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 4;
end;

procedure TFTextDiff.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  var State   := FConfiguration.ReadStringU('TextDiff', 'State', '');
  IgnoreCase  := FConfiguration.ReadBoolU('TextDiff', 'IgnoreCase', false);
  IgnoreBlanks:= FConfiguration.ReadBoolU('TextDiff', 'IgnoreBlanks', false);
  TBIgnoreCase.Down:= IgnoreCase;
  TBIgnoreBlanks.Down:= IgnoreBlanks;
  SetState(State);

  //the diff engine ...
  Diff:= TDiff.create(self);
  //Diff.OnProgress:= DiffProgress;

  //lines1 & lines2 contain the unmodified files
  Lines1:= TSynEditStringList.Create(nil);
  Lines2:= TSynEditStringList.Create(nil);

  //edit windows where color highlighing of diffs and changes are displayed ...
  CodeEdit1:= TSynEditExDiff.create(self);
  with CodeEdit1 do begin
    Nr:= 1;
    Parent:= PLeft;
    Align:= alClient;
    PCaption:= PCaptionLeft;
    PopupMenu:= PopUpEditor;
    onEnter:= SynEditEnter;
  end;
  CodeEdit2:= TSynEditExDiff.create(self);
  with CodeEdit2 do begin
    Nr:= 2;
    Parent:= PRight;
    Align:= alClient;
    PCaption:= PCaptionRight;
    PopupMenu:= PopUpEditor;
    onEnter:= SynEditEnter;
  end;
  Nr:= 1;
  SetFont(FConfiguration.EditFont);
  OnlyDifferences:= false;
  OnClose:= FormClose;
  OnMouseActivate:= FormMouseActivate;
  TBCopyBlockLeft.Hint:= _(LNGCopyBlockLeft);
  TBCopyBlockRight.Hint:= _(LNGCopyBlockRight);
  FConfiguration.RemoveShortcutsFrom(PopupEditor);
  ChangeStyle;
end;

procedure TFTextDiff.New(F1, F2: TFEditForm);
begin
  Pathname:= Caption;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  if assigned(F1) and assigned(F2) then
    Open(F1.Pathname, F2.Pathname)
  else begin
    if assigned(F1) then
      Open(F1.Pathname);
    ChooseFiles(F1);
  end;
  DoCompare;
end;

procedure TFTextDiff.liDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; ItemInfo: TSpTBXMenuItemInfo;
  const PaintStage: TSpTBXPaintStage; var PaintDefault: Boolean);
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault:= False;
    var aColor:= clBtnFace;
    case (Sender as TSpTBXLabelItem).Tag of
      1: aColor:= addClr;
      2: aColor:= modClr;
      3: aColor:= delClr;
    end;
    ACanvas.Brush.Color:= aColor;
    ACanvas.Pen.Color:= aColor;
    ACanvas.Rectangle(ARect);
  end;
end;

procedure TFTextDiff.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FreeAndNil(Diff);
  FreeAndNil(CodeEdit1);
  FreeAndNil(CodeEdit2);
  FreeAndNil(Lines1);
  FreeAndNil(Lines2);
  Action:= caFree;
end;

procedure TFTextDiff.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FConfiguration.WriteStringU('TextDiff', 'State', GetState);
  FConfiguration.WriteBoolU('TextDiff', 'IgnoreCase', IgnoreCase);
  FConfiguration.WriteBoolU('TextDiff', 'IgnoreBlanks', IgnoreBlanks);
  if OnlyDifferences then TBDiffsOnlyClick(Self);
  DoSaveBoth;
  FJava.EditorAgeTimer.Enabled:= FConfiguration.CheckAge;
  CanClose:= true;
end;

procedure TFTextDiff.TBDiffsOnlyClick(Sender: TObject);
  var CodeEdit: TSynEditExDiff; Caret: TBufferCoord;
begin
  if OnlyDifferences then begin
    OnlyDifferences:= false;
    CodeEdit1.ReadOnly:= false;
    CodeEdit2.ReadOnly:= false;
    CodeEdit1.Gutter.Autosize:= true;
    CodeEdit2.Gutter.Autosize:= true;
    TBCopyBlockLeft.Enabled:= true;
    TBCopyBlockRight.Enabled:= true;
    CodeEdit:= GetCodeEdit;
    Caret:= CodeEdit.CaretXY;
    if Caret.Line - 1 > 0 then
      Caret.Line:= CodeEdit.GetLineObj(Caret.Line-1).Tag;
    DisplayDiffs;
    CodeEdit.CaretXY:= Caret;
    SyncScroll(GetCodeEdit, sbVertical);
  end else begin
    DoSaveBoth;
    if not FilesCompared then DoCompare;
    if FilesCompared then begin
      OnlyDifferences:= true;
      CodeEdit1.Gutter.Autosize:= false;
      CodeEdit2.Gutter.Autosize:= false;
      DisplayDiffs;
      CodeEdit1.ReadOnly:= true;
      CodeEdit2.ReadOnly:= true;
      TBCopyBlockLeft.Enabled:= false;
      TBCopyBlockRight.Enabled:= false;
    end;
  end;
end;

function TFTextDiff.getFormType: string;
begin
  Result:= '%D%';
end;

procedure TFTextDiff.Save(WithBackup: boolean);
begin
  DoSaveFile(GetCodeEdit.Nr, WithBackup);
end;

procedure TFTextDiff.Open(const Filename: string);
begin
  DoLoadFile(Filename, Nr);
end;

procedure TFTextDiff.DoLoadFile(const Filename: string; Nr: integer);
begin
  if OnlyDifferences then
    TBDiffsOnlyClick(Self);
  SetFilesCompared(false);
  LinkScroll(false);
  if Nr = 1
    then CodeEdit1.Load(Lines1, Filename)
    else CodeEdit2.Load(Lines2, Filename);
end;

procedure TFTextDiff.Open(const Filename1, Filename2: string);
begin
  if OnlyDifferences then
    TBDiffsOnlyClick(Self);
  SetFilesCompared(false);
  LinkScroll(false);
  CodeEdit1.Load(Lines1, Filename1);
  CodeEdit2.Load(Lines2, Filename2);
end;

procedure TFTextDiff.HorzSplitClick(Sender: TObject);
begin
  TBView.ImageIndex:= 3 - TBView.ImageIndex;
  if TBView.ImageIndex = 2 then begin
    PLeft.Align:= alTop;
    PLeft.Height:= PMain.ClientHeight div 2 -1;
    Splitter.Align:= alTop;
    Splitter.Cursor:= crVSplit;
    TBCopyBlockLeft.Hint:= _('Copy block up');
    TBCopyBlockLeft.ImageIndex:= 10;
    TBCopyBlockRight.Hint:= _('Copy block down');
    TBCopyBlockRight.ImageIndex:= 11;
  end else begin
    PLeft.Align:= alLeft;
    PLeft.Width:= PMain.ClientWidth div 2 -1;
    Splitter.Align:= alLeft;
    Splitter.Left:= PPIScale(10);
    Splitter.Cursor:= crHSplit;
    TBCopyBlockLeft.Hint:= _(LNGCopyBlockLeft);
    TBCopyBlockLeft.ImageIndex:= 8;
    TBCopyBlockRight.Hint:= _(LNGCopyBlockRight);
    TBCopyBlockRight.ImageIndex:= 9;
  end;
  CodeEdit1.ShowFilename;
  GetCodeEdit.EnsureCursorPosVisible;
  SyncScroll(GetCodeEdit, sbVertical);
end;

procedure TFTextDiff.DoCompare;
var
  i: integer;
  HashArray1, HashArray2: TArrOfInteger;
  CodeEdit: TSynEditExDiff;
  Caret: TBufferCoord;
begin
  if (Lines1.Count = 0) or (Lines2.Count = 0) then exit;

  CodeEdit:= GetCodeEdit;
  Caret:= CodeEdit.CaretXY;

  if CodeEdit1.Modified or CodeEdit2.Modified then
    DoSaveBoth;

  CodeEdit1.Color:= DefaultClr;
  CodeEdit2.Color:= DefaultClr;

  //THIS PROCEDURE IS WHERE ALL THE HEAVY LIFTING (COMPARING) HAPPENS ...

  //Because it's SO MUCH EASIER AND FASTER comparing hashes (integers) than
  //comparing whole lines of text, we'll build an array of hashes for each line
  //in the source files. Each line is represented by a (virtually) unique
  //hash that is based on the contents of that line. Also, since the
  //likelihood of 2 different lines generating the same hash is so small,
  //we can safely ignore that possibility.

  try
    Screen.Cursor:= crHourGlass;
    SetLength(HashArray1, Lines1.Count);
    SetLength(HashArray2, Lines2.Count);
    for i:= 0 to Lines1.Count-1 do
      HashArray1[i]:= HashLine(Lines1[i], IgnoreCase, IgnoreBlanks);
    for i:= 0 to Lines2.Count-1 do
      HashArray2[i]:= HashLine(Lines2[i], IgnoreCase, IgnoreBlanks);
    try
      //CALCULATE THE DIFFS HERE ...
       if not Diff.Execute(HashArray1, HashArray2,
                           Lines1.Count, Lines2.Count) then exit;
      SetFilesCompared(true);
      DisplayDiffs;
    finally
    end;
    LinkScroll(true);
    setActiveControl(CodeEdit);
    CodeEdit.CaretXY:= Caret;
    SyncScroll(GetCodeEdit, sbVertical);
  finally
    Screen.Cursor:= crDefault;
    HashArray1:= nil;
    HashArray2:= nil;
  end;
end;

function Log10(int: integer): integer;
begin
  result:= 1;
  while int > 9 do begin inc(result); int:= int div 10; end;
end;

procedure TFTextDiff.DisplayDiffs;

  procedure AddAndFormat(CodeEdit: TSynEditExDiff; const Text: string;
                         Color: TColor; Num: longint);
  begin
    var i:= CodeEdit.Lines.Count;
    var LineObject:= TLineObj.Create;
    LineObject.Spezial:= Color <> DefaultClr;
    LineObject.BackClr:= Color;
    LineObject.Tag:= Num;
    CodeEdit.InsertItem(i, Text, LineObject);
  end;

begin
  // THIS IS WHERE THE TDIFF RESULT IS CONVERTED INTO COLOR HIGHLIGHTING ...
  CodeEdit1.Lines.BeginUpdate;
  CodeEdit2.Lines.BeginUpdate;
  try
    CodeEdit1.LinesClearAll;
    CodeEdit2.LinesClearAll;
    CodeEdit1.OnSpecialLineColors:= CodeEdit1.SynEditorSpecialLineColors;
    CodeEdit2.OnSpecialLineColors:= CodeEdit2.SynEditorSpecialLineColors;

    CodeEdit1.OnGutterGetText:= CodeEdit1.GutterTextEvent;
    CodeEdit2.OnGutterGetText:= CodeEdit2.GutterTextEvent;

    for var i:= 0 to Diff.Count-1 do
      with Diff.Compares[i] do
        if Kind = ckAdd then begin
           AddAndFormat(CodeEdit1, '', addClr, 0);
           AddAndFormat(CodeEdit2, lines2[oldindex2], addClr, oldindex2+1);
        end else if Kind = ckDelete then begin
           AddAndFormat(CodeEdit1, lines1[oldindex1], delClr, oldindex1+1);
           AddAndFormat(CodeEdit2, '', delClr, 0);
        end else if Kind = ckModify then begin
           AddAndFormat(CodeEdit1, lines1[oldindex1], modClr, oldindex1+1);
           AddAndFormat(CodeEdit2, lines2[oldindex2], modClr, oldindex2+1);
        end else if not OnlyDifferences then begin
           AddAndFormat(CodeEdit1, lines1[oldindex1], defaultClr, oldindex1+1);
           AddAndFormat(CodeEdit2, lines2[oldindex2], defaultClr, oldindex2+1);
        end;
    CodeEdit1.SetModified(false);
    CodeEdit2.SetModified(false);
  finally
    CodeEdit1.Lines.EndUpdate;
    CodeEdit2.Lines.EndUpdate;
  end;
  with Diff.DiffStats do
  if adds + modifies + deletes = 0 then begin
    CodeEdit1.withColoredLines:= false;
    CodeEdit2.withColoredLines:= false;
    end
  else begin
    CodeEdit1.withColoredLines:= true;
    CodeEdit2.withColoredLines:= true;
  end;
  ShowDiffState;
end;

procedure TFTextDiff.ShowDiffState;
begin
  liAdded.Caption:= Format(' ' + _('%d lines added') + ' ', [Diff.DiffStats.adds]);
  liModified.Caption:= Format(' ' + _('%d lines modified') + ' ', [Diff.DiffStats.modifies]);;
  liDeleted.Caption:= Format(' ' + _('%d lines deleted') + ' ', [Diff.DiffStats.deletes]);
  with Diff.DiffStats do
    if adds + modifies + deletes = 0
      then liDifferences.Caption:= _('No differences.')
      else liDifferences.Caption:= '';
end;

//Syncronise scrolling of both CodeEdits (once files are compared)...
var IsSyncing: boolean;

procedure TFTextDiff.SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);
begin
  if IsSyncing or not (CodeEdit1.withColoredLines and CodeEdit2.withColoredLines)
    then exit;
  IsSyncing:= true; //stops recursion
  try
    if (Sender as  TSynEditExDiff) = CodeEdit1
      then CodeEdit2.TopLine:= CodeEdit1.TopLine
      else CodeEdit1.TopLine:= CodeEdit2.TopLine;
  finally
    IsSyncing:= false;
  end;
end;

procedure TFTextDiff.ShowFileNames;
begin
  CodeEdit1.ShowFilename;
  CodeEdit2.ShowFilename;
end;

procedure TFTextDiff.LinkScroll(IsLinked: boolean);
begin
  if IsLinked then begin
    CodeEdit1.OnScroll:= SyncScroll;
    CodeEdit2.OnScroll:= SyncScroll;
    SyncScroll(GetCodeEdit, sbVertical);
  end else begin
    CodeEdit1.OnScroll:= nil;
    CodeEdit2.OnScroll:= nil;
  end;
end;

//go to next color block (only enabled if files have been compared)
procedure TFTextDiff.NextClick(Sender: TObject);
begin
  //get next colored block ...
  with GetCodeEdit do begin
    if (lines.Count = 0) or not withColoredLines then exit;
    var i:= CaretY - 1;
    var clr:= GetLineObj(i).BackClr;
    repeat
      inc(i);
    until (i = Lines.Count) or (GetLineObj(i).BackClr <> clr);
    if (i = Lines.Count) then //do nothing here
    else if GetLineObj(i).BackClr = color then
    repeat
      inc(i);
    until (i = Lines.Count) or (GetLineObj(i).BackClr <> color);
    if (i = Lines.Count) then
    begin
      Windows.Beep(600, 100); //not found
      exit;
    end;
    CaretY:= i+1;
    //now make sure as much of the block as possible is visible ...
    clr:= GetLineObj(i).BackClr;
    repeat
      inc(i);
    until(i = Lines.Count) or (GetLineObj(i).BackClr <> clr);
    if i >= TopLine + LinesInWindow then TopLine:= CaretY;
    SyncScroll(GetCodeEdit, sbVertical);
  end;
end;

//go to previous color block (only enabled if files have been compared)
procedure TFTextDiff.PrevClick(Sender: TObject);
begin
  //get prev colored block ...
  with GetCodeEdit do begin
    if not withColoredLines then exit;
    var i:= CaretY-1;
    if i = Lines.count then begin Windows.Beep(600, 100); exit end;
    var clr:= GetLineObj(i).BackClr;
    repeat
      dec(i);
    until (i < 0) or (GetLineObj(i).BackClr <> clr);
    if i < 0 then begin Windows.Beep(600, 100); exit end;
    if GetLineObj(i).BackClr = Color then
    repeat
      dec(i);
    until (i < 0) or (GetLineObj(i).BackClr <> Color);
    if i < 0 then Windows.Beep(600, 100) else begin
      clr:= GetLineObj(i).BackClr;
      while (i > 0) and (GetLineObj(i-1).BackClr = clr) do dec(i);
      //'i' now at the beginning of the previous color block.
      CaretY:= i+1;
      SyncScroll(GetCodeEdit, sbVertical);
    end;
  end;
end;

procedure TFTextDiff.CopyBlockRightClick(Sender: TObject);
  var from, _to: integer;
      emptyClipboard: boolean;
      clr: TColor; LinObj: TLineObj;
begin
  if (myActiveControl <> CodeEdit1) or OnlyDifferences or not FilesCompared then exit;
  with CodeEdit1 do begin
    if lines.Count = 0 then exit;
    from:= CaretY - 1;
    clr:= GetLineObj(from).BackClr;
    if clr = color then exit; //we're not in a colored block !!!
    _to:= from;
    while (from > 0) and
      (GetLineObj(from-1).BackClr = clr) do dec(from);
    while (_to < Lines.Count-1) and
      (GetLineObj(_to+1).BackClr = clr) do inc(_to);
    //make sure color blocks still match up ...
    if (_to > CodeEdit2.Lines.Count -1) or
      (CodeEdit2.GetLineObj(from).BackClr <> clr) or
      (CodeEdit2.GetLineObj(_to).BackClr <> clr) then exit;
    emptyClipboard:= CopyToClipboardFT(from, _to);
    LinObj:= GetLineObj(from);
  end;
  CodeEdit2.PasteFromClipboardFT(emptyClipboard, from, _to, LinObj.Tag);
  SyncScroll(CodeEdit1, sbVertical);
  CodeEdit1.Enter(Self);
end;

procedure TFTextDiff.CopyBlockLeftClick(Sender: TObject);
  var from, _to: integer;
      emptyClipboard: boolean;
      clr: TColor; LinObj: TLineObj;
begin
  if (myActiveControl <> CodeEdit2) or OnlyDifferences or not FilesCompared then exit;
  with CodeEdit2 do begin
    if lines.Count = 0 then exit;
    from:= CaretY - 1;
    clr:= GetLineObj(from).BackClr;
    if clr = color then exit; //we're not in a colored block !!!
    _to:= from;
    while (from > 0) and
      (GetLineObj(from-1).BackClr = clr) do dec(from);
    while (_to < Lines.Count-1) and
      (GetLineObj(_to+1).BackClr = clr) do inc(_to);
    //make sure color blocks still match up ...
    if (_to > CodeEdit1.Lines.Count -1) or
      (CodeEdit1.GetLineObj(from).BackClr <> clr) or
      (CodeEdit1.GetLineObj(_to).BackClr <> clr) then exit;
    emptyClipboard:= CopyToClipboardFT(from, _to);
    LinObj:= GetLineObj(from);
  end;
  CodeEdit1.PasteFromClipboardFT(emptyClipboard, from, _to, LinObj.Tag);
  SyncScroll(CodeEdit2, sbVertical);
  CodeEdit2.Enter(Self);
end;

procedure TFTextDiff.MIUndoClick(Sender: TObject);
begin
  Undo;
end;

procedure TFTextDiff.MIRedoClick(Sender: TObject);
begin
  Redo;
end;

procedure TFTextDiff.MICutClick(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TFTextDiff.MICopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TFTextDiff.MIPasteClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TFTextDiff.MICloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFTextDiff.Undo;
begin
  GetCodeEdit.DoUndo;
end;

procedure TFTextDiff.Redo;
begin
  GetCodeEdit.DoRedo;
end;

procedure TFTextDiff.CutToClipboard;
begin
  GetCodeEdit.CutToClipBoard;
end;

procedure TFTextDiff.CopyToClipboard;
begin
  GetCodeEdit.CopyToClipBoard;
end;

procedure TFTextDiff.PasteFromClipboard;
begin
  with GetCodeEdit do begin
    PasteFromClipBoard;
    CreateObjects;
  end;
end;

function TFTextDiff.GetFont: TFont;
begin
  Result:= CodeEdit1.Font;
end;

procedure TFTextDiff.SetFont(aFont: TFont);
begin
  CodeEdit1.Font.Assign(aFont);
  CodeEdit2.Font.Assign(aFont);
  SetFontSize(0);
end;

procedure TFTextDiff.SetFontSize(Delta: integer);
begin
  var Size:= CodeEdit1.Font.Size + Delta;
  if Size < 6 then Size:= 6;
  CodeEdit1.Font.Size:= Size;
  CodeEdit2.Font.Size:= Size;
  CodeEdit1.Gutter.Font.Size:= Size;
  CodeEdit2.Gutter.Font.Size:= Size;
  CodeEdit1.Gutter.Width:= CodeEdit1.CharWidth*(Log10(Lines1.Count)+1);
  CodeEdit2.Gutter.Width:= CodeEdit2.CharWidth*(Log10(Lines2.Count)+1);
  ShowFileNames;
end;

procedure TFTextDiff.TBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
end;

procedure TFTextDiff.TBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
end;

procedure TFTextDiff.FormResize(Sender: TObject);
begin
  if TBView.Marked
    then PLeft.height:= PMain.ClientHeight div 2 - 1
    else PLeft.width := PMain.ClientWidth  div 2 - 1;
end;

procedure TFTextDiff.Search;
begin
  FJava.ShowSearchReplaceDialog(GetCodeEdit, false);
end;

procedure TFTextDiff.SearchAgain;
begin
  FJava.DoSearchReplaceText(GetCodeEdit, false);
end;

procedure TFTextDiff.Replace;
begin
  FJava.ShowSearchReplaceDialog(GetCodeEdit, true);
end;

procedure TFTextDiff.TBCompareClick(Sender: TObject);
begin
  if OnlyDifferences then TBDiffsOnlyClick(Self);
  DoCompare;
end;

procedure TFTextDiff.DoSaveFile(Nr: integer; WithBackup: boolean);
  var CodeEdit: TSynEditExDiff;
begin
  if Nr = 1
    then CodeEdit:= CodeEdit1
    else CodeEdit:= CodeEdit2;
  if (CodeEdit.Pathname = '') or OnlyDifferences then exit;
  CodeEdit.Save(WithBackup);
  if Nr = 1
    then Lines1.Assign(CodeEdit.Lines)
    else Lines2.Assign(CodeEdit.Lines);
  SetFilesCompared(false);
end;

procedure TFTextDiff.DoSaveBoth;
begin
  DoSaveFile(1, false);
  DoSaveFile(2, false);
end;

procedure TFTextDiff.TBSourcecodeClick(Sender: TObject);
  var CodeEdit: TSynEditExDiff;
      Caret: TBufferCoord;
begin
  if OnlyDifferences then TBDiffsOnlyClick(Self);
  CodeEdit:= GetCodeEdit;
  Caret:= CodeEdit.CaretXY;
  DoSaveFile(1, false);
  DoSaveFile(2, false);
  CodeEdit.CaretXY:= Caret;
  setActiveControl(CodeEdit);
  SyncScroll(GetCodeEdit, sbVertical);
end;

procedure TFTextDiff.SetFilesCompared(Value: boolean);
begin
  FilesCompared:= Value;
end;

procedure TFTextDiff.TBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFTextDiff.GotoLine(i: integer);
begin
  if i = 0 then exit;
  CodeEdit1.CaretX := 1;
  CodeEdit1.CaretY := i;
  CodeEdit1.Topline:= i;
  SyncScroll(CodeEdit1, sbVertical);
end;

function TFTextDiff.GetCodeEdit: TSynEditExDiff;
begin
  if myActiveControl = CodeEdit1
    then Result:= CodeEdit1
    else Result:= CodeEdit2;
end;

procedure TFTextDiff.SplitterMoved(Sender: TObject);
begin
  ShowFileNames;
end;                                         

procedure TFTextDiff.TBIgnoreCaseClick(Sender: TObject);
begin
  IgnoreCase:= not IgnoreCase;
  TBIgnoreCase.Down:= IgnoreCase;
  if FilesCompared then TBCompareClick(Self);
end;

procedure TFTextDiff.TBIgnoreBlanksClick(Sender: TObject);
begin
  IgnoreBlanks:= not IgnoreBlanks;
  TBIgnoreBlanks.Down:= IgnoreBlanks;
  if FilesCompared then TBCompareClick(Self);
end;

procedure TFTextDiff.TBParagraphClick(Sender: TObject);
begin
  var Options:= CodeEdit1.Options;
  if eoShowSpecialChars in Options
    then Exclude(Options, eoShowSpecialChars)
    else Include(Options, eoShowSpecialChars);
  CodeEdit1.Options:= Options;
  Options:= CodeEdit2.Options;
  if eoShowSpecialChars in Options
    then Exclude(Options, eoShowSpecialChars)
    else Include(Options, eoShowSpecialChars);
  CodeEdit2.Options:= Options;
  TBParagraph.Down:= (eoShowSpecialChars in Options);
end;

procedure TFTextDiff.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    addClr:= $97734F;
    modClr:= $16A231;
    delClr:= $621EA6;
    DefaultClr:= StyleServices.GetSystemColor(clWindow);
    TBTextDiff.Images:= vilTextDiffDark;
  end else begin
    addClr:= $F0CCA8;
    modClr:= $6FFB8A;
    delClr:= $BB77FF;
    DefaultClr:= clWindow;
    TBTextDiff.Images:= vilTextDiffLight;
  end;
  DisplayDiffs;
end;

procedure TFTextDiff.ChooseFiles(F1: TFEditForm);

  procedure InitDir;
  begin
    with FJava.ODOpen do begin
      InitialDir:= FConfiguration.Sourcepath;
      if not SysUtils.DirectoryExists(InitialDir) then
        InitialDir:= GetDocumentsPath;
    end;
  end;

begin
  with FJava.ODOpen do begin
    if assigned(F1) then begin
      InitialDir:= ExtractFilePath(F1.Pathname);
      if InitialDir + '\' = FConfiguration.TempDir then
        InitDir;
        Nr:= 2
    end else begin
      InitDir;
      Nr:= 1;
    end;
    Filename:= '';
    var TempFilter:= Filter;
    Filter:= 'Java (*.java)|*.java|HTML (*.html)|*.html;*.htm|Text (*.txt)|*.txt|' + _(LNGAll) + ' (*.*)|*.*';
    FilterIndex:= 1;
    Title:= _(LNGOpenFile);
    try
      if Execute then begin
        FConfiguration.Sourcepath:= ExtractFilePath(Filename);
        if Files.Count >= 2
          then Open(Files.Strings[0], Files.Strings[1])
          else Open(Filename);
      end;
    finally
      Filter:= TempFilter;
    end;
  end;
end;

procedure TFTextDiff.SynEditEnter(Sender: TObject);
begin
  Nr:= (Sender as TSynEditExDiff).Nr;
end;

procedure TFTextDiff.DPIChanged;
begin
  setFontsize(0);
end;

end.
