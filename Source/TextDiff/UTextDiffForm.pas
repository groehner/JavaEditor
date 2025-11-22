unit UTextDiffForm;

// -----------------------------------------------------------------------------
// Application:     TextDiff                                                   .
// Module:          Main                                                       .
// Version:         4.3                                                        .
// Date:            09.07.2022                                                 .
// Target:          Win32, Delphi 7                                            .
// Author:          Angus Johnson - angusj-AT-myrealbox-DOT-com                .
// Copyright:       © 2003-2004 Angus Johnson                                  .
// © 2006-2025 Gerhard Röhner                                  .
// -----------------------------------------------------------------------------

interface

uses
  Windows,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  Menus,
  ComCtrls,
  Classes,
  System.ImageList,
  Vcl.ImgList,
  Vcl.ToolWin,
  Vcl.BaseImageCollection,
  Vcl.VirtualImageList,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  SpTBXSkins,
  SynEditTextBuffer,
  UBaseForm,
  UEditorForm,
  UDiff,
  USynEditDiff;

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
    N1Sep: TSpTBXSeparatorItem;
    MICopy: TSpTBXItem;
    MICut: TSpTBXItem;
    MIRedo: TSpTBXItem;
    MIUndo: TSpTBXItem;
    N2Sep: TSpTBXSeparatorItem;
    MIOpenFile: TSpTBXItem;
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
    procedure MIOpenFileClick(Sender: TObject);
    procedure PopupEditorPopup(Sender: TObject);
  private
    FDiff: TDiff;
    FLines1, FLines2: TSynEditStringList;
    FAddClr, FDelClr, FModClr, FDefaultClr: TColor;
    FCodeEdit1: TSynEditDiff;
    FCodeEdit2: TSynEditDiff;
    FFilesCompared: Boolean;
    FOnlyDifferences: Boolean;
    FIgnoreBlanks: Boolean;
    FIgnoreCase: Boolean;
    FCodeEditNumber: Integer;
    procedure LinkScroll(IsLinked: Boolean);
    procedure DoCompare;
    procedure DoSaveFile(Num: Integer; WithBackup: Boolean);
    procedure DoLoadFile(const FileName: string; Num: Integer);
    procedure ChooseFiles(EditForm: TFEditForm);
    procedure DisplayDiffs;
    procedure SynEditEnter(Sender: TObject);
    procedure DoSaveBoth;
    procedure ShowFileNames;
    function GetCodeEdit: TSynEditDiff;
    procedure SetFilesCompared(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure New(EditForm1, EditForm2: TFEditForm);
    procedure Open(const FileName: string); overload;
    procedure Open(const Filename1, Filename2: string); overload;
    procedure Save(WithBackup: Boolean); override;
    procedure ShowDiffState;
    procedure SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure Search; override;
    procedure SearchAgain; override;
    procedure Replace; override;
    procedure Undo; override;
    procedure Redo; override;
    procedure SetFontSize(Delta: Integer); override;
    procedure SetFont(AFont: TFont); override;
    function GetFont: TFont; override;
    function GetFormType: string; override;
    procedure DPIChanged; override;
    procedure ChangeStyle; override;

    property FilesCompared: Boolean read FFilesCompared;
    property CodeEditNumber: Integer read FCodeEditNumber write FCodeEditNumber;
  end;

implementation

uses
  Winapi.Messages,
  SysUtils,
  Themes,
  Dialogs,
  SynEdit,
  JvGnugettext,
  SynEditScrollBars,
  SynEditTypes,
  UStringRessources,
  UJava,
  UConfiguration,
  UUtils,
  UHashUnit;

{$R *.DFM}

constructor TFTextDiff.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 4;
end;

procedure TFTextDiff.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  var
  State := FConfiguration.ReadStringU('TextDiff', 'State', '');
  FIgnoreCase := FConfiguration.ReadBoolU('TextDiff', 'FIgnoreCase', False);
  FIgnoreBlanks := FConfiguration.ReadBoolU('TextDiff', 'FIgnoreBlanks', False);
  TBIgnoreCase.Down := FIgnoreCase;
  TBIgnoreBlanks.Down := FIgnoreBlanks;
  SetState(State);

  // the Diff engine ...
  FDiff := TDiff.Create(Self);

  // FLines1 & FLines2 contain the unmodified files
  FLines1 := TSynEditStringList.Create(nil);
  FLines2 := TSynEditStringList.Create(nil);

  // edit windows where color highlighing of Diffs and changes are displayed ...
  FCodeEdit1 := TSynEditDiff.Create(Self);
  with FCodeEdit1 do
  begin
    Number := 1;
    Parent := PLeft;
    Align := alClient;
    PCaption := PCaptionLeft;
    PopupMenu := PopupEditor;
    OnEnter := SynEditEnter;
  end;

  FCodeEdit2 := TSynEditDiff.Create(Self);
  with FCodeEdit2 do
  begin
    Number := 2;
    Parent := PRight;
    Align := alClient;
    PCaption := PCaptionRight;
    PopupMenu := PopupEditor;
    OnEnter := SynEditEnter;
  end;
  FCodeEditNumber := 1;
  SetFont(FConfiguration.EditFont);
  FOnlyDifferences := False;
  OnClose := FormClose;
  OnMouseActivate := FormMouseActivate;
  TBCopyBlockLeft.Hint := _(LNGCopyBlockLeft);
  TBCopyBlockRight.Hint := _(LNGCopyBlockRight);
  FConfiguration.RemoveShortcutsFrom(PopupEditor);
  ChangeStyle;
end;

procedure TFTextDiff.New(EditForm1, EditForm2: TFEditForm);
begin
  Pathname := Caption;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  if Assigned(EditForm1) and Assigned(EditForm2) then
    Open(EditForm1.Pathname, EditForm2.Pathname)
  else
  begin
    if Assigned(EditForm1) then
      Open(EditForm1.Pathname);
    ChooseFiles(EditForm1);
  end;
  DoCompare;
end;

procedure TFTextDiff.liDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
  ItemInfo: TSpTBXMenuItemInfo; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPrePaint then
  begin
    PaintDefault := False;
    var
    AColor := clBtnFace;
    case TSpTBXLabelItem(Sender).Tag of
      1:
        AColor := FAddClr;
      2:
        AColor := FModClr;
      3:
        AColor := FDelClr;
    end;
    ACanvas.Brush.Color := AColor;
    ACanvas.Pen.Color := AColor;
    ACanvas.Rectangle(ARect);
  end;
end;

procedure TFTextDiff.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FJava.DeleteTabAndWindow(Number);
  FDiff.Free;
  FCodeEdit2.Free;
  FCodeEdit1.Free;
  FLines2.Free;
  FLines1.Free;
  Action := caFree;
  inherited;
end;

procedure TFTextDiff.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FConfiguration.WriteStringU('TextDiff', 'State', GetState);
  FConfiguration.WriteBoolU('TextDiff', 'FIgnoreCase', FIgnoreCase);
  FConfiguration.WriteBoolU('TextDiff', 'FIgnoreBlanks', FIgnoreBlanks);
  if FOnlyDifferences then
    TBDiffsOnlyClick(Self);
  DoSaveBoth;
  FJava.EditorAgeTimer.Enabled := FConfiguration.CheckAge;
  CanClose := True;
end;

procedure TFTextDiff.TBDiffsOnlyClick(Sender: TObject);
var
  CodeEdit: TSynEditDiff;
  Caret: TBufferCoord;
begin
  if FOnlyDifferences then
  begin
    FOnlyDifferences := False;
    FCodeEdit1.ReadOnly := False;
    FCodeEdit2.ReadOnly := False;
    FCodeEdit1.Gutter.AutoSize := True;
    FCodeEdit2.Gutter.AutoSize := True;
    TBCopyBlockLeft.Enabled := True;
    TBCopyBlockRight.Enabled := True;
    CodeEdit := GetCodeEdit;
    Caret := CodeEdit.CaretXY;
    if Caret.Line - 1 > 0 then
      Caret.Line := CodeEdit.GetLineObj(Caret.Line - 1).Tag;
    DisplayDiffs;
    CodeEdit.CaretXY := Caret;
    SyncScroll(GetCodeEdit, sbVertical);
  end
  else
  begin
    DoSaveBoth;
    if not FilesCompared then
      DoCompare;
    if FilesCompared then
    begin
      FOnlyDifferences := True;
      FCodeEdit1.Gutter.AutoSize := False;
      FCodeEdit2.Gutter.AutoSize := False;
      DisplayDiffs;
      FCodeEdit1.ReadOnly := True;
      FCodeEdit2.ReadOnly := True;
      TBCopyBlockLeft.Enabled := False;
      TBCopyBlockRight.Enabled := False;
    end;
  end;
end;

function TFTextDiff.GetFormType: string;
begin
  Result := '%D%';
end;

procedure TFTextDiff.Save(WithBackup: Boolean);
begin
  DoSaveFile(GetCodeEdit.Number, WithBackup);
end;

procedure TFTextDiff.Open(const FileName: string);
begin
  DoLoadFile(FileName, FCodeEditNumber);
end;

procedure TFTextDiff.DoLoadFile(const FileName: string; Num: Integer);
begin
  if FOnlyDifferences then
    TBDiffsOnlyClick(Self);
  SetFilesCompared(False);
  LinkScroll(False);
  if Num = 1 then begin
    FCodeEdit1.Load(FLines1, FileName);
    FCodeEdit1.Highlighter := FConfiguration.GetHighlighter(FileName);
  end
  else begin
    FCodeEdit2.Load(FLines2, FileName);
    FCodeEdit2.Highlighter := FConfiguration.GetHighlighter(FileName);
  end;
end;

procedure TFTextDiff.Open(const Filename1, Filename2: string);
begin
  if FOnlyDifferences then
    TBDiffsOnlyClick(Self);
  SetFilesCompared(False);
  LinkScroll(False);
  FCodeEdit1.Load(FLines1, Filename1);
  FCodeEdit2.Load(FLines2, Filename2);
  FCodeEdit1.Highlighter := FConfiguration.GetHighlighter(FileName1);
  FCodeEdit2.Highlighter := FConfiguration.GetHighlighter(FileName2);
end;

procedure TFTextDiff.HorzSplitClick(Sender: TObject);
begin
  TBView.ImageIndex := 3 - TBView.ImageIndex;
  if TBView.ImageIndex = 2 then
  begin
    PLeft.Align := alTop;
    PLeft.Height := PMain.ClientHeight div 2 - 1;
    Splitter.Align := alTop;
    Splitter.Cursor := crVSplit;
    TBCopyBlockLeft.Hint := _('Copy block up');
    TBCopyBlockLeft.ImageIndex := 10;
    TBCopyBlockRight.Hint := _('Copy block down');
    TBCopyBlockRight.ImageIndex := 11;
  end
  else
  begin
    PLeft.Align := alLeft;
    PLeft.Width := PMain.ClientWidth div 2 - 1;
    Splitter.Align := alLeft;
    Splitter.Left := PPIScale(10);
    Splitter.Cursor := crHSplit;
    TBCopyBlockLeft.Hint := _(LNGCopyBlockLeft);
    TBCopyBlockLeft.ImageIndex := 8;
    TBCopyBlockRight.Hint := _(LNGCopyBlockRight);
    TBCopyBlockRight.ImageIndex := 9;
  end;
  FCodeEdit1.ShowFilename;
  GetCodeEdit.EnsureCursorPosVisible;
  SyncScroll(GetCodeEdit, sbVertical);
end;

procedure TFTextDiff.DoCompare;
var
  HashArray1, HashArray2: TArrOfInteger;
  CodeEdit: TSynEditDiff;
  Caret: TBufferCoord;
begin
  if (FLines1.Count = 0) or (FLines2.Count = 0) then
    Exit;

  CodeEdit := GetCodeEdit;
  Caret := CodeEdit.CaretXY;

  if FCodeEdit1.Modified or FCodeEdit2.Modified then
    DoSaveBoth;

  FCodeEdit1.Color := FDefaultClr;
  FCodeEdit2.Color := FDefaultClr;

  // THIS PROCEDURE IS WHERE ALL THE HEAVY LIFTING (COMPARING) HAPPENS ...

  // Because it's SO MUCH EASIER AND FASTER comparing hashes (integers) than
  // comparing whole lines of text, we'll build an array of hashes for each line
  // in the source files. Each line is represented by a (virtually) unique
  // hash that is based on the contents of that line. Also, since the
  // likelihood of 2 different lines generating the same hash is so small,
  // we can safely ignore that possibility.

  try
    Screen.Cursor := crHourGlass;
    SetLength(HashArray1, FLines1.Count);
    SetLength(HashArray2, FLines2.Count);
    for var I := 0 to FLines1.Count - 1 do
      HashArray1[I] := HashLine(FLines1[I], FIgnoreCase, FIgnoreBlanks);
    for var I := 0 to FLines2.Count - 1 do
      HashArray2[I] := HashLine(FLines2[I], FIgnoreCase, FIgnoreBlanks);
    // CALCULATE THE DiffS HERE ...
    if not FDiff.Execute(HashArray1, HashArray2, FLines1.Count, FLines2.Count)
    then
      Exit;
    FFilesCompared := True;
    DisplayDiffs;
    LinkScroll(True);
    CodeEdit.CaretXY := Caret;
    SyncScroll(GetCodeEdit, sbVertical);
  finally
    Screen.Cursor := crDefault;
    HashArray1 := nil;
    HashArray2 := nil;
  end;
end;

procedure TFTextDiff.DisplayDiffs;

  procedure AddAndFormat(CodeEdit: TSynEditDiff; const Text: string;
    Color: TColor; Num: LongInt);
  begin
    var
    Count := CodeEdit.Lines.Count;
    var
    LineObject := TLineObj.Create;
    LineObject.Spezial := Color <> FDefaultClr;
    LineObject.BackClr := Color;
    LineObject.Tag := Num;
    CodeEdit.InsertItem(Count, Text, LineObject);
  end;

begin
  // THIS IS WHERE THE TDiff RESULT IS CONVERTED INTO COLOR HIGHLIGHTING ...
  FCodeEdit1.Lines.BeginUpdate;
  FCodeEdit2.Lines.BeginUpdate;
  try
    FCodeEdit1.LinesClearAll;
    FCodeEdit2.LinesClearAll;
    FCodeEdit1.OnSpecialLineColors := FCodeEdit1.SynEditorSpecialLineColors;
    FCodeEdit2.OnSpecialLineColors := FCodeEdit2.SynEditorSpecialLineColors;

    FCodeEdit1.OnGutterGetText := FCodeEdit1.GutterTextEvent;
    FCodeEdit2.OnGutterGetText := FCodeEdit2.GutterTextEvent;

    for var I := 0 to FDiff.Count - 1 do
      with FDiff[I] do
        if Kind = ckAdd then
        begin
          AddAndFormat(FCodeEdit1, '', FAddClr, 0);
          AddAndFormat(FCodeEdit2, FLines2[OldIndex2], FAddClr, OldIndex2 + 1);
        end
        else if Kind = ckDelete then
        begin
          AddAndFormat(FCodeEdit1, FLines1[OldIndex1], FDelClr, OldIndex1 + 1);
          AddAndFormat(FCodeEdit2, '', FDelClr, 0);
        end
        else if Kind = ckModify then
        begin
          AddAndFormat(FCodeEdit1, FLines1[OldIndex1], FModClr, OldIndex1 + 1);
          AddAndFormat(FCodeEdit2, FLines2[OldIndex2], FModClr, OldIndex2 + 1);
        end
        else if not FOnlyDifferences then
        begin
          AddAndFormat(FCodeEdit1, FLines1[OldIndex1], FDefaultClr,
            OldIndex1 + 1);
          AddAndFormat(FCodeEdit2, FLines2[OldIndex2], FDefaultClr,
            OldIndex2 + 1);
        end;
    FCodeEdit1.SetModified(False);
    FCodeEdit2.SetModified(False);
  finally
    FCodeEdit1.Lines.EndUpdate;
    FCodeEdit2.Lines.EndUpdate;
  end;
  with FDiff.DiffStats do
    if adds + modifies + deletes = 0 then
    begin
      FCodeEdit1.WithColoredLines := False;
      FCodeEdit2.WithColoredLines := False;
    end
    else
    begin
      FCodeEdit1.WithColoredLines := True;
      FCodeEdit2.WithColoredLines := True;
    end;
  FCodeEdit1.ClearUndo;
  FCodeEdit2.ClearUndo;
  FCodeEdit1.Invalidate;
  FCodeEdit2.Invalidate;
  ShowDiffState;
end;

procedure TFTextDiff.ShowDiffState;
begin
  liAdded.Caption := Format(' ' + _('%d lines added') + ' ',
    [FDiff.DiffStats.adds]);
  liModified.Caption := Format(' ' + _('%d lines modified') + ' ',
    [FDiff.DiffStats.modifies]);
  liDeleted.Caption := Format(' ' + _('%d lines deleted') + ' ',
    [FDiff.DiffStats.deletes]);
  with FDiff.DiffStats do
    if adds + modifies + deletes = 0 then
      liDifferences.Caption := _('No differences.')
    else
      liDifferences.Caption := '';
end;

// Syncronise scrolling of both CodeEdits (once files are compared)...
var
  IsSyncing: Boolean;

procedure TFTextDiff.SyncScroll(Sender: TObject; ScrollBar: TScrollBarKind);
begin
  if IsSyncing or not(FCodeEdit1.WithColoredLines and
    FCodeEdit2.WithColoredLines) then
    Exit;
  IsSyncing := True; // stops recursion
  try
    if Sender = FCodeEdit1 then
      FCodeEdit2.TopLine := FCodeEdit1.TopLine
    else
      FCodeEdit1.TopLine := FCodeEdit2.TopLine;
    FCodeEdit1.UpdateScrollBars;
    FCodeEdit2.UpdateScrollBars;
  finally
    IsSyncing := False;
  end;
end;

procedure TFTextDiff.ShowFileNames;
begin
  FCodeEdit1.ShowFilename;
  FCodeEdit2.ShowFilename;
end;

procedure TFTextDiff.LinkScroll(IsLinked: Boolean);
begin
  if IsLinked then
  begin
    FCodeEdit1.OnScroll := FCodeEdit1.SyncScroll;
    FCodeEdit2.OnScroll := FCodeEdit2.SyncScroll;
    SyncScroll(GetCodeEdit, sbVertical);
  end
  else
  begin
    FCodeEdit1.OnScroll := nil;
    FCodeEdit2.OnScroll := nil;
  end;
end;

// go to next color block (only enabled if files have been compared)
procedure TFTextDiff.NextClick(Sender: TObject);
begin
  // get next colored block ...
  with GetCodeEdit do
  begin
    if (Lines.Count = 0) or not WithColoredLines then
      Exit;
    var
    Int := CaretY - 1;
    var
    Clr := GetLineObj(Int).BackClr;
    repeat
      Inc(Int);
    until (Int = Lines.Count) or (GetLineObj(Int).BackClr <> Clr);

    if (Int <> Lines.Count) and (GetLineObj(Int).BackClr = Color) then
      repeat
        Inc(Int);
      until (Int = Lines.Count) or (GetLineObj(Int).BackClr <> Color);

    if (Int = Lines.Count) then
    begin
      Windows.Beep(600, 100); // not found
      Exit;
    end;
    CaretY := Int + 1;
    // now make sure as much of the block as possible is visible ...
    Clr := GetLineObj(Int).BackClr;
    repeat
      Inc(Int);
    until (Int = Lines.Count) or (GetLineObj(Int).BackClr <> Clr);
    if Int >= TopLine + LinesInWindow then
      TopLine := CaretY;
    SyncScroll(GetCodeEdit, sbVertical);
  end;
end;

// go to previous color block (only enabled if files have been compared)
procedure TFTextDiff.PrevClick(Sender: TObject);
begin
  // get prev colored block ...
  with GetCodeEdit do
  begin
    if not WithColoredLines then
      Exit;
    var
    Int := CaretY - 1;
    if Int = Lines.Count then
    begin
      Windows.Beep(600, 100);
      Exit;
    end;
    var
    Clr := GetLineObj(Int).BackClr;
    repeat
      Dec(Int);
    until (Int < 0) or (GetLineObj(Int).BackClr <> Clr);
    if Int < 0 then
    begin
      Windows.Beep(600, 100);
      Exit;
    end;
    if GetLineObj(Int).BackClr = Color then
      repeat
        Dec(Int);
      until (Int < 0) or (GetLineObj(Int).BackClr <> Color);
    if Int < 0 then
      Windows.Beep(600, 100)
    else
    begin
      Clr := GetLineObj(Int).BackClr;
      while (Int > 0) and (GetLineObj(Int - 1).BackClr = Clr) do
        Dec(Int);
      // 'i' now at the beginning of the previous color block.
      CaretY := Int + 1;
      SyncScroll(GetCodeEdit, sbVertical);
    end;
  end;
end;

procedure TFTextDiff.CopyBlockRightClick(Sender: TObject);
var
  From, Till: Integer;
  EmptyClipboard: Boolean;
  Clr: TColor;
  LinObj: TLineObj;
begin
  if (MyActiveControl <> FCodeEdit1) or FOnlyDifferences or not FilesCompared
  then
    Exit;
  with FCodeEdit1 do
  begin
    if Lines.Count = 0 then
      Exit;
    From := CaretY - 1;
    Clr := GetLineObj(From).BackClr;
    if Clr = Color then
      Exit; // we're not in a colored block !!!
    Till := From;
    while (From > 0) and (GetLineObj(From - 1).BackClr = Clr) do
      Dec(From);
    while (Till < Lines.Count - 1) and (GetLineObj(Till + 1).BackClr = Clr) do
      Inc(Till);
    // make sure color blocks still match up ...
    if (Till > FCodeEdit2.Lines.Count - 1) or
      (FCodeEdit2.GetLineObj(From).BackClr <> Clr) or
      (FCodeEdit2.GetLineObj(Till).BackClr <> Clr) then
      Exit;
    EmptyClipboard := CopyToClipboardFT(From, Till);
    LinObj := GetLineObj(From);
  end;
  FCodeEdit2.PasteFromClipboardFT(EmptyClipboard, From, Till, LinObj.Tag);
  SyncScroll(FCodeEdit1, sbVertical);
  FCodeEdit1.Enter(Self);
end;

procedure TFTextDiff.CopyBlockLeftClick(Sender: TObject);
var
  From, Till: Integer;
  EmptyClipboard: Boolean;
  Clr: TColor;
  LinObj: TLineObj;
begin
  if (MyActiveControl <> FCodeEdit2) or FOnlyDifferences or not FilesCompared
  then
    Exit;
  with FCodeEdit2 do
  begin
    if Lines.Count = 0 then
      Exit;
    From := CaretY - 1;
    Clr := GetLineObj(From).BackClr;
    if Clr = Color then
      Exit; // we're not in a colored block !!!
    Till := From;
    while (From > 0) and (GetLineObj(From - 1).BackClr = Clr) do
      Dec(From);
    while (Till < Lines.Count - 1) and (GetLineObj(Till + 1).BackClr = Clr) do
      Inc(Till);
    // make sure color blocks still match up ...
    if (Till > FCodeEdit1.Lines.Count - 1) or
      (FCodeEdit1.GetLineObj(From).BackClr <> Clr) or
      (FCodeEdit1.GetLineObj(Till).BackClr <> Clr) then
      Exit;
    EmptyClipboard := CopyToClipboardFT(From, Till);
    LinObj := GetLineObj(From);
  end;
  FCodeEdit1.PasteFromClipboardFT(EmptyClipboard, From, Till, LinObj.Tag);
  SyncScroll(FCodeEdit2, sbVertical);
  FCodeEdit2.Enter(Self);
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

procedure TFTextDiff.MIOpenFileClick(Sender: TObject);
begin
  FJava.MIOpenClick(Self);
  DoCompare;
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
  GetCodeEdit.CutToClipboard;
end;

procedure TFTextDiff.CopyToClipboard;
begin
  GetCodeEdit.CopyToClipboard;
end;

procedure TFTextDiff.PasteFromClipboard;
begin
  with GetCodeEdit do
  begin
    PasteFromClipboard;
    CreateObjects;
  end;
end;

function TFTextDiff.GetFont: TFont;
begin
  Result := FCodeEdit1.Font;
end;

procedure TFTextDiff.SetFont(AFont: TFont);
begin
  FCodeEdit1.Font.Assign(AFont);
  FCodeEdit2.Font.Assign(AFont);
  SetFontSize(0);
end;

procedure TFTextDiff.SetFontSize(Delta: Integer);

  function Log10(Inte: Integer): Integer;
  begin
    Result := 1;
    while Inte > 9 do
    begin
      Inc(Result);
      Inte := Inte div 10;
    end;
  end;

begin
  var
  Size := FCodeEdit1.Font.Size + Delta;
  if Size < 6 then
    Size := 6;
  FCodeEdit1.Font.Size := Size;
  FCodeEdit2.Font.Size := Size;
  FCodeEdit1.Gutter.Font.Size := Size;
  FCodeEdit2.Gutter.Font.Size := Size;
  //FCodeEdit1.Gutter.Width := FCodeEdit1.CharWidth * (Log10(FLines1.Count) + 1);
  //FCodeEdit2.Gutter.Width := FCodeEdit2.CharWidth * (Log10(FLines2.Count) + 1);
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
  if TBView.Marked then
    PLeft.Height := PMain.ClientHeight div 2 - 1
  else
    PLeft.Width := PMain.ClientWidth div 2 - 1;
end;

procedure TFTextDiff.Search;
begin
  FJava.ShowSearchReplaceDialog(GetCodeEdit, False);
end;

procedure TFTextDiff.SearchAgain;
begin
  FJava.DoSearchReplaceText(GetCodeEdit, False);
end;

procedure TFTextDiff.Replace;
begin
  FJava.ShowSearchReplaceDialog(GetCodeEdit, True);
end;

procedure TFTextDiff.TBCompareClick(Sender: TObject);
begin
  if FOnlyDifferences then
    TBDiffsOnlyClick(Self);
  DoCompare;
end;

procedure TFTextDiff.DoSaveFile(Num: Integer; WithBackup: Boolean);
var
  CodeEdit: TSynEditDiff;
begin
  if Num = 1 then
    CodeEdit := FCodeEdit1
  else
    CodeEdit := FCodeEdit2;
  if (CodeEdit.Pathname = '') or FOnlyDifferences then
    Exit;
  CodeEdit.Save(WithBackup);
  if Num = 1 then
    FLines1.Assign(CodeEdit.Lines)
  else
    FLines2.Assign(CodeEdit.Lines);
  SetFilesCompared(False);
end;

procedure TFTextDiff.DoSaveBoth;
begin
  DoSaveFile(1, False);
  DoSaveFile(2, False);
end;

procedure TFTextDiff.TBSourcecodeClick(Sender: TObject);
var
  CodeEdit: TSynEditDiff;
  Caret: TBufferCoord;
begin
  if FOnlyDifferences then
    TBDiffsOnlyClick(Self);
  CodeEdit := GetCodeEdit;
  Caret := CodeEdit.CaretXY;
  DoSaveFile(1, False);
  DoSaveFile(2, False);
  CodeEdit.CaretXY := Caret;
  SyncScroll(GetCodeEdit, sbVertical);
end;

procedure TFTextDiff.SetFilesCompared(Value: Boolean);
begin
  FFilesCompared := Value;
end;

procedure TFTextDiff.TBCloseClick(Sender: TObject);
begin
  Close;
end;

function TFTextDiff.GetCodeEdit: TSynEditDiff;
begin
  if MyActiveControl = FCodeEdit1 then
    Result := FCodeEdit1
  else
    Result := FCodeEdit2;
end;

procedure TFTextDiff.SplitterMoved(Sender: TObject);
begin
  ShowFileNames;
end;

procedure TFTextDiff.TBIgnoreCaseClick(Sender: TObject);
begin
  FIgnoreCase := not FIgnoreCase;
  TBIgnoreCase.Down := FIgnoreCase;
  if FilesCompared then
    TBCompareClick(Self);
end;

procedure TFTextDiff.TBIgnoreBlanksClick(Sender: TObject);
begin
  FIgnoreBlanks := not FIgnoreBlanks;
  TBIgnoreBlanks.Down := FIgnoreBlanks;
  if FilesCompared then
    TBCompareClick(Self);
end;

procedure TFTextDiff.TBParagraphClick(Sender: TObject);
begin
  if FCodeEdit1.VisibleSpecialChars = [scWhitespace, scControlChars, scEOL] then
    FCodeEdit1.VisibleSpecialChars := []
  else
    FCodeEdit1.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL];

  if FCodeEdit2.VisibleSpecialChars = [scWhitespace, scControlChars, scEOL] then
    FCodeEdit2.VisibleSpecialChars := []
  else
    FCodeEdit2.VisibleSpecialChars := [scWhitespace, scControlChars, scEOL];
  TBParagraph.Down := (FCodeEdit1.VisibleSpecialChars <> []);
end;

procedure TFTextDiff.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    FAddClr := $97734F;
    FModClr := $16A231;
    FDelClr := $621EA6;
    FDefaultClr := StyleServices.GetSystemColor(clWindow);
    TBTextDiff.Images := vilTextDiffDark;
  end
  else
  begin
    FAddClr := $F0CCA8;
    FModClr := $6FFB8A;
    FDelClr := $BB77FF;
    FDefaultClr := clWindow;
    TBTextDiff.Images := vilTextDiffLight;
  end;
  FJava.ThemeEditorGutter(FCodeEdit1.Gutter);
  FCodeEdit1.InvalidateGutter;
  FCodeEdit1.CodeFolding.FolderBarLinesColor := FCodeEdit1.Gutter.Font.Color;
  FJava.ThemeEditorGutter(FCodeEdit2.Gutter);
  FCodeEdit2.CodeFolding.FolderBarLinesColor := FCodeEdit2.Gutter.Font.Color;
  FCodeEdit2.InvalidateGutter;
  FCodeEdit1.ChangeStyle;
  FCodeEdit2.ChangeStyle;
  Invalidate;
end;

procedure TFTextDiff.ChooseFiles(EditForm: TFEditForm);

  procedure InitDir;
  begin
    with FJava.ODOpen do
    begin
      InitialDir := FConfiguration.Sourcepath;
      if not SysUtils.DirectoryExists(InitialDir) then
        InitialDir := GetDocumentsPath;
    end;
  end;

begin
  with FJava.ODOpen do
  begin
    if Assigned(EditForm) then
    begin
      InitialDir := ExtractFilePath(EditForm.Pathname);
      if InitialDir + '\' = FConfiguration.TempDir then
        InitDir;
      FCodeEditNumber := 2;
    end
    else
    begin
      InitDir;
      FCodeEditNumber := 1;
    end;
    FileName := '';
    var
    TempFilter := Filter;
    Filter := 'Java (*.java)|*.java|HTML (*.html)|*.html;*.htm|Text (*.txt)|*.txt|'
      + _(LNGAll) + ' (*.*)|*.*';
    FilterIndex := 1;
    Title := _(LNGOpenFile);
    try
      if Execute then
      begin
        FConfiguration.Sourcepath := ExtractFilePath(FileName);
        if Files.Count >= 2 then
          Open(Files[0], Files[1])
        else
          Open(FileName);
      end;
    finally
      Filter := TempFilter;
    end;
  end;
end;

procedure TFTextDiff.SynEditEnter(Sender: TObject);
begin
  FCodeEditNumber := TSynEditDiff(Sender).Number;
end;

procedure TFTextDiff.DPIChanged;
begin
  SetFontSize(0);
end;

procedure TFTextDiff.PopupEditorPopup(Sender: TObject);
begin
  if PopupEditor.PopupComponent = FCodeEdit1 then
    FCodeEditNumber := 1;
  if PopupEditor.PopupComponent = FCodeEdit2 then
    FCodeEditNumber := 2;
end;

end.
