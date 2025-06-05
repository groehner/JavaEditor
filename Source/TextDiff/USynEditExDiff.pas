unit USynEditExDiff;

interface

uses
  Classes,
  Graphics,
  ExtCtrls,
  SynEdit,
  SynEditTextBuffer,
  USynEditEx;

type

  TLineObj = class
  private
    FBackClr: TColor;
    FSpezial: Boolean;
    FTag: LongInt;
  public
    property BackClr: TColor read FBackClr write FBackClr;
    property Spezial: Boolean read FSpezial write FSpezial;
    property Tag: LongInt read FTag write FTag;
  end;

  TSynEditExDiff = class(TSynEditEx)
  private
    FMyOwner: TComponent;
    FCurrLongestLineLen: Integer; // needed for horizontal scrollbar
    FEditFormNr: Integer;
    FEncoding: string;
    FLineModClr: TColor;
    FYellowGray: TColor;
    FSilveryGray: TColor;
    FModifiedStrs: array [Boolean] of string;
    FInsertModeStrs: array [Boolean] of string;
    FLineBreak: string;
    FNumber: Integer;
    FPathname: string;
    FPCaption: TPanel;
    FWithColoredLines: Boolean;
    procedure DeleteObjects(From, Till: Integer);
    procedure DoExit1(Sender: TObject);
    procedure SynEditorReplaceText(Sender: TObject;
      const ASearch, AReplace: string; Line, Column: Integer;
      var AAction: TSynReplaceAction);
    procedure CodeEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetHighlighter;
    function EncodingAsString(const AEncoding: string): string;
    function LinebreakAsString: string;
    procedure UpdateState;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoUndo;
    procedure DoRedo;
    procedure Enter(Sender: TObject);
    function GetLineObj(Index: Integer): TLineObj;
    procedure InsertItem(Index: Integer; const Str: string;
      LineObject: TLineObj);
    procedure SynEditorSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var Foreground, Background: TColor);
    procedure GutterTextEvent(Sender: TObject; ALine: Integer;
      var AText: string);
    procedure Load(Lines12: TSynEditStringList; const APathname: string);
    procedure Save(WithBackup: Boolean);
    procedure LinesClearAll;
    procedure CreateObjects; overload;
    procedure CreateObjects(From, Till, ATag: Integer); overload;
    procedure ShowFilename;
    procedure SetModified(Value: Boolean);
    function CopyToClipboardFT(From, Till: Integer): Boolean;
    procedure PasteFromClipboardFT(EmptyClipboard: Boolean;
      From, Till, ANr: Integer);
    procedure ChangeStyle; override;

    property Number: Integer read FNumber write FNumber;
    property Pathname: string read FPathname;
    property PCaption: TPanel read FPCaption write FPCaption;
    property WithColoredLines: Boolean read FWithColoredLines write FWithColoredLines;
  end;

implementation

uses
  SysUtils,
  Windows,
  Controls,
  Forms,
  Clipbrd,
  FileCtrl,
  SynEditTypes,
  JvGnugettext,
  UStringRessources,
  UTextDiffForm,
  UDlgConfirmReplace,
  UUtils,
  UConfiguration,
  UJava;

constructor TSynEditExDiff.Create(AOwner: TComponent);
begin
  inherited;
  FMyOwner := AOwner;
  FCurrLongestLineLen := 60;
  FWithColoredLines := False;
  FNumber := 0;
  FPathname := '';
  FEditFormNr := -1;
  FModifiedStrs[False] := '';
  FModifiedStrs[True] := _(LNGModified);
  FInsertModeStrs[False] := _(LNGModusOverwrite);
  FInsertModeStrs[True] := _(LNGModusInsert);
  SearchEngine := FJava.SynEditSearch;
  Gutter.ShowLineNumbers := True;
  Gutter.DigitCount := 0;
  Gutter.LeftOffset := 4;
  Gutter.AutoSize := True;
  MaxUndo := 300;
  TabWidth := 2;
  WantTabs := True;
  Options := [eoAutoIndent, eoDragDropEditing, eoScrollPastEol,
    eoShowScrollHint, eoSmartTabs, eoTabIndent, eoTabsToSpaces,
    eoTrimTrailingSpaces, eoSmartTabDelete, eoGroupUndo, eoKeepCaretX,
    eoEnhanceHomeKey];
  Font.Assign(FConfiguration.EditFont);
  Gutter.Font.Assign(FConfiguration.Font);
  Gutter.Font.Height := Font.Height + 2;
  OnKeyUp := CodeEditKeyUp;
  OnStatusChange := EditorStatusChange;
  OnReplaceText := SynEditorReplaceText;
  OnEnter := Enter;
  OnExit := DoExit1;
  ChangeStyle;
end;

destructor TSynEditExDiff.Destroy;
begin
  OnStatusChange := nil;
  OnEnter := nil;
  OnExit := nil;
  LinesClearAll;
  inherited;
end;

procedure TSynEditExDiff.DoUndo;
begin
  Undo;
  CreateObjects;
end;

procedure TSynEditExDiff.DoRedo;
begin
  Redo;
  CreateObjects;
end;

procedure TSynEditExDiff.Enter(Sender: TObject);
begin
  Gutter.Color := FYellowGray;
  EditorStatusChange(Sender, [scAll]);
  if FWithColoredLines then
    (FMyOwner as TFTextDiff).ShowDiffState;
end;

procedure TSynEditExDiff.DoExit1(Sender: TObject);
begin
  Gutter.Color := FSilveryGray;
end;

procedure TSynEditExDiff.CodeEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  with FMyOwner as TFTextDiff do
    if FilesCompared and (Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT]) then
      SyncScroll(Self, sbVertical)
    else if (Key = VK_RETURN) or ([ssCtrl] = Shift) and (Key = Ord('V')) then
      CreateObjects;
end;

function TSynEditExDiff.GetLineObj(Index: Integer): TLineObj;
begin
  if Lines.Count = 0 then
    Result := nil
  else
  begin
    if (Index < 0) or (Index >= Lines.Count) then
      raise Exception.Create('TLines.GetLineObj() - Index out of bounds.');
    Result := TLineObj(Lines.Objects[Index]);
  end;
end;

procedure TSynEditExDiff.InsertItem(Index: Integer; const Str: string;
  LineObject: TLineObj);
begin
  inherited Lines.InsertObject(Index, Str, LineObject);
end;

procedure TSynEditExDiff.SynEditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var Foreground, Background: TColor);
begin
  if (Line > Lines.Count) or not FWithColoredLines then
    Exit;
  var
  Lo1 := GetLineObj(Line - 1);
  if not Assigned(Lo1) then
  begin
    // Background:= clRed;      // good for debugging
    Background := FLineModClr;
    Special := True;
  end
  else
    with Lo1 do
      if Spezial then
      begin
        Background := BackClr;
        Special := True;
      end;
end;

procedure TSynEditExDiff.UpdateState;
begin
  with FJava do
  begin
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledTB(TBUndo, MIUndo.Enabled);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledTB(TBRedo, MIRedo.Enabled);

    var
    Selection := SelAvail;
    SetEnabledMI(MICut, Selection);
    SetEnabledMI(MICopy, Selection);
    SetEnabledMI(MICopyRTF, Selection);
    SetEnabledMI(MICopyHTML, Selection);
    SetEnabledMI(MICopyHTMLAsText, Selection);
    SetEnabledMI(MICopyNumbered, Selection);
    SetEnabledMI(MICopyRtfNumbered, Selection);
    SetEnabledMI(MIPaste, CanPaste);
  end;
  with FMyOwner as TFTextDiff do
  begin
    SetEnabledMI(MIUndo, CanUndo);
    SetEnabledMI(MIRedo, CanRedo);
    SetEnabledMI(MICut, SelAvail);
    SetEnabledMI(MICopy, SelAvail);
    SetEnabledMI(MIPaste, CanPaste);
  end;
end;

procedure TSynEditExDiff.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if not Assigned(FMyOwner) then
    Exit;

  (FMyOwner as TFTextDiff).Number := FNumber;
  (FMyOwner as TFTextDiff).liLineColumn.Caption :=
    Format(' %4d : %3d ', [CaretY, CaretX]);
  if Changes * [scModified] <> [] then
  begin
    (FMyOwner as TFTextDiff).liModified.Caption := FModifiedStrs[Modified];
    ShowFilename;
    if FEditFormNr = -1 then
    begin
      var
      AForm := FJava.GetTDIWindowType(FPathname, '%E%');
      if Assigned(AForm) then
        FEditFormNr := AForm.Number;
    end;
    if FEditFormNr > -1 then
      FJava.TabModified(FEditFormNr, Modified);
  end;
  (FMyOwner as TFTextDiff).liInsOvr.Caption := FInsertModeStrs[InsertMode];
  (FMyOwner as TFTextDiff).liEncoding.Caption := ' ' + FEncoding + '/' +
    LinebreakAsString + ' ';
  UpdateState;
end;

procedure TSynEditExDiff.GutterTextEvent(Sender: TObject; ALine: Integer;
  var AText: string);
begin
  if FWithColoredLines then
  begin
    var
    LineObject := GetLineObj(ALine - 1);
    if not Assigned(LineObject) then
      Exit;
    with LineObject do
      if Tag = 0 then
        AText := ''
      else
        AText := IntToStr(Tag);
  end
  else
    AText := IntToStr(ALine);
end;

procedure TSynEditExDiff.Load(Lines12: TSynEditStringList;
  const APathname: string);
begin
  try
    Lines12.LoadFromFile(APathname);
  except
    on e: Exception do
      FConfiguration.Log('TSynEditExDiff.Load: ' + APathname, e);
  end;
  Lines.BeginUpdate;
  LinesClearAll;
  Lines.Assign(Lines12);
  Lines.EndUpdate;
  Self.FPathname := APathname;
  FEncoding := EncodingAsString(Lines12.Encoding.EncodingName);
  FLineBreak := Lines12.LineBreak;
  FWithColoredLines := False;
  SetHighlighter;
  SetModified(False);
  (FMyOwner as TFTextDiff).SetActiveControl(Self);
end;

procedure TSynEditExDiff.Save(WithBackup: Boolean);
var
  BackupName, Ext: string;
  ALine: TLineObj;
begin
  if FWithColoredLines then
  begin
    BeginUpdate;
    for var I := Lines.Count - 1 downto 0 do
    begin
      ALine := GetLineObj(I);
      if Assigned(ALine) then
        if ALine.Tag = 0 then
        begin
          FreeAndNil(ALine);
          Lines.Delete(I);
        end
        else
          ALine.Spezial := False;
    end;
    FWithColoredLines := False;
    EndUpdate;
    Invalidate;
  end;

  if Modified then
    try
      if WithBackup then
      begin
        BackupName := FPathname;
        Ext := ExtractFileExt(FPathname);
        if Length(Ext) > 1 then
          Ext[2] := '~';
        BackupName := ChangeFileExt(BackupName, Ext);
        if FileExists(BackupName) then
          SysUtils.DeleteFile(BackupName);
        if FileExists(FPathname) then
          RenameFile(FPathname, BackupName);
      end;
      Lines.SaveToFile(FPathname);
      SetModified(False);
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
end;

procedure TSynEditExDiff.SetHighlighter;
begin
  var
  Str := LowerCase(ExtractFileExt(FPathname));
  Highlighter := FConfiguration.GetHighlighter(Str);
  if Highlighter = nil then
    OnPaintTransient := nil;
end;

function TSynEditExDiff.EncodingAsString(const AEncoding: string): string;
begin
  Result := AEncoding;
  if Pos('ANSI', Result) > 0 then
    Result := 'ANSI'
  else if Pos('ASCII', Result) > 0 then
    Result := 'ASCII'
  else if Pos('UTF-8', Result) > 0 then
    Result := 'UTF-8'
  else if Pos('UTF-16', Result) > 0 then
    Result := 'UTF-16'
  else if Pos('Unicode', Result) > 0 then
    Result := 'UTF-16';
end;

function TSynEditExDiff.LinebreakAsString: string;
begin
  if FLineBreak = #13#10 then
    Result := 'Windows'
  else if FLineBreak = #10 then
    Result := 'Unix'
  else
    Result := 'Mac';
end;

procedure TSynEditExDiff.LinesClearAll;
begin
  DeleteObjects(0, Lines.Count - 1);
  Lines.Clear;
  OnSpecialLineColors := nil;
  OnGutterGetText := nil;
end;

procedure TSynEditExDiff.DeleteObjects(From, Till: Integer);
begin
  for var I := From to Till do
  begin
    var
    LineObject := GetLineObj(I);
    if Assigned(LineObject) then
    begin
      LineObject.Free;
      Lines.Objects[I] := nil;
    end;
  end;
end;

procedure TSynEditExDiff.CreateObjects;
var
  LineObject: TLineObj;
begin
  if not FWithColoredLines then
    Exit;
  Lines.BeginUpdate;
  var
  Int := 0;
  while (Int < Lines.Count) and Assigned(GetLineObj(Int)) do
    Inc(Int);
  while (Int < Lines.Count) and (GetLineObj(Int) = nil) do
  begin
    LineObject := TLineObj.Create;
    LineObject.Spezial := True;
    LineObject.BackClr := FLineModClr;
    LineObject.Tag := Int + 1;
    Lines.Objects[Int] := LineObject;
    Inc(Int);
  end;
  Lines.EndUpdate;
  Invalidate;
end;

procedure TSynEditExDiff.CreateObjects(From, Till, ATag: Integer);
var
  LineObject: TLineObj;
begin
  for var I := From to Till do
    if GetLineObj(I) = nil then
    begin
      LineObject := TLineObj.Create;
      LineObject.Spezial := True;
      LineObject.BackClr := FLineModClr;
      LineObject.Tag := ATag;
      if ATag > 0 then
        Inc(ATag);
      Lines.Objects[I] := LineObject;
    end;
end;

function PointToDisplay(Posi: TPoint): TDisplayCoord;
begin
  Result.Column := Posi.X;
  Result.Row := Posi.Y;
end;

procedure TSynEditExDiff.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: string; Line, Column: Integer;
  var AAction: TSynReplaceAction);
begin
  if ASearch = AReplace then
    AAction := raSkip
  else
  begin
    var
    APos := DisplayCoord(Column, Line);
    APos := PointToDisplay(ClientToScreen(RowColumnToPixels(APos)));
    var
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    with TFConfirmReplace.Create(Application) do
    begin
      PrepareShow(EditRect, APos.Column, APos.Row,
        APos.Row + LineHeight, ASearch);
      case ShowModal of
        mrYes:
          AAction := raReplace;
        mrYesToAll:
          AAction := raReplaceAll;
        mrNo:
          AAction := raSkip;
      else
        AAction := raCancel;
      end;
      Free;
    end;
  end;
end;

procedure TSynEditExDiff.ShowFilename;
var
  Str: string;
  ACanvas: TCanvas;
begin
  ACanvas := (FMyOwner as TFTextDiff).Canvas;
  ACanvas.Font.Assign(FPCaption.Font);
  Str := ' ' + FPathname;
  if Modified then
    Str := Str + '*';
  FPCaption.Caption := MinimizeName(Str, ACanvas, FPCaption.Width);
end;

procedure TSynEditExDiff.SetModified(Value: Boolean);
begin
  Modified := Value;
  ShowFilename;
end;

function TSynEditExDiff.CopyToClipboardFT(From, Till: Integer): Boolean;
begin
  SelStart := RowColToCharIndex(BufferCoord(1, From + 1));
  if Till + 2 > Lines.Count then
    SelEnd := RowColToCharIndex(BufferCoord(Length(Lines[Lines.Count - 1]) +
      1, Till + 1))
  else
    SelEnd := RowColToCharIndex(BufferCoord(1, Till + 2)) - 2;
  Result := (SelLength = 0);
  if Result then
    Clipboard.Clear
  else
    CopyToClipboard;
  SelLength := 0;
end;

procedure TSynEditExDiff.PasteFromClipboardFT(EmptyClipboard: Boolean;
  From, Till, ANr: Integer);
begin
  DeleteObjects(From, Till);
  SelStart := RowColToCharIndex(BufferCoord(1, From + 1));
  if Till + 2 > Lines.Count then
    SelEnd := RowColToCharIndex(BufferCoord(Length(Lines[Lines.Count - 1]) +
      1, Till + 1))
  else
    SelEnd := RowColToCharIndex(BufferCoord(1, Till + 2)) - 2;
  try
    if EmptyClipboard then
    begin
      CutToClipboard;
      Clipboard.Clear;
    end
    else
      PasteFromClipboard;
  except on e: Exception do
    ErrorMsg(e.Message);
  end;
  CreateObjects(From, Till, ANr);
  SetModified(True);
  Invalidate;
end;

procedure TSynEditExDiff.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    FLineModClr := $878787;
    FYellowGray := $519595;
    FSilveryGray := $777777;
  end
  else
  begin
    FLineModClr := $E0E0E0;
    FYellowGray := $97734F;
    FSilveryGray := $16A231;
  end;
  inherited;
  EditorStatusChange(Self, []);
end;

end.
