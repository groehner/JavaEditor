{ -------------------------------------------------------------------------------
  Unit:     UStructogramform
  Author:   Gerhard Röhner
  Based on: NSD-Editor by Marcel Kalt
  Date:     August 2013
  Purpose:  structogram editor
  ------------------------------------------------------------------------------- }

unit UStructogramform;

interface

uses
  Windows,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  Graphics,
  ComCtrls,
  Menus,
  StdCtrls,
  System.ImageList,
  Vcl.ImgList,
  Vcl.ToolWin,
  Vcl.BaseImageCollection,
  Vcl.VirtualImageList,
  Vcl.VirtualImage,
  SVGIconImageCollection,
  TB2Item,
  SpTBXItem,
  UTypes,
  UBaseForm;

type

  TFStructogram = class(TFForm)
    ScrollBox: TScrollBox;
    PanelLeft: TPanel;
    ToolBarStructogram: TToolBar;
    TBClose: TToolButton;
    TBStatement: TToolButton;
    TBIf: TToolButton;
    TBSwitch: TToolButton;
    TBwhile: TToolButton;
    TBDoWhile: TToolButton;
    TBFor: TToolButton;
    TBSubProgram: TToolButton;
    TBBreak: TToolButton;
    TBAlgorithm: TToolButton;
    TBGenerateJava: TToolButton;
    TBZoomOut: TToolButton;
    TBZoomIn: TToolButton;
    TBPuzzleMode: TToolButton;
    icStructogram: TSVGIconImageCollection;
    vilToolbarLight: TVirtualImageList;
    vilToolbarDark: TVirtualImageList;
    TrashImage: TVirtualImage;
    vilPopupMenuLight: TVirtualImageList;
    vilPopupMenuDark: TVirtualImageList;
    StructoPopupMenu: TSpTBXPopupMenu;
    MIAddCase: TSpTBXItem;
    MISwitchWithCaseLine: TSpTBXItem;
    MICopy: TSpTBXItem;
    MIDelete: TSpTBXItem;
    MIDeleteCase: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    MIGenerateMethod: TSpTBXItem;
    MIGenerateProgram: TSpTBXItem;
    MIDataType: TSpTBXSubmenuItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    MICopyAsPicture: TSpTBXItem;
    MIPuzzleMode: TSpTBXItem;
    MIFont: TSpTBXItem;
    MIConfiguration: TSpTBXItem;
    MIBoolean: TSpTBXItem;
    MIString: TSpTBXItem;
    MIInt: TSpTBXItem;
    MIFloat: TSpTBXItem;
    MIDouble: TSpTBXItem;
    MIChar: TSpTBXItem;

    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    // used by buttons
    procedure StrElementMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageDblClick(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState;
      XPos, YPos: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; XPos, YPos: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; XPos, YPos: Integer);
    procedure ScrollBoxClick(Sender: TObject);
    procedure ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure MICopyAsPictureClick(Sender: TObject);
    procedure MIDatatypeClick(Sender: TObject);
    procedure StructoPopupMenuPopup(Sender: TObject);
    procedure EditMemoChange(Sender: TObject);

    procedure MIGenerateMethodClick(Sender: TObject);
    procedure MIGenerateProgramClick(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MISwitchWithCaseLineClick(Sender: TObject);
    procedure MIAddCaseClick(Sender: TObject);
    procedure MIDeleteCaseClick(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure MIPuzzleClick(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure BBGenerateJavaClick(Sender: TObject);
    procedure BBCloseClick(Sender: TObject);
    procedure BBZoomOutClick(Sender: TObject);
    procedure BBZoomInClick(Sender: TObject);
    procedure BBPuzzleClick(Sender: TObject);
    procedure MIConfigurationClick(Sender: TObject);
  private
    FEditMemo: TMemo;
    FOldShape: TRect;
    FOldCanvas: TCanvas;
    FCurElement: TStrElement;
    FCurList: TStrList;
    FEditMemoElement: TStrElement;
    FEditMemoBeginText: string;
    FCurInsert: Integer;
    FIsMoving: Boolean;
    FReadOnly: Boolean;
    FMousePos: TPoint;
    FScreenMousePos: TPoint;
    FDataType: string;
    FVariables: string;
    FIgnoreNextMouseDown: Boolean;
    FIgnoreNextMouseUp: Boolean;
    FSeparating: Integer;
    FPuzzleMode: Integer;
    FSolution: string;
    FVersion: Byte;
    procedure ShowShape;
    procedure HideShape;
    procedure AddParameter(List: TStrList; ParamList: TStringList);
    procedure AddVariable(AText: string; List: TStringList);
    procedure StrElementToJava(Element: TStrElement;
      PList, VariablesList: TStringList; const Indent: string);
    function FindElement(Current: TStrElement; X, Y: Integer): TStrElement;
    function FindVisibleElement(Current: TStrElement; X, Y: Integer)
      : TStrElement;
    function BeforeOrAfter(Current: TStrElement): Boolean;
    function GetParentElement(Element: TStrElement): TStrElement;
    function GetToken(var Str: string; var Token: string): Integer;
    procedure CalculateInsertionShape(DestList, InsertList: TStrList;
      XPos, YPos: Integer);
    procedure InsertElement(DestList, InsertList: TStrList;
      ACurElement: TStrElement);
    procedure SetEvents(Image: TListImage);
    procedure DoEdit(StrElement: TStrElement; Str: string);
    procedure CloseEdit;
    procedure SetLeftBorderForEditMemo;
    function GetAlgorithmParameter(ParamList: TStringList): string;
    procedure JavaProgram(List: TStrList; ProgList: TStringList;
      const AName: string);
    function GetAlgorithm: TStrAlgorithm;
    function GetList: TStrList;
    function GetListAtScreenPos(Point: TPoint): TStrList;
    function GetCurList: Boolean;
    function GetCurListAndCurElement: Boolean;
    function GetStructogramAsBitmap: TBitmap;
    function GetName(StrList: TStrList): string;
    procedure PaintAll;
    procedure Debug(const Str: string);
    function FitsIn(ACurList: TStrList; ACurElement: TStrElement): Boolean;
    procedure SetPuzzleMode(Mode: Integer);
    procedure MakeVeryHard;
    function StringFromStream(Stream: TStream): string;
    function IntegerFromStream(Stream: TStream): Integer;
  protected
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromFileOld(FileName: string): Boolean;
    procedure SaveToFile(const FileName: string);
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure New;
    function Open(const Pathname: string): Boolean;
    procedure Save(WithBackup: Boolean); override;
    procedure Print; override;
    procedure GenerateFromText(const Text: string);
    procedure RenewFromText(const Text: string);
    function GetFormType: string; override;
    function GetSaveAsName: string; override;
    procedure SetOptions; override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure SetFont(AFont: TFont); override;
    procedure Enter(Sender: TObject); override;
    procedure DoExport; override;
    procedure ChangeStyle; override;
    procedure DPIChanged; override;
  end;

implementation

{$R *.DFM}

uses
  Messages,
  Buttons,
  SysUtils,
  Math,
  Clipbrd,
  Dialogs,
  Themes,
  Types,
  UITypes,
  IOUtils,
  StrUtils,
  JvGnugettext,
  UStringRessources,
  UJava,
  UConfiguration,
  UUtils,
  UGenerateStructogram,
  UMessages,
  UTemplates;

constructor TFStructogram.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 11;
end;

procedure TFStructogram.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  FOldShape := Rect(-1, -1, -1, -1);
  FEditMemo := TMemo.Create(Self);
  FEditMemo.Parent := Self;
  FEditMemo.SetBounds(136, 156, 99, 37);
  FEditMemo.Color := StyleServices.GetSystemColor(clSkyBlue);
  FEditMemo.OnChange := EditMemoChange;
  FEditMemo.Visible := False;
  FEditMemo.WordWrap := False;
  FEditMemo.BevelInner := bvNone;

  OnMouseActivate := FormMouseActivate;
  Font.Assign(FConfiguration.StructogramFont);
  ScrollBox.DoubleBuffered := True;
  FSeparating := 0;
  FPuzzleMode := 0;
  TBPuzzleMode.Visible := False;
  ToolBarStructogram.Height := 308 - 22;
  FVersion := $0E;
  FCurList := nil;
  FCurElement := nil;
  UpdateState;
  SetOptions;
end;

procedure TFStructogram.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified and not AlreadySavedAs then
  begin
    FJava.DoSave(Self, True);
    AlreadySavedAs := True;
  end;
  CanClose := True;
end;

procedure TFStructogram.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  for var I := ScrollBox.ControlCount - 1 downto 0 do
    TListImage(ScrollBox.Controls[I]).StrList.Destroy;
  Action := caFree;
end;

procedure TFStructogram.New;
var
  Elem: TStrElement;
  StrList: TStrAlgorithm;
begin
  if Pathname = '' then
    Pathname := FJava.GetFilename('.jsg');
  StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font);
  StrList.Text := FConfiguration.Algorithm + ' ' +
    ChangeFileExt(ExtractFileName(Pathname), '');
  Elem := TStrStatement.Create(StrList);
  StrList.Insert(StrList, Elem);
  SetEvents(StrList.ListImage);
  StrList.ResizeAll;
  StrList.Paint;
  Modified := False;
  Enter(Self); // must stay!
  if CanFocus then
    SetFocus;
  Caption := Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFStructogram.GenerateFromText(const Text: string);
var
  Generator: TGenerateStructogram;
  StrList: TStrAlgorithm;
begin
  if Pathname = '' then
    Pathname := FJava.GetFilename('.jsg');
  StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font);
  StrList.Text := FConfiguration.Algorithm + ' ';
  Generator := TGenerateStructogram.Create(True);
  Generator.GenerateStructogram(Text, StrList);
  FreeAndNil(Generator);
  SetEvents(StrList.ListImage);
  StrList.ResizeAll;
  StrList.Paint;
  Save(False);
  Enter(Self); // must stay!
  if CanFocus then
    SetFocus;
  Caption := Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
end;

procedure TFStructogram.RenewFromText(const Text: string);
var
  Generator: TGenerateStructogram;
  StrList: TStrList;
  Algorithm: string;
begin
  for var I := ScrollBox.ControlCount - 1 downto 0 do
  begin
    StrList := TListImage(ScrollBox.Controls[I]).StrList;
    if StrList is TStrAlgorithm then
      Algorithm := StrList.Text;
    FreeAndNil(StrList);
  end;
  StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font);
  StrList.Text := Algorithm;
  SetEvents(StrList.ListImage);
  StrList.Text := FConfiguration.Algorithm + ' ';
  Generator := TGenerateStructogram.Create(True);
  Generator.GenerateStructogram(Text, StrList);
  FreeAndNil(Generator);
  StrList.ResizeAll;
  StrList.Paint;
  Save(False);
  Enter(Self); // must stay!
  if CanFocus then
    SetFocus;
end;

function TFStructogram.Open(const Pathname: string): Boolean;
begin
  CloseEdit;
  Result := LoadFromFile(Pathname);
  if Result then
  begin
    Self.Pathname := Pathname;
    Caption := Pathname;
    FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
    Modified := False;
    Enter(Self); // must stay!
    if CanFocus then
      SetFocus;
    FReadOnly := IsWriteProtected(Pathname);
    UpdateState;
  end;
end;

procedure TFStructogram.Enter(Sender: TObject);
begin
  inherited;
  if ToolBarStructogram.Visible and ToolBarStructogram.CanFocus then
    ToolBarStructogram.SetFocus;
end;

function TFStructogram.LoadFromFile(const FileName: string): Boolean;
var
  StrList: TStrList;
  SwitchWithCaseLine: Boolean;
  Reader: TStringListReader;

  procedure Init(List: TStrList);
  begin
    List.SwitchWithCaseLine := SwitchWithCaseLine;
    List.SetPuzzleMode(FPuzzleMode);
    List.SetFont(Font);
    List.SetLineHeight;
    SetEvents(List.ListImage);
    List.ResizeAll;
    List.Paint;
  end;

begin
  Result := True;
  try
    Reader := TStringListReader.Create(FileName);
    try
      if Reader.hasOldFormat then
        LoadFromFileOld(FileName)
      else
      begin
        repeat
          Reader.ReadLine;
          if Reader.Key = 'FontName' then
            Font.Name := Reader.Val
          else if Reader.Key = 'FontSize' then
            Font.Size := PPIScale(StrToInt(Reader.Val))
          else if Reader.Key = 'FontColor' then
            Font.Color := StrToInt(Reader.Val)
          else if Reader.Key = 'FontBold' then
            if Reader.Val = 'true' then
              Font.Style := Font.Style + [fsBold]
            else
              Font.Style := Font.Style - [fsBold]
          else if Reader.Key = 'FontItalic' then
            if Reader.Val = 'true' then
              Font.Style := Font.Style + [fsItalic]
            else
              Font.Style := Font.Style - [fsItalic]
          else if Reader.Key = 'PuzzleMode' then
            FPuzzleMode := StrToInt(Reader.Val)
          else if Reader.Key = 'Solution' then
            FSolution := UnHideCrLf(Reader.Val);
        until (Reader.Key = '- Kind') or (Reader.Key = '');

        while (Reader.Key = '- Kind') and Result do
        begin
          if Reader.Val = 'Algorithm' then
            StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font)
          else
            StrList := TStrList.Create(ScrollBox, FPuzzleMode, Font);
          Reader.ReadLine;
          if Reader.Key = 'SwitchWithCaseLine' then
            SwitchWithCaseLine := (Reader.Val = 'true')
          else
            Reader.LineBack;
          StrList.LoadFromReader(Reader);
          Init(StrList);
          Result := Result and not StrList.LoadError;
          Reader.ReadLine;
        end;
      end;
    finally
      FreeAndNil(Reader);
    end;
  except
    on e: Exception do
    begin
      ErrorMsg(e.Message);
      Result := False;
    end;
  end;
  FVersion := $0F;
end;

function TFStructogram.LoadFromFileOld(FileName: string): Boolean;
var
  SigName: ShortString;
  FontName: ShortString;
  Style: Byte;
  Stream: TFileStream;
  LeftMargin, TopMargin, Zoomfactor: SmallInt;
  Kind: Byte;
  StrList: TStrList;
  StructogramAligned: Boolean;
  SwitchWithCaseLine: Boolean;

  procedure Init(List: TStrList);
  begin
    List.SwitchWithCaseLine := SwitchWithCaseLine;
    List.ListImage.Left := LeftMargin;
    List.ListImage.Top := TopMargin;
    List.SetPuzzleMode(FPuzzleMode);
    List.SetFont(Font);
    List.SetLineHeight;
    SetEvents(List.ListImage);
    List.ResizeAll;
    List.Paint;
  end;

begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Stream.Read(SigName, 4); { read signature }
      if not((SigName = 'JSG') or (SigName = 'NSD')) then
        ErrorMsg(_('Invalid file format!'))
      else
      begin
        Stream.Read(FVersion, 1); { read FVersion }

        if $0E <= FVersion then
        begin
          Stream.Read(FPuzzleMode, 1);
          SetPuzzleMode(FPuzzleMode);
          FSolution := StringFromStream(Stream);
          Font.Name := StringFromStream(Stream);
          if Font.Name = 'Arial' then
            Font.Name := 'Segoe UI';
        end;

        if $0C = FVersion then
        begin
          Stream.Read(StructogramAligned, 1);
          Stream.Read(SwitchWithCaseLine, 1);
        end;

        if ($0B <= FVersion) and (FVersion <= $0C) then
        begin
          Stream.Read(LeftMargin, 2); { read left margin }
          Stream.Read(TopMargin, 2); { read top margin }
          Stream.Read(Zoomfactor, 2); { read zoom factor }
        end;

        if ($0A <= FVersion) and (FVersion < $0E) then
        begin
          Stream.Read(FontName[0], 1);
          Stream.Read(FontName[1], Ord(FontName[0])); { read font name }
          Font.Name := string(FontName);
          if Font.Name = 'Arial' then
            Font.Name := 'Segoe UI';
        end;

        if ($0A <= FVersion) then
        begin
          Font.Size := IntegerFromStream(Stream);
          Stream.Read(Style, 1);
          Font.Style := [];
          if (Style and $01) > 0 then
            Font.Style := Font.Style + [fsBold];
          if (Style and $02) > 0 then
            Font.Style := Font.Style + [fsItalic];
          Font.Color := IntegerFromStream(Stream);

          if FVersion < $0D then
          begin
            StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font);
            StrList.LoadFromStream(Stream, FVersion);
            Init(StrList);
            Result := Result and not StrList.LoadError;
          end
          else
            while (Stream.Read(Kind, 1) = 1) and Result do
            begin
              if Kind = Byte(nsAlgorithm) then
                StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font)
              else
                StrList := TStrList.Create(ScrollBox, FPuzzleMode, Font);
              Stream.Read(SwitchWithCaseLine, 1);
              Stream.Read(LeftMargin, 2);
              Stream.Read(TopMargin, 2);
              StrList.LoadFromStream(Stream, FVersion);
              Init(StrList);
              Result := Result and not StrList.LoadError;
            end;
        end;
      end;
    finally
      FreeAndNil(Stream);
    end;
  except
    on e: Exception do
    begin
      ErrorMsg(e.Message);
      Result := False;
    end;
  end;
  FVersion := $0E;
end;

procedure TFStructogram.SaveToFile(const FileName: string);
var
  JSGFile: TextFile;
  AList: TStrList;
begin
  try
    try
      AssignFile(JSGFile, FileName);
      Rewrite(JSGFile);
      Writeln(JSGFile, 'JSG: true');
      if Font.Name = 'Arial' then
        Font.Name := 'Segoe UI';
      Writeln(JSGFile, 'FontName: ' + Font.Name);
      Writeln(JSGFile, 'FontSize: ' + IntToStr(PPIUnScale(Font.Size)));
      if fsBold in Font.Style then
        Writeln(JSGFile, 'FontBold: true');
      if fsItalic in Font.Style then
        Writeln(JSGFile, 'FontItalic: true');
      Writeln(JSGFile, 'FontColor: ' + IntToStr(Font.Color));
      if FPuzzleMode > 0 then
      begin
        Writeln(JSGFile, 'PuzzleMode: ' + IntToStr(FPuzzleMode));
        Writeln(JSGFile, 'Solution: ' + HideCrLf(FSolution));
      end;

      for var I := 0 to ScrollBox.ControlCount - 1 do
      begin
        AList := (ScrollBox.Controls[I] as TListImage).StrList;
        Writeln(JSGFile, '- Kind: ' + AList.GetKind);
        Writeln(JSGFile, '  SwitchWithCaseLine: ' +
          BoolToStr(AList.SwitchWithCaseLine, True));
        Writeln(JSGFile, '  RectPos' + AList.GetRectPos('  '));
        Write(JSGFile, AList.GetText('  '));
      end;
    finally
      CloseFile(JSGFile);
    end;
  except
    on e: Exception do
      ErrorMsg(e.Message);
  end;
end;

function TFStructogram.StringFromStream(Stream: TStream): string;
var
  Size: LongInt;
begin
  Stream.Read(Size, SizeOf(Size));
  SetLength(Result, Size div SizeOf(Char));
  Stream.Read(Pointer(Result)^, Size);
end;

function TFStructogram.IntegerFromStream(Stream: TStream): Integer;
begin
  Stream.Read(Result, SizeOf(Integer));
end;

procedure TFStructogram.ShowShape;
begin
  with FOldCanvas, FOldShape do
  begin
    Pen.Color := clRed;
    if Top = Bottom then
    begin
      MoveTo(Left + 1, Top - 2);
      LineTo(Right, Top - 2);
      MoveTo(Left + 1, Top - 1);
      LineTo(Right, Top - 1);
      MoveTo(Left + 1, Top + 1);
      LineTo(Right, Top + 1);
      MoveTo(Left + 1, Top + 2);
      LineTo(Right, Top + 2);
    end
    else
    begin
      MoveTo(Left, Top);
      LineTo(Right, Top);
      LineTo(Right, Bottom);
      LineTo(Left, Bottom);
      LineTo(Left, Top);
      MoveTo(Left + 1, Top + 1);
      LineTo(Right - 1, Top + 1);
      LineTo(Right - 1, Bottom - 1);
      LineTo(Left + 1, Bottom - 1);
      LineTo(Left + 1, Top + 1);
    end;
  end;
end;

procedure TFStructogram.HideShape;
begin
  if FOldShape.Top > -1 then
  begin
    with FOldCanvas, FOldShape do
    begin
      Pen.Color := StyleServices.GetStyleColor(scPanel);
      if Top = Bottom then
      begin
        MoveTo(Left + 1, Top - 2);
        LineTo(Right, Top - 2);
        MoveTo(Left + 1, Top - 1);
        LineTo(Right, Top - 1);
        MoveTo(Left + 1, Top + 1);
        LineTo(Right, Top + 1);
        MoveTo(Left + 1, Top + 2);
        LineTo(Right, Top + 2);
      end
      else
      begin
        MoveTo(Left, Top);
        LineTo(Right, Top);
        LineTo(Right, Bottom);
        LineTo(Left, Bottom);
        LineTo(Left, Top);
        MoveTo(Left + 1, Top + 1);
        LineTo(Right - 1, Top + 1);
        LineTo(Right - 1, Bottom - 1);
        LineTo(Left + 1, Bottom - 1);
        LineTo(Left + 1, Top + 1);
      end;
    end;
    FOldShape := Rect(-1, -1, -1, -1);
    PaintAll;
  end;
end;

// used by buttons
procedure TFStructogram.StrElementMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  StrList: TStrList;
  Element: TStrElement;
  PtScreen, PtClient: TPoint;
begin
  if Button = mbLeft then
  begin
    CloseEdit;
    if TSpeedButton(Sender).Tag = 0 then
      StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font)
    else
      StrList := TStrList.Create(ScrollBox, FPuzzleMode, Font);
    Element := nil;
    case TSpeedButton(Sender).Tag of
      Ord(nsAlgorithm):
        begin
          Element := TStrStatement.Create(StrList);
          StrList.Text := FConfiguration.Algorithm + ' ';
        end;
      Ord(nsStatement):
        Element := TStrStatement.Create(StrList);
      Ord(nsIf):
        Element := TStrIf.Create(StrList);
      Ord(nsWhile):
        begin
          Element := TStrWhile.Create(StrList);
          Element.Text := FConfiguration._While + ' ';
        end;
      Ord(nsDoWhile):
        begin
          Element := TStrDoWhile.Create(StrList);
          Element.Text := FConfiguration.DoWhile + ' ';
        end;
      Ord(nsFor):
        begin
          Element := TStrFor.Create(StrList);
          Element.Text := ReplaceStr(ReplaceStr(FConfiguration._For, '[', ''), ']',
            '') + ' ';
        end;
      Ord(nsSwitch):
        Element := TStrSwitch.Create(StrList);
      Ord(nsSubProgram):
        Element := TStrSubprogram.Create(StrList);
      Ord(nsBreak):
        Element := TStrBreak.Create(StrList);
    end;
    StrList.Insert(StrList, Element);
    StrList.SetFont(Font);
    StrList.ResizeAll;
    PtScreen := (Sender as TToolButton).ClientToScreen(Point(X, Y));
    PtClient := ToolBarStructogram.ScreenToClient(PtScreen);
    StrList.ListImage.SetBounds(PtClient.X - StrList.RctList.Width div 2,
      PtClient.Y - StrList.LineHeight div 2, StrList.RctList.Width,
      StrList.RctList.Height);
    SetEvents(StrList.ListImage);
    StrList.Paint;
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    if TSpeedButton(Sender).Tag = Ord(nsDoWhile) then
      SetCursorPos(Mouse.CursorPos.X + 30, Mouse.CursorPos.Y +
        StrList.LineHeight)
    else
      SetCursorPos(Mouse.CursorPos.X + 30, Mouse.CursorPos.Y);
    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  end;
end;

procedure TFStructogram.ImageDblClick(Sender: TObject);
begin
  FCurList := (Sender as TListImage).StrList;
  FCurElement := FindVisibleElement(FCurList, FMousePos.X, FMousePos.Y);
  DoEdit(FCurElement, '');
  FIgnoreNextMouseDown := True;
end;

procedure TFStructogram.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; XPos, YPos: Integer);
begin
  if FIgnoreNextMouseDown then
  begin
    FIgnoreNextMouseDown := False;
    Exit;
  end;
  if FIsMoving then
    Exit;
  var
  Image := (Sender as TListImage);
  Image.BringToFront;
  FCurList := Image.StrList;
  FMousePos := Point(XPos, YPos);
  FScreenMousePos := Image.ClientToScreen(Point(XPos, YPos));
  if FEditMemo.Visible then
    CloseEdit;
  FCurElement := FindVisibleElement(FCurList, FMousePos.X, FMousePos.Y);
  if (FSeparating = 0) and Assigned(FCurElement) and Assigned(FCurElement.Prev)
  then
    if (FCurElement.Prev is TStrAlgorithm) or
      not((FCurElement.Prev is TStrList) or (FCurElement is TStrCase)) then
      FSeparating := 1;
  UpdateState;
end;

// mouse move over a structogram
procedure TFStructogram.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  XPos, YPos: Integer);
var
  ScrollBoxPt, Image2Pt, ScreenPt: TPoint;
  DeltaX, DeltaY, DeltaX1, DeltaY1: Integer;
  Image, Image2: TListImage;
  StrList: TStrList;
  StrStatement: TStrStatement;
  ARect: TRect;
begin
  HideShape;
  if FEditMemo.Visible then
    Exit;
  if ssLeft in Shift then
  begin
    Image := (Sender as TListImage);
    FCurList := Image.StrList;
    // start moving
    ScreenPt := Image.ClientToScreen(Point(XPos, YPos));
    ScrollBoxPt := ScrollBox.ScreenToClient(ScreenPt);
    DeltaX := ScreenPt.X - FScreenMousePos.X;
    DeltaY := ScreenPt.Y - FScreenMousePos.Y;
    if (DeltaX = 0) and (DeltaY = 0) then
      Exit;

    if (Abs(DeltaX) + Abs(DeltaY) > 5) or FIsMoving then
    begin // move Image
      FIsMoving := True;
      Modified := True;
      FScreenMousePos := ScreenPt;
      FCurElement := FindVisibleElement(FCurList, XPos - DeltaX, YPos - DeltaY);
      if Assigned(FCurElement) and (FSeparating = 1) then
      begin
        DeltaX1 := XPos - FCurElement.Rct.Left;
        DeltaY1 := YPos - FCurElement.Rct.Top;
        if FCurElement.Prev = nil then
          Exit;
        ARect := FCurElement.Rct;
        if (FCurElement.Prev is TStrListHead) or (FCurElement.Prev is TStrCase) or
          (FPuzzleMode = 1) then
        begin
          StrStatement := TStrStatement.Create(FCurList);
          StrStatement.Rct := FCurElement.Rct;
          FCurList.Insert(FCurElement.Prev, StrStatement);
        end;

        if FPuzzleMode = 1 then
        begin // handle single statements, no lists
          FCurElement.Prev.Next := FCurElement.Next;
          if Assigned(FCurElement.Next) then
            FCurElement.Next.Prev := FCurElement.Prev;
          FCurElement.Prev := nil;
          FCurElement.Next := nil;
        end
        else
        begin
          FCurElement.Prev.Next := nil;
          FCurElement.Prev := nil;
        end;
        FCurList.ResizeAll;
        FCurList.Paint;

        // create new List
        StrList := TStrList.Create(ScrollBox, FPuzzleMode, Font);
        StrList.SetFont(Font);
        StrList.Insert(StrList, FCurElement);
        StrList.SetList(StrList);
        SetEvents(StrList.ListImage);
        StrList.ResizeAll;
        var
        Rect := StrList.RctList;
        Rect.Right := Max(ARect.Right - ARect.Left, StrList.RctList.Right);
        // ToDo prüfen
        Rect.Bottom := Max(ARect.Bottom - ARect.Top, StrList.RctList.Bottom);
        StrList.Rct := StrList.RctList;
        StrList.ListImage.Left := Image.Left + ARect.Left + DeltaY;
        StrList.ListImage.Top := Image.Top + ARect.Top + DeltaX;
        StrList.Paint;
        FSeparating := 2;
        FCurList.DontMove := True;
        FIgnoreNextMouseUp := True;
        FIsMoving := False;
        ScreenPt := StrList.ListImage.ClientToScreen(Point(DeltaX1, DeltaY1));
        SetCursorPos(ScreenPt.X, ScreenPt.Y);
        mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
        mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
        FCurList := StrList;
      end
      else
      begin
        // crucial for switching mouse to new image
        Image.SetBounds(Image.Left + DeltaX, Image.Top + DeltaY, Image.Width,
          Image.Height);
        if FCurList.DontMove then
        begin
          Image.SetBounds(Image.Left - DeltaX, Image.Top - DeltaY, Image.Width,
            Image.Height);
          FCurList.DontMove := False;
        end;
        if FCurList.Kind <> Byte(nsAlgorithm) then
        begin
          for var I := ScrollBox.ControlCount - 1 downto 0 do
          begin
            Image2 := TListImage(ScrollBox.Controls[I]);
            if Image2 <> Image then
            begin
              ARect := Image2.BoundsRect;
              ARect.Inflate(20, 20);
              if ARect.Contains(ScrollBoxPt) then
              begin
                Image2Pt := Image2.ScreenToClient(ScreenPt);
                FCurElement := FindElement(Image2.StrList, Image2Pt.X,
                  Image2Pt.Y);
                CalculateInsertionShape(Image2.StrList, FCurList, Image2Pt.X,
                  Image2Pt.Y);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFStructogram.CalculateInsertionShape(DestList, InsertList: TStrList;
  XPos, YPos: Integer);
var
  DeltaY: Integer;
begin
  // FCurElement.debug()
  if not Assigned(FCurElement) then
    Exit;

  with FCurElement do
    if not EqualRect(FOldShape, Rect(Rct.Bottom, Rct.Left, Rct.Bottom, Rct.Right))
    then
    begin
      if BeforeOrAfter(FCurElement) then
      begin
        if YPos < FCurElement.Rct.Top + FCurElement.GetMaxDelta then
        begin
          FOldShape := Rect(Rct.Left, Rct.Top, Rct.Right, Rct.Top);
          FCurInsert := -1; // Insert before
        end
        else if YPos > FCurElement.Rct.Bottom - FCurElement.GetMaxDelta then
        begin
          if FCurElement = List then
            FOldShape := Rect(Rct.Left, Rct.Bottom, List.RctList.Right,
              Rct.Bottom)
          else
            FOldShape := Rect(Rct.Left, Rct.Bottom, Rct.Right, Rct.Bottom);
          FCurInsert := +1; // Insert after
        end;
      end
      else
      begin
        DeltaY := (FCurElement.Rct.Bottom - FCurElement.Rct.Top) div 4;
        if YPos < FCurElement.Rct.Top + DeltaY then
        begin
          FOldShape := Rect(Rct.Left, Rct.Top, Rct.Right, Rct.Top);
          FCurInsert := -1;
        end
        else if YPos < FCurElement.Rct.Bottom - DeltaY then
        begin
          FOldShape := Rect(Rct.Left + 1, Rct.Top + 1, Rct.Right - 1,
            Rct.Bottom - 1);
          FCurInsert := 0;
        end
        else
        begin
          FOldShape := Rect(Rct.Left, Rct.Bottom, Rct.Right, Rct.Bottom);
          FCurInsert := +1;
        end;
      end;
      FOldCanvas := DestList.ListImage.Canvas;
      if FitsIn(InsertList, FCurElement) then
      begin
        ShowShape;
        DestList.Dirty := True;
      end;
    end;
end;

procedure TFStructogram.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; XPos, YPos: Integer);
var
  ScreenPt, QPoint, ScrollBoxPt, Image2Pt: TPoint;
  Image, Image2: TListImage;
  StrList: TStrList;
  ARect: TRect;
begin
  FIsMoving := False;
  Image := (Sender as TListImage);
  StrList := Image.StrList;

  if FIgnoreNextMouseUp then
  begin
    FIgnoreNextMouseUp := False;
    Exit;
  end;

  ARect := StrList.RctList;
  StrList.ResizeAll;
  if ARect <> StrList.RctList then
    StrList.Paint;
  if StrList.Kind in [Byte(nsAlgorithm), Byte(nsList)] then
  begin
    ScreenPt := Image.ClientToScreen(Point(XPos, YPos));
    QPoint := Self.ScreenToClient(ScreenPt);
    if QPoint.X < ScrollBox.Left then
    begin
      FreeAndNil(StrList);
      Exit;
    end;
    if QPoint.Y < ScrollBox.Top then
      Image.Top := 0;
  end;
  if StrList.Kind = Byte(nsList) then
  begin
    ScrollBoxPt := ScrollBox.ScreenToClient(ScreenPt);
    for var I := ScrollBox.ControlCount - 1 downto 0 do
    begin
      Image2 := TListImage(ScrollBox.Controls[I]);
      ARect := Image2.BoundsRect;
      ARect.Inflate(30, 30);
      if (Image2 <> Image) and ARect.Contains(ScrollBoxPt) then
      begin
        Image2Pt := Image2.ScreenToClient(ScreenPt);
        FCurElement := FindElement(Image2.StrList, Image2Pt.X, Image2Pt.Y);
        if Assigned(FCurElement) and FitsIn(StrList, FCurElement) then
          InsertElement(Image2.StrList, StrList, FCurElement);
      end;
    end;
  end;
  FSeparating := 0;
  PaintAll;
end;

// dropped on a structogram, Insert Element
procedure TFStructogram.InsertElement(DestList, InsertList: TStrList;
  ACurElement: TStrElement);
var
  Elems, APrevious: TStrElement;

  procedure Inserted;
  begin
    DestList.ResizeAll;
    DestList.Paint;
    Modified := True;
    InsertList.Next := nil;
    FreeAndNil(InsertList);
  end;

begin
  HideShape;
  if Assigned(FOldCanvas) then
    FOldCanvas.Pen.Mode := pmCopy;
  InsertList.SetList(DestList);
  Elems := InsertList.Next;
  if Assigned(Elems) then
  begin
    if FPuzzleMode = 1 then
    begin // replace empty statement
      DestList.Insert(ACurElement, Elems);
      DestList.DeleteElem(ACurElement);
    end
    else
    begin
      if (FCurInsert = 1) or (ACurElement.Prev = nil) then
        DestList.Insert(ACurElement, Elems)
      else if FCurInsert = 0 then
      begin
        DestList.Insert(ACurElement, Elems);
        DestList.DeleteElem(ACurElement);
      end
      else
      begin
        APrevious := ACurElement.Prev;
        DestList.Insert(APrevious, Elems);
      end;
    end;
    Inserted;
  end;
  FIsMoving := False;
end;

procedure TFStructogram.MIGenerateMethodClick(Sender: TObject);
begin
  CloseEdit;
  FConfiguration.GenerateJavaAsProgram := 0;
  GetCurList;
  BBGenerateJavaClick(Self);
end;

procedure TFStructogram.MIGenerateProgramClick(Sender: TObject);
begin
  CloseEdit;
  FConfiguration.GenerateJavaAsProgram := 1;
  GetCurList;
  BBGenerateJavaClick(Self);
end;

procedure TFStructogram.MIPuzzleClick(Sender: TObject);
begin
  if FPuzzleMode > 0 then
  begin
    SetPuzzleMode(0);
    FSolution := '';
  end
  else
  begin
    if not((ScrollBox.ControlCount = 1) and (TListImage(ScrollBox.Controls[0])
      .StrList is TStrAlgorithm)) then
    begin
      ShowMessage
        (_('Switch from a single algorithm structogram as the final FSolution to puzzle mode.')
        );
      Exit;
    end;
    SetPuzzleMode(1);
    var
    StrList := TListImage(ScrollBox.Controls[0]).StrList;
    StrList.SetPuzzleMode(1);
    FSolution := StrList.AsString;
  end;
end;

procedure TFStructogram.SetPuzzleMode(Mode: Integer);
begin
  FPuzzleMode := Mode;
  MIPuzzleMode.Checked := (FPuzzleMode = 1);
  TBPuzzleMode.Visible := (FPuzzleMode = 1);
end;

procedure TFStructogram.MISwitchWithCaseLineClick(Sender: TObject);
begin
  if not FReadOnly then
  begin
    for var I := 0 to ScrollBox.ControlCount - 1 do
    begin
      var
      StrList := TListImage(ScrollBox.Controls[I]).StrList;
      StrList.SwitchWithCaseLine := not StrList.SwitchWithCaseLine;
      StrList.Paint;
    end;
    UpdateState;
  end;
end;

procedure TFStructogram.MIAddCaseClick(Sender: TObject);
begin
  CloseEdit;
  if GetCurListAndCurElement and (FCurElement is TStrSwitch) then
  begin
    var
    Switch := (FCurElement as TStrSwitch);
    var
    Int := Length(Switch.CaseElems);
    var
    Elems := Switch.CaseElems;
    SetLength(Elems, Int + 1);
    Switch.CaseElems := Elems;
    Switch.CaseElems[Int] := Switch.CaseElems[Int - 1];
    Switch.CaseElems[Int - 1] := TStrListHead.Create(FCurList, Switch);
    FCurList.ResizeAll;
    FCurList.Paint;
    Modified := True;
    UpdateState;
  end;
end;

procedure TFStructogram.MIDeleteCaseClick(Sender: TObject);
var
  Int, Num: Integer;
begin
  CloseEdit;
  if GetCurListAndCurElement and (FCurElement is TStrSwitch) then
  begin
    var
    Switch := (FCurElement as TStrSwitch);
    Int := 0;
    while (Int < Length(Switch.CaseElems)) and
      (FMousePos.X > Switch.CaseElems[Int].Rct.Right) do
      Inc(Int);
    FreeAndNil(Switch.CaseElems[Int]);
    Num := High(Switch.CaseElems);
    for var K := Int to Num - 1 do
      Switch.CaseElems[K] := Switch.CaseElems[K + 1];
    var
    Elems := Switch.CaseElems;
    SetLength(Elems, Num);
    Switch.CaseElems := Elems;
    FCurList.ResizeAll;
    FCurList.Paint;
    Modified := True;
    UpdateState;
  end;
end;

procedure TFStructogram.MIDeleteClick(Sender: TObject);
begin
  if GetCurListAndCurElement then
  begin
    if (FCurElement.Kind = Byte(nsAlgorithm)) or
      (Assigned(FCurElement.Prev) and (FCurElement.Prev.Kind = Byte(nsList))) then
      FreeAndNil(FCurList)
    else
    begin
      FCurList.DeleteElem(FCurElement);
      FCurList.ResizeAll;
      FCurList.Paint;
    end;
    Modified := True;
    UpdateState;
  end;
end;

procedure TFStructogram.MICopyClick(Sender: TObject);
var
  StrList: TStrList;
  Stream: TMemoryStream;
begin
  if GetCurList then
  begin
    Stream := TMemoryStream.Create;
    try
      FCurList.SaveToStream(Stream);
      if FCurList is TStrAlgorithm then
        StrList := TStrAlgorithm.Create(ScrollBox, FPuzzleMode, Font)
      else
        StrList := TStrList.Create(ScrollBox, FPuzzleMode, Font);
      Stream.Position := 0;
      StrList.LoadFromStream(Stream, FVersion);
      StrList.SetFont(Font);
      StrList.ResizeAll;
      StrList.ListImage.SetBounds(FCurList.ListImage.Left + 30,
        FCurList.ListImage.Top + 30, FCurList.ListImage.Width,
        FCurList.ListImage.Height);
      SetEvents(StrList.ListImage);
      StrList.SetList(StrList);
      StrList.Paint;
      Modified := True;
      UpdateState;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

procedure TFStructogram.MIConfigurationClick(Sender: TObject);
begin
  FConfiguration.OpenAndShowPage('Structograms');
end;

procedure TFStructogram.MIFontClick(Sender: TObject);
begin
  FJava.MIFontClick(Sender);
end;

procedure TFStructogram.DoEdit(StrElement: TStrElement; Str: string);
var
  Left, Top, Width, Height: Integer;
  Image: TListImage;
begin
  if Assigned(StrElement) and not FReadOnly then
  begin
    FEditMemoElement := StrElement;
    Image := StrElement.List.ListImage;
    FEditMemo.Text := StrElement.Text + Str;
    FEditMemoBeginText := StrElement.Text;
    Left := ToolBarStructogram.Width + Image.Left + StrElement.Rct.Left;
    Top := Image.Top + StrElement.Rct.Top;
    Width := StrElement.Rct.Right - StrElement.Rct.Left + 1;
    Height := FEditMemo.Lines.Count * StrElement.List.LineHeight + 1;
    Height := Max(StrElement.GetHeadHeight + 1, Height);

    if StrElement is TStrIf then
    begin
      Width := StrElement.List.GetWidthOfLines(FEditMemo.Text) + 10;
      Left := ToolBarStructogram.Width + Image.Left + StrElement.TextPos.X - 5;
    end
    else if StrElement is TStrSwitch then
    begin
      Left := ToolBarStructogram.Width + Image.Left + StrElement.TextPos.X - 5;
      Width := Math.Max(100, StrElement.List.GetWidthOfLines(FEditMemo.Text) + 10);
    end
    else if (StrElement is TStrSubprogram) then
    begin
      Left := ToolBarStructogram.Width + Image.Left + StrElement.TextPos.X - 5;
      FEditMemo.Width := FEditMemo.Width - LEFT_RIGHT;
    end
    else if (StrElement is TStrBreak) then
    begin
      Left := ToolBarStructogram.Width + Image.Left + StrElement.TextPos.X - 5;
      Width := FEditMemo.Width - LEFT_RIGHT;
    end
    else if (StrElement is TStrDoWhile) then
      Top := Image.Top + (StrElement as TStrDoWhile).DoElem.Rct.Bottom;

    FEditMemo.SetBounds(Left + 2, Top + 2, Width, Height);
    SetLeftBorderForEditMemo;
    FEditMemo.Visible := True;
    if FEditMemo.CanFocus then
      FEditMemo.SetFocus;
    FEditMemo.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TFStructogram.SetLeftBorderForEditMemo;
begin
  var
  Rect := FEditMemo.ClientRect;
  Rect.Left := 4;
  SendMessage(FEditMemo.Handle, EM_SETRECT, 0, LPARAM(@Rect));
end;

procedure TFStructogram.EditMemoChange(Sender: TObject);
var
  Width, Height: Integer;
  AList: TStrList;
begin
  AList := GetList;
  if Assigned(AList) then
  begin
    AList.GetWidthHeigthOfText(FEditMemo.Lines.Text, Width, Height);
    if FEditMemo.Height < Height then
      FEditMemo.Height := Height;
    if FEditMemo.Width < Width then
      FEditMemo.Width := Width;
  end;
  SendMessage(FEditMemo.Handle, WM_VSCROLL, SB_TOP, 0);
  SetLeftBorderForEditMemo;
end;

procedure TFStructogram.CloseEdit;
begin
  if FEditMemo.Visible then
  begin
    if Assigned(FEditMemoElement) then
    begin
      FEditMemoElement.Text := FEditMemo.Text;
      if FEditMemo.Text <> FEditMemoBeginText then
        Modified := True;
    end;
    if Assigned(FCurList) then
    begin
      FCurList.ResizeAll;
      FCurList.Paint;
    end;
    UpdateState;
    FEditMemo.Visible := False;
  end;
end;

procedure TFStructogram.FormKeyPress(Sender: TObject; var Key: Char);
var
  Str: string;
begin
  if Key = #08 then
    Str := ''
  else
    Str := Key;
  if not FEditMemo.Visible and Assigned(FCurElement) then
    DoEdit(FCurElement, Str);
end;

procedure TFStructogram.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then
    CutToClipboard
  else if Key = VK_INSERT then
    PasteFromClipboard
  else if (Key = VK_F2) and Assigned(FCurElement) then
    DoEdit(FCurElement, '');
end;

procedure TFStructogram.BBCloseClick(Sender: TObject);
begin
  Close;
end;

function TFStructogram.GetName(StrList: TStrList): string;
begin
  if StrList is TStrAlgorithm then
    Result := (StrList as TStrAlgorithm).GetAlgorithmName;
  if Result = '' then
    Result := TPath.GetFileNameWithoutExtension(Pathname);
end;

procedure TFStructogram.BBGenerateJavaClick(Sender: TObject);
var
  FileName, AName: string;
  ProgList: TStringList;
  StrList: TStrList;
begin
  if not Assigned(FCurList) then
    FCurList := GetAlgorithm;
  if not Assigned(FCurList) then
    FCurList := GetList;
  if not Assigned(FCurList) then
    Exit;
  StrList := FCurList;
  AName := GetName(StrList);
  FileName := ExtractFilePath(Pathname) + AName + '.java';
  ProgList := TStringList.Create;
  try
    try
      JavaProgram(StrList, ProgList, AName);
      if (Assigned(FJava.GetTDIWindowType(FileName, '%E%')) or
        FileExists(FileName)) and
        (MessageDlg(Format(_(LNGFileAlreadyExists), [FileName]), mtConfirmation,
        mbYesNoCancel, 0) <> mrYes) then
        FileName := FJava.GetFilename('.java');
      if FileName <> '' then
      begin
        ProgList.SaveToFile(FileName);
        if FJava.Open(FileName) then
          FJava.RearrangeFileHistory(FileName);
      end;
    except
      on e: Exception do
        ErrorMsg(Format(_(LNGCanNotCreateFile), [FileName, e.Message]));
    end;
  finally
    FreeAndNil(ProgList);
  end;
end;

procedure TFStructogram.BBPuzzleClick(Sender: TObject);
begin
  for var I := ScrollBox.ControlCount - 1 downto 0 do
  begin
    var
    StrList := TListImage(ScrollBox.Controls[I]).StrList;
    if StrList is TStrAlgorithm then
    begin
      if StrList.AsString = FSolution then
        ShowMessage(_('Great, solved!'))
      else
        ShowMessage(_('Try again!'));
      Exit;
    end;
  end;
end;

procedure TFStructogram.BBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
end;

procedure TFStructogram.BBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
end;

procedure TFStructogram.Save(WithBackup: Boolean);
var
  BackupName, FileName, APathname, Filepath, Ext: string;
  FStructogram: TFForm;
begin
  CloseEdit;
  try
    if (FPuzzleMode = 1) and (Pos(_('Easy'), Pathname) = 0) then
    begin
      FileName := ChangeFileExt(ExtractFileName(Pathname), '');
      Filepath := ExtractFilePath(Pathname);

      FPuzzleMode := 2;
      APathname := Filepath + FileName + _('Medium') + '.jsg';
      SaveToFile(APathname);
      FJava.Open(APathname);

      FPuzzleMode := 3;
      APathname := Filepath + FileName + _('Hard') + '.jsg';
      SaveToFile(APathname);
      FJava.Open(APathname);

      FPuzzleMode := 4;
      APathname := Filepath + FileName + _('VeryHard') + '.jsg';
      SaveToFile(APathname);
      FJava.Open(APathname);
      FStructogram := FJava.GetTDIWindowType(APathname, '%S%');
      (FStructogram as TFStructogram).MakeVeryHard;
      (FStructogram as TFStructogram).Save(False);

      FPuzzleMode := 1;
      Pathname := Filepath + FileName + _('Easy') + '.jsg';
      SaveToFile(Pathname);
      Caption := Pathname;
      FJava.RenameTabAndWindow(Number, Pathname);
      FJava.RearrangeFileHistory(Pathname);
      FJava.Open(Pathname);
    end
    else
    begin
      if FReadOnly then
        Exit;
      if WithBackup then
      begin
        BackupName := Pathname;
        Ext := ExtractFileExt(Pathname);
        if Length(Ext) >= 2 then
          Ext[2] := '~'
        else
          Ext := '.~';
        BackupName := ChangeFileExt(BackupName, Ext);
        if FileExists(BackupName) then
          DeleteFile(PChar(BackupName));
        if FileExists(Pathname) then
          RenameFile(Pathname, BackupName);
      end;
      SaveToFile(Pathname);
    end;
    Modified := False;
  except
    on e: Exception do
    begin
      ErrorMsg(e.Message);
    end;
  end;

  { debugging

    for var i:= 0 to ScrollBox.ControlCount - 1 do begin
    var AList:= (ScrollBox.Controls[i] as TListImage).StrList;
    AList.debug;
    AList.Paint;
    end;
  }

end;

// Click outside of any image
procedure TFStructogram.ScrollBoxClick(Sender: TObject);
begin
  UpdateState;
  if CanFocus then
    SetFocus;
  CloseEdit;
end;

procedure TFStructogram.ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  HideShape;
end;

procedure TFStructogram.DoExport;
begin
  var Bitmap := GetStructogramAsBitmap;
  FJava.DoExport(Pathname, Bitmap);
  FreeAndNil(Bitmap);
  if CanFocus then
    SetFocus;
end;

procedure TFStructogram.Print;
begin
  var Bitmap := GetStructogramAsBitmap;
  PrintBitmap(Bitmap, PixelsPerInch);
  FreeAndNil(Bitmap);
end;

procedure TFStructogram.CopyToClipboard;
begin
  if FEditMemo.Visible then
    FEditMemo.CopyToClipboard
  else begin
    var Bitmap:= GetStructogramAsBitmap;
    Clipboard.Assign(Bitmap);
    FreeAndNil(Bitmap);
    UpdateState;
  end;
end;

procedure TFStructogram.MICopyAsPictureClick(Sender: TObject);
var
  Bitmap: Graphics.TBitmap;
begin
  if GetCurList then
  begin
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := FCurList.ListImage.Width +
        FConfiguration.StructogramShadowWidth;
      Bitmap.Height := FCurList.ListImage.Height +
        FConfiguration.StructogramShadowWidth;
      FCurList.Paint(True);
      Bitmap.Canvas.Draw(0, 0, FCurList.ListImage.Picture.Graphic);
      FCurList.Paint(False);
      Clipboard.Assign(Bitmap);
    finally
      FreeAndNil(Bitmap);
    end;
  end
  else
  begin
    CloseEdit;
    CopyToClipboard;
  end;
end;

procedure TFStructogram.CutToClipboard;
begin
  if FEditMemo.Visible then
  begin
    FEditMemo.CutToClipboard;
    Modified := True;
    UpdateState;
  end;
end;

procedure TFStructogram.PasteFromClipboard;
begin
  if FEditMemo.Visible then
    FEditMemo.PasteFromClipboard;
end;

procedure TFStructogram.MIDatatypeClick(Sender: TObject);
begin
  var
  Datatype := (Sender as TSpTBXItem).Caption;
  FConfiguration.StructoDatatype := ReplaceStr(Datatype, '&', '');
end;

procedure TFStructogram.SetFont(AFont: TFont);
begin
  Font.Assign(AFont);
  for var I := 0 to ScrollBox.ControlCount - 1 do
  begin
    var
    StrList := TListImage(ScrollBox.Controls[I]).StrList;
    StrList.SetFont(AFont);
    StrList.SetLineHeight;
    StrList.ResizeAll;
    StrList.Paint;
  end;
  Modified := True;
  FConfiguration.StructogramFont.Assign(AFont);
end;

procedure TFStructogram.StructoPopupMenuPopup(Sender: TObject);
var
  Found, AParent: TStrElement;
begin
  MISwitchWithCaseLine.Caption := FConfiguration.CBSwitchWithCaseLine.Caption;
  MIGenerateMethod.Caption := Trim(FConfiguration.RGGenerateJavacode.Caption) +
    ' ' + FConfiguration.RGGenerateJavacode.Items[0];
  MIGenerateProgram.Caption := Trim(FConfiguration.RGGenerateJavacode.Caption) +
    ' ' + FConfiguration.RGGenerateJavacode.Items[1];
  MIDataType.Caption := FConfiguration.LDatatype.Caption;

  MIAddCase.Visible := False;
  MIDeleteCase.Visible := False;
  MISwitchWithCaseLine.Visible := False;

  var Datatype := FConfiguration.StructoDatatype;
  for var I := 0 to MIDataType.Count - 1 do
    MIDataType[I].Checked :=
      (ReplaceStr(MIDataType[I].Caption, '&', '') = Datatype);

  if GetCurList then
  begin
    MISwitchWithCaseLine.Checked := FCurList.SwitchWithCaseLine;
    Found := FindVisibleElement(FCurList, FMousePos.X, FMousePos.Y);

    AParent := GetParentElement(Found);
    FCurElement := nil;
    if Assigned(Found) and (Found is TStrSwitch) then
      FCurElement := Found
    else if Assigned(AParent) and (AParent is TStrSwitch) then
      FCurElement := AParent;

    if Assigned(FCurElement) then
    begin
      MIAddCase.Visible := True;
      MIDeleteCase.Visible :=
        (Length((FCurElement as TStrSwitch).CaseElems) > 2);
      MISwitchWithCaseLine.Visible := False;
    end
    else
      FCurElement := Found;
  end;
  MIDelete.Enabled := Assigned(FCurList);
  MICopy.Enabled := Assigned(FCurList);
end;

procedure TFStructogram.UpdateState;
begin
  inherited;
  with FJava do
  begin
    SetEnabledMI(MICopyNormal, True);
    SetEnabledMI(MICopy, True);
  end;
  MISwitchWithCaseLine.Checked := FConfiguration.SwitchWithCaseLine;
  TBStatement.Enabled := not FReadOnly;
  TBIf.Enabled := not FReadOnly;
  TBSwitch.Enabled := not FReadOnly;
  TBwhile.Enabled := not FReadOnly;
  TBDoWhile.Enabled := not FReadOnly;
  TBFor.Enabled := not FReadOnly;
  TBSubProgram.Enabled := not FReadOnly;
  TBBreak.Enabled := not FReadOnly;
end;

function TFStructogram.GetFormType: string;
begin
  Result := '%S%';
end;

function TFStructogram.GetSaveAsName: string;
begin
  var
  StrList := GetAlgorithm;
  if Assigned(StrList) then
    Result := StrList.GetAlgorithmName
  else
    Result := '';
end;

procedure TFStructogram.SetOptions;
begin
  var
  NotLocked := not FConfiguration.LockedStructogram;
  TBGenerateJava.Enabled := NotLocked;
  MIGenerateMethod.Visible := NotLocked;
  MIGenerateProgram.Visible := NotLocked;
  MIDataType.Visible := NotLocked;
end;

procedure TFStructogram.AddParameter(List: TStrList; ParamList: TStringList);
var
  AText, Token, Param: string;
  Posi, Typ: Integer;
begin
  Posi := Pos('(', List.Text);
  if Posi > 0 then
  begin
    AText := Copy(List.Text, Posi + 1, Length(List.Text));
    Typ := GetToken(AText, Token);
    while (Typ > 0) and (Token <> ')') do
    begin
      if Typ = 1 then
      begin
        Param := Token;
        Typ := GetToken(AText, Token);
        if Typ = 1 then
          Param := Param + ' ' + Token
        else
          Param := FDataType + ' ' + Param;
        ParamList.Add(Param);
      end;
      Typ := GetToken(AText, Token);
    end;
  end;
end;

procedure TFStructogram.AddVariable(AText: string; List: TStringList);
var
  Typ: Integer;
  IsArray: Boolean;
  Token, Variable: string;
begin
  if Pos('=', AText) > 0 then
  begin
    AText := Trim(AText);
    GetToken(AText, Variable);
    if Pos('#' + Variable + '#', FVariables) = 0 then
    begin
      FVariables := FVariables + '#' + Variable + '#'; // store of FVariables;
      GetToken(AText, Token);
      IsArray := (Token = '[');
      Typ := GetToken(AText, Token);
      if Pos('InOut.read', Token) = 1 then
      begin
        if Token = 'Char' then
          Typ := 2
        else if Token = 'Boolean' then
          Typ := 3
        else if Token = 'Int' then
          Typ := 4
        else if Token = 'Double' then
          Typ := 5
        else if Token = 'Float' then
          Typ := 6
        else if Token = 'String' then
          Typ := 7;
      end;
      while Typ > 0 do
      begin
        if (Typ > 1) and (Typ < 8) then
          Break;
        Typ := GetToken(AText, Token);
      end;
      Variable := ReplaceStr(Variable, ' ', '_');
      if IsArray then
        case Typ of
          2:
            Variable := 'char[] ' + Variable + ' = new char[100];';
          3:
            Variable := 'boolean[] ' + Variable + ' = new boolean[100];';
          4:
            Variable := 'int[] ' + Variable + ' = new int[100];';
          5:
            Variable := 'double[] ' + Variable + ' = new double[100];';
          6:
            Variable := 'float[] ' + Variable + ' = new float[100];';
          7:
            Variable := 'String[] ' + Variable + ' = new String[100];';
        else
          Variable := FDataType + '[] ' + Variable + ' = new ' + FDataType
            + '[100];';
        end
      else
        case Typ of
          2:
            Variable := 'char ' + Variable + ';';
          3:
            Variable := 'boolean ' + Variable + ';';
          4:
            Variable := 'int ' + Variable + ';';
          5:
            Variable := 'double ' + Variable + ';';
          6:
            Variable := 'float ' + Variable + ';';
          7:
            Variable := 'String ' + Variable + ';';
        else
          Variable := FDataType + ' ' + Variable + ';';
        end;
      if FConfiguration.GenerateJavaAsProgram = 0 then
        List.Add(FConfiguration.Indent1 + Variable)
      else
        List.Add(FConfiguration.Indent2 + Variable);
    end;
  end;
end;

function TFStructogram.GetAlgorithmParameter(ParamList: TStringList): string;
var
  Param: string;
begin
  Param := '';
  for var I := 0 to ParamList.Count - 1 do
    Param := Param + Trim(ParamList[I]) + ', ';
  if Param <> '' then
    Delete(Param, Length(Param) - 1, 2);
  Result := '(' + Param + ')';
end;

procedure TFStructogram.JavaProgram(List: TStrList; ProgList: TStringList;
  const AName: string);
var
  Int, JPos, Posi: Integer;
  ParamList, VariablesList: TStringList;
  Str1, Str2, Str, Indent, Param, Typ: string;
begin
  FDataType := FConfiguration.StructoDatatype;
  FVariables := '';
  ParamList := TStringList.Create;
  VariablesList := TStringList.Create;
  try
    AddParameter(List, ParamList);
    if FConfiguration.GenerateJavaAsProgram = 0 then
      Indent := FConfiguration.Indent1
    else
      Indent := FConfiguration.Indent2;
    StrElementToJava(List.Next, ProgList, VariablesList, Indent);
    for var I := 0 to ParamList.Count - 1 do
    begin
      JPos := 0;
      while JPos < VariablesList.Count do
      begin
        Param := ParamList[I];
        Posi := Pos(' ', Param);
        Delete(Param, 1, Posi);
        if (Pos(' ' + Param + ';', VariablesList[JPos]) > 0) then
        begin
          Typ := Trim(VariablesList[JPos]);
          Posi := Pos(' ', Typ);
          Delete(Typ, Posi, Length(Typ));
          ParamList[I] := Typ + ' ' + Param;
          VariablesList.Delete(JPos);
        end;
        Inc(JPos);
      end;
    end;
    ProgList.Insert(0, '');
    if VariablesList.Count > 0 then
    begin
      for var I := VariablesList.Count - 1 downto 0 do
        if ProgList.IndexOf(VariablesList[I]) < 0 then
          ProgList.Insert(0, VariablesList[I]);
    end;

    if FConfiguration.GenerateJavaAsProgram = 1 then
    begin
      Str1 := '';
      Str2 := '';
      if FConfiguration.CommentClosingBrackets then
      begin
        Str1 := ' // end of main';
        Str2 := ' // end of class ' + AName;
      end;
      Str := FTemplates.GetTemplate(AName, 1);
      if Str = '' then
      begin
        Str := FConfiguration.HeadText + CrLf + 'public class ' + AName + ' {' +
          CrLf + FConfiguration.Indent1 + CrLf + FConfiguration.Indent1 +
          'public static void main(String[] args) {' + CrLf +
          FConfiguration.Indent2 + '' + CrLf + FConfiguration.Indent1 + '}' +
          Str1 + CrLf + CrLf + '}' + Str2 + CrLf;
      end;
      Int := Pos('public static void', Str);
      while (Int < Length(Str)) and (Str[Int] <> #13) do
        Inc(Int);
      Str1 := Copy(Str, 1, Int + 1);
      Str2 := Copy(Str, Int + 3, Length(Str));
    end
    else
    begin
      Str1 := 'public void ' + AName + GetAlgorithmParameter(ParamList) +
        ' {' + CrLf;
      Str2 := CrLf + '}';
      if FConfiguration.CommentClosingBrackets then
        Str2 := Str2 + ' // end of ' + AName;
    end;
  finally
    FreeAndNil(VariablesList);
    FreeAndNil(ParamList);
  end;
  ProgList.Insert(0, Str1);
  ProgList.Add(Str2);
end;

procedure TFStructogram.StrElementToJava(Element: TStrElement;
  PList, VariablesList: TStringList; const Indent: string);
var
  Int, Posi, QPos, TPos, Int1, Int2: Integer;
  AText, Str, Vari, Start, End_, For_, Step, Part: string;
  Str1, Str2, Repl: string;
  StringList: TStringList;
  SwitchElement: TStrSwitch;

  function StripLNG(const LNG: string; Str: string): string;
  var
    Posi: Integer;
  begin
    Posi := Pos(LNG, Str);
    if Posi > 0 then
      Delete(Str, Posi, Length(LNG));
    Result := Trim(Str);
  end;

  function withoutCrLf(const Str: string): string;
  begin
    Result := ReplaceStr(Str, CrLf, ' ');
  end;

  procedure SkipToChar(Chr: Char);
  begin
    Inc(TPos);
    while (TPos < StringList.Count) and (Pos(Chr, StringList[TPos]) = 0) do
    begin
      PList.Add(Indent + StringList[TPos]);
      Inc(TPos);
    end;
  end;

  procedure ReplaceInsertMarker(const Repl: string);
  var
    Posi: Integer;
  begin
    Str := StringList[TPos];
    Posi := Pos('|', Str);
    if Posi = 0 then
    begin // do while
      Posi := Pos('(', Str);
      Insert(Repl, Str, Posi + 1);
    end
    else
    begin
      Delete(Str, Posi, 1);
      Insert(Repl, Str, Posi);
    end;
    PList.Add(Indent + Str);
  end;

  procedure SkipToInsertLine;
  begin
    Inc(TPos);
    while (TPos < StringList.Count) and not((Trim(StringList[TPos]) = '') or (Trim(StringList[TPos]) = '|')) do
    begin
      PList.Add(Indent + StringList[TPos]);
      Inc(TPos);
    end;
  end;

  procedure SkipToEnd;
  begin
    Inc(TPos);
    while TPos < StringList.Count do
    begin
      PList.Add(Indent + StringList[TPos]);
      Inc(TPos);
    end;
  end;

begin
  while Assigned(Element) do
  begin
    if Element is TStrStatement then
    begin
      Str := withoutCrLf(Element.Text);
      if Pos(FConfiguration.Input, Str) = 1 then
      begin
        Delete(Str, 1, Length(FConfiguration.Input));
        Str := Trim(Str);
        Str := ReplaceStr(Str, ' ', '_');
        AddVariable(Str + '= ;', VariablesList);
        Str := Str + ' = InOut.read' + UpperLower(FDataType) + '("' + Str
          + ': ");';
      end
      else if Pos(FConfiguration.Output, Str) = 1 then
      begin
        Delete(Str, 1, Length(FConfiguration.Output));
        Str := Trim(Str);
        if Pos('"" + ', Str) > 0 then
          Str := 'System.out.println(' + Str + ');'
        else
          Str := 'System.out.println("" + ' + Str + ');';
      end;
      AText := Indent + Str;
      Int := Pos(';', AText);
      if Int = 0 then
        AText := AText + ';';
      if Trim(Str) <> '' then
        PList.Add(AText);
      { declaration of FVariables only in TStrStatement possible }
      AddVariable(AText, VariablesList);
    end
    else if Element is TStrIf then
    begin
      if (TStrIf(Element).ElseElem.Next.Text <> '') or
        (TStrIf(Element).ElseElem.Next.Next <> nil) then
        StringList := FConfiguration.ControlStructureTemplates[9]
      else
        StringList := FConfiguration.ControlStructureTemplates[1];
      TPos := -1;
      SkipToChar('|');
      ReplaceInsertMarker(withoutCrLf(Element.Text));
      SkipToInsertLine;
      StrElementToJava(TStrIf(Element).ThenElem.Next, PList, VariablesList,
        Indent + FConfiguration.Indent1);
      SkipToChar('}');
      if TPos < StringList.Count then
        PList.Add(Indent + StringList[TPos]);
      if (TStrIf(Element).ElseElem.Next.Text <> '') or
        (TStrIf(Element).ElseElem.Next.Next <> nil) then
      begin
        SkipToInsertLine;
        StrElementToJava(TStrIf(Element).ElseElem.Next, PList, VariablesList,
          Indent + FConfiguration.Indent1);
      end;
      SkipToEnd;
    end
    else if Element is TStrDoWhile then
    begin
      StringList := FConfiguration.ControlStructureTemplates[4];
      TPos := -1;
      SkipToInsertLine;
      StrElementToJava(TStrDoWhile(Element).DoElem.Next, PList, VariablesList,
        Indent + FConfiguration.Indent1);
      Repl := StripLNG(FConfiguration.DoWhile, withoutCrLf(Element.Text));
      SkipToChar('(');
      ReplaceInsertMarker(Repl);
      SkipToEnd;
    end
    else if Element is TStrFor then
    begin
      Repl := withoutCrLf(Element.Text);
      For_ := FConfiguration._For;
      Posi := Pos('[', For_);
      if Posi > 0 then
      begin
        Part := Copy(For_, 1, Posi - 1);
        Delete(Repl, 1, Posi - 1);
        Delete(For_, 1, Posi + 2);
        Posi := Pos('[', For_);

        if Posi > 0 then
        begin
          Part := Copy(For_, 1, Posi - 1);
          QPos := Pos(Part, Repl);
          Vari := Copy(Repl, 1, QPos - 1);
          Delete(Repl, 1, QPos + Length(Part) - 1);
          Delete(For_, 1, Posi + 2);
        end
        else
          Vari := 'i';

        Posi := Pos('[', For_);
        if Posi > 0 then
        begin
          Part := Copy(For_, 1, Posi - 1);
          QPos := Pos(Part, Repl);
          Start := Copy(Repl, 1, QPos - 1);
          Delete(Repl, 1, QPos + Length(Part) - 1);
          Delete(For_, 1, Posi + 2);
        end
        else
          Start := '1';

        Posi := Pos('[', For_);
        if Posi > 0 then
        begin
          Part := Copy(For_, 1, Posi - 1);
          QPos := Pos(Part, Repl);
          if QPos > 0 then
          begin
            End_ := Copy(Repl, 1, QPos - 1);
            Delete(Repl, 1, QPos + Length(Part) - 1);
            Delete(For_, 1, Posi - 1);
          end
          else
            End_ := Repl;
        end
        else
          End_ := 'n';

        if For_ = '[s]' then
        begin
          if TryStrToInt(Repl, Int1) then
            if Int1 < 0 then
              Step := Vari + ' = ' + Vari + ' - ' + IntToStr(Int1)
            else
              Step := Vari + ' = ' + Vari + ' + ' + IntToStr(Int1)
          else
            Step := Vari + ' = ' + Vari + ' + ' + Repl;
        end
        else if TryStrToInt(Start, Int1) and TryStrToInt(End_, Int2) and
          (Int2 < Int1) then
          Step := Vari + '--'
        else
          Step := Vari + '++';
        if Pos('+', Step) > 0 then
          Repl := 'int ' + Vari + ' = ' + Start + '; ' + Vari + ' <= ' + End_ +
            '; ' + Step
        else
          Repl := 'int ' + Vari + ' = ' + Start + '; ' + Vari + ' >= ' + End_ +
            '; ' + Step;
      end
      else
        Repl := 'int i = 1; i < n; i++';
      StringList := FConfiguration.ControlStructureTemplates[3];
      TPos := -1;
      SkipToChar('|');

      Str := StringList[TPos];
      Posi := Pos('|', Str);
      Str1 := Copy(Str, 1, Posi - 1);
      Posi := Pos(')', Str);
      Str2 := Copy(Str, Posi, Length(Str));
      PList.Add(Indent + Str1 + Repl + Str2);
      SkipToInsertLine;
      StrElementToJava(TStrWhile(Element).DoElem.Next, PList, VariablesList,
        Indent + FConfiguration.Indent1);
      SkipToEnd;
    end
    else if Element is TStrWhile then
    begin
      Repl := StripLNG(FConfiguration._While, withoutCrLf(Element.Text));
      StringList := FConfiguration.ControlStructureTemplates[2];
      TPos := -1;
      SkipToChar('|');
      ReplaceInsertMarker(Repl);
      SkipToInsertLine;
      StrElementToJava(TStrWhile(Element).DoElem.Next, PList, VariablesList,
        Indent + FConfiguration.Indent1);
      SkipToEnd;
    end
    else if Element is TStrSwitch then
    begin
      StringList := FConfiguration.ControlStructureTemplates[5];
      TPos := -1;
      SkipToChar('|');
      ReplaceInsertMarker(withoutCrLf(Element.Text));
      SwitchElement := Element as TStrSwitch;
      for var I := 0 to High(SwitchElement.CaseElems) - 1 do
      begin
        AText := Indent + FConfiguration.Indent1 + 'case ' +
          SwitchElement.CaseElems[I].Next.Text + ': ';
        PList.Add(AText);
        StrElementToJava(SwitchElement.CaseElems[I].Next.Next, PList,
          VariablesList, Indent + FConfiguration.Indent2);
        AText := Indent + FConfiguration.Indent2 + 'break;';
        PList.Add(AText);
      end;
      Int := High(SwitchElement.CaseElems);
      AText := Indent + FConfiguration.Indent1 + 'default: ';
      PList.Add(AText);
      StrElementToJava(SwitchElement.CaseElems[Int].Next.Next, PList,
        VariablesList, Indent + FConfiguration.Indent2);
      PList.Add(Indent + '}');
    end
    else if Element is TStrSubprogram then
    begin
      Str := withoutCrLf(Element.Text);
      if Trim(Str) <> '' then
        PList.Add(Indent + Str);
    end
    else if Element is TStrBreak then
      PList.Add(Indent + 'break;');
    Element := Element.Next;
  end;
end;

function TFStructogram.FindElement(Current: TStrElement; X, Y: Integer)
  : TStrElement;
var
  IfElement: TStrIf;
  WhileElement: TStrWhile;
  DoWhileElement: TStrDoWhile;
  SwitchElement: TStrSwitch;
begin
  while Assigned(Current) and Assigned(Current.Next) and
    (Y > Current.Rct.Bottom) do
    Current := Current.Next;
  if Current is TStrIf then
  begin { search recursively inside IF }
    IfElement := (Current as TStrIf);
    if Y > Current.Rct.Top + Current.GetHeadHeight div 2 then
      if X < IfElement.ThenElem.Rct.Right - 5 then { except if ±5 from middle }
        Current := FindElement(IfElement.ThenElem.Next, X, Y)
      else if X > IfElement.ElseElem.Rct.Left + 5 then
        Current := FindElement(IfElement.ElseElem.Next, X, Y);
  end
  else if Current is TStrDoWhile then
  begin { search recursively inside DO-WHILE }
    DoWhileElement := Current as TStrDoWhile;
    if (X > DoWhileElement.DoElem.Rct.Left) and (Y > Current.Rct.Top) then
      Current := FindElement(DoWhileElement.DoElem.Next, X, Y);
  end
  else if Current is TStrWhile then
  begin { search recursively inside WHILE/REPEAT/FOR(!) }
    WhileElement := Current as TStrWhile;
    if (X > WhileElement.DoElem.Rct.Left) and
      (Y > Current.Rct.Top + Current.GetHeadHeight div 2) then
      Current := FindElement(WhileElement.DoElem.Next, X, Y);
  end
  else if Current is TStrSwitch then
  begin { search recursively inside SWITCH }
    SwitchElement := (Current as TStrSwitch);
    if Y > Current.Rct.Top + Current.GetHeadHeight then
      for var I := 0 to High(SwitchElement.CaseElems) do
        if (SwitchElement.CaseElems[I].Rct.Left + 5 < X) and
        { except if ±5 from middle }
          (X < SwitchElement.CaseElems[I].Rct.Right - 5) then
        begin
          Current := FindElement(SwitchElement.CaseElems[I].Next.Next, X, Y);
          Break;
        end;
  end;
  if Assigned(Current) and BeforeOrAfter(Current) and
    (((Y > Current.Rct.Top + Current.GetMaxDelta) and
    (Y < Current.Rct.Bottom - Current.GetMaxDelta)) or
    ((Current is TStrAlgorithm) and (Y < Current.Rct.Top + Current.GetMaxDelta)))
  then
    Current := nil;
  Result := Current;
end;

function TFStructogram.BeforeOrAfter(Current: TStrElement): Boolean;
var
  TestElement: TStrElement;
begin
  if Current is TStrListHead then
    TestElement := Current.Next
  else
    TestElement := Current;
  Result := (TestElement.Kind <> Byte(nsStatement)) or (TestElement.Text <> '');
end;

function TFStructogram.FindVisibleElement(Current: TStrElement; X, Y: Integer)
  : TStrElement;
var
  IfElement: TStrIf;
  WhileElement: TStrWhile;
  DoWhileElement: TStrDoWhile;
  SwitchElement: TStrSwitch;
begin
  while Assigned(Current) and Assigned(Current.Next) and
    (Y > Current.Rct.Bottom) do
    Current := Current.Next;

  if Current is TStrIf then
  begin { search recursively inside IF }
    IfElement := (Current as TStrIf);
    if Y > Current.Rct.Top + Current.GetHeadHeight then
      if X <= IfElement.ThenElem.Rct.Right then
        Current := FindVisibleElement(IfElement.ThenElem.Next, X, Y)
      else
        Current := FindVisibleElement(IfElement.ElseElem.Next, X, Y);
  end
  else if Current is TStrDoWhile then
  begin { search recursively inside DO-WHILE }
    DoWhileElement := Current as TStrDoWhile;
    if (X > DoWhileElement.DoElem.Rct.Left) and
      (Y < DoWhileElement.DoElem.Rct.Bottom) then
      Current := FindVisibleElement(DoWhileElement.DoElem.Next, X, Y);
  end
  else if Current is TStrWhile then
  begin { search recursively inside WHILE/REPEAT/FOR(!) }
    WhileElement := Current as TStrWhile;
    if (X > WhileElement.DoElem.Rct.Left) and (Y > WhileElement.DoElem.Rct.Top)
    then
      Current := FindVisibleElement(WhileElement.DoElem.Next, X, Y);
  end
  else if Current is TStrSwitch then
  begin { search recursively inside SWITCH }
    SwitchElement := (Current as TStrSwitch);
    if Y > Current.Rct.Top + Current.GetHeadHeight then
      for var I := 0 to High(SwitchElement.CaseElems) do
        if X < SwitchElement.CaseElems[I].Rct.Right then
        begin { except if ±5 from middle }
          Current := FindVisibleElement(SwitchElement.CaseElems[I]
            .Next, X, Y);
          Break; // not next.next like in FindElement
        end;
  end;
  Result := Current;
end;

function TFStructogram.GetParentElement(Element: TStrElement): TStrElement;
begin
  var
  Tmp := Element;
  while Assigned(Tmp) and (Tmp.Prev <> nil) do
    Tmp := Tmp.Prev;
  if Assigned(Tmp) and (Tmp is TStrListHead) then
    Result := TStrListHead(Tmp).Parent
  else
    Result := nil;
end;

function TFStructogram.GetToken(var Str: string; var Token: string): Integer;
{ get next token in <S> and return in <Token>.
  Return type of token: -1=no token, 0=invalid, 1=name, 2=Char, 3=boolean, 4=integer,
  5=real, 6=Hex number, 7=string, 8=special Char.
}
var
  Chr: Char;
  Int: Integer;
  Hex: Boolean;

  procedure NextChr;
  begin
    Token[Int] := Chr;
    Inc(Int);
    if Int <= Length(Str) then
      Chr := Str[Int]
    else
      Chr := #0;
  end;

begin
  SetLength(Token, 255);
  Int := 1;
  while Int <= Length(Str) do
  begin { overread leading blanks and tabs }
    Chr := Str[Int];
    if (Chr <> ' ') and (Ord(Chr) <> 9) then
      Break;
    Inc(Int);
  end;
  Delete(Str, 1, Int - 1);
  if Str = '' then
  begin
    Token := '';
    Result := -1; { no token }
    Exit;
  end;
  Int := 1;
  if not IsWordBreakChar(Chr) and not CharInSet(Chr, ['0' .. '9']) then
  begin { name }
    repeat
      NextChr;
    until IsWordBreakChar(Chr) and not IsDigit(Chr) and (Chr <> '.') or
      (Int > Length(Str));
    Dec(Int);
    SetLength(Token, Int);
    if (UpperCase(Token) = 'TRUE') or (UpperCase(Token) = 'FALSE') then
      Result := 3 { boolean }
    else
      Result := 1; { name }
  end
  else if (Chr = '''') then
  begin { Char }
    NextChr;
    while (Chr <> '''') and (Chr >= ' ') and (Int <= Length(Str)) do
      NextChr;
    Token[Int] := '''';
    Result := 2; { Char }
  end
  else if (Chr = '"') then
  begin { string }
    NextChr;
    while (Chr <> '"') and (Chr >= ' ') and (Int <= Length(Str)) do
      NextChr;
    Token[Int] := '"';
    Result := 7; { String }
  end
  else
  begin
    if (Chr = '-') then
      NextChr;
    if ('0' <= Chr) and (Chr <= '9') then
    begin { number }
      Hex := False;
      while True do
      begin
        NextChr;
        if (Chr < '0') or (Int > Length(Str)) then
          Break;
        if (Chr > '9') then
        begin
          if ('A' <= UpperCase(Chr)) and (UpperCase(Chr) <= 'F') then
            Hex := True
          else
            Break;
        end;
      end;
      if (UpperCase(Chr) = 'H') then
      begin { Hex number }
        NextChr;
        Dec(Int);
        Result := 6; { Hex number }
      end
      else if (Chr = '.') then
      begin { real }
        NextChr;
        while ('0' <= Chr) and (Chr <= '9') do
          NextChr;
        if (Chr = 'E') then
          NextChr;
        if (Chr = '+') or (Chr = '-') then
          NextChr;
        while ('0' <= Chr) and (Chr <= '9') do
          NextChr;
        Dec(Int);
        if Hex then
          Result := 0 { invalid }
        else
          Result := 5; { real number }
      end
      else
      begin
        if Hex then
          Result := 0 { invalid }
        else
          Result := 4; { integer }
      end;
    end
    else
    begin
      NextChr;
      Dec(Int);
      Result := 8; { special Char }
    end;
  end;
  SetLength(Token, Int);
  Delete(Str, 1, Int);
end;

procedure TFStructogram.SetEvents(Image: TListImage);
begin
  with Image do
  begin
    OnDblClick := ImageDblClick;
    OnMouseDown := ImageMouseDown;
    OnMouseMove := ImageMouseMove;
    OnMouseUp := ImageMouseUp;
  end;
end;

function TFStructogram.GetAlgorithm: TStrAlgorithm;
begin
  Result := nil;
  for var I := 0 to ScrollBox.ControlCount - 1 do
  begin
    var
    AList := TListImage(ScrollBox.Controls[I]).StrList;
    if AList is TStrAlgorithm then
      Exit(AList as TStrAlgorithm);
  end;
end;

function TFStructogram.GetListAtScreenPos(Point: TPoint): TStrList;
begin
  Result := nil;
  for var I := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[I].BoundsRect.Contains(Point) then
      Exit(TListImage(ScrollBox.Controls[I]).StrList);
end;

function TFStructogram.GetCurList: Boolean;
begin
  var
  Point := ScrollBox.ScreenToClient(StructoPopupMenu.PopupPoint);
  FCurList := GetListAtScreenPos(Point);
  Result := Assigned(FCurList);
end;

function TFStructogram.GetCurListAndCurElement: Boolean;
begin
  FCurElement := nil;
  if GetCurList then
  begin
    var
    Point := FCurList.ListImage.ScreenToClient(StructoPopupMenu.PopupPoint);
    FCurElement := FindVisibleElement(FCurList, Point.X, Point.Y);
  end;
  Result := Assigned(FCurElement);
end;

function TFStructogram.GetList: TStrList;
begin
  if ScrollBox.ControlCount = 0 then
    Result := nil
  else
    Result := TListImage(ScrollBox.Controls[0]).StrList;
end;

function TFStructogram.GetStructogramAsBitmap: TBitmap;
var
  ARect: TRect;
  ABitmap: TBitmap;
begin
  for var I := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[I] is TListImage then
      TListImage(ScrollBox.Controls[I]).StrList.Paint(True);

  ARect := Rect(-1, -1, -1, -1);
  for var I := 0 to ScrollBox.ControlCount - 1 do
    if ARect.Left = -1 then
      ARect := TListImage(ScrollBox.Controls[I]).BoundsRect
    else
      ARect := UnionRect(ARect, TListImage(ScrollBox.Controls[I]).BoundsRect);
  ABitmap := TBitmap.Create;
  ABitmap.Width := ARect.Width + FConfiguration.StructogramShadowWidth;
  ABitmap.Height := ARect.Height + FConfiguration.StructogramShadowWidth;
  for var I := 0 to ScrollBox.ControlCount - 1 do begin
    var ListImage := TListImage(ScrollBox.Controls[I]);
    ABitmap.Canvas.Draw(ListImage.Left - ARect.Left, ListImage.Top - ARect.Top, ListImage.Picture.Graphic);
  end;
  Result := ABitmap;

  for var I := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[I] is TListImage then
      TListImage(ScrollBox.Controls[I]).StrList.Paint(False);
end;

procedure TFStructogram.PaintAll;
begin
  for var I := 0 to ScrollBox.ControlCount - 1 do
  begin
    var
    AList := (ScrollBox.Controls[I] as TListImage).StrList;
    if AList.Dirty then
      AList.Paint;
    AList.Dirty := False;
  end;
end;

procedure TFStructogram.Debug(const Str: string);
begin
  FMessages.OutputToTerminal(Str);
end;

function TFStructogram.FitsIn(ACurList: TStrList;
  ACurElement: TStrElement): Boolean;
var
  Width, Height: Integer;
begin
  if FPuzzleMode = 0 then
    Result := True
  else if (ACurElement is TStrListHead) or (ACurElement is TStrCase) or
    ((FPuzzleMode = 1) and (FCurInsert <> 0)) then
    Result := False
  else
  begin
    Width := ACurElement.Rct.Right - ACurElement.Rct.Left;
    Height := ACurElement.Rct.Bottom - ACurElement.Rct.Top;
    if FPuzzleMode = 1 then
      Result := (ACurList.RctList.Width = Width) and (ACurList.RctList.Height = Height)
    else if FPuzzleMode = 2 then
      Result := (ACurList.RctList.Width = Width)
    else
      Result := True;
  end;
end;

procedure TFStructogram.MakeVeryHard;
begin
  for var I := 0 to ScrollBox.ControlCount - 1 do
  begin
    var
    AList := TListImage(ScrollBox.Controls[I]).StrList;
    AList.Collapse;
    if AList.Next = nil then
      AList.Destroy
    else
    begin
      AList.ResizeAll;
      AList.Paint;
    end;
  end;
end;

procedure TFStructogram.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    ToolBarStructogram.Images := vilToolbarDark;
    TrashImage.ImageIndex := 29;
    StructoPopupMenu.Images := vilPopupMenuDark;
  end
  else
  begin
    ToolBarStructogram.Images := vilToolbarLight;
    TrashImage.ImageIndex := 14;
    StructoPopupMenu.Images := vilPopupMenuLight;
  end;
  for var I := 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[I] is TListImage then
      TListImage(ScrollBox.Controls[I]).StrList.Paint;
end;

procedure TFStructogram.DPIChanged;
begin
  SetFontSize(0);
  Hide;
  Show;
end;

end.
