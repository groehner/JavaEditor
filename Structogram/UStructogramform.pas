{-------------------------------------------------------------------------------
 Unit:     UStructogramform
 Author:   Gerhard Röhner
 Based on: NSD-Editor by Marcel Kalt
 Date:     August 2013
 Purpose:  structogram editor
-------------------------------------------------------------------------------}

unit UStructogramform;

interface

uses
  Windows,
  Classes, Controls, ExtCtrls, Forms, Graphics, ComCtrls, Menus, StdCtrls,
  UTypes, UBaseForm, System.ImageList, Vcl.ImgList, Vcl.ToolWin,
  Vcl.BaseImageCollection, SVGIconImageCollection, Vcl.VirtualImageList,
  SVGIconImage, Vcl.VirtualImage, TB2Item, SpTBXItem, TB2Dock, TB2Toolbar;

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
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScrollBoxClick(Sender: TObject);
    procedure ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    EditMemo: TMemo;
    oldShape: TRect;
    oldCanvas: TCanvas;
    curElement: TStrElement;
    curList: TStrList;
    EditMemoElement: TStrElement;
    EditMemoBeginText: string;
    CurInsert: integer;
    IsMoving: boolean;
    ReadOnly: boolean;
    MousePos: TPoint;
    ScreenMousePos: TPoint;
    DataType: string;
    Variables: string;
    ControlCanvas: TCanvas;
    IgnoreNextMouseDown: boolean;
    IgnoreNextMouseUp: boolean;
    Separating: integer;
    PuzzleMode: integer;
    Solution: string;
    Version: Byte;
    procedure ShowShape;
    procedure HideShape;
    procedure AddParameter(list: TStrList; paramList: TStringList);
    procedure AddVariable(aText: string; list: TStringList);
    procedure StrElementToJava(element: TStrElement; pList, VariablesList: TStringList; const indent: string);
    function FindElement(current: TStrElement; X, Y: Integer): TStrElement;
    function FindVisibleElement(current: TStrElement; X, Y: Integer): TStrElement;
    function BeforeOrAfter(current: TStrElement): boolean;
    function getParentElement(element: TStrElement): TStrElement;
    function getToken(var S: string; var Token: string): Integer;
    procedure CalculateInsertionShape(DestList, InsertList: TStrList; x, y: integer);
    procedure InsertElement(DestList, InsertList: TStrList; aCurElement: TStrElement);
    procedure setEvents(Image: TListImage);
    procedure DoEdit(StrElement: TStrElement; s: string);
    procedure CloseEdit(b: boolean);
    procedure setLeftBorderForEditMemo;
    function getAlgorithmParameter(ParamList: TStringList): string;
    procedure JavaProgram(list: TStrList; progList: TStringList; const aName: string);
    function getAlgorithm: TStrAlgorithm;
    function getList: TStrList;
    function getListAtScreenPos(Pt: TPoint): TStrList;
    function getCurList: boolean;
    function getCurListAndCurElement: boolean;
    function getBitmap: TBitmap;
    function getName(StrList: TStrList): string;
    procedure PaintAll;
    function fitsIn(aCurList: TStrList; aCurElement: TStrElement): boolean;
    procedure SetPuzzleMode(Mode: Integer);
    procedure MakeVeryHard;
    function StringFromStream(Stream: TStream): string;
    function IntegerFromStream(stream: Tstream): integer;
  protected
    function LoadFromFile(const FileName: string): boolean;
    function LoadFromFileOld(FileName: string): boolean;
    procedure SaveToFile(const FileName: string);
    procedure UpdateState; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure New;
    function Open(const aPathname: string): boolean;
    procedure Save(MitBackup: boolean); override;
    procedure Print; override;
    procedure FromText(const s: string);
    procedure RenewFromText(const s: string);
    function getFormType: string; override;
    procedure SetOptions; override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure PasteFromClipboard; override;
    procedure SetFont(aFont: TFont); override;
    procedure Enter(Sender: TObject); override;
    function GetSaveAsName: string; override;
    procedure DoExport; override;
    procedure debug(const s: string);
    procedure ChangeStyle; override;
    procedure DPIChanged; override;
  end;

implementation

{$R *.DFM}

uses Messages, Buttons, SysUtils, Math, Clipbrd, Dialogs,
     Themes, Types, UITypes, IOUtils, StrUtils, JvGnugettext, UStringRessources,
     UJava, UConfiguration, UUtils, UGenerateStructogram, UMessages, UTemplates;

constructor TFStructogram.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 11;
end;

procedure TFStructogram.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  oldShape:= Rect(-1, -1, -1, -1);
  EditMemo:= TMemo.Create(Self);
  EditMemo.Parent:= Self;
  EditMemo.SetBounds(136, 156, 99, 37);
  EditMemo.Color:= StyleServices.GetSystemColor(clSkyBlue);
  EditMemo.OnChange:= EditMemoChange;
  EditMemo.Visible:= false;
  EditMemo.WordWrap:= false;
  EditMemo.BevelInner:= bvNone;

  OnMouseActivate:= FormMouseActivate;
  ControlCanvas:= TControlCanvas.Create;
  TControlCanvas(ControlCanvas).Control:= Scrollbox;
  Font.assign(FConfiguration.StructogramFont);
  UpdateState;
  ScrollBox.DoubleBuffered:= true;
  Separating:= 0;
  PuzzleMode:= 0;
  TBPuzzleMode.Visible:= false;
  ToolBarStructogram.Height:= 308 - 22;
  Version:= $0E;
  curList:= nil;
  curElement:= nil;
  setOptions;
end;

procedure TFStructogram.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified and not AlreadySavedAs then begin
    FJava.DoSave(Self, true);
    AlreadySavedAs:= true;
  end;
  CanClose:= True;
end;

procedure TFStructogram.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  for var i:= Scrollbox.ControlCount - 1 downto 0 do
    TListImage(ScrollBox.Controls[i]).StrList.Destroy;
  FreeAndNil(ControlCanvas);
  Action:= caFree;
end;

procedure TFStructogram.New;
  var elem: TStrElement;
      StrList: TStrAlgorithm;
begin
  if Pathname = '' then Pathname:= FJava.getFilename('.jsg');
  Caption:= Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  StrList:= TStrAlgorithm.create(ScrollBox, PuzzleMode, Font);
  StrList.text:= FConfiguration.Algorithm + ' ' + ChangeFileExt(ExtractFilename(Pathname), '');
  elem:= TStrStatement.create(StrList);
  StrList.insert(StrList, elem);
  setEvents(StrList.Image);
  StrList.ResizeAll;
  StrList.Paint;
  Modified:= false;
  Enter(Self); // must stay!
  if canFocus then SetFocus;
end;

procedure TFStructogram.FromText(const s: string);
  var Generator: TGenerateStructogram;
      StrList: TStrAlgorithm;
begin
  if Pathname = '' then Pathname:= FJava.getFilename('.jsg');
  Caption:= Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  StrList:= TStrAlgorithm.create(ScrollBox, PuzzleMode, Font);
  StrList.Text:= FConfiguration.Algorithm + ' ';
  Generator:= TGenerateStructogram.Create(true);
  try
    Generator.GenerateStructogram(s, StrList);
  finally
    FreeAndNil(Generator);
  end;
  setEvents(StrList.Image);
  StrList.ResizeAll;
  StrList.Paint;
  Save(false);
  Enter(Self); // must stay!
  if canFocus then SetFocus;
end;

procedure TFStructogram.RenewFromText(const s: string);
  var Generator: TGenerateStructogram;
      StrList: TStrList; i: integer;
      Alg: string;
begin
  for i:= ScrollBox.ControlCount - 1 downTo 0 do begin
    Strlist:= TListImage(ScrollBox.Controls[i]).StrList;
    if StrList is TStrAlgorithm then Alg:= Strlist.text;
    FreeAndNil(Strlist);
  end;

  StrList:= TStrAlgorithm.create(ScrollBox, PuzzleMode, Font);
  StrList.Text:= Alg;
  setEvents(StrList.Image);
  StrList.Text:= FConfiguration.Algorithm + ' ';
  Generator:= TGenerateStructogram.Create(true);
  try
    Generator.GenerateStructogram(s, StrList);
  finally
    FreeAndNil(Generator);
  end;
  StrList.ResizeAll;
  StrList.Paint;
  Save(false);
  Enter(Self); // must stay!
  if canFocus then SetFocus;
end;

function TFStructogram.Open(const aPathname: string): boolean;
begin
  CloseEdit(true);
  Result:= LoadFromFile(aPathname);
  if Result then begin
    Self.Pathname:= aPathname;
    Caption:= aPathname;
    FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
    Modified:= false;
    Enter(Self); // must stay!
    if canFocus then SetFocus;
    ReadOnly:= IsWriteProtected(aPathName);
    UpdateState;
  end;;
end;

procedure TFStructogram.enter(Sender: TObject);
begin
  inherited;
  if ToolBarStructogram.Visible and ToolBarStructogram.CanFocus then
    ToolBarStructogram.SetFocus;
end;

function TFStructogram.LoadFromFile(const FileName: string): boolean;
var
  StrList: TStrList;
  SwitchWithCaseLine: boolean;
  Reader: TStringListReader;

  procedure Init(aList: TStrList);
  begin
    aList.SwitchWithCaseLine:= SwitchWithCaseLine;
    aList.setPuzzleMode(PuzzleMode);
    aList.setFont(Font);
    aList.setLineHeight;
    setEvents(aList.Image);
    aList.ResizeAll;
    aList.Paint;
  end;

begin
  Result:= true;
  try
    Reader:= TStringListReader.create(Filename);
    try
      if Reader.hasOldFormat then
        LoadFromFileOld(Filename)
      else begin
        repeat
          Reader.ReadLine;
          if Reader.key = 'FontName' then
            Font.Name:= Reader.val
          else if Reader.key = 'FontSize' then
            Font.Size:= PPIScale(StrToInt(Reader.val))
          else if Reader.key = 'FontColor' then
            Font.Color:= StrToInt(Reader.val)
          else if Reader.key = 'FontBold' then
            if Reader.val = 'true'
              then Font.Style:= Font.Style + [fsBold]
              else Font.Style:= Font.Style - [fsBold]
          else if Reader.key = 'FontItalic' then
            if Reader.val = 'true'
              then Font.Style:= Font.Style + [fsItalic]
              else Font.Style:= Font.Style - [fsItalic]
          else if Reader.key = 'PuzzleMode' then
            PuzzleMode:= StrToInt(Reader.val)
          else if Reader.key = 'Solution' then
            Solution:= UnhideCrLf(Reader.val);
        until (Reader.key = '- Kind') or (Reader.key = '');

        while (Reader.key = '- Kind') and Result do begin
          if Reader.val = 'Algorithm'
            then StrList:= TStrAlgorithm.Create(ScrollBox, PuzzleMode, Font)
            else StrList:= TStrList.create(Scrollbox, PuzzleMode, Font);
          Reader.ReadLine;
          if Reader.key = 'SwitchWithCaseLine'
            then SwitchWithCaseLine:= (Reader.val = 'true')
            else Reader.LineBack;
          StrList.LoadFromReader(Reader);
          Init(StrList);
          Result:= Result and not StrList.LoadError;
          Reader.ReadLine;
        end;
        FConfiguration.StructogramFont.assign(Font);
      end;
    finally
      FreeAndNil(Reader);
    end
  except
    on e: exception do begin
      ErrorMsg(e.Message);
      Result:= false;
    end;
  end;
  Version:= $0F;
end;

function TFStructogram.LoadFromFileOld(FileName: string): boolean;
var
  sigName: ShortString;
  fontName: ShortString;
  Style: Byte;
  stream: TFileStream;
  leftMargin,
  topMargin,
  zoomfactor: SmallInt;
  kind: Byte;
  StrList: TStrList;
  StructogramAligned: boolean;
  SwitchWithCaseLine: boolean;

  procedure Init(aList: TStrList);
  begin
    aList.SwitchWithCaseLine:= SwitchWithCaseLine;
    aList.Image.Left:= leftMargin;
    aList.Image.Top:= topMargin;
    aList.setPuzzleMode(PuzzleMode);
    aList.setFont(Font);
    aList.setLineHeight;
    setEvents(aList.Image);
    aList.ResizeAll;
    aList.Paint;
  end;

begin
  Result:= true;
  try
    stream:= TFileStream.Create(FileName, fmOpenRead);
    try
      stream.Read(sigName, 4);                               {read signature}
      if not ((sigName = 'JSG') or (sigName = 'NSD')) then
        ErrorMsg(_('Invalid file format!'))
      else begin
        stream.Read(Version, 1);                             {read version}

        if $0E <= Version  then begin
          stream.read(PuzzleMode, 1);
          setPuzzleMode(PuzzleMode);
          Solution:= StringFromStream(stream);
          Font.Name:= StringFromStream(stream);
          if Font.Name = 'Arial' then Font.Name:= 'Segoe UI';
        end;

        if $0C = Version then begin
          stream.read(StructogramAligned, 1);
          stream.read(SwitchWithCaseLine, 1);
        end;

        if ($0B <= Version) and (Version <= $0C) then begin
          stream.Read(leftMargin, 2);          {read left margin}
          stream.Read(topMargin, 2);           {read top margin}
          stream.Read(zoomFactor, 2);          {read zoom factor}
        end;

        if ($0A <= Version) and (Version < $0E) then begin
          stream.Read(fontName[0], 1);
          stream.Read(fontName[1], ord(fontName[0]));          {read font name}
          Font.Name:= string(fontName);
          if Font.Name = 'Arial' then Font.Name:= 'Segoe UI';
        end;

        if ($0A <= Version) then begin
          Font.Size:= IntegerFromStream(stream);
          stream.Read(Style, 1);
          Font.Style:= [];
          if (Style and $01) > 0 then Font.Style:= Font.Style + [fsBold];
          if (Style and $02) > 0 then Font.Style:= Font.Style + [fsItalic];
          Font.Color:= IntegerFromStream(stream);

          if Version < $0D then begin
            StrList:= TStrAlgorithm.Create(ScrollBox, PuzzleMode, Font);
            StrList.LoadFromStream(stream, Version);
            Init(strList);
            Result:= Result and not StrList.LoadError;
          end else
            while (stream.Read(Kind, 1) = 1) and Result do begin
              if Kind = byte(nsAlgorithm)
                then StrList:= TStrAlgorithm.Create(ScrollBox, PuzzleMode, Font)
                else StrList:= TStrList.create(Scrollbox, PuzzleMode, Font);
              stream.Read(SwitchWithCaseLine, 1);
              stream.Read(leftMargin, 2);
              stream.Read(topMargin, 2);
              StrList.LoadFromStream(stream, Version);
              Init(StrList);
              Result:= Result and not StrList.LoadError;
            end;
          FConfiguration.StructogramFont.assign(Font);
        end;
      end;
    finally
      FreeAndNil(stream);
    end
  except
    on e: exception do begin
      ErrorMsg(e.Message);
      Result:= false;
    end;
  end;
  Version:= $0E;
end;

procedure TFStructogram.SaveToFile(const FileName: string);
  var JSGFile: TextFile;
      aList: TStrList;
begin
  try
    try
      AssignFile(JSGFile, FileName);
      Rewrite(JSGFile);
      Writeln(JSGFile, 'JSG: true');
      if Font.Name = 'Arial' then Font.Name:= 'Segoe UI';
      Writeln(JSGFile, 'FontName: ' + Font.Name);
      Writeln(JSGFile, 'FontSize: ' + IntToStr(PPIUnscale(Font.Size)));
      if fsBold in Font.Style then
        Writeln(JSGFile, 'FontBold: true');
      if fsItalic in Font.Style then
        Writeln(JSGFile, 'FontItalic: true');
      Writeln(JSGFile, 'FontColor: ' + IntToStr(Font.Color));
      if PuzzleMode > 0 then begin
        Writeln(JSGFile, 'PuzzleMode: ' + IntToStr(PuzzleMode));
        Writeln(JSGFile, 'Solution: ' + HideCrLf(Solution));
      end;

      for var i:= 0 to ScrollBox.ControlCount - 1 do begin
        aList:= (ScrollBox.Controls[i] as TListImage).StrList;
        Writeln(JSGFile, '- Kind: ' + aList.getKind);
        Writeln(JSGFile, '  SwitchWithCaseLine: ' + BoolToStr(aList.SwitchWithCaseLine, true));
        Writeln(JSGFile, '  RectPos' + aList.getRectPos('  '));
        Write(JSGFile, aList.getText('  '));
      end;
    finally
      CloseFile(JSGFile);
    end
  except
    on e: exception do
      ErrorMsg(e.Message);
  end;
end;

function TFStructogram.StringFromStream(Stream: TStream): string;
  var Size: LongInt;
begin
  stream.read(Size, SizeOf(Size));
  setLength(Result, Size div SizeOf(Char));
  stream.read(Pointer(Result)^, Size);
end;

function TFStructogram.IntegerFromStream(stream: TStream): integer;
begin
  stream.read(Result, SizeOf(Integer));
end;

procedure TFStructogram.ShowShape;
begin
  with OldCanvas, OldShape do begin
    Pen.Color:= clRed; //StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
    if Top = Bottom then begin
      MoveTo(Left+1, Top-2);
      LineTo(Right, Top-2);
      MoveTo(Left+1, Top-1);
      LineTo(Right, Top-1);
      MoveTo(Left+1, Top+1);
      LineTo(Right, Top+1);
      MoveTo(Left+1, Top+2);
      LineTo(Right, Top+2);
    end else begin
      MoveTo(Left, Top);
      LineTo(Right, Top);
      LineTo(Right, Bottom);
      LineTo(Left,  Bottom);
      LineTo(Left, Top);
      MoveTo(Left+1, Top+1);
      LineTo(Right-1, Top+1);
      LineTo(Right-1, Bottom-1);
      LineTo(Left+1,  Bottom-1);
      LineTo(Left+1, Top+1);
    end;
  end;
end;


procedure TFStructogram.HideShape;
begin
  if oldShape.top > -1 then begin
    with OldCanvas, OldShape do begin
      Pen.Color:= StyleServices.GetStyleColor(scPanel);
      if Top = Bottom then begin
        MoveTo(Left+1, Top-2);
        LineTo(Right, Top-2);
        MoveTo(Left+1, Top-1);
        LineTo(Right, Top-1);
        MoveTo(Left+1, Top+1);
        LineTo(Right, Top+1);
        MoveTo(Left+1, Top+2);
        LineTo(Right, Top+2);
      end else begin
        MoveTo(Left, Top);
        LineTo(Right, Top);
        LineTo(Right, Bottom);
        LineTo(Left,  Bottom);
        LineTo(Left, Top);
        MoveTo(Left+1, Top+1);
        LineTo(Right-1, Top+1);
        LineTo(Right-1, Bottom-1);
        LineTo(Left+1,  Bottom-1);
        LineTo(Left+1, Top+1);
      end;
    end;
    oldShape:= Rect(-1, -1, -1, -1);
    PaintAll;
  end;
end;

// used by buttons
procedure TFStructogram.StrElementMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var StrList: TStrList; elem: TStrElement;
      PtScreen, PtClient: TPoint;
begin
  if Button = mbLeft then begin
    CloseEdit(true);
    if TSpeedButton(Sender).Tag = 0
      then StrList:= TStrAlgorithm.create(ScrollBox, PuzzleMode, Font)
      else StrList:= TStrList.create(Scrollbox, PuzzleMode, Font);
    elem:= nil;
    case TSpeedButton(Sender).Tag of
      ord(nsAlgorithm):  begin
                           elem:= TStrStatement.Create(StrList);
                           StrList.text:= FConfiguration.Algorithm + ' ';
                         end;
      ord(nsStatement):  elem:= TStrStatement.Create(StrList);
      ord(nsIf):         elem:= TStrIf.Create(StrList);
      ord(nsWhile):      begin
                           elem:= TStrWhile.Create(StrList);
                           elem.text:= FConfiguration._While + ' ';
                         end;
      ord(nsDoWhile):    begin
                           elem:= TStrDoWhile.Create(StrList);
                           elem.text:= FConfiguration.DoWhile + ' ';
                         end;
      ord(nsFor):        begin
                           elem:= TStrFor.Create(StrList);
                           elem.text:= ReplaceStr(ReplaceStr(FConfiguration._For, '[', ''), ']', '') + ' ';
                         end;
      ord(nsSwitch):     elem:= TStrSwitch.Create(StrList);
      ord(nsSubprogram): elem:= TStrSubProgram.Create(StrList);
      ord(nsBreak):      elem:= TStrBreak.create(StrList);
    end;
    StrList.insert(StrList, elem);
    StrList.setFont(Font);
    StrList.ResizeAll;
    PtScreen:= (Sender as TToolButton).ClientToScreen(Point(X, Y));
    PtClient:= ToolBarStructogram.ScreenToClient(PtScreen);
    StrList.Image.SetBounds(PtClient.X - StrList.rctList.Width div 2,
                            PtClient.y - StrList.LineHeight div 2,
                            StrList.rctList.Width, StrList.rctList.Height);
    setEvents(StrList.Image);
    StrList.Paint;
    mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
    if TSpeedButton(Sender).Tag = ord(nsDoWhile)
      then SetCursorPos(Mouse.CursorPos.X + 30, Mouse.CursorPos.Y + StrList.LineHeight)
      else SetCursorPos(Mouse.CursorPos.X + 30, Mouse.CursorPos.Y);
    mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  end;
end;

procedure TFStructogram.ImageDblClick(Sender: TObject);
begin
  curList:= (Sender as TListImage).StrList;
  curElement:= FindVisibleElement(curList, MousePos.X, MousePos.Y);
  DoEdit(CurElement, '');
  IgnoreNextMouseDown:= true;
end;

procedure TFStructogram.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ignoreNextMouseDown then begin
    ignoreNextMouseDown:= false;
    exit;
  end;
  if IsMoving then exit;
  var Image:= (Sender as TListImage);
  Image.BringToFront;
  curList:= Image.StrList;
  MousePos:= Point(X, Y);
  ScreenMousePos:= Image.ClientToScreen(Point(X, Y));
  if EditMemo.Visible then
    CloseEdit(True);
  curElement:= FindVisibleElement(curList, MousePos.X, MousePos.Y);
  if (Separating = 0) and assigned(curElement) and assigned(curElement.prev) then
    if (curElement.prev is TStrAlgorithm) or
       not ((curElement.prev is TStrList) or (curElement is TStrCase)) then
      Separating:= 1;
  UpdateState;
end;

// mouse move over a structogram
procedure TFStructogram.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ScrollBoxPt, Image2Pt, ScreenPt: TPoint;
  dx, dy, dx1, dy1, i: Integer;
  Image, Image2: TListImage;
  StrList: TStrList; StrStatement: TStrStatement;
  aRect: TRect;
begin
  HideShape;
  if EditMemo.Visible then exit;
  inherited;

  if ssLeft in Shift then begin
    Image:= (Sender as TListImage);
    curList:= Image.StrList;
    // start moving
    ScreenPt:= Image.ClientToScreen(Point(X, Y));
    ScrollBoxPt:= ScrollBox.ScreenToClient(ScreenPt);
    dx:= ScreenPt.x - ScreenMousePos.x;
    dy:= ScreenPt.y - ScreenMousePos.y;
    if (dx = 0) and (dy = 0) then exit;

    if (Abs(dx) + Abs(dy) > 5) or IsMoving then begin  // move Image
      IsMoving:= true;
      Modified:= true;
      ScreenMousePos:= ScreenPt;
      curElement:= FindVisibleElement(curList, X - dx, Y - dy);

      // if assigned(curElement) then curElement.debug;

      if assigned(curElement) and (Separating = 1) then begin
        dx1:= X - curElement.rct.Left;
        dy1:= Y - curelement.rct.Top;
        if curElement.prev = nil then
          exit;
        aRect:= curElement.rct;
        if (curElement.prev is TStrListHead) or (curElement.prev is TStrCase) or
           (Puzzlemode = 1)
        then begin
          StrStatement:= TStrStatement.create(curList);
          StrStatement.rct:= curElement.rct;
          ControlCanvas.Font.Assign(Font);
          curList.insert(curElement.prev, StrStatement);
        end;

        if PuzzleMode = 1 then begin // handle single statements, no lists
          curElement.prev.next:= curElement.next;
          if assigned(curElement.next) then
            curElement.next.prev:= curElement.prev;
          curElement.prev:= nil;
          curElement.next:= nil;
        end else begin
          curElement.prev.next:= nil;
          curElement.prev:= nil;
        end;
        curList.ResizeAll;
        curList.Paint;

        // create new list
        StrList:= TStrList.create(ScrollBox, PuzzleMode, Font);
        StrList.setFont(Font);
        StrList.insert(StrList, curElement);
        StrList.setList(StrList);
        setEvents(StrList.Image);
        StrList.ResizeAll;
        StrList.rctList.Right:= max(aRect.Right - aRect.Left, StrList.rctList.Right);
        StrList.rctList.Bottom:= max(aRect.Bottom - aRect.Top, strList.rctList.Bottom);
        StrList.rct:= StrList.rctList;
        StrList.Image.Left:= Image.Left + aRect.Left + dy;
        StrList.Image.Top := Image.Top + aRect.Top + dx;
        StrList.Paint;
        Separating:= 2;
        CurList.DontMove:= true;
        ignoreNextMouseUp:= true;
        isMoving:= false;
        ScreenPt:= StrList.Image.ClientToScreen(Point(dx1, dy1));
        SetCursorPos(ScreenPt.x, Screenpt.y);
        mouse_event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
        mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
        CurList:= StrList;
      end else begin
        // crucial for switching mouse to new image
        Image.SetBounds(Image.Left + dx, Image.Top + dy, Image.Width, Image.Height);
        if curList.DontMove then begin
          Image.SetBounds(Image.Left - dx, Image.Top - dy, Image.Width, Image.Height);
          curList.DontMove:= false;
        end;
        if curList.Kind <> byte(nsAlgorithm) then begin
          for i:= ScrollBox.ControlCount - 1 downto 0 do begin
            Image2:= TListImage(ScrollBox.Controls[i]);
            if Image2 <> Image then begin
              aRect:= Image2.BoundsRect;
              aRect.Inflate(20, 20);
              if aRect.Contains(ScrollBoxPt) then begin
                Image2Pt:= Image2.ScreenToClient(ScreenPt);
                curElement:= FindElement(Image2.StrList, Image2Pt.X, Image2Pt.Y);
                CalculateInsertionShape(Image2.StrList, curList, Image2Pt.X, Image2Pt.Y);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFStructogram.CalculateInsertionShape(DestList, InsertList: TStrList; x, y: integer);
  var dy: integer;
begin
  // curElement.debug()
  if curElement = nil then exit;

  with curElement do
    if not EqualRect(oldShape, Rect(rct.bottom, rct.left, rct.bottom, rct.right)) then begin
      if BeforeOrAfter(curElement) then begin
        if Y < curElement.rct.Top + curElement.getMaxDelta then begin
          oldShape:= Rect(rct.left, rct.top, rct.right, rct.top);
          CurInsert:= -1;  // insert before
        end else if Y > curElement.rct.Bottom - curElement.getMaxDelta then begin
          if CurElement = List then
            oldShape:= Rect(rct.left, rct.bottom, List.rctList.right, rct.bottom)
          else
            oldShape:= Rect(rct.left, rct.bottom, rct.right, rct.bottom);
          CurInsert:= +1;  // insert after
        end
      end else begin
        dy:= (curElement.rct.Bottom - curElement.rct.Top) div 4;
        if Y < curElement.rct.Top + dy then begin
          oldShape:= Rect(rct.left, rct.top, rct.right, rct.top);
          CurInsert:= -1;
        end else if Y < curElement.rct.Bottom - dy then begin
          oldShape:= Rect(rct.left + 1, rct.top + 1, rct.right - 1, rct.bottom - 1);
          CurInsert:= 0;
        end else begin
          oldShape:= Rect(rct.left, rct.bottom, rct.right, rct.bottom);
          CurInsert:= +1;
        end;
      end;
      OldCanvas:= DestList.Image.Canvas;
      if FitsIn(InsertList, curElement) then begin
        ShowShape;
        DestList.dirty:= true;
      end;
    end;
end;

procedure TFStructogram.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var ScreenPt, q, ScrollBoxPt, Image2Pt: TPoint;
      Image, Image2: TListImage;
      StrList: TStrList;
      i: integer;
      aRect: TRect;
begin
  IsMoving:= false;
  Image:= (Sender as TListImage);
  StrList:= Image.StrList;

  if ignoreNextMouseUp then begin
    ignoreNextMouseUp:= false;
    exit;
  end;

  aRect:= StrList.rctList;
  StrList.ResizeAll;
  if aRect <> StrList.rctList then
    StrList.Paint;
  if StrList.Kind in [byte(nsAlgorithm), byte(nsList)] then begin
    ScreenPt:= Image.ClientToScreen(Point(X, Y));
    q:= Self.ScreenToClient(ScreenPt);
    if q.x < ScrollBox.Left then begin
       FreeAndNil(StrList);
       exit;
    end;
    if q.y < ScrollBox.Top then
      Image.Top:= 0;
  end;
  if StrList.Kind = byte(nsList) then begin
    ScrollBoxPt:= ScrollBox.ScreenToClient(ScreenPt);
    for i:= ScrollBox.ControlCount - 1 downto 0 do begin
      Image2:= TListImage(ScrollBox.Controls[i]);
      aRect:= Image2.BoundsRect;
      aRect.Inflate(30, 30);
      if (Image2 <> Image) and aRect.Contains(ScrollBoxPt) then begin
        Image2Pt:= Image2.ScreenToClient(ScreenPt);
        curElement:= FindElement(Image2.StrList, Image2Pt.X , Image2Pt.Y);
        if assigned(curElement) and FitsIn(StrList, curElement) then
          InsertElement(Image2.StrList, StrList, curElement);
      end;
    end;
  end;
  Separating:= 0;
  PaintAll;
end;

// dropped on a structogram, insert element
procedure TFStructogram.InsertElement(DestList, InsertList: TStrList; aCurElement: TStrElement);
  var elems, aPrevious: TStrElement;

  procedure Inserted;
  begin
    DestList.ResizeAll;
    DestList.Paint;
    Modified:= true;
    InsertList.next:= nil;
    FreeAndNil(InsertList);
  end;

begin
  HideShape;
  if assigned(OldCanvas) then
    OldCanvas.Pen.Mode:= pmCopy;
  InsertList.setList(DestList);
  elems:= InsertList.next;
  if elems <> nil then begin
    if PuzzleMode = 1 then begin // replace empty statement
      DestList.Insert(aCurElement, elems);
      DestList.deleteElem(aCurElement);
    end else begin
      if (CurInsert = 1) or (aCurElement.prev = nil) then
        DestList.Insert(aCurElement, elems)
      else if CurInsert = 0 then begin
        DestList.Insert(aCurElement, elems);
        DestList.deleteElem(aCurElement);
      end else begin
        aPrevious:= aCurElement.prev;
        DestList.insert(aPrevious, elems);
      end;
    end;
    Inserted;
  end;
  isMoving:= false;
end;

procedure TFStructogram.MIGenerateMethodClick(Sender: TObject);
begin
  CloseEdit(true);
  FConfiguration.GenerateJavaAsProgram:= 0;
  getCurList;
  BBGenerateJavaClick(Self);
end;

procedure TFStructogram.MIGenerateProgramClick(Sender: TObject);
begin
  CloseEdit(true);
  FConfiguration.GenerateJavaAsProgram:= 1;
  getCurList;
  BBGenerateJavaClick(Self);
end;

procedure TFStructogram.MIPuzzleClick(Sender: TObject);
begin
  if PuzzleMode > 0 then begin
    setPuzzleMode(0);
    Solution:= '';
  end else begin
    if not ((ScrollBox.ControlCount = 1) and (TListImage(Scrollbox.Controls[0]).StrList is TStrAlgorithm)) then begin
      ShowMessage(_('Switch from a single algorithm structogram as the final solution to puzzle mode.'));
      exit;
    end;
    setPuzzleMode(1);
    var StrList:= TListImage(ScrollBox.Controls[0]).StrList;
    StrList.setPuzzleMode(1);
    Solution:= StrList.asString;
  end;
end;

procedure TFStructogram.SetPuzzleMode(Mode: Integer);
begin
  PuzzleMode:= Mode;
  MIPuzzleMode.Checked:= (PuzzleMode = 1);
  TBPuzzleMode.Visible:= (PuzzleMode = 1);
end;

procedure TFStructogram.MISwitchWithCaseLineClick(Sender: TObject);
begin
  if not ReadOnly then begin
    for var i:= 0 to ScrollBox.ControlCount -1 do begin
      var StrList:= TListImage(Scrollbox.Controls[i]).StrList;
      StrList.SwitchWithCaseLine:= not StrList.SwitchWithCaseLine;
      StrList.Paint;
    end;
    UpdateState;
  end;
end;

procedure TFStructogram.MIAddCaseClick(Sender: TObject);
begin
  CloseEdit(true);
  if getCurListAndCurElement and (curElement is TStrSwitch) then begin
    var Switch:= (curElement as TStrSwitch);
    var i:= Length(Switch.case_elems);
    setLength(Switch.case_elems, i+1);
    Switch.case_elems[i]:= Switch.case_elems[i-1];
    Switch.case_elems[i-1]:= TStrListHead.create(curList, Switch);
    curList.ResizeAll;
    curList.Paint;
    Modified:= true;
    UpdateState;
  end;
end;

procedure TFStructogram.MIDeleteCaseClick(Sender: TObject);
  var x, i, k, n: integer;
begin
  CloseEdit(true);
  if getCurListAndCurElement and (curElement is TStrSwitch) then begin
    var Switch:= (curElement as TStrSwitch);
    x:= MousePos.X;
    i:= 0;
    while (i < Length(Switch.case_elems)) and (x > Switch.case_elems[i].rct.Right) do
      inc(i);
    FreeAndNil(Switch.case_elems[i]);
    n:= high(Switch.case_elems);
    for k:= i to n - 1 do
      Switch.case_elems[k]:= Switch.case_elems[k+1];
    setLength(Switch.case_elems, n);
    curList.ResizeAll;
    curList.Paint;
    Modified:= true;
    UpdateState;
  end;
end;

procedure TFStructogram.MIDeleteClick(Sender: TObject);
begin
  if getCurListAndCurElement then begin
    if (curElement.Kind = byte(nsAlgorithm)) or
      (assigned(curElement.prev) and (curElement.prev.Kind = byte(nsList))) then
      FreeAndNil(curList)
    else begin
      curList.deleteElem(curElement);
      curList.ResizeAll;
      curList.Paint;
    end;
    Modified:= true;
    UpdateState;
  end;
end;

procedure TFStructogram.MICopyClick(Sender: TObject);
  var StrList: TStrList;
      Stream: TMemoryStream;
begin
  if getCurList then begin
    Stream:= TMemoryStream.Create();
    try
      curList.SaveToStream(Stream);
      if curList is TStrAlgorithm
        then StrList:= TStrAlgorithm.Create(ScrollBox, PuzzleMode, Font)
        else StrList:= TStrList.create(Scrollbox, PuzzleMode, Font);
      Stream.Position:= 0;
      StrList.LoadFromStream(stream, Version);
      StrList.setFont(Font);
      StrList.ResizeAll;
      StrList.Image.SetBounds(curList.Image.Left + 30, curList.Image.Top + 30,
                              curList.Image.Width, curList.Image.Height);
      setEvents(StrList.Image);
      StrList.setList(StrList);
      StrList.Paint;
      Modified:= true;
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

procedure TFStructogram.DoEdit(StrElement: TStrElement; s: string);
  var le, aTop, wi, he: integer;
      Image: TListImage;
begin
  if Assigned(StrElement) and not ReadOnly then begin
    EditMemoElement:= StrElement;
    Image:= StrElement.List.Image;
    EditMemo.Text:= StrElement.text + s;
    EditMemoBeginText:= StrElement.text;
    le:= ToolBarStructogram.Width + Image.Left + StrElement.rct.Left;
    aTop:= Image.Top + StrElement.rct.Top;
    wi:= StrElement.rct.Right - StrElement.rct.Left + 1;
    he:= EditMemo.Lines.Count*StrElement.list.LineHeight + 1;
    he:= max(StrElement.getHeadHeight + 1, he);

    if StrElement is TStrIf then begin
      wi:= StrElement.list.getWidthOfLines(EditMemo.Text) + 10;
      le:= ToolBarStructogram.Width + Image.Left + StrElement.TextPos.x - 5;
    end else if StrElement is TStrSwitch then begin
      le:= ToolBarStructogram.Width + Image.Left + StrElement.TextPos.x - 5;
      wi:= Math.max(100, StrElement.list.getWidthOfLines(EditMemo.Text) + 10);
    end else if (StrElement is TStrSubProgram) then begin
      le:= ToolBarStructogram.Width + Image.Left + StrElement.TextPos.x - 5;
      EditMemo.Width:= EditMemo.Width - LEFT_RIGHT;
    end else if (StrElement is TStrBreak) then begin
      le:= ToolBarStructogram.Width + Image.Left + StrElement.TextPos.x - 5;
      wi:= EditMemo.Width - LEFT_RIGHT;
    end
    else if (StrElement is TStrDoWhile) then
      aTop:= Image.Top + (StrElement as TStrDowhile).do_elem.rct.Bottom;

    EditMemo.SetBounds(le + 2, aTop + 2, wi, he);
    setLeftBorderForEditMemo;
    EditMemo.Visible:= true;
    if EditMemo.canFocus then EditMemo.SetFocus;
    EditMemo.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TFStructogram.setLeftBorderForEditMemo;
begin
  var R:= EditMemo.ClientRect;
  R.Left:= 4;
  SendMessage(EditMemo.Handle, EM_SETRECT,0, LPARAM(@R)) ;
end;

procedure TFStructogram.EditMemoChange(Sender: TObject);
  var w, h: integer; aList: TStrList;
begin
  aList:= getList;
  if assigned(aList) then begin
    aList.getWidthHeigthOfText(EditMemo.Lines.Text, w, h);
    if EditMemo.Height < h then
      EditMemo.Height:= h;
    if EditMemo.Width < w then
      EditMemo.Width:= w;
  end;
  SendMessage(EditMemo.handle, WM_VSCROLL, SB_TOP, 0);
  setLeftBorderForEditMemo;
end;

procedure TFStructogram.CloseEdit(b: boolean);
begin
  if EditMemo.Visible then begin
    if b and assigned(EditMemoElement) then begin
      EditMemoElement.text:= EditMemo.text;
      if EditMemo.text <> EditMemoBeginText then
        Modified:= true;
    end;
    if assigned(curList) then begin
      curList.ResizeAll;
      curList.Paint;
    end;
    UpdateState;
    EditMemo.Visible:= false;
  end;
end;

procedure TFStructogram.FormKeyPress(Sender: TObject; var Key: Char);
  var s: string;
begin
  if Key = #08
    then s:= ''
    else s:= Key;
  if not EditMemo.visible and assigned(curElement) then
    DoEdit(curElement, s);
end;

procedure TFStructogram.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    CutToClipboard
  else if Key = VK_INSERT then
    PasteFromClipboard
  else if (Key = VK_F2) and assigned(curElement) then
    DoEdit(curElement, '');
end;

procedure TFStructogram.BBCloseClick(Sender: TObject);
begin
  inherited;
  Close;
end;

function TFStructogram.getName(StrList: TStrList): string;
begin
  if StrList is TStrAlgorithm then
    Result:= (StrList as TStrAlgorithm).getAlgorithmName;
  if Result = '' then
    Result:= TPath.GetFileNameWithoutExtension(Pathname)
end;

procedure TFStructogram.BBGenerateJavaClick(Sender: TObject);
var
  Filename, aName: string;
  progList: TStringList;
  StrList: TStrList;
begin
  if curList = nil then curList:= getAlgorithm;
  if curList = nil then curList:= getList;
  if curList = nil then exit;
  StrList:= curList;
  aName:= getName(StrList);
  Filename:= ExtractFilepath(Pathname) + aName + '.java';
  progList:= TStringList.Create;
  try
    try
      JavaProgram(StrList, progList, aName);
      if (assigned(FJava.getTDIWindowType(Filename, '%E%')) or FileExists(Filename)) and
         (MessageDlg(Format(_(LNGFileAlreadyExists), [Filename]),
                         mtConfirmation, mbYesNoCancel,0) <> mrYes)
      then
        Filename:= FJava.getFilename('.java');
      if Filename <> '' then begin
        progList.SaveToFile(Filename);
        if FJava.Open(Filename) then
          FJava.RearrangeFileHistory(Filename);
      end;
    except on e: Exception do
      ErrorMsg(Format(_(LNGCanNotCreateFile), [Filename, e.Message]));
    end;
  finally
    FreeAndNil(progList);
  end;
end;

procedure TFStructogram.BBPuzzleClick(Sender: TObject);
begin
  for var i:= ScrollBox.ControlCount - 1 downTo 0 do begin
    var StrList:= TListImage(ScrollBox.Controls[i]).StrList;
    if StrList is TStrAlgorithm then begin
      if StrList.asString = Solution
        then ShowMessage(_('Great, solved!'))
        else ShowMessage(_('Try again!'));
      exit;
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

{$WARNINGS OFF}
procedure TFStructogram.Save(MitBackup: boolean);
  var BackupName, Filename, aPathname, Filepath, Ext: string;
      FStructogram: TFForm;
begin
  CloseEdit(True);
  try
    if (PuzzleMode = 1) and (Pos(_('Easy'), Pathname) = 0) then begin
      Filename:= ChangeFileExt(ExtractFilename(Pathname), '');
      Filepath:= ExtractFilepath(Pathname);

      PuzzleMode:= 2;
      aPathname:= Filepath + Filename + _('Medium') + '.jsg';
      SaveToFile(aPathname);
      FJava.Open(aPathname);

      PuzzleMode:= 3;
      aPathname:= Filepath + Filename + _('Hard') + '.jsg';
      SaveToFile(aPathname);
      FJava.Open(aPathname);

      PuzzleMode:= 4;
      aPathname:= Filepath + Filename + _('VeryHard') + '.jsg';
      SaveToFile(aPathname);
      FJava.Open(aPathname);
      FStructogram:= FJava.getTDIWindowType(aPathname, '%S%');
      (FStructogram as TFStructogram).MakeVeryHard;
      (FStructogram as TFStructogram).Save(false);

      PuzzleMode:= 1;
      Pathname:= Filepath + Filename + _('Easy') + '.jsg';
      SaveToFile(Pathname);
      Caption:= Pathname;
      FJava.RenameTabAndWindow(Number, Pathname);
      FJava.RearrangeFileHistory(Pathname);
      FJava.Open(Pathname);
    end else begin
      if ReadOnly then exit;
      if MitBackup then begin
        BackupName:= Pathname;
        Ext:= ExtractFileExt(Pathname);
        if length(ext) >= 2
          then Ext[2]:= '~'
          else Ext:= '.~';
        BackupName:= ChangeFileExt(BackupName, Ext);
        if FileExists(BackupName) then
          DeleteFile(PChar(BackupName));
        if FileExists(Pathname) then
          RenameFile(Pathname, BackupName);
      end;
      SaveToFile(Pathname);
    end;
    Modified:= false;
  except
    on E: Exception do begin
      ErrorMsg(E.Message);
    end;
  end;

  { debugging

  for var i:= 0 to ScrollBox.ControlCount - 1 do begin
    var aList:= (ScrollBox.Controls[i] as TListImage).StrList;
    aList.debug;
    aList.Paint;
  end;
  }

end;
{$WARNINGS ON}

// Click outside of any image
procedure TFStructogram.ScrollBoxClick(Sender: TObject);
begin
  UpdateState;
  if canFocus then SetFocus;
  CloseEdit(true);
end;

procedure TFStructogram.ScrollBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  HideShape;
end;

procedure TFStructogram.DoExport;
begin
  var aBitmap:= getBitmap;
  try
    FJava.DoExport(Pathname, aBitmap);
  finally
    FreeAndNil(aBitmap);
    if canFocus then SetFocus;
  end;
end;

procedure TFStructogram.Print;
  var aBitmap: Graphics.TBitmap;
      i: integer; aRect: TRect; LI: TListImage;
begin
  aRect:= rect(-1, -1, -1, -1);
  for i:= 0 to ScrollBox.ControlCount - 1 do
    if aRect.Left = -1
      then aRect:= TListImage(ScrollBox.Controls[i]).BoundsRect
      else aRect:= UnionRect(aRect, TListImage(ScrollBox.Controls[i]).BoundsRect);
  aBitmap:= TBitmap.Create;
  aBitmap.Width:=  aRect.Width + FConfiguration.StructogramShadowWidth;
  aBitmap.Height:= aRect.Height + FConfiguration.StructogramShadowWidth;
  aBitmap.Canvas.Lock;
  try
    for i:= 0 to ScrollBox.ControlCount - 1 do begin
      LI:= TListImage(ScrollBox.Controls[i]);
      aBitmap.Canvas.Draw(LI.Left - aRect.Left, LI.top - aRect.Top, LI.Picture.Graphic);
    end;
    PrintBitmap(aBitmap, PixelsPerInch);
  finally
    aBitmap.Canvas.Unlock;
    FreeAndNil(aBitmap);
  end;
end;

procedure TFStructogram.MICopyAsPictureClick(Sender: TObject);
  var aBitmap: Graphics.TBitmap;
begin
  if getCurList then begin
    aBitmap:= TBitmap.Create;
    try
      aBitmap.Width:= curList.Image.Width + FConfiguration.StructogramShadowWidth;
      aBitmap.Height:= curList.Image.Height + FConfiguration.StructogramShadowWidth;
      aBitmap.Canvas.Draw(0, 0, curList.Image.Picture.Graphic);
      Clipboard.Assign(aBitmap);
    finally
      FreeAndNil(aBitmap);
    end;
  end else begin
    CloseEdit(true);
    CopyToClipboard;
  end;
end;

procedure TFStructogram.CopyToClipboard;
  var aBitmap: Graphics.TBitmap;
      i: integer; aRect: TRect; LI: TListImage;
begin
  if EditMemo.Visible then
    EditMemo.CopyToClipboard
  else if ScrollBox.ControlCount > 0 then begin
    aRect:= TListImage(ScrollBox.Controls[0]).BoundsRect;
    for i:= 1 to ScrollBox.ControlCount - 1 do
      aRect:= UnionRect(aRect, TListImage(ScrollBox.Controls[i]).BoundsRect);
    try
      aBitmap:= TBitmap.Create;
      aBitmap.Width := aRect.Width + FConfiguration.StructogramShadowWidth;
      aBitmap.Height:= aRect.Height + FConfiguration.StructogramShadowWidth;
      for i:= 0 to ScrollBox.ControlCount - 1 do begin
        LI:= TListImage(ScrollBox.Controls[i]);
        aBitmap.Canvas.Draw(LI.Left - aRect.Left, LI.top - aRect.Top, LI.Picture.Graphic);
      end;
      Clipboard.Assign(aBitmap);
    finally
      FreeAndNil(aBitmap);
    end;
    UpdateState;
  end;
end;

procedure TFStructogram.CutToClipboard;
begin
  if EditMemo.Visible then begin
    EditMemo.CutToClipboard;
    Modified:= true;
    UpdateState;
  end;
end;

procedure TFStructogram.PasteFromClipboard;
begin
  if EditMemo.visible then
    EditMemo.PasteFromClipboard
end;

procedure TFStructogram.MIDatatypeClick(Sender: TObject);
begin
  var s:= (Sender as TSpTBXItem).Caption;
  FConfiguration.StructoDatatype:= ReplaceStr(s, '&', '');
end;

procedure TFStructogram.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
  for var i:= 0 to ScrollBox.ControlCount -1 do begin
    var StrList:= TListImage(ScrollBox.Controls[i]).StrList;
    StrList.setFont(aFont);
    StrList.setLineHeight;
    StrList.ResizeAll;
    StrList.Paint;
  end;
  Modified:= true;
  FConfiguration.StructogramFont.Assign(aFont);
end;

procedure TFStructogram.StructoPopupMenuPopup(Sender: TObject);
  var s: string; i: integer;  found, aParent: TStrElement;
begin
  MISwitchWithCaseLine.Caption:= FConfiguration.CBSwitchWithCaseLine.Caption;
  MIGenerateMethod.Caption:= trim(FConfiguration.RGGenerateJavacode.Caption) + ' ' + FConfiguration.RGGenerateJavacode.Items[0];
  MIGenerateProgram.Caption:= trim(FConfiguration.RGGenerateJavacode.Caption) + ' ' + FConfiguration.RGGenerateJavacode.Items[1];
  MIDatatype.Caption:= FConfiguration.LDatatype.Caption;

  MIAddCase.Visible:= false;
  MIDeleteCase.Visible:= false;
  MISwitchWithCaseLine.Visible:= false;

  s:= FConfiguration.StructoDatatype;
  for i:= 0 to MIDatatype.Count - 1 do
    MIDatatype.Items[i].Checked:= (ReplaceStr(MIDatatype.Items[i].Caption, '&', '') = s);

  if getCurList then begin
    MISwitchWithCaseLine.Checked:= curList.SwitchWithCaseLine;
    found:= FindVisibleElement(curList, MousePos.X, MousePos.Y);

    aParent:= getParentElement(found);
    curElement:= nil;
    if assigned(found) and (found is TStrSwitch) then
      curElement:= found
    else if assigned(aParent) and (aParent is TStrSwitch) then
      curElement:= aParent;

    if assigned(curElement) then begin
      MIAddCase.Visible:= true;
      MIDeleteCase.Visible:= (Length((curElement as TStrSwitch).case_elems) > 2);
      MISwitchWithCaseLine.Visible:= false;
    end else
      curElement:= found;
  end;
  MIDelete.Enabled:= assigned(CurList);
  MICopy.Enabled:= assigned(CurList);
end;

procedure TFStructogram.UpdateState;
begin
  inherited;
  with FJava do begin
    SetEnabledMI(MICopyNormal,true);
    SetEnabledMI(MICopy, true);
  end;
  MISwitchWithCaseLine.Checked:= FConfiguration.SwitchWithCaseLine;
  TBStatement.Enabled:= not ReadOnly;
  TBIf.Enabled:= not ReadOnly;
  TBSwitch.Enabled:= not ReadOnly;
  TBWhile.Enabled:= not ReadOnly;
  TBDoWhile.Enabled:= not ReadOnly;
  TBFor.Enabled:= not ReadOnly;
  TBSubprogram.Enabled:= not ReadOnly;
  TBBreak.Enabled:= not ReadOnly;
end;

function TFStructogram.GetFormType: string;
begin
  Result:= '%S%';
end;

function TFStructogram.GetSaveAsName: string;
begin
  var StrList:= getAlgorithm;
  if assigned(StrList)
    then Result:= StrList.getAlgorithmName
    else Result:= '';
end;

procedure TFStructogram.SetOptions;
begin
  for var i:= 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.Controls[i] is TListImage then
      TListImage(ScrollBox.Controls[i]).StrList.Paint;
  var b:= not FConfiguration.LockedStructogram;
  TBGenerateJava.Enabled:= b;
  MIGenerateMethod.Visible:= b;
  MIGenerateProgram.Visible:= b;
  MIDatatype.Visible:= b;
end;

procedure TFStructogram.AddParameter(list: TStrList; paramList: TStringList);
var
  aText, token, param: string;
  i, typ: Integer;
begin
  i:= Pos('(', list.text);
  if i > 0 then begin
    aText:= Copy(list.text, i+1, Length(list.text));
    typ:= GetToken(aText, token);
    while (typ > 0) and (token <> ')') do begin
      if typ = 1 then begin
        param:= token;
        typ:= GetToken(aText, token);
        if typ = 1
          then param:= param + ' ' + token
          else param:= DataType + ' ' + param;
        paramList.Add(param);
      end;
      typ:= GetToken(aText, token);
    end;
  end;
end;

procedure TFStructogram.AddVariable(aText: string; list: TStringList);
var
  typ: Integer;
  isArray: Boolean;
  token, variable: string;
begin
  if Pos('=', aText)  > 0 then begin
    aText:= trim(aText);
    GetToken(aText, variable);
    if Pos('#' + variable + '#', Variables) = 0 then begin
      Variables:= Variables + '#' + variable + '#';  // store of variables;
      GetToken(aText, token);
      isArray:= (token = '[');
      typ:= GetToken(aText, token);
      if Pos('InOut.read', token) = 1 then begin
        if token = 'Char' then typ:= 2 else
        if token = 'Boolean' then typ:= 3 else
        if token = 'Int' then typ:= 4 else
        if token = 'Double' then typ:= 5 else
        if token = 'Float' then typ:= 6 else
        if token = 'String' then typ:= 7;
      end;
      while typ > 0 do begin
        if (typ > 1) and (typ < 8) then break;
        typ:=GetToken(aText, token);
      end;
      variable:= ReplaceStr(variable, ' ', '_');
      if isArray then
        case typ of
          2: variable:='char[] ' + variable + ' = new char[100];';
          3: variable:='boolean[] ' + variable + ' = new boolean[100];';
          4: variable:='int[] ' + variable + ' = new int[100];';
          5: variable:='double[] ' + variable + ' = new double[100];';
          6: variable:='float[] ' + variable + ' = new float[100];';
          7: variable:='String[] ' + variable + ' = new String[100];';
          else variable:= DataType + '[] ' + variable + ' = new ' + DataType + '[100];';
        end
      else
        case typ of
          2: variable:='char ' + variable + ';';
          3: variable:='boolean ' + variable + ';';
          4: variable:='int ' + variable + ';';
          5: variable:='double ' + variable + ';';
          6: variable:='float ' + variable + ';';
          7: variable:='String ' + variable + ';';
          else variable:= DataType + ' ' + variable + ';';
        end;
      if FConfiguration.GenerateJavaAsProgram = 0
        then list.Add(FConfiguration.Indent1 + variable)
        else list.Add(FConfiguration.Indent2 + variable)
    end;
  end;
end;

function TFStructogram.getAlgorithmParameter(ParamList: TStringList): string;
  var Param, s: string; i: integer;
begin
  Param:= '';
  for i:= 0 to ParamList.Count - 1 do begin
    s:= trim(ParamList.Strings[i]);
    Param:= Param + s + ', ';
  end;
  if Param <> '' then
    delete(Param, length(Param)-1, 2);
  Result:= '(' + Param + ')';
end;

procedure TFStructogram.JavaProgram(list: TStrList; progList: TStringList; const aName: string);
var
  i, j, p: Integer;
  paramList, VariablesList: TStringList;
  s1, s2, s, Indent, param, typ: string;
begin
  Datatype:= FConfiguration.StructoDatatype;
  Variables:= '';
  paramList:= TStringList.Create;
  VariablesList:= TStringList.Create;
  try
    AddParameter(list, paramList);
    if FConfiguration.GenerateJavaAsProgram = 0
      then Indent:= FConfiguration.Indent1
      else Indent:= FConfiguration.Indent2;
    StrElementToJava(list.next, progList, VariablesList, Indent);
    for i:= 0 to paramList.Count-1 do begin
      j:= 0;
      while j < VariablesList.Count do begin
        param:= ParamList.Strings[i];
        p:= Pos(' ', param);
        delete(param, 1, p);
        if (Pos(' ' + param + ';', VariablesList.Strings[j]) > 0) then begin
          typ:= trim(VariablesList.Strings[j]);
          p:= Pos(' ', typ);
          delete(typ, p, length(typ));
          ParamList.Strings[i]:= typ + ' ' + param;
          VariablesList.Delete(j);
        end;
        inc(j);
      end;
    end;
    progList.Insert(0, '');
    if VariablesList.Count > 0 then begin
      for i:= VariablesList.Count-1 downto 0 do
        if progList.IndexOf(VariablesList.Strings[i]) < 0 then
          progList.Insert(0, VariablesList.Strings[i]);
    end;

    if FConfiguration.GenerateJavaAsProgram = 1 then begin
      s1:= '';
      s2:= '';
      if FConfiguration.CommentClosingBrackets then begin
        s1:= ' // end of main';
        s2:= ' // end of class ' + aName;
      end;
      s:= FTemplates.GetTemplate(aName, 1);
      if s = '' then begin
        s:= FConfiguration.HeadText + CrLf +
            'public class ' + aName + ' {' + CrLf +
            FConfiguration.Indent1 + CrLf +
            FConfiguration.Indent1 + 'public static void main(String[] args) {' + CrLf +
            FConfiguration.Indent2 + '' + CrLf +
            FConfiguration.Indent1 + '}' + s1 + CrLf + CrLf +
            '}' + s2 + CrLf;
      end;
      i:= Pos('public static void', s);
      while (i < length(s)) and (s[i] <> #13) do inc(i);
      s1:= copy(s, 1, i+1);
      s2:= copy(s, i+3, length(s));
    end else begin
      s1:= 'public void ' + aName + getAlgorithmParameter(paramList) + ' {' + CrLf;
      s2:= CrLf + '}';
      if FConfiguration.CommentClosingBrackets then
        s2:= s2 + ' // end of ' + aName;
    end;
  finally
    FreeAndNil(VariablesList);
    FreeAndNil(paramList);
  end;
  progList.Insert(0, s1);
  progList.Add(s2);
end;

procedure TFStructogram.StrElementToJava(element: TStrElement; pList, VariablesList: TStringList; const indent: string);
var
  i, p, q, t,i1, i2: Integer;
  aText, s, vari, start, _end, _for, step, part: string;
  s1, s2, repl: string; SL: TStringList;
  SwitchElement: TStrSwitch;

  function StripLNG(const LNG: string; s: string): string;
    var p: integer;
  begin
    p:= Pos(LNG, s);
    if p > 0 then delete(s, p, Length(LNG));
    Result:= trim(s);
  end;

  function withoutCrLf(const s: string): string;
  begin
    Result:= ReplaceStr(s, CrLf, ' ');
  end;

  procedure SkipToChar(ch: char);
  begin
    inc(t);
    while (t < SL.Count) and (Pos(ch, SL.Strings[t]) = 0) do begin
      plist.Add(indent + SL.Strings[t]);
      inc(t);
    end;
  end;

  procedure ReplaceInsertMarker(const repl: string);
    var p: integer;
  begin
    s:= SL.Strings[t];
    p:= Pos('|', s);
    if p = 0 then begin // do while
      p:= Pos('(', s);
      insert(repl, s, p+1);
    end else begin
      delete(s, p, 1);
      insert(repl, s, p);
    end;
    pList.Add(indent + s);
  end;

  procedure SkipToInsertLine;
  begin
    inc(t);
    while (t < SL.Count) and not ((trim(SL.Strings[t]) = '') or (trim(SL.Strings[t]) = '|')) do begin
      pList.Add(indent + SL.Strings[t]);
      inc(t);
    end;
  end;

  procedure SkipToEnd;
  begin
    inc(t);
    while t < SL.Count do begin
      pList.Add(indent + SL.Strings[t]);
      inc(t);
    end;
  end;

begin
  while element <> nil do begin
    if element is TStrStatement then begin
      s:= withoutCrLf(element.text);
      if Pos(FConfiguration.Input, s) = 1 then begin
        delete(s, 1, length(FConfiguration.Input));
        s:= trim(s);
        s:= ReplaceStr(s, ' ', '_');
        AddVariable( s + '= ;', VariablesList);
        s:= s + ' = InOut.read' + Upperlower(DataType)  + '("' + s + ': ");';
      end else if Pos(FConfiguration.Output, s) = 1 then begin
        delete(s, 1, length(FConfiguration.Output));
        s:= trim(s);
        if Pos('"" + ', s) > 0
          then s:= 'System.out.println(' + s + ');'
          else s:= 'System.out.println("" + ' + s + ');';
      end;
      aText:= indent + s;
      i:= Pos(';', aText);
      if i = 0 then
        aText:=aText + ';';
      if trim(s) <> '' then pList.Add(aText);
      { declaration of variables only in TStrStatement possible }
      AddVariable(aText, VariablesList);
    end
    else if element is TStrIf then begin
      if (TStrIf(element).else_elem.next.text <> '') or (TStrIf(element).else_elem.next.next <> nil)
        then SL:= FConfiguration.ControlStructureTemplates[9]
        else SL:= FConfiguration.ControlStructureTemplates[1];
      t:= -1;
      SkipToChar('|');
      ReplaceInsertMarker(withoutCrLF(element.text));
      SkipToInsertLine;
      StrElementToJava(TStrIf(element).then_elem.next, pList, VariablesList, indent + FConfiguration.Indent1);
      SkipToChar('}');
      if t < SL.Count then
        pList.Add(indent + SL.Strings[t]);
      if (TStrIf(element).else_elem.next.text <> '') or (TStrIf(element).else_elem.next.next <> nil) then begin
        SkipToInsertLine;
        StrElementToJava(TStrIf(element).else_elem.next, pList, VariablesList, indent + FConfiguration.Indent1);
      end;
      SkipToEnd;
    end else if element is TStrDoWhile then begin
      SL:= FConfiguration.ControlStructureTemplates[4];
      t:= -1;
      SkipToInsertLine;
      StrElementToJava(TStrDoWhile(element).do_elem.next, pList, VariablesList, indent + FConfiguration.Indent1);
      repl:= stripLNG(FConfiguration.DoWhile, withOutCrLf(element.text));
      SkipToChar('(');
      ReplaceInsertMarker(repl);
      SkipToEnd;
    end else if element is TStrFor then begin
      repl:= withoutCrLf(element.text);
      _for:= FConfiguration._for;
      p:= Pos('[', _for);
      if p > 0 then begin
        part:= copy(_for, 1, p-1);
        delete(repl, 1, p-1);
        delete(_for, 1, p+2);
        p:= Pos('[', _for);

        if p > 0 then begin
          part:= Copy(_for, 1, p-1);
          q:= Pos(Part, repl);
          vari:= Copy(repl, 1, q-1);
          delete(repl, 1, q + length(part)-1);
          delete(_for, 1, p+2);
        end else
          vari:= 'i';

        p:= Pos('[', _for);
        if p > 0 then begin
          part:= Copy(_for, 1, p-1);
          q:= Pos(part, repl);
          Start:= Copy(repl, 1, q-1);
          delete(repl, 1, q + length(part)-1);
          delete(_for, 1, p+2);
        end else
          Start:= '1';

        p:= Pos('[', _for);
        if p > 0 then begin
          part:= Copy(_for, 1, p-1);
          q:= Pos(part, repl);
          if q > 0 then begin
            _end:= Copy(repl, 1, q-1);
            delete(repl, 1, q + length(part)-1);
            delete(_for, 1, p-1);
          end else
            _end:= repl
        end else
          _end:= 'n';

        if _for = '[s]' then begin
          if TryStrToInt(repl, i1) then
            if i1 < 0
              then step:= vari + ' = ' + vari + ' - ' + IntToStr(i1)
              else step:= vari + ' = ' + vari + ' + ' + IntToStr(i1)
          else step:= vari + ' = ' + vari + ' + ' + repl;
        end else
          if TryStrToInt(Start, i1) and TryStrToInt(_end, i2) and (i2 < i1)
            then step:= vari + '--'
            else step:= vari + '++';
        if Pos('+', step) > 0
          then repl:= 'int ' + vari + ' = ' + Start + '; ' + vari + ' <= ' + _end + '; ' + step
          else repl:= 'int ' + vari + ' = ' + Start + '; ' + vari + ' >= ' + _end + '; ' + step;
      end else
        repl:= 'int i = 1; i < n; i++';
      SL:= FConfiguration.ControlStructureTemplates[3];
      t:= -1;
      SkipToChar('|');

      s:= SL.Strings[t];
      p:= Pos('|', s);
      s1:= copy(s, 1, p-1);
      p:= Pos(')', s);
      s2:= copy(s, p, length(s));
      pList.Add(indent + s1 + repl + s2);
      SkipToInsertLine;
      StrElementToJava(TStrWhile(element).do_elem.next, pList, VariablesList, indent + FConfiguration.Indent1);
      SkipToEnd;
    end else if element is TStrWhile then begin
      repl:= stripLNG(FConfiguration._While, withoutCrLf(element.text));
      SL:= FConfiguration.ControlStructureTemplates[2];
      t:= -1;
      SkipToChar('|');
      ReplaceInsertMarker(repl);
      SkipToInsertLine;
      StrElementToJava(TStrWhile(element).do_elem.next, pList, VariablesList, indent + FConfiguration.Indent1);
      SkipToEnd;
    end else if element is TStrSwitch then begin
      SL:= FConfiguration.ControlStructureTemplates[5];
      t:= -1;
      SkipToChar('|');
      ReplaceInsertMarker(withoutCrLf(element.text));
      SwitchElement:= element as TStrSwitch;
      for i:= 0 to high(SwitchElement.case_elems)-1 do begin
        aText:= indent + FConfiguration.Indent1 + 'case ' + SwitchElement.case_elems[i].next.text + ': ';
        pList.Add(aText);
        StrElementToJava(SwitchElement.case_elems[i].next.next, pList, VariablesList, indent + FConfiguration.Indent2);
        aText:= indent + FConfiguration.Indent2 + 'break;';
        pList.Add(aText);
      end;
      i:= high(SwitchElement.case_elems);
      aText:= indent + FConfiguration.Indent1 + 'default: ';
      pList.Add(aText);
      StrElementToJava(SwitchElement.case_elems[i].next.next, pList, VariablesList, indent + FConfiguration.Indent2);
      pList.Add(indent + '}');
    end else if element is TStrSubProgram then begin
      s:= withoutCrLf(element.text);
      if trim(s) <> '' then pList.Add(indent + s);
    end else if element is TStrBreak then
      pList.Add(indent + 'break;');
    element:= element.next;
  end;
end;

function TFStructogram.FindElement(current: TStrElement; X, Y: Integer): TStrElement;
var
  ifElement: TStrIf;
  whileElement: TStrWhile;
  doWhileElement: TStrDoWhile;
  switchElement: TStrSwitch;
begin
  while (current <> nil) and (current.next <> nil) and (Y > current.rct.bottom) do
    current:= current.next;
  if current is TStrIf then begin                    { search recursively inside IF }
    ifElement:= (current as TStrIf);
    if Y > current.rct.Top + current.getHeadHeight div 2 then
      if X < ifElement.then_elem.rct.right - 5 then  { except if ±5 from middle}
        current:= FindElement(ifElement.then_elem.next, X, Y)
      else if X > ifElement.else_elem.rct.left + 5 then
        current:= FindElement(ifElement.else_elem.next, X, Y)
  end else if current is TStrDoWhile then begin      { search recursively inside DO-WHILE }
    doWhileElement:= current as TStrDoWhile;
    if (X > doWhileElement.do_elem.rct.left) and
       (Y > current.rct.Top) then
      current:= FindElement(doWhileElement.do_elem.next, X, Y);
  end else if current is TStrWhile then begin        { search recursively inside WHILE/REPEAT/FOR(!) }
    whileElement:= current as TStrWhile;
    if (X > whileElement.do_elem.rct.left) and
       (Y > current.rct.Top + current.getHeadHeight div 2) then
      current:= FindElement(whileElement.do_elem.next, X, Y);
  end else if current is TStrSwitch then begin       { search recursively inside SWITCH }
    SwitchElement:= (current as TStrSwitch);
    if Y > current.rct.Top + current.getHeadHeight then
      for var i:= 0 to high(SwitchElement.case_elems) do
        if (SwitchElement.case_elems[i].rct.left + 5 < X) and {except if ±5 from middle}
           (X < SwitchElement.case_elems[i].rct.right - 5)
        then begin
          current:= FindElement(SwitchElement.case_elems[i].next.next, X, Y);
          break;
        end;
  end;
  if assigned(current) and BeforeOrAfter(current) and
    (((Y > current.rct.top + current.getMaxDelta) and
      (Y < current.rct.bottom - current.getMaxDelta)) or
    ((current is TStrAlgorithm) and
     (Y < current.rct.top + current.getMaxDelta))) then
      current:= nil;
  Result:= current;
  // FMessages.OutputToTerminal(Result.text);  // for debugging
end;

function TFStructogram.BeforeOrAfter(current: TStrElement): boolean;
  var TestElement: TStrElement;
begin
  if current is TStrListHead
    then TestElement:= current.next
    else TestElement:= current;
  Result:= (TestElement.kind <> byte(nsStatement)) or (TestElement.text <> '');
end;

function TFStructogram.FindVisibleElement(current: TStrElement; X, Y: Integer): TStrElement;
var
  ifElement: TStrIf;
  whileElement: TStrWhile;
  doWhileElement: TStrDoWhile;
  switchElement: TStrSwitch;
begin
  while (current <> nil) and (current.next <> nil) and (Y > current.rct.bottom) do
    current:= current.next;

  if current is TStrIf then begin                    { search recursively inside IF }
    ifElement:= (current as TStrIf);
    if Y > current.rct.Top + current.getHeadHeight then
      if X <= ifElement.then_elem.rct.right
        then current:= FindVisibleElement(ifElement.then_elem.next, X, Y)
        else current:= FindVisibleElement(ifElement.else_elem.next, X, Y)
  end else if current is TStrDoWhile then begin      { search recursively inside DO-WHILE }
    doWhileElement:= current as TStrDoWhile;
    if (X > doWhileElement.do_elem.rct.left) and (Y < doWhileElement.do_elem.rct.bottom) then
      current:= FindVisibleElement(doWhileElement.do_elem.next, X, Y);
  end else if current is TStrWhile then begin        { search recursively inside WHILE/REPEAT/FOR(!) }
    whileElement:= current as TStrWhile;
    if (X > whileElement.do_elem.rct.left) and (Y > whileElement.do_elem.rct.top) then
      current:= FindVisibleElement(whileElement.do_elem.next, X, Y);
  end else if current is TStrSwitch then begin       { search recursively inside SWITCH }
    SwitchElement:= (current as TStrSwitch);
    if Y > current.rct.Top + current.getHeadHeight then
      for var i:= 0 to high(SwitchElement.case_elems) do
         if X < SwitchElement.case_elems[i].rct.right then begin     {except if ±5 from middle}
           current:= FindVisibleElement(SwitchElement.case_elems[i].next, X, Y);
           break;                                            // not next.next like in FindElement
         end;
  end;
  Result:= current;
end;

function TFStructogram.getParentElement(element: TStrElement): TStrElement;
begin
  var tmp:= element;
  while assigned(tmp) and (tmp.prev <> nil) do
    tmp:= tmp.prev;
  if assigned(tmp) and (tmp is TStrListHead)
    then Result:= TStrListhead(tmp).Parent
    else Result:= nil;
end;

function TFStructogram.GetToken(var S: string; var Token: string): Integer;
{  get next token in <S> and return in <Token>.
   Return type of token: -1=no token, 0=invalid, 1=name, 2=char, 3=boolean, 4=integer,
                         5=real, 6=hex number, 7=string, 8=special char.
}
var
  ch: Char;
  i: Integer;
  hex: Boolean;

  procedure NextCh;
  begin
    Token[i]:= ch;
    Inc(i);
    if i <= length(s)
      then ch:= S[i]
      else ch:= #0;
  end;

begin
  setLength(Token, 255);
  i:= 1;
  while i <= Length(S) do begin {overread leading blanks and tabs}
    ch:= S[i];
    if (ch <> ' ') and (ord(ch) <> 9) then break;
    Inc(i);
  end;
  delete(S, 1, i-1);
  if S = '' then begin
    Token:= '';
    Result:= -1;  {no token}
    Exit;
  end;
  i:= 1;
  if not IsWordBreakChar(ch) and not CharInSet(ch, ['0'..'9']) then begin   { name }
    repeat
      NextCh;
    until IsWordBreakChar(ch) and not isDigit(ch) and (ch <> '.') or (i > Length(S));
    dec(i);
    SetLength(Token, i);
    if (UpperCase(Token) = 'TRUE') or (UpperCase(Token) = 'FALSE') then
      Result:= 3   {boolean}
    else
      Result:= 1;  { name }
  end
  else if (ch = '''') then begin                                    { char }
    NextCh;
    while (ch <> '''') and (ch >= ' ') and (i <= Length(S)) do
      NextCh;
    Token[i]:= '''';
    Result:= 2;  { char }
  end
  else if (ch = '"') then begin                                    { string }
    NextCh;
    while (ch <> '"') and (ch >= ' ') and (i <= Length(S)) do
      NextCh;
    Token[i]:= '"';
    Result:= 7;  { String }
  end
  else begin
    if (ch = '-') then
      NextCh;
    if ('0' <= ch) and (ch <= '9') then begin                       { number }
      hex:=false;
      while true do begin
        NextCh;
        if (ch < '0') or (i > Length(S)) then break;
        if (ch > '9') then begin
          if ('A' <= UpperCase(ch)) and (UpperCase(ch) <= 'F') then
            hex:=true
          else
            break;
        end;
      end;
      if (UpperCase(ch) = 'H') then begin                           { hex number }
        NextCh;
        dec(i);
        Result:= 6;  {hex number}
      end
      else if (ch = '.') then begin                                 { real }
        NextCh;
        while ('0' <= ch) and (ch <= '9') do
          NextCh;
        if (ch = 'E') then
          NextCh;
        if (ch = '+') or (ch = '-') then
          NextCh;
        while ('0' <= ch) and (ch <= '9') do
          NextCh;
        dec(i);
        if hex then
          Result:= 0   {invalid}
        else
          Result:= 5;  {real number}
      end
      else begin
        if hex then
          Result:= 0   {invalid}
        else
          Result:= 4;  {integer}
      end;
    end
    else begin
      NextCh;
      dec(i);
      Result:= 8; {special char}
    end;
  end;
  SetLength(Token, i);
  Delete(S, 1, i);
end;

procedure TFStructogram.setEvents(Image: TListImage);
begin
  with Image do begin
    OnDblClick:= ImageDblClick;
    OnMouseDown:= ImageMouseDown;
    OnMouseMove:= ImageMouseMove;
    OnMouseUp:= ImageMouseUp;
  end;
end;

function TFStructogram.getAlgorithm: TStrAlgorithm;
begin
  Result:= nil;
  for var i:= 0 to ScrollBox.ControlCount -1 do begin
    var aList:= TListImage(ScrollBox.controls[i]).StrList;
    if aList is TStrAlgorithm then exit(aList as TStrAlgorithm);
  end;
end;

function TFStructogram.getListAtScreenPos(Pt: TPoint): TStrList;
begin
  Result:= nil;
  for var i:= 0 to ScrollBox.ControlCount - 1 do
    if ScrollBox.controls[i].BoundsRect.contains(pt) then
      exit(TListImage(ScrollBox.controls[i]).StrList);
end;

function TFStructogram.getCurList: boolean;
begin
  var pt:= ScrollBox.ScreenToClient(StructoPopupMenu.PopupPoint);
  curList:= getListAtScreenPos(pt);
  Result:= assigned(curList);
end;

function TFStructogram.getCurListAndCurElement: boolean;
begin
  curElement:= nil;
  if getCurList then begin
    var pt:= CurList.Image.ScreenToClient(StructoPopupMenu.PopupPoint);
    curElement:= FindVisibleElement(curList, Pt.X, Pt.Y);
  end;
  Result:= assigned(curElement);
end;

function TFStructogram.getList: TStrList;
begin
  if ScrollBox.ControlCount = 0
    then Result:= nil
    else Result:= TListImage(ScrollBox.controls[0]).StrList;
end;

function TFStructogram.getBitmap: TBitmap;
  var aRect: TRect;
      i: integer;
      aBitmap: TBitmap;
begin
  aRect:= rect(-1, -1, -1, -1);
  for i:= 0 to ScrollBox.ControlCount - 1 do
    if aRect.Left = -1
      then aRect:= TListImage(ScrollBox.Controls[i]).BoundsRect
      else aRect:= Unionrect(arect, TListImage(ScrollBox.Controls[i]).BoundsRect);
  aBitmap:= TBitmap.Create;
  aBitmap.Width:=  aRect.Width + FConfiguration.StructogramShadowWidth;
  aBitmap.Height:= aRect.Height + FConfiguration.StructogramShadowWidth;
  ScrollBox.PaintTo(aBitmap.Canvas.Handle,  -aRect.Left, -aRect.Top);
  Result:= aBitmap;
end;

procedure TFStructogram.PaintAll;
begin
  for var i:= 0 to ScrollBox.ControlCount - 1 do begin
    var aList:= (ScrollBox.Controls[i] as TListImage).StrList;
    if aList.dirty then
      aList.Paint;
    aList.dirty:= false;
  end;
end;

procedure TFStructogram.debug(const s: string);
begin
  FMessages.OutputToTerminal(s);
end;

function TFStructogram.fitsIn(aCurList: TStrList; aCurElement: TStrElement): boolean;
  var w, h: integer;
begin
  if PuzzleMode = 0 then
    Result:= true
  else if (aCurElement is TStrListHead) or (aCurElement is TStrCase) or
          ((PuzzleMode = 1) and (CurInsert <> 0)) then
    Result:= false
  else begin
    w:= aCurElement.rct.Right - aCurElement.rct.Left;
    h:= aCurElement.rct.Bottom - aCurElement.rct.Top;
    if PuzzleMode = 1 then
      Result:= (aCurList.rctList.Width = w) and (aCurList.rctList.Height = h)
    else if PuzzleMode = 2 then
      Result:= (aCurList.rctList.Width = w)
    else
      Result:= true;
  end;
end;

procedure TFStructogram.MakeVeryHard;
begin
  for var i:= 0 to ScrollBox.ControlCount -1 do begin
    var aList:= TListImage(ScrollBox.controls[i]).StrList;
    aList.Collapse;
    if aList.next = nil then
      aList.Destroy
    else begin
      aList.ResizeAll;
      aList.Paint;
    end;
  end;
end;

procedure TFStructogram.ChangeStyle;
begin
  if FConfiguration.isDark then begin
    ToolBarStructogram.Images:= vilToolbarDark;
    TrashImage.ImageIndex:= 29;
    StructoPopupMenu.Images:= vilPopupMenuDark;
  end else begin
    ToolBarStructogram.Images:= vilToolbarLight;
    TrashImage.ImageIndex:= 14;
    StructoPopupMenu.Images:= vilPopupMenuLight;
  end;
  for var i:= 0 to ScrollBox.ControlCount -1 do
    if Scrollbox.Controls[i] is TListImage then
      TListImage(Scrollbox.Controls[i]).StrList.Paint;
end;

procedure TFStructogram.DPIChanged;
begin
  setFontSize(0);
  Hide;
  Show;
end;

end.
