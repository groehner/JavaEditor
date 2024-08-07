unit ULink;

interface

uses Graphics, Classes;

const
  DelphiBounds = ' Name Left Top Width Height Resizable Undecorated Caption ';
  JavaBounds   = ' Name X    Y   Width Height Resizable Undecorated Title   ';

  JavaColorsText = '(NONE)'#13#10'WHITE'#13#10'YELLOW'#13#10'CYAN'#13#10'GREEN'#13#10 +
                   'ORANGE'#13#10'PINK'#13#10'MAGENTA'#13#10'RED'#13#10'BLUE'#13#10 +
                   'LIGHTGRAY'#13#10'GRAY'#13#10'DARKGRAY'#13#10'BLACK';

  JavaCursorText = 'DEFAULT'#13#10'CROSSHAIR'#13#10'TEXT'#13#10'WAIT'#13#10'SW_RESIZE'#13#10 +
                   'SE_RESIZE'#13#10'NW_RESIZE'#13#10'NE_RESIZE'#13#10'N_RESIZE'#13#10 +
                   'S_RESIZE'#13#10'W_RESIZE'#13#10'E_RESIZE'#13#10'HAND'#13#10'MOVE';

  {
  FrameKonsole = 1;
  FrameAWT     = 2;
  DialogAWT    = 3;
  AppletAWT    = 4;
  FrameSwing   = 5;
  DialogSwing  = 6;
  AppletSwing  = 7;
  JavaFX       = 8;
  }

var
  ComponentNrToInsert: integer;
  PanelCanvasType: string;
  SelectStrings: TStrings;

  function Delphi2JavaValues(const s: string): string;
  function Delphi2JavaNames(Tag: integer; const aClass, Attr: string): string;
  function Java2DelphiValues(const d, j: string): string;
  function Java2DelphiColors(const s: string): string;
  function Delphi2JavaColors(s: string): string;
  function Java2DelphiCursor(const s: string): string;
  function Delphi2JavaCursor(const s: string): string;
  function turnRGB(const s: string): string;
  function TColorToString(Color: TColor): string;
  function toJavaColor(Color: TColor): string;

implementation

uses SysUtils;

const
  DelphiValues  = ' False True ';
  JavaValues    = ' false true ';

  DelphiColors   = ' clBlack $00404040  clGray $00C0C0C0 clBlue $00AFAFFF $0000C8FF clLime clFuchsia clAqua clRed clWhite clYellow clBtnFace clWindowText clWindow ';
  JavaColors     = ' BLACK   DARKGRAY   GRAY   LIGHTGRAY BLUE   PINK      ORANGE    GREEN  MAGENTA   CYAN   RED   WHITE   YELLOW   (NONE)    (NONE)       (NONE)   ';


  DelphiCursor   = ' crDefault crCross   crIBeam crHourGlass crSizeNESW crSizeNWSE crSizeNESW' +
                   ' crSizeNESW crSizeNS crSizeNS crSizeWE crSizeWE crHandPoint crSizeAll ';
  JavaCursor     = ' DEFAULT   CROSSHAIR TEXT    WAIT        SW_RESIZE  SE_RESIZE  NW_RESIZE ' +
                   ' NE_RESIZE  N_RESIZE S_RESIZE W_RESIZE E_RESIZE HAND        MOVE      ';

var
  Delphi, Java: string;

function FromTo(s: string; const d, j: string): string;
begin
  Result:= s;
  if trim(s) = '' then exit;
  var p:= Pos(' ' + s + ' ', d);
  if p > 0 then begin
    s:= copy(j, p + 1, 255);
    delete(s, pos(' ', s), 255);
    Result:= s;
  end;
end;

procedure SetChangeAttributeNames(Tag: integer; const Klasse: string);
  // from Delphi to Java
begin
  case Tag of
    0: if Klasse = 'Style' then begin
         Delphi:= ' fsBold fsItalic ';
         Java  := ' Bold   Italic   ';
       end;
   16: begin // ProgressBar
         Delphi:= ' ProgressString ';
         Java  := ' String         ';  // string is not useable
       end;
   166: begin
         Delphi:= ' _Type ';
         Java  := ' Type  ';  // string is not useable
       end;
   else begin
     Delphi:= ' ';
     Java  := ' ';
   end;
  end;
end;

function Delphi2JavaNames(Tag: integer; const aClass, Attr: string): string;
begin
  SetChangeAttributeNames(Tag, aClass); // Frame/AWT
  Result:= FromTo(Attr, DelphiBounds + Delphi, JavaBounds + Java)
end;

function Java2DelphiValues(const d, j: string): string;
  var dw, jw, prefix: string; 
begin
  prefix:= copy(d, 1, 2);
  if prefix = 'cr' then begin
    dw:= DelphiCursor;
    jw:= JavaCursor;
  end;

  Result:= FromTo(j, jw, dw);
  if Result = '' then Result:= j;
end;

function Delphi2JavaValues(const s: string): string;
begin // used to change attributes of type TColor
  Result:= FromTo(s, DelphiValues + DelphiCursor, JavaValues + JavaCursor);
  if Result = '' then Result:= s;
end;

function Java2DelphiColors(const s: string): string;
begin // used to show color combo box
  Result:= FromTo(s, JavaColors, DelphiColors);
  if Result = '' then Result:= 'clBtnFace';
end;

function Delphi2JavaColors(s: string): string;
begin
  s:= FromTo(s, DelphiColors, JavaColors);
  if copy(s, 1, 3) = '$00'
    then s:= '0x' + copy(s, 8, 2) + copy(s, 6, 2) + copy(s, 4, 2);
  if s = '' then s:= '(NONE)';
  Result:= s;
end;

function turnRGB(const s: string): string;
begin
  Result:= s;
  if copy(s, 1, 2) = '0x' then
    Result:= '0x' + copy(s, 7, 2) + copy(s, 5, 2) + copy(s, 3, 2);
end;

function TColorToString(Color: TColor): string;
begin
  var s:= ColorToString(Color);
  s:= Delphi2JavaColors(s);
  if copy(s, 1, 2) = 'cl' then
    FmtStr(s, '%s%.8x', [HexDisplayPrefix, Color]);
  if copy(s, 1, 2) = '$0' then
    s:= '0x' + copy(s, 8, 2) + copy(s, 6, 2) + copy(s, 4, 2);
  Result:= s;
end;

function toJavaColor(Color: TColor): string;
begin
  var col:= TColorToString(Color);
  if copy(col, 1, 2) = '0x'
    then Result:= 'new Color(' + col + ')'
    else Result:= 'Color.' + col;
end;

function Java2DelphiCursor(const s: string): string;
begin // used to show cursor combo box
  Result:= FromTo(s, JavaCursor, DelphiCursor);
end;

function Delphi2JavaCursor(const s: string): string;
begin
  Result:= FromTo(s, DelphiCursor, JavaCursor);
end;

initialization
  SelectStrings:= TStringlist.Create;

finalization
  SelectStrings.Destroy;
end.
