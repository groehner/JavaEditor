unit ULink;

interface

uses
  Graphics,
  Classes;

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
  GComponentNrToInsert: Integer;
  PanelCanvasType: string;
  GSelectStrings: TStrings;

function Delphi2JavaValues(const Str: string): string;
function Delphi2JavaNames(Tag: Integer; const AClass, Attr: string): string;
function Java2DelphiValues(const DValue, JValue: string): string;
function Java2DelphiColors(const Str: string): string;
function Delphi2JavaColors(Str: string): string;
function Java2DelphiCursor(const Str: string): string;
function Delphi2JavaCursor(const Str: string): string;
function TurnRGB(const Str: string): string;
function TColorToString(Color: TColor): string;
function ToJavaColor(Color: TColor): string;

implementation

uses SysUtils;

const
  DelphiValues  = ' False True ';
  JavaValues    = ' false true ';

  DelphiColors   = ' clBlack $00404040  clGray $00C0C0C0 clBlue $00AFAFFF $0000C8FF clLime' +
                   ' clFuchsia clAqua clRed clWhite clYellow clBtnFace clWindowText clWindow ';
  JavaColors     = ' BLACK   DARKGRAY   GRAY   LIGHTGRAY BLUE   PINK      ORANGE    GREEN ' +
                   ' MAGENTA   CYAN   RED   WHITE   YELLOW   (NONE)    (NONE)       (NONE)   ';

  DelphiCursor   = ' crDefault crCross   crIBeam crHourGlass crSizeNESW crSizeNWSE crSizeNESW' +
                   ' crSizeNESW crSizeNS crSizeNS crSizeWE crSizeWE crHandPoint crSizeAll ';
  JavaCursor     = ' DEFAULT   CROSSHAIR TEXT    WAIT        SW_RESIZE  SE_RESIZE  NW_RESIZE ' +
                   ' NE_RESIZE  N_RESIZE S_RESIZE W_RESIZE E_RESIZE HAND        MOVE      ';

var
  Delphi, Java: string;

function FromTo(Str: string; const Delphi, Java: string): string;
begin
  Result:= Str;
  if Trim(Str) = '' then
    Exit;
  var Posi:= Pos(' ' + Str + ' ', Delphi);
  if Posi > 0 then begin
    Str:= Copy(Java, Posi + 1, 255);
    Delete(Str, Pos(' ', Str), 255);
    Result:= Str;
  end;
end;

procedure SetChangeAttributeNames(Tag: Integer; const Style: string);
  // from Delphi to Java
begin
  case Tag of
    0: if Style = 'Style' then begin
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

function Delphi2JavaNames(Tag: Integer; const AClass, Attr: string): string;
begin
  SetChangeAttributeNames(Tag, AClass); // Frame/AWT
  Result:= FromTo(Attr, DelphiBounds + Delphi, JavaBounds + Java);
end;

function Java2DelphiValues(const DValue, JValue: string): string;
  var DCursor, JCursor, Prefix: string;
begin
  Prefix:= Copy(DValue, 1, 2);
  if Prefix = 'cr' then begin
    DCursor:= DelphiCursor;
    JCursor:= JavaCursor;
  end else
  // ToDo
  ;

  Result:= FromTo(JValue, JCursor, DCursor);
  if Result = '' then
    Result:= JValue;
end;

function Delphi2JavaValues(const Str: string): string;
begin // used to change attributes of type TColor
  Result:= FromTo(Str, DelphiValues + DelphiCursor, JavaValues + JavaCursor);
  if Result = '' then
    Result:= Str;
end;

function Java2DelphiColors(const Str: string): string;
begin // used to show color combo box
  Result:= FromTo(Str, JavaColors, DelphiColors);
  if Result = '' then
    Result:= 'clBtnFace';
end;

function Delphi2JavaColors(Str: string): string;
begin
  Str:= FromTo(Str, DelphiColors, JavaColors);
  if Copy(Str, 1, 3) = '$00'
    then Str:= '0x' + Copy(Str, 8, 2) + Copy(Str, 6, 2) + Copy(Str, 4, 2);
  if Str = '' then
    Str:= '(NONE)';
  Result:= Str;
end;

function TurnRGB(const Str: string): string;
begin
  Result:= Str;
  if Copy(Str, 1, 2) = '0x' then
    Result:= '0x' + Copy(Str, 7, 2) + Copy(Str, 5, 2) + Copy(Str, 3, 2);
end;

function TColorToString(Color: TColor): string;
begin
  var Str:= ColorToString(Color);
  Str:= Delphi2JavaColors(Str);
  if Copy(Str, 1, 2) = 'cl' then
    FmtStr(Str, '%s%.8x', [HexDisplayPrefix, Color]);
  if Copy(Str, 1, 2) = '$0' then
    Str:= '0x' + Copy(Str, 8, 2) + Copy(Str, 6, 2) + Copy(Str, 4, 2);
  Result:= Str;
end;

function ToJavaColor(Color: TColor): string;
begin
  var Col:= TColorToString(Color);
  if Copy(Col, 1, 2) = '0x'
    then Result:= 'new Color(' + Col + ')'
    else Result:= 'Color.' + Col;
end;

function Java2DelphiCursor(const Str: string): string;
begin
  Result:= FromTo(Str, JavaCursor, DelphiCursor);
end;

function Delphi2JavaCursor(const Str: string): string;
begin
  Result:= FromTo(Str, DelphiCursor, JavaCursor);
end;

initialization
  GSelectStrings:= TStringList.Create;

finalization
  GSelectStrings.Destroy;

end.
