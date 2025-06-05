unit UJavaScanner;

interface

uses Classes;

type

  TJavaScanner = class
  private
    FColumn: Integer; // current column
    FComment: string;
    FCommentLineE: Integer;
    FCommentLineS: Integer;
    FCompoundTokens: Boolean;
    FCurrPos: PChar;
    FGenericToken: Integer; // Pocket<Pocket<String>> versus a >> b
    FLastToken: string;
    FLastTokenColumn: Integer;
    FLastTokenLine: Integer;
    FLine: Integer; // current line, calculated by GetNextToken
    FScanStr: string;
    FStartPos: PChar;
    FStream: TMemoryStream;
    FToken: string;
    FTokenColumn: Integer;
    FTokenLine: Integer;
    FTokenTyp: string;
  public
    constructor Create;
    procedure Init(const Str: string); overload; // simple scanning of a string
    procedure Init(Stream: TStream); overload;
    destructor Destroy; override;
    function GetChar: Char;
    procedure EatWhiteSpace;
    function GetNextToken: string;
    function LookAheadToken: string; overload;
    function LookAheadToken(Tokens: string): string; overload;
    function LookDoubleAheadToken: string;
    function GetPackage(const Str: string): string;
    function GetGeneric: string; overload;
    function GetGeneric(Str: string): string; overload;
    function IsInterface(const Str: string): Boolean;
    function GetExtends: string;
    function SkipToken(const What: string): Boolean;
    procedure SkipTo(Chr: Char);
    procedure SkipPair(const Open, Close: string);
    procedure SkipPairTo(const Open, Close: string);
    procedure SkipPairStopAt(const Open, Close, StopAt: string);
    procedure SkipCondition;
    procedure ParseAnnotations;
    procedure ParseModifiers;
    function Empty: Boolean;
    function GetTypeArgument: string;
    function GetTypeName: string;
    function IsIdentifier(const Str: string): Boolean;
    function GetFilename: string;
    function GetFrameType: Integer;
    function NeedsJavaFX(Str: string): Boolean;

    property Comment: string read FComment write FComment;
    property CommentLineE: Integer read FCommentLineE write FCommentLineE;
    property CommentLineS: Integer read FCommentLineS write FCommentLineS;
    property CompoundTokens: Boolean read FCompoundTokens write FCompoundTokens;
    property CurrPos: PChar read FCurrPos;
    property LastToken: string read FLastToken;
    property LastTokenColumn: Integer read FLastTokenColumn;
    property LastTokenLine: Integer read FLastTokenLine;
    property Line: Integer read FLine;
    property StartPos: PChar read FStartPos write FStartPos;
    property Token: string read FToken write FToken;
    property TokenColumn: Integer read FTokenColumn;
    property TokenLine: Integer read FTokenLine;
    property TokenTyp: string read FTokenTyp;
  end;

implementation

uses
  SysUtils,
  Character,
  UUtils;

{ --- Scanner ------------------------------------------------------------------ }

constructor TJavaScanner.Create;
begin
  FLine := 1;
  FColumn := 1;
  FGenericToken := 0;
  FLastTokenLine := -1;
end;

function TJavaScanner.Empty: Boolean;
begin
  Result := (FCurrPos^ = #0);
end;

function TJavaScanner.GetChar: Char;
begin
  Result := FCurrPos^;
  if (FCurrPos^ = #10) or ((FCurrPos^ = #13) and ((FCurrPos + 1)^ <> #10)) then
  begin
    Inc(FLine);
    FColumn := 0;
  end;
  if Result <> #0 then
  begin
    Inc(FCurrPos);
    Inc(FColumn);
  end;
end;

procedure TJavaScanner.EatWhiteSpace;
var
  InComment, ContinueLastComment, State: Boolean;

  procedure EatOne;
  begin
    if InComment then
      FComment := FComment + GetChar
    else
      GetChar;
  end;

  procedure DelOne;
  begin
    GetChar;
  end;

  function EatWhite: Boolean;
  begin
    Result := False;
    while not CharInSet(FCurrPos^, [#0, #33 .. #255]) do
    begin
      Result := True;
      EatOne;
    end;
  end;

  function EatStarComment: Boolean;
  begin
    Result := True;
    while (not((FCurrPos^ = '*') and ((FCurrPos + 1)^ = '/'))) and
      (FCurrPos^ <> #0) do
      EatOne;
    ContinueLastComment := False;
    EatOne;
    EatOne;
  end;

  function EatSlashComment: Boolean;
  begin
    Result := True;
    while (FCurrPos^ <> #13) and (FCurrPos^ <> #10) and (FCurrPos^ <> #0) do
    begin
      Result := True;
      DelOne;
    end;
    ContinueLastComment := True;
    InComment := False;
    while CharInSet(FCurrPos^, [#13, #10]) do
      DelOne;
  end;

begin
  InComment := False;
  ContinueLastComment := False;
  State := True;
  FCommentLineS := -1;
  while State do
  begin
    State := False;
    if (FCurrPos^ = #10) or ((FCurrPos^ = #13) and ((FCurrPos + 1)^ = #10)) then
      ContinueLastComment := False;
    if not CharInSet(FCurrPos^, [#0, #33 .. #255]) then
      State := EatWhite;
    if (FCurrPos^ = '/') and ((FCurrPos + 1)^ = '*') then
    begin
      FCommentLineS := FLine;
      InComment := True;
      FComment := '';
      EatOne;
      EatOne;
      State := EatStarComment;
      InComment := False;
      FCommentLineE := FLine;
    end;
    if (FCurrPos^ = '/') and ((FCurrPos + 1)^ = '/') then
    begin
      if not ContinueLastComment then
        FComment := ''
      else
        FComment := FComment + #13#10;
      EatOne;
      EatOne; // Skip the double slashes
      InComment := True;
      State := EatSlashComment;
      InComment := False;
    end;
  end;
end;

function TJavaScanner.GetNextToken: string;

  procedure AddOne;
  begin
    FToken := FToken + GetChar;
  end;

begin
  FLastToken := FToken;
  FToken := '';
  FTokenTyp := '';
  EatWhiteSpace;
  if FLastTokenLine = -1 then
  begin
    FLastTokenLine := FLine;
    FLastTokenColumn := FColumn;
  end
  else
  begin
    FLastTokenLine := FTokenLine;
    FLastTokenColumn := FTokenColumn;
  end;
  FTokenLine := FLine;
  FTokenColumn := FColumn;
  if FCurrPos^ = '"' then
  begin // Parse String
    AddOne;
    while not CharInSet(FCurrPos^, ['"', #0]) do
    begin
      if ((FCurrPos^ = '\') and CharInSet((FCurrPos + 1)^, ['"', '\'])) then
        AddOne;
      AddOne;
    end;
    AddOne;
    FTokenTyp := 'String';
  end
  else if FCurrPos^ = '''' then
  begin // Parse char
    AddOne;
    while not CharInSet(FCurrPos^, ['''', #0]) do
    begin
      if ((FCurrPos^ = '\') and CharInSet((FCurrPos + 1)^, ['''', '\'])) then
        AddOne;
      AddOne;
    end;
    AddOne;
    FTokenTyp := 'int'; // insteas of char
  end
  else if FCurrPos^.IsLetter or (Pos(FCurrPos^, '_$') > 0) then
  begin
    AddOne;
    while True do
    begin
      while FCurrPos^.IsLetterOrDigit or (FCurrPos^ = '_') do
        AddOne;
      if FCompoundTokens and (FCurrPos^ = '.') then
      begin
        AddOne;
        Continue;
      end;
      Break;
    end;
    while FCurrPos^ = '[' do
    begin
      var
      Bracket := 1;
      AddOne;
      while (Bracket > 0) and (FCurrPos^ <> #0) and (FCurrPos^ <> '}') and
        (FCurrPos^ <> ';') do
      begin
        if FCurrPos^ = '[' then
          Inc(Bracket)
        else if FCurrPos^ = ']' then
          Dec(Bracket);
        AddOne;
      end;
    end;
  end
  else if CharInSet(FCurrPos^, [';', '{', '}', '(', ')', ',', ':', '.', '?',
    '@']) then
  begin
    // single chars to test
    AddOne;
    if FCurrPos^ = ':' then // double colon operator
      AddOne
    else if (FCurrPos^ = '.') and ((FCurrPos + 1)^ = '.') then
    begin
      AddOne;
      AddOne;
    end;
  end
  else if FCurrPos^ = '=' then
  begin // single chars to test
    AddOne;
    while FCurrPos^ = '=' do
      AddOne;
  end
  else if FCurrPos^ = '[' then
  begin // loose brackets
    AddOne;
    EatWhiteSpace;
    if FCurrPos^ = ']' then
      AddOne;
  end
  else if CharInSet(FCurrPos^, ['*', '/', '%', '+', '-', '<', '>', '&', '^',
    '|']) then
  begin // operators
    AddOne;
    if FGenericToken = 0 then
      while CharInSet(FCurrPos^, ['<', '>']) do
        AddOne;
    if CharInSet(FCurrPos^, ['=', '+', '-']) then
      AddOne;
  end
  else if CharInSet(FCurrPos^, ['0' .. '9']) then
  begin
    AddOne;
    FTokenTyp := 'int';
    while CharInSet(FCurrPos^, ['0' .. '9', '.', 'E', 'e']) do
    begin
      AddOne;
      FTokenTyp := 'double';
    end;
  end
  else
    while not CharInSet(FCurrPos^, [#0, #9, #10, #12, #13, #32, ',', '=', ';',
      '{', '}', '(', ')', '"', '''']) do
      AddOne;
  Result := FToken;
end;

function TJavaScanner.LookAheadToken: string;
var
  SaveCurrPos: PChar;
  SaveLastToken: string;
  SaveToken: string;
  SaveLine: Integer;
begin
  SaveCurrPos := FCurrPos;
  SaveLastToken := FLastToken;
  SaveToken := FToken;
  SaveLine := FLine;
  Result := GetNextToken;
  FLastToken := SaveLastToken;
  FToken := SaveToken;
  FCurrPos := SaveCurrPos;
  FLine := SaveLine;
end;

function TJavaScanner.LookAheadToken(Tokens: string): string;
var
  SaveCurrPos: PChar;
  SaveLastToken: string;
  SaveToken: string;
  SaveLine, Posi: Integer;
begin
  Result := '';
  SaveCurrPos := FCurrPos;
  SaveLastToken := FLastToken;
  SaveToken := FToken;
  SaveLine := FLine;
  repeat
    Result := GetNextToken;
    Posi := Pos(Result, Tokens);
  until (Posi > 0) or Empty;
  FLastToken := SaveLastToken;
  FToken := SaveToken;
  FCurrPos := SaveCurrPos;
  FLine := SaveLine;
end;

function TJavaScanner.LookDoubleAheadToken: string;
var
  SaveCurrPos: PChar;
  SaveLastToken: string;
  SaveToken: string;
  SaveLine: Integer;
begin
  SaveCurrPos := FCurrPos;
  SaveLastToken := FLastToken;
  SaveToken := FToken;
  SaveLine := FLine;
  GetNextToken;
  Result := GetNextToken;
  FLastToken := SaveLastToken;
  FToken := SaveToken;
  FCurrPos := SaveCurrPos;
  FLine := SaveLine;
end;

function TJavaScanner.GetPackage(const Str: string): string;
begin
  Result := '';
  FCurrPos := PChar(Str);
  GetNextToken;
  if FToken = 'package' then
  begin
    GetNextToken;
    while (FToken <> ';') and (FToken <> '') do
    begin
      Result := Result + FToken;
      GetNextToken;
    end;
  end;
end;

function TJavaScanner.IsInterface(const Str: string): Boolean;
begin
  FCurrPos := PChar(Str);
  GetNextToken;
  if FToken = 'package' then
  begin
    GetNextToken;
    SkipToken(';');
  end;

  while FToken = 'import' do
  begin
    SkipTo(';');
    GetNextToken;
  end;
  ParseAnnotations;
  ParseModifiers;
  Result := FToken = 'interface';
end;

function TJavaScanner.GetExtends: string;
begin
  Result := '';
  GetNextToken;
  if FToken = 'package' then
  begin
    GetNextToken;
    SkipToken(';');
  end;

  while (FToken = 'import') or (FToken = ';') do
  begin
    SkipTo(';');
    GetNextToken;
  end;
  ParseAnnotations;
  ParseModifiers;
  if FToken = 'class' then
  begin
    GetNextToken;
    GetNextToken;
    if FToken = '<' then
      GetGeneric;
    if FToken = 'extends' then
      Result := GetNextToken;
  end;
end;

destructor TJavaScanner.Destroy;
begin
  inherited;
  FreeAndNil(FStream);
end;

procedure TJavaScanner.Init(const Str: string);
begin
  FScanStr := Str;
  FCurrPos := PChar(FScanStr);
  FStartPos := PChar(FScanStr);
  FCompoundTokens := False;
end;

procedure TJavaScanner.Init(Stream: TStream);
begin
  FreeAndNil(FStream);
  FStream := TMemoryStream(Stream);
  FCurrPos := FStream.Memory;
  FStartPos := FStream.Memory;
  FCompoundTokens := True;
end;

function TJavaScanner.SkipToken(const What: string): Boolean;
begin
  Result := False;
  GetNextToken;
  if FToken = What then
  begin
    GetNextToken;
    Result := True;
  end;
end;

procedure TJavaScanner.SkipTo(Chr: Char);
begin
  while (FToken <> Chr) and (FToken <> '') do
  begin
    if FToken = '{' then
      SkipPairTo('{', '}')
    else if FToken = '(' then
      SkipPairTo('(', ')')
    else
      GetNextToken;
  end;
end;

procedure TJavaScanner.ParseAnnotations;
begin
  while FToken = '@' do
  begin
    GetNextToken;
    GetNextToken;
    if FToken = '(' then
      SkipPair('(', ')');
  end;
end;

procedure TJavaScanner.ParseModifiers;
(*
  ModifiersOpt:
  { Modifier }

  Modifier:
  public
  protected
  private
  static
  abstract
  final
  native
  synchronized
  transient
  volatile
  strictfp
  < T1, T2, ... >
*)
begin
  while True do
  begin
    if FToken = 'public' then
      GetNextToken
    else if FToken = 'protected' then
      GetNextToken
    else if FToken = 'private' then
      GetNextToken
    else if FToken = 'static' then
      GetNextToken
    else if FToken = 'abstract' then
      GetNextToken
    else if FToken = 'final' then
      GetNextToken
    else if FToken = '<' then
    begin
      SkipPair('<', '>');
      Break;
    end
    else if (FToken = 'native') or (FToken = 'synchronized') or
      (FToken = 'transient') or (FToken = 'volatile') or (FToken = 'strictfp')
    then
      GetNextToken
    else
      Break;
  end;
end;

procedure TJavaScanner.SkipPair(const Open, Close: string);
begin
  if Open = '<' then
    Inc(FGenericToken);
  var
  Count := 1;
  while (Count > 0) and (FToken <> '') do
  begin
    GetNextToken;
    if FToken = Open then
      Inc(Count)
    else if FToken = Close then
      Dec(Count);
  end;
  GetNextToken;
  if Open = '<' then
    Dec(FGenericToken);
end;

procedure TJavaScanner.SkipPairStopAt(const Open, Close, StopAt: string);
begin
  if Open = '<' then
    Inc(FGenericToken);
  var
  Count := 1;
  while (Count > 0) and (FToken <> '') and (Pos(FToken, StopAt) = 0) do
  begin
    GetNextToken;
    if FToken = Open then
      Inc(Count)
    else if FToken = Close then
      Dec(Count);
  end;
  if (Pos(FToken, StopAt) = 0) then
    GetNextToken;
  if Open = '<' then
    Dec(FGenericToken);
end;

procedure TJavaScanner.SkipPairTo(const Open, Close: string);
begin
  if Open = '<' then
    Inc(FGenericToken);
  var
  Count := 1;
  while (Count > 0) and (FToken <> '') do
  begin
    GetNextToken;
    if FToken = Open then
      Inc(Count)
    else if FToken = Close then
      Dec(Count);
  end;
  if Open = '<' then
    Dec(FGenericToken);
end;

procedure TJavaScanner.SkipCondition;
begin
  var
  Count := 1;
  while (Count > 0) and (FToken <> '') do
  begin
    GetNextToken;
    if FToken = '(' then
      Inc(Count)
    else if FToken = ')' then
      Dec(Count)
    else if (FToken = '{') or (FToken = '}') then
      Exit;
  end;
  GetNextToken;
end;

function TJavaScanner.GetGeneric(Str: string): string;
begin
  var
  Depth := 1;
  Str := Str + FToken;
  repeat
    GetNextToken;
    if (Pos(Str[Length(Str)], '<.[') > 0) or (FToken = ',') or
      (Copy(FToken, Length(FToken), 1) = '>') then
      Str := Str + FToken
    else
      Str := Str + ' ' + FToken;
    for var I := 1 to Length(FToken) do
    begin
      if FToken[I] = '<' then
        Inc(Depth);
      if FToken[I] = '>' then
        Dec(Depth);
    end;
  until (Depth = 0) or (FToken = '');
  GetNextToken;
  Result := Str;
end;

function TJavaScanner.GetGeneric: string;
begin
  var
  Str := GetGeneric('');
  Result := Copy(Str, 2, Length(Str) - 2);
end;

function TJavaScanner.GetTypeArgument: string;
(*
  TypeArgument:
  ReferenceType
  ? [( extends |super ) ReferenceType]
*)
begin
  if FToken = '?' then
  begin
    Result := '?';
    GetNextToken;
    if (FToken = 'extends') or (FToken = 'super') then
    begin
      Result := Result + ' ' + FToken;
      GetNextToken;
    end;
    Result := Result + ' ' + GetTypeName;
  end
  else
    Result := GetTypeName;
end;

function TJavaScanner.GetTypeName: string;
(*
  UnannPrimitiveType:  byte|short|int|long|char|float|double|boolean  (void)
  UnannReferenceType:
  UnannClassOrInterfaceType
  UnannTypeVariable (Identifier)
  UnannArrayType    (UnannType with []s)

  UnannClassOrInterfaceType (=UnannClassType):
  TypeIdentifier [TypeArguments]
  PackageName . {Annotation} TypeIdentifier [TypeArguments]
  UnannClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]

  TypeArguments:
  < TypeArgument {, TypeArgument} >
*)

begin
  Inc(FGenericToken);
  Result := FToken;
  GetNextToken;
  while FToken <> '' do
  begin
    if FToken = '<' then
    begin
      Result := Result + '<';
      GetNextToken;
      Result := Result + GetTypeArgument;
      while FToken = ',' do
      begin
        GetNextToken;
        Result := Result + ', ' + GetTypeArgument;
      end;
    end
    else if FToken = '.' then
    begin
      Result := Result + FToken;
      GetNextToken;
      Result := Result + GetTypeName;
    end
    else if FToken = '>' then
    begin
      Result := Result + FToken;
      GetNextToken;
      Break;
    end
    else if FToken = '...' then
    begin
      Result := Result + FToken;
      GetNextToken;
      Break;
    end
    else
      Break;
  end;
  while FToken = '[]' do
  begin
    Result := Result + FToken;
    GetNextToken;
  end;
  Dec(FGenericToken);
end;

function TJavaScanner.IsIdentifier(const Str: string): Boolean;
begin
  Result := False;
  if Length(Str) = 0 then
    Exit;
  if not(Str[1].IsLetter or (Pos(Str[1], '_$') > 0)) then
    Exit;
  for var I := 2 to Length(Str) do
    if not(Str[I].IsLetterOrDigit or (Str[I] = '_')) then
      Exit;
  Result := True;
end;

function TJavaScanner.GetFilename: string;
begin
  GetNextToken;
  ParseAnnotations;
  ParseModifiers;
  GetTypeName;
  if IsIdentifier(FToken) then
    Result := FToken
  else
    Result := '';
end;

function TJavaScanner.GetFrameType: Integer;
begin
  FCompoundTokens := True;
  var
  Typ := GetShortType(GetExtends);
  if Typ = 'Application' then
    Result := 8
  else if Typ = 'JApplet' then
    Result := 7
  else if Typ = 'JDialog' then
    Result := 6
  else if Typ = 'JFrame' then
    Result := 5
  else if Typ = 'Applet' then
    Result := 4
  else if Typ = 'Dialog' then
    Result := 3
  else if Typ = 'Frame' then
    Result := 2
  else
    Result := 1;
end;

function TJavaScanner.NeedsJavaFX(Str: string): Boolean;
begin
  Init(Str);
  Result := False;
  GetNextToken;
  if FToken = 'package' then
  begin
    GetNextToken;
    SkipToken(';');
  end;

  while FToken = 'import' do
  begin
    GetNextToken;
    if FToken = 'javafx' then
      Exit(True);
    SkipTo(';');
    GetNextToken;
  end;
end;

end.
