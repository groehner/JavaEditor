unit UJavaScanner;

interface

  uses Classes;

  type

  TJavaScanner = class
  public
    Stream: TMemoryStream;
    Comment: string; // Accumulated comment string used for documentation of entities.
    CommentLineS: integer;
    CommentLineE: integer;
    Line: Integer;               // current line, calculated by GetNextToken
    Column: integer;             // current column
    LastTokenLine: integer;
    LastTokenColumn: integer;
    TokenLine: integer;
    TokenColumn: integer;
    StartPos: PChar;
    CurrPos: PChar;
    ScanStr: string;
    LastToken: string;
    Token: string;
    TokenTyp: string;
    CompoundTokens: boolean;
    GenericToken: integer;   // Pocket<Pocket<String>> versus a >> b
    constructor create;
    procedure Init(const s: string); overload;  // simple scanning of a string
    procedure Init(aStream: TStream); overload;
    destructor Destroy; override;

    function GetChar: char;
    procedure EatWhiteSpace;
    function GetNextToken: string;
    function LookAheadToken: string; overload;
    function LookAheadToken(Tokens: string): string; overload;
    function LookDoubleAheadToken: string;
    function getPackage(const s: string): string;
    function GetGeneric: string; overload;
    function GetGeneric(s: string): string; overload;
    function IsInterface(const s: string): boolean;
    function getExtends: string;
    function SkipToken(const what: string): Boolean;
    procedure SkipTo(ch: char);
    procedure SkipPair(const open, close: string);
    procedure SkipPairTo(const open, close: string);
    procedure SkipPairStopAt(const open, close, stopAt: string);
    procedure SkipCondition;
    procedure ParseAnnotations;
    procedure ParseModifiers;
    function empty: boolean;
    function GetTypeArgument: string;
    function GetTypeName: string;
    function IsIdentifier(const s: string): boolean;
    function getFilename: string;
    function GetFrameType: integer;
    function NeedsJavaFX(s: string): boolean;
  end;

implementation

uses SysUtils, Character, UUtils;

{--- Scanner ------------------------------------------------------------------}

constructor TJavaScanner.create;
begin
  Line:= 1;
  Column:= 1;
  GenericToken:= 0;
  LastTokenLine:= -1;
end;

function TJavaScanner.empty: boolean;
begin
  Result:= (CurrPos^ = #0);
end;

function TJavaScanner.GetChar: char;
begin
  Result:= CurrPos^;
  if (CurrPos^ = #10) or ((CurrPos^ = #13) and ((CurrPos+1)^ <> #10)) then begin
    inc(Line);
    Column:= 0;
  end;
  if Result <> #0 then begin
    inc(CurrPos);
    inc(Column);
  end;
end;

procedure TJavaScanner.EatWhiteSpace;
var
  inComment, continueLastComment, State: Boolean;

  procedure EatOne;
  begin
    if inComment
      then Comment:= Comment + GetChar
      else GetChar;
  end;

  procedure DelOne;
  begin
    GetChar;
  end;

  function EatWhite: Boolean;
  begin
    Result:= False;
    while not CharInSet(CurrPos^, [#0, #33..#255]) do
    begin
      Result:= True;
      EatOne;
    end;
  end;

  function EatStarComment: Boolean;
  begin
    Result:= True;
    while (not ((CurrPos^ = '*') and ((CurrPos + 1)^ = '/'))) and (CurrPos^ <> #0) do
      EatOne;
    continueLastComment:= False;
    EatOne;
    EatOne;
  end;

  function EatSlashComment: Boolean;
  begin
    Result:= True;
    while (CurrPos^ <> #13) and (CurrPos^ <> #10) and (CurrPos^ <> #0) do begin
      Result:= True;
      DelOne;
    end;
    continueLastComment:= True;
    inComment:= False;
    while CharInSet(CurrPos^, [#13, #10]) do
      DelOne;
  end;

begin
  inComment:= False;
  continueLastComment:= False;
  State:= True;
  CommentLineS:= -1;
  while State do begin
    State:= False;
    if (CurrPos^ = #10) or ((CurrPos^ = #13) and ((CurrPos + 1)^ = #10)) then
      continueLastComment:= False;
    if not CharInSet(CurrPos^, [#0, #33..#255]) then State:= EatWhite;
    if (CurrPos^ = '/') and ((CurrPos + 1)^ = '*') then begin
      CommentLineS:= Line;
      inComment:= true;
      Comment:= '';
      EatOne;
      EatOne;
      State:= EatStarComment;
      inComment:= False;
      CommentLineE:= Line;
    end;
    if (CurrPos^ = '/') and ((CurrPos + 1)^ = '/') then begin
      if not continueLastComment
        then Comment:= ''
        else Comment:= Comment + #13#10;
      EatOne;
      EatOne; // Skip the double slashes
      inComment:= true;
      State:= EatSlashComment;
      inComment:= false;
    end;
  end;
end;

function TJavaScanner.GetNextToken: string;

  procedure AddOne;
  begin
    Token:= Token + GetChar;
  end;

begin
  LastToken:= Token;
  Token:= '';
  TokenTyp:= '';
  EatWhiteSpace;
  if LastTokenLine = -1 then begin
    LastTokenLine:= Line;
    LastTokenColumn:= Column;
  end else begin
    LastTokenLine:= TokenLine;
    LastTokenColumn:= TokenColumn;
  end;
  TokenLine:= Line;
  TokenColumn:= Column;
  if CurrPos^ = '"' then begin // Parse String
    AddOne;
    while not CharInSet(CurrPos^, ['"', #0]) do begin
      if ((CurrPos^ = '\') and CharInSet((CurrPos + 1)^, ['"', '\'])) then AddOne;
      AddOne;
    end;
    AddOne;
    TokenTyp:= 'String';
  end else if CurrPos^ = '''' then begin // Parse char
    AddOne;
    while not CharInSet(CurrPos^, ['''', #0]) do begin
      if ((CurrPos^ = '\') and CharInSet((CurrPos + 1)^, ['''', '\'])) then AddOne;
      AddOne;
    end;
    AddOne;
    TokenTyp:= 'int'; // insteas of char
  end else if CurrPos^.isLetter or (Pos(CurrPos^, '_$') > 0) then begin
    AddOne;
    while true do begin
      while CurrPos^.isLetterOrDigit or (CurrPos^ = '_') do
        AddOne;
      if CompoundTokens and (CurrPos^ = '.') then begin
        AddOne;
        Continue;
      end;
      Break;
    end;
    while CurrPos^ = '[' do begin
      var bracket:= 1;
      AddOne;
      while (bracket > 0) and (CurrPos^ <> #0) and (CurrPos^ <> '}') and (CurrPos^ <> ';') do begin
        if CurrPos^ = '['
          then inc(bracket)
        else if CurrPos^ = ']'
          then dec(bracket);
        AddOne;
      end;
    end;
  end
  else if CharInSet(CurrPos^, [';', '{', '}', '(', ')', ',', ':', '.', '?', '@']) then begin
    //single chars to test
    AddOne;
    if CurrPos^= ':' then  // double colon operator
      AddOne
    else if (CurrPos^= '.') and ((CurrPos+1)^ = '.') then begin
      AddOne;
      AddOne;
    end
  end else if CurrPos^ = '=' then begin //single chars to test
    AddOne;
    while CurrPos^ = '=' do AddOne;
  end
  else if CurrPos^ = '[' then begin  // loose brackets
    AddOne;
    EatWhitespace;
    if CurrPos^ = ']' then AddOne;
  end
  else if CharInSet(CurrPos^, ['*', '/', '%', '+', '-', '<', '>', '&', '^', '|']) then begin // operators
    AddOne;
    if GenericToken = 0 then
      while CharInSet(CurrPos^, ['<', '>']) do AddOne;
    if CharInSet(CurrPos^,  ['=', '+', '-']) then AddOne;
  end else if CharInSet(CurrPos^, ['0'..'9']) then begin
    AddOne;
    TokenTyp:= 'int';
    while CharInSet(CurrPos^, ['0'..'9', '.', 'E', 'e']) do begin
      AddOne;
      TokenTyp:= 'double';
    end;
  end else
    while not CharInSet(CurrPos^, [#0, #9, #10, #12, #13, #32, ',', '=', ';', '{', '}', '(', ')', '"', '''']) do
      AddOne;
  Result:= Token;
  //FJava.Memo1.Lines.Add( TimeToStr(Now) + ' Line: ' + IntTostr(LIne) + '  Token: ' + Result);
end;

function TJavaScanner.LookAheadToken: string;
  var SaveCurrPos: PChar;
      SaveLastToken: string;
      SaveToken: string;
      SaveLine: integer;
begin
  SaveCurrPos:= CurrPos;
  SaveLastToken:= LastToken;
  SaveToken:= Token;
  SaveLine:= Line;
  Result:= GetNextToken;
  LastToken:= SaveLastToken;
  Token:= SaveToken;
  CurrPos:= SaveCurrPos;
  Line:= SaveLine;
end;

function TJavaScanner.LookAheadToken(Tokens: string): string;
  var SaveCurrPos: PChar;
      SaveLastToken: string;
      SaveToken: string;
      SaveLine, p: integer;
begin
  Result:= '';
  SaveCurrPos:= CurrPos;
  SaveLastToken:= LastToken;
  SaveToken:= Token;
  SaveLine:= Line;
  repeat
    Result:= getNextToken;
    p:= Pos(Result, Tokens);
  until (p > 0) or empty;
  LastToken:= SaveLastToken;
  Token:= SaveToken;
  CurrPos:= SaveCurrPos;
  Line:= SaveLine;
end;

function TJavaScanner.LookDoubleAheadToken: string;
  var SaveCurrPos: PChar;
      SaveLastToken: string;
      SaveToken: string;
      SaveLine: integer;
begin
  SaveCurrPos:= CurrPos;
  SaveLastToken:= LastToken;
  SaveToken:= Token;
  SaveLine:= Line;
  GetNextToken;
  Result:= GetNextToken;
  LastToken:= SaveLastToken;
  Token:= SaveToken;
  CurrPos:= SaveCurrPos;
  Line:= SaveLine;
end;

function TJavaScanner.getPackage(const s: string): string;
begin
  Result:= '';
  CurrPos:= PChar(s);
  GetNextToken;
  if Token = 'package' then begin
    GetNextToken;
    while (Token <> ';') and (Token <> '') do begin
      Result:= Result + Token;
      GetNextToken;
    end;
  end;
end;

function TJavaScanner.IsInterface(const s: string): boolean;
begin
  CurrPos:= PChar(s);
  GetNextToken;
  if Token = 'package' then begin
    GetNextToken;
    SkipToken(';');
  end;

  while Token = 'import' do begin
    SkipTo(';');
    GetNextToken;
  end;
  ParseAnnotations;
  ParseModifiers;
  Result:= Token = 'interface';
end;

function TJavaScanner.getExtends: string;
begin
  Result:= '';
  GetNextToken;
  if Token = 'package' then begin
    GetNextToken;
    SkipToken(';');
  end;

  while (Token = 'import') or (Token = ';') do begin
    SkipTo(';');
    GetNextToken;
  end;
  ParseAnnotations;
  ParseModifiers;
  if Token = 'class' then begin
    GetNextToken;
    GetNextToken;
    if Token = '<' then
      GetGeneric;
    if Token = 'extends' then
      Result:= GetNextToken;
  end;
end;

destructor TJavaScanner.Destroy;
begin
  inherited;
  FreeAndNil(Stream);
end;

procedure TJavaScanner.Init(const s: string);
begin
  ScanStr:= s;
  CurrPos := PChar(ScanStr);
  StartPos:= PChar(ScanStr);
  CompoundTokens:= false;
end;

procedure TJavaScanner.Init(aStream: TStream);
begin
  if Assigned(Stream) then
    FreeAndNil(Stream);
  Stream:= TMemoryStream(aStream);
  CurrPos:= Stream.Memory;
  StartPos:= Stream.Memory;
  CompoundTokens:= true;
end;

function TJavaScanner.SkipToken(const what: string): Boolean;
begin
  Result:= False;
  GetNextToken;
  if Token = what then begin
    GetNextToken;
    Result:= True;
  end;
end;

procedure TJavaScanner.SkipTo(ch: char);
begin
  while (Token <> ch) and (Token <> '') do begin
    if Token = '{'
      then SkipPairTo('{', '}')
    else if Token = '('
      then SkipPairTo('(', ')')
    else GetNextToken;
  end;
end;

procedure TJavaScanner.ParseAnnotations;
begin
  while Token = '@' do begin
    GetNextToken;
    GetNextToken;
    if Token = '(' then
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
  while True do begin
    if Token = 'public' then
      GetNextToken
    else if Token = 'protected' then
      GetNextToken
    else if Token = 'private' then
      GetNextToken
    else if Token = 'static' then
      GetNextToken
    else if Token = 'abstract' then
      GetNextToken
    else if Token = 'final' then
      GetNextToken
    else if Token = '<' then begin
      SkipPair('<', '>');
      break
    end
    else if (Token = 'native') or (Token = 'synchronized') or (Token = 'transient') or
            (Token = 'volatile') or (Token = 'strictfp') then
           GetNextToken
    else
      Break;
  end;
end;

procedure TJavaScanner.SkipPair(const open, close: string);
begin
  if open = '<' then Inc(GenericToken);
  var Count:= 1;
  while (Count > 0) and (Token <> '') do begin
    GetNextToken;
    if Token = open
      then Inc(Count)
    else if Token = close
      then Dec(Count);
  end;
  GetNextToken;
  if open = '<' then Dec(GenericToken);
end;

procedure TJavaScanner.SkipPairStopAt(const open, close, stopAt: string);
begin
  if open = '<' then Inc(GenericToken);
  var Count:= 1;
  while (Count > 0) and (Token <> '') and (Pos(Token, stopAt) = 0) do begin
    GetNextToken;
    if Token = open
      then Inc(Count)
    else if Token = close
      then Dec(Count);
  end;
  if (Pos(Token, stopAt) = 0) then
    GetNextToken;
  if open = '<' then Dec(GenericToken);
end;

procedure TJavaScanner.SkipPairTo(const open, close: string);
begin
  if open = '<' then Inc(GenericToken);
  var Count:= 1;
  while (Count > 0) and (Token <> '') do begin
    GetNextToken;
    if Token = open
      then Inc(Count)
    else if Token = close
      then Dec(Count);
  end;
  if open = '<' then Dec(GenericToken);
end;

procedure TJavaScanner.SkipCondition;
begin
  var Count:= 1;
  while (Count > 0) and (Token <> '') do begin
    GetNextToken;
    if Token = '('
      then Inc(Count)
    else if Token = ')'
      then Dec(Count)
    else if (Token = '{') or (Token = '}')  then
      exit;
  end;
  GetNextToken;
end;

function TJavaScanner.GetGeneric(s: string): string;
begin
  var Depth:= 1;
  s:= s + Token;
  repeat
    GetNextToken;
    if (Pos(s[length(s)], '<.[') > 0) or (Token = ',') or (copy(Token, length(Token),1) = '>')
      then s:= s + Token
      else s:= s + ' ' + Token;
    for var i:= 1 to length(Token) do begin
      if Token[i] = '<' then inc(Depth);
      if Token[i] = '>' then dec(Depth);
    end;
  until (Depth = 0) or (Token = '');
  GetNextToken;
  Result:= s;
end;

function TJavaScanner.GetGeneric: string;
begin
  var s:= getGeneric('');
  Result:= copy(s, 2, length(s)-2);
end;

function TJavaScanner.GetTypeArgument: string;
(*
  TypeArgument:
    ReferenceType
    ? [( extends |super ) ReferenceType]
*)
begin
  if Token = '?' then begin
    Result:= '?';
    GetNextToken;
    if (Token = 'extends') or (Token = 'super') then begin
      Result:= Result + ' ' + Token;
      GetNextToken;
    end;
    Result:= Result + ' ' + GetTypeName;
  end else
    Result:= GetTypeName;
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
  inc(GenericToken);
  Result:= Token;
  GetNextToken;
  while Token <> '' do begin
    if Token = '<' then begin
      Result:= Result + '<';
      GetNextToken;
      Result:= Result + GetTypeArgument;
      while Token = ',' do begin
        GetNextToken;
        Result:= Result + ', ' + GetTypeArgument;
      end;
    end else if Token = '.' then begin
      Result:= Result + Token;
      GetNextToken;
      Result:= Result + GetTypeName;
    end else if Token = '>' then begin
      Result:= Result + Token;
      GetNextToken;
      break;
    end else if Token = '...' then begin
      Result:= Result + Token;
      GetNextToken;
      break;
    end else
      break;
  end;
  while Token = '[]' do begin
    Result:= Result + Token;
    GetNextToken;
  end;
  dec(GenericToken);
end;

function TJavaScanner.IsIdentifier(const s: string): boolean;
begin
  Result:= false;
  if Length(s) = 0 then exit;
  if not (s[1].isLetter or (Pos(s[1], '_$') > 0)) then
    exit;
  for var i:= 2 to length(s) do
    if not (s[i].isLetterOrDigit or (s[i] = '_')) then
      exit;
  Result:= true;
end;

function TJavaScanner.getFilename: string;
begin
  getNextToken;
  ParseAnnotations;
  ParseModifiers;
  GetTypeName;
  Result:= Token;
end;

function TJavaScanner.GetFrameType: integer;
begin
  CompoundTokens:= true;
  var Typ:= GetShortType(getExtends);
  if Typ = 'Application' then Result:= 8 else
  if Typ = 'JApplet'     then Result:= 7 else
  if Typ = 'JDialog'     then Result:= 6 else
  if Typ = 'JFrame'      then Result:= 5 else
  if Typ = 'Applet'      then Result:= 4 else
  if Typ = 'Dialog'      then Result:= 3 else
  if Typ = 'Frame'       then Result:= 2 else
                              Result:= 1;
end;

function TJavaScanner.NeedsJavaFX(s: string): boolean;
begin
  Init(s);
  Result:= false;
  GetNextToken;
  if Token = 'package' then begin
    GetNextToken;
    SkipToken(';');
  end;

  while Token = 'import' do begin
    GetNextToken;
    if Token = 'javafx' then
      exit(true);
    SkipTo(';');
    GetNextToken;
  end;
end;


end.
