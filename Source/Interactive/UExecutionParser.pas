unit UExecutionParser;

interface

uses UJavaParser, UExecution;

type
  TExecutionParser = class(TJavaParser)
  private
    ExecutionList: TExecutionList;
    procedure ExecuteVariableObjectArrayCreation(Typename: string; const Ident: string);
    procedure ExecuteEqualAssignment(const Variable: string);
    procedure ExecuteDeclaration(Typename: string; const Ident: string);
    procedure ExecuteInvalidStatement;
    procedure ExecuteAssignment(const Variable: string);
    procedure ExecuteUnnamedObjectCreation;
    procedure ExecuteStatement(const Kind: string);
    procedure ExecuteMethodCall(const Call: string);
    procedure ExecuteExpression(const aToken: string);
    procedure ExecuteTerm(E: TExecutionLine);
  public
    procedure PrepareExecute(const s: string; EL: TExecutionList);
  end;

implementation

uses Contnrs, Classes, SysUtils, UModel, UUtils, UComJava1, UConfiguration;

procedure TExecutionParser.PrepareExecute(const s: string; EL: TExecutionList);
  var TypeName, Ident: string;
begin
  Scanner.init(s);
  Scanner.CompoundTokens:= true;
  ExecutionList:= EL;
  ExecutionList.Clear;
  GetNextToken;
  while True do begin
    ParseModifiers; // visibility and others
    if Token = '' then
      break
    else if (Token = ';') or (Token = '}') then begin
      Scanner.StartPos:= Scanner.CurrPos;
      GetNextToken
    end else if Token = 'new' then
      ExecuteUnnamedObjectCreation
    else if IsStatementBegin(Token) or (Token = '{') then
      ExecuteStatement(Token)
    else if Token = '(' then
      ExecuteExpression(Token)
    else if IsTypename(Token) then begin
      Typename:= GetTypeName;
      if Token = '(' then
        ExecuteMethodCall(Typename)
      else if Token = '=' then
        ExecuteEqualAssignment(Typename)
      else if IsAssignmentOperator(Token) or (Token = '++') or (Token = '--') then
        ExecuteAssignment(Typename)
      else begin
        Ident:= Token;
        if IsIdentifier(Ident) then begin
          SwapArrFromTo(Ident, Typename);
          GetNextToken;
          if Token = '=' then
            ExecuteVariableObjectArrayCreation(Typename, Ident)
          else if (Token = '') or (Token = ';') or (Token = ',') then
            ExecuteDeclaration(Typename, Ident)
          else
            ExecuteInvalidStatement;
          while Token = ',' do begin
            GetNextToken;
            if IsIdentifier(Token) then begin
              Ident:= Token;
              GetNextToken;
              if Token = '=' then
                ExecuteVariableObjectArrayCreation(Typename, Ident)
              else if (Token = '') or (Token = ';') or (Token = ',') then
                ExecuteDeclaration(Typename, Ident)
              else
                ExecuteInvalidStatement;
            end else
              ExecuteInvalidStatement;
          end
        end else
          if IsSimpleType(Typename) or isSimpleType(WithoutArray(Typename))
            then ExecuteInvalidStatement
            else ExecuteExpression(Typename + ' ' + Token);
      end
    end else
      ExecuteExpression(Token);
    getNextToken;
  end;
end;

procedure TExecutionParser.ExecuteExpression(const aToken: string);
  var E: TExecutionLine; Start: PChar; s: string;
begin
  E:= TExecutionLine.Create('', Expression);
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(s, Start, Scanner.CurrPos - Start);
  E.Value:= aToken + s;
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteVariableObjectArrayCreation(Typename: string; const Ident: string);
  var E: TExecutionLine; Start: PChar; s: string; p: integer;
begin
  // Typename Identifier = [Value | new constructor]
  E:= TExecutionLine.Create(Ident);
  Start:= Scanner.CurrPos;
  GetNextToken;
  if Token = 'new'
    then E.Kind:= ObjectCreation
  else if Token = '{'
    then E.Kind:= ArrayDeclaration
    else E.Kind:= VariableCreation;

  s:= getImportName(Typename);
  if s <> Typename then begin
    E.ClassImport:= s;
    Typename:= s;
  end;

  if Token = '{' then  // int[] p = {1,2,3,4,4+5};
    SkipPair('{', '}')
  else
    while {(Token <> ',') and } (Token <> ';') and (Token <> '') do  begin
      GetNextToken;
      if Token = '(' then SkipPair('(', ')');
      if Token = '{' then begin
        SkipPair('{', '}');
        E.Kind:= ArrayDeclaration;
      end;
    end;
  setString(s, Start, Scanner.CurrPos - 1 - Start);
  s:= trim(s);
  if copy(s, 1, 1) = '"' then begin // string s = "abc";
    s:= 'new String(' + s + ')';
    E.Kind:= ObjectCreation;
  end;
  s:= s + ';';
  E.Typename:= Typename;
  E.Value:= E.Name + ' = ' + s;
  E.Name:= Ident;
  if E.Kind = ObjectCreation then begin
    p:= Pos('(', s);
    s:= copy(s, p+1, length(s));
    p:= Pos(')', s);
    E.Params:= copy(s, 1, p);
  end;
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteDeclaration(Typename: string; const Ident: string);
  var E: TExecutionLine; s: string;
begin
  // Typename Ident;
  if IsSimpleType(Typename) or isSimpleType(WithoutArray(Typename))
    then E:= TExecutionLine.Create(Ident, VariableDeclaration)
    else E:= TExecutionLine.Create(Ident, ObjectDeclaration);
  s:= getImportName(Typename);
  if s <> Typename then begin
    E.ClassImport:= s;
    Typename:= s;
  end;
  E.Typename:= Typename;
  E.Value:= TypeName + ' ' + E.Name + ';';
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteInvalidStatement;
  var E: TExecutionLine; s: string; Start: PChar;
begin
  // not a valid statement, get error from compiler
  E:= TExecutionLine.Create('Invalid', Statement);
  E.Kind:= TExecutionType.None;
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(s, Start, Scanner.CurrPos - Start);
  E.Value:= s;
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteEqualAssignment(const Variable: string);
  var E: TExecutionLine; s: string; Start: PChar; p: integer;
begin
  GetNextToken;
  if Token = 'new' then begin
    // untyped assignment: object = new Class(...)
    E:= TExecutionLine.Create('', Assignment);
    Start:= Scanner.CurrPos;
    GetNextToken;
    E.Typename:= Token;
    s:= getImportName(E.Typename);
    if s <> E.Typename then begin
      E.ClassImport:= s;
      E.Typename:= s;
    end;
    E.Name:= Variable;
    SkipTo(';');
    setString(s, Start, Scanner.CurrPos - Start);
    if FConfiguration.StrictJavaMode then begin
      E.Name:= '';
      E.Value:= 'new ' + s;
      E.Kind:= Statement;
    end else
      E.Value:= E.Name + ' = new ' + s;
    p:= Pos('(', s);
    s:= copy(s, p+1, length(s));
    p:= Pos(')', s);
    E.Params:= copy(s, 1, p - 1);
  end else begin
    // assignment: var = expression;
    E:= TExecutionLine.Create(Variable, Assignment);
    E.Value:= Token;
    Start:= Scanner.CurrPos;
    SkipTo(';');
    setString(s, Start, Scanner.CurrPos - Start);
    E.Value:= Variable + ' = ' + E.Value + s;
  end;
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteAssignment(const Variable: string);
  var E: TExecutionLine; s: string;
begin
  E:= TExecutionLine.Create(Variable, Assignment);
  SkipTo(';');
  setString(s, Scanner.StartPos, Scanner.CurrPos - Scanner.StartPos);
  E.Value:= s;
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteUnnamedObjectCreation;
  var E: TExecutionLine; Start: PChar; s: string; p: integer;
begin
  // unnamed objectcreation: new Class(...)
  E:= TExecutionLine.Create('', ObjectCreation);
  Start:= Scanner.CurrPos;
  GetNextToken;
  E.Typename:= Token;
  s:= getImportName(E.Typename);
  if s <> E.Typename then begin
    E.ClassImport:= s;
    E.Typename:= s;
  end;
  E.Name:= getShortType(getComJava.GetUniqueObjectName(E.Typename));
  SkipTo(';');
  setString(s, Start, Scanner.CurrPos - Start);

  if FConfiguration.StrictJavaMode then begin
    E.Name:= '';
    E.Value:= 'new ' + s;
    E.Kind:= Statement;
  end else
    E.Value:= E.Name + ' = new ' + s;
  p:= Pos('(', s);
  s:= copy(s, p+1, length(s));
  p:= Pos(')', s);
  E.Params:= copy(s, 1, p);
  ExecuteTerm(E);
end;

procedure TExecutionParser.ExecuteStatement(const Kind: string);
  var E: TExecutionLine; Start: PChar; s: string; O: TOperation;
begin
  E:= TExecutionLine.Create(Kind, Statement);
  O:= TOperation.Create(nil);
  try
    E.Name:= Kind;
    Start:= Scanner.CurrPos;
    ParseBlockStatement(O, true);
    setString(s, Start, Scanner.CurrPos - Start);
    E.Value:= E.Name + s;
    ExecuteTerm(E);
  finally
    FreeAndNil(O);
  end;
end;

procedure TExecutionParser.ExecuteMethodCall(const Call: string);
  var E: TExecutionLine; Start: PChar; s: string; p: integer;
begin
  // Methodcall  [Object|Class].Method(...)
  p:= Pos('.', Call);
  if p > 0  then begin
    E:= TExecutionLine.Create(Left(Call, p-1), MethodCall);
    E.Method:= Right(Call, p+1);
    Start:= Scanner.CurrPos;
    SkipPair('(', ')');
    if (Token = '') or (Token = ';') then begin
      setString(s, Start, Scanner.CurrPos - Start);
      E.Value:= E.Name + '.' + E.Method + '(' + s;
      p:= Pos(')', s);
      delete(s, p+1, length(s));
      E.Params:= s;
    end else begin // expression starting with method-call
      E.Kind:= Expression;
      SkipTo(';');
      setString(s, Start, Scanner.CurrPos - Start);
      E.Value:= E.Name + '.' + E.Method + '(' + s;
    end;
  end else begin
    Start:= Scanner.CurrPos;
    SkipTo(';');
    setString(s, Start, Scanner.CurrPos - Start);
    E:= TExecutionLine.create('', TExecutionType.None { Methodcall});  // public Car(String...) {
    E.Value:= Call + '(' + s;
  end;

  ExecuteTerm(E);
  if (E.Kind = Expression) and (Right(E.Value, -1) = ';') then
    E.Value:= Left(E.Value, -1); // without ";"
end;

procedure TExecutionParser.ExecuteTerm(E: TExecutionLine);
begin
  if Right(E.Value, -2) = #13#10 then
    E.Value:= Left(E.Value, Length(E.Value)-2);
  E.Value:= TrimRight(E.Value);
  if not FConfiguration.StrictJavaMode then
    if (Right(E.Value, -1) <> ';') and (Right(E.Value, -1) <> '}') then E.Value:= E.Value + ';';
  ExecutionList.Add(E);
end;

end.
