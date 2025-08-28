unit UExecutionParser;

interface

uses
  UJavaParser,
  UExecution;

type
  TExecutionParser = class(TJavaParser)
  private
    FExecutionList: TExecutionList;
    procedure ExecuteVariableObjectArrayCreation(Typename: string;
      const Ident: string);
    procedure ExecuteEqualAssignment(const Variable: string);
    procedure ExecuteDeclaration(Typename: string; const Ident: string);
    procedure ExecuteInvalidStatement;
    procedure ExecuteAssignment(const Variable: string);
    procedure ExecuteUnnamedObjectCreation;
    procedure ExecuteStatement(const Kind: string);
    procedure ExecuteMethodCall(const Call: string);
    procedure ExecuteExpression(const Token: string);
    procedure ExecuteTerm(ExecutionLine: TExecutionLine);
  public
    procedure PrepareExecute(const Str: string; ExecutionList: TExecutionList);
  end;

implementation

uses
  Contnrs,
  Classes,
  SysUtils,
  UModel,
  UUtils,
  UComJava1,
  UConfiguration;

procedure TExecutionParser.PrepareExecute(const Str: string;
  ExecutionList: TExecutionList);
var
  Typename, Ident: string;
begin
  Scanner.Init(Str);
  Scanner.CompoundTokens := True;
  FExecutionList := ExecutionList;
  FExecutionList.Clear;
  GetNextToken;
  while True do
  begin
    ParseModifiers; // visibility and others
    if Token = '' then
      Break
    else if (Token = ';') or (Token = '}') then
    begin
      Scanner.StartPos := Scanner.CurrPos;
      GetNextToken;
    end
    else if Token = 'new' then
      ExecuteUnnamedObjectCreation
    else if IsStatementBegin(Token) or (Token = '{') then
      ExecuteStatement(Token)
    else if Token = '(' then
      ExecuteExpression(Token)
    else if IsTypename(Token) then
    begin
      Typename := GetTypeName;
      if Token = '(' then
        ExecuteMethodCall(Typename)
      else if Token = '=' then
        ExecuteEqualAssignment(Typename)
      else if IsAssignmentOperator(Token) or (Token = '++') or (Token = '--')
      then
        ExecuteAssignment(Typename)
      else
      begin
        Ident := Token;
        if IsIdentifier(Ident) then
        begin
          SwapArrFromTo(Ident, Typename);
          GetNextToken;
          if Token = '=' then
            ExecuteVariableObjectArrayCreation(Typename, Ident)
          else if (Token = '') or (Token = ';') or (Token = ',') then
            ExecuteDeclaration(Typename, Ident)
          else
            ExecuteInvalidStatement;
          while Token = ',' do
          begin
            GetNextToken;
            if IsIdentifier(Token) then
            begin
              Ident := Token;
              GetNextToken;
              if Token = '=' then
                ExecuteVariableObjectArrayCreation(Typename, Ident)
              else if (Token = '') or (Token = ';') or (Token = ',') then
                ExecuteDeclaration(Typename, Ident)
              else
                ExecuteInvalidStatement;
            end
            else
              ExecuteInvalidStatement;
          end;
        end
        else if IsSimpleType(Typename) or IsSimpleType(WithoutArray(Typename))
        then
          ExecuteInvalidStatement
        else
          ExecuteExpression(Typename + ' ' + Token);
      end;
    end
    else
      ExecuteExpression(Token);
    GetNextToken;
  end;
end;

procedure TExecutionParser.ExecuteExpression(const Token: string);
var
  ExecutionLine: TExecutionLine;
  Start: PChar;
  Str: string;
begin
  ExecutionLine := TExecutionLine.Create('', Expression);
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Str, Start, Scanner.CurrPos - Start);
  ExecutionLine.Value := Token + Str;
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteVariableObjectArrayCreation(Typename: string;
  const Ident: string);
var
  ExecutionLine: TExecutionLine;
  Start: PChar;
  Str: string;
begin
  // Typename Identifier = [Value | new constructor]
  ExecutionLine := TExecutionLine.Create(Ident);
  Start := Scanner.CurrPos;
  GetNextToken;
  if Token = 'new' then
    ExecutionLine.Kind := ObjectCreation
  else if Token = '{' then
    ExecutionLine.Kind := ArrayDeclaration
  else
    ExecutionLine.Kind := VariableCreation;

  Str := GetImportName(Typename);
  if Str <> Typename then
  begin
    ExecutionLine.ClassImport := Str;
    Typename := Str;
  end;

  if Token = '{' then // int[] p = {1,2,3,4,4+5};
    SkipPair('{', '}')
  else
    while { (Token <> ',') and } (Token <> ';') and (Token <> '') do
    begin
      GetNextToken;
      if Token = '(' then
        SkipPair('(', ')');
      if Token = '{' then
      begin
        SkipPair('{', '}');
        ExecutionLine.Kind := ArrayDeclaration;
      end;
    end;
  SetString(Str, Start, Scanner.CurrPos - 1 - Start);
  Str := Trim(Str);
  if Copy(Str, 1, 1) = '"' then
  begin // string s = "abc";
    Str := 'new String(' + Str + ')';
    ExecutionLine.Kind := ObjectCreation;
  end;
  Str := Str + ';';
  ExecutionLine.Typename := Typename;
  ExecutionLine.Value := ExecutionLine.Name + ' = ' + Str;
  ExecutionLine.Name := Ident;
  if ExecutionLine.Kind = ObjectCreation then
  begin
    var Posi := Pos('(', Str);
    Str := Copy(Str, Posi + 1, Length(Str));
    Posi := Pos(')', Str);
    ExecutionLine.Params := Copy(Str, 1, Posi);
  end;
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteDeclaration(Typename: string;
  const Ident: string);
var
  ExecutionLine: TExecutionLine;
  Str: string;
begin
  // Typename Ident;
  if IsSimpleType(Typename) or IsSimpleType(WithoutArray(Typename)) then
    ExecutionLine := TExecutionLine.Create(Ident, VariableDeclaration)
  else
    ExecutionLine := TExecutionLine.Create(Ident, ObjectDeclaration);
  Str := GetImportName(Typename);
  if Str <> Typename then
  begin
    ExecutionLine.ClassImport := Str;
    Typename := Str;
  end;
  ExecutionLine.Typename := Typename;
  ExecutionLine.Value := Typename + ' ' + ExecutionLine.Name + ';';
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteInvalidStatement;
var
  ExecutionLine: TExecutionLine;
  Str: string;
  Start: PChar;
begin
  // not a valid statement, get error from compiler
  ExecutionLine := TExecutionLine.Create('Invalid', Statement);
  ExecutionLine.Kind := TExecutionType.None;
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Str, Start, Scanner.CurrPos - Start);
  ExecutionLine.Value := Str;
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteEqualAssignment(const Variable: string);
var
  ExecutionLine: TExecutionLine;
  Str: string;
  Start: PChar;
begin
  GetNextToken;
  if Token = 'new' then
  begin
    // untyped assignment: object = new Class(...)
    ExecutionLine := TExecutionLine.Create('', Assignment);
    Start := Scanner.CurrPos;
    GetNextToken;
    ExecutionLine.Typename := Token;
    Str := GetImportName(ExecutionLine.Typename);
    if Str <> ExecutionLine.Typename then
    begin
      ExecutionLine.ClassImport := Str;
      ExecutionLine.Typename := Str;
    end;
    ExecutionLine.Name := Variable;
    SkipTo(';');
    SetString(Str, Start, Scanner.CurrPos - Start);
    if FConfiguration.StrictJavaMode then
    begin
      ExecutionLine.Name := '';
      ExecutionLine.Value := 'new ' + Str;
      ExecutionLine.Kind := Statement;
    end
    else
      ExecutionLine.Value := ExecutionLine.Name + ' = new ' + Str;
    var Posi := Pos('(', Str);
    Str := Copy(Str, Posi + 1, Length(Str));
    Posi := Pos(')', Str);
    ExecutionLine.Params := Copy(Str, 1, Posi - 1);
  end
  else
  begin
    // assignment: var = expression;
    ExecutionLine := TExecutionLine.Create(Variable, Assignment);
    ExecutionLine.Value := Token;
    Start := Scanner.CurrPos;
    SkipTo(';');
    SetString(Str, Start, Scanner.CurrPos - Start);
    ExecutionLine.Value := Variable + ' = ' + ExecutionLine.Value + Str;
  end;
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteAssignment(const Variable: string);
var
  ExecutionLine: TExecutionLine;
  Str: string;
begin
  ExecutionLine := TExecutionLine.Create(Variable, Assignment);
  SkipTo(';');
  SetString(Str, Scanner.StartPos, Scanner.CurrPos - Scanner.StartPos);
  ExecutionLine.Value := Str;
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteUnnamedObjectCreation;
var
  ExecutionLine: TExecutionLine;
  Start: PChar;
  Str: string;
begin
  // unnamed objectcreation: new Class(...)
  ExecutionLine := TExecutionLine.Create('', ObjectCreation);
  Start := Scanner.CurrPos;
  GetNextToken;
  ExecutionLine.Typename := Token;
  Str := GetImportName(ExecutionLine.Typename);
  if Str <> ExecutionLine.Typename then
  begin
    ExecutionLine.ClassImport := Str;
    ExecutionLine.Typename := Str;
  end;
  ExecutionLine.Name :=
    GetShortType(GetComJava.GetUniqueObjectName(ExecutionLine.Typename));
  SkipTo(';');
  SetString(Str, Start, Scanner.CurrPos - Start);

  if FConfiguration.StrictJavaMode then
  begin
    ExecutionLine.Name := '';
    ExecutionLine.Value := 'new ' + Str;
    ExecutionLine.Kind := Statement;
  end
  else
    ExecutionLine.Value := ExecutionLine.Name + ' = new ' + Str;
  var Posi := Pos('(', Str);
  Str := Copy(Str, Posi + 1, Length(Str));
  Posi := Pos(')', Str);
  ExecutionLine.Params := Copy(Str, 1, Posi);
  ExecuteTerm(ExecutionLine);
end;

procedure TExecutionParser.ExecuteStatement(const Kind: string);
var
  ExecutionLine: TExecutionLine;
  Start: PChar;
  Str: string;
  Operation: TOperation;
begin
  ExecutionLine := TExecutionLine.Create(Kind, Statement);
  Operation := TOperation.Create(nil);
  try
    ExecutionLine.Name := Kind;
    Start := Scanner.CurrPos;
    ParseBlockStatement(Operation, True);
    SetString(Str, Start, Scanner.CurrPos - Start);
    ExecutionLine.Value := ExecutionLine.Name + Str;
    ExecuteTerm(ExecutionLine);
  finally
    Operation.Free;
  end;
end;

procedure TExecutionParser.ExecuteMethodCall(const Call: string);
var
  ExecutionLine: TExecutionLine;
  Start: PChar;
  Str: string;
  Posi: Integer;
begin
  // Methodcall  [Object|Class].Method(...)
  Posi := Pos('.', Call);
  if Posi > 0 then
  begin
    ExecutionLine := TExecutionLine.Create(Left(Call, Posi - 1), MethodCall);
    ExecutionLine.Method := Right(Call, Posi + 1);
    Start := Scanner.CurrPos;
    SkipPair('(', ')');
    if (Token = '') or (Token = ';') then
    begin
      SetString(Str, Start, Scanner.CurrPos - Start);
      ExecutionLine.Value := ExecutionLine.Name + '.' + ExecutionLine.Method +
        '(' + Str;
      Posi := Pos(')', Str);
      Delete(Str, Posi + 1, Length(Str));
      ExecutionLine.Params := Str;
    end
    else
    begin // expression starting with method-call
      ExecutionLine.Kind := Expression;
      SkipTo(';');
      SetString(Str, Start, Scanner.CurrPos - Start);
      ExecutionLine.Value := ExecutionLine.Name + '.' + ExecutionLine.Method +
        '(' + Str;
    end;
  end
  else
  begin
    Start := Scanner.CurrPos;
    SkipTo(';');
    SetString(Str, Start, Scanner.CurrPos - Start);
    ExecutionLine := TExecutionLine.Create('',
      TExecutionType.None { Methodcall } ); // public Car(String...) {
    ExecutionLine.Value := Call + '(' + Str;
  end;

  ExecuteTerm(ExecutionLine);
  if (ExecutionLine.Kind = Expression) and (Right(ExecutionLine.Value, -1) = ';')
  then
    ExecutionLine.Value := Left(ExecutionLine.Value, -1); // without ";"
end;

procedure TExecutionParser.ExecuteTerm(ExecutionLine: TExecutionLine);
begin
  if Right(ExecutionLine.Value, -2) = #13#10 then
    ExecutionLine.Value := Left(ExecutionLine.Value,
      Length(ExecutionLine.Value) - 2);
  ExecutionLine.Value := TrimRight(ExecutionLine.Value);
  if not FConfiguration.StrictJavaMode then
    if (Right(ExecutionLine.Value, -1) <> ';') and
      (Right(ExecutionLine.Value, -1) <> '}') then
      ExecutionLine.Value := ExecutionLine.Value + ';';
  FExecutionList.Add(ExecutionLine);
end;

end.
