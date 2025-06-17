{-------------------------------------------------------------------------------
 Unit:     UGenerateStructogram
 Author:   Gerhard Röhner
 Date:     March 2011
 Purpose:  generate a structogram from java code
-------------------------------------------------------------------------------}

unit UGenerateStructogram;

interface

uses
  UJavaParser,
  UTypes;

type
  TGenerateStructogram = class(TJavaParser)
  private
    FStructogram: TStrList;
    FCurrent: TStrElement;
    function GetBetweenPair(const Open, Close: string): string;
    function IsModifier(const Modifier: string): Boolean;
    procedure GenerateMethod(Method: string);
    procedure GenerateMethodCall(const Method: string);
    procedure GenerateStatement;
    procedure GenerateBlock;
    procedure GenerateCaseBlock;
    procedure GenerateIfStatement;
    procedure GenerateWhileStatement;
    procedure GenerateDoStatement;
    procedure GenerateForStatement;
    procedure GenerateSwitchStatement;
    procedure GenerateReturnStatement;
    procedure GenerateBreakStatement;
    procedure GenerateEmptyStatement;
    procedure GenerateAssignment(const Typename, Operand: string);
    procedure GenerateExpressionStatement(const Typename: string);
    procedure GenerateIncrementExpression(const Operand: string);
    procedure GenerateVariableCreation(const Ident: string);
    procedure GenerateDummyStatement(const Typename, Ident: string);
    procedure GenerateVariableDeclaration(Typename: string);
  public
    procedure GenerateStructogram(const Str: string; AStructogram: TStrList);
  end;

implementation

uses
  SysUtils,
  StrUtils,
  UConfiguration;

function TGenerateStructogram.GetBetweenPair(const Open, Close: string): string;
var
  Count: Integer;
  Start: PChar;
begin
  Result := '';
  Count := 1;
  Start := Scanner.CurrPos;
  while (Count > 0) and (Token <> '') do
  begin
    GetNextToken;
    if Token = Open then
      Inc(Count)
    else if Token = Close then
      Dec(Count);
    if (Count = 0) or (Token = '') then
      SetString(Result, Start, Scanner.CurrPos - 1 - Start);
  end;
  GetNextToken;
end;

function TGenerateStructogram.IsModifier(const Modifier: string): Boolean;
begin
  Result := (Modifier = 'public') or (Modifier = 'protected') or
    (Modifier = 'private') or (Modifier = 'static') or (Modifier = 'abstract')
    or (Modifier = 'final');
end;

procedure TGenerateStructogram.GenerateStructogram(const Str: string;
  AStructogram: TStrList);
var
  Typename, Ident, Modifier: string;
begin
  Scanner.Init(Str);
  Scanner.CompoundTokens := True;
  FStructogram := AStructogram;
  FCurrent := AStructogram;

  GetNextToken;
  while Token <> '' do
  begin
    if IsStatementBegin(Token) or (Token = '{') then
      GenerateStatement
    else
    begin
      ParseAnnotations;
      if IsModifier(Token) then
      begin
        Modifier := Token;
        ParseModifiers;
      end
      else
        Modifier := '';
      if IsTypename(Token) or (Token = 'void') then
      begin
        Typename := GetTypeName;
        if Token = '(' then
          if Modifier <> '' then
          begin
            if Assigned(FCurrent.Prev) then
              Break; // no method after statements
            GenerateMethod(Typename); // constructor
            Break;
          end
          else
            GenerateMethodCall(Typename)
        else if IsAssignmentOperator(Token) or (Token = '++') or (Token = '--')
        then
          GenerateExpressionStatement(Typename)
        else
        begin
          Ident := Token;
          if IsIdentifier(Ident) then
          begin
            SwapArrFromTo(Ident, Typename);
            GetNextToken;
            if Token = '=' then
              GenerateVariableCreation(Ident)
            else if Token = '(' then
            begin
              if Assigned(FCurrent.Prev) then
                Break; // no method after statements
              GenerateMethod(Ident); // constructor
              Break;
            end;
          end;
        end;
      end
      else
        GetNextToken;
    end;
  end;
end;

procedure TGenerateStructogram.GenerateStatement;
var
  Typename: string;
begin
  if (Token = ';') or (Token = '}') or (Token = '') then
    GenerateEmptyStatement
  else if Token = '{' then
    GenerateBlock
  else if Token = ';' then
    GetNextToken // EmptyStatement
  else if Token = '{' then
    GenerateBlock
  else if Token = 'if' then
    GenerateIfStatement
  else if Token = 'while' then
    GenerateWhileStatement
  else if Token = 'do' then
    GenerateDoStatement
  else if Token = 'for' then
    GenerateForStatement
  else if Token = 'switch' then
    GenerateSwitchStatement
  else if Token = 'return' then
    GenerateReturnStatement
  else if Token = 'break' then
    GenerateBreakStatement
  else if IsModifier(Token) then
    ParseModifiers
  else if Token = '@' then
    ParseAnnotations
  else
  begin
    Typename := GetTypeName;
    while (Length(Token) > 0) and CharInSet(Token[1], ['.', '[']) do
    begin
      Typename := Typename + Token;
      GetNextToken;
    end;
    if IsExpressionStatementOperator(Token) then
      GenerateExpressionStatement(Typename)
    else if (Token <> ';') and (Token <> '') and (Token <> '}') and
      (Token <> '(') and not IsStatementBegin(Token) then
      GenerateVariableDeclaration(Typename)
    else
      GenerateDummyStatement(Typename, Token);
  end;
end;

procedure TGenerateStructogram.GenerateBlock;
begin
  GetNextToken;
  repeat
    GenerateStatement;
    if Token = ';' then
      GetNextToken;
  until (Token = '}') or (Token = '');
  GetNextToken;
end;

procedure TGenerateStructogram.GenerateCaseBlock;
begin
  repeat
    GenerateStatement;
    if Token = ';' then
      GetNextToken;
  until (Token = 'case') or (Token = 'default') or (Token = '}') or
    (Token = '');
end;

procedure TGenerateStructogram.GenerateIfStatement;
// 14.8
// IfThenStatement: if ( Expression ) Statement
// IfThenElseStatement: if ( Expression ) StatementNoShortIf else Statement
var
  Elem: TStrIf;
begin
  Elem := TStrIf.CreateStructogram(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  GetNextToken;
  if Token = '(' then
    Elem.Text := GetBetweenPair('(', ')');

  FCurrent := Elem.ThenElem;
  GenerateStatement;
  if Token = ';' then
    GetNextToken;
  FCurrent := Elem.ElseElem;
  if Token = 'else' then
  begin
    GetNextToken;
    GenerateStatement;
  end
  else
    GenerateEmptyStatement;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateWhileStatement;
// 14.10
// WhileStatement: while ( Expression ) Statement
var
  Elem: TStrWhile;
begin
  Elem := TStrWhile.CreateStructogram(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  GetNextToken;
  if Token = '(' then
    Elem.Text := FConfiguration._While + ' ' + GetBetweenPair('(', ')');
  FCurrent := Elem.DoElem;
  GenerateStatement;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateDoStatement;
// 14.11
// DoStatement: do Statement while ( Expression ) ;
var
  Elem: TStrDoWhile;
begin
  Elem := TStrDoWhile.CreateStructogram(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  FCurrent := Elem.DoElem;
  GetNextToken;
  GenerateStatement;
  if Token = 'while' then
  begin
    GetNextToken;
    if Token = '(' then
      Elem.Text := FConfiguration.DoWhile + ' ' + GetBetweenPair('(', ')');
  end;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateForStatement;
// 14.12
// ForStatement: for ( ForInitopt ; Expressionopt ; ForUpdateopt ) Statement
// ForStatement: for (Typ Objekt: Kollektion)
var
  Ident, Ident2, Operand, Init, Cond, Whitespace, Str: string;
  Elem: TStrFor;
  Start1: PChar;
  Posi, Number: Integer;
begin
  Whitespace := '';
  Elem := TStrFor.CreateStructogram(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  FCurrent := Elem.DoElem;
  GetNextToken;
  if Token = '(' then
  begin
    GetNextToken;
    Ident := GetNextToken;
    Operand := GetNextToken;
    if Operand = '=' then
    begin
      Start1 := Scanner.CurrPos;
      SkipTo(';');
      SetString(Init, Start1, Scanner.CurrPos - 1 - Start1);
    end;
    Start1 := Scanner.CurrPos;
    GetNextToken;
    SkipTo(';');
    SetString(Cond, Start1, Scanner.CurrPos - 1 - Start1);
    Posi := Pos('<=', Cond);
    if Posi > 0 then
      Delete(Cond, 1, Posi + 1);
    Posi := Pos('<', Cond);
    if Posi > 0 then
    begin
      Delete(Cond, 1, Posi);
      while (Length(Cond) > 0) and (Cond[1] = ' ') do
      begin
        Whitespace := Whitespace + ' ';
        Delete(Cond, 1, 1);
      end;
      if TryStrToInt(Cond, Number) then
      begin
        Number := Number - 1;
        Cond := IntToStr(Number);
      end
      else
        Cond := Cond + '-1';
      Cond := Whitespace + Cond;
    end;
    Posi := Pos('>=', Cond);
    if Posi > 0 then
      Delete(Cond, 1, Posi + 1);
    Posi := Pos('>', Cond);
    if Posi > 0 then
    begin
      Delete(Cond, 1, Posi);
      while (Length(Cond) > 0) and (Cond[1] = ' ') do
      begin
        Whitespace := Whitespace + ' ';
        Delete(Cond, 1, 1);
      end;
      if TryStrToInt(Cond, Number) then
      begin
        Number := Number + 1;
        Cond := IntToStr(Number);
      end
      else
        Cond := Cond + '+1';
      Cond := Whitespace + Cond;
    end;
    Ident2 := GetNextToken;
    Operand := GetNextToken;
    if Ident = Ident2 then
    begin
      if Operand = '++' then
        Operand := '+1'
      else if Operand = '--' then
        Operand := '-1'
      else
      begin
        if Pos('+=', Operand) > 0 then
          Operand := '+';
        if Pos('-=', Operand) > 0 then
          Operand := '-';
        Start1 := Scanner.CurrPos;
        SkipTo(')');
        SetString(Str, Start1, Scanner.CurrPos - 1 - Start1);
        if Operand = '=' then
          Operand := Str
        else
          Operand := Operand + Str;
        Posi := Pos(Ident2, Operand);
        while Posi > 0 do
        begin
          Delete(Operand, Posi, Length(Ident2));
          Posi := Pos(Ident2, Operand);
        end;
        Posi := Pos(' ', Operand);
        while Posi > 0 do
        begin
          Delete(Operand, Posi, 1);
          Posi := Pos(' ', Operand);
        end;
      end;
    end;
    SkipTo(')');
    GetNextToken;
    Str := FConfiguration._For;
    Posi := Pos('[i]', Str);
    if Posi > 0 then
      Str := ReplaceStr(Str, '[i]', Trim(Ident));
    Posi := Pos('[1]', Str);
    if Posi > 0 then
      Str := ReplaceStr(Str, '[1]', Trim(Init));
    Posi := Pos('[n]', Str);
    if Posi > 0 then
      Str := ReplaceStr(Str, '[n]', Trim(Cond));
    Posi := Pos('[s]', Str);
    if Posi > 0 then
      Str := ReplaceStr(Str, '[s]', Trim(Operand));
    Elem.Text := Str;
  end;
  GenerateStatement;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateSwitchStatement;
// 14.9
// SwitchStatement: switch ( Expression ) SwitchBlock
// SwitchBlock: { SwitchBlockStatementGroupsopt SwitchLabelsopt }
var
  Elem: TStrSwitch;
  Start: PChar;
  CaseCount: Integer;
  Expression, Str: string;
begin
  Elem := TStrSwitch.CreateStructogram(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  GetNextToken;
  if Token = '(' then
    Elem.Text := GetBetweenPair('(', ')');
  if Token = '{' then
  begin
    GetNextToken;
    CaseCount := 0;
    while Token = 'case' do
    begin
      Start := Scanner.CurrPos;
      SkipTo(':');
      SetString(Expression, Start, Scanner.CurrPos - 1 - Start);
      var Elems:= Elem.CaseElems;
      SetLength(Elems, CaseCount + 1);
      Elem.CaseElems:= Elems;
      Elem.CaseElems[CaseCount] := TStrListHead.CreateStructogram
        (FStructogram, Elem);
      Elem.CaseElems[CaseCount].Text := 'case ' + IntToStr(CaseCount)
        + ' head';
      Elem.CaseElems[CaseCount].Next.Text := Trim(Expression);
      FCurrent := Elem.CaseElems[CaseCount].Next;
      GetNextToken;
      while Token = 'case' do
      begin
        Start := Scanner.CurrPos;
        SkipTo(':');
        SetString(Expression, Start, Scanner.CurrPos - 1 - Start);
        Str := Elem.CaseElems[CaseCount].Next.Text + #13#10 + Trim(Expression);
        Elem.CaseElems[CaseCount].Next.Text := Str;
        GetNextToken;
      end;
      GenerateCaseBlock;
      Inc(CaseCount);
    end;
    var Elems:= Elem.CaseElems;
    SetLength(Elems, CaseCount + 1);
    Elem.CaseElems:= Elems;
    Elem.CaseElems[CaseCount] := TStrListHead.CreateStructogram
      (FStructogram, Elem);
    Elem.CaseElems[CaseCount].Text := 'case ' + IntToStr(CaseCount) + ' head';
    Elem.CaseElems[CaseCount].Next.Text := FConfiguration.Other;
    FCurrent := Elem.CaseElems[CaseCount].Next;
    if Token = 'default' then
    begin
      GetNextToken;
      GetNextToken;
      GenerateCaseBlock;
    end
    else
      GenerateEmptyStatement;
  end;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateEmptyStatement;
begin
  var
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Elem.Text := '';
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateReturnStatement;
var
  Elem: TStrStatement;
  Start: PChar;
  Expression: string;
begin
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Expression, Start, Scanner.CurrPos - Start - 1);
  Elem.Text := 'return ' + Trim(Expression);
  GetNextToken;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateBreakStatement;
begin
  var
  Elem := TStrBreak.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Elem.Text := 'break';
  FCurrent := Elem;
  GetNextToken;
end;

procedure TGenerateStructogram.GenerateAssignment(const Typename, Operand: string);
var
  Elem: TStrStatement;
  Start: PChar;
  Expression: string;
begin
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Expression, Start, Scanner.CurrPos - Start - 1);
  Expression := Trim(Expression);
  if Pos('InOut.read', Expression) = 1 then
    Elem.Text := FConfiguration.Input + ' ' + Typename
  else
    Elem.Text := Typename + ' ' + Operand + ' ' + Expression;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateIncrementExpression(const Operand: string);
begin
  var
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  SkipTo(';');
  Elem.Text := Operand;
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateExpressionStatement
  (const Typename: string);
// 14.7
// ExpressionStatement: Assignment; | PreIncrementExpression; | PreDecrementExpression;
// PostIncrementExpression; | PostDecrementExpression;
// MethodInvocation; | ClassInstanceCreationExpression;

// 15.8
// ClassInstanceCreationExpression: new ClassType ( ArgumentListopt )

// 15.26
// Assignment: LeftHandSide AssignmentOperator AssignmentExpression
begin
  if IsAssignmentOperator(Token) then // Assignment
    GenerateAssignment(Typename, Token)
  else if (Token = '++') or (Token = '--') then // Pre/PostIncrementExpression
    GenerateIncrementExpression(Typename + Token)
  else if Token = '(' then // MethodInvocation
    GenerateMethodCall(Typename)
  else
  begin // ClassInstanceCreationExpression
    GetNextToken;
    NeedClassifier(GetImportName(Token));
    SkipTo(';');
  end;
  SkipTo(';');
end;

procedure TGenerateStructogram.GenerateVariableCreation(const Ident: string);
var
  Elem: TStrStatement;
  Start: PChar;
  Expression: string;
begin
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Expression, Start, Scanner.CurrPos - Start - 1);
  Elem.Text := Ident + ' = ' + Trim(Expression);
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateMethod(Method: string);
var
  ParType, Ident: string;
begin
  Method := Method + '(';
  GetNextToken;
  while (Token <> '') and (Token <> ')') do
  begin
    if Token = 'final' then
      GetNextToken;
    ParType := GetTypeName;
    Ident := Token;
    SwapArrFromTo(Ident, ParType);
    Method := Method + Ident;
    GetNextToken;
    if Token = ',' then
    begin
      Method := Method + ', ';
      GetNextToken;
    end;
  end;
  Method := Method + ')';
  FStructogram.Text := FStructogram.Text + ' ' + Method;
  GetNextToken;
  if Token = 'throws' then
  begin
    GetNextToken;
    GetNextToken;
  end;
  if Token = '{' then
    GenerateBlock;
end;

procedure TGenerateStructogram.GenerateMethodCall(const Method: string);
var
  Elem: TStrStatement;
  Str: string;
begin
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  if (Method = 'System.out.println') or (Method = 'System.out.print') then
  begin
    Str := GetBetweenPair('(', ')');
    if Pos('"" + ', Str) = 1 then
      Delete(Str, 1, 5);
    Elem.Text := FConfiguration.Output + ' ' + Str;
  end
  else
    Elem.Text := Method + '(' + GetBetweenPair('(', ')') + ')';
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateDummyStatement(const Typename,
  Ident: string);
var
  Elem: TStrStatement;
  Start: PChar;
  Expression, Str: string;
begin
  Elem := TStrStatement.Create(FStructogram);
  FStructogram.Insert(FCurrent, Elem);
  Start := Scanner.CurrPos;
  SkipTo(';');
  SetString(Expression, Start, Scanner.CurrPos - Start - 1);
  Str := Typename;
  if Typename <> '' then
    Str := Str + ' ';
  Str := Str + Ident;
  if Ident <> '' then
    Str := Str + ' ';
  Elem.Text := Str + Trim(Expression);
  FCurrent := Elem;
end;

procedure TGenerateStructogram.GenerateVariableDeclaration(Typename: string);
// 14.3
// LocalVariableDeclarationStatement: Type VariableDeclarators
// VariableDeclarators: VariableDeclarator | VariableDeclarators , VariableDeclarator
// VariableDeclarator:  VariableDeclaratorId | VariableDeclaratorId = VariableInitializer
// VariableDeclaratorId: Identifier | VariableDeclaratorId [ ]
// VariableInitializer: Expression | ArrayInitializer
var
  Ident, Generic: string;
begin
  if Token = '<' then
  begin
    Generic := Scanner.GetGeneric;
    Typename := Typename + '<' + Generic + '>';
  end
  else
    Generic := '';
  Ident := Token;
  GetNextToken;
  while Token = '[]' do
  begin
    Ident := Ident + Token;
    GetNextToken;
  end;
  if IsIdentifier(Ident) and IsTypename(Typename) and (Token = '=') then
    GenerateAssignment(Ident, '=');
  SkipTo(';');
  GetNextToken;
end;

end.
