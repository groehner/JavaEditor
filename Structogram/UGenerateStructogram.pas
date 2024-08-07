unit UGenerateStructogram;

interface

uses UJavaParser, UTypes;

type
  TGenerateStructogram = class(TJavaParser)
  private
    FStructogram: TStrList;
    FCurrent: TStrElement;
    function getBetweenPair(const open, close: string): string;
    function isModifier(const modifier: string): boolean;
  public
    procedure GenerateStructogram(const s: string; aStructogram: TStrList);
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
    procedure GenerateAssignment(const Typename, Op: string);
    procedure GenerateExpressionStatement(const Typename: string);
    procedure GenerateIncrementExpression(const Op: string);
    procedure GenerateVariableCreation(const Ident: string);
    procedure GenerateDummyStatement(const Typename, Ident: string);
    procedure GenerateVariableDeclaration(TypeName: string);
  end;

implementation

uses SysUtils, UConfiguration, StrUtils;

function TGenerateStructogram.getBetweenPair(const open, close: string): string;
  var Count: integer; Start: pChar; 
begin
  Result:= '';
  Count:= 1;
  Start:= Scanner.CurrPos;
  while (Count > 0) and (Token <> '') do begin
    GetNextToken;
    if Token = open
      then Inc(Count)
    else if Token = close
      then Dec(Count);
    if (Count = 0) or (Token = '') then
      setString(Result, Start, Scanner.CurrPos - 1 - Start);
  end;
  GetNextToken;
end;

function TGenerateStructogram.isModifier(const modifier: string): boolean;
begin
  Result:= (modifier = 'public') or
           (modifier = 'protected') or
           (modifier = 'private') or
           (modifier = 'static') or
           (modifier = 'abstract') or
           (modifier = 'final');
end;

procedure TGenerateStructogram.GenerateStructogram(const s: string; aStructogram: TStrList);
  var TypeName, Ident, Modifier: string;
begin
  Scanner.Init(s);
  Scanner.CompoundTokens:= true;
  FStructogram:= aStructogram;
  FCurrent:= aStructogram;

  GetNextToken;
  while Token <> '' do begin
    if IsStatementBegin(Token) or (Token = '{') then
      GenerateStatement
    else begin
      ParseAnnotations;
      if isModifier(Token) then begin
        Modifier:= Token;
        ParseModifiers;
      end else
        Modifier:= '';
      if IsTypename(Token) or (Token = 'void') then begin
        Typename:= GetTypeName;
        if Token = '(' then
          if Modifier <> '' then begin
            if FCurrent.prev <> nil then break; // no method after statements
            GenerateMethod(Typename); // constructor
            break;
          end else
            GenerateMethodCall(Typename)
        else if IsAssignmentOperator(Token) or (Token = '++') or (Token = '--') then
          GenerateExpressionStatement(Typename)
        else begin
          Ident:= Token;
          if IsIdentifier(Ident) then begin
            SwapArrFromTo(Ident, Typename);
            GetNextToken;
            if Token = '=' then
              GenerateVariableCreation(Ident)
            else if Token = '(' then begin
              if FCurrent.prev <> nil then break; // no method after statements
              GenerateMethod(Ident); // constructor
              break;
            end;
          end;
        end
      end else
        GetNextToken;
    end;
  end;
end;

procedure TGenerateStructogram.GenerateStatement;
  var TypeName: string;
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
  else if isModifier(Token) then
    ParseModifiers
  else if Token = '@' then
    ParseAnnotations
  else begin
    TypeName:= GetTypeName;
    while (Length(Token) > 0) and CharInSet(Token[1], ['.', '['])  do begin
      Typename:= Typename + Token;
      GetNextToken;
    end;
    if IsExpressionStatementOperator(Token) then
      GenerateExpressionStatement(Typename)
    else if (Token <> ';') and (Token <> '') and (Token <> '}') and (Token <> '(') and not IsStatementBegin(Token) then
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
    if Token = ';' then GetNextToken;
  until (Token = '}') or (Token = '');
  GetNextToken;
end;

procedure TGenerateStructogram.GenerateCaseBlock;
begin
  repeat
    GenerateStatement;
    if Token = ';' then GetNextToken;
  until (Token = 'case') or (Token = 'default') or (Token = '}') or (Token = '');
end;

procedure TGenerateStructogram.GenerateIfStatement;
// 14.8
// IfThenStatement: if ( Expression ) Statement
// IfThenElseStatement: if ( Expression ) StatementNoShortIf else Statement
  var elem: TStrIf;
begin
  elem:= TStrIf.createStructogram(FStructogram);
  FStructogram.insert(FCurrent, elem);
  GetNextToken;
  if Token = '(' then
    elem.text:= getBetweenPair('(', ')');

  FCurrent:= elem.then_elem;
  GenerateStatement;
  if Token = ';' then GetNextToken;
  FCurrent:= elem.else_elem;
  if Token = 'else' then begin
    GetNextToken;
    GenerateStatement;
  end else
    GenerateEmptyStatement;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateWhileStatement;
// 14.10
// WhileStatement: while ( Expression ) Statement
  var elem: TStrWhile;
begin
  elem:= TStrWhile.createStructogram(FStructogram);
  FStructogram.insert(FCurrent, elem);
  GetNextToken;
  if Token = '(' then
     elem.text:= FConfiguration._While + ' ' + getBetweenPair('(', ')');
  FCurrent:= elem.do_elem;
  GenerateStatement;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateDoStatement;
// 14.11
// DoStatement: do Statement while ( Expression ) ;
  var elem: TStrDoWhile;
begin
  elem:= TStrDoWhile.createStructogram(FStructogram);
  FStructogram.insert(FCurrent, elem);
  FCurrent:= elem.do_elem;
  GetNextToken;
  GenerateStatement;
  if Token = 'while' then begin
    GetNextToken;
    if Token = '(' then
      elem.text:= FConfiguration.DoWhile + ' ' + getBetweenPair('(', ')');
  end;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateForStatement;
// 14.12
// ForStatement: for ( ForInitopt ; Expressionopt ; ForUpdateopt ) Statement
// ForStatement: for (Typ Objekt: Kollektion)
  var ident, ident2, Op, Init, Cond, Whitespace, s: string;
      elem: TStrFor; Start1: pChar;
      p, number: integer;
begin
  Whitespace:= '';
  elem:= TStrFor.createStructogram(FStructogram);
  FStructogram.insert(FCurrent, elem);
  FCurrent:= elem.do_elem;
  GetNextToken;
  if Token = '(' then begin
    GetNextToken;
    Ident:= GetNextToken;
    Op:= GetNextToken;
    if Op = '=' then begin
      Start1:= Scanner.CurrPos;
      SkipTo(';');
      setString(Init, Start1, Scanner.CurrPos - 1 - Start1);
    end;
    Start1:= Scanner.CurrPos;
    GetNextToken;
    SkipTo(';');
    setString(Cond, Start1, Scanner.CurrPos - 1 - Start1);
    p:= pos('<=', Cond);
    if p > 0 then delete(Cond, 1, p+1);
    p:= pos('<', Cond);
    if p > 0 then begin
      delete(Cond, 1, p);
      while (length(Cond) > 0) and (Cond[1] = ' ') do begin
        Whitespace:= Whitespace + ' ';
        Delete(Cond, 1, 1);
      end;
      if TryStrToInt(Cond, Number) then begin
        Number:= Number - 1;
        Cond:= IntToStr(Number);
      end else
        Cond:= Cond + '-1';
      Cond:= Whitespace + Cond;
    end;
    p:= Pos('>=', Cond);
    if p > 0 then delete(Cond, 1, p+1);
    p:= pos('>', Cond);
    if p > 0 then begin
      delete(Cond, 1, p);
      while (length(Cond) > 0) and (Cond[1] = ' ') do begin
        Whitespace:= Whitespace + ' ';
        Delete(Cond, 1, 1);
      end;
      if TryStrToInt(Cond, Number) then begin
        Number:= Number + 1;
        Cond:= IntToStr(Number);
      end else
      Cond:= Cond + '+1';
      Cond:= Whitespace + Cond;
    end;
    Ident2:= GetNextToken;
    Op:= GetNextToken;
    if Ident = Ident2 then begin
      if Op = '++' then Op:= '+1' else
      if Op = '--' then Op:= '-1'
      else begin
        if Pos('+=', Op) > 0 then OP:= '+';
        if Pos('-=', Op) > 0 then OP:= '-';
        Start1:= Scanner.CurrPos;
        SkipTo(')');
        setString(s, Start1, Scanner.CurrPos - 1 - Start1);
        if Op = '='
          then Op:= s
          else Op:= Op + s;
        p:= Pos(Ident2, Op);
        while p > 0 do begin
          delete(Op, p, length(Ident2));
          p:= Pos(Ident2, Op);
        end;
        p:= Pos(' ', Op);
        while p > 0 do begin
          delete(Op, p, 1);
          p:= Pos(' ', Op);
        end;
      end;
    end;
    SkipTo(')');
    getNextToken;
    s:= FConfiguration._for;
    p:= Pos('[i]', s);
    if p > 0 then s:= ReplaceStr(s, '[i]', trim(Ident));
    p:= Pos('[1]', s);
    if p > 0 then s:= ReplaceStr(s, '[1]', trim(Init));
    p:= Pos('[n]', s);
    if p > 0 then s:= ReplaceStr(s, '[n]',  trim(Cond));
    p:= Pos('[s]', s);
    if p > 0 then s:= ReplaceStr(s, '[s]', trim(Op));
    elem.text:= s;
  end;
  GenerateStatement;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateSwitchStatement;
// 14.9
// SwitchStatement: switch ( Expression ) SwitchBlock
// SwitchBlock: { SwitchBlockStatementGroupsopt SwitchLabelsopt }
  var elem: TStrSwitch; Start: pChar; CaseCount: integer; Expression, s: string;
begin
  elem:= TStrSwitch.createStructogram(FStructogram);
  FStructogram.insert(FCurrent, elem);
  GetNextToken;
  if Token = '(' then
    elem.text:= getBetweenPair('(', ')');
  if Token = '{' then begin
    getNextToken;
    CaseCount:= 0;
    while Token = 'case' do begin
      Start:= Scanner.CurrPos;
      SkipTo(':');
      setString(Expression, Start, Scanner.CurrPos - 1 - Start);
      SetLength(elem.case_elems, CaseCount+1);
      elem.case_elems[CaseCount]:= TStrListHead.CreateStructogram(FStructogram, elem);
      elem.case_elems[CaseCount].text:= 'case ' + IntToStr(CaseCount) + ' head';
      elem.case_elems[CaseCount].next.text:= trim(Expression);
      FCurrent:= elem.case_elems[CaseCount].next;
      GetNextToken;
      while Token = 'case' do begin
        Start:= Scanner.CurrPos;
        SkipTo(':');
        setString(Expression, Start, Scanner.CurrPos - 1 - Start);
        s:= elem.case_elems[CaseCount].next.text + #13#10 + trim(Expression);
        elem.case_elems[CaseCount].next.text:= s;
        GetNextToken;
      end;
      GenerateCaseBlock;
      inc(CaseCount);
    end;
    SetLength(elem.case_elems, CaseCount+1);
    elem.case_elems[CaseCount]:= TStrListHead.CreateStructogram(FStructogram, elem);
    elem.case_elems[CaseCount].text:= 'case ' + IntToStr(CaseCount) + ' head';
    elem.case_elems[CaseCount].next.text:= FConfiguration.Other;
    FCurrent:= elem.case_elems[CaseCount].next;
    if Token = 'default' then begin
      getNextToken;
      getNextToken;
      GenerateCaseBlock;
    end else
      GenerateEmptyStatement;
  end;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateEmptyStatement;
begin
  var elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  elem.text:= '';
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateReturnStatement;
  var elem: TStrStatement;
      Start: PChar; Expression: string;
begin
  elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(Expression, Start, Scanner.CurrPos - Start -1);
  elem.text:= 'return ' + trim(Expression);
  getNextToken;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateBreakStatement;
begin
  var elem:= TStrBreak.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  elem.text:= 'break';
  FCurrent:= elem;
  getNextToken;
end;

procedure TGenerateStructogram.GenerateAssignment(const Typename, Op: string);
  var elem: TStrStatement;
      Start: PChar; Expression: string;
begin
  elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(Expression, Start, Scanner.CurrPos - Start -1);
  Expression:= trim(Expression);
  if Pos('InOut.read', Expression) = 1
    then elem.text:= FConfiguration.Input + ' ' + Typename
    else elem.text:= Typename + ' ' + Op + ' ' + Expression;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateIncrementExpression(const Op: string);
begin
  var elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  SkipTo(';');
  elem.text:= op;
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateExpressionStatement(const Typename: string);
// 14.7
// ExpressionStatement: Assignment; | PreIncrementExpression; | PreDecrementExpression;
//                      PostIncrementExpression; | PostDecrementExpression;
//                      MethodInvocation; | ClassInstanceCreationExpression;

// 15.8
// ClassInstanceCreationExpression: new ClassType ( ArgumentListopt )

// 15.26
// Assignment: LeftHandSide AssignmentOperator AssignmentExpression
begin
  if IsAssignmentOperator(Token) then // Assignment
    GenerateAssignment(Typename, Token)
  else if (Token = '++') or (Token = '--') then // Pre/PostIncrementExpression
    GenerateIncrementExpression(Typename + Token)
  else if Token = '(' then           // MethodInvocation
    GenerateMethodCall(Typename)
  else begin                         // ClassInstanceCreationExpression
    GetNextToken;
    NeedClassifier(getImportname(Token));
    SkipTo(';');
  end;
  SkipTo(';');
end;

procedure TGenerateStructogram.GenerateVariableCreation(const Ident: string);
  var elem: TStrStatement;
      Start: PChar; Expression: string;
begin
  elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(Expression, Start, Scanner.CurrPos - Start -1);
  elem.text:= Ident + ' = ' + trim(Expression);
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateMethod(Method: string);
  var ParType, Ident: string;
begin
  Method:= Method + '(';
  GetNextToken;
  while (Token <> '') and (Token <> ')') do begin
    if Token = 'final' then GetNextToken;
    ParType:= GetTypeName;
    Ident:= Token;
    SwapArrFromTo(Ident, ParType);
    Method:= Method + Ident;
    GetNextToken;
    if Token = ',' then begin
      Method:= Method + ', ';
      GetNextToken;
    end;
  end;
  Method:= Method + ')';
  FStructogram.text:= FStructogram.text + ' ' + Method;
  GetNextToken;
  if Token = 'throws' then begin
    GetNextToken;
    GetNextToken;
  end;
  if Token = '{' then GenerateBlock;
end;

procedure TGenerateStructogram.GenerateMethodCall(const Method: string);
  var elem: TStrStatement; s: string;
begin
  elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  if (Method = 'System.out.println') or (Method = 'System.out.print') then begin
    s:= getBetweenPair('(', ')');
    if Pos('"" + ', s) = 1 then
      delete(s, 1, 5);
    elem.text:= FConfiguration.Output + ' ' + s;
  end else
    elem.text:= Method + '(' + getBetweenPair('(', ')') + ')';
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateDummyStatement(const Typename, Ident: string);
  var elem: TStrStatement;
      Start: PChar; Expression, s: string;
begin
  elem:= TStrStatement.create(FStructogram);
  FStructogram.insert(FCurrent, elem);
  Start:= Scanner.CurrPos;
  SkipTo(';');
  setString(Expression, Start, Scanner.CurrPos - Start -1);
  s:= Typename;
  if Typename <> '' then s:= s + ' ';
  s:= s + Ident;
  if Ident <> '' then s:= s + ' ';
  elem.text:= s + trim(Expression);
  FCurrent:= elem;
end;

procedure TGenerateStructogram.GenerateVariableDeclaration(TypeName: string);
// 14.3
// LocalVariableDeclarationStatement: Type VariableDeclarators
// VariableDeclarators: VariableDeclarator | VariableDeclarators , VariableDeclarator
// VariableDeclarator:  VariableDeclaratorId | VariableDeclaratorId = VariableInitializer
// VariableDeclaratorId: Identifier | VariableDeclaratorId [ ]
// VariableInitializer: Expression | ArrayInitializer
  var Ident, Generic: string;
begin
  if Token = '<' then begin
    Generic:= Scanner.GetGeneric;
    Typename:= Typename + '<' + Generic + '>';
  end else
    Generic:= '';
  Ident:= Token;
  GetNextToken;
  while Token = '[]' do begin
    Ident:= Ident + Token;
    GetNextToken;
  end;
  if IsIdentifier(Ident) and IsTypename(Typename) and (Token = '=') then
    GenerateAssignment(Ident, '=');
  SkipTo(';');
  GetNextToken;
end;


end.
