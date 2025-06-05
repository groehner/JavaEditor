{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

{
  Implementation of the Delphi parser.

  Known issues

  - The parser now parses each unit from the start to the end. It should really
  first parse the interface part of all units and then parse the
  implementation part of the units.

  - In some cases the parser is order dependent with regards to wich files it can
  find.
  Possible solution would be to allow for external means of supplying a
  searchpath for files the codeprovider cannot find, but that can easily become
  cumbersome and/or unconvenient.

  bugs arising from the above mentioned issues
  delphiparser
  uplayer ärver ifrån unknown::sprite i dduo trots att tsprite finns
  detta är pga att egentligen borde interface delar av alla used units parsas först
  sedan impl delarna
  tdelphiintegrator ärver unknown i essmodel trots att tcodeintegrator finns
  detta är för att när parser är på delphiintegrator så har den inte sökvägen till codeintegrator
  när den tar uses i .dpr filen så ligger sökvägen med
}
unit UDelphiParser;

interface

uses
  Classes,
  UCodeParser,
  UModel,
  UUtils;

type

  TDelphiParser = class(TCodeParser)
  private
    FStream: TMemoryStream;
    FCurrPos: PChar;
    FToken: string;
    FNextToken: string;
    FMarkCurrPos: PChar;
    FMarkToken: string;
    FMarkNextToken: string;
    FOM: TObjectModel;
    FUnit: TUnitPackage;
    FGlobalDefines: TStringList;
    FLocalDefines: TStringList;
    FComment: string;
    // Accumulated comment string used for documentation of entities.
    FFilename: string;

    function SkipToken(const What: string): Boolean;
    function SkipPair(const Open, Close: string): Boolean;
    function GetLowerCaseToken: string;

    function IsCallingConvention(const Str: string): Boolean;
    function IsFunctionDecoration(const Str: string): Boolean;
    function IsHintDirective(const Str: string): Boolean;

    procedure LocalDefine(const Symbol: string);
    procedure LocalUndefine(const Symbol: string);
    function IsDefined(const Symbol: string): Boolean;

    procedure Mark;
    procedure Recall;
  protected
    procedure EatWhiteSpace;
    function GetNextToken: Boolean;

    procedure ParseProgram;
    procedure ParseLibrary;
    procedure ParseUnit;

    procedure ParseProgramBlock;
    procedure ParseInterfaceSection;
    procedure ParseImplementation;

    procedure ParseUses(Visibility: TVisibility = viPublic;
      Recurse: Boolean = True);
    procedure ParseLabelSection;
    procedure ParseTypeSection(Visibility: TVisibility = viPublic);
    procedure ParseConstSection;
    procedure ParseVarSection;
    procedure ParseResourcestringSection;

    procedure ParseClass(AClass: TClass);
    procedure ParseInterface(AClass: TInterface);
    procedure ParseRecord(ARecord: TDataType; DoSkip: Boolean = True);

    procedure ParseFunction(AOperation: TOperation;
      Visibility: TVisibility = viPublic);
    procedure ParseFunctionImplementation;
    procedure ParseParamList(AOperation: TOperation);
    procedure ParseReturnType(AOperation: TOperation);
    procedure ParseAttribute(AAttribute: TAttribute;
      Visibility: TVisibility = viPublic);
    procedure ParseProperty(AProperty: TProperty;
      Visibility: TVisibility = viPublic);

    function ParseType: TClassifier;

  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; Filename: string; Inner: Boolean;
      WithoutNeedSource: Boolean); override;
    procedure ParseStreamWithDefines(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; const GlobalDefines: TStringList;
      const Filename: string);

    property Token: string read FToken;
    property NextToken: string read FNextToken;
    property lToken: string read GetLowerCaseToken;
  end;

implementation

uses Dialogs, SysUtils;

{ TDelphiParser }

constructor TDelphiParser.Create;
begin
  inherited;
  FLocalDefines := TStringList.Create;
end;

destructor TDelphiParser.Destroy;
begin
  inherited;
  FreeAndNil(FLocalDefines);
  FreeAndNil(FStream);
end;

function TDelphiParser.IsCallingConvention(const Str: string): Boolean;
begin
  // 'local', 'varargs' new in Kylix/Delphi 6
  Result := (Str = 'register') or (Str = 'pascal') or (Str = 'cdecl') or
    (Str = 'stdcall') or (Str = 'safecall') or (Str = 'assembler') or
    (Str = 'export') or (Str = 'local') or (Str = 'varargs');
end;

function TDelphiParser.IsFunctionDecoration(const Str: string): Boolean;
begin
  Result := (Str = 'overload') or (Str = 'near') or (Str = 'far');
end;

function TDelphiParser.GetLowerCaseToken: string;
begin
  { TODO : We shoudl be able to optimise this. It is called often. }
  Result := LowerCase(Token);
end;

function TDelphiParser.SkipToken(const What: string): Boolean;
begin
  Result := False;
  GetNextToken;
  if LowerCase(Token) = LowerCase(What) then
  begin
    GetNextToken;
    Result := True;
  end;
end;

function TDelphiParser.SkipPair(const Open, Close: string): Boolean;

  procedure InternalSkipPair(const Open, Close: string);
  begin
    while GetNextToken and (lToken <> Close) do
      if lToken = Open then
        InternalSkipPair(Open, Close)
      else
        { TODO : Find a better way to manage that 'end' can be started in several ways. }
        if Open = 'begin' then
        begin
          if (lToken = 'try') or (lToken = 'case') or (lToken = 'asm') then
            InternalSkipPair(Open, Close);
        end;
  end;

begin
  Result := False;
  if lToken <> Close then
    InternalSkipPair(Open, Close);
  GetNextToken;
  if Token <> '' then
    Result := True;
end;

procedure TDelphiParser.EatWhiteSpace;
type
  TPPEnum = (PP_EOF, PP_IF, PP_IFDEF, PP_IFNDEF, PP_IFOPT, PP_DEFINE, PP_UNDEF,
    PP_ELSE, PP_ELSEIF, PP_ENDIF, PP_IFEND, PP_INCLUDE, PP_UNKNOWN);
  PPEnumSet = set of TPPEnum;
var
  InComment, ContinueLastComment, State: Boolean;
  SGetPPExp: string;

  procedure EatOne;
  begin
    if InComment then
      FComment := FComment + FCurrPos^;
    Inc(FCurrPos);
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

  function EatBraceComment: Boolean;
  begin
    Result := True;
    while not CharInSet(FCurrPos^, [#0, '}']) do
    begin
      Result := True;
      EatOne;
    end;
    ContinueLastComment := False;
    InComment := False;
    if FCurrPos^ <> #0 then
      EatOne;
  end;

  function EatParenComment: Boolean;
  begin
    Result := True;
    while not((FCurrPos^ = '*') and ((FCurrPos + 1)^ = ')')) and
      (FCurrPos^ <> #0) do
    begin
      Result := True;
      EatOne;
    end;
    ContinueLastComment := False;
    InComment := False;
    if FCurrPos^ <> #0 then
    begin
      EatOne;
      EatOne;
    end;
  end;

  function EatSlashComment: Boolean;
  begin
    Result := True;
    while not CharInSet(FCurrPos^, [#13, #10, #0]) do
    begin
      Result := True;
      EatOne;
    end;
    ContinueLastComment := True;
    InComment := False;
  end;

  procedure HandleInclude(var IncludeFile: string);
  var
    IncStr: TStream;
    Offset: Integer;
    Buf: TMemoryStream;
  begin
    if Assigned(NeedPackage) then
    begin
      NeedPackage(IncludeFile, '', IncStr);
      if Assigned(IncStr) then
      begin
        Buf := TMemoryStream.Create;
        try
          Offset := Integer(FCurrPos) - Integer(FStream.Memory);

          Buf.Write(FStream.Memory^, Offset);
          Buf.CopyFrom(IncStr, IncStr.Size);

          FStream.Position := Offset;
          Buf.CopyFrom(FStream, FStream.Size - Offset);

          FStream.LoadFromStream(Buf);
          FStream.Position := 0;
          FCurrPos := PChar(Integer(FStream.Memory) + Offset);
        finally
          FreeAndNil(IncStr);
          FreeAndNil(Buf);
        end;
      end;
    end;
  end;

  function EvalPPExpression(const E: string): Boolean;
  begin
    Result := True;
  end;

  function GetPPExp: string;
  begin
    Result := '';
    while True do
    begin
      if CharInSet(FCurrPos^, [#0, '}']) then
        Break;
      if FCurrPos^ > #31 then
        Result := Result + FCurrPos^;
      Inc(FCurrPos);
    end;
    Result := LowerCase(Trim(Result));
    if FCurrPos^ = '}' then
      Inc(FCurrPos);
  end;

  function GetNextPP: TPPEnum;
  var
    Str: string;
  begin
    Result := PP_EOF;
    while True do
    begin
      case FCurrPos^ of
        #0:
          Break;
        '(':
          if ((FCurrPos + 1)^ = '*') then
            EatParenComment
          else
            Inc(FCurrPos);
        '/':
          if ((FCurrPos + 1)^ = '/') then
            EatSlashComment
          else
            Inc(FCurrPos);
        '''':
          begin
            Inc(FCurrPos);
            while not CharInSet(FCurrPos^, ['''', #10, #13]) or
              ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) do
            begin
              if ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) then
                Inc(FCurrPos);
              Inc(FCurrPos);
            end;
            Inc(FCurrPos);
          end;
        '{':
          if ((FCurrPos + 1)^ = '$') then
          begin
            Inc(FCurrPos, 2);
            Str := '';
            while not CharInSet(FCurrPos^, [#0, #9, #32, #10, #13, '}']) do
            begin
              Str := Str + FCurrPos^;
              Inc(FCurrPos);
            end;
            if FCurrPos^ <> #0 then
            begin
              Str := LowerCase(Str);
              if Str = 'if' then
                Result := PP_IF
              else if Str = 'ifdef' then
                Result := PP_IFDEF
              else if Str = 'ifndef' then
                Result := PP_IFNDEF
              else if Str = 'ifopt' then
                Result := PP_IFOPT
              else if Str = 'define' then
                Result := PP_DEFINE
              else if Str = 'undef' then
                Result := PP_UNDEF
              else if Str = 'else' then
                Result := PP_ELSE
              else if Str = 'elseif' then
                Result := PP_ELSEIF
              else if Str = 'endif' then
                Result := PP_ENDIF
              else if Str = 'ifend' then
                Result := PP_IFEND
              else if Str = 'include' then
                Result := PP_INCLUDE
              else
                Result := PP_UNKNOWN;
            end;
            Break;
          end
          else
            EatBraceComment;
      else
        Inc(FCurrPos);
      end;
    end;
  end;

  function FindMatchingPP(const EndSet: PPEnumSet): TPPEnum;
  var
    Nest: Integer;
  begin
    Nest := 0;
    while True do
    begin
      Result := GetNextPP;
      if (Nest = 0) and (Result in EndSet) then
        Break;
      case Result of
        PP_EOF:
          Break;
        PP_IF, PP_IFDEF, PP_IFNDEF, PP_IFOPT:
          Inc(Nest);
        PP_IFEND, PP_ENDIF:
          if Nest > 0 then
            Dec(Nest);
      end;
    end;
  end;

  procedure SkipToElseOrEndif;
  begin
    while True do
      case FindMatchingPP([PP_ELSE, PP_ELSEIF, PP_ENDIF, PP_IFEND]) of
        PP_ELSEIF:
          if EvalPPExpression(GetPPExp) then
            Break;
        PP_EOF, PP_ELSE, PP_IFEND, PP_ENDIF:
          begin
            EatBraceComment;
            Break;
          end;
      end;
  end;

begin
  InComment := False;
  ContinueLastComment := False;
  State := True;
  while State do
  begin
    State := False;

    if (FCurrPos^ = #13) and ((FCurrPos + 1)^ = #10) then
      ContinueLastComment := False;

    if not CharInSet(FCurrPos^, [#33 .. #255]) then
      State := EatWhite;

    if (FCurrPos^ = '{') and ((FCurrPos + 1)^ = '$') then
    begin
      // Handle compiler directive
      case GetNextPP of
        PP_EOF:
          Break;
        PP_IF:
          if not EvalPPExpression(GetPPExp) then
            SkipToElseOrEndif;
        PP_IFDEF:
          if not IsDefined(GetPPExp) then
            SkipToElseOrEndif;
        PP_IFNDEF:
          if IsDefined(GetPPExp) then
            SkipToElseOrEndif;
        PP_DEFINE:
          LocalDefine(GetPPExp);
        PP_UNDEF:
          LocalUndefine(GetPPExp);
        PP_ELSE, PP_ELSEIF:
          // If we get here it means we have parsed the 'then' part and should skip
          // the else part
          begin
            FindMatchingPP([PP_ENDIF, PP_IFEND]);
            EatBraceComment;
          end;
        PP_INCLUDE:
          begin
            SGetPPExp := GetPPExp;
            HandleInclude(SGetPPExp);
          end;
      else
        // PP_IFOPT, PP_ENDIF, PP_IFEND
        EatBraceComment; // Ignore
      end;
      State := True;
    end
    else if FCurrPos^ = '{' then
    begin
      FComment := '';
      EatOne; // Skip the curlybrace
      InComment := True;
      State := EatBraceComment;
      InComment := False;
    end;

    if (FCurrPos^ = '(') and ((FCurrPos + 1)^ = '*') then
    begin
      FComment := '';
      EatOne;
      EatOne; // Skip paren star
      InComment := True;
      State := EatParenComment;
      InComment := False;
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

function TDelphiParser.GetNextToken: Boolean;

  procedure AddOne;
  begin
    FNextToken := FNextToken + FCurrPos^;
    Inc(FCurrPos);
  end;

begin
  FToken := FNextToken;
  FNextToken := '';
  EatWhiteSpace;

  case FCurrPos^ of
    #0: // End of File
      begin
        if FToken <> '' then
          Result := True
        else
          Result := False;
        Exit;
      end;
    '''': // Parse String
      begin
        AddOne;
        while not CharInSet(FCurrPos^, ['''', #10, #13]) or
          ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) do
        begin
          if ((FCurrPos^ = '''') and ((FCurrPos + 1)^ = '''')) then
            AddOne;
          AddOne;
        end;
        AddOne;
      end;
    '"': // Parse String (asm-statements can have " strings)
      begin
        AddOne;
        while not CharInSet(FCurrPos^, ['"', #10, #13]) or
          ((FCurrPos^ = '"') and ((FCurrPos + 1)^ = '"')) do
        begin
          if ((FCurrPos^ = '"') and ((FCurrPos + 1)^ = '"')) then
            AddOne;
          AddOne;
        end;
        AddOne;
      end;
    '#': // Parse char
      begin
        AddOne;
        while CharInSet(FCurrPos^, ['0' .. '9']) do
          AddOne;
      end;
    '$': // Parse hexnum
      begin
        AddOne;
        while CharInSet(FCurrPos^, ['0' .. '9', 'A' .. 'F', 'a' .. 'f']) do
          AddOne;
      end;
    ':':
      begin
        AddOne;
        if FCurrPos^ = '=' then
          AddOne;
      end;
    '+', '-', '0' .. '9': // Parse numerics
      begin
        AddOne;
        while CharInSet(FCurrPos^, ['0' .. '9']) do
          AddOne;
        if CharInSet((FCurrPos - 1)^, ['0' .. '9']) and (FCurrPos^ = '.') then
        begin
          AddOne;
          while CharInSet(FCurrPos^, ['0' .. '9']) do
            AddOne;
        end;
        if CharInSet((FCurrPos - 1)^, ['0' .. '9']) and
          CharInSet(FCurrPos^, ['e', 'E']) then
        begin
          AddOne;
          if CharInSet(FCurrPos^, ['-', '+']) then
            AddOne;
          while CharInSet(FCurrPos^, ['0' .. '9']) do
            AddOne;
        end;
      end;
    '<', '>':
      begin
        if (FCurrPos + 1)^ = '=' then
        begin
          AddOne;
          AddOne;
        end
        else if (FCurrPos^ = '<') and ((FCurrPos + 1)^ = '>') then
        begin
          AddOne;
          AddOne;
        end
        else
          AddOne;
      end;
    '(', ')', '[', ']', ';', ',', '.', '^', '*', '/', '=':
      begin
        if (FCurrPos^ = '.') and ((FCurrPos + 1)^ = '.') then
          AddOne;
        AddOne;
      end;
    'A' .. 'Z', 'a' .. 'z', '_', '@':
      begin
        AddOne;
        while CharInSet(FCurrPos^, ['A' .. 'Z', 'a' .. 'z', '0' .. '9',
          '_', '@']) do
          AddOne;
      end;
  else
    raise Exception.Create(FUnit.Name + ': Unhandled parser state character:' +
      FCurrPos^);
  end;
  Result := True;
end;

procedure TDelphiParser.ParseStream(AStream: TStream; AModel: TAbstractPackage;
  AOM: TObjectModel; Filename: string; Inner: Boolean;
  WithoutNeedSource: Boolean);
begin
  ParseStreamWithDefines(AStream, AModel, AOM, nil, Filename);
end;

procedure TDelphiParser.ParseStreamWithDefines(AStream: TStream;
  AModel: TAbstractPackage; AOM: TObjectModel; const GlobalDefines: TStringList;
  const Filename: string);
begin
  FGlobalDefines := GlobalDefines;
  FFilename := Filename;
  if Assigned(FGlobalDefines) then
    FLocalDefines.Assign(FGlobalDefines);

  FreeAndNil(FStream);
  FStream := StreamToMemory(AStream);
  FCurrPos := FStream.Memory;

  FModel := AModel;
  FOM := AOM;
  GetNextToken;

  GetNextToken;
  if lToken = 'program' then
    ParseProgram
  else if lToken = 'library' then
    ParseLibrary
  else if lToken = 'unit' then
    ParseUnit;
end;

procedure TDelphiParser.ParseLibrary;
begin
  if GetNextToken then
  begin
    FUnit := (FModel as TLogicPackage).AddUnit(Token);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');
    if lToken = 'uses' then
      ParseUses;
  end;
end;

procedure TDelphiParser.ParseProgram;
begin
  if GetNextToken then
  begin
    FUnit := (FModel as TLogicPackage).AddUnit(Token);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');
    if lToken = 'uses' then
    begin
      Mark;
      ParseUses(viPublic, False);
      Recall;
      ParseUses;
    end;
    ParseProgramBlock;
  end;
end;

procedure TDelphiParser.ParseProgramBlock;
begin
  while (lToken <> 'begin') and (lToken <> 'asm') and (Token <> '') do
  begin
    if lToken = 'label' then
      ParseLabelSection
    else if lToken = 'type' then
      ParseTypeSection(viPrivate)
    else if lToken = 'const' then
      ParseConstSection
    else if lToken = 'var' then
      ParseVarSection
    else if lToken = 'resourcestring' then
      ParseResourcestringSection
    else if (lToken = 'procedure') or (lToken = 'function') or
      (lToken = 'constructor') or (lToken = 'destructor') or (lToken = 'class')
    { Implementation of class functions } then
    begin
      if (lToken = 'class') then
        GetNextToken;
      ParseFunctionImplementation;
    end
    else
      raise Exception.Create(FUnit.Name + ': Parseprogramblock: Token:' + Token
        + ' Next:' + NextToken + ' Context:' + Copy(FCurrPos, -100, 200));
  end;
  if (lToken = 'begin') or (lToken = 'asm') then
  begin
    SkipPair('begin', 'end');
  end;
  // Token should now be   ; or .
end;

procedure TDelphiParser.ParseUnit;
var
  UName: string;
begin
  if GetNextToken then
  begin
    UName := Token;
    FUnit := (FModel as TLogicPackage).AddUnit(UName);
    FUnit.Documentation.Description := FComment;
    FComment := '';
    SkipToken(';');

    if lToken = 'interface' then
      ParseInterfaceSection;
    if lToken = 'implementation' then
      ParseImplementation;
    { TODO : finalization initialization end. }
  end;
end;

procedure TDelphiParser.ParseImplementation;
begin
  if GetNextToken then
  begin
    if lToken = 'uses' then
      ParseUses(viPrivate);
    while (lToken <> 'finalization') and (lToken <> 'initialization') and
      (lToken <> 'begin') and (lToken <> 'end') do
    begin
      if lToken = 'type' then
        ParseTypeSection(viPrivate)
      else if lToken = 'const' then
        ParseConstSection
      else if (lToken = 'var') or (lToken = 'threadvar') then
        ParseVarSection
      else if lToken = 'resourcestring' then
        ParseResourcestringSection
      else if (lToken = 'procedure') or (lToken = 'function') or
        (lToken = 'constructor') or (lToken = 'destructor') or
        (lToken = 'class') { Implementation of class functions } then
      begin
        if (lToken = 'class') then
          GetNextToken;
        ParseFunctionImplementation;
      end
      else
        raise Exception.Create(FUnit.Name + ' : ParseImplementation: Token:' +
          Token + ' Next:' + NextToken);
    end;
  end;
end;

procedure TDelphiParser.ParseInterfaceSection;
begin
  if GetNextToken then
  begin
    if lToken = 'uses' then
      ParseUses;
    while lToken <> 'implementation' do
    begin
      if lToken = 'type' then
        ParseTypeSection(viPublic)
      else if lToken = 'const' then
        ParseConstSection
      else if lToken = 'var' then
        ParseVarSection
      else if lToken = 'resourcestring' then
        ParseResourcestringSection
      else if (lToken = 'procedure') or (lToken = 'function') then
      begin
        // Skip forward declaration
        while GetNextToken and (Token <> ';') do
          if Token = '(' then
          begin
            SkipPair('(', ')');
            if Token = ';' then
              Break;
          end;
        GetNextToken;
        // 'forward' is valid but not neccessary
        while IsCallingConvention(lToken) or IsFunctionDecoration(lToken) or
          IsHintDirective(lToken) or (lToken = 'forward') do
          if GetNextToken and (Token = ';') then
            GetNextToken;
      end
      else
        raise Exception.Create(FUnit.Name + ' : Parse Interface Section: Token:'
          + Token + ' Next:' + NextToken);
    end;
  end;
end;

procedure TDelphiParser.ParseUses(Visibility: TVisibility = viPublic;
  Recurse: Boolean = True);
var
  Str: TStream;
  Parser: TDelphiParser;
  UName, FName: string;
  Posi: PChar;
begin
  if GetNextToken then
  begin
    while Token <> ';' do
    begin
      UName := Token;
      if LowerCase(NextToken) = 'in' then
        SkipToken('in');
      FName := Token;
      Posi := PChar(FName);
      FName := AnsiExtractQuotedStr(Posi, '''');
      if FName = '' then
        FName := UName;
      if Assigned(NeedPackage) and (FOM.ModelRoot.FindUnitPackage(UName) = nil)
      then
      begin
        Str := nil;
        NeedPackage(FName, '', Str, not Recurse);
        if Assigned(Str) and Recurse then
        begin
          Parser := TDelphiParser.Create;
          Parser.NeedPackage := NeedPackage;
          try
            try
              Parser.ParseStreamWithDefines(Str, FModel, FOM,
                FGlobalDefines, '');
            except
              on E: Exception do
                ShowMessage(E.Message);
            end;
            { TODO : Keep the parsed list somewhere to be able to implement
              a twoway integrator }
          finally
            FreeAndNil(Parser);
          end;
        end;
      end;
      if (FOM.ModelRoot.FindUnitPackage(UName) <> nil) and Recurse then
        FUnit.AddUnitDependency(FOM.ModelRoot.FindUnitPackage(UName),
          Visibility);
      SkipToken(',');
    end;
    GetNextToken;
  end;
end;

procedure TDelphiParser.ParseLabelSection;
begin
  while GetNextToken and (Token <> ';') do;
  GetNextToken;
end;

procedure TDelphiParser.ParseConstSection;
begin
  if GetNextToken then
  begin
    while (NextToken = '=') or (NextToken = ':') do
    begin
      while GetNextToken and (Token <> ';') do
      begin
        if lToken = '(' then
          SkipPair('(', ')');
        if lToken = 'record' then
          ParseRecord(nil);
        if Token = ';' then
          Break;
      end;
      GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseResourcestringSection;
begin
  if GetNextToken then
  begin
    while NextToken = '=' do
    begin
      while GetNextToken and (Token <> ';') do;
      GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseTypeSection(Visibility: TVisibility);
var
  TName: string;
  Classifier: TClassifier;
begin
  GetNextToken;
  while NextToken = '=' do
  begin
    TName := Token;
    GetNextToken;
    GetNextToken;
    if lToken = 'packed' then
      GetNextToken; { TODO : Add packed to teh model? }

    if lToken = 'class' then // Handle a class declaration
    begin
      if LowerCase(NextToken) <> 'of' then
      begin
        Classifier := FUnit.FindClassifier(TName, TClass);
        // Have to be in the current unit, it is ok to decalre a new class with the
        // same name as an existing class
        if (not Assigned(Classifier)) or (Classifier.Owner <> FUnit) then
        begin
          Classifier := FUnit.MakeClass(TName, '');
          Classifier.IsVisible := True;
          FUnit.AddClass(TClass(Classifier));
        end;
        Classifier.Visibility := Visibility;
        ParseClass(Classifier as TClass);
      end
      else
      begin
        { TODO : Manage 'class of ' declarations }
        while GetNextToken and (Token <> ';') do;
        GetNextToken;
      end;
    end
    else if (lToken = 'interface') or (lToken = 'dispinterface') then
    begin
      Classifier := FUnit.FindClassifier(TName, TInterface);
      if (not Assigned(Classifier)) or (Classifier.Owner <> FUnit) then
      begin
        Classifier := FUnit.MakeInterface(TName, '');
        Classifier.IsVisible := True;
        FUnit.AddInterface(TInterface(Classifier));
      end;
      Classifier.Visibility := Visibility;
      ParseInterface(Classifier as TInterface);
    end
    else if Token = '(' then // Enum
    begin
      Classifier := FUnit.FindClassifier(TName);
      if not Assigned(Classifier) then { Classifier := }
        FUnit.AddDatatype(TName);
      // Skip the enum declaration for now
      while GetNextToken and (Token <> ';') do;
      GetNextToken;
    end
    else if lToken = 'record' then
    begin
      Classifier := FUnit.FindClassifier(TName);
      if not Assigned(Classifier) then
        Classifier := FUnit.AddDatatype(TName);
      ParseRecord(Classifier as TDataType);
    end
    else if lToken = 'set' then
    begin
      Classifier := FUnit.FindClassifier(TName);
      if not Assigned(Classifier) then { Classifier := }
        FUnit.AddDatatype(TName);
      { TODO : MAnage sets }
      // set of (x,y,z)
      while GetNextToken and (Token <> ';') do;
      GetNextToken;
    end
    else if NextToken = '..' then
    begin
      Classifier := FUnit.FindClassifier(TName);
      if not Assigned(Classifier) then { Classifier := }
        FUnit.AddDatatype(TName);
      { TODO : Manage subranges }
      // xxx..yyy
      while GetNextToken and (Token <> ';') do;
      GetNextToken;
    end
    else if (lToken = 'procedure') or (lToken = 'function') then
    begin // Procedural type
      ParseFunction(nil); // Ignore for now
      if lToken = 'of' then
      begin
        // Skip 'of Object;'
        GetNextToken;
        GetNextToken;
        GetNextToken;
      end;
    end
    else
    begin
      Classifier := FUnit.FindClassifier(TName);
      if not Assigned(Classifier) then { Classifier := }
        FUnit.AddDatatype(TName);
      { TODO : Manage the 'rest' of the types eg. procedural types.. }
      // Best guess skip
      while GetNextToken and (Token <> ';') do
      begin
        if Token = '(' then
          SkipPair('(', ')');
      end;
      GetNextToken;
    end;
  end; // while
end;

procedure TDelphiParser.ParseVarSection;
begin // Token = var
  if GetNextToken then
  begin // Token = varname
    while (NextToken = ':') or (NextToken = ',') do
    begin
      while GetNextToken and (Token <> ';') do
        if (lToken = 'procedure') or (lToken = 'function') then
        begin
          ParseFunction(nil);
          if Token <> '=' then
            Break;
        end
        else if lToken = 'record'
        then { TODO : Handle records in the var section of functions }
        begin
          ParseRecord(nil);
          Break; // Break out to the outer loop
        end;

      if Token = ';' then
        GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseClass(AClass: TClass);
var
  Visibility: TVisibility;
  Ancestor: TClass;
  Interf: TInterface;
  Classif: TClassifier;
begin // Token is class
  AClass.Documentation.Description := FComment;
  FComment := '';
  if GetNextToken then
  begin { TODO : Class }
    if Token = '(' then
    begin
      GetNextToken; // Should be Ancestor
      Ancestor := FUnit.FindClassifier(Token, TClass) as TClass;
      if not Assigned(Ancestor) then
      begin
        Classif := FOM.UnknownPackage.FindClassifier(Token, TClass);
        if Assigned(Classif) and (Classif is TClass) then
          Ancestor := Classif as TClass;
      end;
      if Assigned(Ancestor) then
        AClass.Ancestor := Ancestor
      else
      begin
        AClass.Ancestor := FOM.UnknownPackage.MakeClass(Token, '');
        AClass.IsVisible := True;
        FOM.UnknownPackage.AddClass(AClass.Ancestor);
      end;
      { TODO : Parse implemented interfaces }

      GetNextToken;
      while Token = ',' do
      begin
        GetNextToken;
        // Should be an implemented interface
        Interf := FUnit.FindClassifier(Token, TInterface) as TInterface;
        if not Assigned(Interf) then
        begin
          Classif := FOM.UnknownPackage.FindClassifier(Token, TInterface);
          if Assigned(Classif) then
            Interf := Classif as TInterface;
        end;
        if not Assigned(Interf) then
          Interf := FOM.UnknownPackage.MakeInterface(Token, '');

        AClass.AddImplements(Interf);
        AClass.ViewImplements(Interf);
        GetNextToken;
      end;
      SkipPair('(', ')');
    end;
    if Token = ';' then
    begin
      // It was only a forward declaration
      GetNextToken;
    end
    else
    begin
      Visibility := viPublished;
      while lToken <> 'end' do
      begin
        if lToken = 'private' then
        begin
          Visibility := viPrivate;
          GetNextToken;
        end
        else if lToken = 'protected' then
        begin
          Visibility := viProtected;
          GetNextToken;
        end
        else if lToken = 'public' then
        begin
          Visibility := viPublic;
          GetNextToken;
        end
        else if lToken = 'published' then
        begin
          Visibility := viPublished;
          GetNextToken;
        end
        else if (lToken = 'function') or (lToken = 'procedure') or
          (lToken = 'constructor') or (lToken = 'destructor') or
          (lToken = 'class') then
        begin
          if lToken = 'class' then
            GetNextToken;
          { TODO : Proper handling of 'class function/procedure' }
          ParseFunction(AClass.AddOperationWithoutType(NextToken), Visibility);
        end
        else if lToken = 'property' then
        begin
          ParseProperty(AClass.AddProperty(NextToken), Visibility);
        end
        else
        begin
          ParseAttribute(AClass.AddAttribute(Token, nil), Visibility);
        end;
      end;
    end;
    if lToken = 'end' then
    begin
      GetNextToken;
      while IsHintDirective(lToken) do
        GetNextToken;
      if Token = ';' then
        GetNextToken;
    end;
  end;
end;

procedure TDelphiParser.ParseInterface(AClass: TInterface);
var
  Interf: TInterface;
  Classif: TClassifier;
begin
  if GetNextToken then
  begin { TODO : Interface }
    if Token = '(' then
    begin
      // This interface inherits from...
      GetNextToken; // Should be Ancestor
      Interf := FUnit.FindClassifier(Token, TInterface) as TInterface;
      if not Assigned(Interf) then
      begin
        Classif := FOM.UnknownPackage.FindClassifier(Token, TInterface);
        if Assigned(Classif) and (Classif is TInterface) then
          Interf := Classif as TInterface;
      end;
      if Assigned(Interf) then
        AClass.Ancestor := Interf
      else
      begin
        Interf := FOM.UnknownPackage.MakeInterface(Token, '');
        AClass.Ancestor := Interf;
      end;

      SkipPair('(', ')');
    end;
    if Token = ';' then
    begin
      // Only a forward declaration
      GetNextToken;
    end
    else
    begin
      while lToken <> 'end' do
        if (lToken = 'procedure') or (lToken = 'function') then
          ParseFunction(AClass.AddOperationWithoutType(NextToken))
        else
          GetNextToken;
    end;
    if lToken = 'end' then
      SkipToken(';');
  end;
end;

procedure TDelphiParser.ParseRecord(ARecord: TDataType; DoSkip: Boolean = True);
begin
  // ARecord CAN be nil
  while GetNextToken and (lToken <> 'end') do
  begin
    if lToken = 'record' then
    begin
      ParseRecord(ARecord);
      if lToken = 'end' then
        Break;
    end;
  end;
  while IsHintDirective(LowerCase(NextToken)) do
    GetNextToken;
  if DoSkip then
    SkipToken(';')
  else
    GetNextToken;
  // Depending on DoSkip current is either after ';' or on ';'
end;

procedure TDelphiParser.ParseFunction(AOperation: TOperation;
  Visibility: TVisibility);
begin
  if Assigned(AOperation) then
  begin
    AOperation.Documentation.Description := FComment;
    FComment := '';
    if lToken = 'procedure' then
      AOperation.OperationType := otProcedure
    else if lToken = 'function' then
      AOperation.OperationType := otFunction
    else if lToken = 'constructor' then
      AOperation.OperationType := otConstructor
    else if lToken = 'destructor' then
      AOperation.OperationType := otDestructor;
    AOperation.Visibility := Visibility;
  end;

  if (NextToken <> '(') and (NextToken <> ';') then
    GetNextToken;
  while GetNextToken and (Token <> '(') and (Token <> ';') and (Token <> ':')
    do; // Skippa mysko interface grejor
  if Assigned(AOperation) then
  begin
    if Token = '(' then
      ParseParamList(AOperation);
    if Token = ':' then
      ParseReturnType(AOperation);
  end
  else
  begin
    if Token = '(' then
      SkipPair('(', ')');
    while (Token <> ';') do
      GetNextToken;
  end;

  while ((NextToken = ';') and (lToken <> 'end')) or (Token = ';') or
    (lToken = 'of') do
  begin
    if Token = ';' then
      GetNextToken;
    while IsHintDirective(lToken) do
      GetNextToken;
    if IsCallingConvention(lToken) then
    begin
      GetNextToken;
      GetNextToken;
    end
    else if (lToken = 'virtual') or (lToken = 'override') or (lToken = 'dynamic')
    then
    begin
      { TODO : Set IsPolymorphic }
      GetNextToken;
      GetNextToken;
    end
    else if (lToken = 'abstract') then
    begin
      if Assigned(AOperation) then
        AOperation.IsAbstract := True;
      GetNextToken;
      GetNextToken;
    end
    else if (lToken = 'overload') or (lToken = 'reintroduce') then
    begin
      GetNextToken;
      GetNextToken;
    end
    else if (lToken = 'message') then
    begin
      { TODO : Handle message? }
      while GetNextToken and (Token <> ';') do;
    end
    else if (lToken = 'of') and (LowerCase(NextToken) = 'object') then
    begin
      { TODO : Handle 'of object'? }
      GetNextToken;
      GetNextToken;
    end;
  end;
  if Token = ';' then
    GetNextToken;
end;

procedure TDelphiParser.ParseFunctionImplementation;
var
  IsClass: TClass;
  IsOperation, IterOperation: TOperation;
begin
  // Skip forward declaration
  GetNextToken;
  if NextToken = '.' then
  begin
    IsClass := FUnit.FindClassifier(Token, TClass) as TClass;
    IsOperation := TOperation.Create(nil);
    try
      // Parse to a temporary TOperation to be able to locate a matching operation in a class.
      GetNextToken;
      GetNextToken; // Skip the '.'
      IsOperation.Name := Token;
      GetNextToken;
      if Token = '(' then
        ParseParamList(IsOperation);
      if Token = ':' then
        ParseReturnType(IsOperation);

      if Token = ';' then
        GetNextToken;

      // Find IsOperation  in IsClass
      if Assigned(IsClass) then
      begin
        IterOperation := IsClass.FindOperation(IsOperation);
        if Assigned(IterOperation) then
        begin
          if IterOperation.Documentation.Description <> '' then
            IterOperation.Documentation.Description :=
              IterOperation.Documentation.Description + #13#10;
          IterOperation.Documentation.Description :=
            IterOperation.Documentation.Description + FComment;
          FComment := '';
        end;
      end;
    finally
      FreeAndNil(IsOperation);
    end;
  end
  else
  begin
    while GetNextToken and (Token <> ';') do
      if Token = '(' then
      begin
        SkipPair('(', ')');
        if Token = ';' then
          Break;
      end;
    GetNextToken;
  end;

  while IsCallingConvention(lToken) or IsFunctionDecoration(lToken) do
    if GetNextToken and (Token = ';') then
      GetNextToken;

  if (lToken = 'external') then
  begin
    // Manage 'external xxx name yyy;'
    // Mess ups:
    // procedure QWidget_destroy; external QtShareName name QtNamePrefix + 'QWidget_destroy';  <--Stringexpression
    // function OffsetWindowOrgEx(): BOOL; external gdi32 name 'OffsetWindowOrgEx'   <--NO semicolon!
    // procedure Sleep; external kernel32 name 'Sleep'; stdcall;  <-- calling convention after external
    while GetNextToken and (Token <> ';') do
      // Weird delphi compiler bug? Semicolon is NOT necessary after NAME, see Windows.pas
      if lToken = 'name' then
      begin
        repeat
          GetNextToken;
          GetNextToken;
        until Token <> '+';
        Break;
      end;
    if Token = ';' then
      SkipToken(';');
    while IsCallingConvention(lToken) do
      if GetNextToken and (Token = ';') then
        GetNextToken;
  end
  else if (lToken = 'forward') then
    SkipToken(';')
  else
  begin
    // Skip function body
    ParseProgramBlock;
    if Token = ';' then
      GetNextToken;
  end;
end;

procedure TDelphiParser.ParseAttribute(AAttribute: TAttribute;
  Visibility: TVisibility);
var
  Attribs: TList;
  Classifier: TClassifier;
begin // Token = Attributename
  AAttribute.Visibility := Visibility;
  Attribs := TList.Create;
  Attribs.Add(AAttribute);
  try
    while NextToken = ',' do
    begin
      GetNextToken;
      GetNextToken;
      Attribs.Add((AAttribute.Owner as TClass).AddAttribute(Token, nil));
    end;
    if NextToken = ':' then
    begin
      { Token = : }
      GetNextToken;
      GetNextToken;
      Classifier := ParseType;
      for var I := 0 to Attribs.Count - 1 do
        TAttribute(Attribs[I]).TypeClassifier := Classifier;
    end
    else
      // The line below is needed to step ahead if ':' is missing (an error)
      // OR do a parseerror here
      raise Exception.Create(FUnit.Name + ': Bad attribute:' + FCurrPos^);
    if Token = ';' then
      GetNextToken;
  finally
    FreeAndNil(Attribs);
  end;
end;

procedure TDelphiParser.ParseParamList(AOperation: TOperation);
var
  Params: TList;
  ParenLevel: Integer;
  Classifier: TClassifier;
begin
  Params := TList.Create;
  ParenLevel := 0;
  try
    GetNextToken;
    while ((Token <> ')') or (ParenLevel > 0)) do
    begin
      if Token = '(' then
        Inc(ParenLevel);

      if Token = ')' then
        Dec(ParenLevel);

      if (NextToken = ',') or (NextToken = ':') then
      begin
        Params.Add(AOperation.AddParameter(Token));
      end;

      if Token = ':' then
      begin
        GetNextToken;
        Classifier := ParseType;

        for var I := 0 to Params.Count - 1 do
          TParameter(Params[I]).TypeClassifier := Classifier;
        Params.Clear;
        Continue;
      end;
      GetNextToken;
    end;

    GetNextToken;
  finally
    FreeAndNil(Params);
  end;
end;

procedure TDelphiParser.ParseReturnType(AOperation: TOperation);
var
  Return: string;
  Classifier: TClassifier;
begin
  Return := '';
  // Do not include Token ':'
  while GetNextToken and (Token <> ';') and (not IsCallingConvention(lToken)) do
    Return := Return + Token;
  if (Token <> ';') and IsCallingConvention(lToken) then
  begin
    { TODO : Set calling convention }
    GetNextToken;
  end;
  if Return <> '' then
  begin
    Classifier := FUnit.FindClassifier(Return);
    if not Assigned(Classifier) then
      Classifier := FOM.UnknownPackage.FindClassifier(Return);
    if not Assigned(Classifier) then
      Classifier := FOM.UnknownPackage.AddDatatype(Return);
    AOperation.ReturnValue := Classifier;
  end;
  GetNextToken;
end;

procedure TDelphiParser.ParseProperty(AProperty: TProperty;
  Visibility: TVisibility);
begin // Token = Propertyname
  AProperty.Visibility := Visibility;

  AProperty.Documentation.Description := FComment;
  FComment := '';

  // Mess ups:
  // property ActionModes default [amInsert, amEdit, amBrowse];  <-- Override on default value

  // ** Don't we set the type of a property? Bug?

  while (NextToken <> ':') and (NextToken <> ';') do
  begin
    GetNextToken;
    if Token = '[' then
    begin
      SkipPair('[', ']');
      if Token = ';' then
        Break;
    end;
  end;

  while Token <> ';' do
    GetNextToken;
  if LowerCase(NextToken) = 'default' then
  begin
    GetNextToken;
    GetNextToken;
  end;
  while (Token <> ';') do
    GetNextToken;
  GetNextToken;
end;

procedure TDelphiParser.LocalDefine(const Symbol: string);
begin
  FLocalDefines.Add(Symbol);
end;

procedure TDelphiParser.LocalUndefine(const Symbol: string);
var
  Index: Integer;
begin
  Index := -1;
  for var I := 0 to FLocalDefines.Count - 1 do
  begin
    if CompareText(Symbol, FLocalDefines[I]) = 0 then
    begin
      Index := I;
      Break;
    end;
  end;

  if Index <> -1 then
    FLocalDefines.Delete(Index);
end;

function TDelphiParser.IsDefined(const Symbol: string): Boolean;
begin
  Result := False;
  for var I := 0 to FLocalDefines.Count - 1 do
  begin
    if CompareText(Symbol, FLocalDefines[I]) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TDelphiParser.ParseType: TClassifier;
var
  Name: string;
begin // Interprets and returns a datatype.
  // Token should be the first token after the ':' in a var declaration or paramlist.
  Name := '';
  Result := nil;
  while (Token <> ';') and (Token <> ')') and (Token <> '=') and
    (lToken <> 'end') do
  begin
    if Token = '(' then
    begin
      SkipPair('(', ')');
      Continue; // Token could already be at the end.
    end;

    if lToken = 'array' then
    begin
      while lToken <> 'of' do
        GetNextToken;
      Result := ParseType;
      Exit;
    end;
    if lToken = 'record' then
    begin
      ParseRecord(nil, False);
      Exit;
    end;
    if Name = '' then
      Name := Token
    else
      Name := Name + ' ' + Token;
    GetNextToken;
  end;

  Result := FUnit.FindClassifier(Name);
  if not Assigned(Result) then
  begin
    if Name = '' then
      Name := 'Unidentified datatype';
    { TODO : MAybe a little smarter. }
    Result := FOM.UnknownPackage.FindClassifier(Name);
    if not Assigned(Result) then
      Result := FOM.UnknownPackage.AddDatatype(Name);
  end;

  if Token = '=' then
  begin
    { TODO : Skip default values for now }
    while (Token <> ';') and (Token <> ')') and (Token <> '=') do
    begin
      if Token = '(' then
        SkipPair('(', ')')
      else
        GetNextToken;
    end;
  end;
end;

function TDelphiParser.IsHintDirective(const Str: string): Boolean;
// D6 hjälpen:
// The 'hint' directives platform, deprecated, and library may be appended to
// any declaration, except that units cannot be declared with deprecated. In the
// case of a procedure or function declaration, the hint directive should be separated
// from the rest of the declaration with a semicolon.
begin
  Result := (Str = 'platform') or (Str = 'deprecated') or (Str = 'library');
end;

procedure TDelphiParser.Mark;
begin
  FMarkCurrPos := FCurrPos;
  FMarkToken := FToken;
  FMarkNextToken := FNextToken;
end;

procedure TDelphiParser.Recall;
begin
  FCurrPos := FMarkCurrPos;
  FToken := FMarkToken;
  FNextToken := FMarkNextToken;
end;

end.
