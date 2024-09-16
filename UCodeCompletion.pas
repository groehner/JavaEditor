unit UCodeCompletion;

interface

uses Classes, Controls, uHTMLParser, UScpHint, UModel,
     UEditorForm, UJavaScanner, SynCompletionProposal;

type

    TOnResizeEvent = procedure(Sender: TObject) of object;

     TSynBaseCompletionProposalFormEx =
       class(TSynBaseCompletionProposalForm)
     private
       Age: TDateTime;
       HintForm: TFScpHint;
       procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
         Shift: TShiftState; X, Y, HitTest: Integer;
         var MouseActivate: TMouseActivate);
     public
       constructor Create(aOwner: TComponent); override;
       procedure Deactivate; override;
       procedure Resize; override;
       procedure SetOnResize(ResizeEvent: TOnResizeEvent);
       procedure KeyPressW(var Key: Char); // override;
       procedure DoFormShow(Sender: TObject); // override;
       procedure ChangeStyle;
     end;

     TCodeCompletion = class
     private
       withTable: boolean;
       function SearchMFeature(MClassifier: TClassifier; SFeature: string; isMethod: boolean): TFeature;
       function hasAPISuperclass: boolean;
       function addThis(const aObject: string): string;
       function getTooltipClass(aClassifier: TClassifier; var Description: string): string;
       function getStartType(Typ, aObject: string; line: integer; asDescription: boolean): string;
       function getType(const SClass, SObject, Args: string; isMethod: boolean; asDescription: boolean): string;
       function getAPIType(const WhereOf, Args: string; AttrMethCons: integer; asDescription: boolean): string;
       function MakeAttribute(Nr: Integer; const Name, Typ: string): string;
       function MakeMethod(Nr: Integer; const Name, ParameterAndType: string): string;
       function getArgTypes(Scanner: TJavaScanner; line: Integer): string;
       procedure CalculateSelfDefinedAttributesAndMethods(var Typ: string; line: integer; const StartWith: string; StaticOnly: boolean);
       procedure CalculateJavaAPIAttributesAndMethods(const Objekt, Typ, StartWith: string);
       function ClassInDocFile(aClass, aFile: string; var FullClass: string): boolean;
       procedure CalculateClassesAndInterfaces(const StartWith: string);
       function getMethod(var s: string): string;
       function getMethodname(const s: string): string;
       function getMethodWithoutParameternames(const s: string; var countParams: integer): string;
       function TypeWithoutDecoration(s: string): string;
       function getAttribute(var s: string): string;
       procedure SplitAttributeComment(AttributeComment: string; var Attribute, Comment: string);
       procedure SplitMethodComment(MethodComment: string; var Method, Comment: string);
       function HTMLPathnameToClass(Pathname: string): string;
       function getDescription(Pathname: string): string;
       procedure getFullClassAndPath(Line: string; out FullClass, Path: string);
       procedure CheckMissingDocumentation;
       procedure ChangeStyle;
       function FieldsFound: boolean;
       function ConstructorsFound: boolean;
       function MethodsFound: boolean;
       function getTagName: string;
       procedure getNextTag;
       procedure PrepareScpJavaForm;
     public
       DocuList: TStringList;
       HTMLFileofJavaClass: string;
       HTMLFileFromFolder: string;
       HTMLFullClass: string;
       HTMLParser: THTMLParser;
       IndexScpJava: integer;
       informed: boolean;
       EditForm: TFEditForm;

       CCClassifier: TClassifier;
       CCAttribute: TAttribute;
       CCOperation: TOperation;
       CCParameter: TParameter;
       constructor create;
       destructor Destroy; override;
       procedure ParseSourceCodes;
       function getMClassifier(SClassifier: string; var aEditForm: TFEditForm): TClassifier;
       function getTypeOfCode(Code: string; Line, State: Integer; asDescription: boolean): string;
       function getToTypeOfObject(aObject: string; var MethodAttribute: string; line: Integer): string;
       function isJavaAPIClass(aClass: string; var FullClass: string): boolean; overload;
       function isJavaAPIClass(const aClass: string): boolean; overload;
       function getJavaAPIMethodParameters: TStringList;
       function IsSelfDefinedClassOrInterface(var Classe, Pathname: string; var aEditForm: TFEditForm): boolean;
       function LoadTextFromFile(var Pathname: string): string;
       function getObjectFrom(s: string): string;
       function getLine(var SClassifier: string; const SObject: string): integer;
       function getAPIReference(const WhereOf: string): string;
       function DoScpJavaExecute(var State: integer; const LocLine, StartWith: string; aEditForm: TFEditForm): boolean;
       function DoScpParamsExecute(State: integer): Boolean;
       function SearchDocumentation(const JavaCode: string; Line: integer): string;
       function FormatParamList(const S: string; CurrentIndex: Integer): string;
     end;

var MyCodeCompletion: TCodeCompletion;

implementation

uses Windows, SysUtils, Dialogs, Types, Grids, Math, Graphics, Themes, DateUtils,
     StrUtils, Character, Contnrs, Forms, UUtils,
     SynEditHighlighter, SynEdit, JvGnugettext,
     UConfiguration, UHTMLHelp, UFileStructure, UMessages,
     UJava, UFileProvider, UTooltip, UStringRessources,
     UModelEntity, UJavaParser, UTemplates;

constructor TSynBaseCompletionProposalFormEx.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  onMouseActivate:= FormMouseActivate;
  Caption:= 'CodeCompletion';
end;

procedure TSynBaseCompletionProposalFormEx.SetOnResize(ResizeEvent: TOnResizeEvent);
begin
  OnResize:= ResizeEvent;
end;

procedure TSynBaseCompletionProposalFormEx.Deactivate;
begin
  if PtInRect(HintForm.BoundsRect, Mouse.CursorPos) or (MilliSecondsBetween(now, age) <1000)
    then (CurrentEditor as TCustomSynEdit).RemoveFocusControl(Self)
    else inherited Deactivate;
end;

procedure TSynBaseCompletionProposalFormEx.Resize;
begin
  inherited Resize;
  if assigned(HintForm) then
    HintForm.SetBounds(Left, Top + Height - 7, Width, HintForm.Height);
end;

procedure TSynBaseCompletionProposalFormEx.KeyPressW(var Key: Char);
begin
  if Key = #9 then begin
    var myMousePos:= Mouse.CursorPos;
    Mouse.CursorPos:= Point(HintForm.Left + 5, HintForm.Top + 5);
    HintForm.TabActiv:= false;
    if HintForm.CanFocus then
      HintForm.SetFocus;
    Mouse.CursorPos:= myMousePos;
  end
  //else inherited KeyPressW(Key);
end;

procedure TSynBaseCompletionProposalFormEx.DoFormShow(Sender: TObject);
begin
  inherited;
  Age:= now; // due to strange behavior under Ubuntu
end;

procedure TSynBaseCompletionProposalFormEx.FormMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 13;
end;

procedure TSynBaseCompletionProposalFormEx.ChangeStyle;
begin
  if StyleServices.IsSystemStyle then begin
    CLSelect:= clHighlight;
    ClSelectedText:= clHighlightText;
    ClBackground:= clWindow;
    //ClBackgroundBorder:= clBtnFace;
    ClTitleBackground:= clBtnFace;
  end else begin
    Font.Color:= StyleServices.getStyleFontColor(sfTabTextInactiveNormal);
    ClSelect:= StyleServices.GetSystemColor(clHighlight);
    ClSelectedText:= StyleServices.GetSystemColor(clHighlightText);
    ClBackground:= StyleServices.GetSystemColor(clWindow);
    //ClBackgroundBorder:= StyleServices.GetSystemColor(clBtnFace);
    ClTitleBackground:= StyleServices.GetSystemColor(clBtnFace);
  end;
end;

// -----------------

constructor TCodeCompletion.create;
begin
  DocuList:= TStringList.Create;
  HTMLParser:= THTMLParser.Create;
  informed:= false;
  PrepareScpJavaForm;
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(DocuList);
  FreeAndNil(HTMLParser);
end;

procedure TCodeCompletion.PrepareScpJavaForm;
  var
     myOnCancel: TNotifyEvent;
     myOnKeyPress: TKeyPressEvent;
     myOnValidate: TValidateEvent;
     myOnChange: TCompletionChange;
     myOnShow: TNotifyEvent;
     myOnClose: TNotifyEvent;
     myOptions: TSynCompletionOptions;
     myCurrentString: string;
begin
  // after reducing this a click in the FScpHint-Window raised an exception
  with FJava do begin
    myOptions:= scpJava.Options;
    myOnCancel:= scpJava.OnCancel;
    myOnKeyPress:= scpJava.OnKeyPress;
    myOnValidate:= scpJava.OnValidate;
    myOnChange:= scpJava.OnChange;
    myOnShow:= scpJava.OnShow;
    myOnClose:= scpJava.OnClose;
    myCurrentString:= scpJava.CurrentString;
    scpJava.Form:= TSynBaseCompletionProposalFormEx.Create(scpJava);
    TSynBaseCompletionProposalFormEx(scpJava.Form).SetOnResize(OnResize);
    TSynBaseCompletionProposalFormEx(scpJava.Form).HintForm:= FScpHint;
    scpJava.OnCancel:= myOnCancel;
    scpJava.OnKeyPress:= myOnKeyPress;
    scpJava.OnValidate:= myOnValidate;
    scpJava.OnChange:= myOnChange;
    scpJava.OnShow:= myOnShow;
    scpJava.OnClose:= myOnClose;
    scpJava.Options:= [];
    scpJava.Options:= myOptions;
    scpJava.EndOfTokenChr:= '()[]. ';
    scpJava.Images:= FFileStructure.vilFileStructureLight;
  end;
  ChangeStyle;
end;

function TCodeCompletion.addThis(const aObject: string): string;
  var p: integer; s: string;
begin
  p:= Pos('.', aObject);
  if p > 0
    then s:= Copy(aObject, 1, p-1)
    else s:= aObject;
  p:= Pos('(', s);
  if p > 1
    then Result:= 'this.' + aObject
  else if s = ''
    then Result:= 'this'
    else Result:= aObject;
end;

function TCodeCompletion.getTooltipClass(aClassifier: TClassifier; var Description: string): string;
  var s, aClassname: string; E: TFEditForm;
begin
  if not assigned(aClassifier) or isSimpleType(aClassifier.Name) then
    Result:= '<code>'
  else begin
    if aClassifier is TClass
      then  s:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> '
      else  s:= '<code><img src="' + FConfiguration.EditorFolder + 'img\interface.png"> ';
    aClassname:= aClassifier.GetShortType;
    if (aClassifier.Pathname <> '') or IsSelfDefinedClassOrInterface(aClassname, aClassifier.Pathname, E) then begin
      s:= s + aClassifier.name;
      Description:= aClassifier.Documentation.Description;
      FTooltip.setFile(aClassifier.Pathname, IntToStr(aClassifier.LineS));
    end else if isJavaAPIClass(aClassifier.Name) then begin
      s:= s + '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + aClassifier.name + '</a>';
      Description:= getDescription(HTMLFileofJavaClass);
      FTooltip.setUrl(HTMLFileofJavaClass);
    end;
    Result:= s + '<br><br>';
  end;
end;

function TCodeCompletion.getStartType(Typ, aObject: string; Line: integer; asDescription: boolean): string;
  var Ci, it, it2: IModelIterator;
      cent: TModelEntity;
      MClassifier: TClassifier;
      Attr: TAttribute;
      Operation: TOperation;
      Param: TParameter;
      SG: TStringGrid;
      i, p: integer;
      E: TFEditForm;
      Pathname, Description, s, aObjectCopy: string;
      aClassifier: TClassifier;

   function ArrayCompatibel(Attr, Objekt, Typ: string): boolean;
     var p, c1, c2, p1: integer;
   begin
     Result:= true;
     // array brackets are possible at the variable or the type
     // make typ[] name instead of typ name[]
     p:= Pos('[', Attr);
     if p > 0 then begin
       Typ := Typ + Copy(Attr, p, length(Attr));
       Attr:= Copy(Attr, 1, p-1);
     end else
       if Objekt = Typ then
         exit;
     if Attr = withoutArray(Objekt) then begin
       Result:= true;
       // access to array-variable or whole array
       if Attr = Objekt then exit;
       c1:= CountChar('[', Objekt);
       c2:= CountChar('[', Typ);
       if (c1 > 0) and (c2 > 0) then
         if c1 = c2 then begin // access to array-variable
           p1:= Pos('[', Objekt);
           // p2:= Pos('[', Typ);
           Objekt:= Copy(Objekt, 1, p1-1);
           if Attr = Objekt then exit;
          end
         else if c2 > c1 then begin
           while CountChar('[', Typ) <> c2 - c1 do begin
             p1:= Pos('[', Typ);
             Delete(Typ, p1, 2);
           end;
           exit;
         end;
     end;
     Result:= false;
   end;

begin
  CCClassifier:= nil;
  CCAttribute:= nil;
  CCOperation:= nil;
  CCParameter:= nil;
  Result:= Typ;
  if Result <> '' then exit;
  try
    if FMessages.InteractiveEditActive then begin
      SG:= FMessages.GetCurrentStringGrid;
      for i:= 1 to SG.RowCount - 1 do
        if SG.Cells[0, i] = aObject then
          Result:= SG.Cells[1, i]
    end else if assigned(FJava.EditorForm) then begin
      FJava.EditorForm.ParseSourcecode(false);
      if assigned(FJava.EditorForm.Model) and assigned(FJava.EditorForm.Model.ModelRoot) then begin
        Ci:= FJava.EditorForm.Model.ModelRoot.GetAllClassifiers;
        while Ci.HasNext do begin
          cent:= Ci.Next;
          CCClassifier:= cent as TClassifier;
          if ((cent.Name = aObject) or endsWith(cent.Name, '.' + aObject) or endsWith(cent.Name, '$' + aObject))
             and (cent.LineS = Line) then begin
            if asDescription then begin
              s:= getTooltipClass(Cent as TClassifier, Description);
              Result:= s + '</code>' + Description;
              if Description = '' then
                if cent is TInterface
                  then Result:= Result + 'The interface <i>' + cent.Name + '</i>'
                  else Result:= Result + 'The class <i>' + cent.Name + '</i>';
            end
              else Result:= cent.Name;
            exit;
          end;
          if (cent.LineS <= line) and (line <= cent.LineE) then begin
            MClassifier:= cent as TClassifier;
            if (aObject = 'this') or (aObject = '') then begin
               Result:= MClassifier.Name;
               break;
            end;
            if aObject = 'super' then begin
               Result:= MClassifier.getAncestorName;
               if Result = '' then Result:= 'Object';
               break;
            end;

            // Methods
            It:= MClassifier.GetOperations;
            while It.HasNext do begin
              Operation:= It.Next as TOperation;
              if not ((Operation.LineS <= line) and (line <= Operation.LineE)) then continue;

              if Operation.Name = aObject then begin
                if assigned(Operation.ReturnValue)
                  then Typ:= Operation.ReturnValue.Name
                  else Typ:= 'void';
                if asDescription or (Operation.LineS = line) then begin
                  Result:= getTooltipClass(cent as TClassifier, Description);
                  if Operation.OperationType = otConstructor
                    then Result:= Result + '<img src="' + FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                                           '<bold>' + toHtml(Operation.toLongString)  + '</bold></code><br><br>Constructor <i>'
                    else Result:= Result + '<img src="' + FConfiguration.EditorFolder + 'img\methods' + IntToStr(Integer(Operation.Visibility))  + '.png"> ' +
                                           '<bold>' + toHtml(Operation.toLongString)  + '</bold></code><br><br>Method <i>';
                  if MClassifier is TInterface
                    then Result:= Result + Operation.Name + '</i> of interface <i>' + cent.name + '</i>.<br><br>'
                    else Result:= Result + Operation.Name + '</i> of class <i>' + cent.name + '</i>.<br><br>';
                  Result:= Result + Operation.getFormattedDescription;
                  end
                else
                  Result:= Typ;
                FTooltip.setFile(MClassifier.Pathname, IntToStr(Operation.LineS));
                CCOperation:= Operation;
                exit;
              end;

              // Parameter of a method
              it2:= Operation.GetParameters;
              while it2.HasNext do begin
                Param:= it2.Next as TParameter;
                if Param.TypeClassifier = nil then continue;
                if ArrayCompatibel(Param.Name, aObject, Param.TypeClassifier.ShortName) then begin
                  if asDescription then
                    Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\param.png"> ' +
                             toHtml(Param.TypeClassifier.ShortName) + ' ' + Param.Name + '</code>' +
                             '<br><br><p>Parameter <i>' + Param.Name + '</i> of method <i>' + Operation.Name + '</i>.'
                  else
                    Result:= Param.TypeClassifier.Name;
                  CCParameter:= Param;
                  FTooltip.setFile(MClassifier.Pathname, IntToStr(Param.LineS));
                  exit;
                end;
              end;

              // local variable of a method
              it2:= Operation.GetAttributes;
              while it2.HasNext do begin
                Attr:= it2.Next as TAttribute;
                if Attr.TypeClassifier = nil then continue;
                if Arraycompatibel(Attr.Name, aObject, Attr.TypeClassifier.ShortName) then begin
                  if asDescription then
                    Result:= getTooltipClass(Attr.TypeClassifier, Description) +
                             '<img src="' + FConfiguration.EditorFolder + 'img\local.png"> ' +
                             toHtml(Attr.toLongString) + '</code>' +
                             '<br><br><p>Local variable <i>' + Attr.Name + '</i> of method <i>' + Operation.Name + '</i>.'
                  else
                    Result:= Attr.TypeClassifier.Name;
                  CCAttribute:= Attr;
                  FTooltip.setFile(MClassifier.Pathname, IntToStr(Attr.LineS));
                  exit;
                end;
              end;
            end; // end of methods

            // Attributes (after method parameters and local variables)
            It:= MClassifier.GetAttributes;
            while It.HasNext do begin
              Attr:= It.Next as TAttribute;
              if Attr.TypeClassifier = nil then continue;
              if ArrayCompatibel(Attr.Name, aObject, Attr.TypeClassifier.ShortName) then begin
                if asDescription then
                  Result:= getTooltipClass(cent as TClassifier, Description) +
                           '<img src="' + FConfiguration.EditorFolder + 'img\attribute' + IntTostr(Integer(Attr.Visibility)) + '.png"> ' +
                           toHtml(Attr.TypeClassifier.ShortName) + ' ' + Attr.Name + '</code>' +
                           '<br><br><p>Attribute of class <i>' + cent.Name + '</i>.' +
                           '<br><br> ' + Attr.Documentation.Description
                else
                  Result:= Attr.TypeClassifier.Name;
                CCAttribute:= Attr;
                FTooltip.setFile(MClassifier.Pathname, IntToStr(Attr.LineS));
                exit;
              end;
            end;

          end;
        end;
      end;
    end;
    except
  end;
  CCClassifier:= nil;

  if Result = '' then begin
    Typ:= '';
    aObjectCopy:= aObject;
    aObject:= WithoutArray(aObject);
    if IsSelfDefinedClassOrInterface(aObject, Pathname, E) then begin
      aClassifier:= getMClassifier(aObjectCopy, E);
      if asDescription then begin
        Result:= getTooltipClass(aClassifier, Description) + '</code>' +  Description;
        if (Description = '') and assigned(aClassifier) then
          if Pos('[]', aObjectCopy) = 0 then
            if (aClassifier is TInterface)
              then Result:= Result + 'The interface <i>' + aClassifier.Name + '</i>'
              else Result:= Result + 'The class <i>' + aClassifier.Name + '</i>'
          else Result:= Result + 'Array of <i>' + aClassifier.Name + '</i>'
      end else
        Result:= aObject;
      CCClassifier:= aClassifier;
    end else if isJavaAPIClass(aObject, Typ) then begin
      Description:= getDescription(HTMLFileofJavaClass);
      if asDescription then begin
        if FConfiguration.IsAPIInterface(aObject) then
          Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\interface.png"> ' +
                   '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + Typ + '</a></code>'
        else
          Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
                   '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + Typ + '</a></code>';
        Result:= result + '<br><br>' + Description;
      end
      else
        Result:= Typ;
      FTooltip.setUrl(ToWeb('',HTMLFileofJavaClass));
    end;
  end;

  if Result = '' then
    if (copy(aObject, 1, 1) = '"') and (copy(aObject, length(aObject), 1) = '"') then
      Result:= 'String'
    else if (copy(aObject, 1, 1) = '''') and (copy(aObject, length(aObject), 1) = '''') then
      Result:= 'char'
    else if (aObject = 'true') or (aObject = 'false') then
      Result:= 'boolean'
    else begin
      p:= 1;
      while p <= length(aObject) do begin
        if CharInset(aObject[p], ['(', ')', '+', '-', ' ']) then
          inc(p)
        else if aObject[p].isDigit then begin
          inc(p);
          while (p <= length(aObject)) and aObject[p].isDigit do
            inc(p);
          if (p <= length(aObject)) and (aObject[p] = '.')
            then Result:= 'double'
            else result:= 'int';
        end else
          break;
      end;
    end;
end;

function TCodeCompletion.hasAPISuperclass: boolean;
  var p, i: Integer; s, path: string;
begin
  with HTMLParser do begin
    i:= Pos('Method Summary', text);
    p:= Pos('extends ', text);
    if (p > 0) and (p < i) then begin
      GotoPos(p);
      NextTag;
      if Tag.Name = 'A' then begin // extends with link
        for i:= 0 to Tag.Params.Count - 1 do
          if Uppercase(Tag.Params.Names[i]) = 'HREF' then begin
            s:= Tag.Params[i];
            s:= Copy(s, 7, length(s) - 7); // href="
          end;
        p:= Pos('#', s); if p > 0 then delete(s, p, length(s));
        p:= Pos('?', s); if p > 0 then delete(s, p, length(s));
        path:= withoutTrailingSlash(ExtractFilePath(HTMLFileofJavaClass));
        repeat
          p:= Pos('../', s);
          if p = 1 then begin
            delete(s, 1, 3);
            p:= LastDelimiter('\', path);
            delete(path, p, length(Path));
          end;
        until p = 0;
        HTMLFullClass:= s;
        p:= Pos('/api/', HTMLFullClass);
        if p > 0 then delete(HTMLFullClass, 1, p+4);
        delete(HTMLFullClass, Length(HTMLFullClass)-4, 5);
        HTMLFullClass:= ReplaceStr(HTMLFullClass, '/', '.');
        s:= ReplaceStr(s, '/', '\');
        p:= Pos('.chm\', path);
        if p > 0 then begin
          delete(path, p + 4, length(path));
          delete(s, 1, 1);
        end;
        if StartsWith(s, 'https')
          then HTMLFileOfJavaClass:= s
          else HTMLFileofJavaClass:= path + '\' + s;
        if not FConfiguration.ShowClassObject and EndsWith(s, 'java\lang\Object.html')
          then exit(false)
          else Result:= FConfiguration.GlobalFileExists(HTMLFileofJavaClass);
      end else begin // extends without link: extends java.lang.String
        s:= TextBetween;
        Delete(s, 1, 8);
        p:= Pos(#13#10, s);
        if p > 0 then Delete(s, p, Length(s)); // ... implements
        HTMLFullClass:= s;
        s:= ReplaceStr(s, '.', '/') + '.html';
        Result:= false
      end;
      if not Result  then
        for i:= 0 to FConfiguration.AllDocumentations.Count -1 do begin
          HTMLFileFromFolder:= FConfiguration.AllDocumentations.Strings[i];
          if ClassInDocFile(s, HTMLFileFromFolder, HTMLFullClass) then begin
            Result:= true;
            break;
           end;
        end;
    end
    else
     Result:= false;
  end;
end;

function TCodeCompletion.getAPIType(const WhereOf, Args: string; AttrMethCons: integer; asDescription: boolean): string;
  var p, countParams, countArgs: integer; found: boolean;
      Typ, Method, Attribute, aclass, Description, htmlfile,
      WithoutParameternames: string;

  function makeConstructor: string;
  begin
    found:= true;
    WithoutParameternames:= getMethodWithoutParameternames(Method, countParams);
    if countParams = countArgs then begin
      if asDescription then begin
        htmlfile:= ToWeb('', HTMLFileofJavaClass) + '#' + WithoutParameternames;
        FTooltip.setUrl(htmlfile);
        Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
                 '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + aclass + '</a><br>' +
                 '<br><img src="' + FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                 '<bold><a href="' + htmlfile + '">' + Method + '</a></bold></code>' +
                 '<br><br>' + Description;
      end else
        Result:= TypeWithoutDecoration(Typ);
    end;
  end;

  function makeMethod: string;
  begin
    found:= true;
    WithoutParameternames:= getMethodWithoutParameternames(Method, countParams);
    if asDescription then begin
      htmlfile:= ToWeb('', HTMLFileofJavaClass) + '#' + WithoutParameternames;
      FTooltip.setUrl(htmlfile);
      Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
               '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + aclass + '</a><br><br>' +
               '<img src="' + FConfiguration.EditorFolder + 'img\method.png"> ' +
               '<bold><a href="' + htmlfile + '">' + Typ + ' ' + Method + '</a></bold></code>' +
               '<br><br>' + Description;
    end else
      // due to Mr. Ehrlich konten.get(i).  should deliver account
      Result:= TypeWithoutDecoration(Typ);
  end;

  function makeAttribute: string;
  begin
    if asDescription then begin
      htmlfile:= ToWeb('', HTMLFileofJavaClass) + '#' + Attribute;
      FTooltip.setUrl(htmlfile);
      Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
               '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + aclass + '</a><br>' +
               '<br><img src="' + FConfiguration.EditorFolder + 'img\attribute.png"> ' +
               '<bold><a href="' + htmlfile + '">' +  Typ + ' ' + Attribute + '</a></bold></code>' +
               '<br><br> ' + Description
    end else
      Result:= WithoutGeneric(WithoutArray(TypeWithoutDecoration(Typ)));
  end;

  function IsModifierOrVisibility(Method: string): boolean;
  begin
    var p:= Pos(',', Method);
    if p > 0 then Method:= copy(Method, 1, p-1);
    Result:= IsModifier(Method) or IsVisibility(Method);
  end;

begin
  Result:= '';
  while true do begin
    with HTMLParser do begin
      Text:= LoadTextFromFile(HTMLFileofJavaClass);
      aclass:= HTMLPathnameToClass(HTMLFileofJavaClass);
      countArgs:= 0;
      if length(args) > 0 then begin
        inc(countArgs);
        for p:= 1 to length(args) do
          if args[p] = ',' then inc(countArgs);
      end;
      withTable:= false;
      found:= false;
      case AttrMethCons of
       0: begin
            if ConstructorsFound then begin
              if withTable then
                repeat
                  Description:= getTDCell;
                  Method:= getMethod(Description);
                  if getMethodname(Method) = WhereOf then begin
                    Result:= makeConstructor;
                    if WithoutParameternames = WhereOf + '(' + args + ')' then
                      break;
                    if countParams = countArgs then
                      break;
                  end else
                    if found then break;
                until getTagName <> 'TR'
              else
                repeat
                  Method:= trim(getDIVCell);
                  if (Method = '') or IsModifierOrVisibility(Method) then
                    Method:= trim(getDIVCell);
                  Description:= getDIVCell;
                  if getMethodname(Method) = WhereOf then begin
                    Result:= makeConstructor;
                    if WithoutParameternames = WhereOf + '(' + args + ')' then
                      break;
                    if countParams = countArgs then
                      break;
                  end else
                    if found then break;
                  NextDiv;
                until Pos('div class="col-first', Tag.Text) <> 1;
            end else // default constructor
              if asDescription then begin
                htmlfile:= ToWeb('', HTMLFileofJavaClass) + '#' + WithoutParameternames;
                FTooltip.setUrl(htmlfile);
                Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
                         '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + aclass + '</a><br>' +
                         '<br><img src="' + FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                         '<bold><a href="' + htmlfile + '">' + WhereOf + '</a></bold></code>' +
                         '<br><br>' + Description;
              end else
                Result:= WithoutGeneric(WithoutArray(TypeWithoutDecoration(Typ)));
          end;
       1: if MethodsFound then
            if withTable then
              repeat
                Typ:= getTDCell;
                Description:= getTDCell;
                Method:= getMethod(Description);

                if getMethodname(Method) = WhereOf then begin
                  Result:= makeMethod;
                  if WithoutParameternames = WhereOf + '(' + args + ')' then
                    break;
                  if countParams = countArgs then
                    break;
                end else
                  if found then break;
              until getTagName <> 'TR'
            else
              repeat
                Typ:= getDIVCell;
                Method:= getDIVCell;
                Description:= getDIVCell;

                if getMethodname(Method) = WhereOf then begin
                  Result:= makeMethod;
                  if WithoutParameternames = WhereOf + '(' + args + ')' then
                    break;
                  if countParams = countArgs then
                    break;
                end else
                  if found then
                    break;
                NextDiv;
              until Pos('div class="col-first', Tag.Text) <> 1;
        2: if FieldsFound then
            if withTable then
              repeat
                Typ:= getTDCell;
                Description:= getTDCell;
                Attribute:= getAttribute(Description);
                if Attribute = WhereOf then
                  exit(makeAttribute);
              until getTagName <> 'TR'
            else
              repeat
                Typ:= getDIVCell;
                Attribute:= getDIVCell;
                Description:= getDIVCell;
                if Attribute = WhereOf then
                  exit(makeAttribute);
                NextDiv;
              until Pos('div class="col-first', Tag.Text) <> 1;
      end;
      if (Result = '') and hasAPISuperclass
        then Continue
        else break;
    end;
  end;
end;

function TCodeCompletion.FieldsFound: boolean;
begin
  Result:= false;
  with HTMLParser do begin
    var p:= Pos('Field Summary', text);
    if p > 0 then begin
      GotoPos(p);
      p:= 0;
      repeat
        inc(p);
        getNextTag;
      until (TextBetween = 'Fields') or (Tag.Name = 'CODE') or (p = 15);
      if (TextBetween = 'Fields') and (p < 15) then begin
        p:= 0;
        repeat
          inc(p);
          getNextTag;
        until (Tag.Name = 'CODE') or (p = 40);
      end;
      Result:= (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.ConstructorsFound: boolean;
begin
  Result:= false;
  with HTMLParser do begin
    var p:= Pos('Constructor Summary', text);
    if p > 0 then begin
      GotoPos(p);
      p:= 0;
      repeat
        inc(p);
        getNextTag;
      until (TextBetween = 'Constructors') or (Tag.Name = 'CODE') or (p = 15);
      if (TextBetween = 'Constructors') and (p < 15) then begin
        p:= 0;
        repeat
          inc(p);
          getNextTag;
        until (Tag.Name = 'CODE') or (p = 40);
      end;
      Result:= (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.MethodsFound: boolean;
begin
  Result:= false;
  with HTMLParser do begin
    var p:= Pos('Method Summary', text);
    if p > 0 then begin
      GotoPos(p);
      p:= 0;
      repeat
        inc(p);
        getNextTag;
      until (TextBetween = 'All Methods') or (TextBetween = 'Methods') or (Tag.Name = 'CODE') or (p = 15);
      if ((TextBetween = 'All Methods') or (TextBetween = 'Methods')) and (p < 15) then begin
        p:= 0;
        repeat
          inc(p);
          getNextTag;
        until (Tag.Name = 'CODE') or (p = 60);
      end;
      Result:= (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.getTagName: string;
begin
  with HTMLParser do begin
    NextTag;
    NextTag;
    Result:= Tag.Name;
    NextTag;
  end;
end;

procedure TCodeCompletion.getNextTag;
begin
  with HTMLParser do begin
    NextTag;
    if (Tag.Name = 'TR') or (Tag.Name = 'TABLE') then
      withTable:= true;
  end;
end;

function TCodeCompletion.getAPIReference(const WhereOf: string): string;
  var p: integer;
      link, path, afile: string;

  function found(s: string): boolean;
  begin
    result:= false;
    var p:= Pos('#' + WhereOf, s);
    if p = 0 then exit;
    delete(s, 1, p + length(WhereOf));
    if (s = '') or (s[1] = '(') then Result:= true;
  end;

begin
  if FConfiguration.GlobalFileExists(HTMLFileofJavaClass) then
    with HTMLParser do begin
      repeat
        link:= getNextLink;
      until (link = '') or found(link);
      if link = '' then
        Result:= HTMLFileofJavaClass
      else begin
        path:= ExtractFilePath(HTMLFileofJavaClass);
        p:= Pos('#' + WhereOf, link);
        afile:= copy(link, 1, p-1);
        delete(link, 1, p-1);
        p:= Pos('/', afile);
        while p > 0 do begin
          delete(afile, 1, p);
          p:= Pos('/', afile);
        end;
        Result:= path + afile + link;
      end;
    end
  else
    Result:= '';
end;

function TCodeCompletion.getType(const SClass, SObject, args: string; isMethod: boolean; asDescription: boolean): string;
  var MClassifier: TClassifier;
      aClassifier, Description, styp: string;
      MFeature: TFeature;
      Attr: TAttribute;
      Operation: TOperation;
      p, AttrMethCons: integer;
      Typ: string;
      E: TFEditForm;
      isAPIClass: boolean;
begin
  styp:= getShortType(SClass);
  if isMethod and ((SClass = SObject) or (styp = SObject)) and (args = '') then begin  // default constructor
    if asDescription then
        Result:= '<img src="' + FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                 '<bold>' + styp  + '</bold></code><br><br>Constructor <i>' + styp + '</i>'
    else
      Result:= SClass;
    exit;
  end;
  aClassifier:= SClass;
  MFeature:= nil;
  Result:= '';

  repeat
    MClassifier:= getMClassifier(aClassifier, E);
    if Assigned(MClassifier) then begin
      MFeature:= SearchMFeature(MClassifier, SObject, false);
      if not Assigned(MFeature) then
        MFeature:= SearchMFeature(MClassifier, SObject, true);
      if not Assigned(MFeature) then
        aClassifier:= ExtractClassName(MClassifier.getAncestorName)
    end
  until Assigned(MFeature) or (MClassifier = nil) or (aClassifier = '');

  if Assigned(MFeature) then begin
    CCClassifier:= MClassifier;
    CCOperation:= nil;
    CCAttribute:= nil;
    CCParameter:= nil;
    if MFeature is TAttribute then begin
      Attr:= MFeature as TAttribute;
      if asDescription then begin
        Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\attribute' + IntTostr(Integer(Attr.Visibility)) + '.png"> ' +
                 toHtml(Attr.toShortString) + '</code>';
        if MClassifier is TInterface
          then Result:= Result + '<br><br><p>Attribute of interface <i>' + MClassifier.Name + '</i>.'
          else Result:= Result + '<br><br><p>Attribute of class <i>' + MClassifier.Name + '</i>.';
        Result:= Result + '<br><br> ' + Attr.Documentation.Description;
        FTooltip.setFile(MClassifier.Pathname, IntToStr(Attr.LineS));
      end else
        Result:= Attr.TypeClassifier.Name;
      CCAttribute:= Attr;
      exit;
    end else if MFeature is TOperation then begin
      Operation:= MFeature as TOperation;
      if Assigned(Operation.ReturnValue)
        then Result:= Operation.ReturnValue.Name     // ExtractClassname
        else Result:= 'void';
      if asDescription then begin
        if Operation.OperationType = otConstructor then
          Result:= getTooltipClass(MClassifier, Description) +
                   '<img src="' + FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                   '<bold>' + toHtml(Operation.toShortString) + '</bold></code><br><br>' +
                   'Constructor <i>' + Operation.Name + '</i> of class <i>' + MClassifier.name + '</i>.<br><br>' +
                    Operation.getFormattedDescription
        else
          Result:= getTooltipClass(MClassifier, Description) +
                   '<img src="' + FConfiguration.EditorFolder + 'img\methods' + IntToStr(Integer(Operation.Visibility)) + '.png"> ' +
                   '<bold>' + toHtml(Operation.toShortString) + '</bold></code><br><br>' +
                   'Method <i>' + Operation.Name + '</i> of class <i>' + MClassifier.name + '</i>.<br><br>' +
                    Operation.getFormattedDescription;
        FTooltip.setFile(MClassifier.Pathname, IntToStr(Operation.LineS));
      end;
      CCOperation:= Operation;
      exit;
    end;
  end;

  aClassifier:= SClass;
  isAPIClass:= isJavaAPIClass(aClassifier);
  while (aClassifier <> '') and not isAPIClass do begin
    MClassifier:= getMClassifier(aClassifier, E);
    if Assigned(MClassifier) then begin
      aClassifier:= ExtractClassName(MClassifier.getAncestorName);
      isAPIClass:= isJavaAPIClass(aClassifier);
    end else
      aClassifier:= '';
  end;

  if isAPIClass then begin
    if isMethod then
      if getShortType(SClass) = SObject
        then AttrMethCons:= 0
        else AttrMethCons:= 1
    else AttrMethCons:= 2;
    Typ:= getAPIType(SObject, args, AttrMethCons, asDescription);
    if Typ = 'E' then begin
      p:= Pos('<', SClass);
      if p > 0
        then Typ:= copy(SClass, p + 1, length(SClass)-p-1)
        else Typ:= 'Object';
    end;
    if Typ = '' then Typ:= SClass;
    Result:= Typ;
  end;
end;

function TCodeCompletion.getLine(var SClassifier: string; const SObject: string): integer;
  var MClassifier: TClassifier;
      MFeature: TFeature;
      E: TFEditForm;
begin
  Result:= 1;
  MFeature:= nil;
  repeat
    MClassifier:= getMClassifier(SClassifier, E);
    if Assigned(MClassifier) then begin
      Result:= MClassifier.LineS;
      if SObject = '' then exit;
      MFeature:= SearchMFeature(MClassifier, SObject, false);
      if not Assigned(MFeature) then
        MFeature:= SearchMFeature(MClassifier, SObject, true);
      if not Assigned(MFeature) then
        SClassifier:= MClassifier.getAncestorName
    end
  until Assigned(MFeature) or (MClassifier = nil) or (SClassifier = '');
  if Assigned(MFeature) then begin
    if MFeature is TAttribute
      then Result:= (MFeature as TAttribute).LineS;
    if MFeature is TOperation
      then Result:= (MFeature as TOperation).LineS;
    end
  else
    Result:= -1;
end;

function TCodeCompletion.getMClassifier(SClassifier: string; var aEditForm: TFEditForm): TClassifier;
  var i: integer; s, SClassifierPath: string;

  function getClassifier: TClassifier;
  begin
    Result:= nil;
    var ci:= aEditForm.Model.ModelRoot.GetAllClassifiers;
    while (Result = nil) and ci.HasNext do begin
      var cent:= ci.next;
      if (cent.Name = SClassifier) or (getShortType(cent.name) = SClassifier) then
        Result:= (cent as TClassifier);
    end;
  end;

begin
  Result:= nil;
  try
    SClassifier:= WithoutGeneric(WithoutArray(SClassifier));
    SClassifierPath:= ReplaceStr(SClassifier, '.', '\') + '.java';

    for i:= 0 to FJava.TDIEditFormCount - 1 do begin
      aEditForm:= FJava.TDIEditFormGet(i);
      if aEditForm.isJava and (ExtractFilename(aEditForm.Pathname) = ExtractFilename(SClassifierPath)) then begin
        Result:= getClassifier;
        if assigned(Result) then begin
          if not Result.SourceRead then
            aEditForm.ParseSourcecode(false);
          exit;
        end;
      end;
    end;
    if assigned(FJava.ActiveTDIChild) and (FJava.ActiveTDIChild.FormTag = 1) then begin
      aEditForm:= FJava.ActiveTDIChild as TFEditForm;
      s:= ExtractFilePath(aEditForm.Pathname) + toBackSlash(sClassifier) + '.java';
      if (aEditForm.Pathname <> '') and FileExists(s) then begin
        aEditForm:= FJava.OpenEditForm(s, true); // hidden
        aEditForm.ParseSourcecode(false);
        Result:= getClassifier;
        if assigned(Result) then exit;
      end;
    end;
    aEditForm:= nil;
  except on e: Exception do
    ErrorMsg(e.Message);
  end;
end;

function TCodeCompletion.SearchMFeature(MClassifier: TClassifier; SFeature: string; isMethod: boolean): TFeature;
begin
  Result:= nil;
  SFeature:= WithoutArray(SFeature);
  var it:= MClassifier.GetFeatures;
  while (Result = nil) and it.HasNext do begin
    var fe:= it.Next as TFeature;
    if fe.Name = SFeature then
      if ((fe is TAttribute) and not isMethod) or ((fe is TOperation) and isMethod) then
        Result:= fe;
  end;
end;

function TCodeCompletion.getTypeOfCode(Code: string; Line, State: Integer; asDescription: boolean): string;
  var i: integer;
      Typ, method, args, Token, StartToken: string;
      isWord: boolean;
      bracket: integer;
      MyUnit: TUnitPackage;
      Scanner: TJavaScanner;

  function isPackage: string;
    var i: integer;
  begin
    if assigned(FJava.EditorForm) and assigned(FJava.EditorForm.Model) and assigned(FJava.EditorForm.Model.ModelRoot) then begin  // package
      MyUnit := FJava.EditorForm.Model.ModelRoot.FindUnitPackage('Default');
      if assigned(MyUnit) then begin
        for i:= 0 to MyUnit.FullImports.Count-1 do
          if Pos(Code + '.', MyUnit.FullImports.Strings[i]) = 1 then begin
            if asDescription
              then Result:= '<code><img src="' + FConfiguration.EditorFolder + 'img\package.png"> ' +
                             Code+ '</code>' +
                             '<br><br>The package <i>' + Code + '</i>.'
              else Result:= 'package';
            exit;
          end;
        for i:= 0 to MyUnit.ClassImports.Count-1 do
          if Code = MyUnit.ClassImports.ValueFromIndex[I] + '.' + MyUnit.ClassImports.Names[i] then begin
            if asDescription
              then Result:= '<code"><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
                             Code + '</code>' +
                             '<br><br>The imported class <i>' + Code + '</i>.'
              else Result:= Code;
            exit;
          end else
           if Pos(Code, MyUnit.ClassImports.ValueFromIndex[I] + '.' + MyUnit.ClassImports.Names[i]) = 1 then begin
            if asDescription
              then Result:= '<code"><img src="' + FConfiguration.EditorFolder + 'img\package.png"> ' +
                             Code+ '</code>' +
                             '<br><br>The package <i>' + Code + '</i>.'
              else Result:= 'package';
            exit;
          end;
      end;
    end;
  end;

begin
  Result:= '';
  // for example:   ((Girokonto)konten.get(0)).
  // package.Class/Object.attribute.Attribute.Method().Method().Attribute. ...

  if copy(Code, 1, 1) = '(' then begin  // Typecast
    isWord:= true;
    bracket:= 1;
    i:= 2;
    while (i <= length(Code)) and (bracket > 0) do begin
      if Code[i] = '(' then inc(bracket) else
      if Code[i] = ')' then dec(bracket) else
      if IsWordBreakChar(Code[i]) then isWord:= false;
      inc(i);
    end;
    if bracket = 0 then begin
      if IsWord then
        Result:= copy(Code, 2, i-3)
      else begin
        delete(Code, i-1, i-1);
        delete(Code, 1, 1);
        Result:= getTypeOfCode(Code, Line, State, false);
      end;
      exit;
    end;
  end;

  if Result = '' then begin
    Result:= isPackage;
    if Result <> '' then exit;
  end;

  //Typ:= getType(Code, '', '', false, asDescription);

  if Typ = '' then begin
    Scanner:= TJavaScanner.create;
    try
      Scanner.Init(Code);

      {
      Scanner.CompoundTokens:= false;
      StartToken:= Scanner.getNextToken;
      Typ:= getStartType('', StartToken, Line, false);

      if Typ = StartToken then // Token is a type
        if asDescription then
          Result:= '<code"><img src="' + FConfiguration.EditorFolder + 'img\class.png"> ' +
                   '<a href="' + ToWeb('', HTMLFileofJavaClass) + '">' + Typ + '</a>' +
                   '</code><br><br>' +
                   getDescription(HTMLFileofJavaClass)
          else Result:= Typ
      else begin }

        StartToken:= Scanner.getNextToken;
        if Scanner.empty then
          Typ:= getStartType(Scanner.Tokentyp, StartToken, Line, asDescription)
        else begin
          method:= Starttoken;
          Typ:= getStartType('', StartToken, Line, false);
          if Typ = '' then   // due to constructor calls
            Typ:= getStartType('', 'this', Line, false);
          while not Scanner.empty and (Pos('<code>', Typ) <> 1) do begin // declaration of method
            Token:= Scanner.getNextToken;
            if Token = '.' then begin   // next entity
            end else if Token = '(' then begin   // a method-call
              args:= getArgTypes(Scanner, Line);
              if Scanner.empty
                then Typ:= getType(Typ, method, args, true, asDescription)
                else Typ:= getType(Typ, method, args, true, false);
            end else
              if Scanner.LookAheadToken = '(' then begin
                method:= Token;
                Continue
              end else if Scanner.empty
                then Typ:= getType(Typ, Token, '', false, asDescription)
                else Typ:= getType(Typ, Token, '', false, false);
          end;
        end;
        Result:= Typ;
    finally
      FreeAndNil(Scanner);
    end;
  end;
end;

function TCodeCompletion.getArgTypes(Scanner: TJavaScanner; line: integer): string;
  var brackets: integer; arg, typ: string;

  procedure addArg;
  begin
    if arg <> '' then begin
      typ:= getTypeOfCode(arg, Line, 0, false);
      if typ = '' then typ:= 'unknown';
      Result:= Result + typ + ', ';
      arg:= '';
    end;
  end;

begin
  Result:= '';
  brackets:= 1;
  arg:= '';
  while (brackets > 0) and not Scanner.empty do begin
    Scanner.getNextToken;
    if (brackets > 1) or ((Scanner.Token <> ',') and (Scanner.Token <> ')' )) then
      arg:= arg + ' ' + Scanner.Token;
    if (brackets = 1) and ((Scanner.Token = ',') or (Scanner.Token = ')')) then  // argument found
      addArg;
    if Scanner.Token = '(' then
      inc(brackets)
    else if Scanner.Token = ')' then
      dec(brackets);
  end;
  addArg;
  Scanner.GetNextToken;
  // delete(aObject, 1, 1); // (...).
  if endsWith(Result, ', ') then
    delete(Result, length(Result)-1, 2);
end;

function TCodeCompletion.getToTypeOfObject(aObject: string; var MethodAttribute: string; line: Integer): string;
  var p: integer;
      Typ, s: string;
      IstMethode: boolean;
begin
  aObject:= addThis(aObject);
  p:= Pos('.', aObject);
  if p = 0 then begin
    Typ:= getStartType('', aObject, line, false);
    MethodAttribute:= '';
    end
  else begin
    Typ:= getStartType('', copy(aObject, 1, p-1), line, false);
    delete(aObject, 1, p);
    p:= Pos('.', aObject);
    while p > 0 do begin
      s:= copy(aObject, 1, p-1);
      delete(aObject, 1, p);
      p:= Pos('(', s);
      IstMethode:= false;
      if p > 0 then begin
        delete(s, p, length(s));
        IstMethode:= true;
      end;
      Typ:= getType(Typ, s, '', IstMethode, false);
      p:= Pos('.', aObject);
    end;
    p:= Pos('(', aObject);
    if p > 0 then
      delete(aObject, p, length(aObject));
    MethodAttribute:= aObject;
  end;
  Result:= Typ;
end;

function TCodeCompletion.MakeAttribute(Nr: Integer; const Name, Typ: string): string;
begin
  if Nr = -1 then
    Result:= '\image{0}' +  Typ
  else begin
    Result:= '\image{' + IntToStr(Nr) + '}';
    Result:= Result + '\style{+B}' + Name + '\style{-B}: ' + Typ;
  end;
end;

function TCodeCompletion.MakeMethod(Nr: Integer; const Name, ParameterAndType: string): string;
begin
  Result:= '\image{' + IntToStr(Nr) + '}';
  Result:= Result + '\style{+B}' + Name + '\style{-B}' + ParameterAndType;
end;

procedure TCodeCompletion.CalculateSelfDefinedAttributesAndMethods(var Typ: string; line: integer; const StartWith: string; StaticOnly: boolean);
  // var parameters, for classes inherited from the API
  var MClassifier: TClassifier;
      E: TFEditForm;

  function getJavaFile(s: string): string;
    var i: integer; s1: string; aForm: TFEditForm;
  begin
    Result:= '';
    s:= ReplaceStr(s, '.', '\'); // packaged class
    s:= s + '.java';
    with FJava do
      for i:= 0 to TDIEditFormCount - 1 do begin
        aForm:= TDIEditFormGet(i);
        s1:= aForm.Pathname;
        if ExtractFileName(s1) = s then begin
          Result:= s1;
          exit;
        end;
        s1:= ExtractFilePath(aForm.Pathname) + s;
        if FileExists(s1) then begin
          Result:= s1;
          exit;
        end;
      end;
  end;

  procedure EditClass(CorI: TClassifier);
    var
      it, it2, it3, it4: IModelIterator;
      Attribute: TAttribute;
      Method: TOperation;
      Parameter: TParameter;
      ClassInserted: boolean;
      classfilename, s: string;

    procedure InsertClass(const classfilename: string);
      var aPackage: string;
    begin
      if classfilename <> '' then begin
        FJava.scpJava.InsertList.Add(classfilename);
        DocuList.Add(_(LNGOpenClass));
      end else begin
        FJava.scpJava.InsertList.Add('');
        DocuList.Add(_(LNGNoDocumentationAvailable));
      end;
      aPackage:= trim(CorI.Package);
      if aPackage <> '' then aPackage:= ' - ' + '\color{clgray}' + aPackage;

      if CorI is TClass
        then FJava.scpJava.ItemList.Add('\image{1}' + CorI.ShortName + aPackage)
        else FJava.scpJava.ItemList.Add('\image{11}' + CorI.ShortName + aPackage);
      classInserted:= true;
    end;

  begin
    // Class
    classInserted:= false;
    classfilename:= getJavaFile(CorI.Name);
    if StartsWithInsensitive(classfilename, StartWith) then
      InsertClass(classfilename);

    // Attributes
    It:= CorI.GetAttributes;
    while It.HasNext do begin
      Attribute:= It.Next as TAttribute;
      if line = Attribute.LineS then continue;
      if StaticOnly and not Attribute.Static then continue;
      if (StartWith <> '') and not StartsWithInsensitive(Attribute.Name, Startwith) then continue;
      if (Attribute.Visibility = viPrivate) and assigned(FJava.EditorForm) and (FJava.EditorForm.Pathname <> E.Pathname) then continue;

      if not classInserted then
        InsertClass(classfilename);
      FJava.scpJava.InsertList.Add(Attribute.Name);
      FJava.scpJava.ItemList.Add(MakeAttribute(Integer(Attribute.Visibility) + 2,
                   Attribute.Name, Attribute.TypeClassifier.GetShortType));
      DocuList.Add(Attribute.Documentation.Description);
    end;

    // Methods
    It:= CorI.GetOperations;
    while It.HasNext do begin
      Method:= It.Next as TOperation;
      if line = Method.LineS then continue;

      if (Method.LineS < line) and (line <= Method.LineE) then begin
        it3:= Method.GetParameters;
        while It3.HasNext do begin
          Parameter:= It3.Next as TParameter;
          if StaticOnly and not Parameter.Static then continue;
          if (StartWith <> '') and not StartsWithInsensitive(Parameter.Name, Startwith) then continue;
          FJava.scpJava.InsertList.Add(Parameter.Name);
          FJava.scpJava.ItemList.Add(MakeAttribute(15, Parameter.Name, Parameter.TypeClassifier.GetShortType));
          DocuList.Add(Parameter.Documentation.Description);
        end;
        it4:= Method.GetAttributes;
        while It4.HasNext do begin
          Attribute:= It4.Next as TAttribute;
          if StaticOnly and not Attribute.Static then continue;
          if (StartWith <> '') and not StartsWithInsensitive(Attribute.Name, StartWith) then continue;
          if Attribute.LineS > line then continue;
          FJava.scpJava.InsertList.Add(Attribute.Name);
          FJava.scpJava.ItemList.Add(MakeAttribute(14, Attribute.Name, Attribute.TypeClassifier.GetShortType));
          DocuList.Add(Attribute.Documentation.Description);
        end;
      end;

      if Method.OperationType = otConstructor
        then Continue;
      if StaticOnly and not Method.Static then continue;
      if (StartWith <> '') and not StartsWithInsensitive(Method.Name, Startwith) then continue;
      if not classInserted then
        InsertClass(classfilename);

      s:= '(';
      it2:= Method.GetParameters;
      while it2.HasNext do begin
        Parameter:= it2.next as TParameter;
        if assigned(Parameter.TypeClassifier) then
          s:= s + Parameter.TypeClassifier.GetShortType + ' ' + Parameter.Name + ', ';
      end;
      if Copy(s, length(s)-1, 2) = ', ' then
        Delete(s, length(s)-1, 2); // delete last comma
      s:= s + ')';
      if Assigned(Method.ReturnValue)
        then s:= s + ': ' + Method.ReturnValue.GetShortType
        else s:= s + ': void';
      FJava.scpJava.InsertList.Add(Method.Name + '(');
      FJava.scpJava.ItemList.Add(MakeMethod(Integer(Method.Visibility) + 7, Method.Name, s));
      s:= Method.Documentation.Description;
      if s = '' then s:= _(LNGNoDocumentationAvailable);
      DocuList.Add(s);
    end;
  end;

begin
  E:= nil;
  repeat
    MClassifier:= getMClassifier(Typ, E);
    if Assigned(MClassifier) then begin
      EditClass(MClassifier);
      Typ:= MClassifier.getAncestorName;
    end
      else break;
    if (Typ = '') and FConfiguration.ShowClassObject then
      Typ:= 'Object';
    if (Typ = '') or FConfiguration.IsAPIClassOrInterface(Typ) then break;
  until false;
end;

function TCodeCompletion.IsSelfDefinedClassOrInterface(var Classe, Pathname: string; var aEditform: TFEditForm): boolean;
begin
  Result:= true;
  Pathname:= '';
  var aClassifier:= getMClassifier(Classe, aEditForm);
  if assigned(aClassifier) then begin
    Classe:= aClassifier.Name;
    Pathname:= aClassifier.Pathname
  end else
    Result:= false;
end;

function TCodeCompletion.isJavaAPIClass(aClass: string; var FullClass: string): boolean;
  // also determines the HTMLFileofJavaClass
  var i, j: integer;
      aClassHTML, aFullClass: string;
      FUnit: TUnitPackage;
      SL, ClassImports, FullImports: TStringList;
begin
  if aClass = '' then
    exit(false);
  SL:= TStringList.Create;
  try
    SL.Text:= FConfiguration.AllDocumentations.Text;
    HTMLFileofJavaClass:= '';
    aClass:= withoutGeneric(withoutArray(aClass));
    HTMLFullClass:= aClass;

    aClassHTML:= ReplaceStr(aClass, '.', '/') + '.html';
    for i:= 0 to FConfiguration.AllDocumentations.Count - 1 do begin
      HTMLFileFromFolder:= SL[i];
      if ClassInDocFile(aClassHTML, HTMLFileFromFolder, FullClass) then
        exit(true);
    end;

    if assigned(FJava.EditorForm) then begin
      FUnit := (FJava.EditorForm.Model.ModelRoot as TLogicPackage).FindUnitPackage('Default');
      ClassImports:= FUnit.ClassImports;
      i:= ClassImports.IndexOfName(aClass);
      if i > -1 then begin
        aFullClass:= ClassImports.ValueFromIndex[i] + '.' + ClassImports.Names[i];
        aClassHTML:= ReplaceStr(aFullClass, '.', '/') + '.html';
        for j:= 0 to SL.Count - 1 do begin
          HTMLFileFromFolder:= SL[j];
          if ClassInDocFile(aClassHTML, HTMLFileFromFolder, FullClass) then
            exit(true);
        end;
      end;
      FullImports:= FUnit.FullImports;
      for i:= 0 to FullImports.Count - 1 do begin
        aClassHTML:= ReplaceStr(FullImports.Strings[i] + aClass, '.', '/') + '.html';
        for j:= 0 to SL.Count - 1 do begin
          HTMLFileFromFolder:= SL[j];
          if ClassInDocFile(aClassHTML, HTMLFileFromFolder, FullClass) then
            exit(true);
        end;
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
  Result:= false;
end;

function TCodeCompletion.isJavaAPIClass(const aClass: string): boolean;
  var dummy: string;
begin
  Result:= isJavaAPIClass(aClass, dummy);
end;

function TCodeCompletion.ClassInDocFile(aClass, aFile: string; var FullClass: string): boolean;
  var i, p: integer;
      SL: TStringList;
      s, path, theFile: string;
begin
  SL:= TStringList.Create;
  try
    path:= ExtractFilePath(aFile);
    p:= Pos('.chm\', path) + Pos('.CHM\', path);
    if p > 0 then
      delete(path, p + 4, length(path));
    if FConfiguration.GlobalFileExists(aFile) then begin
      SL.Text:= LoadTextFromFile(aFile);
      for i:= 0 to SL.Count - 1 do begin
        s:= SL[i];
        p:= Pos(aClass + '"', s);
        if (p > 1) and ((s[p-1] = '/') or (s[p-1] = '"')) then begin
          getFullClassAndPath(s, FullClass, theFile);
          HTMLFileofJavaClass:= path + theFile;
          Exit(FConfiguration.GlobalFileExists(HTMLFileofJavaClass, true));
        end;
      end;
    end;
  finally
    FreeAndNil(SL);
  end;
  Result:= false;
end;

procedure TCodeCompletion.CalculateJavaAPIAttributesAndMethods(const Objekt, Typ, StartWith: string);
  var aTyp, aFile, aClass, aPackage, aField, aMethod, aDescription: string;
      StaticOnly: boolean;

  function BildNrAusTyp(const s: string): integer;
  begin
    if Pos('protected', s) > 0
      then Result:= 4
      else Result:= 5;
  end;

  function getParameter(s: string): string;
  begin
    var p:= Pos('(', s);
    s:= Copy(s, p, length(s));
    while Pos('  ', s) > 0 do
      s:= ReplaceStr(s, '  ', ' ');
    Result:= s;
  end;

  procedure AddField;
  begin
    if not (StaticOnly and (Pos('static ', aTyp) = 0)) then begin
      if StartsWithInsensitive(aField, StartWith) then begin
        FJava.scpJava.InsertList.Add(aField);
        DocuList.Add(aField + ' ' + aDescription);
        FJava.scpJava.ItemList.Add(MakeAttribute(BildNrAusTyp(aTyp), aField, TypeWithoutDecoration(aTyp)));
      end;
    end;
  end;

  procedure AddMethod;
  begin
    if not (StaticOnly and (Pos('static ', aTyp) = 0)) then begin
      if StartsWithInsensitive(aMethod, StartWith) then begin
        FJava.scpJava.InsertList.Add(getMethodname(aMethod)+'(');
        DocuList.Add(aDescription);
        FJava.scpJava.ItemList.Add(MakeMethod(BildNrAusTyp(aTyp)+5, getMethodname(aMethod),
                                   getParameter(aMethod) + ': ' + TypeWithoutDecoration(aTyp)));
      end;
    end;
  end;

begin
  StaticOnly:= EndsWith(Typ, Objekt);
  while true do
    with HTMLParser do begin
      aFile:= HTMLFileofJavaClass;
      aClass:= ExtractClassname(HTMLFullClass);
      aPackage:= ExtractPackagename(HTMLFullClass);
      if aPackage <> '' then aPackage:= ' - ' + '\color{clgray}' + aPackage;
      FJava.scpJava.ItemList.Add('\image{1}' + aClass + aPackage);
      FJava.scpJava.InsertList.Add(HTMLFileofJavaClass);
      DocuList.Add(_('Open documentation'));
      Text:= LoadTextFromFile(aFile);
      withTable:= false;
      if FieldsFound then
        if withTable then
          repeat
            aTyp:= getTDCell;
            SplitAttributeComment(getTDCell, aField, aDescription);
            AddField;
          until getTagName <> 'TR'
        else
          repeat
            aTyp:= getDIVCell;
            aField:= getDIVCell;
            aDescription:= getDIVCell;
            AddField;
            NextDiv;
          until Pos('div class="col-first', Tag.Text) <> 1;
      if MethodsFound then
        if withTable then
          repeat
            aTyp:= getTDCell;
            SplitMethodComment(getTDCell, aMethod, aDescription);
            AddMethod;
          until getTagName <> 'TR'
        else
          repeat
            aTyp:= getDIVCell;
            aMethod:= getDIVCell;
            aDescription:= getDIVCell;
            AddMethod;
            NextDiv;
          until Pos('div class="col-first', Tag.Text) <> 1;
    if hasAPISuperclass
      then continue
      else break;
  end;
end;

function TCodeCompletion.getJavaAPIMethodParameters: TStringList;
  var s, s1, Typ, Method, Parameters, Description, Names, Types: string;
      p, q: integer; SL: TStringList;

  function getParameters(s: string): string;
    var p: integer;
  begin
    p:= Pos('(', s);
    s:= Copy(s, p+1, length(s));
    s[length(s)]:= ',';
    while Pos('  ', s) > 0 do
      s:= ReplaceStr(s, '  ', ' ');
    Result:= s;
  end;

  procedure DoParameters;
  begin
    Names:= '';
    Types:= '';
    p:= Pos(',', Parameters);
    while p > 0 do begin
      s1:= copy(Parameters, 1, p-1);
      delete(Parameters, 1, p);
      Parameters:= trim(Parameters);
      q:= Pos('>', s1);
      if q = 0
        then q:= Pos(' ', s1)
        else inc(q);
      Types:= Types + copy(s1, 1, q-1) + ',';
      Names:= Names + copy(s1, q+1, length(s1)) + ',';
      p:= Pos(',', Parameters);
    end;
    SL.Add(getMethodname(Method) + '/' + Types + '=' + Names);
  end;

begin
  s:= '';
  SL:= TStringList.Create;
  SL.NameValueSeparator:= '=';
  with HTMLParser do begin
    Text:= LoadTextFromFile(HTMLFileofJavaClass);
    if MethodsFound then begin
      if withTable then begin
        repeat
          Typ:= getTDCell;
          Description:= getTDCell;
          Method:= getMethod(Description);
          Parameters:= getParameters(Method);
          DoParameters;
        until getTagName <> 'TR';
      end else begin
        repeat
          Typ:= getDIVCell;
          Method:= getDIVCell;
          Parameters:= getParameters(Method);
          Description:= getDIVCell;
          DoParameters;
          NextDiv;
        until Pos('div class="col-first', Tag.Text) <> 1;
      end;
    end;
  end;
  Result:= SL;
end;

function TCodeCompletion.LoadTextFromFile(var Pathname: string): string;
begin
  Result:= '';
  if FConfiguration.GlobalFileExists(Pathname, false) then
    if IsCHM(Pathname) then
      Result:= LoadFromCHM(Pathname)
    else begin
      var SL:= TStringList.Create;
      SL.LoadFromFile(Pathname);
      Result:= SL.Text;
      FreeAndNil(SL);
    end;
end;

procedure TCodeCompletion.ParseSourceCodes;
  var i: Integer;
      Importer: TJavaImporter;
      Files: TStringList;
      aEditForm: TFEditForm;
begin
  if assigned(FJava) and assigned(FJava.EditorForm) and FJava.EditorForm.NeedsParsing then begin
    FJava.EditorForm.Model.Clear;
    FConfiguration.ImportCache.Clear;
    Files:= TStringlist.Create;
    Importer:= TJavaImporter.Create(FJava.EditorForm.Model, TFileprovider.Create);
    try
      with FJava do
        for i:= 0 to TDIEditFormCount - 1 do begin
          aEditForm:= TDIEditFormGet(i);
          if aEditForm.isJava then
            Files.Add(aEditForm.Pathname);
        end;
      Importer.BuildModelFrom(Files);
    finally
      FreeAndNil(Files);
      FreeAndNil(Importer);
    end;
  end;
end;

function TCodeCompletion.TypeWithoutDecoration(s: string): string;
begin
  s:= ReplaceStr(s, 'protected', '');
  s:= ReplaceStr(s, 'abstract', '');
  s:= ReplaceStr(s, 'static', '');
  s:= ReplaceStr(s, 'final', '');
  Result:= Trim(s);
end;

function TCodeCompletion.getMethod(var s: string): string;
begin
  var p:= Pos(')', s);
  Result:= ReplaceStr(Copy(s, 1, p), '&#8203;', '');
  delete(s, 1, p+1);
end;

function TCodeCompletion.getMethodWithoutParameternames(const s: string; var countParams: integer): string;
  var Token: string; Scanner: TJavaScanner;
begin
  countParams:= 0;
  try
    Scanner:= TJavaScanner.create;
    Scanner.Init(s);
    Token:= Scanner.GetNextToken;
    Result:= Token;
    while Token <> ')' do begin
      repeat
        Token:= Scanner.GetNextToken;
        Result:= Result + Token;
      until (Token = ',') or (Token = ')') or (Token = '');
      if (Token = ',') or ((Token = ')') and (Scanner.LastToken <> '(')) then begin
        Delete(Result, length(Result)-length(Scanner.LastToken), length(Result));
        Result:= Result + ', ';
        inc(countParams);
      end
      else if Token = '' then break;
    end;
    if endsWith(Result, ', ') then begin
      Delete(Result, length(Result)-1, 2);
      Result:= Result + ')';
    end;
  finally
    FreeAndNil(Scanner);
  end;
end;

function TCodeCompletion.getMethodname(const s: string): string;
begin
  var p:= Pos('(', s);
  Result:= trim(Copy(s, 1, p-1));
end;

function TCodeCompletion.getAttribute(var s: string): string;
begin
  var p:= pos(' ', s);
  Result:= Copy(s, 1, p-1);
  delete(s, 1, p);
end;

procedure TCodeCompletion.SplitAttributeComment(AttributeComment: string;
  var Attribute, Comment: string);
begin
  var p:= pos(' ', AttributeComment);
  Attribute:= Copy(AttributeComment, 1, p-1);
  Comment:= Copy(AttributeComment, p+1, length(AttributeComment));
end;

procedure TCodeCompletion.SplitMethodComment(MethodComment: string;
  var Method, Comment: string);
begin
  MethodComment:= ReplaceStr(MethodComment, '&#8203;', '');
  Method:= getMethod(MethodComment);
  Comment:= trim(MethodComment);
end;

function TCodeCompletion.getObjectFrom(s: string): string;
  var p, BracketClose: integer;
      BracketOpen: boolean;

  function valid(ch: char): boolean;
  begin
    Result:= true;
    case ch of
      ')', ']': begin inc(BracketClose); exit; end;
      '(', '[': begin
                  dec(BracketClose);
                  if BracketClose < 0 then Result:= false;
                  exit;
                end;
    end;
    if BracketClose = 0 then
      Result:= ch.isLetterOrDigit or (Pos(ch, '_."') > 0);
  end;

begin
  BracketClose:= 0;
  p:= length(s);
  while (p > 0) and valid(s[p]) do
    dec(p);
  s:= Copy(s, p+1, length(s));

  // remove array indices
  p:= 1;
  BracketOpen:= false;
  while p <= length(s) do begin
    case s[p] of
      '[': BracketOpen:= true;
      ']': BracketOpen:= false;
    else
      if BracketOpen then begin
        delete(s, p, 1);
        dec(p)
      end;
    end;
    inc(p);
  end;
  if EndsWith(s, '.') then
    s:= copy(s, 1, length(s)-1);
  Result:= s;
end;

function TCodeCompletion.DoScpJavaExecute(var State: integer;
         const LocLine, StartWith: string; aEditForm: TFEditForm): boolean;
  var Objekt, s, Token, FullClass, Typ, Pathname, ToComplete: string;
      Attri: TSynHighlighterAttributes;
      XY: TBufferCoord;
      Editor: TSynEdit;
      p: integer;

  procedure DoIfElse;
  begin
    FJava.scpJava.ItemList.Add('if');
    s:= FTemplates.GetControlStructure(1, StringOfChar(' ', Editor.CaretX-4), '');
    s:= copy(trim(FTemplates.GetControlStructure(1, StringOfChar(' ', Editor.CaretX-4), '')), 4, 200);
    FJava.scpJava.InsertList.Add(s);
    FJava.scpJava.ItemList.Add('if - else');
    s:= copy(trim(FTemplates.GetControlStructure(9, StringOfChar(' ', Editor.CaretX-4), '')), 4, 200);
    FJava.scpJava.InsertList.Add(s);
    DocuList.add('');
    DocuList.add('');
    State:= 0;
    Result:= true;
  end;

begin
  if not informed then
    CheckMissingDocumentation;
  Result:= false;
  Self.EditForm:= aEditForm;
  FJava.scpJava.ItemList.Clear;
  FJava.scpJava.InsertList.Clear;
  DocuList.Clear;
  if not FJava.scpParams.Form.Visible then
    if assigned(FJava.EditorForm) and assigned(FJava.EditorForm.ParseThread) and
      (FJava.EditorForm.ParseThread.State > 0) and (FJava.EditorForm.ParseThread.State < 3)
     then FJava.EditorForm.ParseThread.WaitFor
     else ParseSourceCodes;
  if FMessages.InteractiveEditActive
    then Editor:= FMessages.GetCurrentInteractive
  else if Assigned(FJava.EditorForm)
    then Editor:= FJava.EditorForm.Editor
    else exit;

  with Editor do begin
    // No text completion in strings
    XY:= CaretXY;
    Dec(XY.Char);
    if GetHighlighterAttriAtRowCol(XY, Token, Attri) and
       ((Attri = Highlighter.StringAttribute) or (Attri = Highlighter.CommentAttribute)) then
       exit;
    if State = 2
      then s:= LocLine
      else begin
        s:= Copy(LineText, 1, CaretX-1);
        if Startwith <> '' then begin
          s:= trim(copy(s, 1, length(s) - length(StartWith)));
          if not Endswith(s, '.') then s:= '';
        end;

      end;
  end;

  if FConfiguration.CodeCompletionCtrlSpace and
    (EndsWith(s, ' if(') or EndsWith(s, ' if') or EndsWith(s, 'if ')) then begin
    DoIfElse;
    exit;
  end;

  // different cases
  // Linetext = '',               StartWith = '',   s = '',            => Objekt = '', Typ = this.Class
  // LineText = 'zz',             StartWith = 'zz', s = ''             => Objekt = '', Typ = this.Class
  // Linetext = 'xxxx.',          StartWith = '',   s = 'xxxx.'        => Objekt = 'xxxx', Typ = type of xxxx or ''
  // Linetext = 'yyyy.xxxx().',   StartWith = '',   s = 'yyyy.xxxx().' => Objekt = 'yyyy.xxxx()', Typ = type of xxxx() or ''
  // Linetext = 'yyyy.xxxx().zz', StartWith = 'zz', s = 'yyyy.xxxx().' => Objekt = 'yyyy.xxxx()', Typ = type of xxxx() or ''
  // LineText = 'yy.xx().zz  ',   StartWith = '',   s = 'yy.xx().zz'   => Objekt = '', Typ = this.Class

  // LineText = 'Classtype oName, StartWith = oName s = Classtype      => Objekt = Classtype, Typ = Classtype
  // LineText = Attri = Valu      StartWith = Valu  s = 'Attri ='

  Objekt:= getObjectFrom(s);
  Typ:= getTypeOfCode(Objekt, Editor.CaretY, State, false);

  if (State = 0) and (Typ = '') then begin
    p:= LastDelimiter('.', Objekt);
    if p > 0 then begin
      ToComplete:= copy(Objekt, p+1, length(Objekt));
      Objekt:= copy(Objekt, 1, p-1);
      Typ:= getTypeOfCode(Objekt, Editor.CaretY, State, false);
    end
  end;

  if (Typ <> '') and not IsSimpleType(Typ) and (Typ <> 'void') then begin
    while EndsWith(Objekt, '[]') and EndsWith(Typ, '[]') do begin
      delete(Objekt, length(Objekt)-1, 2);
      delete(Typ, length(Typ)-1, 2);
    end;
    if Pos('[]', Typ) > 0 then begin
      DocuList.Add(_(LNGArrayLength));
      FJava.scpJava.InsertList.Add('length');
      FJava.scpJava.ItemList.Add(MakeAttribute(5, 'length', 'integer'));
    end else begin
      if IsSelfDefinedClassOrInterface(Typ, Pathname, aEditForm) then
        CalculateSelfDefinedAttributesAndMethods(Typ, Editor.CaretY, StartWith, Objekt = Typ);
      if (Typ <> '') and isJavaAPIClass(Typ, FullClass) or (Typ = '') and isJavaAPIClass(Objekt, FullClass) then
        CalculateJavaAPIAttributesAndMethods(Objekt, FullClass, StartWith);
      if (StartWith <> '') and (Objekt = '') then
        CalculateClassesAndInterfaces(StartWith);
    end;
  end;
  Result:= (FJava.scpJava.ItemList.Count > 0);
end;

procedure TCodeCompletion.CalculateClassesAndInterfaces(const StartWith: string);
  const ImportantPackages : array[0..8] of string =
           ('java.lang', 'java.math', 'java.io', 'java.awt', 'java.util',
            'javafx.application', 'javafx.scene.canvas', 'javafx.scene.control',
            'java.swing');

  KeyWords: array[0..51] of string = (
    'abstract', 'assert', 'boolean', 'break', 'byte', 'case', 'catch', 'char',
    'class', 'const', 'continue', 'default', 'do', 'double', 'else', 'extends',
    'for',  'float', 'false', 'final', 'finally', 'goto', 'if', 'int', 'implements',
    'import', 'instanceof', 'interface', 'long', 'new', 'null', 'native',
    'package', 'private', 'protected', 'public', 'return', 'switch', 'static', 'super',
    'short', 'strictfp', 'synchronized', 'this', 'try', 'true', 'throw', 'throws',
    'transient', 'void', 'volatile', 'while');

  var aClass, aPackage, s1, s2, indent: string;
      i, j, count: integer;
      InsertList1, InsertList2, ItemList1, ItemList2: TStrings;
      it: IModelIterator;
      cent: TClassifier;
      FUnit: TUnitPackage;

   function AddP(Package: string): string;
   begin
     if aPackage <> '' then aPackage:= ' - ' + '\color{clgray}' + aPackage;
     Result:= aPackage;
   end;

begin
  InsertList1:= TStringList.Create;
  InsertList2:= TStringList.Create;
  ItemList1:= TStringList.Create;
  ItemList2:= TStringList.Create;

  // Todo FJava.EditorForm exists?
  s1:= '';
  indent:= StringOfChar(' ', FJava.EditorForm.Editor.CaretX-Length(Startwith) - 1);
  if StartsWithInsensitive('s', StartWith) then begin
    s1:= 'String';
    s2:= '\image{16}String';
    InsertList1.Add(s1);
    ItemList1.Add(s2);
    s1:= trim(FTemplates.GetControlStructure(17, indent, ''));
    s2:= '\image{16}System.out.println()';
  end else
  if StartsWithInsensitive('m', StartWith) then begin
    s1:= trim(FTemplates.GetControlStructure(19, indent, ''));
    s2:= '\image{16}main() { }';
  end;
  if s1 <> '' then begin
    InsertList1.Add(s1);
    ItemList1.Add(s2);
  end;

  for i:= 0 to 51 do
    if StartsWith(KeyWords[i], StartWith) then begin
      if Keywords[i] = 'if' then begin
        s1:= trim(FTemplates.GetControlStructure(1, indent, ''));
        s2:= '\image{16}\style{+B}if\style{-B} () { }';
      end else
      if Keywords[i] = 'while' then begin
        s1:= trim(FTemplates.GetControlStructure(2, indent, ''));
        s2:= '\image{16}\style{+B}while\style{-B} () { }';
      end else
      if Keywords[i] = 'for' then begin
        s1:= trim(FTemplates.GetControlStructure(3, indent, ''));
        s2:= '\image{16}\style{+B}for\style{-B} (; ; ) { }';
      end else
      if Keywords[i] = 'do' then begin
        s1:= trim(FTemplates.GetControlStructure(4, indent, ''));
        s2:= '\image{16}\style{+B}do\style{-B} { } \style{+B}while\style{-B} ()';
      end else
      if Keywords[i] = 'switch' then begin
        s1:= trim(FTemplates.GetControlStructure(5, indent, ''));
        s2:= '\image{16}\style{+B}switch\style{-B} () {\style{+B}case\style{-B}: \style{+B}break\style{-B};}';
      end else
      if Keywords[i] = 'try' then begin
        s1:= trim(FTemplates.GetControlStructure(6, indent, ''));
        s2:= '\image{16}\style{+B}try\style{-B} { } \style{+B}catch\style{-B} (Exception e) { } \style{+B}finally\style{-B} { }';
      end else
      if Keywords[i] = 'else' then begin
        s1:= trim(FTemplates.GetControlStructure(7, indent, ''));
        s2:= '\image{16}\style{+B}else\style{-B} { }';
      end else
      if Keywords[i] = 'catch' then begin
        s1:= trim(FTemplates.GetControlStructure(13, indent, ''));
        s2:= '\image{16}\style{+B}catch\style{-B} { }';
      end else
      if Keywords[i] = 'finally' then begin
        s1:= trim(FTemplates.GetControlStructure(14, indent, ''));
        s2:= '\image{16}\style{+B}finally\style{-B} { }';
      end else begin
        s1:= Keywords[i];
        s2:= '\image{18}\style{+B}' + Keywords[i] + '\style{-B}';
      end;
      InsertList1.Add(s1);
      ItemList1.Add(s2);
      s1:= '';
      s2:= '';
      if Keywords[i] = 'if' then begin
        s1:= trim(FTemplates.GetControlStructure(9, indent, ''));
        s2:= '\image{16}\style{+B}if\style{-B} () { } \style{+B}else\style{-B} { }';
      end else
      if Keywords[i] = 'else' then begin
        s1:= trim(FTemplates.GetControlStructure(11, indent, ''));
        s2:= '\image{16}\style{+B}else if\style{-B} { }';
      end else
      if Keywords[i] = 'for' then begin
        s1:= trim(FTemplates.GetControlStructure(12, indent, ''));
        s2:= '\image{16}\style{+B}for\style{-B} (\style{+B}int\style{-B} i = 0; i < 10; i++) { }';
      end else
      if Keywords[i] = 'new' then begin
        s1:= trim(FTemplates.GetControlStructure(18, indent, ''));
        s2:= '\image{16}\style{+B}new\style{-B} type();';
      end else
      if Keywords[i] = 'private' then begin
        s1:= trim(FTemplates.GetControlStructure(15, indent, ''));
        s2:= '\image{16}\style{+B}private void\style{-B} name { }';
        InsertList1.Add(s1);
        ItemList1.Add(s2);
        s1:= trim(FTemplates.GetControlStructure(20, indent, ''));
        s2:= '\image{16}\style{+B}private static void\style{-B} name { }';
      end else
      if Keywords[i] = 'public' then begin
        s1:= trim(FTemplates.GetControlStructure(16, indent, ''));
        s2:= '\image{16}\style{+B}public void\style{-B} name { }';
        InsertList1.Add(s1);
        ItemList1.Add(s2);
        s1:= trim(FTemplates.GetControlStructure(21, indent, ''));
        s2:= '\image{16}\style{+B}private static void\style{-B} name { }';
      end;
      if s1 <> '' then begin
        InsertList1.Add(s1);
        ItemList1.Add(s2);
      end;
    end;

  if assigned(FJava.EditorForm) then begin
    FUnit:= (FJava.EditorForm.Model.ModelRoot as TLogicPackage).FindUnitPackage('Default');
    it:= FUnit.GetClassifiers;
    while it.hasNext do begin
      cent:= TClassifier(it.Next);
      if cent.LineS = 0 then continue;
      aclass:= WithoutGeneric(cent.Name);

      if not IsSimpleType(aClass) and StartsWith(aClass, StartWith) and
         (FJava.scpJava.ItemList.IndexOf('\image{1}' + aClass) = -1) then begin
        aPackage:= ExtractPackageName(aclass);
        InsertList1.Add(aClass);
        ItemList1.Add('\image{1}' + aClass + addP(aPackage));
      end;
    end;
  end;

  i:= 0;
  count:= FConfiguration.AllPackages.Count;
  while (i < count) and not StartsWith(FConfiguration.AllPackages.Strings[i], StartWith) do
    inc(i);
  while (i < Count) and StartsWith(FConfiguration.AllPackages.Strings[i], StartWith) do begin
    aPackage:= FConfiguration.AllPackages.Strings[i];
    InsertList2.Add(aPackage);
    ItemList2.Add('\image{17}' + aPackage);
    inc(i);
  end;

  i:= 0;
  count:= FConfiguration.AllClasses.Count;
  while (i < count) and not StartsWith(FConfiguration.AllClasses.Names[i], StartWith) do
    inc(i);
  while (i < Count) and StartsWith(FConfiguration.AllClasses.Names[i], StartWith) do begin
    aClass:= FConfiguration.AllClasses.Names[i];
    aPackage:= ExtractPackageName(FConfiguration.AllClasses.ValueFromIndex[i]);
    j:= 0;
    while j < 9 do begin
      if StartsWith(aPackage, ImportantPackages[j]) and
         (aPackage = ImportantPackages[j]) then begin
        InsertList1.Add(aClass);
        ItemList1.Add('\image{1}' + aClass + AddP(aPackage));
        j:= 10;
      end;
      inc(j);
    end;
    if j = 9 then begin
      InsertList2.Add(aClass);
      ItemList2.Add('\image{1}' + aClass + addP(aPackage));
    end;
    inc(i);
  end;
  i:= 0;
  count:= FConfiguration.AllClassPathClasses.Count;
  while (i < count) and not StartsWith(FConfiguration.AllClassPathClasses.Names[i], StartWith) do
    inc(i);
  while (i < Count) and StartsWith(FConfiguration.AllClassPathClasses.Names[i], StartWith) do begin
    aClass:= FConfiguration.AllClassPathClasses.Names[i];
    aPackage:= ExtractPackagename(FConfiguration.AllClassPathClasses.ValueFromIndex[i]);
    InsertList2.Add(aClass);
    ItemList2.Add('\image{1}' + aClass + AddP(aPackage));
    inc(i);
  end;
  FJava.scpJava.InsertList.AddStrings(InsertList1);
  FJava.scpJava.InsertList.AddStrings(InsertList2);
  FJava.scpJava.ItemList.AddStrings(ItemList1);
  FJava.scpJava.ItemList.AddStrings(ItemList2);
  FreeAndNil(InsertList1);
  FreeAndNil(InsertList2);
  FreeAndNil(ItemList1);
  FreeAndNil(ItemList2);
end;

function TCodeCompletion.DoScpParamsExecute(State: integer): Boolean;
  var locline, lookup: string;
    TmpX, savepos, StartX, i, p1, p2, max,
    ParenCounter,
    TmpLocation    : Integer;
    FoundMatch     : Boolean;
    s: string;
begin
  with FJava.scpParams.Editor do begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;
    while (TmpX > 0) and not FoundMatch do begin
      if LocLine[TmpX] = ',' then
      begin
        inc(TmpLocation);
        dec(TmpX);
      end else if LocLine[TmpX] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do begin
          if LocLine[TmpX] = ')' then inc(ParenCounter)
          else if LocLine[TmpX] = '(' then dec(ParenCounter);
          dec(TmpX);
        end;
        if TmpX > 0 then dec(TmpX);  //eat the open paren
      end else if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not IsIdentChar(locLine[TmpX]) do
          Dec(TmpX);
        if TmpX > 0 then
        begin
          SavePos := TmpX;
          while (TmpX > 0) and IsIdentChar(locLine[TmpX]) do
            dec(TmpX);
          inc(TmpX);
          lookup := Copy(LocLine, TmpX, SavePos - TmpX + 1);

          if State = 1 then  // from scpJava
            FoundMatch:= true
          else begin  // from scpParam
            s:= '';
            if FJava.AfterCodeCompletion then begin
              i:= FJava.ScpParamIndex;
              max:= FJava.ScpJava.InsertList.Count;
              if (i < max) and (FJava.ScpJava.InsertList[i] = Lookup + '(') then
                s:= FJava.ScpJava.ItemList[i];
            end else begin
              State:= 2;
              DoScpJavaExecute(State, copy(LocLine, 1, StartX-1), '', EditForm);
              i:= FJava.scpJava.InsertList.IndexOf(lookup + '(');
              if i <> -1 then s:= FJava.scpJava.ItemList[i];
            end;
            if s <> '' then begin
              if CountChar(',', s) >= TmpLocation then begin
                IndexScpJava:= i;
                p1:= Pos('(', s);
                p2:= Pos(')', s);
                s:= copy(s, p1+1, p2-p1-1);
                FJava.scpParams.ItemList.Clear;
                FJava.scpParams.ItemList.Add(s);
                FoundMatch:= true;
              end;
            end;
          end;
          if not(FoundMatch) then
          begin
            TmpX := StartX;
            dec(TmpX);
          end;
        end;
      end else dec(TmpX)
    end;
  end;
  Result:= FoundMatch;
  FJava.scpParams.Form.CurrentIndex:= 2*TmpLocation;
end;

function TCodeCompletion.FormatParamList(const S: string; CurrentIndex: Integer): string;
// FormatParamList is a function from SynCompletionProposal
// which is adapted for use in Java-Editor
begin
  Result:= '';
  var List:= TStringList.Create;
  try
    List.CommaText:= S;
    var i:= 0;
    while i < List.Count - 1 do begin
      if i = CurrentIndex then
        Result:= Result + '\style{~B}' + List[i] + ' ' + List[i+1] + '\style{~B}'
      else
        Result:= Result + List[i] + ' ' + List[i+1];
      if i < List.Count - 2 then
        Result:= Result + ', ';
      i:= i + 2;
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TCodeCompletion.HTMLPathnameToClass(Pathname: string): string;
begin
  Pathname:= ChangeFileExt(Pathname, '');
  delete(Pathname, 1, length(ExtractFilePath(HTMLFileFromFolder)));
  Result:= ReplaceStr(Pathname, '\', '.');
end;

function TCodeCompletion.getDescription(Pathname: string): string;
  var s: string;
begin
  if FConfiguration.GlobalFileExists(Pathname) then begin
    var Lines:= TStringList.Create;
    Lines.Text:= LoadTextFromFile(Pathname);
    var i:= 0;
    s:= '';
    while i < Lines.Count do begin
      if Pos('<div class="block">', Lines[i]) = 1 then begin
        while (i < Lines.Count) and (Lines[i] <> '<div class="summary">') do begin
          s:= s + Lines[i];
          inc(i);
        end;
        break;
      end;
      inc(i);
    end;
    FreeAndNil(Lines);
  end;
  Result:= s;
end;

function TCodeCompletion.SearchDocumentation(const JavaCode: string; Line: integer): string;
begin
  ParseSourceCodes;
  Result:= getTypeOfCode(JavaCode, Line, 0, true);
end;

procedure TCodeCompletion.getFullClassAndPath(Line: string; out FullClass, Path: string);
  var p: integer; aClass, aPackage, LineCopy: string;
begin
  // '<li><a title="class in javax.lang.model.util" target="classFrame" href="/j2se8/api/javax/lang/model/util/AbstractAnnotationValueVisitor6.html">AbstractAnnotationValueVisitor6</a></li>'
  // '<li><a href="java/lang/String.html" title="class in java.lang" target="classFrame">String</a></li>'
  // '<li><a href="java/util/stream/IntStream.html" title="interface in java.util.stream" target="classFrame"><span class="interfaceName">IntStream</span></a></li>'
  // <li><a href="InOut.html" title="class in &lt;Unnamed&gt;" target="classFrame">InOut</a></li>
  // <li><a href="java.base/java/lang/String.html" title="class in java.lang">String</a></li>
  FullClass:= '';
  Path:= '';
  p:= Pos('class in ', Line) + Pos('interface in ', Line);
  if p > 0 then begin
    LineCopy:= Line;
    p:= Pos(' in ', Line);
    delete(Line, 1, p+3);
    p:= Pos('"', Line);
    aPackage:= copy(Line, 1, p-1);
    p:= Pos('">', Line);
    delete(Line, 1, p+1);
    p:= Pos('</a>', Line);
    if p = 0 then p:= Pos('</A>', Line);  // due to lejos documentation
    aClass:= copy(Line, 1, p-1);
    if Pos('<span', aClass) > 0 then begin
      p:= Pos('">', aClass);
      delete(Line, 1, p+1);
      p:= Pos('</span>', Line);
      aClass:= copy(Line, 1, p-1);
    end;
    if Pos('<code>', aClass) > 0 then begin
      p:= Pos('<code>', aClass);
      delete(aClass, p, 6);
      p:= Pos('</code>', aClass);
      aClass:= copy(aClass, 1, p-1);
      delete(aClass, 1, LastDelimiter('.', aClass));
    end;
    if Pos('<i>', aClass) = 1 then begin
      delete(aClass, 1, 3);
      delete(aClass, Length(AClass)-3, 4);
    end;
    if aPackage = '&lt;Unnamed&gt;'
      then FullClass:= aClass
      else FullClass:= aPackage + '.' + aClass;
    Line:= LineCopy;
    p:= Pos(' href="', Line) + Pos(' HREF="', Line);
    delete(Line, 1, p + 6);
    p:= Pos('"', Line);
    Path:= Copy(Line, 1, p-1);
    Path:= ReplaceStr(Path, '/', '\');
  end;
end;

procedure TCodeCompletion.CheckMissingDocumentation;
begin
  var aDocumentation:= FConfiguration.getJavaManual + '\index.html';
  if not FConfiguration.GlobalFileExists(aDocumentation) then begin
    if not informed then
       ShowMessage(Format(_(LNGFileNotFound), [aDocumentation]) +
                   #13#10 + FConfiguration.LCompletionHint1.Caption + ' ' + FConfiguration.LCompletionHint2.Caption);
    informed:= true;
  end;
end;

procedure TCodeCompletion.ChangeStyle;
begin
  if FConfiguration.isDark
    then FJava.scpJava.Images:= FFileStructure.vilFileStructureDark
    else FJava.scpJava.Images:= FFileStructure.vilFileStructureLight;
  TSynBaseCompletionProposalFormEx(FJava.scpJava.Form).ChangeStyle;
end;

end.
