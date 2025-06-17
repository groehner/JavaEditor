unit UCodeCompletion;

interface

uses
  Classes,
  Controls,
  UHTMLParser,
  UScpHint,
  UModel,
  UEditorForm,
  UJavaScanner,
  SynCompletionProposal;

type

  TOnResizeEvent = procedure(Sender: TObject) of object;

  TSynBaseCompletionProposalFormEx = class(TSynBaseCompletionProposalForm)
  private
    FAge: TDateTime;
    FHintForm: TFScpHint;
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Deactivate; override;
    procedure Resize; override;
    procedure SetOnResize(ResizeEvent: TOnResizeEvent);
    procedure KeyPressW(var Key: Char);
    procedure DoFormShow(Sender: TObject);
    procedure ChangeStyle;
  end;

  TCodeCompletion = class
  private
    FCCAttribute: TAttribute;
    FCCClassifier: TClassifier;
    FCCOperation: TOperation;
    FCCParameter: TParameter;
    FDocuList: TStringList;
    FEditForm: TFEditForm;
    FHTMLFileofJavaClass: string;
    FHTMLFileFromFolder: string;
    FHTMLFullClass: string;
    FHtmlParser: THtmlParser;
    FIndexScpJava: Integer;
    FInformed: Boolean;

    FWithTable: Boolean;
    function SearchMFeature(MClassifier: TClassifier; SFeature: string;
      IsMethod: Boolean): TFeature;
    function HasAPISuperclass: Boolean;
    function AddThis(const AObject: string): string;
    function GetTooltipClass(AClassifier: TClassifier;
      var Description: string): string;
    function GetStartType(Typ, AObject: string; Line: Integer;
      AsDescription: Boolean): string;
    function GetType(const SClass, SObject, Args: string; IsMethod: Boolean;
      AsDescription: Boolean): string;
    function GetAPIType(const WhereOf, Args: string; AttrMethCons: Integer;
      AsDescription: Boolean): string;
    function MakeAttribute(Num: Integer; const Name, Typ: string): string;
    function MakeMethod(Num: Integer;
      const Name, ParameterAndType: string): string;
    function GetArgTypes(Scanner: TJavaScanner; Line: Integer): string;
    procedure CalculateSelfDefinedAttributesAndMethods(var Typ: string;
      Line: Integer; const StartWith: string; StaticOnly: Boolean);
    procedure CalculateJavaAPIAttributesAndMethods(const Objekt, Typ,
      StartWith: string);
    function ClassInDocFile(AClass, AFile: string;
      var FullClass: string): Boolean;
    procedure CalculateClassesAndInterfaces(const StartWith: string);
    function GetMethod(var Str: string): string;
    function GetMethodname(const Str: string): string;
    function GetMethodWithoutParameternames(const Str: string;
      var CountParams: Integer): string;
    function TypeWithoutDecoration(Str: string): string;
    function GetAttribute(var Str: string): string;
    procedure SplitAttributeComment(AttributeComment: string;
      var Attribute, Comment: string);
    procedure SplitMethodComment(MethodComment: string;
      var Method, Comment: string);
    function HTMLPathnameToClass(Pathname: string): string;
    function GetDescription(Pathname: string): string;
    procedure GetFullClassAndPath(Line: string; out FullClass, Path: string);
    procedure CheckMissingDocumentation;
    procedure ChangeStyle;
    function FieldsFound: Boolean;
    function ConstructorsFound: Boolean;
    function MethodsFound: Boolean;
    function GetTagName: string;
    procedure GetNextTag;
    procedure PrepareScpJavaForm;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseSourceCodes;
    function GetMClassifier(SClassifier: string; var AEditForm: TFEditForm)
      : TClassifier;
    function GetTypeOfCode(Code: string; Line, State: Integer;
      AsDescription: Boolean): string;
    function GetToTypeOfObject(AObject: string; var MethodAttribute: string;
      Line: Integer): string;
    function IsJavaAPIClass(AClass: string; var FullClass: string)
      : Boolean; overload;
    function IsJavaAPIClass(const AClass: string): Boolean; overload;
    function GetJavaAPIMethodParameters: TStringList;
    function IsSelfDefinedClassOrInterface(var Classe, Pathname: string;
      var AEditForm: TFEditForm): Boolean;
    function LoadTextFromFile(var Pathname: string): string;
    function GetObjectFrom(Str: string): string;
    function GetLine(var SClassifier: string; const SObject: string): Integer;
    function GetAPIReference(const WhereOf: string): string;
    function DoScpJavaExecute(var State: Integer;
      const LocLine, StartWith: string; AEditForm: TFEditForm): Boolean;
    function DoScpParamsExecute(State: Integer): Boolean;
    function SearchDocumentation(const JavaCode: string; Line: Integer): string;
    function FormatParamList(const Str: string; CurrentIndex: Integer): string;

    property CCAttribute: TAttribute read FCCAttribute;
    property CCClassifier: TClassifier read FCCClassifier;
    property CCOperation: TOperation read FCCOperation;
    property CCParameter: TParameter read FCCParameter;
    property DocuList: TStringList read FDocuList;
    property EditForm: TFEditForm read FEditForm;
    property HTMLFileofJavaClass: string read FHTMLFileofJavaClass;
  end;

var
  MyCodeCompletion: TCodeCompletion;

implementation

uses
  SysUtils,
  Dialogs,
  Types,
  Grids,
  Graphics,
  Themes,
  DateUtils,
  StrUtils,
  Character,
  Forms,
  UUtils,
  SynEditHighlighter,
  SynEdit,
  JvGnugettext,
  UConfiguration,
  UHTMLHelp,
  UFileStructure,
  UMessages,
  UJava,
  UFileProvider,
  UTooltip,
  UStringRessources,
  UModelEntity,
  UJavaParser,
  UTemplates;

constructor TSynBaseCompletionProposalFormEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseActivate := FormMouseActivate;
  Caption := 'CodeCompletion';
end;

procedure TSynBaseCompletionProposalFormEx.SetOnResize
  (ResizeEvent: TOnResizeEvent);
begin
  OnResize := ResizeEvent;
end;

procedure TSynBaseCompletionProposalFormEx.Deactivate;
begin
  if PtInRect(FHintForm.BoundsRect, Mouse.CursorPos) or
    (MilliSecondsBetween(Now, FAge) < 1000) then
    CurrentEditor.RemoveFocusControl(Self)
  else
    inherited Deactivate;
end;

procedure TSynBaseCompletionProposalFormEx.Resize;
begin
  inherited Resize;
  if Assigned(FHintForm) then
    FHintForm.SetBounds(Left, Top + Height - 7, Width, FHintForm.Height);
end;

procedure TSynBaseCompletionProposalFormEx.KeyPressW(var Key: Char);
begin
  if Key = #9 then
  begin
    var
    MyMousePos := Mouse.CursorPos;
    Mouse.CursorPos := Point(FHintForm.Left + 5, FHintForm.Top + 5);
    FHintForm.TabActiv := False;
    if FHintForm.CanFocus then
      FHintForm.SetFocus;
    Mouse.CursorPos := MyMousePos;
  end;
  // else inherited KeyPressW(Key);
end;

procedure TSynBaseCompletionProposalFormEx.DoFormShow(Sender: TObject);
begin
  inherited;
  FAge := Now; // due to strange behavior under Ubuntu
end;

procedure TSynBaseCompletionProposalFormEx.FormMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool := 13;
end;

procedure TSynBaseCompletionProposalFormEx.ChangeStyle;
begin
  if StyleServices.IsSystemStyle then
  begin
    ClSelect := clHighlight;
    ClSelectedText := clHighlightText;
    ClBackground := clWindow;
    ClTitleBackground := clBtnFace;
  end
  else
  begin
    Font.Color := StyleServices.GetStyleFontColor(sfTabTextInactiveNormal);
    ClSelect := StyleServices.GetSystemColor(clHighlight);
    ClSelectedText := StyleServices.GetSystemColor(clHighlightText);
    ClBackground := StyleServices.GetSystemColor(clWindow);
    ClTitleBackground := StyleServices.GetSystemColor(clBtnFace);
  end;
end;

// -----------------------------------------------------------------------------

constructor TCodeCompletion.Create;
begin
  FDocuList := TStringList.Create;
  FHtmlParser := THtmlParser.Create;
  FInformed := False;
  PrepareScpJavaForm;
end;

destructor TCodeCompletion.Destroy;
begin
  FreeAndNil(FDocuList);
  FreeAndNil(FHtmlParser);
  inherited;
end;

procedure TCodeCompletion.PrepareScpJavaForm;
var
  MyOnCancel: TNotifyEvent;
  MyOnKeyPress: TKeyPressEvent;
  MyOnValidate: TValidateEvent;
  MyOnChange: TCompletionChange;
  MyOnShow: TNotifyEvent;
  MyOnClose: TNotifyEvent;
  MyOptions: TSynCompletionOptions;
begin
  // after reducing this a click in the FScpHint-Window raised an exception
  with FJava do
  begin
    MyOptions := scpJava.Options;
    MyOnCancel := scpJava.OnCancel;
    MyOnKeyPress := scpJava.OnKeyPress;
    MyOnValidate := scpJava.OnValidate;
    MyOnChange := scpJava.OnChange;
    MyOnShow := scpJava.OnShow;
    MyOnClose := scpJava.OnClose;
    scpJava.Form := TSynBaseCompletionProposalFormEx.Create(scpJava);
    TSynBaseCompletionProposalFormEx(scpJava.Form).SetOnResize(OnResize);
    TSynBaseCompletionProposalFormEx(scpJava.Form).FHintForm := FScpHint;
    scpJava.OnCancel := MyOnCancel;
    scpJava.OnKeyPress := MyOnKeyPress;
    scpJava.OnValidate := MyOnValidate;
    scpJava.OnChange := MyOnChange;
    scpJava.OnShow := MyOnShow;
    scpJava.OnClose := MyOnClose;
    scpJava.Options := [];
    scpJava.Options := MyOptions;
    scpJava.EndOfTokenChr := '()[]. ';
    scpJava.Images := FFileStructure.vilFileStructureLight;
  end;
  ChangeStyle;
end;

function TCodeCompletion.AddThis(const AObject: string): string;
var
  Posi: Integer;
  Str: string;
begin
  Posi := Pos('.', AObject);
  if Posi > 0 then
    Str := Copy(AObject, 1, Posi - 1)
  else
    Str := AObject;
  Posi := Pos('(', Str);
  if Posi > 1 then
    Result := 'this.' + AObject
  else if Str = '' then
    Result := 'this'
  else
    Result := AObject;
end;

function TCodeCompletion.GetTooltipClass(AClassifier: TClassifier;
  var Description: string): string;
var
  Str, AClassname, Pathname: string;
  E: TFEditForm;
begin
  if not Assigned(AClassifier) or IsSimpleType(AClassifier.Name) then
    Result := '<code>'
  else
  begin
    if AClassifier is TClass then
      Str := '<code><img src="' + FConfiguration.EditorFolder +
        'img\class.png"> '
    else
      Str := '<code><img src="' + FConfiguration.EditorFolder +
        'img\interface.png"> ';
    AClassname := AClassifier.GetShortType;
    Pathname:= AClassifier.Pathname;
    if (AClassifier.Pathname <> '') or IsSelfDefinedClassOrInterface(AClassname,
      Pathname, E) then
    begin
      AClassifier.Pathname:= Pathname;
      Str := Str + AClassifier.Name;
      Description := AClassifier.Documentation.Description;
      FTooltip.SetFile(AClassifier.Pathname, IntToStr(AClassifier.LineS));
    end
    else if IsJavaAPIClass(AClassifier.Name) then
    begin
      Str := Str + '<a href="' + ToWeb('', FHTMLFileofJavaClass) + '">' +
        AClassifier.Name + '</a>';
      Description := GetDescription(FHTMLFileofJavaClass);
      FTooltip.SetURL(FHTMLFileofJavaClass);
    end;
    Result := Str + '<br><br>';
  end;
end;

function TCodeCompletion.GetStartType(Typ, AObject: string; Line: Integer;
  AsDescription: Boolean): string;
var
  CIte, It1, It2: IModelIterator;
  Cent: TModelEntity;
  MClassifier: TClassifier;
  Attr: TAttribute;
  Operation: TOperation;
  Param: TParameter;
  StringGrid: TStringGrid;
  Posi: Integer;
  E: TFEditForm;
  Pathname, Description, Str, AObjectCopy: string;
  AClassifier: TClassifier;

  function ArrayCompatibel(Attr, Objekt, Typ: string): Boolean;
  var
    Posi, Count1, Count2, Posi1: Integer;
  begin
    Result := True;
    // array Brackets are possible at the variable or the type
    // make Typ[] name instead of Typ name[]
    Posi := Pos('[', Attr);
    if Posi > 0 then
    begin
      Typ := Typ + Copy(Attr, Posi, Length(Attr));
      Attr := Copy(Attr, 1, Posi - 1);
    end
    else if Objekt = Typ then
      Exit;
    if Attr = WithoutArray(Objekt) then
    begin
      Result := True;
      // access to array-variable or whole array
      if Attr = Objekt then
        Exit;
      Count1 := CountChar('[', Objekt);
      Count2 := CountChar('[', Typ);
      if (Count1 > 0) and (Count2 > 0) then
        if Count1 = Count2 then
        begin // access to array-variable
          Posi1 := Pos('[', Objekt);
          Objekt := Copy(Objekt, 1, Posi1 - 1);
          if Attr = Objekt then
            Exit;
        end
        else if Count2 > Count1 then
        begin
          while CountChar('[', Typ) <> Count2 - Count1 do
          begin
            Posi1 := Pos('[', Typ);
            Delete(Typ, Posi1, 2);
          end;
          Exit;
        end;
    end;
    Result := False;
  end;

begin
  FCCClassifier := nil;
  FCCAttribute := nil;
  FCCOperation := nil;
  FCCParameter := nil;
  Result := Typ;
  if Result <> '' then
    Exit;
  try
    if FMessages.InteractiveEditActive then
    begin
      StringGrid := FMessages.GetCurrentStringGrid;
      for var I := 1 to StringGrid.RowCount - 1 do
        if StringGrid.Cells[0, I] = AObject then
          Result := StringGrid.Cells[1, I];
    end
    else if Assigned(FJava.EditorForm) then
    begin
      FJava.EditorForm.ParseSourceCode(False);
      if Assigned(FJava.EditorForm.Model) and
        Assigned(FJava.EditorForm.Model.ModelRoot) then
      begin
        CIte := FJava.EditorForm.Model.ModelRoot.GetAllClassifiers;
        while CIte.HasNext do
        begin
          Cent := CIte.Next;
          FCCClassifier := Cent as TClassifier;
          if ((Cent.Name = AObject) or EndsWith(Cent.Name, '.' + AObject) or
            EndsWith(Cent.Name, '$' + AObject)) and (Cent.LineS = Line) then
          begin
            if AsDescription then
            begin
              Str := GetTooltipClass(Cent as TClassifier, Description);
              Result := Str + '</code>' + Description;
              if Description = '' then
                if Cent is TInterface then
                  Result := Result + 'The interface <i>' + Cent.Name + '</i>'
                else
                  Result := Result + 'The class <i>' + Cent.Name + '</i>';
            end
            else
              Result := Cent.Name;
            Exit;
          end;
          if (Cent.LineS <= Line) and (Line <= Cent.LineE) then
          begin
            MClassifier := Cent as TClassifier;
            if (AObject = 'this') or (AObject = '') then
            begin
              Result := MClassifier.Name;
              Break;
            end;
            if AObject = 'super' then
            begin
              Result := MClassifier.GetAncestorName;
              if Result = '' then
                Result := 'Object';
              Break;
            end;

            // Methods
            It1 := MClassifier.GetOperations;
            while It1.HasNext do
            begin
              Operation := It1.Next as TOperation;
              if not((Operation.LineS <= Line) and (Line <= Operation.LineE))
              then
                Continue;

              if Operation.Name = AObject then
              begin
                if Assigned(Operation.ReturnValue) then
                  Typ := Operation.ReturnValue.Name
                else
                  Typ := 'void';
                if AsDescription or (Operation.LineS = Line) then
                begin
                  Result := GetTooltipClass(Cent as TClassifier, Description);
                  if Operation.OperationType = otConstructor then
                    Result := Result + '<img src="' +
                      FConfiguration.EditorFolder + 'img\constructor.png"> ' +
                      '<bold>' + ToHtml(Operation.ToJava) +
                      '</bold></code><br><br>Constructor <i>'
                  else
                    Result := Result + '<img src="' +
                      FConfiguration.EditorFolder + 'img\methods' +
                      IntToStr(Integer(Operation.Visibility)) + '.png"> ' +
                      '<bold>' + ToHtml(Operation.ToJava) +
                      '</bold></code><br><br>Method <i>';
                  if MClassifier is TInterface then
                    Result := Result + Operation.Name + '</i> of interface <i>'
                      + Cent.Name + '</i>.<br><br>'
                  else
                    Result := Result + Operation.Name + '</i> of class <i>' +
                      Cent.Name + '</i>.<br><br>';
                  Result := Result + Operation.GetFormattedDescription;
                end
                else
                  Result := Typ;
                FTooltip.SetFile(MClassifier.Pathname,
                  IntToStr(Operation.LineS));
                FCCOperation := Operation;
                Exit;
              end;

              // Parameter of a method
              It2 := Operation.GetParameters;
              while It2.HasNext do
              begin
                Param := It2.Next as TParameter;
                if Param.TypeClassifier = nil then
                  Continue;
                if ArrayCompatibel(Param.Name, AObject,
                  Param.TypeClassifier.ShortName) then
                begin
                  if AsDescription then
                    Result := '<code><img src="' + FConfiguration.EditorFolder +
                      'img\param.png"> ' +
                      ToHtml(Param.TypeClassifier.ShortName) + ' ' + Param.Name
                      + '</code>' + '<br><br><p>Parameter <i>' + Param.Name +
                      '</i> of method <i>' + Operation.Name + '</i>.'
                  else
                    Result := Param.TypeClassifier.Name;
                  FCCParameter := Param;
                  FTooltip.SetFile(MClassifier.Pathname, IntToStr(Param.LineS));
                  Exit;
                end;
              end;

              // local variable of a method
              It2 := Operation.GetAttributes;
              while It2.HasNext do
              begin
                Attr := It2.Next as TAttribute;
                if Attr.TypeClassifier = nil then
                  Continue;
                if ArrayCompatibel(Attr.Name, AObject,
                  Attr.TypeClassifier.ShortName) then
                begin
                  if AsDescription then
                    Result := GetTooltipClass(Attr.TypeClassifier, Description)
                      + '<img src="' + FConfiguration.EditorFolder +
                      'img\local.png"> ' + ToHtml(Attr.ToJava) + '</code>' +
                      '<br><br><p>Local variable <i>' + Attr.Name +
                      '</i> of method <i>' + Operation.Name + '</i>.'
                  else
                    Result := Attr.TypeClassifier.Name;
                  FCCAttribute := Attr;
                  FTooltip.SetFile(MClassifier.Pathname, IntToStr(Attr.LineS));
                  Exit;
                end;
              end;
            end; // end of methods

            // Attributes (after method parameters and local variables)
            It1 := MClassifier.GetAttributes;
            while It1.HasNext do
            begin
              Attr := It1.Next as TAttribute;
              if Attr.TypeClassifier = nil then
                Continue;
              if ArrayCompatibel(Attr.Name, AObject,
                Attr.TypeClassifier.ShortName) then
              begin
                if AsDescription then
                  Result := GetTooltipClass(Cent as TClassifier, Description) +
                    '<img src="' + FConfiguration.EditorFolder + 'img\attribute'
                    + IntToStr(Integer(Attr.Visibility)) + '.png"> ' +
                    ToHtml(Attr.TypeClassifier.ShortName) + ' ' + Attr.Name +
                    '</code>' + '<br><br><p>Attribute of class <i>' + Cent.Name
                    + '</i>.' + '<br><br> ' + Attr.Documentation.Description
                else
                  Result := Attr.TypeClassifier.Name;
                FCCAttribute := Attr;
                FTooltip.SetFile(MClassifier.Pathname, IntToStr(Attr.LineS));
                Exit;
              end;
            end;

          end;
        end;
      end;
    end;
  except
    on E: Exception do
      FConfiguration.Log('TOperation.AddAttribute', E);
  end;
  FCCClassifier := nil;

  if Result = '' then
  begin
    Typ := '';
    AObjectCopy := AObject;
    AObject := WithoutArray(AObject);
    if IsSelfDefinedClassOrInterface(AObject, Pathname, E) then
    begin
      AClassifier := GetMClassifier(AObjectCopy, E);
      if AsDescription then
      begin
        Result := GetTooltipClass(AClassifier, Description) + '</code>' +
          Description;
        if (Description = '') and Assigned(AClassifier) then
          if Pos('[]', AObjectCopy) = 0 then
            if (AClassifier is TInterface) then
              Result := Result + 'The interface <i>' + AClassifier.Name + '</i>'
            else
              Result := Result + 'The class <i>' + AClassifier.Name + '</i>'
          else
            Result := Result + 'Array of <i>' + AClassifier.Name + '</i>';
      end
      else
        Result := AObject;
      FCCClassifier := AClassifier;
    end
    else if IsJavaAPIClass(AObject, Typ) then
    begin
      Description := GetDescription(FHTMLFileofJavaClass);
      if AsDescription then
      begin
        if FConfiguration.IsAPIInterface(AObject) then
          Result := '<code><img src="' + FConfiguration.EditorFolder +
            'img\interface.png"> ' + '<a href="' +
            ToWeb('', FHTMLFileofJavaClass) + '">' + Typ + '</a></code>'
        else
          Result := '<code><img src="' + FConfiguration.EditorFolder +
            'img\class.png"> ' + '<a href="' + ToWeb('', FHTMLFileofJavaClass) +
            '">' + Typ + '</a></code>';
        Result := Result + '<br><br>' + Description;
      end
      else
        Result := Typ;
      FTooltip.SetURL(ToWeb('', FHTMLFileofJavaClass));
    end;
  end;

  if Result = '' then
    if (Copy(AObject, 1, 1) = '"') and (Copy(AObject, Length(AObject), 1) = '"')
    then
      Result := 'String'
    else if (Copy(AObject, 1, 1) = '''') and
      (Copy(AObject, Length(AObject), 1) = '''') then
      Result := 'char'
    else if (AObject = 'true') or (AObject = 'false') then
      Result := 'boolean'
    else
    begin
      Posi := 1;
      while Posi <= Length(AObject) do
      begin
        if CharInSet(AObject[Posi], ['(', ')', '+', '-', ' ']) then
          Inc(Posi)
        else if AObject[Posi].IsDigit then
        begin
          Inc(Posi);
          while (Posi <= Length(AObject)) and AObject[Posi].IsDigit do
            Inc(Posi);
          if (Posi <= Length(AObject)) and (AObject[Posi] = '.') then
            Result := 'double'
          else
            Result := 'int';
        end
        else
          Break;
      end;
    end;
end;

function TCodeCompletion.HasAPISuperclass: Boolean;
var
  Posi, Int: Integer;
  Str, Path: string;
begin
  with FHtmlParser do
  begin
    Int := Pos('Method Summary', Text);
    Posi := Pos('extends ', Text);
    if (Posi > 0) and (Posi < Int) then
    begin
      GotoPos(Posi);
      NextTag;
      if Tag.Name = 'A' then
      begin // extends with Link
        for var I := 0 to Tag.Params.Count - 1 do
          if UpperCase(Tag.Params.Names[I]) = 'HREF' then
          begin
            Str := Tag.Params[I];
            Str := Copy(Str, 7, Length(Str) - 7); // href="
          end;
        Posi := Pos('#', Str);
        if Posi > 0 then
          Delete(Str, Posi, Length(Str));
        Posi := Pos('?', Str);
        if Posi > 0 then
          Delete(Str, Posi, Length(Str));
        Path := ExcludeTrailingPathDelimiter(ExtractFilePath(FHTMLFileofJavaClass));
        repeat
          Posi := Pos('../', Str);
          if Posi = 1 then
          begin
            Delete(Str, 1, 3);
            Posi := LastDelimiter('\', Path);
            Delete(Path, Posi, Length(Path));
          end;
        until Posi = 0;
        FHTMLFullClass := Str;
        Posi := Pos('/api/', FHTMLFullClass);
        if Posi > 0 then
          Delete(FHTMLFullClass, 1, Posi + 4);
        Delete(FHTMLFullClass, Length(FHTMLFullClass) - 4, 5);
        FHTMLFullClass := ReplaceStr(FHTMLFullClass, '/', '.');
        Str := ReplaceStr(Str, '/', '\');
        Posi := Pos('.chm\', Path);
        if Posi > 0 then
        begin
          Delete(Path, Posi + 4, Length(Path));
          Delete(Str, 1, 1);
        end;
        if StartsWith(Str, 'https') then
          FHTMLFileofJavaClass := Str
        else
          FHTMLFileofJavaClass := Path + '\' + Str;
        if not FConfiguration.ShowClassObject and
          EndsWith(Str, 'java\lang\Object.html') then
          Exit(False)
        else
          Result := FConfiguration.GlobalFileExists(FHTMLFileofJavaClass);
      end
      else
      begin // extends without Link: extends java.lang.String
        Str := TextBetween;
        Delete(Str, 1, 8);
        Posi := Pos(#13#10, Str);
        if Posi > 0 then
          Delete(Str, Posi, Length(Str)); // ... implements
        FHTMLFullClass := Str;
        Str := ReplaceStr(Str, '.', '/') + '.html';
        Result := False;
      end;
      if not Result then
        for var I := 0 to FConfiguration.AllDocumentations.Count - 1 do
        begin
          FHTMLFileFromFolder := FConfiguration.AllDocumentations[I];
          if ClassInDocFile(Str, FHTMLFileFromFolder, FHTMLFullClass) then
          begin
            Result := True;
            Break;
          end;
        end;
    end
    else
      Result := False;
  end;
end;

function TCodeCompletion.FieldsFound: Boolean;
begin
  Result := False;
  with FHtmlParser do
  begin
    var
    Posi := Pos('Field Summary', Text);
    if Posi > 0 then
    begin
      GotoPos(Posi);
      Posi := 0;
      repeat
        Inc(Posi);
        GetNextTag;
      until (TextBetween = 'Fields') or (Tag.Name = 'CODE') or (Posi = 15);
      if (TextBetween = 'Fields') and (Posi < 15) then
      begin
        Posi := 0;
        repeat
          Inc(Posi);
          GetNextTag;
        until (Tag.Name = 'CODE') or (Posi = 40);
      end;
      Result := (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.ConstructorsFound: Boolean;
begin
  Result := False;
  with FHtmlParser do
  begin
    var
    Posi := Pos('Constructor Summary', Text);
    if Posi > 0 then
    begin
      GotoPos(Posi);
      Posi := 0;
      repeat
        Inc(Posi);
        GetNextTag;
      until (TextBetween = 'Constructors') or (Tag.Name = 'CODE') or
        (Posi = 15);
      if (TextBetween = 'Constructors') and (Posi < 15) then
      begin
        Posi := 0;
        repeat
          Inc(Posi);
          GetNextTag;
        until (Tag.Name = 'CODE') or (Posi = 40);
      end;
      Result := (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.MethodsFound: Boolean;
begin
  Result := False;
  with FHtmlParser do
  begin
    var
    Posi := Pos('Method Summary', Text);
    if Posi > 0 then
    begin
      GotoPos(Posi);
      Posi := 0;
      repeat
        Inc(Posi);
        GetNextTag;
      until (TextBetween = 'All Methods') or (TextBetween = 'Methods') or
        (Tag.Name = 'CODE') or (Posi = 15);
      if ((TextBetween = 'All Methods') or (TextBetween = 'Methods')) and
        (Posi < 15) then
      begin
        Posi := 0;
        repeat
          Inc(Posi);
          GetNextTag;
        until (Tag.Name = 'CODE') or (Posi = 60);
      end;
      Result := (Tag.Name = 'CODE');
    end;
  end;
end;

function TCodeCompletion.GetTagName: string;
begin
  with FHtmlParser do
  begin
    NextTag;
    NextTag;
    Result := Tag.Name;
    NextTag;
  end;
end;

procedure TCodeCompletion.GetNextTag;
begin
  with FHtmlParser do
  begin
    NextTag;
    if (Tag.Name = 'TR') or (Tag.Name = 'TABLE') then
      FWithTable := True;
  end;
end;

function TCodeCompletion.GetAPIReference(const WhereOf: string): string;
var
  Posi: Integer;
  Link, Path, AFile: string;

  function Found(Str: string): Boolean;
  begin
    Result := False;
    var
    Posi := Pos('#' + WhereOf, Str);
    if Posi = 0 then
      Exit;
    Delete(Str, 1, Posi + Length(WhereOf));
    if (Str = '') or (Str[1] = '(') then
      Result := True;
  end;

begin
  if FConfiguration.GlobalFileExists(FHTMLFileofJavaClass) then
    with FHtmlParser do
    begin
      repeat
        Link := getNextLink;
      until (Link = '') or Found(Link);
      if Link = '' then
        Result := FHTMLFileofJavaClass
      else
      begin
        Path := ExtractFilePath(FHTMLFileofJavaClass);
        Posi := Pos('#' + WhereOf, Link);
        AFile := Copy(Link, 1, Posi - 1);
        Delete(Link, 1, Posi - 1);
        Posi := Pos('/', AFile);
        while Posi > 0 do
        begin
          Delete(AFile, 1, Posi);
          Posi := Pos('/', AFile);
        end;
        Result := Path + AFile + Link;
      end;
    end
  else
    Result := '';
end;

function TCodeCompletion.GetType(const SClass, SObject, Args: string;
  IsMethod: Boolean; AsDescription: Boolean): string;
var
  MClassifier: TClassifier;
  AClassifier, Description, STyp: string;
  MFeature: TFeature;
  Attr: TAttribute;
  Operation: TOperation;
  Posi, AttrMethCons: Integer;
  Typ: string;
  E: TFEditForm;
  IsAPIClass: Boolean;
begin
  STyp := GetShortType(SClass);
  if IsMethod and ((SClass = SObject) or (STyp = SObject)) and (Args = '')
  then
  begin // default constructor
    if AsDescription then
      Result := '<img src="' + FConfiguration.EditorFolder +
        'img\constructor.png"> ' + '<bold>' + STyp +
        '</bold></code><br><br>Constructor <i>' + STyp + '</i>'
    else
      Result := SClass;
    Exit;
  end;
  AClassifier := SClass;
  MFeature := nil;
  Result := '';

  repeat
    MClassifier := GetMClassifier(AClassifier, E);
    if Assigned(MClassifier) then
    begin
      MFeature := SearchMFeature(MClassifier, SObject, False);
      if not Assigned(MFeature) then
        MFeature := SearchMFeature(MClassifier, SObject, True);
      if not Assigned(MFeature) then
        AClassifier := ExtractClassName(MClassifier.GetAncestorName);
    end;
  until Assigned(MFeature) or not Assigned(MClassifier) or (AClassifier = '');

  if Assigned(MFeature) then
  begin
    FCCClassifier := MClassifier;
    FCCOperation := nil;
    FCCAttribute := nil;
    FCCParameter := nil;
    if MFeature is TAttribute then
    begin
      Attr := MFeature as TAttribute;
      if AsDescription then
      begin
        Result := '<code><img src="' + FConfiguration.EditorFolder +
          'img\attribute' + IntToStr(Integer(Attr.Visibility)) + '.png"> ' +
          ToHtml(Attr.ToTypeName) + '</code>';
        if MClassifier is TInterface then
          Result := Result + '<br><br><p>Attribute of interface <i>' +
            MClassifier.Name + '</i>.'
        else
          Result := Result + '<br><br><p>Attribute of class <i>' +
            MClassifier.Name + '</i>.';
        Result := Result + '<br><br> ' + Attr.Documentation.Description;
        FTooltip.SetFile(MClassifier.Pathname, IntToStr(Attr.LineS));
      end
      else
        Result := Attr.TypeClassifier.Name;
      FCCAttribute := Attr;
      Exit;
    end
    else if MFeature is TOperation then
    begin
      Operation := MFeature as TOperation;
      if Assigned(Operation.ReturnValue) then
        Result := Operation.ReturnValue.Name // ExtractClassname
      else
        Result := 'void';
      if AsDescription then
      begin
        if Operation.OperationType = otConstructor then
          Result := GetTooltipClass(MClassifier, Description) + '<img src="' +
            FConfiguration.EditorFolder + 'img\constructor.png"> ' + '<bold>'
            + ToHtml(Operation.ToTypeName) + '</bold></code><br><br>' +
            'Constructor <i>' + Operation.Name + '</i> of class <i>' +
            MClassifier.Name + '</i>.<br><br>' +
            Operation.GetFormattedDescription
        else
          Result := GetTooltipClass(MClassifier, Description) + '<img src="' +
            FConfiguration.EditorFolder + 'img\methods' +
            IntToStr(Integer(Operation.Visibility)) + '.png"> ' + '<bold>' +
            ToHtml(Operation.ToTypeName) + '</bold></code><br><br>' +
            'Method <i>' + Operation.Name + '</i> of class <i>' +
            MClassifier.Name + '</i>.<br><br>' +
            Operation.GetFormattedDescription;
        FTooltip.SetFile(MClassifier.Pathname, IntToStr(Operation.LineS));
      end;
      FCCOperation := Operation;
      Exit;
    end;
  end;

  AClassifier := SClass;
  IsAPIClass := IsJavaAPIClass(AClassifier);
  while (AClassifier <> '') and not IsAPIClass do
  begin
    MClassifier := GetMClassifier(AClassifier, E);
    if Assigned(MClassifier) then
    begin
      AClassifier := ExtractClassName(MClassifier.GetAncestorName);
      IsAPIClass := IsJavaAPIClass(AClassifier);
    end
    else
      AClassifier := '';
  end;

  if IsAPIClass then
  begin
    if IsMethod then
      if GetShortType(SClass) = SObject then
        AttrMethCons := 0
      else
        AttrMethCons := 1
    else
      AttrMethCons := 2;
    Typ := GetAPIType(SObject, Args, AttrMethCons, AsDescription);
    if Typ = 'E' then
    begin
      Posi := Pos('<', SClass);
      if Posi > 0 then
        Typ := Copy(SClass, Posi + 1, Length(SClass) - Posi - 1)
      else
        Typ := 'Object';
    end;
    if Typ = '' then
      Typ := SClass;
    Result := Typ;
  end;
end;

function TCodeCompletion.GetLine(var SClassifier: string;
  const SObject: string): Integer;
var
  MClassifier: TClassifier;
  MFeature: TFeature;
  E: TFEditForm;
begin
  Result := 1;
  MFeature := nil;
  repeat
    MClassifier := GetMClassifier(SClassifier, E);
    if Assigned(MClassifier) then
    begin
      Result := MClassifier.LineS;
      if SObject = '' then
        Exit;
      MFeature := SearchMFeature(MClassifier, SObject, False);
      if not Assigned(MFeature) then
        MFeature := SearchMFeature(MClassifier, SObject, True);
      if not Assigned(MFeature) then
        SClassifier := MClassifier.GetAncestorName;
    end;
  until Assigned(MFeature) or not Assigned(MClassifier) or (SClassifier = '');
  if Assigned(MFeature) then
  begin
    if MFeature is TAttribute then
      Result := (MFeature as TAttribute).LineS;
    if MFeature is TOperation then
      Result := (MFeature as TOperation).LineS;
  end
  else
    Result := -1;
end;

function TCodeCompletion.GetMClassifier(SClassifier: string;
  var AEditForm: TFEditForm): TClassifier;
var
  Str, SClassifierPath: string;

  function getClassifier: TClassifier;
  begin
    Result := nil;
    var
    CIte := AEditForm.Model.ModelRoot.GetAllClassifiers;
    while not Assigned(Result) and CIte.HasNext do
    begin
      var
      Cent := CIte.Next;
      if (Cent.Name = SClassifier) or (GetShortType(Cent.Name) = SClassifier)
      then
        Result := (Cent as TClassifier);
    end;
  end;

begin
  Result := nil;
  try
    SClassifier := WithoutGeneric(WithoutArray(SClassifier));
    SClassifierPath := ReplaceStr(SClassifier, '.', '\') + '.java';

    for var I := 0 to FJava.TDIEditFormCount - 1 do
    begin
      AEditForm := FJava.TDIEditFormGet(I);
      if AEditForm.IsJava and
        (ExtractFileName(AEditForm.Pathname)
        = ExtractFileName(SClassifierPath)) then
      begin
        Result := getClassifier;
        if Assigned(Result) then
        begin
          if not Result.SourceRead then
            AEditForm.ParseSourceCode(False);
          Exit;
        end;
      end;
    end;
    if Assigned(FJava.ActiveTDIChild) and (FJava.ActiveTDIChild.FormTag = 1)
    then
    begin
      AEditForm := FJava.ActiveTDIChild as TFEditForm;
      Str := ExtractFilePath(AEditForm.Pathname) + ToBackSlash(SClassifier)
        + '.java';
      if (AEditForm.Pathname <> '') and FileExists(Str) then
      begin
        AEditForm := FJava.OpenEditForm(Str, True); // hidden
        Result := getClassifier;
        if Assigned(Result) then
          Exit;
      end;
    end;
    AEditForm := nil;
  except
    on E: Exception do
      ErrorMsg(E.Message);
  end;
end;

function TCodeCompletion.SearchMFeature(MClassifier: TClassifier;
  SFeature: string; IsMethod: Boolean): TFeature;
begin
  Result := nil;
  SFeature := WithoutArray(SFeature);
  var
  Ite := MClassifier.GetFeatures;
  while not Assigned(Result) and Ite.HasNext do
  begin
    var
    Feature := Ite.Next as TFeature;
    if Feature.Name = SFeature then
      if ((Feature is TAttribute) and not IsMethod) or
        ((Feature is TOperation) and IsMethod) then
        Result := Feature;
  end;
end;

function TCodeCompletion.GetTypeOfCode(Code: string; Line, State: Integer;
  AsDescription: Boolean): string;
var
  Int: Integer;
  Typ, Method, Args, Token, StartToken: string;
  IsWord: Boolean;
  Bracket: Integer;
  MyUnit: TUnitPackage;
  Scanner: TJavaScanner;

  function IsPackage: string;
  begin
    if Assigned(FJava.EditorForm) and Assigned(FJava.EditorForm.Model) and
      Assigned(FJava.EditorForm.Model.ModelRoot) then
    begin // package
      MyUnit := FJava.EditorForm.Model.ModelRoot.FindUnitPackage('Default');
      if Assigned(MyUnit) then
      begin
        for var I := 0 to MyUnit.FullImports.Count - 1 do
          if Pos(Code + '.', MyUnit.FullImports[I]) = 1 then
          begin
            if AsDescription then
              Result := '<code><img src="' + FConfiguration.EditorFolder +
                'img\package.png"> ' + Code + '</code>' +
                '<br><br>The package <i>' + Code + '</i>.'
            else
              Result := 'package';
            Exit;
          end;
        for var I := 0 to MyUnit.ClassImports.Count - 1 do
          if Code = MyUnit.ClassImports.ValueFromIndex[I] + '.' +
            MyUnit.ClassImports.Names[I] then
          begin
            if AsDescription then
              Result := '<code"><img src="' + FConfiguration.EditorFolder +
                'img\class.png"> ' + Code + '</code>' +
                '<br><br>The imported class <i>' + Code + '</i>.'
            else
              Result := Code;
            Exit;
          end
          else if Pos(Code, MyUnit.ClassImports.ValueFromIndex[I] + '.' +
            MyUnit.ClassImports.Names[I]) = 1 then
          begin
            if AsDescription then
              Result := '<code"><img src="' + FConfiguration.EditorFolder +
                'img\package.png"> ' + Code + '</code>' +
                '<br><br>The package <i>' + Code + '</i>.'
            else
              Result := 'package';
            Exit;
          end;
      end;
    end;
  end;

begin
  Result := '';
  // for example:   ((Girokonto)konten.get(0)).
  // package.Class/Object.attribute.Attribute.Method().Method().Attribute. ...

  if Copy(Code, 1, 1) = '(' then
  begin // Typecast
    IsWord := True;
    Bracket := 1;
    Int := 2;
    while (Int <= Length(Code)) and (Bracket > 0) do
    begin
      if Code[Int] = '(' then
        Inc(Bracket)
      else if Code[Int] = ')' then
        Dec(Bracket)
      else if IsWordBreakChar(Code[Int]) then
        IsWord := False;
      Inc(Int);
    end;
    if Bracket = 0 then
    begin
      if IsWord then
        Result := Copy(Code, 2, Int - 3)
      else
      begin
        Delete(Code, Int - 1, Int - 1);
        Delete(Code, 1, 1);
        Result := GetTypeOfCode(Code, Line, State, False);
      end;
      Exit;
    end;
  end;

  if Result = '' then
  begin
    Result := IsPackage;
    if Result <> '' then
      Exit;
  end;

  // Typ:= GetType(Code, '', '', false, AsDescription);

  if Typ = '' then
  begin
    Scanner := TJavaScanner.Create;
    try
      Scanner.Init(Code);
      StartToken := Scanner.GetNextToken;
      if Scanner.Empty then
        Typ := GetStartType(Scanner.TokenTyp, StartToken, Line, AsDescription)
      else
      begin
        Method := StartToken;
        Typ := GetStartType('', StartToken, Line, False);
        if Typ = '' then // due to constructor calls
          Typ := GetStartType('', 'this', Line, False);
        while not Scanner.Empty and (Pos('<code>', Typ) <> 1) do
        begin // declaration of method
          Token := Scanner.GetNextToken;
          if Token = '.' then
          begin // next entity
          end
          else if Token = '(' then
          begin // a method-call
            Args := GetArgTypes(Scanner, Line);
            if Scanner.Empty then
              Typ := GetType(Typ, Method, Args, True, AsDescription)
            else
              Typ := GetType(Typ, Method, Args, True, False);
          end
          else if Scanner.LookAheadToken = '(' then
          begin
            Method := Token;
            Continue;
          end
          else if Scanner.Empty then
            Typ := GetType(Typ, Token, '', False, AsDescription)
          else
            Typ := GetType(Typ, Token, '', False, False);
        end;
      end;
      Result := Typ;
    finally
      FreeAndNil(Scanner);
    end;
  end;
end;

function TCodeCompletion.GetArgTypes(Scanner: TJavaScanner;
  Line: Integer): string;
var
  Brackets: Integer;
  Arg, Typ: string;

  procedure addArg;
  begin
    if Arg <> '' then
    begin
      Typ := GetTypeOfCode(Arg, Line, 0, False);
      if Typ = '' then
        Typ := 'unknown';
      Result := Result + Typ + ', ';
      Arg := '';
    end;
  end;

begin
  Result := '';
  Brackets := 1;
  Arg := '';
  while (Brackets > 0) and not Scanner.Empty do
  begin
    Scanner.GetNextToken;
    if (Brackets > 1) or ((Scanner.Token <> ',') and (Scanner.Token <> ')'))
    then
      Arg := Arg + ' ' + Scanner.Token;
    if (Brackets = 1) and ((Scanner.Token = ',') or (Scanner.Token = ')'))
    then // argument Found
      addArg;
    if Scanner.Token = '(' then
      Inc(Brackets)
    else if Scanner.Token = ')' then
      Dec(Brackets);
  end;
  addArg;
  Scanner.GetNextToken;
  if EndsWith(Result, ', ') then
    Delete(Result, Length(Result) - 1, 2);
end;

function TCodeCompletion.GetToTypeOfObject(AObject: string;
  var MethodAttribute: string; Line: Integer): string;
var
  Posi: Integer;
  Typ, Str: string;
  IstMethode: Boolean;
begin
  AObject := AddThis(AObject);
  Posi := Pos('.', AObject);
  if Posi = 0 then
  begin
    Typ := GetStartType('', AObject, Line, False);
    MethodAttribute := '';
  end
  else
  begin
    Typ := GetStartType('', Copy(AObject, 1, Posi - 1), Line, False);
    Delete(AObject, 1, Posi);
    Posi := Pos('.', AObject);
    while Posi > 0 do
    begin
      Str := Copy(AObject, 1, Posi - 1);
      Delete(AObject, 1, Posi);
      Posi := Pos('(', Str);
      IstMethode := False;
      if Posi > 0 then
      begin
        Delete(Str, Posi, Length(Str));
        IstMethode := True;
      end;
      Typ := GetType(Typ, Str, '', IstMethode, False);
      Posi := Pos('.', AObject);
    end;
    Posi := Pos('(', AObject);
    if Posi > 0 then
      Delete(AObject, Posi, Length(AObject));
    MethodAttribute := AObject;
  end;
  Result := Typ;
end;

function TCodeCompletion.MakeAttribute(Num: Integer;
  const Name, Typ: string): string;
begin
  if Num = -1 then
    Result := '\image{0}' + Typ
  else
  begin
    Result := '\image{' + IntToStr(Num) + '}';
    Result := Result + '\style{+B}' + Name + '\style{-B}: ' + Typ;
  end;
end;

function TCodeCompletion.MakeMethod(Num: Integer;
  const Name, ParameterAndType: string): string;
begin
  Result := '\image{' + IntToStr(Num) + '}';
  Result := Result + '\style{+B}' + Name + '\style{-B}' + ParameterAndType;
end;

procedure TCodeCompletion.CalculateSelfDefinedAttributesAndMethods
  (var Typ: string; Line: Integer; const StartWith: string;
  StaticOnly: Boolean);
// var parameters, for classes inherited from the API
var
  MClassifier: TClassifier;
  E: TFEditForm;

  function getJavaFile(Str: string): string;
  var
    Str1: string;
    AForm: TFEditForm;
  begin
    Result := '';
    Str := ReplaceStr(Str, '.', '\'); // packaged class
    Str := Str + '.java';
    with FJava do
      for var I := 0 to TDIEditFormCount - 1 do
      begin
        AForm := TDIEditFormGet(I);
        Str1 := AForm.Pathname;
        if ExtractFileName(Str1) = Str then
        begin
          Result := Str1;
          Exit;
        end;
        Str1 := ExtractFilePath(AForm.Pathname) + Str;
        if FileExists(Str1) then
        begin
          Result := Str1;
          Exit;
        end;
      end;
  end;

  procedure EditClass(CorI: TClassifier);
  var
    It1, It2, It3, It4: IModelIterator;
    Attribute: TAttribute;
    Method: TOperation;
    Parameter: TParameter;
    ClassInserted: Boolean;
    Classfilename, Str: string;

    procedure InsertClass(const Classfilename: string);
    var
      APackage: string;
    begin
      if Classfilename <> '' then
      begin
        FJava.scpJava.InsertList.Add(Classfilename);
        FDocuList.Add(_(LNGOpenClass));
      end
      else
      begin
        FJava.scpJava.InsertList.Add('');
        FDocuList.Add(_(LNGNoDocumentationAvailable));
      end;
      APackage := Trim(CorI.Package);
      if APackage <> '' then
        APackage := ' - ' + '\color{clgray}' + APackage;

      if CorI is TClass then
        FJava.scpJava.ItemList.Add('\image{1}' + CorI.ShortName + APackage)
      else
        FJava.scpJava.ItemList.Add('\image{11}' + CorI.ShortName + APackage);
      ClassInserted := True;
    end;

  begin
    // Class
    ClassInserted := False;
    Classfilename := getJavaFile(CorI.Name);
    if StartsWithInsensitive(Classfilename, StartWith) then
      InsertClass(Classfilename);

    // Attributes
    It1 := CorI.GetAttributes;
    while It1.HasNext do
    begin
      Attribute := It1.Next as TAttribute;
      if Line = Attribute.LineS then
        Continue;
      if StaticOnly and not Attribute.Static then
        Continue;
      if (StartWith <> '') and not StartsWithInsensitive(Attribute.Name,
        StartWith) then
        Continue;
      if (Attribute.Visibility = viPrivate) and Assigned(FEditForm) and
        (FEditForm.Pathname <> E.Pathname) then
        Continue;

      if not ClassInserted then
        InsertClass(Classfilename);
      FJava.scpJava.InsertList.Add(Attribute.Name);
      FJava.scpJava.ItemList.Add(MakeAttribute(Integer(Attribute.Visibility) +
        2, Attribute.Name, Attribute.TypeClassifier.GetShortType));
      FDocuList.Add(Attribute.Documentation.Description);
    end;

    // Methods
    It1 := CorI.GetOperations;
    while It1.HasNext do
    begin
      Method := It1.Next as TOperation;
      if Line = Method.LineS then
        Continue;

      if (Method.LineS < Line) and (Line <= Method.LineE) then
      begin
        It3 := Method.GetParameters;
        while It3.HasNext do
        begin
          Parameter := It3.Next as TParameter;
          if StaticOnly and not Parameter.Static then
            Continue;
          if (StartWith <> '') and not StartsWithInsensitive(Parameter.Name,
            StartWith) then
            Continue;
          FJava.scpJava.InsertList.Add(Parameter.Name);
          FJava.scpJava.ItemList.Add(MakeAttribute(15, Parameter.Name,
            Parameter.TypeClassifier.GetShortType));
          FDocuList.Add(Parameter.Documentation.Description);
        end;
        It4 := Method.GetAttributes;
        while It4.HasNext do
        begin
          Attribute := It4.Next as TAttribute;
          if StaticOnly and not Attribute.Static then
            Continue;
          if (StartWith <> '') and not StartsWithInsensitive(Attribute.Name,
            StartWith) then
            Continue;
          if Attribute.LineS > Line then
            Continue;
          FJava.scpJava.InsertList.Add(Attribute.Name);
          FJava.scpJava.ItemList.Add(MakeAttribute(14, Attribute.Name,
            Attribute.TypeClassifier.GetShortType));
          FDocuList.Add(Attribute.Documentation.Description);
        end;
      end;

      if Method.OperationType = otConstructor then
        Continue;
      if StaticOnly and not Method.Static then
        Continue;
      if (StartWith <> '') and not StartsWithInsensitive(Method.Name,
        StartWith) then
        Continue;
      if not ClassInserted then
        InsertClass(Classfilename);

      Str := '(';
      It2 := Method.GetParameters;
      while It2.HasNext do
      begin
        Parameter := It2.Next as TParameter;
        if Assigned(Parameter.TypeClassifier) then
          Str := Str + Parameter.TypeClassifier.GetShortType + ' ' +
            Parameter.Name + ', ';
      end;
      if Copy(Str, Length(Str) - 1, 2) = ', ' then
        Delete(Str, Length(Str) - 1, 2); // delete last comma
      Str := Str + ')';
      if Assigned(Method.ReturnValue) then
        Str := Str + ': ' + Method.ReturnValue.GetShortType
      else
        Str := Str + ': void';
      FJava.scpJava.InsertList.Add(Method.Name + '(');
      FJava.scpJava.ItemList.Add(MakeMethod(Integer(Method.Visibility) + 7,
        Method.Name, Str));
      Str := Method.Documentation.Description;
      if Str = '' then
        Str := _(LNGNoDocumentationAvailable);
      FDocuList.Add(Str);
    end;
  end;

begin
  E := nil;
  repeat
    MClassifier := GetMClassifier(Typ, E);
    if Assigned(MClassifier) then
    begin
      EditClass(MClassifier);
      Typ := MClassifier.GetAncestorName;
    end
    else
      Break;
    if (Typ = '') and FConfiguration.ShowClassObject then
      Typ := 'Object';
    if (Typ = '') or FConfiguration.IsAPIClassOrInterface(Typ) then
      Break;
  until False;
end;

function TCodeCompletion.IsSelfDefinedClassOrInterface(var Classe,
  Pathname: string; var AEditForm: TFEditForm): Boolean;
begin
  Result := True;
  Pathname := '';
  var
  AClassifier := GetMClassifier(Classe, AEditForm);
  if Assigned(AClassifier) then
  begin
    Classe := AClassifier.Name;
    Pathname := AClassifier.Pathname;
  end
  else
    Result := False;
end;

function TCodeCompletion.IsJavaAPIClass(AClass: string;
  var FullClass: string): Boolean;
// also determines the FHTMLFileofJavaClass
var
  Int: Integer;
  AClassHTML, AFullClass: string;
  FUnit: TUnitPackage;
  StringList, ClassImports, FullImports: TStringList;
begin
  if AClass = '' then
    Exit(False);
  StringList := TStringList.Create;
  try
    StringList.Text := FConfiguration.AllDocumentations.Text;
    FHTMLFileofJavaClass := '';
    AClass := WithoutGeneric(WithoutArray(AClass));
    FHTMLFullClass := AClass;

    AClassHTML := ReplaceStr(AClass, '.', '/') + '.html';
    for var I := 0 to FConfiguration.AllDocumentations.Count - 1 do
    begin
      FHTMLFileFromFolder := StringList[I];
      if ClassInDocFile(AClassHTML, FHTMLFileFromFolder, FullClass) then
        Exit(True);
    end;

    if Assigned(FJava.EditorForm) then
    begin
      FUnit := FJava.EditorForm.Model.ModelRoot.FindUnitPackage('Default');
      ClassImports := FUnit.ClassImports;
      Int := ClassImports.IndexOfName(AClass);
      if Int > -1 then
      begin
        AFullClass := ClassImports.ValueFromIndex[Int] + '.' +
          ClassImports.Names[Int];
        AClassHTML := ReplaceStr(AFullClass, '.', '/') + '.html';
        for var J := 0 to StringList.Count - 1 do
        begin
          FHTMLFileFromFolder := StringList[J];
          if ClassInDocFile(AClassHTML, FHTMLFileFromFolder, FullClass) then
            Exit(True);
        end;
      end;
      FullImports := FUnit.FullImports;
      for var I := 0 to FullImports.Count - 1 do
      begin
        AClassHTML := ReplaceStr(FullImports[I] + AClass, '.', '/')
          + '.html';
        for var J := 0 to StringList.Count - 1 do
        begin
          FHTMLFileFromFolder := StringList[J];
          if ClassInDocFile(AClassHTML, FHTMLFileFromFolder, FullClass) then
            Exit(True);
        end;
      end;
    end;
  finally
    FreeAndNil(StringList);
  end;
  Result := False;
end;

function TCodeCompletion.IsJavaAPIClass(const AClass: string): Boolean;
var
  Dummy: string;
begin
  Result := IsJavaAPIClass(AClass, Dummy);
end;

function TCodeCompletion.ClassInDocFile(AClass, AFile: string;
  var FullClass: string): Boolean;
var
  Posi: Integer;
  StringList: TStringList;
  Str, Path, TheFile: string;
begin
  StringList := TStringList.Create;
  try
    Path := ExtractFilePath(AFile);
    Posi := Pos('.chm\', Path) + Pos('.CHM\', Path);
    if Posi > 0 then
      Delete(Path, Posi + 4, Length(Path));
    if FConfiguration.GlobalFileExists(AFile) then
    begin
      StringList.Text := LoadTextFromFile(AFile);
      for var I := 0 to StringList.Count - 1 do
      begin
        Str := StringList[I];
        Posi := Pos(AClass + '"', Str);
        if (Posi > 1) and ((Str[Posi - 1] = '/') or (Str[Posi - 1] = '"'))
        then
        begin
          GetFullClassAndPath(Str, FullClass, TheFile);
          FHTMLFileofJavaClass := Path + TheFile;
          Exit(FConfiguration.GlobalFileExists(FHTMLFileofJavaClass, True));
        end;
      end;
    end;
  finally
    FreeAndNil(StringList);
  end;
  Result := False;
end;

procedure TCodeCompletion.CalculateJavaAPIAttributesAndMethods(const Objekt,
  Typ, StartWith: string);
var
  ATyp, AFile, AClass, APackage, AField, AMethod, ADescription: string;
  StaticOnly: Boolean;

  function BildNrAusTyp(const Str: string): Integer;
  begin
    if Pos('protected', Str) > 0 then
      Result := 4
    else
      Result := 5;
  end;

  function getParameter(Str: string): string;
  begin
    var
    Posi := Pos('(', Str);
    Str := Copy(Str, Posi, Length(Str));
    while Pos('  ', Str) > 0 do
      Str := ReplaceStr(Str, '  ', ' ');
    Result := Str;
  end;

  procedure AddField;
  begin
    if not(StaticOnly and (Pos('static ', ATyp) = 0)) then
    begin
      if StartsWithInsensitive(AField, StartWith) then
      begin
        FJava.scpJava.InsertList.Add(AField);
        FDocuList.Add(AField + ' ' + ADescription);
        FJava.scpJava.ItemList.Add(MakeAttribute(BildNrAusTyp(ATyp), AField,
          TypeWithoutDecoration(ATyp)));
      end;
    end;
  end;

  procedure AddMethod;
  begin
    if not(StaticOnly and (Pos('static ', ATyp) = 0)) then
    begin
      if StartsWithInsensitive(AMethod, StartWith) then
      begin
        FJava.scpJava.InsertList.Add(GetMethodname(AMethod) + '(');
        FDocuList.Add(ADescription);
        FJava.scpJava.ItemList.Add(MakeMethod(BildNrAusTyp(ATyp) + 5,
          GetMethodname(AMethod), getParameter(AMethod) + ': ' +
          TypeWithoutDecoration(ATyp)));
      end;
    end;
  end;

begin
  StaticOnly := EndsWith(Typ, Objekt);
  while True do
    with FHtmlParser do
    begin
      AFile := FHTMLFileofJavaClass;
      AClass := ExtractClassName(FHTMLFullClass);
      APackage := ExtractPackageName(FHTMLFullClass);
      if APackage <> '' then
        APackage := ' - ' + '\color{clgray}' + APackage;
      FJava.scpJava.ItemList.Add('\image{1}' + AClass + APackage);
      FJava.scpJava.InsertList.Add(FHTMLFileofJavaClass);
      FDocuList.Add(_('Open documentation'));
      Text := LoadTextFromFile(AFile);
      FWithTable := False;
      if FieldsFound then
        if FWithTable then
          repeat
            ATyp := getTDCell;
            SplitAttributeComment(getTDCell, AField, ADescription);
            AddField;
          until GetTagName <> 'TR'
        else
          repeat
            ATyp := getDIVCell;
            AField := getDIVCell;
            ADescription := getDIVCell;
            AddField;
            NextDIV;
          until Pos('div class="col-first', Tag.Text) <> 1;
      if MethodsFound then
        if FWithTable then
          repeat
            ATyp := getTDCell;
            SplitMethodComment(getTDCell, AMethod, ADescription);
            AddMethod;
          until GetTagName <> 'TR'
        else
          repeat
            ATyp := getDIVCell;
            AMethod := getDIVCell;
            ADescription := getDIVCell;
            AddMethod;
            NextDIV;
          until Pos('div class="col-first', Tag.Text) <> 1;
      if HasAPISuperclass then
        Continue
      else
        Break;
    end;
end;

function TCodeCompletion.GetJavaAPIMethodParameters: TStringList;
var
  Str1, Typ, Method, Parameters, Description, Names, Types: string;
  Posi, Qidx: Integer;
  StringList: TStringList;

  function GetParameters(Str: string): string;
  var
    Posi: Integer;
  begin
    Posi := Pos('(', Str);
    Str := Copy(Str, Posi + 1, Length(Str));
    Str[Length(Str)] := ',';
    while Pos('  ', Str) > 0 do
      Str := ReplaceStr(Str, '  ', ' ');
    Result := Str;
  end;

  procedure DoParameters;
  begin
    Names := '';
    Types := '';
    Posi := Pos(',', Parameters);
    while Posi > 0 do
    begin
      Str1 := Copy(Parameters, 1, Posi - 1);
      Delete(Parameters, 1, Posi);
      Parameters := Trim(Parameters);
      Qidx := Pos('>', Str1);
      if Qidx = 0 then
        Qidx := Pos(' ', Str1)
      else
        Inc(Qidx);
      Types := Types + Copy(Str1, 1, Qidx - 1) + ',';
      Names := Names + Copy(Str1, Qidx + 1, Length(Str1)) + ',';
      Posi := Pos(',', Parameters);
    end;
    StringList.Add(GetMethodname(Method) + '/' + Types + '=' + Names);
  end;

begin
  StringList := TStringList.Create;
  StringList.NameValueSeparator := '=';
  with FHtmlParser do
  begin
    Text := LoadTextFromFile(FHTMLFileofJavaClass);
    if MethodsFound then
    begin
      if FWithTable then
      begin
        repeat
          Typ := getTDCell;
          Description := getTDCell;
          Method := GetMethod(Description);
          Parameters := GetParameters(Method);
          DoParameters;
        until GetTagName <> 'TR';
      end
      else
      begin
        repeat
          Typ := getDIVCell;
          Method := getDIVCell;
          Parameters := GetParameters(Method);
          Description := getDIVCell;
          DoParameters;
          NextDIV;
        until Pos('div class="col-first', Tag.Text) <> 1;
      end;
    end;
  end;
  Result := StringList;
end;

function TCodeCompletion.LoadTextFromFile(var Pathname: string): string;
begin
  Result := '';
  if FConfiguration.GlobalFileExists(Pathname, False) then
    if IsCHM(Pathname) then
      Result := LoadFromCHM(Pathname)
    else
    begin
      var
      StringList := TStringList.Create;
      StringList.LoadFromFile(Pathname);
      Result := StringList.Text;
      FreeAndNil(StringList);
    end;
end;

procedure TCodeCompletion.ParseSourceCodes;
var
  Importer: TJavaImporter;
  Files: TStringList;
  AEditForm: TFEditForm;
begin
  if Assigned(FJava) and Assigned(FJava.EditorForm) and FJava.EditorForm.NeedsParsing
  then
  begin
    FJava.EditorForm.Model.Clear;
    FConfiguration.ImportCache.Clear;
    Files := TStringList.Create;
    Importer := TJavaImporter.Create(FJava.EditorForm.Model,
      TFileProvider.Create);
    try
      with FJava do
        for var I := 0 to TDIEditFormCount - 1 do
        begin
          AEditForm := TDIEditFormGet(I);
          if AEditForm.IsJava then
            Files.Add(AEditForm.Pathname);
        end;
      Importer.BuildModelFrom(Files);
    finally
      FreeAndNil(Files);
      FreeAndNil(Importer);
    end;
  end;
end;

function TCodeCompletion.TypeWithoutDecoration(Str: string): string;
begin
  Str := ReplaceStr(Str, 'protected', '');
  Str := ReplaceStr(Str, 'abstract', '');
  Str := ReplaceStr(Str, 'static', '');
  Str := ReplaceStr(Str, 'final', '');
  Result := Trim(Str);
end;

function TCodeCompletion.GetMethod(var Str: string): string;
begin
  var
  Posi := Pos(')', Str);
  Result := ReplaceStr(Copy(Str, 1, Posi), '&#8203;', '');
  Delete(Str, 1, Posi + 1);
end;

function TCodeCompletion.GetMethodWithoutParameternames(const Str: string;
  var CountParams: Integer): string;
var
  Token: string;
  Scanner: TJavaScanner;
begin
  CountParams := 0;
  try
    Scanner := TJavaScanner.Create;
    Scanner.Init(Str);
    Token := Scanner.GetNextToken;
    Result := Token;
    while Token <> ')' do
    begin
      repeat
        Token := Scanner.GetNextToken;
        Result := Result + Token;
      until (Token = ',') or (Token = ')') or (Token = '');
      if (Token = ',') or ((Token = ')') and (Scanner.LastToken <> '(')) then
      begin
        Delete(Result, Length(Result) - Length(Scanner.LastToken),
          Length(Result));
        Result := Result + ', ';
        Inc(CountParams);
      end
      else if Token = '' then
        Break;
    end;
    if EndsWith(Result, ', ') then
    begin
      Delete(Result, Length(Result) - 1, 2);
      Result := Result + ')';
    end;
  finally
    FreeAndNil(Scanner);
  end;
end;

function TCodeCompletion.GetMethodname(const Str: string): string;
begin
  var
  Posi := Pos('(', Str);
  Result := Trim(Copy(Str, 1, Posi - 1));
end;

function TCodeCompletion.GetAttribute(var Str: string): string;
begin
  var
  Posi := Pos(' ', Str);
  Result := Copy(Str, 1, Posi - 1);
  Delete(Str, 1, Posi);
end;

procedure TCodeCompletion.SplitAttributeComment(AttributeComment: string;
  var Attribute, Comment: string);
begin
  var
  Posi := Pos(' ', AttributeComment);
  Attribute := Copy(AttributeComment, 1, Posi - 1);
  Comment := Copy(AttributeComment, Posi + 1, Length(AttributeComment));
end;

procedure TCodeCompletion.SplitMethodComment(MethodComment: string;
  var Method, Comment: string);
begin
  MethodComment := ReplaceStr(MethodComment, '&#8203;', '');
  Method := GetMethod(MethodComment);
  Comment := Trim(MethodComment);
end;

function TCodeCompletion.GetObjectFrom(Str: string): string;
var
  Posi, BracketClose: Integer;
  BracketOpen: Boolean;

  function valid(Chr: Char): Boolean;
  begin
    Result := True;
    case Chr of
      ')', ']':
        begin
          Inc(BracketClose);
          Exit;
        end;
      '(', '[':
        begin
          Dec(BracketClose);
          if BracketClose < 0 then
            Result := False;
          Exit;
        end;
    end;
    if BracketClose = 0 then
      Result := Chr.IsLetterOrDigit or (Pos(Chr, '_."') > 0);
  end;

begin
  BracketClose := 0;
  Posi := Length(Str);
  while (Posi > 0) and valid(Str[Posi]) do
    Dec(Posi);
  Str := Copy(Str, Posi + 1, Length(Str));

  // remove array indices
  Posi := 1;
  BracketOpen := False;
  while Posi <= Length(Str) do
  begin
    case Str[Posi] of
      '[':
        BracketOpen := True;
      ']':
        BracketOpen := False;
    else
      if BracketOpen then
      begin
        Delete(Str, Posi, 1);
        Dec(Posi);
      end;
    end;
    Inc(Posi);
  end;
  if EndsWith(Str, '.') then
    Str := Copy(Str, 1, Length(Str) - 1);
  Result := Str;
end;

function TCodeCompletion.DoScpJavaExecute(var State: Integer;
  const LocLine, StartWith: string; AEditForm: TFEditForm): Boolean;
var
  Objekt, Str, Token, FullClass, Typ, Pathname, ToComplete: string;
  Attri: TSynHighlighterAttributes;
  XYBuff: TBufferCoord;
  Editor: TSynEdit;
  Posi: Integer;

  procedure DoIfElse;
  begin
    FJava.scpJava.ItemList.Add('if');
    Str := FTemplates.GetControlStructure(1,
      StringOfChar(' ', Editor.CaretX - 4), '');
    Str := Copy(Trim(FTemplates.GetControlStructure(1, StringOfChar(' ',
      Editor.CaretX - 4), '')), 4, 200);
    FJava.scpJava.InsertList.Add(Str);
    FJava.scpJava.ItemList.Add('if - else');
    Str := Copy(Trim(FTemplates.GetControlStructure(9, StringOfChar(' ',
      Editor.CaretX - 4), '')), 4, 200);
    FJava.scpJava.InsertList.Add(Str);
    FDocuList.Add('');
    FDocuList.Add('');
    State := 0;
    Result := True;
  end;

begin
  if not FInformed then
    CheckMissingDocumentation;
  Result := False;
  Self.FEditForm := AEditForm;
  FJava.scpJava.ItemList.Clear;
  FJava.scpJava.InsertList.Clear;
  FDocuList.Clear;
  if not FJava.scpParams.Form.Visible then
    if Assigned(FJava.EditorForm) and Assigned(FJava.EditorForm.ParseThread)
      and (FJava.EditorForm.ParseThread.State > 0) and
      (FJava.EditorForm.ParseThread.State < 3) then
      FJava.EditorForm.ParseThread.WaitFor
    else
      ParseSourceCodes;
  if FMessages.InteractiveEditActive then
    Editor := FMessages.GetCurrentInteractive
  else if Assigned(FJava.EditorForm) then
    Editor := FJava.EditorForm.Editor
  else
    Exit;

  with Editor do
  begin
    // No text completion in strings
    XYBuff := CaretXY;
    Dec(XYBuff.Char);
    if GetHighlighterAttriAtRowCol(XYBuff, Token, Attri) and
      ((Attri = Highlighter.StringAttribute) or
      (Attri = Highlighter.CommentAttribute)) then
      Exit;
    if State = 2 then
      Str := LocLine
    else
    begin
      Str := Copy(LineText, 1, CaretX - 1);
      if StartWith <> '' then
      begin
        Str := Trim(Copy(Str, 1, Length(Str) - Length(StartWith)));
        if not EndsWith(Str, '.') then
          Str := '';
      end;

    end;
  end;

  if FConfiguration.CodeCompletionCtrlSpace and
    (EndsWith(Str, ' if(') or EndsWith(Str, ' if') or EndsWith(Str, 'if '))
  then
  begin
    DoIfElse;
    Exit;
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

  Objekt := GetObjectFrom(Str);
  Typ := GetTypeOfCode(Objekt, Editor.CaretY, State, False);

  if (State = 0) and (Typ = '') then
  begin
    Posi := LastDelimiter('.', Objekt);
    if Posi > 0 then
    begin
      ToComplete := Copy(Objekt, Posi + 1, Length(Objekt));
      Objekt := Copy(Objekt, 1, Posi - 1);
      Typ := GetTypeOfCode(Objekt, Editor.CaretY, State, False);
    end;
  end;

  if (Typ <> '') and not IsSimpleType(Typ) and (Typ <> 'void') then
  begin
    while EndsWith(Objekt, '[]') and EndsWith(Typ, '[]') do
    begin
      Delete(Objekt, Length(Objekt) - 1, 2);
      Delete(Typ, Length(Typ) - 1, 2);
    end;
    if Pos('[]', Typ) > 0 then
    begin
      FDocuList.Add(_(LNGArrayLength));
      FJava.scpJava.InsertList.Add('length');
      FJava.scpJava.ItemList.Add(MakeAttribute(5, 'length', 'integer'));
    end
    else
    begin
      if IsSelfDefinedClassOrInterface(Typ, Pathname, AEditForm) then
        CalculateSelfDefinedAttributesAndMethods(Typ, Editor.CaretY,
          StartWith, Objekt = Typ);
      if (Typ <> '') and IsJavaAPIClass(Typ, FullClass) or
         (Typ = '') and IsJavaAPIClass(Objekt, FullClass) then
        CalculateJavaAPIAttributesAndMethods(Objekt, FullClass, StartWith);
      if (StartWith <> '') and (Objekt = '') then
        CalculateClassesAndInterfaces(StartWith);
    end;
  end;
  Result := (FJava.scpJava.ItemList.Count > 0);
end;

procedure TCodeCompletion.CalculateClassesAndInterfaces
  (const StartWith: string);
const
  ImportantPackages: array [0 .. 8] of string = ('java.lang', 'java.math',
    'java.io', 'java.awt', 'java.util', 'javafx.application',
    'javafx.scene.canvas', 'javafx.scene.control', 'java.swing');

  KeyWords: array [0 .. 51] of string = ('abstract', 'assert', 'boolean',
    'break', 'byte', 'case', 'catch', 'char', 'class', 'const', 'continue',
    'default', 'do', 'double', 'else', 'extends', 'for', 'float', 'false',
    'final', 'finally', 'goto', 'if', 'int', 'implements', 'import',
    'instanceof', 'interface', 'long', 'new', 'null', 'native', 'package',
    'private', 'protected', 'public', 'return', 'switch', 'static', 'super',
    'short', 'strictfp', 'synchronized', 'this', 'try', 'true', 'throw',
    'throws', 'transient', 'void', 'volatile', 'while');

var
  AClass, APackage, Str1, Str2, Indent: string;
  Int, Iidx, Count: Integer;
  InsertList1, InsertList2, ItemList1, ItemList2: TStrings;
  Ite: IModelIterator;
  Cent: TClassifier;
  FUnit: TUnitPackage;

  function AddP(Package: string): string;
  begin
    if APackage <> '' then
      APackage := ' - ' + '\color{clgray}' + APackage;
    Result := APackage;
  end;

begin
  InsertList1 := TStringList.Create;
  InsertList2 := TStringList.Create;
  ItemList1 := TStringList.Create;
  ItemList2 := TStringList.Create;

  // Todo FJava.EditorForm exists?
  Str1 := '';
  Indent := StringOfChar(' ', FJava.EditorForm.Editor.CaretX -
    Length(StartWith) - 1);
  if StartsWithInsensitive('s', StartWith) then
  begin
    Str1 := 'String';
    Str2 := '\image{16}String';
    InsertList1.Add(Str1);
    ItemList1.Add(Str2);
    Str1 := Trim(FTemplates.GetControlStructure(17, Indent, ''));
    Str2 := '\image{16}System.out.println()';
  end
  else if StartsWithInsensitive('m', StartWith) then
  begin
    Str1 := Trim(FTemplates.GetControlStructure(19, Indent, ''));
    Str2 := '\image{16}main() { }';
  end;
  if Str1 <> '' then
  begin
    InsertList1.Add(Str1);
    ItemList1.Add(Str2);
  end;

  for var I := 0 to 51 do
    if StartsWith(KeyWords[I], StartWith) then
    begin
      if KeyWords[I] = 'if' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(1, Indent, ''));
        Str2 := '\image{16}\style{+B}if\style{-B} () { }';
      end
      else if KeyWords[I] = 'while' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(2, Indent, ''));
        Str2 := '\image{16}\style{+B}while\style{-B} () { }';
      end
      else if KeyWords[I] = 'for' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(3, Indent, ''));
        Str2 := '\image{16}\style{+B}for\style{-B} (; ; ) { }';
      end
      else if KeyWords[I] = 'do' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(4, Indent, ''));
        Str2 := '\image{16}\style{+B}do\style{-B} { } \style{+B}while\style{-B} ()';
      end
      else if KeyWords[I] = 'switch' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(5, Indent, ''));
        Str2 := '\image{16}\style{+B}switch\style{-B} () {\style{+B}case\style{-B}: \style{+B}break\style{-B};}';
      end
      else if KeyWords[I] = 'try' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(6, Indent, ''));
        Str2 := '\image{16}\style{+B}try\style{-B} { } \style{+B}catch\style{-B} (Exception e) { } \style{+B}finally\style{-B} { }';
      end
      else if KeyWords[I] = 'else' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(7, Indent, ''));
        Str2 := '\image{16}\style{+B}else\style{-B} { }';
      end
      else if KeyWords[I] = 'catch' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(13, Indent, ''));
        Str2 := '\image{16}\style{+B}catch\style{-B} { }';
      end
      else if KeyWords[I] = 'finally' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(14, Indent, ''));
        Str2 := '\image{16}\style{+B}finally\style{-B} { }';
      end
      else
      begin
        Str1 := KeyWords[I];
        Str2 := '\image{18}\style{+B}' + KeyWords[I] + '\style{-B}';
      end;
      InsertList1.Add(Str1);
      ItemList1.Add(Str2);
      Str1 := '';
      Str2 := '';
      if KeyWords[I] = 'if' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(9, Indent, ''));
        Str2 := '\image{16}\style{+B}if\style{-B} () { } \style{+B}else\style{-B} { }';
      end
      else if KeyWords[I] = 'else' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(11, Indent, ''));
        Str2 := '\image{16}\style{+B}else if\style{-B} { }';
      end
      else if KeyWords[I] = 'for' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(12, Indent, ''));
        Str2 := '\image{16}\style{+B}for\style{-B} (\style{+B}int\style{-B} i = 0; i < 10; i++) { }';
      end
      else if KeyWords[I] = 'new' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(18, Indent, ''));
        Str2 := '\image{16}\style{+B}new\style{-B} type();';
      end
      else if KeyWords[I] = 'private' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(15, Indent, ''));
        Str2 := '\image{16}\style{+B}private void\style{-B} name { }';
        InsertList1.Add(Str1);
        ItemList1.Add(Str2);
        Str1 := Trim(FTemplates.GetControlStructure(20, Indent, ''));
        Str2 := '\image{16}\style{+B}private static void\style{-B} name { }';
      end
      else if KeyWords[I] = 'public' then
      begin
        Str1 := Trim(FTemplates.GetControlStructure(16, Indent, ''));
        Str2 := '\image{16}\style{+B}public void\style{-B} name { }';
        InsertList1.Add(Str1);
        ItemList1.Add(Str2);
        Str1 := Trim(FTemplates.GetControlStructure(21, Indent, ''));
        Str2 := '\image{16}\style{+B}private static void\style{-B} name { }';
      end;
      if Str1 <> '' then
      begin
        InsertList1.Add(Str1);
        ItemList1.Add(Str2);
      end;
    end;

  if Assigned(FJava.EditorForm) then
  begin
    FUnit := FJava.EditorForm.Model.ModelRoot.FindUnitPackage('Default');
    Ite := FUnit.GetClassifiers;
    while Ite.HasNext do
    begin
      Cent := TClassifier(Ite.Next);
      if Cent.LineS = 0 then
        Continue;
      AClass := WithoutGeneric(Cent.Name);

      if not IsSimpleType(AClass) and StartsWith(AClass, StartWith) and
        (FJava.scpJava.ItemList.IndexOf('\image{1}' + AClass) = -1) then
      begin
        APackage := ExtractPackageName(AClass);
        InsertList1.Add(AClass);
        ItemList1.Add('\image{1}' + AClass + AddP(APackage));
      end;
    end;
  end;

  Int := 0;
  Count := FConfiguration.AllPackages.Count;
  while (Int < Count) and not StartsWith(FConfiguration.AllPackages[Int],
    StartWith) do
    Inc(Int);
  while (Int < Count) and StartsWith(FConfiguration.AllPackages[Int],
    StartWith) do
  begin
    APackage := FConfiguration.AllPackages[Int];
    InsertList2.Add(APackage);
    ItemList2.Add('\image{17}' + APackage);
    Inc(Int);
  end;

  Int := 0;
  Count := FConfiguration.AllClasses.Count;
  while (Int < Count) and not StartsWith(FConfiguration.AllClasses.Names[Int],
    StartWith) do
    Inc(Int);
  while (Int < Count) and StartsWith(FConfiguration.AllClasses.Names[Int],
    StartWith) do
  begin
    AClass := FConfiguration.AllClasses.Names[Int];
    APackage := ExtractPackageName
      (FConfiguration.AllClasses.ValueFromIndex[Int]);
    Iidx := 0;
    while Iidx < 9 do
    begin
      if StartsWith(APackage, ImportantPackages[Iidx]) and
        (APackage = ImportantPackages[Iidx]) then
      begin
        InsertList1.Add(AClass);
        ItemList1.Add('\image{1}' + AClass + AddP(APackage));
        Iidx := 10;
      end;
      Inc(Iidx);
    end;
    if Iidx = 9 then
    begin
      InsertList2.Add(AClass);
      ItemList2.Add('\image{1}' + AClass + AddP(APackage));
    end;
    Inc(Int);
  end;
  Int := 0;
  Count := FConfiguration.AllClasspathClasses.Count;
  while (Int < Count) and
    not StartsWith(FConfiguration.AllClasspathClasses.Names[Int],
    StartWith) do
    Inc(Int);
  while (Int < Count) and StartsWith(FConfiguration.AllClasspathClasses.Names
    [Int], StartWith) do
  begin
    AClass := FConfiguration.AllClasspathClasses.Names[Int];
    APackage := ExtractPackageName
      (FConfiguration.AllClasspathClasses.ValueFromIndex[Int]);
    InsertList2.Add(AClass);
    ItemList2.Add('\image{1}' + AClass + AddP(APackage));
    Inc(Int);
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

function TCodeCompletion.DoScpParamsExecute(State: Integer): Boolean;
var
  LocLine, Lookup: string;
  TmpX, SavePos, StartX, Int, Posi1, Posi2, Max, ParenCounter,
    TmpLocation: Integer;
  FoundMatch: Boolean;
  Str: string;
begin
  with FJava.scpParams.Editor do
  begin
    LocLine := LineText;

    // go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > Length(LocLine) then
      TmpX := Length(LocLine)
    else
      Dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;
    while (TmpX > 0) and not FoundMatch do
    begin
      if LocLine[TmpX] = ',' then
      begin
        Inc(TmpLocation);
        Dec(TmpX);
      end
      else if LocLine[TmpX] = ')' then
      begin
        // We Found a close, go till it's opening paren
        ParenCounter := 1;
        Dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if LocLine[TmpX] = ')' then
            Inc(ParenCounter)
          else if LocLine[TmpX] = '(' then
            Dec(ParenCounter);
          Dec(TmpX);
        end;
        if TmpX > 0 then
          Dec(TmpX); // eat the open paren
      end
      else if LocLine[TmpX] = '(' then
      begin
        // we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not IsIdentChar(LocLine[TmpX]) do
          Dec(TmpX);
        if TmpX > 0 then
        begin
          SavePos := TmpX;
          while (TmpX > 0) and IsIdentChar(LocLine[TmpX]) do
            Dec(TmpX);
          Inc(TmpX);
          Lookup := Copy(LocLine, TmpX, SavePos - TmpX + 1);

          if State = 1 then // from scpJava
            FoundMatch := True
          else
          begin // from scpParam
            Str := '';
            if FJava.AfterCodeCompletion then
            begin
              Int := FJava.ScpParamIndex;
              Max := FJava.scpJava.InsertList.Count;
              if (Int < Max) and (FJava.scpJava.InsertList[Int] = Lookup + '(')
              then
                Str := FJava.scpJava.ItemList[Int];
            end
            else
            begin
              State := 2;
              DoScpJavaExecute(State, Copy(LocLine, 1, StartX - 1), '',
                FEditForm);
              Int := FJava.scpJava.InsertList.IndexOf(Lookup + '(');
              if Int <> -1 then
                Str := FJava.scpJava.ItemList[Int];
            end;
            if Str <> '' then
            begin
              if CountChar(',', Str) >= TmpLocation then
              begin
                FIndexScpJava := Int;
                Posi1 := Pos('(', Str);
                Posi2 := Pos(')', Str);
                Str := Copy(Str, Posi1 + 1, Posi2 - Posi1 - 1);
                FJava.scpParams.ItemList.Clear;
                FJava.scpParams.ItemList.Add(Str);
                FoundMatch := True;
              end;
            end;
          end;
          if not FoundMatch then
          begin
            TmpX := StartX;
            Dec(TmpX);
          end;
        end;
      end
      else
        Dec(TmpX);
    end;
  end;
  Result := FoundMatch;
  FJava.scpParams.Form.CurrentIndex := 2 * TmpLocation;
end;

function TCodeCompletion.FormatParamList(const Str: string;
  CurrentIndex: Integer): string;
// FormatParamList is a function from SynCompletionProposal
// which is adapted for use in Java-Editor
begin
  Result := '';
  var
  List := TStringList.Create;
  try
    List.CommaText := Str;
    var
    Int := 0;
    while Int < List.Count - 1 do
    begin
      if Int = CurrentIndex then
        Result := Result + '\style{~B}' + List[Int] + ' ' + List[Int + 1] +
          '\style{~B}'
      else
        Result := Result + List[Int] + ' ' + List[Int + 1];
      if Int < List.Count - 2 then
        Result := Result + ', ';
      Int := Int + 2;
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TCodeCompletion.HTMLPathnameToClass(Pathname: string): string;
begin
  Pathname := ChangeFileExt(Pathname, '');
  Delete(Pathname, 1, Length(ExtractFilePath(FHTMLFileFromFolder)));
  Result := ReplaceStr(Pathname, '\', '.');
end;

function TCodeCompletion.GetDescription(Pathname: string): string;
var
  Str: string;
begin
  if FConfiguration.GlobalFileExists(Pathname) then
  begin
    var
    LineS := TStringList.Create;
    LineS.Text := LoadTextFromFile(Pathname);
    var
    Int := 0;
    Str := '';
    while Int < LineS.Count do
    begin
      if Pos('<div class="block">', LineS[Int]) = 1 then
      begin
        while (Int < LineS.Count) and
          (LineS[Int] <> '<div class="summary">') do
        begin
          Str := Str + LineS[Int];
          Inc(Int);
        end;
        Break;
      end;
      Inc(Int);
    end;
    FreeAndNil(LineS);
  end;
  Result := Str;
end;

function TCodeCompletion.SearchDocumentation(const JavaCode: string;
  Line: Integer): string;
begin
  ParseSourceCodes;
  Result := GetTypeOfCode(JavaCode, Line, 0, True);
end;

procedure TCodeCompletion.GetFullClassAndPath(Line: string;
  out FullClass, Path: string);
var
  Posi: Integer;
  AClass, APackage, LineCopy: string;
begin
  // '<li><a title="class in javax.lang.model.util" target="classFrame" href="/j2se8/api/javax/lang/model/util/AbstractAnnotationValueVisitor6.html">AbstractAnnotationValueVisitor6</a></li>'
  // '<li><a href="java/lang/String.html" title="class in java.lang" target="classFrame">String</a></li>'
  // '<li><a href="java/util/stream/IntStream.html" title="interface in java.util.stream" target="classFrame"><span class="interfaceName">IntStream</span></a></li>'
  // <li><a href="InOut.html" title="class in &lt;Unnamed&gt;" target="classFrame">InOut</a></li>
  // <li><a href="java.base/java/lang/String.html" title="class in java.lang">String</a></li>
  FullClass := '';
  Path := '';
  Posi := Pos('class in ', Line) + Pos('interface in ', Line);
  if Posi > 0 then
  begin
    LineCopy := Line;
    Posi := Pos(' in ', Line);
    Delete(Line, 1, Posi + 3);
    Posi := Pos('"', Line);
    APackage := Copy(Line, 1, Posi - 1);
    Posi := Pos('">', Line);
    Delete(Line, 1, Posi + 1);
    Posi := Pos('</a>', Line);
    if Posi = 0 then
      Posi := Pos('</A>', Line); // due to lejos documentation
    AClass := Copy(Line, 1, Posi - 1);
    if Pos('<span', AClass) > 0 then
    begin
      Posi := Pos('">', AClass);
      Delete(Line, 1, Posi + 1);
      Posi := Pos('</span>', Line);
      AClass := Copy(Line, 1, Posi - 1);
    end;
    if Pos('<code>', AClass) > 0 then
    begin
      Posi := Pos('<code>', AClass);
      Delete(AClass, Posi, 6);
      Posi := Pos('</code>', AClass);
      AClass := Copy(AClass, 1, Posi - 1);
      Delete(AClass, 1, LastDelimiter('.', AClass));
    end;
    if Pos('<i>', AClass) = 1 then
    begin
      Delete(AClass, 1, 3);
      Delete(AClass, Length(AClass) - 3, 4);
    end;
    if APackage = '&lt;Unnamed&gt;' then
      FullClass := AClass
    else
      FullClass := APackage + '.' + AClass;
    Line := LineCopy;
    Posi := Pos(' href="', Line) + Pos(' HREF="', Line);
    Delete(Line, 1, Posi + 6);
    Posi := Pos('"', Line);
    Path := Copy(Line, 1, Posi - 1);
    Path := ReplaceStr(Path, '/', '\');
  end;
end;

procedure TCodeCompletion.CheckMissingDocumentation;
begin
  var
  ADocumentation := FConfiguration.GetJavaManual + '\index.html';
  if not FConfiguration.GlobalFileExists(ADocumentation) then
  begin
    if not FInformed then
      ShowMessage(Format(_(LNGFileNotFound), [ADocumentation]) + #13#10 +
        FConfiguration.LCompletionHint1.Caption + ' ' +
        FConfiguration.LCompletionHint2.Caption);
  end;
  FInformed := True;
end;

procedure TCodeCompletion.ChangeStyle;
begin
  if FConfiguration.IsDark then
    FJava.scpJava.Images := FFileStructure.vilFileStructureDark
  else
    FJava.scpJava.Images := FFileStructure.vilFileStructureLight;
  TSynBaseCompletionProposalFormEx(FJava.scpJava.Form).ChangeStyle;
end;

function TCodeCompletion.GetAPIType(const WhereOf, Args: string;
  AttrMethCons: Integer; AsDescription: Boolean): string;
var
  Posi, CountParams, CountArgs: Integer;
  Found: Boolean;
  Typ, Method, Attribute, AClass, Description, HtmlFile,
    WithoutParameternames: string;

  function makeConstructor: string;
  begin
    Found := True;
    WithoutParameternames := GetMethodWithoutParameternames(Method,
      CountParams);
    if CountParams = CountArgs then
    begin
      if AsDescription then
      begin
        HtmlFile := ToWeb('', FHTMLFileofJavaClass) + '#' +
          WithoutParameternames;
        FTooltip.SetURL(HtmlFile);
        Result := '<code><img src="' + FConfiguration.EditorFolder +
          'img\class.png"> ' + '<a href="' + ToWeb('', FHTMLFileofJavaClass) +
          '">' + AClass + '</a><br>' + '<br><img src="' +
          FConfiguration.EditorFolder + 'img\constructor.png"> ' +
          '<bold><a href="' + HtmlFile + '">' + Method + '</a></bold></code>' +
          '<br><br>' + Description;
      end
      else
        Result := TypeWithoutDecoration(Typ);
    end;
  end;

  function MakeMethod: string;
  begin
    Found := True;
    WithoutParameternames := GetMethodWithoutParameternames(Method,
      CountParams);
    if AsDescription then
    begin
      HtmlFile := ToWeb('', FHTMLFileofJavaClass) + '#' + WithoutParameternames;
      FTooltip.SetURL(HtmlFile);
      Result := '<code><img src="' + FConfiguration.EditorFolder +
        'img\class.png"> ' + '<a href="' + ToWeb('', FHTMLFileofJavaClass) +
        '">' + AClass + '</a><br><br>' + '<img src="' +
        FConfiguration.EditorFolder + 'img\method.png"> ' + '<bold><a href="' +
        HtmlFile + '">' + Typ + ' ' + Method + '</a></bold></code>' + '<br><br>'
        + Description;
    end
    else
      // due to Mr. Ehrlich konten.get(i).  should deliver account
      Result := TypeWithoutDecoration(Typ);
  end;

  function MakeAttribute: string;
  begin
    if AsDescription then
    begin
      HtmlFile := ToWeb('', FHTMLFileofJavaClass) + '#' + Attribute;
      FTooltip.SetURL(HtmlFile);
      Result := '<code><img src="' + FConfiguration.EditorFolder +
        'img\class.png"> ' + '<a href="' + ToWeb('', FHTMLFileofJavaClass) +
        '">' + AClass + '</a><br>' + '<br><img src="' +
        FConfiguration.EditorFolder + 'img\attribute.png"> ' + '<bold><a href="'
        + HtmlFile + '">' + Typ + ' ' + Attribute + '</a></bold></code>' +
        '<br><br> ' + Description;
    end
    else
      Result := WithoutGeneric(WithoutArray(TypeWithoutDecoration(Typ)));
  end;

  function IsModifierOrVisibility(Method: string): Boolean;
  begin
    var
    Posi := Pos(',', Method);
    if Posi > 0 then
      Method := Copy(Method, 1, Posi - 1);
    Result := IsModifier(Method) or IsVisibility(Method);
  end;

begin
  Result := '';
  while True do
  begin
    with FHtmlParser do
    begin
      Text := LoadTextFromFile(FHTMLFileofJavaClass);
      AClass := HTMLPathnameToClass(FHTMLFileofJavaClass);
      CountArgs := 0;
      if Length(Args) > 0 then
      begin
        Inc(CountArgs);
        for Posi := 1 to Length(Args) do
          if Args[Posi] = ',' then
            Inc(CountArgs);
      end;
      FWithTable := False;
      Found := False;
      case AttrMethCons of
        0:
          begin
            if ConstructorsFound then
            begin
              if FWithTable then
                repeat
                  Description := getTDCell;
                  Method := GetMethod(Description);
                  if GetMethodname(Method) = WhereOf then
                  begin
                    Result := makeConstructor;
                    if WithoutParameternames = WhereOf + '(' + Args + ')' then
                      Break;
                    if CountParams = CountArgs then
                      Break;
                  end
                  else if Found then
                    Break;
                until GetTagName <> 'TR'
              else
                repeat
                  Method := Trim(getDIVCell);
                  if (Method = '') or IsModifierOrVisibility(Method) then
                    Method := Trim(getDIVCell);
                  Description := getDIVCell;
                  if GetMethodname(Method) = WhereOf then
                  begin
                    Result := makeConstructor;
                    if WithoutParameternames = WhereOf + '(' + Args + ')' then
                      Break;
                    if CountParams = CountArgs then
                      Break;
                  end
                  else if Found then
                    Break;
                  NextDIV;
                until Pos('div class="col-first', Tag.Text) <> 1;
            end
            else // default constructor
              if AsDescription then
              begin
                HtmlFile := ToWeb('', FHTMLFileofJavaClass) + '#' +
                  WithoutParameternames;
                FTooltip.SetURL(HtmlFile);
                Result := '<code><img src="' + FConfiguration.EditorFolder +
                  'img\class.png"> ' + '<a href="' +
                  ToWeb('', FHTMLFileofJavaClass) + '">' + AClass + '</a><br>' +
                  '<br><img src="' + FConfiguration.EditorFolder +
                  'img\constructor.png"> ' + '<bold><a href="' + HtmlFile + '">'
                  + WhereOf + '</a></bold></code>' + '<br><br>' + Description;
              end
              else
                Result := WithoutGeneric
                  (WithoutArray(TypeWithoutDecoration(Typ)));
          end;
        1:
          if MethodsFound then
            if FWithTable then
              repeat
                Typ := getTDCell;
                Description := getTDCell;
                Method := GetMethod(Description);

                if GetMethodname(Method) = WhereOf then
                begin
                  Result := MakeMethod;
                  if WithoutParameternames = WhereOf + '(' + Args + ')' then
                    Break;
                  if CountParams = CountArgs then
                    Break;
                end
                else if Found then
                  Break;
              until GetTagName <> 'TR'
            else
              repeat
                Typ := getDIVCell;
                Method := getDIVCell;
                Description := getDIVCell;

                if GetMethodname(Method) = WhereOf then
                begin
                  Result := MakeMethod;
                  if WithoutParameternames = WhereOf + '(' + Args + ')' then
                    Break;
                  if CountParams = CountArgs then
                    Break;
                end
                else if Found then
                  Break;
                NextDIV;
              until Pos('div class="col-first', Tag.Text) <> 1;
        2:
          if FieldsFound then
            if FWithTable then
              repeat
                Typ := getTDCell;
                Description := getTDCell;
                Attribute := GetAttribute(Description);
                if Attribute = WhereOf then
                  Exit(MakeAttribute);
              until GetTagName <> 'TR'
            else
              repeat
                Typ := getDIVCell;
                Attribute := getDIVCell;
                Description := getDIVCell;
                if Attribute = WhereOf then
                  Exit(MakeAttribute);
                NextDIV;
              until Pos('div class="col-first', Tag.Text) <> 1;
      end;
      if (Result = '') and HasAPISuperclass then
        Continue
      else
        Break;
    end;
  end;
end;

end.
