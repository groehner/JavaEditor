unit UDlgClassEditor;

interface

uses
  Controls,
  Classes,
  Forms,
  StdCtrls,
  ComCtrls,
  ImgList,
  ExtCtrls,
  Buttons,
  ActnList,
  System.ImageList,
  System.Actions,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  Vcl.VirtualImageList,
  UModel,
  UUMLForm,
  UEditorForm;

type
  TFClassEditor = class(TForm)
    LClass: TLabel;
    TreeView: TTreeView;
    PageControl: TPageControl;
    TSClass: TTabSheet;
    TSAttributes: TTabSheet;
    TSMethods: TTabSheet;
    LClassName: TLabel;
    EClass: TEdit;
    LAttributeName: TLabel;
    LAttributeType: TLabel;
    RGAttributeAccess: TRadioGroup;
    CBAttributeType: TComboBox;
    BAttributeDelete: TButton;
    BAttributeNew: TButton;
    GBAttributeOptions: TGroupBox;
    CBSetMethod: TCheckBox;
    CBGetMethod: TCheckBox;
    CBAttributeStatic: TCheckBox;
    EAttributeName: TEdit;
    LAttributeValue: TLabel;
    EAttributeValue: TEdit;
    LMethodName: TLabel;
    LMethodType: TLabel;
    CBMethodType: TComboBox;
    RGMethodAccess: TRadioGroup;
    GBMethodOptions: TGroupBox;
    CBMethodStatic: TCheckBox;
    BMethodNew: TButton;
    BMethodDelete: TButton;
    CBMethodAbstract: TCheckBox;
    RGMethodKind: TRadioGroup;
    GRFormalParameters: TGroupBox;
    LParameterName: TLabel;
    LParameterType: TLabel;
    CBParamType: TComboBox;
    BParameterNew: TButton;
    LBParams: TListBox;
    BMethodClose: TButton;
    BAttributeClose: TButton;
    BClassClose: TButton;
    CBClassAbstract: TCheckBox;
    LExtends: TLabel;
    EExtends: TEdit;
    CBMethodName: TComboBox;
    CBParamName: TComboBox;
    BInterface: TButton;
    CBParameter: TComboBox;
    LImplements: TLabel;
    EImplements: TEdit;
    CBAttributeFinal: TCheckBox;
    SBUp: TSpeedButton;
    SBDelete: TSpeedButton;
    SBDown: TSpeedButton;
    SBRight: TSpeedButton;
    SBLeft: TSpeedButton;
    BParameterDelete: TButton;
    Timer1: TTimer;
    ActionList: TActionList;
    ActionNew: TAction;
    ActionDelete: TAction;
    ActionClose: TAction;
    ActionInterface: TAction;
    ActionApply: TAction;
    BAttributeApply: TButton;
    BMethodApply: TButton;
    BClassApply: TButton;
    BClassNew: TButton;
    CBClassInner: TCheckBox;
    ILClassEditor: TVirtualImageList;
    icClassEditor: TSVGIconImageCollection;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure PageControlChange(Sender: TObject);

    procedure BClassChangeClick(Sender: TObject);
    procedure BAttributeChangeClick(Sender: TObject);
    procedure BAttributeDeleteClick(Sender: TObject);

    procedure BMethodChangeClick(Sender: TObject);
    procedure BMethodDeleteClick(Sender: TObject);

    procedure BParameterNewClick(Sender: TObject);
    procedure LBParamsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CBParamNameSelect(Sender: TObject);
    procedure CBParameterSelect(Sender: TObject);
    procedure CBAttributeTypeKeyPress(Sender: TObject; var Key: Char);
    procedure CBAttributeTypeSelect(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure SBDeleteClick(Sender: TObject);
    procedure SBRightClick(Sender: TObject);
    procedure SBLeftClick(Sender: TObject);
    procedure BParameterDeleteClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CBParamTypeKeyPress(Sender: TObject; var Key: Char);
    procedure CBParameterKeyPress(Sender: TObject; var Key: Char);
    procedure CBParamTypeSelect(Sender: TObject);
    procedure CBParamNameKeyPress(Sender: TObject; var Key: Char);
    procedure CBMethodTypeKeyPress(Sender: TObject; var Key: Char);
    procedure CBMethodTypeSelect(Sender: TObject);
    procedure CBMethodnameSelect(Sender: TObject);
    procedure EAttributeValueKeyPress(Sender: TObject; var Key: Char);
    procedure ActionApplyExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionInterfaceExecute(Sender: TObject);
    procedure CBAttributeTypeDropDown(Sender: TObject);
    procedure CBMethodTypeDropDown(Sender: TObject);
    procedure CBParamTypeDropDown(Sender: TObject);
    procedure ComboBoxCloseUp(Sender: TObject);

    procedure EAttributeNameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBMethodnameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBParamNameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBAttributeTypeKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBMethodParamTypeKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EClassKeyPress(Sender: TObject; var Key: Char);
    procedure CBMethodnameKeyPress(Sender: TObject; var Key: Char);
    procedure EAttributeNameKeyPress(Sender: TObject; var Key: Char);
    procedure CBClassAbstractMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CBClassAbstractClick(Sender: TObject);
    procedure CBComboBoxEnter(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  private
    FMyEditor: TFEditForm;
    FMyUMLForm: TFUMLForm;
    FIsClass: Boolean;
    FRGMethodAccessValues: string;
    FRGMethodKindValues: string;
    FComboBoxInvalid: Boolean;
    FMakeNewClass: Boolean;
    FTreeViewUpdating: Boolean;
    FAttributeNode: TTreeNode;
    FMethodNode: TTreeNode;
    procedure AttributeToJava(Attribute: TAttribute; ClassNumber: Integer);
    function MethodToJava(Method: TOperation; Source: string): string;
    procedure RGMethodKindChange;
    procedure ShowMandatoryFields;
    procedure SetClassOrInterface(IsClass: Boolean);
    procedure TVClassOrInterface(Classifier: TClassifier);
    procedure TVAttribute(Attribute: TAttribute);
    procedure TVMethod(Method: TOperation);
    function HasMethod(const GetSet: string; Attribute: TAttribute; var Method: TOperation): Boolean;
    procedure ChangeAttribute(var Attribute: TAttribute);
    procedure ChangeGetSet(Attribute: TAttribute; ClassNumber: Integer);
    procedure NewClass;
    function MakeAttribute: TAttribute;
    function MakeType(ComboBox: TComboBox): TClassifier; overload;
    function MakeType(const Classname: string): TClassifier; overload;
    procedure GetParameter(ListBox: TListBox; Method: TOperation);
    procedure ChangeMethod(var Method: TOperation);
    function MakeMethod: TOperation;
    function CreateMethod(const GetSet: string; const Attribute: TAttribute): string;
    function MakeConstructor(Method: TOperation; Source: string): string;
    function Typ2Value(const Typ: string): string;
    procedure MoveNode(TargetNode, SourceNode : TTreeNode);
    procedure EnableEvents(Enable: Boolean);
    procedure SetEditText(E: TEdit; const Text: string);
    procedure SetEditValue(E: TEdit; const Value: string);
    function MakeIdentifier(var Str1: string; Typ: Boolean): Integer; overload;
    function MakeIdentifier(Edit: TEdit; Typ: Boolean):Boolean; overload;
    function MakeIdentifier(ComboBox: TComboBox; Typ: Boolean):Boolean; overload;
    function PartOfClass(Node: TTreeNode): Boolean;
    function GetClassNumber(Node: TTreeNode): Integer;
    function GetNextClassNumber: string;
    function GetLastLineOfLastClass: Integer;
    function GetClassNode: TTreeNode;
    function GetClassInterfaceNode: TTreeNode;
    function GetClassName(ClassNode: TTreeNode): string;
    function GetAttributeNode: TTreeNode;
    function GetMethodNode: TTreeNode;
    function GetClassifier(ClassNode: TTreeNode): TClassifier;
    function GetAttribute(AttributeLeafNode: TTreeNode): TAttribute;
    function GetMethod(MethodLeafNode: TTreeNode): TOperation;
    function GetLevel(Node: TTreeNode): Integer;
    function GetIndent(Level: Integer): string;
    function IsClassOrInterface(Node: TTreeNode): Boolean;
    function IsAttributesNodeLeaf(Node: TTreeNode): Boolean;
    function IsAttributesNode(Node: TTreeNode): Boolean;
    function IsMethodsNodeLeaf(Node: TTreeNode): Boolean;
    function IsMethodsNode(Node: TTreeNode): Boolean;
    procedure AllAttributesAsParameters(var Node: TTreeNode);
    function AttributeAlreadyExists(const AttributeName: string): Boolean;
    function MethodAlreadyExists(const MethodName: string): Boolean;
    procedure Init(Default: Boolean);
    function NameTypeValueChanged: Boolean;
  public
    procedure SaveWindow;
    function CreateTreeView(Editor: TFEditForm; UMLForm: TFUMLForm): Boolean;
    procedure UpdateTreeView(HasChanged: Boolean = True; NodeIndex: Integer = -1);
    procedure ChangeStyle;
  end;

implementation

{$R *.dfm}

uses
  Math,
  UITypes,
  StrUtils,
  Character,
  Windows,
  Messages,
  SysUtils,
  Graphics,
  Dialogs,
  JvGnugettext,
  UStringRessources,
  UGUIForm,
  UModelEntity,
  UJava,
  UConfiguration,
  UUtils,
  UBaseForm,
  UFileStructure;

type
  TParamTyp = class (TObject)
  private
    FTyp: string;
  public
    constructor Create(const Typ: string);
    property Typ: string read FTyp;
  end;

constructor TParamTyp.Create(const Typ: string);
begin
  Self.FTyp:= Typ;
end;

procedure TFClassEditor.TVClassOrInterface(Classifier: TClassifier);
  var Node, Anchor: TTreeNode; Posi: Integer; CName: string;

  function GetCorINode(Str: string): TTreeNode;
    var Node: TTreeNode; Str1: string; Posi: Integer;
  begin
    Result:= nil;
    Node:= TreeView.Items.GetFirstNode;
    Posi:= Pos('$', Str);
    Str1:= Copy(Str, 1, Posi-1); Delete(Str, 1, Posi);
    while Str1 <> '' do begin
      while Assigned(Node) and (WithoutGeneric(Node.Text) <> Str1) do
        Node:= Node.getNextSibling;
      if not Assigned(Node) then
        Exit;
      Node:= Node.getFirstChild;
      Posi:= Pos('$', Str);
      Str1:= Copy(Str, 1, Posi-1); Delete(Str, 1, Posi);
    end;
    Result:= Node;
  end;

begin
  CName:= Classifier.Name;
  Posi:= LastDelimiter('$', CName);
  if Posi = 0 then
    Anchor:= nil
  else begin
    Anchor:= GetCorINode(Copy(CName, 1, Posi));
    Delete(CName, 1, Posi);
  end;
  Node:= TreeView.Items.Add(Anchor, CName);
  if (Classifier is TClass) then begin
    Node.ImageIndex:= 1;
    Node.SelectedIndex:= 1;
  end else begin
    Node.ImageIndex:= 11;
    Node.SelectedIndex:= 11;
  end;

  FAttributeNode:= TreeView.Items.AddChild(Node, _('Attributes'));
  FAttributeNode.ImageIndex:= 12;
  FAttributeNode.SelectedIndex:= 12;

  FMethodNode:= TreeView.Items.AddChild(Node, _('Methods'));
  FMethodNode.ImageIndex:= 13;
  FMethodNode.SelectedIndex:= 13;

  ComboBoxInsert2(CBAttributeType, CName);
  ComboBoxInsert2(CBMethodType, CName);
  ComboBoxInsert2(CBParamType, CName);
end;

procedure TFClassEditor.TVAttribute(Attribute: TAttribute);
begin
  var Node:= TreeView.Items.AddChild(FAttributeNode, Attribute.ToNameTypeValue);
  Node.ImageIndex:= Integer(Attribute.Visibility) + 2;
  Node.SelectedIndex:= Integer(Attribute.Visibility) + 2;
end;

procedure TFClassEditor.TVMethod(Method: TOperation);
  var ImageNr: Integer;
begin
  var Node:= TreeView.Items.AddChild(FMethodNode, Method.ToNameParameterTyp);
  if Method.OperationType = otConstructor
    then ImageNr:= 6
    else ImageNr:= Integer(Method.Visibility) + 7;
  Node.ImageIndex:= ImageNr;
  Node.SelectedIndex:= ImageNr;
end;

procedure TFClassEditor.NewClass;
  var CNumberStr, Indent1, Indent2, Str: string; Line: Integer;
begin
  FMakeNewClass:= False;
  Indent1:= GetIndent(0);
  Indent2:= GetIndent(1);
  if CBClassInner.Checked then begin
    Indent1:= GetIndent(1);
    Indent2:= GetIndent(2);
  end;
  CNumberStr:= GetNextClassNumber;
  if BInterface.Caption = _(LNGClass)
    then Str:= FConfiguration.HeadText(Indent1) + CrLf + Indent1 + 'interface ' + EClass.Text
    else Str:= FConfiguration.HeadText(Indent1) + CrLf + Indent1 + 'class ' + EClass.Text;
  if EExtends.Text <> '' then
    Str:= Str + ' extends ' + EExtends.Text;
  if EImplements.Text <> '' then
    Str:= Str + ' implements ' + EImplements.Text;
  Str:= Str + ' {' + CrLf +
      Indent2 + CrLf +
      Indent2 + _(LNGStartGUIVariables) + CNumberStr + CrLf +
      Indent2 + _(LNGEndGUIVariables) + CNumberStr + CrLf +
      Indent2 + CrLf +
      Indent2 + _(LNGStartEventMethods) + CNumberStr + CrLf +
      Indent2 + _(LNGEndEventMethods) + CNumberStr + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str:= Str + ' // end of ' + EClass.Text;
  Str:= Str + CrLf;
  Line:= GetLastLineOfLastClass;
  if CBClassInner.Checked then
    Dec(Line);
  FMyEditor.InsertLinesAt(Line, Str);
  UpdateTreeView;
  TreeView.Selected:= TreeView.Items[TreeView.Items.Count-3];
  TreeViewChange(Self, TreeView.Selected);
end;

procedure TFClassEditor.BClassChangeClick(Sender: TObject);
  var Str, AClassname, OldFile, NewFile, SearchText, ReplacText, Tail,
      OldClassname, NewClassname: string;
      AClassifier: TClassifier;
      Abstrakt, SkipUpdate: Boolean;
      Posi, NodeIndex: Integer;
      Node: TTreeNode;
      Form2: TFEditForm;
      GUIForm: TFGUIForm;
      StringList: TStringList;
begin
  if Trim(EClass.Text) = '' then Exit;
  if FMakeNewClass then
    NewClass
  else begin
    Node:= TreeView.Selected;
    if not Assigned(Node) then
      Exit;
    NodeIndex:= Node.AbsoluteIndex;
    FIsClass:= PartOfClass(Node);

    AClassifier:= GetClassifier(Node);
    Abstrakt:= AClassifier.IsAbstract;
    OldClassname:= Node.Text;
    NewClassname:= Trim(EClass.Text);

    if FIsClass then begin
      SearchText:= 'class ' + OldClassname;
      ReplacText:= 'class ' + NewClassname;
    end else begin
      SearchText:= 'interface ' + OldClassname;
      ReplacText:= 'interface ' + NewClassname;
    end;

    with FMyEditor do begin
      Go_To(SearchText);
      Str:= Editor.LineText;
      Str:= ReplaceStr(Str, SearchText, ReplacText);

      if Abstrakt and not CBClassAbstract.Checked then begin
        Posi:= Pos('abstract', Str);
        if Posi > 0 then Delete(Str, Posi, 9);
      end else if not Abstrakt and CBClassAbstract.Checked then begin
        Posi:= Pos(SearchText, Str);
        if Posi > 0 then insert('abstract ', Str, Posi);
      end;

      Posi:= Pos('{', Str);
      if Posi > 0 then begin
        Tail:= Copy(Str, Posi, Length(Str));
        Str:= Copy(Str, 1, Posi-1);
      end else
        Tail:= '';

      Posi:= Pos('extends', Str);
      if Posi > 0 then
        Str:= Copy(Str, 1, Posi-1);
      Posi:= Pos('implements', Str);
      if Posi > 0 then
        Str:= Copy(Str, 1, Posi-1);
      Str:= Trim(Str);

      SkipUpdate:= False;

      if EExtends.Text <> '' then
        if FIsClass
          then Str:= Str + ' extends ' + GetFirstWord(EExtends.Text)
        else begin
          StringList:= Split(',', EExtends.Text);
          for var I:= StringList.Count - 1 downto 0 do
            if Trim(StringList[I]) =  '' then begin
              StringList.Delete(I);
              SkipUpdate:= True;
            end;
          if StringList.Count > 0 then begin
            Str:= Str + ' extends ' + Trim(StringList[0]);
            for var I:= 1 to StringList.Count - 1 do
              Str:= Str + ', ' + Trim(StringList[I]);
          end;
          FreeAndNil(StringList);
        end;

      if EImplements.Text <> '' then begin
        StringList:= Split(',', EImplements.Text);
        for var I:= StringList.Count - 1 downto 0 do
          if Trim(StringList[I]) =  '' then begin
            StringList.Delete(I);
            SkipUpdate:= True;
          end;
        if StringList.Count > 0 then begin
          Str:= Str + ' implements ' + Trim(StringList[0]);
          for var I:= 1 to StringList.Count - 1 do
            Str:= Str + ', ' + Trim(StringList[I]);
        end;
        FreeAndNil(StringList);
      end;
      Str:= Str + ' ' + Tail;

      Editor.LineText:= Str;
      Modified:= True;
      AClassname:= Trim(EClass.Text);
      if (OldClassname <> AClassname) then begin
        OldFile:= Pathname;
        NewFile:= ExtractFilePath(Pathname) + WithoutGeneric(AClassname) + '.java';
        Form2:= TFEditForm(FJava.GetTDIWindowType(NewFile, '%E%'));
        if Assigned(Form2) and (Form2 <> FMyEditor) then
          InformationMsg(Format(_(LNGFileAlreadyOpen), [NewFile]))
        else if not FileExists(NewFile) or (FMyEditor.Pathname = NewFile) or FileExists(NewFile) and
            (MessageDlg(Format(_(LNGFileAlreadyExists), [NewFile]),
                         mtConfirmation, mbYesNoCancel, 0) = mrYes)
        then begin
          if Assigned(FMyUMLForm) then
            FMyUMLForm.Exchange(OldFile, NewFile);
          OldFile:= ChangeFileExt(OldFile, '.jfm');
          NewFile:= ChangeFileExt(NewFile, '.jfm');
          GUIForm:= FJava.GetGuiForm(OldFile);
          if Assigned(GUIForm) then
            GUIForm.Change(NewFile)
          else if FileExists(OldFile) then
            RenameFile(OldFile, NewFile);
          OldFile:= ChangeFileExt(OldFile, '.java');
          NewFile:= ChangeFileExt(NewFile, '.java');
          SaveAs(NewFile);
          if OldFile <> NewFile then  // due to generics
            DeleteFile(OldFile);
          ComboBoxDelete2(CBAttributeType, OldClassname);
          ComboBoxDelete2(CBMethodType, OldClassname);
          ComboBoxDelete2(CBParamType, OldClassname);
          FMyEditor.ReplaceWord(OldClassname, WithoutGeneric(NewClassname), True);
        end;
      end;
      if not SkipUpdate then begin
        UpdateTreeView;
        if NodeIndex < TreeView.Items.Count then
          TreeView.Selected:= TreeView.Items[NodeIndex];
      end;
    end;
  end;
  BClassApply.Enabled:= False;
end;

procedure TFClassEditor.ActionInterfaceExecute(Sender: TObject);
  var AClassname: string; Node: TTreeNode; NodeIndex: Integer;
begin
  Node:= TreeView.Selected;
  if Assigned(Node) then begin
    NodeIndex:= Node.AbsoluteIndex;
    with FMyEditor do begin
      AClassname:= Node.Text;
      if BInterface.Caption = _(LNGClass)
        then ReplaceText('interface ' + AClassname, 'class ' + AClassname, True)
        else ReplaceText('class ' + AClassname, 'interface ' + AClassname, True);
      Modified:= True;
      UpdateTreeView;
      if NodeIndex < TreeView.Items.Count then
        TreeView.Selected:= TreeView.Items[NodeIndex];
    end;
  end;
end;

function TFClassEditor.HasMethod(const GetSet: string; Attribute: TAttribute; var Method: TOperation): Boolean;
  var AName, MethodName: string;
      Cent: TClassifier;
begin
  AName:= Attribute.Name;
  if FConfiguration.AttributesAParametersP and (Copy(AName, 1, 1) = 'a') then
    Delete(AName, 1, 1);
  AName:= UpperCase(Copy(AName, 1, 1)) + Copy(AName, 2, Length(AName));
  if GetSet = _(LNGGet)
    then MethodName:= _(LNGGet) + AName
    else MethodName:= _(LNGSet) + AName;
  Cent:= GetClassifier(GetClassNode);
  if Assigned(Cent) then begin
    var It1:= Cent.GetOperations;
    while It1.HasNext do begin
      Method:= It1.Next as TOperation;
      if Method.Name = MethodName then
        Exit(True);
    end;
  end;
  Method:= nil;
  Result:= False;
end;

procedure TFClassEditor.EnableEvents(Enable: Boolean);
begin
  if Enable and not FMyEditor.Editor.ReadOnly then begin
    CBClassAbstract.OnClick:= BClassChangeClick;

    RGAttributeAccess.OnClick:= BAttributeChangeClick;
    CBAttributeStatic.OnClick:= BAttributeChangeClick;
    CBAttributeFinal.OnClick:= BAttributeChangeClick;
    CBGetMethod.OnClick:= BAttributeChangeClick;
    CBSetMethod.OnClick:= BAttributeChangeClick;

    RGMethodKind.OnClick:= BMethodChangeClick;
    RGMethodAccess.OnClick:= BMethodChangeClick;
    CBMethodStatic.OnClick:= BMethodChangeClick;
    CBMethodAbstract.OnClick:= BMethodChangeClick;
  end else begin
    CBClassAbstract.OnClick:= nil;

    EClass.OnChange:= nil;
    EExtends.OnChange:=  nil;
    EImplements.OnChange:= nil;

    RGAttributeAccess.OnClick:= nil;
    CBAttributeStatic.OnClick:= nil;
    CBAttributeFinal.OnClick:= nil;
    CBGetMethod.OnClick:= nil;
    CBSetMethod.OnClick:= nil;

    RGMethodKind.OnClick:= nil;
    RGMethodAccess.OnClick:= nil;
    CBMethodStatic.OnClick:= nil;
    CBMethodAbstract.OnClick:= nil;
  end;
end;

function TFClassEditor.PartOfClass(Node: TTreeNode): Boolean;
begin
  Result:= False;
  if Assigned(Node) then begin
    while (Node.ImageIndex <> 1) and (Node.ImageIndex <> 11) do
      Node:= Node.Parent;
    Result:= (Node.ImageIndex = 1);
  end;
end;

function TFClassEditor.GetClassNumber(Node: TTreeNode): Integer;
begin
  Result:= -1;
  var ANode:= TreeView.Items.GetFirstNode;
  while ANode <> Node do begin
    if IsClassOrInterface(ANode) then
      Inc(Result);
    ANode:= ANode.GetNext;
  end;
end;

function TFClassEditor.GetNextClassNumber: string;
begin
  var Res:= 0;
  var ANode:= TreeView.Items.GetFirstNode;
  while Assigned(ANode) do begin
    if IsClassOrInterface(ANode) then Inc(Res);
    ANode:= ANode.GetNext;
  end;
  Result:= IntToStr(Res);
end;

function TFClassEditor.GetLastLineOfLastClass: Integer;
begin
  Result:= -1;
  FMyEditor.ParseSourceCode(False);
  var ClassIterator:= FMyEditor.Model.ModelRoot.GetAllClassifiers;
  while ClassIterator.HasNext do begin
    var Cent:= TClassifier(ClassIterator.Next);
    if Cent.Pathname = FMyEditor.Pathname then
      Result:= Max(Result, Cent.LineE);
  end;
end;

function TFClassEditor.GetClassInterfaceNode: TTreeNode;
begin
  // we may have multiple classes in the treeview
  Result:= TreeView.Selected;
  if Assigned(Result) then
    while Assigned(Result) and (Result.ImageIndex <> 1) and (Result.ImageIndex <> 11) do
      Result:= Result.Parent
  else
    for var I:= 0 to TreeView.Items.Count - 1 do
      if TreeView.Items[I].ImageIndex in [1, 11] then begin
        Result:= TreeView.Items[I];
        Break;
      end;
end;

function TFClassEditor.GetClassNode: TTreeNode;
begin
  // we may have multiple classes in the treeview
  Result:= TreeView.Selected;
  if Assigned(Result) then
    while Assigned(Result) and (Result.ImageIndex <> 1) do
      Result:= Result.Parent
  else
    for var I:= 0 to TreeView.Items.Count - 1 do
      if TreeView.Items[I].ImageIndex = 1 then begin
        Result:= TreeView.Items[I];
        Break;
      end;
end;

function TFClassEditor.GetClassName(ClassNode: TTreeNode): string;
begin
  // we may have multilevel inner classes in the treeview
  if Assigned(ClassNode) then begin
    Result:= ClassNode.Text;
    while ClassNode.Parent <> nil do begin
      ClassNode:= ClassNode.Parent;
      Result:= ClassNode.Text + '$' + Result;
    end;
  end else
    Result:= '';
end;

function TFClassEditor.GetAttributeNode: TTreeNode;
begin
  Result:= GetClassInterfaceNode;
  if Assigned(Result) then
    Result:= Result.getFirstChild;
end;

function TFClassEditor.GetMethodNode: TTreeNode;
begin
  Result:= GetAttributeNode;
  if Assigned(Result) then
    Result:= Result.getNextSibling;
end;

function TFClassEditor.GetClassifier(ClassNode: TTreeNode): TClassifier;
begin
  var FullClassname:= GetClassName(ClassNode);
  var ClassIterator:= FMyEditor.Model.ModelRoot.GetAllClassifiers;
  while ClassIterator.HasNext do begin
    Result:= TClassifier(ClassIterator.Next);
    if Result.Name = FullClassname then
      Exit(Result);
  end;
  Result:= nil;
end;

function TFClassEditor.GetAttribute(AttributeLeafNode: TTreeNode): TAttribute;
begin
  var Cent:= GetClassifier(AttributeLeafNode.Parent.Parent);
  if Assigned(Cent) then begin
    var Ite:= Cent.GetAttributes;
    while Ite.HasNext do begin
      Result:= TAttribute(Ite.Next);
      if Result.ToNameTypeValue = AttributeLeafNode.Text then
        Exit(Result);
    end;
  end;
  Result:= nil;
end;

function TFClassEditor.GetMethod(MethodLeafNode: TTreeNode): TOperation;
begin
  var Cent:= GetClassifier(MethodLeafNode.Parent.Parent);
  if Assigned(Cent) then begin
    var Ite:= Cent.GetOperations;
    while Ite.HasNext do begin
      Result:= TOperation(Ite.Next);
      if Result.ToNameParameterTyp = MethodLeafNode.Text then
        Exit(Result);
    end;
  end;
  Result:= nil;
end;

function TFClassEditor.GetLevel(Node: TTreeNode): Integer;
  var ANode: TTreeNode;
begin
  if not Assigned(Node) then
    Exit(0);
  Result:= 0;
  if IsClassOrInterface(Node) then
    ANode:= Node
  else if IsAttributesNode(Node) or IsMethodsNode(Node) then
    ANode:= Node.Parent
  else
    ANode:= Node.Parent.Parent;
  while ANode.Parent <> nil do begin
    Result:= Result + 1;
    ANode:= ANode.Parent;
  end;
end;

function TFClassEditor.GetIndent(Level: Integer): string;
begin
  Result:= StringTimesN(FConfiguration.Indent1, Level);
end;

procedure TFClassEditor.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= False;
  EAttributeName.SelStart:= 1;
  EAttributeName.SelLength:= 0;
end;

procedure TFClassEditor.TreeViewChange(Sender: TObject; Node: TTreeNode);
  var AClassifier: TClassifier; AAttribute: TAttribute; AMethod: TOperation;
      Line, ClassNumber, Posi: Integer; Ite: IModelIterator;
      Str, Str1: string;
begin
  if not Assigned(Node) then
    Exit;
  Line:= -1;
  EnableEvents(False);
  try
    FIsClass:= PartOfClass(Node);
    if IsClassOrInterface(Node) then begin
      PageControl.ActivePageIndex:= 0;
      AClassifier:= GetClassifier(Node);
      if not Assigned(AClassifier) then
        Exit;
      EClass.Text:= GetShortType(AClassifier.ShortName);
      CBClassAbstract.Checked:= AClassifier.IsAbstract;
      CBClassInner.Checked:= AClassifier.Inner;
      if FIsClass then begin
        if Assigned((AClassifier as TClass).Ancestor)
          then Str:= GetShortType((AClassifier as TClass).Ancestor.ShortName)
          else Str:= '';
        SetEditText(EExtends, Str);
        Ite:= (AClassifier as TClass).GetImplements;
        Str:= '';
        while Ite.HasNext do begin
          Str1:= GetShortType((Ite.Next as TInterface).ShortName);
          Str:= Str + ', ';
          if Pos('{', Str1) = 0 then
            Str:= Str + Str1;
        end;
        Delete(Str, 1, 2);
        EImplements.Text:= Str;
      end else begin
        Ite:= (AClassifier as TInterface).GetExtends;
        Str:= '';
        while Ite.HasNext do begin
          Str1:= GetShortType((Ite.Next as TInterface).ShortName);
          Str:= Str + ', ';
          if Pos('{', Str1) = 0 then
            Str:= Str + Str1;
        end;
        Delete(Str, 1, 2);
        Posi:= EExtends.SelStart;
        var LenE:= Length(EExtends.Text);
        var Len:= Length(Str);
        EExtends.Text:= Str;
        if Len > LenE
          then EExtends.SelStart:= Posi + 1
        else if Len < LenE
          then EExtends.SelStart:= Posi - 1;
      end;
      Line:= AClassifier.LineS;
    end else if IsAttributesNode(Node) or IsAttributesNodeLeaf(Node) then begin
      PageControl.ActivePageIndex:= 1;
      BAttributeApply.Enabled:= False;
      if IsAttributesNodeLeaf(Node) then begin
        AAttribute:= GetAttribute(Node);
        if not Assigned(AAttribute) then
          Exit;
        SetEditText(EAttributeName, AAttribute.Name);
        CBAttributeType.Text:= AAttribute.TypeClassifier.GetShortType;
        SetEditValue(EAttributeValue, AAttribute.Value);
        if FIsClass
          then RGAttributeAccess.ItemIndex:= Integer(AAttribute.Visibility)
        else if AAttribute.Visibility = viPackage
          then RGAttributeAccess.ItemIndex:= 0
          else RGAttributeAccess.ItemIndex:= 1;
        CBGetMethod.Checked:= HasMethod(_(LNGGet), AAttribute, AMethod);
        CBSetMethod.Checked:= HasMethod(_(LNGSet), AAttribute, AMethod);
        CBAttributeStatic.Checked:= AAttribute.Static;
        CBAttributeFinal.Checked:= AAttribute.IsFinal;
        Line:= AAttribute.LineS;
      end else begin
        CBAttributeType.Text:= '';
        CBAttributeType.ItemIndex:= -1;
        EAttributeValue.Text:= '';
        ActiveControl:= EAttributeName;
        if FConfiguration.AttributesAParametersP then begin
          EAttributeName.Text:= 'a';
          Timer1.Enabled:= True;
        end else
          EAttributeName.Text:= '';
      end;
      ComboBoxInsert(CBAttributeType);
    end else begin
      PageControl.ActivePageIndex:= 2;
      BMethodApply.Enabled:= False;
      AllAttributesAsParameters(Node);
      if FConfiguration.AttributesAParametersP
        then CBParamName.Text:= 'Posi'
        else CBParamName.Text:= '';
      CBParamType.Text:= '';
      CBParamType.ItemIndex:= -1;
      if IsMethodsNodeLeaf(Node) then begin
        EClass.Text:= Node.Parent.Parent.Text; // wg. constructor
        AMethod:= GetMethod(Node);
        if not Assigned(AMethod) then
          Exit;
        CBMethodName.Text:= AMethod.Name;
        if Assigned(AMethod.ReturnValue)
          then CBMethodType.Text:= AMethod.ReturnValue.GetShortType
          else CBMethodType.Text:= '';
        if FIsClass then begin
          RGMethodKind.ItemIndex:= Integer(AMethod.OperationType);
          RGMethodAccess.ItemIndex:= Integer(AMethod.Visibility);
        end else begin
          RGMethodKind.ItemIndex:= Integer(AMethod.OperationType) - 1;
          if AMethod.Visibility = viPackage
            then RGMethodAccess.ItemIndex:= 0
            else RGMethodAccess.ItemIndex:= 1;
        end;
        CBMethodStatic.Checked:= AMethod.Static;
        CBMethodAbstract.Checked:= AMethod.IsAbstract;
        GetParameter(LBParams, AMethod);
        Line:= AMethod.LineS;
      end else begin
        if Assigned(Node)
          then EClass.Text:= Node.Parent.Text // wg. constructor
          else EClass.Text:= '';
        if FIsClass
          then RGMethodKind.ItemIndex:= 1     // wg. constructor
          else RGMethodKind.ItemIndex:= 0;
        CBMethodName.Text:= '';
        CBMethodType.Text:= '';
        CBMethodType.ItemIndex:= -1;
        CBMethodStatic.Checked:= False;
        CBMethodAbstract.Checked:= False;
        LBParams.Clear;
        if FIsClass and (RGMethodKind.ItemIndex > 0) or not FIsClass then begin
          CBMethodName.Enabled:= True;
          ActiveControl:= CBMethodName;
        end;
      end;
      ComboBoxInsert(CBMethodType);
      ComboBoxInsert(CBParamName);
      ComboBoxInsert(CBParamType);
      CBMethodName.Enabled:= (RGMethodKind.ItemIndex > 0) or not FIsClass;
      CBMethodType.Enabled:= (RGMethodKind.ItemIndex = 2) or
                             (not FIsClass and (RGMethodKind.ItemIndex = 1));
    end;
    if IsAttributesNode(Node) or IsMethodsNode(Node) then
      if FMyEditor.Editor.ReadOnly
        then Line:= -1
        else begin
          ClassNumber:= GetClassNumber(Node);
          if Node.ImageIndex = 12
            then FMyEditor.Go_To(FMyEditor.GetLNG(1, ClassNumber))
            else FMyEditor.Go_To(FMyEditor.GetLNG(5, ClassNumber));
          Line:= FMyEditor.Editor.CaretY;
        end;
    if Line <> -1 then
      FMyEditor.GotoLine(Line);
    SetClassOrInterface(FIsClass);
    ShowMandatoryFields;
  finally
    EnableEvents(True);
  end;
end;

procedure TFClassEditor.SetEditText(E: TEdit; const Text: string);
begin
  if E.Text <> Text then begin
    var Posi:= E.SelStart;
    E.Text:= Text;
    E.SelStart:= Posi;
  end;
end;

procedure TFClassEditor.SetEditValue(E: TEdit; const Value: string);
begin
  if E.Text <> Value then begin
    var Posi:= E.SelStart;
    if (Pos('"', Value) > 0) and (Pos('"', E.Text) = 0) then Inc(Posi);
    E.Text:= Value;
    E.SelStart:= Posi;
  end;
end;

procedure TFClassEditor.GetParameter(ListBox: TListBox; Method: TOperation);
  var It2: IModelIterator; Parameter: TParameter;
begin
  ListBox.Clear;
  It2:= Method.GetParameters;
  while It2.HasNext do begin
    Parameter:= It2.Next as TParameter;
    if Assigned(Parameter.TypeClassifier) then
      ListBox.Items.Add(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType);
    if Parameter.Name = CBParamName.Text then
      CBParamType.Text:= Parameter.TypeClassifier.GetShortType;
  end;
  var Str:= CBParamName.Text + ': ' + CBParamType.Text;
  var Index:= ListBox.Items.IndexOf(Str);
  if Index <> -1 then
    ListBox.Items.Delete(Index);
end;

procedure TFClassEditor.PageControlChange(Sender: TObject);
  var Node: TTreeNode;
begin
  var Tab:= PageControl.TabIndex;
  if BClassApply.Enabled then BClassChangeClick(Sender);
  if BAttributeApply.Enabled then BAttributeChangeClick(Sender);
  if BMethodApply.Enabled then BMethodChangeClick(Sender);
  PageControl.TabIndex:= Tab;
  case PageControl.TabIndex of
    0: Node:= GetClassInterfaceNode;
    1: Node:= GetAttributeNode;
    else begin
      Node:= GetMethodNode;
      AllAttributesAsParameters(Node);
    end;
  end;
  TreeView.Selected:= Node;
end;

procedure TFClassEditor.ActionNewExecute(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    1: begin
      EAttributeName.Text:= '';
      CBAttributeType.Text:= '';
      EAttributeValue.Text:= '';
      TreeView.Selected:= GetAttributeNode;
      CBAttributeStatic.Checked:= False;
      CBAttributeFinal.Checked:= False;
      if FConfiguration.DefaultModifiers then begin
        RGAttributeAccess.ItemIndex:= 0;
        CBGetMethod.Checked:= True;
      end;
    end;
    2: begin
      CBMethodName.Text:= '';
      CBMethodType.Text:= '';
      CBParamName.Text:= '';
      CBParamType.Text:= '';
      LBParams.Items.Clear;
      CBParameter.Text:= _(LNGSelectAttributes);
      TreeView.Selected:= GetMethodNode;
      CBMethodStatic.Checked:= False;
      CBMethodAbstract.Checked:= False;
      if FConfiguration.DefaultModifiers and FIsClass then begin
        RGMethodKind.ItemIndex:= 1;
        RGMethodAccess.ItemIndex:= 3;
      end;
    end;
  else begin
      EClass.Text:= '';
      EExtends.Text:= '';
      EImplements.Text:= '';
      CBClassAbstract.Checked:= False;
      CBClassInner.Checked:= False;
      TreeView.Selected:= GetClassInterfaceNode;
      FMakeNewClass:= True;
    end;
  end;
end;

procedure TFClassEditor.AttributeToJava(Attribute: TAttribute; ClassNumber: Integer);
  var Method: TOperation;
begin
  FMyEditor.Editor.BeginUpdate;
  FMyEditor.InsertAttribute(ClassNumber, Attribute.ToJava);
  if FIsClass and CBGetMethod.Checked and not HasMethod(_(LNGGet), Attribute, Method) then
    FMyEditor.InsertProcedure(ClassNumber, CreateMethod(_(LNGGet), Attribute));
  if FIsClass and CBSetMethod.Checked and not HasMethod(_(LNGSet), Attribute, Method) then
    FMyEditor.InsertProcedure(ClassNumber, CreateMethod(_(LNGSet), Attribute));
  FMyEditor.Editor.EndUpdate;
end;

procedure TFClassEditor.ChangeGetSet(Attribute: TAttribute; ClassNumber: Integer);
  var NewGet, NewSet: string;
      Method1, Method2: TOperation;
      GetIsFirst: Boolean;

  procedure DoGet;
  begin
    if CBGetMethod.Checked then
      if Assigned(Method1)
        then FMyEditor.ReplaceMethod(Method1, NewGet)
        else FMyEditor.InsertProcedure(ClassNumber, NewGet)
    else
      if Assigned(Method1) then
        FMyEditor.DeleteMethod(Method1);
  end;

  procedure DoSet;
  begin
    if CBSetMethod.Checked then
      if Assigned(Method2)
        then FMyEditor.ReplaceMethod(Method2, NewSet)
        else FMyEditor.InsertProcedure(ClassNumber, NewSet)
    else
      if Assigned(Method2) then
        FMyEditor.DeleteMethod(Method2);
  end;

begin
  // replace get/set-methods, names could be changed
  HasMethod(_(LNGGet), Attribute, Method1);
  HasMethod(_(LNGSet), Attribute, Method2);
  GetIsFirst:= True;
  if Assigned(Method1) and Assigned(Method2) and
    (Method1.LineS > Method2.LineS) then
      GetIsFirst:= False;
  ChangeAttribute(Attribute);
  FMyEditor.Editor.BeginUpdate;
  if FIsClass then begin
    NewGet:= CreateMethod(_(LNGGet), Attribute);
    NewSet:= CreateMethod(_(LNGSet), Attribute);
    if GetIsFirst then begin
      DoSet;
      DoGet;
    end else begin
      DoGet;
      DoSet;
    end;
  end else begin
    if GetIsFirst then begin
      if Assigned(Method2) then
        FMyEditor.DeleteMethod(Method2);
      if Assigned(Method1) then
        FMyEditor.DeleteMethod(Method1);
    end else begin
      if Assigned(Method1) then
        FMyEditor.DeleteMethod(Method1);
      if Assigned(Method2) then
        FMyEditor.DeleteMethod(Method2);
    end;
  end;
  FMyEditor.Editor.EndUpdate;
end;

function TFClassEditor.NameTypeValueChanged: Boolean;
begin
  var Node:= TreeView.Selected;
  if Assigned(Node) then begin
    var Str:= EAttributeName.Text + ': ' + CBAttributeType.Text;
    if EAttributeValue.Text <> '' then
      Str:= Str + ' = ' + EAttributeValue.Text;
    Result:= (Node.Text <> Str);
  end else
    Result:= False;
end;

procedure TFClassEditor.BAttributeChangeClick(Sender: TObject);
  var OldName, NewName: string;
      ClassNumber, NodeIndex, TopItemIndex: Integer;
      Attribute: TAttribute;
      Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  if not (MakeIdentifier(CBAttributeType, True) and MakeIdentifier(EAttributeName, False)) or
     (EAttributeName.Text = '') or (CBAttributeType.Text = '') or not Assigned(Node) or
     (not PartOfClass(Node) and (EAttributeValue.Text = '')) then
    Exit;
  ClassNumber:= GetClassNumber(Node);
  TopItemIndex:= TreeView.TopItem.AbsoluteIndex;
  NodeIndex:= Node.AbsoluteIndex;

  FMyEditor.Editor.BeginUpdate;
  if IsAttributesNode(Node) then begin
    NodeIndex:= NodeIndex + Node.Count + 1;
    Attribute:= MakeAttribute;
    Attribute.ScopeDepth:= GetLevel(Node) + 1;
    if AttributeAlreadyExists(Attribute.Name)
      then ErrorMsg(Format(_(LNGAlreadyExists), [Attribute.Name]))
      else AttributeToJava(Attribute, ClassNumber);
    FreeAndNil(Attribute);
  end else begin
    Attribute:= GetAttribute(Node);
    if Assigned(Attribute) then begin
      OldName:= Attribute.Name;
      NewName:= EAttributeName.Text;
      if (NewName <> OldName) and AttributeAlreadyExists(NewName) then
        ErrorMsg(Format(_(LNGAlreadyExists), [NewName]))
      else begin
        ChangeGetSet(Attribute, ClassNumber);
        FMyEditor.ReplaceLineWith(Attribute.LineS-1, Attribute.ToJava);
        FMyEditor.ReplaceWord(OldName, Attribute.Name, True);
      end;
    end;
  end;
  FMyEditor.Editor.EndUpdate;
  UpdateTreeView;
  if (0 <= TopItemIndex) and (TopItemIndex < TreeView.Items.Count) then
    TreeView.TopItem:= TreeView.Items[TopItemIndex];
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  BAttributeApply.Enabled:= False;
end;

procedure TFClassEditor.BAttributeDeleteClick(Sender: TObject);
  var Attribute: TAttribute; Method1, Method2: TOperation;
      Pos1, Pos2, Pos3: Integer; Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  if Assigned(Node) and IsAttributesNodeLeaf(Node) then begin
    FMyEditor.Editor.BeginUpdate;
    Attribute:= GetAttribute(Node);
    if Assigned(Attribute) then begin
      HasMethod(_(LNGGet), Attribute, Method1);
      HasMethod(_(LNGSet), Attribute, Method2);
      if Assigned(Method1) and Assigned(Method2) then begin
        if Method1.LineS < Method2.LineS then begin
          FMyEditor.DeleteMethod(Method2);
          FMyEditor.DeleteMethod(Method1);
        end else begin
          FMyEditor.DeleteMethod(Method1);
          FMyEditor.DeleteMethod(Method2);
        end;
      end else begin
        if Assigned(Method1) then FMyEditor.DeleteMethod(Method1);
        if Assigned(Method2) then FMyEditor.DeleteMethod(Method2);
      end;
      FMyEditor.DeleteComponentTotal(GetClassNumber(Node), Attribute.Name, Attribute.TypeClassifier.GetShortType);
      if Assigned(FMyEditor.Partner) then
        (FMyEditor.Partner as TFGUIForm).DeleteGUIComponent(Attribute.Name);
      FMyEditor.Modified:= True;
      FMyEditor.Editor.EndUpdate;
      Pos1:= TreeView.Selected.AbsoluteIndex;
      Pos2:= TreeView.FindNextToSelect.AbsoluteIndex;
      Pos3:= TreeView.TopItem.AbsoluteIndex;
      UpdateTreeView;
      if (0 <= Pos3) and (Pos3 < TreeView.Items.Count) then
        TreeView.TopItem:= TreeView.Items[Pos3];
      Pos1:= Min(Pos1, Pos2);
      if (0 <= Pos1) and (Pos1 < TreeView.Items.Count) then
        TreeView.Selected:= TreeView.Items[Pos1];
      BAttributeApply.Enabled:= False;
    end;
  end;
end;

procedure TFClassEditor.BMethodDeleteClick(Sender: TObject);
  var Node: TTreeNode; Method: TOperation; StringList: TStringList;
      Posi: Integer; HasSourceCode: Boolean;
begin
  Node:= TreeView.Selected;
  if Assigned(Node) and IsMethodsNodeLeaf(Node) then begin
    Method:= GetMethod(Node);
    if Assigned(Method) then begin
      HasSourceCode:= False;
      if Method.HasSourceCode then begin
        StringList:= TStringList.Create;
        try
          StringList.Text:= FMyEditor.GetSource(Method.LineS, Method.LineE-2);
          for Posi:= StringList.Count - 1 downto 0 do begin
            var Str:= Trim(StringList[Posi]);
            if (Pos('return', Str) = 1) or (Str = '') or (Str = _(LNGTODO)) then
              StringList.Delete(Posi);
          end;
          HasSourceCode:= (StringList.Count > 0);
        finally
          StringList.Free;
        end;
      end;
      if HasSourceCode and
         not (MessageDlg(Format('Method %s contains sourcecode.', [Method.Name]) +
                         #13 + _('Delete method with sourcecode?'),
                         mtConfirmation, mbYesNoCancel, 0) = mrYes)
         then Exit;
      FMyEditor.DeleteMethod(Method);
      Posi:= Node.AbsoluteIndex;
      UpdateTreeView;
      if Posi = TreeView.Items.Count then
        Dec(Posi);
      TreeView.Selected:= TreeView.Items[Posi];
    end;
    BMethodApply.Enabled:= False;
  end;
end;

procedure TFClassEditor.ActionApplyExecute(Sender: TObject);
begin
  case PageControl.ActivePageIndex of
    0: BClassChangeClick(Sender);
    1: begin
         BAttributeChangeClick(Sender);
         ActionNewExecute(Sender);
    end;
    2: begin
         BMethodChangeClick(Sender);
         ActionNewExecute(Sender);
    end;
  end;
end;

procedure TFClassEditor.ActionDeleteExecute(Sender: TObject);
begin
  if PageControl.ActivePageIndex = 1
    then BAttributeDeleteClick(Sender)
    else BMethodDeleteClick(Sender);
end;

function TFClassEditor.CreateMethod(const GetSet: string; const Attribute: TAttribute): string;
  var Str, AName, Attname, BigName, Indent1, Indent2, Datatype, Param: string;
begin
  Indent1:= GetIndent(Attribute.ScopeDepth);
  Indent2:= GetIndent(Attribute.ScopeDepth + 1);

  Datatype:= Attribute.TypeClassifier.GetShortType;
  Attname:= Attribute.Name;
  AName:= Attribute.Name;
  Param:= AName;

  if FConfiguration.AttributesAParametersP and (Copy(Param, 1, 1) = 'a') then begin
    Param[1]:= 'p';
    Delete(AName, 1, 1);
  end;
  if FConfiguration.SetterWithoutThis
    then Param:= Param + _('New')
    else Attname:= 'this.' + Attname;

  BigName:= UpperCase(Copy(AName, 1, 1)) + Copy(AName, 2, Length(AName));
  if GetSet = _(LNGGet)
    then Str:= Indent1 + 'public ' + Attribute.TypeClassifier.GetShortType + ' ' + _(LNGGet) + BigName + '() {' + CrLf +
               Indent2 + 'return ' + Attribute.Name + ';' + CrLf +
               Indent1 + '}' + CrLf + CrLf
    else Str:= Indent1 + 'public void ' + _(LNGSet) + BigName + '(' + Datatype + ' ' + Param + ') {' + CrLf +
               Indent2 + Attname + ' = ' + Param + ';' + CrLf +
               Indent1 + '}' + CrLf + CrLf;
  Result:= Str;
end;

function TFClassEditor.Typ2Value(const Typ: string): string;
  const
    Typs: array[1..9] of string = ('int', 'double', 'float', 'char', 'boolean',
                                   'String', 'long', 'byte', 'short');
    Vals: array[1..9] of string = ('0', '0', '0', '''\0''', 'false', '""', '0',
                                   '0', '0');
begin
  Result:= 'null';
  for var I:= 1 to 9 do
    if Typs[I] = Typ then begin
      Result:= Vals[I];
      Break;
    end;
end;

function TFClassEditor.MakeConstructor(Method: TOperation; Source: string): string;
  var Str, Str1, Typ, AName, Val, Indent1, Indent2: string;
      Ite1, Ite2: IModelIterator;
      Parameter: TParameter;
      Found: Boolean;
      Posi: Integer;
      Node: TTreeNode;
      SuperClass: TClass;
      Operation: TOperation;
      StringList: TStringList;

  procedure DeleteSource(const Str: string);
  begin
    Posi:= Pos(Str, Source);
    if Posi > 0 then
      insert('DE_LE_TE', Source, Posi);
  end;

  function GetSuperClass: TClass;
    var ClassIterator: IModelIterator;
  begin
    Result:= nil;
    var FullClassname:= GetClassName(GetClassNode);
    if Assigned(FMyUMLForm)
      then ClassIterator:= FMyUMLForm.MainModul.Model.ModelRoot.GetAllClassifiers
      else ClassIterator:= FMyEditor.Model.ModelRoot.GetAllClassifiers;
    while ClassIterator.HasNext do begin
      var Cent:= TClassifier(ClassIterator.Next);
      if (Cent.Name = FullClassname) and (Cent is TClass) then
        Exit((Cent as TClass).Ancestor);
    end;
  end;

begin
  Indent1:= GetIndent(Method.ScopeDepth);
  Indent2:= GetIndent(Method.ScopeDepth + 1);
  Parameter:= nil;
  Node:= GetAttributeNode.getFirstChild;
  Str:= '';
  Str1:= Indent1 + Method.ToJava + ' {' + CrLf;
  Ite1:= Method.GetParameters;
  while Ite1.HasNext  do begin
    Parameter:= Ite1.Next as TParameter;
    Parameter.UsedForAttribute:= False;
  end;
  while Assigned(Node) do begin // iterate over all attributes
    Ite1.Reset;
    Found:= False;
    while Ite1.HasNext and not Found do begin  // exists a corresponding parameter?
      Parameter:= Ite1.Next as TParameter;
      if Assigned(Parameter.TypeClassifier) then begin
        var Par:= Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType;
        if Pos(Par, Node.Text) = 1 then
          Found:= True;
      end;
    end;
    if Found then begin // corresponding parameter exists or
      Str:= Str + Indent2 + 'this.' + Parameter.Name + ' = ' + Parameter.Name + ';' + CrLf;
      DeleteSource('this.' + Parameter.Name);
      Parameter.UsedForAttribute:= True;
    end else
    if (Pos('=', Node.Text) > 0) then begin
      var Str3:= Copy(Node.Text, 1, Pos(':', Node.Text)-1);
      DeleteSource('this.' + Str3);
    end else begin
      Posi:= Pos(':', Node.Text);
      AName:= Copy(Node.Text, 1, Posi-1);
      Typ:= Copy(Node.Text, Posi+2, 255);
      Val:= Typ2Value(Typ);
      if Val = '' then Val:= 'null';

      Posi:= Pos('this.' + AName + ' = ' + AName, Source);
      if Posi > 0 then begin
        DeleteSource('this.' + AName);
        Str:= Str + Indent2 + 'this.' + AName + ' = ' + Val + ';' + CrLf;
      end else begin
        Posi:= Pos('this.' + AName, Source);
        if Posi = 0 then
          Str:= Str + Indent2 + 'this.' + AName + ' = ' + Val + ';' + CrLf;
      end;
    end;
    Node:= Node.getNextSibling;
  end;

  SuperClass:= GetSuperClass;
  Found:= False;
  if Assigned(SuperClass) then begin
    StringList:= TStringList.Create;
    try
      Ite1.Reset;
      while Ite1.HasNext  do begin
        Parameter:= Ite1.Next as TParameter;
        if not Parameter.UsedForAttribute and Assigned(Parameter.TypeClassifier) then
          StringList.Add(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType);
      end;

      Ite1:= SuperClass.GetOperations;
      while Ite1.HasNext do begin
        Operation:= Ite1.Next as TOperation;
        if Operation.OperationType = otConstructor then begin
          var Str2:= 'super(';
          Found:= True;
          Ite2:= Operation.GetParameters;
          while Ite2.HasNext do begin
            Parameter:= Ite2.Next as TParameter;
            if Assigned(Parameter.TypeClassifier) and
               (StringList.IndexOf(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType) >= 0)
            then Str2:= Str2 + Parameter.Name + ', '
            else Found:= False;
          end;
          if Found then begin
            if Str2.EndsWith(', ') then
              Str2:= UUtils.Left(Str2, -2);
            Str:= Str1 + Indent2 + Str2 + ');' + CrLf + Str;
            Break;
          end;
        end;
      end;
      if not Found then
        Str:= Str1 + Str;
    finally
      FreeAndNil(StringList);
    end;
  end else
    Str:= Str1 + Str;

  while Source <> '' do begin
    Posi:= Pos(#13#10, Source);
    Str1:= Copy(Source, 1, Posi+1);
    Delete(Source, 1, Posi+1);
    if (Pos('DE_LE_TE', Str1) = 0) and (Pos(Str1, Str) = 0) and (Pos(' return ', Str1) = 0) then
      Str:= Str + Str1;
  end;
  Str:= Str + Indent1 + '}' + CrLf + CrLf;
  Result:= Str;
end;

function TFClassEditor.MakeIdentifier(var Str1: string; Typ: Boolean): Integer;
  var Int, Int1, Int2: Integer; Str2, Str3: string; Chars: string;
begin
  Str3:= Str1;
  Result:= 0;
  // delete illegal chars at the beginning
  while (Length(Str1) > 0) and not (Str1[1].IsLetter or (Str1[1] = '_'))
  do begin
    Delete(Str1, 1, 1);
    Result:= 1;
  end;
  // delete illegal chars After the beginning
  Int:= 2;
  if Typ
    then Chars:= '_.[]<>'
    else Chars:= '_';

  while Int <= Length(Str1) do
    if not (Str1[Int].IsLetterOrDigit or (Pos(Str1[Int], Chars) > 0)) then begin
      Delete(Str1, Int, 1);
      Result:= Int;
    end else
      Inc(Int);
  // check for array
  Str2:= Str1;
  Int:= Pos('[]', Str2);
  while Int > 0 do begin
    Delete(Str2, Int, 2);
    Int:= Pos('[]', Str2);
  end;
  Int1:= Pos('[', Str2); Int2:= Pos(']', Str2);
  if (Int1 > 0) and ((Int2 = 0) or (Int2 < Int1)) or ((Int1 = 0) and (Int2 > 0)) then Result:= -1;

  // check for < >
  Int1:= Pos('<', Str1); Int2:= Pos('>', Str1);
  if (Int1 > 0) and ((Int2 = 0) or (Int2 < Int1)) or ((Int1 = 0) and (Int2 > 0)) then Result:= -1;
  if Int2 > Int1 then begin
    Str2:= Trim(Copy(Str1, Int1+1, Int2-Int1-1));
    if Str2 = '' then Result:= -1;
  end;
  if (Str3 = 'final') or (Str3 = 'static') or (Str3 = 'public') or (Str3 = 'private') then Result:= -1;
end;

function TFClassEditor.MakeIdentifier(ComboBox: TComboBox; Typ: Boolean): Boolean;
  var ErrPos, Len: Integer; Str: string;
     OnChange: TNotifyEvent;
begin
  Result:= True;
  Str:= ComboBox.Text;
  ErrPos:= MakeIdentifier(Str, Typ);
  if ErrPos = 0 then Exit;
  if ErrPos = -1 then begin
    Result:= False;
    Exit;
  end;
  Len:= Length(ComboBox.Text) - Length(Str);
  OnChange:= ComboBox.OnChange;
  ComboBox.OnChange:= nil;
  ComboBox.Text:= Str;
  ComboBox.OnChange:= OnChange;
  if Len = 1
    then ComboBox.SelStart:= ErrPos - 1
    else ComboBox.SelStart:= Length(Str);
end;

function TFClassEditor.MakeIdentifier(Edit: TEdit; Typ: Boolean): Boolean;
  var ErrPos, ErrLen: Integer; Str: string;
      OnChange: TNotifyEvent;
begin
  Result:= True;
  Str:= Edit.Text;
  ErrPos:= MakeIdentifier(Str, Typ);
  if ErrPos = 0 then
    Exit;
  Result:= False;
  if ErrPos = -1 then
    Exit;
  ErrLen:= Length(Edit.Text) - Length(Str);
  OnChange:= Edit.OnChange;
  Edit.OnChange:= nil;
  Edit.Text:= Str;
  Edit.OnChange:= OnChange;
  if ErrLen = 1
    then Edit.SelStart:= ErrPos - 1
    else Edit.SelStart:= Length(Str);
end;

procedure TFClassEditor.ChangeAttribute(var Attribute: TAttribute);
begin
  Attribute.Name:= EAttributeName.Text;
  Attribute.TypeClassifier:= MakeType(CBAttributeType);
  Attribute.Value:= EAttributeValue.Text;
  if FIsClass
    then Attribute.Visibility:= TVisibility(RGAttributeAccess.ItemIndex)
  else if RGAttributeAccess.ItemIndex = 0
    then Attribute.Visibility:= viPackage
    else Attribute.Visibility:= viPublic;
  Attribute.Static:= CBAttributeStatic.Checked;
  Attribute.IsFinal:= CBAttributeFinal.Checked;
end;

function TFClassEditor.MakeAttribute: TAttribute;
begin
  Result:= TAttribute.Create(nil);
  ChangeAttribute(Result);
end;

function TFClassEditor.MakeType(ComboBox: TComboBox): TClassifier;
begin
  var Str:= ComboBox.Text;
  var Index:= ComboBox.Items.IndexOf(Str);
  if Index <> -1 then
    Str:= ComboBox.Items[Index];
  if Str = ''
    then Result:= nil
    else Result:= MakeType(Str);
end;

function TFClassEditor.MakeType(const Classname: string): TClassifier;
  var MyUnit: TUnitPackage; AClass: TClass;
begin
  Result:= nil;
  MyUnit := FMyEditor.Model.ModelRoot.FindUnitPackage('Default');
  if Assigned(MyUnit) then
    Result := MyUnit.FindClassifier(Classname, nil, True);
  if not Assigned(Result) then begin
    AClass:= TClass.Create(nil);
    AClass.Name:= Classname;
    if Assigned(MyUnit) then
      MyUnit.AddClass(AClass);
    Result:= AClass;
  end;
end;

procedure TFClassEditor.ChangeMethod(var Method: TOperation);
  var Posi: Integer; AName, Typ, Str: string;
begin
  Method.Name:= CBMethodName.Text;
  Method.ReturnValue:= MakeType(CBMethodType);
  if FIsClass
    then Method.OperationType:= TOperationType(RGMethodKind.ItemIndex)
  else if RGMethodKind.ItemIndex = 0
    then Method.OperationType:= otProcedure
    else Method.OperationType:= otFunction;
  if FIsClass then
    Method.Visibility := TVisibility(RGMethodAccess.ItemIndex)
  else if RGMethodAccess.ItemIndex = 0
    then Method.Visibility:= viPackage
    else Method.Visibility:= viPublic;
  Method.Static:= CBMethodStatic.Checked;
  Method.IsAbstract:= CBMethodAbstract.Checked and FIsClass;
  Method.NewParameters;
  for var I:= 0 to LBParams.Items.Count - 1 do begin
    Str:= LBParams.Items[I];
    Posi:= Pos(':', Str);
    AName:= Copy(Str, 1, Posi-1);
    Typ:= Copy(Str, Posi+2, Length(Str));
    Method.AddParameter(AName).TypeClassifier:= MakeType(Typ);
  end;
  if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
    Method.AddParameter(CBParamName.Text).TypeClassifier:= MakeType(CBParamType);
end;

procedure TFClassEditor.SetClassOrInterface(IsClass: Boolean);

  procedure SetRadioGroup(RadioGroup: TRadioGroup; Text: string);
  begin
    if RadioGroup.Items.Text <> Text then begin
      var Index:= RadioGroup.ItemIndex;
      RadioGroup.Items.Text:= Text;
      RadioGroup.ItemIndex:= Index;
    end;
  end;

begin
  if IsClass then begin
    TSClass.Caption:= '&' + _(LNGClass);
    LClass.Caption:= _(LNGClass);
    BInterface.Caption:= _(LNGInterface);
    SetRadioGroup(RGAttributeAccess, FRGMethodAccessValues);
    SetRadioGroup(RGMethodAccess, FRGMethodAccessValues);
    SetRadioGroup(RGMethodKind, FRGMethodKindValues);
    LImplements.Visible:= True;
    EImplements.Visible:= True;
    CBClassAbstract.Visible:= True;
    CBMethodAbstract.Visible:= True;
    CBGetMethod.Visible:= True;
    CBSetMethod.Visible:= True;
    RGAttributeAccess.Height:= PPIScale(115);
    GBAttributeOptions.Height:= PPIScale(115);
    RGMethodKind.Height:= PPIScale(105);
    RGMethodAccess.Height:= PPIScale(105);
    GBMethodOptions.Visible:= True;
  end else begin
    TSClass.Caption:= '&' + _(LNGInterface);
    LClass.Caption:= _(LNGInterface);
    BInterface.Caption:= _(LNGClass);
    SetRadioGroup(RGAttributeAccess, FRGMethodAccessValues);
    RGAttributeAccess.Items.Delete(0);
    RGAttributeAccess.Items.Delete(1);
    SetRadioGroup(RGMethodAccess, RGAttributeAccess.Items.Text);
    SetRadioGroup(RGMethodKind, FRGMethodKindValues);
    RGMethodKind.Items.Delete(0);
    LImplements.Visible:= False;
    EImplements.Visible:= False;
    CBClassAbstract.Visible:= False;
    CBMethodAbstract.Visible:= False;
    CBGetMethod.Visible:= False;
    CBSetMethod.Visible:= False;
    RGAttributeAccess.Height:= PPIScale(65);
    GBAttributeOptions.Height:= PPIScale(65);
    RGMethodKind.Height:= PPIScale(65);
    RGMethodAccess.Height:= PPIScale(65);
    GBMethodOptions.Visible:= False;
  end;
  CBMethodStatic.Enabled:= IsClass;
end;

procedure TFClassEditor.UpdateTreeView(HasChanged: Boolean = True; NodeIndex: Integer = -1);
  var ClassIterator, Ite: IModelIterator;
      Cent: TClassifier;
      Attribute: TAttribute;
      Method: TOperation;
      IsFirstClass: Boolean;
begin
  if FTreeViewUpdating then Exit;
  FTreeViewUpdating:= True;
  TreeView.OnChange:= nil;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  FMyEditor.ParseSourceCode(HasChanged);
  ClassIterator:= FMyEditor.Model.ModelRoot.GetAllClassifiers;
  IsFirstClass:= True;
  while ClassIterator.HasNext do begin
    Cent:= TClassifier(ClassIterator.Next);
    if (Cent.Pathname <> FMyEditor.Pathname) or Cent.Anonym
      then Continue;
    if IsFirstClass then begin
      EClass.Text:= GetShortType(Cent.ShortName);
      IsFirstClass:= False;
    end;
    TVClassOrInterface(Cent);
    Ite:= Cent.GetAttributes;
    while Ite.HasNext do begin
      Attribute:= Ite.Next as TAttribute;
      TVAttribute(Attribute);
    end;
    Ite:= Cent.GetOperations;
    while Ite.HasNext do begin
      Method:= Ite.Next as TOperation;
      TVMethod(Method);
    end;
  end;
  TreeView.FullExpand;
  TreeView.Items.EndUpdate;
  if NodeIndex >= 0 then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  TreeView.OnChange:= TreeViewChange;
  FTreeViewUpdating:= False;
end;

function TFClassEditor.CreateTreeView(Editor: TFEditForm; UMLForm: TFUMLForm): Boolean;
begin
  FMyEditor:= Editor;
  FMyEditor.EnsureStartEnd(FMyEditor.CountClassOrInterface);
  FMyUMLForm:= UMLForm;
  UpdateTreeView;
  if TreeView.Items.Count > 0 then begin
    TreeView.Selected:= FAttributeNode;
    TreeView.TopItem:= TreeView.Items.GetFirstNode;
  end;
  Editor.Invalidate;
  Result:= True;
end;

procedure TFClassEditor.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
  var Vis: Boolean;
begin
  if AAction is TAction then begin
    if Assigned(FMyEditor)
      then Vis:= not FMyEditor.Editor.ReadOnly
      else Vis:= True;
    (AAction as TAction).Enabled:= Vis;
  end;
  Handled:= True;
end;

procedure TFClassEditor.BParameterNewClick(Sender: TObject);
begin
  if (CBParamName.Text = '') or (CBParamType.Text = '') then Exit;
  LBParams.Items.Add(CBParamName.Text + ': ' + CBParamType.Text );
  if CBParamName.Items.IndexOf(CBParamName.Text) = -1 then
     CBParamName.Items.AddObject(CBParamName.Text,
                                 TParamTyp.Create(CBParamType.Text));
  if FConfiguration.AttributesAParametersP
    then CBParamName.Text:= 'Posi'
    else CBParamName.Text:= '';
  CBParamType.Text:= '';
  CBParamType.ItemIndex:= -1;
  BMethodChangeClick(Self);
  if CBParamName.CanFocus then
    CBParamName.SetFocus;
end;

procedure TFClassEditor.BParameterDeleteClick(Sender: TObject);
begin
  CBParamName.Text:= '';
  CBParamType.Text:= '';
  CBParamType.ItemIndex:= -1;
  BMethodChangeClick(Self);
end;

procedure TFClassEditor.CBParamNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FComboBoxInvalid:= False;
    CBParamNameSelect(Self);
    if CBParamName.CanFocus then
      CBParamType.SetFocus;
  end;
end;

procedure TFClassEditor.CBParamNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
    BMethodApply.Enabled:= True;
  ShowMandatoryFields;
end;

procedure TFClassEditor.CBParamNameSelect(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  Posi:= CBParamName.ItemIndex;
  if (Posi = -1) or (CBParamName.Text <> CBParamName.Items[Posi])
    then Str:= CBParamName.Text
    else Str:= CBParamName.Items[Posi];
  if not FComboBoxInvalid then begin
    if (-1 < Posi) and Assigned(CBParamName.Items.Objects[Posi]) then
      CBParamType.Text:= TParamTyp(CBParamName.Items.Objects[Posi]).Typ;
    CBParamName.Text:= Str;
    if (CBParamName.Text <> '') and (CBParamType.Text <> '') then begin
      SBRightClick(Self);
      BMethodChangeClick(Self);
    end;
  end;
end;

procedure TFClassEditor.AllAttributesAsParameters(var Node: TTreeNode);
  var AClassname: string;
      ClassIterator, Ite: IModelIterator;
      Cent: TClassifier;
      Attribute: TAttribute;
      Method: TOperation;
      Parameter: TParameter;
      Posi: Integer;

  procedure MakeParameterFromAttributes(Cent: TClassifier);
  begin
    var Ite1:= Cent.GetAttributes;
    while Ite1.HasNext do begin
      Attribute:= Ite1.Next as TAttribute;
      var Str:= Attribute.ToNameType;
      if FConfiguration.AttributesAParametersP and (Copy(Str, 1, 1) = 'a') then
        Str[1]:= 'p';
      if CBParameter.Items.IndexOf(Str) = -1 then
        CBParameter.Items.Add(Str);
    end;
  end;

begin
  CBParameter.Clear;
  AClassname:= GetClassName(GetClassNode);
  Cent:= GetClassifier(GetClassNode);
  if Assigned(Cent) then begin
    MakeParameterFromAttributes(Cent);
    while (Cent is TClass) and Assigned((Cent as TClass).Ancestor) do begin
      Cent:= (Cent as TClass).Ancestor;
      MakeParameterFromAttributes(Cent);
    end;
  end;

  // look too in model of uml-diagram for ancestors
  if Assigned(FMyUMLForm) and Assigned(FMyUMLForm.MainModul) and
     Assigned(FMyUMLForm.MainModul.Model) and Assigned(FMyUMLForm.MainModul.Model.ModelRoot)
  then begin
    Cent:= nil;
    ClassIterator:= FMyUMLForm.MainModul.Model.ModelRoot.GetAllClassifiers;
    while ClassIterator.HasNext do begin
      Cent:= TClassifier(ClassIterator.Next);
      if Cent.Name = AClassname then
        Break;
    end;
    if Assigned(Cent) and (Cent.Name = AClassname) then
      while (Cent is TClass) and Assigned((Cent as TClass).Ancestor) do begin
        Cent:= (Cent as TClass).Ancestor;
        MakeParameterFromAttributes(Cent);
      end;
  end;

  if Assigned(Node) and IsMethodsNodeLeaf(Node) then begin
    Method:= GetMethod(Node);
    if Assigned(Method) then begin
      Ite:= Method.GetParameters;
      while Ite.HasNext  do begin
        Parameter:= Ite.Next as TParameter;
        var Str:= Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType;
        Posi:= CBParameter.Items.IndexOf(Str);
        if Posi > -1 then CBParameter.Items.Delete(Posi);
      end;
    end;
  end;
  CBParameter.Text:= _(LNGSelectAttributes);
end;

procedure TFClassEditor.ComboBoxCloseUp(Sender: TObject);
begin
  FComboBoxInvalid:= False;
end;

procedure TFClassEditor.CBAttributeTypeDropDown(Sender: TObject);
begin
  SendMessage(CBAttributeType.Handle, WM_SETCURSOR, 0, 0);
end;

procedure TFClassEditor.CBComboBoxEnter(Sender: TObject);
  var ComboBox: TComboBox; Rect: TRect; LeftPart: Boolean;
begin
  if Sender is TComboBox then begin
    ComboBox:= Sender as TComboBox;
    Rect:= ComboBox.ClientRect;
    Rect.Width:= Rect.Width - 20;
    Rect:= ComboBox.ClientToScreen(Rect);
    LeftPart:= Rect.Contains(Mouse.CursorPos);
    if LeftPart and (ComboBox.Items.Count > 0) then
      SendMessage(ComboBox.Handle, CB_SHOWDROPDOWN, 1, 0);
  end;
end;

procedure TFClassEditor.CBAttributeTypeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FComboBoxInvalid:= False;
    CBAttributeTypeSelect(Self);
    if EAttributeValue.CanFocus then EAttributeValue.SetFocus;
  end;
end;

procedure TFClassEditor.CBAttributeTypeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBAttributeType.Text <> '') and (Key <> VK_RETURN) then
    BAttributeApply.Enabled:= True;
end;

procedure TFClassEditor.CBAttributeTypeSelect(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  Posi:= CBAttributeType.ItemIndex;
  if (Posi = -1) or (CBAttributeType.Text <> CBAttributeType.Items[Posi])
    then Str:= CBAttributeType.Text
    else Str:= CBAttributeType.Items[Posi];
  if not FComboBoxInvalid then begin
    CBAttributeType.Text:= Str;
    if NameTypeValueChanged then
      BAttributeChangeClick(Self);
  end;
end;

procedure TFClassEditor.CBClassAbstractClick(Sender: TObject);
begin
  BClassChangeClick(Self);
end;

procedure TFClassEditor.CBClassAbstractMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BClassApply.Enabled:= True;
end;

procedure TFClassEditor.EAttributeNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and CBAttributeType.CanFocus then
    CBAttributeType.SetFocus;
end;

procedure TFClassEditor.EAttributeNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if CBAttributeType.Text <> '' then begin
    BAttributeApply.Enabled:= True;
    if Key = VK_RETURN then begin
      BAttributeChangeClick(Self);
    end;
  end;
end;

procedure TFClassEditor.EAttributeValueKeyPress(Sender: TObject; var Key: Char);
begin
  BAttributeApply.Enabled:= True;
  if (Key = #13) and NameTypeValueChanged then
    BAttributeChangeClick(Self);
  if (Key = #13) and BAttributeNew.CanFocus then
    BAttributeNew.SetFocus;
end;

procedure TFClassEditor.EClassKeyPress(Sender: TObject; var Key: Char);
begin
  BClassApply.Enabled:= True;
end;

procedure TFClassEditor.CBMethodnameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    BMethodChangeClick(Self);
    if CBMethodType.CanFocus then
      CBMethodType.SetFocus;
  end;
end;

procedure TFClassEditor.CBMethodnameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBMethodType.Text <> '') or
     (FIsClass and (RGMethodKind.ItemIndex = 1) or
      not FIsClass and (RGMethodKind.ItemIndex = 0)) then
    BMethodApply.Enabled:= True;
end;

procedure TFClassEditor.CBMethodnameSelect(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  Posi:= CBMethodName.ItemIndex;
  if (Posi = -1) or (CBMethodName.Text <> CBMethodName.Items[Posi])
    then Str:= CBMethodName.Text
    else Str:= CBMethodName.Items[Posi];
  if not FComboBoxInvalid then begin
    CBMethodName.Text:= Str;
    if CBMethodName.Items.IndexOf(Str) = -11 then
      CBMethodName.Items.Add(Str);
    if (CBMethodName.Text <> '') and (RGMethodKind.ItemIndex < 2) then
      BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.CBMethodTypeDropDown(Sender: TObject);
begin
  SendMessage(CBMethodType.Handle, WM_SETCURSOR, 0, 0);
  RGMethodKind.ItemIndex:= 2;
end;

procedure TFClassEditor.CBMethodTypeKeyPress(Sender: TObject; var Key: Char);
begin
  RGMethodKind.ItemIndex:= 2;
  if Key = #13 then begin
    FComboBoxInvalid:= False;
    CBMethodTypeSelect(Self);
    if CBParamName.CanFocus then
      CBParamName.SetFocus;
  end;
end;

procedure TFClassEditor.CBMethodParamTypeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBMethodType.Text <> '') and (Key <> VK_RETURN) then
    BMethodApply.Enabled:= True;
end;

procedure TFClassEditor.CBMethodTypeSelect(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  RGMethodKind.ItemIndex:= 2;
  Posi:= CBMethodType.ItemIndex;
  if (Posi = -1) or (CBMethodType.Text <> CBMethodType.Items[Posi])
    then Str:= CBMethodType.Text
    else Str:= CBMethodType.Items[Posi];
  BMethodApply.Enabled:= True;

  if not FComboBoxInvalid then begin
    CBMethodType.Text:= Str;
    if CBMethodType.Text <> '' then begin
      RGMethodKind.OnClick:= nil;
      if (RGMethodKind.ItemIndex = 1) and (CBMethodType.Text <> 'void') then
        RGMethodKind.ItemIndex:= 2
      else if (RGMethodKind.ItemIndex = 2) and (CBMethodType.Text = 'void') then
        RGMethodKind.ItemIndex:= 1;
      RGMethodKind.OnClick:= BMethodChangeClick;
    end;
    if (CBMethodName.Text <> '') and (CBMethodType.Text <> '') then
      BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.ComboBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FComboBoxInvalid:= True;
end;

procedure TFClassEditor.CBParameterSelect(Sender: TObject);
begin
  var Str:= CBParameter.Items[CBParameter.ItemIndex];
  if (Str <> _(LNGSelectAttributes)) and (LBParams.Items.IndexOf(Str) = -1) then begin
    LBParams.Items.Add(Str);
    BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.CBParamTypeDropDown(Sender: TObject);
begin
  SendMessage(CBParamType.Handle, WM_SETCURSOR, 0, 0);
end;

procedure TFClassEditor.CBParamTypeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FComboBoxInvalid:= False;
    CBParamTypeSelect(Self);
    if BParameterNew.CanFocus then
      BParameterNew.SetFocus;
  end;
end;

procedure TFClassEditor.CBParameterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    FComboBoxInvalid:= False;
    CBParameterSelect(Self);
  end;
end;

procedure TFClassEditor.CBParamTypeSelect(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  Posi:= CBParamType.ItemIndex;
  if (Posi = -1) or (CBParamType.Text <> CBParamType.Items[Posi])
    then Str:= CBParamType.Text
    else Str:= CBParamType.Items[Posi];
  if not FComboBoxInvalid then begin
    CBParamType.Text:= Str;
    if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
      BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.RGMethodKindChange;
begin
  if FIsClass and (RGMethodKind.ItemIndex = 0) then begin
    CBMethodName.Text:= WithoutGeneric(EClass.Text);
    if CBMethodName.Items.IndexOf(CBMethodName.Text) = -1 then
      CBMethodName.Items.Add(CBMethodName.Text);
  end;
  CBMethodName.Enabled:= FIsClass and (RGMethodKind.ItemIndex > 0) or not FIsClass;
  CBMethodType.Enabled:= FIsClass and (RGMethodKind.ItemIndex in [1, 2]) or
                         not FIsClass and (RGMethodKind.ItemIndex = 1);
  ShowMandatoryFields;
end;

procedure TFClassEditor.ShowMandatoryFields;
begin
  case PageControl.ActivePageIndex of
    1: begin
         if EAttributeName.Text = ''
           then EAttributeName.Color:= clInfoBk
           else EAttributeName.Color:= clWindow;
         if CBAttributeType.Text = ''
           then CBAttributeType.Color:= clInfoBk
           else CBAttributeType.Color:= clWindow;
         if FIsClass then
           EAttributeValue.Color:= clWindow
         else if EAttributeValue.Text = ''
           then EAttributeValue.Color:= clInfoBk
           else EAttributeValue.Color:= clWindow;
       end;
    2: begin
         if CBMethodName.Enabled and (CBMethodName.Text = '')
           then CBMethodName.Color:= clInfoBk
           else CBMethodName.Color:= clWindow;
         if CBMethodType.Enabled and (CBMethodType.Text = '')
             and (FIsClass and (RGMethodKind.ItemIndex = 2) or
                  not FIsClass and (RGMethodKind.ItemIndex = 1))
           then CBMethodType.Color:= clInfoBk
           else CBMethodType.Color:= clWindow;
         if (CBParamName.Text <> '') and (CBParamType.Text = '')
           then CBParamType.Color:= clInfoBk
           else CBParamType.Color:= clWindow;
         if (CBParamName.Text = '') and (CBParamType.Text <> '')
           then CBParamName.Color:= clInfoBk
           else CBParamName.Color:= clWindow;
       end;
    end;
end;

function TFClassEditor.MethodToJava(Method: TOperation; Source: string): string;
  var Str, Str1, Indent1, Indent2: string; StringList: TStringList;

  procedure DelParam(const Param: string);
  begin
    var Int:= 0;
    while Int < StringList.Count do
      if Pos(Param, StringList[Int]) > 0
        then StringList.Delete(Int)
        else Inc(Int);
  end;

  procedure AdjustReturnValue(Str2: string);
    const ReturnValues: array[1..5] of string = (' return 0;', ' return "";',
                        ' return false;', ' return ''\0'';', ' return null;');
    var  Posi: Integer;
  begin
    for var I:= 1 to 5 do begin
      Posi:= Pos(ReturnValues[I], Source);
      if Posi > 0 then begin
        Delete(Source, Posi, Length(ReturnValues[I]));
        Insert(Str2, Source, Posi);
        Break;
      end;
    end;
  end;

begin
  Indent1:= GetIndent(Method.ScopeDepth);
  Indent2:= GetIndent(Method.ScopeDepth + 1);
  Str:= Indent1 + Method.ToJava;
  if CBMethodAbstract.Checked or not FIsClass then
    Str:= Str + ';' + CrLf + CrLf
  else begin
    Str:= Str + ' {' + CrLf;
    Str1:= Indent1 + '}' + CrLf + CrLf;
    case Method.OperationType of
     otConstructor: Str:= MakeConstructor(Method, Source);
     otFunction:    begin
                      var Str2:= ' return ' + Typ2Value(Method.ReturnValue.GetShortType) + ';';
                      if Pos(' return ', Source) > 0 then begin
                        AdjustReturnValue(Str2);
                        Str:= Str + Source + Str1;
                      end else
                        if Source <> '' then
                          Str:= Str + Source + Indent2 + Str2 + CrLf + Str1
                        else
                          Str:= Str + Indent2 + _(LNGTODO) + CrLf +
                                  Indent2 + Str2 + CrLf + Str1;
                    end;
     otProcedure:   begin
                      if Source <> '' then begin
                        while Source <> '' do begin
                          var Pos1:= Pos(CrLf, Source);
                          var Str3:= Copy(Source, 1, Pos1 + 1);
                          Delete(Source, 1, Pos1 + 1);
                          if Pos(' return ', Str3) = 0 then
                            Str:= Str + Str3;
                        end;
                        Str:= Str + Source + Str1;
                      end else
                        Str:= Str + Indent2 + _(LNGTODO) + CrLf +
                                Indent2 + CrLf + Str1;
                    end;
    end;
  end;
  LBParams.Items.Clear;
  if not Method.HasComment then begin
    StringList:= TStringList.Create;
    StringList.Text:= FConfiguration.MethodComment;
    if not (Method.OperationType = otFunction) then
      DelParam('@return');
    Method.HasComment:= True;
    Result:=  StringList.Text + Str;
    FreeAndNil(StringList);
  end else
    Result:= Str;
end;

procedure TFClassEditor.BMethodChangeClick(Sender: TObject);
  var New, Source: string; Method: TOperation;
      Index, ClassNumber, NodeIndex, TopItemIndex: Integer;
      Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  CBMethodType.Enabled:= (RGMethodKind.ItemIndex = 2) or
                         (not FIsClass and (RGMethodKind.ItemIndex = 1));
  if not (MakeIdentifier(CBMethodName, False) and MakeIdentifier(CBMethodType, True) and
          MakeIdentifier(CBParamName, False) and MakeIdentifier(CBParamType, True)) or not Assigned(Node)
    then Exit;
  RGMethodKindChange;
  if FIsClass then Index:= 2 else Index:= 1;
  if (CBMethodName.Text = '') or
     ((RGMethodKind.ItemIndex = Index) and (CBMethodType.Text = '')) or
     ((CBParamName.Text <> '') and (CBParamType.Text = ''))
     then Exit;
  ClassNumber:= GetClassNumber(Node);
  NodeIndex:= Node.AbsoluteIndex;
  TopItemIndex:= TreeView.TopItem.AbsoluteIndex;

  FMyEditor.Editor.BeginUpdate;
  if IsMethodsNode(Node) then begin
    if (RGMethodKind.ItemIndex = 0) and FIsClass then begin
      Inc(NodeIndex);
      Node:= Node.GetNext;
      while Assigned(Node) and (Node.ImageIndex = 6) do begin
        Inc(NodeIndex);
        Node:= Node.GetNext;
      end;
    end else
      NodeIndex:= NodeIndex + Node.Count + 1;
    Method:= MakeMethod;
    Method.ScopeDepth:= GetLevel(Node) + 1;
    if not MethodAlreadyExists(Method.ToNameParameterTyp) then begin
      New:= MethodToJava(Method, '');
      if FIsClass and (RGMethodKind.ItemIndex = 0)
        then FMyEditor.InsertConstructor(ClassNumber, New)
        else FMyEditor.InsertProcedure(ClassNumber, New);
    end else if (Sender = BMethodApply) then
      ErrorMsg(Format(_(LNGAlreadyExists), [Method.ToNameParameterTyp]));
    FreeAndNil(Method);
  end else begin
    Method:= GetMethod(Node);
    if Assigned(Method) then begin
      ChangeMethod(Method);
      if Method.HasSourceCode
        then Source:= FMyEditor.GetSource(Method.LineS, Method.LineE-2)
        else Source:= '';
      if Method.OperationType = otConstructor
        then New:= MakeConstructor(Method, Source)
        else New:= MethodToJava(Method, Source);
      FMyEditor.ReplaceMethod(Method, New);
    end;
  end;
  FMyEditor.Editor.EndUpdate;
  UpdateTreeView;
  if (0 <= TopItemIndex) and (TopItemIndex < TreeView.Items.Count) then
    TreeView.TopItem:= TreeView.Items[TopItemIndex];
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  BMethodApply.Enabled:= False;
end;

function TFClassEditor.MakeMethod: TOperation;
begin
  Result:= TOperation.Create(nil);
  ChangeMethod(Result);
end;

procedure TFClassEditor.SaveWindow;
begin
  FConfiguration.WriteIntegerU('ClassEditor', 'RGAttributeAccess', RGAttributeAccess.ItemIndex);
  FConfiguration.WriteBoolU('ClassEditor', 'CBGetMethod', CBGetMethod.Checked);
  FConfiguration.WriteBoolU('ClassEditor', 'CBSetMethod', CBSetMethod.Checked);
  FConfiguration.WriteIntegerU('ClassEditor', 'RGMethodKind', RGMethodKind.ItemIndex);
  FConfiguration.WriteIntegerU('ClassEditor', 'RGMethodAccess', RGMethodAccess.ItemIndex);
  FConfiguration.WriteBoolU('ClassEditor', 'CBMethodStatic', CBMethodStatic.Checked);
  FConfiguration.WriteBoolU('ClassEditor', 'CBMethodAbstract', CBMethodAbstract.Checked);
end;

procedure TFClassEditor.SBDeleteClick(Sender: TObject);
  var Int: Integer;
begin
  Int:= LBParams.ItemIndex;
  LBParams.DeleteSelected;
  BMethodChangeClick(Self);
  if Int = LBParams.Count then
    Dec(Int);
  if Int >= 0 then
    LBParams.Selected[Int]:= True;
end;

procedure TFClassEditor.SBDownClick(Sender: TObject);
begin
  var Int:= LBParams.ItemIndex;
  if (0 <= Int) and (Int < LBParams.Count-1) then begin
    LBParams.Items.Exchange(Int, Int + 1);
    BMethodChangeClick(Self);
    LBParams.Selected[Int + 1]:= True;
  end;
end;

procedure TFClassEditor.SBUpClick(Sender: TObject);
begin
  var Int:= LBParams.ItemIndex;
  if (0 < Int) and (Int < LBParams.Count) then begin
    LBParams.Items.Exchange(Int, Int - 1);
    BMethodChangeClick(Self);
    LBParams.Selected[Int - 1]:= True;
  end;
end;

procedure TFClassEditor.SBLeftClick(Sender: TObject);
  var Str: string; Posi: Integer;
begin
  if (CBParamName.Text = '') and (CBParamType.Text = '') and
     (LBParams.ItemIndex < LBParams.Items.Count) and
     (0 <= LBParams.ItemIndex)
  then begin
    Str:= LBParams.Items[LBParams.ItemIndex];
    Posi:= Pos(': ', Str);
    CBParamName.Text:= Copy(Str, 1, Posi -1 );
    CBParamType.Text:= Copy(Str, Posi + 2, Length(Str));
    LBParams.Items.Delete(LBParams.ItemIndex);
  end;
end;

procedure TFClassEditor.SBRightClick(Sender: TObject);
begin
  if (CBParamName.Text <> '') and (CBParamType.Text <> '') then begin
    LBParams.Items.Add(CBParamName.Text + ': ' + CBParamType.Text);
    CBParamType.Text:= '';
    CBParamName.Text:= '';
  end;
end;

procedure TFClassEditor.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  TreeView.OnChange:= nil;
  TreeView.Items.Clear;
  for var I:= 0 to CBParamName.Items.Count - 1 do begin
    var AObject:= CBParamName.Items.Objects[I];
    FreeAndNil(AObject);
  end;
end;

procedure TFClassEditor.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  FRGMethodAccessValues:= RGMethodAccess.Items.Text;
  FRGMethodKindValues:= RGMethodKind.Items.Text;
  TreeView.Images:= FFileStructure.vilFileStructureLight;
  FMakeNewClass:= False;
  FTreeViewUpdating:= False;
end;

procedure TFClassEditor.FormShow(Sender: TObject);
  var Str: string;
begin
  Init(FConfiguration.ReadBoolU('UML', 'DefaultModifiers', True));
  var StringList:= TStringList.Create;
  for var I := 0 to FJava.TDIFormsList.Count - 1 do
    FJava.TDIFormsList[I].CollectClasses(StringList);
  for var I:= 0 to StringList.Count - 1 do begin
    Str:= GetShortType(StringList[I]);
    ComboBoxInsert2(CBAttributeType, Str);
    ComboBoxInsert2(CBMethodType, Str);
    ComboBoxInsert2(CBParamType, Str);
  end;
  ComboBoxInsert2(CBMethodType, 'void');
  FreeAndNil(StringList);
end;

procedure TFClassEditor.Init(Default: Boolean);
begin
  EnableEvents(False);
  CBAttributeStatic.Checked:= False;
  CBAttributeFinal.Checked:= False;
  CBMethodStatic.Checked:= False;
  CBMethodAbstract.Checked:= False;
  if Default then begin
    RGAttributeAccess.ItemIndex:= -1;
    RGAttributeAccess.ItemIndex:= 0;
    CBGetMethod.Checked:= True;
    CBSetMethod.Checked:= False;
    RGMethodKind.ItemIndex:= 1;
    RGMethodAccess.ItemIndex:= 3;
  end else begin
    RGAttributeAccess.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGAttributeAccess', 0);
    CBGetMethod.Checked:= FConfiguration.ReadBoolU('ClassEditor', 'CBGetMethod', False);
    CBSetMethod.Checked:= FConfiguration.ReadBoolU('ClassEditor', 'CBSetMethod', False);
    RGMethodKind.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGMethodKind', 1);
    RGMethodAccess.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGMethodAccess', 3);
  end;
  EnableEvents(True);
  ChangeStyle;
end;

procedure TFClassEditor.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
  var TargetNode, SourceNode: TTreeNode;
begin
  with TreeView do begin
    TargetNode:= GetNodeAt(X, Y); // Get target node
    SourceNode:= Selected;
    if not Assigned(TargetNode) then begin
      EndDrag(False);
      Exit;
    end;
    MoveNode(TargetNode, SourceNode);
  end;
end;

procedure TFClassEditor.TreeViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Sender = TreeView then
    Accept:= True;
end;

procedure TFClassEditor.MoveNode(TargetNode, SourceNode: TTreeNode);
  var From, To_, After, After_To, NodeIndex: Integer;
      EmptyLines: string;
      SourceModelEntity, TargetModelEntity: TModelEntity;
begin
  if IsClassOrInterface(SourceNode) or IsAttributesNode(SourceNode) or IsMethodsNode(SourceNode) or
     IsClassOrInterface(TargetNode) or FMyEditor.Editor.ReadOnly  then
    Exit;

  if IsAttributesNode(TargetNode) or IsMethodsNode(TargetNode) then
    TargetNode:= TargetNode.getFirstChild;
  if IsAttributesNodeLeaf(SourceNode) and IsAttributesNodeLeaf(TargetNode) then begin
    EmptyLines:= '';
    SourceModelEntity:= GetAttribute(SourceNode);
    TargetModelEntity:= GetAttribute(TargetNode);
  end else if IsMethodsNodeLeaf(SourceNode) and IsMethodsNodeLeaf(TargetNode) then begin
    EmptyLines:= #13#10;
    SourceModelEntity:= GetMethod(SourceNode);
    TargetModelEntity:= GetMethod(TargetNode);
  end else
    Exit;

  if Assigned(SourceModelEntity) then begin
    From:= SourceModelEntity.LineS;
    To_:= SourceModelEntity.LineE;
  end else
    Exit;

  if Assigned(TargetModelEntity) then begin
    After:= TargetModelEntity.LineS;
    After_To:= TargetModelEntity.LineE;
  end else
    Exit;

  if IsMethodsNodeLeaf(SourceNode) and  (Trim(FMyEditor.Editor.Lines[After_To]) = '') then
    Inc(After_To);
  if (From <= After) and (After <= To_) then
    Exit;
  FMyEditor.MoveBlock(From-1, To_-1, After-1, After_To-1, EmptyLines);
  NodeIndex:= TargetNode.AbsoluteIndex;
  UpdateTreeView;
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
end;

procedure TFClassEditor.LBParamsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and not FMyEditor.Editor.ReadOnly then
    LBParams.DeleteSelected;
end;

function TFClassEditor.IsClassOrInterface(Node: TTreeNode): Boolean;
begin
  Result:= Assigned(Node) and ((Node.ImageIndex = 1) or (Node.ImageIndex = 11));
end;

function TFClassEditor.IsAttributesNodeLeaf(Node: TTreeNode): Boolean;
begin
  Result:= Assigned(Node) and (Node.Parent.ImageIndex = 12);
end;

function TFClassEditor.IsAttributesNode(Node: TTreeNode): Boolean;
begin
  Result:= Assigned(Node) and (Node.ImageIndex = 12);
end;

function TFClassEditor.IsMethodsNodeLeaf(Node: TTreeNode): Boolean;
begin
  Result:= Assigned(Node) and (Node.Parent.ImageIndex = 13);
end;

function TFClassEditor.IsMethodsNode(Node: TTreeNode): Boolean;
begin
  Result:= Assigned(Node) and (Node.ImageIndex = 13);
end;

function TFClassEditor.AttributeAlreadyExists(const AttributeName: string): Boolean;
begin
  var Cent:= GetClassifier(GetClassNode);
  if Assigned(Cent) then begin
    var Ite:= Cent.GetAttributes;
    while Ite.HasNext do
      if TAttribute(Ite.Next).Name = AttributeName then
        Exit(True);
  end;
  Result:= False;
end;

function TFClassEditor.MethodAlreadyExists(const MethodName: string): Boolean;
begin
  var Cent:= GetClassifier(GetClassNode);
  if Assigned(Cent) then begin
    var Ite:= Cent.GetOperations;
    while Ite.HasNext do
      if TOperation(Ite.Next).ToNameParameterTyp = MethodName then
        Exit(True);
  end;
  Result:= False;
end;

procedure TFClassEditor.ChangeStyle;
begin
  if FConfiguration.IsDark then begin
    SBDelete.ImageIndex:= 1;
    TreeView.Images:= FFileStructure.vilFileStructureDark;
  end else begin
    SBDelete.ImageIndex:= 0;
    TreeView.Images:= FFileStructure.vilFileStructureLight;
  end;
end;

procedure TFClassEditor.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  ILClassEditor.SetSize(PPIScale(18), PPIScale(18));
  ChangeStyle;
end;

end.
