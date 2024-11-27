unit UDlgClassEditor;

interface

uses
  Controls, Classes, Forms, StdCtrls, ComCtrls, ImgList, ExtCtrls,
  Buttons, ActnList, System.ImageList, System.Actions, SVGIconImageListBase,
  SVGIconVirtualImageList, Vcl.BaseImageCollection, SVGIconImageCollection,
  Vcl.VirtualImageList,
  UModel, UUMLForm, UEditorForm;

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
    CBsetMethod: TCheckBox;
    CBgetMethod: TCheckBox;
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
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
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
    procedure ActionListUpdate(aAction: TBasicAction; var Handled: Boolean);
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
    myEditor: TFEditForm;
    myUMLForm: TFUMLForm;
    IsClass: boolean;
    RGMethodAccessValues: string;
    RGMethodKindValues: string;
    ComboBoxInvalid: boolean;
    MakeNewClass: boolean;
    TreeViewUpdating: boolean;
    AttributeNode: TTreeNode;
    MethodNode: TTreeNode;
    procedure AttributeToJava(Attribute: TAttribute; ClassNumber: integer);
    function MethodToJava(Method: TOperation; Source: string): string;
    procedure RGMethodKindChange;
    procedure ShowMandatoryFields;
    procedure SetClassOrInterface(aIsClass: boolean);
    procedure TVClassOrInterface(Classifier: TClassifier);
    procedure TVAttribute(Attribute: TAttribute);
    procedure TVMethod(Method: TOperation);
    function HasMethod(const getset: string; attribute: TAttribute; var method: TOperation): boolean;
    procedure ChangeAttribute(var A: TAttribute);
    procedure ChangeGetSet(Attribute: TAttribute; ClassNumber: integer);
    procedure NewClass;
    function MakeAttribute: TAttribute;
    function MakeType(CB: TComboBox): TClassifier; overload;
    function MakeType(const s: string): TClassifier; overload;
    procedure GetParameter(LB: TListBox; Method: TOperation);
    procedure ChangeMethod(var M: TOperation);
    function MakeMethod: TOperation;
    function CreateMethod(const getset: string; const Attribute: TAttribute): string;
    function makeConstructor(method: TOperation; Source: string): string;
    function Typ2Value(const typ: string): string;
    procedure MoveNode(TargetNode, SourceNode : TTreeNode);
    procedure EnableEvents(enable: boolean);
    procedure SetEditText(E: TEdit; const s: string);
    procedure SetEditValue(E: TEdit; const s: string);
    function MakeIdentifier(var s: string; typ: boolean): integer; overload;
    function MakeIdentifier(E: TEdit; typ: boolean):boolean; overload;
    function MakeIdentifier(CB: TComboBox; typ: boolean):boolean; overload;
    function PartOfClass(Node: TTreeNode): boolean;
    function GetClassNumber(Node: TTreeNode): integer;
    function GetNextClassNumber: string;
    function GetLastLineOflastClass: integer;
    function GetClassNode: TTreeNode;
    function GetClassInterfaceNode: TTreeNode;
    function GetClassName(ClassNode: TTreeNode): string;
    function GetAttributeNode: TTreeNode;
    function GetMethodNode: TTreeNode;
    function GetClassifier(ClassNode: TTreeNode): TClassifier;
    function GetAttribute(AttributeLeafNode: TTreeNode): TAttribute;
    function GetMethod(MethodLeafNode: TTreeNode): TOperation;
    function GetLevel(Node: TTreeNode): integer;
    function GetIndent(Level: Integer): string;
    function IsClassOrInterface(Node: TTreeNode): boolean;
    function IsAttributesNodeLeaf(Node: TTreeNode): boolean;
    function IsAttributesNode(Node: TTreeNode): boolean;
    function IsMethodsNodeLeaf(Node: TTreeNode): boolean;
    function IsMethodsNode(Node: TTreeNode): boolean;
    procedure AllAttributesAsParameters(var Node: TTreeNode);
    function AttributeAlreadyExists(const AttributeName: string): boolean;
    function MethodAlreadyExists(const MethodName: string): boolean;
    procedure Init(Default: boolean);
    function NameTypeValueChanged: boolean;
 public
    procedure SaveWindow;
    function CreateTreeView(Editor: TFEditForm; UMLForm: TFUMLForm): boolean;
    procedure UpdateTreeView(HasChanged: boolean = true; NodeIndex: integer = -1);
    procedure ChangeStyle;
  end;

implementation

{$R *.dfm}

uses Math, UITypes, StrUtils, Character, Windows, Messages, SysUtils, Graphics,
     Dialogs,
     SynEdit, JvGnugettext, UStringRessources, UGUIForm, UModelEntity,
     UJava, UConfiguration, UUtils, UBaseForm, UJavaParser, UFileStructure;

type TParamTyp = class (TObject)
     public
       typ: string;
       constructor create(const s: string);
     end;

constructor TParamTyp.create(const s: string);
begin
  typ:= s;
end;

procedure TFClassEditor.TVClassOrInterface(Classifier: TClassifier);
  var Node, Anchor: TTreeNode; p: integer; CName: string;

  function GetCorINode(s: string): TTreeNode;
    var Node: TTreeNode; s1: string; p: integer;
  begin
    Result:= nil;
    Node:= TreeView.Items.GetFirstNode;
    p:= Pos('$', s);
    s1:= copy(s, 1, p-1); delete(s, 1, p);
    while s1 <> '' do begin
      while Assigned(Node) and (withoutGeneric(Node.Text) <> s1) do
        Node:= Node.getNextSibling;
      if Node = nil then exit;
      Node:= Node.getFirstChild;
      p:= Pos('$', s);
      s1:= copy(s, 1, p-1); delete(s, 1, p);
    end;
    Result:= Node;
  end;

begin
  CName:= Classifier.Name;
  p:= LastDelimiter('$', CName);
  if p = 0 then
    Anchor:= nil
  else begin
    Anchor:= GetCorINode(copy(CName, 1, p));
    delete(CName, 1, p);
  end;
  
  Node:= TreeView.Items.Add(Anchor, CName);
  if (Classifier is TClass) then begin
    Node.ImageIndex:= 1;
    Node.SelectedIndex:= 1
  end else begin
    Node.ImageIndex:= 11;
    Node.SelectedIndex:= 11;
  end;

  AttributeNode:= TreeView.Items.AddChild(Node, _('Attributes'));
  AttributeNode.ImageIndex:= 12;
  AttributeNode.SelectedIndex:= 12;

  MethodNode:= TreeView.Items.AddChild(Node, _('Methods'));
  MethodNode.ImageIndex:= 13;
  MethodNode.SelectedIndex:= 13;

  ComboBoxInsert2(CBAttributeType, CName);
  ComboBoxInsert2(CBMethodType, CName);
  ComboBoxInsert2(CBParamType, CName);
end;

procedure TFClassEditor.TVAttribute(Attribute: TAttribute);
begin
  var Node:= TreeView.Items.AddChild(AttributeNode, Attribute.toNameTypeValue);
  Node.ImageIndex:= Integer(Attribute.Visibility) + 2;
  Node.SelectedIndex:= Integer(Attribute.Visibility) + 2;
end;

procedure TFClassEditor.TVMethod(Method: TOperation);
  var ImageNr: integer;
begin
  var Node:= TreeView.Items.AddChild(MethodNode, Method.toNameParameterTyp);
  if Method.OperationType = otConstructor
    then ImageNr:= 6
    else ImageNr:= Integer(Method.Visibility) + 7;
  Node.ImageIndex:= ImageNr;
  Node.SelectedIndex:= ImageNr;
end;

procedure TFClasseditor.NewClass;
  var CNumberStr, Indent1, Indent2, s: string; Line: integer;
begin
  MakeNewClass:= false;
  Indent1:= GetIndent(0);
  Indent2:= GetIndent(1);
  if CBClassInner.Checked then begin
    Indent1:= GetIndent(1);
    Indent2:= GetIndent(2);
  end;
  CNumberStr:= GetNextClassNumber;
  if BInterface.Caption = _(LNGClass)
    then s:= FConfiguration.HeadText(Indent1) + CrLf + Indent1 + 'interface ' + EClass.Text
    else s:= FConfiguration.HeadText(Indent1) + CrLf + Indent1 + 'class ' + EClass.Text;
  if EExtends.Text <> '' then
    s:= s + ' extends ' + EExtends.Text;
  if EImplements.Text <> '' then
    s:= s + ' implements ' + EImplements.Text;
  s:= s + ' {' + CrLf +
      Indent2 + CrLf +
      Indent2 + _(LNGStartGUIVariables) + CNumberStr + CrLf +
      Indent2 + _(LNGEndGUIVariables) + CNumberStr + CrLf +
      Indent2 + CrLf +
      Indent2 + _(LNGStartEventMethods) + CNumberStr + CrLf +
      Indent2 + _(LNGEndEventMethods) + CNumberStr + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    s:= s + ' // end of ' + EClass.Text;
  s:= s + CrLf;
  Line:= getLastLineOfLastClass;
  if CBClassInner.Checked then
    Dec(Line);
  myEditor.InsertLinesAt(Line, s);
  UpdateTreeView;
  TreeView.Selected:= TreeView.Items[TreeView.Items.Count-3];
  TreeViewChange(Self, TreeView.Selected);
end;

procedure TFClassEditor.BClassChangeClick(Sender: TObject);
  var s, aClassname, OldFile, NewFile, Searchtext, ReplacText, tail,
      oldClassname, newClassname: string;
      aClassifier: TClassifier;
      abstrakt, SkipUpdate: boolean;
      p, NodeIndex, i: integer;
      Node: TTreeNode;
      Form2: TFEditForm;
      GUIForm: TFGUIForm;
      SL: TStringList;
begin
  if Trim(EClass.Text) = '' then exit;
  if MakeNewClass then
    NewClass
  else begin
    Node:= TreeView.Selected;
    if not assigned(Node) then
      exit;
    NodeIndex:= Node.AbsoluteIndex;
    IsClass:= PartOfClass(Node);

    aClassifier:= GetClassifier(Node);
    abstrakt:= aClassifier.IsAbstract;
    oldClassname:= Node.Text;
    newClassname:= Trim(EClass.Text);

    if IsClass then begin
      Searchtext:= 'class ' + oldClassname;
      ReplacText:= 'class ' + newClassname;
    end else begin
      Searchtext:= 'interface ' + oldClassname;
      ReplacText:= 'interface ' + newClassname;
    end;

    with myEditor do begin
      Go_To(Searchtext);
      s:= Editor.LineText;
      s:= ReplaceStr(s, SearchText, ReplacText);

      if abstrakt and not CBClassAbstract.Checked then begin
        p:= Pos('abstract', s);
        if p > 0 then delete(s, p, 9);
      end else if not abstrakt and CBClassAbstract.Checked then begin
        p:= Pos(Searchtext, s);
        if p > 0 then insert('abstract ', s, p);
      end;

      p:= Pos('{', s);
      if p > 0 then begin
        tail:= copy(s, p, length(s));
        s:= copy(s, 1, p-1);
      end else
        tail:= '';

      p:= Pos('extends', s);
      if p > 0 then
        s:= copy(s, 1, p-1);
      p:= Pos('implements', s);
      if p > 0 then
        s:= copy(s, 1, p-1);
      s:= trim(s);

      SkipUpdate:= false;

      if eExtends.Text <> '' then
        if IsClass
          then s:= s + ' extends ' + getFirstWord(eExtends.Text)
        else begin
          SL:= split(',', EExtends.Text);
          for i:= SL.Count - 1 downto 0 do
            if trim(SL.Strings[i]) =  '' then begin
              SL.Delete(i);
              SkipUpdate:= true;
            end;
          if SL.Count > 0 then begin
            s:= s + ' extends ' + trim(SL.Strings[0]);
            for i:= 1 to SL.Count - 1 do
              s:= s + ', ' + trim(SL.Strings[i]);
          end;
          FreeAndNil(SL);
        end;

      if eImplements.Text <> '' then begin
        SL:= split(',', eImplements.Text);
        for i:= SL.Count - 1 downto 0 do
          if trim(SL.Strings[i]) =  '' then begin
            SL.Delete(i);
            SkipUpdate:= true;
          end;
        if SL.Count > 0 then begin
          s:= s + ' implements ' + trim(SL.Strings[0]);
          for i:= 1 to SL.Count - 1 do
            s:= s + ', ' + trim(SL.Strings[i]);
        end;
        FreeAndNil(SL);
      end;
      s:= s + ' ' + tail;

      Editor.LineText:= s;
      Modified:= true;
      aClassname:= Trim(EClass.Text);
      if (oldClassname <> aClassname) then begin
        OldFile:= Pathname;
        NewFile:= ExtractFilePath(Pathname) + WithoutGeneric(aClassname) + '.java';
        Form2:= TFEditForm(FJava.getTDIWindowType(NewFile, '%E%'));
        if assigned(Form2) and (Form2 <> myEditor) then
          InformationMsg(Format(_(LNGFileAlreadyOpen), [NewFile]))
        else if not FileExists(NewFile) or (MyEditor.Pathname = NewFile) or FileExists(NewFile) and
            (MessageDlg(Format(_(LNGFileAlreadyExists), [NewFile]),
                         mtConfirmation, mbYesNoCancel, 0) = mrYes)
        then begin
          if assigned(myUMLForm) then
            myUMLForm.Exchange(OldFile, NewFile);
          OldFile:= ChangeFileExt(OldFile, '.jfm');
          NewFile:= ChangeFileExt(NewFile, '.jfm');
          GuiForm:= FJava.getGUIForm(OldFile);
          if assigned(GuiForm) then
            GuiForm.Change(NewFile)
          else if FileExists(OldFile) then
            RenameFile(OldFile, NewFile);
          OldFile:= ChangeFileExt(OldFile, '.java');
          NewFile:= ChangeFileExt(NewFile, '.java');
          SaveAs(NewFile);
          if OldFile <> NewFile then  // due to generics
            deleteFile(OldFile);
          ComboBoxDelete2(CBAttributeType, oldClassname);
          ComboBoxDelete2(CBMethodType, oldClassname);
          ComboBoxDelete2(CBParamType, oldClassname);
          myEditor.ReplaceWord(oldClassname, withoutGeneric(newClassname), true);
        end;
      end;
      if SkipUpdate = false then begin
        UpdateTreeView;
        if NodeIndex < TreeView.Items.Count then
          TreeView.Selected:= TreeView.Items[NodeIndex];
      end;
    end;
  end;
  BClassApply.Enabled:= false;
end;

procedure TFClassEditor.ActionInterfaceExecute(Sender: TObject);
  var aClassname: string; Node: TTreeNode; NodeIndex: integer;
begin
  Node:= TreeView.Selected;
  if assigned(Node) then begin
    NodeIndex:= Node.AbsoluteIndex;
    with myEditor do begin
      aClassName:= Node.Text;
      if BInterface.Caption = _(LNGClass)
        then ReplaceText('interface ' + aClassname, 'class ' + aClassname, true)
        else ReplaceText('class ' + aClassname, 'interface ' + aClassname, true);
      Modified:= true;
      UpdateTreeView;
      if NodeIndex < TreeView.Items.Count then
        TreeView.Selected:= TreeView.Items[NodeIndex];
    end;
  end;
end;

function TFClassEditor.HasMethod(const getset: string; attribute: TAttribute; var method: TOperation): boolean;
  var aName, MethodName: string;
      cent: TClassifier;
begin
  aName:= attribute.Name;
  if FConfiguration.AttributesAParametersP and (copy(aName, 1, 1) = 'a') then
    delete(aName, 1, 1);
  aName:= Uppercase(Copy(aName, 1, 1)) + Copy(aName, 2, length(aName));
  if getset = _(LNGGet)
    then MethodName:= _(LNGGet) + aName
    else MethodName:= _(LNGSet) + aName;
  cent:= GetClassifier(GetClassNode);
  if Assigned(cent) then begin
    var it1:= cent.GetOperations;
    while it1.HasNext do begin
      Method:= it1.Next as TOperation;
      if Method.Name = MethodName then
        Exit(true);
    end;
  end;
  Method:= nil;
  Result:= false;
end;

procedure TFClassEditor.EnableEvents(enable: boolean);
begin
  if enable and not myEditor.Editor.ReadOnly then begin
    CBClassAbstract.OnClick:= BClassChangeClick;
    //EClass.OnChange:= BClassChangeClick;
    //EExtends.OnChange:=  BClassChangeClick;
    //EImplements.OnChange:= BClassChangeClick;

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

function TFClassEditor.PartOfClass(Node: TTreeNode): boolean;
begin
  Result:= false;
  if assigned(Node) then begin
    while (Node.ImageIndex <> 1) and (Node.ImageIndex <> 11) do
      Node:= Node.Parent;
    Result:= (Node.ImageIndex = 1);
  end;
end;

function TFClassEditor.GetClassNumber(Node: TTreeNode): integer;
begin
  Result:= -1;
  var aNode:= TreeView.Items.GetFirstNode;
  while aNode <> Node do begin
    if IsClassOrInterface(aNode) then inc(Result);
    aNode:= aNode.GetNext;
  end;
end;

function TFClassEditor.GetNextClassNumber: string;
begin
  var i:= 0;
  var aNode:= TreeView.Items.GetFirstNode;
  while assigned(aNode) do begin
    if IsClassOrInterface(aNode) then inc(i);
    aNode:= aNode.GetNext;
  end;
  Result:= IntToStr(i);
end;

function TFClassEditor.GetLastLineOflastClass: integer;
begin
  Result:= -1;
  myEditor.ParseSourceCode(false);
  var Ci:= myEditor.Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    var cent:= TClassifier(Ci.Next);
    if cent.Pathname = myEditor.Pathname then
      Result:= max(Result, cent.LineE);
  end;
end;

function TFClassEditor.GetClassInterfaceNode: TTreeNode;
begin
  // we may have multiple classes in the treeview
  Result:= TreeView.Selected;
  if assigned(Result) then
    while (Result <> nil) and (Result.ImageIndex <> 1) and (Result.ImageIndex <> 11) do
      Result:= Result.Parent
  else
    for var i:= 0 to TreeView.Items.Count - 1 do
      if TreeView.Items[i].ImageIndex in [1, 11] then begin
        Result:= TreeView.Items[i];
        break;
      end;
end;

function TFClassEditor.GetClassNode: TTreeNode;
begin
  // we may have multiple classes in the treeview
  Result:= TreeView.Selected;
  if assigned(Result) then
    while (Result <> nil) and (Result.ImageIndex <> 1) do
      Result:= Result.Parent
  else
    for var i:= 0 to TreeView.Items.Count - 1 do
      if TreeView.Items[i].ImageIndex = 1 then begin
        Result:= TreeView.Items[i];
        break;
      end;
end;

function TFClassEditor.GetClassName(ClassNode: TTreeNode): string;
begin
  // we may have multilevel inner classes in the treeview
  if assigned(ClassNode) then begin
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
  if assigned(Result) then
    Result:= Result.GetFirstChild;
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
  var Ci:= myEditor.Model.ModelRoot.GetAllClassifiers;
  while Ci.HasNext do begin
    Result:= TClassifier(Ci.Next);
    if Result.Name = FullClassname then
      exit(Result);
  end;
  Result:= nil;
end;

function TFClassEditor.GetAttribute(AttributeLeafNode: TTreeNode): TAttribute;
begin
  var cent:= GetClassifier(AttributeLeafNode.Parent.Parent);
  if assigned(cent) then begin
    var it:= cent.GetAttributes;
    while it.HasNext do begin
      Result:= TAttribute(it.Next);
      if Result.toNameTypeValue = AttributeLeafNode.Text then
        exit(Result);
    end;
  end;
  Result:= nil;
end;

function TFClassEditor.GetMethod(MethodLeafNode: TTreeNode): TOperation;
begin
  var cent:= GetClassifier(MethodLeafNode.Parent.Parent);
  if assigned(cent) then begin
    var it:= cent.GetOperations;
    while it.HasNext do begin
      Result:= TOperation(it.Next);
      if Result.toNameParameterTyp = MethodLeafNode.Text then
        exit(Result);
    end;
  end;
  Result:= nil;
end;

function TFClassEditor.GetLevel(Node: TTreeNode): integer;
  var aNode: TTreeNode;
begin
  if Node = nil then
    exit(0);
  Result:= 0;
  if IsClassOrInterface(Node) then
    aNode:= Node
  else if IsAttributesNode(Node) or IsMethodsNode(Node) then
    aNode:= Node.Parent
  else
    aNode:= Node.Parent.Parent;
  while aNode.Parent <> nil do begin
    Result:= Result + 1;
    aNode:= aNode.Parent;
  end;
end;

function TFClassEditor.GetIndent(Level: Integer): string;
begin
  Result:= StringTimesN(FConfiguration.Indent1, Level);
end;

procedure TFClassEditor.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;
  EAttributeName.SelStart:= 1;
  EAttributeName.SelLength:= 0;
end;

procedure TFClassEditor.TreeViewChange(Sender: TObject; Node: TTreeNode);
  var aClassifier: TClassifier; aAttribute: TAttribute; aMethod: TOperation;
      Line, ClassNumber, p, q, l: Integer; it: IModelIterator;
      s, s1: string;
begin
  if Node = nil then exit;
  Line:= -1;
  EnableEvents(false);
  try
    IsClass:= PartOfClass(Node);
    if IsClassOrInterface(Node) then begin
      PageControl.ActivePageIndex:= 0;
      aClassifier:= GetClassifier(Node);
      if aClassifier = nil then exit;
      EClass.Text:= getShortType(aClassifier.ShortName);
      CBClassAbstract.Checked:= aClassifier.IsAbstract;
      CBClassInner.Checked:= aClassifier.Inner;
      if IsClass then begin
        if Assigned((aClassifier as TClass).Ancestor)
          then s:= getShortType((aClassifier as TClass).Ancestor.ShortName)
          else s:= '';
        SetEditText(EExtends, s);
        it:= (aClassifier as TClass).GetImplements;
        s:= '';
        while it.HasNext do begin
          s1:= getShortType((it.Next as TInterface).Shortname);
          s:= s + ', ';
          if Pos('{', s1) = 0 then
            s:= s + s1;
        end;
        Delete(s, 1, 2);
        EImplements.Text:= s;
      end else begin
        it:= (aClassifier as TInterface).GetExtends;
        s:= '';
        while it.HasNext do begin
          s1:= getShortType((it.Next as TInterface).Shortname);
          s:= s + ', ';
          if Pos('{', s1) = 0 then
            s:= s + s1;
        end;
        Delete(s, 1, 2);
        p:= EExtends.SelStart;
        q:= length(EExtends.Text);
        l:= length(s);
        EExtends.Text:= s;
        if l > q
          then EExtends.SelStart:= p+1
        else if l < q
          then EExtends.SelStart:= p-1;
      end;
      Line:= aClassifier.LineS;
    end else if IsAttributesNode(Node) or IsAttributesNodeLeaf(Node) then begin
      PageControl.ActivePageIndex:= 1;
      BAttributeApply.Enabled:= false;
      if IsAttributesNodeLeaf(Node) then begin
        aAttribute:= GetAttribute(Node);
        if aAttribute = nil then exit;
        SetEditText(EAttributeName, aAttribute.Name);
        CBAttributeType.Text:= aAttribute.TypeClassifier.GetShortType;
        SetEditValue(EAttributeValue, aAttribute.Value);
        if IsClass
          then RGAttributeAccess.ItemIndex:= Integer(aAttribute.Visibility)
        else if aAttribute.Visibility = viPackage
          then RGAttributeAccess.ItemIndex:= 0
          else RGAttributeAccess.ItemIndex:= 1;
        CBGetMethod.Checked:= HasMethod(_(LNGGet), aAttribute, aMethod);
        CBSetMethod.Checked:= HasMethod(_(LNGSet), aAttribute, aMethod);
        CBAttributeStatic.Checked:= aAttribute.Static;
        CBAttributeFinal.Checked:= aAttribute.IsFinal;
        Line:= aAttribute.LineS;
      end else begin
        CBAttributeType.Text:= '';
        CBAttributeType.ItemIndex:= -1;
        EAttributeValue.Text:= '';
        ActiveControl:= EAttributeName;
        if FConfiguration.AttributesAParametersP then begin
          EAttributeName.Text:= 'a';
          Timer1.Enabled:= true;
        end else
          EAttributeName.Text:= '';
      end;
      ComboBoxInsert(CBAttributeType);
    end else begin
      PageControl.ActivePageIndex:= 2;
      BMethodApply.Enabled:= false;
      AllAttributesAsParameters(Node);
      if FConfiguration.AttributesAParametersP
        then CBParamName.Text:= 'p'
        else CBParamName.Text:= '';
      CBParamType.Text:= '';
      CBParamType.ItemIndex:= -1;
      if IsMethodsNodeLeaf(Node) then begin
        EClass.Text:= Node.Parent.Parent.Text; // wg. constructor
        aMethod:= GetMethod(Node);
        if aMethod = nil then exit;
        CBMethodName.Text:= aMethod.Name;
        if Assigned(aMethod.ReturnValue)
          then CBMethodType.Text:= aMethod.ReturnValue.GetShortType
          else CBMethodType.Text:= '';
        if IsClass then begin
          RGMethodKind.ItemIndex:= Integer(aMethod.OperationType);
          RGMethodAccess.ItemIndex:= Integer(aMethod.Visibility);
        end else begin
          RGMethodKind.ItemIndex:= Integer(aMethod.OperationType) - 1;
          if aMethod.Visibility = viPackage
            then RGMethodAccess.ItemIndex:= 0
            else RGMethodAccess.ItemIndex:= 1;
        end;
        CBMethodStatic.Checked:= aMethod.Static;
        CBMethodAbstract.Checked:= aMethod.IsAbstract;
        GetParameter(LBParams, aMethod);
        Line:= aMethod.LineS;
      end else begin
        if Assigned(Node)
          then EClass.Text:= Node.Parent.Text // wg. constructor
          else EClass.Text:= '';
        if IsClass
          then RGMethodKind.ItemIndex:= 1     // wg. constructor
          else RGMethodKind.ItemIndex:= 0;
        CBMethodName.Text:= '';
        CBMethodType.Text:= '';
        CBMethodType.ItemIndex:= -1;
        CBMethodStatic.Checked:= false;
        CBMethodAbstract.Checked:= false;
        LBParams.Clear;
        if IsClass and (RGMethodKind.ItemIndex > 0) or not IsClass then begin
          CBMethodName.Enabled:= true;
          ActiveControl:= CBMethodName;
        end;
      end;
      ComboBoxInsert(CBMethodType);
      ComboBoxInsert(CBParamName);
      ComboBoxInsert(CBParamType);
      CBMethodName.Enabled:= (RGMethodKind.ItemIndex > 0) or not IsClass;
      CBMethodType.Enabled:= (RGMethodKind.ItemIndex = 2) or
                             (not IsClass and (RGMethodKind.ItemIndex = 1));
    end;
    if IsAttributesNode(Node) or IsMethodsNode(Node) then
      if myEditor.Editor.ReadOnly
        then Line:= -1
        else begin
          ClassNumber:= GetClassNumber(Node);
          if Node.ImageIndex = 12
            then myEditor.Go_To(myEditor.GetLNG(1, ClassNumber))
            else myEditor.Go_To(myEditor.GetLNG(5, ClassNumber));
          Line:= myEditor.Editor.CaretY;
        end;
    if Line <> -1 then
      myEditor.GotoLine(Line);
    SetClassOrInterface(IsClass);
    ShowMandatoryFields;
  finally
    EnableEvents(true);
  end;
end;

procedure TFClassEditor.SetEditText(E: TEdit; const s: string);
begin
  if E.Text <> s then begin
    var p:= E.SelStart;
    E.Text:= s;
    E.SelStart:= p;
  end;
end;

procedure TFClassEditor.SetEditValue(E: TEdit; const s: string);
begin
  if E.Text <> s then begin
    var p:= E.SelStart;
    if (Pos('"', s) > 0) and (Pos('"', E.Text) = 0) then inc(p);
    E.Text:= s;
    E.SelStart:= p;
  end;
end;

procedure TFClassEditor.GetParameter(LB: TListBox; Method: TOperation);
  var It2: IModelIterator; Parameter: TParameter; s: string; i: integer;
begin
  LB.Clear;
  it2:= Method.GetParameters;
  while it2.HasNext do begin
    Parameter:= it2.next as TParameter;
    if assigned(Parameter.TypeClassifier) then
      LB.Items.Add(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType);
    if Parameter.Name = CBParamName.Text then
      CBParamType.Text:= Parameter.TypeClassifier.GetShortType;
  end;
  s:= CBParamName.Text + ': ' + CBParamType.Text;
  i:= LB.Items.IndexOf(s);
  if i <> -1 then LB.Items.Delete(i);
end;

procedure TFClassEditor.PageControlChange(Sender: TObject);
  var Node: TTreeNode; tab: integer;
begin
  tab:= PageControl.TabIndex;
  if BClassApply.Enabled then BClassChangeClick(Sender);
  if BAttributeApply.Enabled then BAttributeChangeClick(Sender);
  if BMethodApply.Enabled then BMethodChangeClick(Sender);
  PageControl.TabIndex:= tab;
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
      CBAttributeStatic.Checked:= false;
      CBAttributeFinal.Checked:= false;
      if FConfiguration.DefaultModifiers then begin
        RGAttributeAccess.ItemIndex:= 0;
        CBGetMethod.Checked:= true;
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
      CBMethodStatic.Checked:= false;
      CBMethodAbstract.Checked:= false;
      if FConfiguration.DefaultModifiers and IsClass then begin
        RGMethodKind.ItemIndex:= 1;
        RGMethodAccess.ItemIndex:= 3;
      end;
    end;
  else begin
      EClass.Text:= '';
      EExtends.Text:= '';
      EImplements.Text:= '';
      CBClassAbstract.Checked:= false;
      CBClassInner.Checked:= false;
      TreeView.Selected:= GetClassInterfaceNode;
      MakeNewClass:= true;
    end;
  end;
end;

procedure TFClassEditor.AttributeToJava(Attribute: TAttribute; ClassNumber: integer);
  var Method: TOperation;
begin
  myEditor.Editor.BeginUpdate;
  myEditor.InsertAttribute(ClassNumber, Attribute.toJava);
  if IsClass and CBGetMethod.Checked and not HasMethod(_(LNGGet), Attribute, Method) then
    myEditor.InsertProcedure(ClassNumber, CreateMethod(_(LNGGet), Attribute));
  if IsClass and CBSetMethod.Checked and not HasMethod(_(LNGSet), Attribute, Method) then
    myEditor.InsertProcedure(ClassNumber, CreateMethod(_(LNGSet), Attribute));
  myEditor.Editor.EndUpdate;
end;

procedure TFClassEditor.ChangeGetSet(Attribute: TAttribute; Classnumber: integer);
  var NewGet, NewSet: string;
      Method1, Method2: TOperation;
      getIsFirst: boolean;

  procedure DoGet;
  begin
    if CBGetMethod.Checked then
      if assigned(Method1)
        then myEditor.ReplaceMethod(Method1, NewGet)
        else myEditor.InsertProcedure(ClassNumber, NewGet)
    else
      if assigned(Method1) then
        myEditor.DeleteMethod(Method1);
  end;

  procedure DoSet;
  begin
    if CBSetMethod.Checked then
      if assigned(Method2)
        then myEditor.ReplaceMethod(Method2, NewSet)
        else myEditor.InsertProcedure(ClassNumber, NewSet)
    else
      if assigned(Method2) then
        myeditor.DeleteMethod(Method2);
  end;

begin
  // replace get/set-methods, names could be changed
  HasMethod(_(LNGGet), Attribute, Method1);
  HasMethod(_(LNGSet), Attribute, Method2);
  GetIsFirst:= true;
  if Assigned(Method1) and Assigned(Method2) and
    (Method1.LineS > Method2.LineS) then
      getIsFirst:= false;
  ChangeAttribute(Attribute);
  myEditor.Editor.BeginUpdate;
  if IsClass then begin
    NewGet:= CreateMethod(_(LNGGet), Attribute);
    NewSet:= CreateMethod(_(LNGSet), Attribute);
    if getIsFirst then begin
      DoSet;
      DoGet;
    end else begin
      DoGet;
      DoSet;
    end;
  end else begin
    if getIsFirst then begin
      if assigned(Method2) then
        myEditor.DeleteMethod(Method2);
      if assigned(Method1) then
        myEditor.DeleteMethod(Method1);
    end else begin
      if assigned(Method1) then
        myEditor.DeleteMethod(Method1);
      if assigned(Method2) then
        myEditor.DeleteMethod(Method2);
    end;
  end;
  myEditor.Editor.EndUpdate;
end;

function TFClassEditor.NameTypeValueChanged: boolean;
begin
  var Node:= TreeView.Selected;
  if assigned(Node) then begin
    var s:= EAttributeName.text + ': ' + CBAttributeType.Text;
    if EAttributeValue.Text <> '' then
      s:= s + ' = ' + EAttributeValue.Text;
    Result:= (Node.Text <> s)
  end else
    Result:= false;
end;

procedure TFClassEditor.BAttributeChangeClick(Sender: TObject);
  var OldName, NewName: string;
      ClassNumber, NodeIndex, TopItemIndex: integer;
      Attribute: TAttribute;
      Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  if not (MakeIdentifier(CBAttributeType, true) and MakeIdentifier(EAttributeName, false)) or
     (EAttributeName.Text = '') or (CBAttributeType.Text = '') or (Node = nil) or
     (not PartOfClass(Node) and (EAttributeValue.Text = '')) then
    exit;
  ClassNumber:= GetClassNumber(Node);
  TopItemIndex:= TreeView.TopItem.AbsoluteIndex;
  NodeIndex:= Node.AbsoluteIndex;

  myEditor.Editor.BeginUpdate;
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
    if assigned(Attribute) then begin
      OldName:= Attribute.Name;
      NewName:= EAttributeName.Text;
      if (NewName <> OldName) and AttributeAlreadyExists(NewName) then
        ErrorMsg(Format(_(LNGAlreadyExists), [NewName]))
      else begin
        ChangeGetSet(Attribute, Classnumber);
        myEditor.ReplaceLineWith(Attribute.LineS-1, Attribute.toJava);
        myEditor.ReplaceWord(OldName, Attribute.Name, true);
      end;
    end;
  end;
  myEditor.Editor.EndUpdate;
  UpdateTreeView;
  if (0 <= TopItemIndex) and (TopItemIndex < TreeView.Items.Count) then
    TreeView.TopItem:= TreeView.Items[TopItemIndex];
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  BAttributeApply.Enabled:= false;
end;

procedure TFClassEditor.BAttributeDeleteClick(Sender: TObject);
  var Attribute: TAttribute; Method1, Method2: TOperation;
      p1, p2, p3: integer; Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  if assigned(Node) and IsAttributesNodeLeaf(Node) then begin
    myEditor.Editor.BeginUpdate;
    Attribute:= GetAttribute(Node);
    if assigned(Attribute) then begin
      HasMethod(_(LNGGet), Attribute, Method1);
      HasMethod(_(LNGSet), Attribute, Method2);
      if Assigned(Method1) and Assigned(Method2) then begin
        if Method1.LineS < Method2.LineS then begin
          myEditor.DeleteMethod(Method2);
          myEditor.DeleteMethod(Method1);
        end else begin
          myEditor.DeleteMethod(Method1);
          myEditor.DeleteMethod(Method2);
        end;
      end else begin
        if Assigned(Method1) then myEditor.DeleteMethod(Method1);
        if Assigned(Method2) then myEditor.DeleteMethod(Method2);
      end;
      myEditor.DeleteComponentTotal(GetClassNumber(Node), Attribute.Name, Attribute.TypeClassifier.GetShortType);
      if Assigned(myEditor.Partner) then
        (myEditor.Partner as TFGUIForm).DeleteGUIComponent(Attribute.Name);
      myEditor.Modified:= true;
      myEditor.Editor.EndUpdate;
      p1:= TreeView.Selected.AbsoluteIndex;
      p2:= TreeView.FindNextToSelect.AbsoluteIndex;
      p3:= TreeView.TopItem.AbsoluteIndex;
      UpdateTreeView;
      if (0 <= p3) and (p3 < TreeView.Items.Count) then
        TreeView.TopItem:= TreeView.Items[p3];
      p1:= min(p1, p2);
      if (0 <= p1) and (p1 < TreeView.Items.Count) then
        TreeView.Selected:= TreeView.Items[p1];
      BAttributeApply.Enabled:= false;
    end;
  end;
end;

procedure TFClassEditor.BMethodDeleteClick(Sender: TObject);
  var Node: TTreeNode; Method: TOperation; SL: TStringList;
      p: integer; s: string; hasSourceCode: boolean;
begin
  Node:= TreeView.Selected;
  if assigned(Node) and IsMethodsNodeLeaf(Node) then begin
    Method:= GetMethod(Node);
    if assigned(Method) then begin
      hasSourceCode:= false;
      if Method.HasSourceCode then begin
        SL:= TStringList.Create;
        try
          SL.Text:= myEditor.getSource(Method.LineS, Method.LineE-2);
          for p:= SL.Count - 1 downto 0 do begin
            s:= trim(SL.Strings[p]);
            if (Pos('return', s) = 1) or (s = '') or (s = _(LNGTODO)) then
              SL.Delete(p);
          end;
          hasSourceCode:= (SL.Count > 0);
        finally
          SL.Free;
        end;
      end;
      if hasSourceCode and
         not (MessageDlg(Format('Method %s contains sourcecode.', [Method.Name]) +
                         #13 + _('Delete method with sourcecode?'),
                         mtConfirmation, mbYesNoCancel, 0) = mrYes)
         then exit;
      myEditor.DeleteMethod(Method);
      p:= Node.AbsoluteIndex;
      UpdateTreeView;
      if p = TreeView.Items.Count then
        dec(p);
      TreeView.Selected:= TreeView.Items[p];
    end;
    BMethodApply.Enabled:= false;
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

function TFClassEditor.CreateMethod(const getset: string; const Attribute: TAttribute): string;
  var s, aName, attname, bigName, Indent1, Indent2, datatype, param: string;
begin
  Indent1:= GetIndent(Attribute.ScopeDepth);
  Indent2:= GetIndent(Attribute.ScopeDepth + 1);

  datatype:= Attribute.TypeClassifier.GetShortType;
  attname:= Attribute.Name;
  aName:= Attribute.Name;
  param:= aName;

  if FConfiguration.AttributesAParametersP and (copy(param, 1, 1) = 'a') then begin
    param[1]:= 'p';
    delete(aName, 1, 1);
  end;
  if FConfiguration.SetterWithoutThis
    then param:= param + _('New')
    else attname:= 'this.' + attname;

  bigName:= Uppercase(Copy(aName, 1, 1)) + Copy(aName, 2, length(aName));
  if getset = _(LNGGet)
    then s:= Indent1 + 'public ' + Attribute.TypeClassifier.GetShortType + ' ' + _(LNGGet) + bigName + '() {' + CrLf +
             Indent2 + 'return ' + Attribute.Name + ';' + CrLf +
             Indent1 + '}' + CrLf + CrLf
    else s:= Indent1 + 'public void ' + _(LNGSet) + bigName + '(' + datatype + ' ' + param + ') {' + CrLf +
             Indent2 + attname + ' = ' + param + ';' + CrLf +
             Indent1 + '}' + CrLf + CrLf;
  result:= s;
end;

function TFClassEditor.Typ2Value(const typ: string): string;
  const typs: array[1..9] of string = ('int', 'double', 'float', 'char', 'boolean',
                                       'String', 'long', 'byte', 'short');
  const vals: array[1..9] of string = ('0', '0', '0', '''\0''', 'false', '""', '0',
                                       '0', '0');
  var i: integer;
begin
  Result:= 'null';
  for i:= 1 to 9 do
    if typs[i] = typ then begin
      Result:= vals[i];
      break;
    end;
end;

function TFClassEditor.makeConstructor(Method: TOperation; Source: string): string;
  var s, s1, s2, s3, p1, typ, aName, val, Indent1, Indent2: string;
      it, it2: IModelIterator;
      Parameter: TParameter;
      found: boolean;
      p: integer; Node: TTreeNode;
      SuperClass: TClass;
      Operation: TOperation;
      SL: TStringList;

  procedure deleteSource(const s: string);
  begin
    p:= Pos(s, Source);
    if p > 0 then
      insert('DE_LE_TE', Source, p);
  end;

  function getSuperClass: TClass;
    var Ci: IModelIterator;
  begin
    Result:= nil;
    var FullClassname:= GetClassName(GetClassNode);
    if assigned(myUMLForm)
      then Ci:= myUMLForm.MainModul.Model.ModelRoot.GetAllClassifiers
      else Ci:= myEditor.Model.ModelRoot.GetAllClassifiers;
    while Ci.HasNext do begin
      var cent:= TClassifier(Ci.Next);
      if (cent.name = FullClassname) and (cent is TClass) then
        Exit((cent as TClass).Ancestor);
    end;
  end;

begin
  Indent1:= GetIndent(Method.ScopeDepth);
  Indent2:= GetIndent(Method.ScopeDepth + 1);
  Parameter:= nil;
  Node:= getAttributeNode.getFirstChild;
  s:= '';
  s1:= Indent1 + Method.toJava + ' {' + CrLf;
  it:= Method.GetParameters;
  while it.HasNext  do begin
    Parameter:= it.next as TParameter;
    Parameter.UsedForAttribute:= false;
  end;
  while assigned(Node) do begin // iterate over all attributes
    it.Reset;
    found:= false;
    while it.HasNext and not found do begin  // exists a corresponding parameter?
      Parameter:= it.next as TParameter;
      if assigned(Parameter.TypeClassifier) then begin
        p1:= Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType;
        if Pos(p1, Node.Text) = 1 then found:= true;
      end;
    end;
    if found then begin // corresponding parameter exists or
      s:= s + Indent2 + 'this.' + Parameter.Name + ' = ' + Parameter.Name + ';' + CrLf;
      deleteSource('this.' + Parameter.Name);
      Parameter.UsedForAttribute:= true;
    end else
    if (Pos('=', Node.Text) > 0) then begin
      s3:= copy(Node.Text, 1, Pos(':', Node.Text)-1);
      deleteSource('this.' + s3);
    end else begin
      p:= Pos(':', Node.Text);
      aName:= copy(Node.Text, 1, p-1);
      typ:= copy(Node.Text, p+2, 255);
      val:= typ2value(typ);
      if val = '' then val:= 'null';

      p:= Pos('this.' + aName + ' = ' + aName, Source);
      if p > 0 then begin
        DeleteSource('this.' + aName);
        s:= s + Indent2 + 'this.' + aName + ' = ' + val + ';' + CrLf;
      end else begin
        p:= Pos('this.' + aName, Source);
        if p = 0 then
          s:= s + Indent2 + 'this.' + aName + ' = ' + val + ';' + CrLf;
      end;
    end;
    Node:= Node.getNextSibling;
  end;

  SuperClass:= GetSuperClass;
  found:= false;
  if assigned(SuperClass) then begin
    SL:= TStringList.Create;
    try
      it.Reset;
      while it.HasNext  do begin
        Parameter:= it.next as TParameter;
        if not Parameter.UsedForAttribute and assigned(Parameter.TypeClassifier) then
          SL.Add(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType);
      end;

      It:= SuperClass.GetOperations;
      while It.HasNext do begin
        Operation:= It.Next as TOperation;
        if Operation.OperationType = otConstructor then begin
          s2:= 'super(';
          found:= true;
          it2:= Operation.GetParameters;
          while it2.HasNext do begin
            Parameter:= it2.next as TParameter;
            if assigned(Parameter.TypeClassifier) and
               (SL.IndexOf(Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType) >= 0)
            then s2:= s2 + Parameter.Name + ', '
            else found:= false;
          end;
          if found then begin
            if s2.EndsWith(', ') then
              s2:= UUtils.Left(s2, -2);
            s:= s1 + Indent2 + s2 + ');' + CrLf + s;
            break;
          end;
        end;
      end;
      if not found then
        s:= s1 + s;
    finally
      FreeAndNil(SL);
    end
  end else
    s:= s1 + s;

  while Source <> '' do begin
    p:= Pos(#13#10, Source);
    s1:= copy(Source, 1, p+1);
    delete(Source, 1, p+1);
    if (Pos('DE_LE_TE', s1) = 0) and (Pos(s1, s) = 0) and (Pos(' return ', s1) = 0) then
      s:= s + s1;
  end;
  s:= s + Indent1 + '}' + CrLf + CrLf;
  result:= s;
end;

function TFClassEditor.MakeIdentifier(var s: string; typ: boolean): integer;
  var i, i1, i2: integer; s1, s2: string; Chars: string;
begin
  s2:= s;
  Result:= 0;
  // delete illegal chars at the beginning
  while (length(s) > 0) and not (s[1].isLetter or (s[1] = '_'))
  do begin
    delete(s, 1, 1);
    Result:= 1;
  end;
  
  // delete illegal chars after the beginning
  i:= 2;
  if Typ
    then Chars:= '_.[]<>'
    else Chars:= '_';

  while i <= length(s) do
    if not (s[i].isLetterOrDigit or (Pos(s[i], Chars) > 0)) then begin
      delete(s, i, 1);
      Result:= i;
    end else
      inc(i);
  
  // check for array
  s1:= s;
  i:= Pos('[]', s1);
  while i > 0 do begin
    delete(s1, i, 2);
    i:= Pos('[]', s1);
  end;
  i1:= Pos('[', s1); i2:= Pos(']', s1);
  if (i1 > 0) and ((i2 = 0) or (i2 < i1)) or ((i1 = 0) and (i2 > 0)) then Result:= -1;

  // check for < > 
  i1:= Pos('<', s); i2:= Pos('>', s);
  if (i1 > 0) and ((i2 = 0) or (i2 < i1)) or ((i1 = 0) and (i2 > 0)) then Result:= -1;
  if i2 > i1 then begin
    s1:= trim(copy(s, i1+1, i2-i1-1));
    if s1 = '' then Result:= -1;
  end;
  if (s2 = 'final') or (s2 = 'static') or (s2 = 'public') or (s2 = 'private') then Result:= -1;
end;

function TFClassEditor.MakeIdentifier(CB: TComboBox; typ: boolean): boolean;
  var errpos, len: integer; s: string;
     OnChange: TNotifyEvent;
begin
  Result:= true;
  s:= CB.Text;
  errpos:= MakeIdentifier(s, typ);
  if errpos = 0 then exit;
  if errpos = -1 then begin
    Result:= false;
    exit;
  end;
  len:= length(CB.text) - length(s);
  OnChange:= CB.OnChange;
  CB.OnChange:= nil;
  CB.Text:= s;
  CB.OnChange:= OnChange;
  if len = 1
    then CB.SelStart:= errpos - 1
    else CB.SelStart:= length(s);
end;

function TFClassEditor.MakeIdentifier(E: TEdit; typ: boolean): boolean;
  var errpos, errlen: integer; s: string;
     OnChange: TNotifyEvent;
begin
  Result:= true;
  s:= E.Text;
  errpos:= MakeIdentifier(s, typ);
  if errpos = 0 then exit;
  Result:= false;
  if errpos = -1 then exit;
  errlen:= length(E.text) - length(s);
  OnChange:= E.OnChange;
  E.OnChange:= nil;
  E.Text:= s;
  E.OnChange:= OnChange;
  if errlen = 1
    then E.SelStart:= errpos - 1
    else E.SelStart:= length(s);
end;

procedure TFClassEditor.ChangeAttribute(var A: TAttribute);
begin
  A.Name:= EAttributeName.Text;
  A.TypeClassifier:= MakeType(CBAttributeType);
  A.Value:= EAttributeValue.Text;
  if IsClass
    then A.Visibility:= TVisibility(RGAttributeAccess.ItemIndex)
  else if RGAttributeAccess.ItemIndex = 0
    then A.Visibility:= viPackage
    else A.Visibility:= viPublic;
  A.Static:= CBAttributeStatic.Checked;
  A.IsFinal:= CBAttributeFinal.Checked;
end;

function TFClassEditor.MakeAttribute: TAttribute;
begin
  Result:= TAttribute.Create(nil);
  ChangeAttribute(Result);
end;

function TFClassEditor.MakeType(CB: TComboBox): TClassifier;
begin
  var s:= CB.Text;
  var i:= CB.Items.IndexOf(s);
  if i <> -1 then
    s:= CB.Items[i];
  if s = ''
    then Result:= nil
    else Result:= MakeType(s);
end;

function TFClassEditor.MakeType(const s: string): TClassifier;
  var MyUnit: TUnitPackage; aClass: TClass;
begin
  Result:= nil;
  MyUnit := myEditor.Model.ModelRoot.FindUnitPackage('Default');
  if Assigned(myUnit) then
    Result := MyUnit.FindClassifier(s, nil, true);
  if Result = nil then begin
    aClass:= TClass.Create(nil);
    aClass.Name:= s;
    if assigned(MyUnit) then
      MyUnit.AddClass(aClass);
    Result:= aClass;
  end;
end;

procedure TFClassEditor.ChangeMethod(var M: TOperation);
  var i, p: integer; aName, Typ, s: string;
begin
  M.Name:= CBMethodName.Text;
  M.ReturnValue:= MakeType(CBMethodType);
  if IsClass
    then M.OperationType:= TOperationType(RGMethodKind.ItemIndex)
  else if RGMethodKind.ItemIndex = 0
    then M.OperationType:= otProcedure
    else M.OperationType:= otFunction;
  if IsClass then
    M.Visibility := TVisibility(RGMethodAccess.ItemIndex)
  else if RGMethodAccess.ItemIndex = 0
    then M.Visibility:= viPackage
    else M.Visibility:= viPublic;
  M.Static:= CBMethodStatic.Checked;
  M.IsAbstract:= CBMethodAbstract.Checked and IsClass;
  M.NewParameters;
  for i:= 0 to LBParams.Items.Count - 1 do begin
    s:= LBParams.Items[i];
    p:= Pos(':', s);
    aName:= copy(s, 1, p-1);
    typ:= copy(s, p+2, length(s));
    M.AddParameter(aName).TypeClassifier:= MakeType(typ);
  end;
  if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
    M.AddParameter(CBParamName.Text).TypeClassifier:= MakeType(CBParamType);
end;

procedure TFClassEditor.SetClassOrInterface(aIsClass: boolean);

  procedure setRadioGroup(RG: TRadioGroup; text: string);
  begin
    if RG.Items.Text <> text then begin
      var index:= RG.ItemIndex;
      RG.Items.Text:= text;
      RG.ItemIndex:= index;
    end;
  end;

begin
  if aIsClass then begin
    TSClass.Caption:= '&' + _(LNGClass);
    LClass.Caption:= _(LNGClass);
    BInterface.Caption:= _(LNGInterface);
    setRadioGroup(RGAttributeAccess, RGMethodAccessValues);
    setRadioGroup(RGMethodAccess, RGMethodAccessValues);
    setRadiogroup(RGMethodKind, RGMethodKindValues);
    LImplements.Visible:= true;
    EImplements.Visible:= true;
    CBClassAbstract.Visible:= true;
    CBMethodAbstract.Visible:= true;
    CBGetMethod.Visible:= true;
    CBSetMethod.Visible:= true;
    RGAttributeAccess.Height:= PPIScale(115);
    GBAttributeOptions.Height:= PPIScale(115);
    RGMethodKind.Height:= PPIScale(105);
    RGMethodAccess.Height:= PPIScale(105);
    GBMethodOptions.Visible:= true;
  end else begin
    TSClass.Caption:= '&' + _(LNGInterface);
    LClass.Caption:= _(LNGInterface);
    BInterface.Caption:= _(LNGClass);
    setRadioGroup(RGAttributeAccess, RGMethodAccessValues);
    RGAttributeAccess.Items.Delete(0);
    RGAttributeAccess.Items.Delete(1);
    setRadioGroup(RGMethodAccess, RGAttributeAccess.Items.Text);
    setRadioGroup(RGMethodKind, RGMethodKindValues);
    RGMethodKind.Items.Delete(0);
    LImplements.Visible:= false;
    EImplements.Visible:= false;
    CBClassAbstract.Visible:= false;
    CBMethodAbstract.Visible:= false;
    CBGetMethod.Visible:= false;
    CBSetMethod.Visible:= false;
    RGAttributeAccess.Height:= PPIScale(65);
    GBAttributeOptions.Height:= PPIScale(65);
    RGMethodKind.Height:= PPIScale(65);
    RGMethodAccess.Height:= PPIScale(65);
    GBMethodOptions.Visible:= false;
  end;
  CBMethodStatic.Enabled:= aIsClass;
end;

procedure TFClassEditor.UpdateTreeView(HasChanged: boolean = true; NodeIndex: integer = -1);
  var Ci, it: IModelIterator;
      cent: TClassifier;
      Attribute: TAttribute;
      Method: TOperation;
      IsFirstClass: boolean;
begin
  if TreeViewUpdating then exit;
  TreeViewUpdating:= true;
  TreeView.OnChange:= nil;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  myEditor.ParseSourceCode(hasChanged);
  Ci:= myEditor.Model.ModelRoot.GetAllClassifiers;
  IsFirstClass:= true;
  while Ci.HasNext do begin
    cent:= TClassifier(Ci.Next);
    if (cent.pathname <> myeditor.Pathname) or Cent.anonym
      then continue;
    if IsFirstClass then begin
      EClass.Text:= getShortType(cent.ShortName);
      IsFirstClass:= false;
    end;
    TVClassOrInterface(cent);
    It:= cent.GetAttributes;
    while It.HasNext do begin
      Attribute:= It.Next as TAttribute;
      TVAttribute(Attribute);
    end;
    It:= cent.GetOperations;
    while It.HasNext do begin
      Method:= It.Next as TOperation;
      TVMethod(Method);
    end;
  end;
  TreeView.FullExpand;
  TreeView.Items.EndUpdate;
  if NodeIndex >= 0 then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  TreeView.OnChange:= TreeViewChange;
  TreeViewUpdating:= false;
end;

function TFClassEditor.CreateTreeView(Editor: TFEditForm; UMLForm: TFUMLForm): boolean;
begin
  myEditor:= Editor;
  myEditor.EnsureStartEnd(myEditor.CountClassOrInterface);
  myUMLForm:= UMLForm;
  UpdateTreeView;
  if TreeView.Items.Count > 0 then begin
    TreeView.Selected:= AttributeNode;
    TreeView.TopItem:= TreeView.Items.GetFirstNode;
  end;
  Editor.Invalidate;
  Result:= true;
end;

procedure TFClassEditor.ActionListUpdate(aAction: TBasicAction; var Handled: Boolean);
  var vis: boolean;
begin
  if aAction is TAction then begin
    if assigned(myEditor)
      then vis:= not myEditor.Editor.ReadOnly
      else vis:= true;
    (aAction as TAction).Enabled:= vis;
  end;
  Handled:= true;
end;

procedure TFClassEditor.BParameterNewClick(Sender: TObject);
begin
  if (CBParamName.Text = '') or (CBParamType.Text = '') then exit;
  LBParams.Items.Add(CBParamName.Text + ': ' + CBParamType.Text );
  if CBParamName.Items.IndexOf(CBParamName.Text) = -1 then
     CBParamName.Items.AddObject(CBParamName.Text,
                                 TParamTyp.create(CBParamType.Text));
  if FConfiguration.AttributesAParametersP
    then CBParamName.Text:= 'p'
    else CBParamName.Text:= '';
  CBParamType.Text:= '';
  CBParamType.ItemIndex:= -1;
  BMethodChangeClick(Self);
  if CBParamName.canFocus then CBParamName.SetFocus;
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
    ComboBoxInvalid:= false;
    CBParamNameSelect(Self);
    if CBParamName.canFocus then CBParamType.SetFocus;
  end;
end;

procedure TFClassEditor.CBParamNameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
    BMethodApply.Enabled:= true;
  ShowMandatoryFields;
end;

procedure TFClassEditor.CBParamNameSelect(Sender: TObject);
  var s: string; p: integer;
begin
  p:= CBParamName.ItemIndex;
  if (p = -1) or (CBParamName.Text <> CBParamName.Items[p])
    then s:= CBParamName.Text
    else s:= CBParamName.Items[p];
  if not ComboBoxInvalid then begin
    if (-1 < p) and assigned(CBParamName.Items.Objects[p]) then
      CBParamType.Text:= TParamTyp(CBParamName.Items.Objects[p]).typ;
    CBParamName.Text:= s;
    if (CBParamName.Text <> '') and (CBParamType.Text <> '') then begin
      SBRightClick(Self);
      BMethodChangeClick(Self);
    end;
  end;
end;

procedure TFClassEditor.AllAttributesAsParameters(var Node: TTreeNode);
  var s, aClassname: string;
      Ci, it: IModelIterator;
      cent: TClassifier;
      Attribute: TAttribute;
      Method: TOperation;
      Parameter: TParameter;
      p: integer;

  procedure MakeParameterFromAttributes(Cent: TClassifier);
  begin
    var it:= cent.GetAttributes;
    while it.HasNext do begin
      Attribute:= It.Next as TAttribute;
      s:= Attribute.toNameType;
      if FConfiguration.AttributesAParametersP and (copy(s, 1, 1) = 'a') then
        s[1]:= 'p';
      if CBParameter.Items.IndexOf(s) = -1 then
        CBParameter.Items.Add(s);
    end;
  end;

begin
  CBParameter.Clear;
  aClassname:= GetClassName(GetClassNode);
  cent:= GetClassifier(GetClassNode);
  if assigned(cent) then begin
    MakeParameterFromAttributes(Cent);
    while (Cent is TClass) and assigned((Cent as TClass).Ancestor) do begin
      Cent:= (Cent as TClass).Ancestor;
      MakeParameterFromAttributes(Cent);
    end;
  end;

  // look too in model of uml-diagram for ancestors
  if assigned(myUMLForm) and assigned(MyUMLForm.MainModul) and
     assigned(myUMLForm.MainModul.Model) and assigned(myUMLForm.MainModul.Model.ModelRoot)
  then begin
    cent:= nil;
    Ci:= myUMLForm.MainModul.Model.ModelRoot.GetAllClassifiers;
    while Ci.hasNext do begin
      cent:= TClassifier(Ci.Next);
      if cent.name = aClassname then
        break;
    end;
    if assigned(cent) and (cent.Name = aClassname) then
      while (Cent is TClass) and assigned((Cent as TClass).Ancestor) do begin
        Cent:= (Cent as TClass).Ancestor;
        MakeParameterFromAttributes(Cent);
      end;
  end;

  if assigned(Node) and IsMethodsNodeLeaf(Node) then begin
    Method:= GetMethod(Node);
    if assigned(Method) then begin
      it:= Method.GetParameters;
      while it.HasNext  do begin
        Parameter:= it.next as TParameter;
        s:= Parameter.Name + ': ' + Parameter.TypeClassifier.GetShortType;
        p:= CBParameter.Items.IndexOf(s);
        if p > -1 then CBParameter.Items.Delete(p);
      end;
    end;
  end;
  CBParameter.Text:= _(LNGSelectAttributes);
end;

procedure TFClassEditor.ComboBoxCloseUp(Sender: TObject);
begin
  ComboBoxInvalid:= false;
end;

procedure TFClassEditor.CBAttributeTypeDropDown(Sender: TObject);
begin
  SendMessage(CBAttributeType.Handle, WM_SETCURSOR, 0, 0);
end;

procedure TFClassEditor.CBComboBoxEnter(Sender: TObject);
  var CB: TComboBox; R: TRect; LeftPart: boolean;
begin
  if Sender is TComboBox then begin
    CB:= Sender as TComboBox;
    R:= CB.ClientRect;
    R.Width:= R.Width - 20;
    R:= CB.ClientToScreen(R);
    LeftPart:= R.Contains(Mouse.CursorPos);
    if LeftPart and (CB.Items.Count > 0) then
      SendMessage(CB.Handle, CB_SHOWDROPDOWN, 1, 0);
  end;
end;

procedure TFClassEditor.CBAttributeTypeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ComboBoxInvalid:= false;
    CBAttributeTypeSelect(Self);
    if EAttributeValue.canFocus then EAttributeValue.SetFocus;
  end;
end;

procedure TFClassEditor.CBAttributeTypeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBAttributeType.Text <> '') and (Key <> VK_Return) then
    BAttributeApply.Enabled:= true
end;

procedure TFClassEditor.CBAttributeTypeSelect(Sender: TObject);
  var s: string; p: integer;
begin
  p:= CBAttributeType.ItemIndex;
  if (p = -1) or (CBAttributeType.Text <> CBAttributeType.Items[p])
    then s:= CBAttributeType.Text
    else s:= CBAttributeType.Items[p];
  if not ComboBoxInvalid then begin
    CBAttributeType.Text:= s;
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
  BClassApply.Enabled:= true;
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
    BAttributeApply.Enabled:= true;
    if Key = VK_Return then begin
      BAttributeChangeClick(Self);
    end;
  end;
end;

procedure TFClassEditor.EAttributeValueKeyPress(Sender: TObject; var Key: Char);
begin
  BAttributeApply.Enabled:= true;
  if (Key = #13) and NameTypeValueChanged then
    BAttributeChangeClick(Self);
  if (Key = #13) and BAttributeNew.CanFocus then
    BAttributeNew.SetFocus;
end;

procedure TFClassEditor.EClassKeyPress(Sender: TObject; var Key: Char);
begin
  BClassApply.Enabled:= true;
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
     (IsClass and (RGMethodKind.ItemIndex = 1) or
      not IsClass and (RGMethodKind.ItemIndex = 0)) then
    BMethodApply.Enabled:= true
end;

procedure TFClassEditor.CBMethodnameSelect(Sender: TObject);
  var s: string; p: integer;
begin
  p:= CBMethodName.ItemIndex;
  if (p = -1) or (CBMethodName.Text <> CBMethodName.Items[p])
    then s:= CBMethodName.Text
    else s:= CBMethodName.Items[p];
  if not ComboBoxInvalid then begin
    CBMethodName.Text:= s;
    if CbMethodName.Items.IndexOf(s) = -11 then
      CBMethodName.Items.Add(s);
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
    ComboBoxInvalid:= false;
    CBMethodTypeSelect(Self);
    if CBParamName.CanFocus then
      CBParamName.SetFocus;
  end;
end;

procedure TFClassEditor.CBMethodParamTypeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (CBMethodType.Text <> '') and (Key <> VK_Return) then
    BMethodApply.Enabled:= true
end;

procedure TFClassEditor.CBMethodTypeSelect(Sender: TObject);
  var s: string; p: integer;
begin
  RGMethodKind.ItemIndex:= 2;
  p:= CBMethodType.ItemIndex;
  if (p = -1) or (CBMethodType.Text <> CBMethodType.Items[p])
    then s:= CBMethodType.Text
    else s:= CBMethodType.Items[p];
  BMethodApply.Enabled:= true;

  if not ComboBoxInvalid then begin
    CBMethodType.Text:= s;
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
  ComboBoxInvalid:= true;
end;

procedure TFClassEditor.CBParameterSelect(Sender: TObject);
begin
  var s:= CBParameter.Items[CBParameter.ItemIndex];
  if (s <> _(LNGSelectAttributes)) and (LBParams.Items.IndexOf(s) = -1) then begin
    LBParams.Items.Add(s);
    BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.CBParamTypeDropDown(Sender: TObject);
begin
  SendMessage(CBParamType.Handle, WM_SETCURSOR, 0, 0)
end;

procedure TFClassEditor.CBParamTypeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ComboBoxInvalid:= false;
    CBParamTypeSelect(Self);
    if BParameterNew.CanFocus then
      BParameterNew.SetFocus;
  end;
end;

procedure TFClassEditor.CBParameterKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    ComboBoxInvalid:= false;
    CBParameterSelect(Self);
  end;
end;

procedure TFClassEditor.CBParamTypeSelect(Sender: TObject);
  var s: string; p: integer;
begin
  p:= CBParamType.ItemIndex;
  if (p = -1) or (CBParamType.Text <> CBParamType.Items[p])
    then s:= CBParamType.Text
    else s:= CBParamType.Items[p];
  if not ComboBoxInvalid then begin
    CBParamType.Text:= s;
    if (CBParamName.Text <> '') and (CBParamType.Text <> '') then
      BMethodChangeClick(Self);
  end;
end;

procedure TFClassEditor.RGMethodKindChange;
begin
  if IsClass and (RGMethodKind.ItemIndex = 0) then begin
    CBMethodName.Text:= withoutGeneric(EClass.Text);
    if CBMethodName.Items.IndexOf(CBMethodName.Text) = -1 then
      CBMethodName.Items.Add(CBMethodName.Text);
  end;
  CBMethodName.Enabled:= IsClass and (RGMethodKind.ItemIndex > 0) or not IsClass;
  CBMethodType.Enabled:= IsClass and (RGMethodKind.ItemIndex in [1, 2]) or
                         not IsClass and (RGMethodKind.ItemIndex = 1);
  ShowMandatoryFields;
end;

procedure TFClassEditor.ShowMandatoryFields;
begin
  case PageControl.ActivePageIndex of
    1: begin
         if EAttributeName.Text = ''
           then EAttributeName.Color:= clInfoBK
           else EAttributeName.Color:= clWindow;
         if CBAttributeType.Text = ''
           then CBAttributeType.Color:= clInfoBK
           else CBAttributeType.Color:= clWindow;
         if IsClass then
           EAttributeValue.Color:= clWindow
         else if EAttributeValue.Text = ''
           then EAttributeValue.Color:= clInfoBK
           else EAttributeValue.Color:= clWindow;
       end;
    2: begin
         if CBMethodName.Enabled and (CBMethodName.Text = '')
           then CBMethodName.Color:= clInfoBK
           else CBMethodName.Color:= clWindow;
         if CBMethodType.Enabled and (CBMethodType.Text = '')
             and (IsClass and (RGMethodKind.ItemIndex = 2) or
                  not IsClass and (RGMethodKind.ItemIndex = 1))
           then CBMethodType.Color:= clInfoBK
           else CBMethodType.Color:= clWindow;
         if (CBParamName.Text <> '') and (CBParamType.Text = '')
           then CBParamType.Color:= clInfoBK
           else CBParamType.Color:= clWindow;
         if (CBParamName.Text = '') and (CBParamType.Text <> '')
           then CBParamName.Color:= clInfoBK
           else CBParamName.Color:= clWindow;
       end;
    end;
end;

function TFClassEditor.MethodToJava(Method: TOperation; Source: string): string;
  var s, s1, s2, s3, Indent1, Indent2: string; p1: integer; SL: TStringList;

  procedure delparam(const param: string);
  begin
    var i:= 0;
    while i < SL.Count do
      if Pos(param, SL.Strings[i]) > 0
        then SL.Delete(i)
        else inc(i);
  end;

  procedure AdjustReturnValue;
    const ReturnValues: array[1..5] of string = (' return 0;', ' return "";',
       ' return false;', ' return ''\0'';', ' return null;');
    var i, p: integer;
  begin
    for i:= 1 to 5 do begin
      p:= Pos(ReturnValues[i], Source);
      if p > 0 then begin
        Delete(Source, p, length(ReturnValues[i]));
        Insert(s2, Source, p);
        break;
      end;
    end;
  end;

begin
  Indent1:= GetIndent(Method.ScopeDepth);
  Indent2:= GetIndent(Method.ScopeDepth + 1);
  s:= Indent1 + Method.toJava;
  if CBMethodAbstract.Checked or not IsClass then
    s:= s + ';' + CrLf + CrLf
  else begin
    s:= s + ' {' + CrLf;
    s1:= Indent1 + '}' + CrLf + CrLf;
    case Method.OperationType of
     otConstructor: s:= makeConstructor(Method, Source);
     otFunction:    begin
                      s2:= ' return ' + Typ2Value(Method.ReturnValue.GetShortType) + ';';
                      if Pos(' return ', Source) > 0 then begin
                        AdjustReturnValue;
                        s:= s + Source + s1;
                      end else
                        if Source <> '' then
                          s:= s + Source + Indent2 + s2 + CrLf + s1
                        else
                          s:= s + Indent2 + _(LNGTODO) + CrLf +
                                  Indent2 + s2 + CrLf + s1
                    end;
     otProcedure:   begin
                      if Source <> '' then begin
                        while Source <> '' do begin
                          p1:= Pos(CrLf, Source);
                          s3:= copy(Source, 1, p1+1);
                          delete(Source, 1, p1+1);
                          if Pos(' return ', s3) = 0 then
                            s:= s + s3;
                        end;
                        s:= s + Source + s1
                      end else
                        s:= s + Indent2 + _(LNGTODO) + CrLf +
                                Indent2 + CrLf + s1;
                    end;
    end;
  end;
  LBParams.Items.Clear;
  if not Method.hasComment then begin
    SL:= TStringList.Create;
    SL.Text:= FConfiguration.MethodComment;
    if not (Method.OperationType = otFunction) then
      delParam('@return');
    Method.hasComment:= true;
    Result:=  SL.Text + s;
    FreeAndNil(SL);    
  end else
    Result:= s;
end;

procedure TFClassEditor.BMethodChangeClick(Sender: TObject);
  var New, Source: string; Method: TOperation;
      ii, ClassNumber, NodeIndex, TopItemIndex: integer;
      Node: TTreeNode;
begin
  Node:= TreeView.Selected;
  CBMethodType.Enabled:= (RGMethodKind.ItemIndex = 2) or
                         (not IsClass and (RGMethodKind.ItemIndex = 1));
  if not (MakeIdentifier(CBMethodName, false) and MakeIdentifier(CBMethodType, true) and
          MakeIdentifier(CBParamName, false) and MakeIdentifier(CBParamType, true)) or (Node = nil)
    then exit;
  RGMethodKindChange;
  if IsClass then ii:= 2 else ii:= 1;
  if (CBMethodName.Text = '') or
     ((RGMethodKind.ItemIndex = ii) and (CBMethodType.Text = '')) or
     ((CBParamName.Text <> '') and (CBParamType.Text = ''))
     then exit;
  ClassNumber:= GetClassNumber(Node);
  NodeIndex:= Node.AbsoluteIndex;
  TopItemIndex:= TreeView.TopItem.AbsoluteIndex;

  myEditor.Editor.BeginUpdate;
  if IsMethodsNode(Node) then begin
    if (RGMethodKind.ItemIndex = 0) and IsClass then begin
      inc(NodeIndex);
      Node:= Node.getNext;
      while Assigned(Node) and (Node.ImageIndex = 6) do begin
        inc(NodeIndex);
        Node:= Node.getNext;
      end;
    end else
      NodeIndex:= NodeIndex + Node.Count + 1;
    Method:= MakeMethod;
    Method.ScopeDepth:= GetLevel(Node) + 1;
    if not MethodAlreadyExists(Method.toNameParameterTyp) then begin
      New:= MethodToJava(Method, '');
      if IsClass and (RGMethodKind.ItemIndex = 0)
        then myEditor.InsertConstructor(ClassNumber, New)
        else myEditor.InsertProcedure(ClassNumber, New);
    end else if (Sender = BMethodApply) then
      ErrorMsg(Format(_(LNGAlreadyExists), [Method.toNameParameterTyp]));
    FreeAndNil(Method);
  end else begin
    Method:= GetMethod(Node);
    if Assigned(Method) then begin
      ChangeMethod(Method);
      if Method.HasSourceCode
        then Source:= myEditor.getSource(Method.LineS, Method.LineE-2)
        else Source:= '';
      if Method.OperationType = otConstructor
        then New:= makeConstructor(Method, Source)
        else New:= MethodToJava(Method, Source);
      myEditor.ReplaceMethod(Method, New);
    end;
  end;
  myEditor.Editor.EndUpdate;
  UpdateTreeView;
  if (0 <= TopItemIndex) and (TopItemIndex < TreeView.Items.Count) then
    TreeView.TopItem:= TreeView.Items[TopItemIndex];
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
  BMethodApply.enabled:= false;
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
  var i: integer;
begin
  i:= LBParams.ItemIndex;
  LBParams.DeleteSelected;
  BMethodChangeClick(Self);
  if i = LBParams.Count
    then dec(i);
  if i >= 0 then
    LBParams.Selected[i]:= true;
end;

procedure TFClassEditor.SBDownClick(Sender: TObject);
begin
  var i:= LBParams.ItemIndex;
  if (0 <= i) and (i < LBParams.Count-1) then begin
    LBParams.Items.Exchange(i, i+1);
    BMethodChangeClick(Self);
    LBParams.Selected[i+1]:= true;
  end;
end;

procedure TFClassEditor.SBUpClick(Sender: TObject);
begin
  var i:= LBParams.ItemIndex;
  if (0 < i) and (i < LBParams.Count) then begin
    LBParams.Items.Exchange(i, i-1);
    BMethodChangeClick(Self);
    LBParams.Selected[i-1]:= true;
  end;
end;

procedure TFClassEditor.SBLeftClick(Sender: TObject);
  var s: string; p: integer;
begin
  if (CBParamName.Text = '') and (CBParamType.Text = '') and
     (LBParams.ItemIndex < LBParams.Items.Count) and
     (0 <= LBParams.ItemIndex)
  then begin
    s:= LBParams.Items[LBParams.ItemIndex];
    p:= Pos(': ', s);
    CBParamName.Text:= copy(s, 1, p-1);
    CBParamType.Text:= copy(s, p+2, length(s));
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

procedure TFClassEditor.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  TreeView.OnChange:= nil;
  TreeView.Items.Clear;
  for var i:= 0 to CBParamName.Items.Count - 1 do begin
    var aObject:= CBParamName.Items.Objects[i];
    FreeAndNil(aObject);
  end;
end;

procedure TFClassEditor.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  RGMethodAccessValues:= RGMethodAccess.Items.Text;
  RGMethodKindValues:= RGMethodKind.Items.Text;
  TreeView.Images:= FFileStructure.vilFileStructureLight;
  MakeNewClass:= false;
  TreeViewUpdating:= false;
  //SetBitmaps;
end;

procedure TFClassEditor.FormShow(Sender: TObject);
  var i: integer;  st: string;
begin
  Init(FConfiguration.ReadBoolU('UML', 'DefaultModifiers', true));
  var SL:= TStringList.Create;
  for i:= 0 to FJava.TDIFormsList.Count - 1 do
    FJava.TDIFormsList[i].CollectClasses(SL);
  for i:= 0 to SL.Count - 1 do begin
    st:= GetShortType(SL[i]);
    ComboBoxInsert2(CBAttributeType, st);
    ComboBoxInsert2(CBMethodType, st);
    ComboBoxInsert2(CBParamType, st);
  end;
  ComboBoxInsert2(CBMethodType, 'void');
  FreeAndNil(SL);
end;

procedure TFClassEditor.Init(Default: boolean);
begin
  EnableEvents(false);
  CBAttributeStatic.Checked:= false;
  CBAttributeFinal.Checked:= false;
  CBMethodStatic.Checked:= false;
  CBMethodAbstract.Checked:= false;
  if Default then begin
    RGAttributeAccess.ItemIndex:= -1;
    RGAttributeAccess.ItemIndex:= 0;
    CBGetMethod.Checked:= true;
    CBSetMethod.Checked:= false;
    RGMethodKind.ItemIndex:= 1;
    RGMethodAccess.ItemIndex:= 3;
  end else begin
    RGAttributeAccess.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGAttributeAccess', 0);
    CBGetMethod.Checked:= FConfiguration.ReadBoolU('ClassEditor', 'CBGetMethod', false);
    CBSetMethod.Checked:= FConfiguration.ReadBoolU('ClassEditor', 'CBSetMethod', false);
    RGMethodKind.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGMethodKind', 1);
    RGMethodAccess.ItemIndex:= FConfiguration.ReadIntegerU('ClassEditor', 'RGMethodAccess', 3);
  end;
  EnableEvents(true);
  ChangeStyle;
end;

procedure TFClassEditor.TreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
  var TargetNode, SourceNode: TTreeNode;
begin
  with TreeView do begin
    TargetNode:= GetNodeAt(X, Y); // Get target node
    SourceNode:= Selected;
    if TargetNode = nil then begin
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
  var from, _to, after, after_to, NodeIndex: integer;
      emptyLines: string;
      SourceModelEntity, TargetModelEntity: TModelEntity;
begin
  if IsClassOrInterface(SourceNode) or IsAttributesNode(SourceNode) or IsMethodsNode(SourceNode) or
     IsClassOrInterface(TargetNode) or myEditor.Editor.ReadOnly  then
    exit;

  if IsAttributesNode(TargetNode) or IsMethodsNode(TargetNode) then
    TargetNode:= TargetNode.getFirstChild;
  if IsAttributesNodeLeaf(SourceNode) and IsAttributesNodeLeaf(TargetNode) then begin
    emptyLines:= '';
    SourceModelEntity:= GetAttribute(SourceNode);
    TargetModelEntity:= GetAttribute(TargetNode);
  end else if IsMethodsNodeLeaf(SourceNode) and IsMethodsNodeLeaf(TargetNode) then begin
    emptyLines:= #13#10;
    SourceModelEntity:= GetMethod(SourceNode);
    TargetModelEntity:= GetMethod(TargetNode);
  end else
    exit;

  if assigned(SourceModelEntity) then begin
    from:= SourceModelEntity.LineS;
    _to:= SourceModelEntity.LineE;
  end else
    exit;

  if assigned(TargetModelEntity) then begin
    after:= TargetModelEntity.LineS;
    after_to:= TargetModelEntity.LineE;
  end else
    exit;

  if IsMethodsNodeLeaf(SourceNode) and  (trim(myEditor.Editor.lines[after_to]) = '') then
    inc(after_to);
  if (from <= after) and (after <= _to) then
    exit;
  myEditor.MoveBlock(from-1, _to-1, after-1, after_to-1, emptyLines);
  NodeIndex:= TargetNode.AbsoluteIndex;
  UpdateTreeView;
  if (0 <= NodeIndex) and (NodeIndex < TreeView.Items.Count) then
    TreeView.Selected:= TreeView.Items[NodeIndex];
end;

procedure TFClassEditor.LBParamsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_Delete) and not myEditor.Editor.ReadOnly then
    LBParams.DeleteSelected;
end;

function TFClassEditor.IsClassOrInterface(Node: TTreeNode): boolean;
begin
  Result:= assigned(Node) and ((Node.ImageIndex = 1) or (Node.ImageIndex = 11));
end;

function TFClassEditor.IsAttributesNodeLeaf(Node: TTreeNode): boolean;
begin
  Result:= assigned(Node) and (Node.Parent.ImageIndex = 12);
end;

function TFClassEditor.IsAttributesNode(Node: TTreeNode): boolean;
begin
  Result:= assigned(Node) and (Node.ImageIndex = 12);
end;

function TFClassEditor.IsMethodsNodeLeaf(Node: TTreeNode): boolean;
begin
  Result:= assigned(Node) and (Node.Parent.ImageIndex = 13);
end;

function TFClassEditor.IsMethodsNode(Node: TTreeNode): boolean;
begin
  Result:= assigned(Node) and (Node.ImageIndex = 13);
end;

function TFClassEditor.AttributeAlreadyExists(const AttributeName: string): boolean;
begin
  var cent:= GetClassifier(GetClassNode);
  if assigned(cent) then begin
    var it:= cent.GetAttributes;
    while it.HasNext do
      if TAttribute(it.Next).Name = AttributeName then
        exit(true);
  end;
  Result:= false;
end;

function TFClassEditor.MethodAlreadyExists(const MethodName: string): boolean;
begin
  var cent:= GetClassifier(GetClassNode);
  if assigned(cent) then begin
    var it:= cent.GetOperations;
    while it.HasNext do
      if TOperation(it.Next).toNameParameterTyp = MethodName then
        exit(true);
  end;
  Result:= false;
end;

procedure TFClassEditor.ChangeStyle;
begin
  if FConfiguration.isDark then begin
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
