unit UObjectInspector;

interface

uses
  Classes, Graphics, Controls, ComCtrls, StdCtrls, ExtCtrls,
  TypInfo, Menus, Forms, ELPropInsp, ELDsgnr, UDockForm, TB2Item, SpTBXItem,
  System.ImageList, Vcl.ImgList, Vcl.VirtualImageList, Vcl.BaseImageCollection,
  SVGIconImageCollection;

type

  TFObjectInspector = class(TDockableForm)
    PObjects: TPanel;
      CBObjects: TComboBox;
    TCAttributesEvents: TTabControl;
    PNewDel: TPanel;
      BNewDelete: TButton;
      BMore: TButton;
    PMObjectInspector: TSpTBXPopupMenu;
    MIFont: TSpTBXItem;
    MIDefaultLayout: TSpTBXItem;
    MIPaste: TSpTBXItem;
    MICopy: TSpTBXItem;
    MICut: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    MIClose: TSpTBXItem;
    vilObjectInspectorLight: TVirtualImageList;
    vilObjectInspectorDark: TVirtualImageList;
    icObjectInspector: TSVGIconImageCollection;

    procedure FormShow(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    procedure ELPropertyInspectorModified(Sender: TObject);
    procedure ELPropertyInspectorFilterProp(Sender: TObject; AInstance: TPersistent;
      APropInfo: PPropInfo; var AIncludeProp: Boolean);

    procedure ELPropertyInspectorClick(Sender: TObject);
    procedure ELEventInspectorClick(Sender: TObject);
    procedure ELEventInspectorDblClick(Sender: TObject);
    procedure ELEventInspectorFilterProp(Sender: TObject; AInstance: TPersistent;
      APropInfo: PPropInfo; var AIncludeProp: Boolean);
    procedure ELObjectInspectorDeactivate(Sender: TObject);

    procedure BMoreClick(Sender: TObject);
    procedure BNewDeleteClick(Sender: TObject);

    procedure MICopyClick(Sender: TObject);
    procedure MICutClick(Sender: TObject);
    procedure MIPasteClick(Sender: TObject);
    procedure MIDefaultLayoutClick(Sender: TObject);
    procedure MIFontClick(Sender: TObject);
    procedure MICloseClick(Sender: TObject);

    procedure CBObjectsChange(Sender: TObject);
    procedure TCAttributesEventsChange(Sender: TObject);
    procedure PMObjectInspectorPopup(Sender: TObject);
    procedure FormMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer;
      var MouseActivate: TMouseActivate);
    procedure OnMouseDownEvent(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TCAttributesEventsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  public
    ELPropertyInspector: TELPropertyInspector;
    ELEventInspector: TELPropertyInspector;
    Events: string;
    Attributes: string;
    ShowEvents: integer;
    ShowAttributes: integer;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshCB(newname: string = '');
    procedure RefreshCBObjects;
    procedure MyOnGetComponentNames(Sender: TObject; AClass: TComponentClass; AResult: TStrings);
    procedure OnEnterEvent(Sender: TObject);
    procedure SetSelectedObject(Control: TControl);
    procedure ChangeSelection(SelectedControls: TELDesignerSelectedControls);
    procedure ChangeName(const OldName, NewName: string);
    procedure SaveWindow;
    procedure LoadWindow;
    procedure SetBNewDeleteCaption;
    procedure SetButtonCaption(aShow: integer);
    procedure CutToClipboard;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure SetFontSize(Delta: integer);
    procedure SetFont(aFont: TFont);
    procedure UpdateState;
    procedure UpdateItems;
    procedure UpdatePropertyInspector;
    procedure UpdateEventInspector;
    procedure Add(AObject: TControl);
    procedure ShowIt;
    procedure HideIt;
    procedure ChangeHideShow;
    procedure ChangeStyle;
  end;

var
  FObjectInspector: TFObjectInspector = nil;

implementation

uses Windows, SysUtils, Dialogs, Clipbrd, Math, Themes, StrUtils, JvGnugettext,
     UGUIForm, UGUIDesigner, UJava, UConfiguration,
     UUtils, UJPanel, UFXComponents, UAComponents,
     UJEComponents, UFXGuiForm, UBaseForm, UEditorForm, UJGuiForm;

{$R *.dfm}

constructor TFObjectInspector.Create(aOwner: TComponent);
begin
  inherited create(aOwner);
  visible:= false;
  ShowEvents:= 1;
  ShowAttributes:= 1;
  ELPropertyInspector:= TELPropertyInspector.Create(Self);
  with ELPropertyInspector do begin
    Parent:= Self;
    Align:= alClient;
    BorderStyle:= bsNone;
    OnFilterProp:= ELPropertyInspectorFilterProp;
    OnModified  := ELPropertyInspectorModified;
    OnClick     := ELPropertyInspectorClick;
    OnGetComponentNames:= MyOnGetComponentNames;
    OnMouseDown := OnMouseDownEvent;
    OnEnter     := OnEnterEvent;
  end;
  ELEventInspector:= TELPropertyInspector.Create(Self);
  with ELEventInspector do begin
    Parent:= self;
    Align:= alClient;
    BorderStyle:= bsNone;
    Visible:= False;
    ReadOnly:= true;
    OnFilterProp:= ELEventInspectorFilterProp;
    OnClick     := ELEventInspectorClick;
    OnDblClick  := ELEventInspectorDblClick;
    OnMouseDown := OnMouseDownEvent;
    OnEnter     := OnEnterEvent;
  end;
  OnDeactivate:= ELObjectInspectorDeactivate;
  PObjects.Autosize:= true;
  LoadWindow;
  ChangeStyle;
end;

procedure TFObjectInspector.OnMouseDownEvent(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // due to wrong determination auf parent form if form is docked
  if canFocus then SetFocus;
end;

destructor TFObjectInspector.Destroy;
begin
  FreeAndNil(ELPropertyInspector);
  FreeAndNil(ELEventInspector);
  inherited Destroy;
end;

procedure TFObjectInspector.LoadWindow;
begin
  UndockLeft:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'UndockLeft', 1));
  UndockTop:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'UndockTop', 1));
  UndockWidth:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'UndockWidth', 200));
  UndockHeight:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'UndockHeight', 200));
  UndockLeft:= min(UndockLeft, Screen.DesktopWidth - 50);
  UndockTop:= min(UndockTop, Screen.DesktopHeight - 50);
  ManualFloat(Rect(UnDockLeft, UnDockTop, UnDockLeft + UnDockWidth, UnDockTop + UnDockHeight));
  CBObjects.Align:= alTop;
  ELPropertyInspector.Splitter:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'ELPropertyInspector.Splitter', 100));
  ELEventInspector.Splitter:=    PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'ELEventInspector.Splitter', 100));
  Font.Size:= PPIScale(FConfiguration.ReadIntegerU('ObjectInspector', 'Font.Size', 10));
  Font.Name:= FConfiguration.ReadStringU('ObjectInspector', 'Font.Name', 'Segoe UI');
end;

procedure TFObjectInspector.SaveWindow;
begin
  FConfiguration.WriteBoolU('ObjectInspector', 'Visible', Visible);
  FConfiguration.WriteBoolU('ObjectInspector', 'Floating', Floating);
  if Floating then begin
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockLeft', Left);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockTop',  Top);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockWidth', Width);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockHeight', Height);
  end else begin
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockLeft', UndockLeft);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockTop',  UndockTop);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockWidth', UndockWidth);
    FConfiguration.WriteIntegerU('ObjectInspector', 'UndockHeight', UndockHeight);
  end;
  FConfiguration.WriteIntegerU('ObjectInspector', 'ELPropertyInspector.Splitter', PPIUnScale(ELPropertyInspector.Splitter));
  FConfiguration.WriteIntegerU('ObjectInspector', 'ELEventInspector.Splitter', PPIUnscale(ELEventInspector.Splitter));
  FConfiguration.WriteIntegerU('ObjectInspector', 'Font.Size', PPIUnScale(Font.Size));
  FConfiguration.WriteStringU('ObjectInspector', 'Font.Name', Font.Name);
end;

procedure TFObjectInspector.MyOnGetComponentNames(Sender: TObject; AClass: TComponentClass; AResult: TStrings);
begin
  if not FGUIDesigner.ELDesigner.Active then exit;
  var Form:= TFForm(FGUIDesigner.ELDesigner.DesignControl);
  for var i:= 0 to Form.ComponentCount - 1 do
    AResult.Add(Form.Components[i].Name);
end;

procedure TFObjectInspector.OnEnterEvent(Sender: TObject);
begin
  UpdateState;
end;

procedure TFObjectInspector.RefreshCB(newname: string = '');
  var i, index: integer; typ, nam, namtyp: string; Form: TFForm; Partner: TFEditForm;
begin
  if not FGUIDesigner.ELDesigner.Active then exit;
  Form:= TFForm(FGUIDesigner.ELDesigner.DesignControl);
  Partner:= TFEditForm(Form.Partner);
  if Assigned(Form) then begin
    index:= CBObjects.ItemIndex;
    CBObjects.Clear;
    CBObjects.Items.AddObject(Form.Name + ': ' + Partner.FrameTypToString, Form);
    for i:= 0 to Form.ComponentCount - 1 do begin
      nam:= Form.Components[i].Name;
      if Form.Components[i] is TJEComponent then
        typ:= (Form.Components[i] as TJEComponent).JavaType;
      if (nam <> '') and (typ <> '') then begin
        CBObjects.Items.AddObject(nam + ': ' + typ, Form.Components[i]);
        if nam = newname then
          namtyp:= nam + ': ' + typ;
      end;
    end;
    if newname = ''
      then CBObjects.ItemIndex:= index
      else CBObjects.ItemIndex:= CBObjects.Items.IndexOf(namtyp);
  end;
end;

procedure TFObjectInspector.RefreshCBObjects;
begin
  RefreshCB;
  if FGUIDesigner.ELDesigner.SelectedControls.Count = 1 then begin
    CBObjects.ItemIndex:= CBObjects.Items.IndexOfObject(FGUIDesigner.ELDesigner.SelectedControls[0]);
    try
      SetSelectedObject(FGUIDesigner.ELDesigner.SelectedControls[0]);
    except
    end;
  end;
end;

procedure TFObjectInspector.SetSelectedObject(Control: TControl);
begin
  if Control = nil then begin
    CBObjects.ItemIndex:= -1;
    CBObjects.Repaint;
  end else begin
    CBObjects.ItemIndex:= CBObjects.Items.IndexOfObject(Control);
    ELPropertyInspector.Clear;
    ELEventInspector.Clear;
    ELPropertyInspector.Add(Control);
    ELEventInspector.Add(Control);
  end;
end;

procedure TFObjectInspector.CBObjectsChange(Sender: TObject);
begin
  if CBObjects.ItemIndex <= -1 then exit;
  var Control:= TControl(CBObjects.Items.Objects[CBObjects.ItemIndex]);
  if assigned(Control) then begin
    Control.BringToFront;
    if Control is TJPanel then (Control as TJPanel).setTab;
    if FGUIDesigner.ELDesigner.Active then begin
      FGUIDesigner.ELDesigner.SelectedControls.Clear;
      FGUIDesigner.ELDesigner.SelectedControls.Add(Control);
    end;
  end;
end;

procedure TFObjectInspector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FJava.ActiveTool:= -1;
end;

procedure TFObjectInspector.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFObjectInspector.FormDeactivate(Sender: TObject);
begin
  ELPropertyInspector.UpdateActiveRow;
end;

procedure TFObjectInspector.FormMouseActivate(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y, HitTest: Integer;
  var MouseActivate: TMouseActivate);
begin
  FJava.ActiveTool:= 8;
  FJava.UpdateMenuItems(Self);
  if ELPropertyInspector.Visible and ELPropertyInspector.canFocus
    then ELPropertyInspector.SetFocus
  else if ELEventInspector.Visible and ELEventInspector.canFocus
    then ELEventInspector.SetFocus;
end;

procedure TFObjectInspector.FormShow(Sender: TObject);
begin
  FJava.ActiveTool:= 8;
  SetButtonCaption(1);
  TCAttributesEvents.Height:= Canvas.TextHeight('Attribute') + 10;
  PObjects.Height:= TCAttributesEvents.Height;
  if ELPropertyInspector.CanFocus then
    ELPropertyInspector.SetFocus;
end;

procedure TFObjectInspector.MICloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFObjectInspector.MICutClick(Sender: TObject);
begin
  CutToClipboard;
end;

procedure TFObjectInspector.MICopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TFObjectInspector.MIPasteClick(Sender: TObject);
begin
  PasteFromClipboard;
end;

procedure TFObjectInspector.MIDefaultLayoutClick(Sender: TObject);
begin
  FJava.MIDefaultLayoutClick(Self);
end;

procedure TFObjectInspector.MIFontClick(Sender: TObject);
begin
  FJava.FDFont.Font.Assign(Font);
  FJava.FDFont.Options:= [];
  if FJava.FDFont.Execute then begin
    Font.Assign(FJava.FDFont.Font);
    ELPropertyInspector.Font.Size:= Font.Size;
    ELPropertyInspector.Font.Name:= Font.Name;
    ELEventInspector.Font.Size:= Font.Size;
    ELEventInspector.Font.Name:= Font.Name;
  end;
  TCAttributesEvents.Height:= Canvas.TextHeight('Attribute') + 10;
  PObjects.Height:= TCAttributesEvents.Height;
end;

// DPI awareness for object inspector
// width, height, x, y are scaled to fit the real widget values
// setPositionAndSize must unscale for correct values in the source code
//
// width, height, x, y must be shown unscaled
// this is done in TELPropertyInspectorItem.UpdateParams;

procedure TFObjectInspector.ELPropertyInspectorModified(Sender: TObject);
  var i, iValue: integer; Partner: TFEditForm;
      OldName, NewName, Caption, Events: string;
      PropertyItem: TELPropertyInspectorItem;
      Control: TControl;
      JEControl: TJEComponent;
      Designer: TELDesigner;

  function PPIScale(ASize: integer): integer;
  begin
    Result := MulDiv(ASize, FCurrentPPI, 96);
  end;

begin
  PropertyItem:= TELPropertyInspectorItem(ELPropertyInspector.ActiveItem);
  if PropertyItem = nil then exit;
  Caption:= PropertyItem.Caption;
  Designer:= FGUIDesigner.ELDesigner;

  TFForm(Designer.DesignControl).Modified:= true;
  Partner:= TFEditForm(TFForm(Designer.DesignControl).Partner);
  Partner.EnsureStartEnd;
  if (Caption = 'Name') and (PropertyItem.Level = 0) then begin
    OldName:= CBObjects.Text;
    delete(OldName, Pos(':', OldName), length(OldName));
    Control:= Designer.SelectedControls.Items[0];
    NewName:= Control.Name;
    (Control as TJEComponent).Rename(OldName, NewName, Events);
    RefreshCB(NewName);
  end else begin
    for i:= 0 to Designer.SelectedControls.Count-1 do begin
      Control:= Designer.SelectedControls.Items[i];
      if Control is TJEComponent
        then JEControl:= Control as TJEComponent
        else JEControl:= nil;
      if ((Caption = 'Width') or (Caption = 'Height') or
         (Caption = 'X') or (Caption = 'Y') or // X,Y in JFrame
         (Caption = 'LayoutX') or (Caption = 'LayoutY') or
         (Caption = 'PrefWidth') or (Caption = 'PrefHeight')) and
         assigned(JEControl) and TryStrToInt(PropertyItem.Editor.Value, iValue)
      then begin
        if Caption = 'Width' then
          JEControl.Width:= PPIScale(iValue)
        else if Caption = 'Height' then
          JEControl.Height:= PPIScale(iValue)
        else if Caption = 'X' then
          JEControl.Left:= PPIScale(iValue)
        else if Caption = 'Y' then
          JEControl.Top:= PPIScale(iValue);
       { else if Caption = 'LayoutX' then
          JEControl.LayoutX:= PPIScale(iValue)
        else if Caption = 'LayoutY' then
          JEControl.LayoutY:= PPIScale(iValue);
        else if Caption = 'X' then
          JEControl.Left:= PPIScale(iValue)
        else if Caption = 'Y' then
          JEControl.Top:= PPIScale(iValue);}
        FGuiDesigner.MoveComponent(JEControl)
      end else begin
        if (Caption = 'Text') and assigned(JEControl) then begin
          JEControl.SizeToText;
          if FConfiguration.NameFromText then begin
            OldName:= JEControl.Name;
            JEControl.NameFromText;
            Newname:= JEControl.Name;
            (Control as TJEComponent).Rename(OldName, NewName, Events);
            RefreshCB(JEControl.Name);
            UpdatePropertyInspector;
          end;
        end;
        FGuiDesigner.SetAttributForComponent(
          Caption, PropertyItem.Editor.Value,
          string(PropertyItem.Editor.PropTypeInfo.Name), Control);
      end;
    end;
  end;
end;

procedure TFObjectInspector.ELPropertyInspectorFilterProp(
  Sender: TObject; AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
  AIncludeProp:= false;
  if not (assigned(APropInfo) and assigned(AInstance)) then
    exit;
  var s:= string(APropInfo.Name);
  if (length(s) > 0) and not IsLower(s[1]) then
    if AInstance.ClassName = 'TFont'
      then AIncludeProp:= (Pos(' ' + s + ' ', ' Name Size Style ') > 0)
      else AIncludeProp:= (Pos('|' + s + '|', Attributes) > 0);
end;

procedure TFObjectInspector.ELEventInspectorFilterProp(
  Sender: TObject; AInstance: TPersistent; APropInfo: PPropInfo; var AIncludeProp: Boolean);
begin
  AIncludeProp:= Pos('|' + string(APropInfo.Name) + '|', Events) > 0;
end;

procedure TFObjectInspector.ELPropertyInspectorClick(Sender: TObject);
begin
  UpdateState;
end;

procedure TFObjectInspector.ELObjectInspectorDeactivate(Sender: TObject);
  // take last input from ObjectInspector
begin
  ELPropertyInspector.UpdateActiveRow;
end;

procedure TFObjectInspector.TCAttributesEventsChange(Sender: TObject);
begin
  case TCAttributesEvents.TabIndex of
    0: begin
         ELEventInspector.Visible:= false;
         ELPropertyInspector.Visible:= true;
         BNewDelete.visible:= false;
         SetButtonCaption(ShowAttributes);
         if ELPropertyInspector.canFocus then ELPropertyInspector.SetFocus;
      end;
   1: begin
         ELPropertyInspector.Visible:= false;
         ELEventInspector.Visible:= true;
         BNewDelete.Visible:= true;
         SetBNewDeleteCaption;
         SetButtonCaption(ShowEvents);
         if ELEventInspector.CanFocus then ELEventInspector.SetFocus;
       end;
  end;
  UpdateState;
end;

procedure TFObjectInspector.TCAttributesEventsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if canFocus then
    SetFocus;
end;

procedure TFObjectInspector.setButtonCaption(aShow: integer);
begin
  case aShow of
      1: BMore.Caption:= _('More');
      2: BMore.Caption:= _('Full');
    else BMore.Caption:= _('Default');
  end;
end;

procedure TFObjectInspector.SetBNewDeleteCaption;
begin
  if ELEventInspector.ActiveItem = nil then exit;
  if ELEventInspector.ActiveItem.DisplayValue = ''
    then BNewDelete.Caption:= _('New')
    else BNewDelete.Caption:= _('Delete');
  UpdateEventInspector;
end;

procedure TFObjectInspector.BMoreClick(Sender: TObject);
begin
  if ELEventInspector.Visible then begin
    Inc(ShowEvents);
    if ShowEvents = 4 then
       ShowEvents:= 1;
    if FGUIDesigner.ELDesigner.SelectedControls.Count > 0 then begin
      ELEventInspector.Clear;
      Add(FGUIDesigner.ELDesigner.SelectedControls[0]);
      ELEventInspector.Add(FGUIDesigner.ELDesigner.SelectedControls[0]);
    end;
    SetButtonCaption(ShowEvents);
    SetBNewDeleteCaption;
  end else begin
    Inc(ShowAttributes);
    if ShowAttributes = 4 then
       ShowAttributes:= 1;
    if FGUIDesigner.ELDesigner.SelectedControls.Count > 0 then begin
      ELPropertyInspector.Clear;
      Add(FGUIDesigner.ELDesigner.SelectedControls[0]);
      ELPropertyInspector.Add(FGUIDesigner.ELDesigner.SelectedControls[0]);
    end;
    SetButtonCaption(ShowAttributes);
    SetBNewDeleteCaption;
  end;
end;

procedure TFObjectInspector.BNewDeleteClick(Sender: TObject);
  var PropertyItem: TELPropsPageItem;
      Partner: TFEditForm;
      Control: TControl;
      JEComponent: TJEComponent;

  procedure setListener(Control: TJEComponent);
  begin
    var Event:= PropertyItem.Caption;
    Control.DeleteListener(Event);
    if PropertyItem.DisplayValue <> '' then   // Delete
      PropertyItem.DisplayValue:= ''
    else begin                                // Add
      PropertyItem.DisplayValue:= Control.MakeEventProcedureName(Event);
      Control.AddListener(Event);
    end;
  end;

begin
  PropertyItem:= ELEventInspector.ActiveItem;
  var i:= CBObjects.ItemIndex;
  if not assigned(PropertyItem) or (i = -1) then
    exit;
  Control:= TControl(CBObjects.Items.Objects[i]);
  Partner:= TFEditForm(TFForm(FGUIDesigner.ELDesigner.DesignControl).Partner);
  Partner.Editor.BeginUpdate;
  Partner.EnsureStartEnd;

  if Control is TFXGuiForm  then begin
    JEComponent:= TFXNode.Create(Control);
    JEComponent.Name:= 'primaryStage';
    setListener(JEComponent);
    Partner.InsertImport('javafx.scene.input.*');
    Partner.InsertImport('javafx.event.*');
    FreeAndNil(JEComponent);
  end else if Control is TFGuiForm then begin
    JEComponent:= TJGuiForm.Create(Control);
    JEComponent.Name:= 'cp';
    setListener(JEComponent);
    FreeAndNil(JEComponent);
  end else if Control is TFXNode then begin
    setListener(Control as TFXNode);
    Partner.InsertImport('javafx.scene.input.*');
    Partner.InsertImport('javafx.event.*');
  end else if Control is TAWTComponent then
    setListener(Control as TAWTComponent);
  Partner.Editor.EndUpdate;
  ELEventInspector.UpdateItems;
  SetBNewDeleteCaption;
end;

procedure TFObjectInspector.ELEventInspectorClick(Sender: TObject);
begin
  SetBNewDeleteCaption;
end;

procedure TFObjectInspector.ELEventInspectorDblClick(Sender: TObject);
begin
  if assigned(ELEventInspector.ActiveItem) then begin
    var s:= ELEventInspector.ActiveItem.DisplayValue;
    if s = '' then
      BNewDeleteClick(Self)
    else begin
      var Partner:= TFEditForm(TFForm(FGUIDesigner.ELDesigner.DesignControl).Partner);
      Partner.Go_To('public void ' + s);
      if assigned(FGuiDesigner) then
        FGuiDesigner.GUIDesignerTimer.Enabled:= true;
    end;
  end;
  UpdateEventInspector;
end;

procedure TFObjectInspector.PMObjectInspectorPopup(Sender: TObject);
begin
  MICut.Visible:= (TCAttributesEvents.TabIndex = 0);
  MICopy.Visible:= MICut.Visible;
  MIPaste.Visible:= MICut.Visible;
end;

procedure TFObjectInspector.CutToClipboard;
begin
  if TCAttributesEvents.TabIndex = 0 then begin
    var PropertyItem:= TELPropertyInspectorItem(ELPropertyInspector.ActiveItem);
    if assigned(PropertyItem) then begin
      Clipboard.AsText:= PropertyItem.DisplayValue;
      PropertyItem.DisplayValue:= '';
    end;
  end;
end;

procedure TFObjectInspector.CopyToClipboard;
begin
  if TCAttributesEvents.TabIndex = 0 then begin
    var PropertyItem:= TELPropertyInspectorItem(ELPropertyInspector.ActiveItem);
    if assigned(PropertyItem) then
      Clipboard.AsText:= PropertyItem.DisplayValue;
  end;
end;

procedure TFObjectInspector.PasteFromClipboard;
begin
  if TCAttributesEvents.TabIndex = 0 then begin
    var PropertyItem:= TELPropertyInspectorItem(ELPropertyInspector.ActiveItem);
    if assigned(PropertyItem) then
      PropertyItem.DisplayValue:= Clipboard.AsText;
  end;
end;

procedure TFObjectInspector.ChangeSelection(SelectedControls: TELDesignerSelectedControls);
  var i: integer; s: string;
begin
  if assigned(ELPropertyInspector.ActiveItem)
    then s:= ELPropertyInspector.ActiveItem.Caption
    else s:= '';
  try
    ELPropertyInspector.Clear;
    ELEventInspector.Clear;
  except
  end;
  if SelectedControls.Count <> 1
    then CBObjects.ItemIndex:= -1
    else CBObjects.ItemIndex:=
           CBObjects.Items.IndexOfObject(SelectedControls[0]);
  if SelectedControls.Count > 0 then
    Add(SelectedControls[0]);
  for i:= 0 to SelectedControls.Count - 1 do begin
    ELPropertyInspector.Add(SelectedControls[i]);
    ELEventInspector.Add(SelectedControls[i]);
  end;
  if s <> '' then
    ELPropertyInspector.SelectByCaption(s);
  SetBNewDeleteCaption;
end;

procedure TFObjectInspector.SetFont(aFont: TFont);
begin
  Font.Assign(aFont);
  ELPropertyInspector.Font.Size:= Font.Size;
  ELPropertyInspector.Font.Name:= Font.Name;
  ELEventInspector.Font.Size:= Font.Size;
  ELEventInspector.Font.Name:= Font.Name;
  TCAttributesEvents.Height:= Canvas.TextHeight('Attribute') + 10;
  PObjects.Height:= TCAttributesEvents.Height;
end;

procedure TFObjectInspector.SetFontSize(Delta: integer);
begin
  var Size:= Font.Size + Delta;
  if Size < 6 then Size:= 6;
  Font.Size:= Size;
  ELPropertyInspector.Font.Size:= Font.Size;
  ELEventInspector.Font.Size:= Font.Size;
  TCAttributesEvents.Height:= Canvas.TextHeight('Attribute') + 10;
  PObjects.Height:= TCAttributesEvents.Height;
  Show;
end;

procedure TFObjectInspector.UpdateState;
begin
  var b:= (TCAttributesEvents.TabIndex = 0) and (ELPropertyInspector.SelText <> '');
  with FJava do begin
    SetEnabledMI(MICut, b);
    SetEnabledMI(MICopy, b);
    SetEnabledMI(MIPaste, Clipboard.HasFormat(CF_Text) and (TCAttributesEvents.TabIndex = 0));
  end;
end;

procedure TFObjectInspector.UpdatePropertyInspector;
begin
  TThread.ForceQueue(nil, procedure
    begin
      ELPropertyInspector.UpdateItems;
    end);
end;

procedure TFObjectInspector.UpdateEventInspector;
begin
  TThread.ForceQueue(nil, procedure
    begin
      ELEventInspector.UpdateItems;
    end);
end;

procedure TFObjectInspector.UpdateItems;
begin
  UpdatePropertyInspector;
  UpdateEventInspector;
end;

procedure TFObjectInspector.Add(AObject: TControl);
begin
  if AObject is TJEComponent then begin
    Events:= (AObject as TJEComponent).getEvents(ShowEvents);
    Attributes:= (AObject as TJEComponent).getAttributes(ShowAttributes) + '|';
  end else if AObject is TFXGuiForm then begin
    Events:= (AObject as TFXGuiForm).getEvents(ShowEvents);
    Attributes:= (AObject as TFXGuiForm).getAttributes(ShowAttributes);
  end else if AObject is TFGuiForm then begin
    Events:= (AObject as TFGuiForm).getEvents(ShowEvents);
    Attributes:= (AObject as TFGuiForm).getAttributes(ShowAttributes);
  end;
end;

procedure TFObjectInspector.ChangeName(const OldName, NewName: string);
begin
  var i:= CBObjects.ItemIndex;
  var s:= ReplaceStr(CBObjects.Items[i], OldName, NewName);
  CBObjects.Items[i]:= s;
  CBObjects.ItemIndex:= i;
  FGUIDesigner.ELDesigner.SelectedControls.Items[0].Repaint;
end;

procedure TFObjectInspector.ShowIt;
begin
  FJava.ShowDockableForm(Self);
end;

procedure TFObjectInspector.HideIt;
begin
  Close;
end;

procedure TFObjectInspector.ChangeHideShow;
begin
  if Visible
    then HideIt
    else ShowIt;
end;

procedure TFObjectInspector.ChangeStyle;
begin
  if StyleServices.IsSystemStyle then begin
    ELPropertyInspector.Color:= clBtnFace;
    ELPropertyInspector.Font.Color:= clBlack;
    ELPropertyInspector.ValuesColor:= clNavy;
    ELEventInspector.Color:= clBtnFace;
    ELEventInspector.Font.Color:= clBlack;
    ELEventInspector.ValuesColor:= clNavy;
  end else begin
    ELPropertyInspector.Color:= StyleServices.GetSystemColor(clBtnFace);
    ELPropertyInspector.Font.Color:= StyleServices.getStyleFontColor(sfTabTextInactiveNormal);
    ELPropertyInspector.ValuesColor:= StyleServices.getStyleFontColor(sfTabTextActiveNormal);
    ELEventInspector.Color:= StyleServices.GetSystemColor(clBtnFace);
    ELEventInspector.Font.Color:= StyleServices.getStyleFontColor(sfTabTextInactiveNormal);
    ELEventInspector.ValuesColor:= StyleServices.getStyleFontColor(sfTabTextActiveNormal);
  end;
  if FConfiguration.isDark
    then PMObjectInspector.Images:= vilObjectInspectorDark
    else PMObjectInspector.Images:= vilObjectInspectorLight;
end;

end.


