unit
  UJTabbedPane;

interface

uses
  Classes, Controls, ComCtrls, UJComponents;

type

  TTabPlacement = (BOTTOM, LEFT, RIGHT, TOP);
  TTabLayoutPolicy = (WRAP_TAB_LAYOUT, SCROLL_TAB_LAYOUT);

  TJTabbedPane = class (TSwingComponent)
  private
    FSelectedIndex: integer;
    FTabs: TStrings;
    FTabPlacement: TTabPlacement;
    FTabLayoutPolicy: TTabLayoutPolicy;
    function  getStrings: TStrings;
    procedure setStrings(Strings: TStrings);
    procedure setTabPlacement(aValue: TTabPlacement);
    procedure setTabLayoutPolicy(aValue: TTabLayoutPolicy);
    procedure setSelectedIndex(aValue: integer);
    procedure MakeTabs;
    function GetUpperEvents(ShowEvents: integer): string;
    function MakeUpperEvents(eve: string): string;
  protected
    procedure ResizePane(Sender: TObject);
  public
    TabsOld: TStrings;
    constructor Create (AOwner: TComponent); override;
    constructor CreateFrom(aTabControl: TTabControl);
    destructor Destroy; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure Delete(Control: TControl);
    function MaxTab: integer;
    procedure placeTabs;
  published
    property SelectedIndex: integer read FSelectedIndex write setSelectedIndex;
    property Tabs: TStrings read getStrings write setStrings;
    property TabPlacement: TTabPlacement read FTabPlacement write setTabPlacement;
    property TabLayoutPolicy: TTabLayoutPolicy read FTabLayoutPolicy write setTabLayoutPolicy;
  end;


implementation

uses SysUtils, Math, Types, Graphics,
     UObjectInspector, UAComponents, UJPanel, UUtils;

const paddingH = 11;
      paddingV = 3;

constructor TJTabbedPane.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  Tag:= 18;
  Width:= 200;
  Height:= 80;
  Foreground:= DefaultForeground;
  Background:= SelectionColor; 
  FTabs:= TStringList.Create;
  FTabs.Text:= defaultItems;
  TabsOld:= TStringList.Create;
  FTabPlacement:= UJTabbedPane.TOP;
  FTabLayoutPolicy:= WRAP_TAB_LAYOUT;
  FSelectedIndex:= -1;
  JavaType:= 'JTabbedPane';
  OnResize:= ResizePane;
end;

constructor TJTabbedPane.CreateFrom(aTabControl: TTabControl);
begin
  Create(aTabControl.Owner);
  CreateFromJ(aTabControl);
  Tabs:= aTabControl.Tabs;
  SelectedIndex:= aTabControl.TabIndex;
end;

destructor TJTabbedPane.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(TabsOld);
  inherited;
end;

function TJTabbedPane.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|SelectedIndex|Tabs|TabPlacement|TabLayoutPolicy' + inherited;
end;

procedure TJTabbedPane.setAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Tabs' then
    MakeTabs
  else if (Attr = 'TabPlacement') or (Attr = 'TabLayoutPolicy') then
    MakeAttribut(Attr, 'JTabbedPane.' + Value)
  else
    inherited;
end;

function TJTabbedPane.getEvents(ShowEvents: integer): string;
begin
  Result:= '|stateChanged' + inherited getEvents(ShowEvents);
end;

procedure TJTabbedPane.Rename(const OldName, NewName, Events: string);
  var i: integer; OldPanel, NewPanel: string; Comp: TComponent;
begin
  inherited;
  for i:= 0 to FTabs.Count - 1 do begin
    OldPanel:= WithoutSpaces(OldName + 'TabPanel' + Tabs[i]);
    NewPanel:= WithoutSpaces(NewName + 'TabPanel' + Tabs[i]);
    Partner.ReplaceWord(OldPanel, NewPanel, true);
    Comp:= Owner.FindComponent(OldPanel);
    if assigned(Comp) then
      Comp.Name:= NewPanel;
  end;
end;

procedure TJTabbedPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JTabbedPane ' + Name + ' = new JTabbedPane();');
  MakeTabs;
end;

procedure TJTabbedPane.MakeTabs;
  var key, s, OldTab, NewTab, OldName, NewName: string;
      i, deleted, inserted: integer;
      aPanel: TJPanel; Comp: TComponent;
begin
  if TabsOld.Count > Tabs.Count then begin
    deleted:= 0;
    for i:= 0 to TabsOld.Count - 1 do begin
      OldTab := TabsOld[i];
      OldName:= WithoutSpaces(Name + 'TabPanel' + OldTab);
      if Tabs.IndexOf(OldTab) >= 0 then
        Continue;

      if deleted < TabsOld.Count - Tabs.Count then begin
        Partner.DeleteAttribute('private JPanel ' + OldName);
        Partner.DeleteAttributeValue(Name + '.addTab("' + OldTab + '"');
        Comp:= Owner.FindComponent(OldName);
        if assigned(Comp) then begin
          (Comp as TAWTComponent).DeleteComponent;
          FreeAndNil(Comp);
        end;
        inc(deleted);
      end else begin // Tabstext changed
        NewTab := Tabs[i];
        NewName:= WithoutSpaces(Name + 'TabPanel' + NewTab);
        Comp:= Owner.FindComponent(OldName);
        if assigned(Comp) then
          try
            Comp.Name:= NewName;
            Partner.ReplaceText(Name + '.addTab("' + OldTab + '"',
                                Name + '.addTab("' + NewTab + '"', false);
            Partner.ReplaceComponentname(OldName, NewName, GetUpperEvents(3) + '|');
          except on e: Exception do begin
            ErrorMsg(e.Message);
            Tabs.Strings[i]:= OldTab;
            end;
          end;
      end;
    end;
  end
  else if TabsOld.Count <= Tabs.Count then begin
    inserted:= 0;
    for i:= 0 to Tabs.Count - 1 do begin
      NewTab := Tabs[i];
      NewName:= WithoutSpaces(Name + 'TabPanel' + NewTab);
      if TabsOld.IndexOf(NewTab) >= 0 then
        Continue;
      if inserted < Tabs.Count - TabsOld.Count then begin
        if i = 0
          then key:= Name + '.setBounds('
          else key:= Name + '.addTab("' + Tabs[i-1];
        InsertNewVariable('  private JPanel ' + NewName + ' = new JPanel(null, true);');
        s:= Indent2 + Name + '.addTab("' + NewTab + '", ' + NewName + ');';
        Partner.InsertAttributAfter(key, s);
        aPanel:= TJPanel.Create(Owner);
        try
          aPanel.Parent:= self;
          aPanel.Name:= NewName;
          inc(inserted);
        except on e: Exception do
          ErrorMsg(e.Message);
        end;
      end else begin
        OldTab := TabsOld[i];
        OldName:= Name + 'TabPanel' + OldTab;
        Comp:= Owner.FindComponent(OldName);
        if assigned(Comp) then
          try
            Comp.Name:= NewName;
            Partner.ReplaceText(Name + '.addTab("' + OldTab + '"',
                                Name + '.addTab("' + NewTab + '"', false);
            Partner.ReplaceComponentname(OldName, NewName, GetUpperEvents(3) + '|');
          except on e: Exception do begin
            ErrorMsg(e.Message);
            Tabs[i]:= OldTab;
            end;
          end;
      end;
    end;
  end;
  placeTabs;
  FObjectInspector.RefreshCBObjects;
end;

function TJTabbedPane.GetUpperEvents(ShowEvents: integer): string;
begin
  Result:= MakeUpperEvents(GetEvents(ShowEvents));
end;

function TJTabbedPane.MakeUpperEvents(eve: string): string;
  var i: integer;
begin
  if eve <> '' then begin
    eve[1]:= UpCase(eve[1]);
    for i:= 1 to length(eve) do
      if (eve[i] = '|') and (i < length(eve)) then
        eve[i+1]:= UpCase(eve[i+1]);
  end;
  Result:= eve;
end;

procedure TJTabbedPane.Paint;
  var th, tw, i, x1, y1: integer;
      Points: array[0..4] of TPoint;

  function ShowHorTab(x1, y1: integer; const s: string; TopCorner: boolean): integer;
    var dx, dy, p6: integer;
  begin
    tw:= Canvas.TextWidth(s);
    dx:= min(tw + 2*paddingH, Width-2);
    dy:= th + 2*paddingV;
    p6:= PPIScale(6);
    if TopCorner then begin
      Points[0]:= Point(x1, y1 + p6);
      Points[1]:= Point(x1, y1 + dy);
      Points[2]:= Point(x1 + dx, y1 + dy);
      Points[3]:= Point(x1 + dx, y1);
      Points[4]:= Point(x1 + p6, y1);
    end else begin
      Points[0]:= Point(x1, y1);
      Points[1]:= Point(x1, y1 + dy - p6);
      Points[2]:= Point(x1 + p6, y1 + dy);
      Points[3]:= Point(x1 + dx, y1 + dy);
      Points[4]:= Point(x1 + dx, y1);
    end;
    if (x1 < Width - PPIScale(10)) then begin
      Canvas.Polygon(Points);
      Canvas.TextOut(x1 + paddingH, y1 + paddingV, s);
    end;
    Result:= x1 + dx;
  end;

  procedure ShowVerTab(x1, y1: integer; const s: string; LeftCorner: boolean);
    var dy, w, p6: integer;
  begin
    w:= Canvas.TextWidth(s);
    dy:= th + 2*paddingV;
    p6:= PPIScale(6);
    if LeftCorner then begin
      Points[0]:= Point(x1, y1 + p6);
      Points[1]:= Point(x1, y1 + dy);
      Points[2]:= Point(x1 + tw, y1 + dy);
      Points[3]:= Point(x1 + tw, y1);
      Points[4]:= Point(x1 + p6, y1);
    end else begin
      Points[0]:= Point(x1, y1);
      Points[1]:= Point(x1, y1 + dy);
      Points[2]:= Point(x1 + tw, y1 + dy);
      Points[3]:= Point(x1 + tw, y1 + p6);
      Points[4]:= Point(x1 + tw - p6, y1);
    end;
    if (y1 + dy < Height - PPIScale(10)) then begin
      Canvas.Polygon(Points);
      Canvas.TextOut(x1 + (tw-w)div 2, y1 + paddingV, s)
    end;
  end;

  procedure SetBrushColor(i: integer);
  begin
    if (i = FSelectedIndex) or ((i = 0) and (FSelectedIndex = -1)) 
      then Canvas.Brush.Color:= SelectionColor
    else if Background = SelectionColor
      then Canvas.Brush.Color:= DefaultBackground
      else Canvas.Brush.Color:= Background
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground; // FFontColor
  Canvas.Pen.Color:= DarkShadow;
  Canvas.Brush.Color:= Background; // Background;
  th:= Canvas.TextHeight('Hg');
  if FTabs.Count > 0 then
    case FTabPlacement of
      UJTabbedPane.TOP: begin
        Canvas.Rectangle(Rect(0, th+2*paddingV, Width, Height));
        x1:= 2;
        y1:= 0;
        for i:= 0 to FTabs.Count - 1 do begin
          SetBrushColor(i);
          x1:= ShowHorTab(x1, y1, FTabs.Strings[i], true);
        end
      end;
      UJTabbedPane.BOTTOM: begin
        Canvas.Rectangle(Rect(0, 0, Width, Height-th-2*paddingV));
        x1:= 2;
        y1:= Height-th-2*paddingV-1;
        for i:= 0 to FTabs.Count - 1 do begin
          SetBrushColor(i);
          x1:= ShowHorTab(x1, y1, FTabs.Strings[i], false);
        end
      end;
      UJTabbedPane.LEFT: begin
        tw:= MaxTab + 2*paddingH;
        Canvas.Rectangle(Rect(tw, 0, Width, Height));
        x1:= 0;
        y1:= 2;
        for i:= 0 to FTabs.Count - 1 do begin
          SetBrushColor(i);
          ShowVerTab(x1, y1, FTabs.Strings[i], true);
          y1:= y1 + th + 2*paddingV;
        end
      end;
      UJTabbedPane.RIGHT: begin
        tw:= MaxTab + 2*paddingH;
        Canvas.Rectangle(Rect(0, 0, Width-tw, Height));
        x1:= Width-tw-1;
        y1:= 2;
        for i:= 0 to FTabs.Count - 1 do begin
          SetBrushColor(i);
          ShowVerTab(x1, y1, FTabs.Strings[i], false);
          y1:= y1 + th + 2*paddingV;
        end
      end;
  end
  else begin
    Canvas.Brush.Color:= DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, Height));
  end;
end;

function TJTabbedPane.getStrings: TStrings;
begin
  Result:= FTabs;
end;

function TJTabbedPane.MaxTab: integer;
  var i, max, w: integer;
begin
  max:= 0;
  for i:= 0 to FTabs.Count - 1 do begin
    w:= Canvas.TextWidth(FTabs.Strings[i]);
    if w > max then max:= w;
  end;
  Result:= max;
end;

procedure TJTabbedPane.placeTabs;
  var i, t, l: integer; aPanel: TJPanel;
begin
  CanvasFontAssign;
  t:= Canvas.TextHeight('Hg') + 3*paddingV;
  l:= MaxTab + 2*paddingH + paddingV;
  for i:= 0 to ControlCount - 1 do begin
    aPanel:= (Controls[i] as TJPanel);
    aPanel.setBounds(2, 3, Width-4, Height-3);
    case FTabPlacement of
      UJTabbedPane.TOP:    begin
                             aPanel.Top:= t;
                             aPanel.Height:= aPanel.Height - t;
                           end;
      UJTabbedPane.LEFT:   begin
                             aPanel.Left:= l;
                             aPanel.Width:= aPanel.Width - l;
                             aPanel.Height:= aPanel.Height - 4;
                           end;
      UJTabbedPane.Bottom: begin
                             aPanel.Height:= aPanel.Height - t;
                           end;
      UJTabbedPane.RIGHT:  begin
                             aPanel.Width:= aPanel.Width - l;
                             aPanel.Height:= aPanel.Height - 4;
                             aPanel.Left:= 4;
                           end;
    end;
  end;
  SetSelectedIndex(FSelectedIndex);  
end;

procedure TJTabbedPane.SetStrings(Strings: TStrings);
begin
  TabsOld.Text:= FTabs.Text;
  if FTabs.Text <> Strings.Text then begin
    FTabs.Assign(Strings);
    if FSelectedIndex >= FTabs.Count then
      SelectedIndex:= FTabs.Count-1;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetTabPlacement(aValue: TTabPlacement);
begin
  if aValue <> FTabPlacement then begin
    FTabPlacement:= aValue;
    PlaceTabs;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetTabLayoutPolicy(aValue: TTabLayoutPolicy);
begin
  if aValue <> FTabLayoutPolicy then begin
    FTabLayoutPolicy:= aValue;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetSelectedIndex(aValue: integer);
  var i, j: integer; s: string;
begin
  if aValue < ControlCount then begin
    FSelectedIndex:= aValue;
    if FSelectedIndex = -1
      then i:= 0
      else i:= FSelectedIndex;
    for j:= 0 to Tabs.Count-1 do
      if (j < ControlCount) and (Controls[j] is TJPanel) then begin
        s:= (Controls[j] as TJPanel).Name;
        if s = Self.Name + 'TabPanel' + Tabs[i] then begin
          Controls[j].BringToFront;
          Controls[j].Invalidate;
          Invalidate;
          break;
        end;
    end;
  end;
end;

procedure TJTabbedPane.Delete(Control: TControl);
  var i: integer; SL: TStringList;
begin
  i:= (Control as TJPanel).getIndex;
  SL:= TStringList.Create;
  SL.Text:= FTabs.Text;
  if (0 <= i) and (i < SL.Count) then begin
    SL.Delete(i);
    setStrings(SL);
  end;
  FreeAndNil(SL);
end;

procedure TJTabbedPane.ResizePane(Sender: TObject);
begin
  placeTabs;
end;

end.
