unit UJTabbedPane;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  UJComponents;

type

  TTabPlacement = (BOTTOM, LEFT, RIGHT, TOP);
  TTabLayoutPolicy = (WRAP_TAB_LAYOUT, SCROLL_TAB_LAYOUT);

  TJTabbedPane = class(TSwingComponent)
  private
    FSelectedIndex: Integer;
    FTabs: TStrings;
    FTabPlacement: TTabPlacement;
    FTabLayoutPolicy: TTabLayoutPolicy;
    FTabsOld: TStrings;
    function GetTabs: TStrings;
    procedure SetTabs(Strings: TStrings);
    procedure SetTabPlacement(AValue: TTabPlacement);
    procedure SetTabLayoutPolicy(AValue: TTabLayoutPolicy);
    procedure SetSelectedIndex(AValue: Integer);
    procedure MakeTabs;
    function GetUpperEvents(ShowEvents: Integer): string;
    function MakeUpperEvents(Eve: string): string;
  protected
    procedure ResizePane(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure NewControl; override;
    procedure Paint; override;
    procedure Delete(Control: TControl);
    function MaxTab: Integer;
    procedure PlaceTabs;
  published
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Tabs: TStrings read GetTabs write SetTabs;
    property TabPlacement: TTabPlacement read FTabPlacement
      write SetTabPlacement;
    property TabLayoutPolicy: TTabLayoutPolicy read FTabLayoutPolicy
      write SetTabLayoutPolicy;
  end;

implementation

uses
  SysUtils,
  Math,
  Types,
  Graphics,
  UObjectInspector,
  UAComponents,
  UJPanel,
  UUtils;

const
  CPaddingH = 11;
  CPaddingV = 3;

constructor TJTabbedPane.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := 18;
  Width := 200;
  Height := 80;
  Foreground := DefaultForeground;
  Background := SelectionColor;
  FTabs := TStringList.Create;
  FTabs.Text := DefaultItems;
  FTabsOld := TStringList.Create;
  FTabPlacement := UJTabbedPane.TOP;
  FTabLayoutPolicy := WRAP_TAB_LAYOUT;
  FSelectedIndex := -1;
  JavaType := 'JTabbedPane';
  OnResize := ResizePane;
end;

destructor TJTabbedPane.Destroy;
begin
  FreeAndNil(FTabs);
  FreeAndNil(FTabsOld);
  inherited;
end;

function TJTabbedPane.GetAttributes(ShowAttributes: Integer): string;
begin
  Result := '|SelectedIndex|Tabs|TabPlacement|TabLayoutPolicy' + inherited;
end;

procedure TJTabbedPane.SetAttribute(Attr, Value, Typ: string);
begin
  if Attr = 'Tabs' then
    MakeTabs
  else if (Attr = 'TabPlacement') or (Attr = 'TabLayoutPolicy') then
    MakeAttribut(Attr, 'JTabbedPane.' + Value)
  else
    inherited;
end;

function TJTabbedPane.GetEvents(ShowEvents: Integer): string;
begin
  Result := '|stateChanged' + inherited GetEvents(ShowEvents);
end;

procedure TJTabbedPane.Rename(const OldName, NewName, Events: string);
var
  OldPanel, NewPanel: string;
  Comp: TComponent;
begin
  inherited;
  for var I := 0 to FTabs.Count - 1 do
  begin
    OldPanel := WithoutSpaces(OldName + 'TabPanel' + Tabs[I]);
    NewPanel := WithoutSpaces(NewName + 'TabPanel' + Tabs[I]);
    FPartner.ReplaceWord(OldPanel, NewPanel, True);
    Comp := Owner.FindComponent(OldPanel);
    if Assigned(Comp) then
      Comp.Name := NewPanel;
  end;
end;

procedure TJTabbedPane.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private JTabbedPane ' + Name + ' = new JTabbedPane();');
  MakeTabs;
end;

procedure TJTabbedPane.MakeTabs;
var
  Key, Str, OldTab, NewTab, OldName, NewName: string;
  Deleted, Inserted: Integer;
  APanel: TJPanel;
  Comp: TComponent;
begin
  if FTabsOld.Count > Tabs.Count then
  begin
    Deleted := 0;
    for var I := 0 to FTabsOld.Count - 1 do
    begin
      OldTab := FTabsOld[I];
      OldName := WithoutSpaces(Name + 'TabPanel' + OldTab);
      if Tabs.IndexOf(OldTab) >= 0 then
        Continue;

      if Deleted < FTabsOld.Count - Tabs.Count then
      begin
        FPartner.DeleteAttribute('private JPanel ' + OldName);
        FPartner.DeleteAttributeValue(Name + '.addTab("' + OldTab + '"');
        Comp := Owner.FindComponent(OldName);
        if Assigned(Comp) then
        begin
          (Comp as TAWTComponent).DeleteComponent;
          FreeAndNil(Comp);
        end;
        Inc(Deleted);
      end
      else
      begin // Tabstext changed
        NewTab := Tabs[I];
        NewName := WithoutSpaces(Name + 'TabPanel' + NewTab);
        Comp := Owner.FindComponent(OldName);
        if Assigned(Comp) then
          try
            Comp.Name := NewName;
            FPartner.ReplaceText(Name + '.addTab("' + OldTab + '"',
              Name + '.addTab("' + NewTab + '"', False);
            FPartner.ReplaceComponentname(OldName, NewName,
              GetUpperEvents(3) + '|');
          except
            on E: Exception do
            begin
              ErrorMsg(E.Message);
              Tabs[I] := OldTab;
            end;
          end;
      end;
    end;
  end
  else if FTabsOld.Count <= Tabs.Count then
  begin
    Inserted := 0;
    for var I := 0 to Tabs.Count - 1 do
    begin
      NewTab := Tabs[I];
      NewName := WithoutSpaces(Name + 'TabPanel' + NewTab);
      if FTabsOld.IndexOf(NewTab) >= 0 then
        Continue;
      if Inserted < Tabs.Count - FTabsOld.Count then
      begin
        if I = 0 then
          Key := Name + '.setBounds('
        else
          Key := Name + '.addTab("' + Tabs[I - 1];
        InsertNewVariable('  private JPanel ' + NewName +
          ' = new JPanel(null, true);');
        Str := Indent2 + Name + '.addTab("' + NewTab + '", ' + NewName + ');';
        FPartner.InsertAttributAfter(Key, Str);
        APanel := TJPanel.Create(Owner);
        try
          APanel.Parent := Self;
          APanel.Name := NewName;
          Inc(Inserted);
        except
          on E: Exception do
            ErrorMsg(E.Message);
        end;
      end
      else
      begin
        OldTab := FTabsOld[I];
        OldName := Name + 'TabPanel' + OldTab;
        Comp := Owner.FindComponent(OldName);
        if Assigned(Comp) then
          try
            Comp.Name := NewName;
            FPartner.ReplaceText(Name + '.addTab("' + OldTab + '"',
              Name + '.addTab("' + NewTab + '"', False);
            FPartner.ReplaceComponentname(OldName, NewName,
              GetUpperEvents(3) + '|');
          except
            on E: Exception do
            begin
              ErrorMsg(E.Message);
              Tabs[I] := OldTab;
            end;
          end;
      end;
    end;
  end;
  PlaceTabs;
  FObjectInspector.RefreshCBObjects;
end;

function TJTabbedPane.GetUpperEvents(ShowEvents: Integer): string;
begin
  Result := MakeUpperEvents(GetEvents(ShowEvents));
end;

function TJTabbedPane.MakeUpperEvents(Eve: string): string;
begin
  if Eve <> '' then
  begin
    Eve[1] := UpCase(Eve[1]);
    for var I := 1 to Length(Eve) do
      if (Eve[I] = '|') and (I < Length(Eve)) then
        Eve[I + 1] := UpCase(Eve[I + 1]);
  end;
  Result := Eve;
end;

procedure TJTabbedPane.Paint;
var
  TextHeight, TextWidth, X1Pos, Y1Pos: Integer;
  Points: array [0 .. 4] of TPoint;

  function ShowHorTab(X1Pos, Y1Pos: Integer; const Str: string;
    TopCorner: Boolean): Integer;
  var
    DeltaX, DeltaY, Posi6: Integer;
  begin
    TextWidth := Canvas.TextWidth(Str);
    DeltaX := Min(TextWidth + 2 * CPaddingH, Width - 2);
    DeltaY := TextHeight + 2 * CPaddingV;
    Posi6 := PPIScale(6);
    if TopCorner then
    begin
      Points[0] := Point(X1Pos, Y1Pos + Posi6);
      Points[1] := Point(X1Pos, Y1Pos + DeltaY);
      Points[2] := Point(X1Pos + DeltaX, Y1Pos + DeltaY);
      Points[3] := Point(X1Pos + DeltaX, Y1Pos);
      Points[4] := Point(X1Pos + Posi6, Y1Pos);
    end
    else
    begin
      Points[0] := Point(X1Pos, Y1Pos);
      Points[1] := Point(X1Pos, Y1Pos + DeltaY - Posi6);
      Points[2] := Point(X1Pos + Posi6, Y1Pos + DeltaY);
      Points[3] := Point(X1Pos + DeltaX, Y1Pos + DeltaY);
      Points[4] := Point(X1Pos + DeltaX, Y1Pos);
    end;
    if X1Pos < Width - PPIScale(10) then
    begin
      Canvas.Polygon(Points);
      Canvas.TextOut(X1Pos + CPaddingH, Y1Pos + CPaddingV, Str);
    end;
    Result := X1Pos + DeltaX;
  end;

  procedure ShowVerTab(X1Pos, Y1Pos: Integer; const Str: string;
    LeftCorner: Boolean);
  var
    DeltaY, Wint, Posi6: Integer;
  begin
    Wint := Canvas.TextWidth(Str);
    DeltaY := TextHeight + 2 * CPaddingV;
    Posi6 := PPIScale(6);
    if LeftCorner then
    begin
      Points[0] := Point(X1Pos, Y1Pos + Posi6);
      Points[1] := Point(X1Pos, Y1Pos + DeltaY);
      Points[2] := Point(X1Pos + TextWidth, Y1Pos + DeltaY);
      Points[3] := Point(X1Pos + TextWidth, Y1Pos);
      Points[4] := Point(X1Pos + Posi6, Y1Pos);
    end
    else
    begin
      Points[0] := Point(X1Pos, Y1Pos);
      Points[1] := Point(X1Pos, Y1Pos + DeltaY);
      Points[2] := Point(X1Pos + TextWidth, Y1Pos + DeltaY);
      Points[3] := Point(X1Pos + TextWidth, Y1Pos + Posi6);
      Points[4] := Point(X1Pos + TextWidth - Posi6, Y1Pos);
    end;
    if (Y1Pos + DeltaY < Height - PPIScale(10)) then
    begin
      Canvas.Polygon(Points);
      Canvas.TextOut(X1Pos + (TextWidth - Wint) div 2, Y1Pos + CPaddingV, Str);
    end;
  end;

  procedure SetBrushColor(Num: Integer);
  begin
    if (Num = FSelectedIndex) or ((Num = 0) and (FSelectedIndex = -1)) then
      Canvas.Brush.Color := SelectionColor
    else if Background = SelectionColor then
      Canvas.Brush.Color := DefaultBackground
    else
      Canvas.Brush.Color := Background;
  end;

begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := DarkShadow;
  Canvas.Brush.Color := Background;
  TextHeight := Canvas.TextHeight('Hg');
  if FTabs.Count > 0 then
    case FTabPlacement of
      UJTabbedPane.TOP:
        begin
          Canvas.Rectangle(Rect(0, TextHeight + 2 * CPaddingV, Width, Height));
          X1Pos := 2;
          Y1Pos := 0;
          for var I := 0 to FTabs.Count - 1 do
          begin
            SetBrushColor(I);
            X1Pos := ShowHorTab(X1Pos, Y1Pos, FTabs[I], True);
          end;
        end;
      UJTabbedPane.BOTTOM:
        begin
          Canvas.Rectangle(Rect(0, 0, Width, Height - TextHeight - 2 *
            CPaddingV));
          X1Pos := 2;
          Y1Pos := Height - TextHeight - 2 * CPaddingV - 1;
          for var I := 0 to FTabs.Count - 1 do
          begin
            SetBrushColor(I);
            X1Pos := ShowHorTab(X1Pos, Y1Pos, FTabs[I], False);
          end;
        end;
      UJTabbedPane.LEFT:
        begin
          TextWidth := MaxTab + 2 * CPaddingH;
          Canvas.Rectangle(Rect(TextWidth, 0, Width, Height));
          X1Pos := 0;
          Y1Pos := 2;
          for var I := 0 to FTabs.Count - 1 do
          begin
            SetBrushColor(I);
            ShowVerTab(X1Pos, Y1Pos, FTabs[I], True);
            Y1Pos := Y1Pos + TextHeight + 2 * CPaddingV;
          end;
        end;
      UJTabbedPane.RIGHT:
        begin
          TextWidth := MaxTab + 2 * CPaddingH;
          Canvas.Rectangle(Rect(0, 0, Width - TextWidth, Height));
          X1Pos := Width - TextWidth - 1;
          Y1Pos := 2;
          for var I := 0 to FTabs.Count - 1 do
          begin
            SetBrushColor(I);
            ShowVerTab(X1Pos, Y1Pos, FTabs[I], False);
            Y1Pos := Y1Pos + TextHeight + 2 * CPaddingV;
          end;
        end;
    end
  else
  begin
    Canvas.Brush.Color := DefaultBackground;
    Canvas.Rectangle(Rect(0, 0, Width, Height));
  end;
end;

function TJTabbedPane.GetTabs: TStrings;
begin
  Result := FTabs;
end;

function TJTabbedPane.MaxTab: Integer;
var
  Max, TextWidth: Integer;
begin
  Max := 0;
  for var I := 0 to FTabs.Count - 1 do
  begin
    TextWidth := Canvas.TextWidth(FTabs[I]);
    if TextWidth > Max then
      Max := TextWidth;
  end;
  Result := Max;
end;

procedure TJTabbedPane.PlaceTabs;
var
  TextHeight, LVal: Integer;
  APanel: TJPanel;
begin
  CanvasFontAssign;
  TextHeight := Canvas.TextHeight('Hg') + 3 * CPaddingV;
  LVal := MaxTab + 2 * CPaddingH + CPaddingV;
  for var I := 0 to ControlCount - 1 do
  begin
    APanel := (Controls[I] as TJPanel);
    APanel.SetBounds(2, 3, Width - 4, Height - 3);
    case FTabPlacement of
      UJTabbedPane.TOP:
        begin
          APanel.Top := TextHeight;
          APanel.Height := APanel.Height - TextHeight;
        end;
      UJTabbedPane.LEFT:
        begin
          APanel.Left := LVal;
          APanel.Width := APanel.Width - LVal;
          APanel.Height := APanel.Height - 4;
        end;
      UJTabbedPane.BOTTOM:
        begin
          APanel.Height := APanel.Height - TextHeight;
        end;
      UJTabbedPane.RIGHT:
        begin
          APanel.Width := APanel.Width - LVal;
          APanel.Height := APanel.Height - 4;
          APanel.Left := 4;
        end;
    end;
  end;
  SetSelectedIndex(FSelectedIndex);
end;

procedure TJTabbedPane.SetTabs(Strings: TStrings);
begin
  FTabsOld.Text := FTabs.Text;
  if FTabs.Text <> Strings.Text then
  begin
    FTabs.Assign(Strings);
    if FSelectedIndex >= FTabs.Count then
      SelectedIndex := FTabs.Count - 1;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetTabPlacement(AValue: TTabPlacement);
begin
  if AValue <> FTabPlacement then
  begin
    FTabPlacement := AValue;
    PlaceTabs;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetTabLayoutPolicy(AValue: TTabLayoutPolicy);
begin
  if AValue <> FTabLayoutPolicy then
  begin
    FTabLayoutPolicy := AValue;
    Invalidate;
  end;
end;

procedure TJTabbedPane.SetSelectedIndex(AValue: Integer);
var
  Int: Integer;
  Str: string;
begin
  if AValue < ControlCount then
  begin
    FSelectedIndex := AValue;
    if FSelectedIndex = -1 then
      Int := 0
    else
      Int := FSelectedIndex;
    for var J := 0 to Tabs.Count - 1 do
      if (J < ControlCount) and (Controls[J] is TJPanel) then
      begin
        Str := (Controls[J] as TJPanel).Name;
        if Str = Self.Name + 'TabPanel' + Tabs[Int] then
        begin
          Controls[J].BringToFront;
          Controls[J].Invalidate;
          Invalidate;
          Break;
        end;
      end;
  end;
end;

procedure TJTabbedPane.Delete(Control: TControl);
var
  Int: Integer;
  StringList: TStringList;
begin
  Int := (Control as TJPanel).getIndex;
  StringList := TStringList.Create;
  StringList.Text := FTabs.Text;
  if (0 <= Int) and (Int < StringList.Count) then
  begin
    StringList.Delete(Int);
    SetTabs(StringList);
  end;
  FreeAndNil(StringList);
end;

procedure TJTabbedPane.ResizePane(Sender: TObject);
begin
  PlaceTabs;
end;

end.
