unit UFXListView;

interface

uses
  Classes,
  UFXComponents;

type

  TFXListViewOrientation = (VERTICAL, HORIZONTAL);

  TFXListViewSelectionMode = (MULTIPLE, SINGLE);

  TFXListView = class(TFXControl)
  private
    FEditable: Boolean;
    FFixedCellSize: Integer;
    FItems: TStrings;
    FOrientation: TFXListViewOrientation;
    FSelectionMode: TFXListViewSelectionMode;

    FeditStart: string;
    FeditCommit: string;
    FeditCancel: string;
    procedure SetItems(AItems: TStrings);
    procedure SetFixedCellSize(AValue: Integer);
    procedure SetOrientation(AValue: TFXListViewOrientation);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure SetAttribute(Attr, Value, Typ: string); override;
    function GetEvents(ShowEvents: Integer): string; override;
    function GetAttributes(ShowAttributes: Integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property Editable: Boolean read FEditable write FEditable;
    property FixedCellSize: Integer read FFixedCellSize write SetFixedCellSize;
    property Items: TStrings read FItems write SetItems;
    property Orientation: TFXListViewOrientation read FOrientation
      write SetOrientation;
    property SelectionMode: TFXListViewSelectionMode read FSelectionMode
      write FSelectionMode;

    property editStart: string read FeditStart write FeditStart;
    property editCommit: string read FeditCommit write FeditCommit;
    property editCancel: string read FeditCancel write FeditCancel;
  end;

implementation

uses
  Windows,
  SysUtils,
  Math,
  Graphics,
  UUtils;

{ --- TFXListView -------------------------------------------------------------- }

constructor TFXListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag := +109;
  PrefWidth := 120;
  PrefHeight := 80;
  Background := DefaultBorderColor;
  FItems := TStringList.Create;
  FItems.Text := DefaultItems;
  FOrientation := VERTICAL;
  FSelectionMode := SINGLE;
  FFixedCellSize := -1;
  JavaType := 'ListView';
end;

destructor TFXListView.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFXListView.Paint;
var
  XPos, YPos, DeltaX, DeltaY, TextHeight: Integer;
  Str: string;
  Rect1: TRect;
begin
  CanvasFontAssign;
  Canvas.Font.Color := Foreground;
  Canvas.Pen.Color := Background;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  TextHeight := Canvas.TextHeight('Hg');
  if FFixedCellSize < 0 then
    DeltaY := PPIScale(23)
  else
    DeltaY := PPIScale(FFixedCellSize);

  Canvas.Brush.Style := bsSolid; // filled rectangles
  XPos := 0;
  YPos := 0;
  for var I := 0 to FItems.Count - 1 do
  begin
    Str := FItems[I];
    if I mod 2 = 0 then
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
    end
    else
    begin
      Canvas.Pen.Color := $F9F9F9;
      Canvas.Brush.Color := $F9F9F9;
    end;

    case Orientation of
      VERTICAL:
        begin // 1
          // 2
          // 3
          Rect1 := Rect(1, YPos, Width - 1, YPos + DeltaY);
          if I mod 2 = 1 then
            Canvas.Rectangle(Rect1);
          Canvas.TextRect(Rect1, PPIScale(6), YPos + (DeltaY - TextHeight)
            div 2, Str);
          Inc(YPos, DeltaY);
        end;

      HORIZONTAL:
        begin // 1 2 3
          DeltaX := Max(TextHeight + PPIScale(8),
            Canvas.TextWidth(Str + '    '));
          Rect1 := Rect(XPos, 1, XPos + DeltaX, Height - 1);
          if I mod 2 = 1 then
            Canvas.Rectangle(Rect1);
          Canvas.TextRect(Rect1, XPos + PPIScale(6),
            (Height - TextHeight) div 2, Str);
          Inc(XPos, DeltaX);
        end;
    end;
  end;
end;

procedure TFXListView.SetFixedCellSize(AValue: Integer);
begin
  if AValue <> FFixedCellSize then
  begin
    FFixedCellSize := AValue;
    Invalidate;
  end;
end;

procedure TFXListView.SetOrientation(AValue: TFXListViewOrientation);
begin
  if AValue <> FOrientation then
  begin
    FOrientation := AValue;
    Invalidate;
  end;
end;

procedure TFXListView.SetItems(AItems: TStrings);
begin
  if AItems.Text <> FItems.Text then
  begin
    FItems.Assign(AItems);
    Invalidate;
  end;
end;

procedure TFXListView.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ListView<String> ' + Name + ' = new ListView<>();'
    + CrLf + GetIndentation + '  private ObservableList<String> ' + Name +
    'ObservableList = ' + CrLf + GetIndentation +
    '          FXCollections.observableArrayList();');
  DefaultComponent;
  MakeAttribut('Items', Name + 'ObservableList');
  MakeList;
end;

procedure TFXListView.SetAttribute(Attr, Value, Typ: string);
var
  Str, Key: string;
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectionMode' then
  begin
    Key := Name + '.getSelectionModel()';
    Str := Key + '.setSelectionMode(SelectionMode.' + Value + ');';
    SetAttributValue(Key, Str);
  end
  else if Attr = 'Orientation' then
  begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end
  else
    inherited;
end;

procedure TFXListView.MakeList;
begin
  FPartner.DeleteComponent(Name + 'ObservableList.add(');
  for var I := 0 to FItems.Count - 1 do
  begin
    var
    Str := Name + 'ObservableList.add("' + FItems[I] + '");';
    SetAttributValue('___XXX___', Str);
  end;
end;

function TFXListView.GetEvents(ShowEvents: Integer): string;
const
  Events = '|editStart|editCommit|editCancel';
begin
  Result := Events + inherited GetEvents(ShowEvents);
end;

function TFXListView.GetAttributes(ShowAttributes: Integer): string;
const
  Attributes1 = '|Editable|Items|Orientation|SelectionMode';
  Attributes2 = Attributes1 + '|FixedCellSize';
begin
  if ShowAttributes = 1 then
    Result := Attributes1 + inherited GetAttributes(ShowAttributes)
  else
    Result := Attributes2 + inherited GetAttributes(ShowAttributes);
end;

procedure TFXListView.Rename(const OldName, NewName, Events: string);
  procedure Rename(var Name: string);
  begin
    if Name <> '' then
      Name := NewName + UUtils.Right(Name, Length(OldName) + 1);
  end;

begin
  inherited;
  Rename(FeditStart);
  Rename(FeditCommit);
  Rename(FeditCancel);
  FPartner.ReplaceWord(OldName + 'ObservableList',
    NewName + 'ObservableList', True);
end;

procedure TFXListView.DeleteComponent;
var
  Int: Integer;
begin
  inherited;
  FPartner.DeleteAttribute('private ListView<String> ' + Name);
  Int := FPartner.GetLineNumberWith('private ObservableList<String> ' + Name +
    'ObservableList');
  FPartner.DeleteBlock(Int, Int + 1);
  FPartner.DeleteComponent(Name + 'ObservableList.add');
end;

end.
