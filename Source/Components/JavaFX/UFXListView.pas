unit UFXListView;

interface

uses
  Classes, Graphics, UFXComponents;

type

  TFXListViewOrientation = (VERTICAL, HORIZONTAL);

  TFXListViewSelectionMode = (MULTIPLE, SINGLE);

  TFXListView = class (TFXControl)
  private
    FEditable: boolean;
    FFixedCellSize: integer;
    FItems: TStrings;
    FOrientation: TFXListViewOrientation;
    FSelectionMode: TFXListViewSelectionMode;

    FeditStart: string;
    FeditCommit: string;
    FeditCancel: string;
    //FscrollTo: string;
    procedure setItems(aItems: TStrings);
    procedure setFixedCellSize(aValue: integer);
    procedure setOrientation(aValue: TFXListViewOrientation);
    procedure MakeList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure NewControl; override;
    procedure setAttribute(Attr, Value, Typ: string); override;
    function getEvents(ShowEvents: integer): string; override;
    function getAttributes(ShowAttributes: integer): string; override;
    procedure Rename(const OldName, NewName, Events: string); override;
    procedure DeleteComponent; override;
  published
    property Editable: boolean read FEditable write FEditable;
    property FixedCellSize: integer read FFixedCellSize write SetFixedCellSize;
    property Items: TStrings read FItems write setItems;
    property Orientation: TFXListViewOrientation read FOrientation write setOrientation;
    property SelectionMode: TFXListViewSelectionMode read FSelectionMode write FSelectionMode;

    property editStart: string read FeditStart write FeditStart;
    property editCommit: string read FeditCommit write FeditCommit;
    property editCancel: string read FeditCancel write FeditCancel;
    //property scrollTo: string read FscrollTo write FscrollTo;
  end;

implementation

uses Windows, SysUtils, Math, UUtils;

{--- TFXListView --------------------------------------------------------------}

constructor TFXListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Tag:= +109;
  PrefWidth := 120;
  PrefHeight:= 80;
  Background:= DefaultBorderColor;
  FItems:= TStringList.Create;
  FItems.Text:= defaultItems;
  FOrientation:= VERTICAL;
  FSelectionMode:= SINGLE;
  FFixedCellSize:= -1;
  JavaType:= 'ListView';
end;

destructor TFXListView.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TFXListView.Paint;
  var x, y, dx, dy, th, i: integer;
      s: string; R1: TRect;
begin
  CanvasFontAssign;
  Canvas.Font.Color:= Foreground;
  Canvas.Pen.Color:= Background;
  Canvas.Brush.Color:= clWhite;
  Canvas.Rectangle(Rect(0, 0, Width, Height));

  th:= Canvas.TextHeight('Hg');
  if FFixedCellSize < 0
    then dy:= PPIScale(23)
    else dy:= PPIScale(FFixedCellSize);

  Canvas.Brush.Style:= bsSolid;  // filled rectangles
  x:= 0;
  y:= 0;
  for i:= 0 to FItems.Count - 1 do begin
    s:= FItems.Strings[i];
    if i mod 2 = 0 then begin
      Canvas.Pen.Color:= clWhite;
      Canvas.Brush.Color:= clWhite;
    end else begin
      Canvas.Pen.Color:= $F9F9F9;
      Canvas.Brush.Color:= $F9F9F9;
    end;

    case Orientation of
      VERTICAL: begin     // 1
                          // 2
                          // 3
        R1:= Rect(1, y, width-1, y + dy);
        if i mod 2 = 1 then
          Canvas.Rectangle(R1);
        Canvas.TextRect(R1, PPIScale(6), y + (dy - th) div 2, s);
        inc(y, dy);
      end;

      HORIZONTAL: begin       // 1 2 3
        dx:= max(th + PPIScale(8), Canvas.TextWidth(s + '    '));
        R1:= Rect(x, 1, x + dx, height-1);
        if i mod 2 = 1 then
          Canvas.Rectangle(R1);
        Canvas.TextRect(R1, x + PPIScale(6), (height-th) div 2, s);
        inc(x, dx);
      end;
    end;
  end;
end;

procedure TFXListView.setFixedCellSize(aValue: integer);
begin
  if aValue <> FFixedCellSize then begin
    FFixedCellSize:= aValue;
    Invalidate;
  end;
end;

procedure TFXListView.setOrientation(aValue: TFXListViewOrientation);
begin
  if aValue <> FOrientation then begin
    FOrientation:= aValue;
    Invalidate;
  end;
end;

procedure TFXListView.setItems(aItems: TStrings);
begin
  if aItems.Text <> FItems.Text then begin
    FItems.Assign(aItems);
    Invalidate;
  end;
end;

procedure TFXListView.NewControl;
begin
  InsertImport('javafx.collections.*');
  InsertNewVariable('private ListView<String> ' + Name + ' = new ListView<>();' + CrLf + GetIndentation +
                    '  private ObservableList<String> ' + Name + 'ObservableList = ' + CrLf + GetIndentation +
                    '          FXCollections.observableArrayList();');
  DefaultComponent;
  MakeAttribut('Items', Name + 'ObservableList');
  MakeList;
end;

procedure TFXListView.SetAttribute(Attr, Value, Typ: string);
  var s, key: string;
begin
  if Attr = 'Items' then
    MakeList
  else if Attr = 'SelectionMode' then begin
    key:= Name + '.getSelectionModel()';
    s:= key + '.setSelectionMode(SelectionMode.' + Value + ');';
    setAttributValue(key, s);
  end else if Attr = 'Orientation' then begin
    InsertImport('javafx.geometry.*');
    MakeAttribut(Attr, 'Orientation.' + Value);
  end else
    inherited
end;

procedure TFXListView.MakeList;
begin
  Partner.DeleteComponent(Name + 'ObservableList.add(');
  for var i:= 0 to FItems.Count - 1 do begin
    var s:= Name + 'ObservableList.add("' + FItems[i] + '");';
    setAttributValue('___XXX___', s);
  end
end;

function TFXListView.getEvents(ShowEvents: integer): string;
  const Events = '|editStart|editCommit|editCancel';
begin
  Result:= Events + inherited getEvents(ShowEvents);
end;

function TFXListView.getAttributes(ShowAttributes: integer): string;
  const Attributes1 = '|Editable|Items|Orientation|SelectionMode';
        Attributes2 = Attributes1 + '|FixedCellSize';
begin
  if ShowAttributes = 1
    then Result:= Attributes1 + inherited getAttributes(ShowAttributes)
    else Result:= Attributes2 + inherited getAttributes(ShowAttributes)
end;

procedure TFXListView.Rename(const OldName, NewName, Events: string);
  procedure rename(var name: string);
  begin
    if name <> '' then
      name:= NewName + UUtils.Right(name, Length(OldName) + 1);
  end;

begin
  inherited;
  rename(FeditStart);
  rename(FeditCommit);
  rename(FeditCancel);
  Partner.ReplaceWord(OldName + 'ObservableList' , NewName + 'ObservableList', true);
end;

procedure TFXListView.DeleteComponent;
  var i: integer;
begin
  inherited;
  //Partner.DeleteAttribute('private ObservableList<String> ' + Name + 'ObservableList');
  Partner.DeleteAttribute('private ListView<String> ' + Name);
  i:= Partner.getLineNumberWith('private ObservableList<String> ' + Name + 'ObservableList');
  Partner.DeleteBlock(i, i+1);
  Partner.DeleteComponent(Name + 'ObservableList.add');
end;

end.
