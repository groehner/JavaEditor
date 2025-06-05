unit UWatches;

interface

uses
  Forms,
  StdCtrls,
  Buttons,
  System.Classes,
  System.ImageList,
  Vcl.Controls,
  Vcl.BaseImageCollection,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  SVGIconImageCollection;

type
  TFWatches = class(TForm)
    LNewWatch: TLabel;
    EWatch: TEdit;
    BAdd: TButton;
    LWatches: TLabel;
    LBWatches: TListBox;
    BDelete: TButton;
    BClose: TButton;
    SBUp: TSpeedButton;
    SBDown: TSpeedButton;
    icWatches: TSVGIconImageCollection;
    ILWatches: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure BAddClick(Sender: TObject);
    procedure SBUpClick(Sender: TObject);
    procedure SBDownClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject;
      OldDPI, NewDPI: Integer);
  public
    procedure DeleteAll;
    procedure Delete(const Str: string);
    procedure Insert(Str: string);
    procedure SaveWindow;
  end;

var
  FWatches: TFWatches = nil;

implementation

uses
  SysUtils,
  JvGnugettext,
  UConfiguration,
  UDebugger,
  UUtils;

{$R *.dfm}

procedure TFWatches.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  var
  Count := FConfiguration.ReadIntegerU('Watches', 'Count', 0);
  for var I := 1 to Count do
  begin
    var
    Str := FConfiguration.ReadStringU('Watches', 'Watch' + IntToStr(I), '');
    if Str <> '' then
      LBWatches.Items.Add(Str);
  end;
end;

procedure TFWatches.BAddClick(Sender: TObject);
begin
  Insert(EWatch.Text);
  EWatch.Text := '';
end;

procedure TFWatches.SBUpClick(Sender: TObject);
begin
  var
  Pos := LBWatches.ItemIndex;
  if (0 < Pos) and (Pos < LBWatches.Count) then
  begin
    LBWatches.Items.Exchange(Pos, Pos - 1);
    LBWatches.ItemIndex := Pos - 1;
    MyDebugger.Watch;
  end;
end;

procedure TFWatches.SBDownClick(Sender: TObject);
begin
  var
  Pos := LBWatches.ItemIndex;
  if (-1 < Pos) and (Pos < LBWatches.Count - 1) then
  begin
    LBWatches.Items.Exchange(Pos, Pos + 1);
    LBWatches.ItemIndex := Pos + 1;
    MyDebugger.Watch;
  end;
end;

procedure TFWatches.BDeleteClick(Sender: TObject);
begin
  var
  Pos := LBWatches.ItemIndex;
  if Pos >= 0 then
    Delete(LBWatches.Items[Pos]);
end;

procedure TFWatches.Insert(Str: string);
begin
  Str := Trim(Str);
  if Str = '' then
    Exit;
  if LBWatches.Items.IndexOf(Str) = -1 then
  begin
    LBWatches.Items.Add(Str);
    MyDebugger.Watch;
  end;
end;

procedure TFWatches.Delete(const Str: string);
begin
  for var I := 0 to LBWatches.Items.Count - 1 do
  begin
    var
    Ueb := LBWatches.Items[I];
    if (Ueb = Str) or // also clears tmp[0] for tmp
      ((Pos(Str, Ueb) = 1) and (Pos('[', Ueb) = Length(Str) + 1)) then
    begin
      LBWatches.Items.Delete(I);
      MyDebugger.Watch;
      Exit;
    end;
  end;
end;

procedure TFWatches.SaveWindow;
begin
  FConfiguration.WriteIntegerU('Watches', 'Count', LBWatches.Items.Count);
  for var I := 1 to LBWatches.Items.Count do
    FConfiguration.WriteStringU('Watches', 'Watch' + IntToStr(I),
      LBWatches.Items[I - 1]);
end;

procedure TFWatches.FormAfterMonitorDpiChanged(Sender: TObject;
  OldDPI, NewDPI: Integer);
begin
  ILWatches.SetSize(PPIScale(18), PPIScale(18));
end;

procedure TFWatches.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveWindow;
  CanClose := True;
end;

procedure TFWatches.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFWatches.DeleteAll;
begin
  LBWatches.Items.Clear;
end;

end.
