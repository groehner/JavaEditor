unit UWatches;

interface

uses
  Forms, StdCtrls, Buttons, Vcl.Controls, System.Classes,
  Vcl.BaseImageCollection, SVGIconImageCollection, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList;

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
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
      NewDPI: Integer);
  public
    procedure DeleteAll;
    procedure Delete(const s: string);
    procedure Insert(s: string);
    procedure SaveWindow;
  end;

var
  FWatches: TFWatches = nil;

implementation

uses SysUtils, Graphics, JvGnugettext, UConfiguration, UDebugger, UUtils, UJava;

{$R *.dfm}

procedure TFWatches.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  var Anzahl:= FConfiguration.ReadIntegerU('Watches', 'Count', 0);
  for var i:= 1 to Anzahl do begin
    var s:= FConfiguration.ReadStringU('Watches', 'Watch' + IntToStr(i), '');
    if s <> '' then LBWatches.Items.Add(s);
  end;
end;

procedure TFWatches.BAddClick(Sender: TObject);
begin
  Insert(EWatch.Text);
  EWatch.Text:= '';
end;

procedure TFWatches.SBUpClick(Sender: TObject);
begin
  var i:= LBWatches.ItemIndex;
  if (0 < i) and (i < LBWatches.Count) then begin
    LBWatches.Items.Exchange(i, i-1);
    LBWatches.ItemIndex:= i-1;
    myDebugger.Watch;
  end;
end;

procedure TFWatches.SBDownClick(Sender: TObject);
begin
  var i:= LBWatches.ItemIndex;
  if (-1 < i) and (i < LBWatches.Count-1) then begin
    LBWatches.Items.Exchange(i, i+1);
    LBWatches.ItemIndex:= i+1;
    myDebugger.Watch;
  end;
end;

procedure TFWatches.BDeleteClick(Sender: TObject);
begin
  var i:= LBWatches.ItemIndex;
  if i >= 0 then
    Delete(LBWatches.Items[i]);
end;

procedure TFWatches.Insert(s: string);
begin
  s:= trim(s);
  if s = '' then exit;
  if LBWatches.Items.IndexOf(s) = -1 then begin
    LBWatches.Items.Add(s);
    myDebugger.Watch;
  end;
end;

procedure TFWatches.Delete(const s: string);
begin
  for var i:= 0 to LBWatches.Items.Count - 1 do begin
    var ueb:= LBWatches.Items[i];
    if (ueb = s) or    // also clears tmp[0] for tmp
       ((Pos(s, ueb) = 1) and (Pos('[', ueb) = length(s) + 1))
    then begin
      LBWatches.Items.delete(i);
      myDebugger.Watch;
      exit;
    end;
  end;
end;

procedure TFWatches.SaveWindow;
begin
  FConfiguration.WriteIntegerU('Watches', 'Count', LBWatches.Items.Count);
  for var i:= 1 to LBWatches.Items.Count do
    FConfiguration.WriteStringU('Watches', 'Watch' + IntToStr(i), LBWatches.Items[i-1]);
end;

procedure TFWatches.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI,
  NewDPI: Integer);
begin
  ILWatches.SetSize(PPIScale(18), PPIScale(18));
end;

procedure TFWatches.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveWindow;
  CanClose:= true;
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
