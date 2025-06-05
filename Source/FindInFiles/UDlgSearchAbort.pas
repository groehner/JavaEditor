unit UDlgSearchAbort;

interface

uses
  Forms,
  StdCtrls,
  System.Classes,
  Vcl.Controls;

type
  TFSearchAbort = class(TForm)
    BSearchAbort: TButton;
    procedure BSearchAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure ShowWith(Str: string);
  end;

implementation

uses
  SysUtils,
  JvGnugettext,
  UGrepResults;

{$R *.dfm}

procedure TFSearchAbort.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFSearchAbort.BSearchAbortClick(Sender: TObject);
begin
  myGrepResults.Aborting:= True;
end;

procedure TFSearchAbort.ShowWith(Str: string);
begin
  Str:= Format(_('Cancel search for "%s"'), [Str]);
  BSearchAbort.Caption:= Str;
  var AWidth:= Canvas.TextWidth(Str) + 16;
  BSearchAbort.Width:= AWidth;
  Width:= AWidth + 16;
  Show;
end;

end.
