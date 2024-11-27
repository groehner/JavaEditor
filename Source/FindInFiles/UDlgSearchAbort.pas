unit UDlgSearchAbort;

interface

uses
  Forms, StdCtrls, System.Classes, Vcl.Controls;

type
  TFSearchAbort = class(TForm)
    BSearchAbort: TButton;
    procedure BSearchAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure ShowWith(s: string);
  end;

implementation

uses Graphics, SysUtils, UGrepResults, JvGnugettext;

{$R *.dfm}

procedure TFSearchAbort.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFSearchAbort.BSearchAbortClick(Sender: TObject);
begin
  myGrepResults.Aborting:= true;
end;

procedure TFSearchAbort.ShowWith(s: string);
begin
  s:= Format(_('Cancel search for "%s"'), [s]);
  BSearchAbort.Caption:= s;
  var w:= Canvas.TextWidth(s) + 16;
  BSearchAbort.Width:= w;
  Width:= w + 16;
  Show;
end;

end.
