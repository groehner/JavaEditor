unit UDlgEvaluate;

interface

uses
  Forms,
  StdCtrls,
  Vcl.Controls,
  System.Classes;

type
  TFEvaluate = class(TForm)
    LExpression: TLabel;
    EExpression: TEdit;
    LResult: TLabel;
    MEvaluate: TMemo;
    BEvaluate: TButton;
    BWatch: TButton;
    BClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BEvaluateClick(Sender: TObject);
    procedure BCloseClick(Sender: TObject);
    procedure BWatchClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  FEvaluate: TFEvaluate = nil;

implementation

uses
  SysUtils,
  JvGnugettext,
  UDebugger,
  UWatches;

{$R *.dfm}

procedure TFEvaluate.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFEvaluate.BEvaluateClick(Sender: TObject);
begin
  MyDebugger.NewCommand(12, 'eval ' + EExpression.Text);
end;

procedure TFEvaluate.BWatchClick(Sender: TObject);
begin
  FWatches.Insert(Trim(EExpression.Text));
end;

procedure TFEvaluate.BCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFEvaluate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

end.
