unit UDlgParameter;

interface

uses
  Forms, Dialogs, StdCtrls, Vcl.Controls, System.Classes;

type
  TFParameterDialog = class(TForm)
    LParameter: TLabel;
    EParameter: TEdit;
    BOK: TButton;
    BDelete: TButton;
    BCancel: TButton;
    LStartClass: TLabel;
    EStartClass: TEdit;
    BSelect: TButton;
    ODParaSelect: TOpenDialog;
    procedure BDeleteClick(Sender: TObject);
    procedure BSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Pathname: string;
  end;

implementation

uses SysUtils, JvGnugettext,
     UJava, UEditorForm, UConfiguration, UUtils;

{$R *.DFM}

procedure TFParameterDialog.BDeleteClick(Sender: TObject);
begin
  EStartClass.Text:= '';
  EParameter.Text:= '';
end;

procedure TFParameterDialog.BSelectClick(Sender: TObject);
  var Editform: TFEditForm;
begin
  with ODParaSelect do begin
    InitialDir:= ExtractFilepath(Pathname);
    if InitialDir = '' then
      InitialDir:= FConfiguration.Sourcepath;
    if not SysUtils.DirectoryExists(InitialDir) then
      InitialDir:= 'C:\';
    Filename:= '';
    Filter:= '*.java|*.java';
    if Execute then
     if (Filename <> '') and hasJavaExtension(Filename) then begin
       LockWindow(FJava.Handle);
       EditForm:= FJava.OpenEditForm(Filename, true);
       if assigned(EditForm) and EditForm.hasMainInModel then begin
         EStartClass.Text:= Filename;
         FConfiguration.JavaStartClass:= Filename;
         FConfiguration.JavaStartKlasseIsApplet:= EditForm.isApplet;
         if not EditForm.Visible then EditForm.Close;
       end else begin
         EStartClass.Text:= '';
         FConfiguration.JavaStartClass:= '';
         ErrorMsg(Filename + ' has no main-method');  // TODO LNG
       end;
       UnlockWindow;
     end;
  end;
end;

procedure TFParameterDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

end.
