unit UDlgParameter;

interface

uses
  Forms,
  Dialogs,
  StdCtrls,
  Vcl.Controls,
  System.Classes;

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
  private
    FPathname: string;
  public
    property Pathname: string read FPathname write FPathname;
  end;

implementation

uses
  SysUtils,
  JvGnugettext,
  UJava,
  UEditorForm,
  UConfiguration,
  UUtils;

{$R *.DFM}

procedure TFParameterDialog.BDeleteClick(Sender: TObject);
begin
  EStartClass.Text := '';
  EParameter.Text := '';
end;

procedure TFParameterDialog.BSelectClick(Sender: TObject);
var
  EditForm: TFEditForm;
begin
  with ODParaSelect do
  begin
    InitialDir := ExtractFilePath(Pathname);
    if InitialDir = '' then
      InitialDir := FConfiguration.Sourcepath;
    if not SysUtils.DirectoryExists(InitialDir) then
      InitialDir := 'C:\';
    FileName := '';
    Filter := '*.java|*.java';
    if Execute then
      if (FileName <> '') and HasJavaExtension(FileName) then
      begin
        LockWindow(FJava.Handle);
        EditForm := FJava.OpenEditForm(FileName, True);
        if Assigned(EditForm) and EditForm.HasMainInModel then
        begin
          EStartClass.Text := FileName;
          FConfiguration.JavaStartClass := FileName;
          FConfiguration.JavaStartClassIsApplet := EditForm.IsApplet;
          if not EditForm.Visible then
            EditForm.Close;
        end
        else
        begin
          EStartClass.Text := '';
          FConfiguration.JavaStartClass := '';
          ErrorMsg(FileName + _(' has no main-method')); // TODO LNG
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
