unit UDlgOpenFolder;

interface

{$WARN UNIT_PLATFORM OFF}

uses
  Forms, StdCtrls, ShellCtrls, Vcl.Controls, System.Classes;

type

  TFOpenFolderDialog = class(TForm)
    LFiletype: TLabel;
    BOK: TButton;
    BCancel: TButton;
    CBFiletype: TComboBox;
    CBWithSubFolder: TCheckBox;
    CBPath: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure CBPathChange(Sender: TObject);
  public
    PathTreeView: TShellTreeView;
    destructor Destroy; override;
 end;

implementation

uses SysUtils, JvGnugettext, UConfiguration, UUtils;

{$R *.dfm}

procedure TFOpenFolderDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  PathTreeView:= TShellTreeView.Create(self);
  CBPath.Items.Text:=
    LoadComboBoxItems(FConfiguration.ReadStringU('Java', 'OpenFolderFormItems', ''));
  if (CBPath.Items.Count > 0) and DirectoryExists(CBPath.Items[0])
    then CBPath.Text:= CBPath.Items[0]
    else CBPath.Text:= FConfiguration.Sourcepath;
  with PathTreeView do begin
    Parent:= Self;
    SetBounds(8, 8, 380, 270);
    HideSelection:= False;
    Path:= CBPath.Text;
    AutoRefresh:= true;
  end;
end;

procedure TFOpenFolderDialog.CBPathChange(Sender: TObject);
begin
  if DirectoryExists(CBPath.Text) then begin
    ComboBoxAdd(CBPath);
    FConfiguration.WriteStringU('Java', 'OpenFolderFormItems',
      SaveComboBoxItems(CBPath.Items.Text));
    PathTreeView.Path:= CBPath.Text;
    PathTreeView.Refresh(PathtreeView.Selected);
  end;
end;

destructor TFOpenFolderDialog.Destroy;
begin
  FreeAndNil(PathTreeView);
  inherited;
end;

end.
