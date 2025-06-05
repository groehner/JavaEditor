unit USaveDialog;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Dialogs;

type

  { Extended Dialogs }
  TExtOpenDialog = class(TOpenDialog)
  private
    FExtraPanel: TPanel;
    FLEncoding: TLabel;
    FCBEncoding: TComboBox;
    FEncoding: string;
  protected
    procedure DoClose; override;
    procedure DoShow; override;
  public
    constructor CreateWith(AOwner: TComponent;
      const AEncoding, EncodingAsText: string);
    destructor Destroy; override;
    function Execute: Boolean; override;
    property Encoding: string read FEncoding;
  end;

  TExtSaveDialog = class(TExtOpenDialog)
  public
    function Execute: Boolean; override;
  end;

implementation

uses Windows, SysUtils, Forms, CommDlg;

{$R *.RES}
{ TExtOpenDialog }

constructor TExtOpenDialog.CreateWith(AOwner: TComponent;
  const AEncoding, EncodingAsText: string);
begin
  inherited Create(AOwner);
  if AEncoding <> '' then
  begin
    Self.FEncoding := AEncoding;
    FExtraPanel := TPanel.Create(Self);
    with FExtraPanel do
    begin
      Name := 'ExtraPanel';
      Caption := '';
      BevelOuter := bvNone;
      BorderWidth := 2;
      TabOrder := 1;
      FLEncoding := TLabel.Create(Self);
      with FLEncoding do
      begin
        Name := 'FLEncoding';
        Caption := EncodingAsText;
        SetBounds(101, 3, 80, 21);
        Align := alNone;
        Ctl3D := True;
        AutoSize := True;
        Parent := FExtraPanel;
      end;
      FCBEncoding := TComboBox.Create(Self);
      with FCBEncoding do
      begin
        Name := 'FCBEncoding';
        Text := AEncoding;
        SetBounds(195, 1, 246, 21);
        Enabled := True;
        Parent := FExtraPanel;
      end;
    end;
  end;
end;

destructor TExtOpenDialog.Destroy;
begin
  if FEncoding <> '' then
  begin
    FreeAndNil(FLEncoding);
    FreeAndNil(FCBEncoding);
    FreeAndNil(FExtraPanel);
  end;
  inherited Destroy;
end;

procedure TExtOpenDialog.DoClose;
begin
  if FEncoding <> '' then
    FEncoding := FCBEncoding.Text;
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TExtOpenDialog.DoShow;
var
  ExtRect, StaticRect: TRect;
begin
  // DOD implementation
  if FEncoding <> '' then
  begin
    StaticRect := GetStaticRect;
    { Set ExtraText area to bottom of static area }
    ExtRect.Top := StaticRect.Bottom;
    ExtRect.Left := StaticRect.Left;
    ExtRect.Bottom := ExtRect.Top + 30;
    ExtRect.Right := StaticRect.Right;
    FExtraPanel.ParentWindow := Handle;
    FExtraPanel.BoundsRect := ExtRect;
    FCBEncoding.Items.Text :=
      'ANSI/Windows'#13#10'ANSI/Unix'#13#10'ANSI/Mac'#13#10 +
      'UTF-8/Windows'#13#10'UTF-8/Unix'#13#10'UTF-8/Mac'#13#10 +
      'UTF-16/Windows'#13#10'UTF-16/Unix'#13#10'UTF-16/Mac';
    FCBEncoding.ItemIndex := FCBEncoding.Items.indexOf(FEncoding);
  end;
  inherited DoShow;
end;

function TExtOpenDialog.Execute: Boolean;
begin
  if FEncoding = '' then
    Result := inherited Execute
  else
  begin
    if NewStyleControls and not(ofOldStyleDialog in Options) then
      Template := 'EXTDLGTEMPLATE'
    else
      Template := nil;
    Result := inherited Execute;
  end;
end;

{ TExtSaveDialog }

function TExtSaveDialog.Execute: Boolean;
begin
  if FEncoding = '' then
    Result := inherited Execute
  else
  begin
    if NewStyleControls and not(ofOldStyleDialog in Options) then
      Template := 'EXTDLGTEMPLATE'
    else
      Template := nil;
    Result := DoExecute(@GetSaveFileName);
  end;
end;

end.
