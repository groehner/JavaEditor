unit USaveDialog;

interface

uses Classes, Controls, StdCtrls, ExtCtrls, Dialogs;

type

  {Extended Dialogs}
  TExtOpenDialog = class(TOpenDialog)
  private
    FExtraPanel: TPanel;
    LEncoding  : TLabel;
    CBEncoding : TComboBox;
  protected
    procedure DoClose; override;
    procedure DoShow; override;
  public
    Encoding  : string;
    constructor CreateWith(AOwner: TComponent; const aEncoding, EncodingAsText: string);
    destructor Destroy; override;
    function Execute: Boolean; override;
  end;

  TExtSaveDialog = class(TExtOpenDialog)
  public
    function Execute: Boolean; override;
  end;

implementation

uses Windows, SysUtils, Forms, CommDlg;

{$R *.RES}

{ TExtOpenDialog }

constructor TExtOpenDialog.CreateWith(AOwner: TComponent; const aEncoding, EncodingAsText: string);
begin
  inherited Create(AOwner);
  if aEncoding <> '' then begin
    Self.Encoding:= aEncoding;
    FExtraPanel := TPanel.Create(Self);
    with FExtraPanel do begin
      Name := 'ExtraPanel';
      Caption := '';
      BevelOuter := bvNone;
      BorderWidth := 2;
      TabOrder := 1;
      LEncoding:= TLabel.Create(Self);
      with LEncoding do begin
        Name := 'LEncoding';
        Caption := EncodingAsText;
        SetBounds(101, 3, 80, 21);
        Align := alNone;
        Ctl3D:=true;
        AutoSize := True;
        Parent := FExtraPanel;
      end;
      CBEncoding := TComboBox.Create(Self);
      with CBEncoding do begin
        Name := 'CBEncoding';
        Text := aEncoding;
        SetBounds(195, 1, 246, 21);
        Enabled := True;
        Parent := FExtraPanel;
      end;
    end;
  end;
end;

destructor TExtOpenDialog.Destroy;
begin
  if Encoding <> '' then begin
    FreeAndNil(LEncoding);
    FreeAndNil(CBEncoding);
    FreeAndNil(FExtraPanel);
  end;  
  inherited Destroy;
end;

procedure TExtOpenDialog.DoClose;
begin
  if Encoding <> '' then
    Encoding:= CBEncoding.Text;
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TExtOpenDialog.DoShow;
  var ExtRect, StaticRect: TRect;
begin
  // DOD implementation
  if Encoding <> '' then begin
    StaticRect := GetStaticRect;
    { Set ExtraText area to bottom of static area }
    ExtRect.Top := StaticRect.Bottom;
    ExtRect.Left := StaticRect.Left;
    ExtRect.Bottom := ExtRect.Top+30;
    ExtRect.Right := StaticRect.Right;
    FExtraPanel.ParentWindow := Handle;
    FExtraPanel.BoundsRect := ExtRect;
    CBEncoding.Items.Text:=
      'ANSI/Windows'#13#10'ANSI/Unix'#13#10'ANSI/Mac'#13#10 +
      'UTF-8/Windows'#13#10'UTF-8/Unix'#13#10'UTF-8/Mac'#13#10 +
      'UTF-16/Windows'#13#10'UTF-16/Unix'#13#10'UTF-16/Mac';
    CBEncoding.ItemIndex:= CBEncoding.Items.indexOf(Encoding);
  end;
  inherited DoShow;
end;

function TExtOpenDialog.Execute: boolean;
begin
  if Encoding = '' then
    Result:= inherited Execute
  else begin
    if NewStyleControls and not (ofOldStyleDialog in Options) then
      Template := 'EXTDLGTEMPLATE' else
      Template := nil;
    Result := inherited Execute;
  end;  
end;

{ TExtSaveDialog }

function TExtSaveDialog.Execute: Boolean;
begin
  if Encoding = '' then Result:= inherited Execute
  else begin
    if NewStyleControls and not (ofOldStyleDialog in Options) then
      Template := 'EXTDLGTEMPLATE' else
      Template := nil;
    Result := DoExecute(@GetSaveFileName);
  end;
end;

end.
