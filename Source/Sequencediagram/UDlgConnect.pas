unit UDlgConnect;

interface

uses
  Windows,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  System.ImageList,
  System.Classes,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  USequencePanel;

type

  TFConnectDialog = class(TForm)
    LBConnections: TListBox;
    LSelect: TLabel;
    BOK: TButton;
    BTurn: TButton;
    BDelete: TButton;
    BCancel: TButton;
    LMessage: TLabel;
    ERelation: TEdit;
    vilConnectionsDark: TVirtualImageList;
    vilConnectionsLight: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBConnectionsClick(Sender: TObject);
    procedure LBConnectionsDblClick(Sender: TObject);
    procedure LBConnectionsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FIsTurned: Boolean;
    FILConnections: TVirtualImageList;
  public
    procedure Init(IsConnecting: Boolean; Conn: TConnection;
      SelectedControls: Integer);
    function GetConnectionAttributes: TConnectionAttributes;
    property IsTurned: Boolean read FIsTurned;
  end;

implementation

uses
  SysUtils,
  Graphics,
  Themes,
  JvGnugettext,
  UConfiguration;

{$R *.dfm}

procedure TFConnectDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LBConnections.Color := StyleServices.GetSystemColor(clWindow);
  LBConnections.ItemHeight := LBConnections.Height div LBConnections.
    Items.Count;
  if FConfiguration.IsDark then
    FILConnections := vilConnectionsDark
  else
    FILConnections := vilConnectionsLight;
end;

procedure TFConnectDialog.FormShow(Sender: TObject);
begin
  Top := Mouse.CursorPos.Y + 25;
  if Top + Height > Application.MainForm.Height then
    Top := Application.MainForm.Height - Height - 25;
  Left := Mouse.CursorPos.X + 25;
  if Left + Width > Application.MainForm.Width then
    Left := Application.MainForm.Width - Width - 25;
  ShowScrollBar(LBConnections.Handle, SB_VERT, False);
  if ERelation.CanFocus then
    ERelation.SetFocus;
end;

procedure TFConnectDialog.LBConnectionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var
  ACaption := (Control as TListBox).Items[Index];
  var
  ACanvas := (Control as TListBox).Canvas;
  ACanvas.FillRect(Rect);
  FILConnections.SetSize(Rect.Height, Rect.Height);
  FILConnections.Draw(ACanvas, 4, Rect.Top, 12 + Index);
  ACanvas.TextOut(4 + FILConnections.Width + 8, Rect.Top + 2, ACaption);
end;

procedure TFConnectDialog.LBConnectionsClick(Sender: TObject);
begin
  if ERelation.CanFocus then
    ERelation.SetFocus;
end;

procedure TFConnectDialog.LBConnectionsDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFConnectDialog.Init(IsConnecting: Boolean; Conn: TConnection;
  SelectedControls: Integer);
begin
  if IsConnecting then
  begin
    BTurn.Enabled := False;
    BDelete.Enabled := False;
  end
  else
  begin
    BTurn.Enabled := True;
    BDelete.Enabled := True;
  end;
  if Assigned(Conn) then
  begin
    ERelation.Text := Conn.AMessage;
    LBConnections.ItemIndex := Ord(Conn.ArrowStyle);
    FIsTurned := False;
    if Conn.IsRecursiv then
    begin
      LBConnections.Items.Delete(4);
      LBConnections.Items.Delete(3);
    end;
  end
  else
  begin
    ERelation.Text := '';
    LBConnections.ItemIndex := 0;
  end;
end;

function TFConnectDialog.GetConnectionAttributes: TConnectionAttributes;
begin
  Result := TConnectionAttributes.Create;
  Result.ArrowStyle := TArrowStyle(LBConnections.ItemIndex);
  if Result.ArrowStyle = casClose then
  begin
    var
    Str := Trim(ERelation.Text);
    if Str = '' then
      Str := FConfiguration.SDClose;
    ERelation.Text := Str;
  end;
  Result.AMessage := ERelation.Text;
end;

end.
