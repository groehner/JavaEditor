unit UDlgConnect;

interface

uses
  Windows, Controls, Forms, StdCtrls, ComCtrls, USequencePanel,
  System.ImageList, Vcl.ImgList, System.Classes, Vcl.VirtualImageList;

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
    ILConnections: TVirtualImageList;
  public
    isTurned: boolean;
    procedure init(IsConnecting: boolean; conn: TConnection; SelectedControls: integer);
    function getConnectionAttributes: TConnectionAttributes;
  end;

implementation

uses SysUtils, Graphics, Themes, JvGnugettext, UImages, UConfiguration;

{$R *.dfm}

procedure TFConnectDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LBConnections.Color:= StyleServices.GetSystemColor(clWindow);
  LBConnections.ItemHeight:= LBConnections.Height div LBConnections.Items.Count;
  if FConfiguration.isDark
    then ILConnections:= vilConnectionsDark
    else ILConnections:= vilConnectionsLight;
end;

procedure TFConnectDialog.FormShow(Sender: TObject);
begin
  Top:= Mouse.CursorPos.Y + 25;
  if Top + Height > Application.MainForm.Height then
    Top:= Application.MainForm.Height - Height - 25;
  Left:= Mouse.CursorPos.X + 25;
  if Left + Width > Application.MainForm.Width then
    Left:= Application.MainForm.Width - Width - 25;
  ShowScrollBar(LBConnections.Handle, SB_VERT, false);
  if ERelation.CanFocus then
    ERelation.SetFocus;
end;

procedure TFConnectDialog.LBConnectionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var aCaption:= (Control as TListBox).Items[Index];
  var aCanvas:= (Control as TListBox).Canvas;
  aCanvas.FillRect(Rect);
  ILConnections.SetSize(Rect.Height, Rect.Height);
  ILConnections.Draw(aCanvas, 4, Rect.Top, 12 + Index);
  aCanvas.TextOut(4 + ILConnections.Width + 8, Rect.Top + 2, aCaption);
end;

procedure TFConnectDialog.LBConnectionsClick(Sender: TObject);
begin
  if ERelation.CanFocus then
    ERelation.SetFocus;
end;

procedure TFConnectDialog.LBConnectionsDblClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;

procedure TFConnectDialog.init(IsConnecting: Boolean; conn: TConnection; SelectedControls: integer);
begin
  if IsConnecting then begin
    BTurn.Enabled:= false;
    BDelete.Enabled:= false;
  end else begin
    BTurn.Enabled:= true;
    BDelete.Enabled:= true;
  end;
  if assigned(conn) then begin
    ERelation.Text:= conn.aMessage;
    LBConnections.ItemIndex:= Ord(conn.ArrowStyle);
    IsTurned:= false;
    if conn.isRecursiv then begin
      LBConnections.Items.delete(4);
      LBConnections.items.delete(3);
    end;
  end else begin
    ERelation.Text:= '';
    LBConnections.ItemIndex:= 0;
  end;
end;

function TFConnectDialog.getConnectionAttributes: TConnectionAttributes;
begin
  Result:= TConnectionAttributes.Create;
  Result.ArrowStyle:= TArrowStyle(LBConnections.ItemIndex);
  if Result.ArrowStyle = casClose then begin
    var s:= trim(ERelation.Text);
    if s = '' then
      s:= FConfiguration.SDClose;
    ERelation.Text:= s;
  end;
  Result.aMessage:= ERelation.Text;
end;

end.
