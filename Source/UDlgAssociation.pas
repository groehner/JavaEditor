unit UDlgAssociation;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  System.ImageList,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  UConnection;

type
  TFAssociationDialog = class(TForm)
    LBAssociations: TListBox;
    LSelect: TLabel;
    BOK: TButton;
    BTurn: TButton;
    BDelete: TButton;
    BCancel: TButton;
    LMultiplicityA: TLabel;
    LRelation: TLabel;
    LMultiplicityB: TLabel;
    ERelation: TEdit;
    RGRecursivCorner: TRadioGroup;
    LRoleA: TLabel;
    LRoleB: TLabel;
    CBReadingOrderA: TCheckBox;
    CBReadingOrderB: TCheckBox;
    MRoleA: TMemo;
    MRoleB: TMemo;
    MMultiplicityA: TMemo;
    MMultiplicityB: TMemo;
    icMenuConnection: TSVGIconImageCollection;
    vilConnectionsLight: TVirtualImageList;
    vilConnectionsDark: TVirtualImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LBAssociationsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure LBAssociationsDblClick(Sender: TObject);
    procedure CBReadingOrderAClick(Sender: TObject);
    procedure CBReadingOrderBClick(Sender: TObject);
  private
    FILAssociations: TVirtualImageList;
    FIsTurned: Boolean;
  public
    procedure Init(IsConnecting: Boolean; Conn: TConnection; SelectedControls: Integer);
    function GetCorner: Integer;
    procedure SetCorner(Corner: Integer);
    function GetConnectionAttributes: TConnectionAttributes;
  end;

implementation

uses
  JvGnugettext,
  UConfiguration;

{$R *.dfm}

procedure TFAssociationDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LBAssociations.ItemHeight:= LBAssociations.Height div LBAssociations.Items.Count;
  CBReadingOrderA.Caption:= #$25C0 + ' ';
  CBReadingOrderB.Caption:= #$25B6 + ' ';
  if FConfiguration.IsDark
    then FILAssociations:= vilConnectionsDark
    else FILAssociations:= vilConnectionsLight;
end;

procedure TFAssociationDialog.FormShow(Sender: TObject);
begin
  Top:= Mouse.CursorPos.Y + 25;
  if Top + Height > Application.MainForm.Height then
    Top:= Application.MainForm.Height - Height - 25;
  Left:= Mouse.CursorPos.X + 25;
  if Left + Width > Application.MainForm.Width then
    Left:= Application.MainForm.Width - Width - 25;
end;

procedure TFAssociationDialog.LBAssociationsDblClick(Sender: TObject);
begin
  ModalResult:= mrOk;
end;

procedure TFAssociationDialog.LBAssociationsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var ACaption:=  TListBox(Control).Items[Index];
  var ACanvas:= TListBox(Control).Canvas;
  ACanvas.FillRect(Rect);
  FILAssociations.Draw(ACanvas, 4, Rect.Top +
                       (Rect.Height - FILAssociations.Height) div 2, Index);
  ACanvas.TextOut(4 + FILAssociations.Width + 8, Rect.Top + 2, ACaption);
end;

function TFAssociationDialog.GetCorner: Integer;
begin
  case RGRecursivCorner.ItemIndex of
    0: Result:= 2;
    1: Result:= 3;
    2: Result:= 1;
    3: Result:= 4;
    else Result:= 0;
  end;
end;

procedure TFAssociationDialog.SetCorner(Corner: Integer);
  var Index: Integer;
begin
  case Corner of
    1: Index:= 2;
    2: Index:= 0;
    3: Index:= 1;
    4: Index:= 3;
    else Index:= -1;
  end;
  RGRecursivCorner.ItemIndex:= Index;
end;

procedure TFAssociationDialog.Init(IsConnecting: Boolean; Conn: TConnection; SelectedControls: Integer);
begin
  ClientHeight:= 495;
  var ClientH:= ((MMultiplicityA.Top + MMultiplicityA.Height) + RGRecursivCorner.Top) div 2 + 4;
  if IsConnecting then begin
    BTurn.Enabled:= False;
    BDelete.Enabled:= False;
    ClientHeight:= ClientH;
  end else begin
    BTurn.Enabled:= True;
    BDelete.Enabled:= True;
  end;
  if Assigned(Conn) then begin
    MRoleA.Lines.Text:= Conn.RoleA;
    MMultiplicityA.Lines.Text:= Conn.MultiplicityA;
    ERelation.Text:= Conn.Relation;
    CBReadingOrderA.Checked:= Conn.ReadingOrderA;
    CBReadingOrderB.Checked:= Conn.ReadingOrderB;
    MMultiplicityB.Lines.Text:= Conn.MultiplicityB;
    MRoleB.Lines.Text:= Conn.RoleB;
    LBAssociations.ItemIndex:= Ord(Conn.ArrowStyle);
    if Conn.IsRecursiv
      then SetCorner(Conn.RecursivCorner)
      else ClientHeight:= ClientH;
    FIsTurned:= Conn.IsTurned;
  end else begin
    MRoleA.Lines.Text:= '';
    MMultiplicityA.Lines.Text:= '';
    CBReadingOrderA.Checked:= False;
    ERelation.Text:= '';
    CBReadingOrderB.Checked:= False;
    MMultiplicityB.Lines.Text:= '';
    MRoleB.Lines.Text:= '';
    LBAssociations.ItemIndex:= 0;
    if SelectedControls = 1
      then SetCorner(1)
      else ClientHeight:= ClientH;
  end;
end;

procedure TFAssociationDialog.CBReadingOrderAClick(Sender: TObject);
  const ArrowLeft : string = #$25C0 + ' ';
  var Str: string; Posi: Integer;
begin
  Posi:= Pos(ArrowLeft, ERelation.Text);
  if CBReadingOrderA.Checked then begin
    if Posi = 0 then
      ERelation.Text:= ArrowLeft + ERelation.Text;
  end else if Posi > 0 then begin
    Str:= ERelation.Text;
    Delete(Str, Posi, 2);
    ERelation.Text:= Str;
  end;
end;

procedure TFAssociationDialog.CBReadingOrderBClick(Sender: TObject);
  const ArrowRight: string = ' ' + #$25B6;
  var Str: string; Posi: Integer;
begin
  Posi:= Pos(ArrowRight, ERelation.Text);
  if CBReadingOrderB.Checked then begin
    if Posi = 0 then
     ERelation.Text:= ERelation.Text + ArrowRight;
  end else if Posi > 0 then begin
    Str:= ERelation.Text;
    Delete(Str, Posi, 2);
    ERelation.Text:= Str;
  end;
end;

function TFAssociationDialog.GetConnectionAttributes: TConnectionAttributes;
begin
  var Attri:= TConnectionAttributes.Create;
  Attri.ArrowStyle:= TEssConnectionArrowStyle(LBAssociations.ItemIndex);
  Attri.RoleA:= MRoleA.Lines.Text;
  Attri.MultiplicityA:= MMultiplicityA.Lines.Text;
  Attri.ReadingOrderA:= CBReadingOrderA.Checked;
  Attri.Relation:= ERelation.Text;
  Attri.ReadingOrderB:= CBReadingOrderB.Checked;
  Attri.MultiplicityB:= MMultiplicityB.Lines.Text;
  Attri.RoleB:= MRoleB.Lines.Text;
  Attri.RecursivCorner:= GetCorner;
  Attri.IsTurned:= FIsTurned;
  Attri.IsEdited:= True;
  Attri.Visible:= True;
  Result:= Attri;
end;

end.
