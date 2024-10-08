unit UDlgAssociation;

interface

uses
  Windows, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls,
  System.ImageList, Vcl.ImgList, UConnection, Vcl.VirtualImageList,
  Vcl.BaseImageCollection, SVGIconImageCollection;

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
    ILAssociations: TVirtualImageList;
  public
    isTurned: boolean;
    procedure init(IsConnecting: boolean; conn: TConnection; SelectedControls: integer);
    function getCorner: integer;
    procedure setCorner(i: integer);
    function getConnectionAttributes: TConnectionAttributes;
  end;

implementation

uses SysUtils, Graphics, JvGnugettext, UConfiguration, UUtils;

{$R *.dfm}

procedure TFAssociationDialog.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  LBAssociations.ItemHeight:= LBAssociations.Height div LBAssociations.Items.Count;
  CBReadingOrderA.Caption:= #$25C0 + ' ';
  CBReadingOrderB.Caption:= #$25B6 + ' ';
  if FConfiguration.isDark
    then ILAssociations:= vilConnectionsDark
    else ILAssociations:= vilConnectionsLight;
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
  ModalResult:= mrOK;
end;

procedure TFAssociationDialog.LBAssociationsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var aCaption:=  (Control as TListBox).Items[Index];
  var aCanvas:= (Control as TListBox).Canvas;
  aCanvas.FillRect(Rect);
  ILAssociations.Draw(aCanvas, 4, Rect.Top + (Rect.Height - ILAssociations.Height) div 2, Index);
  aCanvas.TextOut(4 + ILAssociations.Width + 8, Rect.Top + 2, aCaption);
end;

function TFAssociationDialog.getCorner: integer;
begin
  case RGRecursivCorner.ItemIndex of
    0: Result:= 2;
    1: Result:= 3;
    2: Result:= 1;
    3: Result:= 4;
    else Result:= 0;
  end;
end;

procedure TFAssociationDialog.setCorner(i: integer);
  var j: integer;
begin
  case i of
    1: j:= 2;
    2: j:= 0;
    3: j:= 1;
    4: j:= 3;
    else j:= -1;
  end;
  RGRecursivCorner.ItemIndex:= j;
end;

procedure TFAssociationDialog.init(IsConnecting: Boolean; conn: TConnection; SelectedControls: integer);
begin
  ClientHeight:= 495;
  var h:= ((MMultiplicityA.Top + MMultiplicityA.Height) + RGRecursivCorner.Top) div 2 + 4;
  if IsConnecting then begin
    BTurn.Enabled:= false;
    BDelete.Enabled:= false;
    ClientHeight:= h;
  end else begin
    BTurn.Enabled:= true;
    BDelete.Enabled:= true;
  end;
  if assigned(conn) then begin
    MRoleA.Lines.Text:= conn.RoleA;
    MMultiplicityA.Lines.Text:= conn.MultiplicityA;
    ERelation.Text:= conn.Relation;
    CBReadingOrderA.Checked:= conn.ReadingOrderA;
    CBReadingOrderB.Checked:= conn.ReadingOrderB;
    MMultiplicityB.Lines.Text:= conn.MultiplicityB;
    MRoleB.Lines.Text:= conn.RoleB;
    LBAssociations.ItemIndex:= Ord(conn.ArrowStyle);
    if conn.isRecursiv
      then setCorner(conn.RecursivCorner)
      else ClientHeight:= h;
    IsTurned:= conn.IsTurned;
  end else begin
    MRoleA.Lines.Text:= '';
    MMultiplicityA.Lines.Text:= '';
    CBReadingOrderA.Checked:= false;
    ERelation.Text:= '';
    CBReadingOrderB.Checked:= false;
    MMultiplicityB.Lines.Text:= '';
    MRoleB.Lines.Text:= '';
    LBAssociations.ItemIndex:= 0;
    if SelectedControls = 1
      then setCorner(1)
      else ClientHeight:= h;
  end;
end;

procedure TFAssociationDialog.CBReadingOrderAClick(Sender: TObject);
  const ArrowLeft : string = #$25C0 + ' ';
  var s: string; p: integer;
begin
  p:= Pos(ArrowLeft, ERelation.Text);
  if CBReadingOrderA.Checked then begin
    if p = 0 then
      ERelation.Text:= ArrowLeft + ERelation.Text
  end else if p > 0 then begin
    s:= ERelation.Text;
    Delete(s, p, 2);
    ERelation.Text:= s;
  end;
end;

procedure TFAssociationDialog.CBReadingOrderBClick(Sender: TObject);
  const ArrowRight: string = ' ' + #$25B6;
  var s: string; p: integer;
begin
  p:= Pos(ArrowRight, ERelation.Text);
  if CBReadingOrderB.Checked then begin
    if p = 0 then
     ERelation.Text:= ERelation.Text + ArrowRight
  end else if p > 0 then begin
    s:= ERelation.Text;
    Delete(s, p, 2);
    ERelation.Text:= s;
  end;
end;

function TFAssociationDialog.getConnectionAttributes: TConnectionAttributes;
begin
  var A:= TConnectionAttributes.Create;
  A.ArrowStyle:= TessConnectionArrowStyle(LBAssociations.ItemIndex);
  A.RoleA:= MRoleA.Lines.Text;
  A.MultiplicityA:= MMultiplicityA.Lines.Text;
  A.ReadingOrderA:= CBReadingOrderA.Checked;
  A.Relation:= ERelation.Text;
  A.ReadingOrderB:= CBReadingOrderB.Checked;
  A.MultiplicityB:= MMultiplicityB.Lines.Text;
  A.RoleB:= MRoleB.Lines.Text;
  A.RecursivCorner:= getCorner;
  A.isTurned:= isTurned;
  A.isEdited:= true;
  A.Visible:= true;
  Result:= A;
end;

end.
