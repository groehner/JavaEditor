unit ELDlgBorder;

interface

uses
  Forms, StdCtrls, ExtCtrls, ComCtrls,
  UJComponents, UBorderColorBox, Classes, Vcl.Controls;

type
  TFBorder = class(TForm)
    PCBorder: TPageControl;
    TSNo: TTabSheet;
    TSLine: TTabSheet;
    TSEtched: TTabSheet;
    TSBevel: TTabSheet;
    PButtons: TPanel;
    BOK: TButton;
    BCancel: TButton;
    TSTitled: TTabSheet;
    LLineBorderColor: TLabel;
    LLineBorderThickness: TLabel;
    EThickness: TEdit;
    UPThickness: TUpDown;
    CBLineRounded: TCheckBox;
    LTitle: TLabel;
    ETitle: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    RGEtchtype: TRadioGroup;
    Label3: TLabel;
    Label4: TLabel;
    RGBevelType: TRadioGroup;
    TSMatte: TTabSheet;
    Label5: TLabel;
    LMatteTop: TLabel;
    LMatteLeft: TLabel;
    LMatteBottom: TLabel;
    LMatteRight: TLabel;
    EMatteTop: TEdit;
    EMatteLeft: TEdit;
    EMatteRight: TEdit;
    EMatteBottom: TEdit;
    Panel1: TPanel;
    RGBordertype: TRadioGroup;
    Panel2: TPanel;
    procedure BOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGBordertypeClick(Sender: TObject);
  private
    FBorder: TBorder;
    CBLineColor: TBorderColorBox;
    CBEtchHighlight: TBorderColorBox;
    CBEtchShadow: TBorderColorBox;
    CBBevelHighlight: TBorderColorBox;
    CBBevelShadow: TBorderColorBox;
    CBMatteColor: TBorderColorBox;
    procedure setBorder(aValue: TBorder);
  public
    property Border: TBorder read FBorder write setBorder;
  end;

implementation

uses SysUtils, JvGnugettext;

{$R *.dfm}

procedure TFBorder.BOKClick(Sender: TObject);
begin
  FBorder.BorderType:= TBorderType(PCBorder.ActivePageIndex);
  FBorder.LineColor:= CBLineColor.Selected;
  FBorder.LineThickness:= UPThickness.Position;
  FBorder.LineRounded:= CBLineRounded.Checked;
  FBorder.Title:= ETitle.Text;
  FBorder.EtchHighlightColor:= CBEtchHighlight.Selected;
  FBorder.EtchShadowColor:= CBEtchShadow.Selected;
  FBorder.Etchtype:= RGEtchtype.ItemIndex;
  FBorder.BevelHighlightColor:= CBBevelHighlight.Selected;
  FBorder.BevelShadowColor:= CBBevelShadow.Selected;
  FBorder.Beveltype:= RGBeveltype.ItemIndex;
  FBorder.MatteColor:= CBMatteColor.Selected;
  FBorder.MatteTop:= StrToInt(EMatteTop.Text);
  FBorder.MatteLeft:= StrToInt(EMatteLeft.Text);
  FBorder.MatteBottom:= StrToInt(EMatteBottom.Text);
  FBorder.MatteRight:= StrToInt(EMatteRight.Text);
end;

procedure TFBorder.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  CBLineColor:= TBorderColorBox.Create3(Self, TSLine, 16, 30);
  CBEtchHighlight:= TBorderColorBox.Create3(Self, TSEtched, 16, 30);
  CBEtchShadow:= TBorderColorBox.Create3(Self, TSEtched, 16, 75);
  CBBevelHighlight:= TBorderColorBox.Create3(Self, TSBevel, 16, 30);
  CBBevelShadow:= TBorderColorBox.Create3(Self, TSBevel, 16, 75);
  CBMatteColor:=  TBorderColorBox.Create3(Self, TSMatte, 16, 30);
  FBorder:= nil;
end;

procedure TFBorder.RGBordertypeClick(Sender: TObject);
begin
  PCBorder.ActivePageIndex:= RGBordertype.ItemIndex;
end;

procedure TFBorder.setBorder(aValue: TBorder);
begin
  if aValue <> FBorder then begin
    FBorder:= aValue;
    PCBorder.ActivePageIndex:= Ord(FBorder.Bordertype);
    RGBordertype.ItemIndex:= Ord(FBorder.Bordertype);
    CBLineColor.Selected:= FBorder.LineColor;
    UPThickness.Position:= FBorder.LineThickness;
    CBLineRounded.Checked:= FBorder.LineRounded;
    ETitle.Text:= FBorder.Title;
    CBEtchHighlight.Selected:= FBorder.EtchHighlightColor;
    CBEtchShadow.Selected:= FBorder.EtchShadowColor;
    RGEtchtype.ItemIndex:= FBorder.Etchtype;
    CBBevelHighlight.Selected:= FBorder.BevelHighlightColor;
    CBBevelShadow.Selected:= FBorder.BevelShadowColor;
    RGBeveltype.ItemIndex:= FBorder.Beveltype;
    CBMatteColor.Selected:= FBorder.MatteColor;
    EMatteTop.Text:= IntToStr(FBorder.MatteTop);
    EMatteLeft.Text:= IntToStr(FBorder.MatteLeft);
    EMatteBottom.Text:= IntToStr(FBorder.MatteBottom);
    EMatteRight.Text:= IntToStr(FBorder.MatteRight);
  end;
end;

end.
