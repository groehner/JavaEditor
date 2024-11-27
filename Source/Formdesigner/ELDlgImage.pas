{-----------------------------------------------------------------------------
 Unit:     ELDlgImage
 Author:   Gerhard Röhner
 Date:     July 2014
 Purpose:  icon editor for object inspector
-----------------------------------------------------------------------------}

unit ELDlgImage;

interface

uses
  Forms, ExtDlgs, ExtCtrls, Vcl.StdCtrls, Vcl.Controls,
  System.Classes, Vcl.Dialogs;

type
  TFIconEditor = class(TForm)
    BSelect: TButton;
    BDelete: TButton;
    BOK: TButton;
    ODIconDialog: TOpenPictureDialog;
    ImagePanel: TPanel;
    Image: TImage;
    procedure SetValue(const Value: string);
    procedure BSelectClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FValue: string;
    FFX: boolean;
  public
    property Value: string read FValue write SetValue;
    property FX: boolean read FFX write FFX;
  end;

implementation

uses SysUtils, Graphics, JvGnugettext, UGuiDesigner, UUtils;

{$R *.dfm}

procedure TFIconEditor.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TFIconEditor.SetValue(const Value: string);
  var pathname: string;
begin
  if assigned(Image.Picture) then
    Image.Picture.Assign(nil);
  if pos('images/', Value) = 1
    then pathname:= FGuiDesigner.getPath + 'images\' + copy(Value, 8, length(Value))
    else pathname:= Value;
  if FileExists(pathname) then
    Image.Picture.LoadFromFile(pathname);
  FValue:= Value;
end;

procedure TFIconEditor.BDeleteClick(Sender: TObject);
begin
  if FFX
    then Value:= '(Graphic)'
    else Value:= '(Icon)';
  Image.Picture.Assign(nil);
end;

procedure TFIconEditor.BSelectClick(Sender: TObject);
begin
  with ODIconDialog do begin
    Filename:= '';
    InitialDir:= FGuiDesigner.getPath + 'images';
    ForceDirectories(Initialdir);
    Filter:= '*.jpg;*.jpeg;*.png;*.gif|*.jpg;*.jpeg;*.png;*.gif|*.jpg;*.jpeg|*.jpg;*.jpeg|*.png|*.png|*.gif|*.gif|*.*|*.*';
    FilterIndex:= 0;
    try
      if Execute then
        Value:= Filename;
    except on e: Exception do
      ErrorMsg(e.Message);
    end;
  end;
end;

end.
