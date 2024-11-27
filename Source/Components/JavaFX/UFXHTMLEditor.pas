unit UFXHTMLEditor;

interface

uses
  Classes, UFXComponents;

type

  TFXHTMLEditor = class (TFXControl)
  private
    FHtmlText: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure NewControl; override;
    function getAttributes(ShowAttributes: integer): string; override;
  published
    property HtmlText: string read FHtmlText write FHtmlText;
  end;


implementation

uses Graphics;

{--- TFXHTMLEditor ------------------------------------------------------------}

constructor TFXHTMLEditor.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Tag:= 140;
  PrefWidth:= 200;
  PrefHeight:= 200;
  Background:= clBtnFace;
  JavaType:= 'HTMLEditor';
end;

procedure TFXHTMLEditor.Paint;
begin
  Canvas.Pen.Color:= DefaultBorderColor;
  Canvas.Brush.Color:= Background;
  Canvas.Rectangle(Rect(0, 0, Width, Height));
  Canvas.Brush.Color:= clWhite;
  Canvas.FillRect(Rect(1, 70, Width-1, Height-1));
end;

procedure TFXHTMLEditor.NewControl;
begin
  DefaultComponent;
  InsertNewVariable('private HTMLEditor ' + Name + ' = new HTMLEditor();');
  InsertImport('javafx.scene.web.HTMLEditor');
end;

function TFXHTMLEditor.getAttributes(ShowAttributes: integer): string;
begin
  Result:= '|HtmlText' + inherited getAttributes(ShowAttributes);
end;

end.
