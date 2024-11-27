unit UImages;

interface

uses
  Classes, Controls, System.ImageList, Vcl.ImgList, Vcl.BaseImageCollection,
  SVGIconImageCollection, Vcl.VirtualImageList, TB2Item, SpTBXItem, Vcl.Menus;

type
  TDMImages = class(TDataModule)
    icAWTSwing1: TSVGIconImageCollection;
    icSwing2: TSVGIconImageCollection;
    icFXControls: TSVGIconImageCollection;
    icShapes: TSVGIconImageCollection;
    icProgram: TSVGIconImageCollection;
    icUtilities: TSVGIconImageCollection;
    icLayout: TSVGIconImageCollection;
    icToolbarLight: TSVGIconImageCollection;
    icToolbarDark: TSVGIconImageCollection;
    icToolbarDisabledLight: TSVGIconImageCollection;
    icToolbarDisabledDark: TSVGIconImageCollection;
    icSequencediagram: TSVGIconImageCollection;
    icMenuLight: TSVGIconImageCollection;
    icMenuDark: TSVGIconImageCollection;
  end;

var
  DMImages: TDMImages;

implementation

uses UJava;

{$R *.dfm}


end.
