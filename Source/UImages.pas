unit UImages;

interface

uses
  Classes,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  Vcl.VirtualImageList;

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

{$R *.dfm}

end.
