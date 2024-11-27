unit UImageTransparent;


interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TImageTransparent = class (TImage)
  private
    procedure CnCtlColorStatic (var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WmEraseBkgnd (var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure CreateParams (var Params: TCreateParams); override;
  end;


implementation


procedure TImageTransparent.CreateParams (var Params: TCreateParams);
  begin
    inherited CreateParams(Params);
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
  end;

procedure TImageTransparent.WmEraseBkgnd(var Msg: TWMEraseBkgnd);
  begin
    Msg.Result := 1;
  end;

procedure TImageTransparent.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
  begin
    SetBKMode (Msg.ChildDC, TRANSPARENT);
    Msg.Result := GetStockObject (NULL_BRUSH);
  end;

procedure TImageTransparent.Paint;
  begin
    SetBKMode (Handle, TRANSPARENT);
    //inherited;
  end;

end.
