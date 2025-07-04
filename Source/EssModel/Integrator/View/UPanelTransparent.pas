unit UPanelTransparent;

interface

uses
  Messages,
  Controls,
  ExtCtrls;

type
  TPanelTransparent = class(TPanel)
  private
    procedure CnCtlColorStatic(var Msg: TWMCtlColorStatic);
      message CN_CTLCOLORSTATIC;
    procedure WmEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

uses Windows;

procedure TPanelTransparent.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TPanelTransparent.WmEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  // SetBkMode (msg.DC, TRANSPARENT);
  Msg.Result := 1;
end;

procedure TPanelTransparent.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  SetBkMode(Msg.ChildDC, TRANSPARENT);
  Msg.Result := GetStockObject(NULL_BRUSH);
end;

procedure TPanelTransparent.Paint;
begin
  SetBkMode(Handle, TRANSPARENT);
end;

end.
