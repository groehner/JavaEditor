{ Unit to demonstrate the usage of EnumWindows and some according API-functions
  Christoph Handel, Simon Reinhardt and Christian Kästner in October 1998

  Uses Messages to transport the Wnd-Handle.
  This way you do not need to directly use the Form variable }

unit UWindow;

interface

uses
  Windows, Messages, Forms;

type
  TFWindow = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    WM_ENUMERATE_ID: integer;
    aWindowHandle: HWnd;
    WindowCaption: string;
  public
    procedure WriteText(Wnd: HWnd);
    procedure WndProc(var message: TMessage); override;
    function GetWindowHandle(const aCaption: string): Hwnd;
  end;

var
  FWindow: TFWindow;


implementation

uses SysUtils, Controls;

{$R *.DFM}

function RegisterMessage: integer;
begin
  Result:= RegisterWindowMessage('Enumerate this Window');
end;

// this is the callbackfunction. Don't miss stdcall
// can't be part of the form.
function EnumWinProc(Wnd: HWnd; param: lParam): boolean; stdcall;
  var iMsgID: integer;
begin
  iMsgID:= RegisterMessage;
  SendMessage(param, iMsgID, 0, Wnd);
  // give data to main form
  Result:=true;
end;

{$WARNINGS OFF}
procedure TFWindow.WndProc(var message: TMessage);
begin
  if message.Msg = WM_ENUMERATE_ID then
    // oh! Enumerate Window found a window, lets do something
    WriteText(message.lParam)
  else
    inherited WndProc(message);
end;
{$WARNINGS ON}

procedure TFWindow.WriteText(Wnd: HWnd);
  var pcWinText: PChar;
begin
  if IsWindowVisible(wnd) then begin
    pcWinText:= StrAlloc(202);
    GetWindowText(Wnd, pcWinText, 200);
    if Pos(WindowCaption, pcWinText) > 0 then
      aWindowHandle:= Wnd;
    StrDispose(pcWinText);
  end;
end;

procedure TFWindow.FormCreate(Sender: TObject);
begin
  WM_ENUMERATE_ID:= RegisterMessage;  // get our msgID
end;

function TFWindow.GetWindowHandle(const aCaption: string): HWnd;
begin
  aWindowHandle:= 0;
  WindowCaption:= aCaption;
  EnumWindows(@EnumWinProc, self.Handle);
  Result:= aWindowHandle;
end;

end.
