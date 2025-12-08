{ Unit to demonstrate the usage of EnumWindows and some according API-functions
  Christoph Handel, Simon Reinhardt and Christian Kästner in October 1998

  Uses Messages to transport the Wnd-Handle.
  This way you do not need to directly use the Form variable }

unit UWindow;

interface

uses
  Windows,
  Messages,
  Forms;

type
  TFWindow = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FWM_ENUMERATE_ID: Cardinal;
    FWindowHandle: HWND;
    FWindowCaption: string;
  public
    procedure WriteText(Wnd: HWND);
    procedure WndProc(var Message: TMessage); override;
    function GetWindowHandle(const ACaption: string): HWND;
  end;

var
  FWindow: TFWindow;

implementation

uses SysUtils;

{$R *.DFM}

function RegisterMessage: Cardinal;
begin
  Result := RegisterWindowMessage('Enumerate this Window');
end;

// this is the callbackfunction. Don't miss stdcall
// can't be part of the form.
function EnumWinProc(Wnd: HWND; Param: LParam): Boolean; stdcall;
  var IMsgID: Cardinal;
begin
  IMsgID := RegisterMessage;
  SendMessage(Param, IMsgID, 0, Wnd);
  // give data to main form
  Result := True;
end;

procedure TFWindow.WndProc(var Message: TMessage);
begin
  if Message.Msg = FWM_ENUMERATE_ID then
    // oh! Enumerate Window found a window, lets do something
    WriteText(Message.LParam)
  else
    inherited WndProc(Message);
end;

procedure TFWindow.WriteText(Wnd: HWND);
  var PcWinText: PChar;
begin
  if IsWindowVisible(Wnd) then begin
    PcWinText := StrAlloc(202);
    GetWindowText(Wnd, PcWinText, 200);
    if Pos(FWindowCaption, PcWinText) > 0 then
      FWindowHandle := Wnd;
    StrDispose(PcWinText);
  end;
end;

procedure TFWindow.FormCreate(Sender: TObject);
begin
  FWM_ENUMERATE_ID := RegisterMessage;  // get our msgID
end;

function TFWindow.GetWindowHandle(const ACaption: string): HWND;
begin
  FWindowHandle := 0;
  FWindowCaption := ACaption;
  EnumWindows(@EnumWinProc, Self.Handle);
  Result := FWindowHandle;
end;

end.
