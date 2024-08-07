unit UJe2Java;

// JNI-Interface must have an own process, otherwise it isn't possible to
// restart JNI in case of changing a loaded class.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Pipes;

type
  TFJe2Java = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    ECommand: TEdit;
    LCommand: TLabel;
    BExecute: TButton;
    aPipeServer: TPipeServer;
    aConsolePipeServer: TPipeServer;
    procedure FormCreate(Sender: TObject);
    procedure BExecuteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure aPipeServerPipeConnect(Sender: TObject; Pipe: HPIPE);
    procedure aPipeServerPipeDisconnect(Sender: TObject; Pipe: HPIPE);
    procedure aPipeServerPipeMessage(Sender: TObject; Pipe: HPIPE;
      Stream: TStream);
    procedure aConsolePipeServerPipeMessage(Sender: TObject; Pipe: HPIPE;
      Stream: TStream);
    procedure aPipeServerPipeError(Sender: TObject; Pipe: HPIPE;
      PipeContext: TPipeContext; ErrorCode: Integer);
    procedure aConsolePipeServerPipeError(Sender: TObject; Pipe: HPIPE;
      PipeContext: TPipeContext; ErrorCode: Integer);
  private
    aConsolePipe: HPIPE;
    procedure WriteToPipe(Pipe: HPIPE; s: string);
    procedure WriteToJava(const s: ANSIString);
  public
    procedure WriteToConsolePipe(s: string);
    procedure WMCOPYDATA(var msg: TWMCopyData); message WM_COPYDATA;
    procedure ShowInMemo(s: string);
  end;

var
  FJe2Java: TFJe2Java;

implementation

{$R *.dfm}

uses UComJava2, UUtils;

procedure TFJe2Java.WriteToConsolePipe(s: string);
begin
  if not aConsolePipeServer.Write(aConsolePipe, s[1], length(s)*SizeOf(Char)) then
    ShowInMemo('Cannot write to ConsolePipe: ' + s);
end;

procedure TFJe2Java.WriteToJava(const s: ANSIString);
begin
  myComJava2.WriteToJava(s);
end;

procedure TFJe2Java.aPipeServerPipeConnect(Sender: TObject; Pipe: HPIPE);
begin
  ShowInMemo('Client has connected ' + IntToStr(Pipe));
end;

procedure TFJe2Java.aPipeServerPipeDisconnect(Sender: TObject; Pipe: HPIPE);
begin
  ShowInMemo('Client has disconnected ' + IntToStr(Pipe));
  Close;
end;

procedure TFJe2Java.aPipeServerPipeError(Sender: TObject; Pipe: HPIPE;
  PipeContext: TPipeContext; ErrorCode: Integer);
begin
  ShowInMemo('Pipe Server Error: ' + IntToStr(ErrorCode));
end;

procedure TFJe2Java.aConsolePipeServerPipeError(Sender: TObject; Pipe: HPIPE;
  PipeContext: TPipeContext; ErrorCode: Integer);
begin
  ShowInMemo('Console Pipe Error: ' + IntToStr(ErrorCode));
end;

procedure TFJe2Java.aPipeServerPipeMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
  var s: string;
begin
  SetLength(s, Stream.Size div 2);
  Stream.Read(Pointer(s)^, Stream.Size);
  ShowInMemo('> ' + s);
  try
    s:= myComJava2.ExecuteCommand(s);
  except
    on e: exception do
      s:= '-ERR ' + e.Message;
  end;
  ShowInMemo('< ' + s);
  ShowInMemo(myComJava2.ReadConsole);
  WriteToPipe(Pipe, s);
end;

procedure TFJe2Java.aConsolePipeServerPipeMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
  var s: string;
      us: AnsiString;
begin
  aConsolePipe:= Pipe;
  SetLength(s, Stream.Size div 2);
  Stream.Read(Pointer(s)^, Stream.Size);
  ShowInMemo('CON> ' + s);
  us:= AnsiString(s);
  // __init__ is used to establish the ConsolePipe-Connection
  if s <> '__init__' then WriteToJava(us);
end;

procedure TFJe2Java.WriteToPipe(Pipe: HPIPE; s: string);
begin
  aPipeServer.Write(Pipe, s[1], length(s)*SizeOf(Char));
end;

procedure TFJe2Java.BExecuteClick(Sender: TObject);
begin
  ShowInMemo(ECommand.Text);
  var s:= myComJava2.ExecuteCommand(ECommand.Text);
  ShowInMemo(s);
end;

procedure TFJe2Java.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  aPipeserver.Active:= false;
  aConsolePipeserver.Active:= false;
  if assigned(myComJava2) then
    myComJava2.Close;
end;

procedure TFJe2Java.FormCreate(Sender: TObject);
begin
  var ConnectID:= ParamStr(1);
  myComJava2:= TComJava2.create;
  Caption:= 'FJe2Java' + ConnectID;

  // aPipeServer is for communicating commands
  aPipeServer.PipeName:= 'je2java' + ConnectID;
  aPipeServer.Active:= true;
  ShowInMemo('Pipe server started with ' + aPipeServer.PipeName);

  // during the execution of a command (callMethod, createObject, ...) it's not
  // possible to use the command pipe, so we need the second pipe.

  // aConsolePipeServer is for communicating with the java console
  aConsolePipeServer.PipeName:= 'javaconsole' + ConnectID;
  aConsolePipeServer.Active:= true;
  ShowInMemo('Console pipe server started with ' + aConsolePipeServer.PipeName);
end;

procedure TFJe2Java.FormDestroy(Sender: TObject);
begin
  FreeAndNil(myComJava2);
end;

procedure TFJe2Java.WMCOPYDATA(var msg: TWMCopyData);
  var s: string;
begin
  s:= PChar(msg.CopyDataStruct.lpData);
  s:= copy(s, 1, msg.CopyDataStruct.cbData div 2);
  myComJava2.processMessage(s);
  if s = 'term' then
    Close;
end;

procedure TFJe2Java.ShowInMemo(s: string);
begin
  Memo1.Lines.Add(s);
end;

end.
