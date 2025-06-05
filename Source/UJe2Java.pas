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
    procedure WriteToPipe(Pipe: HPIPE; Str: string);
    procedure WriteToJava(const Str: ANSIString);
  public
    procedure WriteToConsolePipe(Str: string);
    procedure WMCOPYDATA(var msg: TWMCopyData); message WM_COPYDATA;
    procedure ShowInMemo(Str: string);
  end;

var
  FJe2Java: TFJe2Java;

implementation

{$R *.dfm}

uses UComJava2, UUtils;

procedure TFJe2Java.WriteToConsolePipe(Str: string);
begin
  if not aConsolePipeServer.Write(aConsolePipe, Str[1], Length(Str)*SizeOf(Char)) then
    ShowInMemo('Cannot write to ConsolePipe: ' + Str);
end;

procedure TFJe2Java.WriteToJava(const Str: ANSIString);
begin
  myComJava2.WriteToJava(Str);
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
  var Str: string;
begin
  SetLength(Str, Stream.Size div 2);
  Stream.Read(Pointer(Str)^, Stream.Size);
  ShowInMemo('> ' + Str);
  try
    Str:= myComJava2.ExecuteCommand(Str);
  except
    on e: Exception do
      Str:= '-ERR ' + e.Message;
  end;
  ShowInMemo('< ' + Str);
  ShowInMemo(myComJava2.ReadConsole);
  WriteToPipe(Pipe, Str);
end;

procedure TFJe2Java.aConsolePipeServerPipeMessage(Sender: TObject; Pipe: HPIPE;
  Stream: TStream);
  var Str: string;
      us: AnsiString;
begin
  aConsolePipe:= Pipe;
  SetLength(Str, Stream.Size div 2);
  Stream.Read(Pointer(Str)^, Stream.Size);
  ShowInMemo('CON> ' + Str);
  us:= AnsiString(Str);
  // __init__ is used to establish the ConsolePipe-Connection
  if Str <> '__init__' then WriteToJava(us);
end;

procedure TFJe2Java.WriteToPipe(Pipe: HPIPE; Str: string);
begin
  aPipeServer.Write(Pipe, Str[1], Length(Str)*SizeOf(Char));
end;

procedure TFJe2Java.BExecuteClick(Sender: TObject);
begin
  ShowInMemo(ECommand.Text);
  var Str:= myComJava2.ExecuteCommand(ECommand.Text);
  ShowInMemo(Str);
end;

procedure TFJe2Java.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  aPipeserver.Active:= False;
  aConsolePipeserver.Active:= False;
  if Assigned(myComJava2) then
    myComJava2.Close;
end;

procedure TFJe2Java.FormCreate(Sender: TObject);
begin
  var ConnectID:= ParamStr(1);
  myComJava2:= TComJava2.Create;
  Caption:= 'FJe2Java' + ConnectID;

  // aPipeServer is for communicating commands
  aPipeServer.PipeName:= 'je2java' + ConnectID;
  aPipeServer.Active:= True;
  ShowInMemo('Pipe server started with ' + aPipeServer.PipeName);

  // during the execution of a command (callMethod, createObject, ...) it's not
  // possible to use the command pipe, so we need the second pipe.

  // aConsolePipeServer is for communicating with the java console
  aConsolePipeServer.PipeName:= 'javaconsole' + ConnectID;
  aConsolePipeServer.Active:= True;
  ShowInMemo('Console pipe server started with ' + aConsolePipeServer.PipeName);
end;

procedure TFJe2Java.FormDestroy(Sender: TObject);
begin
  FreeAndNil(myComJava2);
end;

procedure TFJe2Java.WMCOPYDATA(var msg: TWMCopyData);
  var Str: string;
begin
  Str:= PChar(msg.CopyDataStruct.lpData);
  Str:= Copy(Str, 1, msg.CopyDataStruct.cbData div 2);
  myComJava2.processMessage(Str);
  if Str = 'term' then
    Close;
end;

procedure TFJe2Java.ShowInMemo(Str: string);
begin
  Memo1.Lines.Add(Str);
end;

end.
