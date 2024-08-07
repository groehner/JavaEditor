program je2java;

uses
  Windows,
  Forms,
  UJe2Java in 'UJe2Java.pas' {FJe2Java},
  JavaRuntime in 'Jni\JavaRuntime.pas',
  JNI in 'Jni\JNI.pas',
  UThreads in 'Jni\UThreads.pas',
  UJniWrapper2 in 'Jni\UJniWrapper2.pas',
  UComJava2 in 'Jni\UComJava2.pas',
  UUtils in 'Util\UUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Je2Java';
  Application.CreateForm(TFJe2Java, FJe2Java);
  Application.ShowMainForm:= false;        // debug je2java
  Application.Run;
end.
