unit UParseThread;

interface

uses Classes, UBaseForm;

type

  // this thread is parsing a java-program
  TParseThread = class(TThread)
  private
    Form: TFForm;
    FAbort: boolean;
    procedure setAbort(value: boolean);
  public
    abortable: boolean;
    State: integer;  // 1 = created, 2 = start executing, 3 = end executing
    property Abort: boolean read FAbort write setAbort;
    constructor create(aForm: TFForm; aAbortable: boolean);
    procedure Execute; override;
  end;

implementation

uses SysUtils, UConfiguration, UJavaParser, uFileProvider, UEditorForm, UUtils;

var i: integer;

constructor TParseThread.create(aForm: TFForm; aAbortable: boolean);
  const not_suspended = false;
begin
  Self.Form:= aForm;
  self.abortable:= aAbortable;
  FAbort:= false;
  State:= 1;
  var EditForm:= aForm as TFEditForm;
  OnTerminate:= EditForm.TerminateThread;
  NameThreadForDebugging('ParseThread');
  inherited create(not_suspended);
end;

procedure TParseThread.Execute;
begin
  State:= 2;
  inc(i);
  var EditForm:= Form as TFEditForm;
  if EditForm.IsJava and EditForm.NeedsParsing then begin
    EditForm.Model.Clear;
    FConfiguration.ImportCache.Clear;
    var Importer:= TJavaImporter.Create(EditForm.Model, TFileProvider.Create);
    try
      Importer.AddClasspath(UnHideBlanks(FConfiguration.getClassPathJarExpanded(EditForm.Pathname, EditForm.getPackage)), EditForm.Pathname);
      var Str:= Importer.CodeProvider.LoadStream(EditForm.Pathname, EditForm);
      if assigned(Str) then begin
        FreeAndNil(EditForm.Parser);
        EditForm.Parser:= TJavaParser.Create(true);
        EditForm.Parser.NeedPackage:= Importer.NeedPackageHandler;
        EditForm.Parser.Thread:= Self;
        EditForm.Parser.ParseStream(Str, EditForm.Model.ModelRoot, EditForm.Model, EditForm.Pathname, false, false);
        EditForm.Editor.Structures:= EditForm.Parser.Structures.clone;
      end;
    finally
      FreeAndNil(Importer);
    end;
    //FreeAndNil(Str); handeld by Scanner
    if not abort then begin
      if FConfiguration.FixImports then begin
        EditForm.AutomatedCompleteImports;
        EditForm.NeedsParsing:= true;
      end else begin
        EditForm.NeedsParsing:= false;
        // EditForm.CreateTVFileStructure;
        // this gave a strange EIO-Error
        // don't manipulate VCL-Elements outside the VCL-Thread!
      end;
    end;
  end;
  FConfiguration.FixImports:= false;
  State:= 3;
end;

procedure TParseThread.setAbort(value: boolean);
begin
  FAbort:= abortable and (value = true);
  if FAbort then
    Terminate;
end;

initialization
  i:= 0;

end.
