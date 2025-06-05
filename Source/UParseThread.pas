unit UParseThread;

interface

uses
  Classes,
  UBaseForm;

type

  // this thread is parsing a java program
  TParseThread = class(TThread)
  private
    FForm: TFForm;
    FAbort: Boolean;
    FAbortable: Boolean;
    FState: Integer;
    procedure SetAbort(Value: Boolean);
  public
    constructor Create(AForm: TFForm; Abortable: Boolean);
    procedure Execute; override;
    property Abort: Boolean read FAbort write SetAbort;
    property Abortable: Boolean read FAbortable;
    property State: Integer read FState write FState;
  end;

implementation

uses
  SysUtils,
  UConfiguration,
  UJavaParser,
  UFileProvider,
  UEditorForm,
  UUtils;

constructor TParseThread.Create(AForm: TFForm; Abortable: Boolean);
const
  Not_suspended = False;
begin
  FForm := AForm;
  FAbortable := Abortable;
  FAbort := False;
  FState := 1;
  var EditForm := AForm as TFEditForm;
  OnTerminate := EditForm.TerminateThread;
  NameThreadForDebugging('ParseThread');
  inherited Create(Not_suspended);
end;

procedure TParseThread.Execute;
begin
  FState := 2;
  var EditForm := FForm as TFEditForm;
  if EditForm.IsJava and EditForm.NeedsParsing then
  begin
    EditForm.Model.Clear;
    FConfiguration.ImportCache.Clear;
    var
    Importer := TJavaImporter.Create(EditForm.Model, TFileProvider.Create);
    try
      Importer.AddClasspath
        (UnHideBlanks(FConfiguration.GetClassPathJarExpanded(EditForm.Pathname,
        EditForm.GetPackage)), EditForm.Pathname);
      var
      Str := Importer.CodeProvider.LoadStream(EditForm.Pathname, EditForm);
      if Assigned(Str) then
      begin
        FreeAndNil(EditForm.Parser);
        EditForm.Parser := TJavaParser.Create(True);
        EditForm.Parser.NeedPackage := Importer.NeedPackageHandler;
        EditForm.Parser.Thread := Self;
        EditForm.Parser.ParseStream(Str, EditForm.Model.ModelRoot,
          EditForm.Model, EditForm.Pathname, False, False);
        EditForm.Editor.Structures := EditForm.Parser.Structures.Clone;
      end;
    finally
      FreeAndNil(Importer);
    end;
    // FreeAndNil(Str); handeld by Scanner
    if not Abort then
    begin
      if FConfiguration.FixImports then
      begin
        EditForm.AutomatedCompleteImports;
        EditForm.NeedsParsing := True;
      end
      else
        EditForm.NeedsParsing := False;
    end;
  end;
  FConfiguration.FixImports := False;
  FState := 3;
end;

procedure TParseThread.SetAbort(Value: Boolean);
begin
  FAbort := FAbortable and Value;
  if FAbort then
    Terminate;
end;

end.
