unit UTemplates;

interface

uses
  UEditorForm;

type

  TTemplates = class
    function Indent1: string;
    function Indent2: string;
    function Indent3: string;
    procedure PutTemplate(EditForm: TFEditForm; Template: string);
    function GetTemplate(const FName: string; Number: Integer): string;
    procedure SBProgram(EditForm: TFEditForm);
    procedure SBNewClass(EditForm: TFEditForm);
    procedure FrameDialogApplet(EditForm: TFEditForm; Num: Integer);
    function GetFrameCode(const AName: string; AWT: Boolean): string;
    function GetFrameCodeFX(const AName: string): string;
    function GetDialogCode(const AName: string; AWT: Boolean): string;
    function GetAppletCode(const AName: string; AWT: Boolean): string;
    function GetTestClassCode(const AClassname: string): string;
    procedure SBControlStructures(EditForm: TFEditForm; KTag: Integer;
      OnKey: Boolean = False);
    function GetControlStructure(KTag: Integer; const Indent: string;
      Block: string): string;
  end;

var
  FTemplates: TTemplates;

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  JvGnugettext,
  UStringRessources,
  UJava,
  UConfiguration;

procedure TTemplates.PutTemplate(EditForm: TFEditForm; Template: string);
begin
  with EditForm do
  begin
    PutText(Template);
    Save(False);
    ParseSourceCode(True);
    Modified := False;
    Editor.UndoList.Clear;
  end;
end;

function TTemplates.GetTemplate(const FName: string; Number: Integer): string;
var
  Str: string;
begin
  Result := '';
  if Number < 1 then
    Exit;
  var
  Template := FConfiguration.Templates[Number];
  if FileExists(Template) then
  begin
    var
    StringList := TStringList.Create;
    try
      StringList.LoadFromFile(Template);
      Str := StringList.Text;
      Str := ReplaceStr(Str, '%ATTRIBUTES%', _(LNGStartGUIVariables) + CrLf +
        Indent1 + _(LNGEndGUIVariables));
      Str := ReplaceStr(Str, '%COMPONENTS%', _(LNGStartComponents) + CrLf +
        Indent2 + _(LNGEndComponents));
      Str := ReplaceStr(Str, '%METHODS%', _(LNGStartEventMethods) + CrLf +
        Indent1 + _(LNGEndEventMethods));
      Str := ReplaceStr(Str, '%NAME%', FName);
      Str := ReplaceStr(Str, '%DATE%', DateToStr(Date));
      Str := ReplaceStr(Str, '%AUTHOR', FConfiguration.JavaAuthor);
    finally
      FreeAndNil(StringList);
    end;
    Result := Str;
  end;
end;

procedure TTemplates.SBProgram(EditForm: TFEditForm);
var
  Str, Str1, Str2, FName: string;
begin
  FName := ChangeFileExt(ExtractFileName(EditForm.Pathname), '');
  Str1 := '';
  Str2 := '';
  if FConfiguration.CommentClosingBrackets then
  begin
    Str1 := ' // end of main';
    Str2 := ' // end of class ' + FName;
  end;
  if FConfiguration.MindstormsMode then
  begin
    Str := GetTemplate(FName, 11);
    if Str = '' then
      if FConfiguration.MindstormsVersion = 2 then
        Str := 'import lejos.hardware.motor.Motor;' + CrLf +
          'import lejos.hardware.Button;' + CrLf + FConfiguration.HeadText +
          CrLf + 'public class ' + FName + ' {' + CrLf + Indent1 + CrLf +
          Indent1 + 'public static void main(String[] args) {' + CrLf + Indent2
          + '|' + CrLf + Indent1 + '}' + Str1 + CrLf + '}' + Str2 + CrLf
      else
        Str := 'import lejos.nxt.*;' + CrLf + CrLf + FConfiguration.HeadText +
          CrLf + 'public class ' + FName + ' {' + CrLf + Indent1 + CrLf +
          Indent1 + 'public static void main(String[] args) throws InterruptedException {'
          + CrLf + Indent2 + '|' + CrLf + Indent1 + '}' + Str1 + CrLf + '}' +
          Str2 + CrLf;
  end
  else
  begin
    Str := GetTemplate(FName, 1);
    if Str = '' then
    begin
      Str := FConfiguration.HeadText + CrLf + 'public class ' + FName + ' {' +
        CrLf + Indent1 + CrLf + Indent1 +
        'public static void main(String[] args) {' + CrLf + Indent2 +
        '|' + CrLf;
      Str := Str + Indent1 + '}' + Str1 + CrLf + CrLf + '}' + Str2 + CrLf;
    end;
  end;
  PutTemplate(EditForm, Str);
end;

procedure TTemplates.SBNewClass(EditForm: TFEditForm);
begin
  var
  FName := ChangeFileExt(ExtractFileName(EditForm.Pathname), '');
  var
  Str := GetTemplate(FName, 10);
  if Str = '' then
  begin
    if FConfiguration.CommentClosingBrackets then
      Str := ' // end of ' + FName;
    Str := FConfiguration.HeadText + CrLf + 'public class ' + FName + ' {' +
      CrLf + Indent1 + CrLf + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1
      + _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf + Indent1 +
      _(LNGStartEventMethods) + CrLf + Indent1 + _(LNGEndEventMethods) + CrLf +
      '}' + Str + CrLf;
  end;
  PutTemplate(EditForm, Str);
end;

procedure TTemplates.FrameDialogApplet(EditForm: TFEditForm; Num: Integer);
var
  Str: string;
begin
  with EditForm do
  begin
    FrameType := Num;
    var
    FName := ChangeFileExt(ExtractFileName(Pathname), '');
    Str := GetTemplate(FName, Num);
    if Str = '' then
      case FrameType of
        2:
          Str := GetFrameCode(FName, True);
        3:
          Str := GetDialogCode(FName, True);
        4:
          Str := GetAppletCode(FName, True);
        5:
          Str := GetFrameCode(FName, False);
        6:
          Str := GetDialogCode(FName, False);
        7:
          Str := GetAppletCode(FName, False);
        8:
          Str := GetFrameCodeFX(FName);
      end;
  end;
  PutTemplate(EditForm, Str);
end;

function TTemplates.GetFrameCodeFX(const AName: string): string;
begin
  var
  Str := 'import javafx.application.Application;' + CrLf +
    'import javafx.scene.Scene;' + CrLf + 'import javafx.scene.layout.Pane;' +
    CrLf + 'import javafx.stage.Stage;' + CrLf;
  Str := Str + CrLf + FConfiguration.HeadText + CrLf;
  Str := Str + 'public class ' + AName + ' extends Application {' + CrLf;
  Str := Str + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1 +
    _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf + Indent1 +
    'public void start(Stage primaryStage) { ' + CrLf + Indent2 +
    'Pane root = new Pane();' + CrLf;
  Str := Str + Indent2 + 'Scene scene = new Scene(root, ' +
    IntToStr(FConfiguration.FrameWidth - 16) + ', ' +
    IntToStr(FConfiguration.FrameHeight - 38) + ');' + CrLf;
  Str := Str + Indent2 + _(LNGStartComponents) + CrLf + Indent2 + '|' + CrLf +
    Indent2 + _(LNGEndComponents) + CrLf + Indent2 + CrLf;
  Str := Str + Indent2 + 'primaryStage.setOnCloseRequest(e -> System.exit(0));'
    + CrLf + Indent2 + 'primaryStage.setTitle("' + AName + '");' + CrLf +
    Indent2 + 'primaryStage.setScene(scene);' + CrLf + Indent2 +
    'primaryStage.show();' + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of public ' + AName + CrLf
  else
    Str := Str + CrLf;

  Str := Str + Indent1 + CrLf + Indent1 + _(LNGStartEventMethods) + CrLf +
    Indent1 + CrLf + Indent1 + 'public static void main(String[] args) {' + CrLf
    + Indent2 + 'launch(args);' + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of main' + CrLf + Indent1 + CrLf + Indent1 +
      _(LNGEndEventMethods) + CrLf + '} // end of class ' + AName + CrLf
  else
    Str := Str + CrLf + Indent1 + _(LNGEndEventMethods) + CrLf + '}' + CrLf;
  Result := Str;
end;

function TTemplates.GetFrameCode(const AName: string; AWT: Boolean): string;
begin
  var
  Str := 'import java.awt.*;' + CrLf + 'import java.awt.event.*;' + CrLf;
  if not AWT then
    Str := Str + 'import javax.swing.*;' + CrLf +
      'import javax.swing.event.*;' + CrLf;
  Str := Str + CrLf + FConfiguration.HeadText + CrLf;
  if AWT then
    Str := Str + 'public class ' + AName + ' extends Frame {' + CrLf
  else
    Str := Str + 'public class ' + AName + ' extends JFrame {' + CrLf;
  Str := Str + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1 +
    _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf + Indent1 + 'public ' + AName
    + '() { ' + CrLf + Indent2 + '// Frame init' + CrLf + Indent2 +
    'super();' + CrLf;
  if AWT then
    Str := Str + Indent2 + 'addWindowListener(new WindowAdapter() {' + CrLf +
      Indent3 + 'public void windowClosing(WindowEvent evt) { dispose(); }' +
      CrLf + Indent2 + '});' + CrLf
  else
    Str := Str + Indent2 +
      'setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);' + CrLf;
  Str := Str + Indent2 + 'int frameWidth = ' +
    IntToStr(FConfiguration.FrameWidth) + ';' + CrLf + Indent2 +
    'int frameHeight = ' + IntToStr(FConfiguration.FrameHeight) + ';' + CrLf +
    Indent2 + 'setSize(frameWidth, frameHeight);' + CrLf + Indent2 +
    'Dimension d = Toolkit.getDefaultToolkit().getScreenSize();' + CrLf +
    Indent2 + 'int x = (d.width - getSize().width) / 2;' + CrLf + Indent2 +
    'int y = (d.height - getSize().height) / 2;' + CrLf + Indent2 +
    'setLocation(x, y);' + CrLf + Indent2 + 'setTitle("' + AName + '");' + CrLf
    + Indent2 + 'setResizable(false);' + CrLf;
  if AWT then
    Str := Str + Indent2 + 'Panel cp = new Panel(null);' + CrLf + Indent2 +
      'add(cp);' + CrLf
  else
    Str := Str + Indent2 + 'Container cp = getContentPane();' + CrLf + Indent2 +
      'cp.setLayout(null);' + CrLf;

  Str := Str + Indent2 + _(LNGStartComponents) + CrLf + Indent2 + '|' + CrLf +
    Indent2 + _(LNGEndComponents) + CrLf + Indent2 + CrLf + Indent2 +
    'setVisible(true);' + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of public ' + AName + CrLf
  else
    Str := Str + CrLf;
  Str := Str + Indent1 + CrLf + Indent1 + _(LNGStartEventMethods) + CrLf +
    Indent1 + CrLf + Indent1 + 'public static void main(String[] args) {' + CrLf
    + Indent2 + 'new ' + AName + '();' + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of main' + CrLf + Indent1 + CrLf + Indent1 +
      _(LNGEndEventMethods) + CrLf + '} // end of class ' + AName + CrLf
  else
    Str := Str + CrLf + Indent1 + _(LNGEndEventMethods) + CrLf + '}' + CrLf;
  Result := Str;
end;

function TTemplates.GetDialogCode(const AName: string; AWT: Boolean): string;
begin
  var
  Str := 'import java.awt.*;' + CrLf + 'import java.awt.event.*;' + CrLf;
  if not AWT then
    Str := Str + 'import javax.swing.*;' + CrLf +
      'import javax.swing.event.*;' + CrLf;
  Str := Str + CrLf + FConfiguration.HeadText + CrLf;
  if AWT then
    Str := Str + 'public class ' + AName + ' extends Dialog {' + CrLf
  else
    Str := Str + 'public class ' + AName + ' extends JDialog {' + CrLf;
  Str := Str + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1 +
    _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf;
  if AWT then
    Str := Str + Indent1 + 'public ' + AName +
      '(Frame owner, boolean modal) { ' + CrLf
  else
    Str := Str + Indent1 + 'public ' + AName +
      '(JFrame owner, boolean modal) { ' + CrLf;
  Str := Str + Indent2 + '// Dialog init' + CrLf + Indent2 +
    'super(owner, modal);' + CrLf;
  if AWT then
    Str := Str + Indent2 + 'addWindowListener(new WindowAdapter() {' + CrLf +
      Indent3 + 'public void windowClosing(WindowEvent evt) { dispose(); }' +
      CrLf + Indent2 + '});' + CrLf
  else
    Str := Str + Indent2 +
      'setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);' + CrLf;
  Str := Str + Indent2 + 'int frameWidth = ' +
    IntToStr(FConfiguration.FrameWidth) + ';' + CrLf + Indent2 +
    'int frameHeight = ' + IntToStr(FConfiguration.FrameHeight) + ';' + CrLf +
    Indent2 + 'setSize(frameWidth, frameHeight);' + CrLf + Indent2 +
    'Dimension d = Toolkit.getDefaultToolkit().getScreenSize();' + CrLf +
    Indent2 + 'int x = (d.width - getSize().width) / 2;' + CrLf + Indent2 +
    'int y = (d.height - getSize().height) / 2;' + CrLf + Indent2 +
    'setLocation(x, y);' + CrLf + Indent2 + 'setTitle("' + AName + '");' + CrLf;
  if AWT then
    Str := Str + Indent2 + 'Panel cp = new Panel(null);' + CrLf + Indent2 +
      'add(cp);' + CrLf
  else
    Str := Str + Indent2 + 'Container cp = getContentPane();' + CrLf + Indent2 +
      'cp.setLayout(null);' + CrLf;
  Str := Str + Indent2 + _(LNGStartComponents) + CrLf + Indent2 + '|' + CrLf +
    Indent2 + _(LNGEndComponents) + CrLf + Indent2 + CrLf + Indent2 +
    'setResizable(false);' + CrLf + Indent2 + 'setVisible(true);' + CrLf +
    Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of public ' + AName;
  Str := Str + CrLf + Indent1 + CrLf + Indent1 + _(LNGStartEventMethods) + CrLf
    + Indent1 + _(LNGEndEventMethods) + CrLf + Indent1 + CrLf + '}';

  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of class ' + AName;
  Str := Str + CrLf;
  Result := Str;
end;

function TTemplates.GetAppletCode(const AName: string; AWT: Boolean): string;
begin
  var
  Str := 'import java.awt.*;' + CrLf + 'import java.awt.event.*;' + CrLf;
  if AWT then
    Str := Str + 'import java.applet.Applet;' + CrLf
  else
    Str := Str + 'import javax.swing.*;' + CrLf +
      'import javax.swing.event.*;' + CrLf;

  Str := Str + CrLf + FConfiguration.HeadText + CrLf;
  if AWT then
    Str := Str + 'public class ' + AName + ' extends Applet {' + CrLf
  else
    Str := Str + 'public class ' + AName + ' extends JApplet {' + CrLf;
  Str := Str + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1 +
    _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf + Indent1 +
    'public void init() {' + CrLf;

  if AWT then
    Str := Str + Indent2 + 'Panel cp = new Panel(null);' + CrLf + Indent2 +
      'cp.setBounds(0, 0, ' + IntToStr(FConfiguration.FrameWidth) + ', ' +
      IntToStr(FConfiguration.FrameHeight) + ');' + CrLf + Indent2 +
      'add(cp);' + CrLf
  else
    Str := Str + Indent2 + 'Container cp = getContentPane();' + CrLf + Indent2 +
      'cp.setLayout(null);' + CrLf + Indent2 + 'cp.setBounds(0, 0, ' +
      IntToStr(FConfiguration.FrameWidth) + ', ' +
      IntToStr(FConfiguration.FrameHeight) + ');' + CrLf;

  Str := Str + Indent2 + _(LNGStartComponents) + CrLf + Indent2 + '|' + CrLf +
    Indent2 + _(LNGEndComponents) + CrLf + Indent2 + CrLf + Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of init';
  Str := Str + CrLf + CrLf + Indent1 + _(LNGStartEventMethods) + CrLf + Indent1
    + _(LNGEndEventMethods) + CrLf + Indent1 + CrLf + '}';
  if FConfiguration.CommentClosingBrackets then
    Str := Str + ' // end of class ' + AName;
  Str := Str + CrLf;
  Result := Str;
end;

function TTemplates.GetTestClassCode(const AClassname: string): string;
begin
  var
  Str := 'import static org.junit.jupiter.api.Assertions.*;' + CrLf +
    'import org.junit.jupiter.api.*;' + CrLf;
  Str := Str + CrLf + FConfiguration.HeadText + CrLf;
  Str := Str + 'public class ' + AClassname + ' {' + CrLf;
  Str := Str + Indent1 + _(LNGStartGUIVariables) + CrLf + Indent1 +
    _(LNGEndGUIVariables) + CrLf + Indent1 + CrLf;

  Str := Str + CrLf + Indent1 + _(LNGStartEventMethods) + CrLf;

  if FConfiguration.JUnitBeforeEach then
    Str := Str + Indent1 + CrLf + Indent1 + '/**' + CrLf + Indent1 +
      ' * called before each test method' + CrLf + Indent1 + ' */' + CrLf +
      Indent1 + '@BeforeEach' + CrLf + Indent1 + 'public void beforeEach() {' +
      CrLf + Indent2 + CrLf + Indent1 + '}' + CrLf + CrLf;
  Str := Str + Indent1 + CrLf + Indent1 + '/**' + CrLf + Indent1 +
    ' * a test method' + CrLf + Indent1 + ' */' + CrLf + Indent1 + '@Test' +
    CrLf + Indent1 + 'public void aTest() {' + CrLf + Indent2 + _(LNGTODO) +
    CrLf + Indent1 + '}' + CrLf + CrLf;

  if FConfiguration.JUnitAfterEach then
    Str := Str + Indent1 + CrLf + Indent1 + '/**' + CrLf + Indent1 +
      ' * called after each test method' + CrLf + Indent1 + ' */' + CrLf +
      Indent1 + '@AfterEach' + CrLf + Indent1 + 'public void afterEach() {' +
      CrLf + Indent2 + CrLf + Indent1 + '}';

  Str := Str + CrLf + CrLf + Indent1 + _(LNGEndEventMethods) + CrLf;

  if FConfiguration.CommentClosingBrackets then
    Str := Str + '} // end of class ' + AClassname + CrLf
  else
    Str := Str + '}' + CrLf;
  Result := Str;
end;

function TTemplates.GetControlStructure(KTag: Integer; const Indent: string;
  Block: string): string;
var
  Str, Str1, End1: string;
  Posi: Integer;
begin
  Result := '';
  Str := '';
  Posi := 0;
  while Posi < FConfiguration.ControlStructureTemplates[KTag].Count do
  begin
    Str1 := FConfiguration.ControlStructureTemplates[KTag].Strings[Posi];
    if (Block <> '') and (Trim(Str1) = '') then
    begin
      Str := Str + Block;
      Block := '';
    end
    else if (Block <> '') and (Trim(Str1) = '|') then
    begin
      if KTag = 10 then
        Str := Str + '|' + Block
      else
      begin
        Str := Str + Indent + Str1 + CrLf;
        Str := Str + Block;
      end;
      Block := '';
    end
    else
      Str := Str + Indent + Str1 + CrLf;
    Inc(Posi);
  end;
  if FConfiguration.CommentClosingBrackets then
    case KTag of
      1:
        End1 := ' // end of if';
      2:
        End1 := ' // end of while';
      3:
        End1 := ' // end of for';
      4:
        End1 := ' // end of do-while';
      5:
        End1 := ' // end of switch';
      6:
        End1 := ' // end of try';
      7, 8, 9:
        End1 := ' // end of if-else';
      10:
        End1 := '';
      12:
        End1 := ' // end of for';
    end
  else
    End1 := '';
  Delete(Str, Length(Str) - 1, 2);
  Result := Str + End1;
end;

procedure TTemplates.SBControlStructures(EditForm: TFEditForm; KTag: Integer;
  OnKey: Boolean = False);
var
  Str, Line, SelectedBlock, Indent: string;
  IndentI: Integer;
  Empty: Boolean;

  function PrepareBlock(Str: string): string;
  var
    Str1, Str2: string;
    Posi: Integer;
  begin
    Str1 := '';
    Posi := System.Pos(CrLf, Str);
    while Posi > 0 do
    begin
      Str2 := Copy(Str, 1, Posi + 1);
      if KTag = 5 then // switch needs double indent
        if Trim(Str2) = '' then
          Str1 := Str1 + Indent + Indent2 + CrLf
        else
          Str1 := Str1 + Indent2 + Str2
      else if Trim(Str2) = '' then
        Str1 := Str1 + Indent + Indent1 + CrLf
      else
        Str1 := Str1 + Indent1 + Str2;
      Delete(Str, 1, Posi + 1);
      Posi := Pos(CrLf, Str);
    end;
    Result := Str1;
  end;

begin
  if EditForm.Editor.SelAvail then
  begin
    Indent := StringOfChar(' ', EditForm.Editor.GetStructureIndent
      (EditForm.Editor.BlockBegin.Line));
    SelectedBlock := EditForm.Editor.GetLinesWithSelection + CrLf;
    SelectedBlock := PrepareBlock(SelectedBlock);
  end
  else
  begin
    Line := EditForm.GetLine(EditForm.Editor.CaretY - 1);
    Empty := (Trim(Line) = '');
    Delete(Line, EditForm.Editor.CaretX, MaxInt);
    IndentI := EditForm.Editor.GetStructureIndent(EditForm.Editor.CaretY);
    if Empty then
    begin
      Indent := StringOfChar(' ', IndentI);
      EditForm.Editor.CaretX := 1;
      EditForm.Editor.Lines[EditForm.Editor.CaretY - 1] := '';
    end
    else
      Indent := EditForm.GetIndent;
    SelectedBlock := '';
  end;
  Str := GetControlStructure(KTag, Indent, SelectedBlock);
  if not EditForm.Editor.SelAvail and (Trim(Line) <> '') then
    Delete(Str, 1, Length(Indent));
  if (KTag = 1) and OnKey then
    FJava.scpJava.ActivateTimer(EditForm.Editor) // if - else
  else
    EditForm.PutText(Str);
end;

function TTemplates.Indent1: string;
begin
  Result := FConfiguration.Indent1;
end;

function TTemplates.Indent2: string;
begin
  Result := FConfiguration.Indent2;
end;

function TTemplates.Indent3: string;
begin
  Result := FConfiguration.Indent3;
end;

initialization

FTemplates := TTemplates.Create;

finalization

FreeAndNil(FTemplates);

end.
