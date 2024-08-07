unit UTemplates;

interface

uses UEditorForm;

type

  TTemplates = class
    function Indent1: string;
    function Indent2: string;
    function Indent3: string;
    procedure PutTemplate(EditForm: TFEditForm; Template: string);
    function GetTemplate(const FName: string; Number: integer): string;
    procedure SBProgram(EditForm: TFEditForm);
    procedure SBNewClass(EditForm: TFEditForm);
    procedure FrameDialogApplet(EditForm: TFEditForm; Nr: integer);
      function GetFrameCode(const aName: string; AWT: boolean): string;
      function GetFrameCodeFX(const aName: string): string;
      function GetDialogCode(const aName: string; AWT: boolean): string;
      function GetAppletCode(const aName: string; AWT: boolean): string;
      function GetTestClassCode(const aClassname: string): string;
    procedure SBControlStructures(EditForm: TFEditForm; KTag: integer; OnKey: boolean = false);
    function GetControlStructure(KTag: integer; const indent: string; block: string): string;
  end;

  var FTemplates: TTemplates;

implementation

uses Classes, SysUtils, StrUtils, JvGnugettext, UStringRessources,
     UJava, UConfiguration;

procedure TTemplates.PutTemplate(EditForm: TFEditForm; Template: string);
begin
  with EditForm do begin
    PutText(Template);
    Save(false);
    ParseSourcecode(true);
    Modified:= false;
    Editor.Undolist.Clear;
  end;
end;

function TTemplates.GetTemplate(const FName: string; Number: integer): string;
  var s: string;
begin
  Result:= '';
  if Number < 1 then exit;
  var Template:= FConfiguration.Templates[Number];
  if FileExists(Template) then begin
    var SL:= TStringList.Create;
    try
      SL.LoadFromFile(Template);
      s:= SL.text;
      s:= ReplaceStr(s, '%ATTRIBUTES%', _(LNGStartGUIVariables) + CrLf + Indent1 + _(LNGEndGUIVariables));
      s:= ReplaceStr(s, '%COMPONENTS%', _(LNGStartComponents) + CrLf + Indent2 + _(LNGEndComponents));
      s:= ReplaceStr(s, '%METHODS%', _(LNGStartEventMethods) + CrLf + Indent1 + _(LNGEndEventMethods));
      s:= ReplaceStr(s, '%NAME%', FName);
      s:= ReplaceStr(s, '%DATE%', DateToStr(Date));
      s:= ReplaceStr(s, '%AUTHOR', FConfiguration.JavaAuthor);
    finally
      FreeAndNil(SL);
    end;
    Result:= s;
  end;
end;

procedure TTemplates.SBProgram(EditForm: TFEditForm);
  var s, s1, s2, FName: string;
begin
  FName:= ChangeFileExt(ExtractFilename(EditForm.Pathname), '');
  s1:= ''; s2:= '';
  if FConfiguration.CommentClosingBrackets then begin
    s1:= ' // end of main';
    s2:= ' // end of class ' + FName;
  end;
  if FConfiguration.MindstormsMode then begin
    s:= GetTemplate(FName, 11);
    if s = '' then
      if FConfiguration.MindstormsVersion = 2 then
        s:= 'import lejos.hardware.motor.Motor;' + CrLf +
            'import lejos.hardware.Button;' + CrLf +
          FConfiguration.HeadText + CrLf +
          'public class ' + FName + ' {' + CrLf +
          Indent1 + CrLf +
          Indent1 + 'public static void main(String[] args) {' + CrLf +
          Indent2 + '|' + CrLf +
          Indent1 + '}' + s1 + CrLf +
          '}' + s2 + CrLf
      else
        s:= 'import lejos.nxt.*;' + CrLf + CrLf +
          FConfiguration.HeadText + CrLf +
          'public class ' + FName + ' {' + CrLf +
          Indent1 + CrLf +
          Indent1 + 'public static void main(String[] args) throws InterruptedException {' + CrLf +
          Indent2 + '|' + CrLf +
          Indent1 + '}' + s1 + CrLf +
          '}' + s2 + CrLf;
   end
  else begin
    s:= GetTemplate(FName, 1);
    if s = '' then begin
      s:= FConfiguration.HeadText + CrLf +
          'public class ' + FName + ' {' + CrLf +
          Indent1 + CrLf +
          Indent1 + 'public static void main(String[] args) {' + CrLf +
          Indent2 + '|' + CrLf;
      s:= s + Indent1 + '}' + s1 + CrLf + CrLf +
          '}' + s2 + CrLf;
    end;
  end;
  PutTemplate(EditForm, s);
end;

procedure TTemplates.SBNewClass(EditForm: TFEditForm);
begin
  var FName:= ChangeFileExt(ExtractFilename(EditForm.Pathname), '');
  var s:= GetTemplate(FName, 10);
  if s = '' then begin
    if FConfiguration.CommentClosingBrackets then
      s:= ' // end of ' + FName;
    s:= FConfiguration.HeadText + CrLf +
      'public class ' + FName + ' {' + CrLf +
       Indent1 + CrLf +
       Indent1 + _(LNGStartGUIVariables) + CrLf +
       Indent1 + _(LNGEndGUIVariables) + CrLf +
       Indent1 + CrLf +
       Indent1 + _(LNGStartEventMethods) + CrLf +
       Indent1 + _(LNGEndEventMethods) + CrLf +
       '}' + s + CrLf;
  end;
  PutTemplate(Editform, s);
end;

procedure TTemplates.FrameDialogApplet(EditForm: TFEditForm; Nr: integer);
  var s: string;
begin
  with EditForm do begin
    Frametype:= Nr;
    var FName:= ChangeFileExt(ExtractFilename(Pathname), '');
    s:= GetTemplate(FName, Nr);
    if s = '' then
      case Frametype of
        2: s:= GetFrameCode(FName, true);
        3: s:= GetDialogCode(FName, true);
        4: s:= GetAppletCode(FName, true);
        5: s:= GetFrameCode(FName, false);
        6: s:= GetDialogCode(FName, false);
        7: s:= GetAppletCode(FName, false);
        8: s:= GetFrameCodeFX(FName);
      end;
  end;
  PutTemplate(Editform, s);
end;

function TTemplates.GetFrameCodeFX(const aName: string): string;
begin
  var s:= 'import javafx.application.Application;' + CrLf +
          'import javafx.scene.Scene;' + CrLf +
          'import javafx.scene.layout.Pane;' + CrLf +
          'import javafx.stage.Stage;' + CrLf;
  s:= s + CrLf +  FConfiguration.HeadText + CrLf;
  s:= s + 'public class ' + aName + ' extends Application {' + CrLf;
  s:= s +
      Indent1 + _(LNGStartGUIVariables) + CrLf +
      Indent1 + _(LNGEndGUIVariables) + CrLf +
      Indent1 + CrLf +
      Indent1 + 'public void start(Stage primaryStage) { ' + CrLf +
      Indent2 + 'Pane root = new Pane();' + CrLf;
  s:= s +
      Indent2 + 'Scene scene = new Scene(root, ' +
      IntToStr(FConfiguration.FrameWidth-16) + ', ' +
      IntToStr(FConfiguration.FrameHeight-38) + ');' + CrLf;
  s:= s +
      Indent2 + _(LNGStartComponents) + CrLf +
      Indent2 + '|' + CrLf +
      Indent2 + _(LNGEndComponents) + CrLf +
      Indent2 + CrLf;
  s:= s +
      Indent2 + 'primaryStage.setOnCloseRequest(e -> System.exit(0));' + CrLf +
      Indent2 + 'primaryStage.setTitle("' + aName + '");' + CrLf +
      Indent2 + 'primaryStage.setScene(scene);' + CrLf +
      Indent2 + 'primaryStage.show();' + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets
    then s:= s + ' // end of public ' + aName + CrLf
    else s:= s + CrLf;

  s:= s +
      Indent1 + CrLf +
      Indent1 + _(LNGStartEventMethods) + CrLf +
      Indent1 + CrLf +
      Indent1 + 'public static void main(String[] args) {' + CrLf +
      Indent2 + 'launch(args);' + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
     s:= s + ' // end of main' + CrLf +
         Indent1 + CrLf +
         Indent1 + _(LNGEndEventMethods) + CrLf +
         '} // end of class ' + aName+ CrLf
  else
     s:= s + CrLf +
         Indent1 + _(LNGEndEventMethods) + CrLf +
         '}' + CrLf;
  Result:= s;
end;

function TTemplates.GetFrameCode(const aName: string; AWT: boolean): string;
begin
  var s:= 'import java.awt.*;' + CrLf +
          'import java.awt.event.*;' + CrLf;
  if not AWT then
    s:= s + 'import javax.swing.*;' + CrLf +
            'import javax.swing.event.*;' + CrLf;
  s:= s + CrLf +  FConfiguration.HeadText + CrLf;
  if AWT
    then s:= s + 'public class ' + aName + ' extends Frame {' + CrLf
    else s:= s + 'public class ' + aName + ' extends JFrame {' + CrLf;
  s:= s +
      Indent1 + _(LNGStartGUIVariables) + CrLf +
      Indent1 + _(LNGEndGUIVariables) + CrLf +
      Indent1 + CrLf +
      Indent1 + 'public ' + aName + '() { ' + CrLf +
      Indent2 + '// Frame init' + CrLf +
      Indent2 + 'super();' + CrLf;
  if AWT then
    s:= s +
      Indent2 + 'addWindowListener(new WindowAdapter() {' + CrLf +
      Indent3 +   'public void windowClosing(WindowEvent evt) { dispose(); }' + CrLf +
      Indent2 + '});' + CrLf
  else
    s:= s + Indent2 + 'setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);' + CrLf;
  s:= s +
      Indent2 + 'int frameWidth = ' + IntToStr(FConfiguration.FrameWidth) + ';' + CrLf +
      Indent2 + 'int frameHeight = ' + IntToStr(FConfiguration.FrameHeight) + ';' + CrLf +
      Indent2 + 'setSize(frameWidth, frameHeight);' + CrLf +
      Indent2 + 'Dimension d = Toolkit.getDefaultToolkit().getScreenSize();' + CrLf +
      Indent2 + 'int x = (d.width - getSize().width) / 2;' + CrLf +
      Indent2 + 'int y = (d.height - getSize().height) / 2;' + CrLf +
      Indent2 + 'setLocation(x, y);' + CrLf +
      Indent2 + 'setTitle("' + aName + '");' + CrLf +
      Indent2 + 'setResizable(false);' + CrLf;
  if AWT then
    s:= s +
        Indent2 + 'Panel cp = new Panel(null);' + CrLf +
        Indent2 + 'add(cp);' + CrLf
  else
    s:= s +
        Indent2 + 'Container cp = getContentPane();' + CrLf +
        Indent2 + 'cp.setLayout(null);' + CrLf;

  s:= s +
      Indent2 + _(LNGStartComponents) + CrLf +
      Indent2 + '|' + CrLf +
      Indent2 + _(LNGEndComponents) + CrLf +
      Indent2 + CrLf +
      Indent2 + 'setVisible(true);' + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
     s:= s + ' // end of public ' + aName + CrLf
  else
     s:= s + CrLf;
  s:= s +
      Indent1 + CrLf +
      Indent1 + _(LNGStartEventMethods) + CrLf +
      Indent1 + CrLf +
      Indent1 + 'public static void main(String[] args) {' + CrLf +
      Indent2 + 'new ' + aName + '();' + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
     s:= s + ' // end of main' + CrLf +
         Indent1 + CrLf +
         Indent1 + _(LNGEndEventMethods) + CrLf +
         '} // end of class ' + aName+ CrLf
  else
     s:= s + CrLf +
         Indent1 + _(LNGEndEventMethods) + CrLf +
         '}' + CrLf;
  result:= s;
end;

function TTemplates.GetDialogCode(const aName: string; AWT: boolean): string;
begin
  var s:= 'import java.awt.*;' + CrLf +
          'import java.awt.event.*;' + CrLf;
  if not AWT then
    s:= s + 'import javax.swing.*;' + CrLf +
            'import javax.swing.event.*;' + CrLf;
  s:= s + CrLf + FConfiguration.HeadText + CrLf;
  if AWT
    then s:= s + 'public class ' + aName + ' extends Dialog {' + CrLf
    else s:= s + 'public class ' + aName + ' extends JDialog {' + CrLf;
  s:= s +
      Indent1 + _(LNGStartGUIVariables) + CrLf +
      Indent1 + _(LNGEndGUIVariables) + CrLf +
      Indent1 + CrLf;
  if AWT
    then s:= s + Indent1 + 'public ' + aName + '(Frame owner, boolean modal) { ' + CrLf
    else s:= s + Indent1 + 'public ' + aName + '(JFrame owner, boolean modal) { ' + CrLf;
  s:= s +
      Indent2 + '// Dialog init' + CrLf +
      Indent2 + 'super(owner, modal);' + CrLf;
  if AWT then
    s:= s +
      Indent2 + 'addWindowListener(new WindowAdapter() {' + CrLf +
      Indent3 +   'public void windowClosing(WindowEvent evt) { dispose(); }' + CrLf +
      Indent2 + '});' + CrLf
  else
    s:= s + Indent2 + 'setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);' + CrLf;
  s:= s +
      Indent2 + 'int frameWidth = ' + IntToStr(FConfiguration.FrameWidth) + ';' + CrLf +
      Indent2 + 'int frameHeight = ' + IntToStr(FConfiguration.FrameHeight) + ';' + CrLf +
      Indent2 + 'setSize(frameWidth, frameHeight);' + CrLf +
      Indent2 + 'Dimension d = Toolkit.getDefaultToolkit().getScreenSize();' + CrLf +
      Indent2 + 'int x = (d.width - getSize().width) / 2;' + CrLf +
      Indent2 + 'int y = (d.height - getSize().height) / 2;' + CrLf +
      Indent2 + 'setLocation(x, y);' + CrLf +
      Indent2 + 'setTitle("' + aName + '");' + CrLf;
  if AWT then
    s:= s +
        Indent2 + 'Panel cp = new Panel(null);' + CrLf +
        Indent2 + 'add(cp);' + CrLf
  else
    s:= s +
        Indent2 + 'Container cp = getContentPane();' + CrLf +
        Indent2 + 'cp.setLayout(null);' + CrLf;
  s:= s +
      Indent2 + _(LNGStartComponents) + CrLf +
      Indent2 + '|' + CrLf +
      Indent2 + _(LNGEndComponents) + CrLf +
      Indent2 + CrLf +
      Indent2 + 'setResizable(false);' + CrLf +
      Indent2 + 'setVisible(true);' + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    s:= s + ' // end of public ' + aName;
  s:= s + CrLf +
      Indent1 + CrLf +
      Indent1 + _(LNGStartEventMethods) + CrLf +
      Indent1 + _(LNGEndEventMethods) + CrLf +
      Indent1 + CrLf +
      '}';

  if FConfiguration.CommentClosingBrackets then
    s:= s + ' // end of class ' + aName;
  s:= s + CrLf;
  result:= s;
end;

function TTemplates.GetAppletCode(const aName: string; AWT: boolean): string;
begin
  var s:= 'import java.awt.*;' + CrLf +
          'import java.awt.event.*;' + CrLf;
  if AWT
    then s:= s + 'import java.applet.Applet;' + CrLf
    else s:= s + 'import javax.swing.*;' + CrLf +
                 'import javax.swing.event.*;' + CrLf;

  s:= s + CrLf + FConfiguration.HeadText + CrLf;
  if AWT
    then s:= s + 'public class ' + aName + ' extends Applet {' + CrLf
    else s:= s + 'public class ' + aName + ' extends JApplet {' + CrLf;
  s:= s +
      Indent1 + _(LNGStartGUIVariables) + CrLf +
      Indent1 + _(LNGEndGUIVariables) + CrLf +
      Indent1 + CrLf +
      Indent1 + 'public void init() {' + CrLf;

  if AWT then
    s:= s +
        Indent2 + 'Panel cp = new Panel(null);' + CrLf +
        Indent2 + 'cp.setBounds(0, 0, ' + IntToStr(FConfiguration.FrameWidth) + ', ' + IntTostr(FConfiguration.FrameHeight) + ');' + CrLf +
        Indent2 + 'add(cp);' + CrLf
  else
    s:= s +
      Indent2 + 'Container cp = getContentPane();' + CrLf +
      Indent2 + 'cp.setLayout(null);' + CrLf +
      Indent2 + 'cp.setBounds(0, 0, ' + IntToStr(FConfiguration.FrameWidth) + ', ' + IntTostr(FConfiguration.FrameHeight) + ');' + CrLf;

  s:= s +
      Indent2 + _(LNGStartComponents) + CrLf +
      Indent2 + '|' + CrLf +
      Indent2 + _(LNGEndComponents) + CrLf +
      Indent2 + CrLf +
      Indent1 + '}';
  if FConfiguration.CommentClosingBrackets then
    s:= s + ' // end of init';
  s:= s + CrLf + CrLf +
      Indent1 + _(LNGStartEventMethods) + CrLf +
      Indent1 + _(LNGEndEventMethods) + CrLf +
      Indent1 + CrLf +
      '}';
  if FConfiguration.CommentClosingBrackets then
    s:= s + ' // end of class ' + aName;
  s:= s + CrLf;
  result:= s;
end;

function TTemplates.GetTestClassCode(const aClassname: string): string;
begin
  var s:= 'import static org.junit.jupiter.api.Assertions.*;' + CrLf +
          'import org.junit.jupiter.api.*;' + CrLf;
  s:= s + CrLf + FConfiguration.HeadText + CrLf;
  s:= s + 'public class ' + aClassname + ' {' + CrLf;
  s:= s +
      Indent1 + _(LNGStartGUIVariables) + CrLf +
      Indent1 + _(LNGEndGUIVariables) + CrLf +
      Indent1 + CrLf;

  s:= s + CrLf +
      Indent1 + _(LNGStartEventMethods) + CrLf;

  if FConfiguration.JUnitBeforeEach then
    s:= s +
      Indent1 + CrLf +
      Indent1 + '/**' + CrLF +
      Indent1 + ' * called before each test method' + CrLf +
      Indent1 + ' */' + CrLf +
      Indent1 + '@BeforeEach' + CrLf +
      Indent1 + 'public void beforeEach() {' + CrLf +
      Indent2 + CrLf +
      Indent1 + '}' + CrLf + CrLf;
  s:= s +
      Indent1 + CrLf +
      Indent1 + '/**' + CrLF +
      Indent1 + ' * a test method' + CrLf +
      Indent1 + ' */' + CrLf +
      Indent1 + '@Test' + CrLf +
      Indent1 + 'public void aTest() {' + CrLf +
      Indent2 + _(LNGTODO) + CrLf +
      Indent1 + '}' + CrLf + CrLf;

  if FConfiguration.JUnitAfterEach then
    s:= s +
      Indent1 + CrLf +
      Indent1 + '/**' + CrLF +
      Indent1 + ' * called after each test method' + CrLf +
      Indent1 + ' */' + CrLf +
      Indent1 + '@AfterEach' + CrLf +
      Indent1 + 'public void afterEach() {' + CrLf +
      Indent2 + CrLf +
      Indent1 + '}';

  s:= s + CrLf + CrLf +
      Indent1 + _(LNGEndEventMethods) + CrLf;

  if FConfiguration.CommentClosingBrackets
    then s:= s + '} // end of class ' + aClassname+ CrLf
    else s:= s + '}' + CrLf;
  result:= s;
end;

function TTemplates.GetControlStructure(KTag: integer; const Indent: string; block: string): string;
  var s, s1, e: string;
      p: integer;
begin
  Result:= '';
  s:= '';
  p:= 0;
  while p < FConfiguration.ControlStructureTemplates[KTag].Count do begin
    s1:= FConfiguration.ControlStructureTemplates[KTag].Strings[p];
    if (block <> '') and (trim(s1) = '') then begin
      s:= s + block;
      block:= '';
    end
    else if (block <> '') and (trim(s1) = '|') then begin
      if KTag = 10 then
        s:= s + '|' + block
      else begin
        s:= s + Indent + s1 + CrLf;
        s:= s + block
      end;
      block:= '';
    end
    else
      s:= s + Indent + s1 + CrLf;
    inc(p);
  end;
  if FConfiguration.CommentClosingBrackets then
    case KTag of
      1: e:= ' // end of if';
      2: e:= ' // end of while';
      3: e:= ' // end of for';
      4: e:= ' // end of do-while';
      5: e:= ' // end of switch';
      6: e:= ' // end of try';
      7,
      8,
      9: e:= ' // end of if-else';
     10: e:= '';
    end
  else
    e:= '';
  delete(s, length(s)-1, 2);
  Result:= s +  e ;
end;

procedure TTemplates.SBControlStructures(EditForm: TFEditForm; KTag: integer; OnKey: boolean = false);
  var s, line, SelectedBlock, Indent: string; _indent: integer; empty: boolean;

  function PrepareBlock(s: string): string;
    var s1, s2: string; p: integer;
  begin
    s1:= '';
    p:= System.Pos(CrLf, s);
    while p > 0 do begin
      s2:= copy(s, 1, p+1);
      if KTag = 5 then // switch needs double indent
        if trim(s2) = ''
          then s1:= s1 + Indent + Indent2 + CrLf
          else s1:= s1 + Indent2 + s2
      else
        if trim(s2) = ''
          then s1:= s1 + Indent + Indent1 + CrLf
          else s1:= s1 + Indent1 + s2;
      delete(s, 1, p+1);
      p:= Pos(CrLf, s);
    end;
    Result:= s1;
  end;

begin
  if EditForm.Editor.SelAvail then begin
    Indent:= StringOfChar(' ', EditForm.Editor.GetStructureIndent(EditForm.Editor.BlockBegin.Line));
    SelectedBlock:= EditForm.Editor.GetLinesWithSelection + CrLf;
    SelectedBlock:= PrepareBlock(SelectedBlock);
  end else begin
    line:= EditForm.getLine(EditForm.Editor.CaretY-1);
    empty:= (trim(line) = '');
    delete(line, EditForm.Editor.CaretX, MaxInt);
    _indent:=  EditForm.Editor.GetStructureIndent(EditForm.Editor.CaretY);
    if empty then begin
      Indent:= StringOfChar(' ', _indent);
      EditForm.Editor.CaretX:= 1;
      EditForm.Editor.Lines[EditForm.Editor.CaretY-1]:= '';
    end else
      Indent:= EditForm.getIndent;
    SelectedBlock:= '';
  end;
  s:= GetControlStructure(KTag, Indent, SelectedBlock);
  if not EditForm.Editor.SelAvail and (trim(line) <> '') then
    delete(s, 1, length(Indent));
  if (KTag = 1) and OnKeY
    then FJava.scpJava.ActivateTimer(EditForm.Editor) // if - else
    else EditForm.PutText(s);
end;

function TTemplates.Indent1: string;
begin
  Result:= FConfiguration.Indent1;
end;

function TTemplates.Indent2: string;
begin
  Result:= FConfiguration.Indent2;
end;

function TTemplates.Indent3: string;
begin
  Result:= FConfiguration.Indent3;
end;

initialization
  FTemplates:= TTemplates.Create;

finalization
  FreeAndNil(FTemplates);

end.
