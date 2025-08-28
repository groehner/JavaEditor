unit USequenceForm;

{
  In the case of the UML window, a form with a symbol for sequence diagram is displayed
  with the same name but jsd opened, the sequence diagram belonging to the UML window
  source code
  Program with main method returns the name when creating a new JSD window
  If there is no program with main, you will be prompted to make a selection
  Debugger must have sequence window or it's nil
}

interface

uses
  Windows,  // don't move to implementation because of TBitmap problems
  Classes,
  Graphics,
  Controls,
  Forms,
  ComCtrls,
  StdCtrls,
  Menus,
  System.ImageList,
  Vcl.ImgList,
  Vcl.ToolWin,
  Vcl.VirtualImageList,
  TB2Item,
  SpTBXItem,
  USequencePanel,
  UBaseForm,
  ULifeline;

type
  TFSequenceForm = class(TFForm)
    SequenceScrollbox: TScrollBox;
    TBSequence: TToolBar;
    TBClose: TToolButton;
    TBLifeline: TToolButton;
    TBZoomOut: TToolButton;
    TBZoomIn: TToolButton;
    TBActor: TToolButton;
    TBNewLayout: TToolButton;
    TBRefresh: TToolButton;
    EMessage: TEdit;
    PopupMenuLifelineAndSequencePanel: TSpTBXPopupMenu;
    MIPopupConfiguration: TSpTBXItem;
    MIPopupFont: TSpTBXItem;
    MIPopupAsText: TSpTBXItem;
    MIPopupRefresh: TSpTBXItem;
    MIPopupNewLayout: TSpTBXItem;
    MIPopupDeleteLifeline: TSpTBXItem;
    MIPopupCreateLifeline: TSpTBXItem;
    MIPopupNewActor: TSpTBXItem;
    MIPopupNewLifeline: TSpTBXItem;
    MIPopupConnectWith: TSpTBXSubmenuItem;
    PopupMenuConnection: TSpTBXPopupMenu;
    MIDelete: TSpTBXItem;
    MITurn: TSpTBXItem;
    MIMessage: TSpTBXItem;
    MIClose: TSpTBXItem;
    MICreate: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    NIReturn: TSpTBXItem;
    MIAsynchron: TSpTBXItem;
    MISynchron: TSpTBXItem;
    vilToolbarLight: TVirtualImageList;
    vilToolbarDark: TVirtualImageList;
    vilPopupLight: TVirtualImageList;
    vilPopupDark: TVirtualImageList;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    MIPopupCopyAsPicture: TSpTBXItem;
    procedure TBCloseClick(Sender: TObject);
    procedure TBZoomOutClick(Sender: TObject);
    procedure TBZoomInClick(Sender: TObject);
    procedure MIPopupNewLayoutClick(Sender: TObject);
    procedure LifelineDblClick(Sender: TObject);
    procedure OnPanelClick(Sender: TObject);
    procedure OnPanelModified(Sender: TObject);
    procedure OnShowAll(Sender: TObject);
    procedure OnConnectionSet(Sender: TObject);
    procedure OnConnectionChanged(Sender: TObject;
      ArrowStyleOld, ArrowStyleNew: TArrowStyle);
    procedure OnLifelineSequencePanel(Sender: TObject);
    procedure SequenceScrollboxResize(Sender: TObject);
    procedure MIConnectionClick(Sender: TObject);
    procedure MIPopupRefreshClick(Sender: TObject);
    procedure MIPopupCreateObjectClick(Sender: TObject);
    procedure MIPopupDeleteLifelineClick(Sender: TObject);
    procedure MIPopupNewLifelineClick(Sender: TObject);
    procedure MIPopupNewActorClick(Sender: TObject);
    procedure ConnectLifelines(Sender: TObject);
    procedure PopupMenuConnectionPopup(Sender: TObject);
    procedure PopupMenuLifelineAndSequencePanelPopup(Sender: TObject);
    procedure MIPopupConfigurationClick(Sender: TObject);
    procedure MIPopupAsTextClick(Sender: TObject);
    procedure FEditMemoChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MIPopupCopyAsPictureClick(Sender: TObject);
    procedure MIPopupFontClick(Sender: TObject);
  private
    FEditMemo: TMemo;
    FReadOnly: Boolean;
    FLifelines: TList;
    FEditMemoElement: TLifeline;
    FEditConnection: TConnection;
    FLifelinesTop: Integer;
    FPopupAtYPos: Integer;
    FPopupAtLifeline: TLifeline;
    FDistY: Integer;
    FActivationWidth: Integer;
    FAResult: string;
    FEndParticipant: string;
    FMinWidth: Integer;
    FMinHeight: Integer;
    FMinDist: Integer;
    FMaxHeadHeight: Integer;
    FMessage: string;
    FMethodArrowStyle: TArrowStyle;
    FOnCloseNotify: TNotifyEvent;
    FSequencePanel: TSequencePanel;
    FStartParticipant: string;

    procedure SaveToFile(const FileName: string);
    function LoadFromFile(const FileName: string): Boolean;
    procedure DoEdit(Lifeline: TLifeline);
    procedure DoEditMessage(Conn: TConnection);
    procedure SetLeftBorderForEditMemo;
    procedure CloseEdit;
    function GetLifeline(const Participant: string): TLifeline;
    procedure OnBackgroundDblClick(Sender: TObject; Conn: TConnection);
    procedure OnCreatedChanged(Sender: TObject);
    function GetParticipantName: string;
    function GetActorName: string;
    function GetMaxLifelineHeight: Integer;
    function GetMinLifelineTop: Integer;
    function GetMinLifelineLeft: Integer;
    function GetSequenceDiagramAsBitmap: TBitmap;
    procedure SetPanelFont(AFont: TFont);
    procedure CalculateXPositions;
    procedure CalculateDiagram;
    procedure SortLifelines;
    procedure PrepareMethod(const Method: string);
    procedure AddConnection(const Connection: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure New(const Filename: string);
    function Open(const Pathname: string): Boolean;
    procedure Save(WithBackup: Boolean); override;
    procedure DoExport; override;
    procedure RefreshFromFile;
    procedure Print; override;
    procedure CopyToClipboard; override;
    procedure UpdateState; override;
    procedure AddLifeline(const Participant: string);
    procedure SetFont(AFont: TFont); override;
    procedure SetFontSize(Delta: Integer); override;
    function GetFormType: string; override;
    procedure MoveDiagram;
    procedure AddParameter(const Parameter: string);
    procedure ChangeParameter(Parameter: TStringList);
    procedure ChangeLifelineName(const Value, AName: string);
    // create from debugger or UML-Windows
    procedure MethodEntered(const Method: string);
    procedure MethodExited(Method: string);
    procedure ObjectDelete;
    function PrepareParticipant(const Participant: string): string;
    procedure MakeStartParticipant(const Participant: string);
    procedure MakeEndParticipant(const Participant: string);
    procedure MakeConnection;
    procedure SetModified(Modified: Boolean); override;
    procedure ChangeStyle; override;
    procedure DPIChanged; override;

    property AResult: string read FAResult write FAResult;
    property OnCloseNotify: TNotifyEvent read FOnCloseNotify write FOnCloseNotify;
    property SequencePanel: TSequencePanel read FSequencePanel;
    property StartParticipant: string read FStartParticipant write FStartParticipant;
    property EndParticipant: string read FEndParticipant write FEndParticipant;
  end;

implementation

{$R *.dfm}

uses
  Messages,
  SysUtils,
  IniFiles,
  Math,
  Clipbrd,
  Themes,
  StrUtils,
  JvGnugettext,
  UJava,
  UConfiguration,
  UUtils,
  UDebugger;

const
  CLifelinesTop = 30;

constructor TFSequenceForm.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 14;
end;

procedure TFSequenceForm.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  FSequencePanel := TSequencePanel.Create(SequenceScrollbox);
  FSequencePanel.OnClick := OnPanelClick;
  FSequencePanel.OnModified := OnPanelModified;
  FSequencePanel.OnShowAll := OnShowAll;
  FSequencePanel.OnBackgroundDblClicked := OnBackgroundDblClick;
  FSequencePanel.OnConnectionSet := OnConnectionSet;
  FSequencePanel.OnConnectionChanged := OnConnectionChanged;
  FSequencePanel.OnLifelineSequencePanel := OnLifelineSequencePanel;
  FSequencePanel.PopupMenuConnection := PopupMenuConnection;
  FSequencePanel.PopupMenuLifelineAndSequencePanel :=
    PopupMenuLifelineAndSequencePanel;
  FSequencePanel.Parent := SequenceScrollbox;

  FEditMemo := TMemo.Create(Self);
  FEditMemo.Parent := Self;
  FEditMemo.SetBounds(136, 156, 99, 37);
  FEditMemo.Color := clSkyBlue;
  FEditMemo.OnChange := FEditMemoChange;
  FEditMemo.Visible := False;
  FEditMemo.WordWrap := False;
  FEditMemo.BevelInner := bvNone;

  OnClose := FormClose;
  OnCloseQuery := FormCloseQuery;
  FLifelines := TList.Create;
  SetFont(FConfiguration.SequenceFont);
  ChangeStyle;
end;

procedure TFSequenceForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Modified and not AlreadySavedAs then
  begin
    FJava.DoSave(Self, True);
    AlreadySavedAs := True;
  end;
  if Assigned(OnCloseNotify) then
    OnCloseNotify(Self);
  CanClose := True;
end;

procedure TFSequenceForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  inherited;
  FLifelines.Clear;
  FreeAndNil(FLifelines);
  AAction := caFree;
end;

procedure TFSequenceForm.New(const FileName: string);
begin
  Caption := FileName;
  Pathname := FileName;
  if Pathname = '' then
    Pathname := FJava.GetFilename('.jsd');
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  Modified := False;
  Enter(Self); // must stay!
  if CanFocus then
    SetFocus;
end;

function TFSequenceForm.Open(const Pathname: string): Boolean;
begin
  Result := LoadFromFile(Pathname);
  if Result then
  begin
    Self.Pathname := Pathname;
    Caption := Pathname;
    FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
    Modified := False;
    Enter(Self); // must stay!
    if CanFocus then
      SetFocus;
    FReadOnly := IsWriteProtected(Pathname);
    UpdateState;
  end;
end;

function TFSequenceForm.LoadFromFile(const FileName: string): Boolean;
var
  Ini: TMemIniFile;
  StringList: TStringList;
begin
  Result := False;
  FLifelines.Clear;
  FSequencePanel.IsLocked := True;
  FSequencePanel.ClearManagedObjects;
  Ini := TMemIniFile.Create(FileName, TEncoding.UTF8);
  StringList := TStringList.Create;
  try
    FLifelinesTop :=
      PPIScale(Min(Ini.ReadInteger('Diagram', 'Top', 30), 500));
    Ini.ReadSectionValues('Participants', StringList);
    for var I := 0 to StringList.Count - 1 do
      AddLifeline(UnHideCrLf(StringList[I]));
    Ini.ReadSectionValues('Messages', StringList);
    for var I := 0 to StringList.Count - 1 do
      AddConnection(StringList[I]);
    Font.Name := Ini.ReadString('Diagram', 'FontName', 'Segoe UI');
    Font.Size := PPIScale(Ini.ReadInteger('Diagram', 'FontSize', 12));
    SetFont(Font);
    Result := True;
  finally
    StringList.Free;
    Ini.Free;
    FSequencePanel.IsLocked := False;
  end;
end;

procedure TFSequenceForm.SaveToFile(const FileName: string);
var
  Lifeline: TLifeline;
  Conn: TConnection;
  ConnStr: string;
  Connections: TList;
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Add('[Participants]');
    StringList.Add('# Object | x-position');
    for LifeLine in FLifelines do
      StringList.Add(HideCrLf(Lifeline.Participant) + ' | ' +
        IntToStr(PPIUnScale(Lifeline.Left)));

    StringList.Add('');
    StringList.Add('[Messages]');
    StringList.Add('# Object1 ->  Object2 | synchron message');
    StringList.Add('# Object1 --> Object2 | return message');
    StringList.Add('# Object1 ->> Object2 | asynchron message');
    StringList.Add('# Object1 ->o Object2 | new message');
    StringList.Add('# Object1 ->x Object2 | close message');
    Connections := FSequencePanel.GetConnections;
    try
      for Conn in Connections do begin
        ConnStr :=
          HideCrLf(TLifeline(Conn.StartControl).Participant) +
          conn.GetArrowStyleAsString +
          HideCrLf(TLifeline(Conn.EndControl)
          .Participant) + ' | ' + Conn.AMessage;
        StringList.Add(ConnStr);
      end;
    finally
      Connections.Free;
    end;
    StringList.Add('');
    StringList.Add('[Diagram]');
    if FLifelines.Count = 0 then
      StringList.Add('Top=' + IntToStr(PPIUnScale(FLifelinesTop)))
    else
      StringList.Add('Top=' + IntToStr(PPIUnScale(TLifeline(FLifelines[0]).Top)));
    StringList.Add('FontName=' + Font.Name);
    StringList.Add('FontSize=' + IntToStr(PPIUnScale(Font.Size)));

    try
      StringList.SaveToFile(FileName, TEncoding.UTF8);
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
  finally
    StringList.Free;
  end;
end;

procedure TFSequenceForm.Save(WithBackup: Boolean);
var
  BackupName, Ext: string;
begin
  if FReadOnly then
    Exit;
  if WithBackup then
  begin
    BackupName := Pathname;
    Ext := ExtractFileExt(Pathname);
    if Length(Ext) >= 2 then
      Ext[2] := '~'
    else
      Ext := '.~';
    BackupName := ChangeFileExt(BackupName, Ext);
    if FileExists(BackupName) then
      DeleteFile(BackupName);
    if FileExists(Pathname) then
      RenameFile(Pathname, BackupName);
  end;
  SaveToFile(Pathname);
  Modified := False;
end;

procedure TFSequenceForm.ConnectLifelines(Sender: TObject);
begin
  var
  Src := FSequencePanel.GetFirstSelected;
  var
  Dest := TControl(FLifelines[TSpTBXItem(Sender).Tag]);
  FSequencePanel.FindManagedControl(Dest).Selected := True;
  FSequencePanel.ConnectBoxesAt(Src, Dest, FPopupAtYPos);
end;

procedure TFSequenceForm.MIConnectionClick(Sender: TObject);
begin
  FSequencePanel.DoConnection(TSpTBXItem(Sender).Tag);
end;

procedure TFSequenceForm.MIPopupFontClick(Sender: TObject);
begin
  FJava.MIFontClick(Sender);
end;

procedure TFSequenceForm.CalculateDiagram;
var
  YPos, TheWidth, NewWidth, NewHeight: Integer;
  Conn, Conn1: TConnection;
  ConnList: TList;
  Lifeline, Lifeline1, Lifeline2: TLifeline;
  Activations: array of Integer;
begin
  if FSequencePanel.IsLocked then
    Exit;

  ConnList := FSequencePanel.GetConnections;
  // init activation calculating
  for var I := 0 to ConnList.Count - 1 do
  begin
    Conn1 := TConnection(ConnList[I]);
    Conn1.FromActivation := 0;
    Conn1.ToActivation := 0;
  end;

  if ConnList.Count > 1 then
    TConnection(ConnList[0]).FromActivation := 1;

  // calculate activations
  SetLength(Activations, FLifelines.Count);
  for var I := 0 to FLifelines.Count - 1 do
    TLifeline(FLifelines[I]).Activation := 0;
  if FLifelines.Count > 0 then
    TLifeline(FLifelines[0]).Activation := 1;
  for var I := 0 to ConnList.Count - 1 do
  begin
    Conn := TConnection(ConnList[I]);
    if Conn.IsRecursiv then
    begin
      Lifeline := TLifeline(Conn.StartControl);
      Conn.FromActivation := Lifeline.Activation;
      if Conn.ArrowStyle = casReturn then
        Lifeline.Activation := Lifeline.Activation - 1
      else
        Lifeline.Activation := Lifeline.Activation + 1;
      Conn.ToActivation := Lifeline.Activation;
    end
    else
    begin
      Lifeline1 := TLifeline(Conn.StartControl);
      Lifeline2 := TLifeline(Conn.EndControl);
      if Conn.ArrowStyle = casReturn then
      begin
        Conn.FromActivation := Lifeline1.Activation;
        Conn.ToActivation := Lifeline2.Activation;
        Lifeline1.Activation := Lifeline2.Activation - 1;
      end
      else if Conn.ArrowStyle = casNew then
      begin
        Conn.FromActivation := Lifeline1.Activation;
        Conn.ToActivation := 0;
      end
      else
      begin
        Conn.FromActivation := Lifeline1.Activation;
        Lifeline2.Activation := 1;
        Conn.ToActivation := Lifeline2.Activation;
      end;
    end;
  end;

  // calculate y-positions
  YPos := FMinHeight;
  if ConnList.Count > 0 then
  begin
    YPos := TConnection(ConnList[0]).StartControl.Top +
      FMaxHeadHeight + FDistY;
    for var I := 0 to ConnList.Count - 1 do
    begin
      TConnection(ConnList[I]).YPosition := YPos;
      if TConnection(ConnList[I]).IsRecursiv then
        YPos := YPos + Round(1.5 * FDistY)
      else
      begin
        if (I > 0) and (TConnection(ConnList[I - 1])
          .ArrowStyle = casNew) and
          (TConnection(ConnList[I - 1]).EndControl = TConnection
          (ConnList[I]).StartControl)

        then
        begin
          while YPos <= TConnection(ConnList[I - 1]).YPosition +
            TLifeline(TConnection(ConnList[I - 1]).EndControl)
            .HeadHeight div 2 + Canvas.TextHeight('A') do
            YPos := YPos + FDistY div 2;
          TConnection(ConnList[I]).YPosition := YPos;
        end;
        YPos := YPos + FDistY;
      end;
    end;
  end
  else
    YPos := YPos + FMaxHeadHeight + FDistY;

  TheWidth := 0;
  for var I := 0 to FLifelines.Count - 1 do
  begin
    Lifeline := TLifeline(FLifelines[I]);
    TheWidth := Max(TheWidth, Lifeline.Left + Lifeline.Width);
    if Lifeline.Created then
    begin
      for var J := 0 to ConnList.Count - 1 do
      begin
        Conn1 := TConnection(ConnList[J]);
        if (Conn1.ArrowStyle = casNew) and (Conn1.EndControl = Lifeline) then
        begin
          Lifeline.Top := Conn1.YPosition - Lifeline.HeadHeight div 2;
          Break;
        end;
      end;
      Lifeline.Height := TLifeline(FLifelines[0]).Top +
        TLifeline(FLifelines[0]).Height - Lifeline.Top;
    end
    else
    begin
      Lifeline.Height := YPos;
      Lifeline.Top := TLifeline(FLifelines[0]).Top;
    end;
    if Lifeline.Closed then
    begin
      for var J := 0 to ConnList.Count - 1 do
      begin
        Conn1 := TConnection(ConnList[J]);
        if (Conn1.ArrowStyle = casClose) and (Conn1.EndControl = Lifeline) then
        begin
          Lifeline.Height := Conn1.YPosition - Lifeline.Top - 10;
          Break;
        end;
      end;
    end;
  end;
  NewWidth := Max(SequenceScrollbox.Width, TheWidth + 30);
  NewHeight := Max(SequenceScrollbox.Height, YPos + 30);
  if (NewWidth > SequenceScrollbox.Width) or
    (NewHeight > SequenceScrollbox.Height) then
    SequenceScrollbox.SetBounds(0, 0, NewWidth, NewHeight);

  if (NewWidth > FSequencePanel.Width) or (NewHeight > FSequencePanel.Height) or
     (FSequencePanel.Left <> 0) or (FSequencePanel.Top <> 0) then
    FSequencePanel.SetBounds(0, 0, NewWidth, NewHeight);
  FreeAndNil(ConnList);
end;

procedure TFSequenceForm.MIPopupAsTextClick(Sender: TObject);
begin
  Save(True);
  FJava.EditorForm := FJava.OpenEditForm(Pathname, False);
  FJava.UpdateMenuItems(Self);
end;

procedure TFSequenceForm.MIPopupConfigurationClick(Sender: TObject);
begin
  FConfiguration.OpenAndShowPage('Sequencediagrams');
end;

procedure TFSequenceForm.MIPopupCreateObjectClick(Sender: TObject);
var
  Lifeline1, Lifeline2: TLifeline;
  Attributes: TConnectionAttributes;
  Connections: TList;
  Conn: TConnection;
  Int, Pos: Integer;
begin
  if FLifelines.Count > 0 then
  begin
    Lifeline1 := TLifeline(FSequencePanel.GetFirstSelected);
    AddLifeline(GetParticipantName);
    Lifeline2 := TLifeline(FLifelines.Last);
    Lifeline2.Created := True;
    if Lifeline1 = TLifeline(FLifelines[FLifelines.Count - 2]) then
      Lifeline2.Left := Lifeline2.Left - FMinDist +
        Canvas.TextWidth(FConfiguration.SDNew);
    if Assigned(Lifeline1) and Assigned(Lifeline2) then
    begin
      Attributes := TConnectionAttributes.Create;
      try
        Attributes.ArrowStyle := casNew;
        Attributes.AMessage := FConfiguration.SDNew;
        Connections := FSequencePanel.GetConnections;
        Int := 0;
        Pos := 0;
        while Int < Connections.Count do
        begin
          Conn := TConnection(Connections[Int]);
          if Conn.YPosition < FPopupAtYPos then
            Pos := Int + 1;
          Inc(Int);
        end;
        FSequencePanel.ConnectObjectsAt(Lifeline1, Lifeline2, Attributes, Pos);
        Modified := True;
        CalculateDiagram;
      finally
        FreeAndNil(Attributes);
        FreeAndNil(Connections);
      end;
    end;
  end;
end;

procedure TFSequenceForm.MIPopupDeleteLifelineClick(Sender: TObject);
begin
  var
  Lifeline := TLifeline(FSequencePanel.GetFirstSelected);
  if Assigned(Lifeline) then
  begin
    var
    Int := 0;
    while Int < FLifelines.Count do
    begin
      if TLifeline(FLifelines[Int]) = Lifeline then
      begin
        FLifelines.Delete(Int);
        Int := FLifelines.Count;
      end;
      Inc(Int);
    end;
  end;
  FSequencePanel.DeleteSelectedControls;
end;

procedure TFSequenceForm.MIPopupRefreshClick(Sender: TObject);
begin
  Save(True);
  RefreshFromFile;
end;

procedure TFSequenceForm.RefreshFromFile;
begin
  LoadFromFile(Pathname);
  CalculateDiagram;
  FSequencePanel.ShowAll;
  Invalidate;
  Modified := True;
end;

procedure TFSequenceForm.PopupMenuConnectionPopup(Sender: TObject);
begin
  var
  Point := FSequencePanel.ScreenToClient(Mouse.CursorPos);
  FPopupAtYPos := Point.Y;
end;

procedure TFSequenceForm.OnLifelineSequencePanel(Sender: TObject);
var
  Point: TPoint;
begin
  Point := FSequencePanel.ScreenToClient(Mouse.CursorPos);
  FPopupAtYPos := Point.Y;
  if Assigned(Sender) and (Sender is TLifeline) then
    FPopupAtLifeline := TLifeline(Sender)
  else
    FPopupAtLifeline := nil;
end;

procedure TFSequenceForm.PopupMenuLifelineAndSequencePanelPopup
  (Sender: TObject);
var
  MenuItem: TSpTBXItem;
  Posi: Integer;
  Str: string;
begin
  for var I := MIPopupConnectWith.Count - 1 downto 0 do
    FreeAndNil(MIPopupConnectWith[I]);

  if Assigned(FPopupAtLifeline) then
  begin
    MIPopupConnectWith.Visible := True;
    MIPopupDeleteLifeline.Visible := True;
    MIPopupCreateLifeline.Visible := True;
    for var I := 0 to FLifelines.Count - 1 do
    begin
      Str := TLifeline(FLifelines[I]).Participant;
      Posi := Pos(#13#10, Str);
      if Posi > 0 then
        Delete(Str, Posi, Length(Str));
      MenuItem := TSpTBXItem.Create(PopupMenuLifelineAndSequencePanel);
      MenuItem.Caption := Str;
      MenuItem.OnClick := ConnectLifelines;
      MenuItem.ImageIndex := 16;
      MenuItem.Tag := I;
      MIPopupConnectWith.Add(MenuItem);
    end;
  end
  else
  begin
    MIPopupConnectWith.Visible := False;
    MIPopupDeleteLifeline.Visible := False;
    MIPopupCreateLifeline.Visible := False;
  end;
end;

function TFSequenceForm.GetSequenceDiagramAsBitmap: TBitmap;
begin
  FSequencePanel.ChangeStyle(True);
  var EnclosingRect:= FSequencePanel.GetEnclosingRect;
  Result := TBitmap.Create;
  Result.Width := EnclosingRect.Right - EnclosingRect.Left + 2;
  Result.Height := EnclosingRect.Bottom - EnclosingRect.Top + 2;
  Result.Canvas.Lock;
  FSequencePanel.PaintTo(Result.Canvas.Handle, -EnclosingRect.Left + 1, -EnclosingRect.Top + 1);
  Result.Canvas.Unlock;
  FSequencePanel.ChangeStyle(False);
end;

procedure TFSequenceForm.Print;
begin
  var
  Bitmap := GetSequenceDiagramAsBitmap;
  PrintBitmap(Bitmap, PixelsPerInch);
  FreeAndNil(Bitmap);
end;

procedure TFSequenceForm.CopyToClipboard;
begin
  var
  Bitmap := GetSequenceDiagramAsBitmap;
  Clipboard.Assign(Bitmap);
  FreeAndNil(Bitmap);
end;

procedure TFSequenceForm.DoExport;
begin
  var
  Bitmap := GetSequenceDiagramAsBitmap;
  FJava.DoExport(Pathname, Bitmap);
  FreeAndNil(Bitmap);
  if CanFocus then
    SetFocus;
end;

procedure TFSequenceForm.UpdateState;
begin
  inherited;
  with FJava do
  begin
    SetEnabledMI(MICopyNormal, True);
    SetEnabledMI(MICopy, True);
    SetEnabledMI(MIPaste, False);
  end;
end;

procedure TFSequenceForm.SequenceScrollboxResize(Sender: TObject);
begin
  FSequencePanel.RecalcSize;
end;

procedure TFSequenceForm.SetFont(AFont: TFont);
const
  CMinDist = 20;
begin
  Font.Assign(AFont);
  FDistY := Round(CDistY * Font.Size / 12.0);
  FActivationWidth := Round(CActivationWidth * Font.Size / 12.0);
  FMinWidth := Round(CMinWidth * Font.Size / 12.0);
  FMinHeight := Round(CMinHeight * Font.Size / 12.0);
  FLifelinesTop := Round(CLifelinesTop * Font.Size / 12.0);
  FMinDist := Round(CMinDist * Font.Size / 12.0);
  Canvas.Font.Assign(AFont);
  SetPanelFont(AFont);
  SortLifelines;
  CalculateDiagram;
  FConfiguration.SequenceFont.Assign(AFont);
  Invalidate;
end;

procedure TFSequenceForm.SetFontSize(Delta: Integer);
begin
  inherited;
  CalculateXPositions;
end;

procedure TFSequenceForm.CalculateXPositions;
var
  Activation, Width1, Width2, Width3: Integer;
  Connections: TList;
  Conn, ConnPrev: TConnection;
  Lifeline1, Lifeline2: TLifeline;
begin
  Connections := FSequencePanel.GetConnections;
  for var I := 0 to FLifelines.Count - 2 do
  begin
    Lifeline1 := TLifeline(FLifelines[I]);
    Lifeline2 := TLifeline(FLifelines[I + 1]);
    Width1 := Lifeline1.Width div 2 + FMinDist + Lifeline2.Width div 2;
    Activation := 0;
    for var J := 0 to Connections.Count - 1 do
    begin
      Conn := TConnection(Connections[J]);
      if (Conn.StartControl = Lifeline1) and (Conn.EndControl = Lifeline2) or
        (Conn.StartControl = Lifeline2) and (Conn.EndControl = Lifeline1) or
        (Conn.StartControl = Lifeline1) and (Conn.EndControl = Lifeline1) then
      begin
        Width2 := Canvas.TextWidth(Conn.AMessage);
        if Conn.ArrowStyle = casNew then
          Width1 := Max(Width1, Width2 + Lifeline2.Width div 2)
        else
          Width1 := Max(Width1, Width2);
        Activation := Max(Activation, (Conn.FromActivation + Conn.ToActivation)
          * FActivationWidth);
      end;
      if J > 0 then
      begin
        ConnPrev := TConnection(Connections[J - 1]);
        if (ConnPrev.ArrowStyle = casNew) and
          (ConnPrev.StartControl = Lifeline1) and
          (ConnPrev.EndControl = Lifeline2) then
        begin
          Width2 := Canvas.TextWidth(ConnPrev.AMessage) +
            ConnPrev.EndControl.Width div 2;
          Width3 := Canvas.TextWidth(Conn.AMessage) +
            +ConnPrev.EndControl.Width div 2 - FMinDist;
          Width1 := Max(Max(Width1, Width2), Width3);
        end;
      end;
    end;
    Lifeline2.Left := Lifeline1.Left + Lifeline1.Width div 2 + Width1 + Activation +
      FMinDist - Lifeline2.Width div 2;
  end;
  FreeAndNil(Connections);
end;

procedure TFSequenceForm.SetPanelFont(AFont: TFont);
begin
  FMaxHeadHeight := 2 * FDistY;
  for var I := 0 to FLifelines.Count - 1 do
  begin
    TLifeline(FLifelines[I]).SetFont(AFont);
    FMaxHeadHeight := Max(FMaxHeadHeight, TLifeline(FLifelines[I])
      .HeadHeight);
  end;
  FSequencePanel.SetFont(AFont);
end;

procedure TFSequenceForm.TBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFSequenceForm.AddLifeline(const Participant: string);
var
  Lifeline: TLifeline;
  MaxX, X1Pos, Lef: Integer;
  StringList: TStringList;
begin
  if Copy(Participant, 1, 1) = '#' then
    Exit;
  StringList := Split('|', Participant);
  Lifeline := TLifeline.CreateLL(FSequencePanel, Trim(StringList[0]), Font);
  Lifeline.OnDblClick := LifelineDblClick;
  Lifeline.PopupMenu := PopupMenuLifelineAndSequencePanel;
  Lifeline.OnCreatedChanged := OnCreatedChanged;
  if (StringList.Count > 1) and TryStrToInt(Trim(StringList[1]), Lef) then
  begin
    Lifeline.Left := PPIScale(Lef);
    if FLifelines.Count > 0 then
      Lifeline.Top := TLifeline(FLifelines[0]).Top
    else
      Lifeline.Top := FLifelinesTop;
  end
  else
  begin
    MaxX := 0;
    for var I := 0 to FLifelines.Count - 1 do
    begin
      X1Pos := TLifeline(FLifelines[I]).Left +
        TLifeline(FLifelines[I]).Width;
      if X1Pos > MaxX then
        MaxX := X1Pos;
    end;
    Lifeline.Left := MaxX + FMinDist;
    if FLifelines.Count > 0 then
    begin
      Lifeline.Top := TLifeline(FLifelines[0]).Top;
      Lifeline.Height := TLifeline(FLifelines[0]).Height;
    end
    else
    begin
      Lifeline.Top := FLifelinesTop;
      Lifeline.Height := Lifeline.HeadHeight + 2 * FDistY;
    end;
  end;
  FSequencePanel.AddManagedObject(Lifeline);
  FLifelines.Add(Lifeline);
  Lifeline.First := (FLifelines.Count = 1);
  FreeAndNil(StringList);
end;

procedure TFSequenceForm.AddConnection(const Connection: string);
var
  Participant1, Participant2, BMessage: string;
  Posi: Integer;
  ArrowStyle: TArrowStyle;
  Attributes: TConnectionAttributes;
  Lifeline1, Lifeline2: TLifeline;
begin
  if Copy(Connection, 1, 1) = '#' then
    Exit;
  ArrowStyle := casSynchron;
  Posi := Pos('->>', Connection);
  if Posi > 0 then
    ArrowStyle := casAsynchron
  else
  begin
    Posi := Pos('-->', Connection);
    if Posi > 0 then
      ArrowStyle := casReturn
    else
    begin
      Posi := Pos('->x', Connection);
      if Posi > 0 then
        ArrowStyle := casClose
      else
      begin
        Posi := Pos('->o', Connection);
        if Posi > 0 then
          ArrowStyle := casNew
        else
          Posi := Pos('->', Connection);
      end;
    end;
  end;
  if Posi = 0 then
    Exit;
  Participant1 := UnHideCrLf(Trim(Copy(Connection, 1, Posi - 1)));
  if ArrowStyle = casSynchron then
    Participant2 := Trim(Copy(Connection, Posi + 2, Length(Connection)))
  else
    Participant2 := Trim(Copy(Connection, Posi + 3, Length(Connection)));
  Posi := Pos('|', Participant2);
  if Posi > 0 then
  begin
    BMessage := Trim(Copy(Participant2, Posi + 1, Length(Participant2)));
    Participant2 := Trim(Copy(Participant2, 1, Posi - 1));
  end
  else
    BMessage := '';
  Participant2 := UnHideCrLf(Participant2);
  Lifeline1 := GetLifeline(Participant1);
  Lifeline2 := GetLifeline(Participant2);
  if Assigned(Lifeline1) and Assigned(Lifeline2) then
  begin
    Attributes := TConnectionAttributes.Create;
    try
      Attributes.ArrowStyle := ArrowStyle;
      Attributes.AMessage := BMessage;
      if Attributes.ArrowStyle = casNew then
        Lifeline2.Created := True;
      if Attributes.ArrowStyle = casClose then
        Lifeline2.Closed := True;
      FSequencePanel.ConnectObjects(Lifeline1, Lifeline2, Attributes);
    finally
      FreeAndNil(Attributes);
    end;
  end;
end;

procedure TFSequenceForm.MIPopupNewLifelineClick(Sender: TObject);
begin
  AddLifeline(GetParticipantName);
  Modified := True;
end;

function TFSequenceForm.GetParticipantName: string;
var
  Num: Integer;
  Okay: Boolean;
begin
  Num := 1;
  repeat
    Okay := True;
    Result := FConfiguration.SDObject + IntToStr(Num);
    for var I := 0 to FLifelines.Count - 1 do
      if Pos(Result, TLifeline(FLifelines[I]).Participant) = 1 then
        Okay := False;
    Inc(Num);
  until Okay;
end;

procedure TFSequenceForm.MIPopupNewActorClick(Sender: TObject);
begin
  AddLifeline(GetActorName);
  Modified := True;
end;

function TFSequenceForm.GetActorName: string;
var
  Num: Integer;
  Okay: Boolean;
begin
  Num := 1;
  repeat
    Okay := True;
    Result := 'Actor' + IntToStr(Num);
    for var I := 0 to FLifelines.Count - 1 do
      if Pos(Result, TLifeline(FLifelines[I]).Participant) = 1 then
        Okay := False;
    Inc(Num);
  until Okay;
end;

procedure TFSequenceForm.MoveDiagram;
var
  Delta: Integer;
begin
  Delta := GetMinLifelineLeft - FMinDist;
  if Delta <> 0 then
    for var I := 0 to FLifelines.Count - 1 do
      TLifeline(FLifelines[I]).Left := TLifeline(FLifelines[I])
        .Left - Delta;
  Delta := GetMinLifelineTop - CLifelinesTop;
  if Delta <> 0 then
    for var I := 0 to FLifelines.Count - 1 do
      TLifeline(FLifelines[I]).Top := TLifeline(FLifelines[I])
        .Top - Delta;
end;

procedure TFSequenceForm.MIPopupNewLayoutClick(Sender: TObject);
begin
  FSequencePanel.IsLocked := True;
  SortLifelines;
  CalculateXPositions;
  MoveDiagram;
  SequenceScrollbox.HorzScrollBar.Position := 0;
  SequenceScrollbox.VertScrollBar.Position := 0;
  FSequencePanel.IsLocked := False;
  FSequencePanel.ShowAll;
end;

procedure TFSequenceForm.TBZoomInClick(Sender: TObject);
begin
  SetFontSize(+1);
  Modified := True;
end;

procedure TFSequenceForm.TBZoomOutClick(Sender: TObject);
begin
  SetFontSize(-1);
  Modified := True;
end;

function TFSequenceForm.GetFormType: string;
begin
  Result := '%Q%';
end;

procedure TFSequenceForm.DoEdit(Lifeline: TLifeline);
begin
  if Assigned(Lifeline) and not FReadOnly then
  begin
    FEditMemoElement := Lifeline;
    FEditMemo.Text := Lifeline.Participant;
    FEditMemo.SetBounds(Lifeline.Left + 2, TBSequence.Height + Lifeline.Top + 2,
      Lifeline.Width, Lifeline.HeadHeight);
    SetLeftBorderForEditMemo;
    FEditMemo.Visible := True;
    if FEditMemo.CanFocus then
      FEditMemo.SetFocus;
    FEditMemo.Perform(EM_SCROLLCARET, 0, 0);
  end;
end;

procedure TFSequenceForm.DoEditMessage(Conn: TConnection);
begin
  if Assigned(Conn) and not FReadOnly then
  begin
    FEditConnection := Conn;
    EMessage.Text := Conn.AMessage;
    EMessage.Font.Assign(Font);
    EMessage.SetBounds(Conn.TextRect.Left - 1, TBSequence.Height +
      Conn.TextRect.Top + 1, Round(Conn.TextRect.Width + 100),
      Conn.TextRect.Height);
    EMessage.Visible := True;
    if EMessage.CanFocus then
      EMessage.SetFocus;
    EMessage.SelStart := Length(Conn.AMessage);
  end;
end;

procedure TFSequenceForm.SetLeftBorderForEditMemo;
begin
  var
  Rect := FEditMemo.ClientRect;
  Rect.Left := 4;
  SendMessage(FEditMemo.Handle, EM_SETRECT, 0, LPARAM(@Rect));
end;

procedure TFSequenceForm.FEditMemoChange(Sender: TObject);
var
  Width, Height: Integer;
  Lifeline: TLifeline;
begin
  Lifeline := FEditMemoElement;
  if Assigned(Lifeline) then
  begin
    Lifeline.GetWidthHeigthOfText(FEditMemo.Lines.Text, Width, Height);
    if FEditMemo.Height < Height then
      FEditMemo.Height := Height;
    if FEditMemo.Width < Width then
      FEditMemo.Width := Width;
  end;
  SendMessage(FEditMemo.Handle, WM_VSCROLL, SB_TOP, 0);
  SetLeftBorderForEditMemo;
end;

procedure TFSequenceForm.CloseEdit;
begin
  if FEditMemo.Visible then
  begin
    if Assigned(FEditMemoElement) then
    begin
      FEditMemoElement.Participant := FEditMemo.Text;
      FEditMemoElement.CalcWidthHeight;
      Modified := True;
      FSequencePanel.Invalidate;
    end;
    FEditMemo.Visible := False;
    FEditMemoElement := nil;
  end;
  if EMessage.Visible then
  begin
    if Assigned(FEditConnection) then
    begin
      FEditConnection.AMessage := EMessage.Text;
      Modified := True;
      FSequencePanel.Invalidate;
    end;
    EMessage.Visible := False;
    FEditConnection := nil;
  end;
end;

function TFSequenceForm.GetLifeline(const Participant: string): TLifeline;
begin
  var
  Int := 0;
  while Int < FLifelines.Count do
    if (TLifeline(FLifelines[Int]).Participant = Participant) or
      (TLifeline(FLifelines[Int]).InternalName = Participant) then
    begin
      Result := TLifeline(FLifelines[Int]);
      Exit;
    end
    else
      Inc(Int);
  AddLifeline(Participant);
  Result := TLifeline(FLifelines.Last);
end;

procedure TFSequenceForm.LifelineDblClick(Sender: TObject);
var
  Posi: TPoint;
  Connection: TConnection;
begin
  if Sender is TLifeline then
  begin
    Posi := TLifeline(Sender).ScreenToClient(Mouse.CursorPos);
    if Posi.Y <= TLifeline(Sender).HeadHeight then
      DoEdit(TLifeline(Sender))
    else
    begin
      Connection := FSequencePanel.GetConnectionOfClickedTextRect;
      if Assigned(Connection) then
        DoEditMessage(Connection);
    end;
  end;
end;

procedure TFSequenceForm.OnBackgroundDblClick(Sender: TObject;
  Conn: TConnection);
begin
  if Assigned(Conn) then
    DoEditMessage(Conn);
end;

procedure TFSequenceForm.OnPanelClick(Sender: TObject);
begin
  UpdateState;
  CloseEdit;
end;

procedure TFSequenceForm.OnPanelModified(Sender: TObject);
begin
  Modified := True;
end;

procedure TFSequenceForm.OnShowAll(Sender: TObject);
begin
  CalculateDiagram;
end;

procedure TFSequenceForm.OnConnectionSet(Sender: TObject);
begin
  var
  Conn := TConnection(Sender);
  if Conn.ArrowStyle = casClose then
    TLifeline(Conn.EndControl).Closed := True;
  if Conn.ArrowStyle = casNew then
    TLifeline(Conn.EndControl).Created := True;
end;

procedure TFSequenceForm.OnConnectionChanged(Sender: TObject;
  ArrowStyleOld, ArrowStyleNew: TArrowStyle);
var
  Conn: TConnection;
  Lifeline: TLifeline;
  MinTop, MaxHeight: Integer;
begin
  Conn := TConnection(Sender);
  Lifeline := TLifeline(Conn.EndControl);
  Lifeline.Created := False;
  Lifeline.Closed := False;
  if ArrowStyleNew = casNew then
    Lifeline.Created := True;
  if ArrowStyleNew = casClose then
    Lifeline.Closed := True;
  MinTop := GetMinLifelineTop;
  MaxHeight := GetMaxLifelineHeight;
  if ArrowStyleNew = casNew then
    Lifeline.Top := Conn.YPosition - Lifeline.HeadHeight div 2
  else
    Lifeline.Top := MinTop;
  if ArrowStyleNew = casClose then
    Lifeline.Height := Conn.YPosition - Lifeline.Top - 10
  else if ArrowStyleNew = casNew then
    Lifeline.Height := MinTop + MaxHeight - Lifeline.Top
  else
    Lifeline.Height := MaxHeight;
  Conn.StartControl.Invalidate;
  Conn.EndControl.Invalidate;
end;

procedure TFSequenceForm.SortLifelines;
begin
  if FLifelines.Count > 1 then
    for var J := FLifelines.Count downto 2 do
      for var I := 0 to J - 2 do
        if TLifeline(FLifelines[I]).Left > TLifeline(FLifelines[I + 1]
          ).Left then
          FLifelines.Exchange(I, I + 1);
  for var I := 0 to FLifelines.Count - 1 do
    TLifeline(FLifelines[I]).First := (I = 0);
end;

procedure TFSequenceForm.OnCreatedChanged(Sender: TObject);
begin
  CalculateDiagram;
end;

// Sequence diagram from Debugger

procedure TFSequenceForm.PrepareMethod(const Method: string);
var
  Posi: Integer;
begin
  FMessage := Method;
  Posi := Pos('(', FMessage);
  if Posi > 0 then
    Delete(FMessage, Posi, Length(FMessage));
  Posi := Pos('.', FMessage);
  if Posi > 0 then
    Delete(FMessage, 1, Posi);
end;

procedure TFSequenceForm.AddParameter(const Parameter: string);
begin
  FMessage := FMessage + '(' + Parameter + ')';
end;

procedure TFSequenceForm.ChangeParameter(Parameter: TStringList);
var
  Conn: TConnection;
begin
  Conn := FSequencePanel.Get2NdLastConnection;
  if Assigned(Conn) then
    for var I := 0 to Parameter.Count - 1 do
      if Pos(Parameter.Names[I], Conn.AMessage) > 0 then
        Conn.AMessage := ReplaceStr(Conn.AMessage, Parameter.Names[I],
          Parameter.ValueFromIndex[I]);
  Conn := FSequencePanel.GetLastConnection;
  if Assigned(Conn) then
    for var I := 0 to Parameter.Count - 1 do
      if Pos(Parameter.Names[I], Conn.AMessage) > 0 then
        Conn.AMessage := ReplaceStr(Conn.AMessage, Parameter.Names[I],
          Parameter.ValueFromIndex[I]);
end;

procedure TFSequenceForm.ChangeLifelineName(const Value, AName: string);
begin
  for var I := 0 to FLifelines.Count - 1 do
  begin
    if TLifeline(FLifelines[I]).Participant <> '' then
      Continue;
    if TLifeline(FLifelines[I]).InternalName = Value then
    begin
      TLifeline(FLifelines[I]).RenameParticipant(AName);
      Exit;
    end;
  end;
end;

procedure TFSequenceForm.MethodEntered(const Method: string);
begin
  PrepareMethod(Method);
  if FMessage = '<init>' then
  begin
    FMessage := FConfiguration.SDNew;
    FMethodArrowStyle := casNew;
  end
  else
    FMethodArrowStyle := casSynchron;
end;

procedure TFSequenceForm.MethodExited(Method: string);
var
  Posi: Integer;
begin
  Posi := Pos(', ', Method);
  if Posi > 0 then
  begin
    FAResult := Copy(Method, 1, Posi - 1);
    if FAResult = '<void value>' then
      FAResult := '';
    Delete(Method, 1, Posi + 1);
  end;
  PrepareMethod(Method);
  FMethodArrowStyle := casReturn;
end;

procedure TFSequenceForm.ObjectDelete;
begin
  FMessage := FConfiguration.SDClose;
  FMethodArrowStyle := casClose;
end;

function TFSequenceForm.PrepareParticipant(const Participant: string): string;
begin
  if Participant = 'main' then
    Result := 'Actor'
  else
    Result := Participant;
end;

procedure TFSequenceForm.MakeStartParticipant(const Participant: string);
begin
  FStartParticipant := PrepareParticipant(Participant);
end;

procedure TFSequenceForm.MakeEndParticipant(const Participant: string);
begin
  FEndParticipant := PrepareParticipant(Participant);
  if (FStartParticipant = FEndParticipant) and (MyDebugger.SequenceForm = Self)
  then
    if FMethodArrowStyle in [casSynchron, casNew] then
      FStartParticipant := 'Actor'
    else
      FEndParticipant := 'Actor';
end;

procedure TFSequenceForm.MakeConnection;
var
  Lifeline1, Lifeline2: TLifeline;
  Attributes: TConnectionAttributes;
begin
  if not FConfiguration.SDShowMainCall and (FStartParticipant = 'Actor') and
    (FEndParticipant = 'Actor') then
    Exit;
  Lifeline1 := GetLifeline(FStartParticipant);
  Lifeline2 := GetLifeline(FEndParticipant);
  if Assigned(Lifeline1) and Assigned(Lifeline2) then
  begin
    Attributes := TConnectionAttributes.Create;
    try
      Attributes.ArrowStyle := FMethodArrowStyle;
      if Attributes.ArrowStyle = casNew then
        Lifeline2.Created := True;
      if not((FMethodArrowStyle = casReturn) and (FMessage = '<init>')) then
      begin
        if FMethodArrowStyle = casReturn then
          if FConfiguration.SDShowReturn then
            FMessage := ReplaceStr(FAResult, '"', '')
          else
            FMessage := '';
        Attributes.AMessage := FMessage;
        FSequencePanel.ConnectObjects(Lifeline1, Lifeline2, Attributes);
      end;
    finally
      FreeAndNil(Attributes);
    end;
    MIPopupNewLayoutClick(Self);
  end;
end;

procedure TFSequenceForm.SetModified(Modified: Boolean);
begin
  inherited;
  FSequencePanel.OnModified := nil;
  FSequencePanel.IsModified := Modified;
  FSequencePanel.OnModified := OnPanelModified;
end;

procedure TFSequenceForm.ChangeStyle;
begin
  if FConfiguration.IsDark then
  begin
    TBSequence.Images := vilToolbarDark;
    PopupMenuLifelineAndSequencePanel.Images := vilToolbarDark;
    PopupMenuConnection.Images := vilPopupDark;
  end
  else
  begin
    TBSequence.Images := vilToolbarLight;
    PopupMenuLifelineAndSequencePanel.Images := vilToolbarLight;
    PopupMenuConnection.Images := vilPopupLight;
  end;
  FSequencePanel.ChangeStyle;
end;

function TFSequenceForm.GetMaxLifelineHeight: Integer;
begin
  Result := 0;
  for var I := 0 to FLifelines.Count - 1 do
    Result := Max(Result, TLifeline(FLifelines[I]).Height);
end;

function TFSequenceForm.GetMinLifelineTop: Integer;
begin
  Result := MaxInt;
  for var I := 0 to FLifelines.Count - 1 do
    Result := Min(Result, TLifeline(FLifelines[I]).Top);
end;

function TFSequenceForm.GetMinLifelineLeft: Integer;
begin
  Result := MaxInt;
  for var I := 0 to FLifelines.Count - 1 do
    Result := Min(Result, TLifeline(FLifelines[I]).Left);
end;

procedure TFSequenceForm.DPIChanged;
begin
  SetFontSize(0);
end;

procedure TFSequenceForm.MIPopupCopyAsPictureClick(Sender: TObject);
begin
  var Bitmap:= GetSequenceDiagramAsBitmap;
  Clipboard.Assign(Bitmap);
  FreeAndNil(Bitmap);
end;

end.
