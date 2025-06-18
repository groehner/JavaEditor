unit UExplorerForm;

interface

uses
  Classes,
  StdCtrls,
  ExtCtrls,
  Menus,
  System.ImageList,
  Vcl.Controls,
  Vcl.ImgList,
  Vcl.VirtualImageList,
  Vcl.BaseImageCollection,
  SVGIconImageCollection,
  StShlCtl,
  SsShlDlg,
  TB2Dock,
  TB2Toolbar,
  SpTBXItem,
  TB2Item,
  UBaseForm;

type
  TFExplorer = class(TFForm)
    PTop: TPanel;
    PLeft: TPanel;
    CBFavorites: TComboBox;
    Splitter: TSplitter;
    PRight: TPanel;
    PFolder: TPanel;
    PTreeView: TPanel;
    SplitterBottom: TSplitter;
    PListView: TPanel;
    ExplorerTimer: TTimer;
    vilExplorerLight: TVirtualImageList;
    vilExplorerDark: TVirtualImageList;
    icExplorer: TSVGIconImageCollection;
    ToolbarExplorer: TSpTBXToolbar;
    TBNewFolder: TSpTBXItem;
    TBMoveUp: TSpTBXItem;
    TBTreeView: TSpTBXItem;
    TBFavoritDelete: TSpTBXItem;
    TBFavoritAdd: TSpTBXItem;
    TBRefresh: TSpTBXItem;
    TBBack: TSpTBXItem;
    TBClose: TSpTBXItem;
    TBViewMenu: TSpTBXSubmenuItem;
    TBViewDetails: TSpTBXItem;
    TBViewList: TSpTBXItem;
    TBViewSmallIcons: TSpTBXItem;
    TBViewLargeIcons: TSpTBXItem;
    CBFilter: TComboBox;
    PMViewMenu: TSpTBXPopupMenu;
    MIDetails: TSpTBXItem;
    MILIst: TSpTBXItem;
    MISmallIcons: TSpTBXItem;
    MILargeIcons: TSpTBXItem;

    procedure CBFavoritesClick(Sender: TObject);
    procedure CBFavoritesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TBCloseClick(Sender: TObject);
    procedure TBBackClick(Sender: TObject);
    procedure TBRefreshClick(Sender: TObject);
    procedure TBFavoritesAddClick(Sender: TObject);
    procedure SplitterMoved(Sender: TObject);
    procedure TBFavoritesDeleteClick(Sender: TObject);
    procedure TBMoveUpClick(Sender: TObject);
    procedure MIDetailsClick(Sender: TObject);
    procedure TBTreeViewClick(Sender: TObject);
    procedure TBNewFolderClick(Sender: TObject);
    procedure MILargeIconsClick(Sender: TObject);
    procedure MISmallIconsClick(Sender: TObject);
    procedure MIListClick(Sender: TObject);
    procedure CBFilterSelect(Sender: TObject);
    procedure SplitterBottomMoved(Sender: TObject);
    procedure ExplorerTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    FTreeInitialisiert: Boolean;
    FStShellTreeView: TStShellTreeView;
    FStShellNavigator: TStShellNavigator;
    FStShellListView: TStShellListView;
    procedure OnFolderSelectedTreeView(Sender: TObject; Folder: TStShellFolder);
    procedure OnItemDblClickEvent(Sender: TObject; Item: TStShellItem;
      var DefaultAction: Boolean);
    procedure OnListKeyUpEvent(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnListFilledEvent(Sender: TObject);
    procedure TreeViewVerstecken;
    procedure TreeViewZeigen;
  public

    constructor Create(AOwner: TComponent); override;
    procedure New(const FileName: string; State: string);
    procedure UpdateState; override;
    function GetFormType: string; override;
    function GetState: string; override;
    procedure SetState(var Str: string); override;
    procedure PasteFromClipboard; override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure SetFontSize(Delta: Integer); override;
    procedure ChangeStyle; override;
  end;

implementation

uses
  Windows,
  SysUtils,
  Forms,
  Math,
  Clipbrd,
  ComCtrls,
  JvGnugettext,
  StFileOp,
  UUtils,
  UJava,
  UConfiguration,
  UJavaCommands;

{$R *.DFM}

constructor TFExplorer.Create(AOwner: TComponent);
begin
  inherited;
  FormTag := 6;
end;

procedure TFExplorer.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  PLeft.Width := FConfiguration.ReadIntegerU('Explorer', 'PLeft.Width', 200);
  FConfiguration.ReadFiles('Explorer', 'Favorites', CBFavorites.Items);
  PTreeView.Width := Max(FConfiguration.ReadIntegerU('Explorer',
    'PTreeView.Width', 200), 50);
  if FConfiguration.ReadBoolU('Explorer', 'PTreeView.Visible', True) then
    TreeViewZeigen
  else
    TreeViewVerstecken;

  FStShellListView := TStShellListView.Create(Self);
  FStShellListView.Parent := PListView;
  FStShellListView.Align := alClient;
  FStShellListView.PopupMenu := PMViewMenu;
  FStShellListView.Filtered := True;
  FStShellListView.ViewStyle := vsReport;
  FStShellListView.OnListFilled := OnListFilledEvent;
  FStShellListView.OnItemDblClick := OnItemDblClickEvent;
  FStShellListView.OnKeyUp := OnListKeyUpEvent;

  FStShellNavigator := TStShellNavigator.Create(Self);
  FStShellNavigator.Parent := PFolder;
  FStShellNavigator.ListView := FStShellListView;
  FStShellNavigator.Visible := False;

  FStShellTreeView := TStShellTreeView.Create(Self);
  FStShellTreeView.Parent := PTreeView;
  FStShellTreeView.Align := alClient;
  FStShellTreeView.ListView := FStShellListView;
  FStShellTreeView.OnFolderSelected := OnFolderSelectedTreeView;
  FStShellTreeView.OnKeyUp := OnListKeyUpEvent;
  FTreeInitialisiert := False;
  FStShellListView.OpenDialogMode := False;
  CBFilter.Text := 'Java-Editor';
  CBFilterSelect(Self);
  OnClose := FormClose;
  OnMouseActivate := FormMouseActivate;
  ChangeStyle;
  TThread.ForceQueue(nil,
    procedure
    begin
      MIDetailsClick(Self);
    end);
end;

procedure TFExplorer.New(const FileName: string; State: string);
begin
  Pathname := FileName;
  Caption := Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  try
    if Assigned(FStShellListView) then
      FStShellListView.RootFolder := FileName;
  except
    on E: Exception do
      FConfiguration.Log('TFExplorer.New', E);
  end;
  SetState(State);
end;

procedure TFExplorer.TBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFExplorer.CBFavoritesClick(Sender: TObject);
begin
  var
  Str := CBFavorites.Text;
  if (Length(Str) = 2) and (Str[2] = ':') then
    Str := Str + '\';
  if IsHTTP(Str) then
    FJava.NewBrowser(Str, '')
  else
  begin
    CBFavorites.Text := Str;
    if DirectoryExists(Str) then
    begin
      FJava.RenameTabAndWindow(Number, Str);
      Pathname := Str;
      FStShellListView.RootFolder := Str;
      if PTreeView.Visible then
        FStShellTreeView.SelectFolder(Str);
    end;
  end;
end;

procedure TFExplorer.CBFavoritesKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if Key = VK_RETURN then
    CBFavoritesClick(Self);
end;

procedure TFExplorer.TBBackClick(Sender: TObject);
begin
  FStShellNavigator.BackButton.Click;
end;

procedure TFExplorer.TBRefreshClick(Sender: TObject);
begin
  if PTreeView.Visible then
    FStShellTreeView.Refresh(nil);
  FStShellListView.Refresh;
end;

procedure TFExplorer.UpdateState;
begin
  inherited;
  with FJava do
  begin
    if CBFavorites.Focused then
    begin
      SetEnabledMI(MICut, CBFavorites.SelLength > 0);
      SetEnabledMI(MICopy, CBFavorites.SelLength > 0);
      SetEnabledMI(MIPaste, Clipboard.AsText <> '');
    end
    else
    begin
      SetEnabledMI(MICut, False);
      SetEnabledMI(MICopy, False);
      SetEnabledMI(MIPaste, False);
    end;
  end;
end;

procedure TFExplorer.SplitterMoved(Sender: TObject);
begin
  FConfiguration.WriteIntegerU('Explorer', 'PLeft.Width', PLeft.Width);
end;

procedure TFExplorer.SplitterBottomMoved(Sender: TObject);
begin
  FConfiguration.WriteIntegerU('Explorer', 'PTreeView.Width', PTreeView.Width);
end;

procedure TFExplorer.TBFavoritesAddClick(Sender: TObject);
begin
  var
  Int := CBFavorites.Items.IndexOf(CBFavorites.Text);
  if Int = -1 then
  begin
    CBFavorites.Items.Insert(0, CBFavorites.Text);
    FConfiguration.SaveFiles('Explorer', 'Favorites', CBFavorites.Items);
  end;
end;

procedure TFExplorer.TBFavoritesDeleteClick(Sender: TObject);
begin
  var
  Int := CBFavorites.Items.IndexOf(CBFavorites.Text);
  if Int >= 0 then
  begin
    CBFavorites.Items.Delete(Int);
    FConfiguration.SaveFiles('Explorer', 'Favorites', CBFavorites.Items);
  end;
  CBFavorites.Text := '';
end;

procedure TFExplorer.TBMoveUpClick(Sender: TObject);
begin
  FStShellNavigator.MoveUpButton.Click;
end;

procedure TFExplorer.FormPaint(Sender: TObject);
begin
  if not FTreeInitialisiert and PFolder.Visible then
  begin
    if Assigned(FStShellListView) then
      try
        FStShellTreeView.SelectFolder(FStShellListView.RootFolder);
      except
        on E: Exception do
          FConfiguration.Log('TFExplorer.FormPaint', E);
      end;
    CBFilter.SelLength := 0;
    CBFavorites.SelLength := 0;
    FTreeInitialisiert := True;
  end;
end;

procedure TFExplorer.OnListFilledEvent(Sender: TObject);
begin
  var
  Str := FStShellListView.Folder.Path;
  FJava.RenameTabAndWindow(Number, Str);
  Pathname := Str;
  Caption := Str;
  CBFavorites.Text := Str;
end;

procedure TFExplorer.OnItemDblClickEvent(Sender: TObject; Item: TStShellItem;
var DefaultAction: Boolean);
begin
  if Item.IsFolder then
    if UpperCase(ExtractFileExt(Item.Path)) = '.ZIP' then
    begin
      MyJavaCommands.ExecWithoutWait(Item.Path, '', '', SW_SHOWNORMAL);
      DefaultAction := False;
    end
    else
      DefaultAction := True
  else if Item.IsFileSystem then
  begin
    if FJava.Open(Item.Path) then
      FJava.RearrangeFileHistory(Item.Path);
    DefaultAction := False;
  end;
end;

procedure TFExplorer.OnListKeyUpEvent(Sender: TObject; var Key: Word;
Shift: TShiftState);
var
  FileOp: TStFileOperation;
begin
  if Key = VK_DELETE then
  begin
    FileOp := TStFileOperation.Create(Self);
    with FileOp do
    begin
      Operation := fopDelete;
      if (Sender = FStShellListView) then
        for var I := 0 to FStShellListView.SelectedItems.Count - 1 do
          SourceFiles.Add(FStShellListView.SelectedItems[I].Path)
      else if FStShellTreeView.SelectedFolder.IsFolder then
        SourceFiles.Add(FStShellTreeView.SelectedFolder.Path);
      Options := [foAllowUndo];
      ConfirmFiles := True;
      Screen.Cursor := crHourGlass;
      try
        Execute;
        FStShellListView.Refresh;
      finally
        Screen.Cursor := crDefault;
        Free;
      end;
    end;
  end;
  if (Key = VK_RETURN) and (Sender = FStShellListView) then
    for var I := 0 to FStShellListView.SelectedItems.Count - 1 do
      FJava.Open(FStShellListView.SelectedItems[I].Path);
end;

procedure TFExplorer.OnFolderSelectedTreeView(Sender: TObject;
Folder: TStShellFolder);
begin
  CBFavorites.Text := Folder.Path;
  TBBack.Enabled := FStShellNavigator.BackButton.Enabled;
end;

procedure TFExplorer.TreeViewVerstecken;
begin
  PTreeView.Visible := False;
  SplitterBottom.Visible := False;
  FConfiguration.WriteBoolU('Explorer', 'PTreeView.Visible', False);
end;

procedure TFExplorer.TreeViewZeigen;
begin
  PTreeView.Visible := True;
  SplitterBottom.Visible := True;
  SplitterBottom.Left := PTreeView.Width;
  FConfiguration.WriteBoolU('Explorer', 'PTreeView.Visible', True);
end;

procedure TFExplorer.TBTreeViewClick(Sender: TObject);
begin
  if PTreeView.Visible then
    TreeViewVerstecken
  else
    TreeViewZeigen;
end;

procedure TFExplorer.TBNewFolderClick(Sender: TObject);
begin
  FStShellNavigator.NewFolderButton.Click;
end;

procedure TFExplorer.MILargeIconsClick(Sender: TObject);
begin
  FStShellListView.ViewStyle := vsIcon;
  FStShellListView.ViewStyle := vsIcon; // needed!
  for var I := 0 to 3 do
  begin
    PMViewMenu.Items[I].Checked := False;
    TBViewMenu[I].Checked := False;
  end;
  PMViewMenu.Items[0].Checked := True;
  TBViewMenu[0].Checked := True;
end;

procedure TFExplorer.MISmallIconsClick(Sender: TObject);
begin
  FStShellListView.ViewStyle := vsSmallIcon;
  FStShellListView.ViewStyle := vsSmallIcon;
  for var I := 0 to 3 do
  begin
    PMViewMenu.Items[I].Checked := False;
    TBViewMenu[I].Checked := False;
  end;
  PMViewMenu.Items[1].Checked := True;
  TBViewMenu[1].Checked := True;
end;

procedure TFExplorer.MIListClick(Sender: TObject);
begin
  FStShellListView.ViewStyle := vsList;
  FStShellListView.ViewStyle := vsList;
  for var I := 0 to 3 do
  begin
    PMViewMenu.Items[I].Checked := False;
    TBViewMenu[I].Checked := False;
  end;
  PMViewMenu.Items[2].Checked := True;
  TBViewMenu[2].Checked := True;
end;

procedure TFExplorer.MIDetailsClick(Sender: TObject);
begin
  FStShellListView.ViewStyle := vsReport;
  FStShellListView.ViewStyle := vsReport;
  for var I := 0 to 3 do
  begin
    PMViewMenu.Items[I].Checked := False;
    TBViewMenu[I].Checked := False;
  end;
  PMViewMenu.Items[3].Checked := True;
  TBViewMenu[3].Checked := True;
end;

function TFExplorer.GetFormType: string;
begin
  Result := '%X%';
end;

function TFExplorer.GetState: string;
begin
  var
  Str := inherited GetState;
  Str := Str + 'W' + IntToStr(PLeft.Width) + ')' + 'V' +
    IntToStr(Ord(FStShellListView.ViewStyle)) + ')' + 'F' + CBFilter.Text + ')' +
    'T' + BoolToStr(PTreeView.Visible) + ')';
  Result := Str + ')';
end;

procedure TFExplorer.SetState(var Str: string);
var
  Posi: Integer;
begin
  inherited SetState(Str);
  if Copy(Str, 1, 1) = 'W' then
  begin
    Posi := Pos(')', Str);
    PLeft.Width := StrToInt(Copy(Str, 2, Posi - 2));
    Delete(Str, 1, Posi);
  end;
  if Copy(Str, 1, 1) = 'V' then
  begin
    if TryStrToInt(Str[2], Posi) then
      FStShellListView.ViewStyle := TViewStyle(Posi);
    Delete(Str, 1, 3);
  end;
  if Copy(Str, 1, 1) = 'F' then
  begin
    Posi := Pos(')', Str);
    if Pos('(', Str) > 0 then
      Inc(Posi);
    CBFilter.Text := Copy(Str, 2, Posi - 2);
    CBFilterSelect(Self);
    Delete(Str, 1, Posi);
  end;
  if Copy(Str, 1, 1) = 'T' then
  begin
    Posi := Pos(')', Str);
    if Copy(Str, 2, Posi - 2) = '0' then
      TreeViewVerstecken
    else
      TreeViewZeigen;
    Delete(Str, 1, Posi);
  end;
end;

procedure TFExplorer.PasteFromClipboard;
begin
  if CBFavorites.Focused then
    CBFavorites.SelText := Clipboard.AsText;
end;

procedure TFExplorer.CutToClipboard;
begin
  if CBFavorites.Focused then
  begin
    Clipboard.AsText := CBFavorites.SelText;
    CBFavorites.SelText := '';
  end;
end;

procedure TFExplorer.CopyToClipboard;
begin
  if CBFavorites.Focused then
    Clipboard.AsText := CBFavorites.SelText;
end;

procedure TFExplorer.CBFilterSelect(Sender: TObject);
var
  Str: string;
  Posi: Integer;
begin
  Str := CBFilter.Text;
  Posi := Pos('(', Str);
  if Posi = 0 then
    Str := '*.java;*.jfm;*.uml;*.html;*.htm;*.jep;*.txt;*.jar;*.jsp;*.jsg;*.jsd'
  else
  begin
    Str := Copy(Str, Posi + 1, Length(Str) - (Posi + 1));
    if Str = '*.html' then
      Str := '*.html;*.htm';
  end;
  FStShellListView.FileFilter := Str;
end;

procedure TFExplorer.ExplorerTimerTimer(Sender: TObject);
begin
  UpdateState;
end;

procedure TFExplorer.FormDeactivate(Sender: TObject);
begin
  ExplorerTimer.Enabled := False;
end;

procedure TFExplorer.SetFontSize(Delta: Integer);
begin
  FStShellTreeView.Font.Size := FStShellTreeView.Font.Size + Delta;
  if FStShellTreeView.Font.Size < 6 then
    FStShellTreeView.Font.Size := 6;
  FStShellListView.Font.Size := FStShellTreeView.Font.Size;
end;

procedure TFExplorer.ChangeStyle;
begin
  if FConfiguration.IsDark then
    ToolbarExplorer.Images := vilExplorerDark
  else
    ToolbarExplorer.Images := vilExplorerLight;
end;

end.
