unit UExplorerForm;

interface

uses
  Classes, StdCtrls, ExtCtrls, ComCtrls, StShlCtl, SsShlDlg, Menus,
  UBaseForm, Vcl.Controls, Vcl.ToolWin, System.ImageList, Vcl.ImgList,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, SVGIconImageCollection,
  TB2Dock, TB2Toolbar, SpTBXItem, TB2Item;

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
    TreeInitialisiert: boolean;
    procedure OnFolderSelectedTreeView(Sender: TObject; Folder: TStShellFolder);
    procedure OnItemDblClickEvent(Sender: TObject;
      Item: TStShellItem; var DefaultAction : Boolean);
    procedure OnListKeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnListFilledEvent(Sender: TObject);
    procedure TreeViewVerstecken;
    procedure TreeViewZeigen;
  public
    StShellTreeView: TStShellTreeView;
    StShellNavigator: TStShellNavigator;
    StShellListView: TStShellListView;
    constructor Create(AOwner: TComponent); override;
    procedure New(const Filename: string; Zustand: string);
    procedure UpdateState;  override;
    function getFormType: string; override;
    function getState: string; override;
    procedure setState(var s: string); override;
    procedure PasteFromClipboard; override;
    procedure CutToClipboard; override;
    procedure CopyToClipboard; override;
    procedure SetFontSize(Delta: integer); override;
    procedure ChangeStyle; override;
  end;

implementation

uses Windows, SysUtils, Forms, Math, Clipbrd, JvGnugettext, StFileOp,
     UUtils, UJava, UConfiguration, UJavaCommands;

{$R *.DFM}

constructor TFExplorer.Create(AOwner: TComponent);
begin
  inherited;
  FormTag:= 6;
end;

procedure TFExplorer.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
  ToMainPanel;
  PLeft.Width:= FConfiguration.ReadIntegerU('Explorer', 'PLeft.Width', 200);
  FConfiguration.ReadStrings('Explorer', 'Favorites', CBFavorites.Items);
  PTreeView.Width:= max(FConfiguration.ReadIntegerU('Explorer', 'PTreeView.Width', 200), 50);
  if FConfiguration.ReadBoolU('Explorer', 'PTreeView.Visible', true)
    then TreeViewZeigen
    else TreeViewVerstecken;

  StShellListView:= TStShellListView.Create(Self);
  StShellListView.Parent:= PListView;
  StShellListView.Align:= alClient;
  StShellListView.PopupMenu:= PMViewMenu;
  StShellListView.Filtered:= true;
  StShellListView.ViewStyle:= vsReport;
  StShellListView.OnListFilled:= OnListFilledEvent;
  StShellListView.OnItemDblClick:= OnItemDblClickEvent;
  StShellListView.OnKeyUp:= OnListKeyUpEvent;

  StShellNavigator:= TStShellNavigator.Create(Self);
  StShellNavigator.Parent:= PFolder;
  StShellNavigator.ListView:= StShellListView;
  StShellNavigator.Visible:= false;

  StShellTreeView:= TStShellTreeView.Create(Self);
  StShellTreeView.Parent:= PTreeView;
  StShellTreeView.Align:= alClient;
  StShellTreeView.ListView:= StShellListView;
  StShellTreeView.OnFolderSelected:= OnFolderSelectedTreeView;
  StShellTreeView.OnKeyUp:= OnListKeyUpEvent;
  TreeInitialisiert:= false;
  StShellListView.OpenDialogMode:= false;
  CBFilter.Text:= 'Java-Editor';
  CBFilterSelect(Self);
  OnClose:= FormClose;
  OnMouseActivate:= FormMouseActivate;
  ChangeStyle;
  TThread.ForceQueue(nil, procedure
  begin
    MIDetailsClick(Self);
  end);
end;

procedure TFExplorer.New(const Filename: string; Zustand: string);
begin
  Pathname:= Filename;
  Caption:= Pathname;
  FJava.AddToWindowMenuAndTabBar(Number, OpenWindow, Self);
  try
    if assigned(StShellListView) then
      StShellListview.RootFolder:= Filename;
  except
  end;
  SetState(Zustand);
end;

procedure TFExplorer.TBCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFExplorer.CBFavoritesClick(Sender: TObject);
begin
  var s:= CBFavorites.Text;
  if (length(s) = 2) and (s[2] = ':') then s:= s + '\'; 
  if IsHttp(s) then
    FJava.NewBrowser(s, '')
  else begin
    CBFavorites.Text:= s;
    if DirectoryExists(s) then begin
      FJava.RenameTabAndWindow(Number, s);
      Pathname:= s;
      StShellListView.RootFolder:= s;
      if PTreeView.Visible then
        StShellTreeView.SelectFolder(s);
    end;
  end;
end;

procedure TFExplorer.CBFavoritesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Return then
    CBFavoritesClick(Self);
end;

procedure TFExplorer.TBBackClick(Sender: TObject);
begin
  StShellNavigator.BackButton.Click;
end;

procedure TFExplorer.TBRefreshClick(Sender: TObject);
begin
  if PTreeView.Visible then
    StShellTreeView.Refresh(nil);
  StShellListView.Refresh;
end;

procedure TFExplorer.UpdateState;
begin
  inherited;
  with FJava do begin
    if CBFavorites.Focused then begin
      SetEnabledMI(MICut, CBFavorites.SelLength > 0);
      SetEnabledMI(MICopy, CBFavorites.SelLength > 0);
      SetEnabledMI(MIPaste, Clipboard.AsText <> '');
    end else begin
      SetEnabledMI(MICut, false);
      SetEnabledMI(MICopy, false);
      SetEnabledMI(MIPaste, false);
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
  var i:= CBFavorites.Items.IndexOf(CBFavorites.Text);
  if i = -1 then begin
    CBFavorites.Items.Insert(0, CBFavorites.Text);
    FConfiguration.SaveStrings('Explorer', 'Favorites', CBFavorites.Items);
  end;
end;

procedure TFExplorer.TBFavoritesDeleteClick(Sender: TObject);
begin
  var i:= CBFavorites.Items.IndexOf(CBFavorites.Text);
  if i >= 0 then begin
    CBFavorites.Items.Delete(i);
    FConfiguration.SaveStrings('Explorer', 'Favorites', CBFavorites.Items);
  end;
  CBFavorites.Text:= '';
end;

procedure TFExplorer.TBMoveUpClick(Sender: TObject);
begin
  StShellNavigator.MoveUpButton.Click;
end;

procedure TFExplorer.FormPaint(Sender: TObject);
begin
  if not TreeInitialisiert and PFolder.Visible then begin
    if assigned(StShellListView) then
      try
        StShellTreeView.SelectFolder(StShellListView.RootFolder);
      except
      end;
    CBFilter.SelLength:= 0;
    CBFavorites.SelLength:= 0;
    TreeInitialisiert:= true;
  end;
end;

procedure TFExplorer.OnListFilledEvent(Sender: TObject);
begin
  var s:= StShellListView.Folder.Path;
  FJava.RenameTabAndWindow(Number, s);
  Pathname:= s;
  Caption:= s;
  CBFavorites.Text:= s;
end;

procedure TFExplorer.OnItemDblClickEvent(Sender: TObject;
    Item: TStShellItem; var DefaultAction: Boolean);
begin
  if Item.IsFolder then
    if Uppercase(ExtractFileExt(Item.Path)) = '.ZIP' then begin
      myJavaCommands.ExecWithoutWait(Item.Path, '', '', SW_ShowNormal);
      DefaultAction:= false;
    end else
      DefaultAction:= true
  else if Item.IsFileSystem then begin
    if FJava.Open(Item.Path) then
      FJava.RearrangeFileHistory(Item.Path);
    DefaultAction:= false;
  end;
end;

procedure TFExplorer.OnListKeyUpEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
  var FileOp: TStFileOperation; i: integer;
begin
  if Key = VK_Delete then begin
    FileOp:= TStFileOperation.Create(Self);
    with FileOp do begin
      Operation:= fopDelete;
      if (Sender = StShellListView) then
        for i:= 0 to StShellListView.SelectedItems.Count - 1 do
          SourceFiles.Add(StShellListView.SelectedItems[i].Path)
      else
        if StShellTreeView.SelectedFolder.IsFolder then
          SourceFiles.Add(StShellTreeView.SelectedFolder.Path);
      Options:= [foAllowUndo];
      ConfirmFiles:= true;
      Screen.Cursor:= crHourGlass;
      try
        Execute;
        StShellListView.Refresh;
      finally
        Screen.Cursor:= crDefault;
        Free;
      end;
    end;
  end;
  if (Key = VK_Return) and (Sender = StShellListView) then
    for i:= 0 to StShellListView.SelectedItems.Count - 1 do
        FJava.Open(StShellListView.SelectedItems[i].Path)
end;

procedure TFExplorer.OnFolderSelectedTreeView(Sender: TObject; Folder: TStShellFolder);
begin
  CBFavorites.Text:= Folder.Path;
  TBBack.Enabled:= StShellNavigator.BackButton.Enabled;
end;

procedure TFExplorer.TreeViewVerstecken;
begin
  PTreeView.Visible:= False;
  SplitterBottom.Visible:= False;
  FConfiguration.WriteBoolU('Explorer', 'PTreeView.Visible', false);
end;

procedure TFExplorer.TreeViewZeigen;
begin
  PTreeView.Visible:= True;
  SplitterBottom.Visible:= True;
  SplitterBottom.Left:= PTreeView.Width;
  FConfiguration.WriteBoolU('Explorer', 'PTreeView.Visible', true);
end;

procedure TFExplorer.TBTreeViewClick(Sender: TObject);
begin
  if PTreeView.Visible
    then TreeViewVerstecken
    else TreeViewZeigen;
end;

procedure TFExplorer.TBNewFolderClick(Sender: TObject);
begin
  StShellNavigator.NewFolderButton.Click;
end;

procedure TFExplorer.MILargeIconsClick(Sender: TObject);
begin
  StShellListView.ViewStyle:= vsIcon;
  StShellListView.ViewStyle:= vsIcon;  // needed!
  for var i:= 0 to 3 do begin
    PMViewMenu.Items[i].Checked:= false;
    TBViewMenu.Items[i].Checked:= false;
  end;
  PMViewMenu.Items[0].Checked:= true;
  TBViewMenu.Items[0].Checked:= true;
end;

procedure TFExplorer.MISmallIconsClick(Sender: TObject);
begin
  StShellListView.ViewStyle:= vsSmallIcon;
  StShellListView.ViewStyle:= vsSmallIcon;
  for var i:= 0 to 3 do begin
    PMViewMenu.Items[i].Checked:= false;
    TBViewMenu.Items[i].Checked:= false;
  end;
  PMViewMenu.Items[1].Checked:= true;
  TBViewMenu.Items[1].Checked:= true;
end;

procedure TFExplorer.MIListClick(Sender: TObject);
begin
  StShellListView.ViewStyle:= vsList;
  StShellListView.ViewStyle:= vsList;
  for var i:= 0 to 3 do begin
    PMViewMenu.Items[i].Checked:= false;
    TBViewMenu.Items[i].Checked:= false;
  end;
  PMViewMenu.Items[2].Checked:= true;
  TBViewMenu.Items[2].Checked:= true;
end;

procedure TFExplorer.MIDetailsClick(Sender: TObject);
begin
  StShellListView.ViewStyle:= vsReport;
  StShellListView.ViewStyle:= vsReport;
  for var i:= 0 to 3 do begin
    PMViewMenu.Items[i].Checked:= false;
    TBViewMenu.Items[i].Checked:= false;
  end;
  PMViewMenu.Items[3].Checked:= true;
  TBViewMenu.Items[3].Checked:= true;
end;

function TFExplorer.getFormType: string;
begin
  Result:= '%X%';
end;

function TFExplorer.getState: string;
begin
  var s:= inherited GetState;
  s:= s + 'W' + IntToStr(PLeft.Width) + ')' +
          'V' + IntToStr(Ord(StShellListView.ViewStyle)) + ')' +
          'F' + CBFilter.Text + ')' +
          'T' + BoolToStr(PTreeView.Visible) + ')';
  Result:= s + ')';
end;

procedure TFExplorer.setState(var s: string);
  var p: integer;
begin
  inherited SetState(s);
  if copy(s, 1, 1) = 'W' then begin
    p:= Pos(')', s);
    PLeft.Width:= StrToInt(Copy(s, 2, p-2));
    Delete(s, 1, p);
  end;
  if copy(s, 1, 1) = 'V' then begin
    if TryStrToInt(s[2], p) then
      StShellListView.ViewStyle:= TViewStyle(p);
    delete(s, 1, 3);
  end;
  if copy(s, 1, 1) = 'F' then begin
    p:= Pos(')', s);
    if Pos('(', s) > 0 then inc(p);
    CBFilter.Text:= copy(s, 2, p-2);
    CBFilterSelect(Self);
    delete(s, 1, p);
  end;
  if copy(s, 1, 1) = 'T' then begin
    p:= Pos(')', s);
    if copy(s, 2, p-2) = '0'
      then TreeViewVerstecken
      else TreeViewZeigen;
    delete(s, 1, p);
  end;
end;

procedure TFExplorer.PasteFromClipboard;
begin
  if CBFavorites.Focused then
    CBFavorites.SelText:= Clipboard.AsText;
end;

procedure TFExplorer.CutToClipboard;
begin
  if CBFavorites.Focused then begin
    Clipboard.AsText:= CBFavorites.SelText;
    CBFavorites.SelText:= '';
  end;
end;

procedure TFExplorer.CopyToClipboard;
begin
  if CBFavorites.Focused then
    Clipboard.AsText:= CBFavorites.SelText;
end;

procedure TFExplorer.CBFilterSelect(Sender: TObject);
  var s: string; p: integer;
begin
  s:= CBFilter.Text;
  p:= Pos('(', s);
  if p = 0
    then s:= '*.java;*.jfm;*.uml;*.html;*.htm;*.jep;*.txt;*.jar;*.jsp;*.jsg;*.jsd'
  else begin
    s:= copy(s, p + 1, length(s) - (p+1));
    if s = '*.html' then s:= '*.html;*.htm';
  end;
  StShellListView.FileFilter:= s;
end;

procedure TFExplorer.ExplorerTimerTimer(Sender: TObject);
begin
  UpdateState;
end;

procedure TFExplorer.FormDeactivate(Sender: TObject);
begin
  ExplorerTimer.Enabled:= false;
end;

procedure TFExplorer.SetFontSize(Delta: integer);
begin
  StShellTreeView.Font.Size:= StShellTreeView.Font.Size + Delta;
  if StShellTreeView.Font.Size < 6 then StShellTreeView.Font.Size:= 6;
  StShellListView.Font.Size:= StShellTreeView.Font.Size;
end;

procedure TFExplorer.ChangeStyle;
begin
  if FConfiguration.isDark
    then ToolBarExplorer.Images:= vilExplorerDark
    else ToolBarExplorer.Images:= vilExplorerLight;
end;

end.
