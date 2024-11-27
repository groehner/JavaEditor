(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Sebastian Zierer (Unicode)
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsShlDlg.pas 1.02                         *}
{*********************************************************}
{* ShellShock: Custom File Dialog Components             *}
{*   StShellNavigator                                    *}
{*   StShellDialogPanel                                  *}
{*********************************************************}

{$I SsDefine.inc}
{$R SsNav.res}

{$I+} {I/O Checking On}
{$H+} {Huge strings}

unit SsShlDlg;

{$IFDEF VER130}
  {$IFDEF BCB}
    {$HPPEMIT '#define NO_WIN32_LEAN_AND_MEAN' }
  {$ENDIF}
{$ENDIF}

{$IFNDEF VERSION3}
  !! Error: This unit can only be compiled with Delphi 3 and above
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Forms, Classes, Controls, ExtCtrls,
  StdCtrls, Buttons, StShlCtl, ComCtrls, Graphics,
  {$IFDEF VERSION6} {$WARN UNIT_PLATFORM OFF} {$ENDIF}
  {$IFNDEF VERSION2010} FileCtrl, {$ENDIF}
  {$IFDEF VERSION6} {$WARN UNIT_PLATFORM ON} {$ENDIF}
  System.Types;

const
  MSG_DOINIT = WM_USER + 1;

type

  TSsNavigatorStyle = (nsWin9x, nsWin2k, nsWinXP);
  TSsNavigatorButtons = (
    nbBack, nbMoveUp, nbNewFolder, nbList, nbDetails, nbView);

  TSsNavigatorButtonsSet = set of TSsNavigatorButtons;

  TSsNavigatorButtonClickEvent = procedure(Sender : TObject;
    Button : TSsNavigatorButtons; var DefaultAction : Boolean) of object;

  TSsNavigatorViewStyleChangingEvent = procedure(Sender : TObject;
    Style : TViewStyle; var DefaultAction : Boolean) of object;

  TSsClickEvent = procedure(
    Sender : TObject; var DefaultAction : Boolean) of object;

  TSsMenuButton = class;  
  TSsPanelButton = class;
  TSsSpeedButton = class;
  TSsShellListView = class;
  TSsShellComboBox = class;
  TSsComboBox = class;

  TStCustomShellNavigator = class(TCustomPanel)
  protected {private}
    FBackButton      : TSsSpeedButton;
    FButtons         : TSsNavigatorButtonsSet;
    FComboBoxLabel   : TLabel;
    FDetailsButton   : TSsSpeedButton;
    FLeftOffset      : Integer;
    FListButton      : TSsSpeedButton;
    FListView        : TStCustomShellListView;
    FMoveUpButton    : TSsSpeedButton;
    FNewFolderButton : TSsSpeedButton;
    FShellComboBox   : TSsShellComboBox;
    FStyle           : TSsNavigatorStyle;
    FViewButton      : TSsMenuButton;

    FOnButtonClick    : TSsNavigatorButtonClickEvent;
    FOnFolderSelected : TStFolderSelectedEvent;
    FOnViewStyleChanging : TSsNavigatorViewStyleChangingEvent;

    { Internal variables }
    Initialized       : Boolean;
    RecentFoldersList : TList;

    { Overridden methods }
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure WndProc(var Message : TMessage); override;

    { Internal methods }
    procedure FolderSelected(Sender : TObject; Folder : TStShellFolder);
    procedure Layout;
    procedure ButtonClick(Sender: TObject);

    { Property methods }
    procedure SetButtons(Value : TSsNavigatorButtonsSet);
    procedure SetListView(Value : TStCustomShellListView);
    procedure SetStyle(Value : TSsNavigatorStyle);
    procedure SetLeftOffset(Value : Integer);
    function  GetVersion : string;
    procedure SetVersion(const Value : string);

    procedure SetOnFolderSelected(Value : TStFolderSelectedEvent);

    { popup menu event handlers }
    procedure DoLargeIconClick(Sender : TObject);
    procedure DoSmallIconClick(Sender : TObject);
    procedure DoListClick(Sender : TObject);
    procedure DoDetailsClick(Sender : TObject);

    { Properties }
    property BackButton : TSsSpeedButton
      read FBackButton;

    property Buttons : TSsNavigatorButtonsSet
      read FButtons
      write SetButtons;

    property ComboBox : TSsShellComboBox
      read FShellComboBox;

    property ComboBoxLabel : TLabel
      read FComboBoxLabel;

    property DetailsButton : TSsSpeedButton
      read FDetailsButton;

    property LeftOffset : Integer
      read FLeftOffset
      write SetLeftOffset;

    property ListView : TStCustomShellListView
      read FListView
      write SetListView;

    property ListButton : TSsSpeedButton
      read FListButton;

    property MoveUpButton : TSsSpeedButton
      read FMoveUpButton;

    property NewFolderButton : TSsSpeedButton
      read FNewFolderButton;

    property Style : TSsNavigatorStyle
      read FStyle
      write SetStyle;

    property ViewButton : TSsMenuButton
      read FViewButton;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    { Events }
    property OnButtonClick : TSsNavigatorButtonClickEvent
      read FOnButtonClick
      write FOnButtonClick;

    property OnFolderSelected : TStFolderSelectedEvent
      read FOnFolderSelected
      write SetOnFolderSelected;

    property OnViewStyleChanging : TSsNavigatorViewStyleChangingEvent
      read FOnViewStyleChanging
      write FOnViewStyleChanging;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStShellNavigator = class(TStCustomShellNavigator)
  public
    property BackButton;
    property ComboBox;
    property ComboBoxLabel;
    property DetailsButton;
    property ListButton;
    property MoveUpButton;
    property NewFolderButton;
    property ViewButton;
  published
    property Buttons;
    property ListView;
    property LeftOffset;
    property Style;
    property Version;

    property OnButtonClick;
    property OnFolderSelected;
    property OnViewStyleChanging;

    { TPanel properties and events }
  protected
    property Caption;
  public
    {$IFDEF VERSION4}
    property DockManager;
    {$ENDIF}
  published
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;
    {$ENDIF}
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  TStCustomDialogPanel = class(TCustomPanel)
  protected
    FAllowResize      : Boolean;
    FCancelButton     : TSsPanelButton;
    FDefaultExt       : string;
    FFileName         : string;
    FFileNameEdit     : TEdit;
    FFileNameLabel    : TLabel;
    FFiles            : TStringList;
    FFileTypeComboBox : TSsComboBox;
    FFileTypeLabel    : TLabel;
    FFilter           : string;
    FFilterIndex      : Integer;
    FInitialDir       : string;
    FListView         : TSsShellListView;
    FNavigator        : TStShellNavigator;
    FOpenButton       : TSsPanelButton;
    FOpenButtonCaption: string;
    FParentForm       : TForm;
    FStyle            : TSsNavigatorStyle;

    FOnItemClick         : TSsClickEvent;
    FOnItemDblClick      : TSsClickEvent;
    FOnOpenButtonClick   : TSsClickEvent;
    FOnCancelButtonClick : TSsClickEvent;

    FilterList    : TStringList;
    RecreatingWnd : Boolean;

    {internal methods}
    procedure DoOpenButtonClick; virtual;
    procedure DoCancelButtonClick; virtual;
    procedure DoComboBoxChange; virtual;
    procedure DoListViewDblClick; virtual;
    procedure DoListViewClick; virtual;

    {property methods}
    procedure SetInitialDir(Value : string);
    procedure SetFileName(Value : string);
    procedure SetFilter(Value : string);
    procedure SetOpenButtonCaption(Value : string);
    procedure SetStyle(Value : TSsNavigatorStyle);
    function  GetVersion : string;
    procedure SetVersion(const Value : string);

    {overridden methods}
    procedure WndProc(var Message : TMessage); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Resize; override;

    {properties}
    property AllowResize : Boolean
      read FAllowResize
      write FAllowResize;

    property CancelButton : TSsPanelButton
      read FCancelButton;

    property DefaultExt : string
      read FDefaultExt
      write FDefaultExt;

    property FileName : string
      read FFileName
      write SetFileName;

    property FileNameEdit : TEdit
      read FFileNameEdit;

    property FileNameLabel : TLabel
      read FFileNameLabel;

    property Files : TStringList
      read FFiles;

    property FileTypeComboBox : TSsComboBox
      read FFileTypeComboBox;

    property FileTypeLabel : TLabel
      read FFileTypeLabel;

    property Filter : string
      read FFilter
      write SetFilter;

    property FilterIndex : Integer
      read FFilterIndex
      write FFilterIndex;

    property InitialDir : string
      read FInitialDir
      write SetInitialDir;

    property ListView : TSsShellListView
      read FListView;

    property Navigator : TStShellNavigator
      read FNavigator;

    property OpenButton : TSsPanelButton
      read FOpenButton;

    property OpenButtonCaption : string
      read FOpenButtonCaption
      write SetOpenButtonCaption;

    property ParentForm : TForm
      read FParentForm
      write FParentForm;

    property Style : TSsNavigatorStyle
      read FStyle
      write SetStyle;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    {events}
    property OnItemClick : TSsClickEvent
      read FOnItemClick
      write FOnItemClick;

    property OnItemDblClick : TSsClickEvent
      read FOnItemDblClick
      write FOnItemDblClick;

    property OnOpenButtonClick : TSsClickEvent
      read FOnOpenButtonClick
      write FOnOpenButtonClick;

    property OnCancelButtonClick : TSsClickEvent
      read FOnCancelButtonClick
      write FOnCancelButtonClick;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStDialogPanel = class(TStCustomDialogPanel)
  public
    property CancelButton;
    property FileNameEdit;
    property FileNameLabel;
    property Files;
    property FileTypeComboBox;
    property FileTypeLabel;
    property ListView;
    property Navigator;
    property OpenButton;
  published
    property AllowResize;
    property DefaultExt;
    property FileName;
    property Filter;
    property FilterIndex;
    property InitialDir;
    property OpenButtonCaption;
    property ParentForm;
    property Style;
    property Version;

    {events}
    property OnItemClick;
    property OnItemDblClick;
    property OnOpenButtonClick;
    property OnCancelButtonClick;

    { TPanel properties and events }
  protected
    property Caption;
  public
    {$IFDEF VERSION4}
    property DockManager;
    {$ENDIF}
  published
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property OnCanResize;
    property OnConstrainedResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGetSiteInfo;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;
    {$ENDIF}
    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Locked;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  TSsButtonType = (btOpen, btCancel);
  TSsPanelButton = class(TButton)
  protected
    FButtonType : TSsButtonType;
  public
    procedure Click; override;
    property ButtonType : TSsButtonType
      read FButtonType
      write FButtonType;
  end;

  TSsSpeedButton = class(TSpeedButton)
  public
    procedure Click; override;
  end;

  TSsShellComboBox = class(TStShellComboBox)
  protected
    procedure Click; override;
    procedure DoFolderChanging(SF : TStShellFolder); override;
  end;

  TSsComboBox = class(TComboBox)
  protected
    procedure Change; override;
  end;

  TSsShellListView = class(TStShellListView)
  protected
    procedure DblClick; override;
    procedure Click; override;
  end;

  TSsDrawFrameType = (ftUp, ftDown, ftNone);
  TSsMenuButton = class(TCustomControl)
  private
    function GetGlyph: TBitmap;
  protected
    FShowGlyph    : Boolean;

    mbDown        : Boolean;
    mbGlyph       : TBitmap;
    mbImage       : TImageList;
    mbState       : TButtonState;
    DrawFrameType : TSsDrawFrameType;
    Timer         : TTimer;

    procedure SetShowGlyph(Value : Boolean);
    procedure SetGlyph(Glyph: TBitmap);

    procedure TimerEvent(Sender: TObject);

    procedure WMRButtonUp(var Msg : TWMRButtonUp);
      message WM_RBUTTONUP;

  protected
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseMove(Shift : TShiftState; X, Y : Integer);
      override;
    procedure MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
      override;
    procedure Paint;
      override;

    property ShowGlyph : Boolean
      read FShowGlyph
      write SetShowGlyph
      default True;

    property Glyph : TBitmap
      read GetGlyph write SetGlyph;

  public
    constructor Create(AOwner : TComponent);
      override;
    procedure Click;
      override;
    destructor Destroy;
      override;
  end;

implementation

uses Menus, SsBase, SsConst;

const
  DefaultPanelWidth = 426;
  DefaultPanelHeight = 242;
{ TStCustomShellNavigator }

constructor TStCustomShellNavigator.Create(AOwner : TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  Align := alTop;
  Height := 35;
  Left := 0;
  Caption := '';
  ShowHint := True;
  FStyle := nsWin2k;
  FButtons := [nbBack, nbMoveUp, nbNewFolder, nbList, nbDetails, nbView];
  FLeftOffset := 10;
  FComboBoxLabel := TLabel.Create(Self);
  with FComboBoxLabel do begin
    Top := 10;
    Left := FLeftOffset;
    Caption := ssscNavLookInCaption;
    Parent := Self;
  end;
  FShellComboBox := TSsShellComboBox.Create(Self);
  with FShellComboBox do begin
    Top := 6;
    Left := FComboBoxLabel.Left + FComboBoxLabel.Width + 5;
    Width := 220;
    Parent := Self;
    OnFolderSelected := FolderSelected;
  end;
  FComboBoxLabel.FocusControl := FShellComboBox;

  RecentFoldersList := TList.Create;
  { Create all of the buttons so that accidentally accessing an }
  { invalid button doesn't result in an AV for the user. }
  FMoveUpButton := TSsSpeedButton.Create(Self);
  FNewFolderButton := TSsSpeedButton.Create(Self);
  FListButton := TSsSpeedButton.Create(Self);
  FDetailsButton := TSsSpeedButton.Create(Self);
  FBackButton := TSsSpeedButton.Create(Self);

  {!!.02 - Added }
  FComboBoxLabel.Parent := Self;
  FShellComboBox.Parent := Self;
  FMoveUpButton.Parent := Self;
  FNewFolderButton.Parent := Self;
  FListButton.Parent := Self;
  FDetailsButton.Parent := Self;
  FBackButton.Parent := Self;
  {!!.02 - End Added }

  FViewButton := nil;
end;

destructor TStCustomShellNavigator.Destroy;
var
  I : Integer;
begin
  if Assigned(FListView) and not (csDestroying in ComponentState) then
    with FListView as TStShellListView do begin
      ComboBox := nil;
      if Length(RootFolder) <> 0 then
        RootFolder := '';
      if SpecialRootFolder <> sfNone then
        SpecialRootFolder := sfNone;
      Clear;
    end;
  for I := 0 to RecentFoldersList.Count - 1 do
    TStShellFolder(RecentFoldersList[I]).Free;
  RecentFoldersList.Free;
  inherited;
end;

procedure TStCustomShellNavigator.CreateWnd;
begin
  inherited;
  FShellComboBox.HandleNeeded;
  Caption := '';
  if not (csLoading in ComponentState) then begin
    Layout;
    if FShellComboBox.ListView <> nil then
      FShellComboBox.DoClick;
  end;
end;

procedure TStCustomShellNavigator.Loaded;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    Layout;
    if FShellComboBox.ListView <> nil then
      FShellComboBox.DoClick;
  end;
end;

procedure TStCustomShellNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FListView) and (Operation = opRemove)
      and (csDesigning in ComponentState) then
    FListView := nil;
end;

procedure TStCustomShellNavigator.WndProc(var Message: TMessage);
begin
  inherited;
  with Message do begin
    if (Msg = WM_PAINT) and (not Initialized) then begin
      Initialized := True;
      Repaint;
      PostMessage(Handle, MSG_DOINIT, 0, 0);
    end;
    { Force a click of the combo box. We need to use this trick here }
    { because we are now assured that all components are created,    }
    { loaded, and hooked up by this point. }
    if Msg = MSG_DOINIT then
      if FShellComboBox.ListView <> nil then begin
        if FShellComboBox.ItemIndex = -1 then
          FShellComboBox.ItemIndex := 0;
        FShellComboBox.DoClick;
        if Assigned(FViewButton) and Assigned(FListView) then
          case TStShellListView(FListView).ViewStyle of
            vsIcon      : FViewButton.PopupMenu.Items[0].Checked := True;
            vsSmallIcon : FViewButton.PopupMenu.Items[1].Checked := True;
            vsList      : FViewButton.PopupMenu.Items[2].Checked := True;
            vsReport    : FViewButton.PopupMenu.Items[3].Checked := True;
          end;
      end;
  end;
end;

procedure TStCustomShellNavigator.FolderSelected(Sender : TObject;
  Folder : TStShellFolder);
begin
  if Assigned(FOnFolderSelected) then
    FOnFolderSelected(FShellComboBox, Folder);
end;

procedure TStCustomShellNavigator.SetStyle(Value : TSsNavigatorStyle);
begin
  if Value <> FStyle then begin
    FStyle := Value;
    Layout;
  end;
end;

procedure TStCustomShellNavigator.SetButtons(Value : TSsNavigatorButtonsSet);
begin
  if FButtons <> Value then begin
    FButtons := Value;
    Layout;
  end;
end;

procedure TStCustomShellNavigator.SetListView(Value : TStCustomShellListView);
begin
  if Value = FListView then
    Exit;
  FListView := Value;
  if (FShellComboBox <> nil) and (Value <> nil) then begin
    FShellComboBox.ListView := Value;
    TStShellListView(FListView).ComboBox := FShellComboBox;
    TStShellListView(FListView).OpenDialogMode := True;
    FShellComboBox.DoClick;
  end;
end;

procedure TStCustomShellNavigator.Layout;
var
  NeedMenu : Boolean;
  Menu : TPopupMenu;
  MI : TMenuItem;
begin
  if (csDesigning in ComponentState) then begin
    if Assigned(FBackButton) then
      FBackButton.Free;
    if Assigned(FMoveUpButton) then
      FMoveUpButton.Free;
    if Assigned(FNewFolderButton) then
      FNewFolderButton.Free;
    if Assigned(FListButton) then
      FListButton.Free;
    if Assigned(FDetailsButton) then
      FDetailsButton.Free;
    if Assigned(FViewButton) then
      FViewButton.Free;
    FMoveUpButton := TSsSpeedButton.Create(Self);
    FNewFolderButton := TSsSpeedButton.Create(Self);
    FListButton := TSsSpeedButton.Create(Self);
    FDetailsButton := TSsSpeedButton.Create(Self);
    FBackButton := TSsSpeedButton.Create(Self);
    {!!.02 - Added }
    FMoveUpButton.Parent := Self;
    FNewFolderButton.Parent := Self;
    FListButton.Parent := Self;
    FDetailsButton.Parent := Self;
    FBackButton.Parent := Self;
    {!!.02 - End Added }
    FViewButton := nil;
  end;
  FBackButton.Visible := False;
  FMoveUpButton.Visible := False;
  FNewFolderButton.Visible := False;
  FListButton.Visible := False;
  FDetailsButton.Visible := False;
  if Assigned(FViewButton) then
    FViewButton.Visible := False;
  case FStyle of
    nsWin9x : begin
      if (nbMoveUp in FButtons) then begin
        with FMoveUpButton do begin
          Parent := Self;
          Left := FShellComboBox.Left + FShellComboBox.Width + 10;
          Top := 6;
          Width := 23;
          Height := 22;
          Flat := False;
          Hint := ssscNavHintMoveUp;
          Glyph.LoadFromResourceName(HInstance, 'MOVEUP');
          OnClick := ButtonClick;
          Visible := True;
          NumGlyphs := 1;
        end;
      end;
      if (nbNewFolder in FButtons) then begin
        with FNewFolderButton do begin
          Parent := Self;
          if FMoveUpButton.Visible then
            Left := FMoveUpButton.Left + FMoveUpButton.Width + 10
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 43;
          Left := 320;
          Top := 6;
          Width := 23;
          Height := 22;
          Flat := False;
          Hint := ssscNavHintNewFolder;
          Glyph.LoadFromResourceName(HInstance, 'NEWFOLDER');
          OnClick := ButtonClick;
          Visible := True;
          NumGlyphs := 1;
        end;
      end;
      if (nbList in FButtons) then begin
        with FListButton do begin
          Parent := Self;
          if FNewFolderButton.Visible then
            Left := FNewFolderButton.Left + FNewFolderButton.Width + 10
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 76;
          Left := 352;
          Top := 6;
          Width := 23;
          Height := 22;
          GroupIndex := 1;
          Glyph.LoadFromResourceName(HInstance, 'LIST');
          OnClick := ButtonClick;
          Down := True;
          Visible := True;
          NumGlyphs := 1;
        end;
      end;
      if (nbDetails in FButtons) then begin
        with FDetailsButton do begin
          Parent := Self;
          if FListButton.Visible then
            Left := FListButton.Left + FListButton.Width
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 102;
          Top := 6;
          Width := 23;
          Height := 22;
          Down := False;
          GroupIndex := 1;
          Glyph.LoadFromResourceName(HInstance, 'DETAILS');
          OnClick := ButtonClick;
          Visible := True;
          NumGlyphs := 1;
        end;
      end;
    end;
    nsWin2k, nsWinXP : begin
      if (nbBack in FButtons) then begin
        with FBackButton do begin
          Parent := Self;
          Left := FShellComboBox.Left + FShellComboBox.Width + 3;
          Top := 6;
          Width := 23;
          Height := 22;
          Hint := ssscNavHintLastFolder;
          Flat := True;
          Enabled := False;
          NumGlyphs := 1;
          if FStyle = nsWin2k then
            Glyph.LoadFromResourceName(HInstance, 'BACK')
          else begin
            Glyph.LoadFromResourceName(HInstance, 'XPBACK');
            NumGlyphs := 2;
          end;
          OnClick := ButtonClick;
          Visible := True;
        end;
      end;
      if (nbMoveUp in FButtons) then begin
        with FMoveUpButton do begin
          Parent := Self;
          if FBackButton.Visible then
            Left := FBackButton.Left + FBackButton.Width
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 26;
          Top := 6;
          Width := 23;
          Height := 22;
          Hint := ssscNavHintMoveUp;
          Flat := True;
          NumGlyphs := 1;
          if FStyle = nsWin2k then
            Glyph.LoadFromResourceName(HInstance, 'MOVEUP')
          else begin
            Glyph.LoadFromResourceName(HInstance, 'XPMOVEUP');
            NumGlyphs := 2;
          end;
          OnClick := ButtonClick;
          Visible := True;
        end;
      end;
      if (nbNewFolder in FButtons) then begin
        with FNewFolderButton do begin
          Parent := Self;
          if FMoveUpButton.Visible then
            Left := FMoveUpButton.Left + FMoveUpButton.Width
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 49;
          Top := 6;
          Width := 23;
          Height := 22;
          Hint := ssscNavHintNewFolder;
          Flat := True;
          NumGlyphs := 1;
          if FStyle = nsWin2k then
            Glyph.LoadFromResourceName(HInstance, 'NEWFOLDER')
          else
            Glyph.LoadFromResourceName(HInstance, 'XPNEWFOLDER');
          OnClick := ButtonClick;
          Visible := True;
        end;
      end;
      if (nbView in FButtons) then begin
        if not Assigned(FViewButton) then begin
          FViewButton := TSsMenuButton.Create(Self);
          FViewButton.Parent := Self;    {!!.02}
          NeedMenu := True;
        end else
          NeedMenu := False;
        with FViewButton do begin
          Parent := Self;
          if FNewFolderButton.Visible then
            Left := FNewFolderButton.Left + FNewFolderButton.Width
          else
            Left := FShellComboBox.Left + FShellComboBox.Width + 72;
          Top := 6;
          Width := 32;
          Height := 22;
          Hint := ssscNavHintViewMenu;
          TabStop := False;
          if FStyle = nsWin2k then
            Glyph.LoadFromResourceName(HInstance, 'VIEW')
          else
            Glyph.LoadFromResourceName(HInstance, 'XPVIEW');
          OnClick := ButtonClick;
          Visible := True;
        end;
        if NeedMenu then begin
          Menu := TPopupMenu.Create(Self);
          MI := TMenuItem.Create(Self);
          MI.Caption := ssscViewCaptionLargeIcon;
          MI.RadioItem := True;
          MI.OnClick := DoLargeIconClick;
          Menu.Items.Add(MI);

          MI := TMenuItem.Create(Self);
          MI.Caption := ssscViewCaptionSmallIcon;
          MI.RadioItem := True;
          MI.OnClick := DoSmallIconClick;
          Menu.Items.Add(MI);

          MI := TMenuItem.Create(Self);
          MI.Caption := ssscViewCaptionList;
          MI.RadioItem := True;
          MI.OnClick := DoListClick;
          Menu.Items.Add(MI);

          MI := TMenuItem.Create(Self);
          MI.RadioItem := True;
          MI.Caption := ssscViewCaptionDetails;
          MI.OnClick := DoDetailsClick;
          Menu.Items.Add(MI);
          FViewButton.PopupMenu := Menu;
        end;
      end;
    end;
  end;
  Repaint;
end;

procedure TStCustomShellNavigator.ButtonClick(Sender: TObject);
var
  LV : TStShellListView;
  DefaultAction : Boolean;
  SF : TStShellFolder;
begin
  DefaultAction := True;
  LV := FShellComboBox.ListView as TStShellListView;
  if Sender = FBackButton then begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, nbBack, DefaultAction);
    if (DefaultAction) then begin
      { Load last folder in list and delete it from list. }
      if RecentFoldersList.Count > 0 then begin
        SF := TStShellFolder(RecentFoldersList[RecentFoldersList.Count - 1]);
        if (LV <> nil) then
          LV.LoadFolder(SF)
        else
          FShellComboBox.ItemIndex :=
            FShellComboBox.Items.IndexOf(SF.DisplayName);
        SF.Free;
        RecentFoldersList.Delete(RecentFoldersList.Count - 1);
        FBackButton.Enabled := RecentFoldersList.Count > 0;
      end;
    end;
  end;
  if Sender = FMoveUpButton then begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, nbMoveUp, DefaultAction);
    if (DefaultAction) then begin
      if (LV <> nil) then begin
        LV.MoveUpOneLevel;
        if (LV.Folder.IsDesktop) then
          FMoveUpButton.Enabled := False;
      end;
    end;
  end;
  if Sender = FNewFolderButton then begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, nbNewFolder, DefaultAction);
    if (LV <> nil) and (DefaultAction) then
      LV.AddFolder('');
  end;
  if Sender = FListButton then begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, nbList, DefaultAction);
    if (LV <> nil) and (DefaultAction) then
      LV.ViewStyle := vsList;
  end;
  if Sender = FDetailsButton then begin
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self, nbMoveUp, DefaultAction);
    if (LV <> nil) and (DefaultAction) then
      LV.ViewStyle := vsReport;
  end;
end;

procedure TStCustomShellNavigator.DoDetailsClick(Sender: TObject);
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FShellComboBox.ListView) then begin
    if Assigned(FOnViewStyleChanging) then
      FOnViewStyleChanging(Self, vsReport, DefaultAction);
    if DefaultAction then begin
      with FShellComboBox.ListView as TStShellListView do
        ViewStyle := vsReport;
      with Sender as TMenuItem do
        Checked := True;
    end;
  end;
  FViewButton.DrawFrameType := ftNone;
end;

procedure TStCustomShellNavigator.DoLargeIconClick(Sender: TObject);
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FShellComboBox.ListView) then begin
    if Assigned(FOnViewStyleChanging) then
      FOnViewStyleChanging(Self, vsIcon, DefaultAction);
    if DefaultAction then begin
      with FShellComboBox.ListView as TStShellListView do
        ViewStyle := vsIcon;
      with Sender as TMenuItem do
        Checked := True;
    end;
  end;
  FViewButton.DrawFrameType := ftNone;
end;

procedure TStCustomShellNavigator.DoListClick(Sender: TObject);
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FShellComboBox.ListView) then begin
    if Assigned(FOnViewStyleChanging) then
      FOnViewStyleChanging(Self, vsList, DefaultAction);
    if DefaultAction then begin
      with FShellComboBox.ListView as TStShellListView do
        ViewStyle := vsList;
      with Sender as TMenuItem do
        Checked := True;
    end;
  end;
  FViewButton.DrawFrameType := ftNone;
end;

procedure TStCustomShellNavigator.DoSmallIconClick(Sender: TObject);
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FShellComboBox.ListView) then begin
    if Assigned(FOnViewStyleChanging) then
      FOnViewStyleChanging(Self, vsSmallIcon, DefaultAction);
    if DefaultAction then begin
      with FShellComboBox.ListView as TStShellListView do
        ViewStyle := vsSmallIcon;
      with Sender as TMenuItem do
        Checked := True;
    end;
  end;
  FViewButton.DrawFrameType := ftNone;
end;

procedure TStCustomShellNavigator.SetOnFolderSelected(
  Value : TStFolderSelectedEvent);
begin
  FShellComboBox.OnFolderSelected := Value;
  FOnFolderSelected := Value;
end;

procedure TStCustomShellNavigator.SetLeftOffset(Value : Integer);
begin
  if (Value <> FLeftOffset) then begin
    FLeftOffset := Value;
    FComboBoxLabel.Left := FLeftOffset;
    FShellComboBox.Left := FComboBoxLabel.Left + FComboBoxLabel.Width + 5;
    Layout;
  end;
end;

function TStCustomShellNavigator.GetVersion: string;
begin
  Result := SsVersionStr;
end;

procedure TStCustomShellNavigator.SetVersion(const Value: string);
begin
end;

{ TSsMenuButton }

constructor TSsMenuButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csDoubleClicks];
  TabStop := False;
  Width := 75;
  Height := 24;
  ParentFont := True;
  FShowGlyph := True;
  Timer := TTimer.Create(Self);
  Timer.Interval := 100;
  Timer.OnTimer := TimerEvent;
end;

destructor TSsMenuButton.Destroy;
begin
  mbGlyph.Free;
  mbGlyph:= nil;
  mbImage.Free;
  mbImage := nil;
  Timer.Free;
  inherited Destroy;
end;

procedure TSsMenuButton.Click;
var
  P : TPoint;
begin
  if PopupMenu <> nil then begin
    P := Point(0, Height);
    P := ClientToScreen(P);
    PopupMenu.PopupComponent := Self;
    PopupMenu.Popup(P.X, P.Y);
  end;
  inherited Click;
end;

procedure TSsMenuButton.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then begin
    SetFocus;
    if not mbDown then begin
      mbState := bsDown;
      DrawFrameType := ftDown;
      Repaint;
    end;
    Refresh;
  end;
end;

procedure TSsMenuButton.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if DrawFrameType = ftNone then begin
    DrawFrameType := ftUp;
    Refresh;
    Timer.Enabled := True;
  end;
end;

procedure TSsMenuButton.MouseUp(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSsMenuButton.Paint;
var
  X : Integer;
  Y : Integer;
  B : TBitmap;
  G : TImageList;
begin
  Canvas.Font := Font;
  {draw the button face}
  if csDesigning in ComponentState then
    DrawFrameType := ftUp;
  case DrawFrameType of
    ftUp :
      begin
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.MoveTo(ClientRect.Right, 0);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, ClientRect.Bottom - 1);
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(1, ClientRect.Bottom - 1);
        Canvas.LineTo(ClientRect.Right - 1, ClientRect.Bottom - 1);
        Canvas.LineTo(ClientRect.Right - 1, 0);
      end;
    ftDown :
      begin
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(ClientRect.Right, 0);
        Canvas.LineTo(0, 0);
        Canvas.LineTo(0, ClientRect.Bottom - 1);
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.MoveTo(1, ClientRect.Bottom - 1);
        Canvas.LineTo(ClientRect.Right - 1, ClientRect.Bottom - 1);
        Canvas.LineTo(ClientRect.Right - 1, 0);
      end;
  end;
  with Canvas do begin
    {draw glyph}
    if FShowGlyph and Assigned(mbGlyph) and not mbGlyph.Empty then begin
      B := TBitmap.Create;
      try
        B.Assign(mbGlyph); {copy}
        G := TImageList.CreateSize(B.Width, B.Height);
        try
          G.AddMasked(B, B.Canvas.Pixels[0,0]);
          X := (Width - G.Width) shr 1;
          Y := (Height - G.Height) shr 1;

          if mbState = bsDown then
            Inc(Y);

          G.Draw(Canvas, X, Y, 0)
        finally
          G.Free;
        end;
      finally
        B.Free;
      end;
    end;
  end;
end;

procedure TSsMenuButton.SetShowGlyph(Value : Boolean);
begin
  if Value <> FShowGlyph then begin
    FShowGlyph := Value;
    Invalidate;
  end;
end;

procedure TSsMenuButton.WMRButtonUp(var Msg : TWMRButtonUp);
begin
  {ignore so that local menu is not displayed}
end;

procedure TSsMenuButton.SetGlyph(Glyph: TBitmap);
begin
  if Assigned(mbGlyph) then begin
    mbGlyph.Free;
  end;
  mbGlyph := TBitmap.Create;
  mbGlyph.Assign(Glyph);
  Invalidate;
end;

function TSsMenuButton.GetGlyph: TBitmap;
begin
  if not Assigned(mbGlyph) then
    mbGlyph := TBitmap.Create;
  Result := mbGlyph;
end;

procedure TSsMenuButton.TimerEvent(Sender: TObject);
var
  Pt : TPoint;
begin
  if DrawFrameType = ftDown then
    Exit;
  GetCursorPos(Pt);
  Pt := ScreenToClient(Pt);
  if not PtInRect(ClientRect, Pt) then begin
    Timer.Enabled := False;
    DrawFrameType := ftNone;
    Invalidate;
  end;
end;

{ TStCustomDialogPanel }

constructor TStCustomDialogPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  Top := 0;
  Left := 0;
  Width := DefaultPanelWidth;
  Height := DefaultPanelHeight;
  TabOrder := 0;
  FilterList := TStringList.Create;
  FFilterIndex := 0;
  FAllowResize := True;
  FOpenButtonCaption := ssscNavBtnOpenCaption;
  FStyle := nsWin2k;
  FFiles := TStringList.Create;
  RecreatingWnd := False;
  FFileNameEdit := TEdit.Create(Self);
  FFileTypeComboBox := TSsComboBox.Create(Self);
  FOpenButton := TSsPanelButton.Create(Self);
  FCancelButton := TSsPanelButton.Create(Self);
  FNavigator := TStShellNavigator.Create(Self);
  {!!.02 - Added }
  FFileNameEdit.Parent := Self;
  FFileTypeComboBox.Parent := Self;
  FOpenButton.Parent := Self;
  FCancelButton.Parent := Self;
  FNavigator.Parent := Self;
  {!!.02 - End Added }

  FNavigator.Name := 'Navigator';
end;

destructor TStCustomDialogPanel.Destroy;
begin
  inherited;
  FFiles.Free;
  FilterList.Free;
end;

procedure TStCustomDialogPanel.CreateWnd;
begin
  inherited;
  if RecreatingWnd then begin
    PostMessage(Handle, MSG_DOINIT, 0, 0);
    Exit;
  end;
  FListView := TSsShellListView.Create(Self);
  Caption := '';
  FOpenButton.Caption := '';
  with FListView do begin
    Parent := Self;
    Name := 'ListView';
    Top := 35;
    Left := 8;
    Width := Self.Width - 16;
    ViewStyle := vsList;
    OpenDialogMode := True;
  end;
  FNavigator.ComboBox.Controller := FListView.Controller;
  FNavigator.Parent := Self;
  FNavigator.ListView := FListView;

  { Create the remaining controls from the bottom up so that they }
  { maintain their positions when the panel is resized. }
  with FCancelButton do begin
    Top := Self.Height - 32;
    Left := FListView.Left + FListView.Width - 83;
    Caption := ssscNavBtnCancelCaption;
    Cancel := True;
    Height := 23;
    Parent := Self;
    ButtonType := btCancel;
    Name := 'CancelButton';
  end;
  with FFileTypeComboBox do begin
    Top := FCancelButton.Top + 1;
    Left := 81;
    Width := FCancelButton.Left - Left - 20;
    Parent := Self;
    Name := 'FileTypeComboBox';
    Text := '';
    ItemIndex := FFilterIndex;
    Style := csDropdownList;
  end;
  FFileTypeLabel := TLabel.Create(Self);
  with FFileTypeLabel do begin
    Top := FFileTypeComboBox.Top + 5;
    Left := 8;
    Caption := ssscNavLblTypeCaption;
    FocusControl := FFileTypeComboBox;
    Parent := Self;
    Name := 'FileTypeLabel';
  end;

  with FFileNameEdit do begin
    Top := FFileTypeComboBox.Top - 31;
    Left := FFileTypeComboBox.Left;
    Width := FFileTypeComboBox.Width;
    Parent := Self;
    Name := 'FileNameEdit';
    Text := '';
  end;
  FFileNameLabel := TLabel.Create(Self);
  with FFileNameLabel do begin
    Top := FFileNameEdit.Top + 5;
    Left := 8;
    Caption := ssscNavLblFileNameCaption;
    FocusControl := FFileNameEdit;
    Parent := Self;
    Name := 'FileNameLabel';
  end;
  with FOpenButton do begin
    Top := FFileNameEdit.Top;
    Left := FCancelButton.Left;
    Default := True;
    Height := FFileNameEdit.Height + 2;
    Parent := Self;
    ButtonType := btOpen;
    Name := 'OpenButton';
    Caption := FOpenButtonCaption;
  end;
  FListView.Height := FFileNameEdit.Top - FListView.Top - 10;
  { Need to set tab stops after everything is created for some reason. }
  FFileNameEdit.TabOrder := 0;
  FFileTypeComboBox.TabOrder := 1;
  FOpenButton.TabOrder := 2;
  FCancelButton.TabOrder := 3;
  FNavigator.TabOrder := 4;
  FListView.TabOrder := 5;
  FFileNameEdit.Text := FFileName;
  PostMessage(Handle, MSG_DOINIT, 0, 0);
end;

procedure TStCustomDialogPanel.DestroyWnd;
begin
  if not (csLoading in ComponentState)
      or (csDesigning in ComponentState) then
    RecreatingWnd := True;
  inherited;
end;

procedure TStCustomDialogPanel.Loaded;
begin
  inherited;
  if RecreatingWnd then
    RecreatingWnd := False;
end;

procedure TStCustomDialogPanel.DoCancelButtonClick;
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FOnCancelButtonClick) then
    FOnCancelButtonClick(FOpenButton, DefaultAction);
  if DefaultAction = True then begin
    if CancelButton.Cancel = True then begin
      if Assigned(FParentForm) then
        FParentForm.ModalResult := mrCancel
      else if Parent is TForm then
        with Parent as TForm do
          ModalResult := mrCancel
      else
        Screen.ActiveForm.ModalResult := mrCancel;
    end;
  end;
end;

procedure TStCustomDialogPanel.DoComboBoxChange;
begin
  if (FilterList.Count <> 0) and (FListView <> nil) then begin
    FListView.FileFilter := FilterList.Strings[FFileTypeComboBox.ItemIndex];
    FListView.Filtered := True;
  end;
end;

procedure TStCustomDialogPanel.DoListViewClick;
var
  I : Integer;
  S : string;
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, DefaultAction);
  if DefaultAction then
    if Assigned(FListView.SelectedItem) then
      if FileExists(FListView.SelectedItem.Path) then begin
        FFiles.Clear;
        for I := 0 to Pred(FListView.Items.Count) do
          if FListView.Items[I].Selected then
            FFiles.Insert(0, FListView.ShellItems[
              Integer(FListView.Items[I].Data)].Path);
        if FFiles.Count = 1 then
          FFileNameEdit.Text := ExtractFileName(FListView.SelectedItem.Path)
        else begin
          for I := 0 to Pred(FFiles.Count) do
            S := S + '"' + ExtractFileName(FFiles[I]) + '" ';
          FFileNameEdit.Text := S;
        end;
      end;
end;

procedure TStCustomDialogPanel.DoListViewDblClick;
var
  DefaultAction : Boolean;
begin
  DefaultAction := True;
  if Assigned(FOnItemDblClick) then
    FOnItemDblClick(Self, DefaultAction);
  if DefaultAction then
    if Assigned(FListView.SelectedItem) then
      if FileExists(FListView.SelectedItem.Path) then begin
        FFileNameEdit.Text := ExtractFileName(FListView.SelectedItem.Path);
        DoOpenButtonClick;
        Exit;
      end;
  FFileNameEdit.Text := '';
end;

procedure TStCustomDialogPanel.DoOpenButtonClick;
var
  DefaultAction : Boolean;
  S : string;
begin
  DefaultAction := True;

  if FFileNameEdit.Text = '' then
    Exit;

  if FFileNameEdit.Text = '.' then begin
    FFileNameEdit.Text := '';
    Exit;
  end;
  
  { If the edit text is .. then we need to move up one level. }
  if FFileNameEdit.Text = '..' then begin
    FFileNameEdit.Text := '';
    FListView.MoveUpOneLevel;
    Exit;
  end;

  { If just a slash was entered we need to move to the root of this drive.}
  if FFileNameEdit.Text = '\' then begin
    FListView.RootFolder := ExtractFileDrive(FListView.Folder.Path);
    FFileNameEdit.Text := '';
    Exit;
  end;

  { If the edit text contains a wildcard character then we }
  { need to reset the file filter. }
  if (Pos('*', FileNameEdit.Text) <> 0) or
     (Pos('?', FileNameEdit.Text) <> 0) then begin
    FListView.FileFilter := FileNameEdit.Text;
    FListView.Filtered := True;
    Exit;
  end;

  { A directory name was entered. If it exists then switch to it. }
  if (Pos('\', FileNameEdit.Text) <> 0) then
    if DirectoryExists(FileNameEdit.Text) then begin
      FListView.RootFolder := FileNameEdit.Text;
      FileNameEdit.SelStart := 0;
      FileNameEdit.SelLength := Length(FileNameEdit.Text);
      Exit;
    end;

  { Possibly a directory under the current folder was entered. }
  S := FListView.Folder.Path;
  if S[Length(S)] <> '\' then
    S := S + '\';
  S := S + FileNameEdit.Text;
  if DirectoryExists(S) then begin
    try
      FListView.RootFolder := S;
    except
      FFileNameEdit.Text := '';
      Exit;
    end;
    FileNameEdit.SelStart := 0;
    FileNameEdit.SelLength := Length(FileNameEdit.Text);
    Exit;
  end;

  FFileName := S;
  if Assigned(FOnOpenButtonClick) then
    FOnOpenButtonClick(FOpenButton, DefaultAction);
  if DefaultAction = True then begin
    if OpenButton.Default = True then begin
      if (FDefaultExt <> '') and (Pos('.', FFileName) = 0) then
        FFileName := FFileName + '.' + FDefaultExt;
      if Assigned(FParentForm) then begin
        FParentForm.ModalResult := mrOK;
        Exit;
      end else if Parent is TForm then
        with Parent as TForm do begin
          ModalResult := mrOK;
          Exit;
        end
      else
        Screen.ActiveForm.ModalResult := mrOK;
    end;
  end;
end;

procedure TStCustomDialogPanel.Resize;
begin
  inherited;

  if not FAllowResize then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;
  if Width < DefaultPanelWidth then
    Width := DefaultPanelWidth;
  if Height < DefaultPanelHeight then
    Height := DefaultPanelHeight;

  if Assigned(FListView) then begin
    with FCancelButton do begin
      Top := Self.Height - 32;
      Left := Self.Width - 89;
    end;
    with FFileTypeComboBox do begin
      Top := FCancelButton.Top + 1;
      Width := FCancelButton.Left - Left - 20;
    end;
    with FFileTypeLabel do begin
      Top := FFileTypeComboBox.Top + 5;
    end;
    with FFileNameEdit do begin
      Top := FFileTypeComboBox.Top - 31;
      Width := FFileTypeComboBox.Width;
    end;
    with FFileNameLabel do begin
      Top := FFileNameEdit.Top + 5;
    end;
    with FOpenButton do begin
      Top := FFileNameEdit.Top;
      Left := FCancelButton.Left;
    end;
    FListView.Height := FFileNameEdit.Top - FListView.Top - 10;
    FListView.Width := FCancelButton.Left + FCancelButton.Width;
  end;
end;

procedure TStCustomDialogPanel.SetFileName(Value : string);
begin
  FFileName := Value;
  FFileNameEdit.Text := Value;
end;

procedure TStCustomDialogPanel.SetFilter(Value : string);
var
  P : Integer;
  S : string;
  FilterStr : string;
begin
  FFilter := Value;

  if not Assigned(FFileTypeComboBox) or (FFilter = '') then
    Exit;

  FilterStr := Value;

  FFileTypeComboBox.Items.BeginUpdate;  {!!.02}

  { Parse the string and add to the combo box. }
  FFileTypeComboBox.Items.Clear;
  P := Pos('|', FilterStr);
  while P <> 0 do begin
    S := Copy(FilterStr, 1, P - 1);
    FFileTypeComboBox.Items.Add(S);
    Delete(FilterStr, 1, P);
    P := Pos('|', FilterStr);
    if P = 0 then
      FilterList.Add(FilterStr)
    else begin
      S := Copy(FilterStr, 1, P - 1);
      FilterList.Add(S);
      Delete(FilterStr, 1, P);
    end;
    P := Pos('|', FilterStr);
  end;
  FFileTypeComboBox.ItemIndex := 0;

  FFileTypeComboBox.Items.EndUpdate;  {!!.02}
end;

procedure TStCustomDialogPanel.SetOpenButtonCaption(Value : string);
begin
  FOpenButtonCaption := Value;
  OpenButton.Caption := Value;
end;

procedure TStCustomDialogPanel.SetInitialDir(Value : string);
begin
  FInitialDir := Value;
  if Assigned(FListView) then
    FListView.RootFolder := Value;
end;

procedure TStCustomDialogPanel.SetStyle(Value : TSsNavigatorStyle);
begin
  FStyle := Value;
  if Assigned(FNavigator) then
    FNavigator.Style := FStyle;
end;

procedure TStCustomDialogPanel.WndProc(var Message: TMessage);
begin
  inherited;
  with Message do
    if Msg = MSG_DOINIT then begin
      if FInitialDir <> '' then
        ListView.RootFolder := FInitialDir;
      SetStyle(FStyle);
      SetFilter(FFilter);
      FFileTypeComboBox.ItemIndex := FFilterIndex;
      FFileTypeComboBox.Change;
    end;
end;

function TStCustomDialogPanel.GetVersion : string;
begin
  Result := SsVersionStr;
end;

procedure TStCustomDialogPanel.SetVersion(const Value : string);
begin
end;

{ TSsPanelButton }

procedure TSsPanelButton.Click;
begin
  inherited;
  { Find out which button was clicked. }
  case ButtonType of
    btOpen :
      with Parent as TStCustomDialogPanel do
        DoOpenButtonClick;
    btCancel :
      with Parent as TStCustomDialogPanel do
        DoCancelButtonClick;
  end;
end;

{ TSsSpeedButton }

procedure TSsSpeedButton.Click;
begin
  { For future use. }
  inherited;
end;

{ TSsComboBox }

procedure TSsComboBox.Change;
begin
  inherited;
  with Parent as TStCustomDialogPanel do
    DoComboBoxChange;
end;

{ TSsShellListView }

procedure TSsShellListView.Click;
begin
  inherited;
  with Parent as TStCustomDialogPanel do
    DoListViewClick;
end;

procedure TSsShellListView.DblClick;
begin
  with Parent as TStCustomDialogPanel do
    DoListViewDblClick;
  inherited;
end;

{ TSsShellComboBox }

procedure TSsShellComboBox.Click;
var
  LV : TStShellListView;
  NV : TStShellNavigator;
begin
  inherited;
  LV := ListView as TStShellListView;
  NV := Parent as TStShellNavigator;
  if (LV <> nil) and (NV <> nil) then
    if LV.Folder <> nil then
      NV.MoveUpButton.Enabled := not LV.Folder.IsDesktop;
end;

procedure TSsShellComboBox.DoFolderChanging(SF: TStShellFolder);
var
  NV : TStShellNavigator;
  LV : TStShellListView;
  Folder : TStShellFolder;
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  NV := Parent as TStShellNavigator;
  if (NV <> nil) and (SF <> nil) then begin
    Folder := TStShellFolder.Create(nil);
    Folder.Assign(SF);
    NV.RecentFoldersList.Add(Folder);
  end;
  LV := ListView as TStShellListView;
  if (LV <> nil) and (NV <> nil) then
    if LV.Folder <> nil then
      NV.MoveUpButton.Enabled := not LV.Folder.IsDesktop;
  if NV <> nil then
    NV.BackButton.Enabled := (NV.RecentFoldersList.Count > 0);
end;

end.
