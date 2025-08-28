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
{* ShellShock: StShlCtl.pas 1.02                         *}
{*********************************************************}
{* ShellShock: Explorer Work-Alike components            *}
{*   StShellListView: File Viewer                        *}
{*   StShellTreeView: Directory Tree Viewer              *}
{*   StShellCombobox: Folders in a Combobox              *}
{*********************************************************}

{$I SsDefine.inc}

{$I+} {I/O Checking On}
{$H+} {Huge strings}
unit StShlCtl;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ShellApi, Dialogs,
  ActiveX,  ShlObj, ComCtrls, CommCtrl, StdCtrls, Clipbrd,
  Vcl.Menus, Graphics, Registry, SsBase, SsDict, SsDque;

const
  MaxMsgQueue = 255;

type

  PStDualPidlArray = ^TStDualPidlArray;
  TStDualPidlArray = packed record
    Pidl1 : PItemIDList;
    Pidl2 : PItemIDList;
  end;

  TStNotificationItem = class
  private
    Pidl1     : PItemIDList;
    Pidl2     : PItemIDList;
    EventMask : DWORD;
  public
    constructor Create(var PidlOne, PidlTwo : PItemIDList; Mask : DWORD);
    destructor Destroy; override;
  end;

  TStShellItem = class;
  TStShellFolder = class;
  TStShellListView = class;

  { TStShellTreeView types }
  TStShellFolderAttributes = (iaCanCopy, iaCanDelete, iaCanLink,
    iaCanRename, iaIsDropTarget, iaHasPropSheet, iaIsGhosted,
    iaIsLink, iaReadOnly, iaIsShared, iaHasSubFolder, iaIsBrowseable,
    iaIsCompressed, iaIsFileSystem, iaIsFileSystemAncestor, iaIsFolder,
    iaHasNewContent, iaIsNonEumerated, iaIsRemoveable, iaValidate);

  TStShellFolderAttributeSet = set of TStShellFolderAttributes;

  TStTreeOptions = (toAllowRename, toAllowDrag, toAllowDrop,
    toColorCompressed, toExpandTopNode, toExtendedMenu,
    toShellMenu, toShowFiles, toShowHidden);

  TStTreeOptionsSet = set of TStTreeOptions;

  TStListOptions = (loAllowRename, loAllowDrag, loAllowDrop,
    loColorCompressed, loExtendedMenu, loOpenFoldersInNewWindow,
    loShellMenu, loShowHidden, loSortTypeByExt);

  TStListOptionsSet = set of TStListOptions;

  TStOptimization = (otEnumerate, otDisplay);

  TStShellEnumType = (etTree, etList, etCombo, etNone);

  TStNotifyEvents = (neAssociationChange, neAttributesChange,
    neFileChange, neFileCreate, neFileDelete, neFileRename,
    neDriveAdd, neDriveRemove, neShellDriveAdd, neDriveSpaceChange,
    neMediaInsert, neMediaRemove, neFolderCreate, neFolderDelete,
    neFolderRename, neFolderUpdate, neNetShare, neNetUnShare,
    neServerDisconnect, neImageListChange);

  TStNotifyEventsSet = set of TStNotifyEvents;

  TStEnumeratorOptions = (eoIncludeFolders, eoIncludeHidden, eoIncludeNonFolders);

  TStEnumeratorOptionsSet = set of TStEnumeratorOptions;

  TStMenuAction = (caCopy, caCut, caPaste, caProperties);

  TStSortDirection = (sdAscending, sdDescending, sdToggle);

  { Event types }
  TStItemSelectedEvent = procedure(
    Sender : TObject; Item : TStShellItem) of object;

  TStFolderSelectedEvent = procedure(
    Sender : TObject; Folder : TStShellFolder) of object;

  TStShellNotifyEvent1 = procedure(
    Sender : TObject; ShellItem : TStShellItem) of object;

  TStShellNotifyEvent2 = procedure(
    Sender : TObject; OldShellItem : TStShellItem;
    NewShellItem : TStShellItem) of object;

  TStShellNotifyEvent3 = procedure(
    Sender : TObject; OldShellItem : TStShellItem;
    NewShellItem : TStShellItem; Events : TStNotifyEventsSet) of object;

  TStShellNotifyEvent4 = procedure(Sender : TObject;
  {$IFDEF VERSION4}
     Drive : Cardinal) of object;
  {$ELSE}
     Drive : Integer) of object;
  {$ENDIF}

  TStEnumItemEvent = procedure(Sender : TObject;
    ShellItem : TStShellItem; var Accept : Boolean) of object;

  TStItemDblClickEvent = procedure(Sender : TObject;
    Item : TStShellItem; var DefaultAction : Boolean) of object;

  TStCustomShellTreeView = class;
  TStCustomShellListView = class;
  TStCustomShellComboBox = class;

  TStCustomShellNotification = class(TSsComponent)
  protected {private}
    { property variables }
    FActive             : Boolean;
    FMaxNotifications   : Integer;
    FNotifyEvents       : TStNotifyEventsSet;
    FShellVersion       : Double;
    FSpecialWatchFolder : TStSpecialRootFolder;
    FWatchFolder        : string;
    FWatchPidl          : PItemIDList;
    FWatchSubFolders    : Boolean;

    { internal variables }
    Handle      : HWnd;
    Registered  : Boolean;
    HNotify     : THandle;
    Events      : TStNotifyEventsSet;
    EventQueue  : TStDQue;

    { event variables }
    FOnAssociationChange : TNotifyEvent;
    FOnAttributeChange   : TStShellNotifyEvent2;
    FOnDriveAdd          : TStShellNotifyEvent1;
    FOnDriveRemove       : TStShellNotifyEvent1;
    FOnDriveSpaceChange  : TStShellNotifyEvent4;
    FOnFileChange        : TStShellNotifyEvent1;
    FOnFileCreate        : TStShellNotifyEvent1;
    FOnFileDelete        : TStShellNotifyEvent2;
    FOnFileRename        : TStShellNotifyEvent2;
    FOnFolderChange      : TStShellNotifyEvent1;
    FOnFolderCreate      : TStShellNotifyEvent1;
    FOnFolderDelete      : TStShellNotifyEvent1;
    FOnFolderRename      : TStShellNotifyEvent2;
    FOnImageListChange   : TNotifyEvent;
    FOnMediaInsert       : TStShellNotifyEvent1;
    FOnMediaRemove       : TStShellNotifyEvent1;
    FOnNetShare          : TStShellNotifyEvent1;
    FOnNetUnShare        : TStShellNotifyEvent1;
    FOnServerDisconnect  : TStShellNotifyEvent1;
    FOnShellChangeNotify : TStShellNotifyEvent3;
    FOnShellDriveAdd     : TStShellNotifyEvent1;

    { property methods }
    procedure SetActive(const Value : Boolean);
    procedure SetNotifyEvents(const Value : TStNotifyEventsSet);
    procedure SetSpecialWatchFolder(const Value : TStSpecialRootFolder);
    procedure SetWatchFolder(const Value : string);
    procedure SetWatchPidl(Value : PItemIDList);
    procedure SetWatchSubFolders(const Value : Boolean);

    { virtual event methods }
    procedure DoAssociationChange; virtual;
    procedure DoAttributesChange(SI1 : TStShellItem; SI2 : TStShellItem); virtual;
    procedure DoDriveAdd(SI : TStShellItem); virtual;
    procedure DoDriveRemove(SI : TStShellItem); virtual;
    procedure DoDriveSpaceChange(Drives : DWORD); virtual;
    procedure DoFileChange(SI : TStShellItem); virtual;
    procedure DoFileCreate(SI : TStShellItem); virtual;
    procedure DoFileDelete(SI1 : TStShellItem; SI2 : TStShellItem); virtual;
    procedure DoFileRename(SI1 : TStShellItem; SI2 : TStShellItem); virtual;
    procedure DoFolderChange(SI : TStShellItem); virtual;
    procedure DoFolderCreate(SI : TStShellItem); virtual;
    procedure DoFolderDelete(SI : TStShellItem); virtual;
    procedure DoFolderRename(SI1 : TStShellItem; SI2 : TStShellItem); virtual;
    procedure DoImageListChange; virtual;
    procedure DoMediaInsert(SI : TStShellItem); virtual;
    procedure DoMediaRemove(SI : TStShellItem); virtual;
    procedure DoNetShare(SI : TStShellItem); virtual;
    procedure DoNetUnShare(SI : TStShellItem); virtual;
    procedure DoServerDisconnect(SI : TStShellItem); virtual;
    procedure DoShellDriveAdd(SI : TStShellItem); virtual;
    procedure DoShellChangeNotify(SI1 : TStShellItem; SI2 : TStShellItem;
      Events : TStNotifyEventsSet); virtual;

    { internal methods }
    procedure WndProc(var Msg : TMessage);
    procedure ShellNotifyRegister;
    procedure ShellNotifyUnRegister;
    procedure ItemFromPidl(Pidl : PItemIDList; var SI : TStShellItem);

    { overridden base class methods }
    procedure Loaded; override;

    { properties }
    property Active : Boolean
      read FActive
      write SetActive;

    property MaxNotifications : Integer
      read FMaxNotifications
      write FMaxNotifications;                                         

    property NotifyEvents : TStNotifyEventsSet
      read FNotifyEvents
      write SetNotifyEvents;

    property ShellVersion : Double
      read FShellVersion;

    property SpecialWatchFolder : TStSpecialRootFolder
      read FSpecialWatchFolder
      write SetSpecialWatchFolder;

    property WatchFolder : string
      read FWatchFolder
      write SetWatchFolder;

    property WatchPidl : PItemIDList
      read FWatchPidl
      write SetWatchPidl;

    property WatchSubFolders : Boolean
      read FWatchSubFolders
      write SetWatchSubFolders;

    { events }
    property OnAssociationChange : TNotifyEvent
      read FOnAssociationChange
      write FOnAssociationChange;

    property OnAttributeChange : TStShellNotifyEvent2
      read FOnAttributeChange
      write FOnAttributeChange;

    property OnDriveAdd : TStShellNotifyEvent1
      read FOnDriveAdd
      write FOnDriveAdd;

    property OnDriveRemove : TStShellNotifyEvent1
      read FOnDriveRemove
      write FOnDriveRemove;

    property OnDriveSpaceChange : TStShellNotifyEvent4
      read FOnDriveSpaceChange
      write FOnDriveSpaceChange;

    property OnFileChange : TStShellNotifyEvent1
      read FOnFileChange
      write FOnFileChange;

    property OnFileCreate : TStShellNotifyEvent1
      read FOnFileCreate
      write FOnFileCreate;

    property OnFileDelete : TStShellNotifyEvent2
      read FOnFileDelete
      write FOnFileDelete;

    property OnFileRename : TStShellNotifyEvent2
      read FOnFileRename
      write FOnFileRename;

    property OnFolderChange : TStShellNotifyEvent1
      read FOnFolderChange
      write FOnFolderChange;

    property OnFolderCreate : TStShellNotifyEvent1
      read FOnFolderCreate
      write FOnFolderCreate;

    property OnFolderDelete : TStShellNotifyEvent1
      read FOnFolderDelete
      write FOnFolderDelete;

    property OnFolderRename : TStShellNotifyEvent2
      read FOnFolderRename
      write FOnFolderRename;

    property OnImageListChange : TNotifyEvent
      read FOnImageListChange
      write FOnImageListChange;

    property OnMediaInsert : TStShellNotifyEvent1
      read FOnMediaInsert
      write FOnMediaInsert;

    property OnMediaRemove : TStShellNotifyEvent1
      read FOnMediaRemove
      write FOnMediaRemove;

    property OnNetShare : TStShellNotifyEvent1
      read FOnNetShare
      write FOnNetShare;

    property OnNetUnShare : TStShellNotifyEvent1
      read FOnNetUnShare
      write FOnNetUnShare;

    property OnServerDisconnect : TStShellNotifyEvent1
      read FOnServerDisconnect
      write FOnServerDisconnect;

    property OnShellChangeNotify : TStShellNotifyEvent3
      read FOnShellChangeNotify
      write FOnShellChangeNotify;

    property OnShellDriveAdd : TStShellNotifyEvent1
      read FOnShellDriveAdd
      write FOnShellDriveAdd;

  public
    { methods }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStShellNotification = class(TStCustomShellNotification)
  public
    { properties }
    property MaxNotifications;
    property ShellVersion;
    property WatchPidl;
  published
    { properties }
    property Active;
    property NotifyEvents;
    property SpecialWatchFolder;
    property WatchFolder;
    property WatchSubFolders;

    { events }
    property OnAssociationChange;
    property OnAttributeChange;
    property OnDriveAdd;
    property OnDriveRemove;
    property OnDriveSpaceChange;
    property OnFileChange;
    property OnFileCreate;
    property OnFileDelete;
    property OnFileRename;
    property OnFolderChange;
    property OnFolderCreate;
    property OnFolderDelete;
    property OnFolderRename;
    property OnImageListChange;
    property OnMediaInsert;
    property OnMediaRemove;
    property OnNetShare;
    property OnNetUnShare;
    property OnServerDisconnect;
    property OnShellChangeNotify;
    property OnShellDriveAdd;
  end;

  TStShellItemList = class
  protected {private}
    FList : TList;

    { property methods }
    function GetCount : Integer;
    function GetShellItem(Index : Integer) : TStShellItem;

    { internal methods }
    procedure Clear;

  public
    property Items[Index : Integer] : TStShellItem
      read GetShellItem; default;
    property Count : Integer
      read GetCount;
    constructor Create;
    destructor  Destroy; override;
  end;

  TStShellFolderList = class
  protected
    { property variables }
    FList : TList;
    { property methods }
    function GetCount : Integer;
    function GetShellFolder(Index : Integer) : TStShellFolder;
    { internal methods }
    procedure Clear;
  public
    { properties }
    property Items[Index : Integer] : TStShellFolder
      read GetShellFolder; default;
    property Count : Integer
      read GetCount;
    { methods }
    constructor Create;
    destructor  Destroy; override;
  end;

  TStCustomShellController = class;                                    

  TStShellItem = class(TObject)
  protected {private}
    { property variables }
    FColText        : TStringList;
    FController     : TStCustomShellController;                        
    FDate           : TDateTime;
    FSize           : Int64;
    FDisplayName    : string;
    FFileAttributes : DWORD;
    FFileAttributesStr : string;                                       
    FIconIndex      : Integer;
    FIsDesktop      : Boolean;
    FIsFile         : Boolean;
    FIsFileFolder   : Boolean;
    FIsFileSystem   : Boolean;
    FLargeIcon      : TIcon;
    FOpenIcon       : TIcon;
    FOpenIconIndex  : Integer;
    FOverlayIconIndex : Integer;
    FParentFolder   : IShellFolder;
    FPath           : string;
    FPidl           : PItemIDList;
    FSelected       : Boolean;
    FSimplePidl     : PItemIDList;
    FSmallIcon      : TIcon;
    FSmallOpenIcon  : TIcon;
    FTypeName       : string;

    { internal variables }
    DOSDate     : Integer;
    HaveDetails : Boolean;
    ParentList  : TList;
    OwnController : Boolean;                                           

    { property methods }
    function GetCanCopy : Boolean;
    function GetCanLink : Boolean;
    function GetCanPaste : Boolean;
    function GetCanRename : Boolean;
    function GetDate : TDateTime;                                      
    function GetFileAttributes : DWORD;                                
    function GetFileAttributesStr : string;
    function GetHasRemovableMedia : Boolean;
    function GetHasSubFolder : Boolean;
    function GetIconIndex : Integer;                                   
    function GetIsCompressed : Boolean;
    function GetIsDropTarget : Boolean;
    function GetIsFileSystemAncestor : Boolean;
    function GetIsFolder : Boolean;
    function GetIsGhosted : Boolean;
    function GetIsHidden : Boolean;
    function GetIsLink : Boolean;
    function GetIsReadOnly : Boolean;
    function GetIsShared : Boolean;
    function GetHasPropSheet : Boolean;
    function GetOpenIcon : TIcon;
    function GetOpenIconIndex : Integer;                               
    function GetOverlayIconIndex : Integer;                            
    function GetLargeIcon : TIcon;
    function GetSize : Int64;
    function GetSmallIcon : TIcon;
    function GetSmallOpenIcon : TIcon;

    { internal methods }
    procedure GetItemDetails;                                          
  public
    { properties }
    property CanCopy : Boolean
      read GetCanCopy;

    property CanLink : Boolean
      read GetCanLink;

    property CanPaste : Boolean
      read GetCanPaste;

    property CanRename : Boolean
      read GetCanRename;

    property ColText : TStringList
      read FColText;

    property Controller : TStCustomShellController
      read FController;

    property Date : TDateTime
      read GetDate;

    property DisplayName : string
      read FDisplayName;

    property FileAttributes : DWORD
      read GetFileAttributes;

    property FileAttributesStr : string
      read GetFileAttributesStr;

    property HasPropSheet : Boolean
      read GetHasPropSheet;

    property HasRemovableMedia : Boolean
      read GetHasRemovableMedia;

    property HasSubFolder : Boolean
      read GetHasSubFolder;

    property IconIndex : Integer
      read GetIconIndex;

    property IsCompressed : Boolean
      read GetIsCompressed;

    property IsDesktop : Boolean
      read FIsDesktop;

    property IsDropTarget : Boolean
      read GetIsDropTarget;

    property IsFile : Boolean
      read FIsFile;

    property IsFileSystemAncestor : Boolean
      read GetIsFileSystemAncestor;

    property IsFileFolder : Boolean
      read FIsFileFolder;

    property IsFileSystem : Boolean
      read FIsFileSystem;

    property IsFolder : Boolean
      read GetIsFolder;

    property IsGhosted : Boolean
      read GetIsGhosted;

    property IsHidden : Boolean
      read GetIsHidden;

    property IsLink : Boolean
      read GetIsLink;

    property IsReadOnly : Boolean
      read GetIsReadOnly;

    property IsShared : Boolean
      read GetIsShared;

    property LargeIcon : TIcon
      read GetLargeIcon;

    property ParentFolder : IShellFolder
      read FParentFolder;

    property Path : string
      read FPath;

    property Pidl : PItemIDList
      read FPidl;

    property OpenIcon : TIcon
      read GetOpenIcon;

    property OpenIconIndex : Integer
      read GetOpenIconIndex;

    property OverlayIconIndex : Integer
      read GetOverlayIconIndex;

    property SimplePidl : PItemIDList
      read FSimplePidl;

    property Size : Int64
      read GetSize;

    property SmallIcon : TIcon
      read GetSmallIcon;

    property SmallOpenIcon : TIcon
      read GetSmallOpenIcon;

    property TypeName : string
      read FTypeName;

    { methods }
    procedure Assign(AValue : TStShellItem);
    procedure CopyToClipboard;
    procedure CutToClipboard;
    function  Execute : Boolean;
    procedure PasteFromClipboard;

    constructor Create(AController : TStCustomShellController);
    constructor CreateFromPidl(Pidl : PItemIDList;
      AController : TStCustomShellController);
    constructor CreateFromPath(Path : string;
      AController : TStCustomShellController);
    destructor  Destroy; override;
    function GetFolderSize(Recursive : Boolean;                        
      IncludeHidden : Boolean) : Cardinal;                             
  end;

  TStShellFolder = class(TStShellItem)
  protected {private}
    { property variables }
    FFolderCount : Integer;
    FHiddenCount : Integer;
    FItemCount   : Integer;
    FLevel       : Integer;

    { internal variables }
    ComboItem   : Boolean;

    { property methods }
    function GetFolderCount : Integer;
    function GetHiddenCount : Integer;
    function GetItemCount : Integer;

    property Level : Integer
      read FLevel
      write FLevel;

  public
    { properties }
    property FolderCount : Integer
      read GetFolderCount;

    property HiddenCount : Integer
      read GetHiddenCount;

    property ItemCount : Integer
      read GetItemCount;

    { methods }
    procedure Assign(AValue : TStShellFolder);

    constructor Create(AController : TStCustomShellController);
    constructor CreateFromPidl(Pidl : PItemIDList;
      AController : TStCustomShellController);
    constructor CreateFromPath(Path : string;
      AController : TStCustomShellController);
  end;

  TStCustomShellController = class(TSsComponent)
  protected {private}
    { property variables }
    FComboBox          : TStCustomShellComboBox;
    FDesktopFolder     : IShellFolder;
    FDragSource        : TObject;                                      
    FLargeFolderImages : TImageList;
    FListView          : TStCustomShellListView;
    FShellItems        : TStShellItemList;
    FSmallFolderImages : TImageList;
    FTreeView          : TStCustomShellTreeView;
    FTreeViewNode      : TTreeNode;

    { internal variables }
    Animate      : TAnimate;
    DosExeIndex  : Integer;
    OverlayIndex : Integer;

    { properties }
    property ComboBox : TStCustomShellComboBox
      read FComboBox
      write FComboBox;

    property DesktopFolder : IShellFolder
      read FDesktopFolder;

    property DragSource : TObject                                      
      read FDragSource                                                 
      write FDragSource;                                               

    property LargeFolderImages : TImageList
      read FLargeFolderImages;

    property ListView : TStCustomShellListView
      read FListView
      write FListView;

    property ShellItems : TStShellItemList
      read FShellItems
      write FShellItems;

    property SmallFolderImages : TImageList
      read FSmallFolderImages;

    property TreeViewNode : TTreeNode
      read FTreeViewNode
      write FTreeViewNode;

    property TreeView : TStCustomShellTreeView
      read FTreeView
      write FTreeView;

    { internal methods }
    function  BindToObject(ParentFolder : IShellFolder;
      Pidl : PItemIDList; var NewFolder : IShellFolder) : DWORD;
    procedure Enumerate(
      PidlIn   : PItemIDList; EnumType : TStShellEnumType);
    function ExecutePopup(
      const Sender : TObject; const Folder : IShellFolder;
      var Pidl : PItemIDList; const Count : Integer;
      const X, Y : Integer; const AHandle : THandle) : Integer;
    procedure StartAnimation;
    procedure GetItemInfo(const Pidl : PItemIDList;
      var IconIndex, OpenIconIndex, OverlayIndex : Integer;
      var DisplayName : string);
  public
    function  GetDisplayName(Folder : IShellFolder;
      Pidl : PItemIDList; Flags : DWORD) : string;
    procedure GetFileInfo(Pidl : PItemIDList; var Attributes : Cardinal;
      var IconIndex : Integer; var OpenIconIndex : Integer;
      var DisplayName : string);
    function  RenameItem(SI : TStShellItem; NewName : string) : Boolean;
    function  ShowPropertySheet(const SI : TStShellItem) : Boolean;

    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;

  TStShellEnumerator = class(TStCustomShellController)
  protected {private}
    { property variables }
    FFolder            : TStShellFolder;
    FOptions           : TStEnumeratorOptionsSet;
    FRootFolder        : string;
    FRootPidl          : PItemIDList;
    FShellVersion      : Double;
    FSpecialRootFolder : TStSpecialRootFolder;
    FSorted            : Boolean;
    FSortDirection     : TStSortDirection;

    { event variables }
    FOnEnumItem : TStEnumItemEvent;

    { property methods }
    procedure SetRootFolder(const Value : string);
    procedure SetSpecialRootFolder(const Value : TStSpecialRootFolder);
    procedure SetRootPidl(Value : PItemIDList);
    procedure SetSorted(const Value : Boolean);
    procedure SetSortDirection(const Value : TStSortDirection);

    { event methods }
    procedure DoEnumItem(SI : TStShellItem; var Accept : Boolean);
  published

    {properties}
    property Options : TStEnumeratorOptionsSet
      read FOptions
      write FOptions;

    property RootFolder : string
      read FRootFolder
      write SetRootFolder;

    property Sorted : Boolean
      read FSorted
      write SetSorted;

    property SortDirection : TStSortDirection
      read FSortDirection
      write SetSortDirection;

    property SpecialRootFolder : TStSpecialRootFolder
      read FSpecialRootFolder
      write SetSpecialRootFolder;

    {events}
    property OnEnumItem : TStEnumItemEvent
      read FOnEnumItem
      write FOnEnumItem;

  public
    property Folder : TStShellFolder
      read FFolder;

    property RootPidl : PItemIDList
      read FRootPidl
      write SetRootPidl;

    property ShellVersion : Double
      read FShellVersion;

    property DesktopFolder;
    property LargeFolderImages;
    property ListView;
    property ShellItems;
    property SmallFolderImages;

    procedure Execute;

    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;

  TStCustomShellTreeView = class(TCustomTreeView, IDropTarget)
  protected { private }
    { property variables }
    FCompressedColor      : TColor;
    FExpandInterval       : Integer;                                   
    FFiltered             : Boolean;
    FFolders              : TStShellFolderList;
    FListView             : TStCustomShellListView;
    FMaxNotifications     : Integer;                                   
    FOptions              : TStTreeOptionsSet;
    FRootFolder           : string;
    FSelectedFolder       : TStShellFolder;
    FShellVersion         : Double;
    FSpecialRootFolder    : TStSpecialRootFolder;
    FSpecialStartInFolder : TStSpecialRootFolder;                      
    FStartInFolder        : string;                                    

    { event variables }
    FOnFilterItem        : TStEnumItemEvent;
    FOnFolderSelected    : TStFolderSelectedEvent;
    FOnShellChangeNotify : TStShellNotifyEvent3;

    { internal variables }
    Controller         : TStCustomShellController;
    DataObject         : IDataObject;
    DoingDragDrop      : Boolean;
    DragScroll         : Boolean;
    DropTargetNode     : TTreeNode;
    InternalEvent      : Boolean;
    LastSelectedFolder : TTreeNode;
    ListViewChange     : Boolean;
    RootFolderIndex    : Integer;
    ShellMonitor       : TStShellNotification;
    RecreatingWnd      : Boolean;                                      
    HTimer             : UINT;                                         
    StartFolderSet     : Boolean;                                      
    OwnController      : Boolean;                                           

    { property methods }
    procedure SetCompressedColor(const Value : TColor);
    procedure SetListView(const Value : TStCustomShellListView);
    procedure SetMaxNotifications(const Value : Integer);              
    procedure SetOptions(const Value : TStTreeOptionsSet);
    procedure SetRootFolder(const Value : string);
    procedure SetSpecialRootFolder(const Value : TStSpecialRootFolder);
    procedure SetSpecialStartInFolder(const Value : TStSpecialRootFolder);
    procedure SetStartInFolder(const Value : string);

    { event methods }
    procedure DoFilterItem(const SI : TStShellItem; var Accept : Boolean); virtual;
    procedure DoShellChangeNotify(const SI1 : TStShellItem;
      const SI2 : TStShellItem; const Events : TStNotifyEventsSet); virtual;
    procedure ShellEvent(Sender : TObject;
      SI1, SI2 : TStShellItem; Events : TStNotifyEventsSet);

    { overridden base class methods }
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure Change(Node : TTreeNode); override;
    function  CanExpand(Node : TTreeNode) : Boolean; override;
    procedure Edit(const Item : TTVItem); override;
    function  CanEdit(Node : TTreeNode): Boolean; override;
    procedure Click; override;
    procedure WndProc(var Message : TMessage); override;
    procedure CNNotify(var Message : TWMNotify); message CN_Notify;
    {$IFDEF VERSION4}
    procedure DoCustomDrawItem(Sender : TCustomTreeView; Node : TTreeNode;
      State : TCustomDrawState; var DefaultDraw : Boolean); virtual;
    {$ENDIF}

    { IDropTarget declarations }
    function DragEnter(const dataObj : IDataObject; grfKeyState : Longint;
      pt : TPoint; var dwEffect : Longint): HResult; stdcall;
    function IDropTarget.DragOver = StDragOver;
    function StDragOver(grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint): HResult; stdcall;

    { property methods }
    procedure SetFiltered(const Value : Boolean);

    { internal methods }
    procedure ClearTree;

    procedure FillTree; virtual;
    function  FindNodeByPath(Path : string) : TTreeNode;
    function  FindNodeByPidl(Pidl : PItemIDList) : TTreeNode;
    function  FindParentNode(SI : TStShellItem) : TTreeNode;
    procedure FindAndSelectNode(APidl : PItemIDList);
    function  ListViewFolderChange(
      SI : TStShellItem; MovingUp : Boolean) : Boolean;
    procedure ListViewFolderCreate(SI : TStShellItem);
    procedure SetNodeAttributes(Node : TTreeNode; Pidl : PItemIDList;
      SetDisplayText : Boolean; Attributes : Cardinal);

  protected

    property CompressedColor : TColor
      read FCompressedColor
      write SetCompressedColor;

    property ExpandInterval : Integer                                  
      read FExpandInterval                                             
      write FExpandInterval;                                           

    property Filtered : Boolean
      read FFiltered
      write SetFiltered;

    property Folders : TStShellFolderList
      read FFolders
      write FFolders;

    property ListView : TStCustomShellListView
      read FListView
      write SetListView;

    property MaxNotifications : Integer
      read FMaxNotifications                                           
      write SetMaxNotifications;

    property Options : TStTreeOptionsSet
      read FOptions
      write SetOptions;

    property RootFolder : string
      read FRootFolder
      write SetRootFolder;

    property SelectedFolder : TStShellFolder
      read FSelectedFolder;

    property ShellVersion : Double
      read FShellVersion;

    property SpecialRootFolder : TStSpecialRootFolder
      read FSpecialRootFolder
      write SetSpecialRootFolder;

    property SpecialStartInFolder : TStSpecialRootFolder               
      read FSpecialStartInFolder                                       
      write SetSpecialStartInFolder;                                   

    property StartInFolder : string                                    
      read FStartInFolder                                              
      write SetStartInFolder;                                          

    {events}

    property OnFilterItem : TStEnumItemEvent
      read FOnFilterItem
      write FOnFilterItem;

    property OnFolderSelected : TStFolderSelectedEvent
      read FOnFolderSelected
      write FOnFolderSelected;

    property OnShellChangeNotify : TStShellNotifyEvent3
      read FOnShellChangeNotify
      write FOnShellChangeNotify;

  public
    function  AddFolder(FolderName : string) : Boolean;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    function  DeleteFolder(Recycle : Boolean; Confirm : Boolean) : Boolean;
    procedure Refresh(ANode : TTreeNode);
    function  RenameFolder(NewName : string) : Boolean;
    function  ShowPropertySheet : Boolean;
    procedure SelectFolder(Path : string);                             
    procedure SelectSpecialFolder(Folder : TStSpecialRootFolder);      
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStShellTreeView = class(TStCustomShellTreeView)
  protected {private}
    { property methods }
    function  GetVersion : string;
    procedure SetVersion(const Value : string);
  public
    {properties}
    property Folders;
    property MaxNotifications;                                         
    property SelectedFolder;
    property ShellVersion;
  published
    {properties}
    property CompressedColor;
    property ExpandInterval;                                           
    property Filtered;
    property ListView;
    property Options;
    property RootFolder;
    property SpecialRootFolder;
    property SpecialStartInFolder;
    property StartInFolder;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    {events}
    property OnFilterItem;
    property OnFolderSelected;
    property OnShellChangeNotify;

    { VCL base class properties }
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property HideSelection;
    property Indent;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$IFDEF VERSION4}
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderWidth;
    property ChangeDelay;
    property Constraints;
    property HotTrack;
    property ParentBiDiMode;
    property ToolTips;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;
    {$ENDIF}
  end;

  TStCustomShellListView = class(TCustomListView, IDropTarget)
  protected
    { property variables }
    FComboBox          : TStCustomShellComboBox;
    FCompressedColor   : TColor;
    FFileFilter        : string;
    FFiltered          : Boolean;
    FFolder            : TStShellFolder;
    FMaxNotifications  : Integer;                                      
    FOpenDialogMode    : Boolean;
    FOptions           : TStListOptionsSet;
    FOptimization      : TStOptimization;
    FRootFolder        : string;
    FSelectedItem      : TStShellItem;
    FSelectedItems     : TStShellItemList;                             
    FSpecialRootFolder : TStSpecialRootFolder;
    FShellVersion      : Double;
    FTreeView          : TStCustomShellTreeView;
    FViewStyle         : TViewStyle;

    { event variables }
    FOnItemDblClick : TStItemDblClickEvent;
    FOnItemSelected : TStItemSelectedEvent;
    FOnFilterItem   : TStEnumItemEvent;
    FOnListFilled   : TNotifyEvent;
    FOnShellChangeNotify : TStShellNotifyEvent3;

    { internal variables }
    Controller      : TStCustomShellController;
    DataObject      : IDataObject;
    DefPopup        : TPopupMenu;
    DropTargetItem  : TStShellItem;
    HaveColumns     : Boolean;
    HaveDetails     : Boolean;
    FileSystemSort  : Boolean;
    FilteredList    : TStShellItemList;
    FullList        : TStShellItemList;
    FilterChange    : Boolean;
    FolderCreated   : Boolean;
    ShellMonitor    : TStShellNotification;
    stNameCol       : string;
    stSizeCol       : string;
    stTypeCol       : string;
    stModifiedCol   : string;
    stAttributesCol : string;
    stFileFolder    : string;
    stSystemFolder  : string;
    stOriginalLoc   : string;
    stDateDeleted   : string;
    stFile          : string;
    TypeNames       : TStDictionary;
    RecreatingWnd   : Boolean;
    OwnController   : Boolean;
    {$IFDEF VERSION3}
    Popup : TPopupMenu;
    {$ENDIF}

    { property methods }
    function GetShellItems : TStShellItemList;
    procedure SetCompressedColor(const Value : TColor);
    procedure SetComboBox(const Value : TStCustomShellComboBox);
    procedure SetFileFilter(const Value : string);
    procedure SetFiltered(const Value : Boolean);
    procedure SetMaxNotifications(const Value : Integer);              
    procedure SetOptions(const Value : TStListOptionsSet);
    procedure SetRootFolder(const Value : string);
    procedure SetSpecialRootFolder(const Value : TStSpecialRootFolder);
    procedure SetTreeView(const Value : TStCustomShellTreeView);
    {$IFDEF VERSION6}
    procedure SetViewStyle(Value : TViewStyle); override;
    {$ELSE}
    procedure SetViewStyle(const Value : TViewStyle);
    {$ENDIF}

    { event methods }
    procedure DoFilterItem(const SI : TStShellItem; var Accept : Boolean); virtual;
    procedure DoItemDblClick(                                          
      const SI : TStShellItem; var DefaultAction : Boolean); virtual;  
    procedure DoItemSelected(const SI : TStShellItem); virtual;
    procedure DoListFilled; virtual;
    procedure DoShellChangeNotify(const SI1 : TStShellItem;
      const SI2 : TStShellItem; const Events : TStNotifyEventsSet); virtual;

    { overridden base class methods }
    function  CanEdit(Item : TListItem): Boolean; override;
    procedure ColClick(Column : TListColumn); override;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Edit(const Item : TLVItem); override;
    procedure Loaded; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure Change(Item : TListItem; Change : Integer); override;
    {$IFDEF VERSION4}
    function  OwnerDataFetch(Item : TListItem; Request : TItemRequest): Boolean; override;
    function  OwnerDataFind(Find : TItemFind; const FindString : string;
      const FindPosition : TPoint; FindData : Pointer; StartIndex : Integer;
      Direction : TSearchDirection; Wrap : Boolean): Integer; override;
    function  OwnerDataHint(StartIndex, EndIndex : Integer): Boolean; override;
    procedure DoCustomDrawItem(Sender : TCustomListView; Item : TListItem;
      State : TCustomDrawState; var DefaultDraw : Boolean);
    {$ENDIF}
    procedure Click; override;
    procedure DblClick; override;
    {$IFNDEF VERSION5}
    procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
    {$ENDIF}

    { IDropTarget declarations }
    function DragEnter(const dataObj : IDataObject; grfKeyState : Longint;
      pt : TPoint; var dwEffect : Longint): HResult; stdcall;
    function IDropTarget.DragOver = StDragOver;
    function StDragOver(grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint): HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function Drop(const dataObj : IDataObject; grfKeyState : Longint; pt : TPoint;
      var dwEffect : Longint): HResult; stdcall;

    { default popup menu event handlers }
    procedure DoLargeIconClick(Sender : TObject);
    procedure DoSmallIconClick(Sender : TObject);
    procedure DoListClick(Sender : TObject);
    procedure DoDetailsClick(Sender : TObject);

    { internal methods }
    procedure CNNotify(var Message : TWMNotify); message CN_Notify;
    procedure ComboBoxSelChange(const SI : TStShellItem);
    procedure FillList(SI : TStShellFolder); virtual;
    procedure FilterList;
    procedure GetColHeader(SI : TStShellItem; Columns : TListColumns); virtual;
    procedure GetItemDetails(SI : TStShellItem); virtual;
    procedure RefreshList(Enumerate : Boolean);
    procedure ShellEvent(Sender : TObject;
      SI1, SI2 : TStShellItem; Events : TStNotifyEventsSet);
    procedure SortItems; virtual;
    procedure TreeViewSelChange(const SI : TStShellItem);
    procedure WndProc(var Message : TMessage); override;

    { properties }
    property ComboBox : TStCustomShellComboBox
      read FComboBox
      write SetComboBox;

    property CompressedColor : TColor
      read FCompressedColor
      write SetCompressedColor;

    property Filtered : Boolean
      read FFiltered
      write SetFiltered;

    property FileFilter : string
      read FFileFilter
      write SetFileFilter;

    property Folder : TStShellFolder
      read FFolder
      write FFolder;

    property MaxNotifications : Integer                                
      read FMaxNotifications                                           
      write SetMaxNotifications;                                       

    property OpenDialogMode : Boolean
      read FOpenDialogMode
      write FOpenDialogMode;

    property Optimization : TStOptimization
      read FOptimization
      write FOptimization;

    property Options : TStListOptionsSet
      read FOptions
      write SetOptions;

    property RootFolder : string
      read FRootFolder
      write SetRootFolder;

    property SelectedItem : TStShellItem
      read FSelectedItem;

    property SelectedItems : TStShellItemList
      read FSelectedItems;                                             

    property ShellItems : TStShellItemList
      read GetShellItems;

    property ShellVersion : Double
      read FShellVersion;

    property SpecialRootFolder : TStSpecialRootFolder
      read FSpecialRootFolder
      write SetSpecialRootFolder;

    property TreeView : TStCustomShellTreeView
      read FTreeView
      write SetTreeView;

    property ViewStyle : TViewStyle
      read FViewStyle
      write SetViewStyle;

    {events}
    property OnItemDblClick : TStItemDblClickEvent
      read FOnItemDblClick
      write FOnItemDblClick;

    property OnItemSelected : TStItemSelectedEvent
      read FOnItemSelected
      write FOnItemSelected;

    property OnFilterItem : TStEnumItemEvent
      read FOnFilterItem
      write FOnFilterItem;

    property OnListFilled : TNotifyEvent
      read FOnListFilled
      write FOnListFilled;

    property OnShellChangeNotify : TStShellNotifyEvent3
      read FOnShellChangeNotify
      write FOnShellChangeNotify;

  public
    function  AddFolder(FolderName : string) : Boolean;
    procedure Clear; {$IFDEF VERSION6} override; {$ENDIF}
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    function  DeleteItem(Recycle : Boolean; Confirm : Boolean) : Boolean;
    procedure MoveUpOneLevel;
    procedure Refresh;
    function  RenameItem(NewName : string) : Boolean;
    procedure LoadFolder(Folder : TStShellFolder);
    function  ShowPropertySheet : Boolean;
    procedure SortColumn(Index : Integer; SortDir : TStSortDirection);
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStShellListView = class(TStCustomShellListView)
  protected {private}
    { property methods }
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  public
    { properties }
    property Columns;
    property Items;
    property Folder;
    property MaxNotifications;
    property ShellItems;
    property ShellVersion;
    property SelectedItem;
    property SelectedItems;
  published
    {properties}
    property ComboBox;
    property CompressedColor;
    property Filtered;
    property FileFilter;
    property OpenDialogMode;
    property Optimization;
    property Options;
    property RootFolder;
    property SpecialRootFolder;
    property TreeView;
    property ViewStyle;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    {events}
    property OnItemDblClick;
    property OnItemSelected;
    property OnFilterItem;
    property OnListFilled;
    property OnShellChangeNotify;

    { VCL base class properties }
    property Align;
    property AllocBy;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property GridLines;
    property HideSelection;
    property HotTrack;
    property IconOptions;
    property MultiSelect nodefault;
    property ReadOnly;
    property RowSelect;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property ShowHint;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnDblClick;
    property OnDeletion;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    {$IFDEF VERSION4}
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property FlatScrollBars;
    property FullDrag;
    property HotTrackStyles;
    property ParentBiDiMode;
    property OnEndDock;
    property OnGetImageIndex;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    {$ENDIF}
    { VCL 5 properties }
    {$IFDEF VERSION5}
    property HoverTime;
    property ShowWorkAreas;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnGetSubItemImage;
    property OnInfoTip;
    {$ENDIF}
  end;

  TStCustomShellComboBox = class(TCustomComboBox)
  protected {private}
    { property variables }
    FSelectedFolder : TStShellFolder;
    FShellVersion   : Double;
    FListView       : TStCustomShellListView;

    { internal variables }
    Controller    : TStCustomShellController;
    CurrentFolder : TStShellFolder;
    CurrentIndex  : Integer;
    Originator    : Boolean;
    ShellItems    : TStShellFolderList;
    ShellMonitor  : TStShellNotification;
    ItemHeightSet : Boolean;
    OwnController : Boolean;

    { event variables }
    FOnFolderSelected : TStFolderSelectedEvent;
    FOnFolderChanging : TStFolderSelectedEvent;

    { property methods }
    procedure SetListView(const Value : TStCustomShellListView);

    { overridden base class methods }
    procedure Loaded; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure Click; override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure WndProc(var Message : TMessage); override;
    procedure DrawItem(Index : Integer; Rect : TRect;
      State : TOwnerDrawState); override;
    procedure CreateWnd; override;

    { internal methods }
    procedure ListViewFolderChange(
      const SI : TStShellItem; MovingUp : Boolean);
    procedure RefreshView(const SI : TStShellItem);
    procedure ShellEvent(Sender : TObject;
      SI1, SI2 : TStShellItem; Events : TStNotifyEventsSet);
    procedure FillCombo;
    procedure DoFolderSelected(SF : TStShellFolder); virtual;
    procedure DoFolderChanging(SF : TStShellFolder); virtual;
    procedure SelectItem(const SI : TStShellItem);

    { properties }
    property ListView : TStCustomShellListView
      read FListView
      write SetListView;

    property SelectedFolder : TStShellFolder
      read FSelectedFolder;

    property ShellVersion : Double
      read FShellVersion;

    {events}
    property OnFolderSelected : TStFolderSelectedEvent
      read FOnFolderSelected
      write FOnFolderSelected;

    property OnFolderChanging : TStFolderSelectedEvent
      read FOnFolderChanging
      write FOnFolderChanging;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TStShellComboBox = class(TStCustomShellComboBox)
  protected {private}
    { property methods }
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  public
    { properties }
    property ShellVersion;
    {base class properties }
    property Items;
    property SelectedFolder;
    property Text;

    procedure DoClick;
  published
    { properties }
    property ListView;
    property OnFolderSelected;
    property OnFolderChanging;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    { VCL base class properties }
    {$IFDEF VERSION4}
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    {$ENDIF}
    {$IFDEF VERSION5}
    property OnContextPopup;
    {$ENDIF}
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
  end;

var
  Ascending     : Boolean;
  ClickedColumn : Integer;
  TempFolder    : IShellFolder;


implementation

uses
  Forms, JvGnugettext, UStringRessources, SsConst, StFileOp, SsRegEx, SsList,
  StShlDD, System.UITypes, System.Types;

const
  IID_IShellDetails : TGUID = (
    D1:$000214EC; D2:$0000; D3:$0000; D4:($C0,$00,$00,$00,$00,$00,$00,$46));
  SID_IShellDetails = '{000214EC-0000-0000-C000-000000000046}';
  IID_IShellIconOverlay : TGUID = (
    D1:$7D688A70; D2:$C613; D3:$11D0; D4:($99,$9B,$00,$C0,$4F,$D6,$55,$E1));
  SID_IShellIconOverlay = '{7D688A70-C613-11D0-999B-00C04FD655E1}';

  {$IFDEF VERSION3ONLY}
  SFGAO_Hidden = $00080000;
  {$ENDIF}
  SHCNF_Accept_Interrupts = $0001;
  SHCNF_Accept_Non_Interrupts = $0002;
  SHCNF_No_Proxy = $8000;
  SHGFI_Attr_Specified = $000020000;
  MSG_SHELLNOTIFY = WM_USER + 1;
  MSG_SHELLNOTIFY2 = WM_USER + 2;
  {$IFDEF VERSION3}
  MSG_RESTOREPOPUP = WM_USER + 3;
  {$ENDIF}
  {$IFNDEF VERSION4}
  NM_CustomDraw = -12;
  {$ENDIF}
  MSG_INITCOMBO = WM_USER + 4;

type

  PStShellDetails = ^TStShellDetails;
  TStShellDetails = record
    fmt    : Integer;
    cxChar : Integer;
    str    : TStrRet;
  end;

  PStDriveSpaceRec = ^TStDriveSpaceRec;
  TStDriveSpaceRec = packed record
    Dummy : Word;
    Item1 : DWORD;
    Item2 : DWORD;
  end;

  {.$EXTERNALSYM IShellDetails}
  IShellDetails = interface(IUnknown)
    [SID_IShellDetails]
   function GetDetailsOf(Pidl : PItemIDList; Col : Byte;
     var lpsd : TStShellDetails) : HResult; stdcall;
   function ColumnClick(Col : Byte) : HResult; stdcall;
  end;

  {$IFDEF VERSION3ONLY}
  IShellIconOverlay = interface(IUnknown)
    [SID_IShellIconOverlay]
    function GetOverlayIndex(pidl : PItemIDList; out pIndex : Integer): HResult; stdcall;
    function GetOverlayIconIndex(pidl : PItemIDList; out pIconIndex : Integer): HResult; stdcall;
  end;
  {$ENDIF}

procedure QueueDispose(Data : pointer);
begin
  TStNotificationItem(Data).Free;
end;

procedure DictionaryDispose(Data : Pointer);
begin
  FreeMem(Data);
end;

procedure PidlFromPath(const Path : string;
  const ParentFolder : IShellFolder; var Pidl : PItemIDList);
var
  {$IFDEF VER100}
  Count    : DWORD;
  Eaten    : DWORD;
  {$ELSE}
  Count    : Cardinal;
  Eaten    : Cardinal;
  {$ENDIF}
  //WidePath : array [0..MAX_PATH - 1] of Wide Char;
begin
  //StringToWideChar(Path, WidePath, MAX_PATH);
  ParentFolder.ParseDisplayName(
    0, nil, PChar(string(Path)), Eaten, Pidl, Count);
end;

procedure ShellMenuExecute(
  const Sender : TObject; const Folder : IShellFolder;
  var Pidl : PItemIDList; const Count : Integer;
  const AHandle : THandle; ClipboardAction : TStMenuAction);
var
  CM : IContextMenu;
  CI : TCmInvokeCommandInfo;
begin
  if Assigned(Folder) then begin
    if (Folder.GetUIObjectOf(AHandle, Count, Pidl,
      IID_IContextMenu, nil, Pointer(CM)) = NOERROR)
    then begin
      ZeroMemory(@CI, SizeOf(CI));
      CI.cbSize := SizeOf(TCmInvokeCommandInfo);
      CI.hwnd := AHandle;
      case ClipboardAction of
        caCut   : CI.lpVerb := 'cut';
        caCopy  : CI.lpVerb := 'copy';
        caPaste : CI.lpVerb := 'paste';
        caProperties : CI.lpVerb := 'properties';
      end;
      CM.InvokeCommand(CI);
      CM := nil;
    end;
  end;
end;

procedure GetParentFolder(Pidl : PItemIDList; var Folder : IShellFolder);
var
  TempPidl      : PItemIDList;
  DesktopFolder : IShellFolder;
begin
  TempPidl := ILClone(Pidl);
  ILRemoveLastID(TempPidl);
  SHGetDesktopFolder(DesktopFolder);
  DesktopFolder.BindToObject(TempPidl, nil, IID_ISHellFolder, Pointer(Folder));
  ILFree(TempPidl);
  DesktopFolder := nil;
end;

function TreeCompareFunc(lParam1, lParam2, lParamSort : {$IFDEF VERSIONXE2}NativeInt{$ELSE}Integer{$ENDIF}): Integer stdcall;
var
  Node1, Node2 : TTreeNode;
  SI1, SI2     : TStShellFolder;
  TreeView     : TStCustomShellTreeView;
begin
  TreeView := TStCustomShellTreeView(lParamSort);
  Node1 := TTreeNode(lParam1);
  Node2 := TTreeNode(lParam2);
  SI1 := TreeView.Folders[Integer(Node1.Data)];
  SI2 := TreeView.Folders[Integer(Node2.Data)];
  Result := SmallInt(TreeView.Controller.DesktopFolder.CompareIDs(
    0, SI1.Pidl, SI2.Pidl));
end;

function ItemPidlSortFuncEx(Item1, Item2 : Pointer): Integer;
begin
  Result := SmallInt(TempFolder.CompareIDs(
    0, TStShellFolder(Item1).Pidl, TStShellFolder(Item2).Pidl));
end;

function ItemPidlSortFunc(Item1, Item2 : Pointer): Integer;
begin
  Result := SmallInt(TStShellItem(Item1).ParentFolder.CompareIDs(
    0, TStShellItem(Item1).SimplePidl, TStShellItem(Item2).SimplePidl));
end;

function ItemPidlSortFuncDesc(Item1, Item2 : Pointer): Integer;
begin
  Result := SmallInt(TStShellItem(Item1).ParentFolder.CompareIDs(
    0, TStShellItem(Item2).SimplePidl, TStShellItem(Item1).SimplePidl));
end;

function ItemTextSortFunc(Item1, Item2 : Pointer): Integer;
begin
  Result := CompareText(
    TStShellItem(Item1).ColText[ClickedColumn - 1],
    TStShellItem(Item2).ColText[ClickedColumn - 1]);
end;

function ItemTypeSortFuncName(Item1, Item2 : Pointer): Integer;
begin
  if not TStShellItem(Item1).IsFileFolder and TStShellItem(Item2).IsFileFolder then
    { File system folder. }
    Result := 1
  else if TStShellItem(Item1).IsFileFolder and not TStShellItem(Item2).IsFileFolder then
    Result := -1
  else if TStShellItem(Item1).IsFileFolder and TStShellItem(Item2).IsFileFolder then
    Result :=  CompareText(TStShellItem(Item1).DisplayName, TStShellItem(Item2).DisplayName)
  else
    Result := AnsiCompareText(
      TStShellItem(Item1).ColText[ClickedColumn - 1],
      TStShellItem(Item2).ColText[ClickedColumn - 1]);
end;

function ItemTypeSortFuncExt(Item1, Item2 : Pointer): Integer;
begin
  if not TStShellItem(Item1).IsFileFolder and TStShellItem(Item2).IsFileFolder then
    { File system folder. }
    Result := 1
  else if TStShellItem(Item1).IsFileFolder and not TStShellItem(Item2).IsFileFolder then
    Result := -1
  else if TStShellItem(Item1).IsFileFolder and TStShellItem(Item2).IsFileFolder then
    Result :=  CompareText(TStShellItem(Item1).DisplayName, TStShellItem(Item2).DisplayName)
  else
    Result := AnsiCompareStr(
      ExtractFileExt(TStShellItem(Item1).Path),
      ExtractFileExt(TStShellItem(Item2).Path));
end;

function ItemSizeSortFunc(Item1, Item2 : Pointer): Integer;
begin
  if TStShellItem(Item2).IsFileFolder and not TStShellItem(Item1).IsFileFolder then
    Result := 1
  else if TStShellItem(Item1).IsFileFolder and not TStShellItem(Item2).IsFileFolder then
    Result := -1
  else if TStShellItem(Item1).Size > TStShellItem(Item2).Size then
    Result := 1
  else if TStShellItem(Item1).Size < TStShellItem(Item2).Size then
    Result := -1
  else
    Result := CompareText(
      TStShellItem(Item1).Path, TStShellItem(Item2).Path);
end;

function ItemDateSortFunc(Item1, Item2 : Pointer): Integer;
begin
  if TStShellItem(Item1).IsFileFolder and not TStShellItem(Item2).IsFileFolder then
    Result := -1
  else if TStShellItem(Item2).IsFileFolder and not TStShellItem(Item1).IsFileFolder then
    Result := 1
  else if TStShellItem(Item2).Date > TStShellItem(Item1).Date then
    Result := 1
  else if TStShellItem(Item2).Date < TStShellItem(Item1).Date then
    Result := -1
  else
    Result := 0;
end;

function ItemAttrSortFunc(Item1, Item2 : Pointer): Integer;
begin
  if TStShellItem(Item1).IsFileFolder and not TStShellItem(Item2).IsFileFolder then
    Result := -1
  else if TStShellItem(Item2).IsFileFolder and not TStShellItem(Item1).IsFileFolder then
    Result := 1
  else
    Result := CompareText(
      TStShellItem(Item1).ColText[ClickedColumn - 1],
      TStShellItem(Item2).ColText[ClickedColumn - 1]);
end;

constructor TStCustomShellController.Create(AOwner : TComponent);
var
  SHFileInfo  : TShFileInfo;
  Folder      : IShellFolder;
  Pidl        : PItemIDList;
  ParentPidl  : PItemIDList;
  IconOverlay : IShellIconOverlay;
  TempDir     : array [0..MAX_PATH - 1] of Char;
  TempFile    : array [0..MAX_PATH - 1] of Char;
  Res         : DWORD;
  Index       : Integer;
  Reg         : TRegistry;
  BootDir     : string;
  HFile       : THandle;                                               
begin
  inherited Create(AOwner);
  {$IFNDEF VERSION2009}
  if not Assigned(@ILClone) then
    LoadILFunctions;
  {$ENDIF}
  FShellItems := TStShellItemList.Create;

  { Get the desktop folder. }
  SHGetDesktopFolder(FDesktopFolder);
  { Set up the image lists used to hold the large and
  { small icon images. These images come from the system
  { image list. As such, the ShareImages property must be
  { set to True.
  {
  { A further note on how the image lists work on NT: Each time
  { we call SHGetFileInfo for a particular pidl, Windows will
  { add images to the image list. The image list may start
  { out with only four images but may easily grow to over
  { 50 images on some systems. Windows gives us the index
  { into the system list for each folder (including files). }
  FSmallFolderImages := TImageList.Create(nil);
  FLargeFolderImages := TImageList.Create(nil);
  SmallFolderImages.ShareImages := True;
  LargeFolderImages.ShareImages := True;
  SmallFolderImages.Handle :=
    SHGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  LargeFolderImages.Handle :=
    SHGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_LARGEICON);

  { There is a "feature" of IE 4.x and above when run on Windows
  { NT that causes the overlay icons (sharing and shortcut) to
  { be missing from our image list. The following code fixes that
  { feature. We have to create a temporary shortcut, get the overlay
  { icons for that shortcut, and then delete the temporary file.     }
  OverlayIndex := 0;
  DosExeIndex := -1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    GetTempPath(Length(TempDir), TempDir);
    StrCopy(TempFile, TempDir);
    StrCat(TempFile, 'temp.lnk');
    HFile := CreateFile(TempFile, GENERIC_WRITE, 0,
      nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    CloseHandle(HFile);
    PidlFromPath(TempDir, DesktopFolder, ParentPidl);
    DesktopFolder.BindToObject(ParentPidl,
      nil, IID_IShellFolder, Pointer(Folder));
    if not Assigned(Folder) then
      Folder := DesktopFolder;
    Res := Folder.QueryInterface(
      IID_IShellIconOverlay, IconOverlay);
    if Res = 0 then begin
      PidlFromPath('temp.lnk', Folder, Pidl);
      IconOverlay.GetOverlayIndex(Pidl, Index);
      OverlayIndex := Index;
      ILFree(Pidl);
      IconOverlay := nil;
      { Get the icon index for NTDETECT.COM. This is to work   }
      { around a problem with getting the correct icon for DOS }
      { executable files. This may fail on some systems if     }
      { NTDETECT is not on the root drive. }
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Setup', False);
        BootDir := Reg.ReadString('BootDir');
      finally
        Reg.Free;
      end;
      BootDir := BootDir + 'ntdetect.com';
      SHGetFileInfo(PChar(BootDir), 0, SHFileInfo,
        SizeOf(TSHFileInfo), SHGFI_SYSICONINDEX);
      DosExeIndex := SHFileInfo.iIcon;
    end;
    DeleteFile(TempFile);
    if Folder <> DesktopFolder then begin
      ILFree(ParentPidl);
      Folder := nil;
    end;
  end;
  RegisterClipboardFormat(CFSTR_FILECONTENTS);
  RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
end;

destructor TStCustomShellController.Destroy;
begin
  FDesktopFolder := nil;
  FShellItems.Free;
  FSmallFolderImages.Free;
  FLargeFolderImages.Free;
  inherited;
end;

procedure TStCustomShellController.Enumerate(
  PidlIn   : PItemIDList;
  EnumType : TStShellEnumType);
var
  ParentHandle  : HWND;
  Folder2       : IShellFolder;
  ParentFolder  : IShellFolder;
  CurrentFolder : IShellFolder;
  Enum          : IEnumIDList;
  Enum2         : IEnumIDList;
  ChildPidl     : PItemIDList;
  Pidl1         : PItemIDList;
  {$IFDEF VER100}
  Count         : DWORD;
  {$ELSE}
  Count         : Cardinal;
  {$ENDIF}
  Attr          : DWORD;
  ChildNode     : TTreeNode;
  SI            : TStShellItem;
  DriveType     : Byte;
  EnumFlags     : DWORD;
  FI            : TSHFileInfo;
  AddItem       : Boolean;
  P             : Integer;
begin
  EnumFlags := SHCONTF_FOLDERS;
  { Try to get a parent folder. }
  GetParentFolder(PidlIn, ParentFolder);
  if Assigned(ParentFolder) then
    ChildPidl := ILClone(ILFindLastID(PidlIn))
  else begin
    BindToObject(DesktopFolder, PidlIn, ParentFolder);
    if Assigned(ParentFolder) then begin
      ChildPidl := ILClone(PidlIn);
      ParentFolder := DesktopFolder;
    end else begin
      ParentFolder := DesktopFolder;
      CurrentFolder := DesktopFolder;
      ChildPidl := ILClone(PidlIn);
    end;
  end;

  case EnumType of
    etTree :
      begin
        if (toShowFiles in FTreeView.Options) then
          EnumFlags := EnumFlags or SHCONTF_NONFOLDERS;
        if (toShowHidden in FTreeView.Options) then
          EnumFlags := EnumFlags or SHCONTF_INCLUDEHIDDEN;
        Animate := nil;
      end;
    etList :
      begin
        if (loShowHidden in FListView.Options) then
          EnumFlags := EnumFlags or SHCONTF_INCLUDEHIDDEN;
        EnumFlags := EnumFlags or SHCONTF_NONFOLDERS;
      end;
  end;

  ChildNode := nil;
  ParentHandle := 0;

  if not Assigned(CurrentFolder) then
    BindToObject(ParentFolder, ChildPidl, CurrentFolder);
  CurrentFolder.EnumObjects(
    ParentHandle, EnumFlags, Enum);
  ILFree(ChildPidl);

  if EnumType = etList then begin
    SHGetFileInfo(PChar(PidlIn), 0, FI, SizeOf(FI),
        SHGFI_ATTRIBUTES or SHGFI_PIDL or SHGFI_DISPLAYNAME);
      if (FI.dwAttributes and SFGAO_FILESYSTEM) = 0 then
        StartAnimation;
    end;
  { Enumerate the items. }
  if Assigned(Enum) then begin
    while Enum.Next(1, Pidl1, Count) <> S_False do begin

      if Assigned(Animate) then begin
        Animate.Free;
        Animate := nil;
      end;

      if Count = 0 then
        Exit;

      { Get the attributes of the item. }
      Attr := SFGAO_FILESYSTEM or SFGAO_FOLDER or SFGAO_SHARE;
      if EnumType = etTree then
        Attr := Attr or SFGAO_HASSUBFOLDER;

      CurrentFolder.GetAttributesOf(1, Pidl1, Attr);

      if (EnumType = etList) then
        SI := TStShellItem.Create(Self)
      else
        SI := TStShellFolder.Create(Self);
      SI.FPidl := ILClone(ILCombine(PidlIn, Pidl1));
      SI.FSimplePidl := ILClone(Pidl1);
      SI.FPath := GetDisplayName(CurrentFolder, Pidl1, SHGDN_FORPARSING);
      SI.FIsFileSystem := (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM;
      SI.FIsFileFolder := SI.FIsFileSystem and
        ((Attr and SFGAO_FOLDER) = SFGAO_FOLDER);
      SI.FIsFile := SI.FIsFileSystem and
        ((Attr and SFGAO_FOLDER) = 0);
      SI.FParentFolder := CurrentFolder;
      {$IFDEF VERSION4}
      if (EnumType = etList) then begin
        if Assigned(ListView) and (ListView.Optimization = otDisplay) then
          GetItemInfo(SI.Pidl, SI.FIconIndex, SI.FOpenIconIndex,
            SI.FOverlayIconIndex, SI.FDisplayName)
        else
          SI.FDisplayName := GetDisplayName(CurrentFolder, Pidl1, SHGDN_NORMAL);
      end else
        SI.FDisplayName := GetDisplayName(CurrentFolder, Pidl1, SHGDN_NORMAL);
      {$ELSE}
      GetItemInfo(SI.Pidl, SI.FIconIndex,
        SI.FOpenIconIndex, SI.FOverlayIconIndex, SI.FDisplayName);
      {$ENDIF}
      { Fix up display names for items on remote machines. }
      P := Pos(ssscDisplayPreposition, SI.FDisplayName);
      if (Pos('\\', SI.FPath) = 1) and (P <> 0) then
        Delete(SI.FDisplayName, P, Length(SI.FDisplayName) - P + 1);
      if Assigned(FListView) and (EnumType = etList) then begin
        SI.ParentList := FListView.FullList.FList;
        AddItem := True;
        if Assigned(FListView.FOnFilterItem) then
          FListView.DoFilterItem(SI, AddItem);
        if AddItem then begin
          FListView.FullList.FList.Add(SI);
          if FListView.ViewStyle = vsReport then
            FListView.GetItemDetails(SI);
        end;
      end;
      if Assigned(FTreeView) and (EnumType = etTree) then begin
        AddItem := True;
        if Assigned(FTreeView.FOnFilterItem) then
          FTreeView.DoFilterItem(SI, AddItem);
        if AddItem then begin
          ChildNode := FTreeView.Items.AddChild(FTreeViewNode, SI.DisplayName);
          FTreeView.SetNodeAttributes(ChildNode, SI.Pidl, False, Attr);
          ChildNode.Text := SI.FDisplayName;
          if SI.IsShared then
            ChildNode.OverlayIndex := 0
          else if SI.IsLink then
            ChildNode.OverlayIndex := 1
          else
            ChildNode.OverlayIndex := -1;
          SI.ParentList := FTreeView.Folders.FList;
          ChildNode.Data := Pointer(FTreeView.Folders.FList.Add(SI))
        end;
      end;
      if Assigned(FComboBox) and (EnumType = etCombo) then begin
        GetItemInfo(SI.Pidl, SI.FIconIndex, SI.FOpenIconIndex,
          SI.FOverlayIconIndex, SI.FDisplayName);
        FComboBox.ShellItems.FList.Add(SI);
        AddItem := True;
      end;
      if not AddItem then begin
        SI.Free;
        SI := nil;
      end;

      if (EnumType = etTree) and Assigned(SI) then begin
        if ((Attr and SFGAO_HASSUBFOLDER) = SFGAO_HASSUBFOLDER) then begin
          { Don't enumerate network drives or removeable media drives. }
          DriveType := GetDriveType(PChar(SI.Path));
          if (DriveType = DRIVE_REMOVABLE) or
             (DriveType = 1) or
             (DriveType = DRIVE_CDROM) or
             (DriveType = DRIVE_REMOTE) then begin
            if EnumType = etTree then
              ChildNode.HasChildren := True;
            Continue;
          end;
        end;

        if ((toShowFiles in FTreeView.Options) and
             ((Attr and SFGAO_FOLDER) <> 0)) or
           ((Attr and SFGAO_HASSUBFOLDER) = SFGAO_HASSUBFOLDER) then begin
          { Create an enum object for the next level folder }
          { to see if there are any items in this folder.   }
          if (EnumType = etTree) then begin
            BindToObject(CurrentFolder, Pidl1, Folder2);
            if Assigned(Folder2) then begin
              Folder2.EnumObjects(ParentHandle,
                 SHCONTF_FOLDERS, Enum2);
              if Assigned(Enum2) then
                ChildNode.HasChildren := True;
              Folder2 := nil;
              Enum2 := nil;
            end;
          end;
        end;
      end;
      ILFree(Pidl1);
    end;
  end;
  Animate.Free;
  Animate := nil;
  Enum := nil;
  ParentFolder := nil;
end;

procedure TStCustomShellController.StartAnimation;
begin
  if FListView.HandleAllocated then begin
    Animate := TAnimate.Create(FListView);
    Animate.Parent := FListView;
    Animate.Top := (FListView.Height div 2) - (Animate.Height div 2);
    Animate.Left := (FListView.Width div 2) - (Animate.Width div 2);
    Animate.Color := FListView.Color;
    Animate.CommonAVI := aviFindFolder;
    Animate.Active := True;
  end;
end;

procedure TStCustomShellController.GetFileInfo(Pidl : PItemIDList;
  var Attributes : Cardinal; var IconIndex : Integer;
  var OpenIconIndex : Integer; var DisplayName : string);
var
  SHFileInfo : TSHFileInfo;
  Flags      : DWORD;
begin
  { Get the icon for the folder in its normal (closed) state. }
  Flags := SHGFI_PIDL or
           SHGFI_SYSICONINDEX or
           SHGFI_DISPLAYNAME or
           SHGFI_ATTRIBUTES or
           SHGFI_ATTR_SPECIFIED;

  SHFileInfo.dwAttributes := SFGAO_FOLDER or SFGAO_LINK or SFGAO_SHARE;
  SHGetFileInfo(PChar(Pidl), 0,
    SHFileInfo, SizeOf(TSHFileInfo), Flags);

  { Set the image index with the value returned by Windows. }
  { If the Desktop Update is installed and the icon index   }
  { is 2 then use the DosExeIndex instead. }
  if (DosExeIndex <> -1) and (SHFileInfo.iIcon = 2) then
    IconIndex := DosExeIndex
  else
    IconIndex := SHFileInfo.iIcon;
  DisplayName := SHFileInfo.szDisplayName;
  Attributes := ShFileInfo.dwAttributes;

  OpenIconIndex := IconIndex;
  if (Attributes and SFGAO_FOLDER) = SFGAO_FOLDER then begin
    Flags := SHGFI_PIDL or
             SHGFI_SYSICONINDEX or
             SHGFI_OPENICON;

    SHGetFileInfo(PChar(Pidl), 0,
      SHFileInfo, SizeOf(TSHFileInfo), Flags);
    OpenIconIndex := SHFileInfo.iIcon;
  end;
end;

procedure TStCustomShellController.GetItemInfo(const Pidl : PItemIDList;
  var IconIndex, OpenIconIndex, OverlayIndex : Integer; var DisplayName : string);
var
  Attr : Cardinal;
begin
  { Set the image index with the value returned by Windows. }
  GetFileInfo(Pidl, Attr,
    IconIndex, OpenIconIndex, DisplayName);

  { Handle special cases where an overlay is required. }
  if (Attr and SFGAO_SHARE) = SFGAO_SHARE then
    OverlayIndex := 0
  else if (Attr and SFGAO_LINK) = SFGAO_LINK then
    OverlayIndex := 1
  else
    OverlayIndex := -1;
end;

function TStCustomShellController.GetDisplayName(Folder : IShellFolder;
  Pidl : PItemIDList; Flags : DWORD): string;
var
  StrRet : TStrRet;
  PBuff  : PChar;
  Res    : DWORD;
begin
  { We'll ask for the display name as a PChar
  { but we can't be sure we'll get it. }
  StrRet.uType := STRRET_CSTR;
  { Get display name. }
  Res := Folder.GetDisplayNameOf(Pidl, Flags, StrRet);
   if Res <> 0 then begin
    Result := '';
    Exit;
  end;
  { We request the display name as a PChar, but Windows will }
  { decide how the display name will be returned. We need to }
  { be able to handle all cases. }
  case StrRet.uType of
    STRRET_CSTR : Result := string(StrRet.cStr);
    STRRET_WSTR : begin
                    Result := WideCharToString(StrRet.pOleStr);
                    CoTaskMemFree(StrRet.pOleStr); // SZ: pOleStr must be freed
                  end;
    STRRET_OFFSET :
      begin
        PBuff := PChar(PAnsiChar(Pidl) + StrRet.uOffset);
        Result := PBuff;
      end;
  end;
end;

function TStCustomShellController.BindToObject(ParentFolder : IShellFolder;
  Pidl : PItemIDList; var NewFolder : IShellFolder): DWORD;
begin
  Result := ParentFolder.BindToObject(
    Pidl, nil, IID_IShellFolder, Pointer(NewFolder));
end;

function TStCustomShellController.ExecutePopup(
  const Sender : TObject; const Folder : IShellFolder;
  var Pidl : PItemIDList; const Count : Integer; const X, Y : Integer;
  const AHandle : THandle) : Integer;
var
  hMenu        : THandle;
  CM           : IContextMenu;
  ParentFolder : IShellFolder;
  CI           : TCmInvokeCommandInfo;
  Cmd          : DWORD;
  Flags        : DWORD;
begin
  hMenu := 0;
  Result := -1;
  if Assigned(Folder) then begin
    hMenu := CreatePopupMenu;
    if (Folder.GetUIObjectOf(AHandle, Count, Pidl,
      IID_IContextMenu, nil, Pointer(CM)) = NOERROR)
    then begin
      Flags := CMF_EXPLORE;
      if Sender = FTreeView then begin
        if (toAllowRename in FTreeView.Options) then
          Flags := Flags or CMF_CANRENAME;
        if (toExtendedMenu in FTreeView.Options) and (GetKeyState(VK_SHIFT) < 0) then
          Flags := Flags or $00000080;
      end else if Sender = FListView then begin
        if (loAllowRename in FListView.Options) then
          Flags := Flags or CMF_CANRENAME;
        if (loExtendedMenu in FListView.Options) and (GetKeyState(VK_SHIFT) < 0) then
          Flags := Flags or $00000080;
      end;
      CM.QueryContextMenu(hMenu, 0, 1, $7FFF, Flags);
      Cmd := Cardinal(TrackPopupMenu(hMenu,
        TPM_LEFTALIGN or TPM_LEFTBUTTON or
        TPM_RIGHTBUTTON or TPM_RETURNCMD,
        X, Y, 0, AHandle, nil));
      ZeroMemory(@CI, SizeOf(CI));
      if Cmd <> 0 then begin
        Result := Cmd;
        CI.cbSize := SizeOf(TCmInvokeCommandInfo);
        CI.hwnd := AHandle;
        CI.lpVerb := MAKEINTRESOURCEA(Cmd - 1); // todo: check ~~~
        CI.lpParameters := '';
        CI.lpDirectory := '';
        CI.nShow := SW_SHOWNORMAL;
        CM.InvokeCommand(CI);
      end;
      CM := nil;
    end;
    ParentFolder := nil;
  end;
  if hMenu <> 0 then
    DestroyMenu(hMenu);
end;

function TStCustomShellController.RenameItem(
  SI : TStShellItem; NewName : string) : Boolean;
var
  NewPidl  : PItemIDList;
  S        : string;
  //NewNameW : array [0..MAX_PATH - 1] of Wide Char;
begin
  Result := False;
  if SI.CanRename then begin
    S := GetDisplayName(
      SI.ParentFolder, SI.SimplePidl, SHGDN_NORMAL);
    if S <> NewName then begin
      //StringToWideChar(NewName, NewNameW, MAX_PATH);
      Result := SI.ParentFolder.SetNameOf(Application.MainForm.Handle,
        SI.SimplePidl, PChar(NewName), SHGDN_NORMAL, NewPidl) = NOERROR;
      if Assigned(NewPidl) then begin
        { Rename was succesful so copy the new pidl. }
        SI.FSimplePidl := ILClone(NewPidl);
        SI.FPidl := ILCombine(GetParentPidl(SI.FPidl), NewPidl);
        ILFree(NewPidl);
        SI.FPath := GetDisplayName(
          SI.ParentFolder, SI.SimplePidl, SHGDN_FORPARSING);
        SI.FDisplayName := NewName;
      end;
    end;
  end;
end;

function TStCustomShellController.ShowPropertySheet(
  const SI : TStShellItem): Boolean;
begin
  Result := SI.HasPropSheet;
  if Result then
    ShellMenuExecute(nil, SI.ParentFolder,
      SI.FSimplePidl, 1, Application.Handle, caProperties);
end;

constructor TStShellEnumerator.Create(AOwner : TComponent);
var
  VI     : TSsVersionInfo;
  WinDir : array [0..MAX_PATH - 1] of Char;
begin
  inherited Create(AOwner);
  FFolder            := nil;
  FOptions           := [eoIncludeFolders, eoIncludeNonFolders];
  FRootFolder        := '';
  FSpecialRootFolder := sfNone;
  FSorted            := True;
  FSortDirection     := sdAscending;

  { Get the version of Shell32.dll. Some special folders }
  { require Version 4.71 or higher. }
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;
end;

destructor TStShellEnumerator.Destroy;
begin
  if Assigned(FFolder) then
    FFolder.Free;
  inherited;
end;

procedure TStShellEnumerator.DoEnumItem(SI : TStShellItem;
  var Accept : Boolean);
begin
  if Assigned(FOnEnumItem) then
    FOnEnumItem(Self, SI, Accept);
end;

procedure TStShellEnumerator.Execute;
var
  Accept        : Boolean;
  Enum          : IEnumIDList;
  Pidl1         : PItemIDList;
  SI            : TStShellItem;
  EnumFlags     : DWORD;
  CurrentFolder : IShellFolder;
  Attr2         : DWORD;
  {$IFDEF VER100}
  Attr          : DWORD;
  Count         : DWORD;
  {$ELSE}
  Attr          : Cardinal;
  Count         : Cardinal;
  {$ENDIF}
  P             : Integer;
begin
  if (FSpecialRootFolder = sfNone) and (FRootFolder = '')
      and not Assigned(FRootPidl) then
    Exit;
  if Assigned(FFolder) then begin
    FFolder.Free;
    FFolder := nil;
  end;
  ShellItems.Clear;
  if (FRootFolder = '') then begin
    if not Assigned(FRootPidl) then
      SHGetSpecialFolderLocation(0, ShellFolders[FSpecialRootFolder], FRootPidl);
    FFolder := TStShellFolder.CreateFromPidl(FRootPidl, Self);
  end else begin
    { Starting with a folder name rather than with a
    { special folder. }
    if FRootFolder <> '' then begin
      Attr := SFGAO_FILESYSTEM or SFGAO_FOLDER;
      PidlFromPath(FRootFolder, DesktopFolder, FRootPidl);
      if not Assigned(FRootPidl) then
        RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
      FFolder := TStShellFolder.CreateFromPidl(FRootPidl, Self);
      FFolder.FSimplePidl := ILClone(ILFindLastID(FRootPidl));
      if not Assigned(FFolder.SimplePidl) then
        FFolder.FSimplePidl := FRootPidl;
      FFolder.FPath :=
        GetDisplayName(DesktopFolder, FRootPidl, SHGDN_FORPARSING);
      FFolder.FDisplayName :=
        GetDisplayName(DesktopFolder, FRootPidl, SHGDN_NORMAL);
      { Fix up display names for items on remote machines. }
      P := Pos(ssscDisplayPreposition, FFolder.FDisplayName);
      if P <> 0 then
        Delete(FFolder.FDisplayName,
          P, Length(FFolder.FDisplayName) - P + 1);
      GetParentFolder(FRootPidl, FFolder.FParentFolder);
      Attr2 := SFGAO_FILESYSTEM or SFGAO_FOLDER;
      FFolder.ParentFolder.GetAttributesOf(1, FFolder.FSimplePidl, Attr2);
      FFolder.FIsFileSystem := (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM;
      if (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM then
        FFolder.FIsFileFolder := (Attr and SFGAO_FOLDER) = SFGAO_FOLDER
      else
        FFolder.FIsFileFolder := False;
    end;
  end;

  EnumFlags := 0;
  if (eoIncludeFolders in FOptions) then
    EnumFlags := EnumFlags or SHCONTF_FOLDERS;
  if (eoIncludeNonFolders in FOptions) then
    EnumFlags := EnumFlags or SHCONTF_NONFOLDERS;
  if (eoIncludeHidden in FOptions) then
    EnumFlags := EnumFlags or SHCONTF_INCLUDEHIDDEN;

  { Try to get a parent folder. }
  if Assigned(FFolder.FParentFolder) then
    BindToObject(FFolder.FParentFolder,
      FFolder.FSimplePidl, CurrentFolder)
  else
    BindToObject(DesktopFolder,
      FFolder.FPidl, CurrentFolder);
  if not Assigned(CurrentFolder) then
    CurrentFolder := DesktopFolder;
  CurrentFolder.EnumObjects(
    Application.Handle, EnumFlags, Enum);

  { Enumerate the items. }
  if Assigned(Enum) then
    while Enum.Next(1, Pidl1, Count) <> S_False do begin
      if Count = 0 then
        Exit;
      { Get the attributes of the item. }
      Attr2 := SFGAO_FILESYSTEM or SFGAO_FOLDER;
      CurrentFolder.GetAttributesOf(1, Pidl1, Attr2);
      SI := TStShellItem.CreateFromPidl(ILCombine(FRootPidl, Pidl1), Self);
      Accept := True;
      DoEnumItem(SI, Accept);
      SI.ParentList := ShellItems.FList;
      if Accept then
        ShellItems.FList.Add(SI)
      else
        SI.Free;
      ILFree(Pidl1);
  end;
  Enum := nil;
  if Sorted and (ShellItems.Count <> 0) then begin
    if FSortDirection = sdAscending then
      ShellItems.FList.Sort(ItemPidlSortFunc)
    else
      ShellItems.FList.Sort(ItemPidlSortFuncDesc)
  end;
end;

procedure TStShellEnumerator.SetRootFolder(const Value : string);
begin
  FRootFolder := Value;
  { Add trailing backslash if the root folder is a drive. }
  if FRootFolder <> '' then
    if (Pos(':', FRootFolder) <> 0) and (Length(FRootFolder) = 2) then
      FRootFolder := FRootFolder + '\';
  if not (csLoading in ComponentState) and (FRootFolder <> '') then begin
    { Can't have a SpecialRootFolder if RootFolder is assigned. }
    FSpecialRootFolder := sfNone;
    FRootPidl := nil;
  end;
end;

procedure TStShellEnumerator.SetRootPidl(Value : PItemIDList);
begin
  FRootPidl := Value;
  FSpecialRootFolder := sfNone;
  FRootFolder := '';
end;

procedure TStShellEnumerator.SetSpecialRootFolder(
  const Value : TStSpecialRootFolder);
begin
  if ((ShellFolders[Value] = CSIDL_INTERNET) or
     (ShellFolders[Value] = CSIDL_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_FAVORITES) or
     (ShellFolders[Value] = CSIDL_INTERNET_CACHE) or
     (ShellFolders[Value] = CSIDL_COOKIES) or
     (ShellFolders[Value] = CSIDL_HISTORY)) and
     (ShellVersion < 4.7) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  if FSpecialRootFolder <> Value then begin
    FSpecialRootFolder := Value;
    if (FSpecialRootFolder <> sfNone) and not (csLoading in ComponentState) then
      { Can't have a RootFolder if SpecialRootFolder is assigned. }
      FRootFolder := '';
      FRootPidl := nil;
  end;
end;

procedure TStShellEnumerator.SetSorted(const Value : Boolean);
begin
  FSorted := Value;
  if Sorted and (ShellItems.Count <> 0) then
    if FSortDirection = sdAscending then
      ShellItems.FList.Sort(ItemPidlSortFunc)
    else
      ShellItems.FList.Sort(ItemPidlSortFuncDesc)
end;

procedure TStShellEnumerator.SetSortDirection(const Value : TStSortDirection);
begin
  { Added sdToggle to TStSortDirection for use in the list view. }
  { That value is not valid for this property. }
  if Value = sdToggle then
    RaiseStError(ESsInvalidSortDir, ssscInvalidSortDir);
  if Sorted and (ShellItems.Count <> 0) and (FSortDirection <> Value) then begin
    if Value = sdAscending then
      ShellItems.FList.Sort(ItemPidlSortFunc)
    else
      ShellItems.FList.Sort(ItemPidlSortFuncDesc)
  end;
  FSortDirection := Value;
end;

{ TStShellItem }

constructor TStShellItem.Create(AController : TStCustomShellController);
begin
  inherited Create;
  FColText := TStringList.Create;
  { Item index of -1 means that we don't yet have all of the file info. }
  FIconIndex := -1;
  FSize := -1;
  if not Assigned(AController) then begin
    FController := TStCustomShellController.Create(nil);
    OwnController := True;
  end else
    FController := AController;
end;

constructor TStShellItem.CreateFromPidl(Pidl : PItemIDList;
  AController : TStCustomShellController);
var
  {$IFNDEF VERSION4}
  Attr       : DWORD;
  {$ELSE}
  Attr       : Cardinal;
  {$ENDIF}
  TempPidl   : PItemIDList;
  P          : Integer;
begin
  inherited Create;
  FColText := TStringList.Create;
  { Item index of -1 means that we don't yet have all of the file info. }
  FIconIndex := -1;
  FSize := -1;
  if not Assigned(Pidl) then
    Exit;
  FPidl := ILClone(Pidl);
  FSimplePidl := ILClone(ILFindLastID(Pidl));
  GetParentFolder(FPidl, FParentFolder);
  if not Assigned(FParentFolder) then
    SHGetDesktopFolder(FParentFolder);
  if not Assigned(AController) then begin
    FController := TStCustomShellController.Create(nil);
    OwnController := True;
  end else
    FController := AController;
  FPath := FController.GetDisplayName(
    ParentFolder, FSimplePidl, SHGDN_FORPARSING);
  FDisplayName := FController.GetDisplayName(
    ParentFolder, FSimplePidl, SHGDN_NORMAL);
  { Defer getting the item info until actually needed. }
  { Controller.GetItemInfo(FPidl, FIconIndex, FOpenIconIndex, }
  {   FOverlayIconIndex, FDisplayName); }
  { Fix up display names for items on remote machines. }
  P := Pos(ssscDisplayPreposition, FDisplayName);
  if (Pos('\\', FPath) = 1) and (P <> 0) then
    Delete(FDisplayName, P, Length(FDisplayName) - P + 1);
  Attr := SFGAO_FILESYSTEM or SFGAO_FOLDER;
  ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
  FIsFileSystem := (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM;
  FIsFileFolder := FIsFileSystem and
    ((Attr and SFGAO_FOLDER) = SFGAO_FOLDER);
  FIsFile := (FIsFileSystem and not FIsFileFolder);
  SHGetSpecialFolderLocation(
    Application.Handle, CSIDL_DESKTOP, TempPidl);
  if ILIsEqual(Pidl, TempPidl) then
    FIsDesktop := True;
end;

constructor TStShellItem.CreateFromPath(Path : string;
  AController : TStCustomShellController);
var
  {$IFNDEF VERSION4}
  Attr       : DWORD;
  {$ELSE}
  Attr       : Cardinal;
  {$ENDIF}
  Pidl       : PItemIDList;
  TempPidl   : PItemIDList;
  P          : Integer;
  DesktopFolder : IShellFolder;
begin
  inherited Create;
  FColText := TStringList.Create;
  { Item index of -1 means that we don't yet have all of the file info. }
  FIconIndex := -1;
  FSize := -1;
  SHGetDesktopFolder(DesktopFolder);
  PidlFromPath(Path, DesktopFolder, Pidl);
  if not Assigned(Pidl) then
    RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
  FPidl := ILClone(Pidl);
  FSimplePidl := ILClone(ILFindLastID(Pidl));
  GetParentFolder(FPidl, FParentFolder);
  if not Assigned(FParentFolder) then
    SHGetDesktopFolder(FParentFolder);
  if not Assigned(AController) then begin
    FController := TStCustomShellController.Create(nil);
    OwnController := True;
  end else
    FController := AController;
  FDisplayName := FController.GetDisplayName(
    ParentFolder, FSimplePidl, SHGDN_NORMAL);
  FPath := FController.GetDisplayName(
    ParentFolder, FSimplePidl, SHGDN_FORPARSING);
  { Defer getting the item info until actually needed. }
  { Controller.GetItemInfo(FPidl, FIconIndex, FOpenIconIndex, }
  {   FOverlayIconIndex, FDisplayName); }
  P := Pos(ssscDisplayPreposition, FDisplayName);
  if (Pos('\\', FPath) = 1) and (P <> 0) then
    Delete(FDisplayName, P, Length(FDisplayName) - P + 1);
  Attr := SFGAO_FILESYSTEM or SFGAO_FOLDER;
  ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
  FIsFileSystem := (Attr and SFGAO_FILESYSTEM) = SFGAO_FILESYSTEM;
  FIsFileFolder := FIsFileSystem and
    ((Attr and SFGAO_FOLDER) = SFGAO_FOLDER);
  FIsFile := (FIsFileSystem and not FIsFileFolder);
  SHGetSpecialFolderLocation(
    Application.Handle, CSIDL_DESKTOP, TempPidl);
  if ILIsEqual(Pidl, TempPidl) then
    FIsDesktop := True;
end;

destructor TStShellItem.Destroy;
begin
  FColText.Free;
  ILFree(FPidl);
  ILFree(FSimplePidl);
  FParentFolder := nil;
  if Assigned(FSmallIcon) then
    FSmallIcon.Free;
  if Assigned(FSmallOpenIcon) then
    FSmallOpenIcon.Free;
  if Assigned(FLargeIcon) then
    FLargeIcon.Free;
  if Assigned(FOpenIcon) then
    FOpenIcon.Free;
  if OwnController then
    FController.Free;
  inherited;
end;

procedure TStShellItem.Assign(AValue : TStShellItem);
begin
  FFileAttributes   := AValue.FFileAttributes;
  FFileAttributesStr := AValue.FFileAttributesStr;
  FDate             := AValue.FDate;
  FSize             := AValue.FSize;
  FDisplayName      := AValue.FDisplayName;
  FIconIndex        := AValue.FIconIndex;
  FIsDesktop        := AValue.FIsDesktop;
  FIsFileFolder     := AValue.FIsFileFolder;
  FIsFileSystem     := AValue.FIsFileSystem;
  FOpenIconIndex    := AValue.FOpenIconIndex;
  FOverlayIconIndex := AValue.FOverlayIconIndex;
  FPath             := AValue.FPath;
  FTypeName         := AValue.FTypeName;
  DOSDate           := AValue.DOSDate;
  HaveDetails       := AValue.HaveDetails;
  FPidl             := ILClone(AValue.FPidl);
  FSimplePidl       := ILClone(AValue.SimplePidl);
  ParentList        := AValue.ParentList;
  FColText.Assign(AValue.FColText);
  GetParentFolder(FPidl, FParentFolder);
  FController       := AValue.FController;
  { Don't bother with the icon properties as a new icon }
  { will be created when requested. }
end;

procedure TStShellItem.CopyToClipboard;
begin
  if CanCopy then
    ShellMenuExecute(nil, ParentFolder,
      FSimplePidl, 1, Application.Handle, caCopy);
end;

procedure TStShellItem.CutToClipboard;
begin
  if CanCopy then
    ShellMenuExecute(nil, ParentFolder,
      FSimplePidl, 1, Application.Handle, caCut);
end;

function TStShellItem.Execute : Boolean;
var
  SHI : TShellExecuteInfo;
begin
  ZeroMemory(@SHI, SizeOf(SHI));
  SHI.cbSize := SizeOf(SHI);
  SHI.fMask := SEE_MASK_IDLIST or SEE_MASK_INVOKEIDLIST;
  SHI.nShow := SW_SHOWNORMAL;
  SHI.lpIDList := Pidl;
  Result := ShellExecuteEx(@SHI);
end;

procedure TStShellItem.PasteFromClipboard;
begin
  if CanPaste then
    ShellMenuExecute(nil, ParentFolder,
      FSimplePidl, 1, Application.Handle, caPaste);
end;

procedure TStShellItem.GetItemDetails;
var
  S    : string;
  Res  : DWORD;
  FD   : TWin32FindData;
  FT   : TFileTime;
  SR   : TSearchRec;
  ST   : TSystemTime;
begin
  if IsFileSystem then begin
    { SHGetDataFromIDList is faster than FindFirst and works    }
    { 90% of the time. In those cases where SHGetDataFromIDList }
    { fails we'll have to use FindFirst. }
    Res := SHGetDataFromIDList(FParentFolder, FSimplePidl,
      SHGDFIL_FINDDATA, @FD, SizeOf(TWin32FindData));
    if Res = 0 then begin
      FSize := FD.nFileSizeLow or Int64(FD.nFileSizeHigh) shl 32;
      { File size }
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
        FIsFileFolder := True;
        FSize := 0;
      end else
        FSize := FD.nFileSizeLow or Int64(FD.nFileSizeHigh) shl 32;
      { Modified }
      FileTimeToLocalFileTime(FD.ftLastWriteTime, FT);
      FileTimeToDosDateTime(FT,
        LongRec(DOSDate).Hi, LongRec(DOSDate).Lo);
      if DOSDate <> 0 then
        FDate := FileDateToDateTime(DOSDate)
      else begin
        FileTimeToSystemTime(FT, ST);
        FDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) +
          EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, 0);
      end;
      {S := LongTimeFormat; //SZ - 09.09.2010 what is this good for?
      LongTimeFormat := ShortTimeFormat;
      LongTimeFormat := S; }
      { Attributes }
      S := '';
      FFileAttributes := FD.dwFileAttributes;
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN then
        S := S + 'H';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM then
        S := S + 'S';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY then
        S := S + 'R';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED) = FILE_ATTRIBUTE_COMPRESSED then
        S := S + 'C';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE then
        S := S + 'A';
      FFileAttributesStr := S;
    end else if FindFirst(Path, faAnyFile, SR) = 0 then begin
      SysUtils.FindClose(SR);
      { File size }
      FSize := SR.Size;
      { Modified }
      //DOSDate := SR.Time;
      {$IFDEF VERSIONXE}
      FDate := SR.TimeStamp;
      {$ELSE}
      FDate := FileDateToDateTime(SR.Time);
      {$ENDIF}
      { Attributes }
      S := '';
      {$IFDEF VERSION6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
      FFileAttributes := SR.FindData.dwFileAttributes;
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN then
        S := S + 'H';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM then
        S := S + 'S';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY then
        S := S + 'R';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED) = FILE_ATTRIBUTE_COMPRESSED then
        S := S + 'C';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE then
        S := S + 'A';
      {$IFDEF VERSION6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
      FFileAttributesStr := S;
    end else begin
      FSize := 0;
    end;
  end;
end;

function TStShellItem.GetFolderSize(
  Recursive : Boolean; IncludeHidden : Boolean) : Cardinal;
var
  Enum : TStShellEnumerator;
  I    : Integer;
  FS   : DWORD;
begin
  Result := 0;
  if IsFileSystem and IsFileFolder then begin
    { Enumerate the directory to find the folder size. }
    Enum := TStShellEnumerator.Create(nil);
    Enum.RootFolder := Path;
    if IncludeHidden then
      Enum.Options := Enum.Options + [eoIncludeHidden];
    Enum.Execute;
    FS := 0;
    if Recursive then begin
      for I := 0 to Pred(Enum.ShellItems.Count) do begin
        if Enum.ShellItems[I].IsFile then
          Inc(FS, Enum.ShellItems[I].Size)
        else if Enum.ShellItems[I].IsFileFolder then begin
          { The "Documents and Settings" folder on Windows 2000 }
          { contains a hidden folder called "NetHood" that can  }
          { mess up the enumeration. We'll skip any "folders"   }
          { that are UNC paths or are www addresses. }
          if Enum.ShellItems[I].IsLink then
            Inc(FS, Enum.ShellItems[I].Size)
          else
            Inc(FS, Enum.ShellItems[I].GetFolderSize(True, IncludeHidden))
        end;
      end;
    end else
      for I := 0 to Pred(Enum.ShellItems.Count) do
        if Enum.ShellItems[I].IsFile then
          Inc(FS, Enum.ShellItems[I].Size);
    Result := FS;
    Enum.Free;
  end;
end;

function TStShellItem.GetCanCopy : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_CANCOPY;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_CANCOPY) = SFGAO_CANCOPY;
  end else
    Result := False;
end;

function TStShellItem.GetCanLink : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_CANLINK;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_CANLINK) = SFGAO_CANLINK;
  end else
    Result := False;
end;

function TStShellItem.GetCanPaste : Boolean;
begin
  Result := (Clipboard.HasFormat(PidlFormat) and IsFileFolder);
end;

function TStShellItem.GetCanRename : Boolean;
var
  Attr : DWORD;
begin
  if IsDesktop then
    Result := False
  else begin
    Attr := SFGAO_CANRENAME;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_CANRENAME) = SFGAO_CANRENAME;
  end;
end;

function TStShellItem.GetDate : TDateTime;
begin
  if FSize = -1 then
    GetItemDetails;
  Result := FDate;
end;

function TStShellItem.GetFileAttributes : DWORD;
begin
  if FSize = -1 then
    GetItemDetails;
  Result := FFileAttributes;
end;

function TStShellItem.GetFileAttributesStr : string;
begin
  if FSize = -1 then
    GetItemDetails;
  Result := FFileAttributesStr;
end;

function TStShellItem.GetHasPropSheet : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_HASPROPSHEET;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_HASPROPSHEET) = SFGAO_HASPROPSHEET;
  end else
    Result := False;
end;

function TStShellItem.GetHasRemovableMedia : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_REMOVABLE;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_REMOVABLE) = SFGAO_REMOVABLE;
  end else
    Result := False;
end;

function TStShellItem.GetHasSubFolder : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_HASSUBFOLDER;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_HASSUBFOLDER) = SFGAO_HASSUBFOLDER;
  end else
    Result := False;
end;

function TStShellItem.GetIconIndex : Integer;
var
  S : string;
begin
  if FIconIndex = -1 then
    FController.GetItemInfo(FPidl, FIconIndex,
      FOpenIconIndex, FOverlayIconIndex, S);
  Result := FIconIndex;
end;

function TStShellItem.GetIsCompressed : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_COMPRESSED;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_COMPRESSED) = SFGAO_COMPRESSED;
  end else
    Result := False;
end;

function TStShellItem.GetIsDropTarget : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_DROPTARGET;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_DROPTARGET) = SFGAO_DROPTARGET;
  end else
    Result := False;
end;

function TStShellItem.GetIsFileSystemAncestor : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_FILESYSANCESTOR;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_FILESYSANCESTOR) = SFGAO_FILESYSANCESTOR;
  end else
    Result := False;
end;

function TStShellItem.GetIsGhosted : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_GHOSTED;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_GHOSTED) = SFGAO_GHOSTED;
  end else
    Result := False;
end;

function TStShellItem.GetIsLink : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_LINK;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_LINK) = SFGAO_LINK;
  end else
    Result := False;
end;

function TStShellItem.GetIsReadOnly : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_READONLY;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_READONLY) = SFGAO_READONLY;
  end else
    Result := False;
end;

function TStShellItem.GetIsShared : Boolean;
var
  Attr : DWORD;
begin
  if Assigned(ParentFolder) then begin
    Attr := SFGAO_SHARE;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_SHARE) = SFGAO_SHARE;
  end else
    Result := False;
end;

function TStShellItem.GetIsHidden : Boolean;
var
  Attr : DWORD;
begin
  if not IsDesktop then begin
    Attr := SFGAO_HIDDEN;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_HIDDEN) = SFGAO_HIDDEN;
  end else
    Result := False;
end;

function TStShellItem.GetIsFolder : Boolean;
var
  Attr : DWORD;
begin
  if Assigned(ParentFolder) then begin
    Attr := SFGAO_FOLDER;
    ParentFolder.GetAttributesOf(1, FSimplePidl, Attr);
    Result := (Attr and SFGAO_FOLDER) = SFGAO_FOLDER;
  end else
    Result := False;
end;

function TStShellItem.GetOpenIconIndex : Integer;
var
  S : string;
begin
  if FIconIndex = -1 then
    FController.GetItemInfo(FPidl, FIconIndex,
      FOpenIconIndex, FOverlayIconIndex, S);
  Result := FOpenIconIndex;
end;

function TStShellItem.GetOverlayIconIndex : Integer;
var
  S : string;
begin
  if FIconIndex = -1 then
    FController.GetItemInfo(FPidl, FIconIndex,
      FOpenIconIndex, FOverlayIconIndex, S);
  Result := FOverlayIconIndex;
end;

function TStShellItem.GetSize : Int64;
begin
  if FSize = -1 then
    GetItemDetails;
  Result := FSize;
end;

function TStShellItem.GetSmallIcon : TIcon;
var
  SHFileInfo : TShFileInfo;
begin
  if not Assigned(FSmallIcon) then begin
    FSmallIcon := TIcon.Create;
    SHGetFileInfo(PChar(Pidl), 0,
      SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_ICON or SHGFI_SMALLICON or SHGFI_PIDL);
    FSmallIcon.Handle := SHFileInfo.hIcon;
  end;
  Result := FSmallIcon;
end;

function TStShellItem.GetLargeIcon : TIcon;
var
  SHFileInfo : TShFileInfo;
begin
  if not Assigned(FLargeIcon) then begin
    FLargeIcon := TIcon.Create;
    SHGetFileInfo(PChar(Pidl), 0,
      SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_ICON or SHGFI_PIDL);
    FLargeIcon.Handle := SHFileInfo.hIcon;
  end;
  Result := FLargeIcon;
end;

function TStShellItem.GetOpenIcon : TIcon;
var
  SHFileInfo : TShFileInfo;
begin
  if not Assigned(FOpenIcon) then begin
    FOpenIcon := TIcon.Create;
    SHGetFileInfo(PChar(Pidl), 0,
      SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_ICON or SHGFI_PIDL or SHGFI_OPENICON);
    FOpenIcon.Handle := SHFileInfo.hIcon;
  end;
  Result := FOpenIcon;
end;

function TStShellItem.GetSmallOpenIcon : TIcon;
var
  SHFileInfo : TShFileInfo;
begin
  if not Assigned(FSmallOpenIcon) then begin
    FSmallOpenIcon := TIcon.Create;
    SHGetFileInfo(PChar(Pidl), 0,
      SHFileInfo, SizeOf(TSHFileInfo),
      SHGFI_ICON or SHGFI_SMALLICON or SHGFI_PIDL or SHGFI_OPENICON);
    FSmallOpenIcon.Handle := SHFileInfo.hIcon;
  end;
  Result := FSmallOpenIcon;
end;

{ TStShellItemList }

procedure TStShellItemList.Clear;
var
  I : Integer;
begin
  if Assigned(FList) then begin
    for I := 0 to Pred(FList.Count) do
      TStShellItem(FList[I]).Free;
    if FList.Count > 0 then
      FList.Clear;
  end;
end;

constructor TStShellItemList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TStShellItemList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TStShellItemList.GetCount : Integer;
begin
  Result := FList.Count;
end;

function TStShellItemList.GetShellItem(Index : Integer): TStShellItem;
begin
  if Index < FList.Count then
    Result := TStShellItem(FList[Index])
  else
    Result := nil;
end;

{ TStShellFolder }

constructor TStShellFolder.Create(AController : TStCustomShellController);
begin
  inherited Create(AController);
  FItemCount   := -1;
  FHiddenCount := -1;
  FFolderCount := -1;
  FLevel       := -1;
end;

constructor TStShellFolder.CreateFromPidl(Pidl : PItemIDList;
  AController : TStCustomShellController);
begin
  inherited CreateFromPidl(Pidl, AController);
  FItemCount   := -1;
  FHiddenCount := -1;
  FFolderCount := -1;
  FLevel       := -1;
end;

constructor TStShellFolder.CreateFromPath(Path : string;
  AController : TStCustomShellController);
begin
  inherited CreateFromPath(Path, AController);
  FItemCount   := -1;
  FHiddenCount := -1;
  FFolderCount := -1;
  FLevel       := -1;
end;

procedure TStShellFolder.Assign(AValue : TStShellFolder);
begin
  FDate             := AValue.FDate;
  FSize             := AValue.FSize;
  FDisplayName      := AValue.FDisplayName;
  FIconIndex        := AValue.FIconIndex;
  FIsDesktop        := AValue.FIsDesktop;
  FIsFileFolder     := AValue.FIsFileFolder;
  FIsFileSystem     := AValue.FIsFileSystem;
  FOpenIconIndex    := AValue.FOpenIconIndex;
  FOverlayIconIndex := AValue.FOverlayIconIndex;
  FPath             := AValue.FPath;
  FTypeName         := AValue.FTypeName;
  DOSDate           := AValue.DOSDate;
  HaveDetails       := AValue.HaveDetails;
  FPidl             := ILClone(AValue.FPidl);
  ParentList        := AValue.ParentList;
  FColText.Assign(AValue.FColText);
  if Assigned(AValue.SimplePidl) then
    FSimplePidl   := ILClone(AValue.SimplePidl);
  GetParentFolder(FPidl, FParentFolder);
  { Don't bother with the icon properties as a new icon }
  { will be created when requested. }
  FItemCount   := AValue.FItemCount;
  FHiddenCount := AValue.FHiddenCount;
  FFolderCount := AValue.FFolderCount;
end;

function TStShellFolder.GetFolderCount : Integer;
var
  I : Integer;
begin
  if FFolderCount = -1 then begin
    FFolderCount := 0;
    { Run through the list and get the count of folders. }
    if Assigned(ParentList) then
      for I := 0 to Pred(ParentList.Count) do
        if TStShellItem(ParentList[I]).IsFolder then
          Inc(FFolderCount);
  end;
  Result := FFolderCount;
end;

function TStShellFolder.GetHiddenCount : Integer;
var
  I : Integer;
begin
  if FHiddenCount = -1 then begin
    FHiddenCount := 0;
    { Run through the list and get the count of hidden items. }
    if Assigned(ParentList) then
      for I := 0 to Pred(ParentList.Count) do
        if TStShellItem(ParentList[I]).IsHidden then
          Inc(FHiddenCount);
  end;
  Result := FHiddenCount;
end;

function TStShellFolder.GetItemCount : Integer;
var
  I : Integer;
begin
 if FItemCount = -1 then begin
    FItemCount := 0;
    { Run through the list and get the count of non-folder items. }
    if Assigned(ParentList) then
      for I := 0 to Pred(ParentList.Count) do
        if not TStShellItem(ParentList[I]).IsFolder then
          Inc(FItemCount);
  end;
  Result := FItemCount;
  FItemCount := -1;
end;

{ TStShellFolderList }

procedure TStShellFolderList.Clear;
var
  I : Integer;
begin
  for I := 0 to Pred(FList.Count) do
    TStShellFolder(FList[I]).Free;
  FList.Clear;
end;

constructor TStShellFolderList.Create;
begin
  inherited;
  FList := TList.Create;
end;

destructor TStShellFolderList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TStShellFolderList.GetCount : Integer;
begin
  Result := FList.Count;
end;

function TStShellFolderList.GetShellFolder(Index : Integer): TStShellFolder;
begin
  Result := TStShellFolder(FList[Index]);
end;

{ TCustomShellTreeView }

constructor TStCustomShellTreeView.Create(AOwner : TComponent);
var
  VI     : TSsVersionInfo;
  WinDir : array [0..MAX_PATH - 1] of Char;
begin
  inherited Create(AOwner);
  {$IFNDEF VERSION2009}
  if not Assigned(@ILClone) then
    LoadILFunctions;
  {$ENDIF}
  FOptions           := [toExpandTopNode, toAllowRename,
                             toAllowDrag, toAllowDrop, toShellMenu];
  FRootFolder        := '';
  FSpecialRootFolder := sfDesktop;
  FCompressedColor   := clBlue;
  FExpandInterval    := 2000;
  FFolders           := TStShellFolderList.Create;
  FMaxNotifications  := 5;

  SortType           := stNone;
  Width              := 180;
  Height             := 270;
  RightClickSelect   := True;
  ShowRoot           := False;
  RecreatingWnd      := False;

  FSpecialStartInFolder := sfNone;
  FStartInFolder        := '';

  { Get the version of Shell32.dll. Some special folders }
  { require Version 4.71 or higher. }
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;
  {$IFDEF VERSION4}
  OnCustomDrawItem := DoCustomDrawItem;
  {$ENDIF}
  if not (csDesigning in ComponentState) then begin
    ShellMonitor := TStShellNotification.Create(Self);
    ShellMonitor.WatchSubFolders := True;
    ShellMonitor.Events := [
      neFileChange, neFileCreate, neFileDelete, neFileRename,
      neDriveAdd, neDriveRemove, neShellDriveAdd, neDriveSpaceChange,
      neMediaInsert, neMediaRemove, neFolderCreate, neFolderDelete,
      neFolderRename, neFolderUpdate, neNetShare, neNetUnShare,
      neServerDisconnect, neImageListChange];
    ShellMonitor.WatchPidl := nil;
    ShellMonitor.OnShellChangeNotify := ShellEvent;
    { For the tree view and list view we don't need to know     }
    { about every single file that was changed, deleted, and so }
    { on. Instead, we'll just get a few notifications as that   }
    { will be sufficient to properly update the components.     }
    ShellMonitor.MaxNotifications := FMaxNotifications;
    ShellMonitor.Active := True;
  end;
  DoingDragDrop := False;
end;

destructor TStCustomShellTreeView.Destroy;
begin
  if Assigned(Controller) and OwnController then begin
    Controller.Free;
    Controller := nil;
  end;
  Folders.Free;
  inherited;
end;

procedure TStCustomShellTreeView.CreateWnd;
var
  Node  : TTreeNode;
  SI    : TStShellItem;
{$IFNDEF VER100}
  DD    : Boolean;
  Style : LongInt;
{$ENDIF}
begin
  inherited CreateWnd;
  { If the window has been recreated then the HasChildren property }
  { has been cleared and we need to reset it. }
  if RecreatingWnd then begin
    Node := Items[0];
    while Assigned(Node) do begin
      SI := Folders[Integer(Node.Data)];
      if Assigned(SI) then
        if SI.HasSubFolder then
            Node.HasChildren := True;
      Node := Node.GetNext;
    end;
    Exit;
  end;
  { For cases when the component is being created dynamically. }
  if not (csLoading in ComponentState) then begin
    if not Assigned(Controller) then
      if Assigned(FListView) and Assigned(FListView.Controller) then
        Controller := FListView.Controller
      else begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
    Controller.TreeView := Self;
    Images := Controller.SmallFolderImages;
    FillTree;
  end;
  {$IFNDEF VER100}
  if not (csDesigning in ComponentState) then begin
    if (toAllowDrop in FOptions) then
      RegisterDragDrop(Handle, Self);
    DD := (toAllowDrag in FOptions);
    Style := GetWindowLong(Handle, GWL_STYLE);
    if DD then
      Style := Style and not TVS_DISABLEDRAGDROP
    else
      Style := Style or TVS_DISABLEDRAGDROP;
    SetWindowLong(Handle, GWL_STYLE, Style);
  end;
  {$ENDIF}
end;

procedure TStCustomShellTreeView.DestroyWnd;
begin
  if not (csLoading in ComponentState)
      or (csDesigning in ComponentState) then
    RecreatingWnd := True;
  RevokeDragDrop(Handle);
  inherited;
end;

procedure TStCustomShellTreeView.Loaded;
begin
  inherited Loaded;
  if RecreatingWnd then begin
    RecreatingWnd := False;
    Exit;
  end;
  { See if there is a controller on the form. }
  if not (csLoading in ComponentState) then begin
    { Create a controller if necessary. If the list view has already }
    { been created then hook up to its controller. }
    if not Assigned(Controller) then
      if Assigned(FListView) and Assigned(FListView.Controller) then
        Controller := FListView.Controller
      else begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
    Controller.TreeView := Self;
    if not Assigned(Images) then
      Images := Controller.SmallFolderImages;
    FillTree;
  end;
end;

procedure TStCustomShellTreeView.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FListView) and (Operation = opRemove)
      and (csDesigning in ComponentState) then
    FListView := nil;
end;

function TStCustomShellTreeView.CanEdit(Node : TTreeNode): Boolean;
var
  SI : TStShellItem;
begin
  inherited CanEdit(Node);
  SI := Folders[Integer(Selected.Data)];
  Result := SI.CanRename and (toAllowRename in FOptions);
end;

procedure TStCustomShellTreeView.Edit(const Item : TTVItem);
var
  SI       : TStShellFolder;
  NewPidl  : PItemIDList;
 // NewName  : array [0..MAX_PATH - 1] of Wide Char;
  Node     : TTreeNode;
begin
  inherited Edit(Item);
  SI := Folders[Integer(Selected.Data)];
  { Text didn't change so exit. }
  if (Item.pszText = SI.DisplayName) or (Item.pszText = '') then
    Exit;
  if SI.CanRename then begin
    Screen.Cursor := crHourglass;
    try
      if SI.DisplayName <> Selected.Text then begin
        //StringToWideChar(Selected.Text, NewName, MAX_PATH);
        SI.ParentFolder.SetNameOf(
          Handle, SI.SimplePidl, PChar(Selected.Text), SHGDN_NORMAL, NewPidl);
        Node := Selected;
        Refresh(Selected.Parent);
        Selected := Node;
        ILFree(NewPidl);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

function TStCustomShellTreeView.CanExpand(Node : TTreeNode): Boolean;
var
  SI           : TStShellItem;
  TopNode      : TTreeNode;
  Index : Integer;
begin
  Result := inherited CanExpand(Node);
  if (csDestroying in ComponentState) then
    Exit;
  SI := nil;
  if Node.Count = 0 then begin
    Screen.Cursor := crHourGlass;
    Index := Integer(Node.Data);
    SI := Folders[Index];
    Controller.FTreeViewNode := Node;
    Controller.Enumerate(SI.Pidl, etTree);
    Screen.Cursor := crDefault;
  end;
  if Node.Count = 0 then
    Node.HasChildren := False;
  { If it's not a file system folder then we'll need to resort. }
  if Assigned(SI) then begin
    Perform(WM_SETREDRAW, 0, 0);
    TopNode := TopItem;
    CustomSort(TreeCompareFunc, {$IFDEF VERSIONXE2}NativeInt{$ELSE}Integer{$ENDIF}(Self));
    TopItem := TopNode;
    Perform(WM_SETREDRAW, 1, 0);
  end;
end;

procedure TStCustomShellTreeView.Change(Node : TTreeNode);
var
  SI : TStShellFolder;
begin
  if not StartFolderSet then
    Exit;
  inherited Change(Node);
  LastSelectedFolder := Selected;
  SI := Folders[Integer(Node.Data)];
  if not Assigned(SI) then
    Exit;
  FSelectedFolder := SI;
  if not (csLoading in ComponentState) and
     not (csDesigning in ComponentState) then begin
    if Assigned(FOnFolderSelected) then
      FOnFolderSelected(Self, Folders[Integer(Node.Data)]);
    if not ListViewChange and Assigned(FListView) then begin
       if SI.IsFolder or SI.IsDesktop then
         FListView.TreeViewSelChange(SI);
    end else
      ListViewChange := False;
  end;
end;

procedure TStCustomShellTreeView.SetNodeAttributes(
  Node : TTreeNode; Pidl : PItemIDList;
  SetDisplayText : Boolean; Attributes : Cardinal);
var
  IconIndex     : Integer;
  OpenIconIndex : Integer;
  DisplayName   : string;
begin
  { Set the image index with the value returned by Windows. }
  Controller.GetFileInfo(Pidl, Attributes,
    IconIndex, OpenIconIndex, DisplayName);
  Node.ImageIndex := IconIndex;
  { Set the display text. }
  if SetDisplayText then
    Node.Text := DisplayName;

  Node.SelectedIndex := OpenIconIndex;
end;

procedure TStCustomShellTreeView.SetRootFolder(const Value : string);
begin
  FRootFolder := Value;
  { Add trailing backslash if the root folder is a drive. }
  if FRootFolder <> '' then begin
    if (Pos(':', FRootFolder) <> 0) and (Length(FRootFolder) = 2) then
      FRootFolder := FRootFolder + '\';
  end else
    ClearTree;

  if not (csLoading in ComponentState) and (FRootFolder <> '') then begin
    { Can't have a SpecialRootFolder if RootFolder is assigned. }
    SpecialRootFolder := sfNone;
    if (FSpecialStartInFolder <> sfNone) or (FStartInFolder <> '') then
      StartFolderSet := False;
    ClearTree;
    FillTree;
  end;
end;

procedure TStCustomShellTreeView.SetStartInFolder(const Value : string);
begin
  FStartInFolder := Value;
  { Add trailing backslash if the root folder is a drive. }
  if FStartInFolder <> '' then begin
    if (Pos(':', FStartInFolder) <> 0) and (Length(FStartInFolder) = 2) then
      FStartInFolder := FStartInFolder + '\';
  end else
    ClearTree;

  if not (csLoading in ComponentState) and (FStartInFolder <> '') then
    { Can't have a SpecialStartInFolder if StartInFolder is assigned. }
    FSpecialStartInFolder := sfNone;
end;

function TStCustomShellTreeView.ShowPropertySheet : Boolean;
begin
  Result := Controller.ShowPropertySheet(SelectedFolder);
end;

procedure TStCustomShellTreeView.CNNotify(var Message : TWMNotify);
var
  NMHdr      : TNMHdr;
  NMTree     : TNMTreeView;
  SI         : TStShellItem;
  DropSource : IDropSource;
  PidlArray  : PStPidlArray;
  {$IFDEF VERSION4}
  Attr       : Cardinal;
  {$ELSE}
  Attr       : DWORD;
  {$ENDIF}
  DropEffect : Integer;
  Node       : TTreeNode;
  Pidl       : PItemIDList;
  HDragImgList : HIMAGELIST;
begin
  NMHdr := Message.NMHdr^;
  case NMHdr.code of
    TVN_BEGINDRAG :
      if (toAllowDrag in FOptions) then begin
        Controller.DragSource := Self;
        NMTree := PNMTreeView(Pointer(Message.NMHdr))^;
        Node := GetNodeAt(NMTree.ptDrag.X, NMTree.ptDrag.Y);
        SI := Folders[Integer(Node.Data)];
        DropEffect := 0;
        SI.ParentFolder.GetAttributesOf(1, SI.FSimplePidl, Attr);
        DropEffect := Attr;
        Pidl := ILClone(SI.FSimplePidl);
        GetMem(PidlArray, SizeOf(PItemIDList));
        PidlArray[0] := Pidl;
        DataObject := TStDataObject.Create(PidlArray^, SI.FPidl, 1);
        DropSource := TStDropSource.Create;
        HDragImgList := TreeView_CreateDragImage(Handle, NMTree.itemNew.hItem);
        ImageList_BeginDrag(HDragImgList, 0, 0, 0);
        DoDragDrop(DataObject, DropSource, DropEffect, DropEffect);
        ImageList_EndDrag();
        ImageList_Destroy(HDragImgList);
        FreeMem(PidlArray);
        DataObject := nil;
      end;
  end;
  if (csDesigning in ComponentState) and (NMHdr.code = NM_CUSTOMDRAW) then
    Exit
  else
    inherited;
end;

procedure TStCustomShellTreeView.WndProc(var Message : TMessage);
var
  Res  : Integer;
  SI   : TStShellItem;
  Pidl : PItemIDList;
  Node : TTreeNode;
begin
  with Message do begin
    if (Msg = WM_CONTEXTMENU) and (toShellMenu in FOptions) then begin
      SI := Folders[Integer(Selected.Data)];
      Res := Controller.ExecutePopup(Self, SI.ParentFolder,
        SI.FSimplePidl, 1, LoWord(lParam), HiWord(lParam), Handle);
      { "Rename" item on context menu has ID of 19. }
      if Res = 19 then
        Selected.EditText;
      Exit;
    end;
    if (Msg = WM_ERASEBKGND) and DragScroll then begin
      Result := 0;
      Exit;
    end;
    if (Msg = WM_TIMER) and (WParam = 55) then begin
      if Assigned(DropTargetNode) then
        if DropTargetNode.HasChildren and
            not DropTargetNode.Expanded then begin
          DropTargetNode.Expand(False);
          ImageList_DragShowNoLock(False);
          Repaint;
          ImageList_DragShowNoLock(True);
        end;
      KillTimer(Handle, 55);
    end;
    if (Msg = WM_PAINT) and (not StartFolderSet)
        and (not (csDesigning in ComponentState)) then begin
      { Select the StartIn node. }
      StartFolderSet := True;
      Pidl := nil;
      if FStartInFolder <> '' then
        PidlFromPath(FStartInFolder, Controller.DesktopFolder, Pidl)
      else if FSpecialStartInFolder <> sfNone then
        SHGetSpecialFolderLocation(Handle,
          ShellFolders[FSpecialStartInFolder], Pidl);
      if not Assigned(Pidl) then begin
        if Assigned(FListView) then
          if FListView.Items.Count = 0 then
            FListView.FillList(Folders[0]);
            Exit;
      end;
      Node := FindNodeByPidl(Pidl);
      if Assigned(Node) then begin
        Selected := Node;
        Change(Node);
        Node.MakeVisible;
      end else
        Change(Items[0]);
    end;
  end;
  inherited;
end;

function TStCustomShellTreeView.DragEnter(const dataObj : IDataObject;
  grfKeyState : Integer; pt : TPoint; var dwEffect : Integer): HResult;
var
  FE       : TFormatEtc;
  ClientPt : TPoint;
begin
  DataObject := nil;
  FE.cfFormat := CF_HDROP;
  FE.dwAspect := DVASPECT_CONTENT;
  FE.ptd := nil;
  FE.tymed := TYMED_HGLOBAL;
  FE.lindex := -1;
  if (dataObj.QueryGetData(FE) <> NOERROR) then begin
    dwEffect := DROPEFFECT_NONE;
    Result := NOERROR;
  end else begin
    DataObject := dataObj;
    Result := NOERROR;
  end;
  ImageList_GetDragImage(nil, nil);
  ClientPt := ScreenToClient(pt);
  ImageList_DragEnter(Handle, ClientPt.x, ClientPt.y);
  DragScroll := False;
end;

function TStCustomShellTreeView.DragLeave : HResult;
begin
  if FExpandInterval <> 0 then
    KillTimer(Handle, 55);
  if Assigned(DataObject) then
    DataObject := nil;
  Result := NOERROR;
  DoingDragDrop := False;
  ImageList_DragLeave(Handle);
  if Assigned(DropTargetNode) then begin
    DropTargetNode.DropTarget := False;
    DropTargetNode := nil;
  end;
end;

function TStCustomShellTreeView.StDragOver(grfKeyState : Integer; pt : TPoint;
  var dwEffect : Integer): HResult;
const
  PassCount  : Integer = 0;
var
  Node       : TTreeNode;
  ClientPt   : TPoint;
  SI         : TStShellItem;
  DropTarget : IDropTarget;
  OldRect    : TRect;
  NewRect    : TRect;
  SP         : Integer;
begin
  Result := S_OK;
  if not Assigned(DataObject) then begin
    dwEffect := DROPEFFECT_NONE;
  end else begin
    OldRect := Rect(0, 0, 0, 0);
    DoingDragDrop := True;
    ClientPt := ScreenToClient(pt);
    Node := GetNodeAt(ClientPt.X, ClientPt.Y);
    if Assigned(Node) then begin
      ClientPt := ScreenToClient(pt);
      SP := GetScrollPos(Handle, SB_HORZ);
      if (ClientPt.Y < 10) and (Node <> Items[0]) then begin
        Sleep(30);
        ImageList_DragShowNoLock(False);
        if SP <> 1 then
          SendMessage(Handle, WM_SETREDRAW, 0, 0);
        TopItem := Node.GetPrevVisible;
        if SP <> 1 then begin
          SendMessage(Handle, WM_HSCROLL, SB_LEFT, 0);
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
        end;
        ImageList_DragShowNoLock(True);
        DragScroll := True;
      end;
      if (ClientPt.Y > Height - 30) and (Node.GetNext <> nil) then begin
        Sleep(30);
        ImageList_DragShowNoLock(False);
        if SP <> 1 then
          SendMessage(Handle, WM_SETREDRAW, 0, 0);
        SendMessage(Handle, WM_SETREDRAW, 0, 0);
        TopItem := TopItem.GetNextVisible;
        if SP <> 1 then begin
          SendMessage(Handle, WM_HSCROLL, SB_LEFT, 0);
          SendMessage(Handle, WM_SETREDRAW, 1, 0);
        end;
        ImageList_DragShowNoLock(True);
        DragScroll := True;
      end;
      SI := Folders[Integer(Node.Data)];
      if Assigned(SI.ParentFolder) then
        SI.ParentFolder.GetUIObjectOf(Handle, 1, SI.FSimplePidl,
          IDropTarget, nil, Pointer(DropTarget))
      else
        Controller.DesktopFolder.GetUIObjectOf(Handle, 1, SI.FSimplePidl,
          IDropTarget, nil, Pointer(DropTarget));
      if Assigned(DropTarget) then begin
        DropTarget.DragEnter(DataObject, grfKeyState, pt, dwEffect);
        Result := DropTarget.DragOver(grfKeyState, pt, dwEffect);
        DropTarget := nil;
      end;

      if DropTargetNode <> Node then begin
        { User has dragged to a new node. }
        if Assigned(DropTargetNode) then begin
          DropTargetNode.DropTarget := False;
          OldRect := DropTargetNode.DisplayRect(True);
        end;
        Node.DropTarget := True;
        DropTargetNode := Node;
        ImageList_DragMove(ClientPt.x, ClientPt.y);
        ImageList_DragShowNoLock(False);
        if DragScroll then begin
          NewRect := ClientRect;
          DragScroll := False;
        end else begin
          NewRect := Node.DisplayRect(True);
          InvalidateRect(Handle, @OldRect, False);
        end;
        InvalidateRect(Handle, @NewRect, False);
        UpdateWindow(Handle);
        ImageList_DragShowNoLock(True);
        if FExpandInterval <> 0 then begin
          KillTimer(Handle, 55);
          SetTimer(Handle, 55, FExpandInterval, nil);
        end;
        Exit;
      end;
    end else
      dwEffect := DROPEFFECT_NONE;
  end;
  ImageList_DragMove(ClientPt.x, ClientPt.y);
end;

function TStCustomShellTreeView.Drop(const dataObj : IDataObject;
  grfKeyState : Integer; pt : TPoint; var dwEffect : Integer): HResult;
var
  Node       : TTreeNode;
  ClientPt   : TPoint;
  DropTarget : IDropTarget;
  SI         : TStShellItem;
begin
  if FExpandInterval <> 0 then
    KillTimer(Handle, 55);
  Result := S_OK;
  if not Assigned(DataObject) then begin
    Result := E_FAIL;
    Exit;
  end;
  DragLeave();
  ClientPt := ScreenToClient(pt);
  Node := GetNodeAt(ClientPt.X, ClientPt.Y);
  SI := Folders[Integer(Node.Data)];
  grfKeyState := grfKeyState or MK_LBUTTON;
  if Assigned(SI.ParentFolder) then
    SI.ParentFolder.GetUIObjectOf(Handle, 1,
      SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
  else
    Controller.DesktopFolder.GetUIObjectOf(Handle, 1,
      SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget));
  if Assigned(DropTarget) then begin
    DropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect);
    Result := DropTarget.Drop(dataObj, grfKeyState, pt, dwEffect);
    DropTarget := nil;
  end;
  if Assigned(DropTargetNode) then begin
    DropTargetNode.DropTarget := False;
    DropTargetNode := nil;
  end;
  DoingDragDrop := False;
end;

procedure TStCustomShellTreeView.Click;
begin
  inherited;
  if Assigned(Selected) then begin
    {$IFDEF VERSION4}
    if HotTrack and not AutoExpand then
      Selected.Expand(False);
    {$ENDIF}
  end;
end;

procedure TStCustomShellTreeView.SetSpecialRootFolder(
  const Value : TStSpecialRootFolder);
begin
  if ((ShellFolders[Value] = CSIDL_INTERNET) or
     (ShellFolders[Value] = CSIDL_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_FAVORITES) or
     (ShellFolders[Value] = CSIDL_INTERNET_CACHE) or
     (ShellFolders[Value] = CSIDL_COOKIES) or
     (ShellFolders[Value] = CSIDL_HISTORY)) and
     (ShellVersion < 4.7) then
    RaiseStError(ESsShellError, ssscShellVersionError);

  if FSpecialRootFolder <> Value then begin
    FSpecialRootFolder := Value;

    if (FSpecialRootFolder <> sfNone) and not (csLoading in ComponentState) then begin
      { Can't have a RootFolder if SpecialRootFolder is assigned. }
      RootFolder := '';
      if (FSpecialStartInFolder <> sfNone) or (FStartInFolder <> '') then
        StartFolderSet := False;
      ClearTree;
      FillTree;
    end;
  end;
end;

procedure TStCustomShellTreeView.SetSpecialStartInFolder(
  const Value : TStSpecialRootFolder);
begin
  if ((ShellFolders[Value] = CSIDL_INTERNET) or
     (ShellFolders[Value] = CSIDL_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_FAVORITES) or
     (ShellFolders[Value] = CSIDL_INTERNET_CACHE) or
     (ShellFolders[Value] = CSIDL_COOKIES) or
     (ShellFolders[Value] = CSIDL_HISTORY)) and
     (ShellVersion < 4.7) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  if FSpecialStartInFolder <> Value then begin
    FSpecialStartInFolder := Value;
    if (FSpecialStartInFolder <> sfNone) and not (csLoading in ComponentState) then
      { Can't have a StartInFolder if SpecialStartInFolder is assigned. }
      StartInFolder := '';
  end;
end;

procedure TStCustomShellTreeView.SetCompressedColor(const Value : TColor);
var
  R : TRect;
begin
  FCompressedColor := Value;
  if (toColorCompressed in FOptions) then begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    Update;
  end;
end;

procedure TStCustomShellTreeView.SetOptions(const Value : TStTreeOptionsSet);
var
  DoRepaint : Boolean;
  R         : TRect;
 {$IFNDEF VER100}
  Style     : LongInt;
  DD        : Boolean;
 {$ENDIF}
begin
  if (not (toColorCompressed in FOptions) and (toColorCompressed in Value)) or
     ((toColorCompressed in FOptions) and not (toColorCompressed in Value)) then
    DoRepaint := True
  else
    DoRepaint := False;
  { If toAllowDrop is being added to the Options set }
  { then enable the drop target. }
  {$IFNDEF VER100}
  if not (csDesigning in ComponentState) then begin
    if not (toAllowDrop in FOptions) and (toAllowDrop in Value) then
      if HandleAllocated then
        RegisterDragDrop(Handle, Self);
    { If toAllowDrop is being removed from the Options }
    { set then disable the drop target. }
    if not (toAllowDrop in Value) and (toAllowDrop in FOptions) then
      if HandleAllocated then
        RevokeDragDrop(Handle);
    if HandleAllocated then begin
      DD := (toAllowDrag in FOptions);
      Style := GetWindowLong(Handle, GWL_STYLE);
      if DD then
        Style := Style and not TVS_DISABLEDRAGDROP
      else
        Style := Style or TVS_DISABLEDRAGDROP;
      SetWindowLong(Handle, GWL_STYLE, Style);
    end;
  end;
  {$ENDIF}
  FOptions := Value;
  if DoRepaint then begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    Update;
  end;
end;

procedure TStCustomShellTreeView.ClearTree;
begin
  Items.Clear;
end;

{$IFDEF VERSION4}
procedure TStCustomShellTreeView.DoCustomDrawItem(Sender : TCustomTreeView;
  Node : TTreeNode; State : TCustomDrawState; var DefaultDraw : Boolean);
var
  SI        : TStShellItem;
  DriveType : Byte;
begin
  if DoingDragDrop and Node.DropTarget then
    Exit;
  if not (csDesigning in ComponentState) then begin
    if (toColorCompressed in FOptions) then
      if not (cdsSelected in State) then
      begin
        if Folders.Count = 0 then //SZ
          Exit; //SZ
        SI := Folders[Integer(Node.Data)];
        if Assigned(SI) then begin
          { Reading the IsCompressed property on Windows 2000 will }
          { will result in a "drive not ready" message for some    }
          { removable media drives (Zip drives, for example. Don't }
          { read the IsCompressed property for removable drives.   }
          DriveType := GetDriveType(PChar(SI.Path));
          if DriveType = DRIVE_REMOVABLE then
            Exit;
          if Assigned(SI.ParentFolder) then
            if SI.IsCompressed then
              Canvas.Font.Color := FCompressedColor;
        end;
      end;
  end;
end;
{$ENDIF}

procedure TStCustomShellTreeView.DoFilterItem(const SI : TStShellItem;
  var Accept : Boolean);
begin
  if Assigned(FOnFilterItem) and Filtered then
    FOnFilterItem(Self, SI, Accept);
end;

procedure TStCustomShellTreeView.DoShellChangeNotify(const SI1 : TStShellItem;
  const SI2 : TStShellItem; const Events : TStNotifyEventsSet);
begin
  if Assigned(FOnShellChangeNotify) then
    FOnShellChangeNotify(Self, SI1, SI2, Events);
end;

procedure TStCustomShellTreeView.FindAndSelectNode(APidl : PItemIDList);
var
  I     : Integer;
  Node  : TTreeNode;
  List  : TList;
  Pidl  : PItemIDList;
begin
  Pidl := ILClone(APidl);
  { Find the first node that we have that is in the pidl }
  { for the list view folder that was just selected.     }
  Node := nil;
  List := TList.Create;
  while not Assigned(Node) do begin
    if not Assigned(Pidl) then
      Break;
    Node := FindNodeByPidl(Pidl);
    if Assigned(Node) then
      Break
    else begin
      List.Add(ILClone(Pidl));
      ILRemoveLastID(Pidl);
    end;
  end;
  ILFree(Pidl);

  { List.Count contains the number of nodes for which we don't }
  { have a corresponding tree view folder. }
  SendMessage(Handle, WM_SETREDRAW, 0, 0);
  Node.Expand(False);
  if List.Count > 0 then
    for I := Pred(List.Count) downto 0 do begin
      Pidl := PItemIDList(List.Items[I]);
      Node := FindNodeByPidl(Pidl);
      if Assigned(Node) then
        Node.Expand(False);
      ILFree(Pidl);
    end;
  List.Free;
  if Assigned(Node) then
    Node.MakeVisible
  else begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    MessageDlg('Bad folder', mtError, [mbOk], 0);
    exit;
  end;
  SendMessage(Handle, WM_SETREDRAW, 1, 0);
  Selected := Node;
  FSelectedFolder := Folders[Integer(Node.Data)];
end;

procedure TStCustomShellTreeView.SelectFolder(Path : string);
var
  Pidl  : PItemIDList;
begin
  PidlFromPath(Path, Controller.DesktopFolder, Pidl);
  if not Assigned(Pidl) then
    RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
  FindAndSelectNode(Pidl);
  ILFree(Pidl);
  FStartInFolder := Path;

end;

procedure TStCustomShellTreeView.SelectSpecialFolder(
  Folder : TStSpecialRootFolder);
var
  Pidl  : PItemIDList;
begin
  SHGetSpecialFolderLocation(Handle, ShellFolders[Folder], Pidl);
  if Assigned(Pidl) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  FindAndSelectNode(Pidl);
  ILFree(Pidl);
end;

function TStCustomShellTreeView.ListViewFolderChange(
  SI : TStShellItem; MovingUp : Boolean) : Boolean;
var
  Node  : TTreeNode;
begin
  Result := True;
  { Could be that the list view's MoveUpOneLevel method was called. }
  { In that case we can't allow the level to move up beyond the top }
  { node of the tree view. }
  if MovingUp then begin
    ListViewChange := True;
    Node := FindNodeByPidl(SI.Pidl);
    if Assigned(Node) then
      Selected := Node
    else
      Result := False;
    ListViewChange := False;
  end else begin
    ListViewChange := True;
    FindAndSelectNode(SI.Pidl);
    ListViewChange := False;
  end;
end;

procedure TStCustomShellTreeView.ListViewFolderCreate(
  SI : TStShellItem);
var
  Node    : TTreeNode;
  NewNode : TTreeNode;
  SF      : TStShellFolder;
begin
  Node := FindParentNode(SI);
  if Assigned(Node) then begin
    SF := TStShellFolder.Create(Controller);
    SF.Assign(TStShellFolder(SI));
    if not Assigned(SF.FParentFolder) then
      SF.FParentFolder := Controller.DesktopFolder;
    NewNode := Items.AddChild(Node, SF.DisplayName);
    SF.ParentList := Folders.FList;
    NewNode.Data := Pointer(Folders.FList.Add(SF));
    NewNode.ImageIndex := SF.IconIndex;
    NewNode.SelectedIndex := SF.OpenIconIndex;
    CustomSort(TreeCompareFunc, Integer(Self));
  end;
end;

function TStCustomShellTreeView.FindNodeByPidl(Pidl : PItemIDList) : TTreeNode;
var
  Node : TTreeNode;
begin
  Node := Items[0];
  while Assigned(Node) do begin
    if ILIsEqual(Folders[Integer(Node.Data)].Pidl, Pidl) then begin
      Result := Node;
      Exit;
    end;
    Node := Node.GetNext;
  end;
  Result := Node;
end;

function TStCustomShellTreeView.FindNodeByPath(Path : string) : TTreeNode;
var
  Node : TTreeNode;
begin
  Node := Items[0];
  while Assigned(Node) do begin
    if Folders[Integer(Node.Data)].Path = Path then begin
      Result := Node;
      Exit;
    end;
    Node := Node.GetNext;
  end;
  Result := Node;
end;

function TStCustomShellTreeView.FindParentNode(SI : TStShellItem) : TTreeNode;
var
  I    : Integer;
  Pidl : PItemIDList;
begin
  Result := nil;
  for I := 0 to Pred(Folders.Count) do begin
    Pidl := ILClone(SI.Pidl);
    ILRemoveLastID(Pidl);
    { Search for the parent folder. }
    Result := FindNodeByPidl(Pidl);
  end;
end;

procedure TStCustomShellTreeView.ShellEvent(Sender : TObject;
  SI1, SI2 : TStShellItem; Events : TStNotifyEventsSet);
const
  LastPidl : PItemIDList = nil;
var
  I       : Integer;
  Node    : TTreeNode;
  NewNode : TTreeNode;
  SF      : TStShellFolder;
  Pidl    : PItemIDList;
begin
  DoShellChangeNotify(SI1, SI2, Events);
  if InternalEvent then begin
    InternalEvent := False;
    Exit;
  end;
  if not Assigned(SI1) then
    Exit;
  { Something happened in the shell. See if it's something we   }
  { need to handle. We could get this event twice. If we do, we }
  { don't add to the tree view twice. }
  if ((neFolderCreate in Events) or (neDriveAdd in Events))
       or ((neFileCreate in Events) and (toShowFiles in Options))
       and (SI1.Pidl <> LastPidl) then begin
    { A folder was created. See if its parent is one we have in the list. }
    Node := FindParentNode(SI1);
    if Assigned(Node) then
      { If the node doesn't have any subnodes then we don't }
      { need to do anything. }
      if Node.Count <> 0 then begin
        SF := TStShellFolder.Create(Controller);
        SF.Assign(TStShellFolder(SI1));
        if not Assigned(SF.FParentFolder) then
          SF.FParentFolder := Controller.DesktopFolder;
        SF.ParentList := Folders.FList;
        { It could be that a network drive is in the list but was }
        { not connected and has been reconnected. If that is the  }
        { case then we don't want to add the folder again. }
        NewNode := FindNodeByPath(SF.Path);
        if not Assigned(NewNode) then
          NewNode := Items.AddChild(Node, SF.DisplayName);
        NewNode.Data := Pointer(Folders.FList.Add(SF));
        LastPidl := SF.Pidl;
        NewNode.ImageIndex := SF.IconIndex;
        NewNode.SelectedIndex := SF.OpenIconIndex;
        NewNode.OverlayIndex := SF.OverlayIconIndex;
        if (neDriveAdd in Events) then
          NewNode.HasChildren := True;
        CustomSort(TreeCompareFunc, Integer(Self));
        Exit;
      end;
  end;
  if ((neFolderDelete in Events) or (neDriveRemove in Events)
       or ((neFileDelete in Events) and (toShowFiles in Options))) then begin
    { A folder was deleted. See if it is one we have in the list. }
    Node := FindNodeByPidl(SI1.Pidl);
    if not Assigned(Node) then
      Node := FindNodeByPath(SI1.Path);
    if Assigned(Node) and (Selected = Node) then
      Selected := Node.GetNext;
    if Assigned(Node) then begin
      { Remove it from the underlying list and from the tree view. }
      Folders[Integer(Node.Data)].Free;
      Folders.FList[Integer(Node.Data)] := nil;
      Node.Delete;
      Exit;
    end;
  end;
  if (neFolderRename in Events) or
     ((neFileRename in Events) and (toShowFiles in Options)) then begin
    { A folder was renamed. This could be an item deleted to the }
    { recycle bin. See if the destination is the recycle bin.    }
    SHGetSpecialFolderLocation(
      Application.Handle, CSIDL_DESKTOP, Pidl);

    if Assigned(SI2) then begin
      if ILIsEqual(Pidl, SI2.Pidl) then begin
        { It's the recycle bin so delete from the tree view. }
        Node := FindNodeByPidl(SI1.Pidl);
        if Assigned(Node) then begin
          Folders[Integer(Node.Data)].Free;
          Folders.FList[Integer(Node.Data)] := nil;
          Node.Delete;
          Exit;
        end;
      end else
        { See if it's one we have in the list. }
        for I := 0 to Pred(Folders.Count) do begin
          if AnsiUpperCase(Folders[I].Path) = AnsiUpperCase(SI1.Path) then begin
            Folders[I].Assign(TStShellFolder(SI2));
            if not Assigned(Folders[I].FParentFolder) then
              Folders[I].FParentFolder := Controller.DesktopFolder;
            Node := FindNodeByPidl(SI2.FPidl);
            if Assigned(Node) then begin
              Node.Text := SI2.DisplayName;
              CustomSort(TreeCompareFunc, Integer(Self));
            end;
            Break;
          end;
        end;
    end;
  end;
  if (neNetShare in Events) or (neNetUnShare in Events) then begin
    Node := FindNodeByPidl(SI1.Pidl);
    if Assigned(Node) then
      Node.OverlayIndex := SI1.OverlayIconIndex;
  end;
  if (neMediaInsert in Events) or (neMediaRemove in Events) then begin
    { Media was added or removed. We need to flush the shell  }
    { cache and update the display or we'll just get a cached }
    { icon and display name. }
    Node := FindNodeByPath(SI1.Path);
    if Assigned(Node) then begin
      Node.ImageIndex := SI1.IconIndex;
      Node.Text := SI1.DisplayName;
    end;
  end;
end;

procedure TStCustomShellTreeView.FillTree;
var
  Pidl     : PItemIDList;
  RootNode : TTreeNode;
  SI       : TStShellItem;
  {$IFDEF VER100}
  Attr     : DWORD;
  {$ELSE}
  Attr     : Cardinal;
  {$ENDIF}
  I        : Integer;
  Node     : TTreeNode;
  Res      : Boolean;
  PidlList : TList;
begin
  Folders.Clear;
  if Assigned(TopItem) then begin
    TopItem.DeleteChildren;
    TopItem.Free;
  end;
  RootNode := nil;
  if not Assigned(Controller) or not HandleAllocated then begin
    StartFolderSet := True;
    Exit;
  end;
  if (FSpecialRootFolder = sfNone) and (FRootFolder = '') then begin
    StartFolderSet := True;
    Exit;
  end;

  { Populate the TreeView. }
  if FSpecialRootFolder <> sfNone then begin
    SHGetSpecialFolderLocation(Handle,
      ShellFolders[FSpecialRootFolder], Pidl);
    if not Assigned(Pidl) then
      RaiseStError(ESsShellError, ssscShellVersionError);
    RootNode := Items.AddChild(nil, '');
    SetNodeAttributes(RootNode, Pidl, True, 0);
    SI := TStShellItem.CreateFromPidl(Pidl, Controller);
    SI.ParentList := Folders.FList;
    RootNode.Text := SI.FDisplayName;
    RootFolderIndex := Folders.FList.Add(SI);
    RootNode.Data := Pointer(RootFolderIndex);
    Controller.TreeViewNode := RootNode;
    Controller.Enumerate(Pidl, etTree);
    CustomSort(TreeCompareFunc, Integer(Self));
  end else begin
    { Starting with a folder name rather than with a
    { special folder. }
    if FRootFolder <> '' then begin
      Attr := SFGAO_FILESYSTEM or SFGAO_FOLDER;
      PidlFromPath(FRootFolder, Controller.DesktopFolder, Pidl);
      if not Assigned(Pidl) then
        RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
      RootNode := Items.Add(Selected, '');
      SetNodeAttributes(RootNode, Pidl, True, Attr);
      SI := TStShellItem.CreateFromPidl(ILClone(Pidl), Controller);
      SI.ParentList := Folders.FList;
      RootNode.Text := SI.FDisplayName;
      RootFolderIndex := Folders.FList.Add(SI);
      RootNode.Data := Pointer(RootFolderIndex);
      Controller.TreeViewNode := RootNode;
      Controller.Enumerate(Pidl, etTree);
      CustomSort(TreeCompareFunc, Integer(Self));
    end;
  end;
  if Assigned(RootNode) then
    if (toExpandTopNode in FOptions) then
      RootNode.Expand(False);

  { Expand the StartInFolder or SpecialStartInFolder node. }
  if FStartInFolder <> '' then
    PidlFromPath(FStartInFolder, Controller.DesktopFolder, Pidl)
  else
    SHGetSpecialFolderLocation(Handle,
      ShellFolders[FSpecialStartInFolder], Pidl);
  if not Assigned(Pidl) then begin
    StartFolderSet := True;
    Exit;
  end;
  { Build a list of pidls, working backwards. The first pidl }
  { in the list is the fully-qualified pidl. }
  PidlList := TList.Create;
  PidlList.Add(ILClone(Pidl));
  Res := True;
  while Res do begin
    { Remove the last ID and save the resulting pidl in the list. }
    Res := ILRemoveLastID(Pidl);
    if Res then
      PidlList.Add(ILClone(Pidl));
  end;
  { Work backwards through the list, expanding nodes as we go. }
  for I := Pred(PidlList.Count) downto 0 do begin
    Node := FindNodeByPidl(PidlList[I]);
    if Assigned(Node) then
      if not Node.Expanded then
        Node.Expand(False);
    ILFree(PidlList[I]);
  end;
  PidlList.Free;
end;

procedure TStCustomShellTreeView.SetFiltered(const Value : Boolean);
begin
  FFiltered := Value;
  if not (csDesigning in ComponentState) and
     not (csLoading in ComponentState) then begin
    FillTree;
    Change(TopItem);
  end;
end;

procedure TStCustomShellTreeView.SetListView(
  const Value : TStCustomShellListView);
begin
  FListView := Value;
  if not Assigned(Value) then
    Exit;
  if (csDesigning in ComponentState) then
    FListView.TreeView := Self;
  if not Assigned(Controller) and Assigned(FListView.Controller) then
    Controller := FListView.Controller;
end;

procedure TStCustomShellTreeView.SetMaxNotifications(const Value : Integer);
begin
  if Assigned(ShellMonitor) then
    ShellMonitor.MaxNotifications := FMaxNotifications;
end;

function TStCustomShellTreeView.AddFolder(FolderName : string) : Boolean;
var
  Path  : string;
  S     : string;
  SF    : TStShellFolder;
  Pidl  : PItemIDList;
  Count : Integer;
  Done  : Boolean;
  Enum  : TStShellEnumerator;
  Index : Integer;
  I     : Integer;
begin
  if SelectedFolder.IsReadOnly then
    RaiseStError(ESsInvalidFolder, ssscFolderReadOnly);
  if FolderName = '' then
    FolderName := ssscDefaultFolderName;
  { If the folder name already exists in this folder, give it a new name. }
  Enum := TStShellEnumerator.Create(nil);
  Enum.RootPidl := SelectedFolder.FPidl;
  Enum.Execute;
  S := FolderName;
  Count := 2;
  Done := False;
  while not Done do begin
    Index := -1;
    for I := 0 to Pred(Enum.ShellItems.Count) do
      if AnsiUpperCase(S) = AnsiUpperCase(Enum.ShellItems[I].DisplayName) then begin
        S := FolderName + ' (' + IntToStr(Count) + ')';
        Inc(Count);
        Index := I;
        Break;
      end;
    { No match found. }
    Done := (Index = -1);
  end;
  Enum.Free;
  Path := SelectedFolder.Path;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  Path := Path + S;

  { Expand the current node, if necessary. }
  if not Selected.Expanded then
    Selected.Expand(False);

  { Create the directory. }
  Result := Boolean(CreateDirectory(PChar(Path), nil));

  { Add it to the tree view and go to edit mode. }
  PidlFromPath(Path, Controller.DesktopFolder, Pidl);
  SF := TStShellFolder.CreateFromPidl(Pidl, Controller);
  SF.ParentList := Folders.FList;
  { Creating a directory using CreateDirectory will not result }
  { in a shell event being sent. However, we can let the shell }
  { know by calling SHChangeNotify. }
  SHChangeNotify(SHCNE_MKDIR, SHCNF_IDLIST, SF.Pidl, nil);
end;

function TStCustomShellTreeView.DeleteFolder(
  Recycle : Boolean; Confirm : Boolean) : Boolean;
var
  FO : TStFileOperation;
begin
  if not Assigned(SelectedFolder) then
    Result := False
  else begin
    FO := TStFileOperation.Create(Self);
    FO.SourceFiles.Add(SelectedFolder.FPath);
    FO.Operation := fopDelete;
    if not Recycle then
      FO.Options := FO.Options - [foAllowUndo];
    if not Confirm then
      FO.Options := FO.Options + [foNoConfirmation];
    Result := FO.Execute;
    FO.Free;
  end;
end;

function TStCustomShellTreeView.RenameFolder(NewName : string) : Boolean;
begin
  Result := Controller.RenameItem(Folders[Integer(Selected.Data)], NewName);
  if Result then begin
    Selected.Text := NewName;
    CustomSort(TreeCompareFunc, Integer(Self));
  end;
end;

procedure TStCustomShellTreeView.Refresh(ANode : TTreeNode);
var
  ExpandedNodes : TList;
  Node          : TTreeNode;
  I             : Integer;
  SF            : TStShellFolder;
begin
  if not Assigned(ANode) then
    ANode := Items[0];
  {if ANode <> nil then begin}
  ExpandedNodes := TList.Create;
  Node := ANode;
  { Enumerate the selected node's children, saving any }
  { nodes that are expanded in a list. }
  while Assigned(Node) do begin
    Node := Node.GetNextVisible;
    if Assigned(Node) then
      if Node.Expanded then begin
        SF := TStShellFolder.Create(Controller);
        SF.Assign(Folders[Integer(Node.Data)]);
        if not Assigned(SF.FParentFolder) then
          SF.FParentFolder := Controller.DesktopFolder;
        { Free the memory for the old folder item, but }
        { don't remove it from the list or the indexes }
        { of the remaining items will be off. }
        TStShellFolder(Folders[Integer(Node.Data)]).Free;
        Folders.FList[Integer(Node.Data)] := nil;
        ExpandedNodes.Add(SF);
      end;
    { If this node's parent is the same as the selected }
    { node's parent then we are done enumerating nodes. }
    if Assigned(Node) then
      if Node.Parent = ANode.Parent then
        Node := nil;
  end;
  SendMessage(Handle, WM_SETREDRAW, 0, 0);
  ANode.DeleteChildren;
  ANode.HasChildren := True;
  ANode.Expand(False);
  { Expand the previously expanded nodes if they still exist. }
  for I := 0 to Pred(ExpandedNodes.Count) do begin
    SF := ExpandedNodes[I];
    Node := FindNodeByPidl(SF.FPidl);
    if Assigned(Node) then
      Node.Expand(False);
  end;
  for I := 0 to Pred(ExpandedNodes.Count) do begin
    SF := ExpandedNodes[I];
    SF.Free;
  end;
  ExpandedNodes.Clear;
  ExpandedNodes.Free;
  SendMessage(Handle, WM_SETREDRAW, 1, 0);
  {end; }
end;

procedure TStCustomShellTreeView.CopyToClipboard;
var
  SF : TStShellFolder;
begin
  if Assigned(Selected) then begin
    SF := Folders[Integer(Selected.Data)];
    SF.CopyToClipboard;
  end;
end;

procedure TStCustomShellTreeView.CutToClipboard;
var
  SF : TStShellFolder;
begin
  if Assigned(Selected) then begin
    SF := Folders[Integer(Selected.Data)];
    SF.CutToClipboard;
  end;
end;

procedure TStCustomShellTreeView.PasteFromClipboard;
var
  SF : TStShellFolder;
begin
  if Assigned(Selected) then begin
    SF := Folders[Integer(Selected.Data)];
    SF.PasteFromClipboard;
  end;
end;

{ TStCustomShellListView }

constructor TStCustomShellListView.Create(AOwner : TComponent);
var
  MI         : TMenuItem;
  VI         : TSsVersionInfo;
  WinDir     : array [0..MAX_PATH - 1] of Char;
  Reg        : TRegistry;
  RegKeys    : TStringList;
  I          : Integer;
  S1, S2     : string;
  Str        : PChar;
begin
  inherited Create(AOwner);
  {$IFNDEF VERSION2009}
  if not Assigned(@ILClone) then
    LoadILFunctions;
  {$ENDIF}
  FOptions         := [loAllowRename, loAllowDrag, loAllowDrop, loShellMenu];
  FCompressedColor := clBlue;
  FOptimization    := otEnumerate;
  FFiltered        := False;
  FFileFilter      := '';
  FRootFolder      := '';
  FMaxNotifications := 5;
  FOpenDialogMode  := False;
  FSpecialRootFolder := sfNone;
  { Base class property defaults }
  Width       := 270;
  Height      := 180;
  SortType    := stNone;
  MultiSelect := True;
  {$IFDEF VERSION4}
  OwnerData   := True;
  {$ENDIF}
  FilteredList := TStShellItemList.Create;
  FullList     := TStShellItemList.Create;
  FSelectedItems := TStShellItemList.Create;

  ClickedColumn := 0;
  Ascending     := True;
  RecreatingWnd := False;

  {$IFDEF VERSION4}
  OnCustomDrawItem := DoCustomDrawItem;
  {$ENDIF}

  { Create a default menu. }
  if not (csDesigning in ComponentState) then begin
    DefPopup := TPopupMenu.Create(Self);
    MI := TMenuItem.Create(Self);
    MI.Caption := ssscViewCaptionLargeIcon;
    MI.RadioItem := True;
    MI.OnClick := DoLargeIconClick;
    DefPopup.Items.Add(MI);

    MI := TMenuItem.Create(Self);
    MI.Caption := ssscViewCaptionSmallIcon;
    MI.RadioItem := True;
    MI.OnClick := DoSmallIconClick;
    DefPopup.Items.Add(MI);

    MI := TMenuItem.Create(Self);
    MI.Caption := ssscViewCaptionList;
    MI.RadioItem := True;
    MI.OnClick := DoListClick;
    DefPopup.Items.Add(MI);

    MI := TMenuItem.Create(Self);
    MI.RadioItem := True;
    MI.Caption := ssscViewCaptionDetails;
    MI.OnClick := DoDetailsClick;
    DefPopup.Items.Add(MI);

    PopupMenu := DefPopup;
  end;

  { Get the version of Shell32.dll. Some special folders }
  { require Version 4.71 or higher. }
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;
  if not (csDesigning in ComponentState) then begin
    stNameCol       := ShellShockStr(ssscName);
    stSizeCol       := ShellShockStr(ssscSize);
    stTypeCol       := ShellShockStr(ssscType);
    stModifiedCol   := ShellShockStr(ssscModified);
    stAttributesCol := ShellShockStr(ssscAttributes);
    stFileFolder    := ShellShockStr(ssscFileFolder);
    stSystemFolder  := ShellShockStr(ssscSystemFolder);
    stOriginalLoc   := ShellShockStr(ssscOriginalLoc);
    stDateDeleted   := ShellShockStr(ssscDateDeleted);
    stFile          := ShellShockStr(ssscFile);
  end;

  { Get a list of file associations and store them in a TStDictionary. }
  if not (csDesigning in ComponentState) then begin
    Reg := TRegistry.Create;
    Reg.RootKey := HKEY_CLASSES_ROOT;
    RegKeys := TStringList.Create;
    Reg.OpenKey('', False);
    Reg.GetKeyNames(RegKeys);
    TypeNames := TStDictionary.Create(250);
    TypeNames.DisposeData := DictionaryDispose;
    for I := 0 to Pred(RegKeys.Count) do begin
      S1 := RegKeys[I];
      if S1[1] = '.' then begin
        if Reg.OpenKey(RegKeys[I], False) then begin
          S1 := Reg.ReadString('');
          Reg.CloseKey;
          if Reg.OpenKey(S1, False) then begin
            S2 := Reg.ReadString('');
            if S2 <> '' then begin
              GetMem(Str, (Length(S2) + 1) * SizeOf(Char));
              StrPCopy(Str, S2);
              TypeNames.Add(RegKeys[I], Str);
            end;
            Reg.CloseKey;
          end;
        end;
      end;
    end;
    RegKeys.Free;
    Reg.Free;
  end;
  { Set up shell notifications. }
  if not (csDesigning in ComponentState) then begin
    ShellMonitor := TStShellNotification.Create(Self);
    { For the tree view and list view we don't need to know     }
    { about every single file that was changed, deleted, and so }
    { on. Instead, we'll just get a few notifications as that   }
    { will be sufficient to properly update the components.     }
    ShellMonitor.MaxNotifications := FMaxNotifications;                
    ShellMonitor.WatchSubFolders := False;
    ShellMonitor.OnShellChangeNotify := ShellEvent;
  end;
end;

destructor TStCustomShellListView.Destroy;
begin
  ShellMonitor.Free;
  if Assigned(TypeNames) then begin
    TypeNames.Clear;
    TypeNames.Free;
  end;
  FilteredList.Free;
  FullList.Free;
  FSelectedItems.Free;
  if Assigned(Controller) and OwnController and
      not (csDesigning in ComponentState) then begin
    Controller.Free;
    Controller := nil;
  end;
  if Assigned(FFolder) then
    FFolder.Free;
  if Assigned(DropTargetItem) then
    DropTargetItem.Free;
  inherited;
end;

procedure TStCustomShellListView.CreateWnd;
begin
  inherited CreateWnd;
  {$IFNDEF VER100}
  if not (csDesigning in ComponentState) then
    if (loAllowDrop in FOptions) then
      RegisterDragDrop(Handle, Self);
  {$ENDIF}
  if RecreatingWnd then
    Exit;
  if not (csLoading in ComponentState) then begin
    if not Assigned(Controller) then
      if Assigned(FTreeView) then
        Controller := FTreeView.Controller
      else if Assigned(FComboBox) then
        Controller := FComboBox.Controller
      else begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
    Controller.ListView := Self;
    if not Assigned(SmallImages) then begin
      SmallImages := Controller.SmallFolderImages;
      LargeImages := Controller.LargeFolderImages;
    end;
    if Assigned(FTreeView) and
       Assigned(Controller.FTreeView) and
       not Assigned(Controller.FTreeView.Selected) then begin
      if FTreeView.Folders.Count <> 0 then
        FillList(FTreeView.Folders[FTreeView.RootFolderIndex])
    end else
      FillList(nil);
  end;
end;

procedure TStCustomShellListView.DestroyWnd;
begin
  if not (csLoading in ComponentState)
      or (csDesigning in ComponentState) then
    RecreatingWnd := True;                                             
  RevokeDragDrop(Handle);
  inherited;
end;

procedure TStCustomShellListView.Loaded;
begin
  inherited Loaded;
  if RecreatingWnd then begin                                          
    RecreatingWnd := False;                                            
    Exit;                                                              
  end;                                                                 
  if not (csLoading in ComponentState) then begin
    { Create a controller if necessary. If the tree view has already been }
    { created then hook up to its controller. }
    if not Assigned(Controller) then
      if Assigned(FTreeView) and Assigned(FTreeView.Controller) then
        Controller := FTreeView.Controller
      else if Assigned(FComboBox) and Assigned(FComboBox.Controller) then
        Controller := FComboBox.Controller
      else begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
    Controller.ListView := Self;
    if not Assigned(SmallImages) then begin
      SmallImages := Controller.SmallFolderImages;
      LargeImages := Controller.LargeFolderImages;
    end;
    FillList(Folder);
  end;
end;

procedure TStCustomShellListView.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not (csDesigning in ComponentState) then
    Exit;
  if (AComponent = FTreeView) and (Operation = opRemove) then
    FTreeView := nil;
  if (AComponent = FComboBox) and (Operation = opRemove) then
    FComboBox := nil;
end;

function TStCustomShellListView.CanEdit(Item : TListItem): Boolean;
var
  SI : TStShellItem;
begin
  inherited CanEdit(Item);
  SI := ShellItems[Integer(Selected.Data)];
  Result := SI.CanRename and (loAllowRename in FOptions);
end;

procedure TStCustomShellListView.Edit(const Item : TLVItem);
var
  SI      : TStShellItem;
  NewPidl : PItemIDList;
  //NewName : array [0..MAX_PATH - 1] of Wide Char;
begin
  inherited Edit(Item);
  SI := ShellItems[Integer(Selected.Data)];
  { Text didn't change so exit. }
  if (Item.pszText = SI.DisplayName) or (Item.pszText = '') then
    Exit;
  if SI.CanRename then begin
    Screen.Cursor := crHourglass;
    try
      //StringToWideChar(Item.pszText, NewName, MAX_PATH);
      SI.ParentFolder.SetNameOf(
        Handle, SI.FSimplePidl, PChar(Item.pszText), SHGDN_NORMAL, NewPidl);
      if Assigned(NewPidl) then begin
        { Rename was succesful so copy the new pidl. }
        SI.FSimplePidl := ILClone(NewPidl);
        SI.FPidl := ILCombine(GetParentPidl(SI.FPidl), NewPidl);
        ILFree(NewPidl);
        SI.FPath := Controller.GetDisplayName(
          SI.ParentFolder, SI.FSimplePidl, SHGDN_FORPARSING);
        SI.FDisplayName := Item.pszText;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TStCustomShellListView.SetCompressedColor(const Value : TColor);
var
  R : TRect;
begin
  FCompressedColor := Value;
  if (loColorCompressed in FOptions) then begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    Update;
  end;
end;

procedure TStCustomShellListView.SetMaxNotifications(const Value : Integer); 
begin
  if Assigned(ShellMonitor) then
    ShellMonitor.MaxNotifications := FMaxNotifications;
end;

procedure TStCustomShellListView.SetOptions(const Value : TStListOptionsSet);
var
  DoRepaint : Boolean;
  R         : TRect;
begin
  if (not (loColorCompressed in FOptions) and (loColorCompressed in Value)) or
     ((loColorCompressed in FOptions) and not (loColorCompressed in Value)) then
    DoRepaint := True
  else
    DoRepaint := False;
  { If loAllowDrop is being added to the Options set }
  { then enable the drop target. }

  {$IFNDEF VER100}
  if not (csDesigning in ComponentState) then begin
    if not (loAllowDrop in FOptions) and (loAllowDrop in Value) then
      if HandleAllocated then
        RegisterDragDrop(Handle, Self);
    { If toAllowDrop is being removed from the Options }
    { set then disable the drop target. }
    if not (loAllowDrop in Value) and (loAllowDrop in FOptions) then
      if HandleAllocated then
        RevokeDragDrop(Handle);
  end;
  {$ENDIF}

  FOptions := Value;
  if DoRepaint then begin
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    Update;
  end;
end;

{$IFDEF VERSION4}
procedure TStCustomShellListView.DoCustomDrawItem(Sender : TCustomListView;
  Item : TListItem; State : TCustomDrawState; var DefaultDraw : Boolean);
var
  SI        : TStShellItem;
  DriveType : Byte;
begin
  if not (csDesigning in ComponentState) then
    if (loColorCompressed in FOptions) then begin
      SI := ShellItems[Integer(Item.Data)];
      if Assigned(SI) then begin
        { Reading the IsCompressed property on Windows 2000 will }
        { will result in a "drive not ready" message for some    }
        { removable media drives (Zip drives, for example. Don't }
        { read the IsCompressed property for removable drives.   }
        DriveType := GetDriveType(PChar(SI.Path));
        if DriveType = DRIVE_REMOVABLE then
          Exit;
        if SI.IsCompressed then
          Canvas.Font.Color := FCompressedColor;
      end;
    end;
end;
{$ENDIF}

procedure TStCustomShellListView.TreeViewSelChange(const SI : TStShellItem);
begin
  { The TreeView selection changed. Enumerate the folder. }
  if Assigned(FComboBox) then
    FComboBox.ListViewFolderChange(SI, False);
  FillList(TStShellFolder(SI));
end;

procedure TStCustomShellListView.ComboBoxSelChange(const SI : TStShellItem);
begin
  { The ComboBox selection changed. }
  FillList(TStShellFolder(SI));
  if Assigned(FTreeView) then                                          
    FTreeView.ListViewFolderChange(SI, False);                         
end;

procedure TStCustomShellListView.DoDetailsClick(Sender : TObject);
begin
  ViewStyle := vsReport;
  DefPopup.Items[0].Checked := False;
  DefPopup.Items[1].Checked := False;
  DefPopup.Items[2].Checked := False;
  DefPopup.Items[3].Checked := True;
end;

procedure TStCustomShellListView.DoLargeIconClick(Sender : TObject);
begin
  ViewStyle := vsIcon;
  DefPopup.Items[0].Checked := True;
  DefPopup.Items[1].Checked := False;
  DefPopup.Items[2].Checked := False;
  DefPopup.Items[3].Checked := False;
end;

procedure TStCustomShellListView.DoListClick(Sender : TObject);
begin
  ViewStyle := vsList;
  DefPopup.Items[0].Checked := False;
  DefPopup.Items[1].Checked := False;
  DefPopup.Items[2].Checked := True;
  DefPopup.Items[3].Checked := False;
end;

procedure TStCustomShellListView.DoSmallIconClick(Sender : TObject);
begin
  ViewStyle := vsSmallIcon;
  DefPopup.Items[0].Checked := False;
  DefPopup.Items[1].Checked := True;
  DefPopup.Items[2].Checked := False;
  DefPopup.Items[3].Checked := False;
end;

procedure TStCustomShellListView.DblClick;
var
  Item          : TListItem;
  SI            : TStShellItem;
  SI2           : TStShellItem;
  DefaultAction : Boolean;
begin
  inherited;
  if not Assigned(Selected) then
    Exit;
  Item := Selected;
  SI := ShellItems[Integer(Item.Data)];
  DefaultAction := True;
  if FOpenDialogMode and Assigned(SI) then begin
    DoItemDblClick(SI, DefaultAction);
    if DefaultAction then begin
      if SI.IsFolder then begin
        SI2 := TStShellItem.Create(Controller);
        SI2.Assign(SI);
        if not Assigned(SI2.FParentFolder) then
          SI2.FParentFolder := Controller.DesktopFolder;
        if Assigned(FTreeView) then
          FTreeView.ListViewFolderChange(SI, False);
        if Assigned(FComboBox) then
          FComboBox.ListViewFolderChange(SI, False);
        FillList(TStShellFolder(SI2));
        SI2.Free;
      end;
    end;
  end else begin
    DoItemDblClick(SI, DefaultAction);
    if DefaultAction then begin
      if SI.IsFileSystem and not SI.IsFileFolder then
        ShellExecute(Handle, nil, PChar(SI.Path), '', '', SW_NORMAL) //SZ replaced 'open' with nil so that the default verb will be executed
      else if not SI.IsFolder then
        SI.Execute
      else if (loOpenFoldersInNewWindow in FOptions) then
        SI.Execute
      else begin
        SI2 := TStShellItem.Create(Controller);
        SI2.Assign(SI);
        if not Assigned(SI2.FParentFolder) then
          SI2.FParentFolder := Controller.DesktopFolder;
        if Assigned(FTreeView) then
          FTreeView.ListViewFolderChange(SI, False);
        if Assigned(FComboBox) then
          FComboBox.ListViewFolderChange(SI, False);
        FillList(TStShellFolder(SI2));
        SI2.Free;
      end;
    end;                                                               
  end;
end;

procedure TStCustomShellListView.GetColHeader(
  SI : TStShellItem; Columns : TListColumns);
var
  Details : IShellDetails;
  SD      : TStShellDetails;
  Col     : Integer;
  ColText : string;
  PBuff   : PChar;
  AFolder : IShellFolder;
  Pidl    : PItemIDList;
  LC      : TListColumn;
begin
  Columns.Clear;

  { Try to get an IShellDetails object. }
  Controller.BindToObject(Controller.DesktopFolder, SI.Pidl, AFolder);
  Pidl := SI.Pidl;
  if not Assigned(AFolder) then
    AFolder := Controller.DesktopFolder;
  AFolder.QueryInterface(IID_IShellDetails, Pointer(Details));
  AFolder.CreateViewObject(0, IID_IShellDetails, Pointer(Details));
  if not Assigned(Details) then begin
    { In some cases you can't get an IShellDetails for the file system }
    { objects. For those cases we have to manually create the col headers. }
    if SI.IsFileFolder or not Assigned(SI.ParentFolder) then begin
      LC := Columns.Add;
      LC.Caption := _(LNGNameCol);
      LC.Width := 175;
      LC := Columns.Add;
      LC.Caption := _(LNGSizeCol);
      LC.Width := 75;
      LC.Alignment := taRightJustify;
      LC := Columns.Add;
      LC.Caption := _(LNGTypeCol);
      LC.Width := 125;
      LC := Columns.Add;
      LC.Caption := _(LNGModifiedCol);
      LC.Width := 125;
      LC := Columns.Add;
      LC.Caption := _('Attributes');
      LC.Width := 60;
    end else begin
      { Is it the Recycle Bin? }
      SHGetSpecialFolderLocation(0, CSIDL_BITBUCKET, Pidl);
      if ILIsEqual(SI.Pidl, Pidl) then begin
        LC := Columns.Add;
        LC.Caption := _(LNGNameCol);
        LC.Width := 125;
        LC := Columns.Add;
        LC.Caption := _('Original location');
        LC.Width := 125;
        LC := Columns.Add;
        LC.Caption := _('Date deleted');
        LC.Width := 100;
        LC := Columns.Add;
        LC.Caption := _(LNGTypeCol);
        LC.Width := 125;
        LC := Columns.Add;
        LC.Caption := _(LNGSizeCol);
        LC.Width := 60;
      end else begin
        LC := Columns.Add;
        LC.Caption := _(LNGNameCol);
        LC.Width := 175;
      end;
    end;
    FileSystemSort := True;
    Exit;
  end;
  FileSystemSort := SI.IsFileSystem;
  Col := 0;
  while Details.GetDetailsOf(nil, Col, SD) = 0 do begin
    case SD.str.uType of
      STRRET_CSTR : ColText := string(SD.str.cStr);
      STRRET_WSTR : begin
                      ColText := WideCharToString(SD.str.pOleStr);
                      CoTaskMemFree(SD.str.pOleStr); // SZ: pOleStr must be freed
                    end;
      STRRET_OFFSET :
        begin
          PBuff := PChar(PAnsiChar(Pidl) + SD.str.uOffset);
          ColText := PBuff;
        end;
    end;
    with Columns.Add do begin
      Caption := ColText;
      {$IFDEF VERSION4}
      Width := SD.cxChar * Canvas.TextWidth('a');
      {$ELSE}
      Width := SD.cxChar * 6;
      {$ENDIF}
    end;
    Inc(Col);
  end;
  HaveColumns := True;
  Update;
end;

procedure TStCustomShellListView.GetItemDetails(SI : TStShellItem);
var
  Details : IShellDetails;
  SD      : TStShellDetails;
  Col     : Integer;
  ColText : string;
  S       : string;
  PBuff   : PChar;
  Size    : Integer;
  FD      : TWin32FindData;
  FT      : TFileTime;
  Res     : DWORD;
  SR      : TSearchRec;
  ST      : TSystemTime;
  Str     : Pointer;

  function GetDetails : Boolean;
  var
    I : Integer;
  begin
    SI.ParentFolder.QueryInterface(IID_IShellDetails, Pointer(Details));
    if not Assigned(Details) then
      SI.ParentFolder.CreateViewObject(
        0, IID_IShellDetails, Pointer(Details));
    if not Assigned(Details) then begin
      Result := False;
      Exit;
    end else
      Result := True;
    Col := 0;
    { Don't want the first column, but have to get   }
    { it anyway for the rest of the columns to work. }
    for I := 0 to Pred(Columns.Count) do begin
      Details.GetDetailsOf(SI.FSimplePidl, I, SD);
      if I = 0 then
        Continue;
      case SD.str.uType of
        STRRET_CSTR : ColText := string(SD.str.cStr);
        STRRET_WSTR : begin
                        ColText := WideCharToString(SD.str.pOleStr);
                        CoTaskMemFree(SD.str.pOleStr); // SZ: pOleStr must be freed
                       end;
        STRRET_OFFSET :
          begin
            PBuff := PChar(PAnsiChar(SI.FSimplePidl) + SD.str.uOffset);
            ColText := PBuff;
          end;
      end;
      if SI.IsFile and (ColText = '') and
         (Columns.Items[I].Caption = _(LNGTypeCol)) then begin
        S := ExtractFileExt(SI.Path);
        if TypeNames.Exists(S, Str) then
          ColText := PChar(Str);
        if S = '' then
          ColText := _(LNGFile)
        else if ColText = '' then
          ColText := AnsiUpperCase(Copy(S, 2, Length(S) - 1)) + ' ' + _(LNGFile);
      end;
      SI.ColText.Add(ColText);
      { It's a file system object so get the details. }
      if Columns.Items[I].Caption = _(LNGSizeCol) then begin
        { Size }
        SHGetDataFromIDList(SI.ParentFolder, SI.FSimplePidl,
          SHGDFIL_FINDDATA, @FD, SizeOf(TWin32FindData));
        SI.FSize := FD.nFileSizeLow or Int64(FD.nFileSizeHigh) shl 32;
      end else if Columns.Items[I].Caption = _(LNGModifiedCol) then begin
        FileTimeToLocalFileTime(FD.ftLastWriteTime, FT);
        FileTimeToDosDateTime(FT,
          LongRec(SI.DOSDate).Hi, LongRec(SI.DOSDate).Lo);
        if SI.DOSDate <> 0 then
          SI.FDate := FileDateToDateTime(SI.DOSDate)
        else
          SI.FDate := EncodeDate(1980, 1, 1);
      end;
    end;
  end;

begin
  if (csDesigning in ComponentState) then                              
    Exit;                                                              
  SI.ColText.Clear;
  if not GetDetails then begin
    { SHGetDataFromIDList is faster than FindFirst and works    }
    { 90% of the time. In those cases where SHGetDataFromIDList }
    { fails we'll have to use FindFirst. }
    Res := SHGetDataFromIDList(SI.ParentFolder, SI.FSimplePidl,
      SHGDFIL_FINDDATA, @FD, SizeOf(TWin32FindData));
    if Res = 0 then begin
      SI.FSize := FD.nFileSizeLow or Int64(FD.nFileSizeHigh) shl 32;
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
        SI.FIsFileFolder := True;
        SI.ColText.Add('');
        SI.ColText.Add(_('File folder'));
        SI.FSize := -1;
      end else begin
        { File size }
        SI.FSize := FD.nFileSizeLow or Int64(FD.nFileSizeHigh) shl 32;
        Size := SI.FSize div 1024;
        if SI.FSize <> 0 then
          if Size = 0 then
            Size := 1;
        SI.ColText.Add(CommaizeL(Size) + 'KB');
        { File type }
        if (SI.TypeName = '') and SI.IsFile then begin
          S := ExtractFileExt(SI.Path);
          if TypeNames.Exists(S, Str) then
            ColText := PChar(Str);
          if S = '' then
            ColText := _(LNGFile)
          else if ColText = '' then
            ColText := AnsiUpperCase(Copy(S, 2, Length(S) - 1)) + ' ' + _(LNGFile);
        end else
          ColText := SI.TypeName;
        SI.ColText.Add(ColText);
      end;
      { Modified }
      FileTimeToLocalFileTime(FD.ftLastWriteTime, FT);
      FileTimeToDosDateTime(FT,
        LongRec(SI.DOSDate).Hi, LongRec(SI.DOSDate).Lo);
      if SI.DOSDate <> 0 then
        SI.FDate := FileDateToDateTime(SI.DOSDate)
      else begin
        FileTimeToSystemTime(FT, ST);
        SI.FDate := EncodeDate(ST.wYear, ST.wMonth, ST.wDay) +
          EncodeTime(ST.wHour, ST.wMinute, ST.wSecond, 0);
      end;
      {$IFDEF VERSIONXE}
      SI.ColText.Add(FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat, SI.FDate));
      {$ELSE}
      S := LongTimeFormat;
      LongTimeFormat := ShortTimeFormat;
      SI.ColText.Add(DateTimetoStr(SI.FDate));
      LongTimeFormat := S;
      {$ENDIF}
      { Attributes }
      S := '';
      SI.FFileAttributes := FD.dwFileAttributes;
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN then
        S := S + 'H';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM then
        S := S + 'S';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY then
        S := S + 'R';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED) = FILE_ATTRIBUTE_COMPRESSED then
        S := S + 'C';
      if (FD.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE then
        S := S + 'A';
      SI.ColText.Add(S);
    end else if FindFirst(SI.Path, faAnyFile, SR) = 0 then begin
      SysUtils.FindClose(SR);
      { File size }
      SI.FSize := SR.Size;
      Size := SR.Size div 1024;
      if SI.FSize <> 0 then
        if Size = 0 then
          Size := 1;
      SI.ColText.Add(CommaizeL(Size) + 'KB');
      { File type }
      if SI.TypeName = '' then begin
        S := ExtractFileExt(SI.Path);
        if S = '' then
          ColText := _('LNGFile')
        else if AnsiUpperCase(S) = '.LNK' then
          ColText := 'Shortcut'
        else
          ColText := AnsiUpperCase(Copy(S, 2, Length(S) - 1)) + ' ' + _('LNGFile');
      end else
        ColText := SI.TypeName;
      SI.ColText.Add(ColText);
      { Modified }
      //SI.DOSDate := SR.Time;
      {$IFDEF VERSIONXE}
      SI.FDate := SR.TimeStamp;
      {$ELSE}
      SI.FDate := FileDateToDateTime(SR.Time);
      {$ENDIF}

      {$IFDEF VERSIONXE}
      SI.ColText.Add(FormatDateTime(FormatSettings.ShortDateFormat + ' ' + FormatSettings.ShortTimeFormat, SI.FDate));
      {$ELSE}
      S := LongTimeFormat;
      LongTimeFormat := ShortTimeFormat;
      SI.ColText.Add(DateTimetoStr(SI.FDate));
      LongTimeFormat := S;
      {$ENDIF}
      { Attributes }
      S := '';
      {$IFDEF VERSION6} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
      SI.FFileAttributes := SR.FindData.dwFileAttributes;
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_HIDDEN) = FILE_ATTRIBUTE_HIDDEN then
        S := S + 'H';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_SYSTEM) = FILE_ATTRIBUTE_SYSTEM then
        S := S + 'S';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY) = FILE_ATTRIBUTE_READONLY then
        S := S + 'R';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_COMPRESSED) = FILE_ATTRIBUTE_COMPRESSED then
        S := S + 'C';
      if (SR.FindData.dwFileAttributes and FILE_ATTRIBUTE_ARCHIVE) = FILE_ATTRIBUTE_ARCHIVE then
        S := S + 'A';
      {$IFDEF VERSION6} {$WARN SYMBOL_PLATFORM ON} {$ENDIF}
      SI.ColText.Add(S);
    end else begin
      SI.ColText.Add('');
      SI.ColText.Add(_('System folder'));
      SI.FSize := -1;
    end;
    SI.HaveDetails := True;
  end;
end;

procedure TStCustomShellListView.CNNotify(var Message : TWMNotify);
var
  NMHdr        : TNMHdr;
  NMList       : TNMListView;
  SI, SI1      : TStShellItem;
  DropSource   : IDropSource;
  PidlArray    : PStPidlArray;
  {$IFDEF VERSION4}
  Attr         : Cardinal;
  {$ELSE}
  Attr         : DWORD;
  {$ENDIF}
  DropEffect   : Integer;
  Item         : TListItem;
  HDragImgList : HIMAGELIST;
  Pidl         : PItemIDList;
  Count, I     : Integer;
  BM           : TBitmap;
  PageIcon     : TIcon;
  S            : string;
  FileCount    : Integer;
  FolderCount  : Integer;
  R            : TRect;
  {$IFDEF VERSION2009}
  Shell32Inst:  THandle;
  {$ENDIF}
begin
  NMHdr := Message.NMHdr^;
  case NMHdr.code of
    LVN_BEGINDRAG :
      if (loAllowDrag in FOptions) then begin
        Controller.DragSource := Self;                                 
        NMList := PNMListView(Message.NMHdr)^;
        SI := ShellItems[Integer(Selected.Data)];
        if SelCount > 1 then begin
          GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
          DropEffect := 0;
          Item := Selected;
          PidlArray^[0] := ILClone(SI.FSimplePidl);
          FileCount := 0;
          FolderCount := 0;
          if SI.IsFile then
            Inc(FileCount)
          else if SI.IsFileFolder then
            Inc(FolderCount);
          for I := 1 to Pred(SelCount) do begin
            Item := GetNextItem(Item, sdAll, [isSelected]);
            SI1 := ShellItems[Integer(Item.Data)];
            SI1.ParentFolder.GetAttributesOf(1, SI1.FSimplePidl, Attr);
            DropEffect := DropEffect or Integer(Attr);
            PidlArray^[I] := ILClone(SI1.FSimplePidl);
            if SI1.IsFile then
              Inc(FileCount)
            else if SI1.IsFileFolder then
              Inc(FolderCount);
          end;
          BM := TBitmap.Create;
          BM.Canvas.Font.Assign(Font);
          if FileCount <> 0 then
            S := IntToStr(FileCount) + ssscItemDisplayFile;
          if FileCount > 1 then
            S := IntToStr(FileCount) + ssscItemDisplayFiles;

          if FolderCount <> 0 then begin
            if FileCount <> 0 then
              S := S + ', ';
            S := S + IntToStr(FolderCount) + ssscItemDisplayFolder;
          end;
          if FolderCount > 1 then
            S := S + IntToStr(FolderCount) + ssscItemDisplayFolders;

          if (FileCount = 0) and (FolderCount = 0) then
            S := IntToStr(SelCount) + ssscItemDispalyObjects;
          if ViewStyle = vsIcon then
            BM.Height := GetSystemMetrics(SM_CYICON)
          else
            BM.Height := GetSystemMetrics(SM_CYSMICON);
          BM.Width := BM.Height + 4 + BM.Canvas.TextWidth(S);
          {$IFDEF VERSION2009}
          Shell32Inst := GetModuleHandle('shell32.dll');
          Assert(Shell32Inst <> 0); // this should never happen because we use ILClone
          {$ENDIF}
          PageIcon := TIcon.Create;
          PageIcon.Handle := LoadImage(Shell32Inst,
            MAKEINTRESOURCE(133), IMAGE_ICON, BM.Height, BM.Height, 0);
          BM.Canvas.Brush.Color := clFuchsia;
          BM.Canvas.Pen.Color := clFuchsia;
          BM.Canvas.Rectangle(0, 0, BM.Width, BM.Height);
          BM.Canvas.Brush.Style := bsClear;
          BM.Canvas.Draw(0, 0, PageIcon);
          R := Rect(BM.Height, 0, BM.Width, BM.Height);
          DrawText(BM.Canvas.Handle, PChar(S), -1, R,
            DT_SINGLELINE or DT_CENTER or DT_VCENTER);
          PageIcon.Free;
          Count := SelCount;
        end else begin
          BM := nil;
          Item := Items[NMList.iItem];
          SI := ShellItems[Integer(Item.Data)];
          DropEffect := 0;
          SI.ParentFolder.GetAttributesOf(1, SI.FSimplePidl, Attr);
          DropEffect := Attr;
          Pidl := ILClone(SI.FSimplePidl);
          GetMem(PidlArray, SizeOf(PItemIDList));
          PidlArray[0] := Pidl;
          Count := 1;
        end;
        DataObject := TStDataObject.Create(PidlArray^, SI.FPidl, Count);
        DropSource := TStDropSource.Create;
        HDragImgList := ListView_CreateDragImage(
          Handle, NMList.iItem, Point(0, 0));
        if Assigned(BM) then begin
          ImageList_Remove(HDragImgList, -1);
          ImageList_SetIconSize(HDragImgList, BM.Width, BM.Height);
          ImageList_AddMasked(HDragImgList, BM.Handle, $00FF00FF);
          BM.Free;
        end;
        ImageList_BeginDrag(HDragImgList, 0, 0, 0);
        if Assigned(DropTargetItem) then begin
          DropTargetItem.Free;
          DropTargetItem := nil;
        end;
        DoDragDrop(DataObject, DropSource, DropEffect, DropEffect);
        { Fix for problem where dragging and dropping between two   }
        { TStShellListViews breaks after first drag/drop operation. }
        Controller.DragSource := nil;
        ImageList_EndDrag();
        ImageList_Destroy(HDragImgList);
        FreeMem(PidlArray);
        DataObject := nil;
      end;
{!!.02 - Added }
    LVN_ITEMCHANGED: begin
      with PNMListView(Message.NMHdr)^ do begin

        SelectedItems.Clear;
        for i := 0 to Pred(Items.Count) do
          if Items[i].Selected then begin
            SI := TStShellItem.Create(Controller);
            SI.Assign(ShellItems[Integer(Items[i].Data)]);
            SelectedItems.FList.Add(SI);
          end;

          Item := Items[iItem];
          if Assigned(Item) and Item.Selected then
            if Assigned(OnItemSelected) then
              DoItemSelected(ShellItems[Integer(Items[iItem].Data)]);
      end; { with }
    end; { LVN_ITEMCHANGED }
{!!.02 - Added end }

  end; { case }
  if (csDesigning in ComponentState) and (NMHdr.code = NM_CUSTOMDRAW) then
    Exit
  else
    inherited;
end;

procedure TStCustomShellListView.WndProc(var Message : TMessage);
{$IFDEF VERSION5}
var
  Res       : Integer;                                                 
  SI        : TStShellItem;
  SI1       : TStShellItem;
  I         : Integer;
  PidlArray : PStPidlArray;
  Item      : TListItem;
{$ENDIF}
begin
  with Message do begin
    {$IFNDEF VERSION5}
    if Msg = MSG_RESTOREPOPUP then
      PopupMenu := Popup;
    {$ELSE}
    if (Msg = WM_CONTEXTMENU) and (loShellMenu in FOptions)  then begin
      if Assigned(Selected) then begin
        SI := ShellItems[Integer(Selected.Data)];
        if SelCount > 1 then begin
          GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
          Item := Selected;
          PidlArray^[0] := SI.FSimplePidl;
          for I := 1 to Pred(SelCount) do begin
            Item := GetNextItem(Item, sdAll, [isSelected]);
            SI1 := ShellItems[Integer(Item.Data)];
            PidlArray^[I] := SI1.FSimplePidl;
          end;
          Res := Controller.ExecutePopup(Self, SI.ParentFolder,
            PidlArray^[0], SelCount, LoWord(lParam), HiWord(lParam), Handle);
          FreeMem(PidlArray);
        end else
          Res := Controller.ExecutePopup(Self, SI.ParentFolder,
            SI.FSimplePidl, 1, LoWord(lParam), HiWord(lParam), Handle);
        { "Rename" item on context menu has ID of 19. }
        if Res = 19 then                                               
          Selected.EditCaption;                                        
        Exit;
      end;
    end;
    {$ENDIF}
  end;
  inherited WndProc(Message);
end;

function TStCustomShellListView.DragEnter(const dataObj : IDataObject;
  grfKeyState : Integer; pt : TPoint; var dwEffect : Integer): HResult;
var
  FE       : TFormatEtc;
  ClientPt : TPoint;
begin
  DataObject := nil;
  FE.cfFormat := CF_HDROP;
  FE.dwAspect := DVASPECT_CONTENT;
  FE.ptd := nil;
  FE.tymed := TYMED_HGLOBAL;
  FE.lindex := -1;
  if (dataObj.QueryGetData(FE) <> NOERROR) then begin
    dwEffect := DROPEFFECT_NONE;
    Result := NOERROR;
  end else begin
    DataObject := dataObj;
    Result := NOERROR;
  end;
  ImageList_GetDragImage(nil, nil);
  ClientPt := ScreenToClient(pt);
  ImageList_DragEnter(Handle, ClientPt.x, ClientPt.y);
end;

function TStCustomShellListView.DragLeave : HResult;
var
  R     : TRect;
  I     : Integer;
  Index : Integer;
begin
  if Assigned(DataObject) then
    DataObject := nil;
  ImageList_DragLeave(Handle);
  if Assigned(DropTargetItem) then begin
    { Find the item. }
    Index := -1;
    for I := 0 to Pred(Items.Count) do
      if Items[I].Caption = DropTargetItem.DisplayName then begin
        Index := I;
        Break;
      end;
    if Index <> -1 then begin
      Items[Index].DropTarget := False;
      R := Items[Index].DisplayRect(drBounds);
      InvalidateRect(Handle, @R, False);
      UpdateWindow(Handle);
    end;
    DropTargetItem.Free;
    DropTargetItem := nil;
  end;
  Result := NOERROR;
end;

function TStCustomShellListView.StDragOver(grfKeyState : Integer; pt : TPoint;
  var dwEffect : Integer): HResult;
var
  ClientPt    : TPoint;
  SI          : TStShellItem;
  Item        : TListItem;
  DropTarget  : IDropTarget;
  OldRect     : TRect;
  NewRect     : TRect;
  ItemChanged : Boolean;
  NewItem     : TListItem;
  DoScroll    : Boolean;
  Delay       : Integer;
begin
  Result := S_OK;
  OldRect := Rect(0, 0, 0, 0);
  if not Assigned(DataObject) then begin
    dwEffect := DROPEFFECT_NONE;
    Result := NOERROR;
  end else begin
    ClientPt := ScreenToClient(pt);
    Item := GetItemAt(ClientPt.X, ClientPt.Y);
    DropTarget := nil;
    NewItem := nil;
    if Assigned(Item) then begin
      { See if we need to scroll. }
      DoScroll := False;
      ClientPt := ScreenToClient(pt);
      Delay := 0;
      case ViewStyle of
        vsReport,
        vsSmallIcon :
          begin
            if (ClientPt.Y < 30) then begin
              NewItem := GetNextItem(Item, sdAbove, [isNone]);
              if Assigned(NewItem) then begin
                Delay := 30;
                DoScroll := True;
              end;
            end;
            if (ClientPt.Y > Height - 30) then begin
              NewItem := GetNextItem(Item, sdBelow, [isNone]);
              if Assigned(NewItem) then begin
                Delay := 30;
                DoScroll := True;
              end;
            end;
          end;
        vsIcon :
          begin
            if (ClientPt.Y < 30) then begin
              NewItem := GetNextItem(Item, sdAbove, [isNone]);
              if Assigned(NewItem) then begin
                Delay := 100;
                DoScroll := True;
              end;
            end;
            if (ClientPt.Y > Height - 30) then begin
              NewItem := GetNextItem(Item, sdBelow, [isNone]);
              if not Assigned(NewItem) then
                NewItem := GetNextItem(Item, sdAll, [isNone]);
              if Assigned(NewItem) then begin
                DoScroll := True;
                Delay := 100;
              end;
            end;
          end;
        vsList :
          begin
            if (ClientPt.X < 10) then begin
              NewItem := GetNextItem(Item, sdLeft, [isNone]);
              if Assigned(NewItem) then begin
                DoScroll := True;
                Delay := 400;
              end;
            end;
            if (ClientPt.X > Width - 30) then begin
              NewItem := GetNextItem(Item, sdRight, [isNone]);
              if not Assigned(NewItem) then
                NewItem := GetNextItem(Item, sdAll, [isNone]);
              if Assigned(NewItem) then begin
                DoScroll := True;
                Delay := 400;
              end;
            end;
          end;
      end;
      if Assigned(NewItem) and DoScroll then begin
        ImageList_DragShowNoLock(False);
        NewItem.MakeVisible(False);
        ImageList_DragShowNoLock(True);
        Sleep(Delay);
      end;
      SI := ShellItems[Integer(Item.Data)];
      if Assigned(SI.ParentFolder) then
        SI.ParentFolder.GetUIObjectOf(Handle, 1,
          SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
      else
        Controller.DesktopFolder.GetUIObjectOf(Handle, 1,
          SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
    end else begin
      { If the list view is the drag source then we don't allow }
      { a drop on the folder itself. }
      if Controller.DragSource = Self then begin                       
        ImageList_DragMove(ClientPt.x, ClientPt.y);                    
        dwEffect := DROPEFFECT_NONE;                                   
        Exit;                                                          
      end;                                                             
      { No item is under the cursor so we need to get the drop }
      { target for the parent folder. }
      if Assigned(Folder.ParentFolder) then
        Folder.ParentFolder.GetUIObjectOf(Handle, 1,
          Folder.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
      else
        Controller.DesktopFolder.GetUIObjectOf(Handle, 1,
          Folder.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
    end;
    if Assigned(DropTarget) then begin
      DropTarget.DragEnter(DataObject, grfKeyState, pt, dwEffect);
      Result := DropTarget.DragOver(grfKeyState, pt, dwEffect);
      DropTarget := nil;
    end;
    if Assigned(Item) then begin
      SI := ShellItems[Integer(Item.Data)];
      ItemChanged := True;
      if Assigned(DropTargetItem) then
        ItemChanged := (not ILIsEqual(SI.FPidl, DropTargetItem.FPidl));
      if ItemChanged then begin
        if Assigned(DropTargetItem) then
          DropTargetItem.Free;
        DropTargetItem := TStShellItem.Create(Controller);
        DropTargetItem.Assign(ShellItems[Integer(Item.Data)]);
        Item.DropTarget := True;
        ImageList_DragMove(ClientPt.x, ClientPt.y);
        ImageList_DragShowNoLock(False);
        NewRect := Item.DisplayRect(drSelectBounds);
        if (ViewStyle = vsReport) then begin
          { Some video drivers don't properly repaint the icon after }
          { scroll highlighting, so expand the update rect a bit.    }
          Dec(NewRect.Top, 40);
          Inc(NewRect.Bottom, 40);
        end;
        InvalidateRect(Handle, @NewRect, False);
        UpdateWindow(Handle);
        ImageList_DragShowNoLock(True);
        Exit;
      end;
    end;
  end;
  ImageList_DragMove(ClientPt.x, ClientPt.y);
end;

function TStCustomShellListView.Drop(const dataObj : IDataObject;
  grfKeyState : Integer; pt : TPoint; var dwEffect : Integer): HResult;
var
  Item       : TListItem;
  ClientPt   : TPoint;
  DropTarget : IDropTarget;
  SI         : TStShellItem;
  I, Index   : Integer;
  R          : TRect;
begin
  Result := S_OK;
  if not Assigned(DataObject) then begin
    Result := E_FAIL;
    Exit;
  end;
  DragLeave();
  grfKeyState := grfKeyState or MK_LBUTTON;
  if (grfKeyState and MK_CONTROL) <> 0 then
    dwEffect := DROPEFFECT_COPY;
  ClientPt := ScreenToClient(pt);
  Item := GetItemAt(ClientPt.X, ClientPt.Y);
  if Assigned(Item) then begin
    SI := ShellItems[Integer(Item.Data)];
    if Assigned(SI.ParentFolder) then
      SI.ParentFolder.GetUIObjectOf(Handle, 1,
        SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
    else
      Controller.DesktopFolder.GetUIObjectOf(Handle, 1,
        SI.FSimplePidl, IDropTarget, nil, Pointer(DropTarget));
  end else begin
    if Assigned(Folder.ParentFolder) then
      Folder.ParentFolder.GetUIObjectOf(Handle, 1,
        Folder.FSimplePidl, IDropTarget, nil, Pointer(DropTarget))
    else
      Controller.DesktopFolder.GetUIObjectOf(Handle, 1,
        Folder.FSimplePidl, IDropTarget, nil, Pointer(DropTarget));
  end;
  if Assigned(DropTarget) then begin
    DropTarget.DragEnter(dataObj, grfKeyState, pt, dwEffect);
    Result := DropTarget.Drop(dataObj, grfKeyState, pt, dwEffect);
    DropTarget := nil;
  end;
  if Assigned(DropTargetItem) then begin
    { Find the item. }
    Index := -1;
    for I := 0 to Pred(Items.Count) do
      if Items[I].Caption = DropTargetItem.DisplayName then begin
        Index := I;
        Break;
      end;
    if Index <> -1 then begin
      Items[Index].DropTarget := False;
      R := Items[Index].DisplayRect(drBounds);
      InvalidateRect(Handle, @R, False);
      UpdateWindow(Handle);
    end;
    DropTargetItem.Free;
    DropTargetItem := nil;
  end;
end;

{$IFNDEF VERSION5}
procedure TStCustomShellListView.MouseDown(
  Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
var
  Res       : Integer;                                                 
  SI        : TStShellItem;
  SI1       : TStShellItem;
  I         : Integer;
  PidlArray : PStPidlArray;
  Item      : TListItem;
  Pt        : TPoint;
begin
  if (Button = mbRight) and (Selected <> nil)
      and (loShellMenu in FOptions) then begin                         
    Popup := PopupMenu;
    PopupMenu := nil;
    Pt.x := X;
    Pt.y := Y;
    Pt := ClientToScreen(Pt);
    SI := ShellItems[Integer(Selected.Data)];
    if SelCount > 1 then begin
      GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
      Item := Selected;
      PidlArray^[0] := SI.FSimplePidl;
      for I := 1 to Pred(SelCount) do begin
        Item := GetNextItem(Item, sdAll, [isSelected]);
        SI1 := ShellItems[Integer(Item.Data)];
        PidlArray^[I] := SI1.FSimplePidl;
      end;
      Res := Controller.ExecutePopup(Self, SI.ParentFolder,
        PidlArray^[0], SelCount, Pt.X, Pt.Y, Handle);
      FreeMem(PidlArray);
    end else
      Res := Controller.ExecutePopup(Self, SI.ParentFolder,
        SI.FSimplePidl, 1, Pt.X, Pt.Y, Handle);
    PostMessage(Handle, MSG_RESTOREPOPUP, 0, 0);
    { "Rename" item on context menu has ID of 19. }
    if Res = 19 then
      Selected.EditCaption;
  end else
    inherited;
end;
{$ENDIF}

procedure TStCustomShellListView.ColClick(Column : TListColumn);
var
  TopIndex  : Integer;
  R         : TRect;
  I         : Integer;
  J         : Integer;
  SI        : TStShellItem;
  {$IFNDEF VERSION4}
  Index     : Integer;
  Item      : TListItem;
  {$ENDIF}
  FocusItemPidl : PItemIDList;                                             
  FocusItemStr  : string;
begin
  { Save the focused item so we can focus it later. }                  
  if Assigned(ItemFocused) then begin
    SI := ShellItems[Integer(ItemFocused.Data)];                       
    if SI.IsFileSystem then                                            
      FocusItemStr := SI.Path;                                         
    FocusItemPidl := SI.Pidl;                                          
  end else                                                             
    FocusItemPidl := nil;                                              
  inherited;
  if ClickedColumn = Column.Index then begin
    Ascending := not Ascending;
    { We don't resort the list when the sorting direction }
    { changes. Instead, we just reference the list in     }
    { reverse order. All we have to do here is refresh.   }
    {$IFDEF VERSION4}
    R := ClientRect;
    InvalidateRect(Handle, @R, False);
    Update;
    { Restore the selected items. It's faster to compare by string }
    { than by pidl so we'll do it that way when we can. }
    if SelectedItems.Count > 0 then begin                              
      for I := 0 to Pred(Items.Count) do begin                         
        if Assigned(FocusItemPidl) then begin
          if FocusItemStr <> '' then begin                             
            if FocusItemStr = ShellItems[Integer(Items[I].Data)].Path then 
              ItemFocused := Items[I];                                 
          end else                                                     
            if ILIsEqual(                                              
              ShellItems[Integer(Items[I].Data)].Pidl, FocusItemPidl) then 
              ItemFocused := Items[I];
        end;                                                           
        Items[I].Selected := False;                                    
        for J := 0 to Pred(SelectedItems.Count) do                     
          if SelectedItems[J].IsFileSystem then begin                  
            if SelectedItems[J].Path =                                 
                 ShellItems[Integer(Items[I].Data)].Path then begin    
              Items[I].Selected := True;                               
              Break;                                                   
            end;                                                       
          end else                                                     
            if ILIsEqual(ShellItems[Integer(Items[I].Data)].Pidl,      
                 SelectedItems[J].Pidl) then begin                     
              Items[I].Selected := True;                               
              Break;                                                   
          end;                                                         
      end;                                                             
    end;                                                               
    R := ClientRect;                                                   
    InvalidateRect(Handle, @R, False);                                 
    Update;                                                            
    Exit;
    {$ENDIF}
  end else
    Ascending := True;
  ClickedColumn := Column.Index;
  TopIndex := TopItem.Index;

  SortItems;
  {$IFNDEF VERSION4}
  Perform(WM_SETREDRAW, 0, 0);
  Items.Clear;
  for I := 0 to Pred(ShellItems.Count) do begin
    if Ascending then
      Index := I
    else
      Index := Pred(ShellItems.Count) - I;
    SI := ShellItems[Index];
    Item := Items.Add;
    Item.Caption := SI.DisplayName;
    Item.ImageIndex := SI.IconIndex;
    Item.OverlayIndex := SI.OverlayIconIndex;
    Item.Selected := False;                                            
    { Restore the selected items. }                                    
    if SelectedItems.Count > 0 then begin                              
      if FocusItemPidl <> nil then begin                               
        if FocusItemStr <> '' then begin                               
          if FocusItemStr = SI.Path then                               
            Item.Focused := True;                                      
        end else if ILIsEqual(SI.Pidl, FocusItemPidl) then             
          Item.Focused := True;                                        
      end;                                                             
      for J := 0 to Pred(SelectedItems.Count) do                       
        if SelectedItems[J].IsFileSystem then begin                    
          if SelectedItems[J].Path = SI.Path then begin                
            Item.Selected := True;                                     
            Break;                                                     
          end;                                                         
        end else if ILIsEqual(SI.Pidl, SelectedItems[J].Pidl) then begin 
          Item.Selected := True;                                       
          Break;                                                       
        end;                                                           
    end;                                                               
    GetItemDetails(SI);
    HaveDetails := True;
    Item.Data := Pointer(Index);
    for J := 0 to Pred(SI.ColText.Count) do
      Item.SubItems.Add(SI.ColText[J]);
  end;
  Perform(WM_SETREDRAW, 1, 0);
  {$ENDIF}
  R := ClientRect;
  InvalidateRect(Handle, @R, False);
  Update;
  {$IFDEF VERSION4}                                                    
  { Restore the selected items. It's faster to compare by string }
  { than by pidl so we'll do it that way when we can. }
  if SelectedItems.Count >0 then begin                                 
    ItemFocused := nil;                                                
    for I := 0 to Pred(Items.Count) do begin                           
      if Assigned(FocusItemPidl) then begin
        if FocusItemStr <> '' then begin                               
          if FocusItemStr = ShellItems[Integer(Items[I].Data)].Path then 
            ItemFocused := Items[I];                                   
        end else                                                       
          if ILIsEqual(                                                
            ShellItems[Integer(Items[I].Data)].Pidl, FocusItemPidl) then 
            ItemFocused := Items[I];                                   
      end;                                                             
      Items[I].Selected := False;                                      
      for J := 0 to Pred(SelectedItems.Count) do                       
        if SelectedItems[J].IsFileSystem then begin                    
          if SelectedItems[J].Path =                                   
               ShellItems[Integer(Items[I].Data)].Path then begin      
            Items[I].Selected := True;                                 
            Break;                                                     
          end;                                                         
        end else                                                       
          if ILIsEqual(ShellItems[Integer(Items[I].Data)].Pidl,        
               SelectedItems[J].Pidl) then begin                       
            Items[I].Selected := True;                                 
            Break;                                                     
        end;                                                           
    end;                                                               
  end;                                                                 
  {$ENDIF}                                                             
  ListView_EnsureVisible(Handle, TopIndex, True);
  R := ClientRect;                                                     
  InvalidateRect(Handle, @R, False);                                   
  Update;                                                              
end;

{$IFDEF VERSION4}
function TStCustomShellListView.OwnerDataFetch(Item : TListItem;
  Request : TItemRequest): Boolean;
var
  SI    : TStShellItem;
  Index : Integer;
  I     : Integer;
  Temp  : string;                                                      
begin
  inherited OwnerDataFetch(Item, Request);
  Result := False;
  if Item.Index < ShellItems.Count then begin
    { If we are sorting in ascending order then we reference }
    { the list as-is. If we are sorting in descending order  }
    { then we reference the list starting from the end.      }
    if Ascending then
      Index := Item.Index
    else
      Index := Pred(ShellItems.Count) - Item.Index;
    SI := ShellItems[Index];
    Item.Caption := SI.DisplayName;
    { If we're optimizing for speed then we don't get the  }
    { item details until painting is required. If we don't }
    { yet have the details then IconIndex will be -1. Once }
    { we have the details we don't need to get them again. }
    if (Optimization = otEnumerate) and (SI.FIconIndex = -1) then
      Controller.GetItemInfo(SI.Pidl, SI.FIconIndex, SI.FOpenIconIndex,
        SI.FOverlayIconIndex, Temp);                                   
    if (ViewStyle = vsReport) and (SI.ColText.Count <> 0) then
      for I :=0 to Pred(SI.ColText.Count) do
        Item.SubItems.Add(SI.ColText[i]);
    Item.ImageIndex := SI.IconIndex;
    Item.OverlayIndex := SI.OverlayIconIndex;
    Item.Data := Pointer(Index);
    Item.Cut := SI.IsGhosted;
    Result := True;
  end;
end;

function TStCustomShellListView.OwnerDataFind(Find : TItemFind;
  const FindString : string; const FindPosition : TPoint; FindData : Pointer;
  StartIndex : Integer; Direction : TSearchDirection;
  Wrap : Boolean): Integer;
var
  I     : Integer;
  Index : Integer;
begin
  inherited OwnerDataFind(Find, FindString, FindPosition,
    FindData, StartIndex, Direction, Wrap);
  Index := -1;
  for I := StartIndex to Pred(ShellItems.Count) do begin               
    if Find = ifData then begin                                        
      if ShellItems[I].Pidl = FindData then begin
        Index := I;
        Break;
      end
    end else if Find = ifPartialString then begin                      
      if Pos(FindString, ShellItems[I].DisplayName) <> 0 then begin
        Index := I;
        Break;
      end
    end else if Find = ifExactString then begin                        
      if ShellItems[I].DisplayName = FindString then begin
        Index := I;
        Break;
      end;
    end;                                                               
  end;
  Result := Index;
end;

function TStCustomShellListView.OwnerDataHint(StartIndex,
  EndIndex : Integer): Boolean;
begin
  Result := inherited OwnerDataHint(StartIndex, EndIndex);
end;
{$ENDIF}

{$IFDEF VERSION6}
procedure TStCustomShellListView.SetViewStyle(Value : TViewStyle);
{$ELSE}
procedure TStCustomShellListView.SetViewStyle(const Value : TViewStyle);
{$ENDIF}
var
  I : Integer;
begin
{$IFDEF VERSION6}
  inherited SetViewStyle(Value);
{$ELSE}
  inherited ViewStyle := Value;
{$ENDIF}
  FViewStyle := Value;
  if not (csDesigning in ComponentState) and Assigned(Controller) then begin
    if FViewStyle <> vsReport then
      Ascending := True;
    Perform(WM_SETREDRAW, 0, 0);
    if (FViewStyle = vsReport) and (ShellItems.Count > 0) then begin
      if not HaveColumns then
        GetColHeader(FFolder, Columns);
      if not HaveDetails then begin
        for I := 0 to Pred(ShellItems.Count) do
          GetItemDetails(ShellItems[I]);
        HaveDetails := True;
      end;
      {$IFDEF VERSION3}
      Ascending := False;
      ColClick(Column[0]);
      {$ENDIF}
    end;
    Perform(WM_SETREDRAW, 1, 0);
  end;
end;

procedure TStCustomShellListView.Click;
var
  I : Integer;
  R : TRect;
  SI : TStShellItem;
begin
  inherited;
  if Assigned(Selected) then begin
    SelectedItems.Clear;                                               
    for I := 0 to Pred(Items.Count) do                                 
      if Items[I].Selected then begin                                  
        SI := TStShellItem.Create(Controller);
        SI.Assign(ShellItems[Integer(Items[I].Data)]);
        SelectedItems.FList.Add(SI);
        R := Items[I].DisplayRect(drBounds);
        InvalidateRect(Handle, @R, False);
        Update;
      end;
    if Assigned(OnItemSelected) then
      DoItemSelected(ShellItems[Integer(Selected.Data)]);
    if HotTrack then
      DblClick;
  end;
end;

procedure TStCustomShellListView.FillList(SI : TStShellFolder);
var
  Pidl    : PItemIDList;
  OwnItem : Boolean;
  {$IFNDEF VERSION4}
  Item    : TListItem;
  SI1     : TStShellItem;
  I, J    : Integer;
  {$ENDIF}
begin
  if Assigned(FTreeView) then
    if not FTreeView.StartFolderSet then
      Exit;
  if SelectedItems.Count > 0 then
    SelectedItems.Clear;
  OwnItem := False;
  HaveDetails := False;
  HaveColumns := False;
  Clear;
  { SI will be valid if this function is being called as a }
  { result of the associated tree view selection changing. }
  { It will be nil if the list view is being used alone.   }
  if not Assigned(SI) then begin
    if not Assigned(Controller) or not HandleAllocated then
      Exit;
    if (FSpecialRootFolder = sfNone) and (FRootFolder = '') then
      Exit;

    if FSpecialRootFolder <> sfNone then begin
      Screen.Cursor := crHourGlass;
      SHGetSpecialFolderLocation(Handle, ShellFolders[FSpecialRootFolder], Pidl);
      Controller.Enumerate(Pidl, etList);
      SI := TStShellFolder.CreateFromPidl(ILClone(Pidl), Controller);
      OwnItem := True;
    end else if FRootFolder <> '' then begin
      { Starting with a folder name rather than with a
      { special folder. }
      Screen.Cursor := crHourGlass;
      try
        PidlFromPath(FRootFolder, Controller.DesktopFolder, Pidl);
        if not Assigned(Pidl) then begin
          RootFolder := '';
          RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
        end;
        SI := TStShellFolder.CreateFromPidl(ILClone(Pidl), Controller);
        OwnItem := True;
        Controller.Enumerate(Pidl, etList);
      finally
        Screen.Cursor := crDefault;
      end;
    end else
      Exit;
  end else begin
    Screen.Cursor := crHourGlass;
    Controller.Enumerate(SI.Pidl, etList);
  end;
  Screen.Cursor := crDefault;
  if (ViewStyle = vsReport) then
    GetColHeader(SI, Columns);
  if not FilterChange then begin
    if Assigned(FFolder) then
      FFolder.Free;
    FFolder := TStShellFolder.Create(Controller);
    FFolder.Assign(SI);
    if not Assigned(FFolder.FParentFolder) then
      FFolder.FParentFolder := Controller.DesktopFolder;               
  end;
  if OwnItem then
    SI.Free;
  if ViewStyle = vsReport then
    HaveDetails := True;
  SortItems;
  {$IFDEF VERSION4}
  { Apply the file filter, if any. }
  if (FileFilter <> '') and Filtered then
    FilterList;
  Items.Count := ShellItems.Count;
  {$ELSE}
  for I := 0 to Pred(ShellItems.Count) do begin
    SI1 := ShellItems[I];
    Item := Items.Add;
    Item.Caption := SI1.DisplayName;
    Item.ImageIndex := SI1.IconIndex;
    Item.OverlayIndex := SI1.OverlayIconIndex;
    Item.Data := Pointer(I);
    Item.Cut := SI1.IsGhosted;
    for J := 0 to Pred(SI1.ColText.Count) do
      Item.SubItems.Add(SI1.ColText[J]);
  end;
  {$ENDIF}
  if Assigned(Items[0]) then                                           
    Items[0].MakeVisible(False);                                       
  Perform(WM_SETREDRAW, 1, 0);

  if Assigned(FOnListFilled) then
    DoListFilled;
  if not (csDesigning in ComponentState) then begin
    ShellMonitor.Active := False;
    ShellMonitor.WatchPidl := FFolder.Pidl;
    ShellMonitor.MaxNotifications := 5;                                
    ShellMonitor.Active := True;
  end;
  if Assigned(FComboBox) then begin
    { If the combo box ItemIndex is -1 it means that we've just }
    { loaded and the combo box item index is not yet set.       }
    if not (csDesigning in ComponentState) then begin
      if FComboBox.ItemIndex = -1 then
        FComboBox.ListViewFolderChange(Folder, False);
      FComboBox.SelectItem(Folder);
    end;
  end;                                                                 
end;

procedure TStCustomShellListView.DoFilterItem(const SI : TStShellItem;
  var Accept : Boolean);
begin
  if Assigned(FOnFilterItem) and Filtered then
    FOnFilterItem(Self, SI, Accept);
end;

procedure TStCustomShellListView.DoListFilled;
begin
  if Assigned(FOnListFilled)then
    FOnListFilled(Self);
end;

procedure TStCustomShellListView.DoItemSelected(const SI : TStShellItem);
begin
  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, SI);
end;

procedure TStCustomShellListView.DoItemDblClick(
  const SI : TStShellItem; var DefaultAction : Boolean);
begin
  if Assigned(FOnItemDblClick) then
    FOnItemDblClick(Self, SI, DefaultAction);
end;

procedure TStCustomShellListView.DoShellChangeNotify(const SI1 : TStShellItem;
  const SI2 : TStShellItem; const Events : TStNotifyEventsSet);
begin
  if Assigned(FOnShellChangeNotify) then
    FOnShellChangeNotify(Self, SI1, SI2, Events);
end;

procedure TStCustomShellListView.SetRootFolder(const Value : string);
begin
  FRootFolder := Value;
  { Add trailing backslash if the root folder is a drive. }
  if FRootFolder <> '' then begin
    if (Pos(':', FRootFolder) <> 0) and (Length(FRootFolder) = 2) then
      FRootFolder := FRootFolder + '\'
  end else
    Clear;
  if not (csLoading in ComponentState) and (FRootFolder <> '') then begin
    { Can't have a SpecialRootFolder if RootFolder is assigned. }
    SpecialRootFolder := sfNone;
    if (FViewStyle = vsReport) and (csDesigning in ComponentState) then
      Exit;
    FillList(nil);
    if Assigned(FTreeView) and not (csLoading in ComponentState) then  
      FTreeView.ListViewFolderChange(Folder, False);                   
    if Assigned(FComboBox) and not (csDesigning in ComponentState) then
      FComboBox.ListViewFolderChange(Folder, False);                   
  end;
end;

procedure TStCustomShellListView.SetSpecialRootFolder(
  const Value : TStSpecialRootFolder);
begin
  if ((ShellFolders[Value] = CSIDL_INTERNET) or
     (ShellFolders[Value] = CSIDL_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_ALTSTARTUP) or
     (ShellFolders[Value] = CSIDL_COMMON_FAVORITES) or
     (ShellFolders[Value] = CSIDL_INTERNET_CACHE) or
     (ShellFolders[Value] = CSIDL_COOKIES) or
     (ShellFolders[Value] = CSIDL_HISTORY)) and
     (ShellVersion < 4.7) then
    RaiseStError(ESsShellError, ssscShellVersionError);
  if FSpecialRootFolder <> Value then begin
    FSpecialRootFolder   := Value;
    if (FSpecialRootFolder <> sfNone) and not (csLoading in ComponentState) then begin
      { Can't have a RootFolder if SpecialRootFolder is assigned. }
      RootFolder := '';
      if (FViewStyle = vsReport)                                       
          and (csDesigning in ComponentState) then                     
        Exit;                                                          
      FillList(nil);
      if Assigned(FTreeView) and not (csLoading in ComponentState) then
        FTreeView.ListViewFolderChange(Folder, False);                 
      if Assigned(FComboBox) and not (csDesigning in ComponentState) then 
        FComboBox.ListViewFolderChange(Folder, False);                 
    end else
      Clear;
  end;
end;

procedure TStCustomShellListView.MoveUpOneLevel;
var
  SI      : TStShellItem;
  AFolder : IShellFolder;
  Pidl    : PItemIDList;
begin
  with Controller do begin
    Pidl := ILClone(FFolder.Pidl);
    GetParentFolder(Pidl, AFolder);
    if not Assigned(AFolder) then
      AFolder := DesktopFolder;
    ILRemoveLastID(Pidl);
    SI := TStShellItem.CreateFromPidl(Pidl, Controller);
  end;
  if Assigned(FTreeView) then
    if not FTreeView.ListViewFolderChange(SI, True) then
      Exit;
  if Assigned(FComboBox) then
    FComboBox.ListViewFolderChange(SI, True);
  FillList(TStShellFolder(SI));
  SI.Free;
end;

procedure TStCustomShellListView.ShellEvent(Sender : TObject;
  SI1, SI2 : TStShellItem; Events : TStNotifyEventsSet);
begin
  if IsEditing then
    Exit;
  { Something in the shell changed so refresh the folder. }
  if not (neAttributesChange in Events) then
    RefreshList(True)
  else if ViewStyle = vsReport then begin
    { neAttributesChange usuall occurs as a result of an }
    { item in the Printers folder changing. Update the   }
    { Details if we are in report view. }
    HaveDetails := False;
    if Assigned(SI1) then
      GetItemDetails(SI1);
    Update;
  end;
  DoShellChangeNotify(SI1, SI2, Events);
end;

procedure TStCustomShellListView.RefreshList(Enumerate : Boolean);
var
  SelText : string;
  Index   : Integer;
  I       : Integer;
  SI      : TStShellItem;
{$IFNDEF VERSION4}
  J       : Integer;
  Item    : TListItem;
{$ENDIF}
begin
  { Save the selected item. }
  if Assigned(Selected) then begin
    Index := Selected.Index;
    SelText := Selected.Caption;
  end else begin
    Index := 0;
    SelText := '';
  end;

  Perform(WM_SETREDRAW, 0, 0);
  if Enumerate then
    Clear;
  {$IFDEF VERSION4}
  Items.Count := 0;
  {$ENDIF}
  Items.Clear;
  FSelectedItem := nil;                                                
  if Enumerate then
    Controller.Enumerate(FFolder.Pidl, etList);
  SortItems;
  if Enumerate then                                                    
    { Apply the file filter, if any. }                                 
    if (FileFilter <> '') and Filtered then                            
      FilterList;                                                      
  {$IFDEF VERSION4}
  Items.Count := ShellItems.Count;
  Perform(WM_SETREDRAW, 1, 0);
  {$ELSE}
  for I := 0 to Pred(ShellItems.Count) do begin
    SI := ShellItems[I];
    Item := Items.Add;
    Item.Caption := SI.DisplayName;
    Item.ImageIndex := SI.IconIndex;
    Item.OverlayIndex := SI.OverlayIconIndex;
    Item.Data := Pointer(I);
    for J := 0 to Pred(SI.ColText.Count) do
      Item.SubItems.Add(SI.ColText[J]);
  end;
  {$ENDIF}
  { Restore the item that was previously selected, if it still exists. }
  if SelText <> '' then begin                                          
    Selected := FindCaption(0, SelText, False, True, True);
    if not Assigned(Selected) then
      Selected := Items[Index];
    if Assigned(Selected) then
      Selected.MakeVisible(False);
  end;
  SelectedItems.Clear;                                                 
  for I := 0 to Pred(Items.Count) do                                   
    if Items[I].Selected then begin                                    
      SI := TStShellItem.Create(Controller);                           
      SI.Assign(ShellItems[Integer(Items[I].Data)]);                   
      SelectedItems.FList.Add(SI);                                     
    end;                                                               
  Perform(WM_SETREDRAW, 1, 0);
end;

procedure TStCustomShellListView.SetTreeView(
  const Value : TStCustomShellTreeView);
begin
  if (FTreeView <> Value) and Assigned(Value) then begin
    RootFolder := '';
    SpecialRootFolder := sfNone;
    FTreeView := Value;
    if (csDesigning in ComponentState) then
      FTreeView.FListView := Self;
    if Assigned(FTreeView.Controller) then begin
      Controller := FTreeView.Controller;
      if Assigned(Controller) then begin
        Controller.ListView := Self;
        if Assigned(FTreeView.Selected) then
          if not (csDesigning in ComponentState) then
            FillList(FTreeView.Folders[Integer(FTreeView.Selected.Data)])
        else
          FillList(FTreeView.Folders[0]);
      end;
    end;
  end else begin
    FTreeView := Value;
    if Assigned(FTreeView) then
      if FTreeView.Folders.Count <> 0 then                             
        FillList(FTreeView.Folders[0]);
  end;
end;

procedure TStCustomShellListView.Change(Item : TListItem; Change : Integer);
begin
  inherited Change(Item, Change);
  if Assigned(Item) then
    FSelectedItem := ShellItems[Integer(Item.Data)];
end;

function TStCustomShellListView.AddFolder(FolderName : string): Boolean;
var
  Path    : string;
  Pidl    : PItemIDList;
  SI      : TStShellItem;
  I       : Integer;
  AddItem : Boolean;
  Count   : Integer;
  S       : string;
  Index   : Integer;
  Done    : Boolean;
  {.$IFNDEF VERSION4}
  Item    : TListItem;
  {.$ENDIF}
begin
  if FFolder.IsReadOnly then                                           
    RaiseStError(ESsInvalidFolder, ssscFolderReadOnly);                
  if FolderName = '' then
    FolderName := ssscDefaultFolderName;
  { If the folder name already exists give it a new name. }
  S := FolderName;
  Count := 2;
  Done := False;
  while not Done do begin
    Index := -1;
    for I := 0 to Pred(ShellItems.Count) do
      if AnsiUpperCase(S) = AnsiUpperCase(ShellItems[I].DisplayName) then begin
        S := FolderName + ' (' + IntToStr(Count) + ')';
        Inc(Count);
        Index := I;
        Break;
      end;
    { No match found. }
    Done := (Index = -1);
  end;
  Path := FFolder.Path;
  if Path[Length(Path)] <> '\' then
    Path := Path + '\';
  Path := Path + S;
  Result := Boolean(CreateDirectory(PChar(Path), nil));

  { Add it to the list view and go to edit mode. }
  PidlFromPath(Path, Controller.DesktopFolder, Pidl);
  SI := TStShellItem.CreateFromPidl(Pidl, Controller);
  SI.ParentList := FullList.FList;
  AddItem := True;
  {$IFNDEF VERSION4}
  Index := 0;
  {$ENDIF}
  if Assigned(FOnFilterItem) then
    DoFilterItem(SI, AddItem);
  if AddItem then begin
    {$IFDEF VERSION4}
    FullList.FList.Add(SI);
    {$ELSE}
    Index := FullList.FList.Add(SI);
    {$ENDIF}
    if ViewStyle = vsReport then
      GetItemDetails(SI);
  end else
    SI.Free;
  {$IFDEF VERSION4}
  { Apply the file filter, if any. }
  if (FileFilter <> '') and Filtered then
    FilterList;
  Items.Count := ShellItems.Count;
  { Find the new item and set it into edit mode. }
  Item := FindCaption(0, SI.DisplayName, False, True, True);           
  if Assigned(Item) then begin
    Item.MakeVisible(False);                                           
    Item.EditCaption;                                                  
  end;                                                                 
  {$ELSE}
  if AddItem then begin
    Item := Items.Add;
    Item.Caption := SI.DisplayName;
    Item.ImageIndex := SI.IconIndex;
    Item.OverlayIndex := SI.OverlayIconIndex;
    Item.Data := Pointer(Index);
    Item.Cut := SI.IsGhosted;
    for I := 0 to Pred(SI.ColText.Count) do
      Item.SubItems.Add(SI.ColText[I]);
  end;
  {$ENDIF}

  { Don't send the change notify if the list is filtered. This }
  { eliminates problems that arise from notification reaching  }
  { the list view before the user is done editing the item.    }
  if (FileFilter <> '') and Filtered then                              
    Exit;                                                              
  { Creating a directory using CreateDirectory will not result }
  { in a shell event being sent. However, we can let the shell }
  { know by calling SHChangeNotify. }
  SHChangeNotify(SHCNE_MKDIR, SHCNF_IDLIST, SI.Pidl, nil);
end;

function TStCustomShellListView.DeleteItem(
  Recycle : Boolean; Confirm : Boolean): Boolean;
var
  FO   : TStFileOperation;
  I    : Integer;                                                      
  Item : TListItem;                                                    
begin
  if not Assigned(SelectedItem) then
    Result := False
  else begin
    FO := TStFileOperation.Create(Self);
    if SelCount = 1 then                                               
      FO.SourceFiles.Add(SelectedItem.FPath)
    else begin                                                         
      Item := Selected;                                                
      FO.SourceFiles.Add(ShellItems[Integer(Item.Data)].FPath);        
      for I := 1 to Pred(SelCount) do begin                            
        Item := GetNextItem(Item, sdAll, [isSelected]);                
        FO.SourceFiles.Add(ShellItems[Integer(Item.Data)].FPath);      
      end;                                                             
    end;                                                               
    FO.Operation := fopDelete;
    if not Recycle then
      FO.Options := FO.Options - [foAllowUndo];
    if not Confirm then
      FO.Options := FO.Options + [foNoConfirmation];
    Result := FO.Execute;
    FO.Free;
  end;
end;

procedure TStCustomShellListView.Refresh;
var
  SF : TStShellFolder;
begin
  SF := TStShellFolder.Create(Controller);
  SF.Assign(FFolder);
  if not Assigned(SF.FParentFolder) then
    SF.FParentFolder := Controller.DesktopFolder;                      
  FillList(SF);
  SF.Free;
end;

function TStCustomShellListView.RenameItem(NewName : string): Boolean;
begin
  Result := Controller.RenameItem(
    ShellItems[Integer(Selected.Data)], NewName);
end;

procedure TStCustomShellListView.Clear;
begin
  if Assigned(FullList) then
    FullList.Clear;
  if Assigned(FilteredList) then
    FilteredList.Clear;
  Perform(WM_SETREDRAW, 0, 0);
  if Items.Count <> 0 then
    Items.Clear;
  ClickedColumn := 0;
  Perform(WM_SETREDRAW, 1, 0);
end;

procedure TStCustomShellListView.CopyToClipboard;
var
  SI        : TStShellItem;
  SI1       : TStShellItem;
  I         : Integer;
  PidlArray : PStPidlArray;
  Item      : TListItem;
begin
  if Assigned(Selected) then begin
    SI := ShellItems[Integer(Selected.Data)];
    if SI.CanCopy then begin
      if SelCount > 1 then begin
        GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
        Item := Selected;
        PidlArray^[0] := SI.FSimplePidl;
        for I := 1 to Pred(SelCount) do begin
          Item := GetNextItem(Item, sdAll, [isSelected]);
          SI1 := ShellItems[Integer(Item.Data)];
          PidlArray^[I] := SI1.FSimplePidl;
        end;
        ShellMenuExecute(Self, SI.ParentFolder,
          PidlArray^[0], SelCount, Handle, caCopy);
        FreeMem(PidlArray);
      end else
        ShellMenuExecute(Self, SI.ParentFolder,
          SI.FSimplePidl, 1, Handle, caCopy);
    end;
  end;
end;

procedure TStCustomShellListView.CutToClipboard;
var
  SI        : TStShellItem;
  SI1       : TStShellItem;
  I         : Integer;
  PidlArray : PStPidlArray;
  Item      : TListItem;
begin
  if Assigned(Selected) then begin
    SI := ShellItems[Integer(Selected.Data)];
    if SI.CanCopy then begin
      if SelCount > 1 then begin
        GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
        Item := Selected;
        PidlArray^[0] := SI.FSimplePidl;
        for I := 1 to Pred(SelCount) do begin
          Item := GetNextItem(Item, sdAll, [isSelected]);
          SI1 := ShellItems[Integer(Item.Data)];
          PidlArray^[I] := SI1.FSimplePidl;
        end;
        ShellMenuExecute(Self, SI.ParentFolder,
          PidlArray^[0], SelCount, Handle, caCut);
        FreeMem(PidlArray);
      end else
        ShellMenuExecute(Self, SI.ParentFolder,
          SI.FSimplePidl, 1, Handle, caCut);
    end;
  end;
end;

procedure TStCustomShellListView.PasteFromClipboard;
var
  SI : TStShellItem;
begin
  SI := nil;
  if Assigned(Selected) then
    SI := ShellItems[Integer(Selected.Data)]
  else if Assigned(FFolder) then
    SI := FFolder;
  if Assigned(SI) then
    if SI.CanPaste then
      ShellMenuExecute(Self, SI.ParentFolder,
        SI.FSimplePidl, 1, Handle, caPaste);
end;

procedure TStCustomShellListView.LoadFolder(Folder : TStShellFolder);
begin
  FillList(Folder);
end;

function TStCustomShellListView.ShowPropertySheet : Boolean;
var
  SI        : TStShellItem;
  SI1       : TStShellItem;
  I         : Integer;
  PidlArray : PStPidlArray;
  Item      : TListItem;
begin
  if Assigned(Selected) then begin
    SI := ShellItems[Integer(Selected.Data)];
    Result := SI.HasPropSheet;
    if SI.HasPropSheet then begin
      if SelCount > 1 then begin
        GetMem(PidlArray, SizeOf(PItemIDList) * SelCount);
        Item := Selected;
        PidlArray^[0] := SI.FSimplePidl;
        for I := 1 to Pred(SelCount) do begin
          Item := GetNextItem(Item, sdAll, [isSelected]);
          SI1 := ShellItems[Integer(Item.Data)];
          PidlArray^[I] := SI1.FSimplePidl;
        end;
        ShellMenuExecute(Self, SI.ParentFolder,
          PidlArray^[0], SelCount, Handle, caProperties);
        FreeMem(PidlArray);
      end else
        ShellMenuExecute(Self, SI.ParentFolder,
          SI.FSimplePidl, 1, Handle, caProperties);
    end;
  end else
    Result := False;
end;

procedure TStCustomShellListView.SortColumn(                           
  Index : Integer; SortDir : TStSortDirection);                        
begin                                                                  
  if ViewStyle = vsReport then begin                                   
    if SortDir <> sdToggle then                                        
      Ascending := (SortDir = sdDescending);                           
    ColClick(Columns[Index]);                                          
  end;                                                                 
end;                                                                   

procedure TStCustomShellListView.SortItems;
begin
  { Save the selected items. }
  if not FileSystemSort and (ClickedColumn <> 0) then begin
    ShellItems.FList.Sort(ItemTextSortFunc);
  end else if ShellItems.Count > 1 then
    case ClickedColumn of
      0 : ShellItems.FList.Sort(ItemPidlSortFunc);
      1 : ShellItems.FList.Sort(ItemSizeSortFunc);
      2 :
        if (loSortTypeByExt in FOptions) then
          ShellItems.FList.Sort(ItemTypeSortFuncExt)
        else
          ShellItems.FList.Sort(ItemTypeSortFuncName);
      3 : ShellItems.FList.Sort(ItemDateSortFunc);
      4 : ShellItems.FList.Sort(ItemAttrSortFunc);
    end;
end;

function TStCustomShellListView.GetShellItems : TStShellItemList;
begin
  if (FileFilter <> '') and Filtered and not Assigned(FOnFilterItem) then
    Result := FilteredList
  else
    Result := FullList;
end;

procedure TStCustomShellListView.FilterList;
var
  I     : Integer;
  SI    : TStShellItem;
  RegEx : TStRegEx;
  MP    : TMatchPosition;
begin
  FilteredList.Clear;
  RegEx := TStRegEx.Create(nil);
  RegEx.IgnoreCase := True;
  RegEx.DOSMasksToRegEx(FFileFilter);
  for I := 0 to Pred(FullList.Count) do begin
    SI := TStShellItem.Create(Controller);
    SI.Assign(FullList[I]);
    if not Assigned(SI.FParentFolder) then
      SI.FParentFolder := Controller.DesktopFolder;
    { Original Code: if not FullList[I].IsFileFolder then begin }
    if not FullList[I].IsFile then begin                               
      FilteredList.FList.Add(SI);
      Continue;
    end else if RegEx.CheckString(ExtractFileName(SI.Path), MP) then
      FilteredList.FList.Add(SI)
    else
      SI.Free;
  end;
  RegEx.Free;
  RefreshList(False);
end;

procedure TStCustomShellListView.SetFileFilter(const Value : string);
begin
  if FFileFilter <> Value then begin
    FFileFilter := Value;
    FilteredList.Clear;
    if (FFileFilter <> '') and FFiltered then
      if not Assigned(FOnFilterItem) then
        FilterList
      else if Assigned(FOnFilterItem) then
        { The OnFilterItem event is assigned so we'll have to }
        { enumerate again. }
        FillList(Folder)
    else begin
      Filtered := False;
      RefreshList(False);
    end;
  end;
end;

procedure TStCustomShellListView.SetFiltered(const Value : Boolean);
begin
  FFiltered := Value;
  FilterChange := False;
  if not (csDesigning in ComponentState) then
    if (FFileFilter <> '') and FFiltered then
      if not Assigned(FOnFilterItem) then
        FilterList
      else if not (csLoading in ComponentState) then begin
        { The OnFilterItem event is assigned so we'll have to }
        { enumerate again. }
        FilterChange := True;
        FillList(Folder);
      end;
end;

procedure TStCustomShellListView.SetComboBox(
  const Value : TStCustomShellComboBox);
begin
  if RecreatingWnd then
    Exit;                                                              
  if (FComboBox <> Value) and Assigned(Value) then begin
    FComboBox := Value;
    if (csDesigning in ComponentState) then
      FComboBox.ListView := Self;
    if Assigned(FComboBox.Controller) then begin
      Controller := FComboBox.Controller;
      if Assigned(Controller) then
        Controller.ListView := Self;
    end;
  end else
    FComboBox := Value;
end;

constructor TStNotificationItem.Create(
  var PidlOne, PidlTwo : PItemIDList; Mask : DWORD);
begin
  inherited Create;
  if Assigned(PidlOne) then
    Pidl1 := ILClone(PidlOne);
  if Assigned(PidlTwo) then
    Pidl2 := ILClone(PidlTwo);
  EventMask := Mask;
end;

destructor TStNotificationItem.Destroy;
begin
  if Assigned(Pidl1) then
    ILFree(Pidl1);
  if Assigned(Pidl2) then
    ILFree(Pidl2);
  inherited;
end;

{ TStCustomShellNotification }

constructor TStCustomShellNotification.Create(AOwner : TComponent);
var
  VI     : TSsVersionInfo;
  WinDir : array [0..MAX_PATH - 1] of Char;
begin
  inherited Create(AOwner);
  {$IFNDEF VERSION2009}
  if not Assigned(@ILClone) then
    LoadILFunctions;
  {$ENDIF}
  FMaxNotifications := 0;
  FNotifyEvents := [neAssociationChange, neAttributesChange,
    neFileChange, neFileCreate, neFileDelete, neFileRename,
    neDriveAdd, neDriveRemove, neShellDriveAdd, neDriveSpaceChange,
    neMediaInsert, neMediaRemove, neFolderCreate, neFolderDelete,
    neFolderRename, neFolderUpdate, neNetShare, neNetUnShare,
    neServerDisconnect, neImageListChange];
  FWatchFolder        := '';
  FWatchSubFolders    := False;
  FSpecialWatchFolder := sfNone;
  EventQueue := TStDQue.Create(TStListNode);
  EventQueue.DisposeData := QueueDispose;
  if not (csDesigning in ComponentState) then
    { Create a hidden window so we can receive notification }
    { of shell changes. }
    Handle := AllocateHWnd(WndProc);
  { Get the version of Shell32.dll. Some special folders }
  { require Version 4.71 or higher. }
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;
end;

destructor TStCustomShellNotification.Destroy;
begin
  if HNotify <> 0 then
    ShellNotifyUnRegister;
  if Handle <> 0 then
    DeallocateHWnd(Handle);
  EventQueue.Free;
  inherited;
end;

procedure TStCustomShellNotification.Loaded;
begin
  inherited;
  if FActive and not Registered then
    ShellNotifyRegister;
end;

procedure TStCustomShellNotification.SetActive(const Value : Boolean);
begin
  FActive := Value;
  if (FActive = True) and not Registered then
    ShellNotifyRegister
  else if (FActive = False) and Registered then
    ShellNotifyUnRegister;
end;

procedure TStCustomShellNotification.SetWatchFolder(const Value : string);
var
  DesktopFolder : IShellFolder;
begin
  FWatchFolder := Value;
  { Add trailing backslash if the root folder is a drive. }
  if FWatchFolder <> '' then
    if (Pos(':', FWatchFolder) <> 0) and (Length(FWatchFolder) = 2) then
      FWatchFolder := FWatchFolder + '\';
  if {not (csLoading in ComponentState) and} (FWatchFolder <> '') then begin 
    { Can't have a SpecialRootFolder if RootFolder is assigned. }
    SpecialWatchFolder := sfNone;
    { Starting with a folder name rather than with a
    { special folder. }
    SHGetDesktopFolder(DesktopFolder);
    PidlFromPath(FWatchFolder, DesktopFolder, FWatchPidl);
    if not Assigned(WatchPidl) then
      RaiseStError(ESsInvalidFolder, ssscInvalidFolder);
    DesktopFolder := nil;
    if FActive then
      ShellNotifyRegister;
  end;
end;

procedure TStCustomShellNotification.SetWatchPidl(Value : PItemIDList);
begin
  FWatchPidl := Value;
  if FActive then
    ShellNotifyRegister;
end;

procedure TStCustomShellNotification.SetSpecialWatchFolder(
  const Value : TStSpecialRootFolder);
begin
  FSpecialWatchFolder := Value;
  if not (csDesigning in ComponentState) then
    if ((ShellFolders[Value] = CSIDL_INTERNET) or
       (ShellFolders[Value] = CSIDL_ALTSTARTUP) or
       (ShellFolders[Value] = CSIDL_COMMON_ALTSTARTUP) or
       (ShellFolders[Value] = CSIDL_COMMON_FAVORITES) or
       (ShellFolders[Value] = CSIDL_INTERNET_CACHE) or
       (ShellFolders[Value] = CSIDL_COOKIES) or
       (ShellFolders[Value] = CSIDL_HISTORY)) and
       (ShellVersion < 4.7) then
      RaiseStError(ESsShellError, ssscShellVersionError);
  if FSpecialWatchFolder <> Value then begin
    FSpecialWatchFolder   := Value;
    if (FSpecialWatchFolder <> sfNone) {and not (csLoading in ComponentState)} then begin 
      { Can't have a RootFolder if SpecialRootFolder is assigned. }
      WatchFolder := '';
      if FSpecialWatchFolder = sfDesktop then
        WatchPidl := nil
      else
        SHGetSpecialFolderLocation(
          Handle, ShellFolders[FSpecialWatchFolder], FWatchPidl);
      if FActive then
        ShellNotifyRegister;
    end;
  end;
end;

procedure TStCustomShellNotification.SetWatchSubFolders(
  const Value : Boolean);
begin
  FWatchSubFolders := Value;
  if FActive then
    ShellNotifyRegister;
end;

procedure TStCustomShellNotification.ShellNotifyRegister;
var
  NR    : TSHChangeNotifyEntry; //TStNotifyRegister;
  Flags : DWORD;
begin
  if not (csDesigning in ComponentState) and
     not (csLoading in ComponentState) then begin
    NR.Pidl := WatchPidl;
    NR.fRecursive {WatchSubTree} := FWatchSubFolders;
    { Registration is changing so remove existing registration. }
    if HNotify <> 0 then
      ShellNotifyUnRegister;
    Flags := 0;
    if neAssociationChange in FNotifyEvents then
      Flags := Flags or SHCNE_ASSOCCHANGED;
    if neAttributesChange in FNotifyEvents then
      Flags := Flags or SHCNE_ATTRIBUTES;
    if neFileChange in FNotifyEvents then
      Flags := Flags or SHCNE_UPDATEITEM;
    if neFileCreate in FNotifyEvents then
      Flags := Flags or SHCNE_CREATE;
    if neFileDelete in FNotifyEvents then
      Flags := Flags or SHCNE_DELETE;
    if neFileRename in FNotifyEvents then
      Flags := Flags or SHCNE_RENAMEITEM;
    if neDriveAdd in FNotifyEvents then
      Flags := Flags or SHCNE_DRIVEADD;
    if neDriveRemove in FNotifyEvents then
      Flags := Flags or SHCNE_DRIVEREMOVED;
    if neShellDriveAdd in FNotifyEvents then
      Flags := Flags or SHCNE_DRIVEADDGUI;
    if neDriveSpaceChange in FNotifyEvents then
      Flags := Flags or SHCNE_FREESPACE;
    if neMediaInsert in FNotifyEvents then
      Flags := Flags or SHCNE_MEDIAINSERTED;
    if neMediaRemove in FNotifyEvents then
      Flags := Flags or SHCNE_MEDIAREMOVED;
    if neFolderCreate in FNotifyEvents then
      Flags := Flags or SHCNE_MKDIR;
    if neFolderDelete in FNotifyEvents then
      Flags := Flags or SHCNE_RMDIR;
    if neFolderRename in FNotifyEvents then
      Flags := Flags or SHCNE_RENAMEFOLDER;
    if neFolderUpdate in FNotifyEvents then
      Flags := Flags or SHCNE_UPDATEDIR;
    if neNetShare in FNotifyEvents then
      Flags := Flags or SHCNE_NETSHARE;
    if neNetUnShare in FNotifyEvents then
      Flags := Flags or SHCNE_NETUNSHARE;
    if neServerDisconnect in FNotifyEvents then
      Flags := Flags or SHCNE_SERVERDISCONNECT;
    if neImageListChange in FNotifyEvents then
      Flags := Flags or SHCNE_UPDATEIMAGE;
    HNotify := SHChangeNotifyRegister(Handle,
      SHCNF_ACCEPT_INTERRUPTS or SHCNF_ACCEPT_NON_INTERRUPTS,
      Flags, MSG_SHELLNOTIFY, 1, NR);
    Registered := (HNotify <> 0);
  end;
end;

procedure TStCustomShellNotification.ShellNotifyUnRegister;
begin
  if HNotify <> 0 then begin
    SHChangeNotifyDeregister(HNotify);
    HNotify := 0;
    Registered := False;
  end;
end;

procedure TStCustomShellNotification.WndProc(var Msg : TMessage);
const
  Count : Integer = 0;
var
  SI1, SI2   : TStShellItem;
  DrvSpace   : PStDriveSpaceRec;
  PidlArray  : PStDualPidlArray;
  FI         : TSHFileInfo;
  Attr       : UINT;
  EventMask  : DWORD;
  NItem      : TStNotificationItem;
begin
  case Msg.Msg of
    MSG_SHELLNOTIFY :
      begin
        if (FMaxNotifications <> 0) and
           (EventQueue.Count > FMaxNotifications) then
          Exit;                                                        
        EventMask := DWORD(Msg.lParam);
        PidlArray := PStDualPidlArray(Msg.wParam);
        if (EventMask and SHCNE_ATTRIBUTES) = SHCNE_ATTRIBUTES then begin
          Events := [];
          SI1 := TStShellItem.CreateFromPidl(PidlArray.Pidl1, nil);
          SI2 := TStShellItem.CreateFromPidl(PidlArray.Pidl2, nil);
          Events := [neAttributesChange];
          DoAttributesChange(SI1, SI2);
          DoShellChangeNotify(SI1, SI2, Events);
          SI1.Free;
          SI2.Free;
          Exit;
        end;
        { Some shell events don't actually take place until after      }
        { WndProc exits. For this reason, we post ourselves a message  }
        { and do the actual notifications in response to that message. }
        NItem := TStNotificationItem.Create(
          PidlArray.Pidl1, PidlArray.Pidl2, EventMask);
        EventQueue.Append(NItem);                                      
        if EventQueue.Count = 1 then                                   
          PostMessage(Handle, MSG_SHELLNOTIFY2, 0, 0);
        Exit;
      end;
    MSG_SHELLNOTIFY2 :
      begin
        if EventQueue.Count = 0 then
          Exit;                                                        
        EventQueue.PeekHead(Pointer(NItem));                           
        while Assigned(NItem) do begin
          Events := [];
          EventQueue.PeekHead(Pointer(NItem));                         
          if Assigned(NItem.Pidl1) then
            SI1 := TStShellItem.CreateFromPidl(NItem.Pidl1, nil)
          else
            SI1 := nil;
          if Assigned(NItem.Pidl2) then
            SI2 := TStShellItem.CreateFromPidl(NItem.Pidl2, nil)
          else                                                         
            SI2 := nil;                                                
          if (NItem.EventMask and SHCNE_ASSOCCHANGED) = SHCNE_ASSOCCHANGED then begin
            Events := Events + [neAssociationChange];
            DoAssociationChange;
          end;
          if (NItem.EventMask and SHCNE_UPDATEITEM) = SHCNE_UPDATEITEM then begin
            Events := Events + [neFileChange];
            DoFileChange(SI1);
          end;
          if (NItem.EventMask and SHCNE_CREATE) = SHCNE_CREATE then begin
            Events := Events + [neFileCreate];
            DoFileCreate(SI1);
          end;
          if (NItem.EventMask and SHCNE_DELETE) = SHCNE_DELETE then begin
            Events := Events + [neFileDelete];
            DoFileDelete(SI1, SI2);
          end;
          if (NItem.EventMask and SHCNE_RENAMEITEM) = SHCNE_RENAMEITEM then begin
            Events := Events + [neFileRename];
            DoFileRename(SI1, SI2);
          end;
          if (NItem.EventMask and SHCNE_DRIVEADD) = SHCNE_DRIVEADD then begin
            Events := Events + [neDriveAdd];
            DoDriveAdd(SI1);
          end;
          if (NItem.EventMask and SHCNE_DRIVEREMOVED) = SHCNE_DRIVEREMOVED then begin
            Events := Events + [neDriveRemove];
            DoDriveRemove(SI1);
          end;
          if (NItem.EventMask and SHCNE_DRIVEADDGUI) = SHCNE_DRIVEADDGUI then begin
            Events := Events + [neShellDriveAdd];
            DoShellDriveAdd(SI1);
          end;
          if (NItem.EventMask and SHCNE_MEDIAINSERTED) = SHCNE_MEDIAINSERTED then begin
            Events := Events + [neMediaInsert];
            { Media was inserted. We need to flush the shell cache }
            { or we'll just get a cached icon and display name. }
            Attr := SFGAO_VALIDATE;
            SI1.ParentFolder.GetAttributesOf(0, SI1.FSimplePidl, Attr);
            SHGetFileInfo(PChar(SI1.Pidl), 0, FI, SizeOf(FI),
              SHGFI_PIDL or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX);
            SI1.FDisplayName := FI.szDisplayName;
            SI1.FIconIndex := FI.iIcon;
            DoMediaInsert(SI1);
          end;
          if (NItem.EventMask and SHCNE_MEDIAREMOVED) = SHCNE_MEDIAREMOVED then begin
            Events := Events + [neMediaRemove];
            { Media was removed. We need to flush the shell cache }
            { or we'll just get a cached icon and display name. }
            Attr := SFGAO_VALIDATE;
            SI1.ParentFolder.GetAttributesOf(0, SI1.FSimplePidl, Attr);
            SHGetFileInfo(PChar(SI1.Pidl), 0, FI, SizeOf(FI),
              SHGFI_PIDL or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX);
            SI1.FDisplayName := FI.szDisplayName;
            SI1.FIconIndex := FI.iIcon;
            DoMediaRemove(SI1);
          end;
          if (NItem.EventMask and SHCNE_MKDIR) = SHCNE_MKDIR then begin
            Events := Events + [neFolderCreate];
            DoFolderCreate(SI1);
          end;
          if (NItem.EventMask and SHCNE_RMDIR) = SHCNE_RMDIR then begin
            Events := Events + [neFolderDelete];
            DoFolderDelete(SI1);
          end;
          if (NItem.EventMask and SHCNE_RENAMEFOLDER) = SHCNE_RENAMEFOLDER then begin
            Events := Events + [neFolderRename];
            DoFolderRename(SI1, SI2);
          end;
          if (NItem.EventMask and SHCNE_UPDATEDIR) = SHCNE_UPDATEDIR then begin
            Events := Events + [neFolderUpdate];
            DoFolderChange(SI1);
          end;
          if (NItem.EventMask and SHCNE_NETSHARE) = SHCNE_NETSHARE then begin
            Events := Events + [neNetShare];
            DoNetShare(SI1);
          end;
          if (NItem.EventMask and SHCNE_NETUNSHARE) = SHCNE_NETUNSHARE then begin
            Events := Events + [neNetUnShare];
            DoNetUnShare(SI1);
          end;
          if (NItem.EventMask and SHCNE_SERVERDISCONNECT) = SHCNE_SERVERDISCONNECT then begin
            Events := Events + [neServerDisconnect];
            DoServerDisconnect(SI1);
          end;
          if (NItem.EventMask and SHCNE_UPDATEIMAGE) = SHCNE_UPDATEIMAGE then begin
            Events := Events + [neImageListChange];
            DoImageListChange;
          end;
          if (NItem.EventMask and SHCNE_FREESPACE) = SHCNE_FREESPACE then begin
            DrvSpace := PStDriveSpaceRec(NItem.Pidl1);
            Events := Events + [neDriveSpaceChange];
            DoDriveSpaceChange(DrvSpace.Item1);
          end;
          DoShellChangeNotify(SI1, SI2, Events);
          SI1.Free;
          SI2.Free;
          EventQueue.PopHead;                                          
          EventQueue.PeekHead(Pointer(NItem));                         
          { Enable notifications again. }
        end;
        Exit;
      end;
    else
      Msg.Result := DefWindowProc(
        Handle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
end;

procedure TStCustomShellNotification.SetNotifyEvents(
  const Value : TStNotifyEventsSet);
begin
  FNotifyEvents := Value;
  if FActive then
    ShellNotifyRegister;
end;

procedure TStCustomShellNotification.DoAssociationChange;
begin
  if Assigned(FOnAssociationChange) then
    FOnAssociationChange(Self);
end;

procedure TStCustomShellNotification.DoDriveAdd(SI : TStShellItem);
begin
  if Assigned(FOnDriveAdd) then
    FOnDriveAdd(Self, SI);
end;

procedure TStCustomShellNotification.DoDriveRemove(SI : TStShellItem);
begin
  if Assigned(FOnDriveRemove) then
    FOnDriveRemove(Self, SI);
end;

procedure TStCustomShellNotification.DoDriveSpaceChange(Drives : DWORD);
begin
  if Assigned(FOnDriveSpaceChange) then
    FOnDriveSpaceChange(Self, Drives);
end;

procedure TStCustomShellNotification.DoFileChange(SI : TStShellItem);
begin
  if Assigned(FOnFileChange) then
    FOnFileChange(Self, SI);
end;

procedure TStCustomShellNotification.DoFileCreate(SI : TStShellItem);
begin
  if Assigned(FOnFileCreate) then
    FOnFileCreate(Self, SI);
end;

procedure TStCustomShellNotification.DoFileDelete(SI1, SI2 : TStShellItem);
begin
  if Assigned(FOnFileDelete) then
    FOnFileDelete(Self, SI1, SI2);
end;

procedure TStCustomShellNotification.DoFileRename(SI1, SI2 : TStShellItem);
begin
  if Assigned(FOnFileRename) then
    FOnFileRename(Self, SI1, SI2);
end;

procedure TStCustomShellNotification.DoFolderCreate(SI : TStShellItem);
begin
  if Assigned(FOnFolderCreate) then
    FOnFolderCreate(Self, SI);
end;

procedure TStCustomShellNotification.DoFolderDelete(SI : TStShellItem);
begin
  if Assigned(FOnFolderDelete) then
    FOnFolderDelete(Self, SI);
end;

procedure TStCustomShellNotification.DoFolderRename(SI1, SI2 : TStShellItem);
begin
  if Assigned(FOnFolderRename) then
    FOnFolderRename(Self, SI1, SI2);
end;

procedure TStCustomShellNotification.DoFolderChange(SI : TStShellItem);
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self, SI);
end;

procedure TStCustomShellNotification.DoImageListChange;
begin
  if Assigned(FOnImageListChange) then
    FOnImageListChange(Self);
end;

procedure TStCustomShellNotification.DoMediaInsert(SI : TStShellItem);
begin
  if Assigned(FOnMediaInsert) then
    FOnMediaInsert(Self, SI);
end;

procedure TStCustomShellNotification.DoMediaRemove(SI : TStShellItem);
begin
  if Assigned(FOnMediaRemove) then
    FOnMediaRemove(Self, SI);
end;

procedure TStCustomShellNotification.DoNetShare(SI : TStShellItem);
begin
  if Assigned(FOnNetShare) then
    FOnNetShare(Self, SI);
end;

procedure TStCustomShellNotification.DoNetUnShare(SI : TStShellItem);
begin
  if Assigned(FOnNetUnShare) then
    FOnNetUnShare(Self, SI);
end;

procedure TStCustomShellNotification.DoServerDisconnect(SI : TStShellItem);
begin
  if Assigned(FOnServerDisconnect) then
    FOnServerDisconnect(Self, SI);
end;

procedure TStCustomShellNotification.DoShellDriveAdd(SI : TStShellItem);
begin
  if Assigned(FOnDriveAdd) then
    FOnDriveAdd(Self, SI);
end;

procedure TStCustomShellNotification.DoAttributesChange(SI1, SI2 : TStShellItem);
begin
  if Assigned(FOnAttributeChange) then
    FOnAttributeChange(Self, SI1, SI2);
end;

procedure TStCustomShellNotification.DoShellChangeNotify(SI1, SI2 : TStShellItem;
  Events : TStNotifyEventsSet);
begin
  if Assigned(FOnShellChangeNotify) then
    FOnShellChangeNotify(Self, SI1, SI2, Events);
end;

procedure TStCustomShellNotification.ItemFromPidl(Pidl : PItemIDList;
  var SI : TStShellItem);
var
  Controller    : TStCustomShellController;
  OwnController : Boolean;
  ParentFolder  : IShellFolder;
  Pidl2         : PItemIDList;
  Path          : string;
begin
  { This component doesn't automatically create a controller but it is }
  { convenient here so we'll create one and use it. }
  if not Assigned(Pidl) then begin
    SI := nil;
    Exit;
  end;
  OwnController := False;
  if (Owner is TStCustomShellListView) then
    Controller := TStCustomShellListView(Owner).Controller
  else if (Owner is TStCustomShellTreeView) then
    Controller := TStCustomShellTreeView(Owner).Controller
  else begin
    Controller := TStCustomShellController.Create(nil);
    OwnController := True;
  end;
  { Pidls from the shell are flakey so try first to create }
  { the pidl using the path. }
  GetParentFolder(Pidl, ParentFolder);
  Pidl2 := ILFindLastID(Pidl);
  Path := '';
  if Assigned(ParentFolder) then
    Path := Controller.GetDisplayName(
      ParentFolder, Pidl2, SHGDN_FORPARSING);
  Pidl2 := nil;
  if Path <> '' then
    PidlFromPath(Path, Controller.DesktopFolder, Pidl2);
  if Assigned(Pidl2) then
    SI := TStShellItem.CreateFromPidl(ILClone(Pidl2), Controller)
  else
    SI := TStShellItem.CreateFromPidl(ILClone(Pidl), Controller);
  if OwnController then
    Controller.Free;
end;

{ TStCustomShellComboBox }

constructor TStCustomShellComboBox.Create(AOwner : TComponent);
var
  VI         : TSsVersionInfo;
  WinDir : array [0..MAX_PATH - 1] of Char;
begin
  inherited Create(AOwner);
  {$IFNDEF VERSION2009}
  if not Assigned(@ILClone) then
    LoadILFunctions;
  {$ENDIF}
  { Base class property defaults }
  Style := csOwnerDrawVariable;

  ShellItems := TStShellFolderList.Create;

  { Get the version of Shell32.dll. Some special folders }
  { require Version 4.71 or higher. }
  VI := TSsVersionInfo.Create(AOwner);
  try
    GetSystemDirectory(WinDir, MAX_PATH);
    VI.FileName := WinDir + '\shell32.dll';
    FShellVersion := VI.FileVersionFloat;
  finally
    VI.Free;
  end;

  { Set up shell notifications. }
  ShellMonitor := TStShellNotification.Create(Self);
  ShellMonitor.WatchSubFolders := False;
  ShellMonitor.NotifyEvents := [neDriveAdd, neDriveRemove, neShellDriveAdd];
  ShellMonitor.SpecialWatchFolder := sfDesktop;
  ShellMonitor.OnShellChangeNotify := ShellEvent;
  ShellMonitor.Active := True;
end;

destructor TStCustomShellComboBox.Destroy;
begin
  FreeAndNil(ShellItems);
  if Assigned(Controller) and OwnController and
      not (csDesigning in ComponentState) then
    FreeAndNil(Controller);
  FreeAndNil(ShellMonitor);
  FreeAndNil(CurrentFolder);
  inherited;
end;

procedure TStCustomShellComboBox.CreateWnd;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    if not Assigned(Controller) then
      if Assigned(FListView) then
        if Assigned(FListView.Controller) then
          Controller := FListView.Controller;
      if not Assigned(Controller) then begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
    if Items.Count = 0 then begin
      if (csDesigning in ComponentState) then
        PostMessage(Handle, MSG_INITCOMBO, 0, 0)
      else
        FillCombo;
    end;
  end;
end;

procedure TStCustomShellComboBox.Loaded;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    if not Assigned(Controller) then
      if Assigned(FListView) and Assigned(FListView.Controller) then
        Controller := FListView.Controller
      else begin
        Controller := TStCustomShellController.Create(Self);
        OwnController := True;
      end;
  end;
end;

procedure TStCustomShellComboBox.Notification(AComponent : TComponent;
  Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FListView) and (Operation = opRemove)
      and (csDesigning in ComponentState) then
    FListView := nil;
end;

procedure TStCustomShellComboBox.Click;
var
  SF : TStShellFolder;
begin
  inherited;
  if DroppedDown then
    Exit;
  DoFolderChanging(CurrentFolder);
  if ItemIndex >= 0 then begin
    SF := TStShellFolder.Create(Controller);
    SF.Assign(ShellItems[ItemIndex]);
    if not Assigned(SF.FParentFolder) then
      SF.FParentFolder := Controller.DesktopFolder;
    if Assigned(FListView) then begin
      FListView.ComboBoxSelChange(SF);
      RefreshView(SF);
    end;
    DoFolderSelected(SF);
    CurrentFolder.Free;
    CurrentFolder := TStShellFolder.Create(nil);
    CurrentFolder.Assign(SF);
    SF.Free;
  end;
end;

procedure TStCustomShellComboBox.DrawItem(Index : Integer; Rect : TRect;
  State : TOwnerDrawState);
var
  {$IFNDEF VERSION5}
  Icon   : TIcon;                                                    {!!.02}
  BM1    : TBitmap;
  BM2    : TBitmap;
  {$ENDIF}
  Offset : Integer;
  SI     : TStShellFolder;
  R      : TRect;
begin
  { Set the item height based on the greater of the size of an }
  { icon or the height of the font. }
  Canvas.Pen.Color := clWindow;
  Canvas.Brush.Color := clWindow;
  R := Rect;
  if (odSelected in State) then
    Canvas.DrawFocusRect(Rect);
  Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  Offset := 5;
  SI := ShellItems[Index];
  {$IFDEF VERSION5}
  if not (odComboBoxEdit in State) then
    Offset := Offset + (SI.Level * 12);
  Inc(Rect.Left, Offset);

  if SI.IsShared then
    Controller.SmallFolderImages.DrawOverlay(Canvas, Rect.Left,
      Rect.Top + ((Rect.Bottom - Rect.Top - 16) div 2),
      SI.IconIndex, SI.OverlayIconIndex)
  else
    Controller.SmallFolderImages.Draw(Canvas, Rect.Left,
      Rect.Top + ((Rect.Bottom - Rect.Top - 16) div 2),                {!!.02}
      SI.IconIndex);                                                   {!!.02}
//    Canvas.Draw(Rect.Left,                                           {!!.02}
//      Rect.Top + ((Rect.Bottom - Rect.Top - 16) div 2), Icon);       {!!.02}
  {$ELSE}
{!!.02 - Moved }
  if Index = CurrentIndex then
    Icon := SI.SmallOpenIcon
  else
    Icon := SI.SmallIcon;
{!!.02 - Moved End}
  { If Rect.Top and Left = 3 then we're drawing the text portion. }
  if (Rect.Top <> 3) and (Rect.Left <> 3) then
    Offset := Offset + (SI.Level * 12);
  { D3 and BCB3 have a fixed icon size of 32 x 32. Further, you can't }
  { use stretch draw to shrink an icon. As such, two TBitmaps are     }
  { required to draw the icons in VCL 3.x. }
  BM1 := TBitmap.Create;
  BM1.Width := 32;
  BM1.Height := 32;
  BM1.Canvas.Draw(0, 0, Icon);
  BM2 := TBitmap.Create;
  BM2.Width := 16;
  BM2.Height := 16;
  BM2.Canvas.StretchDraw(Classes.Rect(0, 0, 16, 16), BM1);
  Inc(Rect.Left, Offset);
  Canvas.Draw(Rect.Left,
    Rect.Top + ((Rect.Bottom - Rect.Top - 16) div 2), BM2);
  BM1.Free;
  BM2.Free;
  {$ENDIF}
  Inc(Rect.Left, (Controller.SmallFolderImages.Width) + 3);            {!!.02}
//  Inc(Rect.Left, (Icon.Width div 2) + 5);                            {!!.02}
  Rect.Right := Rect.Left +
    Canvas.TextWidth(SI.DisplayName) + 2;
  Dec(Rect.Left, 2);
  if (odSelected in State) then
    Canvas.Brush.Color := clHighlight;
  Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  Inc(Rect.Left, 2);
  DrawText(Canvas.Handle, PChar(SI.DisplayName),
    -1, Rect, DT_VCENTER or DT_SINGLELINE);
  if (odSelected in State) then
    Canvas.DrawFocusRect(R);
  Dec(Rect.Left, 2);
  if (odSelected in State) then
    Canvas.DrawFocusRect(Rect);
end;

procedure TStCustomShellComboBox.KeyUp(var Key: Word; Shift: TShiftState);
var
  SF : TStShellFolder;
begin
  inherited;
  if Key = VK_RETURN then begin
    DoFolderChanging(CurrentFolder);
    SF := TStShellFolder.Create(Controller);
    SF.Assign(ShellItems[ItemIndex]);
    if not Assigned(SF.FParentFolder) then
      SF.FParentFolder := Controller.DesktopFolder;
    if Assigned(FListView) then begin
      FListView.ComboBoxSelChange(SF);
      RefreshView(SF);
    end;
    DoFolderSelected(SF);
    CurrentFolder.Free;
    CurrentFolder := TStShellFolder.Create(nil);
    CurrentFolder.Assign(SF);
    SF.Free;
  end;
end;

procedure TStCustomShellComboBox.FillCombo;
var
  Pidl  : PItemIDList;
  I     : Integer;
  Index : Integer;
  SI    : TStShellFolder;
  C     : TCanvas;
begin
  { Set the item height based on the greater of the small icon }
  { height and the font's height + 5. We can't use the combo   }
  { box's canvas because it has not yet been create. }
  ItemHeight := Controller.SmallFolderImages.Height;
  C := TCanvas.Create;
  C.Handle := GetDC(0);
  C.Font.Assign(Font);
  I := C.TextHeight('Aj');
  if ItemHeight < I then
    ItemHeight := I;
  C.Free;
  { Clear the old items. }
  ShellItems.Clear;
  { Get the base list of items. }
  SHGetSpecialFolderLocation(0, ShellFolders[sfDesktop], Pidl);
  Controller.FComboBox := Self;
  SI := TStShellFolder.CreateFromPidl(Pidl, Controller);
  SI.FLevel := 0;
  ShellItems.FList.Add(SI);
  Controller.Enumerate(Pidl, etCombo);
  for I := 0 to Pred(ShellItems.Count) do
    if ShellItems[I].Level = -1 then
      ShellItems[I].Level := 1;
  { Now the get the My Computer items. }
  SHGetSpecialFolderLocation(0, ShellFolders[sfDrives], Pidl);
  Controller.Enumerate(Pidl, etCombo);
  { Set the level for the new items. }
  for I := 0 to Pred(ShellItems.Count) do
    if ShellItems[I].Level = -1 then
      ShellItems[I].Level := 2;
  { Remove the non-file system items under the My Computer   }
  { item but only if the list view's OpenDialogMode property }
  { is true. }
  if Assigned(FListView) then
    if FListView.FOpenDialogMode then begin
      for I := Pred(ShellItems.Count) downto 0 do begin
        SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, Pidl);
        SI := ShellItems[I];
        if ILIsEqual(Pidl, GetParentPidl(SI.FPidl)) then begin
          if not SI.IsFileSystem then begin
            SI.Free;
            ShellItems.FList.Delete(I);
          end;
        end else if ILIsEqual(Pidl, SI.FPidl) then
          { If the current item is My Computer then keep going. }
          Continue
        else begin
          { Want Network Neighborhood but no other non-system items. }
          SHGetSpecialFolderLocation(Handle, CSIDL_NETWORK, Pidl);
          if (not SI.IsFileSystem) and (not ILIsEqual(Pidl, SI.FPidl)) then begin
            SI.Free;
            ShellItems.FList.Delete(I);
          end;
        end;
      end;
    end;
  { Sort the list. }
  TempFolder := Controller.DesktopFolder;
  ShellItems.FList.Sort(ItemPidlSortFuncEx);
  TempFolder := nil;
  { Move the Desktop item to the top of the list. }
  SHGetSpecialFolderLocation(Handle, CSIDL_DESKTOP, Pidl);
  Index := 0;
  for I := 0 to Pred(ShellItems.Count) do begin
    if ILIsEqual(Pidl, ShellItems[I].FPidl) then begin
      Index := I;
      Break;
    end;
  end;
  ShellItems.FList.Move(Index, 0);
  Items.Clear;
  if HandleAllocated then
    for I := 0 to Pred(ShellItems.Count) do
      Items.Add(ShellItems[I].DisplayName);
  if not Assigned(FListView) then begin
    ItemIndex := 0;
    FSelectedFolder := ShellItems[0];
  end;
end;

procedure TStCustomShellComboBox.ListViewFolderChange(
  const SI : TStShellItem; MovingUp : Boolean);
begin
  if MovingUp then begin
    DoFolderChanging(CurrentFolder);
    CurrentIndex := Items.IndexOf(SI.DisplayName);
    ItemIndex := CurrentIndex;
    CurrentFolder.Free;
    CurrentFolder := TStShellFolder.Create(nil);
    CurrentFolder.Assign(TStShellFolder(SI));
  end else if Assigned(SI) then begin
    DoFolderChanging(CurrentFolder);
    DoFolderSelected(TStShellFolder(SI));
    FreeAndNil(CurrentFolder);
    CurrentFolder := TStShellFolder.Create(nil);
    CurrentFolder.Assign(TStShellFolder(SI));
    RefreshView(SI);
    if ItemIndex <> -1 then
      FSelectedFolder := ShellItems[ItemIndex]
    else
      FSelectedFolder := ShellItems[Items.IndexOf(SI.DisplayName)];
  end;
end;

procedure TStCustomShellComboBox.RefreshView(const SI : TStShellItem);
var
  I           : Integer;
  SF          : TStShellFolder;
  InsertIndex : Integer;
  Levels      : Integer;
  Done        : Boolean;
  S           : string;
  Pidl        : PItemIDList;
  AFolder     : IShellFolder;
  ParentStr   : string;
  NetworkStr  : string;
begin
  InsertIndex := 0;
  if not Assigned(SI) then
    Exit;                                                              
  { Remove any items that aren't in the base list. }
  for I := Pred(ShellItems.FList.Count) downto 0 do begin
    SF := ShellItems[I];
    { See if the item clicked one of the base items. }
    if (SI.Path = SF.Path) and                                         
         not SF.ComboItem then
       Exit;
    if SF.ComboItem then begin
      SF.Free;
      ShellItems.FList.Delete(I);
      Items.Delete(I);
    end;
  end;
  { Find out how many levels we need to add. }
  Levels := -1;
  Pidl := ILClone(SI.Pidl);
  while Assigned(Pidl) do begin
    Inc(Levels);
    Pidl := ILGetNext(Pidl);
  end;

  SHGetSpecialFolderLocation(Handle, ShellFolders[sfNetwork], Pidl);
  NetworkStr := Controller.GetDisplayName(
    Controller.DesktopFolder, Pidl, SHGDN_NORMAL);
  ILFree(Pidl);
  { Find the parent folder for this item. }
  Done := False;
  Pidl := ILClone(SI.Pidl);
  while not Done do begin
    { Get a pidl for the parent folder. }
    ILRemoveLastID(Pidl);
    { Get an IShellFolder for this folder's parent. }
    GetParentFolder(Pidl, AFolder);
    if not Assigned(AFolder) then
      AFolder := Controller.DesktopFolder;
    ParentStr := Controller.GetDisplayName(
      AFolder, ILFindLastID(Pidl), SHGDN_NORMAL);
    if ParentStr = '' then
      ParentStr := Controller.GetDisplayName(
        AFolder, Pidl, SHGDN_NORMAL);
    if ParentStr = '' then
      ParentStr := NetworkStr;
    for I := 0 to Pred(Items.Count) do begin
      Done := (ParentStr = Items[I]);
      if Done then begin
        InsertIndex := I + 1;
        Break;
      end;
    end;
  end;

  S := Controller.GetDisplayName(
    Controller.DesktopFolder, nil, SHGDN_NORMAL);
  if ParentStr = S then
    Exit;
  { Work backwards, building a TStShellFolder for each level in the path. }
  Done := False;
  Pidl := ILClone(SI.Pidl);
  while not Done do begin
    { Get a pidl for the parent folder. }
    GetParentFolder(Pidl, AFolder);
    if not Assigned(AFolder) then
      AFolder := Controller.DesktopFolder;
    S := Controller.GetDisplayName(
      AFolder, ILFindLastID(Pidl), SHGDN_NORMAL);
    { Special case for virtual shell folders. }
    if S = '' then
      S := Controller.GetDisplayName(
        AFolder, Pidl, SHGDN_NORMAL);
    if S = '' then
      Break;
    if ParentStr = S then
      Done := True
    else begin
      SF := TStShellFolder.CreateFromPidl(ILClone(Pidl), Controller);
      { Get an IShellFolder for this folder's parent. }
      SF.Level := Levels;
      SF.ComboItem := True;
      ShellItems.FList.Insert(InsertIndex, SF);
      Items.Insert(InsertIndex, SF.DisplayName);
      Done := not ILRemoveLastID(Pidl);
      Dec(Levels);
    end;
  end;
  { Find and select the new item. }
  SelectItem(SI);                                                      
  Repaint;
end;

procedure TStCustomShellComboBox.SetListView(
  const Value : TStCustomShellListView);
begin
  FListView := Value;
  if not Assigned(Value) then
    Exit;
  if (csDesigning in ComponentState) then
    FListView.ComboBox := Self;
  if Assigned(FListView.Controller) then
    Controller := FListView.Controller;
end;

procedure TStCustomShellComboBox.ShellEvent(Sender : TObject; SI1,
  SI2: TStShellItem; Events : TStNotifyEventsSet);
var
  S : string;
  I : Integer;
  Idx : Integer;
begin
  { A drive was added or removed. Reset the list. }
  S := Items[ItemIndex];
  I := ItemIndex;
  FillCombo;
  if Assigned(FListView) then
    RefreshView(FListView.Folder);
  if SI1.DisplayName = S then
    { The drive that was removed was the active drive. }
    ItemIndex := I
  else begin
    Idx := Items.IndexOf(S);
    if Idx <> -1 then begin
      CurrentIndex := Idx;
      ItemIndex := Idx;
    end else
      ItemIndex := I;
  end;
  Click;
end;

procedure TStCustomShellComboBox.WndProc(var Message : TMessage);
var
  I : Integer;
begin
  inherited;
  with Message do begin
    if Msg = WM_CREATE then
      for I := 0 to Pred(ShellItems.Count) do
        Items.Add(ShellItems[I].DisplayName);
    {$IFNDEF VERSION4}
    if Msg = WM_PAINT then
    {$ENDIF}
    if Msg = MSG_INITCOMBO then
      FillCombo;
  end;
end;

procedure TStCustomShellComboBox.DoFolderChanging(SF : TStShellFolder);
begin
  if Assigned(FOnFolderChanging) then
    FOnFolderChanging(Self, SF);
end;

procedure TStCustomShellComboBox.DoFolderSelected(SF : TStShellFolder);
begin
  if Assigned(FOnFolderSelected) then
    FOnFolderSelected(Self, SF);
end;

procedure TStCustomShellComboBox.SelectItem(const SI : TStShellItem);
var
  I     : Integer;
  Index : Integer;
begin
  Index := -1;
  for I := 0 to Pred(ShellItems.Count) do begin
    if SI.Path = ShellItems[I].Path then begin
      Index := I;
      Break;
    end;
  end;
  if Index <> -1 then begin
    CurrentIndex := Index;
    ItemIndex := CurrentIndex;
  end;
end;

procedure TStShellComboBox.DoClick;
begin
  Click;
end;

procedure TStShellTreeView.SetVersion(const Value : string);
begin
end;

function TStShellTreeView.GetVersion : string;
begin
  Result := SsVersionStr;
end;

procedure TStShellListView.SetVersion(const Value : string);
begin
end;

function TStShellListView.GetVersion : string;
begin
  Result := SsVersionStr;
end;

procedure TStShellComboBox.SetVersion(const Value : string);
begin
end;

function TStShellComboBox.GetVersion : string;
begin
  Result := SsVersionStr;
end;


var
  NeedToUninitialize: Boolean;

initialization
  if not IsLibrary then
    NeedToUninitialize := Succeeded(OleInitialize(nil))
  else
    NeedToUninitialize := False;

finalization
  if NeedToUninitialize then
    OleUninitialize;

end.

