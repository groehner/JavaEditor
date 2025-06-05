object FExplorer: TFExplorer
  Left = 315
  Top = 254
  Align = alClient
  Caption = 'Explorer'
  ClientHeight = 385
  ClientWidth = 708
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnPaint = FormPaint
  TextHeight = 15
  object PTop: TPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 23
    Align = alTop
    TabOrder = 0
    object Splitter: TSplitter
      Left = 593
      Top = 1
      Width = 4
      Height = 21
      Beveled = True
      Color = clBtnFace
      ParentColor = False
      OnMoved = SplitterMoved
      ExplicitLeft = 496
      ExplicitTop = 8
      ExplicitHeight = 23
    end
    object PLeft: TPanel
      Left = 1
      Top = 1
      Width = 592
      Height = 21
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object ToolbarExplorer: TSpTBXToolbar
        Left = 0
        Top = 0
        Width = 207
        Height = 21
        Align = alLeft
        Images = vilExplorerLight
        TabOrder = 0
        object TBClose: TSpTBXItem
          Caption = 'Close'
          ImageIndex = 0
          ImageName = 'Close'
          OnClick = TBCloseClick
        end
        object TBBack: TSpTBXItem
          Caption = 'Back'
          ImageIndex = 1
          ImageName = 'Backward'
          OnClick = TBBackClick
        end
        object TBRefresh: TSpTBXItem
          Caption = 'Refresh'
          ImageIndex = 2
          ImageName = 'Refresh'
          OnClick = TBRefreshClick
        end
        object TBFavoritAdd: TSpTBXItem
          Caption = 'Favorit add'
          ImageIndex = 3
          ImageName = 'FavoritAdd'
          OnClick = TBFavoritesAddClick
        end
        object TBFavoritDelete: TSpTBXItem
          Caption = 'Favort delete'
          ImageIndex = 4
          ImageName = 'FavoritDelete'
          OnClick = TBFavoritesDeleteClick
        end
        object TBTreeView: TSpTBXItem
          Caption = 'Tree view'
          ImageIndex = 5
          ImageName = 'TreeView'
          OnClick = TBTreeViewClick
        end
        object TBMoveUp: TSpTBXItem
          Caption = 'Move up'
          ImageIndex = 6
          ImageName = 'MoveUp'
          OnClick = TBMoveUpClick
        end
        object TBNewFolder: TSpTBXItem
          Caption = 'New folder'
          ImageIndex = 7
          ImageName = 'NewFolder'
          OnClick = TBNewFolderClick
        end
        object TBViewMenu: TSpTBXSubmenuItem
          Caption = 'View menu'
          ImageIndex = 8
          ImageName = 'ViewMenu'
          object TBViewLargeIcons: TSpTBXItem
            Caption = 'Large icons'
            GroupIndex = 1
            RadioItem = True
            OnClick = MILargeIconsClick
          end
          object TBViewSmallIcons: TSpTBXItem
            Caption = 'Small icons'
            GroupIndex = 1
            RadioItem = True
            OnClick = MISmallIconsClick
          end
          object TBViewList: TSpTBXItem
            Caption = 'List'
            GroupIndex = 1
            RadioItem = True
            OnClick = MIListClick
          end
          object TBViewDetails: TSpTBXItem
            Caption = 'Details'
            GroupIndex = 1
            RadioItem = True
            OnClick = MIDetailsClick
          end
        end
      end
      object CBFilter: TComboBox
        Left = 207
        Top = 0
        Width = 98
        Height = 23
        Hint = 'Set file filter'
        Align = alLeft
        DropDownCount = 10
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnSelect = CBFilterSelect
        Items.Strings = (
          'Java-Editor'
          'Java (*.java)'
          'GUI (*.jfm)'
          'UML (*.uml)'
          'HTML (*.html)'
          'Project (*.jep)'
          'Text (*.txt)'
          'Jar (*.jar)'
          'Jsp (*.jsp)'
          'All (*.*)')
      end
      object CBFavorites: TComboBox
        Left = 305
        Top = 0
        Width = 287
        Height = 21
        Hint = 'Select favorite'
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        TabStop = False
        OnClick = CBFavoritesClick
        OnKeyDown = CBFavoritesKeyDown
      end
    end
    object PRight: TPanel
      Left = 597
      Top = 1
      Width = 110
      Height = 21
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object PFolder: TPanel
    Left = 0
    Top = 23
    Width = 708
    Height = 362
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterBottom: TSplitter
      Left = 89
      Top = 0
      Width = 4
      Height = 362
      OnMoved = SplitterBottomMoved
      ExplicitHeight = 133
    end
    object PTreeView: TPanel
      Left = 0
      Top = 0
      Width = 89
      Height = 362
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 50
      TabOrder = 0
    end
    object PListView: TPanel
      Left = 93
      Top = 0
      Width = 615
      Height = 362
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object ExplorerTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ExplorerTimerTimer
    Left = 592
    Top = 136
  end
  object vilExplorerLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Backward'
        Name = 'Backward'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 3
        CollectionName = 'FavoritAdd'
        Name = 'FavoritAdd'
      end
      item
        CollectionIndex = 4
        CollectionName = 'FavoritDelete'
        Name = 'FavoritDelete'
      end
      item
        CollectionIndex = 5
        CollectionName = 'TreeView'
        Name = 'TreeView'
      end
      item
        CollectionIndex = 6
        CollectionName = 'MoveUp'
        Name = 'MoveUp'
      end
      item
        CollectionIndex = 7
        CollectionName = 'NewFolder'
        Name = 'NewFolder'
      end
      item
        CollectionIndex = 8
        CollectionName = 'ViewMenu'
        Name = 'ViewMenu'
      end>
    ImageCollection = icExplorer
    Left = 125
    Top = 137
  end
  object vilExplorerDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 9
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Backward'
        Name = 'Backward'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Refresh'
        Name = 'Refresh'
      end
      item
        CollectionIndex = 13
        CollectionName = 'FavoritAdd'
        Name = 'FavoritAdd'
      end
      item
        CollectionIndex = 14
        CollectionName = 'FavoritDelete'
        Name = 'FavoritDelete'
      end
      item
        CollectionIndex = 12
        CollectionName = 'TreeView'
        Name = 'TreeView'
      end
      item
        CollectionIndex = 15
        CollectionName = 'MoveUp'
        Name = 'MoveUp'
      end
      item
        CollectionIndex = 16
        CollectionName = 'NewFolder'
        Name = 'NewFolder'
      end
      item
        CollectionIndex = 8
        CollectionName = 'ViewMenu'
        Name = 'ViewMenu'
      end>
    ImageCollection = icExplorer
    Left = 237
    Top = 137
  end
  object icExplorer: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Backward'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M6 3h1M6' +
          ' 4h1M6 5h1M6 6h1M2 8h1M3 9h1M6 9h7M4 10h1M6 10h1M5 11h2M6 12h1" ' +
          '/>'#13#10'<path stroke="#3360ff" d="M5 4h1M4 5h1M3 6h1M7 6h6M2 7h1M12 ' +
          '7h1M12 8h1" />'#13#10'<path stroke="#97ffff" d="M5 5h1M4 6h2M3 7h9" />' +
          #13#10'<path stroke="#3396ff" d="M3 8h1M5 8h7M4 9h2M5 10h1" />'#13#10'<path' +
          ' stroke="#34c6ff" d="M4 8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Refresh'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#4b4b4b" d="M1 0h11M' +
          '1 1h1M11 1h2M1 2h1M11 2h1M13 2h1M1 3h1M11 3h4M1 4h1M14 4h1M1 5h1' +
          'M14 5h1M1 6h1M14 6h1M1 7h1M14 7h1M1 8h1M14 8h1M1 9h1M14 9h1M1 10' +
          'h1M14 10h1M1 11h1M14 11h1M1 12h1M14 12h1M1 13h1M14 13h1M1 14h1M1' +
          '4 14h1M1 15h14" />'#13#10'<path stroke="#ffffff" d="M2 1h9M2 2h9M12 2h' +
          '1M2 3h5M8 3h3M2 4h5M9 4h5M2 5h3M10 5h4M2 6h2M5 6h2M9 6h5M2 7h2M5' +
          ' 7h2M8 7h6M2 8h2M5 8h5M11 8h3M2 9h5M8 9h2M11 9h3M2 10h4M8 10h2M1' +
          '1 10h3M2 11h3M10 11h4M2 12h4M8 12h6M2 13h5M8 13h6M2 14h12" />'#13#10'<' +
          'path stroke="#4ba34b" d="M7 3h1M7 4h2M5 5h5M4 6h1M7 6h2M4 7h1M7 ' +
          '7h1M4 8h1M10 8h1M7 9h1M10 9h1M6 10h2M10 10h1M5 11h5M6 12h2M7 13h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'FavoritAdd'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M0 0h9M1' +
          '3 0h1M0 1h1M8 1h1M13 1h1M0 2h1M8 2h1M11 2h5M0 3h1M8 3h1M13 3h1M0' +
          ' 4h1M6 4h4M13 4h1M0 5h1M5 5h1M10 5h1M0 6h1M4 6h1M11 6h5M0 7h1M4 ' +
          '7h1M15 7h1M0 8h1M4 8h1M10 8h1M15 8h1M0 9h1M4 9h1M9 9h1M11 9h1M15' +
          ' 9h1M0 10h5M8 10h1M12 10h1M15 10h1M4 11h1M9 11h1M11 11h1M15 11h1' +
          'M4 12h1M10 12h1M15 12h1M4 13h1M15 13h1M4 14h12" />'#13#10'<path stroke' +
          '="#ffffff" d="M1 1h7M1 2h1M4 2h1M7 2h1M1 3h1M4 3h4M1 4h1M4 4h1M1' +
          ' 5h4M6 5h1M1 6h1M1 7h3M5 7h1M7 7h1M9 7h1M11 7h1M13 7h1M1 8h1M1 9' +
          'h3M5 9h1M10 10h1M5 11h1M5 13h1" />'#13#10'<path stroke="#088200" d="M2' +
          ' 2h2M2 3h2M2 4h2" />'#13#10'<path stroke="#0000ff" d="M5 2h2M2 6h2M2 8' +
          'h2" />'#13#10'<path stroke="#828282" d="M5 4h1M10 4h1" />'#13#10'<path strok' +
          'e="#828200" d="M5 6h6" />'#13#10'<path stroke="#ffff00" d="M6 7h1M8 7h' +
          '1M10 7h1M12 7h1M14 7h1M5 8h1M7 8h1M13 8h1M6 9h1M14 9h1M5 10h1M7 ' +
          '10h1M13 10h1M6 11h1M14 11h1M5 12h1M7 12h1M13 12h1M6 13h1M8 13h1M' +
          '10 13h1M12 13h1M14 13h1" />'#13#10'<path stroke="#000082" d="M8 8h1M12' +
          ' 8h1M10 9h1M9 10h1M11 10h1M10 11h1M8 12h1M12 12h1" />'#13#10'</svg>'
      end
      item
        IconName = 'FavoritDelete'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M0 0h9M0' +
          ' 1h1M8 1h1M0 2h1M8 2h1M11 2h5M0 3h1M8 3h1M0 4h1M6 4h4M0 5h1M5 5h' +
          '1M10 5h1M0 6h1M4 6h1M11 6h5M0 7h1M4 7h1M15 7h1M0 8h1M4 8h1M10 8h' +
          '1M15 8h1M0 9h1M4 9h1M9 9h1M11 9h1M15 9h1M0 10h5M8 10h1M12 10h1M1' +
          '5 10h1M4 11h1M9 11h1M11 11h1M15 11h1M4 12h1M10 12h1M15 12h1M4 13' +
          'h1M15 13h1M4 14h12" />'#13#10'<path stroke="#ffffff" d="M1 1h7M1 2h1M4' +
          ' 2h1M7 2h1M1 3h1M4 3h4M1 4h1M4 4h1M1 5h4M6 5h1M1 6h1M1 7h3M5 7h1' +
          'M7 7h1M9 7h1M11 7h1M13 7h1M1 8h1M1 9h3M5 9h1M10 10h1M5 11h1M5 13' +
          'h1" />'#13#10'<path stroke="#088000" d="M2 2h2M2 3h2M2 4h2" />'#13#10'<path ' +
          'stroke="#0000ff" d="M5 2h2M2 6h2M2 8h2" />'#13#10'<path stroke="#82848' +
          '2" d="M5 4h1M10 4h1" />'#13#10'<path stroke="#828000" d="M5 6h6" />'#13#10'<' +
          'path stroke="#ffff00" d="M6 7h1M8 7h1M10 7h1M12 7h1M14 7h1M5 8h1' +
          'M7 8h1M13 8h1M6 9h1M14 9h1M5 10h1M7 10h1M13 10h1M6 11h1M14 11h1M' +
          '5 12h1M7 12h1M13 12h1M6 13h1M8 13h1M10 13h1M12 13h1M14 13h1" />'#13 +
          #10'<path stroke="#000082" d="M8 8h1M12 8h1M10 9h1M9 10h1M11 10h1M1' +
          '0 11h1M8 12h1M12 12h1" />'#13#10'</svg>'
      end
      item
        IconName = 'TreeView'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M2 0h3M2' +
          ' 1h1M4 1h1M2 2h3M3 3h1M3 4h1M6 4h3M3 5h4M8 5h1M3 6h1M6 6h3M3 7h1' +
          'M7 7h1M3 8h1M7 8h1M10 8h3M3 9h1M7 9h4M12 9h1M3 10h1M10 10h3M3 11' +
          'h1M3 12h1M6 12h3M3 13h4M8 13h1M6 14h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'MoveUp'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M3 2h5M2' +
          ' 3h1M8 3h1M1 4h14M1 5h1M15 5h1M1 6h1M6 6h1M15 6h1M1 7h1M5 7h3M15' +
          ' 7h1M1 8h1M4 8h5M15 8h1M1 9h1M6 9h1M15 9h1M1 10h1M6 10h1M15 10h1' +
          'M1 11h1M6 11h6M15 11h1M1 12h1M15 12h1M1 13h1M15 13h1M1 14h15" />' +
          #13#10'<path stroke="#ffff00" d="M3 3h1M5 3h1M7 3h1M3 5h1M5 5h1M7 5h1' +
          'M9 5h1M11 5h1M13 5h1M2 6h1M4 6h1M8 6h1M10 6h1M12 6h1M14 6h1M3 7h' +
          '1M9 7h1M11 7h1M13 7h1M2 8h1M10 8h1M12 8h1M14 8h1M3 9h1M5 9h1M7 9' +
          'h1M9 9h1M11 9h1M13 9h1M2 10h1M4 10h1M8 10h1M10 10h1M12 10h1M14 1' +
          '0h1M3 11h1M5 11h1M13 11h1M2 12h1M4 12h1M6 12h1M8 12h1M10 12h1M12' +
          ' 12h1M14 12h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1M13 13h1" />'#13#10'<' +
          'path stroke="#d2cec6" d="M4 3h1M6 3h1M2 5h1M4 5h1M6 5h1M8 5h1M10' +
          ' 5h1M12 5h1M14 5h1M3 6h1M5 6h1M7 6h1M9 6h1M11 6h1M13 6h1M2 7h1M4' +
          ' 7h1M8 7h1M10 7h1M12 7h1M14 7h1M3 8h1M9 8h1M11 8h1M13 8h1M2 9h1M' +
          '4 9h1M8 9h1M10 9h1M12 9h1M14 9h1M3 10h1M5 10h1M7 10h1M9 10h1M11 ' +
          '10h1M13 10h1M2 11h1M4 11h1M12 11h1M14 11h1M3 12h1M5 12h1M7 12h1M' +
          '9 12h1M11 12h1M13 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h1M12 13' +
          'h1M14 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'NewFolder'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M12 0h1M' +
          '12 2h1M9 3h1M15 3h1M3 4h4M10 4h1M12 4h1M14 4h1M2 5h1M7 5h1M11 5h' +
          '1M13 5h1M1 6h11M14 6h1M1 7h1M12 7h2M1 8h1M12 8h1M14 8h1M1 9h1M12' +
          ' 9h1M15 9h1M1 10h1M12 10h1M1 11h1M12 11h1M1 12h1M12 12h1M1 13h1M' +
          '12 13h1M1 14h12" />'#13#10'<path stroke="#ffff00" d="M3 5h1M5 5h1M3 7h' +
          '1M5 7h1M7 7h1M9 7h1M11 7h1M2 8h1M4 8h1M6 8h1M8 8h1M10 8h1M3 9h1M' +
          '5 9h1M7 9h1M9 9h1M11 9h1M2 10h1M4 10h1M6 10h1M8 10h1M10 10h1M3 1' +
          '1h1M5 11h1M7 11h1M9 11h1M11 11h1M2 12h1M4 12h1M6 12h1M8 12h1M10 ' +
          '12h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1" />'#13#10'<path stroke="#d2c' +
          'ec6" d="M4 5h1M6 5h1M2 7h1M4 7h1M6 7h1M8 7h1M10 7h1M3 8h1M5 8h1M' +
          '7 8h1M9 8h1M11 8h1M2 9h1M4 9h1M6 9h1M8 9h1M10 9h1M3 10h1M5 10h1M' +
          '7 10h1M9 10h1M11 10h1M2 11h1M4 11h1M6 11h1M8 11h1M10 11h1M3 12h1' +
          'M5 12h1M7 12h1M9 12h1M11 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ViewMenu'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M0 1h16M' +
          '0 2h1M15 2h1M0 3h16M0 4h1M15 4h1M0 5h1M5 5h2M12 5h2M15 5h1M0 6h1' +
          'M15 6h1M0 7h1M15 7h1M0 8h1M5 8h2M12 8h2M15 8h1M0 9h1M15 9h1M0 10' +
          'h1M15 10h1M0 11h1M5 11h2M12 11h2M15 11h1M0 12h1M15 12h1M0 13h1M1' +
          '5 13h1M0 14h16" />'#13#10'<path stroke="#00007e" d="M1 2h14M3 5h1M10 5' +
          'h1M2 6h2M9 6h2M3 8h1M10 8h1M2 9h2M9 9h2M3 11h1M10 11h1M2 12h2M9 ' +
          '12h2" />'#13#10'<path stroke="#ffffff" d="M1 4h14M1 5h1M4 5h1M7 5h2M11' +
          ' 5h1M14 5h1M1 6h1M4 6h5M11 6h4M1 7h14M1 8h1M4 8h1M7 8h2M11 8h1M1' +
          '4 8h1M1 9h1M4 9h5M11 9h4M1 10h14M1 11h1M4 11h1M7 11h2M11 11h1M14' +
          ' 11h1M1 12h1M4 12h5M11 12h4M1 13h14" />'#13#10'<path stroke="#7e7e7e" ' +
          'd="M2 5h1M9 5h1M2 8h1M9 8h1M2 11h1" />'#13#10'<path stroke="#d2cec6" d' +
          '="M9 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#ffffff">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Backward'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M6 3h1M6' +
          ' 4h1M6 5h1M6 6h1M2 8h1M3 9h1M6 9h7M4 10h1M6 10h1M5 11h2M6 12h1" ' +
          '/>'#13#10'<path stroke="#3360ff" d="M5 4h1M4 5h1M3 6h1M7 6h6M2 7h1M12 ' +
          '7h1M12 8h1" />'#13#10'<path stroke="#97ffff" d="M5 5h1M4 6h2M3 7h9" />' +
          #13#10'<path stroke="#3396ff" d="M3 8h1M5 8h7M4 9h2M5 10h1" />'#13#10'<path' +
          ' stroke="#34c6ff" d="M4 8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Refresh'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M1 0h11M' +
          '1 1h1M11 1h2M1 2h1M11 2h1M13 2h1M1 3h1M11 3h4M1 4h1M14 4h1M1 5h1' +
          'M14 5h1M1 6h1M14 6h1M1 7h1M14 7h1M1 8h1M14 8h1M1 9h1M14 9h1M1 10' +
          'h1M14 10h1M1 11h1M14 11h1M1 12h1M14 12h1M1 13h1M14 13h1M1 14h1M1' +
          '4 14h1M1 15h14" />'#13#10'<path stroke="#4cff4b" d="M7 3h1M7 4h2M5 5h5' +
          'M4 6h1M7 6h2M4 7h1M7 7h1M4 8h1M10 8h1M7 9h1M10 9h1M6 10h2M10 10h' +
          '1M5 11h5M6 12h2M7 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'TreeView'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M2 0h3M2' +
          ' 1h1M4 1h1M2 2h3M3 3h1M3 4h1M6 4h3M3 5h4M8 5h1M3 6h1M6 6h3M3 7h1' +
          'M7 7h1M3 8h1M7 8h1M10 8h3M3 9h1M7 9h4M12 9h1M3 10h1M10 10h3M3 11' +
          'h1M3 12h1M6 12h3M3 13h4M8 13h1M6 14h3" />'#13#10'</svg>'
      end
      item
        IconName = 'FavoritAdd'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M0 0h9M1' +
          '3 0h1M0 1h1M8 1h1M13 1h1M0 2h1M8 2h1M11 2h5M0 3h1M8 3h1M13 3h1M0' +
          ' 4h1M6 4h4M13 4h1M0 5h1M5 5h1M10 5h1M0 6h1M4 6h1M11 6h5M0 7h1M4 ' +
          '7h1M15 7h1M0 8h1M4 8h1M10 8h1M15 8h1M0 9h1M4 9h1M9 9h1M11 9h1M15' +
          ' 9h1M0 10h5M8 10h1M12 10h1M15 10h1M4 11h1M9 11h1M11 11h1M15 11h1' +
          'M4 12h1M10 12h1M15 12h1M4 13h1M15 13h1M4 14h12" />'#13#10'<path stroke' +
          '="#ffffff" d="M1 1h7M1 2h1M4 2h1M7 2h1M1 3h1M4 3h4M1 4h1M4 4h1M1' +
          ' 5h4M6 5h1M1 6h1M1 7h3M5 7h1M7 7h1M9 7h1M11 7h1M13 7h1M1 8h1M1 9' +
          'h3M5 9h1M10 10h1M5 11h1M5 13h1" />'#13#10'<path stroke="#088200" d="M2' +
          ' 2h2M2 3h2M2 4h2" />'#13#10'<path stroke="#0000ff" d="M5 2h2M2 6h2M2 8' +
          'h2" />'#13#10'<path stroke="#828282" d="M5 4h1M10 4h1" />'#13#10'<path strok' +
          'e="#828200" d="M5 6h6" />'#13#10'<path stroke="#ffff00" d="M6 7h1M8 7h' +
          '1M10 7h1M12 7h1M14 7h1M5 8h1M7 8h1M13 8h1M6 9h1M14 9h1M5 10h1M7 ' +
          '10h1M13 10h1M6 11h1M14 11h1M5 12h1M7 12h1M13 12h1M6 13h1M8 13h1M' +
          '10 13h1M12 13h1M14 13h1" />'#13#10'<path stroke="#000082" d="M8 8h1M12' +
          ' 8h1M10 9h1M9 10h1M11 10h1M10 11h1M8 12h1M12 12h1" />'#13#10'</svg>'
      end
      item
        IconName = 'FavoritDelete'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M0 0h9M0' +
          ' 1h1M8 1h1M0 2h1M8 2h1M11 2h5M0 3h1M8 3h1M0 4h1M6 4h4M0 5h1M5 5h' +
          '1M10 5h1M0 6h1M4 6h1M11 6h5M0 7h1M4 7h1M15 7h1M0 8h1M4 8h1M10 8h' +
          '1M15 8h1M0 9h1M4 9h1M9 9h1M11 9h1M15 9h1M0 10h5M8 10h1M12 10h1M1' +
          '5 10h1M4 11h1M9 11h1M11 11h1M15 11h1M4 12h1M10 12h1M15 12h1M4 13' +
          'h1M15 13h1M4 14h12" />'#13#10'<path stroke="#ffffff" d="M1 1h7M1 2h1M4' +
          ' 2h1M7 2h1M1 3h1M4 3h4M1 4h1M4 4h1M1 5h4M6 5h1M1 6h1M1 7h3M5 7h1' +
          'M7 7h1M9 7h1M11 7h1M13 7h1M1 8h1M1 9h3M5 9h1M10 10h1M5 11h1M5 13' +
          'h1" />'#13#10'<path stroke="#088000" d="M2 2h2M2 3h2M2 4h2" />'#13#10'<path ' +
          'stroke="#0000ff" d="M5 2h2M2 6h2M2 8h2" />'#13#10'<path stroke="#82848' +
          '2" d="M5 4h1M10 4h1" />'#13#10'<path stroke="#828000" d="M5 6h6" />'#13#10'<' +
          'path stroke="#ffff00" d="M6 7h1M8 7h1M10 7h1M12 7h1M14 7h1M5 8h1' +
          'M7 8h1M13 8h1M6 9h1M14 9h1M5 10h1M7 10h1M13 10h1M6 11h1M14 11h1M' +
          '5 12h1M7 12h1M13 12h1M6 13h1M8 13h1M10 13h1M12 13h1M14 13h1" />'#13 +
          #10'<path stroke="#000082" d="M8 8h1M12 8h1M10 9h1M9 10h1M11 10h1M1' +
          '0 11h1M8 12h1M12 12h1" />'#13#10'</svg>'
      end
      item
        IconName = 'MoveUp'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M3 2h5M2' +
          ' 3h1M8 3h1M1 4h14M1 5h1M15 5h1M1 6h1M6 6h1M15 6h1M1 7h1M5 7h3M15' +
          ' 7h1M1 8h1M4 8h5M15 8h1M1 9h1M6 9h1M15 9h1M1 10h1M6 10h1M15 10h1' +
          'M1 11h1M6 11h6M15 11h1M1 12h1M15 12h1M1 13h1M15 13h1M1 14h15" />' +
          #13#10'<path stroke="#ffff00" d="M3 3h1M5 3h1M7 3h1M3 5h1M5 5h1M7 5h1' +
          'M9 5h1M11 5h1M13 5h1M2 6h1M4 6h1M8 6h1M10 6h1M12 6h1M14 6h1M3 7h' +
          '1M9 7h1M11 7h1M13 7h1M2 8h1M10 8h1M12 8h1M14 8h1M3 9h1M5 9h1M7 9' +
          'h1M9 9h1M11 9h1M13 9h1M2 10h1M4 10h1M8 10h1M10 10h1M12 10h1M14 1' +
          '0h1M3 11h1M5 11h1M13 11h1M2 12h1M4 12h1M6 12h1M8 12h1M10 12h1M12' +
          ' 12h1M14 12h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1M13 13h1" />'#13#10'<' +
          'path stroke="#d2cec6" d="M4 3h1M6 3h1M2 5h1M4 5h1M6 5h1M8 5h1M10' +
          ' 5h1M12 5h1M14 5h1M3 6h1M5 6h1M7 6h1M9 6h1M11 6h1M13 6h1M2 7h1M4' +
          ' 7h1M8 7h1M10 7h1M12 7h1M14 7h1M3 8h1M9 8h1M11 8h1M13 8h1M2 9h1M' +
          '4 9h1M8 9h1M10 9h1M12 9h1M14 9h1M3 10h1M5 10h1M7 10h1M9 10h1M11 ' +
          '10h1M13 10h1M2 11h1M4 11h1M12 11h1M14 11h1M3 12h1M5 12h1M7 12h1M' +
          '9 12h1M11 12h1M13 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h1M12 13' +
          'h1M14 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'NewFolder'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M12 0h1M' +
          '12 2h1M9 3h1M15 3h1M3 4h4M10 4h1M12 4h1M14 4h1M2 5h1M7 5h1M11 5h' +
          '1M13 5h1M1 6h11M14 6h1M1 7h1M12 7h2M1 8h1M12 8h1M14 8h1M1 9h1M12' +
          ' 9h1M15 9h1M1 10h1M12 10h1M1 11h1M12 11h1M1 12h1M12 12h1M1 13h1M' +
          '12 13h1M1 14h12" />'#13#10'<path stroke="#ffff00" d="M3 5h1M5 5h1M3 7h' +
          '1M5 7h1M7 7h1M9 7h1M11 7h1M2 8h1M4 8h1M6 8h1M8 8h1M10 8h1M3 9h1M' +
          '5 9h1M7 9h1M9 9h1M11 9h1M2 10h1M4 10h1M6 10h1M8 10h1M10 10h1M3 1' +
          '1h1M5 11h1M7 11h1M9 11h1M11 11h1M2 12h1M4 12h1M6 12h1M8 12h1M10 ' +
          '12h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1" />'#13#10'<path stroke="#d2c' +
          'ec6" d="M4 5h1M6 5h1M2 7h1M4 7h1M6 7h1M8 7h1M10 7h1M3 8h1M5 8h1M' +
          '7 8h1M9 8h1M11 8h1M2 9h1M4 9h1M6 9h1M8 9h1M10 9h1M3 10h1M5 10h1M' +
          '7 10h1M9 10h1M11 10h1M2 11h1M4 11h1M6 11h1M8 11h1M10 11h1M3 12h1' +
          'M5 12h1M7 12h1M9 12h1M11 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h' +
          '1" />'#13#10'</svg>'#13#10
      end>
    Left = 352
    Top = 136
  end
  object PMViewMenu: TSpTBXPopupMenu
    Left = 477
    Top = 137
    object MILargeIcons: TSpTBXItem
      Caption = 'Large icons'
      RadioItem = True
      OnClick = MILargeIconsClick
    end
    object MISmallIcons: TSpTBXItem
      Caption = 'Small icons'
      RadioItem = True
      OnClick = MISmallIconsClick
    end
    object MILIst: TSpTBXItem
      Caption = 'List'
      RadioItem = True
      OnClick = MIListClick
    end
    object MIDetails: TSpTBXItem
      Caption = 'Details'
      RadioItem = True
      OnClick = MIDetailsClick
    end
  end
end
