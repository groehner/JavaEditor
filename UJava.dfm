object FJava: TFJava
  Left = 117
  Top = 541
  Cursor = crHandPoint
  HelpType = htKeyword
  VertScrollBar.Visible = False
  Caption = 'Java-Editor'
  ClientHeight = 504
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  ScreenSnap = True
  ShowHint = True
  Visible = True
  WindowState = wsMaximized
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  TextHeight = 15
  object HSplitter: TSplitter
    Left = 0
    Top = 500
    Width = 851
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    Color = clActiveCaption
    MinSize = 80
    ParentColor = False
    Visible = False
    OnMoved = HSplitterMoved
    ExplicitTop = 563
    ExplicitWidth = 858
  end
  object VSplitter: TSplitter
    Left = 848
    Top = 78
    Height = 422
    Align = alRight
    Color = clActiveCaption
    ParentColor = False
    OnMoved = VSplitterMoved
    ExplicitLeft = 1224
    ExplicitTop = 360
    ExplicitHeight = 100
  end
  object BottomDockPanel: TPanel
    Left = 0
    Top = 503
    Width = 851
    Height = 1
    Align = alBottom
    DockSite = True
    DragKind = dkDock
    TabOrder = 0
    OnDockDrop = DockPanelDockDrop
    OnDockOver = BottomDockPanelDockOver
    OnGetSiteInfo = BottomDockPanelGetSiteInfo
    OnStartDock = BottomDockPanelStartDock
    OnUnDock = BottomDockPanelUnDock
  end
  object RightDockPanel: TPanel
    Left = 851
    Top = 78
    Width = 0
    Height = 422
    Align = alRight
    DockSite = True
    DragKind = dkDock
    TabOrder = 1
    OnDockDrop = DockPanelDockDrop
    OnDockOver = RightDockPanelDockOver
    OnGetSiteInfo = RightDockPanelGetSiteInfo
    OnUnDock = RightDockPanelUnDock
  end
  object MainPanel: TPanel
    Left = 0
    Top = 78
    Width = 848
    Height = 422
    Align = alClient
    TabOrder = 2
  end
  object TBXDockTop: TSpTBXDockablePanel
    Left = 0
    Top = 0
    Width = 851
    Height = 78
    Align = alTop
    DockMode = dmCannotFloatOrChangeDocks
    DockPos = 0
    ParentShowHint = False
    ShowHint = False
    TabOrder = 3
    Options.Close = False
    Options.TitleBarMaxSize = 0
    ShowCaption = False
    ShowCaptionWhenDocked = False
    ExplicitWidth = 855
    object ControlBar: TSpTBXPanel
      Left = 0
      Top = 21
      Width = 851
      Height = 57
      Caption = 'ControlBar'
      Align = alClient
      Constraints.MaxHeight = 59
      TabOrder = 1
      ExplicitWidth = 624
      object DebugToolbar: TSpTBXToolbar
        Left = 3
        Top = 28
        Width = 161
        Height = 26
        DockMode = dmCannotFloatOrChangeDocks
        DockPos = 0
        DockRow = 2
        Images = vilToolbarLight
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object TBRedo: TSpTBXItem
          Hint = 'Redo'
          ImageIndex = 7
          ImageName = '07'
          OnClick = MIRedoClick
          Top = 28
        end
        object TBUndo: TSpTBXItem
          Hint = 'Undo'
          Enabled = False
          ImageIndex = 8
          ImageName = '08'
          OnClick = MIUndoClick
          Left = 23
          Top = 28
        end
        object TBStep: TSpTBXItem
          Hint = 'Step'
          ImageIndex = 9
          ImageName = '09'
          OnClick = MIStepClick
          Left = 46
          Top = 28
        end
        object TBNext: TSpTBXItem
          Hint = 'Step over'
          ImageIndex = 10
          ImageName = '10'
          OnClick = MINextClick
          Left = 69
          Top = 28
        end
        object TBCompileJava: TSpTBXItem
          Hint = 'Compile'
          ImageIndex = 11
          ImageName = '11'
          OnClick = TBCompileJavaClick
          Left = 92
          Top = 28
        end
        object TBCompileAll: TSpTBXItem
          Hint = 'Compile all'
          ImageIndex = 12
          ImageName = '12'
          OnClick = TBCompileAllClick
          Left = 115
          Top = 28
        end
        object TBRun: TSpTBXItem
          Hint = 'Run'
          ImageIndex = 13
          ImageName = '13'
          OnClick = TBRunClick
          Left = 138
          Top = 28
        end
      end
      object PBorder: TPanel
        Left = 765
        Top = 4
        Width = 46
        Height = 57
        BevelOuter = bvNone
        TabOrder = 1
        object SBNorth: TSpeedButton
          Left = 0
          Top = 0
          Width = 39
          Height = 17
          AllowAllUp = True
          Constraints.MaxHeight = 17
          Constraints.MinHeight = 17
          GroupIndex = 2
          Caption = 'North'
          Flat = True
          ParentShowHint = False
          ShowHint = True
        end
        object SBSouth: TSpeedButton
          Left = 0
          Top = 33
          Width = 39
          Height = 17
          AllowAllUp = True
          Constraints.MaxHeight = 17
          Constraints.MinHeight = 17
          GroupIndex = 2
          Caption = 'South'
          Flat = True
          ParentShowHint = False
          ShowHint = True
        end
        object SBWest: TSpeedButton
          Left = 0
          Top = 13
          Width = 13
          Height = 22
          Hint = 'West'
          AllowAllUp = True
          Constraints.MaxHeight = 22
          Constraints.MinHeight = 22
          GroupIndex = 2
          Caption = 'W'
          Flat = True
          ParentShowHint = False
          ShowHint = True
        end
        object SBEast: TSpeedButton
          Left = 26
          Top = 13
          Width = 13
          Height = 22
          Hint = 'East'
          AllowAllUp = True
          Constraints.MaxHeight = 22
          Constraints.MinHeight = 22
          GroupIndex = 2
          Caption = 'E'
          Flat = True
          ParentShowHint = False
          ShowHint = True
        end
        object SBCenter: TSpeedButton
          Left = 13
          Top = 13
          Width = 13
          Height = 22
          Hint = 'Center'
          AllowAllUp = True
          Constraints.MaxHeight = 22
          Constraints.MinHeight = 22
          GroupIndex = 2
          Caption = 'C'
          Flat = True
          ParentShowHint = False
          ShowHint = True
        end
      end
      object TabsControl: TSpTBXTabControl
        Left = 166
        Top = 1
        Width = 595
        Height = 55
        Color = clBtnFace
        ActiveTabIndex = 6
        TabAutofitMaxSize = 80
        HiddenItems = <>
        object TabProgram: TSpTBXTabItem
          Caption = 'Program'
          CustomWidth = 65
        end
        object TabAWT: TSpTBXTabItem
          Caption = 'AWT'
          CustomWidth = 40
        end
        object TabSwing1: TSpTBXTabItem
          Caption = 'Swing1'
          CustomWidth = 60
        end
        object TabSwing2: TSpTBXTabItem
          Caption = 'Swing2'
          CustomWidth = 60
        end
        object TabLayout: TSpTBXTabItem
          Caption = 'Layout'
          CustomWidth = 60
        end
        object TabUtilities: TSpTBXTabItem
          Caption = 'Utilities'
          CustomWidth = 60
        end
        object TabFXBase: TSpTBXTabItem
          Caption = 'FX Base'
          Checked = True
          CustomWidth = 60
        end
        object TabFXControls: TSpTBXTabItem
          Caption = 'FX Controls'
          CustomWidth = 80
        end
        object TabFXShapes: TSpTBXTabItem
          Caption = 'FX Shapes'
          CustomWidth = 65
        end
        object TSAWT: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'AWT'
          ImageIndex = -1
          TabItem = 'TabAWT'
          object ToolbarAWT: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilAWTLight
            TabOrder = 0
            Wrapable = False
            object TBLabel: TToolButton
              Tag = -1
              Left = 0
              Top = 0
              Hint = 'Label'
              Grouped = True
              ImageIndex = 0
              ImageName = 'Label'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBTextField: TToolButton
              Tag = -2
              Left = 28
              Top = 0
              Hint = 'TextField'
              Grouped = True
              ImageIndex = 1
              ImageName = 'TextField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBNumberField: TToolButton
              Tag = -21
              Left = 56
              Top = 0
              Hint = 'NumberField'
              Grouped = True
              ImageIndex = 2
              ImageName = 'NumberField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBTextArea: TToolButton
              Tag = -3
              Left = 84
              Top = 0
              Hint = 'TextArea'
              Grouped = True
              ImageIndex = 3
              ImageName = 'TextArea'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBButton: TToolButton
              Tag = -4
              Left = 112
              Top = 0
              Hint = 'Button'
              Grouped = True
              ImageIndex = 4
              ImageName = 'Button'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBCheckbox: TToolButton
              Tag = -5
              Left = 140
              Top = 0
              Hint = 'Checkbox'
              Grouped = True
              ImageIndex = 5
              ImageName = 'Checkbox'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBCheckboxGroup: TToolButton
              Tag = -50
              Left = 168
              Top = 0
              Hint = 'CheckboxGroup'
              Grouped = True
              ImageIndex = 6
              ImageName = 'CheckboxGroup'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBList: TToolButton
              Tag = -8
              Left = 196
              Top = 0
              Hint = 'List'
              Grouped = True
              ImageIndex = 7
              ImageName = 'List'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBChoice: TToolButton
              Tag = -9
              Left = 224
              Top = 0
              Hint = 'Choice'
              Grouped = True
              ImageIndex = 8
              ImageName = 'Choice'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBScrollbar: TToolButton
              Tag = -10
              Left = 252
              Top = 0
              Hint = 'Scrollbar'
              Grouped = True
              ImageIndex = 9
              ImageName = 'Scrollbar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBScrollPane: TToolButton
              Tag = -11
              Left = 280
              Top = 0
              Hint = 'ScrollPane'
              Grouped = True
              ImageIndex = 10
              ImageName = 'ScrollPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBPanel: TToolButton
              Tag = -12
              Left = 308
              Top = 0
              Hint = 'Panel - right-click for a subclass of Panel'
              Grouped = True
              ImageIndex = 11
              ImageName = 'Panel'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnMouseUp = TBPanelCanvasMouseUp
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBCanvas: TToolButton
              Tag = -13
              Left = 336
              Top = 0
              Hint = 'Canvas - right-click for a subclass of Canvas'
              Grouped = True
              ImageIndex = 12
              ImageName = 'Canvas'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnMouseUp = TBPanelCanvasMouseUp
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBATurtle: TToolButton
              Tag = -14
              Left = 364
              Top = 0
              Hint = 'Turtle'
              Grouped = True
              ImageIndex = 13
              ImageName = 'Turtle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBMenuBar: TToolButton
              Tag = -52
              Left = 392
              Top = 0
              Hint = 'MenuBar'
              Grouped = True
              ImageIndex = 14
              ImageName = 'MenuBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBMenu: TToolButton
              Tag = -43
              Left = 420
              Top = 0
              Hint = 'Menu'
              Grouped = True
              ImageIndex = 15
              ImageName = 'Menu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBPopupMenu: TToolButton
              Tag = -44
              Left = 448
              Top = 0
              Hint = 'PopupMenu'
              Grouped = True
              ImageIndex = 16
              ImageName = 'PopupMenu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSLayout: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'Layout'
          ImageIndex = -1
          TabItem = 'TabLayout'
          object ToolBarLayout: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 26
            Customizable = True
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilLayoutLight
            TabOrder = 0
            object TBBorderLayout: TToolButton
              Left = 0
              Top = 0
              Hint = 'BorderLayout'
              Down = True
              Grouped = True
              ImageIndex = 0
              ImageName = 'BorderLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
            object TBFlowLayout: TToolButton
              Tag = 1
              Left = 26
              Top = 0
              Hint = 'FlowLayout'
              Grouped = True
              ImageIndex = 1
              ImageName = 'FlowLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
            object TBGridLayout: TToolButton
              Tag = 2
              Left = 52
              Top = 0
              Hint = 'GridLayout'
              Grouped = True
              ImageIndex = 4
              ImageName = 'GridLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
            object TBCardLayout: TToolButton
              Tag = 3
              Left = 78
              Top = 0
              Hint = 'CardLayout'
              Grouped = True
              ImageIndex = 2
              ImageName = 'CardLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
            object TBGridBagLayout: TToolButton
              Tag = 4
              Left = 104
              Top = 0
              Hint = 'GridBagLayout'
              Grouped = True
              ImageIndex = 3
              ImageName = 'GridBagLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
            object TBAbsoluteLayout: TToolButton
              Tag = 5
              Left = 130
              Top = 0
              Hint = 'AbsoluteLayout'
              Grouped = True
              ImageIndex = 5
              ImageName = 'AbsoluteLayout'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBLayoutClick
            end
          end
        end
        object TSFXShapes: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'FX Shapes'
          ImageIndex = -1
          TabItem = 'TabFXShapes'
          object ToolBarFXShapes: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilFXShapesLight
            TabOrder = 0
            object TBFXCircle: TToolButton
              Tag = 161
              Left = 0
              Top = 0
              Hint = 'Circle'
              Grouped = True
              ImageIndex = 0
              ImageName = 'Circle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXRectangle: TToolButton
              Tag = 162
              Left = 28
              Top = 0
              Hint = 'Rectangle'
              Grouped = True
              ImageIndex = 1
              ImageName = 'Rectangle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXEllipse: TToolButton
              Tag = 163
              Left = 56
              Top = 0
              Hint = 'Ellipse'
              Grouped = True
              ImageIndex = 2
              ImageName = 'Ellipse'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXPolygon: TToolButton
              Tag = 164
              Left = 84
              Top = 0
              Hint = 'Polygon'
              Grouped = True
              ImageIndex = 3
              ImageName = 'Polygon'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXPolyline: TToolButton
              Tag = 165
              Left = 112
              Top = 0
              Hint = 'Polyline'
              Grouped = True
              ImageIndex = 4
              ImageName = 'Polyline'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXArc: TToolButton
              Tag = 166
              Left = 140
              Top = 0
              Hint = 'Arc'
              Grouped = True
              ImageIndex = 5
              ImageName = 'Arc'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXLine: TToolButton
              Tag = 167
              Left = 168
              Top = 0
              Hint = 'Line'
              Grouped = True
              ImageIndex = 6
              ImageName = 'Line1'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXText: TToolButton
              Tag = 168
              Left = 196
              Top = 0
              Hint = 'Text'
              Grouped = True
              ImageIndex = 7
              ImageName = 'Text'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXQuadCurve: TToolButton
              Tag = 169
              Left = 224
              Top = 0
              Hint = 'QuadCurve'
              Grouped = True
              ImageIndex = 8
              ImageName = 'QuadCurve'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXCubicCurve: TToolButton
              Tag = 170
              Left = 252
              Top = 0
              Hint = 'CubicCurve'
              Grouped = True
              ImageIndex = 9
              ImageName = 'CubicCurve'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXSVGPath: TToolButton
              Tag = 171
              Left = 280
              Top = 0
              Hint = 'SVGPath'
              Grouped = True
              ImageIndex = 10
              ImageName = 'SVGPath'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSUtilities: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'Utilities'
          ImageIndex = -1
          TabItem = 'TabUtilities'
          object ToolBarUtilities: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilUtilities
            TabOrder = 0
            Wrapable = False
            object TBPlayground: TToolButton
              Tag = 33
              Left = 0
              Top = 0
              Hint = 'Turtle playground'
              Grouped = True
              ImageIndex = 0
              ImageName = 'TurtleWorld'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBTurtle: TToolButton
              Tag = 34
              Left = 28
              Top = 0
              Hint = 'Turtle'
              Grouped = True
              ImageIndex = 1
              ImageName = 'Turtle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBTimer: TToolButton
              Tag = 49
              Left = 56
              Top = 0
              Hint = 'Timer'
              Grouped = True
              ImageIndex = 2
              ImageName = 'Timer'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSProgram: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'Program'
          ImageIndex = -1
          TabItem = 'TabProgram'
          object ToolbarProgram: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 30
            Color = clBtnFace
            Images = vilProgramLight
            ParentColor = False
            TabOrder = 0
            object TBNew: TToolButton
              Left = 0
              Top = 0
              Hint = 'New'
              ImageIndex = 0
              ImageName = 'New1'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBNewClick
            end
            object TBClass: TToolButton
              Left = 30
              Top = 0
              Hint = 'New class'
              ImageIndex = 1
              ImageName = 'NewClass'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBClassClick
            end
            object TBStructogram: TToolButton
              Left = 60
              Top = 0
              Hint = 'New structogram'
              ImageIndex = 2
              ImageName = 'Structogram1'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBStructogramClick
            end
            object TBSequence: TToolButton
              Left = 90
              Top = 0
              Hint = 'New sequence diagram'
              ImageIndex = 3
              ImageName = 'Sequencediagram'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBSequenceClick
            end
            object TBConsole: TToolButton
              Tag = 1
              Left = 120
              Top = 0
              Hint = 'Console'
              ImageIndex = 4
              ImageName = 'Console'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBProgramClick
            end
            object TBFrame: TToolButton
              Tag = 2
              Left = 150
              Top = 0
              Hint = 'Frame'
              ImageIndex = 5
              ImageName = 'AWT'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBDialog: TToolButton
              Tag = 3
              Left = 180
              Top = 0
              Hint = 'Dialog'
              ImageIndex = 6
              ImageName = 'AWTDialog'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBApplet: TToolButton
              Tag = 4
              Left = 210
              Top = 0
              Hint = 'Applet'
              ImageIndex = 7
              ImageName = 'AWTApplet'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBJFrame: TToolButton
              Tag = 5
              Left = 240
              Top = 0
              Hint = 'JFrame'
              ImageIndex = 8
              ImageName = 'Swing'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBJDialog: TToolButton
              Tag = 6
              Left = 270
              Top = 0
              Hint = 'JDialog'
              ImageIndex = 9
              ImageName = 'SwingDialog'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBJApplet: TToolButton
              Tag = 7
              Left = 300
              Top = 0
              Hint = 'JApplet'
              ImageIndex = 10
              ImageName = 'SwingApplet'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
            object TBApplication: TToolButton
              Tag = 8
              Left = 330
              Top = 0
              Hint = 'Application'
              ImageIndex = 11
              ImageName = 'JavaFX'
              ParentShowHint = False
              ShowHint = True
              OnClick = SBGUIFrameClick
            end
          end
        end
        object TSSwing2: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'Swing2'
          ImageIndex = -1
          TabItem = 'TabSwing2'
          object ToolbarSwing2: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilSwing2
            TabOrder = 0
            Wrapable = False
            object TBJSlider: TToolButton
              Tag = 15
              Left = 0
              Top = 0
              Hint = 'JSlider'
              Grouped = True
              ImageIndex = 0
              ImageName = 'JSlider'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJProgressBar: TToolButton
              Tag = 16
              Left = 28
              Top = 0
              Hint = 'JProgressBar'
              Grouped = True
              ImageIndex = 1
              ImageName = 'JProgressBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJSplitPane: TToolButton
              Tag = 17
              Left = 56
              Top = 0
              Hint = 'JSplitPane'
              Grouped = True
              ImageIndex = 2
              ImageName = 'JSplitPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTabbedPane: TToolButton
              Tag = 18
              Left = 84
              Top = 0
              Hint = 'JTabbedPane'
              Grouped = True
              ImageIndex = 3
              ImageName = 'JTabbedPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTable: TToolButton
              Tag = 19
              Left = 112
              Top = 0
              Hint = 'JTable'
              Grouped = True
              ImageIndex = 4
              ImageName = 'JTable'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTree: TToolButton
              Tag = 20
              Left = 140
              Top = 0
              Hint = 'JTree'
              Grouped = True
              ImageIndex = 5
              ImageName = 'JTree'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJToolBar: TToolButton
              Tag = 23
              Left = 168
              Top = 0
              Hint = 'JToolBar'
              Grouped = True
              ImageIndex = 6
              ImageName = 'JToolBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TJSeparator: TToolButton
              Tag = 24
              Left = 196
              Top = 0
              Hint = 'JSeparator'
              Grouped = True
              ImageIndex = 7
              ImageName = 'JSeparator'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TJToggleButton: TToolButton
              Tag = 25
              Left = 224
              Top = 0
              Hint = 'JToggleButton'
              Grouped = True
              ImageIndex = 8
              ImageName = 'JToggleButton'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJPasswordField: TToolButton
              Tag = 26
              Left = 252
              Top = 0
              Hint = 'JPasswordField'
              Grouped = True
              ImageIndex = 9
              ImageName = 'JPasswordField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJFormattedTextField: TToolButton
              Tag = 27
              Left = 280
              Top = 0
              Hint = 'JFormattedTextField'
              Grouped = True
              ImageIndex = 10
              ImageName = 'JFormattedTextField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJEditorPane: TToolButton
              Tag = 28
              Left = 308
              Top = 0
              Hint = 'JEditorPane'
              Grouped = True
              ImageIndex = 11
              ImageName = 'JEditorPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTextPane: TToolButton
              Tag = 29
              Left = 336
              Top = 0
              Hint = 'JTextPane'
              Grouped = True
              ImageIndex = 12
              ImageName = 'JTextPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJLayeredPane: TToolButton
              Tag = 30
              Left = 364
              Top = 0
              Hint = 'JLayeredPane'
              Grouped = True
              ImageIndex = 13
              ImageName = 'JLayeredPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJDesktopPane: TToolButton
              Tag = 31
              Left = 392
              Top = 0
              Hint = 'JDesktopPane'
              Grouped = True
              ImageIndex = 14
              ImageName = 'JDesktopPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TJInternalFrame: TToolButton
              Tag = 32
              Left = 420
              Top = 0
              Hint = 'JInternalFrame'
              Grouped = True
              ImageIndex = 15
              ImageName = 'JInternalFrame'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJFileOpen: TToolButton
              Tag = 45
              Left = 448
              Top = 0
              Hint = 'JFileChooser|Open'
              Grouped = True
              ImageIndex = 17
              ImageName = 'JFileOpen'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJFileSave: TToolButton
              Tag = 46
              Left = 476
              Top = 0
              Hint = 'JFileChooser|Save'
              Grouped = True
              ImageIndex = 16
              ImageName = 'JFileSave'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJColorChooser: TToolButton
              Tag = 47
              Left = 504
              Top = 0
              Hint = 'JColorChooser'
              Grouped = True
              ImageIndex = 18
              ImageName = 'JColorChooser'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJOptionPane: TToolButton
              Tag = 48
              Left = 532
              Top = 0
              Hint = 'JOptionPane'
              Grouped = True
              ImageIndex = 19
              ImageName = 'JOptionPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSSwing1: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'Swing1'
          ImageIndex = -1
          TabItem = 'TabSwing1'
          object ToolBarSwing1: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilSwing1Light
            TabOrder = 0
            Wrapable = False
            object TBJLabel: TToolButton
              Tag = 1
              Left = 0
              Top = 0
              Hint = 'JLabel'
              Grouped = True
              ImageIndex = 0
              ImageName = 'JLabel'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTextField: TToolButton
              Tag = 2
              Left = 28
              Top = 0
              Hint = 'JTextField'
              Grouped = True
              ImageIndex = 1
              ImageName = 'TextField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJNumberField: TToolButton
              Tag = 21
              Left = 56
              Top = 0
              Hint = 'JNumberField'
              Grouped = True
              ImageIndex = 2
              ImageName = 'NumberField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTextArea: TToolButton
              Tag = 3
              Left = 84
              Top = 0
              Hint = 'JTextArea'
              Grouped = True
              ImageIndex = 3
              ImageName = 'TextArea'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJButton: TToolButton
              Tag = 4
              Left = 112
              Top = 0
              Hint = 'JButton'
              Grouped = True
              ImageIndex = 4
              ImageName = 'Button'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJCheckbox: TToolButton
              Tag = 5
              Left = 140
              Top = 0
              Hint = 'JCheckbox'
              Grouped = True
              ImageIndex = 5
              ImageName = 'Checkbox'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJButtonGroup: TToolButton
              Tag = 50
              Left = 168
              Top = 0
              Hint = 'ButtonGroup'
              Grouped = True
              ImageIndex = 6
              ImageName = 'CheckboxGroup'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJList: TToolButton
              Tag = 8
              Left = 196
              Top = 0
              Hint = 'JList'
              Grouped = True
              ImageIndex = 7
              ImageName = 'List'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJComboBox: TToolButton
              Tag = 9
              Left = 224
              Top = 0
              Hint = 'JComboBox'
              Grouped = True
              ImageIndex = 8
              ImageName = 'Choice'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJSpinner: TToolButton
              Tag = 22
              Left = 252
              Top = 0
              Hint = 'JSpinner'
              Grouped = True
              ImageIndex = 9
              ImageName = 'JSpinner'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJScrollBar: TToolButton
              Tag = 10
              Left = 280
              Top = 0
              Hint = 'JScrollBar'
              Grouped = True
              ImageIndex = 10
              ImageName = 'Scrollbar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJScrollPane: TToolButton
              Tag = 11
              Left = 308
              Top = 0
              Hint = 'JScrollPane'
              Grouped = True
              ImageIndex = 11
              ImageName = 'ScrollPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJPanel: TToolButton
              Tag = 12
              Left = 336
              Top = 0
              Hint = 'JPanel - right-click for a subclass of JPanel'
              Grouped = True
              ImageIndex = 12
              ImageName = 'Panel'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnMouseUp = TBPanelCanvasMouseUp
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJCanvas: TToolButton
              Tag = -13
              Left = 364
              Top = 0
              Hint = 'Canvas - right-click for a subclass of Canvas'
              Grouped = True
              ImageIndex = 13
              ImageName = 'Canvas'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnMouseUp = TBPanelCanvasMouseUp
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJTurtle: TToolButton
              Tag = -14
              Left = 392
              Top = 0
              Hint = 'Turtle'
              Grouped = True
              ImageIndex = 14
              ImageName = 'Turtle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJMenuBar: TToolButton
              Tag = 52
              Left = 420
              Top = 0
              Hint = 'JMenuBar'
              Grouped = True
              ImageIndex = 15
              ImageName = 'MenuBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJMenu: TToolButton
              Tag = 43
              Left = 448
              Top = 0
              Hint = 'JMenu'
              Grouped = True
              ImageIndex = 16
              ImageName = 'Menu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBJPopupMenu: TToolButton
              Tag = 44
              Left = 476
              Top = 0
              Hint = 'JPopupMenu'
              Grouped = True
              ImageIndex = 17
              ImageName = 'PopupMenu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBSwingClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSFXControls: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'FX Controls'
          ImageIndex = -1
          TabItem = 'TabFXControls'
          object ToolBarFXControls: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilFXControls
            TabOrder = 0
            object TBFXSlider: TToolButton
              Tag = 131
              Left = 0
              Top = 0
              Hint = 'Slider'
              Grouped = True
              ImageIndex = 0
              ImageName = 'Slider'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXProgressBar: TToolButton
              Tag = 132
              Left = 28
              Top = 0
              Hint = 'ProgressBar'
              Grouped = True
              ImageIndex = 1
              ImageName = 'ProgressBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXProgressIndicator: TToolButton
              Tag = 133
              Left = 56
              Top = 0
              Hint = 'ProgressIndicator'
              Grouped = True
              ImageIndex = 2
              ImageName = 'ProgressIndicator'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXToolbar: TToolButton
              Tag = 134
              Left = 84
              Top = 0
              Hint = 'ToolBar'
              Grouped = True
              ImageIndex = 3
              ImageName = 'Toolbar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXSeparator: TToolButton
              Tag = 135
              Left = 112
              Top = 0
              Hint = 'Separator'
              Grouped = True
              ImageIndex = 4
              ImageName = 'Separator'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXToggleButton: TToolButton
              Tag = 136
              Left = 140
              Top = 0
              Hint = 'ToggleButton'
              Grouped = True
              ImageIndex = 5
              ImageName = 'ToggleButton'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXPasswordField: TToolButton
              Tag = 137
              Left = 168
              Top = 0
              Hint = 'PasswordField'
              Grouped = True
              ImageIndex = 6
              ImageName = 'PasswordField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXChoiceBox: TToolButton
              Tag = 138
              Left = 196
              Top = 0
              Hint = 'ChoiceBox'
              Grouped = True
              ImageIndex = 7
              ImageName = 'ChoiceBox'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXHyperlink: TToolButton
              Tag = 139
              Left = 224
              Top = 0
              Hint = 'Hyperlink'
              Grouped = True
              ImageIndex = 8
              ImageName = 'Hyperlink'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXHTMLEditor: TToolButton
              Tag = 140
              Left = 252
              Top = 0
              Hint = 'HTMLEditor'
              Grouped = True
              ImageIndex = 9
              ImageName = 'HTMLEditor'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXWebView: TToolButton
              Tag = 148
              Left = 280
              Top = 0
              Hint = 'WebView'
              Grouped = True
              ImageIndex = 10
              ImageName = 'WebView'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXColorPicker: TToolButton
              Tag = 141
              Left = 308
              Top = 0
              Hint = 'ColorPicker'
              Grouped = True
              ImageIndex = 11
              ImageName = 'ColorPicker'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXDatePicker: TToolButton
              Tag = 142
              Left = 336
              Top = 0
              Hint = 'DatePicker'
              Grouped = True
              ImageIndex = 12
              ImageName = 'DatePicker'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXPagination: TToolButton
              Tag = 143
              Left = 364
              Top = 0
              Hint = 'Pagination'
              Grouped = True
              ImageIndex = 13
              ImageName = 'Pages1'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXFileOpenChooser: TToolButton
              Tag = 144
              Left = 392
              Top = 0
              Hint = 'FileOpenChooser'
              Grouped = True
              ImageIndex = 14
              ImageName = 'FileOpenChooser'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXFileSaveChooser: TToolButton
              Tag = 145
              Left = 420
              Top = 0
              Hint = 'FileSaveChooser'
              Grouped = True
              ImageIndex = 15
              ImageName = 'FileSaveChooser'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXDirectoryChooser: TToolButton
              Tag = 146
              Left = 448
              Top = 0
              Hint = 'DirectoryChooser'
              Grouped = True
              ImageIndex = 16
              ImageName = 'DirectoryChooser'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXImageView: TToolButton
              Tag = 147
              Left = 476
              Top = 0
              Hint = 'ImageView'
              Grouped = True
              ImageIndex = 17
              ImageName = 'ImageView'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXMediaView: TToolButton
              Tag = 150
              Left = 504
              Top = 0
              Hint = 'MediaView'
              Grouped = True
              ImageIndex = 18
              ImageName = 'MediaView'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXTableView: TToolButton
              Tag = 149
              Left = 532
              Top = 0
              Hint = 'TableView'
              Grouped = True
              ImageIndex = 19
              ImageName = 'TableView'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXTreeView: TToolButton
              Tag = 152
              Left = 560
              Top = 0
              Hint = 'TreeView'
              Grouped = True
              ImageIndex = 20
              ImageName = 'TreeView'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
        object TSFXBase: TSpTBXTabSheet
          Left = 0
          Top = 25
          Width = 595
          Height = 30
          Caption = 'FX Base'
          ImageIndex = -1
          TabItem = 'TabFXBase'
          object ToolBarFXBase: TToolBar
            Left = 2
            Top = 0
            Width = 589
            Height = 26
            Align = alClient
            AutoSize = True
            ButtonHeight = 27
            ButtonWidth = 28
            EdgeInner = esNone
            EdgeOuter = esNone
            Images = vilFXBaseLight
            TabOrder = 0
            Wrapable = False
            object TBFXLabel: TToolButton
              Tag = 101
              Left = 0
              Top = 0
              Hint = 'Label'
              Grouped = True
              ImageIndex = 0
              ImageName = 'JLabel'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXTextField: TToolButton
              Tag = 102
              Left = 28
              Top = 0
              Hint = 'TextField'
              Grouped = True
              ImageIndex = 1
              ImageName = 'TextField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXNumberField: TToolButton
              Tag = 103
              Left = 56
              Top = 0
              Hint = 'NumberField'
              Grouped = True
              ImageIndex = 2
              ImageName = 'NumberField'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXTextArea: TToolButton
              Tag = 104
              Left = 84
              Top = 0
              Hint = 'TextArea'
              Grouped = True
              ImageIndex = 3
              ImageName = 'TextArea'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXButton: TToolButton
              Tag = 105
              Left = 112
              Top = 0
              Hint = 'Button'
              Grouped = True
              ImageIndex = 4
              ImageName = 'Button'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXCheckBox: TToolButton
              Tag = 106
              Left = 140
              Top = 0
              Hint = 'CheckBox'
              Grouped = True
              ImageIndex = 5
              ImageName = 'Checkbox'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXButtonGroup: TToolButton
              Tag = 123
              Left = 168
              Top = 0
              Hint = 'ButtonGroup'
              Caption = 'ToggleGroup'
              Grouped = True
              ImageIndex = 6
              ImageName = 'CheckboxGroup'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXListView: TToolButton
              Tag = 109
              Left = 196
              Top = 0
              Hint = 'ListView'
              Grouped = True
              ImageIndex = 7
              ImageName = 'List'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXComboBox: TToolButton
              Tag = 110
              Left = 224
              Top = 0
              Hint = 'ComboBox'
              Grouped = True
              ImageIndex = 8
              ImageName = 'Choice'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXSpinner: TToolButton
              Tag = 111
              Left = 252
              Top = 0
              Hint = 'Spinner'
              Grouped = True
              ImageIndex = 9
              ImageName = 'JSpinner'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXScrollPane: TToolButton
              Tag = 113
              Left = 280
              Top = 0
              Hint = 'ScrollPane'
              Grouped = True
              ImageIndex = 10
              ImageName = 'ScrollPane'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXPane: TToolButton
              Tag = 121
              Left = 308
              Top = 0
              Hint = 'Pane'
              Grouped = True
              ImageIndex = 11
              ImageName = 'Panel'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXCanvas: TToolButton
              Tag = 114
              Left = 336
              Top = 0
              Hint = 'Canvas - right click for a sub class of  Canvas'
              Grouped = True
              ImageIndex = 12
              ImageName = 'Canvas'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnMouseUp = TBPanelCanvasMouseUp
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXTurtle: TToolButton
              Tag = 120
              Left = 364
              Top = 0
              Hint = 'Turtle'
              Grouped = True
              ImageIndex = 13
              ImageName = 'FXTurtle'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXMenuBar: TToolButton
              Tag = 124
              Left = 392
              Top = 0
              Hint = 'MenuBar'
              Grouped = True
              ImageIndex = 14
              ImageName = 'MenuBar'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXMenu: TToolButton
              Tag = 116
              Left = 420
              Top = 0
              Hint = 'Menu'
              Grouped = True
              ImageIndex = 15
              ImageName = 'Menu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXContextMenu: TToolButton
              Tag = 117
              Left = 448
              Top = 0
              Hint = 'ContextMenu'
              Grouped = True
              ImageIndex = 16
              ImageName = 'PopupMenu'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXMenuButton: TToolButton
              Tag = 118
              Left = 476
              Top = 0
              Hint = 'MenuButton'
              Grouped = True
              ImageIndex = 17
              ImageName = 'FXMenuButton'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
            object TBFXSplitMenuButton: TToolButton
              Tag = 119
              Left = 504
              Top = 0
              Hint = 'SplitMenuButton'
              Grouped = True
              ImageIndex = 18
              ImageName = 'FXMenuSplitButton'
              ParentShowHint = False
              ShowHint = True
              Style = tbsCheck
              OnClick = TBJavaFXClick
              OnMouseDown = ToolbuttonMouseDown
              OnStartDrag = ToolbuttonStartDrag
            end
          end
        end
      end
      object MainToolBar: TSpTBXToolbar
        Left = 3
        Top = 2
        Width = 161
        Height = 26
        DockMode = dmCannotFloatOrChangeDocks
        DockPos = 0
        DockRow = 1
        Images = vilToolbarLight
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        object TBOpen: TSpTBXItem
          Caption = 'Open'
          Hint = 'Open'
          ImageIndex = 0
          ImageName = '00'
          OnClick = MIOpenClick
        end
        object TBSave: TSpTBXItem
          Caption = 'Save'
          Hint = 'Save'
          ImageIndex = 1
          ImageName = '01'
          OnClick = MISaveClick
          Left = 23
        end
        object TBSaveAll: TSpTBXItem
          Caption = 'Save all'
          Hint = 'Save all'
          ImageIndex = 2
          ImageName = '02'
          OnClick = MISaveAllClick
          Left = 46
        end
        object TBDefaultLayout: TSpTBXItem
          Caption = 'Default layout'
          Hint = 'Default layout'
          ImageIndex = 3
          ImageName = '03'
          OnClick = MIDefaultLayoutClick
          Left = 69
        end
        object TBPropertyInspector: TSpTBXItem
          Caption = 'Object inspector'
          Hint = 'Object inspector on/off'
          ImageIndex = 4
          ImageName = '04'
          OnClick = MIObjectInspectorClick
          Left = 92
        end
        object TBDiagramFromOpenFiles: TSpTBXItem
          Caption = 'Diagram from open files'
          Hint = 'Diagram from open files'
          ImageIndex = 5
          ImageName = '05'
          OnClick = MIDiagramFromOpenFilesClick
          Left = 115
        end
        object TBMessages: TSpTBXItem
          Caption = 'Messages on/off'
          Hint = 'Messages on/off'
          ImageIndex = 6
          ImageName = '06'
          OnClick = MIMessagesClick
          Left = 138
        end
      end
    end
    object MainMenu: TSpTBXToolbar
      Left = 0
      Top = 0
      Width = 851
      Height = 21
      Align = alTop
      Images = vilMenuLight
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      TabOrder = 2
      Customizable = False
      MenuBar = True
      object MIFile: TSpTBXSubmenuItem
        Caption = '&File'
        object MINew: TSpTBXSubmenuItem
          Caption = 'New'
          object MINewJava: TSpTBXItem
            Tag = 1
            Caption = 'Java'
            ImageIndex = 0
            ImageName = '00'
            ShortCut = 16462
            OnClick = MINewClick
          end
          object MINewText: TSpTBXItem
            Tag = 2
            Caption = 'Text'
            OnClick = MINewClick
          end
          object MINewHtml: TSpTBXItem
            Tag = 3
            Caption = 'HTML'
            OnClick = MINewClick
          end
          object N19: TSpTBXSeparatorItem
          end
          object MIFileNewClass: TSpTBXItem
            Caption = 'Class'
            ImageIndex = 75
            ImageName = '75'
            OnClick = SBClassClick
          end
          object MINewStructogram: TSpTBXItem
            Caption = 'Structogram'
            ImageIndex = 84
            ImageName = '84'
            OnClick = SBStructogramClick
          end
          object MINewSequencediagram: TSpTBXItem
            Caption = 'Sequence diagram'
            ImageIndex = 92
            ImageName = '92'
            OnClick = MINewSequencediagramClick
          end
          object N10: TSpTBXSeparatorItem
          end
          object MINewConsole: TSpTBXItem
            Tag = 1
            Caption = 'Console'
            OnClick = SBProgramClick
          end
          object MINewFrame: TSpTBXItem
            Tag = 2
            Caption = 'Frame'
            OnClick = SBGUIFrameClick
          end
          object MINewDialog: TSpTBXItem
            Tag = 3
            Caption = 'Dialog'
            OnClick = SBGUIFrameClick
          end
          object MINewApplet: TSpTBXItem
            Tag = 4
            Caption = 'Applet'
            OnClick = SBGUIFrameClick
          end
          object MINewJFrame: TSpTBXItem
            Tag = 5
            Caption = 'JFrame'
            OnClick = SBGUIFrameClick
          end
          object MINewJDialog: TSpTBXItem
            Tag = 6
            Caption = 'JDialog'
            OnClick = SBGUIFrameClick
          end
          object MINewJApplet: TSpTBXItem
            Tag = 7
            Caption = 'JApplet'
            OnClick = SBGUIFrameClick
          end
          object MIApplication: TSpTBXItem
            Tag = 8
            Caption = 'FX Application'
            OnClick = SBGUIFrameClick
          end
        end
        object MIOpen: TSpTBXItem
          Caption = 'Open...'
          ImageIndex = 1
          ImageName = '01'
          ShortCut = 16463
          OnClick = MIOpenClick
        end
        object MIReopen: TSpTBXSubmenuItem
          Caption = 'Reopen'
        end
        object N15: TSpTBXSeparatorItem
        end
        object MISave: TSpTBXItem
          Caption = 'Save'
          ImageIndex = 2
          ImageName = '02'
          ShortCut = 16467
          OnClick = MISaveClick
        end
        object MISaveAs: TSpTBXItem
          Caption = 'Save as...'
          ImageIndex = 30
          ImageName = '30'
          OnClick = MISaveAsClick
        end
        object MISaveAll: TSpTBXItem
          Caption = 'Save all'
          ImageIndex = 49
          ImageName = '49'
          OnClick = MISaveAllClick
        end
        object MISaveAllIn: TSpTBXItem
          Caption = 'Save all in...'
          OnClick = MISaveAllInClick
        end
        object MISaveAsProject: TSpTBXItem
          Caption = 'Save as project'
          ImageIndex = 67
          ImageName = '67'
          OnClick = MISaveAsProjectClick
        end
        object MICloseProject: TSpTBXItem
          Caption = 'Close project'
          OnClick = MICloseProjectClick
        end
        object MIClose: TSpTBXItem
          Caption = 'Close'
          ImageIndex = 29
          ImageName = '29'
          ShortCut = 16499
          OnClick = MICloseClick
        end
        object MICloseAllFiles: TSpTBXItem
          Caption = 'Close all'
          ImageIndex = 63
          ImageName = '63'
          OnClick = MICloseAllFilesClick
        end
        object MIExport: TSpTBXItem
          Caption = 'Export'
          OnClick = MIExportClick
        end
        object N1: TSpTBXSeparatorItem
        end
        object MIPrint: TSpTBXItem
          Caption = 'Print'
          ImageIndex = 3
          ImageName = '03'
          ShortCut = 16464
          OnClick = MIPrintClick
        end
        object MIPrintAll: TSpTBXItem
          Caption = 'Print all'
          OnClick = MIPrintAllClick
        end
        object MIPrintSetup: TSpTBXItem
          Caption = 'Printer setup'
          ImageIndex = 32
          ImageName = '32'
          OnClick = MIPrintSetupClick
        end
        object N9: TSpTBXSeparatorItem
        end
        object MIExit: TSpTBXItem
          Caption = 'Exit'
          ImageIndex = 64
          ImageName = '64'
          ShortCut = 16465
          OnClick = MIExitClick
        end
      end
      object MIEdit: TSpTBXSubmenuItem
        Caption = '&Edit'
        object MIUndo: TSpTBXItem
          Caption = 'Undo'
          ImageIndex = 12
          ImageName = '12'
          ShortCut = 16474
          OnClick = MIUndoClick
        end
        object MIRedo: TSpTBXItem
          Caption = 'Redo'
          ImageIndex = 13
          ImageName = '13'
          OnClick = MIRedoClick
        end
        object N3: TSpTBXSeparatorItem
        end
        object MICut: TSpTBXItem
          Caption = 'Cut'
          ImageIndex = 14
          ImageName = '14'
          ShortCut = 16472
          OnClick = MICutClick
        end
        object MICopy: TSpTBXSubmenuItem
          Caption = 'Copy'
          ImageIndex = 15
          ImageName = '15'
          object MICopyNormal: TSpTBXItem
            Caption = 'Normal'
            ImageIndex = 15
            ImageName = '15'
            ShortCut = 16451
            OnClick = MICopyNormalClick
          end
          object MICopyRTF: TSpTBXItem
            Caption = 'RTF'
            ShortCut = 49219
            OnClick = MICopyRTFClick
          end
          object MICopyRtfNumbered: TSpTBXItem
            Caption = 'RTF numbered'
            OnClick = MICopyRtfNumberedClick
          end
          object MICopyHTML: TSpTBXItem
            Caption = 'HTML'
            OnClick = MICopyHTMLClick
          end
          object MICopyHTMLAsText: TSpTBXItem
            Caption = 'HTML as Text'
            OnClick = MICopyHTMLAsTextClick
          end
          object MICopyNumbered: TSpTBXItem
            Caption = 'Numbered'
            OnClick = MICopyNumberedClick
          end
        end
        object MIPaste: TSpTBXItem
          Caption = 'Paste'
          ImageIndex = 16
          ImageName = '16'
          ShortCut = 16470
          OnClick = MIPasteClick
        end
        object N4: TSpTBXSeparatorItem
        end
        object MISearch: TSpTBXItem
          Caption = 'Search'
          ImageIndex = 17
          ImageName = '17'
          ShortCut = 16454
          OnClick = MISearchClick
        end
        object MISearchAgain: TSpTBXItem
          Caption = 'Search again'
          ImageIndex = 34
          ImageName = '34'
          ShortCut = 114
          OnClick = MISearchAgainClick
        end
        object MIReplace: TSpTBXItem
          Caption = 'Replace'
          ImageIndex = 26
          ImageName = '26'
          ShortCut = 16466
          OnClick = MIReplaceClick
        end
        object MISearchInFiles: TSpTBXItem
          Caption = 'In files...'
          ImageIndex = 51
          ImageName = '51'
          OnClick = MISearchInFilesClick
        end
        object N8: TSpTBXSeparatorItem
        end
        object MIUnindent: TSpTBXItem
          Caption = 'Unindent'
          ImageIndex = 18
          ImageName = '18'
          OnClick = MIUnindentClick
        end
        object MIIndent: TSpTBXItem
          Caption = 'Indent'
          ImageIndex = 19
          ImageName = '19'
          OnClick = MIIndentClick
        end
        object MIStructuredIndent: TSpTBXItem
          Caption = 'Structured indentation'
          ImageIndex = 85
          ImageName = '85'
          ShortCut = 49225
          OnClick = SBStructureIndentClick
        end
        object MICommentOnOff: TSpTBXItem
          Caption = 'Comment/Uncomment'
          ImageIndex = 48
          ImageName = '48'
          ShortCut = 16459
          OnClick = MICommentOnOffClick
        end
        object MIStrich10: TSpTBXSeparatorItem
        end
        object MIGotoLine: TSpTBXItem
          Caption = 'Go to line'
          ImageIndex = 20
          ImageName = '20'
          ShortCut = 16455
          OnClick = MIGotoLineClick
        end
        object MISystemOutPrintln: TSpTBXItem
          Caption = 'System.out.println'
          ImageIndex = 36
          ImageName = '36'
          ShortCut = 16469
          OnClick = MISystemOutPrintlnClick
        end
        object MIUnicode: TSpTBXItem
          Caption = 'Insert Unicode'
          ImageIndex = 80
          ImageName = '80'
          OnClick = MIUnicodeClick
        end
      end
      object MIStart: TSpTBXSubmenuItem
        Caption = '&Start'
        object MICompile: TSpTBXItem
          Caption = 'Compile'
          ImageIndex = 21
          ImageName = '21'
          ShortCut = 16504
          OnClick = MICompileClick
        end
        object MIMindstorms: TSpTBXItem
          Caption = 'Upload Lejos firmware'
          ImageIndex = 61
          ImageName = '61'
        end
        object MICompileAll: TSpTBXItem
          Caption = 'Compile all'
          ImageIndex = 47
          ImageName = '47'
          OnClick = MICompileAllClick
        end
        object MIRun: TSpTBXItem
          Caption = 'Run'
          ImageIndex = 22
          ImageName = '22'
          ShortCut = 120
          OnClick = MIRunClick
        end
        object MIProgramReset: TSpTBXItem
          Caption = 'Program reset'
          ImageIndex = 23
          ImageName = '23'
          ShortCut = 16497
          OnClick = MIProgramResetClick
        end
        object MIJavaReset: TSpTBXItem
          Caption = 'Java reset'
          ImageIndex = 82
          ImageName = '82'
          OnClick = MIJavaResetClick
        end
        object MIParameter: TSpTBXItem
          Caption = 'Parameter...'
          ImageIndex = 35
          ImageName = '35'
          OnClick = MIParameterClick
        end
        object MIStrich7: TSpTBXSeparatorItem
        end
        object MIHTMLforApplet: TSpTBXItem
          Caption = 'HTML for applet'
          OnClick = MIHTMLforAppletClick
        end
        object MIHTMLforJavaPlugIn: TSpTBXItem
          Caption = 'HTML for java plugin'
          OnClick = MIHTMLforAppletClick
        end
        object MIAppletviewer: TSpTBXItem
          Caption = 'Appletviewer'
          ImageIndex = 22
          ImageName = '22'
          OnClick = MIAppletviewerClick
        end
        object N6: TSpTBXSeparatorItem
        end
        object MIDebugger: TSpTBXItem
          Caption = 'Debugger'
          ImageIndex = 38
          ImageName = '38'
          OnClick = MIDebuggerClick
        end
        object MIDissasembler: TSpTBXItem
          Caption = 'Disassembler'
          OnClick = MIDissasemblerClick
        end
        object MIJavaDoc: TSpTBXItem
          Caption = 'JavaDoc'
          OnClick = MIJavaDocClick
        end
        object MIJar: TSpTBXSubmenuItem
          Caption = 'Jar file'
          object MIJarCreate: TSpTBXItem
            Caption = 'Create'
            ShortCut = 117
            OnClick = MIJarCreateClick
          end
          object MIJarPack: TSpTBXItem
            Caption = 'Pack'
            OnClick = MIJarPackClick
          end
          object MIJarShow: TSpTBXItem
            Tag = 1
            Caption = 'Show'
            OnClick = MIJarClick
          end
          object MIJarUnpack: TSpTBXItem
            Tag = 2
            Caption = 'Unpack'
            OnClick = MIJarClick
          end
          object MIJarOpen: TSpTBXItem
            Tag = 3
            Caption = 'Open'
            OnClick = MIJarClick
          end
        end
      end
      object MITest: TSpTBXSubmenuItem
        Caption = '&Test'
        object MIStep: TSpTBXItem
          Caption = 'Step'
          ImageIndex = 37
          ImageName = '37'
          ShortCut = 118
          OnClick = MIStepClick
        end
        object MINext: TSpTBXItem
          Caption = 'Next'
          ImageIndex = 38
          ImageName = '38'
          ShortCut = 119
          OnClick = MINextClick
        end
        object MIStepUp: TSpTBXItem
          Caption = 'Step up'
          ImageIndex = 41
          ImageName = '41'
          ShortCut = 8311
          OnClick = MIStepUpClick
        end
        object MIRunToCursor: TSpTBXItem
          Caption = 'Run to cursor'
          ImageIndex = 77
          ImageName = '77'
          ShortCut = 115
          OnClick = MIRunToCursorClick
        end
        object MIShowExecutionPoint: TSpTBXItem
          Caption = 'Show execution line'
          ImageIndex = 76
          ImageName = '76'
          OnClick = MIShowExecutionPointClick
        end
        object N12: TSpTBXSeparatorItem
        end
        object MIBreakpoint: TSpTBXItem
          Caption = 'Breakpoint on/off'
          ImageIndex = 39
          ImageName = '39'
          ShortCut = 116
          OnClick = MIBreakpointClick
        end
        object MIBreakpointsClear: TSpTBXItem
          Caption = 'Delete breakpoints'
          ImageIndex = 40
          ImageName = '40'
          OnClick = MIBreakpointsClearClick
        end
        object N13: TSpTBXSeparatorItem
        end
        object MIExpression: TSpTBXItem
          Caption = 'Evaluate'
          ImageIndex = 44
          ImageName = '44'
          OnClick = MIExpressionClick
        end
        object MIWatches: TSpTBXItem
          Caption = 'Watches'
          ImageIndex = 59
          ImageName = '59'
          OnClick = MIWatchesClick
        end
        object MITestCreateSequencediagram: TSpTBXItem
          Caption = 'Create sequence diagram'
          ImageIndex = 92
          ImageName = '92'
          OnClick = MITestCreateSequencediagramClick
        end
      end
      object MIUML: TSpTBXSubmenuItem
        Caption = '&UML'
        object MINewUML: TSpTBXItem
          Caption = 'New'
          ImageIndex = 0
          ImageName = '00'
          OnClick = MINewUMLClick
        end
        object MINewClass: TSpTBXItem
          Caption = 'New class'
          ImageIndex = 75
          ImageName = '75'
          OnClick = MINewClassClick
        end
        object MIClassOpen: TSpTBXItem
          Caption = 'Open class'
          ImageIndex = 74
          ImageName = '74'
          OnClick = MIClassOpenClick
        end
        object MIClassEditor: TSpTBXItem
          Caption = 'Edit class'
          ImageIndex = 72
          ImageName = '72'
          OnClick = MIClassEditorClick
        end
        object MINewComment: TSpTBXItem
          Caption = 'New comment'
          ImageIndex = 86
          ImageName = '86'
          OnClick = MINewCommentClick
        end
        object MINewLayout: TSpTBXItem
          Caption = 'New layout'
          ImageIndex = 54
          ImageName = '54'
          OnClick = MINewLayoutClick
        end
        object MIRefresh: TSpTBXItem
          Caption = 'Refresh'
          ImageIndex = 73
          ImageName = '73'
          OnClick = MIRefreshClick
        end
        object MIRecognizeAssociations: TSpTBXItem
          Caption = 'Recognize assoziations'
          ImageIndex = 94
          ImageName = '94'
          OnClick = MIRecognizeAssociationsClick
        end
        object MIDiagramFromOpenFiles: TSpTBXItem
          Caption = 'Diagram from open files'
          ImageIndex = 58
          ImageName = '58'
          OnClick = MIDiagramFromOpenFilesClick
        end
        object MIOpenFolder: TSpTBXItem
          Caption = 'Open folder'
          ImageIndex = 57
          ImageName = '57'
          OnClick = MIOpenFolderClick
        end
        object MISaveAsPicture: TSpTBXItem
          Caption = 'Save as picture...'
          ImageIndex = 56
          ImageName = '56'
          OnClick = MISaveAsPictureClick
        end
        object MIUMLCreateSequencediagram: TSpTBXItem
          Caption = 'Create sequence diagram'
          ImageIndex = 92
          ImageName = '92'
          OnClick = MIUMLCreateSequencediagramClick
        end
      end
      object MITools: TSpTBXSubmenuItem
        Caption = 'Tools'
        object MICheckStyle: TSpTBXItem
          Caption = 'Checkstyle'
          ImageIndex = 52
          ImageName = '52'
          OnClick = MICheckstyleClick
        end
        object MIJalopy: TSpTBXItem
          Caption = 'Jalopy'
          ImageIndex = 52
          ImageName = '52'
          OnClick = MIJalopyClick
        end
        object MICompare: TSpTBXItem
          Caption = 'Compare text'
          ImageIndex = 78
          ImageName = '78'
          OnClick = MICompareClick
        end
        object MISubversion: TSpTBXSubmenuItem
          Caption = 'Subversion'
          ImageIndex = 79
          ImageName = '79'
          object MISVNCommit: TSpTBXItem
            Caption = 'Commit'
            OnClick = MISVNCommitClick
          end
          object MISVNAdd: TSpTBXItem
            Caption = 'Add file'
            OnClick = MISVNAddClick
          end
          object MISVNLog: TSpTBXItem
            Caption = 'Log of file'
            OnClick = MISVNLogClick
          end
          object MISVNCompare: TSpTBXItem
            Caption = 'Compare files'
            OnClick = MISVNCompareClick
          end
          object N16: TSpTBXSeparatorItem
          end
          object MISVNStatus: TSpTBXItem
            Caption = 'Status of folder'
            OnClick = MISVNStatusClick
          end
          object MISVNTree: TSpTBXItem
            Caption = 'Tree of repository'
            OnClick = MISVNTreeClick
          end
          object MISVNUpdate: TSpTBXItem
            Caption = 'Update from repository'
            OnClick = MISVNUpdateClick
          end
        end
        object MIGit: TSpTBXSubmenuItem
          Caption = 'Git'
          ImageIndex = 90
          ImageName = '90'
          object MIGitStatus: TSpTBXItem
            Tag = 1
            Caption = 'Status'
            OnClick = MIGitClick
          end
          object MIGitAdd: TSpTBXItem
            Tag = 2
            Caption = 'Add '
            OnClick = MIGitClick
          end
          object MIGitCommit: TSpTBXItem
            Tag = 3
            Caption = 'Commit'
            OnClick = MIGitClick
          end
          object MIGitLog: TSpTBXItem
            Tag = 4
            Caption = 'Log'
            OnClick = MIGitClick
          end
          object N20: TSpTBXSeparatorItem
          end
          object MIGitReset: TSpTBXItem
            Tag = 5
            Caption = 'Reset'
            OnClick = MIGitClick
          end
          object MIGitCheckout: TSpTBXItem
            Tag = 6
            Caption = 'Checkout'
            OnClick = MIGitClick
          end
          object MIGitRemove: TSpTBXItem
            Tag = 7
            Caption = 'Remove'
            OnClick = MIGitClick
          end
          object N21: TSpTBXSeparatorItem
          end
          object MIGitRemote: TSpTBXItem
            Tag = 8
            Caption = 'Remote'
            OnClick = MIGitClick
          end
          object MIGitFetch: TSpTBXItem
            Tag = 9
            Caption = 'Fetch'
            OnClick = MIGitClick
          end
          object MIGitPush: TSpTBXItem
            Tag = 10
            Caption = 'Push'
            OnClick = MIGitClick
          end
          object N22: TSpTBXSeparatorItem
          end
          object MIGitGUI: TSpTBXItem
            Tag = 11
            Caption = 'GUI'
            OnClick = MIGitClick
          end
          object MIGitViewer: TSpTBXItem
            Tag = 12
            Caption = 'Viewer'
            OnClick = MIGitClick
          end
          object MIGitConsole: TSpTBXItem
            Tag = 13
            Caption = 'Console'
            OnClick = MIGitClick
          end
        end
        object MIJUnit: TSpTBXSubmenuItem
          Caption = 'JUnit'
          ImageIndex = 91
          ImageName = '91'
          object MIJUnitRunAllTests: TSpTBXItem
            Caption = 'Execute all tests'
            OnClick = MIJUnitRunAllTestsClick
          end
          object MIJUnitCreateTestclass: TSpTBXItem
            Caption = 'Create test class'
            OnClick = MIJUnitCreateTestclassClick
          end
        end
        object N17: TSpTBXSeparatorItem
        end
        object MIConfigureTools: TSpTBXItem
          Caption = 'Configure tools'
          ImageIndex = 81
          ImageName = '81'
          OnClick = MIConfigureToolsClick
        end
      end
      object MIComponents: TSpTBXSubmenuItem
        Caption = '&Components'
        object MIProgramm: TSpTBXSubmenuItem
          Caption = 'Program'
          object MIConsole: TSpTBXItem
            Tag = 1
            Caption = 'Console'
            OnClick = SBProgramClick
          end
          object MIFrame: TSpTBXItem
            Tag = 2
            Caption = 'Frame'
            OnClick = SBGUIFrameClick
          end
          object MIDialog: TSpTBXItem
            Tag = 3
            Caption = 'Dialog'
            OnClick = SBGUIFrameClick
          end
          object MIApplet: TSpTBXItem
            Tag = 4
            Caption = 'Applet'
            OnClick = SBGUIFrameClick
          end
          object MIJFrame: TSpTBXItem
            Tag = 5
            Caption = 'JFrame'
            OnClick = SBGUIFrameClick
          end
          object MIJDialog: TSpTBXItem
            Tag = 6
            Caption = 'JDialog'
            OnClick = SBGUIFrameClick
          end
          object MIJApplet: TSpTBXItem
            Tag = 7
            Caption = 'JApplet'
            OnClick = SBGUIFrameClick
          end
          object N5: TSpTBXSeparatorItem
          end
          object MIJavaDoc1: TSpTBXItem
            Caption = 'JavaDoc'
          end
          object MIclass: TSpTBXItem
            Caption = 'Class'
          end
        end
        object MIKontrollstrukturen: TSpTBXSubmenuItem
          Caption = 'Control structures'
          object MIIf: TSpTBXItem
            Tag = 1
            Caption = 'if'
            OnClick = SBKontrollstrukturenClick
          end
          object MIifelse: TSpTBXItem
            Tag = 9
            Caption = 'if..else'
            OnClick = SBKontrollstrukturenClick
          end
          object MIwhile: TSpTBXItem
            Tag = 2
            Caption = 'while'
            OnClick = SBKontrollstrukturenClick
          end
          object MIdowhile: TSpTBXItem
            Tag = 4
            Caption = 'do..while'
            OnClick = SBKontrollstrukturenClick
          end
          object MIfor: TSpTBXItem
            Tag = 3
            Caption = 'for'
            OnClick = SBKontrollstrukturenClick
          end
          object MISwitch: TSpTBXItem
            Tag = 5
            Caption = 'switch'
            OnClick = SBKontrollstrukturenClick
          end
          object MITry: TSpTBXItem
            Tag = 6
            Caption = 'try'
            OnClick = SBKontrollstrukturenClick
          end
        end
        object MIDatentypen: TSpTBXSubmenuItem
          Caption = 'Data types'
          object MIByte: TSpTBXItem
            Caption = 'byte'
          end
          object MIShort: TSpTBXItem
            Tag = 1
            Caption = 'short'
          end
          object MIInt: TSpTBXItem
            Tag = 2
            Caption = 'int'
          end
          object MILong: TSpTBXItem
            Tag = 3
            Caption = 'long'
          end
          object MIFloat: TSpTBXItem
            Tag = 4
            Caption = 'float'
          end
          object MIDouble: TSpTBXItem
            Tag = 5
            Caption = 'double'
          end
          object MIChar: TSpTBXItem
            Tag = 6
            Caption = 'char'
          end
          object MIBoolean: TSpTBXItem
            Tag = 7
            Caption = 'boolean'
          end
          object MIString: TSpTBXItem
            Tag = 8
            Caption = 'String'
          end
        end
        object MIAWT: TSpTBXSubmenuItem
          Caption = 'AWT'
          object MILabel: TSpTBXItem
            Tag = -1
            Caption = 'Label'
            OnClick = MISwingClick
          end
          object MITextField: TSpTBXItem
            Tag = -2
            Caption = 'TextField'
            OnClick = MISwingClick
          end
          object MINumberField: TSpTBXItem
            Tag = -21
            Caption = 'NumberField'
            OnClick = MISwingClick
          end
          object MITextArea: TSpTBXItem
            Tag = -3
            Caption = 'TextArea'
            OnClick = MISwingClick
          end
          object MIButton: TSpTBXItem
            Tag = -4
            Caption = 'Button'
            OnClick = MISwingClick
          end
          object MICheckbox: TSpTBXItem
            Tag = -5
            Caption = 'Checkbox'
            OnClick = MISwingClick
          end
          object MIRadiobutton: TSpTBXItem
            Tag = -6
            Caption = 'Radiobutton'
            OnClick = MISwingClick
          end
          object MICheckBoxGroup: TSpTBXItem
            Tag = -7
            Caption = 'CheckBoxGroup'
            OnClick = MISwingClick
          end
          object MIList: TSpTBXItem
            Tag = -8
            Caption = 'List'
            OnClick = MISwingClick
          end
          object MIChoice: TSpTBXItem
            Tag = -9
            Caption = 'Choice'
            OnClick = MISwingClick
          end
          object MIScrollbar: TSpTBXItem
            Tag = -10
            Caption = 'Scrollbar'
            OnClick = MISwingClick
          end
          object MIScrollPane: TSpTBXItem
            Tag = -11
            Caption = 'ScrollPane'
            OnClick = MISwingClick
          end
          object MIPanel: TSpTBXItem
            Tag = -12
            Caption = 'Panel'
            OnClick = MISwingClick
          end
          object MIMenu: TSpTBXItem
            Tag = -43
            Caption = 'Menu'
            OnClick = MISwingClick
          end
          object MICanvas: TSpTBXItem
            Tag = -13
            Caption = 'Canvas'
            OnClick = MISwingClick
          end
          object MITurtle: TSpTBXItem
            Tag = -14
            Caption = 'Turtle'
            OnClick = MISwingClick
          end
          object MIMenuBar: TSpTBXItem
            Tag = -42
            Caption = 'MenuBar'
            OnClick = MISwingClick
          end
          object MITimer: TSpTBXItem
            Tag = 49
            Caption = 'Timer'
            OnClick = MISwingClick
          end
          object MIPopUpMenu: TSpTBXItem
            Tag = -44
            Caption = 'PopUpMenu'
            OnClick = MISwingClick
          end
        end
        object MISwing1: TSpTBXSubmenuItem
          Caption = 'Swing1'
          object MIJLabel: TSpTBXItem
            Tag = 1
            Caption = 'JLabel'
            OnClick = MISwingClick
          end
          object MIJTextField: TSpTBXItem
            Tag = 2
            Caption = 'JTextField'
            OnClick = MISwingClick
          end
          object MIJNumberField: TSpTBXItem
            Tag = 21
            Caption = 'JNumberField'
            OnClick = MISwingClick
          end
          object MIJTextArea: TSpTBXItem
            Tag = 3
            Caption = 'JTextArea'
            OnClick = MISwingClick
          end
          object MIJButton: TSpTBXItem
            Tag = 4
            Caption = 'JButton'
            OnClick = MISwingClick
          end
          object MIJCheckbox: TSpTBXItem
            Tag = 5
            Caption = 'JCheckbox'
            OnClick = MISwingClick
          end
          object MIJRadiobutton: TSpTBXItem
            Tag = 6
            Caption = 'JRadiobutton'
            OnClick = MISwingClick
          end
          object MIJCheckBoxGroup: TSpTBXItem
            Tag = 7
            Caption = 'JCheckBoxGroup'
            OnClick = MISwingClick
          end
          object MIJList: TSpTBXItem
            Tag = 8
            Caption = 'JList'
            OnClick = MISwingClick
          end
          object MIJComboBox: TSpTBXItem
            Tag = 9
            Caption = 'JComboBox'
            OnClick = MISwingClick
          end
          object MIJSpinner: TSpTBXItem
            Tag = 22
            Caption = 'JSpinner'
            OnClick = MISwingClick
          end
          object MIJScrollbar: TSpTBXItem
            Tag = 10
            Caption = 'JScrollbar'
            OnClick = MISwingClick
          end
          object MIJScrollPane: TSpTBXItem
            Tag = 11
            Caption = 'JScrollPane'
            OnClick = MISwingClick
          end
          object MIJPanel: TSpTBXItem
            Tag = 12
            Caption = 'JPanel'
            OnClick = MISwingClick
          end
          object MIJCanvas: TSpTBXItem
            Tag = -13
            Caption = 'Canvas'
            OnClick = MISwingClick
          end
          object MIJTurtle: TSpTBXItem
            Tag = -14
            Caption = 'Turtle'
            OnClick = MISwingClick
          end
          object MIJMenuBar: TSpTBXItem
            Tag = 42
            Caption = 'JMenuBar'
            OnClick = MISwingClick
          end
          object MIJMenu: TSpTBXItem
            Tag = 43
            Caption = 'JMenu'
            OnClick = MISwingClick
          end
          object MIJPopUpMenu: TSpTBXItem
            Tag = 44
            Caption = 'JPopUpMenu'
            OnClick = MISwingClick
          end
          object MIJTimer: TSpTBXItem
            Tag = 49
            Caption = 'Timer'
            Hint = 'Timer'
            OnClick = MISwingClick
          end
        end
        object MISwing2: TSpTBXSubmenuItem
          Caption = 'Swing2'
          object MIJSlider: TSpTBXItem
            Tag = 15
            Caption = 'JSlider'
            OnClick = MISwingClick
          end
          object MIJProgressBar: TSpTBXItem
            Tag = 16
            Caption = 'JProgressBar'
            OnClick = MISwingClick
          end
          object MIJSplitPane: TSpTBXItem
            Tag = 17
            Caption = 'JSplitPane'
            OnClick = MISwingClick
          end
          object MIJTabbedPane: TSpTBXItem
            Tag = 18
            Caption = 'JTabbedPane'
            OnClick = MISwingClick
          end
          object MIJTable: TSpTBXItem
            Tag = 19
            Caption = 'JTable'
            OnClick = MISwingClick
          end
          object MIJTree: TSpTBXItem
            Tag = 23
            Caption = 'JTree'
            OnClick = MISwingClick
          end
          object MIJToolbar: TSpTBXItem
            Tag = 23
            Caption = 'JToolbar'
            OnClick = MISwingClick
          end
          object MIJSeparator: TSpTBXItem
            Tag = 24
            Caption = 'JSeparator'
            OnClick = MISwingClick
          end
          object MIJToggleButton: TSpTBXItem
            Tag = 25
            Caption = 'JToggleButton'
            OnClick = MISwingClick
          end
          object MIJPasswordField: TSpTBXItem
            Tag = 26
            Caption = 'JPasswordField'
            OnClick = MISwingClick
          end
          object MIJFormattedTextField: TSpTBXItem
            Tag = 27
            Caption = 'JFormattedTextField'
            OnClick = MISwingClick
          end
          object MIJEditorPane: TSpTBXItem
            Tag = 28
            Caption = 'JEditorPane'
            OnClick = MISwingClick
          end
          object MIJTextPane: TSpTBXItem
            Tag = 29
            Caption = 'JTextPane'
            OnClick = MISwingClick
          end
          object MIJLayeredPane: TSpTBXItem
            Tag = 30
            Caption = 'JLayeredPane'
            OnClick = MISwingClick
          end
          object MIJDesktopPane: TSpTBXItem
            Tag = 31
            Caption = 'JDesktopPane'
            OnClick = MISwingClick
          end
          object MIJInternalFrame: TSpTBXItem
            Tag = 32
            Caption = 'JInternalFrame'
            OnClick = MISwingClick
          end
          object MIJFileChooserOpen: TSpTBXItem
            Tag = 45
            Caption = 'JFileChooser open'
            OnClick = MISwingClick
          end
          object MIJFileChooserSave: TSpTBXItem
            Tag = 46
            Caption = 'JFileChooser save'
            OnClick = MISwingClick
          end
          object MIJColorChooser: TSpTBXItem
            Tag = 47
            Caption = 'JColorChooser'
            OnClick = MISwingClick
          end
          object MIJOptionPane: TSpTBXItem
            Tag = 48
            Caption = 'JOptionPane'
            OnClick = MISwingClick
          end
        end
        object MILayout: TSpTBXSubmenuItem
          Caption = 'Layout'
          object MIBorderLayout: TSpTBXItem
            Caption = 'BorderLayout'
            OnClick = TBLayoutClick
          end
          object MIFlowLayout: TSpTBXItem
            Tag = 1
            Caption = 'FlowLayout'
            OnClick = TBLayoutClick
          end
          object MIGridLayout: TSpTBXItem
            Tag = 2
            Caption = 'GridLayout'
            OnClick = TBLayoutClick
          end
          object MICardLayout: TSpTBXItem
            Tag = 3
            Caption = 'CardLayout'
            OnClick = TBLayoutClick
          end
          object MIGridBagLayout: TSpTBXItem
            Tag = 4
            Caption = 'GridBagLayout'
            OnClick = TBLayoutClick
          end
          object MIAbsoluteLayout: TSpTBXItem
            Tag = 5
            Caption = 'AbsoluteLayout'
            OnClick = TBLayoutClick
          end
        end
      end
      object MIWindow: TSpTBXSubmenuItem
        Caption = '&Window'
        object MIObjectInspector: TSpTBXItem
          Caption = 'Object inspector on/off'
          ImageIndex = 71
          ImageName = '71'
          ShortCut = 16457
          OnClick = MIObjectInspectorClick
        end
        object MIToolbar: TSpTBXItem
          Caption = 'Toolbar on/off'
          ImageIndex = 62
          ImageName = '62'
          ShortCut = 16460
          OnClick = MIToolbarClick
        end
        object MIMessages: TSpTBXItem
          Caption = 'Messages on/off'
          ImageIndex = 46
          ImageName = '46'
          ShortCut = 16461
          OnClick = MIMessagesClick
        end
        object MIMessagesDocked: TSpTBXItem
          Caption = 'Messages docked/undocked'
          ShortCut = 49229
          Visible = False
          OnClick = MIMessagesDockedClick
        end
        object MIFileStructure: TSpTBXItem
          Caption = 'Structure on/off'
          ImageIndex = 88
          ImageName = '88'
          ShortCut = 16452
          OnClick = MIFileStructureClick
        end
        object MIDefaultLayout: TSpTBXItem
          Caption = 'Default layout'
          ImageIndex = 93
          ImageName = '93'
          OnClick = MIDefaultLayoutClick
        end
        object MIMsDos: TSpTBXItem
          Caption = 'DOS window'
          ImageIndex = 4
          ImageName = '04'
          OnClick = MIMSDosClick
        end
        object MIExplorer: TSpTBXItem
          Caption = 'Explorer'
          ImageIndex = 70
          ImageName = '70'
          ShortCut = 16453
          OnClick = MIExplorerClick
        end
        object MIBrowser: TSpTBXItem
          Caption = 'Browser'
          ImageIndex = 66
          ImageName = '66'
          OnClick = MIBrowserClick
        end
        object MIStrich: TSpTBXSeparatorItem
        end
        object MIFont: TSpTBXItem
          Caption = 'Font'
          ImageIndex = 27
          ImageName = '27'
          OnClick = MIFontClick
        end
        object MIConfiguration: TSpTBXItem
          Caption = 'Configuration'
          ImageIndex = 28
          ImageName = '28'
          OnClick = MIConfigurationClick
        end
        object N14: TSpTBXSeparatorItem
        end
        object MIMaximized: TSpTBXItem
          Caption = 'Maximize'
          ImageIndex = 5
          ImageName = '05'
          OnClick = MIMaximizedClick
        end
        object MICascade: TSpTBXItem
          Caption = 'Cascade'
          ImageIndex = 24
          ImageName = '24'
          OnClick = MICascadeClick
        end
        object MITileVertical: TSpTBXItem
          Caption = 'Tile vertically'
          ImageIndex = 25
          ImageName = '25'
          OnClick = MITileVerticalClick
        end
        object MITileHorizontal: TSpTBXItem
          Caption = 'Tile horizontally'
          ImageIndex = 6
          ImageName = '06'
          OnClick = MITileHorizontalClick
        end
        object N11: TSpTBXSeparatorItem
        end
      end
      object MIHelp: TSpTBXSubmenuItem
        Caption = '&Help'
        object MIHelpHelp: TSpTBXItem
          Caption = 'Index'
          ImageIndex = 11
          ImageName = '11'
          OnClick = MIHelpHelpClick
        end
        object MIAPI: TSpTBXItem
          Caption = 'API'
          ImageIndex = 33
          ImageName = '33'
          ShortCut = 112
          OnClick = MIAPIClick
        end
        object MIJavaFx: TSpTBXItem
          Caption = 'JavaFX'
          OnClick = MIJavaFxClick
        end
        object MIJunitManual: TSpTBXItem
          Caption = 'JUnit'
          OnClick = MIJunitManualClick
        end
        object N7: TSpTBXSeparatorItem
        end
        object MIDemos: TSpTBXItem
          Caption = 'Demos'
          OnClick = MIDemosClick
        end
        object MITutorial: TSpTBXItem
          Caption = 'Tutorial'
          ImageIndex = 31
          ImageName = '31'
          OnClick = MITutorialClick
        end
        object MIJavabook: TSpTBXItem
          Caption = 'Java book'
          OnClick = MIJavabookClick
        end
        object MIMindstormsHelp: TSpTBXItem
          Caption = 'Mindstorms'
          ImageIndex = 61
          ImageName = '61'
          OnClick = MIMindstormsHelpClick
        end
        object N2: TSpTBXSeparatorItem
        end
        object MIWebsite: TSpTBXItem
          Caption = 'Website'
          ImageIndex = 65
          ImageName = '65'
          OnClick = MIWebsiteClick
        end
        object MIUpdate: TSpTBXItem
          Caption = 'Update'
          OnClick = MIUpdateClick
        end
        object MIAbout: TSpTBXItem
          Caption = 'Info'
          ImageIndex = 69
          ImageName = '69'
          OnClick = MIAboutClick
        end
        object MIDebug: TSpTBXItem
          Caption = 'Debug'
          ShortCut = 49220
          Visible = False
          OnClick = MIDebugClick
        end
      end
    end
  end
  object ODOpen: TOpenDialog
    DefaultExt = '.java'
    Filter = 
      'Java-Editor|*.java;*.jfm;*.uml;*.html;*.htm;*.jep;*.jsp;*.jsd;*.' +
      'txt;*.jar|Java (*.java)|*.java|GUI (*.jfm)|*.jfm|UML (*.uml)|*.u' +
      'ml|HTML (*.html)|*.html;*.htm|Project (*.jep)|*.jep|Text (*.txt)' +
      '|*.txt|Jar (*.jar)|*.jar|JSP (*.jsp)|*.jsp|JSD (*.jsd)|*.jsd|Bac' +
      'kup (*.~ava)|*.~ava|All (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofNoChangeDir, ofAllowMultiSelect, ofEnableSizing]
    Left = 108
    Top = 100
  end
  object FDFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 4
    Options = [fdFixedPitchOnly]
    Left = 248
    Top = 100
  end
  object SDSaveAs: TSaveDialog
    DefaultExt = '.java'
    Filter = 
      'Java (*.java)|*.java|HTML (*.html)|*.html;*.htm|Text (*.txt)|*.t' +
      'xt|All (*.*)|*.*'
    Left = 32
    Top = 100
  end
  object System: TDdeServerConv
    OnExecuteMacro = SystemExecuteMacro
    Left = 184
    Top = 100
  end
  object EditorAgeTimer: TTimer
    Enabled = False
    OnTimer = EditorAgeTimerTimer
    Left = 40
    Top = 168
  end
  object ActiveWindowTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = ActiveWindowTimerTimer
    Left = 152
    Top = 168
  end
  object CloseTimer: TTimer
    Enabled = False
    OnTimer = CloseTimerTimer
    Left = 248
    Top = 168
  end
  object StyleTimer: TTimer
    OnTimer = StyleTimerTimer
    Left = 320
    Top = 168
  end
  object vilSwing2: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'JSlider'
        Name = 'JSlider'
      end
      item
        CollectionIndex = 1
        CollectionName = 'JProgressBar'
        Name = 'JProgressBar'
      end
      item
        CollectionIndex = 2
        CollectionName = 'JSplitPane'
        Name = 'JSplitPane'
      end
      item
        CollectionIndex = 3
        CollectionName = 'JTabbedPane'
        Name = 'JTabbedPane'
      end
      item
        CollectionIndex = 4
        CollectionName = 'JTable'
        Name = 'JTable'
      end
      item
        CollectionIndex = 5
        CollectionName = 'JTree'
        Name = 'JTree'
      end
      item
        CollectionIndex = 6
        CollectionName = 'JToolBar'
        Name = 'JToolBar'
      end
      item
        CollectionIndex = 7
        CollectionName = 'JSeparator'
        Name = 'JSeparator'
      end
      item
        CollectionIndex = 8
        CollectionName = 'JToggleButton'
        Name = 'JToggleButton'
      end
      item
        CollectionIndex = 9
        CollectionName = 'JPasswordField'
        Name = 'JPasswordField'
      end
      item
        CollectionIndex = 10
        CollectionName = 'JFormattedTextField'
        Name = 'JFormattedTextField'
      end
      item
        CollectionIndex = 11
        CollectionName = 'JEditorPane'
        Name = 'JEditorPane'
      end
      item
        CollectionIndex = 12
        CollectionName = 'JTextPane'
        Name = 'JTextPane'
      end
      item
        CollectionIndex = 13
        CollectionName = 'JLayeredPane'
        Name = 'JLayeredPane'
      end
      item
        CollectionIndex = 14
        CollectionName = 'JDesktopPane'
        Name = 'JDesktopPane'
      end
      item
        CollectionIndex = 15
        CollectionName = 'JInternalFrame'
        Name = 'JInternalFrame'
      end
      item
        CollectionIndex = 16
        CollectionName = 'JFileSave'
        Name = 'JFileSave'
      end
      item
        CollectionIndex = 17
        CollectionName = 'JFileOpen'
        Name = 'JFileOpen'
      end
      item
        CollectionIndex = 18
        CollectionName = 'JColorChooser'
        Name = 'JColorChooser'
      end
      item
        CollectionIndex = 19
        CollectionName = 'JOptionPane'
        Name = 'JOptionPane'
      end>
    ImageCollection = DMImages.icSwing2
    Width = 21
    Height = 21
    Left = 224
    Top = 312
  end
  object vilFXBaseDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 24
        CollectionName = 'Dark\JLabel'
        Name = 'JLabel'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 25
        CollectionName = 'JSpinner'
        Name = 'JSpinner'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 26
        CollectionName = 'FXTurtle'
        Name = 'FXTurtle'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Dark\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Dark\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Dark\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 27
        CollectionName = 'FXMenuButton'
        Name = 'FXMenuButton'
      end
      item
        CollectionIndex = 28
        CollectionName = 'FXMenuSplitButton'
        Name = 'FXMenuSplitButton'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 136
    Top = 376
  end
  object vilFXBaseLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 23
        CollectionName = 'Light\JLabel'
        Name = 'JLabel'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 25
        CollectionName = 'JSpinner'
        Name = 'JSpinner'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 26
        CollectionName = 'FXTurtle'
        Name = 'FXTurtle'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Light\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Light\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Light\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 27
        CollectionName = 'FXMenuButton'
        Name = 'FXMenuButton'
      end
      item
        CollectionIndex = 28
        CollectionName = 'FXMenuSplitButton'
        Name = 'FXMenuSplitButton'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 40
    Top = 376
  end
  object vilSwing1Dark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 24
        CollectionName = 'Dark\JLabel'
        Name = 'JLabel'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 25
        CollectionName = 'JSpinner'
        Name = 'JSpinner'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Light\Scrollbar'
        Name = 'Scrollbar'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Dark\Turtle'
        Name = 'Turtle'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Dark\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Dark\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Dark\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 136
    Top = 312
  end
  object vilSwing1Light: TVirtualImageList
    Images = <
      item
        CollectionIndex = 23
        CollectionName = 'Light\JLabel'
        Name = 'JLabel'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 25
        CollectionName = 'JSpinner'
        Name = 'JSpinner'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Light\Scrollbar'
        Name = 'Scrollbar'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Light\Turtle'
        Name = 'Turtle'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Light\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Light\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Light\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 40
    Top = 312
  end
  object vilAWTDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 18
        CollectionName = 'Dark\Label'
        Name = 'Label'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Light\Scrollbar'
        Name = 'Scrollbar'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Dark\Turtle'
        Name = 'Turtle'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Dark\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Dark\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Dark\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 136
    Top = 248
  end
  object vilAWTLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Light\Label'
        Name = 'Label'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Light\TextField'
        Name = 'TextField'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\NumberField'
        Name = 'NumberField'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\TextArea'
        Name = 'TextArea'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\Button'
        Name = 'Button'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\Checkbox'
        Name = 'Checkbox'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Light\CheckboxGroup'
        Name = 'CheckboxGroup'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Light\List'
        Name = 'List'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Light\Choice'
        Name = 'Choice'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Light\Scrollbar'
        Name = 'Scrollbar'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Light\ScrollPane'
        Name = 'ScrollPane'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Panel'
        Name = 'Panel'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Canvas'
        Name = 'Canvas'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Light\Turtle'
        Name = 'Turtle'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Light\MenuBar'
        Name = 'MenuBar'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Light\Menu'
        Name = 'Menu'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Light\PopupMenu'
        Name = 'PopupMenu'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icAWTSwing1
    Width = 21
    Height = 21
    Left = 40
    Top = 248
  end
  object vilFXControls: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Slider'
        Name = 'Slider'
      end
      item
        CollectionIndex = 1
        CollectionName = 'ProgressBar'
        Name = 'ProgressBar'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ProgressIndicator'
        Name = 'ProgressIndicator'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Toolbar'
        Name = 'Toolbar'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Separator'
        Name = 'Separator'
      end
      item
        CollectionIndex = 5
        CollectionName = 'ToggleButton'
        Name = 'ToggleButton'
      end
      item
        CollectionIndex = 6
        CollectionName = 'PasswordField'
        Name = 'PasswordField'
      end
      item
        CollectionIndex = 7
        CollectionName = 'ChoiceBox'
        Name = 'ChoiceBox'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Hyperlink'
        Name = 'Hyperlink'
      end
      item
        CollectionIndex = 9
        CollectionName = 'HTMLEditor'
        Name = 'HTMLEditor'
      end
      item
        CollectionIndex = 10
        CollectionName = 'WebView'
        Name = 'WebView'
      end
      item
        CollectionIndex = 11
        CollectionName = 'ColorPicker'
        Name = 'ColorPicker'
      end
      item
        CollectionIndex = 12
        CollectionName = 'DatePicker'
        Name = 'DatePicker'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Pages1'
        Name = 'Pages1'
      end
      item
        CollectionIndex = 14
        CollectionName = 'FileOpenChooser'
        Name = 'FileOpenChooser'
      end
      item
        CollectionIndex = 15
        CollectionName = 'FileSaveChooser'
        Name = 'FileSaveChooser'
      end
      item
        CollectionIndex = 16
        CollectionName = 'DirectoryChooser'
        Name = 'DirectoryChooser'
      end
      item
        CollectionIndex = 17
        CollectionName = 'ImageView'
        Name = 'ImageView'
      end
      item
        CollectionIndex = 18
        CollectionName = 'MediaView'
        Name = 'MediaView'
      end
      item
        CollectionIndex = 19
        CollectionName = 'TableView'
        Name = 'TableView'
      end
      item
        CollectionIndex = 20
        CollectionName = 'TreeView'
        Name = 'TreeView'
      end>
    ImageCollection = DMImages.icFXControls
    Width = 21
    Height = 21
    Left = 224
    Top = 376
  end
  object vilFXShapesDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 11
        CollectionName = 'Circle'
        Name = 'Circle'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Rectangle'
        Name = 'Rectangle'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Ellipse'
        Name = 'Ellipse'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Polygon'
        Name = 'Polygon'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Polyline'
        Name = 'Polyline'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Arc'
        Name = 'Arc'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Line1'
        Name = 'Line1'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Text'
        Name = 'Text'
      end
      item
        CollectionIndex = 19
        CollectionName = 'QuadCurve'
        Name = 'QuadCurve'
      end
      item
        CollectionIndex = 20
        CollectionName = 'CubicCurve'
        Name = 'CubicCurve'
      end
      item
        CollectionIndex = 21
        CollectionName = 'SVGPath'
        Name = 'SVGPath'
      end>
    ImageCollection = DMImages.icShapes
    Width = 21
    Height = 21
    Left = 424
    Top = 376
  end
  object vilFXShapesLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Circle'
        Name = 'Circle'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Rectangle'
        Name = 'Rectangle'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Ellipse'
        Name = 'Ellipse'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Polygon'
        Name = 'Polygon'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Polyline'
        Name = 'Polyline'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Arc'
        Name = 'Arc'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Line1'
        Name = 'Line1'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Text'
        Name = 'Text'
      end
      item
        CollectionIndex = 8
        CollectionName = 'QuadCurve'
        Name = 'QuadCurve'
      end
      item
        CollectionIndex = 9
        CollectionName = 'CubicCurve'
        Name = 'CubicCurve'
      end
      item
        CollectionIndex = 10
        CollectionName = 'SVGPath'
        Name = 'SVGPath'
      end>
    ImageCollection = DMImages.icShapes
    Width = 21
    Height = 21
    Left = 320
    Top = 376
  end
  object vilProgramLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'New1'
        Name = 'New1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'NewClass'
        Name = 'NewClass'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Structogram1'
        Name = 'Structogram1'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Sequencediagram'
        Name = 'Sequencediagram'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Console'
        Name = 'Console'
      end
      item
        CollectionIndex = 5
        CollectionName = 'AWT'
        Name = 'AWT'
      end
      item
        CollectionIndex = 6
        CollectionName = 'AWTDialog'
        Name = 'AWTDialog'
      end
      item
        CollectionIndex = 7
        CollectionName = 'AWTApplet'
        Name = 'AWTApplet'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Swing'
        Name = 'Swing'
      end
      item
        CollectionIndex = 9
        CollectionName = 'SwingDialog'
        Name = 'SwingDialog'
      end
      item
        CollectionIndex = 10
        CollectionName = 'SwingApplet'
        Name = 'SwingApplet'
      end
      item
        CollectionIndex = 11
        CollectionName = 'JavaFX'
        Name = 'JavaFX'
      end>
    ImageCollection = DMImages.icProgram
    Width = 23
    Height = 17
    Left = 40
    Top = 440
  end
  object vilProgramDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 12
        CollectionName = 'New1'
        Name = 'New1'
      end
      item
        CollectionIndex = 13
        CollectionName = 'NewClass'
        Name = 'NewClass'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Structogram1'
        Name = 'Structogram1'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Sequencediagram'
        Name = 'Sequencediagram'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Console'
        Name = 'Console'
      end
      item
        CollectionIndex = 5
        CollectionName = 'AWT'
        Name = 'AWT'
      end
      item
        CollectionIndex = 6
        CollectionName = 'AWTDialog'
        Name = 'AWTDialog'
      end
      item
        CollectionIndex = 7
        CollectionName = 'AWTApplet'
        Name = 'AWTApplet'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Swing'
        Name = 'Swing'
      end
      item
        CollectionIndex = 9
        CollectionName = 'SwingDialog'
        Name = 'SwingDialog'
      end
      item
        CollectionIndex = 10
        CollectionName = 'SwingApplet'
        Name = 'SwingApplet'
      end
      item
        CollectionIndex = 11
        CollectionName = 'JavaFX'
        Name = 'JavaFX'
      end>
    ImageCollection = DMImages.icProgram
    Width = 23
    Height = 17
    Left = 144
    Top = 440
  end
  object vilUtilities: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'TurtleWorld'
        Name = 'TurtleWorld'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Turtle'
        Name = 'Turtle'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Timer'
        Name = 'Timer'
      end>
    ImageCollection = DMImages.icUtilities
    Width = 21
    Height = 21
    Left = 232
    Top = 440
  end
  object vilLayoutLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'BorderLayout'
        Name = 'BorderLayout'
      end
      item
        CollectionIndex = 1
        CollectionName = 'FlowLayout'
        Name = 'FlowLayout'
      end
      item
        CollectionIndex = 2
        CollectionName = 'CardLayout'
        Name = 'CardLayout'
      end
      item
        CollectionIndex = 3
        CollectionName = 'GridBagLayout'
        Name = 'GridBagLayout'
      end
      item
        CollectionIndex = 4
        CollectionName = 'GridLayout'
        Name = 'GridLayout'
      end
      item
        CollectionIndex = 5
        CollectionName = 'AbsoluteLayout'
        Name = 'AbsoluteLayout'
      end>
    ImageCollection = DMImages.icLayout
    Width = 19
    Height = 21
    Left = 320
    Top = 440
  end
  object vilToolbarLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end>
    ImageCollection = DMImages.icToolbarLight
    Height = 20
    Left = 560
    Top = 432
  end
  object vilToolbarDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end>
    ImageCollection = DMImages.icToolbarDark
    Height = 20
    Left = 656
    Top = 432
  end
  object vilToolbarDisabledLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end>
    ImageCollection = DMImages.icToolbarDisabledLight
    Height = 20
    Left = 776
    Top = 360
  end
  object vilToolbarDisabledDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end>
    ImageCollection = DMImages.icToolbarDisabledDark
    Height = 20
    Left = 776
    Top = 432
  end
  object vilMenuLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end
      item
        CollectionIndex = 18
        CollectionName = '18'
        Name = '18'
      end
      item
        CollectionIndex = 19
        CollectionName = '19'
        Name = '19'
      end
      item
        CollectionIndex = 20
        CollectionName = '20'
        Name = '20'
      end
      item
        CollectionIndex = 21
        CollectionName = '21'
        Name = '21'
      end
      item
        CollectionIndex = 22
        CollectionName = '22'
        Name = '22'
      end
      item
        CollectionIndex = 23
        CollectionName = '23'
        Name = '23'
      end
      item
        CollectionIndex = 24
        CollectionName = '24'
        Name = '24'
      end
      item
        CollectionIndex = 25
        CollectionName = '25'
        Name = '25'
      end
      item
        CollectionIndex = 26
        CollectionName = '26'
        Name = '26'
      end
      item
        CollectionIndex = 27
        CollectionName = '27'
        Name = '27'
      end
      item
        CollectionIndex = 28
        CollectionName = '28'
        Name = '28'
      end
      item
        CollectionIndex = 29
        CollectionName = '29'
        Name = '29'
      end
      item
        CollectionIndex = 30
        CollectionName = '30'
        Name = '30'
      end
      item
        CollectionIndex = 31
        CollectionName = '31'
        Name = '31'
      end
      item
        CollectionIndex = 32
        CollectionName = '32'
        Name = '32'
      end
      item
        CollectionIndex = 33
        CollectionName = '33'
        Name = '33'
      end
      item
        CollectionIndex = 34
        CollectionName = '34'
        Name = '34'
      end
      item
        CollectionIndex = 35
        CollectionName = '35'
        Name = '35'
      end
      item
        CollectionIndex = 36
        CollectionName = '36'
        Name = '36'
      end
      item
        CollectionIndex = 37
        CollectionName = '37'
        Name = '37'
      end
      item
        CollectionIndex = 38
        CollectionName = '38'
        Name = '38'
      end
      item
        CollectionIndex = 39
        CollectionName = '39'
        Name = '39'
      end
      item
        CollectionIndex = 40
        CollectionName = '40'
        Name = '40'
      end
      item
        CollectionIndex = 41
        CollectionName = '41'
        Name = '41'
      end
      item
        CollectionIndex = 42
        CollectionName = '42'
        Name = '42'
      end
      item
        CollectionIndex = 43
        CollectionName = '43'
        Name = '43'
      end
      item
        CollectionIndex = 44
        CollectionName = '44'
        Name = '44'
      end
      item
        CollectionIndex = 45
        CollectionName = '45'
        Name = '45'
      end
      item
        CollectionIndex = 46
        CollectionName = '46'
        Name = '46'
      end
      item
        CollectionIndex = 47
        CollectionName = '47'
        Name = '47'
      end
      item
        CollectionIndex = 48
        CollectionName = '48'
        Name = '48'
      end
      item
        CollectionIndex = 49
        CollectionName = '49'
        Name = '49'
      end
      item
        CollectionIndex = 50
        CollectionName = '50'
        Name = '50'
      end
      item
        CollectionIndex = 51
        CollectionName = '51'
        Name = '51'
      end
      item
        CollectionIndex = 52
        CollectionName = '52'
        Name = '52'
      end
      item
        CollectionIndex = 53
        CollectionName = '53'
        Name = '53'
      end
      item
        CollectionIndex = 54
        CollectionName = '54'
        Name = '54'
      end
      item
        CollectionIndex = 55
        CollectionName = '55'
        Name = '55'
      end
      item
        CollectionIndex = 56
        CollectionName = '56'
        Name = '56'
      end
      item
        CollectionIndex = 57
        CollectionName = '57'
        Name = '57'
      end
      item
        CollectionIndex = 58
        CollectionName = '58'
        Name = '58'
      end
      item
        CollectionIndex = 59
        CollectionName = '59'
        Name = '59'
      end
      item
        CollectionIndex = 60
        CollectionName = '60'
        Name = '60'
      end
      item
        CollectionIndex = 61
        CollectionName = '61'
        Name = '61'
      end
      item
        CollectionIndex = 62
        CollectionName = '62'
        Name = '62'
      end
      item
        CollectionIndex = 63
        CollectionName = '63'
        Name = '63'
      end
      item
        CollectionIndex = 64
        CollectionName = '64'
        Name = '64'
      end
      item
        CollectionIndex = 65
        CollectionName = '65'
        Name = '65'
      end
      item
        CollectionIndex = 66
        CollectionName = '66'
        Name = '66'
      end
      item
        CollectionIndex = 67
        CollectionName = '67'
        Name = '67'
      end
      item
        CollectionIndex = 68
        CollectionName = '68'
        Name = '68'
      end
      item
        CollectionIndex = 69
        CollectionName = '69'
        Name = '69'
      end
      item
        CollectionIndex = 70
        CollectionName = '70'
        Name = '70'
      end
      item
        CollectionIndex = 71
        CollectionName = '71'
        Name = '71'
      end
      item
        CollectionIndex = 72
        CollectionName = '72'
        Name = '72'
      end
      item
        CollectionIndex = 73
        CollectionName = '73'
        Name = '73'
      end
      item
        CollectionIndex = 74
        CollectionName = '74'
        Name = '74'
      end
      item
        CollectionIndex = 75
        CollectionName = '75'
        Name = '75'
      end
      item
        CollectionIndex = 76
        CollectionName = '76'
        Name = '76'
      end
      item
        CollectionIndex = 77
        CollectionName = '77'
        Name = '77'
      end
      item
        CollectionIndex = 78
        CollectionName = '78'
        Name = '78'
      end
      item
        CollectionIndex = 79
        CollectionName = '79'
        Name = '79'
      end
      item
        CollectionIndex = 80
        CollectionName = '80'
        Name = '80'
      end
      item
        CollectionIndex = 81
        CollectionName = '81'
        Name = '81'
      end
      item
        CollectionIndex = 82
        CollectionName = '82'
        Name = '82'
      end
      item
        CollectionIndex = 83
        CollectionName = '83'
        Name = '83'
      end
      item
        CollectionIndex = 84
        CollectionName = '84'
        Name = '84'
      end
      item
        CollectionIndex = 85
        CollectionName = '85'
        Name = '85'
      end
      item
        CollectionIndex = 86
        CollectionName = '86'
        Name = '86'
      end
      item
        CollectionIndex = 87
        CollectionName = '87'
        Name = '87'
      end
      item
        CollectionIndex = 88
        CollectionName = '88'
        Name = '88'
      end
      item
        CollectionIndex = 89
        CollectionName = '89'
        Name = '89'
      end
      item
        CollectionIndex = 90
        CollectionName = '90'
        Name = '90'
      end
      item
        CollectionIndex = 91
        CollectionName = '91'
        Name = '91'
      end
      item
        CollectionIndex = 92
        CollectionName = '92'
        Name = '92'
      end
      item
        CollectionIndex = 93
        CollectionName = '93'
        Name = '93'
      end
      item
        CollectionIndex = 94
        CollectionName = '94'
        Name = '94'
      end>
    ImageCollection = DMImages.icMenuLight
    Left = 424
    Top = 152
  end
  object vilMenuDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end
      item
        CollectionIndex = 6
        CollectionName = '06'
        Name = '06'
      end
      item
        CollectionIndex = 7
        CollectionName = '07'
        Name = '07'
      end
      item
        CollectionIndex = 8
        CollectionName = '08'
        Name = '08'
      end
      item
        CollectionIndex = 9
        CollectionName = '09'
        Name = '09'
      end
      item
        CollectionIndex = 10
        CollectionName = '10'
        Name = '10'
      end
      item
        CollectionIndex = 11
        CollectionName = '11'
        Name = '11'
      end
      item
        CollectionIndex = 12
        CollectionName = '12'
        Name = '12'
      end
      item
        CollectionIndex = 13
        CollectionName = '13'
        Name = '13'
      end
      item
        CollectionIndex = 14
        CollectionName = '14'
        Name = '14'
      end
      item
        CollectionIndex = 15
        CollectionName = '15'
        Name = '15'
      end
      item
        CollectionIndex = 16
        CollectionName = '16'
        Name = '16'
      end
      item
        CollectionIndex = 17
        CollectionName = '17'
        Name = '17'
      end
      item
        CollectionIndex = 18
        CollectionName = '18'
        Name = '18'
      end
      item
        CollectionIndex = 19
        CollectionName = '19'
        Name = '19'
      end
      item
        CollectionIndex = 20
        CollectionName = '20'
        Name = '20'
      end
      item
        CollectionIndex = 21
        CollectionName = '21'
        Name = '21'
      end
      item
        CollectionIndex = 22
        CollectionName = '22'
        Name = '22'
      end
      item
        CollectionIndex = 23
        CollectionName = '23'
        Name = '23'
      end
      item
        CollectionIndex = 24
        CollectionName = '24'
        Name = '24'
      end
      item
        CollectionIndex = 25
        CollectionName = '25'
        Name = '25'
      end
      item
        CollectionIndex = 26
        CollectionName = '26'
        Name = '26'
      end
      item
        CollectionIndex = 27
        CollectionName = '27'
        Name = '27'
      end
      item
        CollectionIndex = 28
        CollectionName = '28'
        Name = '28'
      end
      item
        CollectionIndex = 29
        CollectionName = '29'
        Name = '29'
      end
      item
        CollectionIndex = 30
        CollectionName = '30'
        Name = '30'
      end
      item
        CollectionIndex = 31
        CollectionName = '31'
        Name = '31'
      end
      item
        CollectionIndex = 32
        CollectionName = '32'
        Name = '32'
      end
      item
        CollectionIndex = 33
        CollectionName = '33'
        Name = '33'
      end
      item
        CollectionIndex = 34
        CollectionName = '34'
        Name = '34'
      end
      item
        CollectionIndex = 35
        CollectionName = '35'
        Name = '35'
      end
      item
        CollectionIndex = 36
        CollectionName = '36'
        Name = '36'
      end
      item
        CollectionIndex = 37
        CollectionName = '37'
        Name = '37'
      end
      item
        CollectionIndex = 38
        CollectionName = '38'
        Name = '38'
      end
      item
        CollectionIndex = 39
        CollectionName = '39'
        Name = '39'
      end
      item
        CollectionIndex = 40
        CollectionName = '40'
        Name = '40'
      end
      item
        CollectionIndex = 41
        CollectionName = '41'
        Name = '41'
      end
      item
        CollectionIndex = 42
        CollectionName = '42'
        Name = '42'
      end
      item
        CollectionIndex = 43
        CollectionName = '43'
        Name = '43'
      end
      item
        CollectionIndex = 44
        CollectionName = '44'
        Name = '44'
      end
      item
        CollectionIndex = 45
        CollectionName = '45'
        Name = '45'
      end
      item
        CollectionIndex = 46
        CollectionName = '46'
        Name = '46'
      end
      item
        CollectionIndex = 47
        CollectionName = '47'
        Name = '47'
      end
      item
        CollectionIndex = 48
        CollectionName = '48'
        Name = '48'
      end
      item
        CollectionIndex = 49
        CollectionName = '49'
        Name = '49'
      end
      item
        CollectionIndex = 50
        CollectionName = '50'
        Name = '50'
      end
      item
        CollectionIndex = 51
        CollectionName = '51'
        Name = '51'
      end
      item
        CollectionIndex = 52
        CollectionName = '52'
        Name = '52'
      end
      item
        CollectionIndex = 53
        CollectionName = '53'
        Name = '53'
      end
      item
        CollectionIndex = 54
        CollectionName = '54'
        Name = '54'
      end
      item
        CollectionIndex = 55
        CollectionName = '55'
        Name = '55'
      end
      item
        CollectionIndex = 56
        CollectionName = '56'
        Name = '56'
      end
      item
        CollectionIndex = 57
        CollectionName = '57'
        Name = '57'
      end
      item
        CollectionIndex = 58
        CollectionName = '58'
        Name = '58'
      end
      item
        CollectionIndex = 59
        CollectionName = '59'
        Name = '59'
      end
      item
        CollectionIndex = 60
        CollectionName = '60'
        Name = '60'
      end
      item
        CollectionIndex = 61
        CollectionName = '61'
        Name = '61'
      end
      item
        CollectionIndex = 62
        CollectionName = '62'
        Name = '62'
      end
      item
        CollectionIndex = 63
        CollectionName = '63'
        Name = '63'
      end
      item
        CollectionIndex = 64
        CollectionName = '64'
        Name = '64'
      end
      item
        CollectionIndex = 65
        CollectionName = '65'
        Name = '65'
      end
      item
        CollectionIndex = 66
        CollectionName = '66'
        Name = '66'
      end
      item
        CollectionIndex = 67
        CollectionName = '67'
        Name = '67'
      end
      item
        CollectionIndex = 68
        CollectionName = '68'
        Name = '68'
      end
      item
        CollectionIndex = 69
        CollectionName = '69'
        Name = '69'
      end
      item
        CollectionIndex = 70
        CollectionName = '70'
        Name = '70'
      end
      item
        CollectionIndex = 71
        CollectionName = '71'
        Name = '71'
      end
      item
        CollectionIndex = 72
        CollectionName = '72'
        Name = '72'
      end
      item
        CollectionIndex = 73
        CollectionName = '73'
        Name = '73'
      end
      item
        CollectionIndex = 74
        CollectionName = '74'
        Name = '74'
      end
      item
        CollectionIndex = 75
        CollectionName = '75'
        Name = '75'
      end
      item
        CollectionIndex = 76
        CollectionName = '76'
        Name = '76'
      end
      item
        CollectionIndex = 77
        CollectionName = '77'
        Name = '77'
      end
      item
        CollectionIndex = 78
        CollectionName = '78'
        Name = '78'
      end
      item
        CollectionIndex = 79
        CollectionName = '79'
        Name = '79'
      end
      item
        CollectionIndex = 80
        CollectionName = '80'
        Name = '80'
      end
      item
        CollectionIndex = 81
        CollectionName = '81'
        Name = '81'
      end
      item
        CollectionIndex = 82
        CollectionName = '82'
        Name = '82'
      end
      item
        CollectionIndex = 83
        CollectionName = '83'
        Name = '83'
      end
      item
        CollectionIndex = 84
        CollectionName = '84'
        Name = '84'
      end
      item
        CollectionIndex = 85
        CollectionName = '85'
        Name = '85'
      end
      item
        CollectionIndex = 86
        CollectionName = '86'
        Name = '86'
      end
      item
        CollectionIndex = 87
        CollectionName = '87'
        Name = '87'
      end
      item
        CollectionIndex = 88
        CollectionName = '88'
        Name = '88'
      end
      item
        CollectionIndex = 89
        CollectionName = '89'
        Name = '89'
      end
      item
        CollectionIndex = 90
        CollectionName = '90'
        Name = '90'
      end
      item
        CollectionIndex = 91
        CollectionName = '91'
        Name = '91'
      end
      item
        CollectionIndex = 92
        CollectionName = '92'
        Name = '92'
      end
      item
        CollectionIndex = 93
        CollectionName = '93'
        Name = '93'
      end
      item
        CollectionIndex = 94
        CollectionName = '94'
        Name = '94'
      end>
    ImageCollection = DMImages.icMenuDark
    Left = 536
    Top = 152
  end
  object vilLayoutDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 6
        CollectionName = 'BorderLayout'
        Name = 'BorderLayout'
      end
      item
        CollectionIndex = 7
        CollectionName = 'FlowLayout'
        Name = 'FlowLayout'
      end
      item
        CollectionIndex = 8
        CollectionName = 'CardLayout'
        Name = 'CardLayout'
      end
      item
        CollectionIndex = 9
        CollectionName = 'GridBagLayout'
        Name = 'GridBagLayout'
      end
      item
        CollectionIndex = 10
        CollectionName = 'GridLayout'
        Name = 'GridLayout'
      end
      item
        CollectionIndex = 11
        CollectionName = 'AbsoluteLayout'
        Name = 'AbsoluteLayout'
      end>
    ImageCollection = DMImages.icLayout
    Left = 424
    Top = 442
  end
end
