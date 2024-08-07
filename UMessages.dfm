object FMessages: TFMessages
  Left = 332
  Top = 230
  BorderIcons = [biSystemMenu]
  Caption = 'Messages'
  ClientHeight = 615
  ClientWidth = 859
  Color = clBtnFace
  UseDockManager = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseActivate = FormMouseActivate
  OnPaint = FormPaint
  OnShow = FormShow
  TextHeight = 15
  object PMain: TPanel
    Left = 0
    Top = 20
    Width = 859
    Height = 578
    Align = alClient
    Constraints.MinHeight = 40
    TabOrder = 0
    OnExit = PMainExit
    ExplicitWidth = 867
    ExplicitHeight = 580
    object TVSearch: TTreeView
      Tag = 6
      Left = 1
      Top = 1
      Width = 861
      Height = 577
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      HideSelection = False
      Indent = 19
      MultiSelect = True
      MultiSelectStyle = [msControlSelect, msShiftSelect]
      PopupMenu = PMMessages
      RightClickSelect = True
      TabOrder = 1
      OnDblClick = TVSearchDblClick
      OnEnter = TVSearchEnter
      OnKeyUp = TVSearchKeyUp
    end
    object LBMessages: TListBox
      Tag = 7
      Left = 1
      Top = 1
      Width = 857
      Height = 576
      Align = alClient
      ItemHeight = 15
      MultiSelect = True
      PopupMenu = PMMessages
      TabOrder = 2
      OnDblClick = LBMessagesDblClick
      OnEnter = LBMessagesEnter
    end
    object LBCompiler: TListBox
      Tag = 1
      Left = 1
      Top = 1
      Width = 857
      Height = 576
      Align = alClient
      ItemHeight = 15
      MultiSelect = True
      PopupMenu = PMMessages
      TabOrder = 4
      OnDblClick = LBCompilerDblClick
      OnEnter = LBCompilerEnter
    end
    object PDebugger: TPanel
      Left = 1
      Top = 1
      Width = 857
      Height = 576
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 865
      ExplicitHeight = 578
      object SplitterLeft: TSplitter
        Left = 187
        Top = 0
        Width = 4
        Height = 577
        ExplicitLeft = 160
        ExplicitHeight = 194
      end
      object SplitterCenter: TSplitter
        Left = 351
        Top = 0
        Width = 4
        Height = 577
        ExplicitLeft = 324
        ExplicitHeight = 194
      end
      object SplitterRight: TSplitter
        Left = 515
        Top = 0
        Width = 4
        Height = 577
        ExplicitLeft = 488
        ExplicitHeight = 194
      end
      object PDebuggerLeft: TPanel
        Left = 27
        Top = 0
        Width = 160
        Height = 577
        Align = alLeft
        BevelOuter = bvNone
        Constraints.MinHeight = 20
        Constraints.MinWidth = 20
        TabOrder = 0
        OnEnter = PDebuggerLeftEnter
        ExplicitHeight = 578
        object PAttribute: TPanel
          Left = 0
          Top = 0
          Width = 160
          Height = 13
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Attributes'
          TabOrder = 1
        end
        object TVAttributes: TTreeView
          Tag = 2
          Left = 0
          Top = 13
          Width = 160
          Height = 564
          Align = alClient
          Indent = 19
          MultiSelect = True
          MultiSelectStyle = [msControlSelect, msShiftSelect]
          PopupMenu = PMMessages
          RightClickSelect = True
          TabOrder = 0
          OnCollapsing = TVAttributesCollapsing
          OnExpanding = TVAttributesExpanding
          OnKeyUp = TVAttributesKeyUp
        end
      end
      object PDebuggerRight: TPanel
        Left = 519
        Top = 0
        Width = 338
        Height = 576
        Align = alClient
        BevelOuter = bvNone
        Constraints.MinHeight = 20
        Constraints.MinWidth = 20
        TabOrder = 1
        OnEnter = PDebuggerRightEnter
        ExplicitWidth = 346
        ExplicitHeight = 578
        object PStack: TPanel
          Left = 0
          Top = 0
          Width = 346
          Height = 13
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Call stack'
          TabOrder = 0
          ExplicitWidth = 345
        end
        object LBStack: TListBox
          Tag = 5
          Left = 0
          Top = 13
          Width = 346
          Height = 565
          Align = alClient
          ItemHeight = 15
          MultiSelect = True
          PopupMenu = PMMessages
          TabOrder = 1
          OnDblClick = LBStackDblClick
          OnKeyUp = LBStackKeyUp
        end
      end
      object PDebuggerCenterLeft: TPanel
        Left = 191
        Top = 0
        Width = 160
        Height = 576
        Align = alLeft
        BevelOuter = bvNone
        Constraints.MinHeight = 20
        Constraints.MinWidth = 20
        TabOrder = 2
        OnEnter = PDebuggerCenterLeftEnter
        ExplicitHeight = 578
        object PLocalVariables: TPanel
          Left = 0
          Top = 0
          Width = 160
          Height = 13
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Parameter and local variables'
          TabOrder = 0
        end
        object TVLocalVariables: TTreeView
          Tag = 3
          Left = 0
          Top = 13
          Width = 160
          Height = 564
          Align = alClient
          Indent = 19
          MultiSelect = True
          MultiSelectStyle = [msControlSelect, msShiftSelect]
          PopupMenu = PMMessages
          RightClickSelect = True
          TabOrder = 1
          OnCollapsing = TVLocalVariablesCollapsing
          OnExpanding = TVLocalVariablesExpanding
          OnKeyUp = TVLocalVariablesKeyUp
        end
      end
      object PDebuggerCenterRight: TPanel
        Left = 355
        Top = 0
        Width = 160
        Height = 576
        Align = alLeft
        BevelOuter = bvNone
        Constraints.MinHeight = 20
        Constraints.MinWidth = 20
        TabOrder = 3
        OnEnter = PDebuggerCenterRightEnter
        ExplicitHeight = 578
        object PWatches: TPanel
          Left = 0
          Top = 0
          Width = 160
          Height = 13
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Watches'
          TabOrder = 0
        end
        object TVWatchedExpressions: TTreeView
          Tag = 4
          Left = 0
          Top = 13
          Width = 160
          Height = 564
          Align = alClient
          Indent = 19
          MultiSelect = True
          MultiSelectStyle = [msControlSelect, msShiftSelect]
          PopupMenu = PMMessages
          TabOrder = 1
          OnCollapsing = TVWatchedExpressionsCollapsing
          OnDblClick = TVWatchedExpressionsDblClick
          OnExpanding = TVWatchedExpressionsExpanding
          OnKeyUp = TVWatchedExpressionsKeyUp
        end
      end
      object ToolbarDock: TSpTBXDock
        Left = 0
        Top = 0
        Width = 27
        Height = 576
        Position = dpLeft
        ExplicitHeight = 579
        object DebuggerToolbar: TSpTBXToolbar
          Left = 0
          Top = 0
          Align = alLeft
          Images = vilDebuggerToolbarLight
          TabOrder = 0
          object TBStep: TSpTBXItem
            Hint = 'Step'
            ImageIndex = 0
            ImageName = 'Step1'
            OnClick = SBStepClick
          end
          object TBNext: TSpTBXItem
            Hint = 'Next'
            ImageIndex = 1
            ImageName = 'Next1'
            OnClick = SBNextClick
            Top = 22
          end
          object TBStepUp: TSpTBXItem
            Hint = 'Step up'
            ImageIndex = 2
            ImageName = 'StepUp'
            OnClick = SBStepUpClick
            Top = 44
          end
          object TBRunToCursor: TSpTBXItem
            Hint = 'Run to cursor'
            ImageIndex = 3
            ImageName = 'RunToCursor'
            OnClick = SBRunToCursorClick
            Top = 66
          end
          object TBWatches: TSpTBXItem
            Hint = 'Watches'
            ImageIndex = 4
            ImageName = 'Watches1'
            OnClick = SBWatchesClick
            Top = 88
          end
          object TBShowExecutionPoint: TSpTBXItem
            Hint = 'Show execution line'
            ImageIndex = 5
            ImageName = 'ShowExecutionLine1'
            OnClick = SBShowExecutionPointClick
            Top = 110
          end
          object TBExpression: TSpTBXItem
            Hint = 'Expression'
            ImageIndex = 6
            ImageName = 'Expression1'
            OnClick = SBExpressionClick
            Top = 132
          end
          object TBDetails: TSpTBXItem
            Hint = 'Details on/off'
            ImageIndex = 7
            ImageName = 'DetailsOnOff'
            OnClick = SBDetailsClick
            Top = 154
          end
        end
      end
    end
    object PInterpreter: TPanel
      Left = 1
      Top = 1
      Width = 857
      Height = 576
      Align = alClient
      Color = clWhite
      TabOrder = 3
      Visible = False
      ExplicitWidth = 865
      ExplicitHeight = 578
      object SplitterInteractiveLeft: TSplitter
        Left = 186
        Top = 1
        Height = 577
        ExplicitLeft = 170
        ExplicitHeight = 192
      end
      object SplitterInteractiveRight: TSplitter
        Left = 722
        Top = 1
        Height = 577
        Align = alRight
        OnMoved = SplitterInteractiveRightMoved
        ExplicitLeft = 430
        ExplicitTop = -3
        ExplicitHeight = 192
      end
      object PInteractiveMiddle: TPanel
        Left = 189
        Top = 1
        Width = 533
        Height = 577
        Align = alClient
        Color = clWhite
        Constraints.MinWidth = 50
        TabOrder = 2
        OnEnter = PInteractiveMiddleEnter
        ExplicitWidth = 529
        ExplicitHeight = 576
        object TBInteractiveToolbar: TToolBar
          Left = 1
          Top = 1
          Width = 531
          Height = 22
          Color = clBtnFace
          Images = vilInteractiveLight
          ParentColor = False
          TabOrder = 0
          ExplicitWidth = 527
          object TBExecute: TToolButton
            Left = 0
            Top = 0
            Hint = 'Execute current line/selected'
            ImageIndex = 0
            ImageName = 'Execute2'
            ParentShowHint = False
            ShowHint = True
            OnClick = TBExecuteClick
          end
          object TBShowUML: TToolButton
            Left = 23
            Top = 0
            Hint = 'Show UML window'
            ImageIndex = 1
            ImageName = 'OpenClass'
            ParentShowHint = False
            ShowHint = True
            OnClick = TBShowUMLClick
          end
          object TBJavaReset: TToolButton
            Left = 46
            Top = 0
            Hint = 'left: reset java/right: Java-Editor restart'
            ImageIndex = 2
            ImageName = 'ResetJava'
            ParentShowHint = False
            ShowHint = True
            OnMouseUp = TBJavaResetMouseUp
          end
          object TBDelete: TToolButton
            Left = 69
            Top = 0
            Hint = 'Delete selected/all'
            ImageIndex = 3
            ImageName = 'Delete2'
            ParentShowHint = False
            ShowHint = True
            OnClick = TBDeleteClick
          end
        end
      end
      object PInteractiveLeft: TPanel
        Left = 1
        Top = 1
        Width = 185
        Height = 577
        Align = alLeft
        Constraints.MinWidth = 50
        TabOrder = 0
        ExplicitHeight = 576
        object MInterpreter: TMemo
          Left = 1
          Top = 1
          Width = 183
          Height = 575
          Align = alClient
          PopupMenu = PMMessages
          ScrollBars = ssBoth
          TabOrder = 0
          OnDblClick = MInterpreterDblClick
          OnEnter = MInterpreterEnter
          OnKeyPress = MInterpreterKeyPress
          ExplicitHeight = 574
        end
      end
      object PInteractiveRight: TPanel
        Left = 725
        Top = 1
        Width = 143
        Height = 577
        Align = alRight
        Color = clWhite
        Constraints.MinWidth = 50
        TabOrder = 1
        OnEnter = PInteractiveRightEnter
        ExplicitLeft = 721
        ExplicitHeight = 576
      end
    end
  end
  object TabControlMessages: TTabControl
    Left = 0
    Top = 598
    Width = 859
    Height = 17
    Align = alBottom
    Style = tsFlatButtons
    TabOrder = 1
    Tabs.Strings = (
      '&Interpreter'
      '&Compiler'
      '&Debugger'
      '&Search'
      '&Messages')
    TabIndex = 0
    OnChange = TabControlMessagesChange
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 0
    Width = 859
    Height = 20
    Align = alTop
    Panels = <
      item
        Style = psOwnerDraw
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
    ExplicitWidth = 867
  end
  object PMMessages: TSpTBXPopupMenu
    Images = vilPMMessagesLight
    OnPopup = PMMessagesPopup
    Left = 64
    Top = 72
    object MIGotoError: TSpTBXItem
      Caption = 'Go to error'
      OnClick = MIGotoErrorClick
    end
    object N4: TSpTBXSeparatorItem
    end
    object MIPaste: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 1
      ImageName = 'Paste1'
      ShortCut = 16470
      OnClick = MIPasteClick
    end
    object MICopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 0
      ImageName = 'Copy1'
      ShortCut = 16451
      OnClick = MICopyClick
    end
    object MIDelete: TSpTBXItem
      Caption = 'Delete'
      ImageIndex = 4
      ImageName = 'Delete1'
      ShortCut = 16472
      OnClick = MIDeleteClick
    end
    object N1: TSpTBXSeparatorItem
    end
    object MICopyAll: TSpTBXItem
      Caption = 'Copy all'
      ImageIndex = 0
      ImageName = 'Copy1'
      OnClick = MICopyAllClick
    end
    object MIDeleteAll: TSpTBXItem
      Caption = 'Delete all'
      ImageIndex = 4
      ImageName = 'Delete1'
      OnClick = MIDeleteAllClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object MIExpand: TSpTBXItem
      Caption = 'Expand all'
      ImageIndex = 6
      ImageName = 'Expand'
      OnClick = MIExpandClick
    end
    object MICollapse: TSpTBXItem
      Caption = 'Collapse all'
      ImageIndex = 5
      ImageName = 'Collapse'
      OnClick = MICollapseClick
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object MISameWidth: TSpTBXItem
      Caption = 'Same width'
      ImageIndex = 7
      ImageName = 'SameWidth'
      OnClick = MISameWidthClick
    end
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 2
      ImageName = 'Font1'
      OnClick = MIFontClick
    end
    object MIDock: TSpTBXItem
      Caption = 'Dock'
      ImageIndex = 8
      ImageName = 'Dock1'
      OnClick = MIDockClick
    end
    object MIUndock: TSpTBXItem
      Caption = 'Undock'
      ImageIndex = 8
      ImageName = 'Dock1'
      OnClick = MIUndockClick
    end
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 3
      ImageName = 'Close1'
      OnClick = MICloseClick
    end
  end
  object icInteractive: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Execute2'
        SVGText = 
          '<svg viewBox="0 0 16 20">'#13#10'  <polygon points="3,2 11,10 3,18" fi' +
          'll="#25e852" stroke="#222222" stroke-width="0.5" />'#13#10'</svg>'#13#10#13#10
      end
      item
        IconName = 'OpenClass'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'<path stroke="#ffffff" d="M2 2h11M2 3h11M2' +
          ' 4h11M2 6h11M2 7h11M2 8h11M2 10h11M2 11h11M2 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ResetJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'  <path stroke="#333333" d="M7 0h1' +
          'M8 1h1"/>'#13#10'  <path stroke="#cecece" d="M3 1h1M3 4h1M3 7h1M7 7h1M' +
          '5 8h1M3 10h1M7 10h1"/>'#13#10'  <path stroke="#7e7e7e" d="M4 1h3M4 10h' +
          '3"/>'#13#10'  <path stroke="#000000" d="M7 1h1M3 2h6M2 3h1M7 3h1M2 4h1' +
          'M1 5h1M1 6h1M9 6h1M2 7h1M8 7h1M2 8h1M8 8h1M3 9h5"/>'#13#10'  <path str' +
          'oke="#6f6f6f" d="M2 2h1M2 9h1M8 9h1"/>'#13#10'  <path stroke="#aeaeae"' +
          ' d="M9 2h1"/>'#13#10'  <path stroke="#8e8e8e" d="M1 3h1M1 8h1M9 8h1"/>' +
          #13#10'  <path stroke="#171717" d="M3 3h1M3 8h1M7 8h1"/>'#13#10'  <path str' +
          'oke="#9e9e9e" d="M4 3h1M8 3h1M7 4h1M4 8h1M6 8h1"/>'#13#10'  <path stro' +
          'ke="#bebebe" d="M5 3h2"/>'#13#10'  <path stroke="#252525" d="M1 4h1M1 ' +
          '7h1M9 7h1"/>'#13#10'  <path stroke="#6e9b6e" d="M13 4h1M15 4h1M13 5h1M' +
          '15 5h1M13 6h1M15 6h1M13 7h1M15 7h1M13 8h1M15 8h1M13 9h1M15 9h1M1' +
          '3 10h1M15 10h1M13 11h1M15 11h1M8 12h1M13 12h1"/>'#13#10'  <path stroke' +
          '="#055200" d="M14 4h1M14 5h1M14 6h1M14 7h1M14 8h1M14 9h1M14 10h1' +
          'M14 11h1M9 12h1M14 12h1M9 13h1M14 13h1M10 14h4M11 15h1"/>'#13#10'  <pa' +
          'th stroke="#424242" d="M2 5h1M8 6h1"/>'#13#10'  <path stroke="#505050"' +
          ' d="M2 6h1"/>'#13#10'  <path stroke="#80a77f" d="M10 12h1M15 12h1"/>'#13#10 +
          '  <path stroke="#99b999" d="M8 13h1"/>'#13#10'  <path stroke="#226422"' +
          ' d="M10 13h1M14 14h1"/>'#13#10'  <path stroke="#2b6b2b" d="M13 13h1"/>' +
          #13#10'  <path stroke="#a6c1a6" d="M15 13h1"/>'#13#10'  <path stroke="#1056' +
          '0f" d="M9 14h1M12 15h1"/>'#13#10'  <path stroke="#347234" d="M10 15h1"' +
          '/>'#13#10'  <path stroke="#538853" d="M13 15h1"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete2'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Execute2'
        SVGText = 
          '<svg viewBox="0 0 16 20">'#13#10'  <polygon points="3,2 11,10 3,18" fi' +
          'll="#25e852" stroke="#eeeeee" stroke-width="0.5" />'#13#10'</svg>'#13#10#13#10
      end
      item
        IconName = 'OpenClass'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ResetJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#cdcdcd" d="M7 0h1" ' +
          '/>'#13#10'<path stroke="#323232" d="M3 1h1M3 4h1M3 7h1M7 7h1M5 8h1M3 1' +
          '0h1M7 10h1" />'#13#10'<path stroke="#7e7e7e" d="M4 1h3M4 10h3" />'#13#10'<pa' +
          'th stroke="#ffffff" d="M7 1h1M3 2h6M2 3h1M7 3h1M2 4h1M1 5h1M1 6h' +
          '1M9 6h1M2 7h1M8 7h1M2 8h1M8 8h1M3 9h5" />'#13#10'<path stroke="#333333' +
          '" d="M8 1h1" />'#13#10'<path stroke="#8d8d8d" d="M2 2h1M2 9h1M8 9h1" /' +
          '>'#13#10'<path stroke="#505050" d="M9 2h1" />'#13#10'<path stroke="#6e6e6e" ' +
          'd="M1 3h1M1 8h1M9 8h1" />'#13#10'<path stroke="#eeeeee" d="M3 3h1M3 8h' +
          '1M7 8h1" />'#13#10'<path stroke="#5f5f5f" d="M4 3h1M8 3h1M7 4h1M4 8h1M' +
          '6 8h1" />'#13#10'<path stroke="#414141" d="M5 3h2" />'#13#10'<path stroke="#' +
          'dedede" d="M1 4h1M1 7h1M9 7h1" />'#13#10'<path stroke="#abf5ab" d="M13' +
          ' 4h1M15 4h1M13 5h1M15 5h1M13 6h1M15 6h1M13 7h1M15 7h1M13 8h1M15 ' +
          '8h1M13 9h1M15 9h1M13 10h1M15 10h1M13 11h1M15 11h1M8 12h1M13 12h1' +
          '" />'#13#10'<path stroke="#41d140" d="M14 4h1M14 5h1M14 6h1M14 7h1M14 ' +
          '8h1M14 9h1M14 10h1M14 11h1M9 12h1M14 12h1M9 13h1M14 13h1M10 14h4' +
          'M11 15h1" />'#13#10'<path stroke="#bdbdbd" d="M2 5h1M8 6h1" />'#13#10'<path ' +
          'stroke="#adadad" d="M2 6h1" />'#13#10'<path stroke="#bdf8bd" d="M10 12' +
          'h1M15 12h1" />'#13#10'<path stroke="#d8fcd7" d="M8 13h1" />'#13#10'<path str' +
          'oke="#5cdf5b" d="M10 13h1M14 14h1" />'#13#10'<path stroke="#65e264" d=' +
          '"M13 13h1" />'#13#10'<path stroke="#e5ffe5" d="M15 13h1" />'#13#10'<path str' +
          'oke="#47d446" d="M9 14h1M12 15h1" />'#13#10'<path stroke="#6fe66e" d="' +
          'M10 15h1" />'#13#10'<path stroke="#90ef8f" d="M13 15h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete2'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end>
    Left = 550
    Top = 70
  end
  object vilInteractiveLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Execute2'
        Name = 'Execute2'
      end
      item
        CollectionIndex = 1
        CollectionName = 'OpenClass'
        Name = 'OpenClass'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ResetJava'
        Name = 'ResetJava'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Delete2'
        Name = 'Delete2'
      end>
    ImageCollection = icInteractive
    Left = 254
    Top = 78
  end
  object vilInteractiveDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 4
        CollectionName = 'Execute2'
        Name = 'Execute2'
      end
      item
        CollectionIndex = 5
        CollectionName = 'OpenClass'
        Name = 'OpenClass'
      end
      item
        CollectionIndex = 6
        CollectionName = 'ResetJava'
        Name = 'ResetJava'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Delete2'
        Name = 'Delete2'
      end>
    ImageCollection = icInteractive
    Left = 406
    Top = 70
  end
  object vilDebuggerToolbarLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Step1'
        Name = 'Step1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Next1'
        Name = 'Next1'
      end
      item
        CollectionIndex = 2
        CollectionName = 'StepUp'
        Name = 'StepUp'
      end
      item
        CollectionIndex = 3
        CollectionName = 'RunToCursor'
        Name = 'RunToCursor'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Watches1'
        Name = 'Watches1'
      end
      item
        CollectionIndex = 5
        CollectionName = 'ShowExecutionLine1'
        Name = 'ShowExecutionLine1'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Expression1'
        Name = 'Expression1'
      end
      item
        CollectionIndex = 7
        CollectionName = 'DetailsOnOff'
        Name = 'DetailsOnOff'
      end>
    ImageCollection = icDebuggerToolbar
    Left = 254
    Top = 134
  end
  object vilDebuggerToolbarDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 8
        CollectionName = 'Step1'
        Name = 'Step1'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Next1'
        Name = 'Next1'
      end
      item
        CollectionIndex = 10
        CollectionName = 'StepUp'
        Name = 'StepUp'
      end
      item
        CollectionIndex = 11
        CollectionName = 'RunToCursor'
        Name = 'RunToCursor'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Watches1'
        Name = 'Watches1'
      end
      item
        CollectionIndex = 13
        CollectionName = 'ShowExecutionLine1'
        Name = 'ShowExecutionLine1'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Expression1'
        Name = 'Expression1'
      end
      item
        CollectionIndex = 15
        CollectionName = 'DetailsOnOff'
        Name = 'DetailsOnOff'
      end>
    ImageCollection = icDebuggerToolbar
    Left = 406
    Top = 134
  end
  object icDebuggerToolbar: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Step1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M0 0h1M3' +
          ' 0h1M6 1h1M7 3h1M6 5h5M7 6h3M8 7h1" />'#13#10'<path stroke="#000082" d' +
          '="M5 8h7M5 9h1M11 9h1M5 10h1M11 10h1M5 11h1M11 11h1M5 12h1M11 12' +
          'h1M5 13h1M11 13h1M5 14h7" />'#13#10'<path stroke="#ffffff" d="M6 9h5M6' +
          ' 10h5M6 11h5M6 12h5M6 13h5" />'#13#10'<path stroke="#828282" d="M12 9h' +
          '1M12 10h1M12 11h1M12 12h1M12 13h1M12 14h1M6 15h7" />'#13#10'</svg>'
      end
      item
        IconName = 'Next1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M5 0h1M8' +
          ' 0h1M2 1h1M11 1h1M0 3h1M13 3h1M11 5h5M12 6h3M13 7h1" />'#13#10'<path s' +
          'troke="#000082" d="M4 8h7M4 9h1M10 9h1M4 10h1M10 10h1M4 11h1M10 ' +
          '11h1M4 12h1M10 12h1M4 13h1M10 13h1M4 14h7" />'#13#10'<path stroke="#ff' +
          'ffff" d="M5 9h5M5 10h5M5 11h5M5 12h5M5 13h5" />'#13#10'<path stroke="#' +
          '828282" d="M11 9h1M11 10h1M11 11h1M11 12h1M11 13h1M11 14h1M5 15h' +
          '7" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'StepUp'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M11 0h1M' +
          '11 1h2M7 2h1M9 2h1M11 2h3M5 3h1M11 3h2M11 4h1M4 5h1M4 7h1" />'#13#10'<' +
          'path stroke="#000082" d="M2 8h7M2 9h1M8 9h1M2 10h1M8 10h1M2 11h1' +
          'M8 11h1M2 12h1M8 12h1M2 13h1M8 13h1M2 14h7" />'#13#10'<path stroke="#f' +
          'fffff" d="M3 9h5M3 10h5M3 11h5M3 12h5M3 13h5" />'#13#10'<path stroke="' +
          '#828282" d="M9 9h1M9 10h1M9 11h1M9 12h1M9 13h1M9 14h1M3 15h7" />' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'RunToCursor'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000082" d="M0 0h1M1' +
          ' 1h1M0 2h1M5 5h2M8 5h2M7 6h1M7 7h1M7 8h1M7 9h1M7 10h1M7 11h1M5 1' +
          '2h2M8 12h2" />'#13#10#13#10'<path stroke="#828282" d="M0 1h1M1 2h1M0 3h1M1' +
          '1 4h1M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M11 12h1' +
          'M11 13h1M11 14h1M4 15h8" />'#13#10'<path stroke="#000000" d="M2 2h1M1 ' +
          '3h1M3 3h9M0 4h1M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h' +
          '1M3 12h1M3 13h1M3 14h1M3 15h1" />'#13#10'<path stroke="#fe0000" d="M14' +
          ' 2h1M14 4h1M14 6h1M14 8h1M14 9h1M13 10h3M14 11h1M13 13h3" />'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'Watches1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7e7e" d="M1 1h1M1' +
          '4 1h1M1 14h1M14 14h1" />'#13#10'<path stroke="#000000" d="M2 1h12M1 2h' +
          '1M14 2h1M1 3h1M14 3h1M1 4h1M14 4h1M1 5h1M14 5h1M1 6h1M14 6h1M1 7' +
          'h1M14 7h1M1 8h1M4 8h2M7 8h2M10 8h2M14 8h1M1 9h1M4 9h2M7 9h2M10 9' +
          'h2M14 9h1M1 10h1M14 10h1M1 11h1M4 11h2M7 11h2M10 11h2M14 11h1M1 ' +
          '12h1M4 12h2M7 12h2M10 12h2M14 12h1M1 13h1M14 13h1M2 14h12" />'#13#10'<' +
          'path stroke="#00007e" d="M3 3h10M3 4h1M3 5h1M3 6h1" />'#13#10'<path st' +
          'roke="#0000ff" d="M4 4h9M4 5h9M4 6h9" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ShowExecutionLine1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828282" d="M4 1h8M4' +
          ' 2h1M4 3h1M4 4h1M4 5h1M6 5h4M4 6h1M4 7h1M6 7h8M4 8h1M4 9h1M4 10h' +
          '1M4 11h1M6 11h8M4 12h1M4 13h1M6 13h8M4 14h1M4 15h1" />'#13#10'<path st' +
          'roke="#000000" d="M12 1h1M12 2h2M12 3h1M14 3h1M12 4h4M15 5h1M15 ' +
          '6h1M15 7h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h1M15 14h1' +
          'M5 15h11" />'#13#10'<path stroke="#ffffff" d="M5 2h7M5 3h7M13 3h1M5 4h' +
          '7M5 5h1M10 5h5M5 6h10M5 7h1M14 7h1M6 9h8M5 11h1M14 11h1M5 12h10M' +
          '5 13h1M14 13h1M5 14h10" />'#13#10'<path stroke="#000082" d="M0 7h1M0 8' +
          'h2M0 9h3M0 10h3M0 11h2M0 12h1" />'#13#10'<path stroke="#0000ff" d="M5 ' +
          '8h10M5 9h1M14 9h1M5 10h10" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Expression1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7e7e" d="M1 5h1M3' +
          ' 5h1M11 5h1M4 6h1M9 6h1M13 6h1M1 9h1M8 10h2M13 10h1" />'#13#10'<path s' +
          'troke="#000000" d="M2 5h1M12 5h1M1 6h1M3 6h1M5 6h4M10 6h3M1 7h1M' +
          '4 7h1M8 7h2M13 7h1M1 8h1M4 8h1M8 8h2M13 8h1M2 9h1M4 9h1M8 9h2M13' +
          ' 9h1M5 10h3M10 10h3" />'#13#10'<path stroke="#bebebe" d="M5 7h1M7 7h1M' +
          '10 7h1M12 7h1M7 8h1M12 8h1M5 9h3M10 9h3" />'#13#10'<path stroke="#ffff' +
          'ff" d="M6 7h1M11 7h1M5 8h2M10 8h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'DetailsOnOff'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828282" d="M2 1h1M8' +
          ' 1h1M1 2h1M4 2h4M7 3h2M10 3h1M8 4h2M9 5h1M9 6h1M9 7h1M1 8h2M8 8h' +
          '1M3 9h1M7 9h1M3 10h1" />'#13#10'<path stroke="#000000" d="M3 1h5M2 2h2' +
          'M8 2h2M1 3h2M9 3h1M1 4h1M10 4h1M1 5h1M10 5h1M1 6h1M10 6h1M1 7h1M' +
          '10 7h1M9 8h2M2 9h1M8 9h2M11 9h1M4 10h5M12 10h1M9 11h1M13 11h1M10' +
          ' 12h1M14 12h1M11 13h1M14 13h1M12 14h2" />'#13#10'<path stroke="#c4c4c4' +
          '" d="M3 3h4M2 4h4M2 5h4M8 5h1M2 6h1M4 6h5M2 7h2M5 7h4M3 8h2M6 8h' +
          '2M4 9h3" />'#13#10'<path stroke="#ffffff" d="M6 4h2M6 5h2M3 6h1M4 7h1M' +
          '5 8h1M10 9h1M11 10h1M12 11h1M13 12h1" />'#13#10'<path stroke="#000082"' +
          ' d="M9 10h1M10 11h1M11 12h1M12 13h1" />'#13#10'<path stroke="#ffff00" ' +
          'd="M10 10h1M11 11h1M12 12h1M13 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Step1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M0 0h1M3' +
          ' 0h1M6 1h1M7 3h1M6 5h5M7 6h3M8 7h1M6 9h5M6 10h5M6 11h5M6 12h5M6 ' +
          '13h5" />'#13#10'<path stroke="#000082" d="M5 8h7M5 9h1M11 9h1M5 10h1M1' +
          '1 10h1M5 11h1M11 11h1M5 12h1M11 12h1M5 13h1M11 13h1M5 14h7" />'#13#10 +
          '<path stroke="#828282" d="M12 9h1M12 10h1M12 11h1M12 12h1M12 13h' +
          '1M12 14h1M6 15h7" />'#13#10'</svg>'
      end
      item
        IconName = 'Next1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M5 0h1M8' +
          ' 0h1M2 1h1M11 1h1M0 3h1M13 3h1M11 5h5M12 6h3M13 7h1M5 9h5M5 10h5' +
          'M5 11h5M5 12h5M5 13h5" />'#13#10'<path stroke="#000082" d="M4 8h7M4 9h' +
          '1M10 9h1M4 10h1M10 10h1M4 11h1M10 11h1M4 12h1M10 12h1M4 13h1M10 ' +
          '13h1M4 14h7" />'#13#10'<path stroke="#828282" d="M11 9h1M11 10h1M11 11' +
          'h1M11 12h1M11 13h1M11 14h1M5 15h7" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'StepUp'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M11 0h1M' +
          '11 1h2M7 2h1M9 2h1M11 2h3M5 3h1M11 3h2M11 4h1M4 5h1M4 7h1M3 9h5M' +
          '3 10h5M3 11h5M3 12h5M3 13h5" />'#13#10'<path stroke="#000082" d="M2 8h' +
          '7M2 9h1M8 9h1M2 10h1M8 10h1M2 11h1M8 11h1M2 12h1M8 12h1M2 13h1M8' +
          ' 13h1M2 14h7" />'#13#10'<path stroke="#828282" d="M9 9h1M9 10h1M9 11h1' +
          'M9 12h1M9 13h1M9 14h1M3 15h7" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'RunToCursor'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M0 0h1M1' +
          ' 1h1M0 2h1M5 5h2M8 5h2M7 6h1M7 7h1M7 8h1M7 9h1M7 10h1M7 11h1M5 1' +
          '2h2M8 12h2" />'#13#10#13#10'<path stroke="#828282" d="M0 1h1M1 2h1M0 3h1M1' +
          '1 4h1M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M11 12h1' +
          'M11 13h1M11 14h1M4 15h8" />'#13#10'<path stroke="#ffffff" d="M2 2h1M1 ' +
          '3h1M3 3h9M0 4h1M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h' +
          '1M3 12h1M3 13h1M3 14h1M3 15h1" />'#13#10'<path stroke="#fe0000" d="M14' +
          ' 2h1M14 4h1M14 6h1M14 8h1M14 9h1M13 10h3M14 11h1M13 13h3" />'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'Watches1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7e7e" d="M1 1h1M1' +
          '4 1h1M1 14h1M14 14h1" />'#13#10'<path stroke="#ffffff" d="M2 1h12M1 2h' +
          '1M14 2h1M1 3h1M14 3h1M1 4h1M14 4h1M1 5h1M14 5h1M1 6h1M14 6h1M1 7' +
          'h1M14 7h1M1 8h1M4 8h2M7 8h2M10 8h2M14 8h1M1 9h1M4 9h2M7 9h2M10 9' +
          'h2M14 9h1M1 10h1M14 10h1M1 11h1M4 11h2M7 11h2M10 11h2M14 11h1M1 ' +
          '12h1M4 12h2M7 12h2M10 12h2M14 12h1M1 13h1M14 13h1M2 14h12" />'#13#10'<' +
          'path stroke="#32deff" d="M3 3h10M3 4h1M3 5h1M3 6h1" />'#13#10'<path st' +
          'roke="#95fff8" d="M4 4h9M4 5h9M4 6h9" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ShowExecutionLine1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828282" d="M4 1h8M4' +
          ' 2h1M4 3h1M4 4h1M4 5h1M6 5h4M4 6h1M4 7h1M6 7h8M4 8h1M4 9h1M4 10h' +
          '1M4 11h1M6 11h8M4 12h1M4 13h1M6 13h8M4 14h1M4 15h1" />'#13#10'<path st' +
          'roke="#000000" d="M12 1h1M12 2h2M12 3h1M14 3h1M12 4h4M15 5h1M15 ' +
          '6h1M15 7h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h1M15 14h1' +
          'M5 15h11" />'#13#10'<path stroke="#ffffff" d="M5 2h7M5 3h7M13 3h1M5 4h' +
          '7M5 5h1M10 5h5M5 6h10M5 7h1M14 7h1M6 9h8M5 11h1M14 11h1M5 12h10M' +
          '5 13h1M14 13h1M5 14h10" />'#13#10'<path stroke="#95fff8" d="M0 7h1M0 8' +
          'h2M5 8h10M0 9h3M5 9h1M14 9h1M0 10h3M5 10h10M0 11h2M0 12h1" />'#13#10'<' +
          '/svg>'#13#10
      end
      item
        IconName = 'Expression1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#b1b3b5" d="M1 5h1M3' +
          ' 5h1M11 5h1M4 6h1M9 6h1M13 6h1M1 9h1M8 10h2M13 10h1" />'#13#10'<path s' +
          'troke="#ffffff" d="M2 5h1M12 5h1M1 6h1M3 6h1M5 6h4M10 6h3M1 7h1M' +
          '4 7h1M8 7h2M13 7h1M1 8h1M4 8h1M8 8h2M13 8h1M2 9h1M4 9h1M8 9h2M13' +
          ' 9h1M5 10h3M10 10h3" />'#13#10'<path stroke="#32deff" d="M5 7h1M7 7h1M' +
          '10 7h1M12 7h1M7 8h1M12 8h1M5 9h3M10 9h3" />'#13#10'<path stroke="#95ff' +
          'f8" d="M6 7h1M11 7h1M5 8h2M10 8h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'DetailsOnOff'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7a7a7a" d="M2 1h1M8' +
          ' 1h1M1 2h1M4 2h4M7 3h2M10 3h1M8 4h2M9 5h1M9 6h1M9 7h1M1 8h2M8 8h' +
          '1M3 9h1M7 9h1M3 10h1" />'#13#10'<path stroke="#ffffff" d="M3 1h5M2 2h2' +
          'M8 2h2M1 3h2M9 3h1M1 4h1M10 4h1M1 5h1M10 5h1M1 6h1M10 6h1M1 7h1M' +
          '10 7h1M9 8h2M2 9h1M8 9h2M11 9h1M4 10h5M12 10h1M9 11h1M13 11h1M10' +
          ' 12h1M14 12h1M11 13h1M14 13h1M12 14h2" />'#13#10'<path stroke="#000000' +
          '" d="M10 9h1M11 10h1M12 11h1M13 12h1" />'#13#10'<path stroke="#ffff7a"' +
          ' d="M9 10h1M10 11h1M11 12h1M12 13h1" />'#13#10'<path stroke="#0000ff" ' +
          'd="M10 10h1M11 11h1M12 12h1M13 13h1" />'#13#10'</svg>'#13#10
      end>
    Left = 550
    Top = 134
  end
  object icPMMessages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Copy1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#a7a7a7" d="M1 0h4M0' +
          ' 1h1M5 1h1M0 2h1M5 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1' +
          'M0 10h1M1 11h4" />'#13#10'<path stroke="#b1b1b1" d="M5 0h1" />'#13#10'<path ' +
          'stroke="#ececec" d="M6 0h1" />'#13#10'<path stroke="#aeaeae" d="M6 1h1' +
          'M7 2h1" />'#13#10'<path stroke="#e9e9e9" d="M7 1h1" />'#13#10'<path stroke="' +
          '#e6e6e6" d="M6 2h1M5 3h1" />'#13#10'<path stroke="#e4e4e4" d="M8 2h1" ' +
          '/>'#13#10'<path stroke="#6ea5d7" d="M2 3h2M2 5h2M2 7h3M2 9h3" />'#13#10'<pat' +
          'h stroke="#909090" d="M6 4h1" />'#13#10'<path stroke="#777777" d="M7 4' +
          'h4M6 5h1M11 5h1M6 6h1M11 6h1M6 7h1M11 7h1M6 8h1M12 8h3M6 9h1M15 ' +
          '9h1M6 10h1M15 10h1M6 11h1M15 11h1M6 12h1M15 12h1M6 13h1M15 13h1M' +
          '6 14h1M15 14h1M7 15h8" />'#13#10'<path stroke="#858585" d="M11 4h1" />' +
          #13#10'<path stroke="#e2e2e2" d="M12 4h1" />'#13#10'<path stroke="#fcfcfc" ' +
          'd="M7 5h4M7 6h4M7 7h1M10 7h1M12 7h1M7 8h4M7 9h1M10 9h5M7 10h8M7 ' +
          '11h1M14 11h1M7 12h8M7 13h1M14 13h1M7 14h8" />'#13#10'<path stroke="#81' +
          '8181" d="M12 5h1M13 6h1" />'#13#10'<path stroke="#dcdcdc" d="M13 5h1" ' +
          '/>'#13#10'<path stroke="#d8d8d8" d="M12 6h1" />'#13#10'<path stroke="#d7d7d7' +
          '" d="M14 6h1" />'#13#10'<path stroke="#3f77b0" d="M8 7h2M8 9h2M8 11h6M' +
          '8 13h6" />'#13#10'<path stroke="#dedede" d="M13 7h1" />'#13#10'<path stroke=' +
          '"#828282" d="M14 7h1" />'#13#10'<path stroke="#d1d1d1" d="M15 7h1" />'#13 +
          #10'<path stroke="#8c8c8c" d="M11 8h1" />'#13#10'<path stroke="#7e7e7e" d' +
          '="M15 8h1" />'#13#10'<path stroke="#8f8f8f" d="M6 15h1M15 15h1" />'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'Paste1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#898989" d="M4 0h1M7' +
          ' 0h1" />'#13#10'<path stroke="#777777" d="M5 0h2M4 1h4M8 6h3M7 7h1M11 ' +
          '7h1M7 8h1M11 8h1M7 9h1M11 9h1M7 10h1M12 10h3M7 11h1M15 11h1M7 12' +
          'h1M15 12h1M7 13h1M15 13h1M7 14h1M15 14h1M8 15h7" />'#13#10'<path strok' +
          'e="#e8bd78" d="M0 1h1M0 13h1" />'#13#10'<path stroke="#e5b262" d="M1 1' +
          'h1M10 1h1M0 2h2M10 2h2M0 3h12M0 4h12M0 5h6M0 6h6M0 7h6M0 8h6M0 9' +
          'h6M0 10h6M0 11h6M0 12h6M1 13h5" />'#13#10'<path stroke="#7f7f7f" d="M3' +
          ' 1h1" />'#13#10'<path stroke="#838383" d="M8 1h1" />'#13#10'<path stroke="#e' +
          '8be78" d="M11 1h1" />'#13#10'<path stroke="#f8f2e6" d="M2 2h1" />'#13#10'<pa' +
          'th stroke="#ffffff" d="M3 2h6" />'#13#10'<path stroke="#f7f0e7" d="M9 ' +
          '2h1" />'#13#10'<path stroke="#efd5ab" d="M6 5h1" />'#13#10'<path stroke="#fc' +
          'fbf9" d="M7 5h1M6 6h1" />'#13#10'<path stroke="#91908f" d="M7 6h1" />'#13 +
          #10'<path stroke="#858585" d="M11 6h1" />'#13#10'<path stroke="#e2e2e2" d' +
          '="M12 6h1" />'#13#10'<path stroke="#fcfcfc" d="M8 7h3M8 8h3M8 9h3M12 9' +
          'h1M8 10h3M8 11h7M8 12h7M8 13h7M8 14h7" />'#13#10'<path stroke="#818181' +
          '" d="M12 7h1M13 8h1" />'#13#10'<path stroke="#dcdcdc" d="M13 7h1" />'#13#10 +
          '<path stroke="#d8d8d8" d="M12 8h1" />'#13#10'<path stroke="#d7d7d7" d=' +
          '"M14 8h1" />'#13#10'<path stroke="#dedede" d="M13 9h1" />'#13#10'<path strok' +
          'e="#828282" d="M14 9h1" />'#13#10'<path stroke="#d1d1d1" d="M15 9h1" /' +
          '>'#13#10'<path stroke="#8c8c8c" d="M11 10h1" />'#13#10'<path stroke="#7e7e7e' +
          '" d="M15 10h1" />'#13#10'<path stroke="#8f8f8f" d="M7 15h1M15 15h1" />' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'Font1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M186-8' +
          '0q-54 0-80-22t-26-66q0-58 49-74t116-16h21v-56q0-34-1-55.5t-6-35.' +
          '5q-5-14-11.5-19.5T230-430q-9 0-16.5 3t-12.5 8q-4 5-5 10.5t1 11.5' +
          'q6 11 14 21.5t8 24.5q0 25-17.5 42.5T159-291q-25 0-42.5-17.5T99-3' +
          '51q0-27 12-44t32.5-27q20.5-10 47.5-14t58-4q85 0 118 30.5T400-302' +
          'v147q0 19 4.5 28t15.5 9q12 0 19.5-18t9.5-56h11q-3 62-23.5 87T368' +
          '-80q-43 0-67.5-13.5T269-134q-10 29-29.5 41.5T186-80Zm373 0q-20 0' +
          '-32.5-16.5T522-132l102-269q7-17 22-28t34-11q19 0 34 11t22 28l102' +
          ' 269q8 19-4.5 35.5T801-80q-12 0-22-7t-15-19l-20-58H616l-20 58q-4' +
          ' 11-14 18.5T559-80Zm-324-29q13 0 22-20.5t9-49.5v-67q-26 0-38 15.' +
          '5T216-180v11q0 36 4 48t15 12Zm407-125h77l-39-114-38 114Zm-37-285' +
          'q-48 0-76.5-33.5T500-643q0-104 66-170.5T735-880q42 0 68 9.5t26 2' +
          '4.5q0 6-2 12t-7 11q-5 7-12.5 10t-15.5 1q-14-4-32-7t-33-3q-71 0-1' +
          '14 48t-43 127q0 22 8 46t36 24q11 0 21.5-5t18.5-14q17-18 31.5-60T' +
          '712-758q2-13 10.5-18.5T746-782q18 0 27.5 9.5T779-749q-12 43-17.5' +
          ' 75t-5.5 58q0 20 5.5 29t16.5 9q11 0 21.5-8t29.5-30q2-3 15-7 8 0 ' +
          '12 6t4 17q0 28-32 54t-67 26q-26 0-44.5-14T691-574q-15 26-37 40.5' +
          'T605-519Zm-485-1v-220q0-58 41-99t99-41q58 0 99 41t41 99v220h-80v' +
          '-80H200v80h-80Zm80-160h120v-60q0-25-17.5-42.5T260-800q-25 0-42.5' +
          ' 17.5T200-740v60Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Close1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Font1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="M186-8' +
          '0q-54 0-80-22t-26-66q0-58 49-74t116-16h21v-56q0-34-1-55.5t-6-35.' +
          '5q-5-14-11.5-19.5T230-430q-9 0-16.5 3t-12.5 8q-4 5-5 10.5t1 11.5' +
          'q6 11 14 21.5t8 24.5q0 25-17.5 42.5T159-291q-25 0-42.5-17.5T99-3' +
          '51q0-27 12-44t32.5-27q20.5-10 47.5-14t58-4q85 0 118 30.5T400-302' +
          'v147q0 19 4.5 28t15.5 9q12 0 19.5-18t9.5-56h11q-3 62-23.5 87T368' +
          '-80q-43 0-67.5-13.5T269-134q-10 29-29.5 41.5T186-80Zm373 0q-20 0' +
          '-32.5-16.5T522-132l102-269q7-17 22-28t34-11q19 0 34 11t22 28l102' +
          ' 269q8 19-4.5 35.5T801-80q-12 0-22-7t-15-19l-20-58H616l-20 58q-4' +
          ' 11-14 18.5T559-80Zm-324-29q13 0 22-20.5t9-49.5v-67q-26 0-38 15.' +
          '5T216-180v11q0 36 4 48t15 12Zm407-125h77l-39-114-38 114Zm-37-285' +
          'q-48 0-76.5-33.5T500-643q0-104 66-170.5T735-880q42 0 68 9.5t26 2' +
          '4.5q0 6-2 12t-7 11q-5 7-12.5 10t-15.5 1q-14-4-32-7t-33-3q-71 0-1' +
          '14 48t-43 127q0 22 8 46t36 24q11 0 21.5-5t18.5-14q17-18 31.5-60T' +
          '712-758q2-13 10.5-18.5T746-782q18 0 27.5 9.5T779-749q-12 43-17.5' +
          ' 75t-5.5 58q0 20 5.5 29t16.5 9q11 0 21.5-8t29.5-30q2-3 15-7 8 0 ' +
          '12 6t4 17q0 28-32 54t-67 26q-26 0-44.5-14T691-574q-15 26-37 40.5' +
          'T605-519Zm-485-1v-220q0-58 41-99t99-41q58 0 99 41t41 99v220h-80v' +
          '-80H200v80h-80Zm80-160h120v-60q0-25-17.5-42.5T260-800q-25 0-42.5' +
          ' 17.5T200-740v60Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Close1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e6e6e6" d="M4 4h2M1' +
          '0 4h2M5 5h2M9 5h2M6 6h4M7 7h2M6 8h4M5 9h2M9 9h2M4 10h2M10 10h2" ' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Delete1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Collapse'
        SVGText = 
          '<svg  viewBox="0 -960 960 960" fill="#5f6368"><path d="m296-80-5' +
          '6-56 240-240 240 240-56 56-184-184L296-80Zm184-504L240-824l56-56' +
          ' 184 184 184-184 56 56-240 240Z"/></svg>'
      end
      item
        IconName = 'Collapse'
        SVGText = 
          '<svg  viewBox="0 -960 960 960" fill="#e6e6e6"><path d="m296-80-5' +
          '6-56 240-240 240 240-56 56-184-184L296-80Zm184-504L240-824l56-56' +
          ' 184 184 184-184 56 56-240 240Z"/></svg>'
      end
      item
        IconName = 'Expand'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#5f6368"><path d="M480-80 24' +
          '0-320l57-57 183 183 183-183 57 57L480-80ZM298-584l-58-56 240-240' +
          ' 240 240-58 56-182-182-182 182Z"/></svg>'
      end
      item
        IconName = 'Expand'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6"><path d="M480-80 24' +
          '0-320l57-57 183 183 183-183 57 57L480-80ZM298-584l-58-56 240-240' +
          ' 240 240-58 56-182-182-182 182Z"/></svg>'
      end
      item
        IconName = 'SameWidth'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#344f6b" d="M1 5h4M6' +
          ' 5h4M11 5h4M1 6h1M4 6h1M6 6h1M9 6h1M11 6h1M14 6h1M1 7h1M4 7h1M6 ' +
          '7h1M9 7h1M11 7h1M14 7h1M1 8h1M4 8h1M6 8h1M9 8h1M11 8h1M14 8h1M1 ' +
          '9h4M6 9h4M11 9h4" />'#13#10'<path stroke="#c2cddb" d="M2 6h2M7 6h2M12 ' +
          '6h2" />'#13#10'<path stroke="#a3b4c8" d="M2 7h2M7 7h2M12 7h2M2 8h2M7 8' +
          'h2M12 8h2" />'#13#10'</svg>'
      end
      item
        IconName = 'SameWidth'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e0e0e0" d="M1 5h4M6' +
          ' 5h4M11 5h4M1 6h1M4 6h1M6 6h1M9 6h1M11 6h1M14 6h1M1 7h1M4 7h1M6 ' +
          '7h1M9 7h1M11 7h1M14 7h1M1 8h1M4 8h1M6 8h1M9 8h1M11 8h1M14 8h1M1 ' +
          '9h4M6 9h4M11 9h4" />'#13#10'<path stroke="#c2cddb" d="M2 6h2M7 6h2M12 ' +
          '6h2" />'#13#10'<path stroke="#a3b4c8" d="M2 7h2M7 7h2M12 7h2M2 8h2M7 8' +
          'h2M12 8h2" />'#13#10'</svg>'
      end
      item
        IconName = 'Dock1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'  <rect width="9" height="5" x="4"' +
          ' y="1" rx="1" ry="1" fill="#1e1e1e" />'#13#10'  <polyline points="2,1 ' +
          '2,8 13,8" style="fill:none" stroke="#1e1e1e"/>'#13#10'</svg> '
      end
      item
        IconName = 'Dock1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'  <rect width="9" height="5" x="4"' +
          ' y="1" rx="1" ry="1" fill="#e1e1e1" />'#13#10'  <polyline points="2,1 ' +
          '2,8 13,8" style="fill:none" stroke="#e1e1e1"/>'#13#10'</svg> '
      end>
    Left = 552
    Top = 208
  end
  object vilPMMessagesDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Copy1'
        Name = 'Copy1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Paste1'
        Name = 'Paste1'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Font1'
        Name = 'Font1'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Close1'
        Name = 'Close1'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Delete1'
        Name = 'Delete1'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Collapse'
        Name = 'Collapse'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Expand'
        Name = 'Expand'
      end
      item
        CollectionIndex = 13
        CollectionName = 'SameWidth'
        Name = 'SameWidth'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Dock1'
        Name = 'Dock1'
      end>
    ImageCollection = icPMMessages
    Left = 408
    Top = 208
  end
  object vilPMMessagesLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Copy1'
        Name = 'Copy1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Paste1'
        Name = 'Paste1'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Font1'
        Name = 'Font1'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Close1'
        Name = 'Close1'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Delete1'
        Name = 'Delete1'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Collapse'
        Name = 'Collapse'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Expand'
        Name = 'Expand'
      end
      item
        CollectionIndex = 12
        CollectionName = 'SameWidth'
        Name = 'SameWidth'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Dock1'
        Name = 'Dock1'
      end>
    ImageCollection = icPMMessages
    Left = 256
    Top = 200
  end
end
