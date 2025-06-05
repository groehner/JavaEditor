object FStructogram: TFStructogram
  Left = 591
  Top = 96
  Align = alClient
  BorderIcons = []
  Caption = 'Diagram'
  ClientHeight = 672
  ClientWidth = 673
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  Visible = True
  WindowState = wsMaximized
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object ScrollBox: TScrollBox
    Left = 24
    Top = 0
    Width = 649
    Height = 672
    Align = alClient
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = clWhite
    Ctl3D = False
    ParentColor = False
    ParentCtl3D = False
    PopupMenu = StructoPopupMenu
    TabOrder = 0
    OnClick = ScrollBoxClick
    OnMouseMove = ScrollBoxMouseMove
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 24
    Height = 672
    Align = alLeft
    ParentBackground = False
    TabOrder = 1
    object TrashImage: TVirtualImage
      Left = 1
      Top = 309
      Width = 22
      Height = 362
      Hint = 'Trash bin'
      Align = alClient
      ImageCollection = icStructogram
      ImageWidth = 0
      ImageHeight = 0
      ImageIndex = 14
      ImageName = 'Delete'
      ExplicitTop = 336
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object ToolBarStructogram: TToolBar
      Left = 1
      Top = 1
      Width = 22
      Height = 308
      AutoSize = True
      Ctl3D = False
      Images = vilToolbarLight
      TabOrder = 0
      object TBClose: TToolButton
        Left = 0
        Top = 0
        Hint = 'Close'
        ImageIndex = 0
        ImageName = 'Close'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnClick = BBCloseClick
      end
      object TBStatement: TToolButton
        Tag = 1
        Left = 0
        Top = 22
        Hint = 'statement'
        ImageIndex = 1
        ImageName = 'Statement'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBIf: TToolButton
        Tag = 2
        Left = 0
        Top = 44
        Hint = 'if else'
        ImageIndex = 2
        ImageName = 'If'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBSwitch: TToolButton
        Tag = 6
        Left = 0
        Top = 66
        Hint = 'switch'
        ImageIndex = 3
        ImageName = 'Switch'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBwhile: TToolButton
        Tag = 3
        Left = 0
        Top = 88
        Hint = 'while'
        ImageIndex = 4
        ImageName = 'While'
        Wrap = True
        OnMouseDown = StrElementMouseDown
      end
      object TBDoWhile: TToolButton
        Tag = 4
        Left = 0
        Top = 110
        Hint = 'do while'
        ImageIndex = 5
        ImageName = 'DoWhile'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBFor: TToolButton
        Tag = 5
        Left = 0
        Top = 132
        Hint = 'for'
        ImageIndex = 6
        ImageName = 'For'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBSubProgram: TToolButton
        Tag = 7
        Left = 0
        Top = 154
        Hint = 'subprogram'
        ImageIndex = 7
        ImageName = 'SubProgram'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBBreak: TToolButton
        Tag = 10
        Left = 0
        Top = 176
        Hint = 'break'
        ImageIndex = 8
        ImageName = 'Break'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBAlgorithm: TToolButton
        Left = 0
        Top = 198
        Hint = 'Algorithm'
        ImageIndex = 9
        ImageName = 'Algorithm'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnMouseDown = StrElementMouseDown
      end
      object TBGenerateJava: TToolButton
        Left = 0
        Top = 220
        Hint = 'Create java code'
        ImageIndex = 10
        ImageName = 'GenerateJava'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnClick = BBGenerateJavaClick
      end
      object TBZoomOut: TToolButton
        Left = 0
        Top = 242
        Hint = 'Zoom out'
        ImageIndex = 11
        ImageName = 'ZoomOut'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnClick = BBZoomOutClick
      end
      object TBZoomIn: TToolButton
        Left = 0
        Top = 264
        Hint = 'Zoom in'
        ImageIndex = 12
        ImageName = 'ZoomIn'
        ParentShowHint = False
        Wrap = True
        ShowHint = True
        OnClick = BBZoomInClick
      end
      object TBPuzzleMode: TToolButton
        Left = 0
        Top = 286
        Hint = 'Puzzle mode'
        ImageIndex = 13
        ImageName = 'PuzzleMode'
        ParentShowHint = False
        ShowHint = True
        OnClick = BBPuzzleClick
      end
    end
  end
  object icStructogram: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
        GrayScale = True
      end
      item
        IconName = 'Statement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 " />'#13#10'</svg>'
      end
      item
        IconName = 'If'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Switch'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M5.8 5.5 v8.5' +
          ' M10.1 5.5 v8.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'While'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'DoWhile'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 12.25 h11.5 M3.75 ' +
          '1 v11" />'#13#10'</svg>'
      end
      item
        IconName = 'For'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11 M9 7 v1 M9 9 v3.5" />'#13#10'</svg>'
      end
      item
        IconName = 'SubProgram'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M12.5 1 v14 M3.5 1 v14" ' +
          '/>'#13#10'</svg>'
      end
      item
        IconName = 'Break'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M8 1.5 L1.5 8 M8 14.5 L1' +
          '.5 8" />'#13#10'</svg>'
      end
      item
        IconName = 'Algorithm'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 "/>'#13#10'  <line stroke="#00' +
          '0000" x1="4" y1="13" x2="8" y2="3" />'#13#10'  <line stroke="#000000" ' +
          'x1="12" y1="13" x2="8" y2="3" />'#13#10'  <line stroke="#000000" x1="6' +
          '" y1="8" x2="10" y2="8" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'GenerateJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#6e9b6e" d="M9 2h1M1' +
          '1 2h1M9 3h1M11 3h1M9 4h1M11 4h1M9 5h1M11 5h1M9 6h1M11 6h1M9 7h1M' +
          '11 7h1M9 8h1M11 8h1M9 9h1M11 9h1M4 10h1M9 10h1" />'#13#10'<path stroke' +
          '="#055200" d="M10 2h1M10 3h1M10 4h1M10 5h1M10 6h1M10 7h1M10 8h1M' +
          '10 9h1M5 10h1M10 10h1M5 11h1M10 11h1M6 12h4M7 13h1" />'#13#10'<path st' +
          'roke="#80a77f" d="M6 10h1M11 10h1" />'#13#10'<path stroke="#99b999" d=' +
          '"M4 11h1" />'#13#10'<path stroke="#226422" d="M6 11h1M10 12h1" />'#13#10'<pa' +
          'th stroke="#e9efe9" d="M7 11h1" />'#13#10'<path stroke="#f6f8f6" d="M8' +
          ' 11h1" />'#13#10'<path stroke="#2b6b2b" d="M9 11h1" />'#13#10'<path stroke="' +
          '#a6c1a6" d="M11 11h1" />'#13#10'<path stroke="#e3ebe3" d="M4 12h1" />'#13 +
          #10'<path stroke="#10560f" d="M5 12h1M8 13h1" />'#13#10'<path stroke="#f0' +
          'f4f0" d="M11 12h1" />'#13#10'<path stroke="#ccdccc" d="M5 13h1" />'#13#10'<p' +
          'ath stroke="#347234" d="M6 13h1" />'#13#10'<path stroke="#538853" d="M' +
          '9 13h1" />'#13#10'<path stroke="#e1eae1" d="M10 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#191919">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomIn'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#191919">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Zm-40-60v-' +
          '80h-80v-80h80v-80h80v80h80v80h-80v80h-80Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'PuzzleMode'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#4900ff" fill="none" ' +
          'd="M1 1.5h14 M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M5.8 1 v10 M' +
          '10.1 10 v5 M5.8 10.5 h4 M9 3 v6 M9.5 3.5 A .5 .5 0 0 1 9.5 7 " /' +
          '>'#13#10'</svg>'
      end
      item
        IconName = 'Delete'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#ffffff">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Statement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 " />'#13#10'</svg>'
      end
      item
        IconName = 'If'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Switch'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M5.8 5.5 v8.5' +
          ' M10.1 5.5 v8.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'
      end
      item
        IconName = 'While'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11" />'#13#10'</svg>'
      end
      item
        IconName = 'DoWhile'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 12.25 h11.5 M3.75 ' +
          '1 v11" />'#13#10'</svg>'
      end
      item
        IconName = 'For'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11 M9 7 v1 M9 9 v3.5" />'#13#10'</svg>'
      end
      item
        IconName = 'SubProgram'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M12.5 1 v14 M3.5 1 v14" ' +
          '/>'#13#10'</svg>'
      end
      item
        IconName = 'Break'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M8 1.5 L1.5 8 M8 14.5 L1' +
          '.5 8" />'#13#10'</svg>'
      end
      item
        IconName = 'Algorithm'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 "/>'#13#10'  <line stroke="#ff' +
          'ffff" x1="4" y1="13" x2="8" y2="3" />'#13#10'  <line stroke="#ffffff" ' +
          'x1="12" y1="13" x2="8" y2="3" />'#13#10'  <line stroke="#ffffff" x1="6' +
          '" y1="8" x2="10" y2="8" /></svg>'
      end
      item
        IconName = 'GenerateJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#6fff6e" d="M9 2h1M1' +
          '1 2h1M9 3h1M11 3h1M9 4h1M11 4h1M9 5h1M11 5h1M9 6h1M11 6h1M9 7h1M' +
          '11 7h1M9 8h1M11 8h1M9 9h1M11 9h1M4 10h1M6 10h1M9 10h1M11 10h1M4 ' +
          '11h1M11 11h1" />'#13#10'<path stroke="#1dfb19" d="M10 2h1M10 3h1M10 4h' +
          '1M10 5h1M10 6h1M10 7h1M10 8h1M10 9h1M10 10h1M10 11h1" />'#13#10'<path ' +
          'stroke="#1df019" d="M5 10h1M5 11h1M5 12h5M7 13h2" />'#13#10'<path stro' +
          'ke="#24d722" d="M6 11h1M10 12h1" />'#13#10'<path stroke="#2de02b" d="M' +
          '9 11h1" />'#13#10'<path stroke="#35e734" d="M6 13h1" />'#13#10'<path stroke=' +
          '"#54fa53" d="M9 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#e6e6e6">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomIn'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#e6e6e6">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Zm-40-60v-' +
          '80h-80v-80h80v-80h80v80h80v80h-80v80h-80Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'PuzzleMode'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#49ffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M5.8 1 v10 M10.1 10 v5 M' +
          '5.8 10.5 h4 M9 3 v6 M9.5 3.5 A .5 .5 0 0 1 9.5 7 " />'#13#10'</svg>'
      end
      item
        IconName = 'Delete'
        SVGText = 
          '<svg viewBox="0 -960 960 960"  fill="#e6e6e6"><path d="M280-120q' +
          '-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-40v5' +
          '20q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-280' +
          'h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/></sv' +
          'g>'
      end
      item
        IconName = 'Copy'
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
        IconName = 'Font'
        SVGText = 
          '<svg  viewBox="0 -960 960 960"  fill="#191919"><path d="M186-80q' +
          '-54 0-80-22t-26-66q0-58 49-74t116-16h21v-56q0-34-1-55.5t-6-35.5q' +
          '-5-14-11.5-19.5T230-430q-9 0-16.5 3t-12.5 8q-4 5-5 10.5t1 11.5q6' +
          ' 11 14 21.5t8 24.5q0 25-17.5 42.5T159-291q-25 0-42.5-17.5T99-351' +
          'q0-27 12-44t32.5-27q20.5-10 47.5-14t58-4q85 0 118 30.5T400-302v1' +
          '47q0 19 4.5 28t15.5 9q12 0 19.5-18t9.5-56h11q-3 62-23.5 87T368-8' +
          '0q-43 0-67.5-13.5T269-134q-10 29-29.5 41.5T186-80Zm373 0q-20 0-3' +
          '2.5-16.5T522-132l102-269q7-17 22-28t34-11q19 0 34 11t22 28l102 2' +
          '69q8 19-4.5 35.5T801-80q-12 0-22-7t-15-19l-20-58H616l-20 58q-4 1' +
          '1-14 18.5T559-80Zm-324-29q13 0 22-20.5t9-49.5v-67q-26 0-38 15.5T' +
          '216-180v11q0 36 4 48t15 12Zm407-125h77l-39-114-38 114Zm-37-285q-' +
          '48 0-76.5-33.5T500-643q0-104 66-170.5T735-880q42 0 68 9.5t26 24.' +
          '5q0 6-2 12t-7 11q-5 7-12.5 10t-15.5 1q-14-4-32-7t-33-3q-71 0-114' +
          ' 48t-43 127q0 22 8 46t36 24q11 0 21.5-5t18.5-14q17-18 31.5-60T71' +
          '2-758q2-13 10.5-18.5T746-782q18 0 27.5 9.5T779-749q-12 43-17.5 7' +
          '5t-5.5 58q0 20 5.5 29t16.5 9q11 0 21.5-8t29.5-30q2-3 15-7 8 0 12' +
          ' 6t4 17q0 28-32 54t-67 26q-26 0-44.5-14T691-574q-15 26-37 40.5T6' +
          '05-519Zm-485-1v-220q0-58 41-99t99-41q58 0 99 41t41 99v220h-80v-8' +
          '0H200v80h-80Zm80-160h120v-60q0-25-17.5-42.5T260-800q-25 0-42.5 1' +
          '7.5T200-740v60Z"/></svg>'
      end
      item
        IconName = 'Copy'
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
        IconName = 'Font'
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
        IconName = 'Configuration'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#10'<path stroke="#828282" d="M0 1h15M0' +
          ' 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11h1M0 1' +
          '2h1M0 13h1" />'#10'<path stroke="#ffffff" d="M1 2h1M11 2h1M13 2h1M2 ' +
          '4h1M4 4h1M6 4h1M8 4h1M10 4h1M12 4h1M1 5h1M3 5h1M5 5h1M9 5h1M11 5' +
          'h1M13 5h1M2 6h1M4 6h1M8 6h1M10 6h1M12 6h1M1 7h1M7 7h1M9 7h1M11 7' +
          'h1M13 7h1M2 8h1M6 8h1M8 8h1M10 8h1M12 8h1M1 9h1M3 9h1M5 9h1M9 9h' +
          '1M11 9h1M13 9h1M2 10h1M4 10h1M8 10h1M10 10h1M12 10h1M1 11h1M7 11' +
          'h1M9 11h1M11 11h1M13 11h1M2 12h1M6 12h1M8 12h1M10 12h1M12 12h1M1' +
          ' 13h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1M13 13h1" />'#10'<path stro' +
          'ke="#0000ff" d="M2 2h9M12 2h1" />'#10'<path stroke="#000000" d="M14 ' +
          '2h1M1 3h14M14 4h1M6 5h2M14 5h1M3 6h1M5 6h2M14 6h1M3 7h3M14 7h1M4' +
          ' 8h1M14 8h1M6 9h2M14 9h1M3 10h1M5 10h2M14 10h1M3 11h3M14 11h1M4 ' +
          '12h1M14 12h1M14 13h1M0 14h15" />'#10'<path stroke="#c4c4c4" d="M1 4h' +
          '1M3 4h1M5 4h1M7 4h1M9 4h1M11 4h1M13 4h1M2 5h1M4 5h1M8 5h1M10 5h1' +
          'M12 5h1M1 6h1M7 6h1M9 6h1M11 6h1M13 6h1M2 7h1M6 7h1M8 7h1M10 7h1' +
          'M12 7h1M1 8h1M3 8h1M5 8h1M7 8h1M9 8h1M11 8h1M13 8h1M2 9h1M4 9h1M' +
          '8 9h1M10 9h1M12 9h1M1 10h1M7 10h1M9 10h1M11 10h1M13 10h1M2 11h1M' +
          '6 11h1M8 11h1M10 11h1M12 11h1M1 12h1M3 12h1M5 12h1M7 12h1M9 12h1' +
          'M11 12h1M13 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h1M12 13h1" />' +
          #10'</svg>'
      end>
    Left = 296
    Top = 152
  end
  object vilToolbarLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Statement'
        Name = 'Statement'
      end
      item
        CollectionIndex = 2
        CollectionName = 'If'
        Name = 'If'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Switch'
        Name = 'Switch'
      end
      item
        CollectionIndex = 4
        CollectionName = 'While'
        Name = 'While'
      end
      item
        CollectionIndex = 5
        CollectionName = 'DoWhile'
        Name = 'DoWhile'
      end
      item
        CollectionIndex = 6
        CollectionName = 'For'
        Name = 'For'
      end
      item
        CollectionIndex = 7
        CollectionName = 'SubProgram'
        Name = 'SubProgram'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Break'
        Name = 'Break'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Algorithm'
        Name = 'Algorithm'
      end
      item
        CollectionIndex = 10
        CollectionName = 'GenerateJava'
        Name = 'GenerateJava'
      end
      item
        CollectionIndex = 11
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 12
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 13
        CollectionName = 'PuzzleMode'
        Name = 'PuzzleMode'
      end>
    ImageCollection = icStructogram
    Left = 80
    Top = 152
  end
  object vilToolbarDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 15
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Statement'
        Name = 'Statement'
      end
      item
        CollectionIndex = 17
        CollectionName = 'If'
        Name = 'If'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Switch'
        Name = 'Switch'
      end
      item
        CollectionIndex = 19
        CollectionName = 'While'
        Name = 'While'
      end
      item
        CollectionIndex = 20
        CollectionName = 'DoWhile'
        Name = 'DoWhile'
      end
      item
        CollectionIndex = 21
        CollectionName = 'For'
        Name = 'For'
      end
      item
        CollectionIndex = 22
        CollectionName = 'SubProgram'
        Name = 'SubProgram'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Break'
        Name = 'Break'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Algorithm'
        Name = 'Algorithm'
      end
      item
        CollectionIndex = 25
        CollectionName = 'GenerateJava'
        Name = 'GenerateJava'
      end
      item
        CollectionIndex = 26
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 28
        CollectionName = 'PuzzleMode'
        Name = 'PuzzleMode'
      end>
    ImageCollection = icStructogram
    Left = 184
    Top = 152
  end
  object vilPopupMenuLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 14
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 10
        CollectionName = 'GenerateJava'
        Name = 'GenerateJava'
      end
      item
        CollectionIndex = 13
        CollectionName = 'PuzzleMode'
        Name = 'PuzzleMode'
      end
      item
        CollectionIndex = 31
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 34
        CollectionName = 'Configuration'
        Name = 'Configuration'
      end>
    ImageCollection = icStructogram
    Left = 208
    Top = 48
  end
  object vilPopupMenuDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 29
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 32
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 25
        CollectionName = 'GenerateJava'
        Name = 'GenerateJava'
      end
      item
        CollectionIndex = 28
        CollectionName = 'PuzzleMode'
        Name = 'PuzzleMode'
      end
      item
        CollectionIndex = 33
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 34
        CollectionName = 'Configuration'
        Name = 'Configuration'
      end>
    ImageCollection = icStructogram
    Left = 328
    Top = 48
  end
  object StructoPopupMenu: TSpTBXPopupMenu
    Images = vilPopupMenuLight
    OnPopup = StructoPopupMenuPopup
    Left = 80
    Top = 48
    object MIDelete: TSpTBXItem
      Caption = 'Delete'
      ImageIndex = 0
      ImageName = 'Delete'
      OnClick = MIDeleteClick
    end
    object MICopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 1
      ImageName = 'Copy'
      OnClick = MICopyClick
    end
    object MISwitchWithCaseLine: TSpTBXItem
      Caption = 'Switch with case line'
      ImageIndex = 4
      ImageName = 'Font'
      OnClick = MISwitchWithCaseLineClick
    end
    object MIAddCase: TSpTBXItem
      Caption = 'Add case'
      OnClick = MIAddCaseClick
    end
    object MIDeleteCase: TSpTBXItem
      Caption = 'Delete case'
      OnClick = MIDeleteCaseClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object MIGenerateMethod: TSpTBXItem
      Caption = 'Create method'
      ImageIndex = 2
      ImageName = 'GenerateJava'
      OnClick = MIGenerateMethodClick
    end
    object MIGenerateProgram: TSpTBXItem
      Caption = 'Create program'
      ImageIndex = 2
      ImageName = 'GenerateJava'
      OnClick = MIGenerateProgramClick
    end
    object MIDataType: TSpTBXSubmenuItem
      Caption = 'Data type'
      object MIBoolean: TSpTBXItem
        Caption = 'boolean'
        OnClick = MIDatatypeClick
      end
      object MIChar: TSpTBXItem
        Caption = 'char'
        OnClick = MIDatatypeClick
      end
      object MIDouble: TSpTBXItem
        Caption = 'double'
        OnClick = MIDatatypeClick
      end
      object MIFloat: TSpTBXItem
        Caption = 'float'
        OnClick = MIDatatypeClick
      end
      object MIInt: TSpTBXItem
        Caption = 'int'
        OnClick = MIDatatypeClick
      end
      object MIString: TSpTBXItem
        Caption = 'String'
        Hint = 'Trash bin'
        OnClick = MIDatatypeClick
      end
    end
    object SpTBXSeparatorItem2: TSpTBXSeparatorItem
    end
    object MICopyAsPicture: TSpTBXItem
      Caption = 'Copy as picture'
      ImageIndex = 1
      ImageName = 'Copy'
      OnClick = MICopyAsPictureClick
    end
    object MIPuzzleMode: TSpTBXItem
      Caption = 'Puzzle mode'
      Checked = True
      ImageIndex = 3
      ImageName = 'PuzzleMode'
      OnClick = MIPuzzleClick
    end
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 4
      ImageName = 'Font'
      OnClick = MIFontClick
    end
    object MIConfiguration: TSpTBXItem
      Caption = 'Configuration'
      ImageIndex = 5
      ImageName = 'Configuration'
      OnClick = MIConfigurationClick
    end
  end
end
