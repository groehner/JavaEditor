object FTooltip: TFTooltip
  Left = 375
  Top = 177
  HorzScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  ClientHeight = 254
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseActivate = FormMouseActivate
  OnShow = FormShow
  TextHeight = 15
  object WebBrowser: TWebBrowser
    Left = 0
    Top = 22
    Width = 404
    Height = 206
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ControlData = {
      4C000000C12900004A1500000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126209000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object ToolbarTooltip: TSpTBXToolbar
    Left = 0
    Top = 0
    Width = 404
    Height = 22
    Align = alTop
    Images = vilToolbarLight
    TabOrder = 1
    object TBClose: TSpTBXItem
      Caption = 'Close'
      Hint = 'Close'
      ImageIndex = 0
      ImageName = 'Close'
      OnClick = TBCloseClick
    end
    object TBGotoSourcecode: TSpTBXItem
      Caption = 'Go to source code'
      Hint = 'Go to source code'
      ImageIndex = 1
      ImageName = 'GotoSourceCode'
      OnClick = TBGotoSourcecodeClick
      Left = 23
    end
    object TBOpenUrl: TSpTBXItem
      Caption = 'Open URL'
      Hint = 'Open'
      ImageIndex = 2
      ImageName = 'OpenURL'
      OnClick = TBOpenUrlClick
      Left = 46
    end
    object TBBack: TSpTBXItem
      Caption = 'Backward'
      Hint = 'Backward'
      Enabled = False
      ImageIndex = 3
      ImageName = 'Backward'
      OnClick = TBBackClick
      Left = 69
    end
    object TBForward: TSpTBXItem
      Caption = 'Forward'
      Hint = 'Forward'
      Enabled = False
      ImageIndex = 4
      ImageName = 'Forward'
      OnClick = TBForwardClick
      Left = 92
    end
    object TBZoomOut: TSpTBXItem
      Caption = 'Zoom out'
      Hint = 'Zoom out'
      ImageIndex = 5
      ImageName = 'ZoomOut'
      OnClick = TBZoomOutClick
      Left = 115
    end
    object TBZoomIn: TSpTBXItem
      Caption = 'Zoom in'
      ImageIndex = 6
      ImageName = 'ZoomIn'
      OnClick = TBZoomInClick
    end
  end
  object SpTBXStatusBar: TSpTBXStatusBar
    Left = 0
    Top = 228
    Width = 404
    Height = 26
    ExplicitTop = 231
  end
  object CloseTooltipTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = CloseTooltipTimerExecute
    Left = 40
    Top = 32
  end
  object OpenTooltipTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = OpenTooltipTimerExecute
    Left = 152
    Top = 32
  end
  object icTooltip: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'GotoSourceCode'
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
        IconName = 'OpenURL'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828082" d="M3 0h6M2' +
          ' 1h2M1 2h1M0 3h2M0 4h1M0 5h1M0 6h1M0 7h1M11 7h1M0 8h1M11 8h1M3 1' +
          '1h1" />'#13#10'<path stroke="#088082" d="M4 1h1M9 7h1" />'#13#10'<path strok' +
          'e="#c4c1c4" d="M5 1h1M3 2h1M4 3h1M2 4h1" />'#13#10'<path stroke="#0880' +
          '00" d="M6 1h3M5 2h5M3 4h2M6 4h2M2 5h5M3 6h5M5 7h4M5 8h2M8 8h1M5 ' +
          '9h1" />'#13#10'<path stroke="#000000" d="M9 1h1M12 1h1M10 2h1M12 2h2M8' +
          ' 3h5M14 3h1M8 4h1M15 4h1M8 5h1M15 5h1M8 6h5M14 6h1M12 7h2M7 8h1M' +
          '12 8h1M1 9h1M6 9h2M10 9h1M2 10h1M5 10h1M7 10h5M4 11h1M11 11h1M4 ' +
          '12h1M11 12h1M5 13h1M7 13h5M6 14h2M7 15h1" />'#13#10'<path stroke="#fff' +
          'fff" d="M2 2h1M4 2h1M3 3h1" />'#13#10'<path stroke="#0000ff" d="M2 3h1' +
          'M5 3h3M1 4h1M1 5h1M1 6h2M1 7h4M10 7h1M2 8h3M9 8h1M2 9h3M8 9h1M4 ' +
          '10h1" />'#13#10'<path stroke="#11ffff" d="M13 3h1M9 4h6M9 5h6M13 6h1M6' +
          ' 10h1M5 11h6M5 12h6M6 13h1" />'#13#10'<path stroke="#000082" d="M5 4h1' +
          'M7 5h1M1 8h1M10 8h1M9 9h1M3 10h1" />'#13#10'</svg>'#13#10
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
        IconName = 'Forward'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#3462ff" d="M9 3h1M9' +
          ' 4h1M9 5h1M2 6h8M2 7h1M2 8h1M12 8h1M11 9h1M10 10h1" />'#13#10'<path st' +
          'roke="#000000" d="M10 4h1M11 5h1M12 6h1M13 7h1M13 8h1M2 9h8M12 9' +
          'h1M9 10h1M11 10h1M9 11h2M9 12h1" />'#13#10'<path stroke="#35ccff" d="M' +
          '10 5h1M11 6h1M12 7h1M11 8h1" />'#13#10'<path stroke="#ccffff" d="M10 6' +
          'h1M3 7h8" />'#13#10'<path stroke="#9affff" d="M11 7h1" />'#13#10'<path strok' +
          'e="#349aff" d="M3 8h8M10 9h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#191919">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'
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
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e9e9e9">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'GotoSourceCode'
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
        IconName = 'OpenURL'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828082" d="M3 0h6M2' +
          ' 1h2M1 2h1M0 3h2M0 4h1M0 5h1M0 6h1M0 7h1M11 7h1M0 8h1M11 8h1M3 1' +
          '1h1" />'#13#10'<path stroke="#088082" d="M4 1h1M9 7h1" />'#13#10'<path strok' +
          'e="#c4c1c4" d="M5 1h1M3 2h1M4 3h1M2 4h1" />'#13#10'<path stroke="#0880' +
          '00" d="M6 1h3M5 2h5M3 4h2M6 4h2M2 5h5M3 6h5M5 7h4M5 8h2M8 8h1M5 ' +
          '9h1" />'#13#10'<path stroke="#e9e9e9" d="M9 1h1M12 1h1M10 2h1M12 2h2M8' +
          ' 3h5M14 3h1M8 4h1M15 4h1M8 5h1M15 5h1M8 6h5M14 6h1M12 7h2M7 8h1M' +
          '12 8h1M1 9h1M6 9h2M10 9h1M2 10h1M5 10h1M7 10h5M4 11h1M11 11h1M4 ' +
          '12h1M11 12h1M5 13h1M7 13h5M6 14h2M7 15h1" />'#13#10'<path stroke="#181' +
          '818" d="M2 2h1M4 2h1M3 3h1" />'#13#10'<path stroke="#0000ff" d="M2 3h1' +
          'M5 3h3M1 4h1M1 5h1M1 6h2M1 7h4M10 7h1M2 8h3M9 8h1M2 9h3M8 9h1M4 ' +
          '10h1" />'#13#10'<path stroke="#11ffff" d="M13 3h1M9 4h6M9 5h6M13 6h1M6' +
          ' 10h1M5 11h6M5 12h6M6 13h1" />'#13#10'<path stroke="#000082" d="M5 4h1' +
          'M7 5h1M1 8h1M10 8h1M9 9h1M3 10h1" />'#13#10'</svg>'
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
        IconName = 'Forward'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#3462ff" d="M9 3h1M9' +
          ' 4h1M9 5h1M2 6h8M2 7h1M2 8h1M12 8h1M11 9h1M10 10h1" />'#13#10'<path st' +
          'roke="#ffffff" d="M10 4h1M11 5h1M12 6h1M13 7h1M13 8h1M2 9h8M12 9' +
          'h1M9 10h1M11 10h1M9 11h2M9 12h1" />'#13#10'<path stroke="#35ccff" d="M' +
          '10 5h1M11 6h1M12 7h1M11 8h1" />'#13#10'<path stroke="#ccffff" d="M10 6' +
          'h1M3 7h8" />'#13#10'<path stroke="#9affff" d="M11 7h1" />'#13#10'<path strok' +
          'e="#349aff" d="M3 8h8M10 9h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#e6e6e6">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'
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
      end>
    Left = 272
    Top = 120
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
        CollectionName = 'GotoSourceCode'
        Name = 'GotoSourceCode'
      end
      item
        CollectionIndex = 2
        CollectionName = 'OpenURL'
        Name = 'OpenURL'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Backward'
        Name = 'Backward'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Forward'
        Name = 'Forward'
      end
      item
        CollectionIndex = 5
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 6
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end>
    ImageCollection = icTooltip
    Left = 40
    Top = 120
  end
  object vilToolbarDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 7
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 8
        CollectionName = 'GotoSourceCode'
        Name = 'GotoSourceCode'
      end
      item
        CollectionIndex = 9
        CollectionName = 'OpenURL'
        Name = 'OpenURL'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Backward'
        Name = 'Backward'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Forward'
        Name = 'Forward'
      end
      item
        CollectionIndex = 12
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 13
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end>
    ImageCollection = icTooltip
    Left = 160
    Top = 120
  end
end
