object FGUIDesigner: TFGUIDesigner
  Left = 564
  Top = 308
  Caption = 'GUI designer'
  ClientHeight = 553
  ClientWidth = 375
  Color = clBtnFace
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Scaled = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  TextHeight = 15
  object PopupMenu: TSpTBXPopupMenu
    Images = vilGuiDesignerLight
    OnPopup = PopupMenuPopup
    Left = 48
    Top = 16
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 0
      ImageName = 'Close1'
      OnClick = MICloseClick
    end
    object MIToSource: TSpTBXItem
      Caption = 'To source code'
      ImageIndex = 1
      ImageName = 'OpenSource'
      OnClick = MIToSourceClick
    end
    object MIForeground: TSpTBXItem
      Caption = 'In the foreground'
      ImageIndex = 2
      ImageName = 'ToFront'
      OnClick = MIForegroundClick
    end
    object MIBackground: TSpTBXItem
      Caption = 'In the background'
      ImageIndex = 3
      ImageName = 'ToBack'
      OnClick = MIBackgroundClick
    end
    object MIAlign: TSpTBXSubmenuItem
      Caption = 'Align'
      ImageIndex = 4
      ImageName = 'Left1'
      object MIAlignLeft: TSpTBXItem
        Tag = 1
        Caption = 'Left'
        ImageIndex = 9
        ImageName = 'Left1'
        OnClick = MIAlignClick
      end
      object MIAlignCentered: TSpTBXItem
        Tag = 2
        Caption = 'Centered'
        ImageIndex = 10
        ImageName = 'Centered1'
        OnClick = MIAlignClick
      end
      object MIAlignRight: TSpTBXItem
        Tag = 3
        Caption = 'Right'
        ImageIndex = 11
        ImageName = 'Right1'
        OnClick = MIAlignClick
      end
      object MIAlignCenteredInWindowHorz: TSpTBXItem
        Tag = 4
        Caption = 'Centered in the window'
        ImageIndex = 12
        ImageName = 'CenteredWindows'
        OnClick = MIAlignClick
      end
      object MISameDistanceHorz: TSpTBXItem
        Tag = 5
        Caption = 'Equal distance'
        ImageIndex = 13
        ImageName = 'SameDistanceH'
        OnClick = MIAlignClick
      end
      object MIAlignLine: TSpTBXSeparatorItem
      end
      object MIAlignTop: TSpTBXItem
        Tag = 6
        Caption = 'Top'
        ImageIndex = 14
        ImageName = 'Top1'
        OnClick = MIAlignClick
      end
      object MIAlignMiddle: TSpTBXItem
        Tag = 7
        Caption = 'Middle'
        ImageIndex = 15
        ImageName = 'Middle1'
        OnClick = MIAlignClick
      end
      object MIAlignBottom: TSpTBXItem
        Tag = 8
        Caption = 'Bottom'
        ImageIndex = 16
        ImageName = 'Bottom1'
        OnClick = MIAlignClick
      end
      object MIAlignCenteredInWindowVert: TSpTBXItem
        Tag = 9
        Caption = 'Centered in the window'
        ImageIndex = 17
        ImageName = 'MiddleWindows'
        OnClick = MIAlignClick
      end
      object MISameDistanceVert: TSpTBXItem
        Tag = 10
        Caption = 'Equal distance'
        ImageIndex = 18
        ImageName = 'SameDistanceV'
        OnClick = MIAlignClick
      end
    end
    object N1: TSpTBXSeparatorItem
    end
    object MISnapToGrid: TSpTBXItem
      Caption = 'Snap to grid'
      Checked = True
      OnClick = MISnapToGridClick
    end
    object N2: TSpTBXSeparatorItem
    end
    object MIDelete: TSpTBXItem
      Caption = 'Delete'
      ImageIndex = 5
      ImageName = 'Delete1'
      OnClick = MIDeleteClick
    end
    object MICut: TSpTBXItem
      Caption = 'Cut'
      ImageIndex = 6
      ImageName = 'Cut1'
      ShortCut = 16472
      OnClick = CutClick
    end
    object MICopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 7
      ImageName = 'Copy1'
      ShortCut = 16451
      OnClick = CopyClick
    end
    object MIPaste: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 8
      ImageName = 'Paste1'
      ShortCut = 16470
      OnClick = PasteClick
    end
    object N4: TSpTBXSeparatorItem
    end
    object MIZoomOut: TSpTBXItem
      Caption = 'Zoom out'
      ImageIndex = 19
      ImageName = 'ZoomOut'
      OnClick = MIZoomOutClick
    end
    object MIZoomIn: TSpTBXItem
      Caption = 'Zoom in'
      ImageIndex = 20
      ImageName = 'ZoomIn'
      OnClick = MIZoomInClick
    end
  end
  object GUIDesignerTimer: TTimer
    Enabled = False
    Interval = 10
    OnTimer = GUIDesignerTimerTimer
    Left = 304
    Top = 88
  end
  object scGuiDesigner: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Close1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'OpenSource'
        SVGText = 
          '<svg viewBox="0 -0.5 14 17">'#13#10'<path stroke="#656565" d="M1 0h9M0' +
          ' 1h1M9 1h2M0 2h1M9 2h1M11 2h1M0 3h1M9 3h1M0 4h1M9 4h3M0 5h1M0 6h' +
          '1M0 7h1M0 8h1M0 9h1M0 10h1M0 11h1M0 12h1M0 13h1M0 14h1" />'#13#10'<pat' +
          'h stroke="#ffffff" d="M1 1h8M1 2h8M1 3h1M8 3h1M1 4h8M1 5h3M8 5h3' +
          'M1 6h11M1 7h3M11 7h1M1 8h11M1 9h1M11 9h1M1 10h11M1 11h3M11 11h1M' +
          '1 12h11M1 13h1M11 13h1" />'#13#10'<path stroke="#ffffe9" d="M10 2h1M10' +
          ' 3h2" />'#13#10'<path stroke="#4b4b4b" d="M2 3h6M4 5h4M4 7h7M2 9h9M4 1' +
          '1h7M2 13h9M1 15h12" />'#13#10'<path stroke="#4e4e4e" d="M12 3h1M12 4h2' +
          'M13 5h1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1M13 1' +
          '3h1M13 14h1" />'#13#10'<path stroke="#fcfade" d="M11 5h2M12 6h1M12 7h1' +
          'M12 8h1M12 9h1M12 10h1M12 11h1M12 12h1M12 13h1M1 14h12" />'#13#10'<pat' +
          'h stroke="#8f8f8f" d="M0 15h1M13 15h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ToFront'
        SVGText = 
          '<svg viewBox="0 -0.5 18 18">'#13#10'<path stroke="#526e2e" d="M5 1h12M' +
          '5 2h1M16 2h1M5 3h1M16 3h1M5 4h1M16 4h1M5 5h1M16 5h1M5 6h1M16 6h1' +
          'M5 7h1M16 7h1M5 8h1M16 8h1M5 9h1M16 9h1M5 10h1M16 10h1M5 11h1M16' +
          ' 11h1M5 12h12" />'#13#10'<path stroke="#cedbbf" d="M6 2h1" />'#13#10'<path s' +
          'troke="#b4c99e" d="M7 2h4M6 3h1M6 4h1M6 5h1M6 6h1M6 7h1M6 8h1M6 ' +
          '9h1" />'#13#10'<path stroke="#bacda2" d="M11 2h1" />'#13#10'<path stroke="#b' +
          'bcea3" d="M12 2h1" />'#13#10'<path stroke="#bfd2a7" d="M13 2h1" />'#13#10'<p' +
          'ath stroke="#c1d4a9" d="M14 2h2" />'#13#10'<path stroke="#8faf6d" d="M' +
          '7 3h3M7 4h2M7 5h2M7 6h2M7 7h2" />'#13#10'<path stroke="#93b371" d="M10' +
          ' 3h1" />'#13#10'<path stroke="#95b371" d="M11 3h1" />'#13#10'<path stroke="#' +
          '99b675" d="M12 3h1M11 5h1" />'#13#10'<path stroke="#9ebb7a" d="M13 3h1' +
          'M10 7h1" />'#13#10'<path stroke="#9dbb79" d="M14 3h1" />'#13#10'<path stroke' +
          '="#a6c181" d="M15 3h1" />'#13#10'<path stroke="#91b16f" d="M9 4h1" />'#13 +
          #10'<path stroke="#95b573" d="M10 4h1" />'#13#10'<path stroke="#96b472" d' +
          '="M11 4h1" />'#13#10'<path stroke="#9cb879" d="M12 4h1" />'#13#10'<path stro' +
          'ke="#a0be7c" d="M13 4h1" />'#13#10'<path stroke="#a5c080" d="M14 4h1M1' +
          '0 11h1" />'#13#10'<path stroke="#a6c182" d="M15 4h1M14 5h1" />'#13#10'<path ' +
          'stroke="#93b271" d="M9 5h1" />'#13#10'<path stroke="#96b574" d="M10 5h' +
          '1M9 6h1" />'#13#10'<path stroke="#9cba78" d="M12 5h1" />'#13#10'<path stroke' +
          '="#a3bf7e" d="M13 5h1" />'#13#10'<path stroke="#a8c483" d="M15 5h1" />' +
          #13#10'<path stroke="#9ab676" d="M10 6h1M9 8h1" />'#13#10'<path stroke="#9d' +
          'ba79" d="M11 6h1M10 9h1" />'#13#10'<path stroke="#a2bf7d" d="M12 6h1" ' +
          '/>'#13#10'<path stroke="#a5c081" d="M13 6h1" />'#13#10'<path stroke="#a6c281' +
          '" d="M14 6h1" />'#13#10'<path stroke="#abc586" d="M15 6h1" />'#13#10'<path s' +
          'troke="#a9a9a9" d="M1 7h4M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h' +
          '1M12 13h1M1 14h1M12 14h1M1 15h1M12 15h1M1 16h12" />'#13#10'<path strok' +
          'e="#99b676" d="M9 7h1" />'#13#10'<path stroke="#a1bf7e" d="M11 7h1" />' +
          #13#10'<path stroke="#a1bd7c" d="M12 7h1" />'#13#10'<path stroke="#a3be7f" ' +
          'd="M13 7h1" />'#13#10'<path stroke="#a9c485" d="M14 7h1" />'#13#10'<path str' +
          'oke="#abc786" d="M15 7h1M12 11h1" />'#13#10'<path stroke="#e3e3e3" d="' +
          'M2 8h1" />'#13#10'<path stroke="#d7d7d7" d="M3 8h2M2 9h1M2 10h1M2 11h1' +
          'M2 12h1M2 13h1M2 14h1M2 15h1" />'#13#10'<path stroke="#92af6d" d="M7 8' +
          'h1" />'#13#10'<path stroke="#96b272" d="M8 8h1" />'#13#10'<path stroke="#a0b' +
          'd7b" d="M10 8h1" />'#13#10'<path stroke="#9fbc7b" d="M11 8h1M9 10h1" /' +
          '>'#13#10'<path stroke="#a4bf80" d="M12 8h1" />'#13#10'<path stroke="#a9c683"' +
          ' d="M13 8h1" />'#13#10'<path stroke="#acc687" d="M14 8h1M13 9h1" />'#13#10'<' +
          'path stroke="#b1cc8b" d="M15 8h1M14 9h1" />'#13#10'<path stroke="#c6c6' +
          'c6" d="M3 9h2M3 10h2M3 11h2M3 12h2M3 13h3M3 14h3M3 15h2" />'#13#10'<pa' +
          'th stroke="#93b06e" d="M7 9h1" />'#13#10'<path stroke="#98b573" d="M8 ' +
          '9h1" />'#13#10'<path stroke="#9bb777" d="M9 9h1" />'#13#10'<path stroke="#a1' +
          'bd7e" d="M11 9h1" />'#13#10'<path stroke="#a7c382" d="M12 9h1" />'#13#10'<pa' +
          'th stroke="#b3ce8d" d="M15 9h1" />'#13#10'<path stroke="#b5ca9f" d="M6' +
          ' 10h1" />'#13#10'<path stroke="#95b270" d="M7 10h1" />'#13#10'<path stroke="' +
          '#99b574" d="M8 10h1" />'#13#10'<path stroke="#a3c07e" d="M10 10h1" />'#13 +
          #10'<path stroke="#a4bf7f" d="M11 10h1" />'#13#10'<path stroke="#abc686" ' +
          'd="M12 10h1" />'#13#10'<path stroke="#aec988" d="M13 10h1" />'#13#10'<path s' +
          'troke="#b2cd8d" d="M14 10h1M13 11h1" />'#13#10'<path stroke="#b4cf8e" ' +
          'd="M15 10h1M14 11h1" />'#13#10'<path stroke="#b9cba0" d="M6 11h1" />'#13#10 +
          '<path stroke="#97b472" d="M7 11h1" />'#13#10'<path stroke="#99b674" d=' +
          '"M8 11h1" />'#13#10'<path stroke="#9fbc7a" d="M9 11h1" />'#13#10'<path strok' +
          'e="#a6c283" d="M11 11h1" />'#13#10'<path stroke="#b6d190" d="M15 11h1"' +
          ' />'#13#10'<path stroke="#c7c7c7" d="M6 13h1M6 14h1M5 15h1" />'#13#10'<path ' +
          'stroke="#c9c9c9" d="M7 13h1" />'#13#10'<path stroke="#cacaca" d="M8 13' +
          'h1M7 14h2M7 15h1" />'#13#10'<path stroke="#cbcbcb" d="M9 13h2M9 14h1M8' +
          ' 15h1" />'#13#10'<path stroke="#cccccc" d="M11 13h1M10 14h1M9 15h1" />' +
          #13#10'<path stroke="#cdcdcd" d="M11 14h1M10 15h1" />'#13#10'<path stroke="' +
          '#c8c8c8" d="M6 15h1" />'#13#10'<path stroke="#cecece" d="M11 15h1" />'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'ToBack'
        SVGText = 
          '<svg viewBox="0 -0.5 18 18">'#13#10'<path stroke="#a8b697" d="M5 1h12M' +
          '5 2h1M16 2h1M5 3h1M16 3h1M5 4h1M16 4h1M5 5h1M16 5h1M5 6h1M16 6h1' +
          'M16 7h1M16 8h1M16 9h1M16 10h1M16 11h1M13 12h4" />'#13#10'<path stroke=' +
          '"#e2e7db" d="M6 2h1" />'#13#10'<path stroke="#d5e0cc" d="M7 2h4M6 3h1M' +
          '6 4h1M6 5h1M6 6h1" />'#13#10'<path stroke="#d9e2cd" d="M11 2h1" />'#13#10'<p' +
          'ath stroke="#d9e2ce" d="M12 2h1" />'#13#10'<path stroke="#dbe4d0" d="M' +
          '13 2h1" />'#13#10'<path stroke="#dce5d0" d="M14 2h2" />'#13#10'<path stroke=' +
          '"#c5d3b5" d="M7 3h3M7 4h2M7 5h2M7 6h2" />'#13#10'<path stroke="#c7d5b7' +
          '" d="M10 3h1" />'#13#10'<path stroke="#c8d5b7" d="M11 3h1M11 4h1" />'#13#10 +
          '<path stroke="#c9d6b9" d="M12 3h1M11 5h1" />'#13#10'<path stroke="#ccd' +
          '8bb" d="M13 3h1" />'#13#10'<path stroke="#cbd8bb" d="M14 3h1M11 6h1" /' +
          '>'#13#10'<path stroke="#cfdcbf" d="M15 3h1M15 4h1M14 5h1M13 6h1" />'#13#10'<' +
          'path stroke="#c6d4b6" d="M9 4h1" />'#13#10'<path stroke="#c8d6b8" d="M' +
          '10 4h1M10 5h1M9 6h1" />'#13#10'<path stroke="#cbd7bb" d="M12 4h1" />'#13#10 +
          '<path stroke="#ccdbbc" d="M13 4h1" />'#13#10'<path stroke="#cfdcbe" d=' +
          '"M14 4h1" />'#13#10'<path stroke="#c7d4b7" d="M9 5h1" />'#13#10'<path stroke' +
          '="#cbd8ba" d="M12 5h1" />'#13#10'<path stroke="#cedbbd" d="M13 5h1" />' +
          #13#10'<path stroke="#d0debf" d="M15 5h1M13 8h1" />'#13#10'<path stroke="#c' +
          'ad6b9" d="M10 6h1" />'#13#10'<path stroke="#cddbbc" d="M12 6h1" />'#13#10'<p' +
          'ath stroke="#cfddbf" d="M14 6h1" />'#13#10'<path stroke="#d1dec1" d="M' +
          '15 6h1" />'#13#10'<path stroke="#4f4f4f" d="M1 7h12M1 8h1M12 8h1M1 9h1' +
          'M12 9h1M1 10h1M12 10h1M1 11h1M12 11h1M1 12h1M12 12h1M1 13h1M12 1' +
          '3h1M1 14h1M12 14h1M1 15h1M12 15h1M1 16h12" />'#13#10'<path stroke="#ce' +
          'dbbe" d="M13 7h1" />'#13#10'<path stroke="#d0dec0" d="M14 7h1" />'#13#10'<pa' +
          'th stroke="#d1dfc1" d="M15 7h1" />'#13#10'<path stroke="#cecece" d="M2' +
          ' 8h1" />'#13#10'<path stroke="#b5b5b5" d="M3 8h6M2 9h1M2 10h1M2 11h1M2' +
          ' 12h1M2 13h1M2 14h1M2 15h1" />'#13#10'<path stroke="#b7b7b7" d="M9 8h1' +
          '" />'#13#10'<path stroke="#b8b8b8" d="M10 8h1" />'#13#10'<path stroke="#b9b9' +
          'b9" d="M11 8h1" />'#13#10'<path stroke="#d2dec1" d="M14 8h1M13 9h1" />' +
          #13#10'<path stroke="#d4e1c3" d="M15 8h1M14 9h1" />'#13#10'<path stroke="#8' +
          'f8f8f" d="M3 9h5M3 10h4M3 11h4M3 12h4M3 13h3M3 14h3M3 15h2" />'#13#10 +
          '<path stroke="#919191" d="M8 9h1M7 10h1M6 14h1M5 15h1" />'#13#10'<path' +
          ' stroke="#939393" d="M9 9h1M8 10h1M6 15h1" />'#13#10'<path stroke="#95' +
          '9595" d="M10 9h1M9 10h1" />'#13#10'<path stroke="#979797" d="M11 9h1M1' +
          '0 10h1M7 14h1" />'#13#10'<path stroke="#d5e2c4" d="M15 9h1M15 10h1M14 ' +
          '11h1" />'#13#10'<path stroke="#999999" d="M11 10h1M8 14h1M7 15h1" />'#13#10 +
          '<path stroke="#d3e0c2" d="M13 10h1" />'#13#10'<path stroke="#d4e2c4" d' +
          '="M14 10h1M13 11h1" />'#13#10'<path stroke="#929292" d="M7 11h1" />'#13#10'<' +
          'path stroke="#949494" d="M8 11h1M7 12h1" />'#13#10'<path stroke="#9696' +
          '96" d="M9 11h1M8 12h1M7 13h1" />'#13#10'<path stroke="#989898" d="M10 ' +
          '11h1M9 12h1M8 13h1" />'#13#10'<path stroke="#9a9a9a" d="M11 11h1M10 12' +
          'h1M9 13h1" />'#13#10'<path stroke="#d6e3c5" d="M15 11h1" />'#13#10'<path str' +
          'oke="#9c9c9c" d="M11 12h1M9 15h1" />'#13#10'<path stroke="#909090" d="' +
          'M6 13h1" />'#13#10'<path stroke="#9b9b9b" d="M10 13h1M9 14h1M8 15h1" /' +
          '>'#13#10'<path stroke="#9d9d9d" d="M11 13h1M10 14h1" />'#13#10'<path stroke=' +
          '"#9f9f9f" d="M11 14h1" />'#13#10'<path stroke="#9e9e9e" d="M10 15h1" /' +
          '>'#13#10'<path stroke="#a0a0a0" d="M11 15h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Left1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7c73" d="M1 1h1" ' +
          '/>'#13#10'<path stroke="#565249" d="M2 1h1M1 2h1M1 3h1M1 4h1M1 5h1M1 6' +
          'h1M1 7h1M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h1" />'#13#10'<path stro' +
          'ke="#000000" d="M2 2h1M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1' +
          'M2 10h1M2 11h1M2 12h1M2 13h1M1 14h2" />'#13#10'<path stroke="#4f623b" ' +
          'd="M4 2h12M4 3h1M15 3h1M4 4h1M15 4h1M4 5h1M15 5h1M4 6h12" />'#13#10'<p' +
          'ath stroke="#cedbc1" d="M5 3h1" />'#13#10'<path stroke="#b5c7a2" d="M6' +
          ' 3h6M5 4h1M5 5h1" />'#13#10'<path stroke="#b7c9a4" d="M12 3h1" />'#13#10'<pa' +
          'th stroke="#b8caa5" d="M13 3h1" />'#13#10'<path stroke="#b9caa7" d="M1' +
          '4 3h1" />'#13#10'<path stroke="#90ab73" d="M6 4h3M6 5h2" />'#13#10'<path str' +
          'oke="#92ad75" d="M9 4h1M8 5h1" />'#13#10'<path stroke="#97b17d" d="M10' +
          ' 4h1" />'#13#10'<path stroke="#99b37e" d="M11 4h1M10 5h1" />'#13#10'<path st' +
          'roke="#9bb481" d="M12 4h1M11 5h1" />'#13#10'<path stroke="#9db683" d="' +
          'M13 4h1" />'#13#10'<path stroke="#9fb786" d="M14 4h1" />'#13#10'<path stroke' +
          '="#93ae78" d="M9 5h1" />'#13#10'<path stroke="#9cb582" d="M12 5h1" />'#13 +
          #10'<path stroke="#9eb685" d="M13 5h1" />'#13#10'<path stroke="#a0b887" d' +
          '="M14 5h1" />'#13#10'<path stroke="#344f6b" d="M4 8h7M4 9h1M10 9h1M4 1' +
          '0h1M10 10h1M4 11h1M10 11h1M4 12h7" />'#13#10'<path stroke="#c2cddb" d=' +
          '"M5 9h1" />'#13#10'<path stroke="#a3b4c8" d="M6 9h2M5 10h1M5 11h1" />'#13 +
          #10'<path stroke="#a3b5c8" d="M8 9h1" />'#13#10'<path stroke="#a5b6c9" d=' +
          '"M9 9h1" />'#13#10'<path stroke="#7a94b2" d="M6 10h1" />'#13#10'<path stroke' +
          '="#7c96b3" d="M7 10h1M6 11h1" />'#13#10'<path stroke="#7e98b5" d="M8 1' +
          '0h1M7 11h1" />'#13#10'<path stroke="#809ab7" d="M9 10h1M8 11h1" />'#13#10'<p' +
          'ath stroke="#829cb8" d="M9 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete1'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'
      end
      item
        IconName = 'Cut1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#f9f9f9" d="M3 0h1M1' +
          '2 6h1" />'#13#10'<path stroke="#f6f6f6" d="M12 0h1" />'#13#10'<path stroke="' +
          '#a2a2a2" d="M13 0h1M5 4h1" />'#13#10'<path stroke="#929292" d="M2 1h1"' +
          ' />'#13#10'<path stroke="#a6a6a6" d="M3 1h1" />'#13#10'<path stroke="#a4a4a4' +
          '" d="M12 1h1" />'#13#10'<path stroke="#959595" d="M13 1h1" />'#13#10'<path s' +
          'troke="#afafaf" d="M2 2h1" />'#13#10'<path stroke="#777777" d="M3 2h1M' +
          '12 2h1M3 3h1M12 3h1M4 4h1M11 4h1M4 5h2M10 5h2M5 6h1M10 6h1M6 7h1' +
          'M9 7h2M7 8h2" />'#13#10'<path stroke="#cfcfcf" d="M4 2h1M6 5h1" />'#13#10'<p' +
          'ath stroke="#cdcdcd" d="M11 2h1M9 5h1" />'#13#10'<path stroke="#b0b0b0' +
          '" d="M13 2h1" />'#13#10'<path stroke="#dcdcdc" d="M2 3h1" />'#13#10'<path st' +
          'roke="#808080" d="M4 3h1" />'#13#10'<path stroke="#f2f2f2" d="M5 3h1" ' +
          '/>'#13#10'<path stroke="#f1f1f1" d="M10 3h1" />'#13#10'<path stroke="#7f7f7f' +
          '" d="M11 3h1" />'#13#10'<path stroke="#d7d7d7" d="M13 3h1" />'#13#10'<path s' +
          'troke="#888888" d="M3 4h1" />'#13#10'<path stroke="#a0a0a0" d="M10 4h1' +
          '" />'#13#10'<path stroke="#838383" d="M12 4h1" />'#13#10'<path stroke="#fbfb' +
          'fb" d="M13 4h1" />'#13#10'<path stroke="#c8c8c8" d="M3 5h1" />'#13#10'<path ' +
          'stroke="#bebebe" d="M12 5h1" />'#13#10'<path stroke="#909090" d="M4 6h' +
          '1" />'#13#10'<path stroke="#7e7e7e" d="M6 6h1M9 6h1" />'#13#10'<path stroke=' +
          '"#ededed" d="M7 6h1" />'#13#10'<path stroke="#ececec" d="M8 6h1" />'#13#10'<' +
          'path stroke="#868686" d="M11 6h1" />'#13#10'<path stroke="#e7e7e7" d="' +
          'M4 7h1" />'#13#10'<path stroke="#7c7c7c" d="M5 7h1" />'#13#10'<path stroke="' +
          '#919191" d="M7 7h1" />'#13#10'<path stroke="#8f8f8f" d="M8 7h1" />'#13#10'<p' +
          'ath stroke="#d8d8d8" d="M11 7h1" />'#13#10'<path stroke="#d5d5d5" d="M' +
          '5 8h1" />'#13#10'<path stroke="#7a7a7a" d="M6 8h1" />'#13#10'<path stroke="#' +
          '767676" d="M9 8h1" />'#13#10'<path stroke="#c0c0c0" d="M10 8h1" />'#13#10'<p' +
          'ath stroke="#c4d1e0" d="M6 9h1" />'#13#10'<path stroke="#90abc6" d="M7' +
          ' 9h2" />'#13#10'<path stroke="#adc0d4" d="M9 9h1" />'#13#10'<path stroke="#b' +
          'acee3" d="M2 10h1" />'#13#10'<path stroke="#5a89bb" d="M3 10h1" />'#13#10'<p' +
          'ath stroke="#4076b0" d="M4 10h1" />'#13#10'<path stroke="#457bb2" d="M' +
          '5 10h1M10 14h1M13 14h1" />'#13#10'<path stroke="#5384b8" d="M6 10h1M9 ' +
          '10h1" />'#13#10'<path stroke="#3f77b0" d="M7 10h2M6 11h1M9 11h1M13 11h' +
          '1M6 12h1M9 12h1M6 13h1M9 13h1" />'#13#10'<path stroke="#5182b7" d="M10' +
          ' 10h1M12 11h1" />'#13#10'<path stroke="#4278b2" d="M11 10h1" />'#13#10'<path' +
          ' stroke="#5888ba" d="M12 10h1" />'#13#10'<path stroke="#b3c9e1" d="M13' +
          ' 10h1M8 13h1" />'#13#10'<path stroke="#c4d5e7" d="M1 11h1" />'#13#10'<path s' +
          'troke="#4078b1" d="M2 11h1" />'#13#10'<path stroke="#4f81b6" d="M3 11h' +
          '1M3 15h1M11 15h1" />'#13#10'<path stroke="#a1bdda" d="M4 11h1" />'#13#10'<pa' +
          'th stroke="#7ba2c9" d="M5 11h1" />'#13#10'<path stroke="#4077b1" d="M7' +
          ' 11h1M1 13h1M14 13h1" />'#13#10'<path stroke="#497eb4" d="M8 11h1" />'#13 +
          #10'<path stroke="#719ac4" d="M10 11h1" />'#13#10'<path stroke="#a1bcd7" ' +
          'd="M11 11h1" />'#13#10'<path stroke="#c0d1e6" d="M14 11h1" />'#13#10'<path s' +
          'troke="#6692c0" d="M1 12h1" />'#13#10'<path stroke="#4379b1" d="M2 12h' +
          '1M5 14h1" />'#13#10'<path stroke="#dfe8f2" d="M3 12h1" />'#13#10'<path strok' +
          'e="#e1e9f3" d="M5 12h1M6 15h1" />'#13#10'<path stroke="#5b8bbc" d="M7 ' +
          '12h1M9 14h1" />'#13#10'<path stroke="#79a0c8" d="M8 12h1" />'#13#10'<path st' +
          'roke="#d8e3ef" d="M10 12h1" />'#13#10'<path stroke="#dde7f1" d="M12 12' +
          'h1" />'#13#10'<path stroke="#4278b1" d="M13 12h1" />'#13#10'<path stroke="#6' +
          '28fbf" d="M14 12h1M1 14h1" />'#13#10'<path stroke="#6590bf" d="M2 13h1' +
          'M13 13h1" />'#13#10'<path stroke="#a6c0dc" d="M5 13h1" />'#13#10'<path strok' +
          'e="#8dafd1" d="M7 13h1" />'#13#10'<path stroke="#a8c1dc" d="M10 13h1M1' +
          '1 14h1" />'#13#10'<path stroke="#447ab2" d="M2 14h1" />'#13#10'<path stroke=' +
          '"#a9c2dd" d="M3 14h1" />'#13#10'<path stroke="#a0bbd8" d="M4 14h1" />'#13 +
          #10'<path stroke="#4c7eb5" d="M6 14h1" />'#13#10'<path stroke="#e6ecf5" d' +
          '="M7 14h1" />'#13#10'<path stroke="#f9fbfc" d="M8 14h1" />'#13#10'<path stro' +
          'ke="#b2c8e0" d="M12 14h1" />'#13#10'<path stroke="#5f8dbd" d="M14 14h1' +
          '" />'#13#10'<path stroke="#e5edf4" d="M1 15h1" />'#13#10'<path stroke="#7ba1' +
          'c8" d="M2 15h1" />'#13#10'<path stroke="#5283b8" d="M4 15h1" />'#13#10'<path' +
          ' stroke="#80a5cb" d="M5 15h1M10 15h1" />'#13#10'<path stroke="#e9eff5"' +
          ' d="M9 15h1" />'#13#10'<path stroke="#4a7eb5" d="M12 15h1" />'#13#10'<path s' +
          'troke="#759dc7" d="M13 15h1" />'#13#10'<path stroke="#e2eaf3" d="M14 1' +
          '5h1" />'#13#10'</svg>'#13#10
      end
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
          '="M15 8h1" />'#13#10'<path stroke="#8f8f8f" d="M6 15h1M15 15h1" />'#13#10
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
          #13#10'</svg>'
      end
      item
        IconName = 'Left1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7c73" d="M1 1h1" ' +
          '/>'#13#10'<path stroke="#565249" d="M2 1h1M1 2h1M1 3h1M1 4h1M1 5h1M1 6' +
          'h1M1 7h1M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h1" />'#13#10'<path stro' +
          'ke="#000000" d="M2 2h1M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1' +
          'M2 10h1M2 11h1M2 12h1M2 13h1M1 14h2" />'#13#10'<path stroke="#4f623b" ' +
          'd="M4 2h12M4 3h1M15 3h1M4 4h1M15 4h1M4 5h1M15 5h1M4 6h12" />'#13#10'<p' +
          'ath stroke="#cedbc1" d="M5 3h1" />'#13#10'<path stroke="#b5c7a2" d="M6' +
          ' 3h6M5 4h1M5 5h1" />'#13#10'<path stroke="#b7c9a4" d="M12 3h1" />'#13#10'<pa' +
          'th stroke="#b8caa5" d="M13 3h1" />'#13#10'<path stroke="#b9caa7" d="M1' +
          '4 3h1" />'#13#10'<path stroke="#90ab73" d="M6 4h3M6 5h2" />'#13#10'<path str' +
          'oke="#92ad75" d="M9 4h1M8 5h1" />'#13#10'<path stroke="#97b17d" d="M10' +
          ' 4h1" />'#13#10'<path stroke="#99b37e" d="M11 4h1M10 5h1" />'#13#10'<path st' +
          'roke="#9bb481" d="M12 4h1M11 5h1" />'#13#10'<path stroke="#9db683" d="' +
          'M13 4h1" />'#13#10'<path stroke="#9fb786" d="M14 4h1" />'#13#10'<path stroke' +
          '="#93ae78" d="M9 5h1" />'#13#10'<path stroke="#9cb582" d="M12 5h1" />'#13 +
          #10'<path stroke="#9eb685" d="M13 5h1" />'#13#10'<path stroke="#a0b887" d' +
          '="M14 5h1" />'#13#10'<path stroke="#344f6b" d="M4 8h7M4 9h1M10 9h1M4 1' +
          '0h1M10 10h1M4 11h1M10 11h1M4 12h7" />'#13#10'<path stroke="#c2cddb" d=' +
          '"M5 9h1" />'#13#10'<path stroke="#a3b4c8" d="M6 9h2M5 10h1M5 11h1" />'#13 +
          #10'<path stroke="#a3b5c8" d="M8 9h1" />'#13#10'<path stroke="#a5b6c9" d=' +
          '"M9 9h1" />'#13#10'<path stroke="#7a94b2" d="M6 10h1" />'#13#10'<path stroke' +
          '="#7c96b3" d="M7 10h1M6 11h1" />'#13#10'<path stroke="#7e98b5" d="M8 1' +
          '0h1M7 11h1" />'#13#10'<path stroke="#809ab7" d="M9 10h1M8 11h1" />'#13#10'<p' +
          'ath stroke="#829cb8" d="M9 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Centered1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7c73" d="M7 1h1" ' +
          '/>'#13#10'<path stroke="#565249" d="M8 1h1M7 2h1M7 3h1M7 4h1M7 5h1M7 6' +
          'h1M7 7h1M7 8h1M7 9h1M7 10h1M7 11h1M7 12h1M7 13h1" />'#13#10'<path stro' +
          'ke="#4f623b" d="M2 2h5M9 2h5M2 3h1M13 3h1M2 4h1M13 4h1M2 5h1M13 ' +
          '5h1M2 6h5M9 6h5" />'#13#10'<path stroke="#000000" d="M8 2h1M8 3h1M8 4h' +
          '1M8 5h1M8 6h1M8 7h1M8 8h1M8 9h1M8 10h1M8 11h1M8 12h1M8 13h1M7 14' +
          'h2" />'#13#10'<path stroke="#cedbc1" d="M3 3h1" />'#13#10'<path stroke="#b5c' +
          '7a2" d="M4 3h3M9 3h1M3 4h1M3 5h1" />'#13#10'<path stroke="#b7c9a4" d="' +
          'M10 3h1" />'#13#10'<path stroke="#b8caa5" d="M11 3h1" />'#13#10'<path stroke' +
          '="#b9caa7" d="M12 3h1" />'#13#10'<path stroke="#90ab73" d="M4 4h3M4 5h' +
          '2" />'#13#10'<path stroke="#99b37e" d="M9 4h1" />'#13#10'<path stroke="#9bb4' +
          '81" d="M10 4h1M9 5h1" />'#13#10'<path stroke="#9db683" d="M11 4h1" />'#13 +
          #10'<path stroke="#9fb786" d="M12 4h1" />'#13#10'<path stroke="#92ad75" d' +
          '="M6 5h1" />'#13#10'<path stroke="#9cb582" d="M10 5h1" />'#13#10'<path strok' +
          'e="#9eb685" d="M11 5h1" />'#13#10'<path stroke="#a0b887" d="M12 5h1" /' +
          '>'#13#10'<path stroke="#344f6b" d="M4 8h3M9 8h3M4 9h1M11 9h1M4 10h1M11' +
          ' 10h1M4 11h1M11 11h1M4 12h3M9 12h3" />'#13#10'<path stroke="#c2cddb" d' +
          '="M5 9h1" />'#13#10'<path stroke="#a3b4c8" d="M6 9h1M5 10h1M5 11h1" />' +
          #13#10'<path stroke="#a3b5c8" d="M9 9h1" />'#13#10'<path stroke="#a5b6c9" d' +
          '="M10 9h1" />'#13#10'<path stroke="#7a94b2" d="M6 10h1" />'#13#10'<path stro' +
          'ke="#7e98b5" d="M9 10h1" />'#13#10'<path stroke="#809ab7" d="M10 10h1M' +
          '9 11h1" />'#13#10'<path stroke="#7c96b3" d="M6 11h1" />'#13#10'<path stroke=' +
          '"#829cb8" d="M10 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Right1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7c73" d="M13 1h1"' +
          ' />'#13#10'<path stroke="#565249" d="M14 1h1M13 2h1M13 3h1M13 4h1M13 5' +
          'h1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1M13 13h1" ' +
          '/>'#13#10'<path stroke="#4f623b" d="M0 2h12M0 3h1M11 3h1M0 4h1M11 4h1M' +
          '0 5h1M11 5h1M0 6h12" />'#13#10'<path stroke="#000000" d="M14 2h1M14 3h' +
          '1M14 4h1M14 5h1M14 6h1M14 7h1M14 8h1M14 9h1M14 10h1M14 11h1M14 1' +
          '2h1M14 13h1M13 14h2" />'#13#10'<path stroke="#cedbc1" d="M1 3h1" />'#13#10'<' +
          'path stroke="#b5c7a2" d="M2 3h6M1 4h1M1 5h1" />'#13#10'<path stroke="#' +
          'b7c9a4" d="M8 3h1" />'#13#10'<path stroke="#b8caa5" d="M9 3h1" />'#13#10'<pa' +
          'th stroke="#b9caa7" d="M10 3h1" />'#13#10'<path stroke="#90ab73" d="M2' +
          ' 4h3M2 5h2" />'#13#10'<path stroke="#92ad75" d="M5 4h1M4 5h1" />'#13#10'<pat' +
          'h stroke="#97b17d" d="M6 4h1" />'#13#10'<path stroke="#99b37e" d="M7 4' +
          'h1M6 5h1" />'#13#10'<path stroke="#9bb481" d="M8 4h1M7 5h1" />'#13#10'<path ' +
          'stroke="#9db683" d="M9 4h1" />'#13#10'<path stroke="#9fb786" d="M10 4h' +
          '1" />'#13#10'<path stroke="#93ae78" d="M5 5h1" />'#13#10'<path stroke="#9cb5' +
          '82" d="M8 5h1" />'#13#10'<path stroke="#9eb685" d="M9 5h1" />'#13#10'<path s' +
          'troke="#a0b887" d="M10 5h1" />'#13#10'<path stroke="#344f6b" d="M5 8h7' +
          'M5 9h1M11 9h1M5 10h1M11 10h1M5 11h1M11 11h1M5 12h7" />'#13#10'<path st' +
          'roke="#c2cddb" d="M6 9h1" />'#13#10'<path stroke="#a3b4c8" d="M7 9h2M6' +
          ' 10h1M6 11h1" />'#13#10'<path stroke="#a3b5c8" d="M9 9h1" />'#13#10'<path st' +
          'roke="#a5b6c9" d="M10 9h1" />'#13#10'<path stroke="#7a94b2" d="M7 10h1' +
          '" />'#13#10'<path stroke="#7c96b3" d="M8 10h1M7 11h1" />'#13#10'<path stroke' +
          '="#7e98b5" d="M9 10h1M8 11h1" />'#13#10'<path stroke="#809ab7" d="M10 ' +
          '10h1M9 11h1" />'#13#10'<path stroke="#829cb8" d="M10 11h1" />'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = 'CenteredWindows'
        SVGText = 
          '<svg viewBox="0 -0.5 20 20">'#13#10'<path stroke="#7e7e7e" d="M1 1h18M' +
          '1 2h1M18 2h1M1 3h1M18 3h1M1 4h1M18 4h1M1 5h1M18 5h1M1 6h1M18 6h1' +
          'M1 7h1M18 7h1M1 8h1M18 8h1M1 9h1M18 9h1M1 10h1M18 10h1M1 11h1M18' +
          ' 11h1M1 12h1M18 12h1M1 13h1M18 13h1M1 14h1M18 14h1M1 15h1M18 15h' +
          '1M1 16h1M18 16h1M1 17h1M18 17h1M1 18h18" />'#13#10'<path stroke="#7e7c' +
          '73" d="M9 3h1" />'#13#10'<path stroke="#565249" d="M10 3h1M9 4h1M9 5h1' +
          'M9 6h1M9 7h1M9 8h1M9 9h1M9 10h1M9 11h1M9 12h1M9 13h1M9 14h1M9 15' +
          'h1" />'#13#10'<path stroke="#4f623b" d="M4 4h5M11 4h5M4 5h1M15 5h1M4 6' +
          'h1M15 6h1M4 7h1M15 7h1M4 8h5M11 8h5" />'#13#10'<path stroke="#000000" ' +
          'd="M10 4h1M10 5h1M10 6h1M10 7h1M10 8h1M10 9h1M10 10h1M10 11h1M10' +
          ' 12h1M10 13h1M10 14h1M10 15h1M9 16h2" />'#13#10'<path stroke="#cedbc1"' +
          ' d="M5 5h1" />'#13#10'<path stroke="#b5c7a2" d="M6 5h3M11 5h1M5 6h1M5 ' +
          '7h1" />'#13#10'<path stroke="#b7c9a4" d="M12 5h1" />'#13#10'<path stroke="#b' +
          '8caa5" d="M13 5h1" />'#13#10'<path stroke="#b9caa7" d="M14 5h1" />'#13#10'<p' +
          'ath stroke="#90ab73" d="M6 6h3M6 7h2" />'#13#10'<path stroke="#99b37e"' +
          ' d="M11 6h1" />'#13#10'<path stroke="#9bb481" d="M12 6h1M11 7h1" />'#13#10'<' +
          'path stroke="#9db683" d="M13 6h1" />'#13#10'<path stroke="#9fb786" d="' +
          'M14 6h1" />'#13#10'<path stroke="#92ad75" d="M8 7h1" />'#13#10'<path stroke=' +
          '"#9cb582" d="M12 7h1" />'#13#10'<path stroke="#9eb685" d="M13 7h1" />'#13 +
          #10'<path stroke="#a0b887" d="M14 7h1" />'#13#10'<path stroke="#344f6b" d' +
          '="M6 10h3M11 10h3M6 11h1M13 11h1M6 12h1M13 12h1M6 13h1M13 13h1M6' +
          ' 14h3M11 14h3" />'#13#10'<path stroke="#c2cddb" d="M7 11h1" />'#13#10'<path ' +
          'stroke="#a3b4c8" d="M8 11h1M7 12h1M7 13h1" />'#13#10'<path stroke="#a3' +
          'b5c8" d="M11 11h1" />'#13#10'<path stroke="#a5b6c9" d="M12 11h1" />'#13#10'<' +
          'path stroke="#7a94b2" d="M8 12h1" />'#13#10'<path stroke="#7e98b5" d="' +
          'M11 12h1" />'#13#10'<path stroke="#809ab7" d="M12 12h1M11 13h1" />'#13#10'<p' +
          'ath stroke="#7c96b3" d="M8 13h1" />'#13#10'<path stroke="#829cb8" d="M' +
          '12 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SameDistanceH'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#344f6b" d="M1 5h3M6' +
          ' 5h4M12 5h3M1 6h1M3 6h1M6 6h1M9 6h1M12 6h1M14 6h1M1 7h1M3 7h1M6 ' +
          '7h1M9 7h1M12 7h1M14 7h1M1 8h1M3 8h1M6 8h1M9 8h1M12 8h1M14 8h1M1 ' +
          '9h3M6 9h4M12 9h3" />'#13#10'<path stroke="#c2cddb" d="M2 6h1M7 6h2M13 ' +
          '6h1" />'#13#10'<path stroke="#a3b4c8" d="M2 7h1M7 7h2M13 7h1M2 8h1M7 8' +
          'h2M13 8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Top1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7e7c73" d="M1 1h1" ' +
          '/>'#13#10'<path stroke="#565249" d="M2 1h12M1 2h1" />'#13#10'<path stroke="#' +
          '000000" d="M14 1h1M2 2h13" />'#13#10'<path stroke="#4f623b" d="M3 4h5M' +
          '3 5h1M7 5h1M3 6h1M7 6h1M3 7h1M7 7h1M3 8h1M7 8h1M3 9h1M7 9h1M3 10' +
          'h1M7 10h1M3 11h1M7 11h1M3 12h1M7 12h1M3 13h1M7 13h1M3 14h5" />'#13#10 +
          '<path stroke="#344f6b" d="M9 4h5M9 5h1M13 5h1M9 6h1M13 6h1M9 7h1' +
          'M13 7h1M9 8h1M13 8h1M9 9h1M13 9h1M9 10h1M13 10h1M9 11h5" />'#13#10'<pa' +
          'th stroke="#cedbc1" d="M4 5h1" />'#13#10'<path stroke="#b5c7a2" d="M5 ' +
          '5h2M4 6h1M4 7h1M4 8h1M4 9h1M4 10h1" />'#13#10'<path stroke="#c2cddb" d' +
          '="M10 5h1" />'#13#10'<path stroke="#a3b4c8" d="M11 5h2M10 6h1M10 7h1" ' +
          '/>'#13#10'<path stroke="#90ab73" d="M5 6h2M5 7h2M5 8h1" />'#13#10'<path stro' +
          'ke="#7a94b2" d="M11 6h1M11 7h1" />'#13#10'<path stroke="#7c96b3" d="M1' +
          '2 6h1M12 7h1" />'#13#10'<path stroke="#92ad75" d="M6 8h1M5 9h1" />'#13#10'<p' +
          'ath stroke="#a3b5c8" d="M10 8h1M10 9h1" />'#13#10'<path stroke="#7e98b' +
          '5" d="M11 8h1M11 9h1" />'#13#10'<path stroke="#809ab7" d="M12 8h1M12 9' +
          'h1M11 10h1" />'#13#10'<path stroke="#93ae78" d="M6 9h1" />'#13#10'<path stro' +
          'ke="#99b37e" d="M5 10h1" />'#13#10'<path stroke="#9bb481" d="M6 10h1M5' +
          ' 11h1" />'#13#10'<path stroke="#a5b6c9" d="M10 10h1" />'#13#10'<path stroke=' +
          '"#829cb8" d="M12 10h1" />'#13#10'<path stroke="#b7c9a4" d="M4 11h1" />' +
          #13#10'<path stroke="#9cb582" d="M6 11h1" />'#13#10'<path stroke="#b8caa5" ' +
          'd="M4 12h1" />'#13#10'<path stroke="#9db683" d="M5 12h1" />'#13#10'<path str' +
          'oke="#9eb685" d="M6 12h1" />'#13#10'<path stroke="#b9caa7" d="M4 13h1"' +
          ' />'#13#10'<path stroke="#9fb786" d="M5 13h1" />'#13#10'<path stroke="#a0b88' +
          '7" d="M6 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Middle1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#4f623b" d="M2 2h5M2' +
          ' 3h1M6 3h1M2 4h1M6 4h1M2 5h1M6 5h1M2 6h1M6 6h1M2 9h1M6 9h1M2 10h' +
          '1M6 10h1M2 11h1M6 11h1M2 12h1M6 12h1M2 13h5" />'#13#10'<path stroke="#' +
          'cedbc1" d="M3 3h1" />'#13#10'<path stroke="#b5c7a2" d="M4 3h2M3 4h1M3 ' +
          '5h1M3 6h1M3 9h1" />'#13#10'<path stroke="#90ab73" d="M4 4h2M4 5h2M4 6h' +
          '1" />'#13#10'<path stroke="#344f6b" d="M8 4h5M8 5h1M12 5h1M8 6h1M12 6h' +
          '1M8 9h1M12 9h1M8 10h1M12 10h1M8 11h5" />'#13#10'<path stroke="#c2cddb"' +
          ' d="M9 5h1" />'#13#10'<path stroke="#a3b4c8" d="M10 5h2M9 6h1" />'#13#10'<pa' +
          'th stroke="#92ad75" d="M5 6h1" />'#13#10'<path stroke="#7a94b2" d="M10' +
          ' 6h1" />'#13#10'<path stroke="#7c96b3" d="M11 6h1" />'#13#10'<path stroke="#' +
          '7e7c73" d="M1 7h1" />'#13#10'<path stroke="#565249" d="M2 7h12M1 8h1" ' +
          '/>'#13#10'<path stroke="#000000" d="M14 7h1M2 8h13" />'#13#10'<path stroke="' +
          '#99b37e" d="M4 9h1" />'#13#10'<path stroke="#9bb481" d="M5 9h1M4 10h1"' +
          ' />'#13#10'<path stroke="#a3b5c8" d="M9 9h1" />'#13#10'<path stroke="#7e98b5' +
          '" d="M10 9h1" />'#13#10'<path stroke="#809ab7" d="M11 9h1M10 10h1" />'#13 +
          #10'<path stroke="#b7c9a4" d="M3 10h1" />'#13#10'<path stroke="#9cb582" d' +
          '="M5 10h1" />'#13#10'<path stroke="#a5b6c9" d="M9 10h1" />'#13#10'<path stro' +
          'ke="#829cb8" d="M11 10h1" />'#13#10'<path stroke="#b8caa5" d="M3 11h1"' +
          ' />'#13#10'<path stroke="#9db683" d="M4 11h1" />'#13#10'<path stroke="#9eb68' +
          '5" d="M5 11h1" />'#13#10'<path stroke="#b9caa7" d="M3 12h1" />'#13#10'<path ' +
          'stroke="#9fb786" d="M4 12h1" />'#13#10'<path stroke="#a0b887" d="M5 12' +
          'h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bottom1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#4f623b" d="M3 1h5M3' +
          ' 2h1M7 2h1M3 3h1M7 3h1M3 4h1M7 4h1M3 5h1M7 5h1M3 6h1M7 6h1M3 7h1' +
          'M7 7h1M3 8h1M7 8h1M3 9h1M7 9h1M3 10h1M7 10h1M3 11h5" />'#13#10'<path s' +
          'troke="#cedbc1" d="M4 2h1" />'#13#10'<path stroke="#b5c7a2" d="M5 2h2M' +
          '4 3h1M4 4h1M4 5h1M4 6h1M4 7h1" />'#13#10'<path stroke="#90ab73" d="M5 ' +
          '3h2M5 4h2M5 5h1" />'#13#10'<path stroke="#344f6b" d="M9 4h5M9 5h1M13 5' +
          'h1M9 6h1M13 6h1M9 7h1M13 7h1M9 8h1M13 8h1M9 9h1M13 9h1M9 10h1M13' +
          ' 10h1M9 11h5" />'#13#10'<path stroke="#92ad75" d="M6 5h1M5 6h1" />'#13#10'<p' +
          'ath stroke="#c2cddb" d="M10 5h1" />'#13#10'<path stroke="#a3b4c8" d="M' +
          '11 5h2M10 6h1M10 7h1" />'#13#10'<path stroke="#93ae78" d="M6 6h1" />'#13#10 +
          '<path stroke="#7a94b2" d="M11 6h1M11 7h1" />'#13#10'<path stroke="#7c9' +
          '6b3" d="M12 6h1M12 7h1" />'#13#10'<path stroke="#99b37e" d="M5 7h1" />' +
          #13#10'<path stroke="#9bb481" d="M6 7h1M5 8h1" />'#13#10'<path stroke="#b7c' +
          '9a4" d="M4 8h1" />'#13#10'<path stroke="#9cb582" d="M6 8h1" />'#13#10'<path ' +
          'stroke="#a3b5c8" d="M10 8h1M10 9h1" />'#13#10'<path stroke="#7e98b5" d' +
          '="M11 8h1M11 9h1" />'#13#10'<path stroke="#809ab7" d="M12 8h1M12 9h1M1' +
          '1 10h1" />'#13#10'<path stroke="#b8caa5" d="M4 9h1" />'#13#10'<path stroke="' +
          '#9db683" d="M5 9h1" />'#13#10'<path stroke="#9eb685" d="M6 9h1" />'#13#10'<p' +
          'ath stroke="#b9caa7" d="M4 10h1" />'#13#10'<path stroke="#9fb786" d="M' +
          '5 10h1" />'#13#10'<path stroke="#a0b887" d="M6 10h1" />'#13#10'<path stroke=' +
          '"#a5b6c9" d="M10 10h1" />'#13#10'<path stroke="#829cb8" d="M12 10h1" /' +
          '>'#13#10'<path stroke="#7e7c73" d="M1 13h1" />'#13#10'<path stroke="#565249"' +
          ' d="M2 13h12M1 14h1" />'#13#10'<path stroke="#000000" d="M14 13h1M2 14' +
          'h13" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'MiddleWindows'
        SVGText = 
          '<svg viewBox="0 -0.5 20 20">'#13#10'<path stroke="#7e7e7e" d="M1 1h18M' +
          '1 2h1M18 2h1M1 3h1M18 3h1M1 4h1M18 4h1M1 5h1M18 5h1M1 6h1M18 6h1' +
          'M1 7h1M18 7h1M1 8h1M18 8h1M1 9h1M18 9h1M1 10h1M18 10h1M1 11h1M18' +
          ' 11h1M1 12h1M18 12h1M1 13h1M18 13h1M1 14h1M18 14h1M1 15h1M18 15h' +
          '1M1 16h1M18 16h1M1 17h1M18 17h1M1 18h18" />'#13#10'<path stroke="#4f62' +
          '3b" d="M4 4h5M4 5h1M8 5h1M4 6h1M8 6h1M4 7h1M8 7h1M4 8h1M8 8h1M4 ' +
          '11h1M8 11h1M4 12h1M8 12h1M4 13h1M8 13h1M4 14h1M8 14h1M4 15h5" />' +
          #13#10'<path stroke="#cedbc1" d="M5 5h1" />'#13#10'<path stroke="#b5c7a2" d' +
          '="M6 5h2M5 6h1M5 7h1M5 8h1M5 11h1" />'#13#10'<path stroke="#90ab73" d=' +
          '"M6 6h2M6 7h2M6 8h1" />'#13#10'<path stroke="#344f6b" d="M10 6h5M10 7h' +
          '1M14 7h1M10 8h1M14 8h1M10 11h1M14 11h1M10 12h1M14 12h1M10 13h5" ' +
          '/>'#13#10'<path stroke="#c2cddb" d="M11 7h1" />'#13#10'<path stroke="#a3b4c8' +
          '" d="M12 7h2M11 8h1" />'#13#10'<path stroke="#92ad75" d="M7 8h1" />'#13#10'<' +
          'path stroke="#7a94b2" d="M12 8h1" />'#13#10'<path stroke="#7c96b3" d="' +
          'M13 8h1" />'#13#10'<path stroke="#7e7c73" d="M3 9h1" />'#13#10'<path stroke=' +
          '"#565249" d="M4 9h12M3 10h1" />'#13#10'<path stroke="#000000" d="M16 9' +
          'h1M4 10h13" />'#13#10'<path stroke="#99b37e" d="M6 11h1" />'#13#10'<path str' +
          'oke="#9bb481" d="M7 11h1M6 12h1" />'#13#10'<path stroke="#a3b5c8" d="M' +
          '11 11h1" />'#13#10'<path stroke="#7e98b5" d="M12 11h1" />'#13#10'<path strok' +
          'e="#809ab7" d="M13 11h1M12 12h1" />'#13#10'<path stroke="#b7c9a4" d="M' +
          '5 12h1" />'#13#10'<path stroke="#9cb582" d="M7 12h1" />'#13#10'<path stroke=' +
          '"#a5b6c9" d="M11 12h1" />'#13#10'<path stroke="#829cb8" d="M13 12h1" /' +
          '>'#13#10'<path stroke="#b8caa5" d="M5 13h1" />'#13#10'<path stroke="#9db683"' +
          ' d="M6 13h1" />'#13#10'<path stroke="#9eb685" d="M7 13h1" />'#13#10'<path st' +
          'roke="#b9caa7" d="M5 14h1" />'#13#10'<path stroke="#9fb786" d="M6 14h1' +
          '" />'#13#10'<path stroke="#a0b887" d="M7 14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SameDistanceV'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#344f6b" d="M6 1h5M6' +
          ' 2h1M10 2h1M6 3h5M6 6h5M6 7h1M10 7h1M6 8h1M10 8h1M6 9h5M6 12h5M6' +
          ' 13h1M10 13h1M6 14h5" />'#13#10'<path stroke="#a3b4c8" d="M7 2h2M7 7h2' +
          'M7 8h2M7 13h2" />'#13#10'<path stroke="#c2cddb" d="M9 2h1M9 7h1M9 8h1M' +
          '9 13h1" />'#13#10'</svg>'#13#10
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
      end>
    Left = 168
    Top = 16
  end
  object icJavaControls1315: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#9e9e9e" d="M0 1h12M' +
          '0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11h1M0 ' +
          '12h1" />'#13#10'<path stroke="#ffffff" d="M12 1h1M12 2h1M2 3h9M12 3h1M' +
          '2 4h9M12 4h1M2 5h9M12 5h1M2 6h9M12 6h1M2 7h9M12 7h1M2 8h9M12 8h1' +
          'M2 9h9M12 9h1M2 10h9M12 10h1M2 11h9M12 11h1M12 12h1M0 13h13" />'#13 +
          #10'<path stroke="#686868" d="M1 2h10M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1' +
          'M1 8h1M1 9h1M1 10h1M1 11h1" />'#13#10'<path stroke="#e2e2e2" d="M11 2h' +
          '1M11 3h1M11 4h1M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11' +
          'h1M1 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#9e9e9e" d="M0 1h12M' +
          '0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11h1M0 ' +
          '12h1" />'#13#10'<path stroke="#ffffff" d="M12 1h1M12 2h1M2 3h9M12 3h1M' +
          '2 4h7M10 4h1M12 4h1M2 5h6M10 5h1M12 5h1M2 6h1M4 6h3M10 6h1M12 6h' +
          '1M2 7h1M5 7h1M9 7h2M12 7h1M2 8h1M8 8h3M12 8h1M2 9h2M7 9h4M12 9h1' +
          'M2 10h3M6 10h5M12 10h1M2 11h9M12 11h1M12 12h1M0 13h13" />'#13#10'<path' +
          ' stroke="#686868" d="M1 2h10M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1' +
          'M1 9h1M1 10h1M1 11h1" />'#13#10'<path stroke="#e2e2e2" d="M11 2h1M11 3' +
          'h1M11 4h1M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M1 1' +
          '2h11" />'#13#10'<path stroke="#000000" d="M9 4h1M8 5h2M3 6h1M7 6h3M3 7' +
          'h2M6 7h3M3 8h5M4 9h3M5 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#798897" d="M0 1h13M' +
          '0 2h1M12 2h1M0 3h1M12 3h1M0 4h1M12 4h1M0 5h1M12 5h1M0 6h1M12 6h1' +
          'M0 7h1M12 7h1M0 8h1M12 8h1M0 9h1M12 9h1M0 10h1M12 10h1M0 11h1M12' +
          ' 11h1M0 12h1M12 12h1M0 13h13" />'#13#10'<path stroke="#e7eff6" d="M1 2' +
          'h11M1 6h11" />'#13#10'<path stroke="#f3f6fa" d="M1 3h11M1 5h11" />'#13#10'<p' +
          'ath stroke="#ffffff" d="M1 4h11" />'#13#10'<path stroke="#dce7f2" d="M' +
          '1 7h11" />'#13#10'<path stroke="#d6e4f0" d="M1 8h11" />'#13#10'<path stroke=' +
          '"#d1e0ee" d="M1 9h11" />'#13#10'<path stroke="#cbdcec" d="M1 10h11" />' +
          #13#10'<path stroke="#c6d8ea" d="M1 11h11" />'#13#10'<path stroke="#c1d4e8"' +
          ' d="M1 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#798897" d="M0 1h13M' +
          '0 2h1M12 2h1M0 3h1M12 3h1M0 4h1M12 4h1M0 5h1M12 5h1M0 6h1M12 6h1' +
          'M0 7h1M12 7h1M0 8h1M12 8h1M0 9h1M12 9h1M0 10h1M12 10h1M0 11h1M12' +
          ' 11h1M0 12h1M12 12h1M0 13h13" />'#13#10'<path stroke="#e7eff6" d="M1 2' +
          'h11M1 6h2M5 6h2M9 6h3" />'#13#10'<path stroke="#f3f6fa" d="M1 3h11M1 5' +
          'h7M10 5h2" />'#13#10'<path stroke="#ffffff" d="M1 4h8M10 4h2" />'#13#10'<pat' +
          'h stroke="#363636" d="M9 4h1M8 5h2M3 6h2M7 6h2M3 7h2M6 7h2M3 8h4' +
          'M3 9h3M3 10h2" />'#13#10'<path stroke="#dce7f2" d="M1 7h2M5 7h1M8 7h4"' +
          ' />'#13#10'<path stroke="#d6e4f0" d="M1 8h2M7 8h5" />'#13#10'<path stroke="#' +
          'd1e0ee" d="M1 9h2M6 9h6" />'#13#10'<path stroke="#cbdcec" d="M1 10h2M5' +
          ' 10h7" />'#13#10'<path stroke="#c6d8ea" d="M1 11h11" />'#13#10'<path stroke=' +
          '"#c1d4e8" d="M1 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#9e9e9e" d="M4 1h4M2' +
          ' 2h2M8 2h2M1 3h1M1 4h1M0 5h1M0 6h1M0 7h1M0 8h1M1 9h1M1 10h1" />'#13 +
          #10'<path stroke="#686868" d="M4 2h4M2 3h2M8 3h2M2 4h1M1 5h1M1 6h1M' +
          '1 7h1M1 8h1M2 9h1" />'#13#10'<path stroke="#ffffff" d="M4 3h4M10 3h1M3' +
          ' 4h6M10 4h1M2 5h8M11 5h1M2 6h8M11 6h1M2 7h8M11 7h1M2 8h8M11 8h1M' +
          '3 9h6M10 9h1M4 10h4M10 10h1M2 11h2M8 11h2M4 12h4" />'#13#10'<path stro' +
          'ke="#e2e2e2" d="M9 4h1M10 5h1M10 6h1M10 7h1M10 8h1M9 9h1M2 10h2M' +
          '8 10h2M4 11h4" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#9e9e9e" d="M4 1h4M2' +
          ' 2h2M8 2h2M1 3h1M1 4h1M0 5h1M0 6h1M0 7h1M0 8h1M1 9h1M1 10h1" />'#13 +
          #10'<path stroke="#686868" d="M4 2h4M2 3h2M8 3h2M2 4h1M1 5h1M1 6h1M' +
          '1 7h1M1 8h1M2 9h1" />'#13#10'<path stroke="#ffffff" d="M4 3h4M10 3h1M3' +
          ' 4h6M10 4h1M2 5h3M7 5h3M11 5h1M2 6h2M8 6h2M11 6h1M2 7h2M8 7h2M11' +
          ' 7h1M2 8h3M7 8h3M11 8h1M3 9h6M10 9h1M4 10h4M10 10h1M2 11h2M8 11h' +
          '2M4 12h4" />'#13#10'<path stroke="#e2e2e2" d="M9 4h1M10 5h1M10 6h1M10 ' +
          '7h1M10 8h1M9 9h1M2 10h2M8 10h2M4 11h4" />'#13#10'<path stroke="#000000' +
          '" d="M5 5h2M4 6h4M4 7h4M5 8h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '06'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#798897" d="M4 1h4M2' +
          ' 2h2M8 2h2M1 3h1M10 3h1M1 4h1M10 4h1M0 5h1M11 5h1M0 6h1M11 6h1M0' +
          ' 7h1M11 7h1M0 8h1M11 8h1M1 9h1M10 9h1M1 10h1M10 10h1M2 11h2M8 11' +
          'h2M4 12h4" />'#13#10'<path stroke="#dce7f2" d="M4 2h4M1 8h10" />'#13#10'<pat' +
          'h stroke="#e7eff6" d="M2 3h8M1 7h10" />'#13#10'<path stroke="#f3f6fa" ' +
          'd="M2 4h8M1 6h10" />'#13#10'<path stroke="#ffffff" d="M1 5h10" />'#13#10'<pa' +
          'th stroke="#d2e1ef" d="M2 9h8" />'#13#10'<path stroke="#c9dbeb" d="M2 ' +
          '10h8" />'#13#10'<path stroke="#bfd3e8" d="M4 11h4" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '07'
        SVGText = 
          '<svg viewBox="0 -0.5 13 15">'#13#10'<path stroke="#798897" d="M4 1h4M2' +
          ' 2h2M8 2h2M1 3h1M10 3h1M1 4h1M10 4h1M0 5h1M11 5h1M0 6h1M11 6h1M0' +
          ' 7h1M11 7h1M0 8h1M11 8h1M1 9h1M10 9h1M1 10h1M10 10h1M2 11h2M8 11' +
          'h2M4 12h4" />'#13#10'<path stroke="#dce7f2" d="M4 2h4M1 8h2M9 8h2" />'#13 +
          #10'<path stroke="#e7eff6" d="M2 3h8M1 7h2M9 7h2" />'#13#10'<path stroke=' +
          '"#f3f6fa" d="M2 4h2M8 4h2M1 6h2M9 6h2" />'#13#10'<path stroke="#363636' +
          '" d="M4 4h4M3 5h6M3 6h6M3 7h6M3 8h6M4 9h4" />'#13#10'<path stroke="#ff' +
          'ffff" d="M1 5h2M9 5h2" />'#13#10'<path stroke="#d2e1ef" d="M2 9h2M8 9h' +
          '2" />'#13#10'<path stroke="#c9dbeb" d="M2 10h8" />'#13#10'<path stroke="#bfd' +
          '3e8" d="M4 11h4" />'#13#10'</svg>'#13#10
      end>
    Left = 48
    Top = 88
  end
  object icJavaControls1616: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#6380bd" d="M1 0h13M' +
          '0 1h1M14 1h1M0 2h1M14 2h1M0 3h1M14 3h1M0 4h1M14 4h1M0 5h1M14 5h1' +
          'M0 6h1M14 6h1M0 7h1M14 7h1M0 8h1M14 8h1M1 9h1M13 9h1M2 10h1M12 1' +
          '0h1M3 11h1M11 11h1M4 12h1M10 12h1M5 13h1M9 13h1M6 14h1M8 14h1M7 ' +
          '15h1" />'#13#10'<path stroke="#b6cde4" d="M1 1h13M1 2h1M13 2h1M1 3h1M1' +
          '3 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1M1 7h1M13 7h1M1 8h1M' +
          '13 8h1M2 9h1M12 9h1M3 10h1M11 10h1M4 11h1M10 11h1M5 12h1M9 12h1M' +
          '6 13h1M8 13h1M7 14h1" />'#13#10'<path stroke="#d4e5f4" d="M2 2h11M4 10' +
          'h7" />'#13#10'<path stroke="#e3edf8" d="M2 3h11M3 9h9" />'#13#10'<path strok' +
          'e="#f0f6fb" d="M2 4h11M2 8h11" />'#13#10'<path stroke="#ffffff" d="M2 ' +
          '5h11M2 6h11M2 7h11" />'#13#10'<path stroke="#c6dcf1" d="M5 11h5" />'#13#10'<' +
          'path stroke="#c2d8ee" d="M6 12h3" />'#13#10'<path stroke="#bed4eb" d="' +
          'M7 13h1" />'#13#10'</svg>'
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16" >'#13#10'<path stroke="#6380bd" d="M1 0h8M' +
          '0 1h1M9 1h1M0 2h1M10 2h1M0 3h1M11 3h1M0 4h1M12 4h1M0 5h1M13 5h1M' +
          '0 6h1M14 6h1M0 7h1M15 7h1M0 8h1M14 8h1M0 9h1M13 9h1M0 10h1M12 10' +
          'h1M0 11h1M11 11h1M0 12h1M10 12h1M0 13h1M9 13h1M1 14h8" />'#13#10'<path' +
          ' stroke="#b6cde4" d="M1 1h8M1 2h1M9 2h1M1 3h1M10 3h1M1 4h1M11 4h' +
          '1M1 5h1M12 5h1M1 6h1M13 6h1M1 7h1M14 7h1M1 8h1M13 8h1M1 9h1M12 9' +
          'h1M1 10h1M11 10h1M1 11h1M10 11h1M1 12h1M9 12h1M1 13h8" />'#13#10'<path' +
          ' stroke="#d4e5f4" d="M2 2h1M2 3h1M2 4h1M10 4h1M2 5h1M10 5h1M2 6h' +
          '1M10 6h1M2 7h1M10 7h1M2 8h1M10 8h1M2 9h1M10 9h1M2 10h1M10 10h1M2' +
          ' 11h1M2 12h1" />'#13#10'<path stroke="#e3edf8" d="M3 2h1M3 3h1M9 3h1M3' +
          ' 4h1M9 4h1M3 5h1M9 5h1M3 6h1M9 6h1M3 7h1M9 7h1M3 8h1M9 8h1M3 9h1' +
          'M9 9h1M3 10h1M9 10h1M3 11h1M9 11h1M3 12h1" />'#13#10'<path stroke="#f0' +
          'f6fb" d="M4 2h1M8 2h1M4 3h1M8 3h1M4 4h1M8 4h1M4 5h1M8 5h1M4 6h1M' +
          '8 6h1M4 7h1M8 7h1M4 8h1M8 8h1M4 9h1M8 9h1M4 10h1M8 10h1M4 11h1M8' +
          ' 11h1M4 12h1M8 12h1" />'#13#10'<path stroke="#ffffff" d="M5 2h3M5 3h3M' +
          '5 4h3M5 5h3M5 6h3M5 7h3M5 8h3M5 9h3M5 10h3M5 11h3M5 12h3" />'#13#10'<p' +
          'ath stroke="#c6dcf1" d="M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1" />'#13 +
          #10'<path stroke="#c2d8ee" d="M12 6h1M12 7h1M12 8h1" />'#13#10'<path stro' +
          'ke="#bed4eb" d="M13 7h1" />'#13#10'</svg>'
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16" >'#13#10'<path stroke="#e0e0e0" d="M0 0h4"' +
          ' />'#13#10'<path stroke="#e1e1e1" d="M4 0h12" />'#13#10'<path stroke="#e4e4e' +
          '4" d="M0 1h4" />'#13#10'<path stroke="#e5e5e5" d="M4 1h5" />'#13#10'<path st' +
          'roke="#e7e7e7" d="M9 1h1" />'#13#10'<path stroke="#ececec" d="M11 1h1M' +
          '9 2h1M0 3h4M0 4h4M0 5h4M0 6h4M0 7h4M0 8h4M0 9h4M0 10h4M0 11h4M0 ' +
          '12h4" />'#13#10'<path stroke="#efefef" d="M12 1h1M11 2h1M10 3h1M10 4h1' +
          'M10 5h1M10 9h1M10 10h1M10 11h1M10 12h1M9 13h1" />'#13#10'<path stroke=' +
          '"#f1f1f1" d="M13 1h1M12 2h1M5 3h1M7 3h1M12 3h1M8 4h1M12 4h1M9 5h' +
          '1M12 5h1M12 6h1M12 7h1M12 8h1M9 9h1M12 9h1M8 10h1M12 10h1M5 11h1' +
          'M7 11h1M12 11h1M12 12h1M11 13h1M10 14h1" />'#13#10'<path stroke="#f3f3' +
          'f3" d="M14 1h1M14 2h1M6 3h1M14 3h1M5 4h1M7 4h1M14 4h1M5 5h1M8 5h' +
          '1M14 5h1M5 6h1M14 6h1M5 7h1M14 7h1M5 8h1M14 8h1M5 9h1M8 9h1M14 9' +
          'h1M5 10h1M7 10h1M14 10h1M6 11h1M14 11h1M14 12h1M14 13h1M13 14h2"' +
          ' />'#13#10'<path stroke="#eaeaea" d="M0 2h6" />'#13#10'<path stroke="#ebebeb' +
          '" d="M6 2h3" />'#13#10'<path stroke="#ededed" d="M10 2h1M4 3h1M8 3h1M4' +
          ' 4h1M4 5h1M4 6h1M4 7h1M4 8h1M4 9h1M4 10h1M4 11h1M8 11h1M4 12h5" ' +
          '/>'#13#10'<path stroke="#f2f2f2" d="M13 2h1M13 3h1M13 4h1M13 5h1M10 6h' +
          '1M13 6h1M13 7h1M10 8h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1M12 ' +
          '13h2M11 14h2" />'#13#10'<path stroke="#eeeeee" d="M9 3h1M9 4h1M9 10h1M' +
          '9 11h1M9 12h1M0 13h9" />'#13#10'<path stroke="#f0f0f0" d="M11 3h1M11 4' +
          'h1M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M11 12h1M10' +
          ' 13h1M0 14h10" />'#13#10'<path stroke="#333333" d="M6 4h1" />'#13#10'<path s' +
          'troke="#292929" d="M6 5h1M6 6h1" />'#13#10'<path stroke="#555555" d="M' +
          '7 5h1" />'#13#10'<path stroke="#5f5f5f" d="M7 6h1" />'#13#10'<path stroke="#' +
          '747474" d="M8 6h1" />'#13#10'<path stroke="#f4f4f4" d="M9 6h1M10 7h1M9' +
          ' 8h1" />'#13#10'<path stroke="#2c2c2c" d="M6 7h1" />'#13#10'<path stroke="#7' +
          '87878" d="M7 7h1" />'#13#10'<path stroke="#a7a7a7" d="M8 7h1" />'#13#10'<pat' +
          'h stroke="#aaaaaa" d="M9 7h1" />'#13#10'<path stroke="#3e3e3e" d="M6 8' +
          'h1" />'#13#10'<path stroke="#a3a3a3" d="M7 8h1" />'#13#10'<path stroke="#cac' +
          'aca" d="M8 8h1" />'#13#10'<path stroke="#656565" d="M6 9h1" />'#13#10'<path ' +
          'stroke="#bdbdbd" d="M7 9h1" />'#13#10'<path stroke="#8a8a8a" d="M6 10h' +
          '1" />'#13#10'</svg>'
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#dddddd" d="M0 0h13"' +
          ' />'#13#10'<path stroke="#dcdcdc" d="M13 0h3" />'#13#10'<path stroke="#e1e1e' +
          '1" d="M0 1h16" />'#13#10'<path stroke="#e6e6e6" d="M0 2h13" />'#13#10'<path ' +
          'stroke="#e5e5e5" d="M13 2h3" />'#13#10'<path stroke="#e8e8e8" d="M0 3h' +
          '4M5 3h2M10 3h6M0 4h4M5 4h1M10 4h6M0 5h4M10 5h6M0 6h4M10 6h6M0 7h' +
          '4M10 7h6M0 8h4M10 8h6M0 9h4M10 9h6M0 10h4M5 10h1M10 10h6M0 11h4M' +
          '5 11h2M10 11h6M0 12h4M5 12h11" />'#13#10'<path stroke="#e9e9e9" d="M4 ' +
          '3h1M4 4h1M4 5h1M4 9h1M4 10h1M4 11h1M4 12h1M0 15h16" />'#13#10'<path st' +
          'roke="#ededed" d="M7 3h1M9 3h1M6 4h1M5 5h1M4 6h1M4 8h1M5 9h1M6 1' +
          '0h1M7 11h1M9 11h1M0 14h4M5 14h11" />'#13#10'<path stroke="#f0f0f0" d="' +
          'M8 3h1M7 4h1M9 4h1M6 5h1M9 5h1M5 6h1M9 6h1M9 7h1M5 8h1M9 8h1M6 9' +
          'h1M9 9h1M7 10h1M9 10h1M8 11h1" />'#13#10'<path stroke="#555555" d="M8 ' +
          '4h1" />'#13#10'<path stroke="#262626" d="M7 5h1" />'#13#10'<path stroke="#76' +
          '7676" d="M8 5h1" />'#13#10'<path stroke="#232323" d="M6 6h1" />'#13#10'<path' +
          ' stroke="#4a4a4a" d="M7 6h1" />'#13#10'<path stroke="#9b9b9b" d="M8 6h' +
          '1" />'#13#10'<path stroke="#f1f1f1" d="M4 7h1" />'#13#10'<path stroke="#3030' +
          '30" d="M5 7h1" />'#13#10'<path stroke="#373737" d="M6 7h1" />'#13#10'<path s' +
          'troke="#787878" d="M7 7h1" />'#13#10'<path stroke="#acacac" d="M8 7h1"' +
          ' />'#13#10'<path stroke="#5b5b5b" d="M6 8h1" />'#13#10'<path stroke="#8a8a8a' +
          '" d="M7 8h1" />'#13#10'<path stroke="#b5b5b5" d="M8 8h1" />'#13#10'<path str' +
          'oke="#848484" d="M7 9h1" />'#13#10'<path stroke="#bdbdbd" d="M8 9h1" /' +
          '>'#13#10'<path stroke="#b4b4b4" d="M8 10h1" />'#13#10'<path stroke="#ebebeb"' +
          ' d="M0 13h13" />'#13#10'<path stroke="#eaeaea" d="M13 13h3" />'#13#10'<path ' +
          'stroke="#eeeeee" d="M4 14h1" />'#13#10'</svg>'
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e2e2e2" d="M0 0h1M0' +
          ' 1h1M0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11' +
          'h1M0 12h1M0 13h1M0 14h1M0 15h1" />'#13#10'<path stroke="#e6e6e6" d="M1' +
          ' 0h1M1 1h1M1 2h1M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1M1 10h' +
          '1M1 11h1M1 12h1M1 13h1M1 14h1M1 15h1" />'#13#10'<path stroke="#ececec"' +
          ' d="M2 0h1M2 1h1M2 2h1M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1' +
          'M2 10h1M2 11h1M2 12h1M2 13h1M2 14h1M2 15h1" />'#13#10'<path stroke="#e' +
          'eeeee" d="M3 0h4M8 0h1M3 1h4M8 1h1M3 2h4M8 2h1M3 3h4M8 3h1M3 4h4' +
          'M8 4h1M3 5h4M8 5h1M3 6h4M3 7h3M3 8h2M3 9h1M3 10h1M3 11h1M3 12h4M' +
          '8 12h1M3 13h4M8 13h1M3 14h4M8 14h1M3 15h4M8 15h1" />'#13#10'<path stro' +
          'ke="#efefef" d="M7 0h1M9 0h4M7 1h1M9 1h4M7 2h1M9 2h4M7 3h1M9 3h4' +
          'M7 4h1M9 4h4M7 5h1M9 5h4M10 6h3M11 7h2M12 8h1M7 12h1M9 12h4M7 13' +
          'h1M9 13h4M7 14h1M9 14h4M7 15h1M9 15h4" />'#13#10'<path stroke="#f0f0f0' +
          '" d="M13 0h1M13 1h1M13 2h1M13 3h1M13 4h1M13 5h1M13 6h1M13 7h1M13' +
          ' 8h1M13 9h1M13 10h1M13 11h1M13 12h1M13 13h1M13 14h1M13 15h1" />'#13 +
          #10'<path stroke="#f1f1f1" d="M14 0h1M14 1h1M14 2h1M14 3h1M14 4h1M1' +
          '4 5h1M14 6h1M6 7h1M14 7h1M5 8h1M14 8h1M4 9h1M14 9h1M14 10h1M4 11' +
          'h1M14 11h1M14 12h1M14 13h1M14 14h1M14 15h1" />'#13#10'<path stroke="#e' +
          '9e9e9" d="M15 0h1M15 1h1M15 2h1M15 3h1M15 4h1M15 5h1M15 6h1M15 7' +
          'h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h1M15 14h1M15 15h1' +
          '" />'#13#10'<path stroke="#f2f2f2" d="M7 6h1M9 6h1M10 7h1M11 8h1M12 9h' +
          '1M12 11h1" />'#13#10'<path stroke="#f4f4f4" d="M8 6h1M7 7h1M9 7h1M6 8h' +
          '1M10 8h1M5 9h1M11 9h1M4 10h1M12 10h1M5 11h7" />'#13#10'<path stroke="#' +
          '303030" d="M8 7h1" />'#13#10'<path stroke="#232323" d="M7 8h1" />'#13#10'<pa' +
          'th stroke="#373737" d="M8 8h1" />'#13#10'<path stroke="#5b5b5b" d="M9 ' +
          '8h1" />'#13#10'<path stroke="#262626" d="M6 9h1" />'#13#10'<path stroke="#4a' +
          '4a4a" d="M7 9h1" />'#13#10'<path stroke="#787878" d="M8 9h1" />'#13#10'<path' +
          ' stroke="#8a8a8a" d="M9 9h1" />'#13#10'<path stroke="#848484" d="M10 9' +
          'h1" />'#13#10'<path stroke="#555555" d="M5 10h1" />'#13#10'<path stroke="#76' +
          '7676" d="M6 10h1" />'#13#10'<path stroke="#9b9b9b" d="M7 10h1" />'#13#10'<pa' +
          'th stroke="#acacac" d="M8 10h1" />'#13#10'<path stroke="#b5b5b5" d="M9' +
          ' 10h1" />'#13#10'<path stroke="#bdbdbd" d="M10 10h1" />'#13#10'<path stroke=' +
          '"#b4b4b4" d="M11 10h1" />'#13#10'</svg>'
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e2e2e2" d="M0 0h1M0' +
          ' 1h1M0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11' +
          'h1M0 12h1M0 13h1M0 14h1M0 15h1" />'#13#10'<path stroke="#e6e6e6" d="M1' +
          ' 0h1M1 1h1M1 2h1M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1" />'#13#10'<path ' +
          'stroke="#ececec" d="M2 0h1M2 1h1M2 2h1M2 3h1M2 4h1M2 5h1M2 6h1M2' +
          ' 7h1M2 8h1" />'#13#10'<path stroke="#eeeeee" d="M3 0h4M8 0h1M3 1h4M8 1' +
          'h1M3 2h4M8 2h1M3 3h4M8 3h1M3 4h4M8 4h1M3 5h1M3 6h1M3 7h1M3 8h2M2' +
          ' 10h1" />'#13#10'<path stroke="#efefef" d="M7 0h1M9 0h4M7 1h1M9 1h4M7 ' +
          '2h1M9 2h4M7 3h1M9 3h4M7 4h1M9 4h4M12 8h1M3 9h3M11 9h2M1 12h1" />' +
          #13#10'<path stroke="#f0f0f0" d="M13 0h1M13 1h1M13 2h1M13 3h1M13 4h1M' +
          '13 5h1M13 6h1M13 7h1M13 8h1M13 9h1M3 10h4M10 10h3M2 11h1" />'#13#10'<p' +
          'ath stroke="#f1f1f1" d="M14 0h1M14 1h1M14 2h1M14 3h1M14 4h1M4 5h' +
          '1M14 5h1M14 6h1M4 7h1M14 7h1M5 8h1M14 8h1M14 9h1M13 10h1M3 11h10' +
          'M2 12h5M8 12h1M1 13h1" />'#13#10'<path stroke="#e9e9e9" d="M15 0h1M15 ' +
          '1h1M15 2h1M15 3h1M15 4h1M15 5h1M15 6h1M15 7h1M15 8h1M15 9h1M15 1' +
          '0h1M15 11h1M15 12h1M15 13h1M15 14h1M1 15h15" />'#13#10'<path stroke="#' +
          'f4f4f4" d="M5 5h7M4 6h1M12 6h1M5 7h1M11 7h1M6 8h1M10 8h1M7 9h1M9' +
          ' 9h1" />'#13#10'<path stroke="#f2f2f2" d="M12 5h1M12 7h1M11 8h1M6 9h1M' +
          '10 9h1M14 10h1M13 11h2M7 12h1M9 12h6M2 13h5M8 13h1" />'#13#10'<path st' +
          'roke="#333333" d="M5 6h1" />'#13#10'<path stroke="#292929" d="M6 6h2" ' +
          '/>'#13#10'<path stroke="#2c2c2c" d="M8 6h1" />'#13#10'<path stroke="#3e3e3e"' +
          ' d="M9 6h1" />'#13#10'<path stroke="#656565" d="M10 6h1" />'#13#10'<path str' +
          'oke="#8a8a8a" d="M11 6h1" />'#13#10'<path stroke="#555555" d="M6 7h1" ' +
          '/>'#13#10'<path stroke="#5f5f5f" d="M7 7h1" />'#13#10'<path stroke="#787878"' +
          ' d="M8 7h1" />'#13#10'<path stroke="#a3a3a3" d="M9 7h1" />'#13#10'<path stro' +
          'ke="#bdbdbd" d="M10 7h1" />'#13#10'<path stroke="#747474" d="M7 8h1" /' +
          '>'#13#10'<path stroke="#a7a7a7" d="M8 8h1" />'#13#10'<path stroke="#cacaca" ' +
          'd="M9 8h1" />'#13#10'<path stroke="#e7e7e7" d="M1 9h1" />'#13#10'<path strok' +
          'e="#ededed" d="M2 9h1M1 11h1" />'#13#10'<path stroke="#aaaaaa" d="M8 9' +
          'h1" />'#13#10'<path stroke="#eaeaea" d="M1 10h1" />'#13#10'<path stroke="#f3' +
          'f3f3" d="M7 10h1M9 10h1M7 13h1M9 13h6M1 14h14" />'#13#10'<path stroke=' +
          '"#f5f5f5" d="M8 10h1" />'#13#10'</svg>'
      end
      item
        IconName = '06'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#c1c1c1" d="M0 0h1M1' +
          '5 0h1" />'#13#10'<path stroke="#a3a3a3" d="M1 0h1M14 0h1M0 1h1M15 1h1"' +
          ' />'#13#10'<path stroke="#929292" d="M2 0h2M8 0h6" />'#13#10'<path stroke="#' +
          '939393" d="M4 0h4" />'#13#10'<path stroke="#e3e3e3" d="M1 1h1" />'#13#10'<pa' +
          'th stroke="#f3f3f3" d="M2 1h1" />'#13#10'<path stroke="#f1f1f2" d="M3 ' +
          '1h11M3 2h5M2 3h2M6 3h1M9 3h1M12 3h1M3 4h1M6 4h1M9 4h1M12 4h1M1 5' +
          'h1M1 6h1" />'#13#10'<path stroke="#e1e1e1" d="M14 1h1" />'#13#10'<path strok' +
          'e="#949494" d="M0 2h1" />'#13#10'<path stroke="#f3f3f4" d="M1 2h1M1 3h' +
          '1M4 3h2M7 3h2M10 3h2" />'#13#10'<path stroke="#f2f2f3" d="M2 2h1M1 4h1' +
          '" />'#13#10'<path stroke="#f0f0f1" d="M8 2h5M3 5h1M6 5h1M9 5h1M12 5h1"' +
          ' />'#13#10'<path stroke="#efeff0" d="M13 2h1M2 4h1" />'#13#10'<path stroke="' +
          '#ededee" d="M14 2h1M13 3h1" />'#13#10'<path stroke="#979797" d="M15 2h' +
          '1" />'#13#10'<path stroke="#959595" d="M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M' +
          '0 8h1M0 9h1M0 10h1M0 11h1M0 12h1M0 13h1" />'#13#10'<path stroke="#ebeb' +
          'ec" d="M14 3h1" />'#13#10'<path stroke="#999999" d="M15 3h1M15 4h1M15 ' +
          '5h1M15 6h1M15 7h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h1"' +
          ' />'#13#10'<path stroke="#3d3c3c" d="M4 4h1M7 4h1M10 4h1" />'#13#10'<path st' +
          'roke="#656464" d="M5 4h1M8 4h1" />'#13#10'<path stroke="#807f7f" d="M1' +
          '1 4h1" />'#13#10'<path stroke="#e9e9eb" d="M13 4h1" />'#13#10'<path stroke="' +
          '#e9e9e9" d="M14 4h1M14 5h1" />'#13#10'<path stroke="#edeeef" d="M2 5h1' +
          '" />'#13#10'<path stroke="#626262" d="M4 5h1M7 5h1M10 5h1" />'#13#10'<path s' +
          'troke="#9f9e9e" d="M5 5h1M8 5h1" />'#13#10'<path stroke="#aeadad" d="M' +
          '11 5h1M4 11h1M7 11h1M10 11h1" />'#13#10'<path stroke="#e7e8e9" d="M13 ' +
          '5h1M1 7h1" />'#13#10'<path stroke="#ececed" d="M2 6h1" />'#13#10'<path strok' +
          'e="#efefef" d="M3 6h1M6 6h1M9 6h1M12 6h1" />'#13#10'<path stroke="#777' +
          '676" d="M4 6h1M7 6h1M10 6h1M4 7h1M7 7h1M10 7h1M4 8h1M7 8h1M10 8h' +
          '1M4 9h1M7 9h1M10 9h1" />'#13#10'<path stroke="#afafaf" d="M5 6h1M8 6h1' +
          'M5 7h1M8 7h1M5 8h1M8 8h1M5 9h1M8 9h1" />'#13#10'<path stroke="#bcbcbc"' +
          ' d="M11 6h1M11 7h1M11 8h1M11 9h1" />'#13#10'<path stroke="#e5e5e6" d="' +
          'M13 6h1M1 11h1" />'#13#10'<path stroke="#e7e7e8" d="M14 6h1M1 8h1M1 9h' +
          '1" />'#13#10'<path stroke="#dfdfe0" d="M2 7h1" />'#13#10'<path stroke="#e3e3' +
          'e4" d="M3 7h1M6 7h1M9 7h1M12 7h1" />'#13#10'<path stroke="#d2d2d4" d="' +
          'M13 7h1M14 11h1" />'#13#10'<path stroke="#d7d8d8" d="M14 7h1" />'#13#10'<pat' +
          'h stroke="#dcdcde" d="M2 8h1M2 9h1" />'#13#10'<path stroke="#e0e0e2" d' +
          '="M3 8h1M6 8h1M9 8h1M12 8h1M3 9h1M6 9h1M9 9h1M12 9h1" />'#13#10'<path ' +
          'stroke="#ceced1" d="M13 8h1M13 9h1M2 14h1" />'#13#10'<path stroke="#d5' +
          'd5d7" d="M14 8h1M14 9h1" />'#13#10'<path stroke="#e6e6e7" d="M1 10h1" ' +
          '/>'#13#10'<path stroke="#d8d8db" d="M2 10h1" />'#13#10'<path stroke="#dddddf' +
          '" d="M3 10h1M6 10h1M9 10h1M12 10h1" />'#13#10'<path stroke="#8f8e8e" d' +
          '="M4 10h1M7 10h1M10 10h1" />'#13#10'<path stroke="#bbbbbb" d="M5 10h1M' +
          '8 10h1" />'#13#10'<path stroke="#c5c5c5" d="M11 10h1" />'#13#10'<path stroke' +
          '="#c9cacd" d="M13 10h1" />'#13#10'<path stroke="#d4d4d5" d="M14 10h1" ' +
          '/>'#13#10'<path stroke="#d5d5d8" d="M2 11h1" />'#13#10'<path stroke="#dbdbdd' +
          '" d="M3 11h1M6 11h1M9 11h1M12 11h1M1 13h1" />'#13#10'<path stroke="#c8' +
          'c8c8" d="M5 11h1M8 11h1" />'#13#10'<path stroke="#cfcfcf" d="M11 11h1"' +
          ' />'#13#10'<path stroke="#c6c6c9" d="M13 11h1" />'#13#10'<path stroke="#e1e2' +
          'e3" d="M1 12h1" />'#13#10'<path stroke="#cccccf" d="M2 12h1" />'#13#10'<path' +
          ' stroke="#cdcdd0" d="M3 12h1M6 12h1M9 12h1M12 12h1" />'#13#10'<path st' +
          'roke="#d6d6da" d="M4 12h2M7 12h2M10 12h2" />'#13#10'<path stroke="#c0c' +
          '0c4" d="M13 12h1" />'#13#10'<path stroke="#d0d1d2" d="M14 12h1" />'#13#10'<p' +
          'ath stroke="#bebec2" d="M2 13h1" />'#13#10'<path stroke="#bcbcbf" d="M' +
          '3 13h1" />'#13#10'<path stroke="#bababe" d="M4 13h9" />'#13#10'<path stroke=' +
          '"#b9b9bd" d="M13 13h1" />'#13#10'<path stroke="#cfcfd0" d="M14 13h1" /' +
          '>'#13#10'<path stroke="#a9a9a9" d="M0 14h1" />'#13#10'<path stroke="#c8c8c9"' +
          ' d="M1 14h1" />'#13#10'<path stroke="#cbcbce" d="M3 14h1" />'#13#10'<path st' +
          'roke="#cacacc" d="M4 14h10" />'#13#10'<path stroke="#c4c4c6" d="M14 14' +
          'h1" />'#13#10'<path stroke="#a0a0a0" d="M15 14h1" />'#13#10'<path stroke="#f' +
          'e00ff" d="M0 15h1" />'#13#10'<path stroke="#acacac" d="M1 15h1" />'#13#10'<p' +
          'ath stroke="#9c9c9c" d="M2 15h12" />'#13#10'<path stroke="#a2a2a2" d="' +
          'M14 15h1" />'#13#10'<path stroke="#b7b7b7" d="M15 15h1" />'#13#10'</svg>'
      end
      item
        IconName = '07'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#c9c9c9" d="M0 0h1" ' +
          '/>'#13#10'<path stroke="#aaaaaa" d="M1 0h1M0 1h1" />'#13#10'<path stroke="#9' +
          '79797" d="M2 0h6M9 0h1" />'#13#10'<path stroke="#989898" d="M8 0h1M10 ' +
          '0h4" />'#13#10'<path stroke="#a8a8a8" d="M14 0h1" />'#13#10'<path stroke="#c' +
          'ccccc" d="M15 0h1" />'#13#10'<path stroke="#e3e3e3" d="M1 1h1M1 14h1" ' +
          '/>'#13#10'<path stroke="#f5f5f5" d="M2 1h3M1 2h2" />'#13#10'<path stroke="#f' +
          '4f4f4" d="M5 1h3M3 2h1M1 3h2M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1' +
          'M1 10h1M1 11h1M1 12h1" />'#13#10'<path stroke="#ebebec" d="M8 1h1M6 3h' +
          '2M5 4h1M5 5h1M5 6h1M5 7h1M5 8h1M5 9h1M5 10h1M5 11h1M4 12h1M3 13h' +
          '1" />'#13#10'<path stroke="#eaeaec" d="M9 1h1" />'#13#10'<path stroke="#e9e9' +
          'ea" d="M10 1h1M5 12h1M6 14h2" />'#13#10'<path stroke="#e8e8e9" d="M11 ' +
          '1h1M4 13h1" />'#13#10'<path stroke="#e5e5e6" d="M12 1h1" />'#13#10'<path str' +
          'oke="#dfdfe0" d="M13 1h1" />'#13#10'<path stroke="#ceced0" d="M14 1h1M' +
          '14 3h1" />'#13#10'<path stroke="#ababab" d="M15 1h1" />'#13#10'<path stroke=' +
          '"#959595" d="M0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 ' +
          '10h1M0 11h1M0 12h1M0 13h1" />'#13#10'<path stroke="#f1f1f2" d="M4 2h1M' +
          '3 3h1M2 12h1" />'#13#10'<path stroke="#efeff0" d="M5 2h1M4 3h1M3 12h1M' +
          '2 13h1M2 14h1" />'#13#10'<path stroke="#eeeeef" d="M6 2h2" />'#13#10'<path s' +
          'troke="#e2e2e4" d="M8 2h1" />'#13#10'<path stroke="#dfdfe1" d="M9 2h1"' +
          ' />'#13#10'<path stroke="#dcdddf" d="M10 2h1" />'#13#10'<path stroke="#d8d8d' +
          'c" d="M11 2h1" />'#13#10'<path stroke="#cfd0d2" d="M12 2h1" />'#13#10'<path ' +
          'stroke="#c1c2c5" d="M13 2h1" />'#13#10'<path stroke="#d1d1d3" d="M14 2' +
          'h1M8 13h1" />'#13#10'<path stroke="#939493" d="M15 2h1M15 3h1M15 4h1M1' +
          '5 5h1M15 6h1M15 7h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h' +
          '1M2 15h12" />'#13#10'<path stroke="#ededee" d="M5 3h1M4 4h1M4 5h1M4 6h' +
          '1M4 7h1M4 8h1M4 9h1M4 10h1M4 11h1M3 14h1" />'#13#10'<path stroke="#dcd' +
          'cdf" d="M8 3h1" />'#13#10'<path stroke="#d7d7db" d="M9 3h1" />'#13#10'<path ' +
          'stroke="#d4d5d7" d="M10 3h1" />'#13#10'<path stroke="#d1d1d4" d="M11 3' +
          'h1M9 12h1" />'#13#10'<path stroke="#c6c7ca" d="M12 3h1" />'#13#10'<path stro' +
          'ke="#bfbfc3" d="M13 3h1" />'#13#10'<path stroke="#f3f3f3" d="M2 4h1M2 ' +
          '5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M1 13h1" />'#13#10'<path stro' +
          'ke="#f0f0f1" d="M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11' +
          'h1" />'#13#10'<path stroke="#e8e8ea" d="M6 4h2M6 5h2M6 6h2M6 7h2M6 8h2' +
          'M6 9h2M6 10h2M6 11h2" />'#13#10'<path stroke="#d7d8db" d="M8 4h1M8 5h1' +
          'M8 6h1M8 7h1M8 8h1M8 9h1M8 10h1M8 11h1M9 14h1" />'#13#10'<path stroke=' +
          '"#d3d3d6" d="M9 4h1M9 5h1M9 6h1M9 7h1M9 8h1M9 9h1M9 10h1M9 11h1"' +
          ' />'#13#10'<path stroke="#cfcfd2" d="M10 4h1M10 5h1M10 6h1M10 7h1M10 8' +
          'h1M10 9h1M10 10h1M10 11h1" />'#13#10'<path stroke="#cbcbcf" d="M11 4h1' +
          'M11 5h1M11 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1" />'#13#10'<path s' +
          'troke="#c5c5c9" d="M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12' +
          ' 10h1M12 11h1" />'#13#10'<path stroke="#bebec2" d="M13 4h1M13 5h1M13 6' +
          'h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1" />'#13#10'<path stroke="#cdcd' +
          'cf" d="M14 4h1M14 5h1M14 6h1M14 7h1M14 8h1M14 9h1M14 10h1M14 11h' +
          '1M14 12h1M9 13h1M14 13h1" />'#13#10'<path stroke="#e7e7e8" d="M6 12h2"' +
          ' />'#13#10'<path stroke="#d5d5d8" d="M8 12h1" />'#13#10'<path stroke="#cdcdd' +
          '0" d="M10 12h1" />'#13#10'<path stroke="#c9c9cc" d="M11 12h1" />'#13#10'<pat' +
          'h stroke="#c3c4c7" d="M12 12h1" />'#13#10'<path stroke="#bcbdc0" d="M1' +
          '3 12h1" />'#13#10'<path stroke="#e6e6e8" d="M5 13h1" />'#13#10'<path stroke=' +
          '"#e4e4e6" d="M6 13h2" />'#13#10'<path stroke="#c9cacc" d="M10 13h1" />' +
          #13#10'<path stroke="#c5c5c8" d="M11 13h1" />'#13#10'<path stroke="#c0c1c4"' +
          ' d="M12 13h1" />'#13#10'<path stroke="#bbbcbf" d="M13 13h1" />'#13#10'<path ' +
          'stroke="#a6a6a6" d="M0 14h1M1 15h1" />'#13#10'<path stroke="#ebebeb" d' +
          '="M4 14h1" />'#13#10'<path stroke="#eaeaeb" d="M5 14h1" />'#13#10'<path stro' +
          'ke="#dcdcdd" d="M8 14h1" />'#13#10'<path stroke="#d7d8da" d="M10 14h1"' +
          ' />'#13#10'<path stroke="#d6d6d7" d="M11 14h1" />'#13#10'<path stroke="#d4d4' +
          'd6" d="M12 14h1" />'#13#10'<path stroke="#d2d2d4" d="M13 14h1" />'#13#10'<pa' +
          'th stroke="#c7c7c9" d="M14 14h1" />'#13#10'<path stroke="#a3a3a3" d="M' +
          '15 14h1M14 15h1" />'#13#10'<path stroke="#fe00ff" d="M0 15h1" />'#13#10'<pat' +
          'h stroke="#b9b9b9" d="M15 15h1" />'#13#10'</svg>'
      end
      item
        IconName = '08'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#798897" d="M0 0h16M' +
          '0 2h1M15 2h1M0 3h1M15 3h1M0 4h1M15 4h1M0 5h1M15 5h1M0 6h1M15 6h1' +
          'M0 7h1M15 7h1M0 8h1M15 8h1M0 9h1M15 9h1M0 10h1M15 10h1M0 11h1M15' +
          ' 11h1M0 12h1M15 12h1M0 13h1M15 13h1M0 14h1M15 14h1M0 15h16" />'#13#10 +
          '<path stroke="#ededed" d="M0 1h1M15 1h1M2 2h13M2 3h13M2 4h5M8 4h' +
          '7M2 5h5M9 5h6M2 6h5M10 6h5M2 7h5M11 7h4M2 8h5M11 8h4M2 9h5M10 9h' +
          '5M2 10h5M9 10h6M2 11h5M8 11h7M2 12h13M2 13h13M2 14h13" />'#13#10'<path' +
          ' stroke="#ffffff" d="M1 1h14M1 2h1M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1' +
          'M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h1M1 14h1" />'#13#10'<path strok' +
          'e="#363636" d="M7 4h1M7 5h2M7 6h3M7 7h4M7 8h4M7 9h3M7 10h2M7 11h' +
          '1" />'#13#10'</svg>'
      end
      item
        IconName = '09'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#798897" d="M0 0h16M' +
          '15 2h1M15 3h1M15 4h1M15 5h1M15 6h1M15 7h1M15 8h1M15 9h1M15 10h1M' +
          '15 11h1M15 12h1M15 13h1M15 14h1M1 15h15" />'#13#10'<path stroke="#ffff' +
          'ff" d="M0 1h15M0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0' +
          ' 10h1M0 11h1M0 12h1M0 13h1M0 14h1" />'#13#10'<path stroke="#ededed" d=' +
          '"M15 1h1M1 2h14M1 3h14M1 4h7M9 4h6M1 5h6M9 5h6M1 6h5M9 6h6M1 7h4' +
          'M9 7h6M1 8h4M9 8h6M1 9h5M9 9h6M1 10h6M9 10h6M1 11h7M9 11h6M1 12h' +
          '14M1 13h14M1 14h14M0 15h1" />'#13#10'<path stroke="#363636" d="M8 4h1M' +
          '7 5h2M6 6h3M5 7h4M5 8h4M6 9h3M7 10h2M8 11h1" />'#13#10'</svg>'
      end
      item
        IconName = '10'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#798897" d="M0 0h1M1' +
          '5 0h1M0 1h1M0 2h1M15 2h1M0 3h1M15 3h1M0 4h1M15 4h1M0 5h1M15 5h1M' +
          '0 6h1M15 6h1M0 7h1M15 7h1M0 8h1M15 8h1M0 9h1M15 9h1M0 10h1M15 10' +
          'h1M0 11h1M15 11h1M0 12h1M15 12h1M0 13h1M15 13h1M0 14h1M15 14h1M0' +
          ' 15h1M2 15h14" />'#13#10'<path stroke="#ffffff" d="M1 0h14M1 1h1M1 2h1' +
          'M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M' +
          '1 13h1M1 14h1" />'#13#10'<path stroke="#ededed" d="M2 1h14M2 2h13M2 3h' +
          '13M2 4h13M2 5h5M9 5h6M2 6h4M10 6h5M2 7h3M11 7h4M2 8h2M12 8h3M2 9' +
          'h13M2 10h13M2 11h13M2 12h13M2 13h13M2 14h13M1 15h1" />'#13#10'<path st' +
          'roke="#363636" d="M7 5h2M6 6h4M5 7h6M4 8h8" />'#13#10'</svg>'
      end
      item
        IconName = '11'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#798897" d="M0 0h1M2' +
          ' 0h14M0 1h1M15 1h1M0 2h1M15 2h1M0 3h1M15 3h1M0 4h1M15 4h1M0 5h1M' +
          '15 5h1M0 6h1M15 6h1M0 7h1M15 7h1M0 8h1M15 8h1M0 9h1M15 9h1M0 10h' +
          '1M15 10h1M0 11h1M15 11h1M0 12h1M15 12h1M0 13h1M15 13h1M0 14h1M15' +
          ' 14h1M0 15h1M2 15h14" />'#13#10'<path stroke="#ededed" d="M1 0h1M2 2h1' +
          '3M2 3h13M2 4h13M2 5h13M2 6h13M2 7h2M12 7h3M2 8h3M11 8h4M2 9h4M10' +
          ' 9h5M2 10h5M9 10h6M2 11h13M2 12h13M2 13h13M2 14h13M1 15h1" />'#13#10'<' +
          'path stroke="#ffffff" d="M1 1h14M1 2h1M1 3h1M1 4h1M1 5h1M1 6h1M1' +
          ' 7h1M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h1M1 14h1" />'#13#10'<path s' +
          'troke="#363636" d="M4 7h8M5 8h6M6 9h4M7 10h2" />'#13#10'</svg>'
      end
      item
        IconName = '12'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#6380bd" d="M0 0h15M' +
          '0 1h1M14 1h1M0 2h1M14 2h1M0 3h1M14 3h1M0 4h1M5 4h1M7 4h1M9 4h1M1' +
          '4 4h1M0 5h1M5 5h1M7 5h1M9 5h1M14 5h1M0 6h1M5 6h1M7 6h1M9 6h1M14 ' +
          '6h1M0 7h1M5 7h1M7 7h1M9 7h1M14 7h1M0 8h1M5 8h1M7 8h1M9 8h1M14 8h' +
          '1M0 9h1M5 9h1M7 9h1M9 9h1M14 9h1M0 10h1M5 10h1M7 10h1M9 10h1M14 ' +
          '10h1M0 11h1M5 11h1M7 11h1M9 11h1M14 11h1M0 12h1M5 12h1M7 12h1M9 ' +
          '12h1M14 12h1M0 13h1M14 13h1M0 14h1M14 14h1" />'#13#10'<path stroke="#7' +
          '98897" d="M15 0h1M0 15h16" />'#13#10'<path stroke="#b6cde4" d="M1 1h13' +
          'M15 1h1M1 2h1M15 2h1M1 3h1M15 3h1M1 4h1M15 4h1M1 5h1M15 5h1M1 6h' +
          '1M15 6h1M1 7h1M15 7h1M1 8h1M15 8h1M1 9h1M15 9h1M1 10h1M15 10h1M1' +
          ' 11h1M15 11h1M1 12h1M15 12h1M1 13h1M15 13h1M1 14h1M15 14h1" />'#13#10 +
          '<path stroke="#dce7f2" d="M2 2h12M2 8h3M11 8h3" />'#13#10'<path stroke' +
          '="#e7eff6" d="M2 3h12M2 7h3M11 7h3" />'#13#10'<path stroke="#f3f6fa" d' +
          '="M2 4h3M6 4h1M8 4h1M10 4h4M2 6h3M11 6h3" />'#13#10'<path stroke="#fff' +
          'fff" d="M2 5h3M6 5h1M8 5h1M10 5h4M6 6h1M8 6h1M10 6h1M6 7h1M8 7h1' +
          'M10 7h1M6 8h1M8 8h1M10 8h1M6 9h1M8 9h1M10 9h1M6 10h1M8 10h1M10 1' +
          '0h1M6 11h1M8 11h1M10 11h1M6 12h1M8 12h1M10 12h1M6 13h1M8 13h1M10' +
          ' 13h1" />'#13#10'<path stroke="#d6e4f0" d="M2 9h3M11 9h3" />'#13#10'<path st' +
          'roke="#d1e0ee" d="M2 10h3M11 10h3" />'#13#10'<path stroke="#cbdcec" d=' +
          '"M2 11h3M11 11h3" />'#13#10'<path stroke="#c6d8ea" d="M2 12h3M11 12h3"' +
          ' />'#13#10'<path stroke="#c1d4e8" d="M2 13h4M7 13h1M9 13h1M11 13h3" />' +
          #13#10'<path stroke="#bbd1e6" d="M2 14h12" />'#13#10'</svg>'
      end
      item
        IconName = '13'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#6380bd" d="M0 0h15M' +
          '0 1h1M0 2h1M0 3h1M0 4h1M0 5h1M4 5h9M0 6h1M0 7h1M4 7h9M0 8h1M0 9h' +
          '1M4 9h9M0 10h1M0 11h1M0 12h1M0 13h1M0 14h15" />'#13#10'<path stroke="#' +
          '798897" d="M15 0h1M15 1h1M15 2h1M15 3h1M15 4h1M15 5h1M15 6h1M15 ' +
          '7h1M15 8h1M15 9h1M15 10h1M15 11h1M15 12h1M15 13h1M15 14h1M0 15h1' +
          'M15 15h1" />'#13#10'<path stroke="#b6cde4" d="M1 1h14M1 2h1M1 3h1M1 4h' +
          '1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1M1 10h1M1 11h1M1 12h1M1 13h1M1 15' +
          'h14" />'#13#10'<path stroke="#dce7f2" d="M2 2h1M8 2h1M2 3h1M8 3h1M2 4h' +
          '1M8 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M8 11h1M2 12h' +
          '1M8 12h1M2 13h1M8 13h1" />'#13#10'<path stroke="#e7eff6" d="M3 2h1M7 2' +
          'h1M3 3h1M7 3h1M3 4h1M7 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M' +
          '3 11h1M7 11h1M3 12h1M7 12h1M3 13h1M7 13h1" />'#13#10'<path stroke="#f3' +
          'f6fa" d="M4 2h1M6 2h1M4 3h1M6 3h1M4 4h1M6 4h1M4 6h1M4 8h1M4 10h1' +
          'M4 11h1M6 11h1M4 12h1M6 12h1M4 13h1M6 13h1" />'#13#10'<path stroke="#f' +
          'fffff" d="M5 2h1M5 3h1M5 4h1M5 6h9M5 8h9M5 10h9M5 11h1M5 12h1M5 ' +
          '13h1" />'#13#10'<path stroke="#d6e4f0" d="M9 2h1M9 3h1M9 4h1M9 11h1M9 ' +
          '12h1M9 13h1" />'#13#10'<path stroke="#d1e0ee" d="M10 2h1M10 3h1M10 4h1' +
          'M10 11h1M10 12h1M10 13h1" />'#13#10'<path stroke="#cbdcec" d="M11 2h1M' +
          '11 3h1M11 4h1M11 11h1M11 12h1M11 13h1" />'#13#10'<path stroke="#c6d8ea' +
          '" d="M12 2h1M12 3h1M12 4h1M12 11h1M12 12h1M12 13h1" />'#13#10'<path st' +
          'roke="#c1d4e8" d="M13 2h1M13 3h1M13 4h1M13 5h1M13 7h1M13 9h1M13 ' +
          '11h1M13 12h1M13 13h1" />'#13#10'<path stroke="#bbd1e6" d="M14 2h1M14 3' +
          'h1M14 4h1M14 5h1M14 6h1M14 7h1M14 8h1M14 9h1M14 10h1M14 11h1M14 ' +
          '12h1M14 13h1" />'#13#10'</svg>'
      end
      item
        IconName = '14'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M0 0h1M4' +
          ' 0h1M8 0h1M2 2h1M6 2h1M0 4h1M4 4h1M8 4h1M2 6h1M6 6h1M0 8h1M4 8h1' +
          'M8 8h1M2 10h1M6 10h1M0 12h1M4 12h1M8 12h1M2 14h1M6 14h1" />'#13#10'<pa' +
          'th stroke="#ededed" d="M1 0h3M5 0h3M9 0h7M0 1h1M2 1h3M6 1h3M10 1' +
          'h6M0 2h2M3 2h3M7 2h9M0 3h3M4 3h3M8 3h8M1 4h3M5 4h3M9 4h7M0 5h1M2' +
          ' 5h3M6 5h3M10 5h6M0 6h2M3 6h3M7 6h9M0 7h3M4 7h3M8 7h8M1 8h3M5 8h' +
          '3M9 8h7M0 9h1M2 9h3M6 9h3M10 9h6M0 10h2M3 10h3M7 10h9M0 11h3M4 1' +
          '1h3M8 11h8M1 12h3M5 12h3M9 12h7M0 13h1M2 13h3M6 13h3M10 13h6M0 1' +
          '4h2M3 14h3M7 14h9M0 15h3M4 15h3M8 15h8" />'#13#10'<path stroke="#79889' +
          '7" d="M1 1h1M5 1h1M9 1h1M3 3h1M7 3h1M1 5h1M5 5h1M9 5h1M3 7h1M7 7' +
          'h1M1 9h1M5 9h1M9 9h1M3 11h1M7 11h1M1 13h1M5 13h1M9 13h1M3 15h1M7' +
          ' 15h1" />'#13#10'</svg>'
      end
      item
        IconName = '15'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ededed" d="M0 0h1M1' +
          '5 0h1M3 2h2M6 2h2M9 2h2M12 2h2M2 3h1M4 3h2M7 3h2M10 3h2M13 3h1M0' +
          ' 15h1M15 15h1" />'#13#10'<path stroke="#798897" d="M1 0h14M0 1h16M0 2h' +
          '2M14 2h1M0 3h2M14 3h1M0 4h2M14 4h2M0 5h16M0 6h2M14 6h2M0 7h2M14 ' +
          '7h2M0 8h2M14 8h2M0 9h2M14 9h1M0 10h2M14 10h2M0 11h2M14 11h2M0 12' +
          'h2M14 12h2M0 13h2M14 13h2M0 14h16M1 15h14" />'#13#10'<path stroke="#b6' +
          'cde4" d="M2 2h1M5 2h1M8 2h1M11 2h1" />'#13#10'<path stroke="#798879" d' +
          '="M15 2h1M15 3h1M15 9h1" />'#13#10'<path stroke="#6380bd" d="M3 3h1M6 ' +
          '3h1M9 3h1M12 3h1" />'#13#10'<path stroke="#fdfdff" d="M2 4h12M2 6h5M8 ' +
          '6h6M2 7h1M4 7h4M9 7h1M11 7h1M2 8h1M4 8h1M6 8h4M11 8h1M13 8h1M2 9' +
          'h1M4 9h2M7 9h2M10 9h2M13 9h1M3 10h4M9 10h4M3 11h10M3 12h3M10 12h' +
          '3M2 13h1M4 13h1M6 13h4M11 13h1M13 13h1" />'#13#10'<path stroke="#fdfff' +
          'a" d="M7 6h1M10 7h1M3 8h1M12 8h1M7 12h2" />'#13#10'<path stroke="#fdfd' +
          'fb" d="M3 7h1M12 7h1M6 9h1M9 9h1" />'#13#10'<path stroke="#fdfffc" d="' +
          'M8 7h1M5 8h1M10 8h1M2 10h1M7 10h2M13 10h1M2 12h1M13 12h1M3 13h1M' +
          '5 13h1M10 13h1M12 13h1" />'#13#10'<path stroke="#fcfdfd" d="M13 7h1M3 ' +
          '9h1M12 9h1M2 11h1M13 11h1M6 12h1M9 12h1" />'#13#10'</svg>'
      end>
    Left = 48
    Top = 160
  end
  object icJavaControls1618: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#6280bd" d="M10 2h1M' +
          '13 3h1" />'#13#10'<path stroke="#6480be" d="M11 2h1" />'#13#10'<path stroke=' +
          '"#637fbf" d="M12 2h1" />'#13#10'<path stroke="#6280b9" d="M13 2h1" />'#13 +
          #10'<path stroke="#6381be" d="M14 2h1" />'#13#10'<path stroke="#6280bb" d' +
          '="M15 2h1M14 3h1" />'#13#10'<path stroke="#637fbb" d="M9 3h1" />'#13#10'<pat' +
          'h stroke="#6480bc" d="M10 3h1" />'#13#10'<path stroke="#637fbd" d="M11' +
          ' 3h1M15 3h1" />'#13#10'<path stroke="#647fbe" d="M12 3h1" />'#13#10'<path st' +
          'roke="#353637" d="M1 4h1M3 4h2M0 5h1M12 5h1M0 7h1M0 8h1M0 11h1M0' +
          ' 12h1M15 13h1M0 14h1" />'#13#10'<path stroke="#353533" d="M2 4h1M0 6h1' +
          '" />'#13#10'<path stroke="#363636" d="M5 4h1M9 5h1M0 9h1M0 10h1M15 10h' +
          '1M2 14h1M6 14h8" />'#13#10'<path stroke="#353537" d="M6 4h1M10 5h1" />' +
          #13#10'<path stroke="#353736" d="M7 4h1M5 14h1" />'#13#10'<path stroke="#36' +
          '3732" d="M8 4h1" />'#13#10'<path stroke="#a0b7c9" d="M9 4h1" />'#13#10'<path' +
          ' stroke="#a3b5cb" d="M10 4h1M13 4h1" />'#13#10'<path stroke="#a2b7cc" ' +
          'd="M11 4h1" />'#13#10'<path stroke="#a0b5c8" d="M12 4h1M14 4h1" />'#13#10'<p' +
          'ath stroke="#373537" d="M15 4h1" />'#13#10'<path stroke="#fdfdff" d="M' +
          '1 5h2M5 5h4M1 6h1M9 6h3M2 7h13M1 9h1M1 10h1M1 11h1" />'#13#10'<path st' +
          'roke="#fffdfb" d="M3 5h1M1 7h1" />'#13#10'<path stroke="#fdfdfd" d="M4' +
          ' 5h1M14 6h1M1 8h1M1 13h1" />'#13#10'<path stroke="#373536" d="M11 5h1M' +
          '15 11h1" />'#13#10'<path stroke="#343734" d="M13 5h2M15 9h1" />'#13#10'<path' +
          ' stroke="#393538" d="M15 5h1" />'#13#10'<path stroke="#eff4f9" d="M2 6' +
          'h7M2 8h11" />'#13#10'<path stroke="#fcfffc" d="M12 6h1M1 12h1" />'#13#10'<pa' +
          'th stroke="#fffcfd" d="M13 6h1" />'#13#10'<path stroke="#363735" d="M1' +
          '5 6h1" />'#13#10'<path stroke="#353535" d="M15 7h1M4 14h1" />'#13#10'<path s' +
          'troke="#eff4f8" d="M13 8h2" />'#13#10'<path stroke="#363634" d="M15 8h' +
          '1M0 13h1M14 14h1" />'#13#10'<path stroke="#d9e5f1" d="M2 9h1M5 9h7M13 ' +
          '9h1" />'#13#10'<path stroke="#d9e5f2" d="M3 9h2M12 9h1M14 9h1" />'#13#10'<pa' +
          'th stroke="#c2d6ea" d="M2 10h9M13 10h2" />'#13#10'<path stroke="#c3d6e' +
          'b" d="M11 10h2" />'#13#10'<path stroke="#b4cce5" d="M2 11h13M2 12h13M2' +
          ' 13h13" />'#13#10'<path stroke="#373634" d="M15 12h1M3 14h1" />'#13#10'<path' +
          ' stroke="#363435" d="M1 14h1" />'#13#10'<path stroke="#373632" d="M15 ' +
          '14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#366597" d="M2 1h10M' +
          '2 2h1M10 2h2M2 3h1M9 3h1M12 3h1M2 4h1M10 4h1M12 4h1M2 5h1M11 5h1' +
          'M13 5h1M2 6h1M12 6h2M2 7h1M13 7h2M2 8h1M14 8h1M2 9h1M14 9h1M2 10' +
          'h1M14 10h1M2 11h1M14 11h1M2 12h1M14 12h1M2 13h1M14 13h1M2 14h1M1' +
          '4 14h1M2 15h1M14 15h1M2 16h13" />'#13#10'<path stroke="#b6cce2" d="M3 ' +
          '2h1M11 4h1M13 14h1" />'#13#10'<path stroke="#b4cde4" d="M4 2h1M6 2h1M3' +
          ' 12h1M3 14h1M4 15h1M6 15h1" />'#13#10'<path stroke="#b5cee5" d="M5 2h1' +
          'M3 3h1M3 4h1M3 5h1M3 13h1M13 13h1M5 15h1" />'#13#10'<path stroke="#b7c' +
          'ee6" d="M7 2h1M13 12h1" />'#13#10'<path stroke="#b6cce5" d="M8 2h1M3 6' +
          'h1M13 9h1M3 10h1M3 11h1M7 15h1" />'#13#10'<path stroke="#b4cce5" d="M9' +
          ' 2h1" />'#13#10'<path stroke="#fdfdff" d="M4 3h1M6 3h3M11 3h1M6 4h2M4 ' +
          '5h2M7 5h4M4 6h1M6 6h4M11 6h1M4 7h1M6 7h1M8 7h3M12 7h1M6 8h2M10 8' +
          'h1M12 8h1M6 9h3M10 9h1M4 10h1M6 10h2M9 10h2M4 11h6M11 11h2M4 12h' +
          '2M7 12h3M6 13h2M10 13h2M4 14h1M6 14h3M10 14h3" />'#13#10'<path stroke=' +
          '"#fdfdfb" d="M5 3h1M9 4h1M4 9h1M5 13h1M9 14h1" />'#13#10'<path stroke=' +
          '"#b4cde2" d="M10 3h1" />'#13#10'<path stroke="#fdfffc" d="M4 4h2M8 4h1' +
          'M6 5h1M10 6h1M5 7h1M11 7h1M4 8h2M9 8h1M11 8h1M5 9h1M9 9h1M11 9h1' +
          'M5 10h1M12 10h1M10 11h1M6 12h1M11 12h1M4 13h1M8 13h2M12 13h1M5 1' +
          '4h1" />'#13#10'<path stroke="#b7cde3" d="M12 5h1M3 15h1M11 15h1" />'#13#10'<' +
          'path stroke="#fcfdfd" d="M5 6h1M7 7h1M8 8h1M12 9h1M12 12h1" />'#13#10 +
          '<path stroke="#b6cce4" d="M3 7h1M13 11h1" />'#13#10'<path stroke="#b6c' +
          'ee7" d="M3 8h1" />'#13#10'<path stroke="#b5cde6" d="M13 8h1M3 9h1M9 15' +
          'h1M13 15h1" />'#13#10'<path stroke="#fafffd" d="M8 10h1M10 12h1" />'#13#10'<' +
          'path stroke="#fcfcfa" d="M11 10h1" />'#13#10'<path stroke="#b5cee3" d=' +
          '"M13 10h1" />'#13#10'<path stroke="#b7cde6" d="M8 15h1" />'#13#10'<path stro' +
          'ke="#b4cee6" d="M10 15h1" />'#13#10'<path stroke="#b7cbe5" d="M12 15h1' +
          '" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#b6cde4" d="M6 0h1M6' +
          ' 1h1M6 2h1M6 3h1M6 4h1M6 5h1M10 9h6M6 16h1M6 17h1" />'#13#10'<path str' +
          'oke="#a1b6ca" d="M5 6h1M8 6h1M4 7h1M9 7h1M4 10h1M9 10h1M5 11h1M8' +
          ' 11h1" />'#13#10'<path stroke="#6380bd" d="M6 6h2M5 7h4M4 8h2M8 8h2M4 ' +
          '9h2M8 9h2M5 10h4M6 11h2M6 12h2M6 13h2M6 14h2M6 15h2" />'#13#10'</svg>'#13 +
          #10
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#b6cde4" d="M6 0h1M6' +
          ' 1h1M6 2h1M6 3h1M6 4h1M6 5h1M10 9h6" />'#13#10'<path stroke="#a1b6ca" ' +
          'd="M5 6h1M8 6h1M4 7h1M9 7h1M4 10h1M9 10h1M5 11h1M8 11h1" />'#13#10'<pa' +
          'th stroke="#6380bd" d="M6 6h2M5 7h4M4 8h2M8 8h2M4 9h2M8 9h2M5 10' +
          'h4M6 11h2M6 12h2M6 13h2M6 14h2M6 15h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#b6cde4" d="M6 0h1M6' +
          ' 1h1M6 2h1M6 3h1M6 4h1M6 5h1M6 6h1M6 7h1M6 8h1M6 9h1M6 10h1M6 11' +
          'h1M6 12h1M6 13h1M6 14h1M6 15h1M6 16h1M6 17h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#b6cde4" d="M6 0h1M6' +
          ' 1h1M6 2h1M6 3h1M6 4h1M6 5h1M6 6h1M6 7h1M6 8h1M6 9h1M6 10h10" />' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = '06'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#b6cde4" d="M6 0h1M6' +
          ' 1h1M6 2h1M6 3h1M6 4h1M6 5h1M6 6h1M6 7h1M6 8h1M6 9h10M6 10h1M6 1' +
          '1h1M6 12h1M6 13h1M6 14h1M6 15h1M6 16h1M6 17h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '07'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#757575" d="M4 6h1M1' +
          '1 6h1M6 9h1M9 9h1" />'#13#10'<path stroke="#363636" d="M5 6h6M6 7h4M6 ' +
          '8h4M7 9h2" />'#13#10'<path stroke="#eeeeee" d="M4 7h1M11 7h1M6 10h1M9 ' +
          '10h1" />'#13#10'<path stroke="#444444" d="M5 7h1M10 7h1M7 10h2" />'#13#10'<p' +
          'ath stroke="#bababa" d="M5 8h1M10 8h1M7 11h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '08'
        SVGText = 
          '<svg viewBox="0 -0.5 16 18">'#13#10'<path stroke="#757575" d="M6 5h1M9' +
          ' 7h1M9 10h1M6 12h1" />'#13#10'<path stroke="#eeeeee" d="M7 5h1M10 7h1M' +
          '10 10h1M7 12h1" />'#13#10'<path stroke="#363636" d="M6 6h1M6 7h3M6 8h4' +
          'M6 9h4M6 10h3M6 11h1" />'#13#10'<path stroke="#444444" d="M7 6h1M10 8h' +
          '1M10 9h1M7 11h1" />'#13#10'<path stroke="#bababa" d="M8 6h1M11 8h1M11 ' +
          '9h1M8 11h1" />'#13#10'</svg>'#13#10
      end>
    Left = 48
    Top = 232
  end
  object icJavaControls21616: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e8e8e8" d="M4 0h1M1' +
          '1 0h1M0 4h1M15 4h1M1 6h2M13 6h2" />'#13#10'<path stroke="#cecece" d="M' +
          '5 0h1M10 0h1M3 1h1M12 1h1M1 3h1M14 3h1M0 5h1M15 5h1" />'#13#10'<path s' +
          'troke="#bebebe" d="M6 0h1M9 0h1M0 6h1M15 6h1" />'#13#10'<path stroke="' +
          '#b3b3b3" d="M7 0h2M0 7h1M15 7h1M0 8h1M15 8h1M7 15h2" />'#13#10'<path s' +
          'troke="#f0f0f0" d="M2 1h1M13 1h1M1 2h1M6 2h1M9 2h1M14 2h1" />'#13#10'<' +
          'path stroke="#b9b9b9" d="M4 1h1M11 1h1M1 4h1M14 4h1" />'#13#10'<path s' +
          'troke="#dad9da" d="M5 1h1M10 1h1M6 14h1M9 14h1" />'#13#10'<path stroke' +
          '="#efefef" d="M6 1h1M9 1h1M4 3h1M11 3h1M2 5h1M13 5h1M1 7h1M14 7h' +
          '1" />'#13#10'<path stroke="#fafafa" d="M7 1h2" />'#13#10'<path stroke="#c2c2' +
          'c2" d="M2 2h1M13 2h1" />'#13#10'<path stroke="#cccccc" d="M3 2h1M12 2h' +
          '1M5 14h1M10 14h1" />'#13#10'<path stroke="#f7f7f7" d="M4 2h1M11 2h1M3 ' +
          '15h1M12 15h1" />'#13#10'<path stroke="#f5f5f5" d="M5 2h1M10 2h1M2 14h1' +
          'M13 14h1" />'#13#10'<path stroke="#ededed" d="M7 2h2M5 3h6M4 15h1M11 1' +
          '5h1" />'#13#10'<path stroke="#cbcbcb" d="M2 3h1M13 3h1" />'#13#10'<path stro' +
          'ke="#f8f8f8" d="M3 3h1M12 3h1" />'#13#10'<path stroke="#f3f3f3" d="M2 ' +
          '4h1M13 4h1" />'#13#10'<path stroke="#ececec" d="M3 4h1M12 4h1M1 8h1M14' +
          ' 8h1" />'#13#10'<path stroke="#ebebeb" d="M4 4h8" />'#13#10'<path stroke="#d' +
          '5d5d5" d="M1 5h1M14 5h1" />'#13#10'<path stroke="#e9e9e9" d="M3 5h10" ' +
          '/>'#13#10'<path stroke="#e7e7e7" d="M3 6h10" />'#13#10'<path stroke="#e4e4e4' +
          '" d="M2 7h1M13 7h1" />'#13#10'<path stroke="#e5e5e5" d="M3 7h10M2 11h1' +
          'M13 11h1" />'#13#10'<path stroke="#e2e2e2" d="M2 8h1M13 8h1M3 9h10" />' +
          #13#10'<path stroke="#e3e3e3" d="M3 8h10M1 9h2M13 9h2" />'#13#10'<path stro' +
          'ke="#bdbdbd" d="M0 9h1M15 9h1M6 15h1M9 15h1" />'#13#10'<path stroke="#' +
          'cfcfcf" d="M0 10h1M15 10h1M1 12h1M14 12h1M3 14h1M12 14h1M5 15h1M' +
          '10 15h1" />'#13#10'<path stroke="#d0d0d0" d="M1 10h1M14 10h1" />'#13#10'<pat' +
          'h stroke="#e6e6e6" d="M2 10h1M13 10h1M3 12h1M12 12h1" />'#13#10'<path ' +
          'stroke="#e0e0e0" d="M3 10h10" />'#13#10'<path stroke="#eaeaea" d="M0 1' +
          '1h1M15 11h1" />'#13#10'<path stroke="#b7b7b7" d="M1 11h1M14 11h1M4 14h' +
          '1M11 14h1" />'#13#10'<path stroke="#dfdfdf" d="M3 11h1M12 11h1M5 13h1M' +
          '10 13h1" />'#13#10'<path stroke="#dedede" d="M4 11h8M4 12h1M11 12h1" /' +
          '>'#13#10'<path stroke="#f4f4f4" d="M0 12h1M15 12h1M1 13h1M14 13h1" />'#13 +
          #10'<path stroke="#c5c5c5" d="M2 12h1M13 12h1" />'#13#10'<path stroke="#d' +
          'cdcdc" d="M5 12h6" />'#13#10'<path stroke="#c3c3c3" d="M2 13h1M13 13h1' +
          '" />'#13#10'<path stroke="#c4c4c4" d="M3 13h1M12 13h1" />'#13#10'<path strok' +
          'e="#e1e1e1" d="M4 13h1M11 13h1M7 14h2" />'#13#10'<path stroke="#dbdbdb' +
          '" d="M6 13h1M9 13h1" />'#13#10'<path stroke="#d8d8d8" d="M7 13h2" />'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e8e8e8" d="M4 0h1M1' +
          '1 0h1M0 4h1M15 4h1M1 6h2M13 6h2" />'#13#10'<path stroke="#cecece" d="M' +
          '5 0h1M10 0h1M3 1h1M12 1h1M1 3h1M14 3h1M0 5h1M15 5h1" />'#13#10'<path s' +
          'troke="#bebebe" d="M6 0h1M9 0h1M0 6h1M15 6h1" />'#13#10'<path stroke="' +
          '#b3b3b3" d="M7 0h2M0 7h1M15 7h1M0 8h1M15 8h1M7 15h2" />'#13#10'<path s' +
          'troke="#f0f0f0" d="M2 1h1M13 1h1M1 2h1M6 2h1M9 2h1M14 2h1" />'#13#10'<' +
          'path stroke="#b9b9b9" d="M4 1h1M11 1h1M1 4h1M14 4h1" />'#13#10'<path s' +
          'troke="#dad9da" d="M5 1h1M10 1h1M6 14h1M9 14h1" />'#13#10'<path stroke' +
          '="#efefef" d="M6 1h1M9 1h1M4 3h1M11 3h1M2 5h1M13 5h1M1 7h1M14 7h' +
          '1" />'#13#10'<path stroke="#fafafa" d="M7 1h2" />'#13#10'<path stroke="#c2c2' +
          'c2" d="M2 2h1M13 2h1" />'#13#10'<path stroke="#cccccc" d="M3 2h1M12 2h' +
          '1M5 14h1M10 14h1" />'#13#10'<path stroke="#f7f7f7" d="M4 2h1M11 2h1M3 ' +
          '15h1M12 15h1" />'#13#10'<path stroke="#f5f5f5" d="M5 2h1M10 2h1M2 14h1' +
          'M13 14h1" />'#13#10'<path stroke="#ededed" d="M7 2h2M5 3h6M4 15h1M11 1' +
          '5h1" />'#13#10'<path stroke="#cbcbcb" d="M2 3h1M13 3h1" />'#13#10'<path stro' +
          'ke="#f8f8f8" d="M3 3h1M12 3h1" />'#13#10'<path stroke="#f3f3f3" d="M2 ' +
          '4h1M13 4h1M6 12h1M9 12h1" />'#13#10'<path stroke="#ececec" d="M3 4h1M1' +
          '2 4h1M1 8h1M14 8h1" />'#13#10'<path stroke="#ebebeb" d="M4 4h1M11 4h1"' +
          ' />'#13#10'<path stroke="#d1d1d1" d="M5 4h1M10 4h1" />'#13#10'<path stroke="' +
          '#818181" d="M6 4h1M9 4h1M4 6h1M11 6h1" />'#13#10'<path stroke="#5d5d5d' +
          '" d="M7 4h2" />'#13#10'<path stroke="#d5d5d5" d="M1 5h1M14 5h1" />'#13#10'<p' +
          'ath stroke="#e9e9e9" d="M3 5h1M12 5h1" />'#13#10'<path stroke="#d0d0d0' +
          '" d="M4 5h1M11 5h1M1 10h1M14 10h1" />'#13#10'<path stroke="#5e5e5e" d=' +
          '"M5 5h1M10 5h1M4 7h1M11 7h1M4 8h1M11 8h1M5 10h1M10 10h1M7 11h2" ' +
          '/>'#13#10'<path stroke="#575757" d="M6 5h4M5 6h6M5 7h6M5 8h6M5 9h6M6 1' +
          '0h4" />'#13#10'<path stroke="#e7e7e7" d="M3 6h1M12 6h1" />'#13#10'<path stro' +
          'ke="#e4e4e4" d="M2 7h1M13 7h1" />'#13#10'<path stroke="#e5e5e5" d="M3 ' +
          '7h1M12 7h1M2 11h1M13 11h1" />'#13#10'<path stroke="#e2e2e2" d="M2 8h1M' +
          '13 8h1M3 9h1M12 9h1M5 11h1M10 11h1" />'#13#10'<path stroke="#e3e3e3" d' +
          '="M3 8h1M12 8h1M1 9h2M13 9h2M4 11h1M11 11h1" />'#13#10'<path stroke="#' +
          'bdbdbd" d="M0 9h1M15 9h1M6 15h1M9 15h1" />'#13#10'<path stroke="#86868' +
          '6" d="M4 9h1M11 9h1" />'#13#10'<path stroke="#cfcfcf" d="M0 10h1M15 10' +
          'h1M1 12h1M14 12h1M3 14h1M12 14h1M5 15h1M10 15h1" />'#13#10'<path strok' +
          'e="#e6e6e6" d="M2 10h1M13 10h1M3 12h1M12 12h1" />'#13#10'<path stroke=' +
          '"#e0e0e0" d="M3 10h1M12 10h1" />'#13#10'<path stroke="#dcdcdc" d="M4 1' +
          '0h1M11 10h1" />'#13#10'<path stroke="#eaeaea" d="M0 11h1M15 11h1" />'#13#10 +
          '<path stroke="#b7b7b7" d="M1 11h1M14 11h1M4 14h1M11 14h1" />'#13#10'<p' +
          'ath stroke="#dfdfdf" d="M3 11h1M12 11h1M5 13h1M10 13h1" />'#13#10'<pat' +
          'h stroke="#878787" d="M6 11h1M9 11h1" />'#13#10'<path stroke="#f4f4f4"' +
          ' d="M0 12h1M15 12h1M1 13h1M14 13h1" />'#13#10'<path stroke="#c5c5c5" d' +
          '="M2 12h1M13 12h1" />'#13#10'<path stroke="#dedede" d="M4 12h1M11 12h1' +
          '" />'#13#10'<path stroke="#e1e1e1" d="M5 12h1M10 12h1M4 13h1M11 13h1M7' +
          ' 14h2" />'#13#10'<path stroke="#fcfcfc" d="M7 12h2" />'#13#10'<path stroke="' +
          '#c3c3c3" d="M2 13h1M13 13h1" />'#13#10'<path stroke="#c4c4c4" d="M3 13' +
          'h1M12 13h1" />'#13#10'<path stroke="#dbdbdb" d="M6 13h1M9 13h1" />'#13#10'<p' +
          'ath stroke="#d8d8d8" d="M7 13h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#b8b8b8" d="M1 0h1M1' +
          '4 0h1M0 1h1M15 1h1M0 14h1M15 14h1M1 15h1M14 15h1" />'#13#10'<path stro' +
          'ke="#b3b3b3" d="M2 0h12M0 2h1M15 2h1M0 3h1M15 3h1M0 4h1M15 4h1M0' +
          ' 5h1M15 5h1M0 6h1M15 6h1M0 7h1M15 7h1M0 8h1M15 8h1M0 9h1M15 9h1M' +
          '0 10h1M15 10h1M0 11h1M15 11h1M0 12h1M15 12h1M0 13h1M15 13h1M2 15' +
          'h12" />'#13#10'<path stroke="#eeeeee" d="M1 1h1M14 1h1M3 2h10M1 8h1M14' +
          ' 8h1" />'#13#10'<path stroke="#fcfcfc" d="M2 1h12" />'#13#10'<path stroke="#' +
          'fbfbfb" d="M1 2h1M14 2h1" />'#13#10'<path stroke="#f0f0f0" d="M2 2h1M1' +
          '3 2h1" />'#13#10'<path stroke="#f9f9f9" d="M1 3h1M14 3h1" />'#13#10'<path st' +
          'roke="#ededed" d="M2 3h12M1 9h1M14 9h1" />'#13#10'<path stroke="#f7f7f' +
          '7" d="M1 4h1M14 4h1" />'#13#10'<path stroke="#ebebeb" d="M2 4h12M1 10h' +
          '1M14 10h1" />'#13#10'<path stroke="#f5f5f5" d="M1 5h1M14 5h1" />'#13#10'<pat' +
          'h stroke="#e9e9e9" d="M2 5h12M1 11h1M14 11h1" />'#13#10'<path stroke="' +
          '#f3f3f3" d="M1 6h1M14 6h1" />'#13#10'<path stroke="#e7e7e7" d="M2 6h12' +
          'M1 12h1M14 12h1" />'#13#10'<path stroke="#f1f1f1" d="M1 7h1M14 7h1" />' +
          #13#10'<path stroke="#e5e5e5" d="M2 7h12M1 13h1M14 13h1" />'#13#10'<path st' +
          'roke="#e3e3e3" d="M2 8h12M2 14h12" />'#13#10'<path stroke="#e2e2e2" d=' +
          '"M2 9h12" />'#13#10'<path stroke="#e0e0e0" d="M2 10h12" />'#13#10'<path stro' +
          'ke="#dedede" d="M2 11h12" />'#13#10'<path stroke="#dcdcdc" d="M2 12h12' +
          '" />'#13#10'<path stroke="#dbdbdb" d="M2 13h1M13 13h1" />'#13#10'<path strok' +
          'e="#dad9da" d="M3 13h10M1 14h1M14 14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#b8b8b8" d="M1 0h1M1' +
          '4 0h1M0 1h1M15 1h1M0 14h1M15 14h1M1 15h1M14 15h1" />'#13#10'<path stro' +
          'ke="#b3b3b3" d="M2 0h12M0 2h1M15 2h1M0 3h1M15 3h1M0 4h1M15 4h1M0' +
          ' 5h1M15 5h1M0 6h1M15 6h1M0 7h1M15 7h1M0 8h1M15 8h1M0 9h1M15 9h1M' +
          '0 10h1M15 10h1M0 11h1M15 11h1M0 12h1M15 12h1M0 13h1M15 13h1M2 15' +
          'h12" />'#13#10'<path stroke="#eeeeee" d="M1 1h1M14 1h1M3 2h9M1 8h1M14 ' +
          '8h1" />'#13#10'<path stroke="#fcfcfc" d="M2 1h12" />'#13#10'<path stroke="#f' +
          'bfbfb" d="M1 2h1M14 2h1" />'#13#10'<path stroke="#f0f0f0" d="M2 2h1" /' +
          '>'#13#10'<path stroke="#c8c8c8" d="M12 2h1" />'#13#10'<path stroke="#d6d6d6"' +
          ' d="M13 2h1" />'#13#10'<path stroke="#f9f9f9" d="M1 3h1M14 3h1" />'#13#10'<p' +
          'ath stroke="#ededed" d="M2 3h9M1 9h1M14 9h1M7 13h1" />'#13#10'<path st' +
          'roke="#d1d1d1" d="M11 3h1M10 4h1M9 11h1" />'#13#10'<path stroke="#6464' +
          '64" d="M12 3h1" />'#13#10'<path stroke="#e8e8e8" d="M13 3h1M8 12h1" />' +
          #13#10'<path stroke="#f7f7f7" d="M1 4h1M14 4h1M6 13h1" />'#13#10'<path stro' +
          'ke="#ebebeb" d="M2 4h8M13 4h1M10 9h1M1 10h1M14 10h1" />'#13#10'<path s' +
          'troke="#6a6a6a" d="M11 4h1" />'#13#10'<path stroke="#9e9e9e" d="M12 4h' +
          '1" />'#13#10'<path stroke="#f5f5f5" d="M1 5h1M14 5h1" />'#13#10'<path stroke' +
          '="#e9e9e9" d="M2 5h8M13 5h1M12 6h1M1 11h1M14 11h1" />'#13#10'<path str' +
          'oke="#888888" d="M10 5h1" />'#13#10'<path stroke="#5e5e5e" d="M11 5h1M' +
          '9 7h1" />'#13#10'<path stroke="#e5e5e5" d="M12 5h1M2 7h1M5 7h3M12 7h2M' +
          '1 13h1M14 13h1" />'#13#10'<path stroke="#f3f3f3" d="M1 6h1M14 6h1" />'#13 +
          #10'<path stroke="#e7e7e7" d="M2 6h1M5 6h4M13 6h1M4 11h1M1 12h1M14 ' +
          '12h1" />'#13#10'<path stroke="#d5d5d5" d="M3 6h1" />'#13#10'<path stroke="#e' +
          '4e4e4" d="M4 6h1M5 12h1" />'#13#10'<path stroke="#aaaaaa" d="M9 6h1" /' +
          '>'#13#10'<path stroke="#575757" d="M10 6h1M4 8h1M9 8h1M5 9h1M8 9h1M5 1' +
          '0h3M6 11h2" />'#13#10'<path stroke="#a7a7a7" d="M11 6h1" />'#13#10'<path str' +
          'oke="#f1f1f1" d="M1 7h1M14 7h1" />'#13#10'<path stroke="#a2a2a2" d="M3' +
          ' 7h1" />'#13#10'<path stroke="#838383" d="M4 7h1" />'#13#10'<path stroke="#c' +
          'bcbcb" d="M8 7h1" />'#13#10'<path stroke="#686868" d="M10 7h1" />'#13#10'<pa' +
          'th stroke="#ececec" d="M11 7h1" />'#13#10'<path stroke="#e3e3e3" d="M2' +
          ' 8h1M6 8h1M11 8h3M9 10h1M2 14h12" />'#13#10'<path stroke="#d4d4d4" d="' +
          'M3 8h1" />'#13#10'<path stroke="#929292" d="M5 8h1" />'#13#10'<path stroke="' +
          '#dcdcdc" d="M7 8h1M2 12h3M9 12h5" />'#13#10'<path stroke="#6f6f6f" d="' +
          'M8 8h1" />'#13#10'<path stroke="#c0c0c0" d="M10 8h1" />'#13#10'<path stroke=' +
          '"#e2e2e2" d="M2 9h1M11 9h3" />'#13#10'<path stroke="#e6e6e6" d="M3 9h1' +
          '" />'#13#10'<path stroke="#7a7a7a" d="M4 9h1" />'#13#10'<path stroke="#91919' +
          '1" d="M6 9h1" />'#13#10'<path stroke="#848484" d="M7 9h1" />'#13#10'<path st' +
          'roke="#878787" d="M9 9h1M5 11h1" />'#13#10'<path stroke="#e0e0e0" d="M' +
          '2 10h2M10 10h4" />'#13#10'<path stroke="#c6c6c6" d="M4 10h1" />'#13#10'<path' +
          ' stroke="#616161" d="M8 10h1" />'#13#10'<path stroke="#dedede" d="M2 1' +
          '1h2M10 11h4" />'#13#10'<path stroke="#bfbfbf" d="M8 11h1" />'#13#10'<path st' +
          'roke="#727272" d="M6 12h1" />'#13#10'<path stroke="#a1a1a1" d="M7 12h1' +
          '" />'#13#10'<path stroke="#dbdbdb" d="M2 13h1M13 13h1" />'#13#10'<path strok' +
          'e="#dad9da" d="M3 13h2M8 13h5M1 14h1M14 14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#999999" d="M4 6h1" ' +
          '/>'#13#10'<path stroke="#575757" d="M5 6h6M6 7h4M7 8h2" />'#13#10'<path stro' +
          'ke="#aeaeae" d="M11 6h1" />'#13#10'<path stroke="#9c9c9c" d="M5 7h1" /' +
          '>'#13#10'<path stroke="#afafaf" d="M10 7h1" />'#13#10'<path stroke="#9d9d9d"' +
          ' d="M6 8h1" />'#13#10'<path stroke="#b1b1b1" d="M9 8h1" />'#13#10'<path stro' +
          'ke="#9e9e9e" d="M7 9h1" />'#13#10'<path stroke="#b2b2b2" d="M8 9h1" />' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#575757" d="M3 3h9M3' +
          ' 4h9M3 5h9M3 6h1M5 6h1M7 6h1M9 6h1M11 6h1M3 7h9M3 8h1M5 8h1M7 8h' +
          '1M9 8h1M11 8h1M3 9h9M3 10h1M5 10h1M7 10h1M9 10h1M11 10h1M3 11h9"' +
          ' />'#13#10'<path stroke="#ffffff" d="M4 6h1M6 6h1M8 6h1M10 6h1M4 8h1M6' +
          ' 8h1M8 8h1M10 8h1M4 10h1M6 10h1M8 10h1M10 10h1M3 12h9" />'#13#10'</svg' +
          '>'#13#10
      end>
    Left = 48
    Top = 296
  end
  object icPagination: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Pagination'
        SVGText = 
          '<svg viewBox="0 -0.5 125 45">'#13#10'<path stroke="#f3f3f3" d="M1 1h12' +
          '3M1 2h123M1 3h123M1 4h123M1 5h13M28 5h2M50 5h2M72 5h2M94 5h2M111' +
          ' 5h13M1 6h12M28 6h2M50 6h2M72 6h2M94 6h2M111 6h13M1 7h12M14 7h1M' +
          '28 7h2M50 7h2M72 7h2M94 7h2M111 7h13M1 8h12M28 8h2M50 8h2M72 8h2' +
          'M94 8h2M111 8h13M1 9h12M28 9h2M50 9h2M72 9h2M94 9h2M111 9h13M1 1' +
          '0h12M28 10h2M50 10h2M72 10h2M94 10h2M111 10h13M1 11h12M14 11h1M2' +
          '6 11h1M28 11h2M50 11h2M72 11h2M94 11h2M111 11h13M1 12h12M14 12h1' +
          'M26 12h1M28 12h2M50 12h2M53 12h1M70 12h1M72 12h2M75 12h1M92 12h1' +
          'M94 12h2M97 12h1M109 12h1M111 12h13M1 13h12M28 13h2M50 13h2M72 1' +
          '3h2M94 13h2M111 13h13M1 14h12M28 14h2M50 14h2M72 14h2M94 14h2M11' +
          '1 14h13M1 15h12M28 15h2M50 15h2M72 15h2M94 15h2M111 15h13M1 16h1' +
          '2M28 16h2M50 16h2M72 16h2M94 16h2M111 16h13M1 17h12M28 17h2M50 1' +
          '7h2M72 17h2M94 17h2M111 17h13M1 18h12M20 18h1M28 18h2M50 18h2M72' +
          ' 18h2M94 18h2M111 18h13M1 19h12M28 19h2M50 19h2M72 19h2M94 19h2M' +
          '111 19h13M1 20h12M23 20h1M28 20h2M50 20h2M72 20h2M94 20h2M111 20' +
          'h13M1 21h12M28 21h2M50 21h2M72 21h2M94 21h2M111 21h13M1 22h12M28' +
          ' 22h2M50 22h2M72 22h2M94 22h2M111 22h13M1 23h12M28 23h2M50 23h2M' +
          '72 23h2M94 23h2M111 23h13M1 24h12M28 24h2M50 24h2M72 24h2M94 24h' +
          '2M111 24h13M1 25h13M28 25h24M72 25h2M94 25h2M110 25h14M1 26h123M' +
          '1 27h123M1 28h123M1 29h123M1 30h123M1 31h123M1 32h123M1 33h123M1' +
          ' 34h51M72 34h52M1 35h51M72 35h52M1 36h51M72 36h52M1 37h51M72 37h' +
          '52M1 38h51M72 38h52M1 39h51M72 39h52M1 40h51M72 40h52M1 41h55M62' +
          ' 41h62M1 42h123M1 43h123" />'#13#10'<path stroke="#e5e5e5" d="M14 5h1M' +
          '13 6h1M59 13h3M81 13h3M54 14h5M64 14h6M76 14h5M86 14h6M98 14h3M1' +
          '07 14h2M107 17h1M102 19h1M53 21h1M70 21h1M75 21h1M92 21h1M97 21h' +
          '1M109 21h1M13 23h1" />'#13#10'<path stroke="#dbdbdb" d="M15 5h1M13 7h1' +
          'M54 21h16M76 21h16M98 21h11M13 22h1M108 22h1M15 24h1" />'#13#10'<path ' +
          'stroke="#d8d8d8" d="M16 5h12M27 6h1M27 7h1M13 8h1M27 8h1M13 9h1M' +
          '27 9h1M13 10h1M27 10h1M13 11h1M27 11h1M13 12h1M27 12h1M13 13h1M2' +
          '7 13h1M13 14h1M27 14h1M13 15h1M27 15h1M13 16h1M27 16h1M13 17h1M2' +
          '7 17h1M13 18h1M27 18h1M13 19h1M27 19h1M13 20h1M27 20h1M13 21h1M2' +
          '7 21h1M27 22h1M27 23h1M16 24h12" />'#13#10'<path stroke="#077398" d="M' +
          '30 5h20M30 6h1M49 6h1M30 7h1M49 7h1M30 8h1M49 8h1M30 9h1M49 9h1M' +
          '30 10h1M49 10h1M30 11h1M49 11h1M30 12h1M49 12h1M30 13h1M49 13h1M' +
          '30 14h1M49 14h1M30 15h1M49 15h1M30 16h1M49 16h1M30 17h1M49 17h1M' +
          '30 18h1M49 18h1M30 19h1M49 19h1M30 20h1M49 20h1M30 21h1M49 21h1M' +
          '30 22h1M49 22h1M30 23h1M49 23h1M30 24h20" />'#13#10'<path stroke="#b3b' +
          '3b3" d="M52 5h20M74 5h20M96 5h12M52 6h1M71 6h1M74 6h1M93 6h1M96 ' +
          '6h1M52 7h1M71 7h1M74 7h1M93 7h1M96 7h1M52 8h1M71 8h1M74 8h1M93 8' +
          'h1M96 8h1M110 8h1M52 9h1M71 9h1M74 9h1M93 9h1M96 9h1M110 9h1M52 ' +
          '10h1M71 10h1M74 10h1M93 10h1M96 10h1M110 10h1M52 11h1M71 11h1M74' +
          ' 11h1M93 11h1M96 11h1M110 11h1M23 12h1M52 12h1M71 12h1M74 12h1M9' +
          '3 12h1M96 12h1M110 12h1M21 13h3M52 13h1M71 13h1M74 13h1M93 13h1M' +
          '96 13h1M110 13h1M20 14h4M52 14h1M71 14h1M74 14h1M93 14h1M96 14h1' +
          'M110 14h1M18 15h6M52 15h1M71 15h1M74 15h1M93 15h1M96 15h1M110 15' +
          'h1M19 16h5M52 16h1M71 16h1M74 16h1M93 16h1M96 16h1M110 16h1M21 1' +
          '7h3M52 17h1M71 17h1M74 17h1M93 17h1M96 17h1M110 17h1M23 18h1M52 ' +
          '18h1M71 18h1M74 18h1M93 18h1M96 18h1M110 18h1M52 19h1M71 19h1M74' +
          ' 19h1M93 19h1M96 19h1M110 19h1M52 20h1M71 20h1M74 20h1M93 20h1M9' +
          '6 20h1M110 20h1M52 21h1M71 21h1M74 21h1M93 21h1M96 21h1M110 21h1' +
          'M52 22h1M71 22h1M74 22h1M93 22h1M96 22h1M52 23h1M71 23h1M74 23h1' +
          'M93 23h1M96 23h1M52 24h20M74 24h20M96 24h12" />'#13#10'<path stroke="#' +
          'b5b5b5" d="M108 5h1M110 7h1M22 18h1" />'#13#10'<path stroke="#d1d1d1" ' +
          'd="M109 5h1M110 6h1" />'#13#10'<path stroke="#f2f2f2" d="M110 5h1M14 1' +
          '3h1M26 13h1M53 13h1M70 13h1M75 13h1M92 13h1M97 13h1M109 13h1M58 ' +
          '34h1M65 34h1M71 34h1M52 35h2M58 35h1M62 35h1M65 35h1M68 35h1M71 ' +
          '35h1M52 36h2M62 36h1M65 36h1M68 36h1M52 37h2M60 37h1M62 37h1M65 ' +
          '37h1M68 37h1M52 38h2M60 38h1M62 38h1M65 38h1M68 38h1M52 39h2M59 ' +
          '39h2M62 39h1M65 39h1M68 39h1M71 39h1M59 40h2M71 40h1M59 41h3" />' +
          #13#10'<path stroke="#e3e3e3" d="M14 6h1M54 15h5M64 15h6M76 15h5M86 1' +
          '5h6M98 15h3M108 15h1" />'#13#10'<path stroke="#f4f4f4" d="M15 6h1M14 1' +
          '0h1M26 10h1M18 17h1M104 18h1M101 20h1M13 24h1M14 25h1" />'#13#10'<path' +
          ' stroke="#f6f6f6" d="M16 6h11M108 6h1M26 7h1M53 10h1M70 10h1M75 ' +
          '10h1M92 10h1M97 10h1M109 10h1M106 17h1M110 24h1M15 25h13" />'#13#10'<p' +
          'ath stroke="#169aca" d="M31 6h18" />'#13#10'<path stroke="#fcfcfc" d="' +
          'M53 6h18M75 6h18M97 6h11" />'#13#10'<path stroke="#cbcbcb" d="M109 6h1' +
          'M20 13h1" />'#13#10'<path stroke="#f1f1f1" d="M15 7h11M14 14h1M26 14h1' +
          'M14 15h1M26 15h1M21 19h1M56 34h2M66 34h1M56 35h2M61 35h1M56 36h2' +
          'M61 36h1M56 37h2M61 37h1M56 38h2M61 38h1M56 39h1M61 39h1" />'#13#10'<p' +
          'ath stroke="#169ac9" d="M31 7h18" />'#13#10'<path stroke="#fbfbfb" d="' +
          'M53 7h1M70 7h1M75 7h1M92 7h1M97 7h1" />'#13#10'<path stroke="#eeeeee" ' +
          'd="M54 7h16M76 7h16M98 7h10M15 12h6M24 12h2M53 15h1M70 15h1M75 1' +
          '5h1M92 15h1M97 15h1M109 15h1M14 19h1M26 19h1M103 19h1M14 20h1M26' +
          ' 20h1" />'#13#10'<path stroke="#f0f0f0" d="M108 7h1M15 8h11M15 9h11M53' +
          ' 14h1M70 14h1M75 14h1M92 14h1M97 14h1M109 14h1M14 16h1M26 16h1" ' +
          '/>'#13#10'<path stroke="#f5f5f5" d="M109 7h1M14 8h1M26 8h1M14 9h1M26 9' +
          'h1M53 11h1M70 11h1M75 11h1M92 11h1M97 11h1M109 11h1" />'#13#10'<path s' +
          'troke="#1599c8" d="M31 8h18" />'#13#10'<path stroke="#f9f9f9" d="M53 8' +
          'h1M70 8h1M75 8h1M92 8h1M97 8h1M109 8h1" />'#13#10'<path stroke="#edede' +
          'd" d="M54 8h16M76 8h16M98 8h11M15 13h4M24 13h2M15 14h3M24 14h2M5' +
          '3 16h1M70 16h1M75 16h1M92 16h1M97 16h1M109 16h1M17 17h1M22 19h1M' +
          '14 21h1M26 21h1M26 22h1" />'#13#10'<path stroke="#1598c7" d="M31 9h18"' +
          ' />'#13#10'<path stroke="#f8f8f8" d="M53 9h1M70 9h1M75 9h1M92 9h1M97 9' +
          'h1M109 9h1" />'#13#10'<path stroke="#ececec" d="M54 9h16M76 9h16M98 9h' +
          '11M15 15h2M24 15h2M15 16h2M24 16h2M16 23h11" />'#13#10'<path stroke="#' +
          'efefef" d="M15 10h11M15 11h7M24 11h2M14 17h1M26 17h1M14 18h1M19 ' +
          '18h1M26 18h1" />'#13#10'<path stroke="#1597c6" d="M31 10h18" />'#13#10'<path' +
          ' stroke="#eaeaea" d="M54 10h16M76 10h16M98 10h11M53 18h1M70 18h1' +
          'M75 18h1M92 18h1M97 18h1M109 18h1M15 19h5M24 19h2M15 20h7M24 20h' +
          '2M15 23h1" />'#13#10'<path stroke="#ebebeb" d="M22 11h1M19 13h1M15 17h' +
          '2M24 17h2M53 17h1M70 17h1M75 17h1M92 17h1M97 17h1M109 17h1M15 18' +
          'h4M24 18h2M20 19h1M22 20h1M14 22h1" />'#13#10'<path stroke="#c6c6c6" d' +
          '="M23 11h1M106 14h1" />'#13#10'<path stroke="#1496c5" d="M31 11h6M43 1' +
          '1h6" />'#13#10'<path stroke="#14a7e2" d="M37 11h1" />'#13#10'<path stroke="#' +
          'daffff" d="M38 11h1M38 17h1" />'#13#10'<path stroke="#fffff4" d="M39 1' +
          '1h1" />'#13#10'<path stroke="#8fa7c4" d="M40 11h1" />'#13#10'<path stroke="#' +
          '1395c4" d="M41 11h2M31 12h6M43 12h6" />'#13#10'<path stroke="#e9e9e9" ' +
          'd="M54 11h5M64 11h6M76 11h5M86 11h6M98 11h3M103 11h6M105 18h1M15' +
          ' 21h11M15 22h11" />'#13#10'<path stroke="#e8e8e8" d="M59 11h1M54 12h5M' +
          '64 12h6M76 12h5M86 12h6M98 12h3M104 12h5M53 19h1M70 19h1M75 19h1' +
          'M92 19h1M97 19h1M109 19h1" />'#13#10'<path stroke="#af7334" d="M60 11h' +
          '1" />'#13#10'<path stroke="#282828" d="M61 11h1M83 11h1M83 14h1M60 17h' +
          '3M82 17h2M68 34h1M54 40h2M63 40h2M68 40h1" />'#13#10'<path stroke="#34' +
          '3453" d="M62 11h1" />'#13#10'<path stroke="#91cbe8" d="M63 11h1" />'#13#10'<' +
          'path stroke="#e8cb91" d="M81 11h1" />'#13#10'<path stroke="#533434" d=' +
          '"M82 11h1" />'#13#10'<path stroke="#343473" d="M84 11h1" />'#13#10'<path str' +
          'oke="#afe8e8" d="M85 11h1" />'#13#10'<path stroke="#868686" d="M101 11' +
          'h1" />'#13#10'<path stroke="#dfdfdf" d="M102 11h1M54 18h16M76 18h16M98' +
          ' 18h3M106 18h3M104 19h1M14 23h1" />'#13#10'<path stroke="#dedede" d="M' +
          '21 12h1M54 19h16M76 19h16M98 19h3M105 19h4M108 23h1" />'#13#10'<path s' +
          'troke="#b9b9b9" d="M22 12h1" />'#13#10'<path stroke="#1194c3" d="M37 1' +
          '2h2M41 12h2" />'#13#10'<path stroke="#40c9ea" d="M39 12h1M39 13h1" />'#13 +
          #10'<path stroke="#8ea6c3" d="M40 12h1" />'#13#10'<path stroke="#e7c990" ' +
          'd="M59 12h1M62 12h1" />'#13#10'<path stroke="#90aee7" d="M60 12h1" />'#13 +
          #10'<path stroke="#e7e7e7" d="M61 12h1M81 12h3M17 16h1M53 20h1M70 2' +
          '0h1M75 20h1M92 20h1M97 20h1M109 20h1" />'#13#10'<path stroke="#5373ae"' +
          ' d="M63 12h1" />'#13#10'<path stroke="#c99053" d="M84 12h1" />'#13#10'<path ' +
          'stroke="#73aee7" d="M85 12h1" />'#13#10'<path stroke="#575757" d="M101' +
          ' 12h1M101 13h3M101 14h4M101 15h5M101 16h5M101 17h3M101 18h1" />'#13 +
          #10'<path stroke="#666666" d="M102 12h1" />'#13#10'<path stroke="#bfbfbf"' +
          ' d="M103 12h1" />'#13#10'<path stroke="#1294c3" d="M31 13h6M43 13h6" /' +
          '>'#13#10'<path stroke="#1193c2" d="M37 13h2M41 13h2" />'#13#10'<path stroke=' +
          '"#8ea5c2" d="M40 13h1" />'#13#10'<path stroke="#e6e6e6" d="M54 13h5M64' +
          ' 13h6M76 13h5M86 13h6M98 13h3M106 13h3M14 24h1" />'#13#10'<path stroke' +
          '="#e5c88f" d="M62 13h1" />'#13#10'<path stroke="#5372ac" d="M63 13h1" ' +
          '/>'#13#10'<path stroke="#c88f72" d="M84 13h1" />'#13#10'<path stroke="#8fc8e' +
          '5" d="M85 13h1" />'#13#10'<path stroke="#919191" d="M104 13h1" />'#13#10'<pa' +
          'th stroke="#e0e0e0" d="M105 13h1M19 17h1M109 22h1" />'#13#10'<path str' +
          'oke="#e1e1e1" d="M18 14h1M62 16h2M81 16h3M54 17h5M64 17h6M76 17h' +
          '5M86 17h6M98 17h3M108 17h1M102 20h1" />'#13#10'<path stroke="#bbbbbb" ' +
          'd="M19 14h1M20 17h1" />'#13#10'<path stroke="#1293c2" d="M31 14h6M43 1' +
          '4h6" />'#13#10'<path stroke="#1192c1" d="M37 14h2M41 14h2" />'#13#10'<path s' +
          'troke="#3fc8e9" d="M39 14h1M39 15h1" />'#13#10'<path stroke="#8ea4c1" ' +
          'd="M40 14h1" />'#13#10'<path stroke="#e4e4e4" d="M59 14h2M81 14h1M85 1' +
          '4h1M53 22h1M70 22h1M75 22h1M92 22h1M97 22h1" />'#13#10'<path stroke="#' +
          'e4e4c8" d="M61 14h1" />'#13#10'<path stroke="#8f5372" d="M62 14h1" />'#13 +
          #10'<path stroke="#ace4e4" d="M63 14h1" />'#13#10'<path stroke="#ac7234" ' +
          'd="M82 14h1" />'#13#10'<path stroke="#548fc8" d="M84 14h1" />'#13#10'<path s' +
          'troke="#6c6c6c" d="M105 14h1" />'#13#10'<path stroke="#cfcfcf" d="M17 ' +
          '15h1" />'#13#10'<path stroke="#1191c0" d="M31 15h6M43 15h6" />'#13#10'<path ' +
          'stroke="#1090bf" d="M37 15h2M41 15h2M31 16h6M43 16h6" />'#13#10'<path ' +
          'stroke="#8ea2bf" d="M40 15h1" />'#13#10'<path stroke="#e2e2e2" d="M59 ' +
          '15h1M63 15h1M81 15h3M54 16h5M64 16h6M76 16h5M86 16h6M98 16h3M108' +
          ' 16h1M53 23h18M75 23h18M97 23h11" />'#13#10'<path stroke="#e2e2c5" d="' +
          'M60 15h1" />'#13#10'<path stroke="#8d5252" d="M61 15h1" />'#13#10'<path stro' +
          'ke="#8dc5e2" d="M62 15h1" />'#13#10'<path stroke="#c48c52" d="M84 15h1' +
          'M59 17h1" />'#13#10'<path stroke="#3471aa" d="M85 15h1" />'#13#10'<path stro' +
          'ke="#595959" d="M106 15h1" />'#13#10'<path stroke="#9d9d9d" d="M107 15' +
          'h1" />'#13#10'<path stroke="#c4c4c4" d="M18 16h1" />'#13#10'<path stroke="#1' +
          '090be" d="M37 16h2M41 16h2M31 17h6M43 17h6" />'#13#10'<path stroke="#3' +
          'dc8e8" d="M39 16h1" />'#13#10'<path stroke="#8da2be" d="M40 16h1" />'#13#10 +
          '<path stroke="#e1e1aa" d="M59 16h1" />'#13#10'<path stroke="#703452" d' +
          '="M60 16h1" />'#13#10'<path stroke="#8dc4e1" d="M61 16h1" />'#13#10'<path st' +
          'roke="#e1aa70" d="M84 16h1" />'#13#10'<path stroke="#3470aa" d="M85 16' +
          'h1" />'#13#10'<path stroke="#818181" d="M106 16h1" />'#13#10'<path stroke="#' +
          'd6d6d6" d="M107 16h1" />'#13#10'<path stroke="#0fa2de" d="M37 17h1" />' +
          #13#10'<path stroke="#ffffff" d="M39 17h2" />'#13#10'<path stroke="#feecde"' +
          ' d="M41 17h1" />'#13#10'<path stroke="#3c8fbd" d="M42 17h1" />'#13#10'<path ' +
          'stroke="#3470a9" d="M63 17h1" />'#13#10'<path stroke="#e0a970" d="M81 ' +
          '17h1" />'#13#10'<path stroke="#343470" d="M84 17h1" />'#13#10'<path stroke="' +
          '#a9e0e0" d="M85 17h1" />'#13#10'<path stroke="#6b6b6b" d="M104 17h1" /' +
          '>'#13#10'<path stroke="#c3c3c3" d="M105 17h1" />'#13#10'<path stroke="#d3d3d' +
          '3" d="M21 18h1" />'#13#10'<path stroke="#108fbd" d="M31 18h18" />'#13#10'<pa' +
          'th stroke="#5c5c5c" d="M102 18h1" />'#13#10'<path stroke="#a6a6a6" d="' +
          'M103 18h1" />'#13#10'<path stroke="#c5c5c5" d="M23 19h1" />'#13#10'<path str' +
          'oke="#0e8ebc" d="M31 19h18" />'#13#10'<path stroke="#848484" d="M101 1' +
          '9h1" />'#13#10'<path stroke="#0d8dbb" d="M31 20h18" />'#13#10'<path stroke="' +
          '#dddddd" d="M54 20h16M76 20h16M98 20h3M103 20h6" />'#13#10'<path strok' +
          'e="#0d8cba" d="M31 21h18" />'#13#10'<path stroke="#0c8bb9" d="M31 22h1' +
          '8" />'#13#10'<path stroke="#dad9da" d="M54 22h16M76 22h16M98 22h10" />' +
          #13#10'<path stroke="#b6b6b6" d="M110 22h1M108 24h1" />'#13#10'<path stroke' +
          '="#0a8ab8" d="M31 23h18" />'#13#10'<path stroke="#c2c2c2" d="M109 23h1' +
          '" />'#13#10'<path stroke="#d2d2d2" d="M110 23h1M109 24h1" />'#13#10'<path st' +
          'roke="#fafafa" d="M52 25h20M74 25h20M96 25h13" />'#13#10'<path stroke=' +
          '"#f7f7f7" d="M109 25h1" />'#13#10'<path stroke="#f2d396" d="M52 34h1M5' +
          '8 37h1M52 40h1" />'#13#10'<path stroke="#553434" d="M53 34h1M62 34h1M5' +
          '3 40h1M62 40h1" />'#13#10'<path stroke="#343455" d="M54 34h1M63 34h1M6' +
          '9 34h1" />'#13#10'<path stroke="#96d3f2" d="M55 34h1M64 34h1M70 34h1M5' +
          '5 35h1M64 35h1M55 36h1M64 36h1M55 37h1M64 37h1M67 37h1M55 38h1M5' +
          '9 38h1M64 38h1M55 39h1M64 39h1" />'#13#10'<path stroke="#f1f1b5" d="M5' +
          '9 34h1M57 39h1M56 41h1" />'#13#10'<path stroke="#775596" d="M60 34h1M5' +
          '7 41h1" />'#13#10'<path stroke="#d2d295" d="M61 34h1" />'#13#10'<path stroke' +
          '="#b57734" d="M67 34h1M67 40h1" />'#13#10'<path stroke="#d39677" d="M5' +
          '4 35h1M63 35h1M54 36h1M63 36h1M54 37h1M63 37h1M54 38h1M63 38h1M5' +
          '4 39h1M63 39h1" />'#13#10'<path stroke="#f1b577" d="M59 35h1M69 35h1M6' +
          '6 39h1M69 39h1" />'#13#10'<path stroke="#77b5f2" d="M60 35h1M67 36h1M6' +
          '7 38h1M58 40h1" />'#13#10'<path stroke="#f1d295" d="M66 35h1M61 40h1" ' +
          '/>'#13#10'<path stroke="#5577b5" d="M67 35h1" />'#13#10'<path stroke="#3477b' +
          '5" d="M70 35h1M67 39h1M70 39h1" />'#13#10'<path stroke="#f2f2d3" d="M5' +
          '8 36h1M69 36h1M69 37h1M69 38h1" />'#13#10'<path stroke="#965577" d="M5' +
          '9 36h1M70 36h1M70 37h1M70 38h1" />'#13#10'<path stroke="#b5f2f2" d="M6' +
          '0 36h1M71 36h1M71 37h1M71 38h1M70 40h1" />'#13#10'<path stroke="#d2955' +
          '5" d="M66 36h1M66 38h1" />'#13#10'<path stroke="#7796d3" d="M59 37h1" ' +
          '/>'#13#10'<path stroke="#d29577" d="M66 37h1" />'#13#10'<path stroke="#b5775' +
          '5" d="M58 38h1" />'#13#10'<path stroke="#7777b5" d="M58 39h1" />'#13#10'<pat' +
          'h stroke="#335495" d="M56 40h1" />'#13#10'<path stroke="#b99555" d="M5' +
          '7 40h1" />'#13#10'<path stroke="#345596" d="M65 40h1" />'#13#10'<path stroke' +
          '="#d2f1f1" d="M66 40h1" />'#13#10'<path stroke="#343477" d="M69 40h1" ' +
          '/>'#13#10'<path stroke="#d3f2f2" d="M58 41h1" />'#13#10'</svg>'#13#10
      end>
    Left = 48
    Top = 360
  end
  object icTurtles: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#11ffff" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h2M5 5h11M18 5h2M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h2M5 21h11M18 21' +
          'h2M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#fe0000" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#cc3333" d="M2 5h1M18 5h1M2 21h1M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#11ff26" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#37c928" d="M2 5h1M18 5h1M2 21h1M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#002aff" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#0025db" d="M2 5h1M18 5h1M2 21h1M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#fed600" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M18 21' +
          'h2M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#c4b432" d="M2 5h1M18 5h1M2 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#7e7e7e" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#8c886e" d="M2 5h1M18 5h1M2 21h1" />'#13#10'<path stroke="#8c8770" ' +
          'd="M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '06'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#fe00db" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#f116cd" d="M2 5h1M18 5h1M2 21h1M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '07'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#fe6900" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#fb5c23" d="M2 5h1M18 5h1M2 21h1M18 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '08'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#b000ff" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M2 21h1M5 21h11M19 21' +
          'h1M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path stroke="#bc1' +
          '6d5" d="M2 5h1M18 5h1M18 21h1" />'#13#10'<path stroke="#cc2b9d" d="M1 ' +
          '21h1M2 22h1" />'#13#10'<path stroke="#ef5633" d="M1 22h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '09'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#424242" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#543958" d="M2 5h1M18 5h1M18 21h1" />'#13#10'<path stroke="#493b4f"' +
          ' d="M2 21h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '10'
        SVGText = 
          '<svg viewBox="0 -0.5 21 26">'#13#10'<path stroke="#000000" d="M9 0h3M9' +
          ' 1h3M9 2h3M9 3h3M1 4h2M6 4h9M18 4h2M1 5h1M5 5h11M19 5h1M3 6h15M3' +
          ' 7h15M2 8h17M1 9h19M1 10h19M1 11h19M1 12h19M0 13h21M1 14h19M1 15' +
          'h19M1 16h19M1 17h19M2 18h17M3 19h15M3 20h15M1 21h1M5 21h11M19 21' +
          'h1M1 22h2M6 22h9M18 22h2M9 23h3M10 24h1M10 25h1" />'#13#10'<path strok' +
          'e="#0e0c0f" d="M2 5h1M18 5h1M18 21h1" />'#13#10'<path stroke="#0d0c0e"' +
          ' d="M2 21h1" />'#13#10'</svg>'#13#10
      end>
    Left = 48
    Top = 432
  end
  object icFXTurtle: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'FXTurtle'
        SVGText = 
          '<svg viewBox="0 -0.5 26 21">'#10'<path stroke="#11ffff" d="M12 0h1M3' +
          ' 1h2M8 1h9M20 1h2M3 2h2M7 2h11M20 2h2M5 3h15M5 4h15M4 5h17M3 6h1' +
          '9M3 7h19M3 8h19M2 9h24M0 10h26M2 11h24M3 12h19M3 13h19M3 14h19M4' +
          ' 15h17M5 16h15M5 17h15M3 18h2M7 18h11M20 18h2M3 19h2M8 19h9M20 1' +
          '9h2M12 20h1" />'#10'</svg>'
      end>
    Left = 48
    Top = 496
  end
  object vilGuiDesignerLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Close1'
        Name = 'Close1'
      end
      item
        CollectionIndex = 1
        CollectionName = 'OpenSource'
        Name = 'OpenSource'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ToFront'
        Name = 'ToFront'
      end
      item
        CollectionIndex = 3
        CollectionName = 'ToBack'
        Name = 'ToBack'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Left1'
        Name = 'Left1'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Delete1'
        Name = 'Delete1'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Cut1'
        Name = 'Cut1'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Copy1'
        Name = 'Copy1'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Paste1'
        Name = 'Paste1'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Left1'
        Name = 'Left1'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Centered1'
        Name = 'Centered1'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Right1'
        Name = 'Right1'
      end
      item
        CollectionIndex = 12
        CollectionName = 'CenteredWindows'
        Name = 'CenteredWindows'
      end
      item
        CollectionIndex = 13
        CollectionName = 'SameDistanceH'
        Name = 'SameDistanceH'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Top1'
        Name = 'Top1'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Middle1'
        Name = 'Middle1'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Bottom1'
        Name = 'Bottom1'
      end
      item
        CollectionIndex = 17
        CollectionName = 'MiddleWindows'
        Name = 'MiddleWindows'
      end
      item
        CollectionIndex = 18
        CollectionName = 'SameDistanceV'
        Name = 'SameDistanceV'
      end
      item
        CollectionIndex = 19
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 20
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end>
    ImageCollection = scGuiDesigner
    Left = 304
    Top = 16
  end
  object vilControls1315: TVirtualImageList
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
      end>
    ImageCollection = icJavaControls1315
    Width = 13
    Height = 15
    Left = 168
    Top = 88
  end
  object vilControls1616: TVirtualImageList
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
      end>
    ImageCollection = icJavaControls1616
    Left = 168
    Top = 160
  end
  object vilControls1618: TVirtualImageList
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
      end>
    ImageCollection = icJavaControls1618
    Height = 18
    Left = 168
    Top = 232
  end
  object vilControls21616: TVirtualImageList
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
      end>
    ImageCollection = icJavaControls21616
    Left = 168
    Top = 296
  end
  object vilPagination: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Pagination'
        Name = 'Pagination'
      end>
    ImageCollection = icPagination
    Width = 125
    Height = 45
    Left = 168
    Top = 360
  end
  object vilTurtles: TVirtualImageList
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
      end>
    ImageCollection = icTurtles
    Width = 21
    Height = 26
    Left = 168
    Top = 432
  end
  object vilFXTurtle: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'FXTurtle'
        Name = 'FXTurtle'
      end>
    ImageCollection = icFXTurtle
    Width = 26
    Height = 21
    Left = 168
    Top = 496
  end
end
