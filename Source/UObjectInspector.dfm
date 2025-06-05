object FObjectInspector: TFObjectInspector
  Left = 71
  Top = 270
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Object inspector'
  ClientHeight = 326
  ClientWidth = 263
  Color = clBtnFace
  Constraints.MinHeight = 150
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  PopupMenu = PMObjectInspector
  Position = poScreenCenter
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnMouseActivate = FormMouseActivate
  OnShow = FormShow
  TextHeight = 15
  object TCAttributesEvents: TTabControl
    Left = 0
    Top = 33
    Width = 263
    Height = 25
    Align = alTop
    TabOrder = 0
    Tabs.Strings = (
      'Attributes'
      'Events')
    TabIndex = 0
    OnChange = TCAttributesEventsChange
    OnMouseDown = TCAttributesEventsMouseDown
  end
  object PObjects: TPanel
    Left = 0
    Top = 0
    Width = 263
    Height = 33
    Align = alTop
    TabOrder = 1
    object CBObjects: TComboBox
      Left = 5
      Top = 4
      Width = 252
      Height = 23
      Style = csDropDownList
      Sorted = True
      TabOrder = 0
      OnChange = CBObjectsChange
    end
  end
  object PNewDel: TPanel
    Left = 0
    Top = 301
    Width = 263
    Height = 25
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      263
      25)
    object BNewDelete: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = '&New'
      TabOrder = 0
      Visible = False
      OnClick = BNewDeleteClick
    end
    object BMore: TButton
      Left = 164
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'More'
      TabOrder = 1
      OnClick = BMoreClick
    end
  end
  object PMObjectInspector: TSpTBXPopupMenu
    Images = vilObjectInspectorLight
    OnPopup = PMObjectInspectorPopup
    Left = 48
    Top = 104
    object MICut: TSpTBXItem
      Caption = 'Cut'
      ImageIndex = 2
      ImageName = 'Cut'
      OnClick = MICutClick
    end
    object MICopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 3
      ImageName = 'Copy'
      OnClick = MICopyClick
    end
    object MIPaste: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 4
      ImageName = 'Paste'
      OnClick = MIPasteClick
    end
    object SpTBXSeparatorItem1: TSpTBXSeparatorItem
    end
    object MIDefaultLayout: TSpTBXItem
      Caption = 'Default layout'
      ImageIndex = 5
      ImageName = 'DefaultLayout'
    end
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 0
      ImageName = 'Font'
      OnClick = MIFontClick
    end
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 1
      ImageName = 'Close'
      OnClick = MICloseClick
    end
  end
  object vilObjectInspectorLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 8
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icObjectInspector
    Left = 176
    Top = 104
  end
  object vilObjectInspectorDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 5
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 8
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icObjectInspector
    Left = 176
    Top = 168
  end
  object icObjectInspector: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Font'
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
          ' 17.5T200-740v60Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Cut'
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
        IconName = 'Paste'
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
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Cut'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#616467" d="M13 0h1M' +
          '10 4h1" />'#13#10'<path stroke="#66696b" d="M2 1h1M13 1h1" />'#13#10'<path s' +
          'troke="#5e6264" d="M3 1h1" />'#13#10'<path stroke="#5f6265" d="M12 1h1' +
          '" />'#13#10'<path stroke="#5a5e62" d="M2 2h1" />'#13#10'<path stroke="#76767' +
          '6" d="M3 2h1M12 2h1M3 3h1M12 3h1M4 4h1M11 4h1M4 5h2M10 5h2M5 6h1' +
          'M10 6h1M6 7h1M9 7h1M7 8h2" />'#13#10'<path stroke="#4b5258" d="M4 2h1M' +
          '6 5h1" />'#13#10'<path stroke="#4c5358" d="M11 2h1M9 5h1" />'#13#10'<path st' +
          'roke="#5a5f62" d="M13 2h1" />'#13#10'<path stroke="#475055" d="M2 3h1"' +
          ' />'#13#10'<path stroke="#6f7071" d="M4 3h1M11 3h1" />'#13#10'<path stroke="' +
          '#3d464e" d="M5 3h1M10 3h1" />'#13#10'<path stroke="#474f55" d="M13 3h1' +
          '" />'#13#10'<path stroke="#6b6d6e" d="M3 4h1" />'#13#10'<path stroke="#60636' +
          '6" d="M5 4h1" />'#13#10'<path stroke="#6d6e6f" d="M12 4h1" />'#13#10'<path s' +
          'troke="#4e545a" d="M3 5h1" />'#13#10'<path stroke="#52585d" d="M12 5h1' +
          '" />'#13#10'<path stroke="#686a6c" d="M4 6h1M8 7h1" />'#13#10'<path stroke="' +
          '#707172" d="M6 6h1" />'#13#10'<path stroke="#3e474f" d="M7 6h2" />'#13#10'<p' +
          'ath stroke="#717272" d="M9 6h1" />'#13#10'<path stroke="#6d6e70" d="M1' +
          '1 6h1" />'#13#10'<path stroke="#424a51" d="M4 7h1" />'#13#10'<path stroke="#' +
          '727273" d="M5 7h1" />'#13#10'<path stroke="#676a6c" d="M7 7h1" />'#13#10'<pa' +
          'th stroke="#747474" d="M10 7h1" />'#13#10'<path stroke="#485055" d="M1' +
          '1 7h1" />'#13#10'<path stroke="#495056" d="M5 8h1" />'#13#10'<path stroke="#' +
          '727373" d="M6 8h1" />'#13#10'<path stroke="#747575" d="M9 8h1" />'#13#10'<pa' +
          'th stroke="#51575c" d="M10 8h1" />'#13#10'<path stroke="#3f5164" d="M6' +
          ' 9h1" />'#13#10'<path stroke="#43607e" d="M7 9h2" />'#13#10'<path stroke="#4' +
          '0576f" d="M9 9h1" />'#13#10'<path stroke="#3a536c" d="M2 10h1" />'#13#10'<pa' +
          'th stroke="#3c6c9f" d="M3 10h1" />'#13#10'<path stroke="#3e75af" d="M4' +
          ' 10h1M7 11h1M14 13h1" />'#13#10'<path stroke="#3d73aa" d="M5 10h1M10 1' +
          '4h1M13 14h1" />'#13#10'<path stroke="#3c6fa3" d="M6 10h1M9 10h1" />'#13#10'<' +
          'path stroke="#3e76af" d="M7 10h2M2 11h1M6 11h1M9 11h1M6 12h1M9 1' +
          '2h1M6 13h1M9 13h1" />'#13#10'<path stroke="#3d6fa4" d="M10 10h1M12 11h' +
          '1" />'#13#10'<path stroke="#3e74ae" d="M11 10h1" />'#13#10'<path stroke="#3d' +
          '6da0" d="M12 10h1" />'#13#10'<path stroke="#39546f" d="M13 10h1M8 13h1' +
          '" />'#13#10'<path stroke="#395067" d="M1 11h1" />'#13#10'<path stroke="#3c6f' +
          'a4" d="M3 11h1" />'#13#10'<path stroke="#3a5a79" d="M4 11h1" />'#13#10'<path' +
          ' stroke="#3b648d" d="M5 11h1" />'#13#10'<path stroke="#3d71a8" d="M8 1' +
          '1h1" />'#13#10'<path stroke="#3c6692" d="M10 11h1" />'#13#10'<path stroke="#' +
          '3b5978" d="M11 11h1" />'#13#10'<path stroke="#3d76af" d="M13 11h1" />'#13 +
          #10'<path stroke="#3a516a" d="M14 11h1" />'#13#10'<path stroke="#3c6998" ' +
          'd="M1 12h1" />'#13#10'<path stroke="#3d74ac" d="M2 12h1" />'#13#10'<path str' +
          'oke="#38495a" d="M3 12h1" />'#13#10'<path stroke="#384959" d="M5 12h1M' +
          '6 15h1" />'#13#10'<path stroke="#3c6d9e" d="M7 12h1M9 14h1" />'#13#10'<path ' +
          'stroke="#3b648e" d="M8 12h1" />'#13#10'<path stroke="#394c5e" d="M10 1' +
          '2h1" />'#13#10'<path stroke="#374a5a" d="M12 12h1" />'#13#10'<path stroke="#' +
          '3d74ad" d="M13 12h1" />'#13#10'<path stroke="#3c6a9b" d="M14 12h1M1 14' +
          'h1" />'#13#10'<path stroke="#3d74ae" d="M1 13h1" />'#13#10'<path stroke="#3d' +
          '6a99" d="M2 13h1M13 13h1" />'#13#10'<path stroke="#395776" d="M5 13h1"' +
          ' />'#13#10'<path stroke="#3a5f83" d="M7 13h1" />'#13#10'<path stroke="#3a577' +
          '5" d="M10 13h1M11 14h1" />'#13#10'<path stroke="#3d73ac" d="M2 14h1" /' +
          '>'#13#10'<path stroke="#395675" d="M3 14h1" />'#13#10'<path stroke="#3b5a7b"' +
          ' d="M4 14h1" />'#13#10'<path stroke="#3d73ab" d="M5 14h1" />'#13#10'<path st' +
          'roke="#3e71a8" d="M6 14h1" />'#13#10'<path stroke="#384757" d="M7 14h1' +
          '" />'#13#10'<path stroke="#395470" d="M12 14h1" />'#13#10'<path stroke="#3c6' +
          'b9c" d="M14 14h1" />'#13#10'<path stroke="#384957" d="M1 15h1" />'#13#10'<pa' +
          'th stroke="#3c638c" d="M2 15h1" />'#13#10'<path stroke="#3d70a5" d="M3' +
          ' 15h1M11 15h1" />'#13#10'<path stroke="#3c6ea3" d="M4 15h1" />'#13#10'<path ' +
          'stroke="#3a628a" d="M5 15h1M10 15h1" />'#13#10'<path stroke="#384754" ' +
          'd="M9 15h1" />'#13#10'<path stroke="#3d72a8" d="M12 15h1" />'#13#10'<path st' +
          'roke="#3b6590" d="M13 15h1" />'#13#10'<path stroke="#384858" d="M14 15' +
          'h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'DefaultLayout'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16" >'#13#10'<path stroke="#9e9e9e" d="M0 0h16' +
          'M0 1h1M12 1h1M15 1h1M0 2h1M12 2h1M15 2h1M0 3h1M12 3h1M15 3h1M0 4' +
          'h1M12 4h1M15 4h1M0 5h1M12 5h1M15 5h1M0 6h1M12 6h1M15 6h1M0 7h1M1' +
          '2 7h1M15 7h1M0 8h1M12 8h1M15 8h1M0 9h1M12 9h1M15 9h1M0 10h1M12 1' +
          '0h1M15 10h1M0 11h16M0 12h1M15 12h1M0 13h1M15 13h1M0 14h1M15 14h1' +
          'M0 15h16" />'#13#10'<path stroke="#ffffff" d="M1 1h1M13 1h1M1 3h11M13 ' +
          '3h2M1 4h11M13 4h2M1 5h11M13 5h2M1 6h11M13 6h2M1 7h11M13 7h2M1 8h' +
          '11M13 8h2M1 9h11M13 9h2M1 10h11M13 10h2M1 12h1M1 14h14" />'#13#10'<pat' +
          'h stroke="#00007a" d="M2 1h1M4 1h2M7 1h2M10 1h1M14 1h1M1 2h11M13' +
          ' 2h2M2 12h1M4 12h11M1 13h2M4 13h2M7 13h2M10 13h3M14 13h1" />'#13#10'<p' +
          'ath stroke="#00009a" d="M3 1h1M6 1h1M9 1h1M11 1h1M3 12h1M3 13h1M' +
          '6 13h1M9 13h1M13 13h1" />'#13#10'</svg>'
      end>
    Left = 176
    Top = 229
  end
end
