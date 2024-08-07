object FFileStructure: TFFileStructure
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Structure'
  ClientHeight = 383
  ClientWidth = 223
  Color = clBtnFace
  UseDockManager = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseActivate = FormMouseActivate
  OnShow = FormShow
  TextHeight = 15
  object TVFileStructure: TTreeView
    Left = 0
    Top = 0
    Width = 223
    Height = 383
    Align = alClient
    Indent = 19
    ParentShowHint = False
    ShowButtons = False
    ShowHint = True
    ShowLines = False
    ShowRoot = False
    TabOrder = 0
    OnClick = TVFileStructureClick
    OnKeyPress = TVFileStructureKeyPress
    OnMouseDown = TVFileStructureMouseDown
  end
  object PMFileStructure: TSpTBXPopupMenu
    Images = vilFileStructureLight
    Left = 48
    Top = 24
    object MIDefaulLayout: TSpTBXItem
      Caption = 'Default layout'
      ImageIndex = 20
      ImageName = 'DefaultLayout'
      OnClick = MIDefaulLayoutClick
    end
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 19
      ImageName = 'Font'
      OnClick = MIFontClick
    end
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 18
      ImageName = 'Close'
      OnClick = MICloseClick
    end
  end
  object icFileStructure: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Light\Empty'
        SVGText = '<svg viewBox="0 -0.5 16 16">'#13#10'</svg>'
      end
      item
        IconName = 'EditClass'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#10'<path stroke="#e1e1e1e1" d="M2 1h11' +
          'M1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h' +
          '1M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M1' +
          '3 11h1M1 12h1M13 12h1M2 13h12" />'#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5h' +
          '1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3M' +
          '12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#10'<path st' +
          'roke="#468dcd" d="M5 4h1M5 11h1" />'#10'<path stroke="#a0c5e6" d="M6' +
          ' 4h1" />'#10'<path stroke="#f2f7fb" d="M7 4h2" />'#10'<path stroke="#cde' +
          '1f1" d="M9 4h1M9 11h1" />'#10'<path stroke="#5497d1" d="M10 4h1M10 1' +
          '1h1" />'#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#10'<path stroke' +
          '="#b2d0ea" d="M6 5h1M6 10h1" />'#10'<path stroke="#488fce" d="M7 5h1' +
          'M7 10h1" />'#10'<path stroke="#73aada" d="M9 5h1" />'#10'<path stroke="#' +
          'e8f0f8" d="M10 5h1M10 10h1" />'#10'<path stroke="#438bcc" d="M11 5h1' +
          '" />'#10'<path stroke="#609ed4" d="M4 6h1" />'#10'<path stroke="#f8fafc"' +
          ' d="M5 6h1M5 9h1M7 11h2" />'#10'<path stroke="#4990ce" d="M6 6h1M6 9' +
          'h1M11 10h1" />'#10'<path stroke="#70a8da" d="M10 6h1" />'#10'<path strok' +
          'e="#5094d0" d="M11 6h1" />'#10'<path stroke="#7eb1dd" d="M4 7h1" />'#10 +
          '<path stroke="#d0e3f2" d="M5 7h1M5 8h1" />'#10'<path stroke="#77acdb' +
          '" d="M4 8h1" />'#10'<path stroke="#5d9cd4" d="M4 9h1" />'#10'<path strok' +
          'e="#94bee3" d="M10 9h1" />'#10'<path stroke="#6da6d8" d="M11 9h1M9 1' +
          '0h1" />'#10'<path stroke="#aacbe9" d="M6 11h1" />'#10'</svg>'
      end
      item
        IconName = 'Light\PrivateAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h9M3' +
          ' 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<pa' +
          'th stroke="#973636" d="M12 3h1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h' +
          '1M12 9h1M12 10h1M12 11h1M4 12h9" />'#13#10'<path stroke="#fe6536" d="M' +
          '4 4h8M4 5h8M4 6h8M4 7h1M11 7h1M4 8h1M11 8h1M4 9h8M4 10h8M4 11h8"' +
          ' />'#13#10'<path stroke="#ffffff" d="M5 7h6M5 8h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\PackageAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h9M3' +
          ' 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<pa' +
          'th stroke="#973636" d="M12 3h1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h' +
          '1M12 9h1M12 10h1M12 11h1M4 12h9" />'#13#10'<path stroke="#fe6536" d="M' +
          '4 4h8M4 5h8M4 6h8M4 7h8M4 8h2M8 8h2M11 8h1M4 9h1M6 9h2M10 9h2M4 ' +
          '10h8M4 11h8" />'#13#10'<path stroke="#ffffff" d="M6 8h2M10 8h1M5 9h1M8' +
          ' 9h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\ProtectedAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h9M3' +
          ' 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<pa' +
          'th stroke="#973636" d="M12 3h1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h' +
          '1M12 9h1M12 10h1M12 11h1M4 12h9" />'#13#10'<path stroke="#fe6536" d="M' +
          '4 4h8M4 5h2M7 5h2M10 5h2M4 6h1M11 6h1M4 7h2M7 7h2M10 7h2M4 8h2M7' +
          ' 8h2M10 8h2M4 9h1M11 9h1M4 10h2M7 10h2M10 10h2M4 11h8" />'#13#10'<path' +
          ' stroke="#ffffff" d="M6 5h1M9 5h1M5 6h6M6 7h1M9 7h1M6 8h1M9 8h1M' +
          '5 9h6M6 10h1M9 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\PublicAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h9M3' +
          ' 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<pa' +
          'th stroke="#973636" d="M12 3h1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h' +
          '1M12 9h1M12 10h1M12 11h1M4 12h9" />'#13#10'<path stroke="#fe6536" d="M' +
          '4 4h8M4 5h3M9 5h3M4 6h3M9 6h3M4 7h1M11 7h1M4 8h1M11 8h1M4 9h3M9 ' +
          '9h3M4 10h3M9 10h3M4 11h8" />'#13#10'<path stroke="#ffffff" d="M7 5h2M7' +
          ' 6h2M5 7h6M5 8h6M7 9h2M7 10h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Class'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'  <pat' +
          'h d="M 10.5, 5'#13#10'           C 8.5, 3, 4.5, 4, 4.5, 8'#13#10'           ' +
          'C 4.5, 12, 8.5, 13, 10.5, 11"'#13#10#13#10'        fill="none" stroke="whi' +
          'te" stroke-width="1.5"/></svg>'#13#10
      end
      item
        IconName = 'PrivateMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'    <p' +
          'olygon points="5,7.5 11,7.5 11,8.5 5,8.5 " fill="none" stroke="w' +
          'hite" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PackageMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'<text ' +
          'x="3.6" y="12" fill="white">~</text>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ProtectedMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'<path ' +
          'stroke="white" d="M4.5 6 h7 M4.5 10 h7 M6 4.5 v7 M10 4.5 v7" />'#13 +
          #10'</svg>'
      end
      item
        IconName = 'PublicMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'    <p' +
          'olygon points="5,7.5 11,7.5 11,8.5 5,8.5 " fill="none" stroke="w' +
          'hite" />'#13#10'    <polygon points="7.5,5 8.5,5 8.5,11 7.5,11 " fill=' +
          '"none" stroke="white" />'#13#10'</svg>'
      end
      item
        IconName = 'Light\Interface'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#feca36" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#ffffca" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#ffff65" d="M3 3h10M3 4h10M3 5h3M9 5h4M3 6' +
          'h4M8 6h5M3 7h4M8 7h5M3 8h4M8 8h5M3 9h4M8 9h5M3 10h3M9 10h4M3 11h' +
          '10M3 12h10" />'#13#10'<path stroke="#000000" d="M6 5h3M7 6h1M7 7h1M7 8' +
          'h1M7 9h1M6 10h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\Attribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#d63a3a" d="M2 2h11M' +
          '2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12h1M2' +
          ' 13h1" />'#13#10'<path stroke="#963a3a" d="M13 2h1M13 3h1M13 4h1M13 5h' +
          '1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1M3 13h11" /' +
          '>'#13#10'<path stroke="#d6583a" d="M3 3h1M5 3h1M7 3h1M9 3h1M11 3h1M4 4' +
          'h1M6 4h1M8 4h1M10 4h1M12 4h1M3 5h1M5 5h1M7 5h1M9 5h1M11 5h1M4 6h' +
          '1M6 6h1M8 6h1M10 6h1M12 6h1M3 7h1M5 7h1M7 7h1M9 7h1M11 7h1M4 8h1' +
          'M6 8h1M8 8h1M10 8h1M12 8h1M3 9h1M5 9h1M7 9h1M9 9h1M11 9h1M4 10h1' +
          'M6 10h1M8 10h1M10 10h1M12 10h1M3 11h1M5 11h1M7 11h1M9 11h1M11 11' +
          'h1M4 12h1M6 12h1M8 12h1M10 12h1M12 12h1" />'#13#10'<path stroke="#fe77' +
          '3a" d="M4 3h1M6 3h1M8 3h1M10 3h1M12 3h1M3 4h1M5 4h1M7 4h1M9 4h1M' +
          '11 4h1M4 5h1M6 5h1M8 5h1M10 5h1M12 5h1M3 6h1M5 6h1M7 6h1M9 6h1M1' +
          '1 6h1M4 7h1M6 7h1M8 7h1M10 7h1M12 7h1M3 8h1M5 8h1M7 8h1M9 8h1M11' +
          ' 8h1M4 9h1M6 9h1M8 9h1M10 9h1M12 9h1M3 10h1M5 10h1M7 10h1M9 10h1' +
          'M11 10h1M4 11h1M6 11h1M8 11h1M10 11h1M12 11h1M3 12h1M5 12h1M7 12' +
          'h1M9 12h1M11 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\Method'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'Light\Local'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#676767" d="M3 2h10M' +
          '3 3h2M7 3h6M3 4h2M7 4h6M3 5h2M7 5h6M3 6h2M7 6h6M3 7h2M7 7h6M3 8h' +
          '2M7 8h6M3 9h2M7 9h6M3 10h2M11 10h2M3 11h2M11 11h2M3 12h10" />'#13#10'<' +
          'path stroke="#ffffff" d="M5 3h2M5 4h2M5 5h2M5 6h2M5 7h2M5 8h2M5 ' +
          '9h2M5 10h6M5 11h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\Parameter'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#676767" d="M3 2h10M' +
          '3 3h2M10 3h3M3 4h2M7 4h2M11 4h2M3 5h2M7 5h3M12 5h1M3 6h2M7 6h2M1' +
          '1 6h2M3 7h2M10 7h3M3 8h2M7 8h6M3 9h2M7 9h6M3 10h2M7 10h6M3 11h2M' +
          '7 11h6M3 12h10" />'#13#10'<path stroke="#ffffff" d="M5 3h4M5 4h2M9 4h1' +
          'M5 5h2M10 5h1M5 6h2M9 6h1M5 7h4M5 8h2M5 9h2M5 10h2M5 11h2" />'#13#10'<' +
          'path stroke="#979797" d="M9 3h1M10 4h1M11 5h1M10 6h1M9 7h1" />'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'Light\Unknown'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#bd9b5e" d="M2 1h1M2' +
          ' 2h1M2 3h1" />'#13#10'<path stroke="#b59356" d="M3 1h11M13 2h1M13 3h1"' +
          ' />'#13#10'<path stroke="#dac378" d="M3 2h1" />'#13#10'<path stroke="#e5d481' +
          '" d="M4 2h1" />'#13#10'<path stroke="#e6d481" d="M5 2h1" />'#13#10'<path str' +
          'oke="#e6d581" d="M6 2h1" />'#13#10'<path stroke="#e7d581" d="M7 2h1" /' +
          '>'#13#10'<path stroke="#e7d582" d="M8 2h1" />'#13#10'<path stroke="#e8d582" ' +
          'd="M9 2h1" />'#13#10'<path stroke="#e9d682" d="M10 2h1" />'#13#10'<path stro' +
          'ke="#e9d683" d="M11 2h1" />'#13#10'<path stroke="#e3c97c" d="M12 2h1" ' +
          '/>'#13#10'<path stroke="#dbc478" d="M3 3h1" />'#13#10'<path stroke="#936e88"' +
          ' d="M4 3h1" />'#13#10'<path stroke="#927087" d="M5 3h1" />'#13#10'<path stro' +
          'ke="#907184" d="M6 3h1" />'#13#10'<path stroke="#8f7383" d="M7 3h1" />' +
          #13#10'<path stroke="#8e7582" d="M8 3h1" />'#13#10'<path stroke="#8d777f" d' +
          '="M9 3h1" />'#13#10'<path stroke="#8c797e" d="M10 3h1" />'#13#10'<path strok' +
          'e="#897c7b" d="M11 3h1" />'#13#10'<path stroke="#e4ca7d" d="M12 3h1" /' +
          '>'#13#10'<path stroke="#bc9c61" d="M2 4h1" />'#13#10'<path stroke="#ccac6c" ' +
          'd="M3 4h1" />'#13#10'<path stroke="#cdac6c" d="M4 4h1" />'#13#10'<path strok' +
          'e="#cdad6c" d="M5 4h1" />'#13#10'<path stroke="#cead6c" d="M6 4h1" />'#13 +
          #10'<path stroke="#cfae6d" d="M7 4h2" />'#13#10'<path stroke="#d0ae6d" d=' +
          '"M9 4h1" />'#13#10'<path stroke="#d1af6e" d="M10 4h1" />'#13#10'<path stroke' +
          '="#d2af6e" d="M11 4h2" />'#13#10'<path stroke="#b39358" d="M13 4h1" />' +
          #13#10'<path stroke="#9bafc4" d="M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h' +
          '1M2 11h1M2 12h1M2 13h1" />'#13#10'<path stroke="#ddeef0" d="M3 5h1M3 6' +
          'h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<path stroke="#e0' +
          'eff1" d="M4 5h1M4 6h1M4 8h1M4 10h1M4 12h1" />'#13#10'<path stroke="#e3' +
          'f1f2" d="M5 5h1M5 6h1M5 8h1M5 10h1M5 12h1" />'#13#10'<path stroke="#e6' +
          'f2f3" d="M6 5h1M6 6h1M6 8h1M6 10h1M6 12h1" />'#13#10'<path stroke="#e9' +
          'f4f5" d="M7 5h1M7 6h1M7 8h1M7 10h1M7 12h1" />'#13#10'<path stroke="#eb' +
          'f5f6" d="M8 5h1M8 6h1M8 8h1M8 10h1M8 12h1" />'#13#10'<path stroke="#ee' +
          'f6f7" d="M9 5h1M9 6h1M9 8h1M9 10h1M9 11h1M9 12h1" />'#13#10'<path stro' +
          'ke="#f1f8f8" d="M10 5h1M10 6h1M10 8h1M10 10h1M10 11h1M10 12h1" /' +
          '>'#13#10'<path stroke="#f4f9fa" d="M11 5h1M11 6h1M11 8h1M11 10h1M11 11' +
          'h1M11 12h1" />'#13#10'<path stroke="#f7fbfb" d="M12 5h1M12 6h1M12 7h1M' +
          '12 8h1M12 9h1M12 10h1M12 11h1M12 12h1" />'#13#10'<path stroke="#788ba0' +
          '" d="M13 5h1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1' +
          'M13 13h1" />'#13#10'<path stroke="#9db1c6" d="M4 7h1" />'#13#10'<path stroke' +
          '="#99adc2" d="M5 7h1" />'#13#10'<path stroke="#96aabf" d="M6 7h1" />'#13#10 +
          '<path stroke="#93a7bc" d="M7 7h1" />'#13#10'<path stroke="#90a4b9" d="' +
          'M8 7h1" />'#13#10'<path stroke="#8ca0b5" d="M9 7h1" />'#13#10'<path stroke="' +
          '#899db2" d="M10 7h1" />'#13#10'<path stroke="#869aaf" d="M11 7h1" />'#13#10 +
          '<path stroke="#a5b9cb" d="M4 9h1M4 11h1" />'#13#10'<path stroke="#a3b5' +
          'c7" d="M5 9h1" />'#13#10'<path stroke="#a0b2c6" d="M6 9h1" />'#13#10'<path s' +
          'troke="#9db1c3" d="M7 9h1" />'#13#10'<path stroke="#9caec1" d="M8 9h1"' +
          ' />'#13#10'<path stroke="#98abbd" d="M9 9h1" />'#13#10'<path stroke="#96a8ba' +
          '" d="M10 9h1" />'#13#10'<path stroke="#94a5b8" d="M11 9h1" />'#13#10'<path s' +
          'troke="#a1b3c6" d="M5 11h1" />'#13#10'<path stroke="#9cafc2" d="M6 11h' +
          '1" />'#13#10'<path stroke="#98acbe" d="M7 11h1" />'#13#10'<path stroke="#95a' +
          '7ba" d="M8 11h1" />'#13#10'<path stroke="#c2d4dd" d="M3 13h1" />'#13#10'<pat' +
          'h stroke="#c5d5dd" d="M4 13h1" />'#13#10'<path stroke="#c7d6de" d="M5 ' +
          '13h1" />'#13#10'<path stroke="#c9d7df" d="M6 13h1" />'#13#10'<path stroke="#' +
          'cbd9e0" d="M7 13h1" />'#13#10'<path stroke="#cdd9e1" d="M8 13h1" />'#13#10'<' +
          'path stroke="#cfdbe2" d="M9 13h1" />'#13#10'<path stroke="#d1dde3" d="' +
          'M10 13h1" />'#13#10'<path stroke="#d4dde4" d="M11 13h1" />'#13#10'<path stro' +
          'ke="#d6dfe5" d="M12 13h1" />'#13#10'<path stroke="#8698ab" d="M3 14h1"' +
          ' />'#13#10'<path stroke="#8798ab" d="M4 14h3" />'#13#10'<path stroke="#8798a' +
          'c" d="M7 14h1" />'#13#10'<path stroke="#8799ac" d="M8 14h4" />'#13#10'<path ' +
          'stroke="#8899ac" d="M12 14h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\Package'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#83675e" d="M7 1h1M1' +
          '3 7h1" />'#13#10'<path stroke="#c18862" d="M2 2h1" />'#13#10'<path stroke="#' +
          'c08762" d="M3 2h1M5 2h2M8 2h1M10 2h2" />'#13#10'<path stroke="#c08862"' +
          ' d="M4 2h1M9 2h1" />'#13#10'<path stroke="#6a4436" d="M7 2h1" />'#13#10'<pat' +
          'h stroke="#c58f69" d="M12 2h1" />'#13#10'<path stroke="#bb8362" d="M2 ' +
          '3h1" />'#13#10'<path stroke="#cfac7d" d="M3 3h1" />'#13#10'<path stroke="#d5' +
          'b483" d="M4 3h1" />'#13#10'<path stroke="#d1ad7e" d="M5 3h1M10 3h1" />' +
          #13#10'<path stroke="#c59869" d="M6 3h1" />'#13#10'<path stroke="#865a44" d' +
          '="M7 3h1" />'#13#10'<path stroke="#cfac7c" d="M8 3h1" />'#13#10'<path stroke' +
          '="#d6b483" d="M9 3h1" />'#13#10'<path stroke="#c39669" d="M11 3h1" />'#13 +
          #10'<path stroke="#bc8360" d="M12 3h1" />'#13#10'<path stroke="#b77e5f" d' +
          '="M2 4h1" />'#13#10'<path stroke="#e0c898" d="M3 4h1" />'#13#10'<path stroke' +
          '="#ead6a4" d="M4 4h1M9 4h1M4 9h1M9 9h1" />'#13#10'<path stroke="#e2ca9' +
          'a" d="M5 4h1M5 9h1" />'#13#10'<path stroke="#cda979" d="M6 4h1" />'#13#10'<p' +
          'ath stroke="#a17155" d="M7 4h1" />'#13#10'<path stroke="#e0c797" d="M8' +
          ' 4h1M8 9h1" />'#13#10'<path stroke="#e2cb9a" d="M10 4h1M10 9h1" />'#13#10'<p' +
          'ath stroke="#caa476" d="M11 4h1" />'#13#10'<path stroke="#b97f5d" d="M' +
          '12 4h1" />'#13#10'<path stroke="#b37b5c" d="M2 5h1" />'#13#10'<path stroke="' +
          '#d8bd8e" d="M3 5h1M8 5h1M3 10h1M8 10h1" />'#13#10'<path stroke="#e1c99' +
          '9" d="M4 5h1M9 5h1M4 10h1M9 10h1" />'#13#10'<path stroke="#dabf90" d="' +
          'M5 5h1M5 10h1" />'#13#10'<path stroke="#c8a274" d="M6 5h1" />'#13#10'<path s' +
          'troke="#91634b" d="M7 5h1" />'#13#10'<path stroke="#dac091" d="M10 5h1' +
          'M10 10h1" />'#13#10'<path stroke="#c69e71" d="M11 5h1" />'#13#10'<path strok' +
          'e="#b57c5a" d="M12 5h1" />'#13#10'<path stroke="#af7659" d="M2 6h1" />' +
          #13#10'<path stroke="#c6a177" d="M3 6h1M3 11h1M8 11h1" />'#13#10'<path stro' +
          'ke="#caa87d" d="M4 6h1M9 6h1" />'#13#10'<path stroke="#c6a278" d="M5 6' +
          'h1" />'#13#10'<path stroke="#bb8f64" d="M6 6h1" />'#13#10'<path stroke="#805' +
          '542" d="M7 6h1" />'#13#10'<path stroke="#c5a177" d="M8 6h1" />'#13#10'<path ' +
          'stroke="#c7a378" d="M10 6h1" />'#13#10'<path stroke="#ba8c62" d="M11 6' +
          'h1" />'#13#10'<path stroke="#b27858" d="M12 6h1" />'#13#10'<path stroke="#72' +
          '5248" d="M1 7h1" />'#13#10'<path stroke="#694236" d="M2 7h1" />'#13#10'<path' +
          ' stroke="#835742" d="M3 7h1" />'#13#10'<path stroke="#9d6d52" d="M4 7h' +
          '1" />'#13#10'<path stroke="#8d5f48" d="M5 7h1" />'#13#10'<path stroke="#7d51' +
          '3f" d="M6 7h1M8 7h1" />'#13#10'<path stroke="#795042" d="M7 7h1" />'#13#10'<' +
          'path stroke="#7e5240" d="M9 7h1" />'#13#10'<path stroke="#8d6049" d="M' +
          '10 7h1M7 10h1" />'#13#10'<path stroke="#9c6c52" d="M11 7h1" />'#13#10'<path ' +
          'stroke="#684234" d="M12 7h1" />'#13#10'<path stroke="#9d664f" d="M2 8h' +
          '1" />'#13#10'<path stroke="#caac81" d="M3 8h1" />'#13#10'<path stroke="#d4b8' +
          '8c" d="M4 8h1" />'#13#10'<path stroke="#ccae84" d="M5 8h1" />'#13#10'<path s' +
          'troke="#ba916a" d="M6 8h1" />'#13#10'<path stroke="#7c5140" d="M7 8h1"' +
          ' />'#13#10'<path stroke="#c9ab80" d="M8 8h1" />'#13#10'<path stroke="#cfb489' +
          '" d="M9 8h1" />'#13#10'<path stroke="#ceaf84" d="M10 8h1" />'#13#10'<path st' +
          'roke="#bc9169" d="M11 8h1" />'#13#10'<path stroke="#9d664d" d="M12 8h1' +
          '" />'#13#10'<path stroke="#a26950" d="M2 9h1" />'#13#10'<path stroke="#e0c89' +
          '7" d="M3 9h1" />'#13#10'<path stroke="#caa577" d="M6 9h1" />'#13#10'<path st' +
          'roke="#7d5140" d="M7 9h1" />'#13#10'<path stroke="#c59d72" d="M11 9h1"' +
          ' />'#13#10'<path stroke="#a36a4e" d="M12 9h1" />'#13#10'<path stroke="#9e644' +
          'd" d="M2 10h1" />'#13#10'<path stroke="#c59e72" d="M6 10h1" />'#13#10'<path ' +
          'stroke="#c0986e" d="M11 10h1" />'#13#10'<path stroke="#a0654b" d="M12 ' +
          '10h1" />'#13#10'<path stroke="#9a614a" d="M2 11h1" />'#13#10'<path stroke="#' +
          'cba97e" d="M4 11h1M9 11h1" />'#13#10'<path stroke="#c7a379" d="M5 11h1' +
          'M10 11h1" />'#13#10'<path stroke="#b98b62" d="M6 11h1" />'#13#10'<path strok' +
          'e="#9d6d53" d="M7 11h1" />'#13#10'<path stroke="#b68560" d="M11 11h1" ' +
          '/>'#13#10'<path stroke="#9c6249" d="M12 11h1" />'#13#10'<path stroke="#975c4' +
          '5" d="M2 12h1" />'#13#10'<path stroke="#955c46" d="M3 12h4M8 12h4" />'#13 +
          #10'<path stroke="#805645" d="M7 12h1" />'#13#10'<path stroke="#9b6147" d' +
          '="M12 12h1" />'#13#10'<path stroke="#715045" d="M7 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\Empty'
        SVGText = '<svg viewBox="0 -0.5 16 16">'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\ClassEdit'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5' +
          'h1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3' +
          'M12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#13#10'<path ' +
          'stroke="#468dcd" d="M5 4h1M5 11h1" />'#13#10'<path stroke="#a0c5e6" d=' +
          '"M6 4h1" />'#13#10'<path stroke="#f2f7fb" d="M7 4h1" />'#13#10'<path stroke=' +
          '"#cde1f1" d="M8 4h2M8 11h2" />'#13#10'<path stroke="#5497d1" d="M10 4h' +
          '1M10 11h1" />'#13#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#13#10'<pat' +
          'h stroke="#b2d0ea" d="M6 5h1M6 10h1" />'#13#10'<path stroke="#488fce" ' +
          'd="M7 5h1M7 10h1" />'#13#10'<path stroke="#73aada" d="M9 5h1" />'#13#10'<pat' +
          'h stroke="#e8f0f8" d="M10 5h1M10 10h1" />'#13#10'<path stroke="#438bcc' +
          '" d="M11 5h1" />'#13#10'<path stroke="#609ed4" d="M4 6h1" />'#13#10'<path st' +
          'roke="#f8fafc" d="M5 6h1M5 9h1M7 11h1" />'#13#10'<path stroke="#4990ce' +
          '" d="M6 6h1M6 9h1M11 10h1" />'#13#10'<path stroke="#70a8da" d="M10 6h1' +
          '" />'#13#10'<path stroke="#5094d0" d="M11 6h1" />'#13#10'<path stroke="#7eb1' +
          'dd" d="M4 7h1" />'#13#10'<path stroke="#d0e3f2" d="M5 7h1M5 8h1" />'#13#10'<' +
          'path stroke="#77acdb" d="M4 8h1" />'#13#10'<path stroke="#5d9cd4" d="M' +
          '4 9h1" />'#13#10'<path stroke="#94bee3" d="M10 9h1" />'#13#10'<path stroke="' +
          '#6da6d8" d="M11 9h1M9 10h1" />'#13#10'<path stroke="#aacbe9" d="M6 11h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\PrivateAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h10M' +
          '3 4h1M12 4h1M3 5h1M12 5h1M3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1' +
          'M3 9h1M12 9h1M3 10h1M12 10h1M3 11h1M12 11h1M3 12h10" />'#13#10'<path s' +
          'troke="#fe6536" d="M4 4h8M4 5h8M4 6h8M4 7h1M11 7h1M4 8h1M11 8h1M' +
          '4 9h8M4 10h8M4 11h8" />'#13#10'<path stroke="#ffffff" d="M5 7h6M5 8h6"' +
          ' />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\PackageAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h10M' +
          '3 4h1M12 4h1M3 5h1M12 5h1M3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1' +
          'M3 9h1M12 9h1M3 10h1M12 10h1M3 11h1M12 11h1M3 12h10" />'#13#10'<path s' +
          'troke="#fe6536" d="M4 4h8M4 5h8M4 6h8M4 7h8M4 8h2M8 8h2M11 8h1M4' +
          ' 9h1M6 9h2M10 9h2M4 10h8M4 11h8" />'#13#10'<path stroke="#ffffff" d="M' +
          '6 8h2M10 8h1M5 9h1M8 9h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\ProtectedAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h10M' +
          '3 4h1M12 4h1M3 5h1M12 5h1M3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1' +
          'M3 9h1M12 9h1M3 10h1M12 10h1M3 11h1M12 11h1M3 12h10" />'#13#10'<path s' +
          'troke="#fe6536" d="M4 4h8M4 5h2M7 5h2M10 5h2M4 6h1M11 6h1M4 7h2M' +
          '7 7h2M10 7h2M4 8h2M7 8h2M10 8h2M4 9h1M11 9h1M4 10h2M7 10h2M10 10' +
          'h2M4 11h8" />'#13#10'<path stroke="#ffffff" d="M6 5h1M9 5h1M5 6h6M6 7h' +
          '1M9 7h1M6 8h1M9 8h1M5 9h6M6 10h1M9 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\PublicAttribute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ca3636" d="M3 3h10M' +
          '3 4h1M12 4h1M3 5h1M12 5h1M3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1' +
          'M3 9h1M12 9h1M3 10h1M12 10h1M3 11h1M12 11h1M3 12h10" />'#13#10'<path s' +
          'troke="#fe6536" d="M4 4h8M4 5h3M9 5h3M4 6h3M9 6h3M4 7h1M11 7h1M4' +
          ' 8h1M11 8h1M4 9h3M9 9h3M4 10h3M9 10h3M4 11h8" />'#13#10'<path stroke="' +
          '#ffffff" d="M7 5h2M7 6h2M5 7h6M5 8h6M7 9h2M7 10h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Class'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'  <pat' +
          'h d="M 10.5, 5'#13#10'           C 8.5, 3, 4.5, 4, 4.5, 8'#13#10'           ' +
          'C 4.5, 12, 8.5, 13, 10.5, 11"'#13#10#13#10'        fill="none" stroke="whi' +
          'te" stroke-width="1.5"/></svg>'#13#10
      end
      item
        IconName = 'PrivateMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'    <p' +
          'olygon points="5,7.5 11,7.5 11,8.5 5,8.5 " fill="none" stroke="w' +
          'hite" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'PackageMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'<text ' +
          'x="3.6" y="12" fill="white">~</text>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ProtectedMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'<path ' +
          'stroke="white" d="M4.5 6 h7 M4.5 10 h7 M6 4.5 v7 M10 4.5 v7" />'#13 +
          #10'</svg>'
      end
      item
        IconName = 'PublicMethod'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'    <p' +
          'olygon points="5,7.5 11,7.5 11,8.5 5,8.5 " fill="none" stroke="w' +
          'hite" />'#13#10'    <polygon points="7.5,5 8.5,5 8.5,11 7.5,11 " fill=' +
          '"none" stroke="white" />'#13#10'</svg>'
      end
      item
        IconName = 'Dark\Interface'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#feca36" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#ffffca" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#ffff65" d="M3 3h10M3 4h10M3 5h3M9 5h4M3 6' +
          'h4M8 6h5M3 7h4M8 7h5M3 8h4M8 8h5M3 9h4M8 9h5M3 10h3M9 10h4M3 11h' +
          '10M3 12h10" />'#13#10'<path stroke="#000000" d="M6 5h3M7 6h1M7 7h1M7 8' +
          'h1M7 9h1M6 10h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#d63a3a" d="M2 2h11M' +
          '2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12h1M2' +
          ' 13h1" />'#13#10'<path stroke="#ca3636" d="M13 2h1M13 3h1M13 4h1M13 5h' +
          '1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1M3 13h11" /' +
          '>'#13#10'<path stroke="#d6583a" d="M3 3h1M5 3h1M7 3h1M9 3h1M11 3h1M4 4' +
          'h1M6 4h1M8 4h1M10 4h1M12 4h1M3 5h1M5 5h1M7 5h1M9 5h1M11 5h1M4 6h' +
          '1M6 6h1M8 6h1M10 6h1M12 6h1M3 7h1M5 7h1M7 7h1M9 7h1M11 7h1M4 8h1' +
          'M6 8h1M8 8h1M10 8h1M12 8h1M3 9h1M5 9h1M7 9h1M9 9h1M11 9h1M4 10h1' +
          'M6 10h1M8 10h1M10 10h1M12 10h1M3 11h1M5 11h1M7 11h1M9 11h1M11 11' +
          'h1M4 12h1M6 12h1M8 12h1M10 12h1M12 12h1" />'#13#10'<path stroke="#fe77' +
          '3a" d="M4 3h1M6 3h1M8 3h1M10 3h1M12 3h1M3 4h1M5 4h1M7 4h1M9 4h1M' +
          '11 4h1M4 5h1M6 5h1M8 5h1M10 5h1M12 5h1M3 6h1M5 6h1M7 6h1M9 6h1M1' +
          '1 6h1M4 7h1M6 7h1M8 7h1M10 7h1M12 7h1M3 8h1M5 8h1M7 8h1M9 8h1M11' +
          ' 8h1M4 9h1M6 9h1M8 9h1M10 9h1M12 9h1M3 10h1M5 10h1M7 10h1M9 10h1' +
          'M11 10h1M4 11h1M6 11h1M8 11h1M10 11h1M12 11h1M3 12h1M5 12h1M7 12' +
          'h1M9 12h1M11 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'<circle cx="8" cy="8" r="7" fill="#3a' +
          '7758"/>'#13#10'<circle cx="8" cy="8" r="5.5" fill="#3b963a"/> '#13#10'</svg>'
      end
      item
        IconName = 'Dark\Local'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#aaaaaa" d="M3 2h10M' +
          '3 3h2M7 3h6M3 4h2M7 4h6M3 5h2M7 5h6M3 6h2M7 6h6M3 7h2M7 7h6M3 8h' +
          '2M7 8h6M3 9h2M7 9h6M3 10h2M11 10h2M3 11h2M11 11h2M3 12h10" />'#13#10'<' +
          'path stroke="#ffffff" d="M5 3h2M5 4h2M5 5h2M5 6h2M5 7h2M5 8h2M5 ' +
          '9h2M5 10h6M5 11h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#aaaaaa" d="M3 2h10M' +
          '3 3h2M10 3h3M3 4h2M7 4h2M11 4h2M3 5h2M7 5h3M12 5h1M3 6h2M7 6h2M1' +
          '1 6h2M3 7h2M10 7h3M3 8h2M7 8h6M3 9h2M7 9h6M3 10h2M7 10h6M3 11h2M' +
          '7 11h6M3 12h10" />'#13#10'<path stroke="#ffffff" d="M5 3h4M5 4h2M9 4h1' +
          'M5 5h2M10 5h1M5 6h2M9 6h1M5 7h4M5 8h2M5 9h2M5 10h2M5 11h2" />'#13#10'<' +
          'path stroke="#dcdcdc" d="M9 3h1M10 4h1M11 5h1M10 6h1M9 7h1" />'#13#10 +
          '</svg>'#13#10
      end
      item
        IconName = 'Dark\'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#bd9b5e" d="M2 1h1M2' +
          ' 2h1M2 3h1" />'#13#10'<path stroke="#b59356" d="M3 1h11M13 2h1M13 3h1"' +
          ' />'#13#10'<path stroke="#dac378" d="M3 2h1" />'#13#10'<path stroke="#e5d481' +
          '" d="M4 2h1" />'#13#10'<path stroke="#e6d481" d="M5 2h1" />'#13#10'<path str' +
          'oke="#e6d581" d="M6 2h1" />'#13#10'<path stroke="#e7d581" d="M7 2h1" /' +
          '>'#13#10'<path stroke="#e7d582" d="M8 2h1" />'#13#10'<path stroke="#e8d582" ' +
          'd="M9 2h1" />'#13#10'<path stroke="#e9d682" d="M10 2h1" />'#13#10'<path stro' +
          'ke="#e9d683" d="M11 2h1" />'#13#10'<path stroke="#e3c97c" d="M12 2h1" ' +
          '/>'#13#10'<path stroke="#dbc478" d="M3 3h1" />'#13#10'<path stroke="#936e88"' +
          ' d="M4 3h1" />'#13#10'<path stroke="#927087" d="M5 3h1" />'#13#10'<path stro' +
          'ke="#907184" d="M6 3h1" />'#13#10'<path stroke="#8f7383" d="M7 3h1" />' +
          #13#10'<path stroke="#8e7582" d="M8 3h1" />'#13#10'<path stroke="#8d777f" d' +
          '="M9 3h1" />'#13#10'<path stroke="#8c797e" d="M10 3h1" />'#13#10'<path strok' +
          'e="#897c7b" d="M11 3h1" />'#13#10'<path stroke="#e4ca7d" d="M12 3h1" /' +
          '>'#13#10'<path stroke="#bc9c61" d="M2 4h1" />'#13#10'<path stroke="#ccac6c" ' +
          'd="M3 4h1" />'#13#10'<path stroke="#cdac6c" d="M4 4h1" />'#13#10'<path strok' +
          'e="#cdad6c" d="M5 4h1" />'#13#10'<path stroke="#cead6c" d="M6 4h1" />'#13 +
          #10'<path stroke="#cfae6d" d="M7 4h2" />'#13#10'<path stroke="#d0ae6d" d=' +
          '"M9 4h1" />'#13#10'<path stroke="#d1af6e" d="M10 4h1" />'#13#10'<path stroke' +
          '="#d2af6e" d="M11 4h2" />'#13#10'<path stroke="#b39358" d="M13 4h1" />' +
          #13#10'<path stroke="#9bafc4" d="M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h' +
          '1M2 11h1M2 12h1M2 13h1" />'#13#10'<path stroke="#ddeef0" d="M3 5h1M3 6' +
          'h1M3 7h1M3 8h1M3 9h1M3 10h1M3 11h1M3 12h1" />'#13#10'<path stroke="#e0' +
          'eff1" d="M4 5h1M4 6h1M4 8h1M4 10h1M4 12h1" />'#13#10'<path stroke="#e3' +
          'f1f2" d="M5 5h1M5 6h1M5 8h1M5 10h1M5 12h1" />'#13#10'<path stroke="#e6' +
          'f2f3" d="M6 5h1M6 6h1M6 8h1M6 10h1M6 12h1" />'#13#10'<path stroke="#e9' +
          'f4f5" d="M7 5h1M7 6h1M7 8h1M7 10h1M7 12h1" />'#13#10'<path stroke="#eb' +
          'f5f6" d="M8 5h1M8 6h1M8 8h1M8 10h1M8 12h1" />'#13#10'<path stroke="#ee' +
          'f6f7" d="M9 5h1M9 6h1M9 8h1M9 10h1M9 11h1M9 12h1" />'#13#10'<path stro' +
          'ke="#f1f8f8" d="M10 5h1M10 6h1M10 8h1M10 10h1M10 11h1M10 12h1" /' +
          '>'#13#10'<path stroke="#f4f9fa" d="M11 5h1M11 6h1M11 8h1M11 10h1M11 11' +
          'h1M11 12h1" />'#13#10'<path stroke="#f7fbfb" d="M12 5h1M12 6h1M12 7h1M' +
          '12 8h1M12 9h1M12 10h1M12 11h1M12 12h1" />'#13#10'<path stroke="#788ba0' +
          '" d="M13 5h1M13 6h1M13 7h1M13 8h1M13 9h1M13 10h1M13 11h1M13 12h1' +
          'M13 13h1" />'#13#10'<path stroke="#9db1c6" d="M4 7h1" />'#13#10'<path stroke' +
          '="#99adc2" d="M5 7h1" />'#13#10'<path stroke="#96aabf" d="M6 7h1" />'#13#10 +
          '<path stroke="#93a7bc" d="M7 7h1" />'#13#10'<path stroke="#90a4b9" d="' +
          'M8 7h1" />'#13#10'<path stroke="#8ca0b5" d="M9 7h1" />'#13#10'<path stroke="' +
          '#899db2" d="M10 7h1" />'#13#10'<path stroke="#869aaf" d="M11 7h1" />'#13#10 +
          '<path stroke="#a5b9cb" d="M4 9h1M4 11h1" />'#13#10'<path stroke="#a3b5' +
          'c7" d="M5 9h1" />'#13#10'<path stroke="#a0b2c6" d="M6 9h1" />'#13#10'<path s' +
          'troke="#9db1c3" d="M7 9h1" />'#13#10'<path stroke="#9caec1" d="M8 9h1"' +
          ' />'#13#10'<path stroke="#98abbd" d="M9 9h1" />'#13#10'<path stroke="#96a8ba' +
          '" d="M10 9h1" />'#13#10'<path stroke="#94a5b8" d="M11 9h1" />'#13#10'<path s' +
          'troke="#a1b3c6" d="M5 11h1" />'#13#10'<path stroke="#9cafc2" d="M6 11h' +
          '1" />'#13#10'<path stroke="#98acbe" d="M7 11h1" />'#13#10'<path stroke="#95a' +
          '7ba" d="M8 11h1" />'#13#10'<path stroke="#c2d4dd" d="M3 13h1" />'#13#10'<pat' +
          'h stroke="#c5d5dd" d="M4 13h1" />'#13#10'<path stroke="#c7d6de" d="M5 ' +
          '13h1" />'#13#10'<path stroke="#c9d7df" d="M6 13h1" />'#13#10'<path stroke="#' +
          'cbd9e0" d="M7 13h1" />'#13#10'<path stroke="#cdd9e1" d="M8 13h1" />'#13#10'<' +
          'path stroke="#cfdbe2" d="M9 13h1" />'#13#10'<path stroke="#d1dde3" d="' +
          'M10 13h1" />'#13#10'<path stroke="#d4dde4" d="M11 13h1" />'#13#10'<path stro' +
          'ke="#d6dfe5" d="M12 13h1" />'#13#10'<path stroke="#8698ab" d="M3 14h1"' +
          ' />'#13#10'<path stroke="#8798ab" d="M4 14h3" />'#13#10'<path stroke="#8798a' +
          'c" d="M7 14h1" />'#13#10'<path stroke="#8799ac" d="M8 14h4" />'#13#10'<path ' +
          'stroke="#8899ac" d="M12 14h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\Package'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#83675e" d="M7 1h1M1' +
          '3 7h1" />'#13#10'<path stroke="#c18862" d="M2 2h1" />'#13#10'<path stroke="#' +
          'c08762" d="M3 2h1M5 2h2M8 2h1M10 2h2" />'#13#10'<path stroke="#c08862"' +
          ' d="M4 2h1M9 2h1" />'#13#10'<path stroke="#6a4436" d="M7 2h1" />'#13#10'<pat' +
          'h stroke="#c58f69" d="M12 2h1" />'#13#10'<path stroke="#bb8362" d="M2 ' +
          '3h1" />'#13#10'<path stroke="#cfac7d" d="M3 3h1" />'#13#10'<path stroke="#d5' +
          'b483" d="M4 3h1" />'#13#10'<path stroke="#d1ad7e" d="M5 3h1M10 3h1" />' +
          #13#10'<path stroke="#c59869" d="M6 3h1" />'#13#10'<path stroke="#865a44" d' +
          '="M7 3h1" />'#13#10'<path stroke="#cfac7c" d="M8 3h1" />'#13#10'<path stroke' +
          '="#d6b483" d="M9 3h1" />'#13#10'<path stroke="#c39669" d="M11 3h1" />'#13 +
          #10'<path stroke="#bc8360" d="M12 3h1" />'#13#10'<path stroke="#b77e5f" d' +
          '="M2 4h1" />'#13#10'<path stroke="#e0c898" d="M3 4h1" />'#13#10'<path stroke' +
          '="#ead6a4" d="M4 4h1M9 4h1M4 9h1M9 9h1" />'#13#10'<path stroke="#e2ca9' +
          'a" d="M5 4h1M5 9h1" />'#13#10'<path stroke="#cda979" d="M6 4h1" />'#13#10'<p' +
          'ath stroke="#a17155" d="M7 4h1" />'#13#10'<path stroke="#e0c797" d="M8' +
          ' 4h1M8 9h1" />'#13#10'<path stroke="#e2cb9a" d="M10 4h1M10 9h1" />'#13#10'<p' +
          'ath stroke="#caa476" d="M11 4h1" />'#13#10'<path stroke="#b97f5d" d="M' +
          '12 4h1" />'#13#10'<path stroke="#b37b5c" d="M2 5h1" />'#13#10'<path stroke="' +
          '#d8bd8e" d="M3 5h1M8 5h1M3 10h1M8 10h1" />'#13#10'<path stroke="#e1c99' +
          '9" d="M4 5h1M9 5h1M4 10h1M9 10h1" />'#13#10'<path stroke="#dabf90" d="' +
          'M5 5h1M5 10h1" />'#13#10'<path stroke="#c8a274" d="M6 5h1" />'#13#10'<path s' +
          'troke="#91634b" d="M7 5h1" />'#13#10'<path stroke="#dac091" d="M10 5h1' +
          'M10 10h1" />'#13#10'<path stroke="#c69e71" d="M11 5h1" />'#13#10'<path strok' +
          'e="#b57c5a" d="M12 5h1" />'#13#10'<path stroke="#af7659" d="M2 6h1" />' +
          #13#10'<path stroke="#c6a177" d="M3 6h1M3 11h1M8 11h1" />'#13#10'<path stro' +
          'ke="#caa87d" d="M4 6h1M9 6h1" />'#13#10'<path stroke="#c6a278" d="M5 6' +
          'h1" />'#13#10'<path stroke="#bb8f64" d="M6 6h1" />'#13#10'<path stroke="#805' +
          '542" d="M7 6h1" />'#13#10'<path stroke="#c5a177" d="M8 6h1" />'#13#10'<path ' +
          'stroke="#c7a378" d="M10 6h1" />'#13#10'<path stroke="#ba8c62" d="M11 6' +
          'h1" />'#13#10'<path stroke="#b27858" d="M12 6h1" />'#13#10'<path stroke="#72' +
          '5248" d="M1 7h1" />'#13#10'<path stroke="#694236" d="M2 7h1" />'#13#10'<path' +
          ' stroke="#835742" d="M3 7h1" />'#13#10'<path stroke="#9d6d52" d="M4 7h' +
          '1" />'#13#10'<path stroke="#8d5f48" d="M5 7h1" />'#13#10'<path stroke="#7d51' +
          '3f" d="M6 7h1M8 7h1" />'#13#10'<path stroke="#795042" d="M7 7h1" />'#13#10'<' +
          'path stroke="#7e5240" d="M9 7h1" />'#13#10'<path stroke="#8d6049" d="M' +
          '10 7h1M7 10h1" />'#13#10'<path stroke="#9c6c52" d="M11 7h1" />'#13#10'<path ' +
          'stroke="#684234" d="M12 7h1" />'#13#10'<path stroke="#9d664f" d="M2 8h' +
          '1" />'#13#10'<path stroke="#caac81" d="M3 8h1" />'#13#10'<path stroke="#d4b8' +
          '8c" d="M4 8h1" />'#13#10'<path stroke="#ccae84" d="M5 8h1" />'#13#10'<path s' +
          'troke="#ba916a" d="M6 8h1" />'#13#10'<path stroke="#7c5140" d="M7 8h1"' +
          ' />'#13#10'<path stroke="#c9ab80" d="M8 8h1" />'#13#10'<path stroke="#cfb489' +
          '" d="M9 8h1" />'#13#10'<path stroke="#ceaf84" d="M10 8h1" />'#13#10'<path st' +
          'roke="#bc9169" d="M11 8h1" />'#13#10'<path stroke="#9d664d" d="M12 8h1' +
          '" />'#13#10'<path stroke="#a26950" d="M2 9h1" />'#13#10'<path stroke="#e0c89' +
          '7" d="M3 9h1" />'#13#10'<path stroke="#caa577" d="M6 9h1" />'#13#10'<path st' +
          'roke="#7d5140" d="M7 9h1" />'#13#10'<path stroke="#c59d72" d="M11 9h1"' +
          ' />'#13#10'<path stroke="#a36a4e" d="M12 9h1" />'#13#10'<path stroke="#9e644' +
          'd" d="M2 10h1" />'#13#10'<path stroke="#c59e72" d="M6 10h1" />'#13#10'<path ' +
          'stroke="#c0986e" d="M11 10h1" />'#13#10'<path stroke="#a0654b" d="M12 ' +
          '10h1" />'#13#10'<path stroke="#9a614a" d="M2 11h1" />'#13#10'<path stroke="#' +
          'cba97e" d="M4 11h1M9 11h1" />'#13#10'<path stroke="#c7a379" d="M5 11h1' +
          'M10 11h1" />'#13#10'<path stroke="#b98b62" d="M6 11h1" />'#13#10'<path strok' +
          'e="#9d6d53" d="M7 11h1" />'#13#10'<path stroke="#b68560" d="M11 11h1" ' +
          '/>'#13#10'<path stroke="#9c6249" d="M12 11h1" />'#13#10'<path stroke="#975c4' +
          '5" d="M2 12h1" />'#13#10'<path stroke="#955c46" d="M3 12h4M8 12h4" />'#13 +
          #10'<path stroke="#805645" d="M7 12h1" />'#13#10'<path stroke="#9b6147" d' +
          '="M12 12h1" />'#13#10'<path stroke="#715045" d="M7 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Light\Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Dark\Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#ffffff">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Light\Font'
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
        IconName = 'Dark\Font'
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
    Left = 168
    Top = 16
  end
  object vilFileStructureLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Light\Empty'
        Name = 'Empty'
      end
      item
        CollectionIndex = 1
        CollectionName = 'EditClass'
        Name = 'ClassEdit'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Light\PrivateAttribute'
        Name = 'PrivateAttribute'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Light\PackageAttribute'
        Name = 'PackageAttribute'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Light\ProtectedAttribute'
        Name = 'ProtectedAttribute'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Light\PublicAttribute'
        Name = 'PublicAttribute'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Class'
        Name = 'Class'
      end
      item
        CollectionIndex = 7
        CollectionName = 'PrivateMethod'
        Name = 'PrivateMethod'
      end
      item
        CollectionIndex = 8
        CollectionName = 'PackageMethod'
        Name = 'PackageMethod'
      end
      item
        CollectionIndex = 9
        CollectionName = 'ProtectedMethod'
        Name = 'ProtectedMethod'
      end
      item
        CollectionIndex = 10
        CollectionName = 'PublicMethod'
        Name = 'PublicMethod'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Light\Interface'
        Name = 'Interface'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Light\Attribute'
        Name = 'Attribute'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Light\Method'
        Name = 'Method'
      end
      item
        CollectionIndex = 14
        CollectionName = 'Light\Local'
        Name = 'Local'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Light\Parameter'
        Name = 'Parameter'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Light\Unknown'
        Name = 'Unknown'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Light\Package'
        Name = 'Package'
      end
      item
        CollectionIndex = 36
        CollectionName = 'Light\Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 38
        CollectionName = 'Light\Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 40
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icFileStructure
    Left = 48
    Top = 104
  end
  object vilFileStructureDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 18
        CollectionName = 'Dark\Empty'
        Name = 'Empty'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Dark\ClassEdit'
        Name = 'ClassEdit'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Dark\PrivateAttribute'
        Name = 'PrivateAttribute'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Dark\PackageAttribute'
        Name = 'PackageAttribute'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Dark\ProtectedAttribute'
        Name = 'ProtectedAttribute'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Dark\PublicAttribute'
        Name = 'PublicAttribute'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Class'
        Name = 'Class'
      end
      item
        CollectionIndex = 7
        CollectionName = 'PrivateMethod'
        Name = 'PrivateMethod'
      end
      item
        CollectionIndex = 26
        CollectionName = 'PackageMethod'
        Name = 'PackageMethod'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ProtectedMethod'
        Name = 'ProtectedMethod'
      end
      item
        CollectionIndex = 28
        CollectionName = 'PublicMethod'
        Name = 'PublicMethod'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Dark\Interface'
        Name = 'Interface'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Dark\'
        Name = 'Dark\'
      end
      item
        CollectionIndex = 31
        CollectionName = 'Dark\'
        Name = 'Dark\'
      end
      item
        CollectionIndex = 32
        CollectionName = 'Dark\Local'
        Name = 'Local'
      end
      item
        CollectionIndex = 33
        CollectionName = 'Dark\'
        Name = 'Dark\'
      end
      item
        CollectionIndex = 34
        CollectionName = 'Dark\'
        Name = 'Dark\'
      end
      item
        CollectionIndex = 35
        CollectionName = 'Dark\Package'
        Name = 'Package'
      end
      item
        CollectionIndex = 37
        CollectionName = 'Dark\Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 39
        CollectionName = 'Dark\Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 40
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icFileStructure
    Left = 168
    Top = 104
  end
end
