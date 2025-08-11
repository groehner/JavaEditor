object FHelpDialog: TFHelpDialog
  Left = 541
  Top = 42
  ActiveControl = TVAPITree
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'JDK help'
  ClientHeight = 452
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PCJDKHelp: TPageControl
    Left = 0
    Top = 0
    Width = 377
    Height = 417
    ActivePage = TSAPI
    TabOrder = 0
    OnChange = PCJDKHelpChange
    object TSFavorites: TTabSheet
      Caption = 'Favorites'
      ImageIndex = 4
      object SBUp: TSpeedButton
        Left = 8
        Top = 360
        Width = 23
        Height = 22
        Glyph.Data = {
          CE000000424DCE0000000000000076000000280000000A0000000B0000000100
          04000000000058000000880B0000880B00001000000000000000000000003300
          0000003300003333000066000000663300009900000099330000CC000000CC33
          00005858C000FF000000FF603000FF983000FFC83000FFFF9800AAA0DDCAAA01
          1300AAA0DFCAAA011300AAA0DFCAAA011300AAA0DFCAAA011300AAA0DFCAAA01
          1300AAA0DFCAAA0113000000DF0000011300A0DDDFFFCA011300AA0DEFFCAA01
          1300AAA0DFCAAA011300AAAA0CAAAA011300}
        OnClick = SBUpClick
      end
      object SBDown: TSpeedButton
        Left = 40
        Top = 360
        Width = 23
        Height = 22
        Glyph.Data = {
          CE000000424DCE0000000000000076000000280000000A0000000B0000000100
          04000000000058000000880B0000880B00001000000000000000000000003300
          0000003300003333000066000000663300009900000099330000CC000000CC33
          00005858C000FF000000FF603000FF983000FFC83000FFFF9800AAAAC0AAAA01
          1300AAACFD0AAA011300AACFFED0AA011300ACFFFDDD0A0113000000FD000001
          1300AAACFD0AAA011300AAACFD0AAA011300AAACFD0AAA011300AAACFD0AAA01
          1300AAACFD0AAA011300AAACDD0AAA011300}
        OnClick = SBDownClick
      end
      object LBFavorites: TListBox
        Left = 8
        Top = 8
        Width = 353
        Height = 345
        ItemHeight = 15
        TabOrder = 0
        OnDblClick = LBFavoritesDblClick
      end
      object BDelete: TButton
        Left = 208
        Top = 360
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 1
        OnClick = BDeleteClick
      end
      object BEdit: TButton
        Left = 288
        Top = 360
        Width = 75
        Height = 25
        Caption = 'Edit'
        TabOrder = 2
        OnClick = BEditClick
      end
      object BNew: TButton
        Left = 128
        Top = 360
        Width = 75
        Height = 25
        Caption = 'New'
        TabOrder = 3
        OnClick = BNewClick
      end
    end
    object TSSearch: TTabSheet
      Caption = 'Search'
      ImageIndex = 3
      object LSearch: TLabel
        Left = 8
        Top = 16
        Width = 55
        Height = 15
        Caption = 'Searchtext'
      end
      object LFiles: TLabel
        Left = 8
        Top = 176
        Width = 23
        Height = 15
        Caption = 'Files'
      end
      object ESearch: TEdit
        Left = 24
        Top = 40
        Width = 217
        Height = 23
        TabOrder = 0
        OnKeyDown = ESearchKeyDown
        OnKeyUp = ESearchKeyUp
      end
      object BSearch: TButton
        Left = 247
        Top = 38
        Width = 114
        Height = 25
        Caption = 'Search'
        TabOrder = 1
        OnClick = BSearchClick
      end
      object LBFiles: TListBox
        Left = 24
        Top = 200
        Width = 329
        Height = 185
        ItemHeight = 15
        TabOrder = 3
        OnDblClick = LBFilesDblClick
        OnKeyDown = LBFilesKeyDown
      end
      object RGSearchOptions: TRadioGroup
        Left = 24
        Top = 72
        Width = 217
        Height = 89
        Caption = 'Options'
        ItemIndex = 0
        Items.Strings = (
          'API'
          'All'
          'Java book')
        TabOrder = 2
        OnClick = RGSearchOptionsClick
      end
    end
    object TSAPI: TTabSheet
      Caption = 'API'
      object TVAPITree: TTreeView
        Left = 0
        Top = 0
        Width = 369
        Height = 387
        Align = alClient
        Images = vilJDKHelpLight
        Indent = 19
        ReadOnly = True
        SortType = stBoth
        TabOrder = 0
        OnCollapsing = TVTreeCollapsing
        OnDblClick = TVTreeDblClick
        OnExpanding = TVTreeExpanding
        OnGetSelectedIndex = TVTreeGetSelectedIndex
      end
    end
  end
  object BClose: TButton
    Left = 215
    Top = 424
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = BCloseClick
  end
  object BShow: TButton
    Left = 296
    Top = 424
    Width = 75
    Height = 25
    Caption = 'Show'
    TabOrder = 3
    OnClick = BShowClick
  end
  object BAddFavorit: TButton
    Left = 8
    Top = 424
    Width = 145
    Height = 25
    Caption = 'Add to favorites'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnClick = BAddFavoritClick
  end
  object icJDKHelp: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Closed'
        SVGText = 
          '<svg viewBox="0 -0.5 18 18">'#13#10'<path stroke="#000000" d="M8 1h2M6' +
          ' 2h2M10 2h1M4 3h2M11 3h1M2 4h2M12 4h1M1 5h1M13 5h1M1 6h1M14 6h1M' +
          '1 7h1M15 7h1M1 8h1M15 8h2M1 9h1M13 9h2M2 10h1M11 10h2M3 11h1M9 1' +
          '1h2M15 11h2M4 12h1M8 12h1M14 12h2M5 13h1M8 13h1M12 13h2M6 14h1M8' +
          ' 14h1M10 14h2M7 15h3" />'#13#10'<path stroke="#820082" d="M8 2h2M6 3h5' +
          'M4 4h8M3 5h10M2 6h1M4 6h10M2 7h2M5 7h10M2 8h3M6 8h9M2 9h4M7 9h6M' +
          '3 10h4M8 10h3M4 11h4M5 12h3M6 13h2M7 14h1" />'#13#10'<path stroke="#c4' +
          'c1c4" d="M2 5h1M3 6h1M4 7h1M5 8h1M6 9h1M7 10h1M8 11h1M12 11h1M14' +
          ' 11h1M11 12h1M13 12h1M10 13h1" />'#13#10'<path stroke="#828082" d="M15' +
          ' 9h1M13 10h1M15 10h1M11 11h1M9 12h1M9 14h1" />'#13#10'<path stroke="#f' +
          'fffff" d="M16 9h1M14 10h1M16 10h1M13 11h1M10 12h1M12 12h1M9 13h1' +
          'M11 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Open'
        SVGText = 
          '<svg viewBox="0 -0.5 17 17">'#13#10'<path stroke="#828082" d="M3 1h1M2' +
          ' 2h1M4 2h1M5 3h1M10 3h3M5 4h1M8 4h2M12 4h1M6 5h2M12 5h1M12 6h1M1' +
          '2 7h1M12 8h1M12 9h1M12 10h1M11 11h1M9 12h2M7 13h2" />'#13#10'<path str' +
          'oke="#c4c1c4" d="M3 2h1M4 3h1M5 5h1M7 6h1M7 7h1M7 8h1M7 9h1M7 10' +
          'h1M7 11h1M12 11h1M7 12h1M11 12h1M9 13h1" />'#13#10'<path stroke="#0000' +
          '00" d="M1 3h1M1 4h1M1 5h1M13 5h2M1 6h1M6 6h1M15 6h1M1 7h1M6 7h1M' +
          '15 7h1M1 8h1M6 8h1M15 8h1M1 9h1M6 9h1M15 9h1M1 10h1M6 10h1M15 10' +
          'h1M1 11h1M6 11h1M15 11h1M2 12h1M6 12h1M15 12h1M3 13h1M6 13h1M15 ' +
          '13h1M4 14h1M15 14h1M5 15h11" />'#13#10'<path stroke="#820082" d="M2 3h' +
          '1M2 4h1M2 5h1M2 6h1M14 6h1M2 7h1M14 7h1M2 8h1M14 8h1M2 9h1M14 9h' +
          '1M2 10h1M14 10h1M2 11h2M14 11h1M3 12h2M14 12h1M4 13h1M14 13h1M5 ' +
          '14h10" />'#13#10'<path stroke="#ffffff" d="M3 3h1M3 4h2M10 4h2M3 5h2M8' +
          ' 5h3M3 6h3M8 6h1M10 6h2M13 6h1M3 7h3M8 7h3M13 7h1M3 8h3M8 8h1M10' +
          ' 8h2M13 8h1M3 9h3M8 9h3M13 9h1M3 10h3M8 10h1M10 10h2M13 10h1M4 1' +
          '1h2M8 11h3M13 11h1M5 12h1M8 12h1M12 12h2M5 13h1M10 13h4" />'#13#10'<pa' +
          'th stroke="#ffff00" d="M11 5h1M9 6h1M11 7h1M9 8h1M11 9h1M9 10h1"' +
          ' />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Docu'
        SVGText = 
          '<svg viewBox="0 -0.5 14 17">'#13#10'<path stroke="#828082" d="M1 1h10M' +
          '1 2h1M10 2h2M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1M1 10h1M1 ' +
          '11h1M1 12h1M1 13h1M1 14h1" />'#13#10'<path stroke="#ffffff" d="M2 2h3M' +
          '6 2h3M2 3h1M4 3h3M8 3h2M2 4h3M10 4h1M2 5h1M6 5h1M10 5h1M2 6h2M6 ' +
          '6h2M10 6h1M2 7h1M4 7h3M10 7h1M2 8h3M6 8h1M10 8h1M2 9h1M4 9h2M8 9' +
          'h3M2 10h3M6 10h3M10 10h1M2 11h1M4 11h2M8 11h3M2 12h3M8 12h1M10 1' +
          '2h1M2 13h1M4 13h3M8 13h3" />'#13#10'<path stroke="#ffff00" d="M5 2h1M9' +
          ' 2h1M3 3h1M7 3h1M9 4h1M3 5h1M7 5h1M3 7h1M7 7h1M5 8h1M9 8h1M3 9h1' +
          'M5 10h1M9 10h1M3 11h1M5 12h1M9 12h1M3 13h1M7 13h1" />'#13#10'<path str' +
          'oke="#000000" d="M10 3h3M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9' +
          'h1M12 10h1M12 11h1M12 12h1M12 13h1M12 14h1M1 15h12" />'#13#10'<path st' +
          'roke="#0000ff" d="M5 4h4M4 5h2M8 5h2M4 6h2M8 6h2M8 7h2M7 8h2M6 9' +
          'h2M6 11h2M6 12h2" />'#13#10'<path stroke="#c4c1c4" d="M11 4h1M11 5h1M1' +
          '1 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M11 12h1M11 13h1M2 14h' +
          '10" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Plus'
        SVGText = 
          '<svg viewBox="0 -0.5 11 11">'#13#10'<path stroke="#828082" d="M1 1h9M1' +
          ' 2h1M9 2h1M1 3h1M9 3h1M1 4h1M9 4h1M1 5h1M9 5h1M1 6h1M9 6h1M1 7h1' +
          'M9 7h1M1 8h1M9 8h1M1 9h9" />'#13#10'<path stroke="#ffffff" d="M2 2h7M2' +
          ' 3h3M6 3h3M2 4h3M6 4h3M2 5h1M8 5h1M2 6h3M6 6h3M2 7h3M6 7h3M2 8h7' +
          '" />'#13#10'<path stroke="#000000" d="M5 3h1M5 4h1M3 5h5M5 6h1M5 7h1" ' +
          '/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Minus'
        SVGText = 
          '<svg viewBox="0 -0.5 11 11">'#13#10'<path stroke="#828082" d="M1 1h9M1' +
          ' 2h1M9 2h1M1 3h1M9 3h1M1 4h1M9 4h1M1 5h1M9 5h1M1 6h1M9 6h1M1 7h1' +
          'M9 7h1M1 8h1M9 8h1M1 9h9" />'#13#10'<path stroke="#ffffff" d="M2 2h7M2' +
          ' 3h7M2 4h7M2 5h1M8 5h1M2 6h7M2 7h7M2 8h7" />'#13#10'<path stroke="#000' +
          '000" d="M3 5h5" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Closed'
        SVGText = 
          '<svg viewBox="0 -0.5 18 18">'#13#10'<path stroke="#ffffff" d="M8 1h2M6' +
          ' 2h2M10 2h1M4 3h2M11 3h1M2 4h2M12 4h1M1 5h1M13 5h1M1 6h1M14 6h1M' +
          '1 7h1M15 7h1M1 8h1M15 8h2M1 9h1M13 9h2M2 10h1M11 10h2M3 11h1M9 1' +
          '1h2M15 11h2M4 12h1M8 12h1M14 12h2M5 13h1M8 13h1M12 13h2M6 14h1M8' +
          ' 14h1M10 14h2M7 15h3" />'#13#10'<path stroke="#820082" d="M8 2h2M6 3h5' +
          'M4 4h8M3 5h10M2 6h1M4 6h10M2 7h2M5 7h10M2 8h3M6 8h9M2 9h4M7 9h6M' +
          '3 10h4M8 10h3M4 11h4M5 12h3M6 13h2M7 14h1" />'#13#10'<path stroke="#c4' +
          'c1c4" d="M2 5h1M3 6h1M4 7h1M5 8h1M6 9h1M7 10h1M8 11h1M12 11h1M14' +
          ' 11h1M11 12h1M13 12h1M10 13h1" />'#13#10'<path stroke="#828082" d="M15' +
          ' 9h1M13 10h1M15 10h1M11 11h1M9 12h1M9 14h1" />'#13#10'<path stroke="#0' +
          '00000" d="M16 9h1M14 10h1M16 10h1M13 11h1M10 12h1M12 12h1M9 13h1' +
          'M11 13h1" />'#13#10'</svg>'
      end
      item
        IconName = 'Open'
        SVGText = 
          '<svg viewBox="0 -0.5 17 17">'#13#10'<path stroke="#828082" d="M3 1h1M2' +
          ' 2h1M4 2h1M5 3h1M10 3h3M5 4h1M8 4h2M12 4h1M6 5h2M12 5h1M12 6h1M1' +
          '2 7h1M12 8h1M12 9h1M12 10h1M11 11h1M9 12h2M7 13h2" />'#13#10'<path str' +
          'oke="#c4c1c4" d="M3 2h1M4 3h1M5 5h1M7 6h1M7 7h1M7 8h1M7 9h1M7 10' +
          'h1M7 11h1M12 11h1M7 12h1M11 12h1M9 13h1" />'#13#10'<path stroke="#0000' +
          '00" d="M1 3h1M1 4h1M1 5h1M13 5h2M1 6h1M6 6h1M15 6h1M1 7h1M6 7h1M' +
          '15 7h1M1 8h1M6 8h1M15 8h1M1 9h1M6 9h1M15 9h1M1 10h1M6 10h1M15 10' +
          'h1M1 11h1M6 11h1M15 11h1M2 12h1M6 12h1M15 12h1M3 13h1M6 13h1M15 ' +
          '13h1M4 14h1M15 14h1M5 15h11" />'#13#10'<path stroke="#820082" d="M2 3h' +
          '1M2 4h1M2 5h1M2 6h1M14 6h1M2 7h1M14 7h1M2 8h1M14 8h1M2 9h1M14 9h' +
          '1M2 10h1M14 10h1M2 11h2M14 11h1M3 12h2M14 12h1M4 13h1M14 13h1M5 ' +
          '14h10" />'#13#10'<path stroke="#000000" d="M3 3h1M3 4h2M10 4h2M3 5h2M8' +
          ' 5h3M3 6h3M8 6h1M10 6h2M13 6h1M3 7h3M8 7h3M13 7h1M3 8h3M8 8h1M10' +
          ' 8h2M13 8h1M3 9h3M8 9h3M13 9h1M3 10h3M8 10h1M10 10h2M13 10h1M4 1' +
          '1h2M8 11h3M13 11h1M5 12h1M8 12h1M12 12h2M5 13h1M10 13h4" />'#13#10'<pa' +
          'th stroke="#0000ff" d="M11 5h1M9 6h1M11 7h1M9 8h1M11 9h1M9 10h1"' +
          ' />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Docu'
        SVGText = 
          '<svg viewBox="0 -0.5 14 17">'#13#10'<path stroke="#828082" d="M1 1h10M' +
          '1 2h1M10 2h2M1 3h1M1 4h1M1 5h1M1 6h1M1 7h1M1 8h1M1 9h1M1 10h1M1 ' +
          '11h1M1 12h1M1 13h1M1 14h1" />'#13#10'<path stroke="#000000" d="M2 2h3M' +
          '6 2h3M2 3h1M4 3h3M8 3h2M2 4h3M10 4h1M2 5h1M6 5h1M10 5h1M2 6h2M6 ' +
          '6h2M10 6h1M2 7h1M4 7h3M10 7h1M2 8h3M6 8h1M10 8h1M2 9h1M4 9h2M8 9' +
          'h3M2 10h3M6 10h3M10 10h1M2 11h1M4 11h2M8 11h3M2 12h3M8 12h1M10 1' +
          '2h1M2 13h1M4 13h3M8 13h3" />'#13#10'<path stroke="#0000ff" d="M5 2h1M9' +
          ' 2h1M3 3h1M7 3h1M9 4h1M3 5h1M7 5h1M3 7h1M7 7h1M5 8h1M9 8h1M3 9h1' +
          'M5 10h1M9 10h1M3 11h1M5 12h1M9 12h1M3 13h1M7 13h1" />'#13#10'<path str' +
          'oke="#ffffff" d="M10 3h3M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9' +
          'h1M12 10h1M12 11h1M12 12h1M12 13h1M12 14h1M1 15h12" />'#13#10'<path st' +
          'roke="#ffff00" d="M5 4h4M4 5h2M8 5h2M4 6h2M8 6h2M8 7h2M7 8h2M6 9' +
          'h2M6 11h2M6 12h2" />'#13#10'<path stroke="#c4c1c4" d="M11 4h1M11 5h1M1' +
          '1 6h1M11 7h1M11 8h1M11 9h1M11 10h1M11 11h1M11 12h1M11 13h1M2 14h' +
          '10" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Plus'
        SVGText = 
          '<svg viewBox="0 -0.5 11 11">'#13#10'<path stroke="#7d7d7d" d="M1 1h9M1' +
          ' 2h1M9 2h1M1 3h1M9 3h1M1 4h1M9 4h1M1 5h1M9 5h1M1 6h1M9 6h1M1 7h1' +
          'M9 7h1M1 8h1M9 8h1M1 9h9" />'#13#10'<path stroke="#000000" d="M2 2h7M2' +
          ' 3h3M6 3h3M2 4h3M6 4h3M2 5h1M8 5h1M2 6h3M6 6h3M2 7h3M6 7h3M2 8h7' +
          '" />'#13#10'<path stroke="#ffffff" d="M5 3h1M5 4h1M3 5h5M5 6h1M5 7h1" ' +
          '/>'#13#10'</svg>'
      end
      item
        IconName = 'Minus'
        SVGText = 
          '<svg viewBox="0 -0.5 11 11">'#13#10'<path stroke="#7d7d7d" d="M1 1h9M1' +
          ' 2h1M9 2h1M1 3h1M9 3h1M1 4h1M9 4h1M1 5h1M9 5h1M1 6h1M9 6h1M1 7h1' +
          'M9 7h1M1 8h1M9 8h1M1 9h9" />'#13#10'<path stroke="#000000" d="M2 2h7M2' +
          ' 3h7M2 4h7M2 5h1M8 5h1M2 6h7M2 7h7M2 8h7" />'#13#10'<path stroke="#fff' +
          'fff" d="M3 5h5" />'#13#10'</svg>'#13#10#13#10
      end>
    Left = 292
    Top = 50
  end
  object vilJDKHelpLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Closed'
        Name = 'Closed'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Docu'
        Name = 'Docu'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Plus'
        Name = 'Plus'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Minus'
        Name = 'Minus'
      end>
    ImageCollection = icJDKHelp
    Left = 52
    Top = 50
  end
  object vilJDKHelpDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 5
        CollectionName = 'Closed'
        Name = 'Closed'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Open'
        Name = 'Open'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Docu'
        Name = 'Docu'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Plus'
        Name = 'Plus'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Minus'
        Name = 'Minus'
      end>
    ImageCollection = icJDKHelp
    Left = 172
    Top = 50
  end
end
