object FJUnitTests: TFJUnitTests
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'JUnit'
  ClientHeight = 204
  ClientWidth = 356
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
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object PJUnit: TPanel
    Left = 0
    Top = 0
    Width = 356
    Height = 18
    Align = alTop
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 360
  end
  object TVJUnitTests: TTreeView
    Left = 0
    Top = 18
    Width = 356
    Height = 186
    Align = alClient
    Indent = 23
    TabOrder = 1
    OnChange = TVJUnitTestsChange
    OnMouseDown = TVJUnitTestsMouseDown
  end
  object PMJUnitTests: TSpTBXPopupMenu
    Images = vilJUnitLight
    Left = 72
    Top = 48
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 3
      ImageName = 'Font'
      OnClick = MIFontClick
    end
    object MIDefaulLayout: TSpTBXItem
      Caption = 'Default layout'
      ImageIndex = 4
      ImageName = 'DefaultLayout'
      OnClick = MIDefaulLayoutClick
    end
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 2
      ImageName = 'Close'
      OnClick = MICloseClick
    end
  end
  object icJUnit: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Check'
        SVGText = 
          '<svg viewBox="0 0 612 792">'#13#10'<path style="fill:#41AD49" d="M562,' +
          '396c0-141.4-114.6-256-256-256S50,254.6,50,396s114.6,256,256,256S' +
          '562,537.4,562,396L562,396z    M501.7,296.3l-241,241l0,0l-17.2,17' +
          '.2L110.3,421.3l58.8-58.8l74.5,74.5l199.4-199.4L501.7,296.3L501.7' +
          ',296.3z"/>'#13#10'</svg>'
      end
      item
        IconName = 'CloseRed'
        SVGText = 
          '<svg viewBox="0 0 612 792">'#13#10'<path style="fill:#E44061" d="M562,' +
          '396c0-141.4-114.6-256-256-256S50,254.6,50,396s114.6,256,256,256S' +
          '562,537.4,562,396L562,396z M356.8,396   L475,514.2L424.2,565L306' +
          ',446.8L187.8,565L137,514.2L255.2,396L137,277.8l50.8-50.8L306,345' +
          '.2L424.2,227l50.8,50.8L356.8,396   L356.8,396z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
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
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#ffffff">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
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
      end>
    Left = 168
    Top = 48
  end
  object vilJUnitLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Check'
        Name = 'Check'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CloseRed'
        Name = 'CloseRed'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 4
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icJUnit
    Left = 72
    Top = 128
  end
  object vilJUnitDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Check'
        Name = 'Check'
      end
      item
        CollectionIndex = 1
        CollectionName = 'CloseRed'
        Name = 'CloseRed'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 4
        CollectionName = 'DefaultLayout'
        Name = 'DefaultLayout'
      end>
    ImageCollection = icJUnit
    Left = 176
    Top = 128
  end
end
