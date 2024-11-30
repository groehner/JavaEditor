object SuggestWindow: TSuggestWindow
  Left = 0
  Top = 0
  HelpContext = 755
  BorderStyle = bsSingle
  BorderWidth = 1
  ClientHeight = 327
  ClientWidth = 622
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  StyleElements = [seClient]
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object SpTBXDock: TSpTBXDock
    Left = 0
    Top = 305
    Width = 622
    Height = 22
    Position = dpBottom
    object SpTBXToolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      DockMode = dmCannotFloatOrChangeDocks
      DragHandleStyle = dhNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Segoe UI'
      Font.Style = []
      FullSize = True
      Images = vilImages
      ParentFont = False
      ProcessShortCuts = True
      ShrinkMode = tbsmNone
      Stretch = True
      SystemFont = False
      TabOrder = 0
      Customizable = False
      object spiAccept: TSpTBXItem
        Caption = 'Accept (Tab)'
        DisplayMode = nbdmImageAndText
        ImageIndex = 1
        ImageName = 'Check'
        ShortCut = 9
        OnClick = spiAcceptClick
      end
      object spiAcceptWord: TSpTBXItem
        Caption = 'Accept Word (Ctrl+Right)'
        Hint = 'Accept the first suggested word'
        ShortCut = 16423
        OnClick = spiAcceptWordClick
      end
      object spiAcceptLine: TSpTBXItem
        Caption = 'Accept Line (Ctrl+Enter)'
        Hint = 'Accept the first suggested line'
        ShortCut = 16397
        OnClick = spiAcceptLineClick
      end
      object SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem
        CustomWidth = 261
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object spiCancel: TSpTBXItem
        Caption = 'Cancel (Esc)'
        Hint = 'Close the form'
        DisplayMode = nbdmImageAndText
        ImageIndex = 0
        ImageName = 'Cancel'
        ShortCut = 27
        OnClick = spiCancelClick
      end
    end
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Cancel'
        Name = 'Cancel'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Check'
        Name = 'Check'
      end>
    ImageCollection = icImages
    PreserveItems = True
    Width = 10
    Height = 10
    Left = 16
    Top = 240
  end
  object icImages: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Cancel'
        SVGText = 
          '<svg height="24px" viewBox="0 -960 960 960" width="24px" >'#13#10'    ' +
          '<path fill="#E24444"'#13#10'        d="m336-280 144-144 144 144 56-56-' +
          '144-144 144-144-56-56-144 144-144-144-56 56 144 144-144 144 56 5' +
          '6ZM480-80q-83 0-156-31.5T197-197q-54-54-85.5-127T80-480q0-83 31.' +
          '5-156T197-763q54-54 127-85.5T480-880q83 0 156 31.5T763-763q54 54' +
          ' 85.5 127T880-480q0 83-31.5 156T763-197q-54 54-127 85.5T480-80Zm' +
          '0-80q134 0 227-93t93-227q0-134-93-227t-227-93q-134 0-227 93t-93 ' +
          '227q0 134 93 227t227 93Zm0-320Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Check'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#FFCE00" d="M10.2,23.1l-' +
          '7.6-7.6L0.1,18l10.1,10.1L31.9,6.4l-2.5-2.5L10.2,23.1z"/>'#13#10'</svg>' +
          #13#10
      end>
    Left = 120
    Top = 240
  end
end
