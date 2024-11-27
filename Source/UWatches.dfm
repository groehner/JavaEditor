object FWatches: TFWatches
  Left = 371
  Top = 200
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Watches'
  ClientHeight = 261
  ClientWidth = 379
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object LNewWatch: TLabel
    Left = 8
    Top = 16
    Width = 59
    Height = 15
    Caption = 'New watch'
  end
  object LWatches: TLabel
    Left = 8
    Top = 80
    Width = 45
    Height = 15
    Caption = 'Watches'
  end
  object SBUp: TSpeedButton
    Left = 296
    Top = 127
    Width = 35
    Height = 22
    ImageIndex = 0
    ImageName = '00'
    Images = ILWatches
    OnClick = SBUpClick
  end
  object SBDown: TSpeedButton
    Left = 336
    Top = 127
    Width = 35
    Height = 22
    ImageIndex = 1
    ImageName = '01'
    Images = ILWatches
    OnClick = SBDownClick
  end
  object EWatch: TEdit
    Left = 24
    Top = 40
    Width = 257
    Height = 23
    TabOrder = 0
  end
  object BAdd: TButton
    Left = 296
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Add'
    Default = True
    TabOrder = 1
    OnClick = BAddClick
  end
  object LBWatches: TListBox
    Left = 24
    Top = 104
    Width = 257
    Height = 153
    ItemHeight = 15
    TabOrder = 2
  end
  object BDelete: TButton
    Left = 296
    Top = 164
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = BDeleteClick
  end
  object BClose: TButton
    Left = 296
    Top = 232
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 4
    OnClick = BCloseClick
  end
  object icWatches: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#c2cde2" d="M5 2h1" ' +
          '/>'#13#10'<path stroke="#6f96df" d="M6 2h1M3 5h1M2 6h1" />'#13#10'<path stro' +
          'ke="#c1c7d2" d="M7 2h1" />'#13#10'<path stroke="#c3cee3" d="M4 3h1" />' +
          #13#10'<path stroke="#6f9eef" d="M5 3h1M4 4h1" />'#13#10'<path stroke="#9eb' +
          'eff" d="M6 3h1M5 4h1M4 5h1M5 7h1" />'#13#10'<path stroke="#33589e" d="' +
          'M7 3h1" />'#13#10'<path stroke="#c0c6cf" d="M8 3h1" />'#13#10'<path stroke="' +
          '#c4cfe5" d="M3 4h1" />'#13#10'<path stroke="#8eaeff" d="M6 4h1M5 5h1M5' +
          ' 6h1" />'#13#10'<path stroke="#6086df" d="M7 4h1M7 7h1M6 8h1M5 9h1" />' +
          #13#10'<path stroke="#33509e" d="M8 4h1M9 5h1M5 10h1" />'#13#10'<path strok' +
          'e="#bec4ce" d="M9 4h1" />'#13#10'<path stroke="#c5d0e5" d="M2 5h1" />'#13 +
          #10'<path stroke="#6f9eff" d="M6 5h1" />'#13#10'<path stroke="#608eef" d=' +
          '"M7 5h1" />'#13#10'<path stroke="#4277ce" d="M8 5h1" />'#13#10'<path stroke=' +
          '"#bdc3cc" d="M10 5h1" />'#13#10'<path stroke="#426fce" d="M3 6h1" />'#13#10 +
          '<path stroke="#3367ce" d="M4 6h1" />'#13#10'<path stroke="#6096ef" d="' +
          'M6 6h2M6 7h1" />'#13#10'<path stroke="#3358ae" d="M8 6h1" />'#13#10'<path st' +
          'roke="#25509e" d="M9 6h1M8 7h1" />'#13#10'<path stroke="#25498e" d="M1' +
          '0 6h1M8 9h1M6 10h3" />'#13#10'<path stroke="#517edf" d="M4 7h1M4 8h1M4' +
          ' 9h1M4 10h1" />'#13#10'<path stroke="#7fa6ef" d="M5 8h1" />'#13#10'<path str' +
          'oke="#5177ce" d="M7 8h1" />'#13#10'<path stroke="#25499e" d="M8 8h1" /' +
          '>'#13#10'<path stroke="#517ece" d="M6 9h1" />'#13#10'<path stroke="#426fbe" ' +
          'd="M7 9h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#6f9eef" d="M4 2h1M2' +
          ' 6h1" />'#13#10'<path stroke="#6f96df" d="M5 2h1M4 3h1M3 6h1" />'#13#10'<pat' +
          'h stroke="#6f8edf" d="M6 2h1M4 4h1M4 6h1" />'#13#10'<path stroke="#608' +
          '6ce" d="M7 2h2M4 5h1" />'#13#10'<path stroke="#9ebeff" d="M5 3h1M5 4h1' +
          '" />'#13#10'<path stroke="#8eb6ff" d="M6 3h1" />'#13#10'<path stroke="#8eaef' +
          'f" d="M7 3h1" />'#13#10'<path stroke="#516fbe" d="M8 3h1" />'#13#10'<path st' +
          'roke="#7fa6ff" d="M6 4h1M5 6h1" />'#13#10'<path stroke="#608eef" d="M7' +
          ' 4h1M6 6h1" />'#13#10'<path stroke="#4267ae" d="M8 4h1M10 6h1" />'#13#10'<pa' +
          'th stroke="#7faeff" d="M5 5h1" />'#13#10'<path stroke="#608eff" d="M6 ' +
          '5h1" />'#13#10'<path stroke="#5186ef" d="M7 5h1" />'#13#10'<path stroke="#42' +
          '60ae" d="M8 5h1M9 6h1" />'#13#10'<path stroke="#517edf" d="M7 6h1M5 8h' +
          '1" />'#13#10'<path stroke="#33509e" d="M8 6h1M9 7h1M8 8h1" />'#13#10'<path s' +
          'troke="#5177be" d="M3 7h1" />'#13#10'<path stroke="#6f96ef" d="M4 7h1"' +
          ' />'#13#10'<path stroke="#6f9eff" d="M5 7h1" />'#13#10'<path stroke="#5186df' +
          '" d="M6 7h1" />'#13#10'<path stroke="#5177ce" d="M7 7h1" />'#13#10'<path str' +
          'oke="#4267be" d="M8 7h1M7 8h1M6 9h1" />'#13#10'<path stroke="#bdc3cc" ' +
          'd="M10 7h1" />'#13#10'<path stroke="#3358ae" d="M4 8h1M5 9h1M6 10h1" /' +
          '>'#13#10'<path stroke="#5177df" d="M6 8h1" />'#13#10'<path stroke="#bec4ce" ' +
          'd="M9 8h1" />'#13#10'<path stroke="#d2d5df" d="M4 9h1" />'#13#10'<path strok' +
          'e="#33589e" d="M7 9h1" />'#13#10'<path stroke="#c0c6cf" d="M8 9h1" />'#13 +
          #10'<path stroke="#d1d4dd" d="M5 10h1" />'#13#10'<path stroke="#c1c7d2" d' +
          '="M7 10h1" />'#13#10'</svg>'#13#10
      end>
    Left = 192
    Top = 192
  end
  object ILWatches: TVirtualImageList
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
      end>
    ImageCollection = icWatches
    Width = 18
    Height = 18
    Left = 88
    Top = 192
  end
end
