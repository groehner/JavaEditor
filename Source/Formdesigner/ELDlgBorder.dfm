object FBorder: TFBorder
  Left = 0
  Top = 0
  Caption = 'Border'
  ClientHeight = 245
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object PCBorder: TPageControl
    Left = 119
    Top = 0
    Width = 278
    Height = 204
    ActivePage = TSTitled
    Align = alRight
    TabOrder = 0
    ExplicitLeft = 115
    ExplicitHeight = 203
    object TSNo: TTabSheet
      Caption = 'No'
      TabVisible = False
    end
    object TSLine: TTabSheet
      Caption = 'Line'
      ImageIndex = 1
      TabVisible = False
      object LLineBorderColor: TLabel
        Left = 16
        Top = 10
        Width = 29
        Height = 15
        Caption = 'Color'
      end
      object LLineBorderThickness: TLabel
        Left = 16
        Top = 64
        Width = 51
        Height = 15
        Caption = 'Thickness'
      end
      object EThickness: TEdit
        Left = 16
        Top = 83
        Width = 34
        Height = 22
        TabOrder = 0
        Text = '0'
      end
      object UPThickness: TUpDown
        Left = 50
        Top = 83
        Width = 16
        Height = 22
        Associate = EThickness
        Max = 40
        TabOrder = 1
      end
      object CBLineRounded: TCheckBox
        Left = 16
        Top = 128
        Width = 97
        Height = 17
        Caption = 'Rounded'
        TabOrder = 2
      end
    end
    object TSEtched: TTabSheet
      Caption = 'Etched'
      ImageIndex = 2
      TabVisible = False
      object Label1: TLabel
        Left = 16
        Top = 10
        Width = 50
        Height = 15
        Caption = 'Highlight'
      end
      object Label2: TLabel
        Left = 16
        Top = 58
        Width = 42
        Height = 15
        Caption = 'Shadow'
      end
      object RGEtchtype: TRadioGroup
        Left = 16
        Top = 120
        Width = 105
        Height = 54
        ItemIndex = 0
        Items.Strings = (
          'Raised'
          'Lowered')
        TabOrder = 0
      end
    end
    object TSBevel: TTabSheet
      Caption = 'Bevel'
      ImageIndex = 3
      TabVisible = False
      object Label3: TLabel
        Left = 16
        Top = 10
        Width = 50
        Height = 15
        Caption = 'Highlight'
      end
      object Label4: TLabel
        Left = 16
        Top = 58
        Width = 42
        Height = 15
        Caption = 'Shadow'
      end
      object RGBevelType: TRadioGroup
        Left = 16
        Top = 120
        Width = 105
        Height = 71
        Caption = 'Kind'
        ItemIndex = 0
        Items.Strings = (
          'Raised'
          'Lowered')
        TabOrder = 0
      end
    end
    object TSTitled: TTabSheet
      Caption = 'Titled'
      ImageIndex = 4
      TabVisible = False
      object LTitle: TLabel
        Left = 16
        Top = 10
        Width = 22
        Height = 15
        Caption = 'Title'
      end
      object ETitle: TEdit
        Left = 16
        Top = 29
        Width = 121
        Height = 23
        TabOrder = 0
      end
    end
    object TSMatte: TTabSheet
      Hint = 'Matte'
      Caption = 'Matte'
      ImageIndex = 5
      TabVisible = False
      object Label5: TLabel
        Left = 16
        Top = 10
        Width = 29
        Height = 15
        Caption = 'Color'
      end
      object LMatteTop: TLabel
        Left = 76
        Top = 89
        Width = 19
        Height = 15
        Caption = 'Top'
      end
      object LMatteLeft: TLabel
        Left = 16
        Top = 116
        Width = 20
        Height = 15
        Caption = 'Left'
      end
      object LMatteBottom: TLabel
        Left = 60
        Top = 142
        Width = 40
        Height = 15
        Caption = 'Bottom'
      end
      object LMatteRight: TLabel
        Left = 230
        Top = 116
        Width = 28
        Height = 15
        Caption = 'Right'
      end
      object EMatteTop: TEdit
        Left = 100
        Top = 86
        Width = 65
        Height = 22
        TabOrder = 1
      end
      object EMatteLeft: TEdit
        Left = 41
        Top = 114
        Width = 64
        Height = 22
        TabOrder = 0
      end
      object EMatteRight: TEdit
        Left = 159
        Top = 114
        Width = 65
        Height = 22
        TabOrder = 2
      end
      object EMatteBottom: TEdit
        Left = 100
        Top = 142
        Width = 65
        Height = 22
        TabOrder = 3
      end
    end
  end
  object PButtons: TPanel
    Left = 0
    Top = 204
    Width = 397
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 203
    ExplicitWidth = 393
    object BOK: TButton
      Left = 304
      Top = 6
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      OnClick = BOKClick
    end
    object BCancel: TButton
      Left = 223
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 119
    Height = 204
    Align = alClient
    TabOrder = 2
    ExplicitWidth = 115
    ExplicitHeight = 203
    object RGBordertype: TRadioGroup
      Left = 1
      Top = 9
      Width = 117
      Height = 194
      Align = alClient
      Caption = 'Border type'
      Items.Strings = (
        'No border'
        'Line border'
        'Etched border'
        'Bevel border'
        'Titled border'
        'Matte border')
      TabOrder = 0
      OnClick = RGBordertypeClick
      ExplicitWidth = 113
      ExplicitHeight = 193
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 117
      Height = 8
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 113
    end
  end
end
