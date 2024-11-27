object FSearch: TFSearch
  Left = 394
  Top = 278
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Search'
  ClientHeight = 174
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LSearch: TLabel
    Left = 8
    Top = 12
    Width = 56
    Height = 15
    Caption = 'Search for:'
  end
  object CBSearchText: TComboBox
    Tag = 1
    Left = 96
    Top = 8
    Width = 228
    Height = 23
    TabOrder = 0
  end
  object RGSearchDirection: TRadioGroup
    Tag = 9
    Left = 170
    Top = 40
    Width = 154
    Height = 65
    Caption = 'Direction'
    ItemIndex = 0
    Items.Strings = (
      'Forward'
      'Backward')
    TabOrder = 2
  end
  object BOK: TButton
    Left = 170
    Top = 146
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object BCancel: TButton
    Left = 249
    Top = 146
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object GBSearchOptions: TGroupBox
    Tag = -1
    Left = 8
    Top = 40
    Width = 154
    Height = 129
    Caption = 'Options'
    TabOrder = 1
    object LSearchRegSearch: TLabel
      Left = 24
      Top = 105
      Width = 104
      Height = 15
      Caption = 'Regular expressions'
      OnClick = LSearchRegSearchClick
      OnMouseEnter = LSearchRegSearchMouseEnter
      OnMouseLeave = LSearchRegSearchMouseLeave
    end
    object CBSearchCaseSensitive: TCheckBox
      Tag = 4
      Left = 8
      Top = 16
      Width = 140
      Height = 17
      Caption = 'Case sensitive'
      TabOrder = 0
    end
    object CBSearchWholeWords: TCheckBox
      Tag = 5
      Left = 8
      Top = 38
      Width = 140
      Height = 17
      Caption = 'Whole words only'
      TabOrder = 1
    end
    object CBSearchFromCursor: TCheckBox
      Tag = 6
      Left = 8
      Top = 60
      Width = 140
      Height = 17
      Caption = 'From cursor'
      TabOrder = 2
    end
    object CBSearchSelectionOnly: TCheckBox
      Tag = 7
      Left = 8
      Top = 82
      Width = 140
      Height = 17
      Caption = 'Selected text'
      TabOrder = 3
    end
    object CBSearchRegSearch: TCheckBox
      Tag = 8
      Left = 8
      Top = 104
      Width = 17
      Height = 17
      TabOrder = 4
    end
  end
end
