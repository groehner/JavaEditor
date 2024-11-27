object FUnicodeDialog: TFUnicodeDialog
  Left = 359
  Top = 369
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Unicode'
  ClientHeight = 36
  ClientWidth = 108
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object LUnicode: TLabel
    Left = 8
    Top = 12
    Width = 16
    Height = 15
    Caption = 'U+'
  end
  object CBUnicode: TComboBox
    Left = 32
    Top = 8
    Width = 65
    Height = 23
    AutoComplete = False
    TabOrder = 0
    OnKeyDown = CBUnicodeKeyDown
    OnKeyPress = CBUnicodeKeyPress
    OnSelect = CBUnicodeSelect
  end
end
