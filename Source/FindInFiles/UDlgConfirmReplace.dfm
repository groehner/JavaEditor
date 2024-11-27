object FConfirmReplace: TFConfirmReplace
  Left = 604
  Top = 185
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Confirm replace'
  ClientHeight = 98
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LConfirmation: TLabel
    Left = 60
    Top = 12
    Width = 261
    Height = 44
    AutoSize = False
    WordWrap = True
  end
  object IImage: TImage
    Left = 16
    Top = 16
    Width = 32
    Height = 32
  end
  object BYes: TButton
    Left = 8
    Top = 67
    Width = 75
    Height = 23
    Caption = 'Yes'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object BNo: TButton
    Left = 87
    Top = 67
    Width = 75
    Height = 23
    Caption = 'No'
    ModalResult = 7
    TabOrder = 1
  end
  object BCancel: TButton
    Left = 166
    Top = 67
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object BReplaceAll: TButton
    Left = 245
    Top = 67
    Width = 75
    Height = 23
    Caption = 'Replace all'
    ModalResult = 14
    TabOrder = 3
  end
end
