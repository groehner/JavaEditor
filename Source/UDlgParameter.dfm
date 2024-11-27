object FParameterDialog: TFParameterDialog
  Left = 426
  Top = 195
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Start parameter'
  ClientHeight = 182
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LParameter: TLabel
    Left = 16
    Top = 80
    Width = 77
    Height = 15
    Caption = 'Call parameter'
  end
  object LStartClass: TLabel
    Left = 16
    Top = 16
    Width = 52
    Height = 15
    Caption = 'Start class'
  end
  object EParameter: TEdit
    Left = 16
    Top = 104
    Width = 265
    Height = 23
    TabOrder = 1
  end
  object BOK: TButton
    Left = 200
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BDelete: TButton
    Left = 288
    Top = 102
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 3
    OnClick = BDeleteClick
  end
  object BCancel: TButton
    Left = 288
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object EStartClass: TEdit
    Left = 16
    Top = 40
    Width = 265
    Height = 23
    TabOrder = 0
  end
  object BSelect: TButton
    Left = 287
    Top = 38
    Width = 75
    Height = 25
    Caption = 'Select'
    TabOrder = 5
    OnClick = BSelectClick
  end
  object ODParaSelect: TOpenDialog
    Filter = 'java|*.java|*.*|*.*'
    Left = 176
    Top = 8
  end
end
