object FMethodCallDialog: TFMethodCallDialog
  Left = 0
  Top = 0
  Caption = 'Method call'
  ClientHeight = 218
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LMethodCall: TLabel
    Left = 16
    Top = 16
    Width = 99
    Height = 15
    Caption = 'Call of the method'
  end
  object LParametervalues: TLabel
    Left = 16
    Top = 72
    Width = 159
    Height = 15
    Caption = 'With current parameter values'
  end
  object LResult: TLabel
    Left = 16
    Top = 128
    Width = 80
    Height = 15
    Caption = 'Gives the result'
  end
  object EResult: TEdit
    Left = 32
    Top = 147
    Width = 300
    Height = 23
    TabOrder = 0
  end
  object BOK: TButton
    Left = 257
    Top = 180
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 1
    OnClick = BOKClick
  end
  object EMethodCall: TEdit
    Left = 32
    Top = 36
    Width = 300
    Height = 23
    TabOrder = 2
  end
  object EParametervalues: TEdit
    Left = 32
    Top = 92
    Width = 300
    Height = 23
    TabOrder = 3
  end
end
