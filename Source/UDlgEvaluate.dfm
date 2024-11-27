object FEvaluate: TFEvaluate
  Left = 384
  Top = 209
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Evaluate expression'
  ClientHeight = 249
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object LExpression: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 15
    Caption = 'Expression'
  end
  object LResult: TLabel
    Left = 8
    Top = 72
    Width = 32
    Height = 15
    Caption = 'Result'
  end
  object EExpression: TEdit
    Left = 24
    Top = 32
    Width = 321
    Height = 23
    TabOrder = 0
  end
  object MEvaluate: TMemo
    Left = 24
    Top = 96
    Width = 321
    Height = 105
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object BEvaluate: TButton
    Left = 56
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Evaluate'
    Default = True
    TabOrder = 2
    OnClick = BEvaluateClick
  end
  object BWatch: TButton
    Left = 152
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Watch'
    TabOrder = 3
    OnClick = BWatchClick
  end
  object BClose: TButton
    Left = 248
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 4
    OnClick = BCloseClick
  end
end
