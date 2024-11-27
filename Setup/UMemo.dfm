object FMemo: TFMemo
  Left = 306
  Top = 226
  BorderStyle = bsSingle
  Caption = 'Installation'
  ClientHeight = 255
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object MInstallation: TMemo
    Left = 0
    Top = 0
    Width = 685
    Height = 201
    Align = alTop
    Lines.Strings = (
      'Java-Editor Installation'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 681
  end
  object BClose: TButton
    Left = 600
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 1
    OnClick = BCloseClick
  end
end
