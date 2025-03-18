object FUpdater: TFUpdater
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'Java-Editor updater'
  ClientHeight = 250
  ClientWidth = 660
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 660
    Height = 215
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 215
    Width = 660
    Height = 35
    Align = alBottom
    TabOrder = 1
    object BOK: TButton
      AlignWithMargins = True
      Left = 581
      Top = 4
      Width = 75
      Height = 27
      Align = alRight
      Caption = 'OK'
      TabOrder = 0
      OnClick = BOKClick
    end
    object BRetry: TButton
      Left = 504
      Top = 4
      Width = 75
      Height = 27
      Caption = 'Retry'
      TabOrder = 1
      OnClick = BRetryClick
    end
  end
end
