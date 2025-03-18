object FSetup: TFSetup
  Left = 363
  Top = 243
  BorderStyle = bsSingle
  Caption = 'Setup'
  ClientHeight = 236
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object LHint1: TLabel
    Left = 16
    Top = 160
    Width = 217
    Height = 13
    Caption = 'Folder for storage of user specific settings with'
  end
  object LHint2: TLabel
    Left = 16
    Top = 176
    Width = 252
    Height = 13
    Caption = 'permanent write rights. %Username% in path possible.'
  end
  object LFolder: TLabel
    Left = 16
    Top = 16
    Width = 140
    Height = 13
    Caption = 'Installation folder - local or net'
  end
  object SBOrdner: TSpeedButton
    Left = 368
    Top = 40
    Width = 23
    Height = 22
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
      333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
      300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
      333337F373F773333333303330033333333337F3377333333333303333333333
      333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
      333337777F337F33333330330BB00333333337F373F773333333303330033333
      333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
      333377777F77377733330BBB0333333333337F337F33333333330BB003333333
      333373F773333333333330033333333333333773333333333333}
    NumGlyphs = 2
    OnClick = SBOrdnerClick
  end
  object SBUserIni: TSpeedButton
    Left = 368
    Top = 200
    Width = 23
    Height = 22
    Enabled = False
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
      333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
      300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
      333337F373F773333333303330033333333337F3377333333333303333333333
      333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
      333337777F337F33333330330BB00333333337F373F773333333303330033333
      333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
      333377777F77377733330BBB0333333333337F337F33333333330BB003333333
      333373F773333333333330033333333333333773333333333333}
    NumGlyphs = 2
    OnClick = SBOrdnerClick
  end
  object RGRegOrIni: TRadioGroup
    Left = 16
    Top = 72
    Width = 169
    Height = 73
    Caption = 'Configuration settings'
    ItemIndex = 0
    Items.Strings = (
      'save in Registry'
      'save in INI files')
    TabOrder = 0
    OnClick = RGRegOrIniClick
  end
  object EUserfolder: TEdit
    Left = 32
    Top = 200
    Width = 321
    Height = 21
    Color = clMenu
    Enabled = False
    TabOrder = 1
  end
  object EFolder: TEdit
    Left = 32
    Top = 40
    Width = 321
    Height = 21
    TabOrder = 2
  end
  object BInstall: TButton
    Left = 416
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Install'
    TabOrder = 3
    OnClick = BInstallClick
  end
  object BDeinstall: TButton
    Left = 416
    Top = 76
    Width = 75
    Height = 25
    Caption = 'Uninstall'
    TabOrder = 4
    OnClick = BDeinstallClick
  end
  object BClose: TButton
    Left = 416
    Top = 164
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 5
    OnClick = BCloseClick
  end
  object CBDesktopSymbol: TCheckBox
    Left = 200
    Top = 104
    Width = 169
    Height = 17
    Caption = 'Create desktop symbol'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object CBStartmenu: TCheckBox
    Left = 200
    Top = 80
    Width = 185
    Height = 17
    Caption = 'Create entry in start menu'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object BLanguage: TButton
    Left = 416
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Language'
    TabOrder = 8
    OnClick = BLanguageClick
  end
  object CBPortableApplication: TCheckBox
    Left = 200
    Top = 128
    Width = 209
    Height = 17
    Caption = 'Portable application / USB flash drive'
    TabOrder = 9
    OnClick = CBPortableApplicationClick
  end
  object ODSelect: TOpenDialog
    Left = 440
    Top = 192
  end
end
