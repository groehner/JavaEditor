object FAbout: TFAbout
  Left = 579
  Top = 258
  ActiveControl = BOK
  BorderStyle = bsDialog
  Caption = 'Info'
  ClientHeight = 264
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object BOK: TButton
    Left = 128
    Top = 231
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    IsControl = True
  end
  object GBComponents: TGroupBox
    Left = 16
    Top = 120
    Width = 297
    Height = 105
    Caption = 'Used components'
    TabOrder = 1
    object LEditor: TLabel
      Left = 16
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Editor'
    end
    object LEditorValue: TLabel
      Left = 120
      Top = 24
      Width = 54
      Height = 13
      Caption = 'SynEdit 1.3'
    end
    object LUML: TLabel
      Left = 16
      Top = 48
      Width = 93
      Height = 13
      Caption = 'UML representation'
    end
    object LUMLvalue: TLabel
      Left = 120
      Top = 48
      Width = 52
      Height = 13
      Caption = 'ESS model'
    end
    object Label1: TLabel
      Left = 120
      Top = 72
      Width = 53
      Height = 13
      Caption = 'madExcept'
    end
    object Label2: TLabel
      Left = 16
      Top = 72
      Width = 90
      Height = 13
      Caption = 'Exception handling'
    end
  end
  object GBVersion: TGroupBox
    Left = 16
    Top = 16
    Width = 297
    Height = 81
    Caption = 'Version and Author'
    TabOrder = 2
    object ProgramIcon: TImage
      Left = 16
      Top = 24
      Width = 41
      Height = 41
      Picture.Data = {
        055449636F6E0000010001002020100000000000E80200001600000028000000
        2000000040000000010004000000000080020000000000000000000000000000
        0000000000000000000080000080000000808000800000008000800080800000
        80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
        FFFFFF0000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000000000F000000000000000
        0000000000000000FF0000000000FF00000000000000000FFFF0000000FFFFF0
        000000000000000FFFFFFFFFFFFFFFF0000000000000000FFFFF0FFFF0FFFFF0
        000000000000000FFFFF000000FFFFF0000000000000000FFFFF0FFFF0FFFF00
        0000000000000000FFFFFF0FFFFFFF0000000000000000000FFF001000FFFF00
        00000000000000000FF01111110FF00000000000000000000FF01111110F0000
        0000000000000000000017111110000000000000000000000000FF8111100000
        0000000000000000000007711100000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7FFBFFFE3FF1FFFE1FC1EF
        FC0000EFFC0000E3FC000087FC0000C3FC0001DFFE0001DFFE00019FFF00003F
        FC0003FFFD8007FFF98007FFFBC007FFFBC007FFFBE00FFFFBE00FFFFBF01FFF
        FBF81FFFF9FC3FFFF1FE3FFFE0FF7FFFCE7FFFFF9F7FFFFFFFFFFFFFFFFFFFFF
        FFFFFFFF}
      IsControl = True
    end
    object LProductName: TLabel
      Left = 80
      Top = 24
      Width = 148
      Height = 13
      Caption = 'Java-Editor, 1.6 vom 14.9.2000'
      IsControl = True
    end
    object LNameOfCopyrights: TLabel
      Left = 80
      Top = 48
      Width = 91
      Height = 13
      Caption = '(c) Gerhard R'#246'hner'
      IsControl = True
    end
  end
end
