object FClassInsert: TFClassInsert
  Left = 641
  Top = 251
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Insert class'
  ClientHeight = 431
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PListView: TPanel
    Left = 0
    Top = 0
    Width = 469
    Height = 335
    Align = alClient
    TabOrder = 0
    object LVFiles: TListView
      Left = 1
      Top = 1
      Width = 467
      Height = 333
      Align = alClient
      Columns = <
        item
          Caption = 'Class'
          Width = 200
        end
        item
          Caption = 'Package'
          Width = 250
        end>
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = LVFilesColumnClick
      OnCompare = LVFilesCompare
      OnKeyUp = LVFilesKeyUp
    end
  end
  object PButton: TPanel
    Left = 0
    Top = 335
    Width = 469
    Height = 96
    Align = alBottom
    TabOrder = 1
    object BCancel: TButton
      Left = 373
      Top = 64
      Width = 90
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object BOK: TButton
      Left = 373
      Top = 33
      Width = 90
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object CBArchiv: TComboBox
      Left = 0
      Top = 6
      Width = 463
      Height = 23
      TabOrder = 2
      OnSelect = CBArchivSelect
    end
    object BOpenArchive: TButton
      Left = 269
      Top = 33
      Width = 90
      Height = 25
      Caption = 'Open archiv'
      TabOrder = 3
      OnClick = BOpenArchiveClick
    end
    object RGOptions: TRadioGroup
      Left = 17
      Top = 33
      Width = 232
      Height = 56
      Caption = 'Options'
      ItemIndex = 0
      Items.Strings = (
        'Selected class only'
        'Completely')
      TabOrder = 4
    end
  end
  object ODClassInsert: TOpenDialog
    Filter = 'zip|*.zip|jar|*.jar|All|*.*'
    Left = 40
    Top = 240
  end
end
