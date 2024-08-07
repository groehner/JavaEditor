object FOpenFolderDialog: TFOpenFolderDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Open folder'
  ClientHeight = 397
  ClientWidth = 398
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  DesignSize = (
    398
    397)
  TextHeight = 15
  object LFiletype: TLabel
    Left = 14
    Top = 339
    Width = 76
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Select filetype:'
  end
  object BOK: TButton
    Left = 227
    Top = 364
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object BCancel: TButton
    Left = 315
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object CBFiletype: TComboBox
    Left = 108
    Top = 335
    Width = 69
    Height = 23
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    TabStop = False
    ExplicitTop = 334
  end
  object CBWithSubFolder: TCheckBox
    Left = 1
    Top = 370
    Width = 161
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'With subfolders'
    Checked = True
    State = cbChecked
    TabOrder = 3
    ExplicitLeft = -3
    ExplicitTop = 369
  end
  object CBPath: TComboBox
    Left = 14
    Top = 296
    Width = 376
    Height = 23
    TabOrder = 4
    OnChange = CBPathChange
  end
end
