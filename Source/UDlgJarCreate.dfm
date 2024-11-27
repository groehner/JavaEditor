object FJarCreateDialog: TFJarCreateDialog
  Left = 318
  Top = 195
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Jar create'
  ClientHeight = 232
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object BNewItem: TButton
    Left = 288
    Top = 200
    Width = 81
    Height = 25
    Caption = 'New entry'
    TabOrder = 0
    OnClick = BNewItemClick
  end
  object BSave: TButton
    Left = 472
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 2
    OnClick = BSaveClick
  end
  object BDelete: TButton
    Left = 384
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 1
    OnClick = BDeleteClick
  end
  object JarCreateItems: TCheckListBox
    Left = 16
    Top = 8
    Width = 529
    Height = 177
    ItemHeight = 17
    TabOrder = 3
  end
  object ENewItem: TEdit
    Left = 16
    Top = 200
    Width = 266
    Height = 23
    TabOrder = 4
  end
end
