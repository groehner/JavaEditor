object FClasspath: TFClasspath
  Left = 318
  Top = 195
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit classpath'
  ClientHeight = 235
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 13
  object BNewJarFile: TButton
    Left = 168
    Top = 200
    Width = 99
    Height = 25
    Caption = 'New jar file'
    TabOrder = 0
    OnClick = BNewJarFileClick
  end
  object BNewFolder: TButton
    Left = 280
    Top = 200
    Width = 90
    Height = 25
    Caption = 'New folder'
    TabOrder = 1
    OnClick = BNewFolderClick
  end
  object BSave: TButton
    Left = 472
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 3
    OnClick = BSaveClick
  end
  object BDelete: TButton
    Left = 384
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = BDeleteClick
  end
  object CLBPfade: TCheckListBox
    Left = 8
    Top = 8
    Width = 537
    Height = 177
    ItemHeight = 17
    TabOrder = 4
    OnClick = CLBPfadeClick
  end
  object CBAllJarFiles: TCheckBox
    Left = 66
    Top = 204
    Width = 97
    Height = 17
    Caption = 'all jar files'
    TabOrder = 5
    OnClick = CBAllJarFilesClick
  end
  object BNewAllClasses: TButton
    Left = 240
    Top = 80
    Width = 130
    Height = 25
    Caption = 'allclasses-index.html'
    TabOrder = 6
    OnClick = BNewAllClassesClick
  end
end
