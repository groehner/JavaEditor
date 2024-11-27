object FFavoritenDialog: TFFavoritenDialog
  Left = 554
  Top = 161
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Edit favorites'
  ClientHeight = 156
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LDescription: TLabel
    Left = 8
    Top = 64
    Width = 60
    Height = 15
    Caption = 'Description'
  end
  object LUrl: TLabel
    Left = 8
    Top = 8
    Width = 21
    Height = 15
    Caption = 'URL'
  end
  object EDescription: TEdit
    Left = 16
    Top = 84
    Width = 377
    Height = 23
    TabOrder = 0
  end
  object EUrl: TEdit
    Left = 16
    Top = 32
    Width = 377
    Height = 23
    TabOrder = 1
    OnKeyDown = EUrlKeyDown
  end
  object BSave: TButton
    Left = 320
    Top = 122
    Width = 75
    Height = 25
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object BCancel: TButton
    Left = 232
    Top = 122
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
