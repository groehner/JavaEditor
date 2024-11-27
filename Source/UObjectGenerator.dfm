object FObjectGenerator: TFObjectGenerator
  Left = 599
  Top = 236
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Object generator'
  ClientHeight = 519
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object StatusBar: TStatusBar
    Left = 0
    Top = 488
    Width = 219
    Height = 31
    Panels = <>
    DesignSize = (
      219
      31)
    object BCancel: TButton
      Left = 160
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akLeft]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object BFont: TButton
      Left = 81
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Font'
      TabOrder = 1
      OnClick = MIFontClick
    end
    object BOK: TButton
      Left = 0
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akLeft]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object ValueListEditor: TValueListEditor
    Left = 0
    Top = 0
    Width = 219
    Height = 488
    Align = alClient
    Constraints.MinWidth = 219
    TabOrder = 0
    TitleCaptions.Strings = (
      'Attribute'
      'Value')
    OnEditButtonClick = ValueListEditorEditButtonClick
    OnKeyUp = ValueListEditorKeyUp
    ColWidths = (
      145
      76)
  end
  object PictureDialog: TOpenDialog
    Filter = 
      'JPEG and GIF|*.jpg;*.jpeg;*.gif|JPEG (*.jpg, *.jpeg)|*.jpg;*.jpe' +
      'g|GIF (*.gif)|*.gif|All (*.*)|*.*'
    FilterIndex = 0
    Left = 62
    Top = 128
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 150
    Top = 136
  end
end
