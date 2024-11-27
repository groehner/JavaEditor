object FMindstormsDialog: TFMindstormsDialog
  Left = 0
  Top = 0
  Caption = 'Mindstorms'
  ClientHeight = 179
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object GBOptionen: TGroupBox
    Left = 8
    Top = 8
    Width = 281
    Height = 98
    Caption = 'Options'
    TabOrder = 0
    object CBVerbose: TCheckBox
      Left = 16
      Top = 70
      Width = 217
      Height = 17
      Caption = 'Detailed information (-v)'
      TabOrder = 0
    end
    object CBDebug: TCheckBox
      Left = 16
      Top = 47
      Width = 249
      Height = 17
      Caption = 'Interruptable with ENTER+ESCAPE (-g)'
      TabOrder = 1
    end
    object CBRun: TCheckBox
      Left = 16
      Top = 24
      Width = 217
      Height = 17
      Caption = 'Run after upload (-r)'
      TabOrder = 2
    end
  end
  object BRun: TButton
    Left = 198
    Top = 112
    Width = 90
    Height = 25
    Caption = 'Run'
    ModalResult = 1
    TabOrder = 1
    OnClick = BRunClick
  end
  object BClose: TButton
    Left = 198
    Top = 143
    Width = 90
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 4
  end
  object BControlCenter: TButton
    Left = 103
    Top = 112
    Width = 90
    Height = 25
    Caption = 'Control Center'
    ModalResult = 1
    TabOrder = 2
    OnClick = BControlCenterClick
  end
  object BFlash: TButton
    Left = 103
    Top = 143
    Width = 90
    Height = 25
    Caption = 'Flash'
    ModalResult = 1
    TabOrder = 3
    OnClick = BFlashClick
  end
  object BBrowser: TButton
    Left = 8
    Top = 112
    Width = 90
    Height = 25
    Caption = 'Browser'
    ModalResult = 1
    TabOrder = 5
    OnClick = BBrowserClick
  end
  object BConsoleViewer: TButton
    Left = 8
    Top = 143
    Width = 90
    Height = 25
    Caption = 'Console Viewer'
    ModalResult = 1
    TabOrder = 6
    OnClick = BConsoleViewerClick
  end
end
