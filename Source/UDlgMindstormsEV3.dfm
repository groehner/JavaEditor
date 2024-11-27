object FMindstormsEV3Dialog: TFMindstormsEV3Dialog
  Left = 0
  Top = 0
  Caption = 'Mindstorms EV3'
  ClientHeight = 82
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object BRun: TButton
    Left = 200
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Run'
    ModalResult = 1
    TabOrder = 0
    OnClick = BRunClick
  end
  object BClose: TButton
    Left = 200
    Top = 48
    Width = 90
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 2
  end
  object BControlCenter: TButton
    Left = 8
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Control Center'
    ModalResult = 1
    TabOrder = 1
    OnClick = BControlCenterClick
  end
  object BTerminate: TButton
    Left = 8
    Top = 48
    Width = 90
    Height = 25
    Caption = 'Terminate'
    ModalResult = 1
    TabOrder = 3
    OnClick = BTerminateClick
  end
  object BShutDown: TButton
    Left = 104
    Top = 48
    Width = 90
    Height = 25
    Caption = 'Shutdown'
    TabOrder = 4
    OnClick = BShutDownClick
  end
  object BUpload: TButton
    Left = 104
    Top = 8
    Width = 90
    Height = 25
    Caption = 'Upload'
    TabOrder = 5
    OnClick = BUploadClick
  end
end
