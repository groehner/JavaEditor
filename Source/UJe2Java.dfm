object FJe2Java: TFJe2Java
  Left = 0
  Top = 0
  Caption = 'FJe2Java'
  ClientHeight = 405
  ClientWidth = 721
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 49
    Width = 721
    Height = 356
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 717
    ExplicitHeight = 355
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 721
    Height = 49
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 717
    object LCommand: TLabel
      Left = 16
      Top = 19
      Width = 47
      Height = 13
      Caption = 'Command'
    end
    object ECommand: TEdit
      Left = 69
      Top = 16
      Width = 276
      Height = 21
      TabOrder = 0
    end
    object BExecute: TButton
      Left = 360
      Top = 13
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 1
      OnClick = BExecuteClick
    end
  end
  object aPipeServer: TPipeServer
    Active = False
    OnPipeConnect = aPipeServerPipeConnect
    OnPipeDisconnect = aPipeServerPipeDisconnect
    OnPipeMessage = aPipeServerPipeMessage
    OnPipeError = aPipeServerPipeError
    PipeName = 'je2java'
    Left = 184
    Top = 136
  end
  object aConsolePipeServer: TPipeServer
    Active = False
    OnPipeConnect = aPipeServerPipeConnect
    OnPipeDisconnect = aPipeServerPipeDisconnect
    OnPipeMessage = aConsolePipeServerPipeMessage
    OnPipeError = aConsolePipeServerPipeError
    PipeName = 'javaconsole'
    Left = 288
    Top = 136
  end
end
