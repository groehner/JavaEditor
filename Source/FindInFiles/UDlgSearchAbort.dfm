object FSearchAbort: TFSearchAbort
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 42
  ClientWidth = 152
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object BSearchAbort: TButton
    Left = 8
    Top = 8
    Width = 134
    Height = 25
    Caption = 'Abort search'
    TabOrder = 0
    OnClick = BSearchAbortClick
  end
end
