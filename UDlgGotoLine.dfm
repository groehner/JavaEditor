object FGotoLineDialog: TFGotoLineDialog
  Left = 621
  Top = 215
  ActiveControl = CBLinenumber
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Line number'
  ClientHeight = 40
  ClientWidth = 183
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  OnCreate = FormCreate
  TextHeight = 15
  object LGoto: TLabel
    Left = 8
    Top = 14
    Width = 45
    Height = 13
    Caption = 'Go to line'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object CBLinenumber: TComboBox
    Left = 91
    Top = 10
    Width = 78
    Height = 23
    AutoComplete = False
    AutoDropDown = True
    TabOrder = 0
    OnKeyPress = CBLinenumberKeyPress
  end
end
