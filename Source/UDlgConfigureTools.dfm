object FConfigureTools: TFConfigureTools
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Configure tools'
  ClientHeight = 247
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object LTools: TLabel
    Left = 16
    Top = 16
    Width = 27
    Height = 15
    Caption = 'Tools'
  end
  object LTitle: TLabel
    Left = 192
    Top = 16
    Width = 22
    Height = 15
    Caption = 'Title'
  end
  object LProgram: TLabel
    Left = 192
    Top = 72
    Width = 46
    Height = 15
    Caption = 'Program'
  end
  object LParameter: TLabel
    Left = 192
    Top = 128
    Width = 54
    Height = 15
    Caption = 'Parameter'
  end
  object SPUp: TSpeedButton
    Left = 112
    Top = 216
    Width = 25
    Height = 25
    Glyph.Data = {
      CE000000424DCE0000000000000076000000280000000A0000000B0000000100
      04000000000058000000880B0000880B00001000000000000000000000003300
      0000003300003333000066000000663300009900000099330000CC000000CC33
      00005858C000FF000000FF603000FF983000FFC83000FFFF9800AAA0DDCAAA01
      1300AAA0DFCAAA011300AAA0DFCAAA011300AAA0DFCAAA011300AAA0DFCAAA01
      1300AAA0DFCAAA0113000000DF0000011300A0DDDFFFCA011300AA0DEFFCAA01
      1300AAA0DFCAAA011300AAAA0CAAAA011300}
    OnClick = SPUpClick
  end
  object SPDown: TSpeedButton
    Left = 143
    Top = 216
    Width = 25
    Height = 25
    Glyph.Data = {
      CE000000424DCE0000000000000076000000280000000A0000000B0000000100
      04000000000058000000880B0000880B00001000000000000000000000003300
      0000003300003333000066000000663300009900000099330000CC000000CC33
      00005858C000FF000000FF603000FF983000FFC83000FFFF9800AAAAC0AAAA01
      1300AAACFD0AAA011300AACFFED0AA011300ACFFFDDD0A0113000000FD000001
      1300AAACFD0AAA011300AAACFD0AAA011300AAACFD0AAA011300AAACFD0AAA01
      1300AAACFD0AAA011300AAACDD0AAA011300}
    OnClick = SPDownClick
  end
  object LBTools: TListBox
    Left = 16
    Top = 35
    Width = 152
    Height = 167
    ItemHeight = 15
    TabOrder = 3
    OnClick = LBToolsClick
  end
  object ETitle: TEdit
    Left = 192
    Top = 35
    Width = 257
    Height = 23
    TabOrder = 0
  end
  object EProgram: TEdit
    Left = 192
    Top = 91
    Width = 257
    Height = 23
    TabOrder = 1
  end
  object EParameter: TEdit
    Left = 192
    Top = 147
    Width = 257
    Height = 23
    TabOrder = 2
    Text = '%ACTIVEWINDOW%'
  end
  object BProgram: TButton
    Left = 455
    Top = 91
    Width = 75
    Height = 21
    Caption = 'Program'
    TabOrder = 4
    OnClick = BProgramClick
  end
  object BParameter: TButton
    Left = 455
    Top = 147
    Width = 75
    Height = 21
    Caption = 'Default'
    TabOrder = 5
    OnClick = BParameterClick
  end
  object BDelete: TButton
    Left = 16
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = BDeleteClick
  end
  object BSave: TButton
    Left = 285
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = BSaveClick
  end
  object BClose: TButton
    Left = 374
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Close'
    Default = True
    TabOrder = 8
    OnClick = BCloseClick
  end
  object CBWait: TCheckBox
    Left = 192
    Top = 185
    Width = 201
    Height = 17
    Caption = 'Wait for execution'
    TabOrder = 9
  end
  object BNew: TButton
    Left = 192
    Top = 216
    Width = 75
    Height = 25
    Caption = 'New'
    TabOrder = 10
    OnClick = BNewClick
  end
  object OpenDialog: TOpenDialog
    FileName = '*.exe'
    Filter = '*.exe|exe|*.com|com'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 464
    Top = 24
  end
end
