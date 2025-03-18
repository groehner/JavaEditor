object FConfiguration: TFConfiguration
  Left = 203
  Top = 191
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Configuration'
  ClientHeight = 487
  ClientWidth = 776
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PMain: TPanel
    Left = 0
    Top = 0
    Width = 776
    Height = 487
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object TVConfiguration: TTreeView
      Left = 0
      Top = 0
      Width = 145
      Height = 487
      Align = alLeft
      AutoExpand = True
      HideSelection = False
      Indent = 19
      TabOrder = 0
      OnChange = TVConfigurationChange
      Items.NodeData = {
        071600000009540054007200650065004E006F00640065002700000000000000
        00000000FFFFFFFFFFFFFFFF0000000000000000000600000001044A00610076
        0061000000350000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000000000000010B49006E007400650072007000720065007400650072000000
        2F0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000
        00010843006F006D00700069006C006500720000002F00000000000000000000
        00FFFFFFFFFFFFFFFF000000000000000000000000000108500072006F006700
        720061006D00730000002D0000000000000000000000FFFFFFFFFFFFFFFF0000
        000000000000000000000001074100700070006C006500740073000000370000
        000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000010C
        44006900730061007300730065006D0062006C00650072000000250000000000
        000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001034A0061
        00720000002B0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        000006000000010645006400690074006F00720000002D000000000000000000
        0000FFFFFFFFFFFFFFFF0000000000000000000000000001074F007000740069
        006F006E0073000000270000000000000000000000FFFFFFFFFFFFFFFF000000
        00000000000000000000010443006F006400650000002B000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010643006F006C006F
        007200730000002D0000000000000000000000FFFFFFFFFFFFFFFF0000000000
        0000000000000000010743006F006D006D0065006E0074000000310000000000
        000000000000FFFFFFFFFFFFFFFF000000000000000000000000000109540065
        006D0070006C00610074006500730000002F0000000000000000000000FFFFFF
        FFFFFFFFFF0000000000000000000000000001084B006500790062006F006100
        720064000000370000000000000000000000FFFFFFFFFFFFFFFF000000000000
        00000000000000010C4700550049002000640065007300690067006E00650072
        000000350000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        00000000010B5300740072007500630074006F006700720061006D0000003F00
        00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        10530065007100750065006E00630065006400690061006700720061006D006D
        0000002D0000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        000000000107420072006F007700730065007200000039000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010D44006F00630075
        006D0065006E0074006100740069006F006E0000002D00000000000000000000
        00FFFFFFFFFFFFFFFF0000000000000000000000000001075000720069006E00
        7400650072000000330000000000000000000000FFFFFFFFFFFFFFFF00000000
        000000000000000000010A4D0069006E006400730074006F0072006D00730000
        002D0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000
        0000010741006E00640072006F006900640000002F0000000000000000000000
        FFFFFFFFFFFFFFFF0000000000000000000000000001084C0061006E00670075
        0061006700650000002D0000000000000000000000FFFFFFFFFFFFFFFF000000
        0000000000000000000001074F007000740069006F006E00730000002B000000
        0000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000010653
        00740079006C00650073000000370000000000000000000000FFFFFFFFFFFFFF
        FF00000000000000000000000000010C52006500730074007200690063007400
        69006F006E0073000000290000000000000000000000FFFFFFFFFFFFFFFF0000
        000000000000000000000001054C0069006E006B007300000025000000000000
        0000000000FFFFFFFFFFFFFFFF00000000000000000000000000010355004D00
        4C000000350000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
        0000000000010B55004D004C0020006F007000740069006F006E007300000039
        0000000000000000000000FFFFFFFFFFFFFFFF00000000000000000000000000
        010D4C004C004D00200041007300730069007300740061006E00740000002F00
        00000000000000000000FFFFFFFFFFFFFFFF0000000000000000000000000001
        084C004C004D00200043006800610074000000330000000000000000000000FF
        FFFFFFFFFFFFFF00000000000000000000000000010A56006900730069006200
        69006C006900740079000000310000000000000000000000FFFFFFFFFFFFFFFF
        0000000000000000000000000001094C006F0067002000660069006C00650073
        000000290000000000000000000000FFFFFFFFFFFFFFFF000000000000000000
        05000000010554006F006F006C0073000000250000000000000000000000FFFF
        FFFFFFFFFFFF0000000000000000000000000001034700690074000000290000
        000000000000000000FFFFFFFFFFFFFFFF000000000000000000000000000105
        4A0055006E00690074000000330000000000000000000000FFFFFFFFFFFFFFFF
        00000000000000000000000000010A43006800650063006B007300740079006C
        00650000002B0000000000000000000000FFFFFFFFFFFFFFFF00000000000000
        00000000000001064A0061006C006F0070007900000033000000000000000000
        0000FFFFFFFFFFFFFFFF00000000000000000000000000010A53007500620076
        0065007200730069006F006E00}
    end
    object PPanelRight: TPanel
      Left = 145
      Top = 0
      Width = 631
      Height = 487
      Align = alClient
      TabOrder = 1
      object PButtons: TPanel
        Left = 1
        Top = 445
        Width = 629
        Height = 41
        Align = alBottom
        TabOrder = 0
        object BSave: TButton
          Left = 549
          Top = 9
          Width = 75
          Height = 25
          Hint = 'Save the current settings'
          Caption = 'Save'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = BSaveClick
        end
        object BCancel: TButton
          Left = 465
          Top = 9
          Width = 75
          Height = 25
          Caption = 'Cancel'
          TabOrder = 1
          OnClick = BCancelClick
        end
        object BDump: TButton
          Left = 380
          Top = 9
          Width = 75
          Height = 25
          Hint = 'Outputs the entire configuration as a text file.'
          Caption = 'Dump'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = BDumpClick
        end
        object BCheck: TButton
          Left = 294
          Top = 9
          Width = 75
          Height = 25
          Hint = 'Checks the paths and files'
          Caption = 'Check'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = BCheckClick
        end
        object BHelp: TButton
          Left = 209
          Top = 9
          Width = 75
          Height = 25
          Caption = 'Help'
          TabOrder = 4
          OnClick = BHelpClick
        end
      end
      object PTitle: TPanel
        Left = 1
        Top = 1
        Width = 629
        Height = 20
        Align = alTop
        BevelOuter = bvSpace
        TabOrder = 1
        object LTitle: TLabel
          Left = 8
          Top = 4
          Width = 26
          Height = 13
          Caption = 'Title'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object PageList: TPageControl
        Left = 1
        Top = 21
        Width = 629
        Height = 424
        ActivePage = PLLMAssistant
        Align = alClient
        Style = tsFlatButtons
        TabOrder = 2
        object PJava: TTabSheet
          Caption = 'Java'
        end
        object PInterpreter: TTabSheet
          Caption = 'Interpreter'
          object LJavaInterpreter: TLabel
            Left = 16
            Top = 52
            Width = 48
            Height = 13
            Caption = 'Interpreter'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavaInterpreterClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LInterpreterParameter: TLabel
            Left = 36
            Top = 84
            Width = 48
            Height = 13
            Caption = 'Parameter'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LInterpreterParameterClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LClasspathUser: TLabel
            Left = 16
            Top = 212
            Width = 76
            Height = 15
            Caption = 'Classpath user'
          end
          object LClasspathAdmin: TLabel
            Left = 16
            Top = 180
            Width = 88
            Height = 15
            Caption = 'Classpath admin'
          end
          object SBJDKFolderSelect: TSpeedButton
            Tag = 8
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = BJDKFolderSelectClick
          end
          object LJavaDocs: TLabel
            Left = 16
            Top = 244
            Width = 73
            Height = 15
            Caption = 'JavaDocs user'
          end
          object LJavaFXFolder: TLabel
            Left = 16
            Top = 116
            Width = 69
            Height = 15
            Caption = 'JavaFX folder'
          end
          object LJavaFXParameter: TLabel
            Left = 36
            Top = 148
            Width = 92
            Height = 15
            Caption = 'JavaFX parameter'
          end
          object LSDKFolder: TLabel
            Left = 16
            Top = 20
            Width = 53
            Height = 15
            Caption = 'JDK folder'
          end
          object EInterpreterParameter: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            TabOrder = 0
          end
          object EInterpreter: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            Color = clSilver
            TabOrder = 1
          end
          object EClasspathUser: TEdit
            Left = 140
            Top = 208
            Width = 381
            Height = 23
            Hint = 'User specific jar files can be specified here'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object EClasspathAdmin: TEdit
            Left = 140
            Top = 176
            Width = 381
            Height = 23
            TabOrder = 3
            Text = '.'
          end
          object BJDKInstall: TButton
            Left = 550
            Top = 15
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 4
            OnClick = BJDKInstallClick
          end
          object BInterpreterParameter: TButton
            Left = 550
            Top = 79
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 5
            OnClick = BInterpreterParameterClick
          end
          object BInterpreter: TButton
            Left = 550
            Top = 47
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 6
            OnClick = BInterpreterClick
          end
          object BClasspathUser: TButton
            Left = 550
            Top = 208
            Width = 85
            Height = 23
            Caption = 'Edit'
            TabOrder = 7
            OnClick = BClasspathUserClick
          end
          object BClasspathAdmin: TButton
            Left = 550
            Top = 175
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 8
            OnClick = BClasspathAdminClick
          end
          object CBJDKFolder: TComboBox
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            Color = clBtnFace
            TabOrder = 9
            OnSelect = CBJDKFolderSelect
          end
          object EJavaDocs: TEdit
            Left = 140
            Top = 240
            Width = 381
            Height = 23
            Hint = 'Folder with additional JavaDoc documentation'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 10
          end
          object BJavaDocFolder: TButton
            Left = 550
            Top = 239
            Width = 85
            Height = 23
            Caption = 'Edit'
            TabOrder = 11
            OnClick = BJavaDocFolderClick
          end
          object CBShowInterpreterCall: TCheckBox
            Left = 140
            Top = 274
            Width = 142
            Height = 17
            Caption = 'Show interpreter call'
            TabOrder = 12
          end
          object EJavaFXFolder: TEdit
            Left = 140
            Top = 112
            Width = 381
            Height = 23
            TabOrder = 13
          end
          object BJavaFXFolder: TButton
            Left = 550
            Top = 111
            Width = 85
            Height = 25
            Caption = 'Select'
            TabOrder = 14
            OnClick = BJavaFXFolderClick
          end
          object BJavaFXParameter: TButton
            Left = 550
            Top = 143
            Width = 85
            Height = 25
            Caption = 'Default'
            TabOrder = 15
            OnClick = BJavaFXParameterClick
          end
          object EJavaFXParameter: TEdit
            Left = 140
            Top = 144
            Width = 381
            Height = 23
            TabOrder = 16
          end
          object BRunJava: TButton
            Left = 550
            Top = 271
            Width = 85
            Height = 23
            Hint = 'Opens the last batch file for the interpreter call'
            Caption = 'RunJava.BAT'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 17
            OnClick = BRunJavaClick
          end
          object GBConsoleOptions: TGroupBox
            Left = 140
            Top = 305
            Width = 261
            Height = 89
            Caption = 'Console'
            TabOrder = 18
            object LFileEncoding: TLabel
              Left = 16
              Top = 60
              Width = 82
              Height = 15
              Caption = '-Dfile.encoding'
            end
            object LCodepage: TLabel
              Left = 16
              Top = 28
              Width = 54
              Height = 15
              Caption = 'Codepage'
            end
            object CBFileEncoding: TComboBox
              Left = 104
              Top = 56
              Width = 145
              Height = 23
              TabOrder = 0
              Items.Strings = (
                ''
                'ANSI'
                'UTF-8'
                'UTF-16')
            end
            object CBCodepage: TComboBox
              Left = 104
              Top = 24
              Width = 145
              Height = 23
              TabOrder = 1
              Text = '850 Default (Latin 1)'
              Items.Strings = (
                ''
                '437 USA'
                '850 Latin 1'
                '852 Latin 2'
                '855 Cyrillic'
                '857 Turkish'
                '860 Portuguese'
                '861 Icelandic'
                '863 Canadian-French'
                '865 Nordic'
                '866 Russian'
                '869 Modern Greek'
                '936 Chinese Simplified'
                '950 Chinese Traditional'
                '65001 UTF-8')
            end
          end
        end
        object PCompiler: TTabSheet
          Caption = 'Compiler'
          object LJavaCompilerParameter: TLabel
            Left = 40
            Top = 52
            Width = 48
            Height = 13
            Caption = 'Parameter'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavaCompilerParameterClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LJavaCompiler: TLabel
            Left = 16
            Top = 20
            Width = 65
            Height = 13
            Caption = 'Java compiler'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavaCompilerClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LCompilerEncoding: TLabel
            Left = 40
            Top = 84
            Width = 55
            Height = 15
            Caption = '-encoding'
          end
          object EJavaCompilerParameter: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 0
            Text = '-deprecation'
          end
          object BJavaParameter: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 1
            OnClick = BJavaParameterClick
          end
          object BJavaCompiler: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BJavaCompilerClick
          end
          object EJavaCompiler: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            TabOrder = 3
          end
          object CBUseJavaCompilerInternally: TCheckBox
            Left = 140
            Top = 112
            Width = 212
            Height = 17
            Caption = 'Use Java compiler internally'
            TabOrder = 4
          end
          object CBShowCompilerCall: TCheckBox
            Left = 140
            Top = 132
            Width = 305
            Height = 17
            Caption = 'Show compiler call'
            TabOrder = 5
          end
          object CBCompilerEncoding: TComboBox
            Left = 140
            Top = 76
            Width = 120
            Height = 23
            TabOrder = 6
            Items.Strings = (
              ''
              'ANSI'
              'UTF-8'
              'UTF-16')
          end
        end
        object PPrograms: TTabSheet
          Caption = 'Programs'
          object LJavaDocParameter: TLabel
            Left = 40
            Top = 84
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LJavaDoc: TLabel
            Left = 16
            Top = 52
            Width = 43
            Height = 13
            Caption = 'JavaDoc'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavaDocClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LDebugger: TLabel
            Left = 16
            Top = 20
            Width = 47
            Height = 13
            Caption = 'Debugger'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LDebuggerClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object BJavaDocParameter: TButton
            Left = 550
            Top = 80
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 0
            OnClick = BJavaDocParameterClick
          end
          object EDocParameter: TEdit
            Left = 140
            Top = 80
            Width = 394
            Height = 23
            TabOrder = 1
            Text = '-author -version'
          end
          object BJavaDoc: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BJavaDocClick
          end
          object EJavaDoc: TEdit
            Left = 140
            Top = 48
            Width = 394
            Height = 23
            TabOrder = 3
          end
          object BDebugger: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 4
            OnClick = BDebuggerClick
          end
          object EDebugger: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 5
          end
        end
        object PApplets: TTabSheet
          Caption = 'Applets'
          object LAppletviewer: TLabel
            Left = 16
            Top = 20
            Width = 61
            Height = 13
            Caption = 'Appletviewer'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LAppletviewerClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object EAppletviewer: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 0
          end
          object BAppletviewer: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 1
            OnClick = BAppletviewerClick
          end
          object RGApplet: TRadioGroup
            Left = 16
            Top = 59
            Width = 433
            Height = 89
            Caption = 'Launching Applets'
            ItemIndex = 0
            Items.Strings = (
              'Appletviewer for Java files and Browser for HTML files'
              'Always Appletviewer'
              'Always Browser')
            TabOrder = 2
          end
          object CBShowHTMLforApplet: TCheckBox
            Left = 16
            Top = 168
            Width = 489
            Height = 17
            Caption = 'Show HTML file for Applet'
            TabOrder = 3
          end
        end
        object PDisassembler: TTabSheet
          Caption = 'Disassembler'
          object LDissasemblerParameter: TLabel
            Left = 40
            Top = 52
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LDisassembler: TLabel
            Left = 16
            Top = 20
            Width = 62
            Height = 13
            Caption = 'Disassembler'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LDisassemblerClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object SBDisassemblerSelect: TSpeedButton
            Tag = 19
            Left = 523
            Top = 16
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBDisassemblerSelectClick
          end
          object BDisassemblerParameter: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 0
            OnClick = BDisassemblerParameterClick
          end
          object BDisassemblerInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 1
            OnClick = BDisassemblerInstallClick
          end
          object EDisassemblerParameter: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 2
            Text = '-author -version'
          end
          object CBDisassembler: TComboBox
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            TabOrder = 3
            OnSelect = CBDisassemblerSelect
            Items.Strings = (
              'jad.exe')
          end
        end
        object PJar: TTabSheet
          Caption = 'Jar'
          object LJarCreate: TLabel
            Left = 40
            Top = 116
            Width = 34
            Height = 15
            Caption = 'Create'
          end
          object LJarManifest: TLabel
            Left = 40
            Top = 84
            Width = 46
            Height = 15
            Caption = 'Manifest'
          end
          object LJarParameter: TLabel
            Left = 40
            Top = 52
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LJar: TLabel
            Left = 16
            Top = 20
            Width = 14
            Height = 13
            Caption = 'Jar'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJarClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LJarPack: TLabel
            Left = 40
            Top = 148
            Width = 25
            Height = 15
            Caption = 'Pack'
          end
          object LJarClasspath: TLabel
            Left = 40
            Top = 180
            Width = 51
            Height = 15
            Caption = 'Classpath'
          end
          object BJarFiles: TButton
            Left = 550
            Top = 112
            Width = 85
            Height = 23
            Caption = 'Edit'
            TabOrder = 0
            OnClick = BJarFilesClick
          end
          object EJarCreate: TEdit
            Left = 140
            Top = 112
            Width = 394
            Height = 23
            TabOrder = 1
            Text = '*.class'
          end
          object BManifest: TButton
            Left = 550
            Top = 80
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 2
            OnClick = BManifestClick
          end
          object EJarManifest: TEdit
            Left = 140
            Top = 80
            Width = 394
            Height = 23
            TabOrder = 3
          end
          object EJarParameter: TEdit
            Left = 140
            Top = 48
            Width = 394
            Height = 23
            TabOrder = 4
            Text = '-cfv'
          end
          object BJarParameter: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 5
            OnClick = BJarParameterClick
          end
          object BJar: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 6
            OnClick = BJarClick
          end
          object EJar: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 7
          end
          object CBJarPack: TComboBox
            Left = 140
            Top = 144
            Width = 394
            Height = 23
            TabOrder = 8
            Items.Strings = (
              'All open files'
              '*.java *.jfm *.uml'
              '*.java *.jfm'
              '*.*')
          end
          object EJarClasspath: TEdit
            Left = 140
            Top = 176
            Width = 394
            Height = 23
            TabOrder = 9
          end
          object BJarClasspath: TButton
            Left = 550
            Top = 176
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 10
            OnClick = BJarClasspathClick
          end
        end
        object PEditor: TTabSheet
          Caption = 'Editor'
        end
        object PEditorOptions: TTabSheet
          Caption = 'Editor options'
          object LEditorIndent: TLabel
            Left = 16
            Top = 52
            Width = 95
            Height = 15
            Caption = 'Indentation depth'
          end
          object LEditorTabWidth: TLabel
            Left = 16
            Top = 20
            Width = 51
            Height = 15
            Caption = 'Tab width'
          end
          object LIntensity: TLabel
            Left = 431
            Top = 305
            Width = 45
            Height = 15
            Caption = 'Intensity'
          end
          object CBShowBracketpair: TCheckBox
            Left = 313
            Top = 176
            Width = 300
            Height = 17
            Caption = 'Show bracket pair'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object CBIndentHelp: TCheckBox
            Left = 313
            Top = 18
            Width = 300
            Height = 17
            Caption = 'Indentation help with tab'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object CBAutomaticIndent: TCheckBox
            Left = 313
            Top = 38
            Width = 300
            Height = 17
            Caption = 'Automatic line indentation'
            Checked = True
            State = cbChecked
            TabOrder = 5
          end
          object ETabWidth: TEdit
            Left = 128
            Top = 16
            Width = 26
            Height = 23
            TabOrder = 6
            Text = '2'
          end
          object EIndent: TEdit
            Left = 128
            Top = 48
            Width = 26
            Height = 23
            TabOrder = 7
            Text = '2'
          end
          object UDIndent: TUpDown
            Left = 154
            Top = 48
            Width = 15
            Height = 23
            Associate = EIndent
            Min = 1
            Max = 10
            Position = 2
            TabOrder = 8
          end
          object UDTabWidth: TUpDown
            Left = 154
            Top = 16
            Width = 15
            Height = 23
            Associate = ETabWidth
            Min = 1
            Max = 10
            Position = 2
            TabOrder = 9
          end
          object CBCursorBehindLine: TCheckBox
            Left = 313
            Top = 58
            Width = 300
            Height = 17
            Caption = 'Cursor may be to the right of the end of the line'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
          object CBCommentClosingBrackets: TCheckBox
            Left = 313
            Top = 196
            Width = 281
            Height = 17
            Caption = 'Comment closing brackets'
            Checked = True
            State = cbChecked
            TabOrder = 10
          end
          object CBStructureColoring: TCheckBox
            Left = 313
            Top = 282
            Width = 300
            Height = 17
            Caption = 'Structured coloring'
            Checked = True
            State = cbChecked
            TabOrder = 11
          end
          object CBInsertControlStructures: TCheckBox
            Left = 313
            Top = 259
            Width = 300
            Height = 17
            Caption = 'Insert control structures automatically'
            Checked = True
            State = cbChecked
            TabOrder = 12
          end
          object CBInsertSemicolons: TCheckBox
            Left = 313
            Top = 98
            Width = 300
            Height = 17
            Caption = 'Add missing semicolons'
            Checked = True
            State = cbChecked
            TabOrder = 13
          end
          object EIntensity: TEdit
            Left = 488
            Top = 301
            Width = 26
            Height = 23
            TabOrder = 14
            Text = '5'
          end
          object UDIntensity: TUpDown
            Left = 514
            Top = 301
            Width = 16
            Height = 23
            Associate = EIntensity
            Max = 20
            Position = 5
            TabOrder = 15
          end
          object CBCompleteBracket: TCheckBox
            Left = 313
            Top = 136
            Width = 300
            Height = 17
            Caption = 'Add bracket } automatically'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object CBLineNumbering: TCheckBox
            Left = 313
            Top = 78
            Width = 300
            Height = 17
            Caption = 'Number lines'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
          object CBIndentafterBracket: TCheckBox
            Left = 313
            Top = 156
            Width = 281
            Height = 17
            Caption = 'After { bracket indent'
            TabOrder = 16
          end
          object CBStructureColoringPlane: TCheckBox
            Left = 330
            Top = 303
            Width = 84
            Height = 17
            Caption = 'Flat'
            TabOrder = 17
          end
          object CBGUICodeFolding: TCheckBox
            Left = 313
            Top = 239
            Width = 296
            Height = 17
            Caption = 'Code folding for GUI elements'
            Checked = True
            State = cbChecked
            TabOrder = 18
          end
          object CB80Columnline: TCheckBox
            Left = 313
            Top = 326
            Width = 169
            Height = 17
            Caption = 'Show 80 characters line'
            TabOrder = 19
          end
          object BEditorFont: TButton
            Left = 16
            Top = 84
            Width = 85
            Height = 23
            Caption = 'Font'
            TabOrder = 20
            OnClick = BEditorFontClick
          end
        end
        object PCode: TTabSheet
          Caption = 'Code'
          object GBCodeCompletion: TGroupBox
            Left = 10
            Top = 14
            Width = 618
            Height = 193
            Caption = 'Code completion'
            TabOrder = 0
            object LCompletionDelay: TLabel
              Left = 14
              Top = 103
              Width = 29
              Height = 15
              Caption = 'Delay'
            end
            object LCompletionHint1: TLabel
              Left = 284
              Top = 122
              Width = 232
              Height = 13
              Caption = 'For code completion and parameter hints'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object LCompletionHint2: TLabel
              Left = 284
              Top = 141
              Width = 240
              Height = 13
              Caption = 'the Java documentation must be installed.'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clRed
              Font.Height = -12
              Font.Name = 'MS Sans Serif'
              Font.Style = [fsBold]
              ParentFont = False
            end
            object LCompletionMin: TLabel
              Left = 16
              Top = 162
              Width = 31
              Height = 15
              Caption = '50 ms'
            end
            object LCompletionMax: TLabel
              Left = 224
              Top = 162
              Width = 43
              Height = 15
              Caption = '1000 ms'
            end
            object LHeightInLines: TLabel
              Left = 288
              Top = 25
              Width = 76
              Height = 15
              Caption = 'Height in lines'
            end
            object LHeigthInLinesMin: TLabel
              Left = 296
              Top = 52
              Width = 21
              Height = 15
              Caption = 'min'
            end
            object LHeigthInLinesMax: TLabel
              Left = 296
              Top = 77
              Width = 23
              Height = 15
              Caption = 'max'
            end
            object CBParameterHints: TCheckBox
              Left = 18
              Top = 50
              Width = 129
              Height = 17
              Caption = 'Parameter hints'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object TBDelay: TTrackBar
              Left = 16
              Top = 122
              Width = 233
              Height = 30
              Max = 1000
              Min = 50
              PageSize = 50
              Frequency = 100
              Position = 100
              TabOrder = 1
            end
            object BFont: TButton
              Left = 516
              Top = 22
              Width = 85
              Height = 23
              Caption = 'Font'
              TabOrder = 2
              OnClick = BFontClick
            end
            object CBShowClassObject: TCheckBox
              Left = 18
              Top = 75
              Width = 133
              Height = 17
              Caption = 'Show class Object'
              TabOrder = 3
            end
            object RBCodeCompletionAlways: TRadioButton
              Left = 18
              Top = 25
              Width = 64
              Height = 17
              Caption = 'Always'
              Checked = True
              TabOrder = 4
              TabStop = True
            end
            object RBCodeCompletionCtrlSpace: TRadioButton
              Left = 80
              Top = 25
              Width = 198
              Height = 17
              Caption = 'With Ctrl+Space'
              TabOrder = 5
            end
            object ESelectionSizeMin: TEdit
              Left = 324
              Top = 48
              Width = 31
              Height = 23
              TabOrder = 6
              Text = '4'
            end
            object ESelectionSizeMax: TEdit
              Left = 324
              Top = 75
              Width = 31
              Height = 23
              TabOrder = 7
              Text = '10'
            end
            object UpDownSelectionSizeMin: TUpDown
              Left = 355
              Top = 48
              Width = 16
              Height = 23
              Associate = ESelectionSizeMin
              Min = 1
              Position = 4
              TabOrder = 8
            end
            object UpDownSelectionSizeMax: TUpDown
              Left = 355
              Top = 75
              Width = 16
              Height = 23
              Associate = ESelectionSizeMax
              Min = 1
              Position = 10
              TabOrder = 9
            end
          end
          object GBTooltips: TGroupBox
            Left = 10
            Top = 215
            Width = 618
            Height = 165
            Caption = 'Tool tips'
            TabOrder = 1
            object LTooltipsDelay: TLabel
              Left = 16
              Top = 82
              Width = 29
              Height = 15
              Caption = 'Delay'
            end
            object LToolTipMin: TLabel
              Left = 16
              Top = 134
              Width = 31
              Height = 15
              Caption = '50 ms'
            end
            object LToolTipMax: TLabel
              Left = 224
              Top = 134
              Width = 43
              Height = 15
              Caption = '2000 ms'
            end
            object TBTooltipDelay: TTrackBar
              Left = 16
              Top = 98
              Width = 233
              Height = 30
              Max = 2000
              Min = 50
              PageSize = 100
              Frequency = 200
              Position = 650
              TabOrder = 0
            end
            object CBTooltipWithKey: TCheckBox
              Left = 18
              Top = 25
              Width = 157
              Height = 17
              Caption = 'With F2 key'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
            object CBTooltipAutomatic: TCheckBox
              Left = 18
              Top = 50
              Width = 156
              Height = 17
              Caption = 'Automatically after'
              Checked = True
              State = cbChecked
              TabOrder = 2
            end
          end
        end
        object PColors: TTabSheet
          Caption = 'Colors'
          object LTextattribute: TLabel
            Left = 357
            Top = 136
            Width = 74
            Height = 15
            Caption = 'Text attributes'
          end
          object LColor: TLabel
            Left = 185
            Top = 136
            Width = 34
            Height = 15
            Caption = 'Colors'
          end
          object LColorElement: TLabel
            Left = 16
            Top = 136
            Width = 43
            Height = 15
            Caption = 'Element'
            FocusControl = LBColorElements
          end
          object LTextColor: TLabel
            Left = 191
            Top = 160
            Width = 21
            Height = 15
            Caption = 'Text'
          end
          object LBackgroundColor: TLabel
            Left = 191
            Top = 211
            Width = 64
            Height = 15
            Caption = 'Background'
          end
          object LActiveLineColor: TLabel
            Left = 16
            Top = 312
            Width = 126
            Height = 15
            Caption = 'Color of the current line'
          end
          object LEditorStyle: TLabel
            Left = 16
            Top = 8
            Width = 58
            Height = 15
            Caption = 'Editor style'
          end
          object BDefaultColors: TButton
            Tag = 2
            Left = 364
            Top = 252
            Width = 121
            Height = 25
            Caption = 'Default'
            TabOrder = 0
            OnClick = BDefaultColorsClick
          end
          object GBTextattribute: TGroupBox
            Left = 364
            Top = 156
            Width = 121
            Height = 80
            TabOrder = 1
            object CBBold: TCheckBox
              Left = 8
              Top = 12
              Width = 97
              Height = 17
              Caption = 'Bold'
              TabOrder = 0
              OnClick = CBColorBoxChange
            end
            object CBItalic: TCheckBox
              Left = 8
              Top = 34
              Width = 89
              Height = 17
              Caption = 'Italic'
              TabOrder = 1
              OnClick = CBColorBoxChange
            end
            object CBUnderline: TCheckBox
              Left = 8
              Top = 56
              Width = 89
              Height = 17
              Caption = 'Underline'
              TabOrder = 2
              OnClick = CBColorBoxChange
            end
          end
          object LBColorElements: TListBox
            Left = 16
            Top = 155
            Width = 133
            Height = 124
            ItemHeight = 15
            Sorted = True
            TabOrder = 2
            OnClick = LBColorElementsClick
          end
          object rgcolors: TRadioGroup
            Left = 16
            Top = 76
            Width = 257
            Height = 41
            Caption = 'Colors for'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'Java'
              'HTML'
              'Brackets')
            TabOrder = 3
            OnClick = rgcolorsClick
          end
          object CBTextColorBox: TColorBox
            Left = 191
            Top = 178
            Width = 133
            Height = 22
            TabOrder = 4
            OnClick = CBColorBoxChange
          end
          object CBBackgroundColorBox: TColorBox
            Left = 191
            Top = 229
            Width = 133
            Height = 22
            TabOrder = 5
            OnCloseUp = CBColorBoxChange
          end
          object CBNoBackgroundColor: TCheckBox
            Left = 193
            Top = 256
            Width = 160
            Height = 17
            Caption = 'No background color'
            TabOrder = 6
            OnClick = CBColorBoxChange
          end
          object CBActiveLineColor: TColorBox
            Left = 16
            Top = 330
            Width = 133
            Height = 22
            TabOrder = 7
          end
          object CBNoActiveLineColor: TCheckBox
            Left = 16
            Top = 357
            Width = 97
            Height = 17
            Caption = 'No color'
            TabOrder = 8
            OnClick = CBNoActiveLineColorClick
          end
          object CBNoSyntaxHighlighting: TCheckBox
            Left = 370
            Top = 91
            Width = 226
            Height = 17
            Caption = 'No syntax highlighting'
            TabOrder = 9
          end
          object CBEditorStyles: TComboBox
            Left = 16
            Top = 27
            Width = 133
            Height = 23
            TabOrder = 10
            OnChange = CBEditorStylesChange
            OnSelect = CBEditorStylesChange
          end
          object BEditorStyleDefault: TButton
            Left = 191
            Top = 26
            Width = 82
            Height = 23
            Caption = 'Default'
            TabOrder = 11
            OnClick = BEditorStyleDefaultClick
          end
        end
        object PComment: TTabSheet
          Caption = 'Comment'
          object LAuthor: TLabel
            Left = 16
            Top = 180
            Width = 37
            Height = 15
            Caption = 'Author'
          end
          object LComment: TLabel
            Left = 16
            Top = 16
            Width = 218
            Height = 15
            Caption = 'Introductory comment for new programs'
          end
          object LMethodComment: TLabel
            Left = 16
            Top = 239
            Width = 189
            Height = 15
            Caption = 'Introductory comment for methods'
          end
          object EAuthor: TEdit
            Left = 32
            Top = 200
            Width = 337
            Height = 23
            TabOrder = 0
          end
          object RGComment: TRadioGroup
            Left = 400
            Top = 36
            Width = 105
            Height = 89
            Caption = 'Kind'
            ItemIndex = 0
            Items.Strings = (
              'JavaDoc'
              'Short'
              'Free')
            TabOrder = 1
            OnClick = RGCommentClick
          end
          object MComment: TMemo
            Left = 32
            Top = 40
            Width = 337
            Height = 129
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 2
          end
          object MMethodComment: TMemo
            Left = 32
            Top = 262
            Width = 337
            Height = 115
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssVertical
            TabOrder = 3
          end
        end
        object PTemplates: TTabSheet
          Caption = 'Templates'
          object LTemplateJApplet: TLabel
            Left = 16
            Top = 212
            Width = 39
            Height = 15
            Caption = 'JApplet'
          end
          object LTemplateControlstructure: TLabel
            Left = 16
            Top = 276
            Width = 95
            Height = 15
            Caption = 'Control structures'
          end
          object LTemplateHint2: TLabel
            Left = 140
            Top = 388
            Width = 266
            Height = 15
            Caption = '%NAME% as file name, %DATE% and %AUTHOR%'
          end
          object LTemplateHint1: TLabel
            Left = 140
            Top = 372
            Width = 320
            Height = 15
            Caption = 'The following text variables can be used in the template files:'
          end
          object LTemplateHint: TLabel
            Left = 16
            Top = 372
            Width = 23
            Height = 15
            Caption = 'Hint'
          end
          object LTemplateApplet: TLabel
            Left = 16
            Top = 116
            Width = 35
            Height = 15
            Caption = 'Applet'
          end
          object LTemplateJFrame: TLabel
            Left = 16
            Top = 148
            Width = 37
            Height = 15
            Caption = 'JFrame'
          end
          object LTemplateFrame: TLabel
            Left = 16
            Top = 52
            Width = 33
            Height = 15
            Caption = 'Frame'
          end
          object LTemplateConsole: TLabel
            Left = 16
            Top = 20
            Width = 43
            Height = 15
            Caption = 'Console'
          end
          object LTemplateDialog: TLabel
            Left = 16
            Top = 84
            Width = 34
            Height = 15
            Caption = 'Dialog'
          end
          object LTemplateJDialog: TLabel
            Left = 16
            Top = 180
            Width = 38
            Height = 15
            Caption = 'JDialog'
          end
          object LTemplateClass: TLabel
            Left = 16
            Top = 308
            Width = 38
            Height = 15
            Caption = 'Classes'
          end
          object LTemplateapplication: TLabel
            Left = 16
            Top = 244
            Width = 61
            Height = 15
            Caption = 'Application'
          end
          object LTemplateJUnit: TLabel
            Left = 16
            Top = 340
            Width = 48
            Height = 15
            Caption = 'JUnit test'
          end
          object BTemplateJApplet: TButton
            Tag = 7
            Left = 550
            Top = 208
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 10
            OnClick = BVorlageClick
          end
          object ETemplateJApplet: TEdit
            Left = 140
            Top = 208
            Width = 394
            Height = 23
            TabOrder = 0
          end
          object BTemplateControlstructure: TButton
            Tag = 9
            Left = 550
            Top = 272
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 13
            OnClick = BVorlageClick
          end
          object ETemplateControlstructure: TEdit
            Left = 140
            Top = 272
            Width = 394
            Height = 23
            TabOrder = 1
          end
          object BTemplateApplet: TButton
            Tag = 4
            Left = 550
            Top = 112
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 6
            OnClick = BVorlageClick
          end
          object ETemplateApplet: TEdit
            Left = 140
            Top = 112
            Width = 394
            Height = 23
            TabOrder = 5
          end
          object BTemplateJFrame: TButton
            Tag = 5
            Left = 550
            Top = 144
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 7
            OnClick = BVorlageClick
          end
          object ETemplateJFrame: TEdit
            Left = 140
            Top = 144
            Width = 394
            Height = 23
            TabOrder = 9
          end
          object BTemplateFrame: TButton
            Tag = 2
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 3
            OnClick = BVorlageClick
          end
          object ETemplateFrame: TEdit
            Left = 140
            Top = 48
            Width = 394
            Height = 23
            TabOrder = 12
          end
          object BTemplateConsole: TButton
            Tag = 1
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BVorlageClick
          end
          object ETemplateConsole: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 15
          end
          object ETemplateDialog: TEdit
            Left = 140
            Top = 80
            Width = 394
            Height = 23
            TabOrder = 16
          end
          object ETemplateJDialog: TEdit
            Left = 140
            Top = 176
            Width = 394
            Height = 23
            TabOrder = 17
          end
          object BTemplateDialog: TButton
            Tag = 3
            Left = 549
            Top = 80
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 4
            OnClick = BVorlageClick
          end
          object BTemplateJDialog: TButton
            Tag = 6
            Left = 550
            Top = 176
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 8
            OnClick = BVorlageClick
          end
          object ETemplateClass: TEdit
            Left = 140
            Top = 304
            Width = 394
            Height = 23
            TabOrder = 18
          end
          object BTemplateClass: TButton
            Tag = 10
            Left = 550
            Top = 304
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 14
            OnClick = BVorlageClick
          end
          object ETemplateApplication: TEdit
            Left = 140
            Top = 240
            Width = 394
            Height = 23
            TabOrder = 19
          end
          object BTemplateApplication: TButton
            Tag = 8
            Left = 549
            Top = 243
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 11
            OnClick = BVorlageClick
          end
          object ETemplateJUnitTest: TEdit
            Left = 140
            Top = 336
            Width = 394
            Height = 23
            TabOrder = 20
          end
          object BTemplateJUnitTest: TButton
            Left = 550
            Top = 336
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 21
            OnClick = BVorlageClick
          end
        end
        object PKeyboard: TTabSheet
          Caption = 'Keyboard'
          object LKeyboardFromFile: TLabel
            Left = 16
            Top = 301
            Width = 47
            Height = 15
            Caption = 'From file'
          end
          object SBKeyboardfile: TSpeedButton
            Tag = 17
            Left = 430
            Top = 298
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = BKeyboardFileClick
          end
          object LCollision: TLabel
            Left = 182
            Top = 70
            Width = 190
            Height = 13
            Caption = 'Attention: predefined system key!'
            Color = clBtnFace
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentColor = False
            ParentFont = False
            Visible = False
          end
          object LKeyboard: TLabel
            Left = 182
            Top = 16
            Width = 187
            Height = 15
            Caption = 'Keyboard shortcuts for source code'
          end
          object BKeyboardFile: TButton
            Tag = 17
            Left = 481
            Top = 296
            Width = 85
            Height = 23
            Caption = 'Open'
            TabOrder = 0
            OnClick = SBOpenClick
          end
          object EKeyboardFile: TEdit
            Left = 75
            Top = 297
            Width = 349
            Height = 23
            TabOrder = 1
          end
          object MKeyboard: TMemo
            Left = 16
            Top = 137
            Width = 548
            Height = 144
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -12
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 2
          end
          object RGKeyboard: TRadioGroup
            Left = 16
            Top = 16
            Width = 137
            Height = 91
            Caption = 'Keyboard shortcuts for'
            ItemIndex = 0
            Items.Strings = (
              'Editor'
              'Menu'
              'All')
            TabOrder = 3
            OnClick = RGKeyboardClick
          end
          object CBKeyboard: TComboBox
            Left = 182
            Top = 41
            Width = 129
            Height = 23
            Sorted = True
            TabOrder = 4
            OnChange = CBKeyboardChange
          end
        end
        object PGUIDesigner: TTabSheet
          Caption = 'GUI designer'
          ImageIndex = 36
          object LGridSize: TLabel
            Left = 59
            Top = 95
            Width = 127
            Height = 15
            Caption = 'Grid size in GUI designer'
          end
          object LAppletheight: TLabel
            Left = 384
            Top = 112
            Width = 72
            Height = 15
            Caption = 'Applet height'
          end
          object LAppletwidth: TLabel
            Left = 384
            Top = 80
            Width = 68
            Height = 15
            Caption = 'Applet width'
          end
          object LFrameheight: TLabel
            Left = 385
            Top = 48
            Width = 70
            Height = 15
            Caption = 'Frame height'
          end
          object LFrameWidth: TLabel
            Left = 385
            Top = 16
            Width = 66
            Height = 15
            Caption = 'Frame width'
          end
          object LDialog12pt: TLabel
            Left = 97
            Top = 203
            Width = 60
            Height = 15
            Caption = 'Dialog 12pt'
          end
          object LDoesntShow: TLabel
            Left = 97
            Top = 172
            Width = 132
            Height = 15
            Caption = 'doesn'#39't show font Dialog'
          end
          object LZoomsteps: TLabel
            Left = 59
            Top = 127
            Width = 87
            Height = 15
            Caption = 'Font zoom steps'
          end
          object CBNameFromText: TCheckBox
            Left = 16
            Top = 16
            Width = 289
            Height = 17
            Caption = 'Name GUI components by attribute text'
            TabOrder = 0
          end
          object EGridSize: TEdit
            Left = 16
            Top = 92
            Width = 21
            Height = 23
            TabOrder = 1
            Text = '11'
          end
          object UDGridSize: TUpDown
            Left = 37
            Top = 92
            Width = 16
            Height = 23
            Associate = EGridSize
            Min = 2
            Max = 60
            Position = 11
            TabOrder = 2
          end
          object CBGuiDesignerHints: TCheckBox
            Left = 16
            Top = 40
            Width = 290
            Height = 17
            Caption = 'Show hints in the GUI designer permanently'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
          object CBSnapToGrid: TCheckBox
            Left = 16
            Top = 64
            Width = 199
            Height = 17
            Caption = 'Snap to grid'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object EAppletwidth: TEdit
            Left = 469
            Top = 77
            Width = 41
            Height = 23
            TabOrder = 5
            Text = '400'
          end
          object EAppletheight: TEdit
            Left = 469
            Top = 109
            Width = 41
            Height = 23
            TabOrder = 6
            Text = '300'
          end
          object EFrameheight: TEdit
            Left = 469
            Top = 45
            Width = 41
            Height = 23
            TabOrder = 7
            Text = '300'
          end
          object EFrameWidth: TEdit
            Left = 469
            Top = 13
            Width = 41
            Height = 23
            TabOrder = 8
            Text = '300'
          end
          object BGuiFont: TButton
            Left = 16
            Top = 168
            Width = 75
            Height = 25
            Caption = 'Font'
            TabOrder = 9
            OnClick = BGuiFontClick
          end
          object BGuiFontDefault: TButton
            Left = 16
            Top = 199
            Width = 75
            Height = 25
            Caption = 'Default'
            TabOrder = 10
            OnClick = BGuiFontDefaultClick
          end
          object UDZoomSteps: TUpDown
            Left = 37
            Top = 124
            Width = 16
            Height = 23
            Associate = EZoomSteps
            Min = 1
            Max = 5
            Position = 1
            TabOrder = 11
          end
          object EZoomSteps: TEdit
            Left = 16
            Top = 124
            Width = 21
            Height = 23
            TabOrder = 12
            Text = '1'
          end
        end
        object PStructogram: TTabSheet
          Caption = 'Structogram'
          object LInput: TLabel
            Left = 16
            Top = 51
            Width = 28
            Height = 15
            Caption = 'Input'
          end
          object LOutput: TLabel
            Left = 16
            Top = 83
            Width = 38
            Height = 15
            Caption = 'Output'
          end
          object LAlgorithm: TLabel
            Left = 16
            Top = 19
            Width = 54
            Height = 15
            Caption = 'Algorithm'
          end
          object LWhile: TLabel
            Left = 16
            Top = 115
            Width = 28
            Height = 15
            Caption = 'while'
          end
          object LDoWhile: TLabel
            Left = 16
            Top = 147
            Width = 45
            Height = 15
            Caption = 'do while'
          end
          object LFor: TLabel
            Left = 16
            Top = 179
            Width = 15
            Height = 15
            Caption = 'for'
          end
          object LYes: TLabel
            Left = 16
            Top = 211
            Width = 17
            Height = 15
            Caption = 'Yes'
          end
          object LNo: TLabel
            Left = 16
            Top = 243
            Width = 16
            Height = 15
            Caption = 'No'
          end
          object LDatatype: TLabel
            Left = 276
            Top = 94
            Width = 50
            Height = 15
            Caption = 'Data type'
          end
          object LCaseCount: TLabel
            Left = 459
            Top = 148
            Width = 73
            Height = 15
            Caption = 'Count of case'
          end
          object LOther: TLabel
            Left = 16
            Top = 275
            Width = 20
            Height = 15
            Caption = 'Else'
          end
          object LStructogramShadow: TLabel
            Left = 493
            Top = 19
            Width = 42
            Height = 15
            Caption = 'Shadow'
          end
          object LStructogramShadowWidth: TLabel
            Left = 546
            Top = 39
            Width = 32
            Height = 15
            Caption = 'Width'
          end
          object LStructogramShadowIntensity: TLabel
            Left = 546
            Top = 66
            Width = 45
            Height = 15
            Caption = 'Intensity'
          end
          object LFor2: TLabel
            Left = 273
            Top = 179
            Width = 337
            Height = 15
            Caption = 
              '[i]=count variable, [1]=start value, [n]=end value, [s]=incremen' +
              't'
          end
          object EInput: TEdit
            Left = 90
            Top = 48
            Width = 140
            Height = 23
            TabOrder = 1
            Text = 'Input:'
          end
          object EOutput: TEdit
            Left = 90
            Top = 80
            Width = 140
            Height = 23
            TabOrder = 2
            Text = 'Output:'
          end
          object EAlgorithm: TEdit
            Left = 90
            Top = 16
            Width = 140
            Height = 23
            TabOrder = 0
            Text = 'Algorithm'
          end
          object EWhile: TEdit
            Left = 90
            Top = 112
            Width = 140
            Height = 23
            TabOrder = 3
            Text = 'repeat while'
          end
          object EDoWhile: TEdit
            Left = 90
            Top = 144
            Width = 140
            Height = 23
            TabOrder = 4
            Text = 'repeat while'
          end
          object EFor: TEdit
            Left = 90
            Top = 176
            Width = 140
            Height = 23
            TabOrder = 5
            Text = 'repeat for [i] = [1] to [n]'
          end
          object EYes: TEdit
            Left = 90
            Top = 208
            Width = 140
            Height = 23
            TabOrder = 6
            Text = 'yes'
          end
          object ENo: TEdit
            Left = 90
            Top = 240
            Width = 140
            Height = 23
            TabOrder = 7
            Text = 'no'
          end
          object RGGenerateJavacode: TRadioGroup
            Left = 267
            Top = 14
            Width = 185
            Height = 70
            Caption = 'Generate Java code'
            ItemIndex = 1
            Items.Strings = (
              'As method'
              'As program')
            TabOrder = 9
          end
          object CBDataType: TComboBox
            Left = 273
            Top = 112
            Width = 140
            Height = 23
            AutoComplete = False
            AutoDropDown = True
            Sorted = True
            TabOrder = 10
            Items.Strings = (
              'boolean'
              'char'
              'double'
              'float'
              'int'
              'String')
          end
          object CBSwitchWithCaseLine: TCheckBox
            Left = 273
            Top = 146
            Width = 136
            Height = 17
            Caption = 'switch with case line'
            TabOrder = 11
          end
          object UpDowncaseCount: TUpDown
            Left = 435
            Top = 144
            Width = 16
            Height = 23
            Associate = ECaseCount
            Max = 10
            Position = 4
            TabOrder = 15
          end
          object ECaseCount: TEdit
            Left = 415
            Top = 144
            Width = 20
            Height = 23
            TabOrder = 12
            Text = '4'
          end
          object EOther: TEdit
            Left = 90
            Top = 272
            Width = 140
            Height = 23
            TabOrder = 8
            Text = 'else'
          end
          object EStructogramShadowWidth: TEdit
            Left = 495
            Top = 36
            Width = 27
            Height = 23
            TabOrder = 13
            Text = '3'
          end
          object UDStructogramShadowWidth: TUpDown
            Left = 522
            Top = 36
            Width = 16
            Height = 23
            Associate = EStructogramShadowWidth
            Position = 3
            TabOrder = 16
          end
          object EStructogramShadowIntensity: TEdit
            Left = 495
            Top = 63
            Width = 27
            Height = 23
            TabOrder = 14
            Text = '0'
          end
          object UDStructogramShadowIntensity: TUpDown
            Left = 522
            Top = 63
            Width = 16
            Height = 23
            Associate = EStructogramShadowIntensity
            Max = 10
            TabOrder = 17
          end
        end
        object PSequencediagram: TTabSheet
          Caption = 'Sequence diagram'
          object LSDObject: TLabel
            Left = 16
            Top = 19
            Width = 35
            Height = 15
            Caption = 'Object'
          end
          object LSDNew: TLabel
            Left = 16
            Top = 59
            Width = 24
            Height = 15
            Caption = 'New'
          end
          object LSDClose: TLabel
            Left = 16
            Top = 99
            Width = 29
            Height = 15
            Caption = 'Close'
          end
          object LSDFilling: TLabel
            Left = 286
            Top = 19
            Width = 32
            Height = 15
            Caption = 'Filling'
          end
          object ESDObject: TEdit
            Left = 90
            Top = 16
            Width = 140
            Height = 23
            TabOrder = 0
            Text = 'Object'
          end
          object ESDNew: TEdit
            Left = 90
            Top = 56
            Width = 140
            Height = 23
            TabOrder = 1
            Text = 'New'
          end
          object ESDClose: TEdit
            Left = 90
            Top = 96
            Width = 140
            Height = 23
            TabOrder = 2
            Text = 'Close'
          end
          object CBSDFillingColor: TColorBox
            Left = 359
            Top = 16
            Width = 145
            Height = 22
            Selected = clYellow
            TabOrder = 3
          end
          object CBSDShowMainCall: TCheckBox
            Left = 16
            Top = 139
            Width = 145
            Height = 17
            Caption = 'Show main call'
            TabOrder = 4
          end
          object CBSDShowParameter: TCheckBox
            Left = 16
            Top = 179
            Width = 153
            Height = 17
            Caption = 'Show parameter'
            TabOrder = 5
          end
          object CBSDShowReturn: TCheckBox
            Left = 16
            Top = 219
            Width = 153
            Height = 17
            Caption = 'Show return'
            TabOrder = 6
          end
          object CBSDNoFilling: TCheckBox
            Left = 359
            Top = 44
            Width = 97
            Height = 17
            Caption = 'no'
            TabOrder = 7
          end
        end
        object PBrowser: TTabSheet
          Caption = 'Browser'
          object LAltKeysBrowser: TLabel
            Left = 16
            Top = 148
            Width = 166
            Height = 15
            Caption = '"Open address" keys in browser'
          end
          object LBrowser: TLabel
            Left = 16
            Top = 84
            Width = 87
            Height = 15
            Caption = 'External browser'
          end
          object LBrowserTitle: TLabel
            Left = 16
            Top = 116
            Width = 65
            Height = 15
            Caption = 'Browser title'
          end
          object CBOpenBrowserShortcut: TComboBox
            Left = 232
            Top = 144
            Width = 161
            Height = 23
            TabOrder = 0
          end
          object CBOnlyOneBrowserWindow: TCheckBox
            Left = 16
            Top = 40
            Width = 249
            Height = 17
            Caption = 'Use only one browser window'
            TabOrder = 1
            OnClick = CBUseIEinternForDocumentsClick
          end
          object BSelectBrowser: TButton
            Left = 470
            Top = 80
            Width = 75
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BSelectBrowserClick
          end
          object EBrowserProgram: TEdit
            Left = 120
            Top = 80
            Width = 330
            Height = 23
            TabOrder = 3
          end
          object CBUseIEinternForDocuments: TCheckBox
            Left = 16
            Top = 16
            Width = 353
            Height = 17
            Caption = 'Use Internet Explorer internally for documents'
            TabOrder = 4
            OnClick = CBUseIEinternForDocumentsClick
          end
          object BBrowserTitle: TButton
            Left = 470
            Top = 112
            Width = 75
            Height = 23
            Caption = 'Default'
            TabOrder = 5
            OnClick = BBrowserTitleClick
          end
          object EBrowserTitle: TEdit
            Left = 120
            Top = 112
            Width = 330
            Height = 23
            TabOrder = 6
          end
          object GBProxy: TGroupBox
            Left = 16
            Top = 184
            Width = 377
            Height = 121
            Caption = 'Proxy'
            TabOrder = 7
            object LProxyIP: TLabel
              Left = 8
              Top = 56
              Width = 53
              Height = 15
              Caption = 'IP address'
            end
            object LProxyPort: TLabel
              Left = 8
              Top = 88
              Width = 22
              Height = 15
              Caption = 'Port'
            end
            object CBUseProxy: TCheckBox
              Left = 8
              Top = 24
              Width = 105
              Height = 17
              Caption = 'Use proxy'
              TabOrder = 0
            end
            object EProxyIP: TEdit
              Left = 80
              Top = 52
              Width = 89
              Height = 23
              TabOrder = 1
            end
            object EProxyPort: TEdit
              Left = 80
              Top = 84
              Width = 33
              Height = 23
              TabOrder = 2
            end
          end
        end
        object PDocumentation: TTabSheet
          Caption = 'Documentation'
          object LCache: TLabel
            Left = 16
            Top = 148
            Width = 31
            Height = 13
            Caption = 'Cache'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavabookClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LSearchAgainFiles: TLabel
            Left = 210
            Top = 180
            Width = 23
            Height = 15
            Caption = 'Files'
          end
          object LSearchAgain: TLabel
            Left = 16
            Top = 180
            Width = 67
            Height = 15
            Caption = 'Search again'
          end
          object LJavabook: TLabel
            Left = 16
            Top = 116
            Width = 50
            Height = 13
            Caption = 'Java book'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJavabookClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LTutorial: TLabel
            Left = 16
            Top = 84
            Width = 35
            Height = 13
            Caption = 'Tutorial'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LTutorialClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LManual: TLabel
            Left = 16
            Top = 20
            Width = 40
            Height = 15
            Caption = 'Manual'
          end
          object SBManualSelect: TSpeedButton
            Left = 523
            Top = 16
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBManualSelectClick
          end
          object SBTutorialSelect: TSpeedButton
            Tag = 10
            Left = 523
            Top = 80
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBTutorialSelectClick
          end
          object SBJavabookSelect: TSpeedButton
            Tag = 11
            Left = 523
            Top = 112
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBJavabookSelectClick
          end
          object SBCacheSelect: TSpeedButton
            Tag = 12
            Left = 523
            Top = 144
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBCacheSelectClick
          end
          object LManualFX: TLabel
            Left = 16
            Top = 52
            Width = 81
            Height = 15
            Caption = 'Manual Java FX'
          end
          object SBManualFXSelect: TSpeedButton
            Left = 523
            Top = 48
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBManualFXSelectClick
          end
          object CBManual: TComboBox
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            Hint = 'Java documentation folder - default %java%\docs'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnSelect = CBManualSelect
            Items.Strings = (
              '\docs'
              '\chm'
              'https://java.sun.com/javase/6/docs/index.html'
              '')
          end
          object BCache: TButton
            Left = 550
            Top = 144
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 1
            OnClick = BCacheClick
          end
          object ECache: TEdit
            Left = 140
            Top = 144
            Width = 381
            Height = 23
            Hint = 
              'Cache folder for documentation files when accessed via https:// ' +
              'addresses'
            TabOrder = 2
          end
          object BJavabookInstall: TButton
            Left = 550
            Top = 112
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 3
            OnClick = BJavabookInstallClick
          end
          object EJavabook: TEdit
            Left = 140
            Top = 112
            Width = 381
            Height = 23
            TabOrder = 4
          end
          object BTutorialInstall: TButton
            Left = 550
            Top = 80
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 5
            OnClick = BTutorialInstallClick
          end
          object ETutorial: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            Hint = 'The tutorial file - %java%\tutorial\tutorial.chm'
            TabOrder = 6
          end
          object BManualInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 7
            OnClick = BManualInstallClick
          end
          object EMaxSearch: TEdit
            Left = 140
            Top = 176
            Width = 33
            Height = 23
            TabOrder = 8
            Text = '0'
          end
          object UDMaxSearch: TUpDown
            Left = 173
            Top = 176
            Width = 15
            Height = 23
            Associate = EMaxSearch
            TabOrder = 9
          end
          object CBManualFX: TComboBox
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            Hint = 'Java documentation folder - default %java%\docs'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 10
            OnSelect = CBManualFXSelect
          end
          object BManualFXInstall: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 11
            OnClick = BManualFXInstallClick
          end
        end
        object PPrinter: TTabSheet
          Caption = 'Printer'
          object LFooterMacro: TLabel
            Left = 296
            Top = 70
            Width = 34
            Height = 15
            Caption = 'Macro'
          end
          object LHeaderMacro: TLabel
            Left = 296
            Top = 16
            Width = 34
            Height = 15
            Caption = 'Macro'
          end
          object LFooter: TLabel
            Left = 16
            Top = 70
            Width = 34
            Height = 15
            Caption = 'Footer'
          end
          object LHeader: TLabel
            Left = 16
            Top = 16
            Width = 38
            Height = 15
            Caption = 'Header'
          end
          object LBorderBottom: TLabel
            Left = 24
            Top = 224
            Width = 40
            Height = 15
            Caption = 'Bottom'
          end
          object LBorderTop: TLabel
            Left = 24
            Top = 200
            Width = 19
            Height = 15
            Caption = 'Top'
          end
          object LBorderRight: TLabel
            Left = 24
            Top = 176
            Width = 28
            Height = 15
            Caption = 'Right'
          end
          object LBorderLeft: TLabel
            Left = 24
            Top = 152
            Width = 20
            Height = 15
            Caption = 'Left'
          end
          object LBorder: TLabel
            Left = 16
            Top = 128
            Width = 73
            Height = 15
            Caption = 'Border in mm'
          end
          object CBLinenumbersInBorder: TCheckBox
            Left = 176
            Top = 176
            Width = 200
            Height = 17
            Caption = 'Line numbers in border'
            TabOrder = 0
          end
          object CBLinenumbers: TCheckBox
            Left = 176
            Top = 152
            Width = 200
            Height = 17
            Caption = 'With line numbers'
            TabOrder = 1
          end
          object CBFooter: TComboBox
            Left = 304
            Top = 88
            Width = 97
            Height = 23
            TabOrder = 2
            OnChange = CBFooterChange
            Items.Strings = (
              '%FILE%'
              '%PATH%'
              '%DATE%'
              '%TIME%'
              '%PAGENUM%'
              '%PAGECOUNT%')
          end
          object CBHeader: TComboBox
            Left = 304
            Top = 34
            Width = 97
            Height = 23
            TabOrder = 3
            OnChange = CBHeaderChange
            Items.Strings = (
              '%FILE%'
              '%PATH%'
              '%DATE%'
              '%TIME%'
              '%PAGENUM%'
              '%PAGECOUNT%')
          end
          object RGAdjustment: TRadioGroup
            Left = 416
            Top = 14
            Width = 105
            Height = 99
            Caption = 'Alignment'
            ItemIndex = 0
            Items.Strings = (
              'Left'
              'Centered'
              'Right')
            TabOrder = 4
          end
          object EHeader: TEdit
            Left = 24
            Top = 34
            Width = 265
            Height = 23
            Hint = 'Format: left#centered#right'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
            Text = '#%PATH%#%DATE%'
          end
          object EFooter: TEdit
            Left = 24
            Top = 88
            Width = 265
            Height = 23
            Hint = 'Format: left#centered#right'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 6
            Text = '##- %PAGENUM% -'
          end
          object EBorderBottom: TEdit
            Left = 80
            Top = 222
            Width = 49
            Height = 23
            TabOrder = 7
            Text = '20'
          end
          object EBorderTop: TEdit
            Left = 80
            Top = 198
            Width = 49
            Height = 23
            TabOrder = 8
            Text = '20'
          end
          object EBorderRight: TEdit
            Left = 80
            Top = 174
            Width = 49
            Height = 23
            TabOrder = 9
            Text = '20'
          end
          object EBorderLeft: TEdit
            Left = 80
            Top = 150
            Width = 49
            Height = 23
            TabOrder = 10
            Text = '20'
          end
          object CBPrintColored: TCheckBox
            Left = 177
            Top = 200
            Width = 200
            Height = 17
            Caption = 'Print colored'
            TabOrder = 11
          end
        end
        object PMindstorms: TTabSheet
          Caption = 'Mindstorms'
          object LMindstormsParameter: TLabel
            Left = 16
            Top = 180
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LMindstormsTemplate: TLabel
            Left = 16
            Top = 212
            Width = 67
            Height = 15
            Caption = 'Template file'
          end
          object LLejosManual: TLabel
            Left = 16
            Top = 148
            Width = 40
            Height = 15
            Caption = 'Manual'
          end
          object LMindstormsPort: TLabel
            Left = 16
            Top = 244
            Width = 22
            Height = 15
            Caption = 'Port'
          end
          object LLejos: TLabel
            Left = 16
            Top = 20
            Width = 54
            Height = 13
            Caption = 'Lejos folder'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LLejosClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object SBLejosSelect: TSpeedButton
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBLejosSelectClick
          end
          object SBMindstormsManual: TSpeedButton
            Left = 521
            Top = 144
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = BMindstormsManualClick
          end
          object SBMindstormsTemplate: TSpeedButton
            Tag = 11
            Left = 521
            Top = 208
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = BVorlageClick
          end
          object SBLejosUploader: TSpeedButton
            Left = 521
            Top = 80
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBLejosUploaderClick
          end
          object LUploader: TLabel
            Left = 16
            Top = 84
            Width = 48
            Height = 15
            Caption = 'Uploader'
          end
          object LFlasher: TLabel
            Left = 16
            Top = 116
            Width = 37
            Height = 15
            Caption = 'Flasher'
          end
          object SBLejosFlasher: TSpeedButton
            Left = 523
            Top = 112
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBLejosFlasherClick
          end
          object LLejosCompiler: TLabel
            Left = 16
            Top = 52
            Width = 49
            Height = 15
            Caption = 'Compiler'
          end
          object SBLejosCompiler: TSpeedButton
            Left = 521
            Top = 48
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBLejosCompilerClick
          end
          object LMindstormsIP: TLabel
            Left = 16
            Top = 302
            Width = 53
            Height = 15
            Caption = 'IP address'
          end
          object RGMindstormsVersion: TRadioGroup
            Left = 292
            Top = 240
            Width = 179
            Height = 49
            Caption = 'Version'
            Columns = 3
            ItemIndex = 0
            Items.Strings = (
              'RCX'
              'NXT'
              'EV3')
            TabOrder = 0
            OnClick = RGMindstormsVersionClick
          end
          object BMindstormsParameter: TButton
            Left = 550
            Top = 176
            Width = 85
            Height = 23
            Caption = 'Default'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnClick = BMindstormsParameterClick
          end
          object EMindstormsParameter: TEdit
            Left = 140
            Top = 176
            Width = 381
            Height = 23
            TabOrder = 2
          end
          object BMindstormsTemplate: TButton
            Tag = 11
            Left = 550
            Top = 208
            Width = 85
            Height = 23
            Caption = 'Open'
            TabOrder = 3
            OnClick = SBOpenClick
          end
          object EMindstormsTemplate: TEdit
            Left = 140
            Top = 208
            Width = 381
            Height = 23
            TabOrder = 4
          end
          object BMindstormsManual: TButton
            Tag = 4
            Left = 550
            Top = 144
            Width = 85
            Height = 23
            Caption = 'Open'
            TabOrder = 5
            OnClick = SBOpenClick
          end
          object EMindstormsManual: TEdit
            Left = 140
            Top = 144
            Width = 381
            Height = 23
            TabOrder = 6
          end
          object CBMindstormsModus: TCheckBox
            Left = 140
            Top = 272
            Width = 146
            Height = 17
            Caption = 'Mindstorms mode'
            Checked = True
            State = cbChecked
            TabOrder = 7
          end
          object CBMindstormsPort: TComboBox
            Left = 140
            Top = 240
            Width = 97
            Height = 23
            TabOrder = 8
            Text = 'USB'
            Items.Strings = (
              'USB'
              'COM1'
              'COM2'
              'COM3'
              'COM4')
          end
          object BLejosInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 9
            OnClick = BLejosInstallClick
          end
          object ELejosFolder: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            TabOrder = 10
          end
          object ELejosUploader: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            TabOrder = 11
          end
          object ELejosFlasher: TEdit
            Left = 140
            Top = 112
            Width = 381
            Height = 23
            TabOrder = 12
          end
          object ELejosCompiler: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 13
          end
          object EMindstormsIP: TEdit
            Left = 140
            Top = 301
            Width = 97
            Height = 23
            TabOrder = 14
          end
        end
        object PAndroid: TTabSheet
          Caption = 'Android'
          object LAndroidSDK: TLabel
            Left = 16
            Top = 20
            Width = 101
            Height = 15
            Caption = 'Android SDK folder'
          end
          object SBAndroidSDKFolder: TSpeedButton
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBAndroidSDKFolderClick
          end
          object EAndroidSDKFolder: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            TabOrder = 0
          end
          object BAndroidSDKInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 1
            OnClick = BAndroidSDKInstallClick
          end
          object CBAndroidMode: TCheckBox
            Left = 16
            Top = 48
            Width = 134
            Height = 17
            Caption = 'Android mode'
            TabOrder = 2
          end
          object BAndroidAssemble: TButton
            Left = 549
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Assemble.BAT'
            TabOrder = 3
            OnClick = BAndroidAssembleClick
          end
          object Memo1: TMemo
            Left = 140
            Top = 76
            Width = 381
            Height = 146
            Lines.Strings = (
              'realized by Felicia Ruppel'
              ''
              'on behalf of'
              'Roland Kluge and'
              'Prof. Dr. Andy Sch'#252'rr'
              'Fachgebiet Echtzeitsysteme / Real-Time Systems'
              'Fachbereich Elektrotechnik und Informationstechnik'
              'Technische Universit'#228't Darmstadt'
              'www.es.tu-darmstadt.de'
              '')
            TabOrder = 4
          end
        end
        object PLanguage: TTabSheet
          Caption = 'Language'
          object RGLanguages: TRadioGroup
            Left = 16
            Top = 16
            Width = 297
            Height = 249
            Caption = 'Languages'
            TabOrder = 0
          end
        end
        object PGeneralOptions: TTabSheet
          Caption = 'General options'
          object LFontsize: TLabel
            Left = 59
            Top = 162
            Width = 144
            Height = 15
            Caption = 'Font size in component bar'
          end
          object LTempFolder: TLabel
            Left = 16
            Top = 190
            Width = 133
            Height = 15
            Caption = 'Folder for temporary files'
          end
          object SBTempSelect: TSpeedButton
            Tag = 7
            Left = 445
            Top = 214
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBTempSelectClick
          end
          object LFileFilter: TLabel
            Left = 320
            Top = 130
            Width = 106
            Height = 15
            Caption = 'Additional file filters'
          end
          object LFileFilterExample: TLabel
            Left = 461
            Top = 151
            Width = 56
            Height = 15
            Caption = '*.php;*.css'
          end
          object LFileHistory: TLabel
            Left = 59
            Top = 132
            Width = 192
            Height = 15
            Caption = 'Maximum number of files to reopen'
          end
          object CBDebuggerProtocol: TCheckBox
            Left = 320
            Top = 100
            Width = 240
            Height = 17
            Caption = 'Log debugger'
            TabOrder = 0
          end
          object CBRenameWhenSave: TCheckBox
            Left = 16
            Top = 80
            Width = 258
            Height = 17
            Caption = 'Save As renames class'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
          object CBUseInterpreterWindowAsConsole: TCheckBox
            Left = 16
            Top = 17
            Width = 545
            Height = 17
            Caption = 'Use the interpreter window as a console for console programs'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object CBBAKFiles: TCheckBox
            Left = 16
            Top = 100
            Width = 258
            Height = 17
            Caption = 'Create BAK files when saving'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object ETempFolder: TEdit
            Left = 33
            Top = 214
            Width = 412
            Height = 23
            TabOrder = 5
          end
          object BTempFolder: TButton
            Left = 472
            Top = 213
            Width = 85
            Height = 23
            Caption = 'Default'
            TabOrder = 6
            OnClick = BTempFolderClick
          end
          object UDFontSize: TUpDown
            Left = 37
            Top = 159
            Width = 15
            Height = 23
            Associate = EFontSize
            Min = 7
            Max = 40
            Position = 9
            TabOrder = 7
          end
          object EFontSize: TEdit
            Left = 16
            Top = 159
            Width = 21
            Height = 23
            TabOrder = 8
            Text = '9'
          end
          object EFileFilter: TEdit
            Left = 320
            Top = 148
            Width = 121
            Height = 23
            Hint = '*.php;*.css'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 9
          end
          object CBCheckAge: TCheckBox
            Left = 16
            Top = 60
            Width = 258
            Height = 17
            Caption = 'Monitor external modification of files'
            TabOrder = 10
          end
          object EFileHistory: TEdit
            Left = 16
            Top = 127
            Width = 21
            Height = 23
            TabOrder = 11
            Text = '9'
          end
          object UDFileHistory: TUpDown
            Left = 37
            Top = 127
            Width = 16
            Height = 23
            Associate = EFileHistory
            Max = 20
            Position = 9
            TabOrder = 12
          end
          object CBStrictJavaMode: TCheckBox
            Left = 320
            Top = 60
            Width = 290
            Height = 17
            Caption = 'Strict Java in interactive windows'
            TabOrder = 13
          end
          object CBAcceptdefaultname: TCheckBox
            Left = 320
            Top = 40
            Width = 298
            Height = 17
            Caption = 'Accept the default name when saving'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 14
          end
          object CBLoadFiles: TCheckBox
            Left = 16
            Top = 40
            Width = 174
            Height = 17
            Hint = 'Reload the last opened windows on startup'
            Caption = 'Load files on startup'
            Checked = True
            ParentShowHint = False
            ShowHint = True
            State = cbChecked
            TabOrder = 3
          end
          object CBTranslateCompilerErrors: TCheckBox
            Left = 320
            Top = 80
            Width = 209
            Height = 17
            Caption = 'Translate compiler errors'
            TabOrder = 15
          end
        end
        object PStyles: TTabSheet
          Caption = 'Styles'
          ImageIndex = 35
          object LNewStyle: TLabel
            Left = 3
            Top = 59
            Width = 51
            Height = 15
            Caption = 'New style'
          end
          object LPreview: TLabel
            Left = 255
            Top = 8
            Width = 41
            Height = 15
            Caption = 'Preview'
          end
          object LCurrentStyle: TLabel
            Left = 3
            Top = 8
            Width = 67
            Height = 15
            Caption = 'Current style'
          end
          object LBStyleNames: TListBox
            Left = 8
            Top = 80
            Width = 241
            Height = 308
            ItemHeight = 15
            TabOrder = 0
            OnClick = LBStyleNamesClick
          end
          object StylesPreviewPanel: TPanel
            Left = 255
            Top = 27
            Width = 382
            Height = 360
            TabOrder = 1
          end
          object ECurrentStyle: TEdit
            Left = 8
            Top = 29
            Width = 241
            Height = 23
            TabOrder = 2
          end
        end
        object PRestrictions: TTabSheet
          Caption = 'Restrictions'
          object LRestrictions: TLabel
            Left = 16
            Top = 106
            Width = 273
            Height = 13
            Caption = 'Restrictions can only be changed with administrator rights.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object CBBlockedInternet: TCheckBox
            Left = 16
            Top = 36
            Width = 281
            Height = 17
            Caption = 'Block internet access'
            TabOrder = 1
          end
          object CBLockedPaths: TCheckBox
            Left = 16
            Top = 56
            Width = 281
            Height = 17
            Caption = 'Lock paths'
            TabOrder = 2
          end
          object CBDosWindow: TCheckBox
            Left = 16
            Top = 16
            Width = 281
            Height = 17
            Caption = 'Disable DOS window'
            TabOrder = 0
          end
          object CBLockedStructogram: TCheckBox
            Left = 16
            Top = 76
            Width = 305
            Height = 17
            Caption = 'Lock conversion structogram <-> program'
            TabOrder = 3
          end
        end
        object PAssociations: TTabSheet
          Caption = 'Associations'
          object LAssociations: TLabel
            Left = 16
            Top = 20
            Width = 266
            Height = 15
            Caption = 'Associate these file extensions with the Java-Editor'
          end
          object LAdditionalAssociations: TLabel
            Left = 16
            Top = 106
            Width = 122
            Height = 15
            Caption = 'Additional associations'
          end
          object LAssociationsExample: TLabel
            Left = 160
            Top = 130
            Width = 62
            Height = 15
            Caption = '*.pas;*.class'
          end
          object LJEAssociation: TLabel
            Left = 16
            Top = 228
            Width = 156
            Height = 15
            Caption = 'Association of the Java-Editor'
          end
          object CBAssociationJava: TCheckBox
            Left = 30
            Top = 40
            Width = 60
            Height = 19
            Caption = '.java'
            TabOrder = 0
          end
          object CBAssociationJfm: TCheckBox
            Left = 90
            Top = 40
            Width = 60
            Height = 17
            Caption = '.jfm'
            TabOrder = 1
          end
          object CBAssociationHtml: TCheckBox
            Left = 330
            Top = 66
            Width = 60
            Height = 17
            Caption = '.html'
            TabOrder = 2
          end
          object CBAssociationJep: TCheckBox
            Left = 210
            Top = 40
            Width = 60
            Height = 17
            Caption = '.jep'
            TabOrder = 3
          end
          object CBAssociationUml: TCheckBox
            Left = 150
            Top = 40
            Width = 60
            Height = 17
            Caption = '.uml'
            TabOrder = 4
          end
          object CBAssociationTxt: TCheckBox
            Left = 30
            Top = 66
            Width = 60
            Height = 17
            Caption = '.txt'
            TabOrder = 5
          end
          object CBAssociationJsp: TCheckBox
            Left = 90
            Top = 66
            Width = 60
            Height = 17
            Caption = '.jsp'
            TabOrder = 6
          end
          object CBAssociationPhp: TCheckBox
            Left = 150
            Top = 66
            Width = 60
            Height = 17
            Caption = '.php'
            TabOrder = 7
          end
          object CBAssociationCss: TCheckBox
            Left = 210
            Top = 66
            Width = 60
            Height = 17
            Caption = '.css'
            TabOrder = 8
          end
          object EAdditionalAssociations: TEdit
            Left = 30
            Top = 126
            Width = 119
            Height = 23
            Hint = '*.pas;*.jar'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 9
          end
          object CBAssociationInc: TCheckBox
            Left = 270
            Top = 66
            Width = 60
            Height = 17
            Caption = '.inc'
            TabOrder = 10
          end
          object BFileExtensions: TButton
            Left = 16
            Top = 160
            Width = 85
            Height = 25
            Caption = 'Change'
            TabOrder = 11
            OnClick = BFileExtensionsClick
          end
          object EJEAssociation: TEdit
            Left = 30
            Top = 255
            Width = 603
            Height = 23
            TabOrder = 12
          end
          object BJEAssociation: TButton
            Left = 16
            Top = 294
            Width = 85
            Height = 25
            Caption = 'Change'
            TabOrder = 13
            OnClick = BJEAssociationClick
          end
          object CBAssociationJsg: TCheckBox
            Left = 270
            Top = 40
            Width = 60
            Height = 17
            Caption = '.jsg'
            TabOrder = 14
          end
          object CBAssociationJSD: TCheckBox
            Left = 330
            Top = 40
            Width = 55
            Height = 17
            Caption = '.jsd'
            TabOrder = 15
          end
        end
        object PUML: TTabSheet
          Caption = 'UML'
          object LObjectDesign: TLabel
            Left = 16
            Top = 95
            Width = 91
            Height = 15
            Caption = 'Design of objects'
          end
          object LClassDesign: TLabel
            Left = 16
            Top = 15
            Width = 89
            Height = 15
            Caption = 'Design of classes'
          end
          object LValidClassColor: TLabel
            Left = 24
            Top = 38
            Width = 53
            Height = 15
            Caption = 'Valid class'
          end
          object LShadowWidth: TLabel
            Left = 530
            Top = 45
            Width = 32
            Height = 15
            Caption = 'Width'
          end
          object LShadowIntensity: TLabel
            Left = 530
            Top = 72
            Width = 45
            Height = 15
            Caption = 'Intensity'
          end
          object LInvalidClassColor: TLabel
            Left = 170
            Top = 38
            Width = 63
            Height = 15
            Caption = 'Invalid class'
          end
          object LShadow: TLabel
            Left = 477
            Top = 20
            Width = 42
            Height = 15
            Caption = 'Shadow'
          end
          object LObjectColor: TLabel
            Left = 24
            Top = 120
            Width = 29
            Height = 15
            Caption = 'Color'
          end
          object LCommentDesign: TLabel
            Left = 16
            Top = 172
            Width = 110
            Height = 15
            Caption = 'Design of comments'
          end
          object LAttributesAndMethods: TLabel
            Left = 16
            Top = 239
            Width = 125
            Height = 15
            Caption = 'Attributes and methods'
          end
          object RGObjectHead: TRadioGroup
            Left = 170
            Top = 124
            Width = 125
            Height = 65
            Caption = 'Header'
            ItemIndex = 0
            Items.Strings = (
              'rectangular'
              'rounded')
            TabOrder = 0
          end
          object CBObjectColor: TColorBox
            Left = 24
            Top = 140
            Width = 125
            Height = 22
            Selected = clYellow
            TabOrder = 1
          end
          object RGObjectCaption: TRadioGroup
            Left = 475
            Top = 124
            Width = 144
            Height = 103
            Caption = 'Caption'
            ItemIndex = 0
            Items.Strings = (
              'Object: class'
              'Object'
              ': class'
              '')
            TabOrder = 2
          end
          object CBObjectUnderline: TCheckBox
            Left = 483
            Top = 205
            Width = 110
            Height = 17
            Caption = 'Underline'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
          object RGClassHead: TRadioGroup
            Left = 317
            Top = 26
            Width = 125
            Height = 65
            Caption = 'Header'
            ItemIndex = 0
            Items.Strings = (
              'rectangular'
              'rounded')
            TabOrder = 4
          end
          object CBValidClassColor: TColorBox
            Left = 24
            Top = 57
            Width = 125
            Height = 22
            Selected = clWhite
            TabOrder = 5
          end
          object EShadowWidth: TEdit
            Left = 479
            Top = 42
            Width = 27
            Height = 23
            TabOrder = 6
            Text = '3'
          end
          object UDShadowWidth: TUpDown
            Left = 506
            Top = 42
            Width = 16
            Height = 23
            Associate = EShadowWidth
            Position = 3
            TabOrder = 7
          end
          object UDShadowIntensity: TUpDown
            Left = 506
            Top = 69
            Width = 16
            Height = 23
            Associate = EShadowIntensity
            Max = 10
            TabOrder = 8
          end
          object EShadowIntensity: TEdit
            Left = 479
            Top = 69
            Width = 27
            Height = 23
            TabOrder = 9
            Text = '0'
          end
          object CBInvalidClassColor: TColorBox
            Left = 170
            Top = 57
            Width = 125
            Height = 22
            Selected = clWhite
            TabOrder = 10
          end
          object RGObjectFooter: TRadioGroup
            Left = 317
            Top = 124
            Width = 136
            Height = 65
            Caption = 'Footer'
            ItemIndex = 0
            Items.Strings = (
              'rectangular'
              'rounded')
            TabOrder = 11
          end
          object CBCommentColor: TColorBox
            Left = 24
            Top = 197
            Width = 125
            Height = 22
            Selected = clSkyBlue
            TabOrder = 12
          end
          object RGAttributsMethodsDisplay: TRadioGroup
            Left = 24
            Top = 258
            Width = 125
            Height = 123
            Caption = 'View'
            Items.Strings = (
              'none'
              'public'
              'protected'
              'package'
              'all')
            TabOrder = 13
          end
          object RGSequenceAttributsMethods: TRadioGroup
            Left = 170
            Top = 258
            Width = 125
            Height = 83
            Caption = 'Order'
            Items.Strings = (
              'Source code'
              'Visibility'
              'Alphabetical')
            TabOrder = 14
          end
          object RGParameterDisplay: TRadioGroup
            Left = 317
            Top = 258
            Width = 136
            Height = 123
            Caption = 'Parameter'
            Items.Strings = (
              'none'
              ': Type'
              '(...): Type'
              '(Type, Type): Type'
              '(Name: Type): Type')
            TabOrder = 15
          end
          object RGVisibilityDisplay: TRadioGroup
            Left = 475
            Top = 258
            Width = 144
            Height = 83
            Caption = 'Visibility'
            Items.Strings = (
              'none'
              'Text'
              'Icons')
            TabOrder = 16
          end
        end
        object PUMLOptions: TTabSheet
          Caption = 'UML options'
          object GBClassPresentation: TGroupBox
            Left = 16
            Top = 20
            Width = 300
            Height = 213
            Caption = 'Class representation'
            TabOrder = 0
            object CBShowEmptyRects: TCheckBox
              Left = 12
              Top = 24
              Width = 300
              Height = 17
              Caption = 'Show empty attribute and method rectangles'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object CBUseVoid: TCheckBox
              Left = 12
              Top = 44
              Width = 300
              Height = 17
              Caption = 'Use return value "void".'
              TabOrder = 1
            end
            object CBIntegerInsteadofInt: TCheckBox
              Left = 12
              Top = 64
              Width = 269
              Height = 17
              Caption = 'Integer instead of int'
              TabOrder = 2
            end
            object CBStartWithDatatype: TCheckBox
              Left = 12
              Top = 84
              Width = 255
              Height = 17
              Caption = 'Start with the data type'
              TabOrder = 3
            end
            object CBShowClassparameterSeparately: TCheckBox
              Left = 12
              Top = 144
              Width = 283
              Height = 17
              Caption = 'Show class parameters separately'
              TabOrder = 4
            end
            object CBRoleHidesAttribute: TCheckBox
              Left = 12
              Top = 164
              Width = 221
              Height = 17
              Caption = 'Role hides attribute'
              TabOrder = 5
            end
            object CBConstructorWithVisibility: TCheckBox
              Left = 12
              Top = 104
              Width = 281
              Height = 17
              Caption = 'Show constructor with visibility'
              TabOrder = 6
            end
            object CBRelationshipAttributesBold: TCheckBox
              Left = 12
              Top = 124
              Width = 278
              Height = 17
              Caption = 'Relationship attributes in bold'
              TabOrder = 7
            end
            object CBUseAbstract: TCheckBox
              Left = 12
              Top = 184
              Width = 261
              Height = 17
              Caption = 'Use {abstract}'
              TabOrder = 8
            end
          end
          object GBObjectPresentation: TGroupBox
            Left = 334
            Top = 20
            Width = 300
            Height = 213
            Caption = 'Object representation'
            TabOrder = 1
            object CBShowObjectsWithMethods: TCheckBox
              Left = 12
              Top = 44
              Width = 287
              Height = 17
              Caption = 'Show with methods'
              TabOrder = 0
            end
            object CBShowObjectsWithInheritedPrivateAttributes: TCheckBox
              Left = 12
              Top = 24
              Width = 289
              Height = 17
              Caption = 'Show with inherited private attributes'
              TabOrder = 1
            end
            object CBShowAllNewObjects: TCheckBox
              Left = 12
              Top = 84
              Width = 279
              Height = 17
              Caption = 'Show all new objects'
              TabOrder = 2
            end
            object CBLowerCaseLetter: TCheckBox
              Left = 12
              Top = 64
              Width = 279
              Height = 17
              Caption = 'Start names with a lower case letter'
              TabOrder = 3
            end
            object CBObjectsWithoutVisibility: TCheckBox
              Left = 12
              Top = 104
              Width = 229
              Height = 17
              Caption = 'Without visibility'
              TabOrder = 4
            end
            object CBArrayListAsIntegratedList: TCheckBox
              Left = 12
              Top = 124
              Width = 229
              Height = 17
              Caption = 'ArrayList as integrated list'
              TabOrder = 5
            end
          end
          object GBClassEditing: TGroupBox
            Left = 16
            Top = 250
            Width = 300
            Height = 112
            Caption = 'Class editing'
            TabOrder = 2
            object CBOpenPublicClasses: TCheckBox
              Left = 12
              Top = 44
              Width = 269
              Height = 17
              Caption = 'Only open public classes'
              TabOrder = 0
            end
            object CBDefaultModifiers: TCheckBox
              Left = 12
              Top = 24
              Width = 306
              Height = 17
              Caption = 'Default modifiers for attributes and methods'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
            object CBSetterWithoutThis: TCheckBox
              Left = 12
              Top = 64
              Width = 183
              Height = 17
              Caption = 'Setter without this'
              TabOrder = 2
            end
            object CBAttributesAParametersP: TCheckBox
              Left = 12
              Top = 84
              Width = 278
              Height = 17
              Caption = 'Attributes start with a, parameters with p'
              TabOrder = 3
            end
          end
          object GBObjectEditing: TGroupBox
            Left = 334
            Top = 250
            Width = 300
            Height = 112
            Caption = 'Object editing'
            TabOrder = 3
            object CBUMLEdit: TCheckBox
              Left = 12
              Top = 24
              Width = 279
              Height = 17
              Caption = 'Private attributes editable'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
            object CBShowFunctionvalues: TCheckBox
              Left = 12
              Top = 44
              Width = 300
              Height = 17
              Caption = 'Output function values in the interpreter window'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
        end
        object PLLMAssistant: TTabSheet
          Caption = 'LLM Assistant'
          ImageIndex = 37
          object LProvider: TLabel
            Left = 16
            Top = 16
            Width = 44
            Height = 15
            Caption = 'Provider'
          end
          object LEndpoint: TLabel
            Left = 32
            Top = 48
            Width = 48
            Height = 15
            Caption = 'Endpoint'
          end
          object LModel: TLabel
            Left = 32
            Top = 80
            Width = 34
            Height = 15
            Caption = 'Model'
          end
          object LAPIKey: TLabel
            Left = 32
            Top = 112
            Width = 39
            Height = 15
            Caption = 'API key'
          end
          object LSystemPrompt: TLabel
            Left = 32
            Top = 144
            Width = 81
            Height = 15
            Caption = 'System prompt'
          end
          object LMaxTokens: TLabel
            Left = 32
            Top = 176
            Width = 111
            Height = 15
            Caption = 'Max response tokens'
          end
          object LLLMTimeout: TLabel
            Left = 32
            Top = 208
            Width = 103
            Height = 15
            Caption = 'Timeout in seconds'
          end
          object LLLMTemperature: TLabel
            Left = 32
            Top = 241
            Width = 66
            Height = 15
            Caption = 'Temperature'
          end
          object CBProvider: TComboBox
            Left = 160
            Top = 13
            Width = 97
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            OnDropDown = CBProviderDropDown
            OnSelect = CBProviderSelect
            Items.Strings = (
              'OpenAI'
              'Gemini'
              'Ollama'
              'DeepSeek'
              'Grok')
          end
          object EEndPoint: TEdit
            Left = 160
            Top = 45
            Width = 400
            Height = 23
            TabOrder = 1
          end
          object EModel: TEdit
            Left = 160
            Top = 77
            Width = 400
            Height = 23
            TabOrder = 2
          end
          object EAPIKey: TEdit
            Left = 160
            Top = 109
            Width = 400
            Height = 23
            PasswordChar = #9679
            TabOrder = 3
          end
          object ESystemPrompt: TEdit
            Left = 160
            Top = 141
            Width = 400
            Height = 23
            TabOrder = 4
            Text = 'You are my expert java coding assistant'
          end
          object EMaxTokens: TEdit
            Left = 160
            Top = 173
            Width = 60
            Height = 23
            TabOrder = 5
            Text = '1000'
          end
          object ELLMTimeout: TEdit
            Left = 160
            Top = 206
            Width = 60
            Height = 23
            TabOrder = 6
            Text = '20'
          end
          object ELLMTemperature: TEdit
            Left = 160
            Top = 239
            Width = 60
            Height = 23
            TabOrder = 7
            Text = '1.0'
          end
          object BLLMAssistantDefault: TButton
            Left = 160
            Top = 280
            Width = 75
            Height = 25
            Caption = 'Default'
            TabOrder = 8
            OnClick = BLLMAssistantDefaultClick
          end
        end
        object PLLMChat: TTabSheet
          Caption = 'LLM Chat'
          ImageIndex = 38
          object LChatProvider: TLabel
            Left = 16
            Top = 16
            Width = 44
            Height = 15
            Caption = 'Provider'
          end
          object LChatEndPoint: TLabel
            Left = 32
            Top = 48
            Width = 48
            Height = 15
            Caption = 'Endpoint'
          end
          object LChatModel: TLabel
            Left = 32
            Top = 80
            Width = 34
            Height = 15
            Caption = 'Model'
          end
          object LChatApiKey: TLabel
            Left = 32
            Top = 112
            Width = 39
            Height = 15
            Caption = 'API key'
          end
          object LChatSystemPrompt: TLabel
            Left = 32
            Top = 144
            Width = 81
            Height = 15
            Caption = 'System prompt'
          end
          object LChatMaxTokens: TLabel
            Left = 32
            Top = 176
            Width = 111
            Height = 15
            Caption = 'Max response tokens'
          end
          object LChatTimeout: TLabel
            Left = 32
            Top = 209
            Width = 103
            Height = 15
            Caption = 'Timeout in seconds'
          end
          object LChatTemperature: TLabel
            Left = 32
            Top = 242
            Width = 66
            Height = 15
            Caption = 'Temperature'
          end
          object CBChatProvider: TComboBox
            Left = 160
            Top = 13
            Width = 97
            Height = 23
            Style = csDropDownList
            TabOrder = 0
            OnDropDown = CBChatProviderDropDown
            OnSelect = CBChatProviderSelect
            Items.Strings = (
              'OpenAI'
              'Gemini'
              'Ollama'
              'DeepSeek'
              'Grok')
          end
          object EChatEndPoint: TEdit
            Left = 160
            Top = 45
            Width = 400
            Height = 23
            TabOrder = 1
          end
          object EChatModel: TEdit
            Left = 160
            Top = 77
            Width = 400
            Height = 23
            TabOrder = 2
          end
          object EChatApiKey: TEdit
            Left = 160
            Top = 109
            Width = 400
            Height = 23
            PasswordChar = #9679
            TabOrder = 3
          end
          object EChatSystemPrompt: TEdit
            Left = 160
            Top = 141
            Width = 400
            Height = 23
            TabOrder = 4
            Text = 'You are my expert java coding assistant'
          end
          object EChatMaxTokens: TEdit
            Left = 160
            Top = 173
            Width = 60
            Height = 23
            TabOrder = 5
            Text = '1000'
          end
          object EChatTimeout: TEdit
            Left = 160
            Top = 206
            Width = 60
            Height = 23
            TabOrder = 6
            Text = '20'
          end
          object EChatTemperature: TEdit
            Left = 160
            Top = 239
            Width = 60
            Height = 23
            TabOrder = 7
            Text = '1.0'
          end
          object BLLMChatDefault: TButton
            Left = 160
            Top = 280
            Width = 75
            Height = 25
            Caption = 'Default'
            TabOrder = 8
            OnClick = BLLMChatDefaultClick
          end
        end
        object PVisibility: TTabSheet
          Caption = 'Visibility'
          object LVisTabs: TLabel
            Left = 15
            Top = 20
            Width = 23
            Height = 15
            Caption = 'Tabs'
          end
          object LVisItems: TLabel
            Left = 154
            Top = 20
            Width = 48
            Height = 15
            Caption = 'Elements'
          end
          object LVisToolbars: TLabel
            Left = 495
            Top = 20
            Width = 44
            Height = 15
            Caption = 'Toolbars'
          end
          object LVisMenus: TLabel
            Left = 365
            Top = 20
            Width = 36
            Height = 15
            Caption = 'Menus'
          end
          object LVVisibilityTabs: TListView
            Left = 15
            Top = 44
            Width = 122
            Height = 175
            Checkboxes = True
            Columns = <
              item
                AutoSize = True
              end>
            HideSelection = False
            ReadOnly = True
            ShowColumnHeaders = False
            TabOrder = 0
            ViewStyle = vsReport
            OnClick = LVVisibilityTabsClick
          end
          object LVVisibilityElements: TListView
            Left = 154
            Top = 44
            Width = 195
            Height = 373
            Checkboxes = True
            Columns = <
              item
                AutoSize = True
              end>
            HideSelection = False
            ReadOnly = True
            ShowColumnHeaders = False
            TabOrder = 1
            ViewStyle = vsReport
            OnItemChecked = LVVisibilityElementsItemChecked
          end
          object LVVisibilityToolbars: TListView
            Left = 495
            Top = 44
            Width = 112
            Height = 118
            Checkboxes = True
            Columns = <
              item
                AutoSize = True
              end>
            HideSelection = False
            ReadOnly = True
            ShowColumnHeaders = False
            TabOrder = 2
            ViewStyle = vsReport
          end
          object LVVisibilityMenus: TListView
            Left = 365
            Top = 44
            Width = 115
            Height = 80
            Checkboxes = True
            Columns = <
              item
                AutoSize = True
              end>
            HideSelection = False
            ReadOnly = True
            ShowColumnHeaders = False
            TabOrder = 3
            ViewStyle = vsReport
          end
          object BVisDefault: TButton
            Left = 508
            Top = 228
            Width = 85
            Height = 25
            Caption = 'Default'
            TabOrder = 4
            OnClick = BVisDefaultClick
          end
        end
        object PLogfiles: TTabSheet
          Caption = 'Log files'
          object LLogfilenames: TLabel
            Left = 16
            Top = 20
            Width = 44
            Height = 15
            Caption = 'Log files'
          end
          object LLogfileCompiler: TLabel
            Left = 24
            Top = 49
            Width = 49
            Height = 15
            Caption = 'Compiler'
          end
          object LLogfileExceptions: TLabel
            Left = 24
            Top = 124
            Width = 57
            Height = 15
            Caption = 'Exceptions'
          end
          object LLogfileInteractive: TLabel
            Left = 24
            Top = 88
            Width = 55
            Height = 15
            Caption = 'Interactive'
          end
          object ELogfileCompiler: TEdit
            Left = 160
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 0
          end
          object ELogfileExceptions: TEdit
            Left = 160
            Top = 120
            Width = 381
            Height = 23
            TabOrder = 1
          end
          object BLogfileCompiler: TButton
            Left = 550
            Top = 46
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BLogfileCompilerClick
          end
          object BLogfileExceptions: TButton
            Left = 550
            Top = 118
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 3
            OnClick = BLogfileExceptionsClick
          end
          object BLogfileInteractive: TButton
            Left = 548
            Top = 82
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 4
            OnClick = BLogfileInteractiveClick
          end
          object ELogfileInteractive: TEdit
            Left = 160
            Top = 84
            Width = 381
            Height = 23
            TabOrder = 5
          end
        end
        object PTools: TTabSheet
          Caption = 'Tools'
        end
        object PGit: TTabSheet
          Caption = 'Git'
          object LGitFolder: TLabel
            Left = 16
            Top = 20
            Width = 42
            Height = 13
            Caption = 'Git folder'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LGitFolderClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object LLocalRepository: TLabel
            Left = 16
            Top = 52
            Width = 84
            Height = 15
            Caption = 'Repository local'
          end
          object LUserName: TLabel
            Left = 16
            Top = 116
            Width = 55
            Height = 15
            Caption = 'user.name'
          end
          object LRemoteRepository: TLabel
            Left = 16
            Top = 84
            Width = 97
            Height = 15
            Caption = 'Repository remote'
          end
          object LUserEMail: TLabel
            Left = 16
            Top = 148
            Width = 54
            Height = 15
            Caption = 'user.email'
          end
          object EGitFolder: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 0
          end
          object CBLocalRepository: TComboBox
            Left = 140
            Top = 48
            Width = 394
            Height = 23
            TabOrder = 1
          end
          object BGitFolder: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BGitFolderClick
          end
          object BGitRepository: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 3
            OnClick = BGitRepositoryClick
          end
          object CBRemoteRepository: TComboBox
            Left = 140
            Top = 80
            Width = 394
            Height = 23
            TabOrder = 4
          end
          object EUserName: TEdit
            Left = 140
            Top = 112
            Width = 394
            Height = 23
            TabOrder = 5
          end
          object EUserEMail: TEdit
            Left = 140
            Top = 144
            Width = 394
            Height = 23
            TabOrder = 6
          end
          object BGitClone: TButton
            Left = 550
            Top = 80
            Width = 85
            Height = 23
            Caption = 'Clone'
            TabOrder = 7
            OnClick = BGitCloneClick
          end
        end
        object PJUnit: TTabSheet
          Caption = 'JUnit'
          object LJUnit: TLabel
            Left = 16
            Top = 20
            Width = 24
            Height = 13
            Caption = 'JUnit'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJUnitClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object SBJUnit: TSpeedButton
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBJUnitClick
          end
          object LJUnitParameter: TLabel
            Left = 32
            Top = 84
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LJUnitManual: TLabel
            Left = 32
            Top = 52
            Width = 40
            Height = 15
            Caption = 'Manual'
          end
          object SBJUnitManual: TSpeedButton
            Left = 521
            Top = 48
            Width = 21
            Height = 21
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBJUnitManualClick
          end
          object EJUnitJarFile: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            Hint = 'JUnit jar file'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object BJUnitInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 1
            OnClick = BJUnitInstallClick
          end
          object EJunitParameter: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            ParentShowHint = False
            ShowHint = True
            TabOrder = 2
          end
          object CBJUnitBeforeEach: TCheckBox
            Left = 32
            Top = 129
            Width = 97
            Height = 17
            Caption = 'BeforeEach'
            TabOrder = 3
          end
          object CBJUnitAfterEach: TCheckBox
            Left = 32
            Top = 164
            Width = 97
            Height = 17
            Caption = 'AfterEach'
            TabOrder = 4
          end
          object EJUnitManual: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            ParentShowHint = False
            ShowHint = True
            TabOrder = 5
          end
        end
        object PCheckStyle: TTabSheet
          Caption = 'Checkstyle'
          object LCheckstyleConfiguration: TLabel
            Left = 32
            Top = 52
            Width = 74
            Height = 15
            Caption = 'Configuration'
          end
          object LCheckstyleParameter: TLabel
            Left = 32
            Top = 84
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LCheckstyle: TLabel
            Left = 16
            Top = 20
            Width = 52
            Height = 13
            Caption = 'Checkstyle'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LCheckstyleClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object SBCheckStyleSelect: TSpeedButton
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBCheckStyleSelectClick
          end
          object BCheckstyleConfiguration: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 0
            OnClick = BCheckstyleConfigurationClick
          end
          object ECheckstyleConfiguration: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 1
          end
          object ECheckstyleParameter: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            TabOrder = 2
          end
          object BCheckstyleInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 3
            OnClick = BCheckstyleInstallClick
          end
          object ECheckstyle: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            Hint = 'Checkstyle jar file'
            HelpType = htKeyword
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
          end
        end
        object PJalopy: TTabSheet
          Caption = 'Jalopy'
          object LJalopyParameter: TLabel
            Left = 32
            Top = 84
            Width = 54
            Height = 15
            Caption = 'Parameter'
          end
          object LJalopyConfiguration: TLabel
            Left = 32
            Top = 52
            Width = 74
            Height = 15
            Caption = 'Configuration'
          end
          object LJalopy: TLabel
            Left = 16
            Top = 20
            Width = 30
            Height = 13
            Caption = 'Jalopy'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LJalopyClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object SBJalopySelect: TSpeedButton
            Tag = 15
            Left = 521
            Top = 16
            Width = 21
            Height = 19
            Flat = True
            Glyph.Data = {
              7E030000424D7E030000000000003600000028000000120000000F0000000100
              18000000000048030000880B0000880B000000000000000000005D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC10000000000000000000000
              000000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC100000000000000787800787800787800787800
              78780078780078780078780078780000005D5DC15D5DC15D5DC15D5DC15D5DC1
              6F725D5DC100000000FFFF000000007878007878007878007878007878007878
              0078780078780078780000005D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFF0000000078780078780078780078780078780078780078780078
              780078780000005D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF00000000787800787800787800787800787800787800787800787800787800
              00005D5DC15D5DC16F725D5DC1000000FFFFFF00FFFFFFFFFF00FFFF00000000
              00000000000000000000000000000000000000000000000000000000005D5DC1
              6F725D5DC100000000FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF
              00FFFF0000005D5DC15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC1000000
              FFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF00FFFFFFFFFF0000005D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC16F725D5DC100000000FFFFFFFFFF00FF
              FF0000000000000000000000000000000000000000005D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC1
              6F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC10000000000005D5DC15D5DC16F725D5DC15D5DC1
              5D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC10000005D5DC15D5DC15D5D
              C10000005D5DC10000005D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5D
              C15D5DC15D5DC15D5DC15D5DC15D5DC10000000000000000005D5DC15D5DC15D
              5DC15D5DC15D5DC16F725D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D
              5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC15D5DC1
              6F72}
            OnClick = SBJalopySelectClick
          end
          object EJalopyParameter: TEdit
            Left = 140
            Top = 80
            Width = 381
            Height = 23
            TabOrder = 0
          end
          object BJalopyConfiguration: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 1
            OnClick = BJalopyConfigurationClick
          end
          object EJalopyConfiguration: TEdit
            Left = 140
            Top = 48
            Width = 381
            Height = 23
            TabOrder = 2
          end
          object BJalopyInstall: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Install'
            TabOrder = 3
            OnClick = BJalopyInstallClick
          end
          object EJalopy: TEdit
            Left = 140
            Top = 16
            Width = 381
            Height = 23
            Hint = 'Checkstyle jar file'
            HelpType = htKeyword
            ParentShowHint = False
            ShowHint = True
            TabOrder = 4
          end
        end
        object PSubversion: TTabSheet
          Caption = 'Subversion'
          object LRepository: TLabel
            Left = 32
            Top = 52
            Width = 56
            Height = 15
            Caption = 'Repository'
          end
          object LSVN: TLabel
            Left = 16
            Top = 20
            Width = 51
            Height = 13
            Caption = 'SVN folder'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clGreen
            Font.Height = -12
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            OnClick = LSVNClick
            OnMouseEnter = LMouseEnter
            OnMouseLeave = LMouseLeave
          end
          object CBRepository: TComboBox
            Left = 140
            Top = 48
            Width = 394
            Height = 23
            TabOrder = 0
          end
          object BRepository: TButton
            Left = 550
            Top = 48
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 1
            OnClick = BRepositoryClick
          end
          object BSVN: TButton
            Left = 550
            Top = 16
            Width = 85
            Height = 23
            Caption = 'Select'
            TabOrder = 2
            OnClick = BSVNClick
          end
          object ESVNFolder: TEdit
            Left = 140
            Top = 16
            Width = 394
            Height = 23
            TabOrder = 3
          end
        end
      end
    end
  end
  object FolderDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders]
    Left = 736
    Top = 384
  end
end
