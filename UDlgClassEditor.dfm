object FClassEditor: TFClassEditor
  Left = 284
  Top = 181
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UML class editor'
  ClientHeight = 433
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnAfterMonitorDpiChanged = FormAfterMonitorDpiChanged
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 15
  object LClass: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 15
    Caption = 'Class'
  end
  object TreeView: TTreeView
    Left = 8
    Top = 29
    Width = 233
    Height = 404
    DragMode = dmAutomatic
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    HideSelection = False
    Indent = 19
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
    TabStop = False
    OnChange = TreeViewChange
    OnDragDrop = TreeViewDragDrop
    OnDragOver = TreeViewDragOver
  end
  object PageControl: TPageControl
    Left = 247
    Top = 8
    Width = 368
    Height = 425
    ActivePage = TSMethods
    TabOrder = 0
    OnChange = PageControlChange
    object TSClass: TTabSheet
      Caption = '&Class'
      object LClassName: TLabel
        Left = 8
        Top = 20
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object LExtends: TLabel
        Left = 8
        Top = 52
        Width = 41
        Height = 15
        Caption = 'Extends'
      end
      object LImplements: TLabel
        Left = 8
        Top = 84
        Width = 63
        Height = 15
        Caption = 'Implements'
      end
      object EClass: TEdit
        Left = 96
        Top = 16
        Width = 129
        Height = 23
        TabOrder = 0
        OnKeyPress = EClassKeyPress
      end
      object BClassClose: TButton
        Left = 272
        Top = 360
        Width = 80
        Height = 25
        Action = ActionClose
        ModalResult = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object CBClassAbstract: TCheckBox
        Left = 96
        Top = 112
        Width = 97
        Height = 17
        Caption = 'abstract'
        TabOrder = 3
        OnMouseUp = CBClassAbstractMouseUp
      end
      object EExtends: TEdit
        Left = 96
        Top = 48
        Width = 129
        Height = 23
        TabOrder = 1
        OnKeyPress = EClassKeyPress
      end
      object BInterface: TButton
        Left = 184
        Top = 360
        Width = 80
        Height = 25
        Action = ActionInterface
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object EImplements: TEdit
        Left = 96
        Top = 80
        Width = 129
        Height = 23
        TabOrder = 2
        OnKeyPress = EClassKeyPress
      end
      object BClassApply: TButton
        Left = 96
        Top = 360
        Width = 80
        Height = 25
        Action = ActionApply
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object BClassNew: TButton
        Left = 8
        Top = 360
        Width = 80
        Height = 25
        Action = ActionNew
        TabOrder = 7
      end
      object CBClassInner: TCheckBox
        Left = 96
        Top = 135
        Width = 97
        Height = 17
        Caption = 'inner'
        TabOrder = 8
      end
    end
    object TSAttributes: TTabSheet
      Caption = '&Attributes'
      ImageIndex = 1
      object LAttributeName: TLabel
        Left = 8
        Top = 156
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object LAttributeType: TLabel
        Left = 8
        Top = 188
        Width = 24
        Height = 15
        Caption = 'Type'
      end
      object LAttributeValue: TLabel
        Left = 8
        Top = 220
        Width = 28
        Height = 15
        Caption = 'Value'
      end
      object RGAttributeAccess: TRadioGroup
        Left = 64
        Top = 16
        Width = 100
        Height = 115
        Caption = 'Visibility'
        ItemIndex = 0
        Items.Strings = (
          'private'
          'package'
          'protected'
          'public')
        TabOrder = 0
      end
      object CBAttributeType: TComboBox
        Left = 64
        Top = 184
        Width = 217
        Height = 23
        AutoDropDown = True
        Sorted = True
        TabOrder = 3
        OnCloseUp = ComboBoxCloseUp
        OnDropDown = CBAttributeTypeDropDown
        OnEnter = CBComboBoxEnter
        OnKeyDown = ComboBoxKeyDown
        OnKeyPress = CBAttributeTypeKeyPress
        OnKeyUp = CBAttributeTypeKeyUp
        OnSelect = CBAttributeTypeSelect
        Items.Strings = (
          'boolean'
          'char'
          'double'
          'float'
          'int'
          'String')
      end
      object BAttributeDelete: TButton
        Left = 184
        Top = 360
        Width = 80
        Height = 25
        Action = ActionDelete
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
      object BAttributeNew: TButton
        Left = 8
        Top = 360
        Width = 80
        Height = 25
        Action = ActionNew
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
      object GBAttributeOptions: TGroupBox
        Left = 176
        Top = 16
        Width = 105
        Height = 115
        Caption = 'Options'
        TabOrder = 1
        object CBsetMethod: TCheckBox
          Left = 8
          Top = 90
          Width = 90
          Height = 17
          Caption = 'set method'
          Checked = True
          State = cbChecked
          TabOrder = 3
        end
        object CBgetMethod: TCheckBox
          Left = 8
          Top = 66
          Width = 90
          Height = 17
          Caption = 'get method'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object CBAttributeStatic: TCheckBox
          Left = 8
          Top = 18
          Width = 78
          Height = 17
          Caption = 'static'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CBAttributeFinal: TCheckBox
          Left = 8
          Top = 42
          Width = 78
          Height = 17
          Caption = 'final'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
      object EAttributeName: TEdit
        Left = 64
        Top = 152
        Width = 217
        Height = 23
        TabOrder = 2
        OnKeyPress = EAttributeNameKeyPress
        OnKeyUp = EAttributeNameKeyUp
      end
      object EAttributeValue: TEdit
        Left = 64
        Top = 216
        Width = 217
        Height = 23
        TabOrder = 4
        OnExit = BAttributeChangeClick
        OnKeyPress = EAttributeValueKeyPress
      end
      object BAttributeClose: TButton
        Left = 270
        Top = 360
        Width = 80
        Height = 25
        Action = ActionClose
        ModalResult = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
      end
      object BAttributeApply: TButton
        Left = 96
        Top = 360
        Width = 80
        Height = 25
        Action = ActionApply
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
    end
    object TSMethods: TTabSheet
      Caption = '&Methods'
      ImageIndex = 2
      object LMethodName: TLabel
        Left = 8
        Top = 140
        Width = 32
        Height = 15
        Caption = 'Name'
      end
      object LMethodType: TLabel
        Left = 8
        Top = 172
        Width = 24
        Height = 15
        Caption = 'Type'
      end
      object CBMethodType: TComboBox
        Left = 64
        Top = 163
        Width = 162
        Height = 23
        AutoDropDown = True
        Sorted = True
        TabOrder = 4
        OnCloseUp = ComboBoxCloseUp
        OnDropDown = CBMethodTypeDropDown
        OnEnter = CBComboBoxEnter
        OnKeyDown = ComboBoxKeyDown
        OnKeyPress = CBMethodTypeKeyPress
        OnKeyUp = CBMethodParamTypeKeyUp
        OnSelect = CBMethodTypeSelect
        Items.Strings = (
          'boolean'
          'char'
          'double'
          'float'
          'int'
          'String')
      end
      object RGMethodAccess: TRadioGroup
        Left = 126
        Top = 16
        Width = 100
        Height = 105
        Caption = 'Visibility'
        ItemIndex = 3
        Items.Strings = (
          'private'
          'package'
          'protected'
          'public')
        TabOrder = 1
      end
      object GBMethodOptions: TGroupBox
        Left = 244
        Top = 16
        Width = 100
        Height = 105
        Caption = 'Options'
        TabOrder = 2
        object CBMethodStatic: TCheckBox
          Left = 8
          Top = 18
          Width = 57
          Height = 17
          Caption = 'static'
          TabOrder = 0
        end
        object CBMethodAbstract: TCheckBox
          Left = 8
          Top = 42
          Width = 81
          Height = 17
          Caption = 'abstract'
          TabOrder = 1
        end
      end
      object BMethodNew: TButton
        Left = 8
        Top = 360
        Width = 80
        Height = 25
        Action = ActionNew
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
      end
      object BMethodDelete: TButton
        Left = 184
        Top = 360
        Width = 80
        Height = 25
        Action = ActionDelete
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
      end
      object RGMethodKind: TRadioGroup
        Left = 8
        Top = 16
        Width = 100
        Height = 105
        Caption = 'Kind'
        Items.Strings = (
          'Constructor'
          'Procedure'
          'Function')
        TabOrder = 0
      end
      object GRFormalParameters: TGroupBox
        Left = 8
        Top = 208
        Width = 337
        Height = 137
        Caption = 'Formal parameters'
        TabOrder = 5
        object LParameterName: TLabel
          Left = 13
          Top = 32
          Width = 32
          Height = 15
          Caption = 'Name'
        end
        object LParameterType: TLabel
          Left = 13
          Top = 64
          Width = 24
          Height = 15
          Caption = 'Type'
        end
        object SBUp: TSpeedButton
          Left = 310
          Top = 23
          Width = 18
          Height = 18
          ImageIndex = 4
          ImageName = '04'
          Images = ILClassEditor
          OnClick = SBUpClick
        end
        object SBDelete: TSpeedButton
          Left = 310
          Top = 46
          Width = 18
          Height = 19
          ImageIndex = 0
          ImageName = '00'
          Images = ILClassEditor
          NumGlyphs = 2
          OnClick = SBDeleteClick
        end
        object SBDown: TSpeedButton
          Left = 310
          Top = 70
          Width = 18
          Height = 18
          ImageIndex = 5
          ImageName = '05'
          Images = ILClassEditor
          OnClick = SBDownClick
        end
        object SBRight: TSpeedButton
          Left = 176
          Top = 34
          Width = 18
          Height = 18
          ImageIndex = 3
          ImageName = '03'
          Images = ILClassEditor
          OnClick = SBRightClick
        end
        object SBLeft: TSpeedButton
          Left = 176
          Top = 60
          Width = 18
          Height = 18
          ImageIndex = 2
          ImageName = '02'
          Images = ILClassEditor
          OnClick = SBLeftClick
        end
        object CBParamType: TComboBox
          Left = 56
          Top = 60
          Width = 113
          Height = 23
          AutoDropDown = True
          Sorted = True
          TabOrder = 1
          OnCloseUp = ComboBoxCloseUp
          OnDropDown = CBParamTypeDropDown
          OnEnter = CBComboBoxEnter
          OnKeyDown = ComboBoxKeyDown
          OnKeyPress = CBParamTypeKeyPress
          OnKeyUp = CBMethodParamTypeKeyUp
          OnSelect = CBParamTypeSelect
          Items.Strings = (
            'boolean'
            'char'
            'double'
            'float'
            'int'
            'String')
        end
        object BParameterNew: TButton
          Left = 178
          Top = 96
          Width = 75
          Height = 25
          Caption = 'New'
          TabOrder = 4
          OnClick = BParameterNewClick
        end
        object LBParams: TListBox
          Left = 200
          Top = 27
          Width = 104
          Height = 57
          TabStop = False
          ItemHeight = 15
          TabOrder = 3
          OnKeyUp = LBParamsKeyUp
        end
        object CBParamName: TComboBox
          Left = 57
          Top = 33
          Width = 113
          Height = 23
          Sorted = True
          TabOrder = 0
          OnEnter = CBComboBoxEnter
          OnKeyDown = ComboBoxKeyDown
          OnKeyPress = CBParamNameKeyPress
          OnKeyUp = CBParamNameKeyUp
          OnSelect = CBParamNameSelect
        end
        object CBParameter: TComboBox
          Left = 13
          Top = 98
          Width = 156
          Height = 23
          AutoDropDown = True
          TabOrder = 2
          Text = 'Select attribute'
          OnEnter = CBComboBoxEnter
          OnKeyDown = ComboBoxKeyDown
          OnKeyPress = CBParameterKeyPress
          OnSelect = CBParameterSelect
        end
        object BParameterDelete: TButton
          Left = 259
          Top = 96
          Width = 75
          Height = 25
          Caption = 'Delete'
          TabOrder = 5
          OnClick = BParameterDeleteClick
        end
      end
      object BMethodClose: TButton
        Left = 272
        Top = 360
        Width = 80
        Height = 25
        Action = ActionClose
        ModalResult = 1
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
      end
      object CBMethodname: TComboBox
        Left = 64
        Top = 136
        Width = 162
        Height = 23
        Sorted = True
        TabOrder = 3
        OnKeyDown = ComboBoxKeyDown
        OnKeyPress = CBMethodnameKeyPress
        OnKeyUp = CBMethodnameKeyUp
        OnSelect = CBMethodnameSelect
      end
      object BMethodApply: TButton
        Left = 96
        Top = 360
        Width = 80
        Height = 25
        Action = ActionApply
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 144
    Top = 56
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 32
    Top = 56
    object ActionNew: TAction
      Caption = '&New'
      OnExecute = ActionNewExecute
    end
    object ActionDelete: TAction
      Caption = '&Delete'
      OnExecute = ActionDeleteExecute
    end
    object ActionClose: TAction
      Caption = '&Close'
    end
    object ActionInterface: TAction
      Caption = '&Interface'
      OnExecute = ActionInterfaceExecute
    end
    object ActionApply: TAction
      Caption = '&Apply'
      OnExecute = ActionApplyExecute
    end
  end
  object ILClassEditor: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = '00'
        Name = '00'
      end
      item
        CollectionIndex = 1
        CollectionName = '01'
        Name = '01'
      end
      item
        CollectionIndex = 2
        CollectionName = '02'
        Name = '02'
      end
      item
        CollectionIndex = 3
        CollectionName = '03'
        Name = '03'
      end
      item
        CollectionIndex = 4
        CollectionName = '04'
        Name = '04'
      end
      item
        CollectionIndex = 5
        CollectionName = '05'
        Name = '05'
      end>
    ImageCollection = icClassEditor
    Width = 18
    Height = 18
    Left = 40
    Top = 216
  end
  object icClassEditor: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = '00'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13" >'#13#10'<path stroke="#1e1e1e" d="M0 0h3M' +
          '12 0h1M0 1h4M10 1h2M1 2h4M9 2h2M3 3h3M8 3h2M4 4h5M5 5h3M4 6h5M3 ' +
          '7h3M8 7h2M2 8h3M9 8h2M1 9h3M10 9h1M0 10h4M11 10h1M0 11h3M1 12h1M' +
          '12 12h1" />'#13#10'</svg>'
      end
      item
        IconName = '01'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#e1e1e1" d="M0 0h3M1' +
          '2 0h1M0 1h4M10 1h2M1 2h4M9 2h2M3 3h3M8 3h2M4 4h5M5 5h3M4 6h5M3 7' +
          'h3M8 7h2M2 8h3M9 8h2M1 9h3M10 9h1M0 10h4M11 10h1M0 11h3M1 12h1M1' +
          '2 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '02'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#d0d8cc" d="M6 1h1" ' +
          '/>'#13#10'<path stroke="#9ebe9e" d="M7 1h1M6 2h1" />'#13#10'<path stroke="#d' +
          '1dbcd" d="M5 2h1" />'#13#10'<path stroke="#606f50" d="M7 2h1" />'#13#10'<pat' +
          'h stroke="#d2dbce" d="M4 3h1" />'#13#10'<path stroke="#aec69e" d="M5 3' +
          'h1M4 4h1" />'#13#10'<path stroke="#ceefbe" d="M6 3h1M3 6h1" />'#13#10'<path ' +
          'stroke="#515850" d="M7 3h1" />'#13#10'<path stroke="#d3ddd0" d="M3 4h1' +
          '" />'#13#10'<path stroke="#cef7be" d="M5 4h1M4 5h1" />'#13#10'<path stroke="' +
          '#bee7ae" d="M6 4h1M5 5h1" />'#13#10'<path stroke="#425842" d="M7 4h1" ' +
          '/>'#13#10'<path stroke="#8ea68e" d="M8 4h1" />'#13#10'<path stroke="#8e9e7e"' +
          ' d="M9 4h2" />'#13#10'<path stroke="#7f967e" d="M11 4h1" />'#13#10'<path str' +
          'oke="#d4ddd0" d="M2 5h1" />'#13#10'<path stroke="#aece9e" d="M3 5h1" /' +
          '>'#13#10'<path stroke="#aedf8e" d="M6 5h1M5 6h1" />'#13#10'<path stroke="#be' +
          'df9e" d="M7 5h1" />'#13#10'<path stroke="#aed69e" d="M8 5h1" />'#13#10'<path' +
          ' stroke="#9ec68e" d="M9 5h1" />'#13#10'<path stroke="#8ebe8e" d="M10 5' +
          'h1" />'#13#10'<path stroke="#424942" d="M11 5h1M7 8h1" />'#13#10'<path strok' +
          'e="#9eb68e" d="M2 6h1" />'#13#10'<path stroke="#beefae" d="M4 6h1" />'#13 +
          #10'<path stroke="#9ed68e" d="M6 6h1M5 7h1" />'#13#10'<path stroke="#8ec6' +
          '7e" d="M7 6h1" />'#13#10'<path stroke="#7fb66f" d="M8 6h1" />'#13#10'<path s' +
          'troke="#6fae60" d="M9 6h1" />'#13#10'<path stroke="#608650" d="M10 6h1' +
          '" />'#13#10'<path stroke="#334233" d="M11 6h1M8 8h1M7 9h1M6 10h2" />'#13#10 +
          '<path stroke="#c1c5bd" d="M2 7h1" />'#13#10'<path stroke="#516750" d="' +
          'M3 7h1" />'#13#10'<path stroke="#7fa66f" d="M4 7h1M6 8h1" />'#13#10'<path st' +
          'roke="#8ebe6f" d="M6 7h1" />'#13#10'<path stroke="#7fa660" d="M7 7h1" ' +
          '/>'#13#10'<path stroke="#6f9660" d="M8 7h1M5 8h1" />'#13#10'<path stroke="#6' +
          '08e50" d="M9 7h1" />'#13#10'<path stroke="#517e42" d="M10 7h1" />'#13#10'<pa' +
          'th stroke="#333a33" d="M11 7h1M9 8h1M7 11h1" />'#13#10'<path stroke="#' +
          'cecec7" d="M3 8h1" />'#13#10'<path stroke="#516050" d="M4 8h1" />'#13#10'<pa' +
          'th stroke="#252c25" d="M10 8h1" />'#13#10'<path stroke="#252525" d="M1' +
          '1 8h1" />'#13#10'<path stroke="#ccccc5" d="M4 9h1" />'#13#10'<path stroke="#' +
          '607760" d="M5 9h1" />'#13#10'<path stroke="#607e50" d="M6 9h1" />'#13#10'<pa' +
          'th stroke="#c5c5c0" d="M5 10h1" />'#13#10'<path stroke="#c4c4c0" d="M6' +
          ' 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '03'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#7f8e7e" d="M6 1h1M6' +
          ' 2h1M6 3h1" />'#13#10'<path stroke="#c4c4c0" d="M7 1h1" />'#13#10'<path stro' +
          'ke="#252525" d="M7 2h1M8 3h1M9 4h1M10 5h1" />'#13#10'<path stroke="#a7' +
          'aaa2" d="M8 2h1" />'#13#10'<path stroke="#517742" d="M7 3h1M8 4h1M9 5h' +
          '1" />'#13#10'<path stroke="#aaaea6" d="M9 3h1" />'#13#10'<path stroke="#7f96' +
          '7e" d="M2 4h1M10 7h1M6 8h1M9 8h1M8 9h1M7 10h1" />'#13#10'<path stroke=' +
          '"#252c25" d="M3 4h1" />'#13#10'<path stroke="#333a33" d="M4 4h1" />'#13#10'<' +
          'path stroke="#334233" d="M5 4h1" />'#13#10'<path stroke="#424942" d="M' +
          '6 4h1" />'#13#10'<path stroke="#608e50" d="M7 4h1M10 6h1" />'#13#10'<path st' +
          'roke="#a9ada4" d="M10 4h1" />'#13#10'<path stroke="#8e9e7e" d="M2 5h1M' +
          '5 8h1M6 9h1" />'#13#10'<path stroke="#bee7ae" d="M3 5h1M5 7h1M7 8h1" /' +
          '>'#13#10'<path stroke="#7fae6f" d="M4 5h1M9 7h1" />'#13#10'<path stroke="#7f' +
          'a66f" d="M5 5h1" />'#13#10'<path stroke="#6f9e60" d="M6 5h1" />'#13#10'<path' +
          ' stroke="#6f9660" d="M7 5h1" />'#13#10'<path stroke="#6fa660" d="M8 5h' +
          '1M9 6h1" />'#13#10'<path stroke="#a8aba3" d="M11 5h1" />'#13#10'<path stroke' +
          '="#8ea68e" d="M2 6h1M4 8h1M6 10h1" />'#13#10'<path stroke="#beefae" d=' +
          '"M3 6h1M4 7h1" />'#13#10'<path stroke="#aedf8e" d="M4 6h1" />'#13#10'<path s' +
          'troke="#9ed68e" d="M5 6h1" />'#13#10'<path stroke="#9ece7e" d="M6 6h1"' +
          ' />'#13#10'<path stroke="#8ebe6f" d="M7 6h1M8 7h1" />'#13#10'<path stroke="#' +
          '7fb66f" d="M8 6h1" />'#13#10'<path stroke="#607760" d="M11 6h1" />'#13#10'<p' +
          'ath stroke="#9eb68e" d="M2 7h1M3 8h1M6 11h1" />'#13#10'<path stroke="#' +
          'ceefae" d="M3 7h1" />'#13#10'<path stroke="#bee79e" d="M6 7h1" />'#13#10'<pa' +
          'th stroke="#aedf9e" d="M7 7h1" />'#13#10'<path stroke="#c7cbbc" d="M11' +
          ' 7h1" />'#13#10'<path stroke="#9ebe8e" d="M2 8h1" />'#13#10'<path stroke="#8' +
          'ec67e" d="M8 8h1" />'#13#10'<path stroke="#c8cdbe" d="M10 8h1" />'#13#10'<pa' +
          'th stroke="#bedf9e" d="M7 9h1" />'#13#10'<path stroke="#c9cebf" d="M9 ' +
          '9h1" />'#13#10'<path stroke="#cacfc0" d="M8 10h1" />'#13#10'<path stroke="#d' +
          '0d9cc" d="M7 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '04'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#c2cde2" d="M5 2h1" ' +
          '/>'#13#10'<path stroke="#6f96df" d="M6 2h1M3 5h1M2 6h1" />'#13#10'<path stro' +
          'ke="#c1c7d2" d="M7 2h1" />'#13#10'<path stroke="#c3cee3" d="M4 3h1" />' +
          #13#10'<path stroke="#6f9eef" d="M5 3h1M4 4h1" />'#13#10'<path stroke="#9eb' +
          'eff" d="M6 3h1M5 4h1M4 5h1M5 7h1" />'#13#10'<path stroke="#33589e" d="' +
          'M7 3h1" />'#13#10'<path stroke="#c0c6cf" d="M8 3h1" />'#13#10'<path stroke="' +
          '#c4cfe5" d="M3 4h1" />'#13#10'<path stroke="#8eaeff" d="M6 4h1M5 5h1M5' +
          ' 6h1" />'#13#10'<path stroke="#6086df" d="M7 4h1M7 7h1M6 8h1M5 9h1" />' +
          #13#10'<path stroke="#33509e" d="M8 4h1M9 5h1M5 10h1" />'#13#10'<path strok' +
          'e="#bec4ce" d="M9 4h1" />'#13#10'<path stroke="#c5d0e5" d="M2 5h1" />'#13 +
          #10'<path stroke="#6f9eff" d="M6 5h1" />'#13#10'<path stroke="#608eef" d=' +
          '"M7 5h1" />'#13#10'<path stroke="#4277ce" d="M8 5h1" />'#13#10'<path stroke=' +
          '"#bdc3cc" d="M10 5h1" />'#13#10'<path stroke="#426fce" d="M3 6h1" />'#13#10 +
          '<path stroke="#3367ce" d="M4 6h1" />'#13#10'<path stroke="#6096ef" d="' +
          'M6 6h2M6 7h1" />'#13#10'<path stroke="#3358ae" d="M8 6h1" />'#13#10'<path st' +
          'roke="#25509e" d="M9 6h1M8 7h1" />'#13#10'<path stroke="#25498e" d="M1' +
          '0 6h1M8 9h1M6 10h3" />'#13#10'<path stroke="#517edf" d="M4 7h1M4 8h1M4' +
          ' 9h1M4 10h1" />'#13#10'<path stroke="#7fa6ef" d="M5 8h1" />'#13#10'<path str' +
          'oke="#5177ce" d="M7 8h1" />'#13#10'<path stroke="#25499e" d="M8 8h1" /' +
          '>'#13#10'<path stroke="#517ece" d="M6 9h1" />'#13#10'<path stroke="#426fbe" ' +
          'd="M7 9h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = '05'
        SVGText = 
          '<svg viewBox="0 -0.5 13 13">'#13#10'<path stroke="#6f9eef" d="M4 2h1M2' +
          ' 6h1" />'#13#10'<path stroke="#6f96df" d="M5 2h1M4 3h1M3 6h1" />'#13#10'<pat' +
          'h stroke="#6f8edf" d="M6 2h1M4 4h1M4 6h1" />'#13#10'<path stroke="#608' +
          '6ce" d="M7 2h2M4 5h1" />'#13#10'<path stroke="#9ebeff" d="M5 3h1M5 4h1' +
          '" />'#13#10'<path stroke="#8eb6ff" d="M6 3h1" />'#13#10'<path stroke="#8eaef' +
          'f" d="M7 3h1" />'#13#10'<path stroke="#516fbe" d="M8 3h1" />'#13#10'<path st' +
          'roke="#7fa6ff" d="M6 4h1M5 6h1" />'#13#10'<path stroke="#608eef" d="M7' +
          ' 4h1M6 6h1" />'#13#10'<path stroke="#4267ae" d="M8 4h1M10 6h1" />'#13#10'<pa' +
          'th stroke="#7faeff" d="M5 5h1" />'#13#10'<path stroke="#608eff" d="M6 ' +
          '5h1" />'#13#10'<path stroke="#5186ef" d="M7 5h1" />'#13#10'<path stroke="#42' +
          '60ae" d="M8 5h1M9 6h1" />'#13#10'<path stroke="#517edf" d="M7 6h1M5 8h' +
          '1" />'#13#10'<path stroke="#33509e" d="M8 6h1M9 7h1M8 8h1" />'#13#10'<path s' +
          'troke="#5177be" d="M3 7h1" />'#13#10'<path stroke="#6f96ef" d="M4 7h1"' +
          ' />'#13#10'<path stroke="#6f9eff" d="M5 7h1" />'#13#10'<path stroke="#5186df' +
          '" d="M6 7h1" />'#13#10'<path stroke="#5177ce" d="M7 7h1" />'#13#10'<path str' +
          'oke="#4267be" d="M8 7h1M7 8h1M6 9h1" />'#13#10'<path stroke="#bdc3cc" ' +
          'd="M10 7h1" />'#13#10'<path stroke="#3358ae" d="M4 8h1M5 9h1M6 10h1" /' +
          '>'#13#10'<path stroke="#5177df" d="M6 8h1" />'#13#10'<path stroke="#bec4ce" ' +
          'd="M9 8h1" />'#13#10'<path stroke="#d2d5df" d="M4 9h1" />'#13#10'<path strok' +
          'e="#33589e" d="M7 9h1" />'#13#10'<path stroke="#c0c6cf" d="M8 9h1" />'#13 +
          #10'<path stroke="#d1d4dd" d="M5 10h1" />'#13#10'<path stroke="#c1c7d2" d' +
          '="M7 10h1" />'#13#10'</svg>'#13#10
      end>
    Left = 136
    Top = 216
  end
end
