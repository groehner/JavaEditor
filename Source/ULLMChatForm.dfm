inherited LLMChatForm: TLLMChatForm
  Left = 0
  Top = 0
  HelpContext = 497
  Caption = 'Chat'
  ClientHeight = 642
  ClientWidth = 574
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 590
  ExplicitHeight = 681
  TextHeight = 15
  object pnlQuestion: TPanel
    Left = 0
    Top = 557
    Width = 574
    Height = 85
    Align = alBottom
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object pAsk: TPanel
      Left = 540
      Top = 1
      Width = 33
      Height = 83
      Align = alRight
      TabOrder = 0
      DesignSize = (
        33
        83)
      object sbAsk: TSpeedButton
        Left = 1
        Top = 5
        Width = 32
        Height = 32
        Action = actAskQuestion
        Anchors = [akTop, akRight]
        Images = vilImages
        Flat = True
      end
      object aiBusy: TActivityIndicator
        Left = 1
        Top = 51
        Anchors = [akRight, akBottom]
        FrameDelay = 150
        IndicatorType = aitRotatingSector
      end
    end
  end
  object ScrollBox: TScrollBox
    Left = 0
    Top = 34
    Width = 574
    Height = 518
    HorzScrollBar.Visible = False
    VertScrollBar.Tracking = True
    Align = alClient
    ParentBackground = True
    TabOrder = 1
    object QAStackPanel: TStackPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 568
      Height = 23
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      ControlCollection = <>
      DoubleBuffered = True
      HorizontalPositioning = sphpFill
      ParentColor = True
      ParentDoubleBuffered = False
      TabOrder = 0
      ExplicitWidth = 564
    end
  end
  object Splitter: TSpTBXSplitter
    Left = 0
    Top = 552
    Width = 574
    Height = 5
    Cursor = crSizeNS
    Align = alBottom
    ParentColor = False
    MinSize = 90
  end
  object SpTBXDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 574
    Height = 34
    AllowDrag = False
    DoubleBuffered = True
    object SpTBXToolbar: TSpTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockMode = dmCannotFloatOrChangeDocks
      DockPos = 0
      DragHandleStyle = dhNone
      FullSize = True
      Images = vilImages
      ParentShowHint = False
      ShowHint = True
      ShrinkMode = tbsmNone
      Stretch = True
      TabOrder = 0
      Customizable = False
      object spiNewTopic: TSpTBXItem
        Action = actChatNew
      end
      object spiRemoveTopic: TSpTBXItem
        Action = actChatRemove
      end
      object SpTBXSeparatorItem2: TSpTBXSeparatorItem
      end
      object spiSave: TSpTBXItem
        Action = actChatSave
      end
      object SpTBXSeparatorItem4: TSpTBXSeparatorItem
      end
      object spiTitle: TSpTBXItem
        Action = actTopicTitle
      end
      object spiCancel: TTBItem
        Action = actCancelRequest
      end
      object SpTBXRightAlignSpacerItem: TSpTBXRightAlignSpacerItem
        CustomWidth = 345
      end
      object spiPreviousTopic: TSpTBXItem
        Action = actChatPrevious
        Images = vilImages
      end
      object spiNextTopic: TSpTBXItem
        Action = actChatNext
      end
    end
  end
  object ChatActionList: TActionList
    Images = vilImages
    OnUpdate = ChatActionListUpdate
    Left = 32
    Top = 352
    object actChatSave: TAction
      Caption = 'Save chat'
      Hint = 'Save chat history'
      ImageIndex = 4
      ImageName = 'Save'
      OnExecute = actChatSaveExecute
    end
    object actChatRemove: TAction
      Caption = 'Remove Chat Topic'
      Hint = 'Remove current chat topic'
      ImageIndex = 3
      ImageName = 'ChatRemove'
      OnExecute = actChatRemoveExecute
    end
    object actChatNew: TAction
      Caption = 'New Chat Topic'
      Hint = 'Add a new chat topic'
      ImageIndex = 2
      ImageName = 'ChatPlus'
      OnExecute = actChatNewExecute
    end
    object actChatPrevious: TAction
      Caption = 'Previous Chat Topic'
      Hint = 'Show previous chat topic'
      ImageIndex = 0
      ImageName = 'ArrowLeft'
      OnExecute = actChatPreviousExecute
    end
    object actChatNext: TAction
      Caption = 'Next Chat Topic'
      Hint = 'Show next chat topic'
      ImageIndex = 1
      ImageName = 'ArrowRight'
      OnExecute = actChatNextExecute
    end
    object actCopyText: TAction
      Caption = 'Copy'
      Hint = 'Copy text'
      ImageIndex = 7
      ImageName = 'Copy'
      OnExecute = actCopyTextExecute
    end
    object actAskQuestion: TAction
      Hint = 'Ask question'
      ImageIndex = 9
      ImageName = 'ChatQuestion'
      OnExecute = actAskQuestionExecute
    end
    object actTopicTitle: TAction
      Caption = 'Topic Title'
      Hint = 'Set the title of the chat topic'
      ImageIndex = 5
      ImageName = 'Title'
      OnExecute = actTopicTitleExecute
    end
    object actCancelRequest: TAction
      Caption = 'Cancel Request'
      Hint = 'Cancel active request'
      ImageIndex = 6
      ImageName = 'Cancel'
      OnExecute = actCancelRequestExecute
    end
    object actCopyCode: TAction
      Caption = 'Copy Code'
      Hint = 'Copy the python code'
      OnExecute = actCopyCodeExecute
    end
    object actCopyToNewEditor: TAction
      Caption = 'Copy Code to New Editor'
      Hint = 'Copy the python code to a new editor'
      ImageIndex = 8
      ImageName = 'CopyNewEditor'
      OnExecute = actCopyToNewEditorExecute
    end
  end
  object AppEvents: TApplicationEvents
    OnMessage = AppEventsMessage
    Left = 120
    Top = 264
  end
  object SynMultiSyn: TSynMultiSyn
    Schemes = <
      item
        StartExpr = '```python'
        EndExpr = '```'
        MarkerAttri.Background = clNone
        MarkerAttri.Style = []
        SchemeName = 'Python'
      end
      item
        StartExpr = '```'
        EndExpr = '```'
        MarkerAttri.Background = clNone
        MarkerAttri.Style = []
        SchemeName = 'Python'
      end>
    Left = 32
    Top = 264
  end
  object pmAsk: TSpTBXPopupMenu
    Images = vilImages
    Left = 120
    Top = 136
    object mnCopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 7
      ImageName = 'Copy'
      OnClick = mnCopyClick
    end
    object mnPaste: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 10
      ImageName = 'Paste'
      OnClick = mnPasteClick
    end
  end
  object pmTextMenu: TSpTBXPopupMenu
    Images = vilImages
    OnPopup = pmTextMenuPopup
    Left = 32
    Top = 136
    object mnCopyText: TSpTBXItem
      Action = actCopyText
    end
    object mnCopyCode: TSpTBXItem
      Action = actCopyCode
    end
    object SpTBXSeparatorItem5: TSpTBXSeparatorItem
    end
    object mnCopyToNewEditor: TSpTBXItem
      Action = actCopyToNewEditor
    end
  end
  object icMenuAndToolbar: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'ArrowLeft'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M20,9V15H12V19.84L4.16,1' +
          '2L12,4.16V9H20Z" />'#13#10'</svg>'
      end
      item
        IconName = 'ArrowRight'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'    <path d="M4,15V9H12V4.16L19.84,12' +
          'L12,19.84V15H4Z" />'#13#10'</svg>'
      end
      item
        IconName = 'ChatPlus'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'  <path d="M12 3C17.5 3 22 6.58 22 11' +
          'C22 11.58 21.92 12.14 21.78 12.68C21.19 12.38 20.55 12.16 19.88 ' +
          '12.06C19.96 11.72 20 11.36 20 11C20 7.69 16.42 5 12 5C7.58 5 4 7' +
          '.69 4 11C4 14.31 7.58 17 12 17L13.09 16.95L13 18L13.08 18.95L12 ' +
          '19C10.81 19 9.62 18.83 8.47 18.5C6.64 20 4.37 20.89 2 21C4.33 18' +
          '.67 4.75 17.1 4.75 16.5C3.06 15.17 2.05 13.15 2 11C2 6.58 6.5 3 ' +
          '12 3M18 14H20V17H23V19H20V22H18V19H15V17H18V14Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ChatRemove'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'  <path d="M15.46 15.88L16.88 14.46L1' +
          '9 16.59L21.12 14.47L22.54 15.88L20.41 18L22.54 20.12L21.12 21.54' +
          'L19 19.41L16.88 21.54L15.46 20.12L17.59 18L15.47 15.88M12 3C17.5' +
          ' 3 22 6.58 22 11C22 11.58 21.92 12.14 21.78 12.68C21.19 12.38 20' +
          '.55 12.16 19.88 12.06C19.96 11.72 20 11.36 20 11C20 7.69 16.42 5' +
          ' 12 5C7.58 5 4 7.69 4 11C4 14.31 7.58 17 12 17L13.09 16.95L13 18' +
          'L13.08 18.95L12 19C10.81 19 9.62 18.83 8.47 18.5C6.64 20 4.37 20' +
          '.89 2 21C4.33 18.67 4.75 17.1 4.75 16.5C3.06 15.17 2.05 13.15 2 ' +
          '11C2 6.58 6.5 3 12 3Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Save'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path d="M23.5,2.5h-18c-1.7,0-3,1.4-' +
          '3,3v21c0,1.6,1.3,3,3,3h21c1.6,0,3-1.4,3-3v-18L23.5,2.5z M26.5,26' +
          '.5h-21v-21h16.8l4.2,4.2'#13#10#9#9'V26.5z M16,16c-2.5,0-4.5,1.9-4.5,4.5s' +
          '2,4.5,4.5,4.5s4.5-1.9,4.5-4.5S18.6,16,16,16z M7,7h13.5v6H7V7z"/>' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'Title'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'   <path d="M5,4V7H10.5V19H13.5V7H19V' +
          '4H5Z" />'#13#10'</svg>'
      end
      item
        IconName = 'Cancel'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10#9'<path fill="#E24444" d="M24.9,10.2L2' +
          '7,5.9L22.5,8l-6.2,6.2L5.6,3.5L3.4,5.8l10.7,10.7l-9,9l2.2,2.2l9-9' +
          'l9,9l3.3,0.7l-1.2-3l-8.9-8.9L24.9,10.2z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Copy'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(69.2 -212)">'#13 +
          #10#9'<path d="M-46.8,212.2h-17.2c-1.6,0-2.9,1.2-2.9,2.9v20.1h2.9v-2' +
          '0.1h17.2V212.2z M-42.5,218h-15.8'#13#10#9#9'c-1.6,0-2.9,1.2-2.9,2.9v20.1' +
          'c0,1.6,1.2,2.9,2.9,2.9h15.8c1.6,0,2.9-1.2,2.9-2.9v-20.1C-39.6,21' +
          '9.2-40.9,218-42.5,218L-42.5,218z'#13#10#9#9' M-42.5,240.9h-15.8v-20.1h15' +
          '.8V240.9z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CopyNewEditor'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-272.1 -317.4' +
          ')">'#13#10#9'<path d="M290.9,318.8h-11.3c-1.6,0-2.9,1.3-2.8,2.9v23.4c0,' +
          '1.6,1.2,2.9,2.8,2.9h17c1.6,0,2.9-1.3,2.8-2.9v-17.6'#13#10#9#9'L290.9,318' +
          '.8z M279.6,345.2v-23.4h9.9v7.3h7.1v16.1H279.6z"/>'#13#10'</g>'#13#10'</svg>'#13 +
          #10#13#10
      end
      item
        IconName = 'ChatQuestion'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'  <path d="M12 3C6.5 3 2 6.6 2 11C2 1' +
          '3.2 3.1 15.2 4.8 16.5C4.8 17.1 4.4 18.7 2 21C4.4 20.9 6.6 20 8.5' +
          ' 18.5C9.6 18.8 10.8 19 12 19C17.5 19 22 15.4 22 11S17.5 3 12 3M1' +
          '2 17C7.6 17 4 14.3 4 11S7.6 5 12 5 20 7.7 20 11 16.4 17 12 17M12' +
          '.2 6.5C11.3 6.5 10.6 6.7 10.1 7C9.5 7.4 9.2 8 9.3 8.7H11.3C11.3 ' +
          '8.4 11.4 8.2 11.6 8.1C11.8 8 12 7.9 12.3 7.9C12.6 7.9 12.9 8 13.' +
          '1 8.2C13.3 8.4 13.4 8.6 13.4 8.9C13.4 9.2 13.3 9.4 13.2 9.6C13 9' +
          '.8 12.8 10 12.6 10.1C12.1 10.4 11.7 10.7 11.5 10.9C11.1 11.2 11 ' +
          '11.5 11 12H13C13 11.7 13.1 11.5 13.1 11.3C13.2 11.1 13.4 11 13.6' +
          ' 10.8C14.1 10.6 14.4 10.3 14.7 9.9C15 9.5 15.1 9.1 15.1 8.7C15.1' +
          ' 8 14.8 7.4 14.3 7C13.9 6.7 13.1 6.5 12.2 6.5M11 13V15H13V13H11Z' +
          '"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Paste'
        SVGText = 
          '<svg viewBox="0 0 32 32">'#13#10'<g transform="translate(-180.6 -325.9' +
          ')">'#13#10#9'<path d="M206.8,329h-6.2c-0.9-2.3-3.3-3.4-5.6-2.6c-1.2,0.5' +
          '-2.2,1.4-2.6,2.6h-6.2c-1.6,0-2.9,1.2-2.9,2.8v23'#13#10#9#9'c0,1.6,1.3,2.' +
          '8,2.9,2.8h20.5c1.6,0,2.9-1.2,2.9-2.8v-23C209.7,330.2,208.4,329,2' +
          '06.8,329z M196.6,329c0.8,0,1.4,0.7,1.4,1.4'#13#10#9#9's-0.7,1.4-1.4,1.4s' +
          '-1.4-0.7-1.4-1.4S195.8,329,196.6,329z M206.8,354.8h-20.5v-23h2.9' +
          'v4.3h14.6v-4.3h2.9V354.8z"/>'#13#10'</g>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'UserQuestion'
        SVGText = 
          '<svg viewBox="0 0 24 24">'#13#10'  <path d="M20.5,14.5V16H19V14.5H20.5' +
          'M18.5,9.5H17V9A3,3 0 0,1 20,6A3,3 0 0,1 23,9C23,9.97 22.5,10.88 ' +
          '21.71,11.41L21.41,11.6C20.84,12 20.5,12.61 20.5,13.3V13.5H19V13.' +
          '3C19,12.11 19.6,11 20.59,10.35L20.88,10.16C21.27,9.9 21.5,9.47 2' +
          '1.5,9A1.5,1.5 0 0,0 20,7.5A1.5,1.5 0 0,0 18.5,9V9.5M9,13C11.67,1' +
          '3 17,14.34 17,17V20H1V17C1,14.34 6.33,13 9,13M9,4A4,4 0 0,1 13,8' +
          'A4,4 0 0,1 9,12A4,4 0 0,1 5,8A4,4 0 0,1 9,4M9,14.9C6.03,14.9 2.9' +
          ',16.36 2.9,17V18.1H15.1V17C15.1,16.36 11.97,14.9 9,14.9M9,5.9A2.' +
          '1,2.1 0 0,0 6.9,8A2.1,2.1 0 0,0 9,10.1A2.1,2.1 0 0,0 11.1,8A2.1,' +
          '2.1 0 0,0 9,5.9Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Assistant'
        SVGText = 
          '<svg viewBox="0 -960 960 960">'#13#10'    <circle r="70" cx="360" cy="' +
          '-640" fill="#E24444" /> '#13#10'    <circle r="70" cx="600" cy="-640" ' +
          'fill="#E24444" /> '#13#10#13#10'    <path'#13#10'        d="M160-120v-200q0-33 2' +
          '3.5-56.5T240-400h480q33 0 56.5 23.5T800-320v200H160Zm200-320'#13#10'  ' +
          '      q-83 0-141.5-58.5T160-640q0-83 58.5-141.5T360-840h240q83 0' +
          ' 141.5 58.5T800-640'#13#10'        q0 83-58.5 141.5T600-440H360Z'#13#10'    ' +
          '    M240-200h480v-120H240v120Zm120-320h240q50 0 85-35t35-85q0-50' +
          '-35-85t-85-35H360'#13#10'        q-50 0-85 35t-35 85q0 50 35 85t85 35Z' +
          '" />'#13#10'</svg>'
      end>
    Left = 264
    Top = 352
  end
  object vilImages: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'ArrowLeft'
        Name = 'ArrowLeft'
      end
      item
        CollectionIndex = 1
        CollectionName = 'ArrowRight'
        Name = 'ArrowRight'
      end
      item
        CollectionIndex = 2
        CollectionName = 'ChatPlus'
        Name = 'ChatPlus'
      end
      item
        CollectionIndex = 3
        CollectionName = 'ChatRemove'
        Name = 'ChatRemove'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Save'
        Name = 'Save'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Title'
        Name = 'Title'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Cancel'
        Name = 'Cancel'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 8
        CollectionName = 'CopyNewEditor'
        Name = 'CopyNewEditor'
      end
      item
        CollectionIndex = 9
        CollectionName = 'ChatQuestion'
        Name = 'ChatQuestion'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 11
        CollectionName = 'UserQuestion'
        Name = 'UserQuestion'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Assistant'
        Name = 'Assistant'
      end>
    ImageCollection = icMenuAndToolbar
    Width = 24
    Height = 24
    Left = 144
    Top = 352
  end
end