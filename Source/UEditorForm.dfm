object FEditForm: TFEditForm
  Left = 333
  Top = 204
  Align = alClient
  BorderIcons = []
  ClientHeight = 546
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    687
    546)
  TextHeight = 15
  object TVFileStructure: TTreeView
    Left = 192
    Top = -3
    Width = 121
    Height = 20
    Indent = 19
    TabOrder = 0
    Visible = False
    OnChange = TVFileStructureChange
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 525
    Width = 687
    Height = 21
    Align = alBottom
    TabOrder = 1
    object DesignButton: TButton
      Left = 1
      Top = 1
      Width = 56
      Height = 19
      Hint = 'Design on/off'
      Align = alLeft
      Caption = 'Design'
      TabOrder = 0
      OnClick = DesignButtonClick
    end
    object StatusBar: TStatusBar
      Left = 57
      Top = 1
      Width = 633
      Height = 19
      Align = alClient
      Panels = <
        item
          Alignment = taCenter
          Width = 50
        end
        item
          Alignment = taCenter
          Text = 'Column'
          Width = 50
        end
        item
          Alignment = taCenter
          Width = 70
        end
        item
          Width = 70
        end>
      ExplicitWidth = 629
    end
  end
  object EditformToolbar: TToolBar
    Left = 0
    Top = 0
    Width = 46
    Height = 525
    Align = alLeft
    AutoSize = True
    Images = vilEditorToolbarLight
    TabOrder = 2
    object TBClose: TToolButton
      Left = 0
      Top = 0
      Hint = 'Close'
      ImageIndex = 0
      ImageName = 'Close'
      OnClick = SBCloseClick
    end
    object TBExplorer: TToolButton
      Left = 23
      Top = 0
      Hint = 'Explorer'
      ImageIndex = 1
      ImageName = 'Explorer'
      Wrap = True
      OnClick = SBExplorerClick
    end
    object TBBrowser: TToolButton
      Left = 0
      Top = 22
      Hint = 'Show in browser'
      ImageIndex = 2
      ImageName = 'Browser'
      OnClick = SBBrowserClick
    end
    object TBDesignform: TToolButton
      Left = 23
      Top = 22
      Hint = 'Open the associated design form'
      ImageIndex = 3
      ImageName = 'Designform'
      Wrap = True
      OnClick = SBDesignformClick
    end
    object TBStructure: TToolButton
      Left = 0
      Top = 44
      Hint = 'Edit class structure'
      ImageIndex = 4
      ImageName = 'Classedit'
      OnClick = SBClassEditClick
    end
    object TBClassOpen: TToolButton
      Left = 23
      Top = 44
      Hint = 'Open class in UML window'
      ImageIndex = 5
      ImageName = 'ClassOpen'
      Wrap = True
      OnClick = SBClassOpenClick
    end
    object TBMatchBracket: TToolButton
      Left = 0
      Top = 66
      Hint = 'Go to the related bracket'
      ImageIndex = 6
      ImageName = 'MatchBracket'
      OnClick = SBMatchBracketClick
    end
    object TBSystemOutPrintln: TToolButton
      Left = 23
      Top = 66
      Hint = 'System.out.println'
      ImageIndex = 7
      ImageName = 'SystemOutPrintln'
      Wrap = True
      OnClick = SBSystemOutPrintlnClick
    end
    object TBStructureIndent: TToolButton
      Left = 0
      Top = 88
      Hint = 'Structured indentation'
      ImageIndex = 8
      ImageName = 'StructureIndent'
      OnClick = SBStructureIndentClick
    end
    object TBIfStatement: TToolButton
      Tag = 1
      Left = 23
      Top = 88
      Hint = 'if statement'
      ImageIndex = 9
      ImageName = 'IfStatement'
      Wrap = True
      OnClick = SBStatementClick
    end
    object TBIfElseStatement: TToolButton
      Tag = 9
      Left = 0
      Top = 110
      Hint = 'if-else statement'
      ImageIndex = 10
      ImageName = 'IfElseStatement'
      OnClick = SBStatementClick
    end
    object TBWhileStatement: TToolButton
      Tag = 2
      Left = 23
      Top = 110
      Hint = 'while statement'
      ImageIndex = 11
      ImageName = 'WhileStatement'
      Wrap = True
      OnClick = SBStatementClick
    end
    object TBForStatement: TToolButton
      Tag = 3
      Left = 0
      Top = 132
      Hint = 'for statement'
      ImageIndex = 12
      ImageName = 'ForStatement'
      OnClick = SBStatementClick
    end
    object TBDoWhileStatement: TToolButton
      Tag = 4
      Left = 23
      Top = 132
      Hint = 'do-while statement'
      ImageIndex = 13
      ImageName = 'DoWhileStatement'
      Wrap = True
      OnClick = SBStatementClick
    end
    object TBSwitchStatement: TToolButton
      Tag = 5
      Left = 0
      Top = 154
      Hint = 'switch statement'
      ImageIndex = 14
      ImageName = 'SwitchStatement'
      OnClick = SBStatementClick
    end
    object TBTryStatement: TToolButton
      Tag = 6
      Left = 23
      Top = 154
      Hint = 'try statement'
      ImageIndex = 15
      ImageName = 'TryStatement'
      Wrap = True
      OnClick = SBStatementClick
    end
    object TBBlockStatement: TToolButton
      Tag = 10
      Left = 0
      Top = 176
      Hint = 'block statement'
      ImageIndex = 16
      ImageName = 'BlockStatement'
      OnClick = SBStatementClick
    end
    object TBComment: TToolButton
      Left = 23
      Top = 176
      Hint = 'Comment/Uncomment'
      ImageIndex = 17
      ImageName = 'Comment'
      Wrap = True
      OnClick = SBCommentClick
    end
    object TBIndent: TToolButton
      Left = 0
      Top = 198
      Hint = 'Indent'
      ImageIndex = 18
      ImageName = 'Indent'
      OnClick = SBIndentClick
    end
    object TBUnindent: TToolButton
      Left = 23
      Top = 198
      Hint = 'Unindent'
      ImageIndex = 19
      ImageName = 'Unindent'
      Wrap = True
      OnClick = SBUnindentClick
    end
    object TBWordWrap: TToolButton
      Left = 0
      Top = 220
      Hint = 'Word wrap'
      ImageIndex = 20
      ImageName = 'WordWrap'
      OnClick = SBWordWrapClick
    end
    object TBBreakpoint: TToolButton
      Left = 23
      Top = 220
      Hint = 'Breakpoint on/off'
      ImageIndex = 21
      ImageName = 'Breakpoint'
      Wrap = True
      OnClick = SBBreakpointClick
    end
    object TBBreakpointsClear: TToolButton
      Left = 0
      Top = 242
      Hint = 'Clear breakpoints'
      ImageIndex = 22
      ImageName = 'BreakpointsClear'
      OnClick = SBBreakpointsClearClick
    end
    object TBBookmark: TToolButton
      Left = 23
      Top = 242
      Hint = 'Set bookmark'
      ImageIndex = 23
      ImageName = 'Bookmark'
      Wrap = True
      OnClick = SBBookmarkClick
    end
    object TBGotoBookmark: TToolButton
      Left = 0
      Top = 264
      Hint = 'Go to bookmark'
      ImageIndex = 24
      ImageName = 'GotoBookmark'
      OnClick = SBGotoBookmarkClick
    end
    object TBParagraph: TToolButton
      Left = 23
      Top = 264
      Hint = 'Paragraph on/off'
      ImageIndex = 25
      ImageName = 'Paragraph'
      Wrap = True
      OnClick = SBParagraphClick
    end
    object TBNumbers: TToolButton
      Left = 0
      Top = 286
      Hint = 'Line numbers on/off'
      ImageIndex = 26
      ImageName = 'Numbers'
      OnClick = SBNumbersClick
    end
    object TBZoomOut: TToolButton
      Left = 23
      Top = 286
      Hint = 'Zoom out'
      ImageIndex = 27
      ImageName = 'ZoomOut'
      Wrap = True
      OnClick = SBZoomOutClick
    end
    object TBZoomIn: TToolButton
      Left = 0
      Top = 308
      Hint = 'Zoom in'
      ImageIndex = 28
      ImageName = 'ZoomIn'
      OnClick = SBZoomInClick
    end
    object TBValidate: TToolButton
      Left = 23
      Top = 308
      Hint = 'Validate'
      ImageIndex = 29
      ImageName = 'Validate'
      OnClick = SBValidateClick
    end
  end
  object PMain: TPanel
    Left = 46
    Top = 0
    Width = 641
    Height = 525
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
  end
  object ActivityIndicator: TActivityIndicator
    Left = 480
    Top = 32
    Anchors = [akRight]
    FrameDelay = 150
    IndicatorType = aitRotatingSector
  end
  object icEditor: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Explorer'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#c69633" d="M2 1h2M1' +
          ' 2h1M0 3h1M0 4h1M12 9h1" />'#13#10'<path stroke="#c68e33" d="M4 1h1M0 ' +
          '5h1" />'#13#10'<path stroke="#be8e2c" d="M5 1h1M0 6h1M0 7h1M1 9h1" />'#13 +
          #10'<path stroke="#ffffff" d="M2 2h4M1 3h1M6 3h1M9 4h3M10 6h2M4 8h2' +
          '" />'#13#10'<path stroke="#be862c" d="M6 2h1M0 8h1M0 9h1M1 10h1" />'#13#10'<' +
          'path stroke="#a6a69e" d="M7 2h1" />'#13#10'<path stroke="#ffff96" d="M' +
          '2 3h4M10 7h2" />'#13#10'<path stroke="#b68625" d="M7 3h1M10 5h1M2 7h1M' +
          '0 10h1" />'#13#10'<path stroke="#b67e25" d="M8 3h1M11 5h1" />'#13#10'<path s' +
          'troke="#ae7e1e" d="M9 3h3M12 5h2M13 9h1M0 11h1M1 12h2" />'#13#10'<path' +
          ' stroke="#ae771e" d="M12 3h1M3 12h1" />'#13#10'<path stroke="#feef8e" ' +
          'd="M1 4h2M10 8h2" />'#13#10'<path stroke="#efe78e" d="M3 4h1" />'#13#10'<pat' +
          'h stroke="#b6b6ae" d="M4 4h1" />'#13#10'<path stroke="#a6c6df" d="M5 4' +
          'h2" />'#13#10'<path stroke="#b6bed6" d="M7 4h1" />'#13#10'<path stroke="#ced' +
          '6e7" d="M8 4h1M8 5h1" />'#13#10'<path stroke="#a67717" d="M12 4h1M13 8' +
          'h1M12 10h1M4 12h3" />'#13#10'<path stroke="#585858" d="M13 4h1M10 13h1' +
          'M12 14h1" />'#13#10'<path stroke="#fee77e" d="M1 5h1M11 9h1" />'#13#10'<path' +
          ' stroke="#fece67" d="M2 5h1M1 7h1M4 11h4" />'#13#10'<path stroke="#8e8' +
          '67e" d="M3 5h1" />'#13#10'<path stroke="#aee7ff" d="M4 5h2M8 8h1" />'#13#10 +
          '<path stroke="#beefff" d="M6 5h1M6 6h1M7 8h1M7 9h1" />'#13#10'<path st' +
          'roke="#e7ffff" d="M7 5h1M5 7h1M5 9h1" />'#13#10'<path stroke="#ae8650"' +
          ' d="M9 5h1" />'#13#10'<path stroke="#966700" d="M14 5h1" />'#13#10'<path str' +
          'oke="#b6b6a6" d="M15 5h1M7 10h1M14 13h1" />'#13#10'<path stroke="#fedf' +
          '77" d="M1 6h1M12 6h1M10 9h1" />'#13#10'<path stroke="#cea642" d="M2 6h' +
          '1M13 7h1M1 11h1" />'#13#10'<path stroke="#9ebed6" d="M3 6h1" />'#13#10'<path' +
          ' stroke="#ceefff" d="M4 6h1M8 6h1" />'#13#10'<path stroke="#c6ffff" d=' +
          '"M5 6h1" />'#13#10'<path stroke="#b6e7ff" d="M7 6h1M7 7h2" />'#13#10'<path s' +
          'troke="#bec6e7" d="M9 6h1" />'#13#10'<path stroke="#e7d677" d="M13 6h1' +
          '" />'#13#10'<path stroke="#8e6000" d="M14 6h1" />'#13#10'<path stroke="#4949' +
          '42" d="M15 6h1M14 10h1M3 13h7M13 13h1" />'#13#10'<path stroke="#aecedf' +
          '" d="M3 7h1" />'#13#10'<path stroke="#dfffff" d="M4 7h1M6 8h1" />'#13#10'<pa' +
          'th stroke="#d7ffff" d="M6 7h1M6 9h1" />'#13#10'<path stroke="#b6ced6" ' +
          'd="M9 7h1" />'#13#10'<path stroke="#febe60" d="M12 7h1" />'#13#10'<path stro' +
          'ke="#775817" d="M14 7h1" />'#13#10'<path stroke="#42423a" d="M15 7h1M1' +
          '4 9h1M13 12h1" />'#13#10'<path stroke="#dfae49" d="M1 8h1" />'#13#10'<path s' +
          'troke="#ce963a" d="M2 8h1" />'#13#10'<path stroke="#aec6d6" d="M3 8h1"' +
          ' />'#13#10'<path stroke="#c6c6ae" d="M9 8h1" />'#13#10'<path stroke="#e7b650' +
          '" d="M12 8h1" />'#13#10'<path stroke="#50422c" d="M14 8h1" />'#13#10'<path s' +
          'troke="#605858" d="M15 8h1M13 14h1" />'#13#10'<path stroke="#efc67e" d' +
          '="M2 9h1" />'#13#10'<path stroke="#c6bec6" d="M3 9h1" />'#13#10'<path stroke' +
          '="#dfe7ef" d="M4 9h1" />'#13#10'<path stroke="#96aed6" d="M8 9h1" />'#13#10 +
          '<path stroke="#d6be8e" d="M9 9h1M4 10h1" />'#13#10'<path stroke="#968e' +
          '8e" d="M15 9h1" />'#13#10'<path stroke="#ffffce" d="M2 10h1M2 11h1" />' +
          #13#10'<path stroke="#fee7a6" d="M3 10h1" />'#13#10'<path stroke="#bec6c6" ' +
          'd="M5 10h1" />'#13#10'<path stroke="#aebebe" d="M6 10h1" />'#13#10'<path str' +
          'oke="#ae9e8e" d="M8 10h1" />'#13#10'<path stroke="#ce8e58" d="M9 10h1"' +
          ' />'#13#10'<path stroke="#feb650" d="M10 10h1M11 11h1" />'#13#10'<path strok' +
          'e="#fed66f" d="M11 10h1" />'#13#10'<path stroke="#7e5810" d="M13 10h1"' +
          ' />'#13#10'<path stroke="#fece77" d="M3 11h1" />'#13#10'<path stroke="#e7c67' +
          '7" d="M8 11h1" />'#13#10'<path stroke="#a6868e" d="M9 11h1" />'#13#10'<path ' +
          'stroke="#d68649" d="M10 11h1M11 12h1" />'#13#10'<path stroke="#9e6f10"' +
          ' d="M12 11h1M7 12h2" />'#13#10'<path stroke="#584925" d="M13 11h1" />'#13 +
          #10'<path stroke="#676060" d="M14 11h1" />'#13#10'<path stroke="#b69e6f" ' +
          'd="M0 12h1" />'#13#10'<path stroke="#96773a" d="M9 12h1" />'#13#10'<path str' +
          'oke="#9e7e86" d="M10 12h1" />'#13#10'<path stroke="#d69633" d="M12 12h' +
          '1" />'#13#10'<path stroke="#8e8e86" d="M14 12h1" />'#13#10'<path stroke="#86' +
          '7e7e" d="M1 13h1" />'#13#10'<path stroke="#505050" d="M2 13h1" />'#13#10'<pa' +
          'th stroke="#967786" d="M11 13h1" />'#13#10'<path stroke="#a67e60" d="M' +
          '12 13h1" />'#13#10'<path stroke="#96968e" d="M11 14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Browser'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#7a7a7a" d="M5 0h5M3' +
          ' 1h2M2 2h1M1 3h1M6 3h7M1 4h1M13 4h1M0 5h1M14 5h1M0 6h1M0 7h1M0 8' +
          'h1M0 9h1M0 10h1" />'#13#10'<path stroke="#bbbbbb" d="M5 1h1M4 2h1M3 3h' +
          '1M2 4h1M6 4h1M12 4h1M1 5h1M3 5h1M6 5h1M12 5h1M6 6h1M1 7h1M3 7h1M' +
          '5 7h2M12 7h1M14 7h1M2 8h1M4 8h1M6 8h1M10 8h1M14 8h1M2 9h1M6 9h1M' +
          '9 9h1M14 9h1M5 10h2M14 10h1M6 11h1M12 11h1M14 11h1M6 12h1M14 12h' +
          '1M6 13h1M14 13h1M6 14h9" />'#13#10'<path stroke="#ffffff" d="M6 1h1M3 ' +
          '2h1M5 2h1M2 3h1M3 4h1M7 4h3M11 4h1M2 5h1M4 5h1M7 5h1M9 5h3M13 5h' +
          '1M1 6h3M7 6h3M11 6h1M2 7h1M4 7h1M7 7h1M13 7h1M3 8h1M7 8h1M9 8h1M' +
          '13 8h1M7 9h1M13 9h1M7 10h1M13 10h1M7 11h1M13 11h1M7 12h3M11 12h3' +
          'M7 13h1M9 13h3M13 13h1" />'#13#10'<path stroke="#0000ff" d="M7 1h1M10 ' +
          '1h2M6 2h1M8 2h1M11 2h1M5 5h1M4 6h1M3 9h1M11 9h1M2 10h3M9 10h3M1 ' +
          '11h1M3 11h3M2 12h4M3 13h2" />'#13#10'<path stroke="#077a7a" d="M8 1h2M' +
          '4 3h1M9 7h3M1 8h1M8 8h1M1 9h1M4 9h2M8 9h1M8 10h1" />'#13#10'<path stro' +
          'ke="#077a00" d="M7 2h1M9 2h2M5 3h1M4 4h2M5 6h1M5 8h1M11 8h1M10 9' +
          'h1M1 10h1M2 11h1" />'#13#10'<path stroke="#00007a" d="M12 2h1M12 8h1M1' +
          '2 9h1M12 10h1M9 11h3M1 12h1M2 13h1M5 13h1M3 14h3" />'#13#10'<path stro' +
          'ke="#000000" d="M13 3h1M12 6h4M15 7h1M15 8h1M15 9h1M15 10h1M15 1' +
          '1h1M15 12h1M15 13h1M15 14h1M5 15h11" />'#13#10'<path stroke="#ffff00" ' +
          'd="M10 4h1M8 5h1M10 6h1M8 7h1M8 11h1M10 12h1M8 13h1M12 13h1" />'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Designform'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M2 2h3M1' +
          '1 2h3M2 3h3M11 3h3M2 4h3M11 4h3M2 12h3M11 12h3M2 13h3M11 13h3M2 ' +
          '14h3M11 14h3" />'#13#10'<path stroke="#7e7e7e" d="M5 3h6M3 5h1M12 5h1M' +
          '3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1M3 9h1M12 9h1M3 10h1M12 10' +
          'h1M3 11h1M12 11h1M5 13h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Classedit'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#05529a" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5' +
          'h1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3' +
          'M12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#13#10'<path ' +
          'stroke="#468dcd" d="M5 4h1M5 11h1" />'#13#10'<path stroke="#a0c5e6" d=' +
          '"M6 4h1" />'#13#10'<path stroke="#f2f7fb" d="M7 4h1" />'#13#10'<path stroke=' +
          '"#e8f0f8" d="M8 4h1M10 5h1M10 10h1M8 11h1" />'#13#10'<path stroke="#cd' +
          'e1f1" d="M9 4h1M9 11h1" />'#13#10'<path stroke="#5497d1" d="M10 4h1M10' +
          ' 11h1" />'#13#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#13#10'<path st' +
          'roke="#b2d0ea" d="M6 5h1M6 10h1" />'#13#10'<path stroke="#488fce" d="M' +
          '7 5h1M7 10h1" />'#13#10'<path stroke="#73aada" d="M9 5h1" />'#13#10'<path st' +
          'roke="#438bcc" d="M11 5h1" />'#13#10'<path stroke="#609ed4" d="M4 6h1"' +
          ' />'#13#10'<path stroke="#f8fafc" d="M5 6h1M5 9h1M7 11h1" />'#13#10'<path st' +
          'roke="#4990ce" d="M6 6h1M6 9h1M11 10h1" />'#13#10'<path stroke="#70a8d' +
          'a" d="M10 6h1" />'#13#10'<path stroke="#5094d0" d="M11 6h1" />'#13#10'<path ' +
          'stroke="#7eb1dd" d="M4 7h1" />'#13#10'<path stroke="#d0e3f2" d="M5 7h1' +
          'M5 8h1" />'#13#10'<path stroke="#77acdb" d="M4 8h1" />'#13#10'<path stroke="' +
          '#5d9cd4" d="M4 9h1" />'#13#10'<path stroke="#94bee3" d="M10 9h1" />'#13#10'<' +
          'path stroke="#6da6d8" d="M11 9h1M9 10h1" />'#13#10'<path stroke="#aacb' +
          'e9" d="M6 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ClassOpen'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'<path stroke="#ffffff" d="M2 2h11M2 3h11M2' +
          ' 4h11M2 6h11M2 7h11M2 8h11M2 10h11M2 11h11M2 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'MatchBracket'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#9d9d9d" d="M4 2h1M1' +
          '1 2h1M5 3h1M10 3h1M4 6h1M11 6h1M3 7h1M2 8h1M4 8h1M11 8h1M13 8h1M' +
          '5 12h1M10 12h1M12 12h1M4 13h1M11 13h1" />'#13#10'<path stroke="#000000' +
          '" d="M5 2h2M9 2h2M4 3h1M11 3h1M3 4h2M11 4h2M3 5h2M11 5h2M3 6h1M1' +
          '2 6h1M2 7h1M13 7h1M3 8h1M12 8h1M3 9h2M11 9h2M3 10h2M11 10h2M3 11' +
          'h2M11 11h2M4 12h1M11 12h1M5 13h2M9 13h1" />'#13#10'<path stroke="#7e7e' +
          '7e" d="M3 3h1M12 3h1M12 7h1M3 12h1M10 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SystemOutPrintln'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000082" d="M3 1h1M4' +
          ' 2h1M3 3h1" />'#13#10'<path stroke="#828282" d="M3 2h1M4 3h1M3 4h1" />' +
          #13#10'<path stroke="#000000" d="M5 3h1M4 4h1M3 5h1M5 5h6M5 6h1M10 6h' +
          '2M5 7h1M7 7h2M10 7h3M5 8h1M12 8h1M5 9h1M7 9h4M12 9h1M5 10h1M12 1' +
          '0h1M5 11h1M12 11h1M5 12h1M12 12h1M5 13h1M12 13h1M5 14h8" />'#13#10'<pa' +
          'th stroke="#ffffff" d="M6 6h4M6 7h1M9 7h1M6 8h6M6 9h1M11 9h1M7 1' +
          '1h4M6 13h6" />'#13#10'<path stroke="#fe0000" d="M6 10h6M6 11h1M11 11h1' +
          'M6 12h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'StructureIndent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#22ff37" d="M1 1h14M' +
          '1 2h14M1 3h2M1 4h2M1 5h2M1 10h2M1 11h2M1 12h2M1 13h14M1 14h14" /' +
          '>'#13#10'<path stroke="#ffff13" d="M3 3h12M3 4h12M5 4h10M3 5h1M3 10h1M' +
          '3 11h12M5 11h10M3 12h12" />'#13#10'<path stroke="#fe6272" d="M5 5h10 M' +
          '5 6h10 M5 9h10 M5 10h10" />'#13#10'<path stroke="#ffffff" d="M7 7h8M7 ' +
          '8h8" />'#13#10'<polygon points="1,5.5 4,5.5 4,3.5 8,7.5 4,11.5 4,9.5 1' +
          ',9.5 z" style="fill:#0a92ff" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'IfStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14  M10.1 5.5 v8' +
          '.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'
      end
      item
        IconName = 'IfElseStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'
      end
      item
        IconName = 'WhileStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11" />'#13#10'</svg>'
      end
      item
        IconName = 'ForStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11 M9 7 v1 M9 9 v3.5" />'#13#10'</svg>'
      end
      item
        IconName = 'DoWhileStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 12.25 h11.5 M3.75 ' +
          '1 v11" />'#13#10'</svg>'
      end
      item
        IconName = 'SwitchStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M5.8 5.5 v8.5' +
          ' M10.1 5.5 v8.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'
      end
      item
        IconName = 'TryStatement'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M1 1h14M' +
          '1 2h1M14 2h1M1 3h1M14 3h1M1 4h1M4 4h1M14 4h1M1 5h1M4 5h1M14 5h1M' +
          '1 6h1M4 6h1M14 6h1M1 7h1M4 7h1M14 7h1M1 8h1M4 8h1M7 8h1M14 8h1M1' +
          ' 9h1M4 9h1M7 9h1M14 9h1M1 10h1M7 10h1M14 10h1M1 11h1M12 11h1M14 ' +
          '11h1M1 12h1M14 12h1M1 13h1M14 13h1M1 14h14" />'#13#10'<path stroke="#2' +
          '2001f" d="M3 5h1" />'#13#10'<path stroke="#160016" d="M5 5h1" />'#13#10'<pat' +
          'h stroke="#170015" d="M7 7h1" />'#13#10'<path stroke="#1a0018" d="M8 7' +
          'h1" />'#13#10'<path stroke="#160014" d="M9 7h1" />'#13#10'<path stroke="#280' +
          '024" d="M11 7h1" />'#13#10'<path stroke="#250021" d="M13 7h1" />'#13#10'<pat' +
          'h stroke="#410039" d="M9 8h1" />'#13#10'<path stroke="#2c0027" d="M11 ' +
          '8h1" />'#13#10'<path stroke="#2e0029" d="M13 8h1" />'#13#10'<path stroke="#2' +
          '0001d" d="M11 9h1" />'#13#10'<path stroke="#22001e" d="M12 9h1" />'#13#10'<p' +
          'ath stroke="#150012" d="M4 10h1" />'#13#10'<path stroke="#180016" d="M' +
          '5 10h1" />'#13#10'<path stroke="#24001f" d="M12 10h1M11 12h1" />'#13#10'<pat' +
          'h stroke="#240020" d="M10 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'BlockStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 " />'#13#10'</svg>'
      end
      item
        IconName = 'Comment'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#001be1" stroke-widt' +
          'h="1.5" d="M3 12 l5 -11 M8 12 l5 -11 " />'#13#10'</svg>'
      end
      item
        IconName = 'Indent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#00007e" d="M2 4h1M2 5h2M0 6h5M2 7h2M2 ' +
          '8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Unindent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#00007e" d="M2 4h1M1 5h2M0 6h5M1 7h2M2 ' +
          '8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'WordWrap'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#00007e" d="M6 3h7M1' +
          '3 4h1M13 5h1M10 6h1M13 6h1M9 7h2M13 7h1M8 8h5M9 9h2M10 10h1" />'#13 +
          #10'<path stroke="#000000" d="M1 5h6M1 7h4M1 9h4M1 11h6" />'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'Breakpoint'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h2M12 5h1M4 6h2M9 6h3M3 8h2M10 8h3M10 9h1M1' +
          '0 10h1M4 11h2M10 11h2M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h' +
          '1" />'#13#10'<path stroke="#088400" d="M7 2h2M8 3h1M3 4h1M7 4h1M12 4h1' +
          'M4 5h1M6 5h2M11 5h1M6 6h1M5 7h1M8 7h2M5 8h1M8 8h2M2 9h2M5 9h1M8 ' +
          '9h2M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 12h1M2 13h1M6 1' +
          '3h1M13 13h1" />'#13#10'<path stroke="#820000" d="M6 3h1M9 3h1M6 4h1M9 ' +
          '4h1" />'#13#10'<path stroke="#11ff00" d="M7 3h1M7 6h2M6 7h2M6 8h2M6 9h' +
          '2M6 10h1M6 11h1" />'#13#10'<path stroke="#fe0000" d="M5 4h1M10 4h1" />' +
          #13#10'<path stroke="#c4c5c4" d="M5 5h1M10 5h1" />'#13#10'<path stroke="#08' +
          '8482" d="M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'BreakpointsClear'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h1M12 5h1M4 6h1M10 6h2M3 8h2M10 8h3M4 11h2M' +
          '10 11h1M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h1" />'#13#10'<path s' +
          'troke="#088400" d="M7 2h2M8 3h1M7 4h1M12 4h1M6 5h2M11 5h1M5 7h1M' +
          '9 7h1M2 9h2M8 9h1M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 1' +
          '2h1M2 13h1M6 13h1M13 13h1" />'#13#10'<path stroke="#fe000e" d="M3 3h1M' +
          '11 3h1M3 4h2M10 4h2M4 5h2M9 5h2M5 6h2M8 6h2M6 7h3M5 8h2M8 8h2M4 ' +
          '9h2M9 9h2M3 10h2M10 10h2M3 11h1M11 11h1" />'#13#10'<path stroke="#8200' +
          '00" d="M6 3h1M9 3h1M6 4h1M9 4h1" />'#13#10'<path stroke="#11ff00" d="M' +
          '7 3h1M7 6h1M7 8h1M6 9h2M6 10h1M6 11h1" />'#13#10'<path stroke="#fe0000' +
          '" d="M5 4h1" />'#13#10'<path stroke="#088482" d="M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#bebebe" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#7e7e7e" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#087e00" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M7 5h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M9 7h1M4 8h1M' +
          '6 8h1M8 8h1M10 8h1M5 9h1M7 9h1M9 9h1M4 10h1M6 10h1M8 10h1M10 10h' +
          '1M5 11h1M7 11h1M9 11h1M4 12h1M6 12h1M8 12h1M10 12h1M5 13h1M7 13h' +
          '1M9 13h1" />'#13#10'<path stroke="#087e7e" d="M5 4h1M7 4h1M9 4h1M4 5h1' +
          'M6 5h1M8 5h1M10 5h1M5 6h1M7 6h1M9 6h1M4 7h1M6 7h1M8 7h1M10 7h1M5' +
          ' 8h1M7 8h1M9 8h1M4 9h1M6 9h1M8 9h1M10 9h1M5 10h1M7 10h1M9 10h1M4' +
          ' 11h1M6 11h1M8 11h1M10 11h1M5 12h1M7 12h1M9 12h1M4 13h1M6 13h1M8' +
          ' 13h1M10 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'GotoBookmark'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#bebebe" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#7e7e7e" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#087e00" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M10 6h1M5 7h1M9 7h1M4 8h1M10 8h1M5 9h1M9 9h1M4 10h' +
          '1M10 10h1M5 11h1M9 11h1M4 12h1M6 12h1M8 12h1M10 12h1M5 13h1M7 13' +
          'h1M9 13h1" />'#13#10'<path stroke="#087e7e" d="M5 4h1M7 4h1M9 4h1M4 5h' +
          '1M10 5h1M5 6h1M9 6h1M4 7h1M10 7h1M5 8h1M9 8h1M4 9h1M10 9h1M4 11h' +
          '1M10 11h1M5 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path ' +
          'stroke="#000000" d="M6 5h3M6 6h1M8 6h1M6 7h1M8 7h1M6 8h1M8 8h1M6' +
          ' 9h1M8 9h1M5 10h2M8 10h2M6 11h3M7 12h1" />'#13#10'<path stroke="#fffff' +
          'f" d="M7 6h1M7 7h1M7 8h1M7 9h1M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Paragraph'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#001be1" d="M5 4h6M4' +
          ' 5h1M7 5h1M9 5h1M4 6h1M7 6h1M9 6h1M5 7h3M9 7h1M7 8h1M9 8h1M7 9h1' +
          'M9 9h1M7 10h1M9 10h1M6 11h2M9 11h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Numbers'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#001be1" d="M4 5h7 M' +
          '4 8h7 M5 11 L7 2 M8 11 L10 2 " />'#13#10'</svg>'
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#191919">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'ZoomIn'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#191919">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Zm-40-60v-' +
          '80h-80v-80h80v-80h80v80h80v80h-80v80h-80Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Validate'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#04457e" d="M1 0h4M1' +
          '1 0h4M0 1h5M11 1h5M0 2h4M12 2h4M15 3h1M0 9h2M14 9h2M0 10h2M14 10' +
          'h2M0 11h1M15 11h1M0 12h1M15 12h1M0 13h2M14 13h2M1 14h2M7 14h2M13' +
          ' 14h2" />'#13#10'<path stroke="#235584" d="M5 0h1" />'#13#10'<path stroke="#' +
          'c8d2db" d="M6 0h1" />'#13#10'<path stroke="#c9d3dc" d="M7 0h1" />'#13#10'<pa' +
          'th stroke="#cad4dd" d="M8 0h1" />'#13#10'<path stroke="#cbd5de" d="M9 ' +
          '0h1" />'#13#10'<path stroke="#2d5d89" d="M10 0h1" />'#13#10'<path stroke="#6' +
          'e8daa" d="M5 1h1" />'#13#10'<path stroke="#f6eec3" d="M6 1h1" />'#13#10'<pat' +
          'h stroke="#f4dc64" d="M7 1h2" />'#13#10'<path stroke="#eedbb4" d="M9 1' +
          'h1" />'#13#10'<path stroke="#7795b0" d="M10 1h1" />'#13#10'<path stroke="#04' +
          '437b" d="M4 2h1M11 2h1" />'#13#10'<path stroke="#becad4" d="M5 2h1" />' +
          #13#10'<path stroke="#f3df7b" d="M6 2h1" />'#13#10'<path stroke="#f2d23a" d' +
          '="M7 2h1M7 3h1M6 4h2" />'#13#10'<path stroke="#e9c131" d="M8 2h1" />'#13#10 +
          '<path stroke="#d3a056" d="M9 2h1" />'#13#10'<path stroke="#c3cfda" d="' +
          'M10 2h1" />'#13#10'<path stroke="#04447d" d="M0 3h1M15 8h1M14 11h1" />' +
          #13#10'<path stroke="#5a7e9d" d="M1 3h1M0 5h1" />'#13#10'<path stroke="#6f8' +
          'da8" d="M2 3h1" />'#13#10'<path stroke="#6f8fac" d="M3 3h1" />'#13#10'<path ' +
          'stroke="#7793ad" d="M4 3h1" />'#13#10'<path stroke="#f2f1ed" d="M5 3h1' +
          '" />'#13#10'<path stroke="#f2d444" d="M6 3h1" />'#13#10'<path stroke="#e7c35' +
          'a" d="M8 3h1" />'#13#10'<path stroke="#dbb47a" d="M9 3h1" />'#13#10'<path st' +
          'roke="#f7f7f8" d="M10 3h1" />'#13#10'<path stroke="#7996b0" d="M11 3h1' +
          '" />'#13#10'<path stroke="#7494b0" d="M12 3h1" />'#13#10'<path stroke="#7594' +
          'b1" d="M13 3h1" />'#13#10'<path stroke="#537b9f" d="M14 3h1" />'#13#10'<path' +
          ' stroke="#184c7e" d="M0 4h1" />'#13#10'<path stroke="#e6e9e8" d="M1 4h' +
          '1" />'#13#10'<path stroke="#84bb71" d="M2 4h1" />'#13#10'<path stroke="#7baa' +
          '72" d="M3 4h1" />'#13#10'<path stroke="#b8cfb4" d="M4 4h1" />'#13#10'<path s' +
          'troke="#f2e9b8" d="M5 4h1" />'#13#10'<path stroke="#f5efd0" d="M8 4h1"' +
          ' />'#13#10'<path stroke="#e2acaa" d="M9 4h1" />'#13#10'<path stroke="#dd9b99' +
          '" d="M10 4h1" />'#13#10'<path stroke="#dd9c99" d="M11 4h1" />'#13#10'<path s' +
          'troke="#de9d9a" d="M12 4h1" />'#13#10'<path stroke="#e3a7a4" d="M13 4h' +
          '1" />'#13#10'<path stroke="#e8ecf0" d="M14 4h1" />'#13#10'<path stroke="#0e4' +
          '57a" d="M15 4h1" />'#13#10'<path stroke="#c0e1b2" d="M1 5h1" />'#13#10'<path' +
          ' stroke="#3eb800" d="M2 5h1" />'#13#10'<path stroke="#2c9400" d="M3 5h' +
          '1" />'#13#10'<path stroke="#b4ceae" d="M4 5h1" />'#13#10'<path stroke="#f1e5' +
          'a3" d="M5 5h1" />'#13#10'<path stroke="#f2d23b" d="M6 5h1" />'#13#10'<path s' +
          'troke="#f2d752" d="M7 5h1" />'#13#10'<path stroke="#f1e7e6" d="M8 5h1"' +
          ' />'#13#10'<path stroke="#ba2722" d="M9 5h1" />'#13#10'<path stroke="#b8231d' +
          '" d="M10 5h3" />'#13#10'<path stroke="#b8241e" d="M13 5h1" />'#13#10'<path s' +
          'troke="#f2dcdc" d="M14 5h1" />'#13#10'<path stroke="#457096" d="M15 5h' +
          '1" />'#13#10'<path stroke="#a6b6c4" d="M0 6h1" />'#13#10'<path stroke="#8cd1' +
          '6f" d="M1 6h1" />'#13#10'<path stroke="#3eb900" d="M2 6h2M4 7h1" />'#13#10'<' +
          'path stroke="#5ac22d" d="M4 6h1" />'#13#10'<path stroke="#cde7c3" d="M' +
          '5 6h1" />'#13#10'<path stroke="#f1e9b9" d="M6 6h1" />'#13#10'<path stroke="#' +
          'f2e59e" d="M7 6h1" />'#13#10'<path stroke="#e0adab" d="M8 6h1" />'#13#10'<pa' +
          'th stroke="#bd342e" d="M9 6h2" />'#13#10'<path stroke="#be342e" d="M11' +
          ' 6h1" />'#13#10'<path stroke="#b1221d" d="M12 6h1" />'#13#10'<path stroke="#' +
          'af201a" d="M13 6h1" />'#13#10'<path stroke="#d7928f" d="M14 6h1" />'#13#10'<' +
          'path stroke="#96adc1" d="M15 6h1" />'#13#10'<path stroke="#6b8aa6" d="' +
          'M0 7h1" />'#13#10'<path stroke="#dee5df" d="M1 7h1" />'#13#10'<path stroke="' +
          '#82cd60" d="M2 7h1" />'#13#10'<path stroke="#3fb906" d="M3 7h1" />'#13#10'<p' +
          'ath stroke="#42bb0f" d="M5 7h1" />'#13#10'<path stroke="#9ed784" d="M6' +
          ' 7h1" />'#13#10'<path stroke="#f1f2f2" d="M7 7h1" />'#13#10'<path stroke="#f' +
          '1f0f3" d="M8 7h1" />'#13#10'<path stroke="#ebe3f0" d="M9 7h1" />'#13#10'<pat' +
          'h stroke="#ece4f1" d="M10 7h1" />'#13#10'<path stroke="#f3f0f5" d="M11' +
          ' 7h1" />'#13#10'<path stroke="#944c4b" d="M12 7h1" />'#13#10'<path stroke="#' +
          '9b5656" d="M13 7h1" />'#13#10'<path stroke="#ede5e5" d="M14 7h1" />'#13#10'<' +
          'path stroke="#839eb6" d="M15 7h1" />'#13#10'<path stroke="#04447e" d="' +
          'M0 8h1" />'#13#10'<path stroke="#34618a" d="M1 8h1" />'#13#10'<path stroke="' +
          '#c7cfd7" d="M2 8h1" />'#13#10'<path stroke="#b3dda2" d="M3 8h1" />'#13#10'<p' +
          'ath stroke="#4bbd19" d="M4 8h1" />'#13#10'<path stroke="#48bc16" d="M5' +
          ' 8h1" />'#13#10'<path stroke="#aedd9b" d="M6 8h1" />'#13#10'<path stroke="#d' +
          '0dee9" d="M7 8h1" />'#13#10'<path stroke="#ebe9f0" d="M8 8h1" />'#13#10'<pat' +
          'h stroke="#7f15bc" d="M9 8h1M11 12h1" />'#13#10'<path stroke="#7900b9"' +
          ' d="M10 8h1M10 9h1M10 10h2M11 11h1" />'#13#10'<path stroke="#b272d5" d' +
          '="M11 8h1" />'#13#10'<path stroke="#ebe3e3" d="M12 8h1" />'#13#10'<path stro' +
          'ke="#dae0e6" d="M13 8h1" />'#13#10'<path stroke="#446e94" d="M14 8h1" ' +
          '/>'#13#10'<path stroke="#39638a" d="M2 9h1" />'#13#10'<path stroke="#e3e6e8"' +
          ' d="M3 9h1" />'#13#10'<path stroke="#d8e6d4" d="M4 9h1" />'#13#10'<path stro' +
          'ke="#d6e7d0" d="M5 9h1" />'#13#10'<path stroke="#9abcdb" d="M6 9h1" />' +
          #13#10'<path stroke="#2a77bc" d="M7 9h1" />'#13#10'<path stroke="#b7cee3" d' +
          '="M8 9h1" />'#13#10'<path stroke="#a357cd" d="M9 9h1" />'#13#10'<path stroke' +
          '="#8927c1" d="M11 9h1" />'#13#10'<path stroke="#f1f1f3" d="M12 9h1" />' +
          #13#10'<path stroke="#20507e" d="M13 9h1" />'#13#10'<path stroke="#829ab1" ' +
          'd="M2 10h1" />'#13#10'<path stroke="#86a7b8" d="M3 10h1" />'#13#10'<path str' +
          'oke="#457b99" d="M4 10h1" />'#13#10'<path stroke="#5e97ca" d="M5 10h1"' +
          ' />'#13#10'<path stroke="#2271ba" d="M6 10h2M5 11h2" />'#13#10'<path stroke=' +
          '"#73a5d0" d="M8 10h1" />'#13#10'<path stroke="#caa5e0" d="M9 10h1" />'#13 +
          #10'<path stroke="#dbc3e9" d="M12 10h1" />'#13#10'<path stroke="#52789b" ' +
          'd="M13 10h1" />'#13#10'<path stroke="#0c4479" d="M1 11h1" />'#13#10'<path st' +
          'roke="#cfd5db" d="M2 11h1" />'#13#10'<path stroke="#3d789f" d="M3 11h1' +
          '" />'#13#10'<path stroke="#1e6cb1" d="M4 11h1" />'#13#10'<path stroke="#4e8d' +
          'c6" d="M7 11h1" />'#13#10'<path stroke="#d2dee8" d="M8 11h1" />'#13#10'<path' +
          ' stroke="#a78cbb" d="M9 11h1" />'#13#10'<path stroke="#7400b3" d="M10 ' +
          '11h1" />'#13#10'<path stroke="#b172d4" d="M12 11h1" />'#13#10'<path stroke="' +
          '#a0b3c3" d="M13 11h1" />'#13#10'<path stroke="#205382" d="M1 12h1" />'#13 +
          #10'<path stroke="#d4d8de" d="M2 12h1" />'#13#10'<path stroke="#8bb2d3" d' +
          '="M3 12h1" />'#13#10'<path stroke="#2775bb" d="M4 12h1" />'#13#10'<path stro' +
          'ke="#2573bb" d="M5 12h1" />'#13#10'<path stroke="#84aed3" d="M6 12h1" ' +
          '/>'#13#10'<path stroke="#dfe3e6" d="M7 12h1" />'#13#10'<path stroke="#e3e1e7' +
          '" d="M8 12h1" />'#13#10'<path stroke="#744a92" d="M9 12h1" />'#13#10'<path s' +
          'troke="#62009a" d="M10 12h1" />'#13#10'<path stroke="#c79edf" d="M12 1' +
          '2h1" />'#13#10'<path stroke="#cad3dc" d="M13 12h1" />'#13#10'<path stroke="#' +
          '07437b" d="M14 12h1" />'#13#10'<path stroke="#205180" d="M2 13h1" />'#13#10 +
          '<path stroke="#a9b8c5" d="M3 13h1" />'#13#10'<path stroke="#c0d1e0" d=' +
          '"M4 13h1" />'#13#10'<path stroke="#bacee0" d="M5 13h1" />'#13#10'<path strok' +
          'e="#b3c0cc" d="M6 13h1" />'#13#10'<path stroke="#255582" d="M7 13h1" /' +
          '>'#13#10'<path stroke="#456e92" d="M8 13h1" />'#13#10'<path stroke="#d7dce1"' +
          ' d="M9 13h1" />'#13#10'<path stroke="#b38fc9" d="M10 13h1" />'#13#10'<path s' +
          'troke="#dfcee9" d="M11 13h1" />'#13#10'<path stroke="#9fb2c2" d="M12 1' +
          '3h1" />'#13#10'<path stroke="#194e7f" d="M13 13h1" />'#13#10'<path stroke="#' +
          '09447b" d="M3 14h1" />'#13#10'<path stroke="#6988a4" d="M4 14h1" />'#13#10'<' +
          'path stroke="#738fa9" d="M5 14h1" />'#13#10'<path stroke="#0c457b" d="' +
          'M6 14h1" />'#13#10'<path stroke="#205281" d="M9 14h1" />'#13#10'<path stroke' +
          '="#a7b7c6" d="M10 14h1" />'#13#10'<path stroke="#5b7e9e" d="M11 14h1" ' +
          '/>'#13#10'<path stroke="#07447c" d="M12 14h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#ffffff">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Designform'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M2 2h3M1' +
          '1 2h3M2 3h3M11 3h3M2 4h3M11 4h3M2 12h3M11 12h3M2 13h3M11 13h3M2 ' +
          '14h3M11 14h3" />'#13#10'<path stroke="#7e7e7e" d="M5 3h6M3 5h1M12 5h1M' +
          '3 6h1M12 6h1M3 7h1M12 7h1M3 8h1M12 8h1M3 9h1M12 9h1M3 10h1M12 10' +
          'h1M3 11h1M12 11h1M5 13h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ClassEdit'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5' +
          'h1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3' +
          'M12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#13#10'<path ' +
          'stroke="#468dcd" d="M5 4h1M5 11h1" />'#13#10'<path stroke="#a0c5e6" d=' +
          '"M6 4h1" />'#13#10'<path stroke="#f2f7fb" d="M7 4h1" />'#13#10'<path stroke=' +
          '"#cde1f1" d="M8 4h2M8 11h2" />'#13#10'<path stroke="#5497d1" d="M10 4h' +
          '1M10 11h1" />'#13#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#13#10'<pat' +
          'h stroke="#b2d0ea" d="M6 5h1M6 10h1" />'#13#10'<path stroke="#488fce" ' +
          'd="M7 5h1M7 10h1" />'#13#10'<path stroke="#73aada" d="M9 5h1" />'#13#10'<pat' +
          'h stroke="#e8f0f8" d="M10 5h1M10 10h1" />'#13#10'<path stroke="#438bcc' +
          '" d="M11 5h1" />'#13#10'<path stroke="#609ed4" d="M4 6h1" />'#13#10'<path st' +
          'roke="#f8fafc" d="M5 6h1M5 9h1M7 11h1" />'#13#10'<path stroke="#4990ce' +
          '" d="M6 6h1M6 9h1M11 10h1" />'#13#10'<path stroke="#70a8da" d="M10 6h1' +
          '" />'#13#10'<path stroke="#5094d0" d="M11 6h1" />'#13#10'<path stroke="#7eb1' +
          'dd" d="M4 7h1" />'#13#10'<path stroke="#d0e3f2" d="M5 7h1M5 8h1" />'#13#10'<' +
          'path stroke="#77acdb" d="M4 8h1" />'#13#10'<path stroke="#5d9cd4" d="M' +
          '4 9h1" />'#13#10'<path stroke="#94bee3" d="M10 9h1" />'#13#10'<path stroke="' +
          '#6da6d8" d="M11 9h1M9 10h1" />'#13#10'<path stroke="#aacbe9" d="M6 11h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ClassOpen'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'MatchBracket'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#848484" d="M4 2h1M1' +
          '1 2h1M5 3h1M10 3h1M4 6h1M11 6h1M3 7h1M2 8h1M4 8h1M11 8h1M13 8h1M' +
          '5 12h1M10 12h1M12 12h1M4 13h1M11 13h1" />'#13#10'<path stroke="#ffffff' +
          '" d="M5 2h2M9 2h2M4 3h1M11 3h1M3 4h2M11 4h2M3 5h2M11 5h2M3 6h1M1' +
          '2 6h1M2 7h1M13 7h1M3 8h1M12 8h1M3 9h2M11 9h2M3 10h2M11 10h2M3 11' +
          'h2M11 11h2M4 12h1M11 12h1M5 13h2M9 13h1" />'#13#10'<path stroke="#a2a2' +
          'a2" d="M3 3h1M12 3h1M12 7h1M3 12h1M10 13h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'SystemOutPrintln'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffff7a" d="M3 1h1M4' +
          ' 2h1M3 3h1" />'#13#10'<path stroke="#7a7a7a" d="M3 2h1M4 3h1M3 4h1" />' +
          #13#10'<path stroke="#ffffff" d="M5 3h1M4 4h1M3 5h1M5 5h6M5 6h1M10 6h' +
          '2M5 7h1M10 7h3M5 8h1M12 8h1M5 9h1M7 9h4M12 9h1M5 10h1M12 10h1M5 ' +
          '11h1M12 11h1M5 12h1M12 12h1M5 13h1M12 13h1M5 14h8" />'#13#10'<path str' +
          'oke="#11ffff" d="M6 10h6M6 11h1M11 11h1M6 12h6" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'IfStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14  M10.1 5.5 v8' +
          '.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'
      end
      item
        IconName = 'IfElseStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'
      end
      item
        IconName = 'WhileStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11" />'#13#10'</svg>'
      end
      item
        IconName = 'ForStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 3.75 h11.5 M3.75 4' +
          ' v11 M9 7 v1 M9 9 v3.5" />'#13#10'</svg>'
      end
      item
        IconName = 'DoWhileStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M3.25 12.25 h11.5 M3.75 ' +
          '1 v11" />'#13#10'</svg>'
      end
      item
        IconName = 'SwitchStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M5.8 5.5 v8.5' +
          ' M10.1 5.5 v8.5 M10.1 5.5 L 14 2 M10.1 5.5 L 2 2" />'#13#10'</svg>'
      end
      item
        IconName = 'TryStatement'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M1 1h14M' +
          '1 2h1M14 2h1M1 3h1M14 3h1M1 4h1M4 4h1M14 4h1M1 5h1M4 5h1M14 5h1M' +
          '1 6h1M4 6h1M14 6h1M1 7h1M4 7h1M14 7h1M1 8h1M4 8h1M7 8h1M14 8h1M1' +
          ' 9h1M4 9h1M7 9h1M14 9h1M1 10h1M7 10h1M14 10h1M1 11h1M12 11h1M14 ' +
          '11h1M1 12h1M14 12h1M1 13h1M14 13h1M1 14h14" />'#13#10'<path stroke="#f' +
          'fffff" d="M3 5h1" />'#13#10'<path stroke="#ffffff" d="M5 5h1" />'#13#10'<pat' +
          'h stroke="#ffffff" d="M8 7h1" />'#13#10'<path stroke="#ffffff" d="M9 7' +
          'h1" />'#13#10'<path stroke="#ffffff" d="M11 7h1" />'#13#10'<path stroke="#ff' +
          'ffff" d="M13 7h1" />'#13#10'<path stroke="#ffffff" d="M9 8h1" />'#13#10'<pat' +
          'h stroke="#ffffff" d="M11 8h1" />'#13#10'<path stroke="#ffffff" d="M13' +
          ' 8h1" />'#13#10'<path stroke="#ffffff" d="M11 9h1" />'#13#10'<path stroke="#' +
          'ffffff" d="M12 9h1" />'#13#10'<path stroke="#ffffff" d="M4 10h1" />'#13#10'<' +
          'path stroke="#ffffff" d="M5 10h1" />'#13#10'<path stroke="#ffffff" d="' +
          'M12 10h1M11 12h1" />'#13#10'<path stroke="#ffffff" d="M10 12h1" />'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'BlockStatement'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#ffffff" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 " />'#13#10'</svg>'
      end
      item
        IconName = 'Comment'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" stroke-widt' +
          'h="1.5" d="M3 12 l5 -11 M8 12 l5 -11 " />'#13#10'</svg>'
      end
      item
        IconName = 'Indent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#95fff8" d="M2 4h1M2 5h2M0 6h5M2 7h2M2 ' +
          '8h1" />'#13#10'</svg>'
      end
      item
        IconName = 'Unindent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#95fff8" d="M2 4h1M1 5h2M0 6h5M1 7h2M2 ' +
          '8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'WordWrap'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M6 3h7M1' +
          '3 4h1M13 5h1M10 6h1M13 6h1M9 7h2M13 7h1M8 8h5M9 9h2M10 10h1" />'#13 +
          #10'<path stroke="#ffffff" d="M1 5h6M1 7h4M1 9h4M1 11h6" />'#13#10'</svg>' +
          #13#10#13#10
      end
      item
        IconName = 'Paragraph'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M5 4h6M4' +
          ' 5h1M7 5h1M9 5h1M4 6h1M7 6h1M9 6h1M5 7h3M9 7h1M7 8h1M9 8h1M7 9h1' +
          'M9 9h1M7 10h1M9 10h1M6 11h2M9 11h2" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Numbers'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M4 5h7 M' +
          '4 8h7 M5 11 L7 2 M8 11 L10 2 " />'#13#10'</svg>'
      end
      item
        IconName = 'ZoomOut'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#e6e6e6">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400ZM280-540v' +
          '-80h200v80H280Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'ZoomIn'
        SVGText = 
          '<svg viewBox="100 -850 800 800" fill="#e6e6e6">'#13#10'  <path d="M784' +
          '-120 532-372q-30 24-69 38t-83 14q-109 0-184.5-75.5T120-580q0-109' +
          ' 75.5-184.5T380-840q109 0 184.5 75.5T640-580q0 44-14 83t-38 69l2' +
          '52 252-56 56ZM380-400q75 0 127.5-52.5T560-580q0-75-52.5-127.5T38' +
          '0-760q-75 0-127.5 52.5T200-580q0 75 52.5 127.5T380-400Zm-40-60v-' +
          '80h-80v-80h80v-80h80v80h80v80h-80v80h-80Z"/>'#13#10'</svg>'
      end>
    Left = 376
    Top = 47
  end
  object vilEditorToolbarLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Explorer'
        Name = 'Explorer'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Browser'
        Name = 'Browser'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Designform'
        Name = 'Designform'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Classedit'
        Name = 'Classedit'
      end
      item
        CollectionIndex = 5
        CollectionName = 'ClassOpen'
        Name = 'ClassOpen'
      end
      item
        CollectionIndex = 6
        CollectionName = 'MatchBracket'
        Name = 'MatchBracket'
      end
      item
        CollectionIndex = 7
        CollectionName = 'SystemOutPrintln'
        Name = 'SystemOutPrintln'
      end
      item
        CollectionIndex = 8
        CollectionName = 'StructureIndent'
        Name = 'StructureIndent'
      end
      item
        CollectionIndex = 9
        CollectionName = 'IfStatement'
        Name = 'IfStatement'
      end
      item
        CollectionIndex = 10
        CollectionName = 'IfElseStatement'
        Name = 'IfElseStatement'
      end
      item
        CollectionIndex = 11
        CollectionName = 'WhileStatement'
        Name = 'WhileStatement'
      end
      item
        CollectionIndex = 12
        CollectionName = 'ForStatement'
        Name = 'ForStatement'
      end
      item
        CollectionIndex = 13
        CollectionName = 'DoWhileStatement'
        Name = 'DoWhileStatement'
      end
      item
        CollectionIndex = 14
        CollectionName = 'SwitchStatement'
        Name = 'SwitchStatement'
      end
      item
        CollectionIndex = 15
        CollectionName = 'TryStatement'
        Name = 'TryStatement'
      end
      item
        CollectionIndex = 16
        CollectionName = 'BlockStatement'
        Name = 'BlockStatement'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Comment'
        Name = 'Comment'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Indent'
        Name = 'Indent'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Unindent'
        Name = 'Unindent'
      end
      item
        CollectionIndex = 20
        CollectionName = 'WordWrap'
        Name = 'WordWrap'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Breakpoint'
        Name = 'Breakpoint'
      end
      item
        CollectionIndex = 22
        CollectionName = 'BreakpointsClear'
        Name = 'BreakpointsClear'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Bookmark'
        Name = 'Bookmark'
      end
      item
        CollectionIndex = 24
        CollectionName = 'GotoBookmark'
        Name = 'GotoBookmark'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Paragraph'
        Name = 'Paragraph'
      end
      item
        CollectionIndex = 26
        CollectionName = 'Numbers'
        Name = 'Numbers'
      end
      item
        CollectionIndex = 27
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 28
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Validate'
        Name = 'Validate'
      end>
    ImageCollection = icEditor
    Left = 72
    Top = 47
  end
  object vilEditorToolbarDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 30
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Explorer'
        Name = 'Explorer'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Browser'
        Name = 'Browser'
      end
      item
        CollectionIndex = 31
        CollectionName = 'Designform'
        Name = 'Designform'
      end
      item
        CollectionIndex = 32
        CollectionName = 'ClassEdit'
        Name = 'ClassEdit'
      end
      item
        CollectionIndex = 5
        CollectionName = 'ClassOpen'
        Name = 'ClassOpen'
      end
      item
        CollectionIndex = 34
        CollectionName = 'MatchBracket'
        Name = 'MatchBracket'
      end
      item
        CollectionIndex = 35
        CollectionName = 'SystemOutPrintln'
        Name = 'SystemOutPrintln'
      end
      item
        CollectionIndex = 8
        CollectionName = 'StructureIndent'
        Name = 'StructureIndent'
      end
      item
        CollectionIndex = 36
        CollectionName = 'IfStatement'
        Name = 'IfStatement'
      end
      item
        CollectionIndex = 37
        CollectionName = 'IfElseStatement'
        Name = 'IfElseStatement'
      end
      item
        CollectionIndex = 38
        CollectionName = 'WhileStatement'
        Name = 'WhileStatement'
      end
      item
        CollectionIndex = 39
        CollectionName = 'ForStatement'
        Name = 'ForStatement'
      end
      item
        CollectionIndex = 40
        CollectionName = 'DoWhileStatement'
        Name = 'DoWhileStatement'
      end
      item
        CollectionIndex = 41
        CollectionName = 'SwitchStatement'
        Name = 'SwitchStatement'
      end
      item
        CollectionIndex = 42
        CollectionName = 'TryStatement'
        Name = 'TryStatement'
      end
      item
        CollectionIndex = 43
        CollectionName = 'BlockStatement'
        Name = 'BlockStatement'
      end
      item
        CollectionIndex = 44
        CollectionName = 'Comment'
        Name = 'Comment'
      end
      item
        CollectionIndex = 45
        CollectionName = 'Indent'
        Name = 'Indent'
      end
      item
        CollectionIndex = 46
        CollectionName = 'Unindent'
        Name = 'Unindent'
      end
      item
        CollectionIndex = 47
        CollectionName = 'WordWrap'
        Name = 'WordWrap'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Breakpoint'
        Name = 'Breakpoint'
      end
      item
        CollectionIndex = 22
        CollectionName = 'BreakpointsClear'
        Name = 'BreakpointsClear'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Bookmark'
        Name = 'Bookmark'
      end
      item
        CollectionIndex = 24
        CollectionName = 'GotoBookmark'
        Name = 'GotoBookmark'
      end
      item
        CollectionIndex = 48
        CollectionName = 'Paragraph'
        Name = 'Paragraph'
      end
      item
        CollectionIndex = 49
        CollectionName = 'Numbers'
        Name = 'Numbers'
      end
      item
        CollectionIndex = 50
        CollectionName = 'ZoomOut'
        Name = 'ZoomOut'
      end
      item
        CollectionIndex = 51
        CollectionName = 'ZoomIn'
        Name = 'ZoomIn'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Validate'
        Name = 'Validate'
      end>
    ImageCollection = icEditor
    Left = 224
    Top = 47
  end
  object icContextMenu: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Font'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M186-8' +
          '0q-54 0-80-22t-26-66q0-58 49-74t116-16h21v-56q0-34-1-55.5t-6-35.' +
          '5q-5-14-11.5-19.5T230-430q-9 0-16.5 3t-12.5 8q-4 5-5 10.5t1 11.5' +
          'q6 11 14 21.5t8 24.5q0 25-17.5 42.5T159-291q-25 0-42.5-17.5T99-3' +
          '51q0-27 12-44t32.5-27q20.5-10 47.5-14t58-4q85 0 118 30.5T400-302' +
          'v147q0 19 4.5 28t15.5 9q12 0 19.5-18t9.5-56h11q-3 62-23.5 87T368' +
          '-80q-43 0-67.5-13.5T269-134q-10 29-29.5 41.5T186-80Zm373 0q-20 0' +
          '-32.5-16.5T522-132l102-269q7-17 22-28t34-11q19 0 34 11t22 28l102' +
          ' 269q8 19-4.5 35.5T801-80q-12 0-22-7t-15-19l-20-58H616l-20 58q-4' +
          ' 11-14 18.5T559-80Zm-324-29q13 0 22-20.5t9-49.5v-67q-26 0-38 15.' +
          '5T216-180v11q0 36 4 48t15 12Zm407-125h77l-39-114-38 114Zm-37-285' +
          'q-48 0-76.5-33.5T500-643q0-104 66-170.5T735-880q42 0 68 9.5t26 2' +
          '4.5q0 6-2 12t-7 11q-5 7-12.5 10t-15.5 1q-14-4-32-7t-33-3q-71 0-1' +
          '14 48t-43 127q0 22 8 46t36 24q11 0 21.5-5t18.5-14q17-18 31.5-60T' +
          '712-758q2-13 10.5-18.5T746-782q18 0 27.5 9.5T779-749q-12 43-17.5' +
          ' 75t-5.5 58q0 20 5.5 29t16.5 9q11 0 21.5-8t29.5-30q2-3 15-7 8 0 ' +
          '12 6t4 17q0 28-32 54t-67 26q-26 0-44.5-14T691-574q-15 26-37 40.5' +
          'T605-519Zm-485-1v-220q0-58 41-99t99-41q58 0 99 41t41 99v220h-80v' +
          '-80H200v80h-80Zm80-160h120v-60q0-25-17.5-42.5T260-800q-25 0-42.5' +
          ' 17.5T200-740v60Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Cut'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#f9f9f9" d="M3 0h1M1' +
          '2 6h1" />'#13#10'<path stroke="#f6f6f6" d="M12 0h1" />'#13#10'<path stroke="' +
          '#a2a2a2" d="M13 0h1M5 4h1" />'#13#10'<path stroke="#929292" d="M2 1h1"' +
          ' />'#13#10'<path stroke="#a6a6a6" d="M3 1h1" />'#13#10'<path stroke="#a4a4a4' +
          '" d="M12 1h1" />'#13#10'<path stroke="#959595" d="M13 1h1" />'#13#10'<path s' +
          'troke="#afafaf" d="M2 2h1" />'#13#10'<path stroke="#777777" d="M3 2h1M' +
          '12 2h1M3 3h1M12 3h1M4 4h1M11 4h1M4 5h2M10 5h2M5 6h1M10 6h1M6 7h1' +
          'M9 7h2M7 8h2" />'#13#10'<path stroke="#cfcfcf" d="M4 2h1M6 5h1" />'#13#10'<p' +
          'ath stroke="#cdcdcd" d="M11 2h1M9 5h1" />'#13#10'<path stroke="#b0b0b0' +
          '" d="M13 2h1" />'#13#10'<path stroke="#dcdcdc" d="M2 3h1" />'#13#10'<path st' +
          'roke="#808080" d="M4 3h1" />'#13#10'<path stroke="#f2f2f2" d="M5 3h1" ' +
          '/>'#13#10'<path stroke="#f1f1f1" d="M10 3h1" />'#13#10'<path stroke="#7f7f7f' +
          '" d="M11 3h1" />'#13#10'<path stroke="#d7d7d7" d="M13 3h1" />'#13#10'<path s' +
          'troke="#888888" d="M3 4h1" />'#13#10'<path stroke="#a0a0a0" d="M10 4h1' +
          '" />'#13#10'<path stroke="#838383" d="M12 4h1" />'#13#10'<path stroke="#fbfb' +
          'fb" d="M13 4h1" />'#13#10'<path stroke="#c8c8c8" d="M3 5h1" />'#13#10'<path ' +
          'stroke="#bebebe" d="M12 5h1" />'#13#10'<path stroke="#909090" d="M4 6h' +
          '1" />'#13#10'<path stroke="#7e7e7e" d="M6 6h1M9 6h1" />'#13#10'<path stroke=' +
          '"#ededed" d="M7 6h1" />'#13#10'<path stroke="#ececec" d="M8 6h1" />'#13#10'<' +
          'path stroke="#868686" d="M11 6h1" />'#13#10'<path stroke="#e7e7e7" d="' +
          'M4 7h1" />'#13#10'<path stroke="#7c7c7c" d="M5 7h1" />'#13#10'<path stroke="' +
          '#919191" d="M7 7h1" />'#13#10'<path stroke="#8f8f8f" d="M8 7h1" />'#13#10'<p' +
          'ath stroke="#d8d8d8" d="M11 7h1" />'#13#10'<path stroke="#d5d5d5" d="M' +
          '5 8h1" />'#13#10'<path stroke="#7a7a7a" d="M6 8h1" />'#13#10'<path stroke="#' +
          '767676" d="M9 8h1" />'#13#10'<path stroke="#c0c0c0" d="M10 8h1" />'#13#10'<p' +
          'ath stroke="#c4d1e0" d="M6 9h1" />'#13#10'<path stroke="#90abc6" d="M7' +
          ' 9h2" />'#13#10'<path stroke="#adc0d4" d="M9 9h1" />'#13#10'<path stroke="#b' +
          'acee3" d="M2 10h1" />'#13#10'<path stroke="#5a89bb" d="M3 10h1" />'#13#10'<p' +
          'ath stroke="#4076b0" d="M4 10h1" />'#13#10'<path stroke="#457bb2" d="M' +
          '5 10h1M10 14h1M13 14h1" />'#13#10'<path stroke="#5384b8" d="M6 10h1M9 ' +
          '10h1" />'#13#10'<path stroke="#3f77b0" d="M7 10h2M6 11h1M9 11h1M13 11h' +
          '1M6 12h1M9 12h1M6 13h1M9 13h1" />'#13#10'<path stroke="#5182b7" d="M10' +
          ' 10h1M12 11h1" />'#13#10'<path stroke="#4278b2" d="M11 10h1" />'#13#10'<path' +
          ' stroke="#5888ba" d="M12 10h1" />'#13#10'<path stroke="#b3c9e1" d="M13' +
          ' 10h1M8 13h1" />'#13#10'<path stroke="#c4d5e7" d="M1 11h1" />'#13#10'<path s' +
          'troke="#4078b1" d="M2 11h1" />'#13#10'<path stroke="#4f81b6" d="M3 11h' +
          '1M3 15h1M11 15h1" />'#13#10'<path stroke="#a1bdda" d="M4 11h1" />'#13#10'<pa' +
          'th stroke="#7ba2c9" d="M5 11h1" />'#13#10'<path stroke="#4077b1" d="M7' +
          ' 11h1M1 13h1M14 13h1" />'#13#10'<path stroke="#497eb4" d="M8 11h1" />'#13 +
          #10'<path stroke="#719ac4" d="M10 11h1" />'#13#10'<path stroke="#a1bcd7" ' +
          'd="M11 11h1" />'#13#10'<path stroke="#c0d1e6" d="M14 11h1" />'#13#10'<path s' +
          'troke="#6692c0" d="M1 12h1" />'#13#10'<path stroke="#4379b1" d="M2 12h' +
          '1M5 14h1" />'#13#10'<path stroke="#dfe8f2" d="M3 12h1" />'#13#10'<path strok' +
          'e="#e1e9f3" d="M5 12h1M6 15h1" />'#13#10'<path stroke="#5b8bbc" d="M7 ' +
          '12h1M9 14h1" />'#13#10'<path stroke="#79a0c8" d="M8 12h1" />'#13#10'<path st' +
          'roke="#d8e3ef" d="M10 12h1" />'#13#10'<path stroke="#dde7f1" d="M12 12' +
          'h1" />'#13#10'<path stroke="#4278b1" d="M13 12h1" />'#13#10'<path stroke="#6' +
          '28fbf" d="M14 12h1M1 14h1" />'#13#10'<path stroke="#6590bf" d="M2 13h1' +
          'M13 13h1" />'#13#10'<path stroke="#a6c0dc" d="M5 13h1" />'#13#10'<path strok' +
          'e="#8dafd1" d="M7 13h1" />'#13#10'<path stroke="#a8c1dc" d="M10 13h1M1' +
          '1 14h1" />'#13#10'<path stroke="#447ab2" d="M2 14h1" />'#13#10'<path stroke=' +
          '"#a9c2dd" d="M3 14h1" />'#13#10'<path stroke="#a0bbd8" d="M4 14h1" />'#13 +
          #10'<path stroke="#4c7eb5" d="M6 14h1" />'#13#10'<path stroke="#e6ecf5" d' +
          '="M7 14h1" />'#13#10'<path stroke="#f9fbfc" d="M8 14h1" />'#13#10'<path stro' +
          'ke="#b2c8e0" d="M12 14h1" />'#13#10'<path stroke="#5f8dbd" d="M14 14h1' +
          '" />'#13#10'<path stroke="#e5edf4" d="M1 15h1" />'#13#10'<path stroke="#7ba1' +
          'c8" d="M2 15h1" />'#13#10'<path stroke="#5283b8" d="M4 15h1" />'#13#10'<path' +
          ' stroke="#80a5cb" d="M5 15h1M10 15h1" />'#13#10'<path stroke="#e9eff5"' +
          ' d="M9 15h1" />'#13#10'<path stroke="#4a7eb5" d="M12 15h1" />'#13#10'<path s' +
          'troke="#759dc7" d="M13 15h1" />'#13#10'<path stroke="#e2eaf3" d="M14 1' +
          '5h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Copy'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#a7a7a7" d="M1 0h4M0' +
          ' 1h1M5 1h1M0 2h1M5 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1' +
          'M0 10h1M1 11h4" />'#13#10'<path stroke="#b1b1b1" d="M5 0h1" />'#13#10'<path ' +
          'stroke="#ececec" d="M6 0h1" />'#13#10'<path stroke="#aeaeae" d="M6 1h1' +
          'M7 2h1" />'#13#10'<path stroke="#e9e9e9" d="M7 1h1" />'#13#10'<path stroke="' +
          '#e6e6e6" d="M6 2h1M5 3h1" />'#13#10'<path stroke="#e4e4e4" d="M8 2h1" ' +
          '/>'#13#10'<path stroke="#6ea5d7" d="M2 3h2M2 5h2M2 7h3M2 9h3" />'#13#10'<pat' +
          'h stroke="#909090" d="M6 4h1" />'#13#10'<path stroke="#777777" d="M7 4' +
          'h4M6 5h1M11 5h1M6 6h1M11 6h1M6 7h1M11 7h1M6 8h1M12 8h3M6 9h1M15 ' +
          '9h1M6 10h1M15 10h1M6 11h1M15 11h1M6 12h1M15 12h1M6 13h1M15 13h1M' +
          '6 14h1M15 14h1M7 15h8" />'#13#10'<path stroke="#858585" d="M11 4h1" />' +
          #13#10'<path stroke="#e2e2e2" d="M12 4h1" />'#13#10'<path stroke="#fcfcfc" ' +
          'd="M7 5h4M7 6h4M7 7h1M10 7h1M12 7h1M7 8h4M7 9h1M10 9h5M7 10h8M7 ' +
          '11h1M14 11h1M7 12h8M7 13h1M14 13h1M7 14h8" />'#13#10'<path stroke="#81' +
          '8181" d="M12 5h1M13 6h1" />'#13#10'<path stroke="#dcdcdc" d="M13 5h1" ' +
          '/>'#13#10'<path stroke="#d8d8d8" d="M12 6h1" />'#13#10'<path stroke="#d7d7d7' +
          '" d="M14 6h1" />'#13#10'<path stroke="#3f77b0" d="M8 7h2M8 9h2M8 11h6M' +
          '8 13h6" />'#13#10'<path stroke="#dedede" d="M13 7h1" />'#13#10'<path stroke=' +
          '"#828282" d="M14 7h1" />'#13#10'<path stroke="#d1d1d1" d="M15 7h1" />'#13 +
          #10'<path stroke="#8c8c8c" d="M11 8h1" />'#13#10'<path stroke="#7e7e7e" d' +
          '="M15 8h1" />'#13#10'<path stroke="#8f8f8f" d="M6 15h1M15 15h1" />'#13#10'</' +
          'svg>'#13#10
      end
      item
        IconName = 'Paste'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#898989" d="M4 0h1M7' +
          ' 0h1" />'#13#10'<path stroke="#777777" d="M5 0h2M4 1h4M8 6h3M7 7h1M11 ' +
          '7h1M7 8h1M11 8h1M7 9h1M11 9h1M7 10h1M12 10h3M7 11h1M15 11h1M7 12' +
          'h1M15 12h1M7 13h1M15 13h1M7 14h1M15 14h1M8 15h7" />'#13#10'<path strok' +
          'e="#e8bd78" d="M0 1h1M0 13h1" />'#13#10'<path stroke="#e5b262" d="M1 1' +
          'h1M10 1h1M0 2h2M10 2h2M0 3h12M0 4h12M0 5h6M0 6h6M0 7h6M0 8h6M0 9' +
          'h6M0 10h6M0 11h6M0 12h6M1 13h5" />'#13#10'<path stroke="#7f7f7f" d="M3' +
          ' 1h1" />'#13#10'<path stroke="#838383" d="M8 1h1" />'#13#10'<path stroke="#e' +
          '8be78" d="M11 1h1" />'#13#10'<path stroke="#f8f2e6" d="M2 2h1" />'#13#10'<pa' +
          'th stroke="#ffffff" d="M3 2h6" />'#13#10'<path stroke="#f7f0e7" d="M9 ' +
          '2h1" />'#13#10'<path stroke="#efd5ab" d="M6 5h1" />'#13#10'<path stroke="#fc' +
          'fbf9" d="M7 5h1M6 6h1" />'#13#10'<path stroke="#91908f" d="M7 6h1" />'#13 +
          #10'<path stroke="#858585" d="M11 6h1" />'#13#10'<path stroke="#e2e2e2" d' +
          '="M12 6h1" />'#13#10'<path stroke="#fcfcfc" d="M8 7h3M8 8h3M8 9h3M12 9' +
          'h1M8 10h3M8 11h7M8 12h7M8 13h7M8 14h7" />'#13#10'<path stroke="#818181' +
          '" d="M12 7h1M13 8h1" />'#13#10'<path stroke="#dcdcdc" d="M13 7h1" />'#13#10 +
          '<path stroke="#d8d8d8" d="M12 8h1" />'#13#10'<path stroke="#d7d7d7" d=' +
          '"M14 8h1" />'#13#10'<path stroke="#dedede" d="M13 9h1" />'#13#10'<path strok' +
          'e="#828282" d="M14 9h1" />'#13#10'<path stroke="#d1d1d1" d="M15 9h1" /' +
          '>'#13#10'<path stroke="#8c8c8c" d="M11 10h1" />'#13#10'<path stroke="#7e7e7e' +
          '" d="M15 10h1" />'#13#10'<path stroke="#8f8f8f" d="M7 15h1M15 15h1" />' +
          #13#10'</svg>'#13#10
      end
      item
        IconName = 'Delete'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#191919">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Indent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#00007e" d="M2 4h1M2 5h2M0 6h5M2 7h2M2 ' +
          '8h1" />'#13#10'</svg>'
      end
      item
        IconName = 'Unindent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#00007e" d="M2 4h1M1 5h2M0 6h5M1 7h2M2 ' +
          '8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Undo'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#a2c6e8" d="M4 2h1M2' +
          ' 7h1" />'#13#10'<path stroke="#a7caeb" d="M5 2h1" />'#13#10'<path stroke="#c' +
          '3e0fa" d="M6 2h1" />'#13#10'<path stroke="#b5d5f3" d="M1 3h1" />'#13#10'<pat' +
          'h stroke="#6192c4" d="M2 3h1" />'#13#10'<path stroke="#4076b0" d="M3 3' +
          'h1" />'#13#10'<path stroke="#3f77b0" d="M4 3h2M2 4h2M6 4h2M1 5h1M8 5h1' +
          'M1 6h1M9 6h1M14 6h1M1 7h1M10 7h1M14 7h2M1 8h1M11 8h1M14 8h2M1 9h' +
          '1M12 9h1M14 9h2M1 10h1M13 10h3M2 11h1M9 11h7M2 12h1M10 12h5" />'#13 +
          #10'<path stroke="#447ab2" d="M6 3h1" />'#13#10'<path stroke="#78a4d0" d=' +
          '"M7 3h1" />'#13#10'<path stroke="#bfdff9" d="M8 3h1" />'#13#10'<path stroke=' +
          '"#bad8f5" d="M0 4h1M10 9h1" />'#13#10'<path stroke="#4a7fb6" d="M1 4h1' +
          'M7 5h1" />'#13#10'<path stroke="#487eb5" d="M4 4h1M2 10h1" />'#13#10'<path s' +
          'troke="#457cb3" d="M5 4h1" />'#13#10'<path stroke="#4e82b9" d="M8 4h1M' +
          '0 6h1M12 8h1" />'#13#10'<path stroke="#b1d2f1" d="M9 4h1" />'#13#10'<path st' +
          'roke="#c2e0fa" d="M14 4h1" />'#13#10'<path stroke="#74a2cf" d="M0 5h1"' +
          ' />'#13#10'<path stroke="#4c80b7" d="M2 5h1" />'#13#10'<path stroke="#a9cced' +
          '" d="M3 5h1" />'#13#10'<path stroke="#97bde2" d="M6 5h1" />'#13#10'<path str' +
          'oke="#497eb6" d="M9 5h1" />'#13#10'<path stroke="#b2d3f1" d="M10 5h1" ' +
          '/>'#13#10'<path stroke="#588abe" d="M14 5h1M0 8h1" />'#13#10'<path stroke="#' +
          'bfdef7" d="M15 5h1" />'#13#10'<path stroke="#92bae0" d="M2 6h1" />'#13#10'<p' +
          'ath stroke="#b9d8f5" d="M7 6h1M11 10h1" />'#13#10'<path stroke="#5285b' +
          'a" d="M8 6h1" />'#13#10'<path stroke="#4b80b7" d="M10 6h1" />'#13#10'<path s' +
          'troke="#b4d4f1" d="M11 6h1" />'#13#10'<path stroke="#568abe" d="M15 6h' +
          '1M1 12h1" />'#13#10'<path stroke="#467db5" d="M0 7h1" />'#13#10'<path stroke' +
          '="#bad9f6" d="M8 7h1" />'#13#10'<path stroke="#5285bb" d="M9 7h1M10 8h' +
          '1" />'#13#10'<path stroke="#4c80b8" d="M11 7h1" />'#13#10'<path stroke="#b6d' +
          '6f3" d="M12 7h1" />'#13#10'<path stroke="#8cb5dd" d="M2 8h1" />'#13#10'<path' +
          ' stroke="#bbdbf5" d="M9 8h1" />'#13#10'<path stroke="#b8d7f5" d="M13 8' +
          'h1" />'#13#10'<path stroke="#75a2ce" d="M0 9h1" />'#13#10'<path stroke="#6d9' +
          'aca" d="M2 9h1" />'#13#10'<path stroke="#5284ba" d="M11 9h1" />'#13#10'<path' +
          ' stroke="#5084b9" d="M13 9h1" />'#13#10'<path stroke="#95bce2" d="M0 1' +
          '0h1" />'#13#10'<path stroke="#4f83b9" d="M12 10h1" />'#13#10'<path stroke="#' +
          'bcdcf6" d="M0 11h1" />'#13#10'<path stroke="#3f76b0" d="M1 11h1" />'#13#10'<' +
          'path stroke="#a8cced" d="M3 11h1" />'#13#10'<path stroke="#c2e1fa" d="' +
          'M7 11h1" />'#13#10'<path stroke="#598bbf" d="M8 11h1" />'#13#10'<path stroke' +
          '="#83add6" d="M3 12h1" />'#13#10'<path stroke="#c0def8" d="M8 12h1" />' +
          #13#10'<path stroke="#568abd" d="M9 12h1" />'#13#10'<path stroke="#4078b2" ' +
          'd="M15 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Redo'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#cedceb" d="M10 2h1"' +
          ' />'#13#10'<path stroke="#c7d7e8" d="M11 2h1M13 7h1" />'#13#10'<path stroke=' +
          '"#e8eef5" d="M12 2h1" />'#13#10'<path stroke="#f0f4f9" d="M7 3h1" />'#13#10 +
          '<path stroke="#8daed0" d="M8 3h1" />'#13#10'<path stroke="#457bb2" d="' +
          'M9 3h1" />'#13#10'<path stroke="#3f77b0" d="M10 3h2M8 4h2M12 4h2M7 5h1' +
          'M14 5h1M1 6h1M6 6h1M14 6h1M0 7h2M5 7h1M14 7h1M0 8h2M4 8h1M14 8h1' +
          'M0 9h2M3 9h1M14 9h1M0 10h3M14 10h1M0 11h7M13 11h1M1 12h5M13 12h1' +
          '" />'#13#10'<path stroke="#4076b0" d="M12 3h1" />'#13#10'<path stroke="#6e98' +
          'c4" d="M13 3h1" />'#13#10'<path stroke="#e2eaf3" d="M14 3h1" />'#13#10'<path' +
          ' stroke="#f3f6fa" d="M1 4h1" />'#13#10'<path stroke="#dce6f1" d="M6 4h' +
          '1" />'#13#10'<path stroke="#5385b9" d="M7 4h1M15 6h1M3 8h1" />'#13#10'<path ' +
          'stroke="#477db3" d="M10 4h1" />'#13#10'<path stroke="#4c7fb5" d="M11 4' +
          'h1M13 10h1" />'#13#10'<path stroke="#4f81b6" d="M14 4h1M8 5h1" />'#13#10'<pa' +
          'th stroke="#e9eef5" d="M15 4h1" />'#13#10'<path stroke="#eff3f7" d="M0' +
          ' 5h1" />'#13#10'<path stroke="#628fbe" d="M1 5h1M15 8h1" />'#13#10'<path str' +
          'oke="#dee7f1" d="M5 5h1" />'#13#10'<path stroke="#4e80b6" d="M6 5h1" /' +
          '>'#13#10'<path stroke="#b8cce2" d="M9 5h1" />'#13#10'<path stroke="#d1dfed" ' +
          'd="M12 5h1" />'#13#10'<path stroke="#5283b7" d="M13 5h1" />'#13#10'<path str' +
          'oke="#88abcf" d="M15 5h1" />'#13#10'<path stroke="#608ebe" d="M0 6h1M1' +
          '4 12h1" />'#13#10'<path stroke="#e0e9f1" d="M4 6h1" />'#13#10'<path stroke="' +
          '#5082b7" d="M5 6h1" />'#13#10'<path stroke="#5888ba" d="M7 6h1" />'#13#10'<p' +
          'ath stroke="#e7edf5" d="M8 6h1M4 10h1" />'#13#10'<path stroke="#b1c8e0' +
          '" d="M13 6h1" />'#13#10'<path stroke="#e3ebf3" d="M3 7h1" />'#13#10'<path st' +
          'roke="#5283b8" d="M4 7h1" />'#13#10'<path stroke="#5989bb" d="M6 7h1M5' +
          ' 8h1" />'#13#10'<path stroke="#e9eff6" d="M7 7h1" />'#13#10'<path stroke="#4' +
          'a7eb5" d="M15 7h1" />'#13#10'<path stroke="#e6ecf5" d="M2 8h1" />'#13#10'<pa' +
          'th stroke="#eaf0f5" d="M6 8h1" />'#13#10'<path stroke="#a9c2dd" d="M13' +
          ' 8h1" />'#13#10'<path stroke="#5687b9" d="M2 9h1" />'#13#10'<path stroke="#5' +
          '887ba" d="M4 9h1" />'#13#10'<path stroke="#e9eff5" d="M5 9h1" />'#13#10'<pat' +
          'h stroke="#7ea2ca" d="M13 9h1" />'#13#10'<path stroke="#89abce" d="M15' +
          ' 9h1" />'#13#10'<path stroke="#5586b9" d="M3 10h1" />'#13#10'<path stroke="#' +
          'b6cbe2" d="M15 10h1" />'#13#10'<path stroke="#6390bf" d="M7 11h1" />'#13#10 +
          '<path stroke="#f6f7fa" d="M8 11h1" />'#13#10'<path stroke="#d0dfed" d=' +
          '"M12 11h1" />'#13#10'<path stroke="#3f76b0" d="M14 11h1" />'#13#10'<path str' +
          'oke="#ebf1f6" d="M15 11h1" />'#13#10'<path stroke="#4178b0" d="M0 12h1' +
          '" />'#13#10'<path stroke="#608ebd" d="M6 12h1" />'#13#10'<path stroke="#f0f3' +
          'f8" d="M7 12h1" />'#13#10'<path stroke="#9cb9d6" d="M12 12h1" />'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'APIHelp'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828082" d="M3 0h6M2' +
          ' 1h2M1 2h1M0 3h2M0 4h1M0 5h1M0 6h1M0 7h1M11 7h1M0 8h1M11 8h1M3 1' +
          '1h1" />'#13#10'<path stroke="#088082" d="M4 1h1M9 7h1" />'#13#10'<path strok' +
          'e="#c4c1c4" d="M5 1h1M3 2h1M4 3h1M2 4h1" />'#13#10'<path stroke="#0880' +
          '00" d="M6 1h3M5 2h5M3 4h2M6 4h2M2 5h5M3 6h5M5 7h4M5 8h2M8 8h1M5 ' +
          '9h1" />'#13#10'<path stroke="#000000" d="M9 1h1M12 1h1M10 2h1M12 2h2M8' +
          ' 3h5M14 3h1M8 4h1M15 4h1M8 5h1M15 5h1M8 6h5M14 6h1M12 7h2M7 8h1M' +
          '12 8h1M1 9h1M6 9h2M10 9h1M2 10h1M5 10h1M7 10h5M4 11h1M11 11h1M4 ' +
          '12h1M11 12h1M5 13h1M7 13h5M6 14h2M7 15h1" />'#13#10'<path stroke="#fff' +
          'fff" d="M2 2h1M4 2h1M3 3h1" />'#13#10'<path stroke="#0000ff" d="M2 3h1' +
          'M5 3h3M1 4h1M1 5h1M1 6h2M1 7h4M10 7h1M2 8h3M9 8h1M2 9h3M8 9h1M4 ' +
          '10h1" />'#13#10'<path stroke="#11ffff" d="M13 3h1M9 4h6M9 5h6M13 6h1M6' +
          ' 10h1M5 11h6M5 12h6M6 13h1" />'#13#10'<path stroke="#000082" d="M5 4h1' +
          'M7 5h1M1 8h1M10 8h1M9 9h1M3 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ResetJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'  <path stroke="#333333" d="M7 0h1' +
          'M8 1h1"/>'#13#10'  <path stroke="#cecece" d="M3 1h1M3 4h1M3 7h1M7 7h1M' +
          '5 8h1M3 10h1M7 10h1"/>'#13#10'  <path stroke="#7e7e7e" d="M4 1h3M4 10h' +
          '3"/>'#13#10'  <path stroke="#000000" d="M7 1h1M3 2h6M2 3h1M7 3h1M2 4h1' +
          'M1 5h1M1 6h1M9 6h1M2 7h1M8 7h1M2 8h1M8 8h1M3 9h5"/>'#13#10'  <path str' +
          'oke="#6f6f6f" d="M2 2h1M2 9h1M8 9h1"/>'#13#10'  <path stroke="#aeaeae"' +
          ' d="M9 2h1"/>'#13#10'  <path stroke="#8e8e8e" d="M1 3h1M1 8h1M9 8h1"/>' +
          #13#10'  <path stroke="#171717" d="M3 3h1M3 8h1M7 8h1"/>'#13#10'  <path str' +
          'oke="#9e9e9e" d="M4 3h1M8 3h1M7 4h1M4 8h1M6 8h1"/>'#13#10'  <path stro' +
          'ke="#bebebe" d="M5 3h2"/>'#13#10'  <path stroke="#252525" d="M1 4h1M1 ' +
          '7h1M9 7h1"/>'#13#10'  <path stroke="#6e9b6e" d="M13 4h1M15 4h1M13 5h1M' +
          '15 5h1M13 6h1M15 6h1M13 7h1M15 7h1M13 8h1M15 8h1M13 9h1M15 9h1M1' +
          '3 10h1M15 10h1M13 11h1M15 11h1M8 12h1M13 12h1"/>'#13#10'  <path stroke' +
          '="#055200" d="M14 4h1M14 5h1M14 6h1M14 7h1M14 8h1M14 9h1M14 10h1' +
          'M14 11h1M9 12h1M14 12h1M9 13h1M14 13h1M10 14h4M11 15h1"/>'#13#10'  <pa' +
          'th stroke="#424242" d="M2 5h1M8 6h1"/>'#13#10'  <path stroke="#505050"' +
          ' d="M2 6h1"/>'#13#10'  <path stroke="#80a77f" d="M10 12h1M15 12h1"/>'#13#10 +
          '  <path stroke="#99b999" d="M8 13h1"/>'#13#10'  <path stroke="#226422"' +
          ' d="M10 13h1M14 14h1"/>'#13#10'  <path stroke="#2b6b2b" d="M13 13h1"/>' +
          #13#10'  <path stroke="#a6c1a6" d="M15 13h1"/>'#13#10'  <path stroke="#1056' +
          '0f" d="M9 14h1M12 15h1"/>'#13#10'  <path stroke="#347234" d="M10 15h1"' +
          '/>'#13#10'  <path stroke="#538853" d="M13 15h1"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'OpenClass'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'<path stroke="#ffffff" d="M2 2h11M2 3h11M2' +
          ' 4h11M2 6h11M2 7h11M2 8h11M2 10h11M2 11h11M2 12h11" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ClassEdit'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#05529a" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5' +
          'h1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3' +
          'M12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#13#10'<path ' +
          'stroke="#468dcd" d="M5 4h1M5 11h1" />'#13#10'<path stroke="#a0c5e6" d=' +
          '"M6 4h1" />'#13#10'<path stroke="#f2f7fb" d="M7 4h1" />'#13#10'<path stroke=' +
          '"#e8f0f8" d="M8 4h1M10 5h1M10 10h1M8 11h1" />'#13#10'<path stroke="#cd' +
          'e1f1" d="M9 4h1M9 11h1" />'#13#10'<path stroke="#5497d1" d="M10 4h1M10' +
          ' 11h1" />'#13#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#13#10'<path st' +
          'roke="#b2d0ea" d="M6 5h1M6 10h1" />'#13#10'<path stroke="#488fce" d="M' +
          '7 5h1M7 10h1" />'#13#10'<path stroke="#73aada" d="M9 5h1" />'#13#10'<path st' +
          'roke="#438bcc" d="M11 5h1" />'#13#10'<path stroke="#609ed4" d="M4 6h1"' +
          ' />'#13#10'<path stroke="#f8fafc" d="M5 6h1M5 9h1M7 11h1" />'#13#10'<path st' +
          'roke="#4990ce" d="M6 6h1M6 9h1M11 10h1" />'#13#10'<path stroke="#70a8d' +
          'a" d="M10 6h1" />'#13#10'<path stroke="#5094d0" d="M11 6h1" />'#13#10'<path ' +
          'stroke="#7eb1dd" d="M4 7h1" />'#13#10'<path stroke="#d0e3f2" d="M5 7h1' +
          'M5 8h1" />'#13#10'<path stroke="#77acdb" d="M4 8h1" />'#13#10'<path stroke="' +
          '#5d9cd4" d="M4 9h1" />'#13#10'<path stroke="#94bee3" d="M10 9h1" />'#13#10'<' +
          'path stroke="#6da6d8" d="M11 9h1M9 10h1" />'#13#10'<path stroke="#aacb' +
          'e9" d="M6 11h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CreateStructogram'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#000000" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'
      end
      item
        IconName = 'Execute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M3 0h2M3' +
          ' 1h1M5 1h1M3 2h1M6 2h1M3 3h1M7 3h1M3 4h1M8 4h1M3 5h1M9 5h1M3 6h1' +
          'M10 6h1M3 7h1M11 7h1M3 8h1M11 8h1M3 9h1M10 9h1M3 10h1M9 10h1M3 1' +
          '1h1M8 11h1M3 12h1M7 12h1M3 13h1M6 13h1M3 14h1M5 14h1" />'#13#10'<path ' +
          'stroke="#25e852" d="M4 1h1M4 2h2M4 3h3M4 4h4M4 5h5M4 6h6M4 7h7M4' +
          ' 8h7M4 9h6M4 10h5M4 11h4M4 12h3M4 13h2" />'#13#10'<path stroke="#25e85' +
          '1" d="M4 14h1" />'#13#10'<path stroke="#0a0606" d="M3 15h1" />'#13#10'<path ' +
          'stroke="#0a0808" d="M4 15h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Git'
        SVGText = 
          '<svg>'#13#10'  <path fill="#F05133" d="M92.71 44.408 52.591 4.291c-2.3' +
          '1-2.311-6.057-2.311-8.369 0l-8.33 8.332L46.459 23.19c2.456-.83 5' +
          '.272-.273 7.229 1.685 1.969 1.97 2.521 4.81 1.67 7.275l10.186 10' +
          '.185c2.465-.85 5.307-.3 7.275 1.671 2.75 2.75 2.75 7.206 0 9.958' +
          '-2.752 2.751-7.208 2.751-9.961 0-2.068-2.07-2.58-5.11-1.531-7.65' +
          '8l-9.5-9.499v24.997c.67.332 1.303.774 1.861 1.332 2.75 2.75 2.75' +
          ' 7.206 0 9.959-2.75 2.749-7.209 2.749-9.957 0-2.75-2.754-2.75-7.' +
          '21 0-9.959.68-.679 1.467-1.193 2.307-1.537v-25.23c-.84-.344-1.62' +
          '5-.853-2.307-1.537-2.083-2.082-2.584-5.14-1.516-7.698L31.798 16.' +
          '715 4.288 44.222c-2.311 2.313-2.311 6.06 0 8.371l40.121 40.118c2' +
          '.31 2.311 6.056 2.311 8.369 0L92.71 52.779c2.311-2.311 2.311-6.0' +
          '6 0-8.371z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Configuration'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828282" d="M0 1h15M' +
          '0 2h1M0 3h1M0 4h1M0 5h1M0 6h1M0 7h1M0 8h1M0 9h1M0 10h1M0 11h1M0 ' +
          '12h1M0 13h1" />'#13#10'<path stroke="#ffffff" d="M1 2h1M11 2h1M13 2h1M' +
          '2 4h1M4 4h1M6 4h1M8 4h1M10 4h1M12 4h1M1 5h1M3 5h1M5 5h1M9 5h1M11' +
          ' 5h1M13 5h1M2 6h1M4 6h1M8 6h1M10 6h1M12 6h1M1 7h1M7 7h1M9 7h1M11' +
          ' 7h1M13 7h1M2 8h1M6 8h1M8 8h1M10 8h1M12 8h1M1 9h1M3 9h1M5 9h1M9 ' +
          '9h1M11 9h1M13 9h1M2 10h1M4 10h1M8 10h1M10 10h1M12 10h1M1 11h1M7 ' +
          '11h1M9 11h1M11 11h1M13 11h1M2 12h1M6 12h1M8 12h1M10 12h1M12 12h1' +
          'M1 13h1M3 13h1M5 13h1M7 13h1M9 13h1M11 13h1M13 13h1" />'#13#10'<path s' +
          'troke="#0000ff" d="M2 2h9M12 2h1" />'#13#10'<path stroke="#000000" d="' +
          'M14 2h1M1 3h14M14 4h1M6 5h2M14 5h1M3 6h1M5 6h2M14 6h1M3 7h3M14 7' +
          'h1M4 8h1M14 8h1M6 9h2M14 9h1M3 10h1M5 10h2M14 10h1M3 11h3M14 11h' +
          '1M4 12h1M14 12h1M14 13h1M0 14h15" />'#13#10'<path stroke="#c4c4c4" d="' +
          'M1 4h1M3 4h1M5 4h1M7 4h1M9 4h1M11 4h1M13 4h1M2 5h1M4 5h1M8 5h1M1' +
          '0 5h1M12 5h1M1 6h1M7 6h1M9 6h1M11 6h1M13 6h1M2 7h1M6 7h1M8 7h1M1' +
          '0 7h1M12 7h1M1 8h1M3 8h1M5 8h1M7 8h1M9 8h1M11 8h1M13 8h1M2 9h1M4' +
          ' 9h1M8 9h1M10 9h1M12 9h1M1 10h1M7 10h1M9 10h1M11 10h1M13 10h1M2 ' +
          '11h1M6 11h1M8 11h1M10 11h1M12 11h1M1 12h1M3 12h1M5 12h1M7 12h1M9' +
          ' 12h1M11 12h1M13 12h1M2 13h1M4 13h1M6 13h1M8 13h1M10 13h1M12 13h' +
          '1" />'#13#10'</svg>'
      end
      item
        IconName = 'Font'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="M186-8' +
          '0q-54 0-80-22t-26-66q0-58 49-74t116-16h21v-56q0-34-1-55.5t-6-35.' +
          '5q-5-14-11.5-19.5T230-430q-9 0-16.5 3t-12.5 8q-4 5-5 10.5t1 11.5' +
          'q6 11 14 21.5t8 24.5q0 25-17.5 42.5T159-291q-25 0-42.5-17.5T99-3' +
          '51q0-27 12-44t32.5-27q20.5-10 47.5-14t58-4q85 0 118 30.5T400-302' +
          'v147q0 19 4.5 28t15.5 9q12 0 19.5-18t9.5-56h11q-3 62-23.5 87T368' +
          '-80q-43 0-67.5-13.5T269-134q-10 29-29.5 41.5T186-80Zm373 0q-20 0' +
          '-32.5-16.5T522-132l102-269q7-17 22-28t34-11q19 0 34 11t22 28l102' +
          ' 269q8 19-4.5 35.5T801-80q-12 0-22-7t-15-19l-20-58H616l-20 58q-4' +
          ' 11-14 18.5T559-80Zm-324-29q13 0 22-20.5t9-49.5v-67q-26 0-38 15.' +
          '5T216-180v11q0 36 4 48t15 12Zm407-125h77l-39-114-38 114Zm-37-285' +
          'q-48 0-76.5-33.5T500-643q0-104 66-170.5T735-880q42 0 68 9.5t26 2' +
          '4.5q0 6-2 12t-7 11q-5 7-12.5 10t-15.5 1q-14-4-32-7t-33-3q-71 0-1' +
          '14 48t-43 127q0 22 8 46t36 24q11 0 21.5-5t18.5-14q17-18 31.5-60T' +
          '712-758q2-13 10.5-18.5T746-782q18 0 27.5 9.5T779-749q-12 43-17.5' +
          ' 75t-5.5 58q0 20 5.5 29t16.5 9q11 0 21.5-8t29.5-30q2-3 15-7 8 0 ' +
          '12 6t4 17q0 28-32 54t-67 26q-26 0-44.5-14T691-574q-15 26-37 40.5' +
          'T605-519Zm-485-1v-220q0-58 41-99t99-41q58 0 99 41t41 99v220h-80v' +
          '-80H200v80h-80Zm80-160h120v-60q0-25-17.5-42.5T260-800q-25 0-42.5' +
          ' 17.5T200-740v60Z"/>'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Close'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="m291-2' +
          '40-51-51 189-189-189-189 51-51 189 189 189-189 51 51-189 189 189' +
          ' 189-51 51-189-189-189 189Z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Delete'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e6e6e6">'#13#10'  <path d="M280-1' +
          '20q-33 0-56.5-23.5T200-200v-520h-40v-80h200v-40h240v40h200v80h-4' +
          '0v520q0 33-23.5 56.5T680-120H280Zm400-600H280v520h400v-520ZM360-' +
          '280h80v-360h-80v360Zm160 0h80v-360h-80v360ZM280-720v520-520Z"/>'#13 +
          #10'</svg>'#13#10
      end
      item
        IconName = 'Indent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#95fff8" d="M2 4h1M2 5h2M0 6h5M2 7h2M2 ' +
          '8h1" />'#13#10'</svg>'
      end
      item
        IconName = 'UnIndent'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M7 0h1M2' +
          ' 2h4M7 2h9M7 4h9M7 5h9M7 7h6M7 8h6M2 10h4M7 10h9M2 12h4M7 12h2M7' +
          ' 14h1" />'#13#10'<path stroke="#95fff8" d="M2 4h1M1 5h2M0 6h5M1 7h2M2 ' +
          '8h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ResetJava'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#cdcdcd" d="M7 0h1" ' +
          '/>'#13#10'<path stroke="#323232" d="M3 1h1M3 4h1M3 7h1M7 7h1M5 8h1M3 1' +
          '0h1M7 10h1" />'#13#10'<path stroke="#7e7e7e" d="M4 1h3M4 10h3" />'#13#10'<pa' +
          'th stroke="#ffffff" d="M7 1h1M3 2h6M2 3h1M7 3h1M2 4h1M1 5h1M1 6h' +
          '1M9 6h1M2 7h1M8 7h1M2 8h1M8 8h1M3 9h5" />'#13#10'<path stroke="#333333' +
          '" d="M8 1h1" />'#13#10'<path stroke="#8d8d8d" d="M2 2h1M2 9h1M8 9h1" /' +
          '>'#13#10'<path stroke="#505050" d="M9 2h1" />'#13#10'<path stroke="#6e6e6e" ' +
          'd="M1 3h1M1 8h1M9 8h1" />'#13#10'<path stroke="#eeeeee" d="M3 3h1M3 8h' +
          '1M7 8h1" />'#13#10'<path stroke="#5f5f5f" d="M4 3h1M8 3h1M7 4h1M4 8h1M' +
          '6 8h1" />'#13#10'<path stroke="#414141" d="M5 3h2" />'#13#10'<path stroke="#' +
          'dedede" d="M1 4h1M1 7h1M9 7h1" />'#13#10'<path stroke="#abf5ab" d="M13' +
          ' 4h1M15 4h1M13 5h1M15 5h1M13 6h1M15 6h1M13 7h1M15 7h1M13 8h1M15 ' +
          '8h1M13 9h1M15 9h1M13 10h1M15 10h1M13 11h1M15 11h1M8 12h1M13 12h1' +
          '" />'#13#10'<path stroke="#41d140" d="M14 4h1M14 5h1M14 6h1M14 7h1M14 ' +
          '8h1M14 9h1M14 10h1M14 11h1M9 12h1M14 12h1M9 13h1M14 13h1M10 14h4' +
          'M11 15h1" />'#13#10'<path stroke="#bdbdbd" d="M2 5h1M8 6h1" />'#13#10'<path ' +
          'stroke="#adadad" d="M2 6h1" />'#13#10'<path stroke="#bdf8bd" d="M10 12' +
          'h1M15 12h1" />'#13#10'<path stroke="#d8fcd7" d="M8 13h1" />'#13#10'<path str' +
          'oke="#5cdf5b" d="M10 13h1M14 14h1" />'#13#10'<path stroke="#65e264" d=' +
          '"M13 13h1" />'#13#10'<path stroke="#e5ffe5" d="M15 13h1" />'#13#10'<path str' +
          'oke="#47d446" d="M9 14h1M12 15h1" />'#13#10'<path stroke="#6fe66e" d="' +
          'M10 15h1" />'#13#10'<path stroke="#90ef8f" d="M13 15h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'OpenClass'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e6e6e6" d="M1 1h13M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h13M1 6h1M13 6h1M1 7h1' +
          'M13 7h1M1 8h1M13 8h1M1 9h13M1 10h1M13 10h1M1 11h1M13 11h1M1 12h1' +
          'M13 12h1M1 13h13" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'ClassEdit'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#95fff8" d="M2 1h11M' +
          '1 2h1M13 2h1M1 3h1M13 3h1M1 4h1M13 4h1M1 5h1M13 5h1M1 6h1M13 6h1' +
          'M1 7h1M13 7h1M1 8h1M13 8h1M1 9h1M13 9h1M1 10h1M13 10h1M1 11h1M13' +
          ' 11h1M1 12h1M13 12h1M2 13h12" />'#13#10'<path stroke="#8eb3dd" d="M2 2' +
          'h11M2 3h1M2 4h1M2 5h1M2 6h1M2 7h1M2 8h1M2 9h1M2 10h1M2 11h1M2 12' +
          'h1" />'#13#10'<path stroke="#408acc" d="M3 3h10M3 4h2M11 4h2M3 5h2M8 5' +
          'h1M12 5h1M3 6h1M7 6h3M12 6h1M3 7h1M6 7h7M3 8h1M6 8h7M3 9h1M7 9h3' +
          'M12 9h1M3 10h2M8 10h1M12 10h1M3 11h2M11 11h2M3 12h10" />'#13#10'<path ' +
          'stroke="#468dcd" d="M5 4h1M5 11h1" />'#13#10'<path stroke="#a0c5e6" d=' +
          '"M6 4h1" />'#13#10'<path stroke="#f2f7fb" d="M7 4h1" />'#13#10'<path stroke=' +
          '"#cde1f1" d="M8 4h2M8 11h2" />'#13#10'<path stroke="#5497d1" d="M10 4h' +
          '1M10 11h1" />'#13#10'<path stroke="#c1d9ee" d="M5 5h1M5 10h1" />'#13#10'<pat' +
          'h stroke="#b2d0ea" d="M6 5h1M6 10h1" />'#13#10'<path stroke="#488fce" ' +
          'd="M7 5h1M7 10h1" />'#13#10'<path stroke="#73aada" d="M9 5h1" />'#13#10'<pat' +
          'h stroke="#e8f0f8" d="M10 5h1M10 10h1" />'#13#10'<path stroke="#438bcc' +
          '" d="M11 5h1" />'#13#10'<path stroke="#609ed4" d="M4 6h1" />'#13#10'<path st' +
          'roke="#f8fafc" d="M5 6h1M5 9h1M7 11h1" />'#13#10'<path stroke="#4990ce' +
          '" d="M6 6h1M6 9h1M11 10h1" />'#13#10'<path stroke="#70a8da" d="M10 6h1' +
          '" />'#13#10'<path stroke="#5094d0" d="M11 6h1" />'#13#10'<path stroke="#7eb1' +
          'dd" d="M4 7h1" />'#13#10'<path stroke="#d0e3f2" d="M5 7h1M5 8h1" />'#13#10'<' +
          'path stroke="#77acdb" d="M4 8h1" />'#13#10'<path stroke="#5d9cd4" d="M' +
          '4 9h1" />'#13#10'<path stroke="#94bee3" d="M10 9h1" />'#13#10'<path stroke="' +
          '#6da6d8" d="M11 9h1M9 10h1" />'#13#10'<path stroke="#aacbe9" d="M6 11h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'CreateStructogram'
        SVGText = 
          '<svg viewBox="0 0 16 16">'#13#10'  <path stroke="#e6e6e6" d="M1 1.5h14' +
          ' M14.5 1 v14 M15 14.5 h-14 M1.5 15 v-14 M1 5.5 h14 M8 5.5 v8.5 M' +
          '2 2 L8 5.5 M8 5.5 L14 2" />'#13#10'</svg>'
      end
      item
        IconName = 'Execute'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#e6e6e6" d="M3 0h2M3' +
          ' 1h1M5 1h1M3 2h1M6 2h1M3 3h1M7 3h1M3 4h1M8 4h1M3 5h1M9 5h1M3 6h1' +
          'M10 6h1M3 7h1M11 7h1M3 8h1M11 8h1M3 9h1M10 9h1M3 10h1M9 10h1M3 1' +
          '1h1M8 11h1M3 12h1M7 12h1M3 13h1M6 13h1M3 14h1M5 14h1" />'#13#10'<path ' +
          'stroke="#25e852" d="M4 1h1M4 2h2M4 3h3M4 4h4M4 5h5M4 6h6M4 7h7M4' +
          ' 8h7M4 9h6M4 10h5M4 11h4M4 12h3M4 13h2" />'#13#10'<path stroke="#25e85' +
          '1" d="M4 14h1" />'#13#10'<path stroke="#e6e6e6" d="M3 15h2" />'#13#10'</svg>' +
          #13#10
      end
      item
        IconName = 'Cut'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#616467" d="M13 0h1M' +
          '10 4h1" />'#13#10'<path stroke="#66696b" d="M2 1h1M13 1h1" />'#13#10'<path s' +
          'troke="#5e6264" d="M3 1h1" />'#13#10'<path stroke="#5f6265" d="M12 1h1' +
          '" />'#13#10'<path stroke="#5a5e62" d="M2 2h1" />'#13#10'<path stroke="#76767' +
          '6" d="M3 2h1M12 2h1M3 3h1M12 3h1M4 4h1M11 4h1M4 5h2M10 5h2M5 6h1' +
          'M10 6h1M6 7h1M9 7h1M7 8h2" />'#13#10'<path stroke="#4b5258" d="M4 2h1M' +
          '6 5h1" />'#13#10'<path stroke="#4c5358" d="M11 2h1M9 5h1" />'#13#10'<path st' +
          'roke="#5a5f62" d="M13 2h1" />'#13#10'<path stroke="#475055" d="M2 3h1"' +
          ' />'#13#10'<path stroke="#6f7071" d="M4 3h1M11 3h1" />'#13#10'<path stroke="' +
          '#3d464e" d="M5 3h1M10 3h1" />'#13#10'<path stroke="#474f55" d="M13 3h1' +
          '" />'#13#10'<path stroke="#6b6d6e" d="M3 4h1" />'#13#10'<path stroke="#60636' +
          '6" d="M5 4h1" />'#13#10'<path stroke="#6d6e6f" d="M12 4h1" />'#13#10'<path s' +
          'troke="#4e545a" d="M3 5h1" />'#13#10'<path stroke="#52585d" d="M12 5h1' +
          '" />'#13#10'<path stroke="#686a6c" d="M4 6h1M8 7h1" />'#13#10'<path stroke="' +
          '#707172" d="M6 6h1" />'#13#10'<path stroke="#3e474f" d="M7 6h2" />'#13#10'<p' +
          'ath stroke="#717272" d="M9 6h1" />'#13#10'<path stroke="#6d6e70" d="M1' +
          '1 6h1" />'#13#10'<path stroke="#424a51" d="M4 7h1" />'#13#10'<path stroke="#' +
          '727273" d="M5 7h1" />'#13#10'<path stroke="#676a6c" d="M7 7h1" />'#13#10'<pa' +
          'th stroke="#747474" d="M10 7h1" />'#13#10'<path stroke="#485055" d="M1' +
          '1 7h1" />'#13#10'<path stroke="#495056" d="M5 8h1" />'#13#10'<path stroke="#' +
          '727373" d="M6 8h1" />'#13#10'<path stroke="#747575" d="M9 8h1" />'#13#10'<pa' +
          'th stroke="#51575c" d="M10 8h1" />'#13#10'<path stroke="#3f5164" d="M6' +
          ' 9h1" />'#13#10'<path stroke="#43607e" d="M7 9h2" />'#13#10'<path stroke="#4' +
          '0576f" d="M9 9h1" />'#13#10'<path stroke="#3a536c" d="M2 10h1" />'#13#10'<pa' +
          'th stroke="#3c6c9f" d="M3 10h1" />'#13#10'<path stroke="#3e75af" d="M4' +
          ' 10h1M7 11h1M14 13h1" />'#13#10'<path stroke="#3d73aa" d="M5 10h1M10 1' +
          '4h1M13 14h1" />'#13#10'<path stroke="#3c6fa3" d="M6 10h1M9 10h1" />'#13#10'<' +
          'path stroke="#3e76af" d="M7 10h2M2 11h1M6 11h1M9 11h1M6 12h1M9 1' +
          '2h1M6 13h1M9 13h1" />'#13#10'<path stroke="#3d6fa4" d="M10 10h1M12 11h' +
          '1" />'#13#10'<path stroke="#3e74ae" d="M11 10h1" />'#13#10'<path stroke="#3d' +
          '6da0" d="M12 10h1" />'#13#10'<path stroke="#39546f" d="M13 10h1M8 13h1' +
          '" />'#13#10'<path stroke="#395067" d="M1 11h1" />'#13#10'<path stroke="#3c6f' +
          'a4" d="M3 11h1" />'#13#10'<path stroke="#3a5a79" d="M4 11h1" />'#13#10'<path' +
          ' stroke="#3b648d" d="M5 11h1" />'#13#10'<path stroke="#3d71a8" d="M8 1' +
          '1h1" />'#13#10'<path stroke="#3c6692" d="M10 11h1" />'#13#10'<path stroke="#' +
          '3b5978" d="M11 11h1" />'#13#10'<path stroke="#3d76af" d="M13 11h1" />'#13 +
          #10'<path stroke="#3a516a" d="M14 11h1" />'#13#10'<path stroke="#3c6998" ' +
          'd="M1 12h1" />'#13#10'<path stroke="#3d74ac" d="M2 12h1" />'#13#10'<path str' +
          'oke="#38495a" d="M3 12h1" />'#13#10'<path stroke="#384959" d="M5 12h1M' +
          '6 15h1" />'#13#10'<path stroke="#3c6d9e" d="M7 12h1M9 14h1" />'#13#10'<path ' +
          'stroke="#3b648e" d="M8 12h1" />'#13#10'<path stroke="#394c5e" d="M10 1' +
          '2h1" />'#13#10'<path stroke="#374a5a" d="M12 12h1" />'#13#10'<path stroke="#' +
          '3d74ad" d="M13 12h1" />'#13#10'<path stroke="#3c6a9b" d="M14 12h1M1 14' +
          'h1" />'#13#10'<path stroke="#3d74ae" d="M1 13h1" />'#13#10'<path stroke="#3d' +
          '6a99" d="M2 13h1M13 13h1" />'#13#10'<path stroke="#395776" d="M5 13h1"' +
          ' />'#13#10'<path stroke="#3a5f83" d="M7 13h1" />'#13#10'<path stroke="#3a577' +
          '5" d="M10 13h1M11 14h1" />'#13#10'<path stroke="#3d73ac" d="M2 14h1" /' +
          '>'#13#10'<path stroke="#395675" d="M3 14h1" />'#13#10'<path stroke="#3b5a7b"' +
          ' d="M4 14h1" />'#13#10'<path stroke="#3d73ab" d="M5 14h1" />'#13#10'<path st' +
          'roke="#3e71a8" d="M6 14h1" />'#13#10'<path stroke="#384757" d="M7 14h1' +
          '" />'#13#10'<path stroke="#395470" d="M12 14h1" />'#13#10'<path stroke="#3c6' +
          'b9c" d="M14 14h1" />'#13#10'<path stroke="#384957" d="M1 15h1" />'#13#10'<pa' +
          'th stroke="#3c638c" d="M2 15h1" />'#13#10'<path stroke="#3d70a5" d="M3' +
          ' 15h1M11 15h1" />'#13#10'<path stroke="#3c6ea3" d="M4 15h1" />'#13#10'<path ' +
          'stroke="#3a628a" d="M5 15h1M10 15h1" />'#13#10'<path stroke="#384754" ' +
          'd="M9 15h1" />'#13#10'<path stroke="#3d72a8" d="M12 15h1" />'#13#10'<path st' +
          'roke="#3b6590" d="M13 15h1" />'#13#10'<path stroke="#384858" d="M14 15' +
          'h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Undo'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#384f64" d="M4 2h1M2' +
          ' 7h1" />'#13#10'<path stroke="#3a4e62" d="M5 2h1" />'#13#10'<path stroke="#3' +
          '84350" d="M6 2h1" />'#13#10'<path stroke="#384959" d="M1 3h1" />'#13#10'<pat' +
          'h stroke="#3b6694" d="M2 3h1" />'#13#10'<path stroke="#3e75af" d="M3 3' +
          'h1" />'#13#10'<path stroke="#3e76af" d="M4 3h2M2 4h2M6 4h2M1 5h1M8 5h1' +
          'M1 6h1M9 6h1M14 6h1M1 7h1M10 7h1M14 7h2M1 8h1M11 8h1M14 8h2M1 9h' +
          '1M12 9h1M14 9h2M1 10h1M13 10h3M2 11h1M9 11h7M2 12h1M10 12h5" />'#13 +
          #10'<path stroke="#3d73aa" d="M6 3h1" />'#13#10'<path stroke="#3c6083" d=' +
          '"M7 3h1" />'#13#10'<path stroke="#374551" d="M8 3h1" />'#13#10'<path stroke=' +
          '"#384654" d="M0 4h1M10 9h1" />'#13#10'<path stroke="#3d70a5" d="M1 4h1' +
          'M7 5h1M10 6h1" />'#13#10'<path stroke="#3c70a6" d="M4 4h1" />'#13#10'<path s' +
          'troke="#3d73a9" d="M5 4h1" />'#13#10'<path stroke="#3c6ea2" d="M8 4h1M' +
          '0 6h1M12 8h1" />'#13#10'<path stroke="#384a5c" d="M9 4h1" />'#13#10'<path st' +
          'roke="#374450" d="M14 4h1" />'#13#10'<path stroke="#3b6086" d="M0 5h1"' +
          ' />'#13#10'<path stroke="#3d6fa3" d="M2 5h1" />'#13#10'<path stroke="#384d61' +
          '" d="M3 5h1M3 11h1" />'#13#10'<path stroke="#3a536d" d="M6 5h1" />'#13#10'<p' +
          'ath stroke="#3d70a6" d="M9 5h1" />'#13#10'<path stroke="#384a5a" d="M1' +
          '0 5h1" />'#13#10'<path stroke="#3d6b9b" d="M14 5h1M0 8h1" />'#13#10'<path st' +
          'roke="#384550" d="M15 5h1" />'#13#10'<path stroke="#395571" d="M2 6h1"' +
          ' />'#13#10'<path stroke="#394756" d="M7 6h1M11 10h1" />'#13#10'<path stroke=' +
          '"#3d6da0" d="M8 6h1" />'#13#10'<path stroke="#384958" d="M11 6h1" />'#13#10 +
          '<path stroke="#3b6a9b" d="M15 6h1M1 12h1" />'#13#10'<path stroke="#3d7' +
          '2a8" d="M0 7h1" />'#13#10'<path stroke="#374654" d="M8 7h1" />'#13#10'<path ' +
          'stroke="#3c6d9f" d="M9 7h1M10 8h1" />'#13#10'<path stroke="#3c6ea3" d=' +
          '"M11 7h1" />'#13#10'<path stroke="#384857" d="M12 7h1" />'#13#10'<path strok' +
          'e="#395675" d="M2 8h1" />'#13#10'<path stroke="#384753" d="M9 8h1" />'#13 +
          #10'<path stroke="#384757" d="M13 8h1" />'#13#10'<path stroke="#3b6084" d' +
          '="M0 9h1" />'#13#10'<path stroke="#3c628c" d="M2 9h1" />'#13#10'<path stroke' +
          '="#3e6da0" d="M11 9h1" />'#13#10'<path stroke="#3c6ea0" d="M13 9h1" />' +
          #13#10'<path stroke="#39536e" d="M0 10h1" />'#13#10'<path stroke="#3d71a7" ' +
          'd="M2 10h1" />'#13#10'<path stroke="#3c6ea1" d="M12 10h1" />'#13#10'<path st' +
          'roke="#374652" d="M0 11h1" />'#13#10'<path stroke="#3d75af" d="M1 11h1' +
          '" />'#13#10'<path stroke="#3c6a9a" d="M8 11h1" />'#13#10'<path stroke="#3a5a' +
          '7b" d="M3 12h1" />'#13#10'<path stroke="#384451" d="M8 12h1" />'#13#10'<path' +
          ' stroke="#3c6b9b" d="M9 12h1" />'#13#10'<path stroke="#3d74ae" d="M15 ' +
          '12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Redo'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#3a4e62" d="M10 2h1"' +
          ' />'#13#10'<path stroke="#384f64" d="M11 2h1M13 7h1" />'#13#10'<path stroke=' +
          '"#384755" d="M12 2h1" />'#13#10'<path stroke="#374551" d="M7 3h1" />'#13#10 +
          '<path stroke="#3c6083" d="M8 3h1" />'#13#10'<path stroke="#3d73aa" d="' +
          'M9 3h1" />'#13#10'<path stroke="#3e76af" d="M10 3h2M8 4h2M12 4h2M7 5h1' +
          'M14 5h1M1 6h1M6 6h1M14 6h1M0 7h2M5 7h1M14 7h1M0 8h2M4 8h1M14 8h1' +
          'M0 9h2M3 9h1M14 9h1M0 10h3M14 10h1M0 11h7M13 11h1M1 12h5M13 12h1' +
          '" />'#13#10'<path stroke="#3e75af" d="M12 3h1" />'#13#10'<path stroke="#3b66' +
          '94" d="M13 3h1" />'#13#10'<path stroke="#384959" d="M14 3h1" />'#13#10'<path' +
          ' stroke="#374450" d="M1 4h1" />'#13#10'<path stroke="#384a5c" d="M6 4h' +
          '1" />'#13#10'<path stroke="#3c6ea2" d="M7 4h1M15 6h1M3 8h1" />'#13#10'<path ' +
          'stroke="#3d73a9" d="M10 4h1" />'#13#10'<path stroke="#3c70a6" d="M11 4' +
          'h1" />'#13#10'<path stroke="#3d70a5" d="M14 4h1M8 5h1M5 6h1" />'#13#10'<path' +
          ' stroke="#384654" d="M15 4h1" />'#13#10'<path stroke="#384550" d="M0 5' +
          'h1" />'#13#10'<path stroke="#3d6b9b" d="M1 5h1M15 8h1" />'#13#10'<path strok' +
          'e="#384a5a" d="M5 5h1" />'#13#10'<path stroke="#3d70a6" d="M6 5h1" />'#13 +
          #10'<path stroke="#3a536d" d="M9 5h1" />'#13#10'<path stroke="#384d61" d=' +
          '"M12 5h1M12 11h1" />'#13#10'<path stroke="#3d6fa3" d="M13 5h1" />'#13#10'<pa' +
          'th stroke="#3b6086" d="M15 5h1" />'#13#10'<path stroke="#3b6a9b" d="M0' +
          ' 6h1M14 12h1" />'#13#10'<path stroke="#384958" d="M4 6h1" />'#13#10'<path st' +
          'roke="#3d6da0" d="M7 6h1" />'#13#10'<path stroke="#394756" d="M8 6h1M4' +
          ' 10h1" />'#13#10'<path stroke="#395571" d="M13 6h1" />'#13#10'<path stroke="' +
          '#384857" d="M3 7h1" />'#13#10'<path stroke="#3c6ea3" d="M4 7h1" />'#13#10'<p' +
          'ath stroke="#3c6d9f" d="M6 7h1M5 8h1" />'#13#10'<path stroke="#374654"' +
          ' d="M7 7h1" />'#13#10'<path stroke="#3d72a8" d="M15 7h1" />'#13#10'<path str' +
          'oke="#384757" d="M2 8h1" />'#13#10'<path stroke="#384753" d="M6 8h1" /' +
          '>'#13#10'<path stroke="#395675" d="M13 8h1" />'#13#10'<path stroke="#3c6ea0"' +
          ' d="M2 9h1" />'#13#10'<path stroke="#3e6da0" d="M4 9h1" />'#13#10'<path stro' +
          'ke="#384754" d="M5 9h1" />'#13#10'<path stroke="#3c628c" d="M13 9h1" /' +
          '>'#13#10'<path stroke="#3b6084" d="M15 9h1" />'#13#10'<path stroke="#3c6ea1"' +
          ' d="M3 10h1" />'#13#10'<path stroke="#3d71a7" d="M13 10h1" />'#13#10'<path s' +
          'troke="#39536e" d="M15 10h1" />'#13#10'<path stroke="#3c6a9a" d="M7 11' +
          'h1" />'#13#10'<path stroke="#3d75af" d="M14 11h1" />'#13#10'<path stroke="#3' +
          '74652" d="M15 11h1" />'#13#10'<path stroke="#3d74ac" d="M0 12h1" />'#13#10'<' +
          'path stroke="#3c6b9b" d="M6 12h1" />'#13#10'<path stroke="#384451" d="' +
          'M7 12h1" />'#13#10'<path stroke="#3a5a7b" d="M12 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Search'
        SVGText = 
          '<svg viewBox="0 0 32 32" fill="#191919">'#13#10#9'<path d="M-0.1,29.3l2' +
          '.8,2.8l9.2-9.2v-1.5l0.6-0.6c2,1.8,4.8,2.9,7.7,2.9c6.6,0,11.9-5.3' +
          ',11.9-11.9s-5.4-11.9-12-11.9'#13#10#9#9'S8.2,5.2,8.2,11.8c0,2.9,1.1,5.7,' +
          '2.9,7.7l-0.6,0.6H9L-0.1,29.3z M11.8,11.8c0-4.6,3.7-8.3,8.3-8.3c4' +
          '.6,0,8.3,3.7,8.3,8.3'#13#10#9#9's-3.7,8.3-8.3,8.3C15.5,20.1,11.8,16.4,11' +
          '.8,11.8z"/>'#13#10'</svg>'
      end
      item
        IconName = 'Search'
        SVGText = 
          '<svg viewBox="0 0 32 32" fill="#e9e9e9">'#13#10#9'<path d="M-0.1,29.3l2' +
          '.8,2.8l9.2-9.2v-1.5l0.6-0.6c2,1.8,4.8,2.9,7.7,2.9c6.6,0,11.9-5.3' +
          ',11.9-11.9s-5.4-11.9-12-11.9'#13#10#9#9'S8.2,5.2,8.2,11.8c0,2.9,1.1,5.7,' +
          '2.9,7.7l-0.6,0.6H9L-0.1,29.3z M11.8,11.8c0-4.6,3.7-8.3,8.3-8.3c4' +
          '.6,0,8.3,3.7,8.3,8.3'#13#10#9#9's-3.7,8.3-8.3,8.3C15.5,20.1,11.8,16.4,11' +
          '.8,11.8z"/>'#13#10'</svg>'
      end
      item
        IconName = 'SuggestAssistant'
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
      end
      item
        IconName = 'SuggestAssistant'
        SVGText = 
          '<svg viewBox="0 -960 960 960" fill="#e9e9e9">'#13#10'    <circle r="70' +
          '" cx="360" cy="-640" fill="#E24444" /> '#13#10'    <circle r="70" cx="' +
          '600" cy="-640" fill="#E24444" /> '#13#10#13#10'    <path'#13#10'        d="M160-' +
          '120v-200q0-33 23.5-56.5T240-400h480q33 0 56.5 23.5T800-320v200H1' +
          '60Zm200-320'#13#10'        q-83 0-141.5-58.5T160-640q0-83 58.5-141.5T3' +
          '60-840h240q83 0 141.5 58.5T800-640'#13#10'        q0 83-58.5 141.5T600' +
          '-440H360Z'#13#10'        M240-200h480v-120H240v120Zm120-320h240q50 0 8' +
          '5-35t35-85q0-50-35-85t-85-35H360'#13#10'        q-50 0-85 35t-35 85q0 ' +
          '50 35 85t85 35Z" />'#13#10'</svg>'
      end
      item
        IconName = 'SuggestCancel'
        SVGText = 
          '<svg height="24px" viewBox="0 -960 960 960" width="24px" >'#13#10'    ' +
          '<path fill="#E24444"'#13#10'        d="m336-280 144-144 144 144 56-56-' +
          '144-144 144-144-56-56-144 144-144-144-56 56 144 144-144 144 56 5' +
          '6ZM480-80q-83 0-156-31.5T197-197q-54-54-85.5-127T80-480q0-83 31.' +
          '5-156T197-763q54-54 127-85.5T480-880q83 0 156 31.5T763-763q54 54' +
          ' 85.5 127T880-480q0 83-31.5 156T763-197q-54 54-127 85.5T480-80Zm' +
          '0-80q134 0 227-93t93-227q0-134-93-227t-227-93q-134 0-227 93t-93 ' +
          '227q0 134 93 227t227 93Zm0-320Z" />'#13#10'</svg>'
      end>
    Left = 376
    Top = 159
  end
  object vilContextMenuLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Indent'
        Name = 'Indent'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Unindent'
        Name = 'Unindent'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Undo'
        Name = 'Undo'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Redo'
        Name = 'Redo'
      end
      item
        CollectionIndex = 10
        CollectionName = 'APIHelp'
        Name = 'APIHelp'
      end
      item
        CollectionIndex = 11
        CollectionName = 'ResetJava'
        Name = 'ResetJava'
      end
      item
        CollectionIndex = 12
        CollectionName = 'OpenClass'
        Name = 'OpenClass'
      end
      item
        CollectionIndex = 13
        CollectionName = 'ClassEdit'
        Name = 'ClassEdit'
      end
      item
        CollectionIndex = 14
        CollectionName = 'CreateStructogram'
        Name = 'CreateStructogram'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Execute'
        Name = 'Execute'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Git'
        Name = 'Git'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Configuration'
        Name = 'Configuration'
      end
      item
        CollectionIndex = 31
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 33
        CollectionName = 'SuggestAssistant'
        Name = 'SuggestAssistant'
      end
      item
        CollectionIndex = 35
        CollectionName = 'SuggestCancel'
        Name = 'SuggestCancel'
      end>
    ImageCollection = icContextMenu
    Left = 72
    Top = 159
  end
  object vilContextMenuDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 18
        CollectionName = 'Font'
        Name = 'Font'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Close'
        Name = 'Close'
      end
      item
        CollectionIndex = 28
        CollectionName = 'Cut'
        Name = 'Cut'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Copy'
        Name = 'Copy'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Paste'
        Name = 'Paste'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Delete'
        Name = 'Delete'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Indent'
        Name = 'Indent'
      end
      item
        CollectionIndex = 22
        CollectionName = 'UnIndent'
        Name = 'UnIndent'
      end
      item
        CollectionIndex = 29
        CollectionName = 'Undo'
        Name = 'Undo'
      end
      item
        CollectionIndex = 30
        CollectionName = 'Redo'
        Name = 'Redo'
      end
      item
        CollectionIndex = 10
        CollectionName = 'APIHelp'
        Name = 'APIHelp'
      end
      item
        CollectionIndex = 23
        CollectionName = 'ResetJava'
        Name = 'ResetJava'
      end
      item
        CollectionIndex = 24
        CollectionName = 'OpenClass'
        Name = 'OpenClass'
      end
      item
        CollectionIndex = 13
        CollectionName = 'ClassEdit'
        Name = 'ClassEdit'
      end
      item
        CollectionIndex = 26
        CollectionName = 'CreateStructogram'
        Name = 'CreateStructogram'
      end
      item
        CollectionIndex = 15
        CollectionName = 'Execute'
        Name = 'Execute'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Git'
        Name = 'Git'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Configuration'
        Name = 'Configuration'
      end
      item
        CollectionIndex = 32
        CollectionName = 'Search'
        Name = 'Search'
      end
      item
        CollectionIndex = 34
        CollectionName = 'SuggestAssistant'
        Name = 'SuggestAssistant'
      end
      item
        CollectionIndex = 35
        CollectionName = 'SuggestCancel'
        Name = 'SuggestCancel'
      end>
    ImageCollection = icContextMenu
    Left = 224
    Top = 159
  end
  object PopUpEditor: TSpTBXPopupMenu
    Images = vilContextMenuLight
    OnPopup = PopUpEditorPopup
    Left = 504
    Top = 159
    object MISearchDeclaration: TSpTBXItem
      Caption = 'Search declaration'
      ImageIndex = 18
      ImageName = 'Search'
      OnClick = MISearchDeclarationClick
    end
    object MIAPIHelp: TSpTBXItem
      Caption = 'API help'
      ImageIndex = 10
      ImageName = 'APIHelp'
      OnClick = MIAPIHelpClick
    end
    object MIClassOpen: TSpTBXItem
      Caption = 'Open class'
      ImageIndex = 12
      ImageName = 'OpenClass'
      OnClick = SBClassOpenClick
    end
    object MIClassEditor: TSpTBXItem
      Caption = 'Class editor'
      ImageIndex = 13
      ImageName = 'ClassEdit'
      OnClick = MIClassEditorClick
    end
    object MICreateStructogram: TSpTBXItem
      Caption = 'Create structogram'
      ImageIndex = 14
      ImageName = 'CreateStructogram'
      OnClick = MICreateStructogramClick
    end
    object MIAssistant: TSpTBXSubmenuItem
      Caption = 'Assistant'
      ImageIndex = 19
      ImageName = 'SuggestAssistant'
      object mnAssistantExplain: TSpTBXItem
        Caption = 'Explain'
        Hint = 'Add comments explaining the selected code'
        OnClick = mnAssistantExplainClick
      end
      object mnAssistantFixBugs: TSpTBXItem
        Caption = 'Fix Bugs'
        Hint = 'Fix bugs in the selected code'
        OnClick = mnAssistantFixBugsClick
      end
      object mnAssistantOptimize: TSpTBXItem
        Caption = 'Optimize'
        Hint = 'Optimize the selected code'
        OnClick = mnAssistantOptimizeClick
      end
      object mnAssistanSuggest: TSpTBXItem
        Caption = 'Suggest'
        Hint = 'Provide a suggestion for code completion'
        ShortCut = 49184
        OnClick = mnAssistanSuggestClick
      end
      object SpTBXSeparatorItem1: TSpTBXSeparatorItem
      end
      object mnAssistantCancel: TSpTBXItem
        Caption = 'Cancel'
        Hint = 'Cancel Assistant action'
        ImageIndex = 20
        ImageName = 'SuggestCancel'
        OnClick = mnAssistantCancelClick
      end
    end
    object N1: TSpTBXSeparatorItem
    end
    object MIExecute: TSpTBXItem
      Caption = 'Execute'
      ImageIndex = 15
      ImageName = 'Execute'
      OnClick = MIExecuteClick
    end
    object MIExecuteWithoutConsole: TSpTBXItem
      Caption = '  without console'
      OnClick = MIExecuteWithoutConsoleClick
    end
    object MIExecuteWithConsole: TSpTBXItem
      Caption = '  with console'
      OnClick = MIExecuteWithConsoleClick
    end
    object N2: TSpTBXSeparatorItem
    end
    object MIGit: TSpTBXSubmenuItem
      Caption = 'Git'
      ImageIndex = 16
      ImageName = 'Git'
      object MIGitStatus: TSpTBXItem
        Caption = 'Status'
        OnClick = MIGitStatusClick
      end
      object MIGitAdd: TSpTBXItem
        Caption = 'Add'
        OnClick = MIGitAddClick
      end
      object MICommit: TSpTBXItem
        Caption = 'Commit'
        OnClick = MICommitClick
      end
      object MIGitLog: TSpTBXItem
        Caption = 'Log'
        OnClick = MIGitLogClick
      end
      object N3: TSpTBXSeparatorItem
      end
      object MGitReset: TSpTBXItem
        Caption = 'Reset'
        OnClick = MGitResetClick
      end
      object MIGitCheckout: TSpTBXItem
        Caption = 'Checkout'
        OnClick = MIGitCheckoutClick
      end
      object MIGitRemove: TSpTBXItem
        Caption = 'Remove'
        OnClick = MIGitRemoveClick
      end
      object N4: TSpTBXSeparatorItem
      end
      object MIGitRemote: TSpTBXItem
        Caption = 'Remote'
        OnClick = MIGitRemoteClick
      end
      object MIGitFetch: TSpTBXItem
        Caption = 'Fetch'
        OnClick = MIGitFetchClick
      end
      object MIGitPush: TSpTBXItem
        Caption = 'Push'
        OnClick = MIGitPushClick
      end
      object N5: TSpTBXSeparatorItem
      end
      object MIGitGui: TSpTBXItem
        Caption = 'GUI'
        OnClick = MIGitGuiClick
      end
      object MIGitViewer: TSpTBXItem
        Caption = 'Viewer'
        OnClick = MIGitViewerClick
      end
      object MIGitConsole: TSpTBXItem
        Caption = 'Console'
        OnClick = MIGitConsoleClick
      end
    end
    object MIRenewImports: TSpTBXItem
      Caption = 'Renew imports'
      OnClick = MIRenewImportsClick
    end
    object MICopyPath: TSpTBXItem
      Caption = 'Copy path'
      OnClick = MICopyPathClick
    end
    object MILine1: TSpTBXSeparatorItem
    end
    object MIUndo: TSpTBXItem
      Caption = 'Undo'
      ImageIndex = 8
      ImageName = 'Undo'
      ShortCut = 16474
      OnClick = MIUndoClick
    end
    object MIRedo: TSpTBXItem
      Caption = 'Redo'
      ImageIndex = 9
      ImageName = 'Redo'
      OnClick = MIRedoClick
    end
    object MILine2: TSpTBXSeparatorItem
    end
    object MICut: TSpTBXItem
      Caption = 'Cut'
      ImageIndex = 2
      ImageName = 'Cut'
      ShortCut = 16472
      OnClick = MICutClick
    end
    object MICopy: TSpTBXItem
      Caption = 'Copy'
      ImageIndex = 3
      ImageName = 'Copy'
      ShortCut = 16451
      OnClick = MICopyClick
    end
    object MIInsert: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 4
      ImageName = 'Paste'
      ShortCut = 16470
      OnClick = MIInsertClick
    end
    object MILine3: TSpTBXSeparatorItem
    end
    object MIIndent: TSpTBXItem
      Caption = 'Indent'
      ImageIndex = 6
      ImageName = 'Indent'
      OnClick = MIIndentClick
    end
    object MIUnindent: TSpTBXItem
      Caption = 'Unindent'
      ImageIndex = 7
      ImageName = 'Unindent'
      OnClick = MIUnindentClick
    end
    object MILine4: TSpTBXSeparatorItem
    end
    object MIReleaseWindow: TSpTBXItem
      Caption = 'Release window'
      Visible = False
      OnClick = MIReleaseWindowClick
    end
    object MIFont: TSpTBXItem
      Caption = 'Font'
      ImageIndex = 0
      ImageName = 'Font'
      OnClick = MIFontClick
    end
    object MIConfiguration: TSpTBXItem
      Caption = 'Configuration'
      ImageIndex = 17
      ImageName = 'Configuration'
      OnClick = MIConfigurationClick
    end
    object MIClose: TSpTBXItem
      Caption = 'Close'
      ImageIndex = 1
      ImageName = 'Close'
      ShortCut = 16499
      OnClick = MICloseClick
    end
  end
  object icBookmarks: TSVGIconImageCollection
    SVGIconItems = <
      item
        IconName = 'Bookmark0'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M4 8h1M6 8h1M8 8h1M10 8h1' +
          'M7 9h1M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1' +
          'M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1' +
          'M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M7 8h1M4 9h1M6 9h1M8' +
          ' 9h1M10 9h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 ' +
          '13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3' +
          'M5 6h1M9 6h1M5 7h1M9 7h1M5 8h1M9 8h1M5 9h1M9 9h1M5 10h1M9 10h1M5' +
          ' 11h1M9 11h1M6 12h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M8 6h1M10 6h1M9 7h1M4 8h1M6 8h1M8 8h1M10 8h1M5 9h1' +
          'M9 9h1M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M9 11h1M4 12h1M10 12h1' +
          'M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h' +
          '1M9 4h1M4 5h1M6 5h1M8 5h1M10 5h1M5 6h1M9 6h1M4 7h1M6 7h1M8 7h1M1' +
          '0 7h1M5 8h1M9 8h1M4 9h1M6 9h1M8 9h1M10 9h1M5 10h1M9 10h1M4 11h1M' +
          '6 11h1M8 11h1M10 11h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path st' +
          'roke="#ffff00" d="M7 5h1M6 6h2M5 7h1M7 7h1M7 8h1M7 9h1M7 10h1M7 ' +
          '11h1M5 12h5" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark2'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1' +
          'M5 9h1M9 9h1M4 10h1M8 10h1M10 10h1M7 11h1M9 11h1M4 12h1M10 12h1M' +
          '5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1' +
          'M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M5 8h1M7 8h1M9' +
          ' 8h1M4 9h1M6 9h1M8 9h1M10 9h1M5 10h1M7 10h1M9 10h1M4 11h1M6 11h1' +
          'M8 11h1M10 11h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="' +
          '#ffff00" d="M6 5h3M5 6h1M9 6h1M9 7h1M8 8h1M7 9h1M6 10h1M5 11h1M5' +
          ' 12h5" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark3'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1' +
          'M5 9h1M7 9h1M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M' +
          '5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1' +
          'M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M5 8h1M9 8h1M4' +
          ' 9h1M6 9h1M8 9h1M10 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11' +
          'h1M5 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke=' +
          '"#ffff00" d="M6 5h3M5 6h1M9 6h1M9 7h1M7 8h2M9 9h1M9 10h1M5 11h1M' +
          '9 11h1M6 12h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark4'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M7 5' +
          'h1M9 5h1M4 6h1M6 6h1M10 6h1M7 7h1M9 7h1M4 8h1M6 8h1M10 8h1M4 10h' +
          '1M6 10h1M10 10h1M5 11h1M7 11h1M9 11h1M4 12h1M6 12h1M10 12h1M5 13' +
          'h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4' +
          'h1M4 5h1M6 5h1M10 5h1M7 6h1M9 6h1M4 7h1M6 7h1M10 7h1M7 8h1M9 8h1' +
          'M4 9h1M10 9h1M5 10h1M7 10h1M9 10h1M4 11h1M6 11h1M10 11h1M5 12h1M' +
          '7 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#f' +
          'fff00" d="M5 5h1M8 5h1M5 6h1M8 6h1M5 7h1M8 7h1M5 8h1M8 8h1M5 9h5' +
          'M8 10h1M8 11h1M8 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark5'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M4 6' +
          'h1M6 6h1M8 6h1M10 6h1M7 7h1M9 7h1M4 8h1M10 8h1M5 9h1M7 9h1M4 10h' +
          '1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h1M9 13' +
          'h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h' +
          '1M7 6h1M9 6h1M4 7h1M6 7h1M8 7h1M10 7h1M9 8h1M4 9h1M6 9h1M8 9h1M1' +
          '0 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4' +
          ' 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M5 5h' +
          '5M5 6h1M5 7h1M5 8h4M9 9h1M9 10h1M5 11h1M9 11h1M6 12h3" />'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'Bookmark6'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M9 7h1M4 8h1M10 8h1M7 9h1' +
          'M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h' +
          '1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1' +
          'M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M9 8h1M4 9h1M6 9h1M8 9h1M1' +
          '0 9h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6' +
          ' 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3M5 6h1' +
          'M9 6h1M5 7h1M5 8h4M5 9h1M9 9h1M5 10h1M9 10h1M5 11h1M9 11h1M6 12h' +
          '3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark7'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M4 6' +
          'h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1M5 9h1M7 9h1' +
          'M9 9h1M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M9 11h1M4 12h1M6 12h1M' +
          '8 12h1M10 12h1M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" ' +
          'd="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h' +
          '1M5 8h1M7 8h1M9 8h1M4 9h1M6 9h1M10 9h1M5 10h1M9 10h1M4 11h1M6 11' +
          'h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />' +
          #13#10'<path stroke="#ffff00" d="M5 5h5M5 6h1M9 6h1M9 7h1M8 8h1M8 9h1' +
          'M7 10h1M7 11h1M7 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark8'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M10 8h1M7 9h1M4 10h1M6 10' +
          'h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h1M9 13h1" />' +
          #13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h' +
          '1M4 7h1M6 7h1M8 7h1M10 7h1M4 8h2M9 8h1M4 9h1M6 9h1M8 9h1M10 9h1M' +
          '7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6 13h1M' +
          '8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3M5 6h1M9 6h1' +
          'M5 7h1M9 7h1M6 8h3M5 9h1M9 9h1M5 10h1M9 10h1M5 11h1M9 11h1M6 12h' +
          '3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark9'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#0000ff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M4 8h1M6 8h1M10 8h1M5 9h1' +
          'M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M7 11h1M4 12h1M6 12h1M8 12h1' +
          'M10 12h1M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 ' +
          '4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M7 8h' +
          '1M4 9h1M8 9h1M10 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M' +
          '5 12h1M7 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#f' +
          'fff00" d="M6 5h3M5 6h1M9 6h1M5 7h1M9 7h1M5 8h1M8 8h2M6 9h2M9 9h1' +
          'M9 10h1M9 11h1M9 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Warning'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828200" d="M6 3h4M5' +
          ' 4h1M10 4h1M5 5h1M10 5h1M4 7h1M11 7h1M3 9h1M12 9h1M2 10h1M13 10h' +
          '1M2 11h1M13 11h1M2 12h1M13 12h1M2 13h1M3 14h10" />'#13#10'<path stroke' +
          '="#828282" d="M6 4h1M9 4h1M4 6h1M11 6h1M3 8h1M12 8h1" />'#13#10'<path ' +
          'stroke="#ffff00" d="M7 4h2M6 5h4M6 6h1M9 6h1M5 7h2M9 7h2M5 8h2M9' +
          ' 8h2M4 9h3M9 9h3M4 10h3M9 10h3M3 11h10M3 12h4M9 12h4M4 13h8" />'#13 +
          #10'<path stroke="#c4c4c4" d="M5 6h1M10 6h1M4 8h1M11 8h1M3 10h1M12 ' +
          '10h1M3 13h1M12 13h1" />'#13#10'<path stroke="#000000" d="M7 6h2M7 7h2M' +
          '7 8h2M7 9h2M7 10h2M7 12h2" />'#13#10'<path stroke="#946f2a" d="M13 13h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Question'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#11ffff" d="M6 2h3M5' +
          ' 3h1M4 4h1M4 5h1M8 5h1M8 6h1M7 7h1M6 8h1M6 9h1M6 10h1M6 12h1" />' +
          #13#10'<path stroke="#0000ff" d="M6 3h3M5 4h1M8 4h2M5 5h1M9 5h1M9 6h1' +
          'M8 7h1M7 8h1M7 9h1M7 10h1M7 12h1" />'#13#10'<path stroke="#000082" d="' +
          'M9 3h1M6 4h2M10 4h1M6 5h1M10 5h1M5 6h2M10 6h1M9 7h1M8 8h1M8 9h1M' +
          '8 10h1M8 12h1M7 13h2" />'#13#10'<path stroke="#c4c4c4" d="M7 5h1M7 6h1' +
          '" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Debugline'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M6 2h1M6' +
          ' 3h2M6 4h1M8 4h1M3 5h4M9 5h1M3 6h1M10 6h1M3 7h1M11 7h1M3 8h1M10 ' +
          '8h1M3 9h4M9 9h1M6 10h1M8 10h1M6 11h2M6 12h1" />'#13#10'<path stroke="#' +
          'fe0000" d="M7 4h1M7 5h2M4 6h6M4 7h7M4 8h6M7 9h2M7 10h1" />'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'Breakpoint'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h2M12 5h1M4 6h2M9 6h3M3 8h2M10 8h3M10 9h1M1' +
          '0 10h1M4 11h2M10 11h2M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h' +
          '1" />'#13#10'<path stroke="#088200" d="M7 2h2M8 3h1M3 4h1M7 4h1M12 4h1' +
          'M4 5h1M6 5h2M11 5h1M6 6h1M5 7h1M8 7h2M5 8h1M8 8h2M2 9h2M5 9h1M8 ' +
          '9h2M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 12h1M2 13h1M6 1' +
          '3h1M13 13h1" />'#13#10'<path stroke="#820000" d="M6 3h1M9 3h1M6 4h1M9 ' +
          '4h1" />'#13#10'<path stroke="#11ff00" d="M7 3h1M7 6h2M6 7h2M6 8h2M6 9h' +
          '2M6 10h1M6 11h1" />'#13#10'<path stroke="#fe0000" d="M5 4h1M10 4h1" />' +
          #13#10'<path stroke="#c4c4c4" d="M5 5h1M10 5h1" />'#13#10'<path stroke="#08' +
          '8282" d="M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'BreakpointsDelete'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#000000" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h1M12 5h1M4 6h1M10 6h2M3 8h2M10 8h3M4 11h2M' +
          '10 11h1M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h1" />'#13#10'<path s' +
          'troke="#088200" d="M7 2h2M8 3h1M7 4h1M12 4h1M6 5h2M11 5h1M5 7h1M' +
          '9 7h1M2 9h2M8 9h1M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 1' +
          '2h1M2 13h1M6 13h1M13 13h1" />'#13#10'<path stroke="#ffff00" d="M3 3h1M' +
          '11 3h1M3 4h2M10 4h2M4 5h2M9 5h2M5 6h2M8 6h2M6 7h3M5 8h2M8 8h2M4 ' +
          '9h2M9 9h2M3 10h2M10 10h2M3 11h1M11 11h1" />'#13#10'<path stroke="#8200' +
          '00" d="M6 3h1M9 3h1M6 4h1M9 4h1" />'#13#10'<path stroke="#11ff00" d="M' +
          '7 3h1M7 6h1M7 8h1M6 9h2M6 10h1M6 11h1" />'#13#10'<path stroke="#fe0000' +
          '" d="M5 4h1" />'#13#10'<path stroke="#088282" d="M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark0'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M4 8h1M6 8h1M8 8h1M10 8h1' +
          'M7 9h1M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1' +
          'M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1' +
          'M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M7 8h1M4 9h1M6 9h1M8' +
          ' 9h1M10 9h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 ' +
          '13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3' +
          'M5 6h1M9 6h1M5 7h1M9 7h1M5 8h1M9 8h1M5 9h1M9 9h1M5 10h1M9 10h1M5' +
          ' 11h1M9 11h1M6 12h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark1'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M8 6h1M10 6h1M9 7h1M4 8h1M6 8h1M8 8h1M10 8h1M5 9h1' +
          'M9 9h1M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M9 11h1M4 12h1M10 12h1' +
          'M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h' +
          '1M9 4h1M4 5h1M6 5h1M8 5h1M10 5h1M5 6h1M9 6h1M4 7h1M6 7h1M8 7h1M1' +
          '0 7h1M5 8h1M9 8h1M4 9h1M6 9h1M8 9h1M10 9h1M5 10h1M9 10h1M4 11h1M' +
          '6 11h1M8 11h1M10 11h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path st' +
          'roke="#ffff00" d="M7 5h1M6 6h2M5 7h1M7 7h1M7 8h1M7 9h1M7 10h1M7 ' +
          '11h1M5 12h5" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark2'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1' +
          'M5 9h1M9 9h1M4 10h1M8 10h1M10 10h1M7 11h1M9 11h1M4 12h1M10 12h1M' +
          '5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1' +
          'M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M5 8h1M7 8h1M9' +
          ' 8h1M4 9h1M6 9h1M8 9h1M10 9h1M5 10h1M7 10h1M9 10h1M4 11h1M6 11h1' +
          'M8 11h1M10 11h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="' +
          '#ffff00" d="M6 5h3M5 6h1M9 6h1M9 7h1M8 8h1M7 9h1M6 10h1M5 11h1M5' +
          ' 12h5" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark3'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1' +
          'M5 9h1M7 9h1M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M' +
          '5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1' +
          'M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M5 8h1M9 8h1M4' +
          ' 9h1M6 9h1M8 9h1M10 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11' +
          'h1M5 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke=' +
          '"#ffff00" d="M6 5h3M5 6h1M9 6h1M9 7h1M7 8h2M9 9h1M9 10h1M5 11h1M' +
          '9 11h1M6 12h3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark4'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M7 5' +
          'h1M9 5h1M4 6h1M6 6h1M10 6h1M7 7h1M9 7h1M4 8h1M6 8h1M10 8h1M4 10h' +
          '1M6 10h1M10 10h1M5 11h1M7 11h1M9 11h1M4 12h1M6 12h1M10 12h1M5 13' +
          'h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4' +
          'h1M4 5h1M6 5h1M10 5h1M7 6h1M9 6h1M4 7h1M6 7h1M10 7h1M7 8h1M9 8h1' +
          'M4 9h1M10 9h1M5 10h1M7 10h1M9 10h1M4 11h1M6 11h1M10 11h1M5 12h1M' +
          '7 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#f' +
          'fff00" d="M5 5h1M8 5h1M5 6h1M8 6h1M5 7h1M8 7h1M5 8h1M8 8h1M5 9h5' +
          'M8 10h1M8 11h1M8 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark5'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M4 6' +
          'h1M6 6h1M8 6h1M10 6h1M7 7h1M9 7h1M4 8h1M10 8h1M5 9h1M7 9h1M4 10h' +
          '1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h1M9 13' +
          'h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h' +
          '1M7 6h1M9 6h1M4 7h1M6 7h1M8 7h1M10 7h1M9 8h1M4 9h1M6 9h1M8 9h1M1' +
          '0 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4' +
          ' 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M5 5h' +
          '5M5 6h1M5 7h1M5 8h4M9 9h1M9 10h1M5 11h1M9 11h1M6 12h3" />'#13#10'</svg' +
          '>'#13#10
      end
      item
        IconName = 'Bookmark6'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M9 7h1M4 8h1M10 8h1M7 9h1' +
          'M4 10h1M6 10h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h' +
          '1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1' +
          'M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M9 8h1M4 9h1M6 9h1M8 9h1M1' +
          '0 9h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6' +
          ' 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3M5 6h1' +
          'M9 6h1M5 7h1M5 8h4M5 9h1M9 9h1M5 10h1M9 10h1M5 11h1M9 11h1M6 12h' +
          '3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark7'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M4 6' +
          'h1M6 6h1M8 6h1M10 6h1M5 7h1M7 7h1M4 8h1M6 8h1M10 8h1M5 9h1M7 9h1' +
          'M9 9h1M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M9 11h1M4 12h1M6 12h1M' +
          '8 12h1M10 12h1M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" ' +
          'd="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h' +
          '1M5 8h1M7 8h1M9 8h1M4 9h1M6 9h1M10 9h1M5 10h1M9 10h1M4 11h1M6 11' +
          'h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />' +
          #13#10'<path stroke="#ffff00" d="M5 5h5M5 6h1M9 6h1M9 7h1M8 8h1M8 9h1' +
          'M7 10h1M7 11h1M7 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark8'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M10 8h1M7 9h1M4 10h1M6 10' +
          'h1M8 10h1M10 10h1M7 11h1M4 12h1M10 12h1M5 13h1M7 13h1M9 13h1" />' +
          #13#10'<path stroke="#088282" d="M5 4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h' +
          '1M4 7h1M6 7h1M8 7h1M10 7h1M4 8h2M9 8h1M4 9h1M6 9h1M8 9h1M10 9h1M' +
          '7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M5 12h1M9 12h1M4 13h1M6 13h1M' +
          '8 13h1M10 13h1" />'#13#10'<path stroke="#ffff00" d="M6 5h3M5 6h1M9 6h1' +
          'M5 7h1M9 7h1M6 8h3M5 9h1M9 9h1M5 10h1M9 10h1M5 11h1M9 11h1M6 12h' +
          '3" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Bookmark9'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#1adaff" d="M4 1h9M3' +
          ' 2h1M13 2h1M2 3h10M13 3h1M2 4h1M11 4h1M13 4h1M2 5h1M11 5h1M13 5h' +
          '1M2 6h1M11 6h1M13 6h1M2 7h1M11 7h1M13 7h1M2 8h1M11 8h1M13 8h1M2 ' +
          '9h1M11 9h1M13 9h1M2 10h1M11 10h1M13 10h1M2 11h1M11 11h1M13 11h1M' +
          '2 12h1M11 12h1M13 12h1M2 13h1M11 13h2M2 14h10" />'#13#10'<path stroke=' +
          '"#c4c4c4" d="M4 2h8M3 4h1M3 5h1M3 6h1M3 7h1M3 8h1M3 9h1M3 10h1M3' +
          ' 11h1M3 12h1M3 13h1" />'#13#10'<path stroke="#828282" d="M12 2h1M12 3h' +
          '1M12 4h1M12 5h1M12 6h1M12 7h1M12 8h1M12 9h1M12 10h1M12 11h1M12 1' +
          '2h1" />'#13#10'<path stroke="#088200" d="M4 4h1M6 4h1M8 4h1M10 4h1M5 5' +
          'h1M9 5h1M4 6h1M6 6h1M8 6h1M10 6h1M7 7h1M4 8h1M6 8h1M10 8h1M5 9h1' +
          'M4 10h1M6 10h1M8 10h1M10 10h1M5 11h1M7 11h1M4 12h1M6 12h1M8 12h1' +
          'M10 12h1M5 13h1M7 13h1M9 13h1" />'#13#10'<path stroke="#088282" d="M5 ' +
          '4h1M7 4h1M9 4h1M4 5h1M10 5h1M7 6h1M4 7h1M6 7h1M8 7h1M10 7h1M7 8h' +
          '1M4 9h1M8 9h1M10 9h1M5 10h1M7 10h1M4 11h1M6 11h1M8 11h1M10 11h1M' +
          '5 12h1M7 12h1M4 13h1M6 13h1M8 13h1M10 13h1" />'#13#10'<path stroke="#f' +
          'fff00" d="M6 5h3M5 6h1M9 6h1M5 7h1M9 7h1M5 8h1M8 8h2M6 9h2M9 9h1' +
          'M9 10h1M9 11h1M9 12h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Warning'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#828200" d="M6 3h4M5' +
          ' 4h1M10 4h1M5 5h1M10 5h1M4 7h1M11 7h1M3 9h1M12 9h1M2 10h1M13 10h' +
          '1M2 11h1M13 11h1M2 12h1M13 12h1M2 13h1M3 14h10" />'#13#10'<path stroke' +
          '="#828282" d="M6 4h1M9 4h1M4 6h1M11 6h1M3 8h1M12 8h1" />'#13#10'<path ' +
          'stroke="#ffff00" d="M7 4h2M6 5h4M6 6h1M9 6h1M5 7h2M9 7h2M5 8h2M9' +
          ' 8h2M4 9h3M9 9h3M4 10h3M9 10h3M3 11h10M3 12h4M9 12h4M4 13h8" />'#13 +
          #10'<path stroke="#c4c4c4" d="M5 6h1M10 6h1M4 8h1M11 8h1M3 10h1M12 ' +
          '10h1M3 13h1M12 13h1" />'#13#10'<path stroke="#000000" d="M7 6h2M7 7h2M' +
          '7 8h2M7 9h2M7 10h2M7 12h2" />'#13#10'<path stroke="#946f2a" d="M13 13h' +
          '1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Question'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#11ffff" d="M6 2h3M5' +
          ' 3h1M4 4h1M4 5h1M8 5h1M8 6h1M7 7h1M6 8h1M6 9h1M6 10h1M6 12h1" />' +
          #13#10'<path stroke="#0000ff" d="M6 3h3M5 4h1M8 4h2M5 5h1M9 5h1M9 6h1' +
          'M8 7h1M7 8h1M7 9h1M7 10h1M7 12h1" />'#13#10'<path stroke="#1cff96" d="' +
          'M9 3h1M6 4h2M10 4h1M6 5h1M10 5h1M5 6h2M10 6h1M9 7h1M8 8h1M8 9h1M' +
          '8 10h1M8 12h1M7 13h2" />'#13#10'<path stroke="#c4c4c4" d="M7 5h1M7 6h1' +
          '" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'Debugline'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#ffffff" d="M6 2h1M6' +
          ' 3h2M6 4h1M8 4h1M3 5h4M9 5h1M3 6h1M10 6h1M3 7h1M11 7h1M3 8h1M10 ' +
          '8h1M3 9h4M9 9h1M6 10h1M8 10h1M6 11h2M6 12h1" />'#13#10'<path stroke="#' +
          'fe0000" d="M7 4h1M7 5h2M4 6h6M4 7h7M4 8h6M7 9h2M7 10h1" />'#13#10'</sv' +
          'g>'#13#10
      end
      item
        IconName = 'Breakpoint'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#d2d2d2" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h2M12 5h1M4 6h2M9 6h3M3 8h2M10 8h3M10 9h1M1' +
          '0 10h1M4 11h2M10 11h2M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h' +
          '1" />'#13#10'<path stroke="#088400" d="M7 2h2M8 3h1M3 4h1M7 4h1M12 4h1' +
          'M4 5h1M6 5h2M11 5h1M6 6h1M5 7h1M8 7h2M5 8h1M8 8h2M2 9h2M5 9h1M8 ' +
          '9h2M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 12h1M2 13h1M6 1' +
          '3h1M13 13h1" />'#13#10'<path stroke="#820000" d="M6 3h1M9 3h1M6 4h1M9 ' +
          '4h1" />'#13#10'<path stroke="#11ff00" d="M7 3h1M7 6h2M6 7h2M6 8h2M6 9h' +
          '2M6 10h1M6 11h1" />'#13#10'<path stroke="#fe0000" d="M5 4h1M10 4h1" />' +
          #13#10'<path stroke="#c4c5c4" d="M5 5h1M10 5h1" />'#13#10'<path stroke="#08' +
          '8482" d="M7 10h1" />'#13#10'</svg>'#13#10
      end
      item
        IconName = 'BreakpointsDelete'
        SVGText = 
          '<svg viewBox="0 -0.5 16 16">'#13#10'<path stroke="#d2d2d2" d="M5 2h1M1' +
          '0 2h1M8 4h1M3 5h1M8 5h1M12 5h1M4 6h1M10 6h2M3 8h2M10 8h3M4 11h2M' +
          '10 11h1M4 12h1M9 12h1M11 12h1M3 13h1M7 13h2M12 13h1" />'#13#10'<path s' +
          'troke="#088400" d="M7 2h2M8 3h1M7 4h1M12 4h1M6 5h2M11 5h1M5 7h1M' +
          '9 7h1M2 9h2M8 9h1M12 9h2M5 10h1M8 10h2M7 11h3M3 12h1M5 12h4M12 1' +
          '2h1M2 13h1M6 13h1M13 13h1" />'#13#10'<path stroke="#fe0000" d="M3 3h1M' +
          '11 3h1M3 4h3M10 4h2M4 5h2M9 5h2M5 6h2M8 6h2M6 7h3M5 8h2M8 8h2M4 ' +
          '9h2M9 9h2M3 10h2M10 10h2M3 11h1M11 11h1" />'#13#10'<path stroke="#8200' +
          '00" d="M6 3h1M9 3h1M6 4h1M9 4h1" />'#13#10'<path stroke="#11ff00" d="M' +
          '7 3h1M7 6h1M7 8h1M6 9h2M6 10h1M6 11h1" />'#13#10'<path stroke="#bf5d5d' +
          '" d="M10 3h1" />'#13#10'<path stroke="#088482" d="M7 10h1" />'#13#10'</svg>'#13 +
          #10
      end>
    Left = 376
    Top = 287
  end
  object vilBookmarksLight: TVirtualImageList
    Images = <
      item
        CollectionIndex = 0
        CollectionName = 'Bookmark0'
        Name = 'Bookmark0'
      end
      item
        CollectionIndex = 1
        CollectionName = 'Bookmark1'
        Name = 'Bookmark1'
      end
      item
        CollectionIndex = 2
        CollectionName = 'Bookmark2'
        Name = 'Bookmark2'
      end
      item
        CollectionIndex = 3
        CollectionName = 'Bookmark3'
        Name = 'Bookmark3'
      end
      item
        CollectionIndex = 4
        CollectionName = 'Bookmark4'
        Name = 'Bookmark4'
      end
      item
        CollectionIndex = 5
        CollectionName = 'Bookmark5'
        Name = 'Bookmark5'
      end
      item
        CollectionIndex = 6
        CollectionName = 'Bookmark6'
        Name = 'Bookmark6'
      end
      item
        CollectionIndex = 7
        CollectionName = 'Bookmark7'
        Name = 'Bookmark7'
      end
      item
        CollectionIndex = 8
        CollectionName = 'Bookmark8'
        Name = 'Bookmark8'
      end
      item
        CollectionIndex = 9
        CollectionName = 'Bookmark9'
        Name = 'Bookmark9'
      end
      item
        CollectionIndex = 10
        CollectionName = 'Warning'
        Name = 'Warning'
      end
      item
        CollectionIndex = 11
        CollectionName = 'Question'
        Name = 'Question'
      end
      item
        CollectionIndex = 12
        CollectionName = 'Debugline'
        Name = 'Debugline'
      end
      item
        CollectionIndex = 13
        CollectionName = 'Breakpoint'
        Name = 'Breakpoint'
      end
      item
        CollectionIndex = 14
        CollectionName = 'BreakpointsDelete'
        Name = 'BreakpointsDelete'
      end>
    ImageCollection = icBookmarks
    Left = 72
    Top = 287
  end
  object vilBookmarksDark: TVirtualImageList
    Images = <
      item
        CollectionIndex = 15
        CollectionName = 'Bookmark0'
        Name = 'Bookmark0'
      end
      item
        CollectionIndex = 16
        CollectionName = 'Bookmark1'
        Name = 'Bookmark1'
      end
      item
        CollectionIndex = 17
        CollectionName = 'Bookmark2'
        Name = 'Bookmark2'
      end
      item
        CollectionIndex = 18
        CollectionName = 'Bookmark3'
        Name = 'Bookmark3'
      end
      item
        CollectionIndex = 19
        CollectionName = 'Bookmark4'
        Name = 'Bookmark4'
      end
      item
        CollectionIndex = 20
        CollectionName = 'Bookmark5'
        Name = 'Bookmark5'
      end
      item
        CollectionIndex = 21
        CollectionName = 'Bookmark6'
        Name = 'Bookmark6'
      end
      item
        CollectionIndex = 22
        CollectionName = 'Bookmark7'
        Name = 'Bookmark7'
      end
      item
        CollectionIndex = 23
        CollectionName = 'Bookmark8'
        Name = 'Bookmark8'
      end
      item
        CollectionIndex = 24
        CollectionName = 'Bookmark9'
        Name = 'Bookmark9'
      end
      item
        CollectionIndex = 25
        CollectionName = 'Warning'
        Name = 'Warning'
      end
      item
        CollectionIndex = 26
        CollectionName = 'Question'
        Name = 'Question'
      end
      item
        CollectionIndex = 27
        CollectionName = 'Debugline'
        Name = 'Debugline'
      end
      item
        CollectionIndex = 28
        CollectionName = 'Breakpoint'
        Name = 'Breakpoint'
      end
      item
        CollectionIndex = 29
        CollectionName = 'BreakpointsDelete'
        Name = 'BreakpointsDelete'
      end>
    ImageCollection = icBookmarks
    Left = 224
    Top = 287
  end
end
