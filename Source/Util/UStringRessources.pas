unit UStringRessources;

interface
{gnugettext: scan-all}

Const

  LNGTODO = '// TODO add your code here';
  LNGOpenFile = 'Open file';
  LNGValue = 'Value';
  LNGUnknownClass = 'Unknown class';
  LNGNameOfObject= 'Name of object';
  LNGClass = 'Class';
  LNGInterface = 'Interface';
  LNGSelectAttributes = 'Select attributes';
  LNGSet = 'set';
  LNGGet = 'get';
  LNGAlreadyExists = '%s already exists';
  LNGRevisions = 'Revisions';
  LNGRevision = 'Revision';
  LNGAuthor = 'Author';
  LNGDate = 'Date';
  LNGMessage = 'Message';   // Beschreibung
  LNGOpenClass = 'Open class';
  LNGNoDocumentationAvailable = 'No documentation available';
  LNGCompileWith = 'Compile "%s" with';
  LNGUnknown = '<unknown>';
  LNGSuccessfullCompiled = 'successfully compiled!';
  LNGTransferToRCX = 'Transfer %s to the RCX';
  LNGSuccessfullTransfered = 'Successfully transfered!';
  LNGErrorDuringTransferToRCX = 'Error during transfer to the RCX.';
  LNGUnabledToExecute = 'Cannot execute %s. Errorcode:';
  LNGDownloadError = 'Download abort';
  LNGNoInternetConnection = 'No internet connection';
  LNGMissingAdminRights = 'Missing admin rights. Start the Java-Editor with the option "Run as administrator" from the context menu of it''s desktop symbol.';
  LNGCopyBlockLeft = 'Copy block left';
  LNGCopyBlockRight = 'Copy block right';
  LNGNotFound = 'not found.';
  LNGStatement = 'statement';
  LNGSaved = 'Saved';

  // StShCtl
  LNGNameCol = 'Name';
  LNGSizeCol = 'Size';
  LNGTypeCol = 'Type';
  LNGModifiedCol = 'Modified';
  LNGFile = 'File';

  // Editor
  LNGModified = 'Modified';
  LNGWriteProtected = 'write protected';
  LNGModusOverwrite = 'Over';
  LNGModusInsert = 'Ins';
  LNGLine = 'Line';
  LNGColumn = 'Column';
  LNGStartGUIVariables = '// start attributes';
  LNGEndGUIVariables = '// end attributes';
  LNGStartComponents = '// start components';
  LNGEndComponents = '// end components';
  LNGStartEventMethods = '// start methods';
  LNGEndEventMethods = '// end methods';

  // FJava
  LNGSaveAs = 'Save as...';
  LNGExportTo = 'Export to';
  LNGSearchTextNotFound = 'Searchtext "%s" not found.';
  LNGRunApplication = 'Run application';
  LNGAssociatedJavaFileNotFound = 'Corresponding Java file "%s" not found!';
  LNGMindstormsPrograms = 'Unable to debug Mindstorms programs.';
  LNGResetProgram = 'Reset program';
  LNGSuccessfullyTerminated = 'successfully terminated.';
  LNGArrayLength = 'Length of array.';
  LNGClassNotFound = 'Class %s not found!';
  LNGNoDefaultPrinter = 'No default printer selected in Windows.';
  LNGOpenSubClass = 'Open a subclass of';
  LNGInteractive = 'Interactive';
  LNGBlockedInternet = 'Internet access locked!';

  // Configuration
  LNGCompileForMindstorms = 'Compile for mindstorms/Compile';
  LNGTransferToAndroid = 'Transfer to Android';
  LNGDownloadLejos = 'Download Lejos firmware';
  LNGCompileWithJava = 'Compile';
  LNGBrackets = 'Brackets';
  LNGFileNotFound = 'File "%s" not found.';
  LNGNoRunJavaBat = 'No current file "RunJava.Bat" found.';
  LNGIdentifier = 'Identifier';
  LNGAttribute = 'Attribute';
  LNGDocumentation = 'Documentation';
  LNGComment = 'Comment';
  LNGSpace = 'Space';
  LNGReservedWord = 'Reserved word';
  LNGTag = 'Tag';
  LNGString = 'String';
  LNGSymbol = 'Symbol';
  LNGInvalidSymbol = 'Invalid symbol';
  LNGNumber = 'Number';
  LNGEscape = 'Escape character';
  LNGText = 'Text';
  LNGUnknownWord = 'Unknown word';
  LNGValidComponentname1 = 'In the name of a GUI component, only a-z, A-Z, 0-9, and _ may occur. He must not start with a number.';
  LNGValidComponentname2 = 'The name of a GUI component can not be a data type.';
  LNGValidComponentname3 = 'Another component already has the name:';
  LNGFileAlreadyOpen = '%s already open!';
  LNGDoesNotExist = 'doesn''t exist!';
  LNGCanNotCreateFile = 'Can not create file %s! Error: %s';
  LNGCanNotCreateDirectory = 'Can not create directory %s!';
  LNGProject = 'Project';
  LNGStructogram = 'Structogram';
  LNGSequenceDiagram = 'Sequence diagram';

  LNGAll= 'all';
  LNGNone= 'none';
  LNGFileAlreadyExists= '%s already exists. Overwrite?';

  SWebView2Error = 'The WebView2 control creation failed.' + SLineBreak +  'Please install the WebView2 runtime.';
  SWebNavigationError = 'Error in loading html in the browser';
  SEdCmdCopy = 'Copy';


{

  // File Filters
  sPythonFormFileFilter = 'Python form (*.pfm)|*.pfm';
  sUMLFileFilter = 'UML (*.puml)|*.puml';
  sStructogramFileFilter = 'Structogram (*.psg)|*.psg';
  sSequencediagramfileFilter = 'Sequence diagram (*.psd)|*.psd';
  sAllFileFilter = 'All (*.*)|*.*';

  // Editor commands
  //Needs to be manually updated as new editor commands are added
  SEdCmdAutoCompletion = 'Auto Completion';
  SEdCmdBlockIndent = 'Block Indent';
  SEdCmdBlockUnindent = 'Block Unindent';
  SEdCmdClearAll = 'Clear All';
  SEdCmdColumnSelect = 'Column Select';
  SEdCmdCommentBlock = 'Comment Block';
  SEdCmdContextHelp = 'Context Help';
  SEdCmdCopy = 'Copy';
  SEdCmdCopyLineDown = 'Copy Line Down';
  SEdCmdCopyLineUp = 'Copy Line Up';
  SEdCmdCut = 'Cut';
  SEdCmdDeleteBOL = 'Delete BOL';
  SEdCmdDeleteChar = 'Delete Char';
  SEdCmdDeleteEOL = 'Delete EOL';
  SEdCmdDeleteLastChar = 'Delete Last Char';
  SEdCmdDeleteLastWord = 'Delete Last Word';
  SEdCmdDeleteLine = 'Delete Line';
  SEdCmdDeleteWord = 'Delete Word';
  SEdCmdDown = 'Down';
  SEdCmdEditorBottom = 'Editor Bottom';
  SEdCmdEditorTop = 'Editor Top';
  SEdCmdFoldAll = 'Fold All';
  SEdCmdFoldLevel1 = 'Fold Level 1';
  SEdCmdFoldLevel2 = 'Fold Level 2';
  SEdCmdFoldLevel3 = 'Fold Level 3';
  SEdCmdFoldNearest = 'Fold Nearest';
  SEdCmdFoldRanges = 'Fold Ranges';
  SEdCmdGotoBookmark0 = 'Goto Bookmark 0';
  SEdCmdGotoBookmark1 = 'Goto Bookmark 1';
  SEdCmdGotoBookmark2 = 'Goto Bookmark 2';
  SEdCmdGotoBookmark3 = 'Goto Bookmark 3';
  SEdCmdGotoBookmark4 = 'Goto Bookmark 4';
  SEdCmdGotoBookmark5 = 'Goto Bookmark 5';
  SEdCmdGotoBookmark6 = 'Goto Bookmark 6';
  SEdCmdGotoBookmark7 = 'Goto Bookmark 7';
  SEdCmdGotoBookmark8 = 'Goto Bookmark 8';
  SEdCmdGotoBookmark9 = 'Goto Bookmark 9';
  SEdCmdInsertLine = 'Insert Line';
  SEdCmdInsertMode = 'Insert Mode';
  SEdCmdLeft = 'Left';
  SEdCmdLineBreak = 'Line Break';
  SEdCmdLineEnd = 'Line End';
  SEdCmdLineSelect = 'Line Select';
  SEdCmdLineStart = 'Line Start';
  SEdCmdLowerCase = 'Lower Case';
  SEdCmdMatchBracket = 'Match Bracket';
  SEdCmdMoveLineDown = 'Move Line Down';
  SEdCmdMoveLineUp = 'Move Line Up';
  SEdCmdNormalSelect = 'Normal Select';
  SEdCmdOverwriteMode = 'Overwrite Mode';
  SEdCmdPageBottom = 'Page Bottom';
  SEdCmdPageDown = 'Page Down';
  SEdCmdPageLeft = 'Page Left';
  SEdCmdPageRight = 'Page Right';
  SEdCmdPageTop = 'Page Top';
  SEdCmdPageUp = 'Page Up';
  SEdCmdPaste = 'Paste';
  SEdCmdRedo = 'Redo';
  SEdCmdRight = 'Right';
  SEdCmdScrollDown = 'Scroll Down';
  SEdCmdScrollLeft = 'Scroll Left';
  SEdCmdScrollRight = 'Scroll Right';
  SEdCmdScrollUp = 'Scroll Up';
  SEdCmdSelectAll = 'Select All';
  SEdCmdSelectDown = 'Select Down';
  SEdCmdSelectEditorBottom = 'Select Editor Bottom';
  SEdCmdSelectEditorTop = 'Select Editor Top';
  SEdCmdSelectLeft = 'Select Left';
  SEdCmdSelectLineEnd = 'Select Line End';
  SEdCmdSelectLineStart = 'Select Line Start';
  SEdCmdSelectPageBottom = 'Select Page Bottom';
  SEdCmdSelectPageDown = 'Select Page Down';
  SEdCmdSelectPageLeft = 'Select Page Left';
  SEdCmdSelectPageRight = 'Select Page Right';
  SEdCmdSelectPageTop = 'Select Page Top';
  SEdCmdSelectPageUp = 'Select Page Up';
  SEdCmdSelectRight = 'Select Right';
  SEdCmdSelectUp = 'Select Up';
  SEdCmdSelectWord = 'Select Word';
  SEdCmdSelectWordLeft = 'Select Word Left';
  SEdCmdSelectWordRight = 'Select Word Right';
  SEdCmdSetBookmark0 = 'Set Bookmark 0';
  SEdCmdSetBookmark1 = 'Set Bookmark 1';
  SEdCmdSetBookmark2 = 'Set Bookmark 2';
  SEdCmdSetBookmark3 = 'Set Bookmark 3';
  SEdCmdSetBookmark4 = 'Set Bookmark 4';
  SEdCmdSetBookmark5 = 'Set Bookmark 5';
  SEdCmdSetBookmark6 = 'Set Bookmark 6';
  SEdCmdSetBookmark7 = 'Set Bookmark 7';
  SEdCmdSetBookmark8 = 'Set Bookmark 8';
  SEdCmdSetBookmark9 = 'Set Bookmark 9';
  SEdCmdShiftTab = 'Shift Tab';
  SEdCmdTab = 'Tab';
  SEdCmdTitleCase = 'Title Case';
  SEdCmdToggleCase = 'Toggle Case';
  SEdCmdToggleInsertMode = 'Toggle Mode';
  SEdCmdUndo = 'Undo';
  SEdCmdUnfoldAll = 'Unfold All';
  SEdCmdUnfoldLevel1 = 'Unfold Level 1';
  SEdCmdUnfoldLevel2 = 'Unfold Level 2';
  SEdCmdUnfoldLevel3 = 'Unfold Level 3';
  SEdCmdUnfoldNearest = 'Unfold Nearest';
  SEdCmdUnfoldRanges = 'Unfold Ranges';
  SEdCmdUp = 'Up';
  SEdCmdUpperCase = 'Upper Case';
  SEdCmdWordLeft = 'Word Left';
  SEdCmdWordRight = 'Word Right';
  // User editor commands
  SEdCmdCodeCompletion = 'Code Completion';
  SEdCmdParameterCompletion = 'Parameter Completion';
  SEdCmdSelectToBracket = 'Select to Bracket';
  }

  // Delphi dialogs (from Vcl.Consts)
  srNone = '(None)';
  SDlgWarning = 'Warning';
  SDlgError = 'Error';
  SDlgInformation = 'Information';
  SDlgConfirm = 'Confirm';
  SDlgYes = '&Yes';
  SDlgNo = '&No';
  SDlgOK = 'OK';
  SDlgCancel = 'Cancel';
  SDlgHelp = '&Help';
  SDlgHelpHelp = 'Help';
  SDlgAbort = '&Abort';
  SDlgRetry = '&Retry';
  SDlgIgnore = '&Ignore';
  SDlgAll = '&All';
  SDlgNoToAll = 'N&o to All';
  SDlgYesToAll = 'Yes to &All';
  SDlgClose = '&Close';

  STrueValue = 'True';
  SFalseValue = 'False';

  SmkcBkSp = 'BkSp';
  SmkcTab = 'Tab';
  SmkcEsc = 'Esc';
  SmkcEnter = 'Enter';
  SmkcSpace = 'Space';
  SmkcPgUp = 'PgUp';
  SmkcPgDn = 'PgDn';
  SmkcEnd = 'End';
  SmkcHome = 'Home';
  SmkcLeft = 'Left';
  SmkcUp = 'Up';
  SmkcRight = 'Right';
  SmkcDown = 'Down';
  SmkcIns = 'Ins';
  SmkcDel = 'Del';
  SmkcShift = 'Shift+';
  SmkcCtrl = 'Ctrl+';
  SmkcAlt = 'Alt+';

{gnugettext: reset }

implementation

end.








