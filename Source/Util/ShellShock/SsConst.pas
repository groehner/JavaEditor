(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower ShellShock
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: SsConst.pas 1.02                          *}
{*********************************************************}
{* ShellShock: Base unit for ShellShock                  *}
{*********************************************************}

{$I SsDefine.inc}

unit SsConst;
  {-Resource constants for ShellShock}

interface

const
  ssscNoCompare        = 51; {Compare property must be set}
  ssscBadType          = 52; {an incompatible class is passed to a method}
  ssscBadSize          = 53; {bad size for TStDictionary, TStBits, TStCollection}
  ssscDupNode          = 54; {attempt to add duplicate node to TStTree}
  ssscBadIndex         = 55; {bad index passed to TStBits or large array}
  ssscBadWinMode       = 56; {requires enhanced mode operation}
  ssscUnknownClass     = 57; {container class name not registered}
  ssscUnknownNodeClass = 58; {container node class name not registered}
  ssscNoStoreData      = 59; {container has no store data routine}
  ssscNoLoadData       = 60; {container has no load data routine}
  ssscWrongClass       = 61; {container class and streamed class not equal}
  ssscWrongNodeClass   = 62; {container node class and streamed class not equal}
  ssscBadCompare       = 63; {invalid compare function or unable to assign now}
  ssscTooManyCols      = 64; {assign a matrix with >1 col to array}
  ssscBadColCount      = 65; {assign a matrix with wrong col count to virtual matrix}
  ssscBadElSize        = 66; {assign a matrix with wrong elem size to virtual matrix}
  ssscBadDups          = 67; {setting Dups to False in a non-empty sorted collection}

  ssscFileOpen         = 73; {error opening file in TStSorter}

  ssscNoVerInfo        = 100; {no version info in file}
  ssscVerInfoFail      = 101; {error reading version info}

  ssscShellVersionError   = 110; {not available in this version of Shell32.dll}
  ssscShellFileOpSrcError = 111; {no source files specified}
  ssscShellFileOpDstError = 112; {no destination files specified}
  ssscShellFileOpMapError = 113; {mapping incomplete}
  ssscShellFormatError    = 114; {format error}
  ssscShellFormatCancel   = 115; {format cancelled}
  ssscShellFormatNoFormat = 116; {drive cannot be formatted}
  ssscShellFormatBadDrive = 117; {not removable drive}
  ssscTrayIconInvalidOS   = 118; {bad OS (NT 3.51)}
  ssscTrayIconCantAdd     = 119; {can't add icon to the tray}
  ssscTrayIconCantDelete  = 120; {can't delete icon from the tray}
  ssscTrayIconError       = 121; {general tray icon error}
  ssscBadDropTarget       = 122; {drop target is not TWinControl}
  ssscCOMInitFailed       = 123; {COInitialize failed}
  ssscNoPathSpecified     = 124; {No destination path for shortcut}
  ssscIShellLinkError     = 125; {Error creating IShellLink}
  ssscNotShortcut         = 126; {File is not a shortcut}
  ssscTrayIconClose       = 127; {Close}
  ssscTrayIconRestore     = 128; {Restore}
  ssscInvalidTargetFile   = 130; {Shortcut target file not found}
  ssscShellFileOpDelete   = 131; {Can't use file mappings with delete op}
  ssscShellFileNotFound   = 132; {One or more source files is missing}
  ssscTrayIconDuplicate   = 133; {Cant' have more than one tray icon}
  ssscBadVerInfoKey       = 134; {User-defined key not found in ver info}
  ssscImageListInvalid    = 135; {No image list assigned.}

  ssscName            = 230;
  ssscSize            = 231;
  ssscType            = 232;
  ssscModified        = 233;
  ssscAttributes      = 234;
  ssscFileFolder      = 235;
  ssscSystemFolder    = 236;
  ssscOriginalLoc     = 237;
  ssscDateDeleted     = 238;
  ssscFile            = 239;
  ssscInvalidFolder   = 240;
  ssscFolderReadOnly  = 241;
  ssscInvalidSortDir  = 242;

  {StMemoryMappedFile errors}
  ssscCreateFileFailed = 260;
  ssscFileMappingFailed= 261;
  ssscCreateViewFailed = 262;
  ssscBadOrigin        = 263;
  ssscGetSizeFailed    = 264;

  {buffered stream errors}
  ssscNilStream        = 270;
  ssscNoSeekForRead    = 271;
  ssscNoSeekForWrite   = 272;
  ssscCannotWrite      = 273;
  ssscBadTerminator    = 274;
  ssscBadLineLength    = 275;
  ssscCannotSetSize    = 276;

  {RegEx errors}
  ssscUnknownError              = 290;
  ssscExpandingClass            = 291;
  ssscAlternationFollowsClosure = 292;
  ssscUnbalancedParens          = 293;
  ssscFollowingClosure          = 294;
  ssscPatternError              = 295;
  ssscUnbalancedTag             = 296;
  ssscNoPatterns                = 297;
  ssscPatternTooLarge           = 298;
  ssscStreamsNil                = 299;
  ssscInTextStreamError         = 300;
  ssscOutTextStreamError        = 301;
  ssscClosureMaybeEmpty         = 302;
  ssscInFileNotFound            = 303;
  ssscREInFileError             = 304;
  ssscOutFileDelete             = 305;
  ssscOutFileCreate             = 306;

resourcestring
  ssscNoCompareS                 = 'Compare property must be set';
  ssscBadTypeS                   = 'An incompatible class is passed to a method';
  ssscBadSizeS                   = 'Bad size parameter';
  ssscDupNodeS                   = 'Attempt to add duplicate node to TStTree';
  ssscBadIndexS                  = 'Index is out of range';
  ssscBadWinModeS                = 'Requires enhanced mode operation for Windows 3.1x';
  ssscUnknownClassS              = 'Container class name %s read from stream is unregistered';
  ssscUnknownNodeClassS          = 'Node class name %s read from stream is unregistered';
  ssscNoStoreDataS               = 'Container''s StoreData property is unassigned';
  ssscNoLoadDataS                = 'Container''s LoadData property is unassigned';
  ssscWrongClassS                = 'Class name on stream differs from object''s class';
  ssscWrongNodeClassS            = 'Node class name on stream differs from object''s node class';
  ssscBadCompareS                = 'Unable to assign this compare function now';
  ssscTooManyColsS               = 'Cannot assign a matrix with more than 1 column to an array';
  ssscBadColCountS               = 'Can only assign a matrix to a virtual matrix if column counts are equal';
  ssscBadElSizeS                 = 'Can only assign a matrix to a virtual matrix if element sizes are equal';
  ssscBadDupsS                   = 'Can only set Duplicates to False in an empty sorted collection';

  ssscFileOpenS                  = 'Error opening file';

  ssscNoVerInfoS                 = 'File does not contain version info';
  ssscVerInfoFailS               = 'Unable to read version info';

  ssscShellVersionErrorS         = 'Operation not supported in this version of the shell';
  ssscShellFileOpSrcErrorS       = 'No source files specified';
  ssscShellFileOpDstErrorS       = 'No destination files specified';
  ssscShellFileOpMapErrorS       = 'File mapping incomplete';
  ssscShellFormatErrorS          = 'Format failed';
  ssscShellFormatCancelS         = 'Format cancelled';
  ssscShellFormatNoFormatS       = 'Drive cannot be formatted';
  ssscShellFormatBadDriveS       = 'Invalid drive. Drive is not removable';
  ssscTrayIconInvalidOSS         = 'Operating system does not support tray icons';
  ssscTrayIconCantAddS           = 'Error adding tray icon';
  ssscTrayIconCantDeleteS        = 'Error removing tray icon';
  ssscTrayIconErrorS             = 'Tray icon error';
  ssscBadDropTargetS             = 'Drop target must be a TWinControl descendant';
  ssscCOMInitFailedS             = 'Cannot initialize COM';
  ssscNoPathSpecifiedS           = 'Destination directory not specified';
  ssscIShellLinkErrorS           = 'Error creating IShellLink';
  ssscNotShortcutS               = 'File is not a shortcut';
  ssscTrayIconCloseS             = '&Close';
  ssscTrayIconRestoreS           = '&Restore';
  ssscInvalidTargetFileS         = 'Cannot create shortcut. Target file does not exist';
  ssscShellFileOpDeleteS         = 'Cannot use file mappings in a delete operation';
  ssscShellFileNotFoundS         = 'Source file error, file not found';
  ssscTrayIconDuplicateS         = 'Cannot have more than one StTrayIcon per application';
  ssscBadVerInfoKeyS             = 'The specified key cannnot be found in version info';
  ssscImageListInvalidS          = 'ImageList is not assigned';

  ssscNameS                      = 'Name';
  ssscSizeS                      = 'Size';
  ssscTypeS                      = 'Type';
  ssscModifiedS                  = 'Modified';
  ssscAttributesS                = 'Attributes';
  ssscFileFolderS                = 'File Folder';
  ssscSystemFolderS              = 'System Folder';
  ssscOriginalLocS               = 'Original Location';
  ssscDateDeletedS               = 'Date Deleted';
  ssscFileS                      = 'File';
  ssscInvalidFolderS             = 'Invalid folder';
  ssscFolderReadOnlyS            = 'Cannot create folder: Parent folder is read-only';
  ssscInvalidSortDirS            = 'Invalid sort direction';

  ssscCreateFileFailedS          = 'CreateFile failed';
  ssscFileMappingFailedS         = 'CreateFileMapping failed';
  ssscCreateViewFailedS          = 'MapViewOfFile failed';
  ssscBadOriginS                 = 'Bad origin parameter for call to Seek';
  ssscGetSizeFailedS             = 'Error reading size of existing file';

  ssscNilStreamS                 = 'Buffered/text stream: Attempted to read, write, or seek and underlying stream is nil';
  ssscNoSeekForReadS             = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for read request)';
  ssscNoSeekForWriteS            = 'Buffered/text stream: Could not seek to the correct position in the underlying stream (for write request)';
  ssscCannotWriteS               = 'Buffered/text stream: Could not write the entire buffer to the underlying stream';
  ssscBadTerminatorS             = 'Text stream: Case statement was used with a bad value of LineTerminator';
  ssscBadLineLengthS             = 'Text stream: Length of a fixed line must be between 1 and 4096 bytes';
  ssscCannotSetSizeS             = 'Buffered/text stream: Cannot set the size of the underlying stream (needs OnSetStreamSize event)';

  ssscUnknownErrorS              = 'Unknown error creating a pattern token';
  ssscExpandingClassS            = 'Problem in expanding character class';
  ssscAlternationFollowsClosureS = 'Alternation cannot immediately follow a closure marker';
  ssscUnbalancedParensS          = 'Unbalanced nesting parentheses';
  ssscFollowingClosureS          = 'Closure cannot immediately follow BegOfLine, EndOfLine or another closure';
  ssscPatternErrorS              = 'Error detected near end of pattern';
  ssscUnbalancedTagS             = 'Unbalanced tag marker';
  ssscNoPatternsS                = 'No Match, Replace, or SelAvoid Patterns defined';
  ssscPatternTooLargeS           = 'Pattern exceeds MaxPatLen';
  ssscStreamsNilS                = 'Input and/or output stream is not assigned';
  ssscInTextStreamErrorS         = 'Error creating internal input text stream';
  ssscOutTextStreamErrorS        = 'Error creating internal output text stream';
  ssscClosureMaybeEmptyS         = 'A * or + operand could be empty';
  ssscOutFileDeleteS             = 'Error deleting old previous file';
  ssscInFileNotFoundS            = 'Input file not found';
  ssscREInFileErrorS             = 'Error creating internal text stream';
  ssscOutFileCreateS             = 'Error creating output file';

{
embedded component strings for the following units:
  StShrtCt
  StShlCtl
  SsShlDlg
Note: these are accessed directly by the code and are
      not reflected in ShellShockStrArray below
}

resourcestring
  ssscDisplayPreposition = ' on ';
  ssscDefaultFolderName  = 'New Folder';

  ssscViewCaptionLargeIcon = 'Lar&ge Icons';
  ssscViewCaptionSmallIcon = 'S&mall Icons';
  ssscViewCaptionList      = '&List';
  ssscViewCaptionDetails   = '&Details';

  ssscItemDisplayFile      = ' file';
  ssscItemDisplayFiles     = ' files';
  ssscItemDisplayFolder    = ' folder';
  ssscItemDisplayFolders   = ' folders';
  ssscItemDispalyObjects   = ' objects';

  ssscDefaultShortcutPrefix = 'Shortcut to ';

  ssscNavLookInCaption      = 'Look &in:';
  ssscNavHintMoveUp         = 'Up One Level';
  ssscNavHintNewFolder      = 'Create New Folder';
  ssscNavHintLastFolder     = 'Go To Last Folder Visited';
  ssscNavHintViewMenu       = 'View Menu';

  ssscNavBtnOpenCaption     = '&Open';
  ssscNavBtnCancelCaption   = 'Cancel';
  ssscNavLblTypeCaption     = 'Files of &type:';
  ssscNavLblFileNameCaption = 'File &name:';


const
  SsVersionStr = '1.02';

function ShellShockStr(Index : Integer) : string;

implementation

type
  SsStrRec = record
    ID: Integer;
    Str: string;
  end;

const
  ShellShockStrArray : array [0..86] of SsStrRec = (
   (ID: ssscNoCompare; Str: ssscNoCompareS),
   (ID: ssscBadType; Str: ssscBadTypeS),
   (ID: ssscBadSize; Str: ssscBadSizeS),
   (ID: ssscDupNode; Str: ssscDupNodeS),
   (ID: ssscBadIndex; Str: ssscBadIndexS),
   (ID: ssscBadWinMode; Str: ssscBadWinModeS),
   (ID: ssscUnknownClass; Str: ssscUnknownClassS),
   (ID: ssscUnknownNodeClass; Str: ssscUnknownNodeClassS),
   (ID: ssscNoStoreData; Str: ssscNoStoreDataS),
   (ID: ssscNoLoadData; Str: ssscNoLoadDataS),
   (ID: ssscWrongClass; Str: ssscWrongClassS),
   (ID: ssscWrongNodeClass; Str: ssscWrongNodeClassS),
   (ID: ssscBadCompare; Str: ssscBadCompareS),
   (ID: ssscTooManyCols; Str: ssscTooManyColsS),
   (ID: ssscBadColCount; Str: ssscBadColCountS),
   (ID: ssscBadElSize; Str: ssscBadElSizeS),
   (ID: ssscBadDups; Str: ssscBadDupsS),
   (ID: ssscFileOpen; Str: ssscFileOpenS),
   (ID: ssscNoVerInfo; Str: ssscNoVerInfoS),
   (ID: ssscVerInfoFail; Str: ssscVerInfoFailS),
   (ID: ssscShellVersionError; Str: ssscShellVersionErrorS),
   (ID: ssscShellFileOpSrcError; Str: ssscShellFileOpSrcErrorS),
   (ID: ssscShellFileOpDstError; Str: ssscShellFileOpDstErrorS),
   (ID: ssscShellFileOpMapError; Str: ssscShellFileOpMapErrorS),
   (ID: ssscShellFormatError; Str: ssscShellFormatErrorS),
   (ID: ssscShellFormatCancel; Str: ssscShellFormatCancelS),
   (ID: ssscShellFormatNoFormat; Str: ssscShellFormatNoFormatS),
   (ID: ssscShellFormatBadDrive; Str: ssscShellFormatBadDriveS),
   (ID: ssscTrayIconInvalidOS; Str: ssscTrayIconInvalidOSS),
   (ID: ssscTrayIconCantAdd; Str: ssscTrayIconCantAddS),
   (ID: ssscTrayIconCantDelete; Str: ssscTrayIconCantDeleteS),
   (ID: ssscTrayIconError; Str: ssscTrayIconErrorS),
   (ID: ssscBadDropTarget; Str: ssscBadDropTargetS),
   (ID: ssscCOMInitFailed; Str: ssscCOMInitFailedS),
   (ID: ssscNoPathSpecified; Str: ssscNoPathSpecifiedS),
   (ID: ssscIShellLinkError; Str: ssscIShellLinkErrorS),
   (ID: ssscNotShortcut; Str: ssscNotShortcutS),
   (ID: ssscTrayIconClose; Str: ssscTrayIconCloseS),
   (ID: ssscTrayIconRestore; Str: ssscTrayIconRestoreS),
   (ID: ssscInvalidTargetFile; Str: ssscInvalidTargetFileS),
   (ID: ssscShellFileOpDelete; Str: ssscShellFileOpDeleteS),
   (ID: ssscShellFileNotFound; Str: ssscShellFileNotFoundS),
   (ID: ssscTrayIconDuplicate; Str: ssscTrayIconDuplicateS),
   (ID: ssscBadVerInfoKey; Str: ssscBadVerInfoKeyS),
   (ID: ssscImageListInvalid; Str: ssscImageListInvalidS),
   (ID: ssscName; Str: ssscNameS),
   (ID: ssscSize; Str: ssscSizeS),
   (ID: ssscType; Str: ssscTypeS),
   (ID: ssscModified; Str: ssscModifiedS),
   (ID: ssscAttributes; Str: ssscAttributesS),
   (ID: ssscFileFolder; Str: ssscFileFolderS),
   (ID: ssscSystemFolder; Str: ssscSystemFolderS),
   (ID: ssscOriginalLoc; Str: ssscOriginalLocS),
   (ID: ssscDateDeleted; Str: ssscDateDeletedS),
   (ID: ssscFile; Str: ssscFileS),
   (ID: ssscInvalidFolder; Str: ssscInvalidFolderS),
   (ID: ssscFolderReadOnly; Str: ssscFolderReadOnlyS),
   (ID: ssscInvalidSortDir; Str: ssscInvalidSortDirS),
   (ID: ssscCreateFileFailed; Str: ssscCreateFileFailedS),
   (ID: ssscFileMappingFailed; Str: ssscFileMappingFailedS),
   (ID: ssscCreateViewFailed; Str: ssscCreateViewFailedS),
   (ID: ssscBadOrigin; Str: ssscBadOriginS),
   (ID: ssscGetSizeFailed; Str: ssscGetSizeFailedS),
   (ID: ssscNilStream; Str: ssscNilStreamS),
   (ID: ssscNoSeekForRead; Str: ssscNoSeekForReadS),
   (ID: ssscNoSeekForWrite; Str: ssscNoSeekForWriteS),
   (ID: ssscCannotWrite; Str: ssscCannotWriteS),
   (ID: ssscBadTerminator; Str: ssscBadTerminatorS),
   (ID: ssscBadLineLength; Str: ssscBadLineLengthS),
   (ID: ssscCannotSetSize; Str: ssscCannotSetSizeS),
   (ID: ssscUnknownError; Str: ssscUnknownErrorS),
   (ID: ssscExpandingClass; Str: ssscExpandingClassS),
   (ID: ssscAlternationFollowsClosure; Str: ssscAlternationFollowsClosureS),
   (ID: ssscUnbalancedParens; Str: ssscUnbalancedParensS),
   (ID: ssscFollowingClosure; Str: ssscFollowingClosureS),
   (ID: ssscPatternError; Str: ssscPatternErrorS),
   (ID: ssscUnbalancedTag; Str: ssscUnbalancedTagS),
   (ID: ssscNoPatterns; Str: ssscNoPatternsS),
   (ID: ssscPatternTooLarge; Str: ssscPatternTooLargeS),
   (ID: ssscStreamsNil; Str: ssscStreamsNilS),
   (ID: ssscInTextStreamError; Str: ssscInTextStreamErrorS),
   (ID: ssscOutTextStreamError; Str: ssscOutTextStreamErrorS),
   (ID: ssscClosureMaybeEmpty; Str: ssscClosureMaybeEmptyS),
   (ID: ssscInFileNotFound; Str: ssscInFileNotFoundS),
   (ID: ssscREInFileError; Str: ssscREInFileErrorS),
   (ID: ssscOutFileDelete; Str: ssscOutFileDeleteS),
   (ID: ssscOutFileCreate; Str: ssscOutFileCreateS)
  );


function ShellShockStr(Index : Integer) : string;
begin
  for var I := Low(ShellShockStrArray) to High(ShellShockStrArray) do
    if ShellShockStrArray[I].ID = Index then
      Result := ShellShockStrArray[I].Str;
end;



end.
