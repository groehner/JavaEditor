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
{* ShellShock: SsReg.pas 1.02                            *}
{*********************************************************}
{* ShellShock: Component Registration Unit               *}
{*********************************************************}

{$I SsDefine.inc}

{$R SsReg.res}

unit SsReg;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  Classes,
{$IFDEF VERSION6}
  DesignIntf,
  DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

procedure Register;

implementation

uses
  SsBase,
  StAbout,
  StBrowsr,
  StFormat,
  StFileOp,
  StTrIcon,
  StDrop,
  StShrtCt,
  StShlCtl,
  SsRegEx,
  {forces these units to be compiled when components are installed}
  {vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv}
  SsDate,
  SsDict,
  SsDQue,
  SsList,
  SsStrms,
  SsAbout0,
  SsNotif0,
  SsPropEd,
  SsShlDlg;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TSsComponent, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBrowser, 'RootFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TStBrowser, 'SelectedFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShortcut, 'DestinationDir',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShortcut, 'ShortcutFileName',
                         TSsFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShortcut, 'FileName',
                         TSsFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShortcut, 'StartInDir',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TStDropFiles, 'DropTarget',
                         TSsDropTargetProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStBrowser, 'SpecialRootFolder',
                         TSsSpecialFolderProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShortcut, 'IconPath',
                         TSsFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellTreeView, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellListView, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellComboBox, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellNavigator, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStDialogPanel, 'Version',
                         TSsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellTreeView, 'RootFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellTreeView, 'StartInFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStShellTreeView, 'SpecialRootFolder',
                         TSsSpecialFolderProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStShellTreeView, 'SpecialStartInFolder',
                         TSsSpecialFolderProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellListView, 'RootFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStShellListView, 'SpecialRootFolder',
                         TSsSpecialFolderProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellNotification, 'WatchFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStShellNotification, 'SpecialWatchFolder',
                         TSsSpecialFolderProperty);
  RegisterPropertyEditor(TypeInfo(string), TStShellEnumerator, 'RootFolder',
                         TSsDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TStSpecialRootFolder), TStShellEnumerator, 'SpecialRootFolder',
                         TSsSpecialFolderProperty);
  RegisterComponentEditor(TStShellNotification, TSsShellNotificationEditor);
  RegisterPropertyEditor(TypeInfo(string), TStDialogPanel, 'Filter', TSsFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TStDialogPanel, 'InitialDir', TSsDirectoryProperty);

  RegisterComponents('ShellShock',
    [
     TStShellAbout,
     TStBrowser,
     TStDropFiles,
     TStFileOperation,
     TStFormatDrive,
     TStShortcut,
     TStTrayIcon,
     TStShellTreeView,
     TStShellListView,
     TStShellNotification,
     TStShellEnumerator,
     TStShellComboBox,
     TStShellNavigator,
     TStDialogPanel
    ]);
end;

end.
