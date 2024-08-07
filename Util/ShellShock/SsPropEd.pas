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
{* ShellShock: SsPropEd.pas 1.02                         *}
{*********************************************************}
{* ShellShock: Property Editors                          *}
{*********************************************************}

{$I SsDefine.inc}

unit SsPropEd;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  Dialogs,
  Classes,
{$IFDEF VERSION6}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  Forms,
  StBrowsr,
  SsBase,
  SsNotif0,
  SsFilter0,
  StShlCtl,
  Controls,
  TypInfo;

type
  TSsDirectoryProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TSsDropTargetProperty = class(TComponentProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc : TGetStrProc); override;
  end;

  TSsSpecialFolderProperty = class(TEnumProperty)
  public
    //function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TSsFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TSsGenericFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TSsFilterProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

TSsShellNotificationEditor = class(TComponentEditor)
  public
    procedure Edit; override;
  end;

implementation

{ TSsDirectoryProperty }

function TSsDirectoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TSsDirectoryProperty.Edit;
var
  Dlg : TStBrowser;
begin
  Dlg := TStBrowser.Create(Application);
  Dlg.SpecialRootFolder := sfDrives;
  try
    if Dlg.Execute then
      Value := Dlg.SelectedFolder;
  finally
    Dlg.Free;
  end;
end;

{ TSsDropTargetProperty }

function TSsDropTargetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TSsDropTargetProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  with GetComponent(0) as TComponent do
    Proc(Owner.Name);
end;

{ TSsSpecialFolderProperty }

procedure TSsSpecialFolderProperty.GetValues(Proc: TGetStrProc);
var
  I        : Integer;
  EnumType : PTypeInfo;
  S        : string;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do begin
      S := GetEnumName(EnumType, I);
      if S = 'sfNone' then
        S := '(sfNone)';
      Proc(S);
    end;
end;

procedure TSsSpecialFolderProperty.SetValue(const Value: string);
begin
  if Value = '(sfNone)' then begin
    SetOrdValue(Ord(sfNone));
  end else
    inherited SetValue(Value);
end;

function TSsFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TSsFileNameProperty.Edit;
var
  Dlg : TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Application);
  try
    Dlg.DefaultExt := '*.exe';
    Dlg.Filter := 'Executable Files (*.exe)|*.exe' +
                  '|Dynamic Link Libraries (*.dll)|*.dll';
    Dlg.FilterIndex := 0;
    Dlg.Options := [];
    if GetName = 'ShortcutFileName' then
      Dlg.Options := [ofNoDereferenceLinks];
    Dlg.FileName := Value;
    if Dlg.Execute then
      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;


function TSsGenericFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TSsGenericFileNameProperty.Edit;
var
  Dlg : TOpenDialog;
begin
  Dlg := TOpenDialog.Create(Application);
  try
    Dlg.DefaultExt := '*.*';
    Dlg.Filter := 'Text files (*.txt)|*.txt' +
                  '|Pascal files (.pas)|*.pas' +
                  '|C++ files (*.cpp)|*.cpp' +
                  '|All files (*.*)|*.*';
    Dlg.FilterIndex := 0;
    Dlg.Options := [];
    Dlg.FileName := Value;
    if Dlg.Execute then
      Value := Dlg.FileName;
  finally
    Dlg.Free;
  end;
end;

{ TStShellNotificationEditor }
procedure TSsShellNotificationEditor.Edit;
var
  Form : TShellNotifyEditorForm;
  Res  : Integer;
begin
  inherited;
  Form := TShellNotifyEditorForm.Create(nil);
  with Component as TStShellNotification do begin
    Form.AssociationCb.Checked := (neAssociationChange in NotifyEvents);
    Form.AttributeCb.Checked := (neAttributesChange in NotifyEvents);
    Form.FileCreateCb.Checked := (neFileCreate in NotifyEvents);
    Form.FileDeleteCb.Checked := (neFileDelete in NotifyEvents);
    Form.FileRenameCb.Checked := (neFileRename in NotifyEvents);
    Form.FileChangeCb.Checked := (neFileChange in NotifyEvents);
    Form.FolderCreateCb.Checked := (neFolderCreate in NotifyEvents);
    Form.FolderDeleteCb.Checked := (neFolderDelete in NotifyEvents);
    Form.FolderRenameCb.Checked := (neFolderRename in NotifyEvents);
    Form.FolderChangeCb.Checked := (neFolderUpdate in NotifyEvents);
    Form.DriveAddCb.Checked := (neDriveAdd in NotifyEvents);
    Form.DriveRemoveCb.Checked := (neDriveRemove in NotifyEvents);
    Form.ShellDriveAddCb.Checked := (neShellDriveAdd in NotifyEvents);
    Form.NetShareCb.Checked := (neNetShare in NotifyEvents);
    Form.NetUnShareCb.Checked := (neNetUnShare in NotifyEvents);
    Form.ServerDisconnectCb.Checked := (neServerDisconnect in NotifyEvents);
    Form.MediaInsertCb.Checked := (neMediaInsert in NotifyEvents);
    Form.MediaRemoveCb.Checked := (neMediaRemove in NotifyEvents);
    Form.FreeSpaceCb.Checked := (neDriveSpaceChange in NotifyEvents);
    Form.ImageListChangeCb.Checked := (neImageListChange in NotifyEvents);
    Form.ActiveCb.Checked := Active;
    Form.WatchSubFoldersCb.Checked := WatchSubFolders;
  end;
  Res := Form.ShowModal;
  if Res = mrOk then
    with Component as TStShellNotification do begin
      NotifyEvents := Form.Events;
      Active := Form.ActiveCb.Checked;
      WatchSubFolders := Form.WatchSubFoldersCb.Checked;
      if (Form.ShellTreeView.Selected <> nil) then
        if Form.ShellTreeView.SelectedFolder.IsFileFolder then
          WatchFolder := Form.ShellTreeView.SelectedFolder.Path;
      Designer.Modified;
    end;
  Form.Free;
end;

{ TSsFilterProperty }

procedure TSsFilterProperty.Edit;
var
  Form : TFilterEdForm;
begin
  inherited;
  Form := TFilterEdForm.Create(nil);
  Form.LoadGrid(Value);
  if Form.ShowModal = mrOk then
    Value := Form.GetValue;
  Form.Free;
end;

function TSsFilterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
