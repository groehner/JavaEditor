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
{* ShellShock: SsNotif0.pas 1.02                         *}
{*********************************************************}
{* ShellShock: Property Editor for TStShellNotification  *}
{*********************************************************}

unit SsNotif0;

interface

{$I SsDEFINE.INC}

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, StShlCtl, ExtCtrls;

type
  TShellNotifyEditorForm = class(TForm)
    Label1: TLabel;
    EventsGb: TGroupBox;
    AssociationCb: TCheckBox;
    AttributeCb: TCheckBox;
    FileCreateCb: TCheckBox;
    FileDeleteCb: TCheckBox;
    FileRenameCb: TCheckBox;
    FolderRenameCb: TCheckBox;
    FolderChangeCb: TCheckBox;
    FileChangeCb: TCheckBox;
    FolderCreateCb: TCheckBox;
    FolderDeleteCb: TCheckBox;
    DriveAddCb: TCheckBox;
    DriveRemoveCb: TCheckBox;
    ShellDriveAddCb: TCheckBox;
    NetShareCb: TCheckBox;
    NetUnShareCb: TCheckBox;
    FreeSpaceCb: TCheckBox;
    ImageListChangeCb: TCheckBox;
    ServerDisconnectCb: TCheckBox;
    MediaInsertCb: TCheckBox;
    MediaRemoveCb: TCheckBox;
    OptionsGb: TGroupBox;
    WatchSubFoldersCb: TCheckBox;
    ActiveCb: TCheckBox;
    ShellTreeView: TStShellTreeView;
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    SelectAllBtn: TButton;
    ClearAllBtn: TButton;
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SelectAllBtnClick(Sender: TObject);
    procedure ClearAllBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Events : TStNotifyEventsSet;
  end;

var
  ShellNotifyEditorForm: TShellNotifyEditorForm;

implementation

{.$R *.dfm}

procedure TShellNotifyEditorForm.OKBtnClick(Sender: TObject);
begin
  Events := [];
  if AssociationCb.Checked then
    Events := Events + [neAssociationChange];
  if AttributeCb.Checked then
    Events := Events + [neAttributesChange];
  if FileCreateCb.Checked then
    Events := Events + [neFileCreate];
  if FileDeleteCb.Checked then
    Events := Events + [neFileDelete];
  if FileRenameCb.Checked then
    Events := Events + [neFileRename];
  if FileChangeCb.Checked then
    Events := Events + [neFileChange];
  if FolderCreateCb.Checked then
    Events := Events + [neFolderCreate];
  if FolderDeleteCb.Checked then
    Events := Events + [neFolderDelete];
  if FolderRenameCb.Checked then
    Events := Events + [neFolderRename];
  if FolderChangeCb.Checked then
    Events := Events + [neFolderUpdate];
  if DriveAddCb.Checked then
    Events := Events + [neDriveAdd];
  if DriveRemoveCb.Checked then
    Events := Events + [neDriveRemove];
  if ShellDriveAddCb.Checked then
    Events := Events + [neShellDriveAdd];
  if NetShareCb.Checked then
    Events := Events + [neNetShare];
  if NetUnShareCb.Checked then
    Events := Events + [neNetUnShare];
  if ServerDisconnectCb.Checked then
    Events := Events + [neServerDisconnect];
  if MediaInsertCb.Checked then
    Events := Events + [neMediaInsert];
  if MediaRemoveCb.Checked then
    Events := Events + [neMediaRemove];
  if FreeSpaceCb.Checked then
    Events := Events + [neDriveSpaceChange];
  if ImageListChangeCb.Checked then
    Events := Events + [neImageListChange];
  ModalResult := mrOk;
end;

procedure TShellNotifyEditorForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TShellNotifyEditorForm.SelectAllBtnClick(Sender: TObject);
var
  I :  Integer;
begin
  for I := 0 to Pred(ComponentCount) do
    if Components[I] is TCheckBox then
      with Components[I] as TCheckBox do
        if Parent = EventsGb then
          Checked := True;
end;

procedure TShellNotifyEditorForm.ClearAllBtnClick(Sender: TObject);
var
  I :  Integer;
begin
  for I := 0 to Pred(ComponentCount) do
    if Components[I] is TCheckBox then
      with Components[I] as TCheckBox do
        if Parent = EventsGb then
          Checked := False;
end;

end.
