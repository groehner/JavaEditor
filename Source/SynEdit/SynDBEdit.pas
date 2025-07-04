﻿{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynDBEdit.pas, released 2000-05-05.
The Original Code is based on DBmwEdit.pas by Vladimir Kuznetsov, part of
the mwEdit component suite.
Portions created by Vladimir Kuznetsov are Copyright (C) 1999 Vladimir Kuznetsov.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynDBEdit.pas,v 1.11.2.2 2009/06/14 13:33:38 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynDBEdit;

{$I SynEdit.inc}

interface

uses
  Windows,
  Messages,
  Controls,
  DbCtrls,
  SynEdit,
  SynEditTypes,
  SynEditKeyCmds,
  SysUtils,
  Classes,
  DB;

type
  TCustomDBSynEdit = class(TCustomSynEdit)
  private
    FDataLink: TFieldDataLink;
    fEditing: Boolean;
    FBeginEdit: Boolean;
    FLoadData: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetEditing(Value: Boolean);
    procedure UpdateData(Sender: TObject);
  private
    procedure CMEnter(var Msg: TCMEnter); message CM_ENTER;
    procedure CMExit(var Msg: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Msg: TMessage); message CM_GETDATALINK;
  protected
    function GetReadOnly: Boolean; override;
    procedure Loaded; override;
    procedure DoChange; override;
    procedure SetReadOnly(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure ExecuteCommand(Command: TSynEditorCommand; AChar: WideChar;
      Data: pointer); override;
    procedure LoadMemo;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  protected
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Field: TField read GetField;
    property OnLoadData: TNotifyEvent read fLoadData write fLoadData;
  end;

  TDBSynEdit = class(TCustomDBSynEdit)
  published
    // TCustomDBSynEdit properties
    property DataField;
    property DataSource;
    property Field;
    // TCustomDBSynEdit events
    property OnLoadData;
    // inherited properties
    property ActiveLineColor;
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property ImeMode;
    property ImeName;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property MaxUndo;
    property Options;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollBars;
    property SearchEngine;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    property OnGutterGetText;
    property OnGutterPaint;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors;
    property OnStatusChange;
    property OnPaintTransient;
  end;

implementation

constructor TCustomDBSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor TCustomDBSynEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TCustomDBSynEdit.CMEnter(var Msg: TCMEnter);
begin
  SetEditing(True);
  inherited;
end;

procedure TCustomDBSynEdit.CMExit(var Msg: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetEditing(False);
  inherited;
end;

procedure TCustomDBSynEdit.CMGetDataLink(var Msg: TMessage);
begin
  Msg.Result := Integer(FDataLink);
end;

procedure TCustomDBSynEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if FBeginEdit then
    begin
      FBeginEdit := False;
      Exit;
    end;
    if FDataLink.Field.IsBlob then
      LoadMemo
    else
      Text := FDataLink.Field.Text;
    if Assigned(FLoadData) then
      FLoadData(Self);
  end
  else
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

procedure TCustomDBSynEdit.DragDrop(Source: TObject; X, Y: Integer);
begin
  FDataLink.Edit;
  inherited;
end;

procedure TCustomDBSynEdit.EditingChange(Sender: TObject);
begin
  if FDataLink.Editing then
  begin
    if Assigned(FDataLink.DataSource)
      and (FDataLink.DataSource.State <> dsInsert)
    then
      FBeginEdit := True;
  end;
end;

procedure TCustomDBSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  AChar: WideChar; Data: pointer);
begin
  // cancel on [ESC]
  if (Command = ecChar) and (AChar = #27) then
    FDataLink.Reset
  // set editing state if editor command
  else if (Command >= ecEditCommandFirst) and (Command <= ecEditCommandLast) then
      if not FDataLink.Edit then Exit;
 
  inherited;
end;

function TCustomDBSynEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TCustomDBSynEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TCustomDBSynEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TCustomDBSynEdit.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TCustomDBSynEdit.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    DataChange(Self);
end;

procedure TCustomDBSynEdit.LoadMemo;
var
  BlobStream: TStream;
begin
  try
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmRead);
    Lines.BeginUpdate;
    if ((FDataLink.Field is  TBlobField) and
      (TBlobField(FDataLink.Field).BlobType in [ftWideMemo,ftWideString]))
    then
      Lines.LoadFromStream(BlobStream, TEncoding.Unicode)
    else
      //For UTF8 use: System.SysUtils.TEncoding.UTF8
      Lines.LoadFromStream(BlobStream, TEncoding.Default);
    Lines.EndUpdate;
    BlobStream.Free;
    Modified := False;
    ClearUndo;
  except
    // Memo too large 
    on E: EInvalidOperation do
      Lines.Text := Format('(%s)', [E.Message]);
  end;
  EditingChange(Self);
end;

procedure TCustomDBSynEdit.DoChange;
begin
  FDataLink.Modified;
  inherited;
end;

procedure TCustomDBSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource)
  then
    DataSource := nil;
end;

procedure TCustomDBSynEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TCustomDBSynEdit.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCustomDBSynEdit.SetEditing(Value: Boolean);
begin
  if fEditing <> Value then
  begin
    fEditing := Value;
    if not Assigned(FDataLink.Field) or not FDataLink.Field.IsBlob then
      FDataLink.Reset;
  end;
end;

procedure TCustomDBSynEdit.SetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;

procedure TCustomDBSynEdit.UpdateData(Sender: TObject);
var
  BlobStream: TStream;
begin
  if FDataLink.Field.IsBlob then
  begin
    BlobStream := FDataLink.DataSet.CreateBlobStream(FDataLink.Field, bmWrite);
    Lines.SaveToStream(BlobStream);
    BlobStream.Free;
  end else
    FDataLink.Field.AsString := Text;
end;

end.

