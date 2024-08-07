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
 *   Sebastian Zierer (Unicode)
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* ShellShock: StDrop.pas 1.02                           *}
{*********************************************************}
{* ShellShock: Component Wrapper for Shell Drag and Drop *}
{*********************************************************}

{$I SsDefine.inc}

{$I+} {I/O Checking On}
{$H+} {Huge strings}

unit StDrop;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  Forms, Classes, Controls, ShellApi,
  SsBase, SsConst;

{$Z+}
type

  TStDropFilesEvent = procedure(Sender : TObject;
                                Point : TPoint) of object;

  TStCustomDropFiles = class(TSsComponent)
  protected {private}
    {property variables}
    FActive           : Boolean;
    FCount            : Integer;
    FDropTarget       : TWinControl;
    FFiles            : TStrings;
    FTargetStringList : TStrings;

    {event variables}
    FOnDropFiles : TStDropFilesEvent;

    {internal variables}
    NewWndProc  : TFarProc;
    PrevWndProc : TFarProc;
    DelayedLoad : Boolean;                                             

    procedure TargetWndProc(var Msg : TMessage);
    procedure SetDropTarget(const Value: TWinControl);
    procedure SetActive(const Value: Boolean);

    procedure HookTarget;
    procedure UnhookTarget;
  protected
{$Z-}
    {properties}
    property Active : Boolean
      read FActive write SetActive default True;

    property Count : Integer
      read FCount;

    property DropTarget : TWinControl
      read FDropTarget write SetDropTarget;

    property Files : TStrings
      read FFiles;

    property TargetStringList : TStrings
      read FTargetStringList write FTargetStringList;

    {events}
    property OnDropFiles : TStDropFilesEvent
      read FOnDropFiles write FOnDropFiles;

    {methods}
    procedure DoDropFiles(Point : TPoint);

  public
{$Z+}
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
{$Z-}
  end;

  TStDropFiles = class(TStCustomDropFiles)
  public
    {properties}
    property Count;
    property Files;
    property TargetStringList;

  published
    {properties}
    property DropTarget;
    property Active;

    {events}
    property OnDropFiles;
  end;

implementation

constructor TStCustomDropFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FFiles  := TStringList.Create;
  if AOwner is TWinControl then
    if (csDesigning in ComponentState) then
      FDropTarget := AOwner as TWinControl;
end;

destructor TStCustomDropFiles.Destroy;
begin
  UnhookTarget;
  FFiles.Free;
  inherited Destroy;
end;

procedure TStCustomDropFiles.DoDropFiles(Point : TPoint);
begin
  if Assigned(OnDropFiles) then
    OnDropFiles(Self, Point);
end;

procedure TStCustomDropFiles.HookTarget;
begin
  if Assigned(FDropTarget) and                                         
     not FDropTarget.HandleAllocated and not DelayedLoad then begin    
    DelayedLoad := True;                                               
    Exit;                                                              
  end else                                                             
    DelayedLoad := False;                                              
  if not Assigned(NewWndProc) and Assigned(FDropTarget) then begin
    NewWndProc := MakeObjectInstance(TargetWndProc);
    PrevWndProc:= Pointer(
      SetWindowLong(FDropTarget.Handle, GWL_WNDPROC, LongInt(NewWndProc)));
  end;
end;

procedure TStCustomDropFiles.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then begin
    if DelayedLoad then                                                
      SetDropTarget(FDropTarget);                                      
    if FDropTarget = nil then
      SetDropTarget(Owner as TWinControl);
    if FActive then
      SetActive(True);
  end;
end;

procedure TStCustomDropFiles.Notification(AComponent: TComponent;
                                          Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (DropTarget = AComponent) and (Operation = opRemove) and
      not (csDestroying in ComponentState) then
    DropTarget := nil;
end;

procedure TStCustomDropFiles.SetActive(const Value: Boolean);
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
    DragAcceptFiles(FDropTarget.Handle, Value);
end;

procedure TStCustomDropFiles.SetDropTarget(const Value: TWinControl);
var
  OldTarget : TWinControl;
begin
  if (Value <> nil) and (not (Value is TWinControl)) then
    RaiseStError(ESsDropFilesError, ssscBadDropTarget);
  if not (csDesigning in ComponentState) then begin
    UnhookTarget;
    OldTarget := FDropTarget;
    FDropTarget := Value;
    HookTarget;
    if Assigned(OldTarget) then
      { Remove the previous drop target. }
      DragAcceptFiles(OldTarget.Handle, False);
    { Set new drop target. }
    DragAcceptFiles(FDropTarget.Handle, FActive);
  end else
    if Value <> nil then
      FDropTarget := Value
    else
      FDropTarget := Owner as TWinControl;
end;

procedure TStCustomDropFiles.TargetWndProc(var Msg : TMessage);
var
  DropHandle : Integer;
  I          : Integer;
  Buff       : array [0..MAX_PATH - 1] of Char;
  Point      : TPoint;
begin
  with Msg do begin
    if (Msg = WM_DROPFILES) then begin
      DropHandle := wParam;
      FCount := DragQueryFile(DropHandle, Cardinal(-1), nil, 0);
      if FCount > 0 then begin
        FFiles.Clear;
        for I := 0 to Pred(FCount) do begin
          DragQueryFile(DropHandle, I, Buff, Length(Buff));
          FFiles.Add(Buff);
        end;
        DragQueryPoint(DropHandle, Point);
        if Assigned(FTargetStringList) then
          FTargetStringList.Assign(FFiles);
        DoDropFiles(Point);
        DragFinish(DropHandle);
        Exit;
      end;
    end;
    if Assigned(PrevWndProc) then
      Result := CallWindowProc(PrevWndProc,
        FDropTarget.Handle, Msg, wParam, lParam);
  end;
end;

procedure TStCustomDropFiles.UnhookTarget;
begin
  {restore old wnd proc}
  if Assigned(NewWndProc) then begin
    if Assigned(PrevWndProc) and
      not (csDestroying in ComponentState) then begin
        SetWindowLong(FDropTarget.Handle, GWL_WNDPROC, LongInt(PrevWndProc));
        PrevWndProc := nil;
    end;
    FreeObjectInstance(NewWndProc);
    NewWndProc := nil;
  end;
end;


end.
