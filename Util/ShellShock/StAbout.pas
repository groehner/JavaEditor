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
{* ShellShock: StAbout.pas 1.02                          *}
{*********************************************************}
{* ShellShock: Component Wrapper for Shell About Box     *}
{*********************************************************}

{$I SsDefine.inc}

{$I+} {I/O Checking On}
{$H+} {Huge strings}

unit StAbout;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LMessages, LclType, {$ENDIF}
  Forms, Classes, Controls, Graphics, ShellApi,
  SsBase;

type
  TStCustomShellAbout = class(TSsShellComponent)
  protected {private}
{$Z+}
    FAdditionalText : string;
    FCaption        : string;
    FIcon           : TIcon;
    FTitleText      : string;
    { Private declarations }
  protected
    { Protected declarations }

    procedure SetIcon(Value : TIcon);

{$Z-}
    property AdditionalText : string
      read FAdditionalText
      write FAdditionalText;

    property Caption : string
      read FCaption
      write FCaption;

    property Icon : TIcon
      read FIcon
      write SetIcon;

    property TitleText : string
      read FTitleText
      write FTitleText;

  public
    { Public declarations }
{$Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
{$Z-}
    function Execute : Boolean;
  published
    { Published declarations }
  end;

  TStShellAbout = class(TStCustomShellAbout)
  published
    property AdditionalText;
    property Caption;
    property Icon;
    property TitleText;
  end;

implementation

constructor TStCustomShellAbout.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
end;

destructor TStCustomShellAbout.Destroy;
begin
  { Free the Icon. }
  FIcon.Free;
  inherited Destroy;
end;

function TStCustomShellAbout.Execute : Boolean;
var
  IconHandle   : Integer;
  ParentHandle : Integer;
  Text         : string;
begin
  Text := FCaption;
  if (Length(FTitleText) <> 0) then
    Text := Text + '#' + FTitleText;
  { If the icon has been assigned then pass the Icon's Handle. }
  { If the icon has not been assigned then pass 0. }
  if Assigned(FIcon) then
    IconHandle := FIcon.Handle
  else
    IconHandle := 0;

  { Get a handle to the owning window. }
  if Owner is TWinControl then
    ParentHandle := (Owner as TWinControl).Handle
  else if Owner is TApplication then
    ParentHandle := (Owner as TApplication).Handle
  else
    ParentHandle := 0;

  { Show the dialog. }
  Result := Boolean(ShellAbout(
    ParentHandle,
    PChar(Text),
    PChar(AdditionalText),
    IconHandle));
end;

procedure TStCustomShellAbout.SetIcon(Value : TIcon);
begin
  { If the icon has not yet been created, then create it. }
  if not Assigned(FIcon) then
    FIcon := TIcon.Create;

  { Assign the new value. }
  FIcon.Assign(Value);
end;


end.
