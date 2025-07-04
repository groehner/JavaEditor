{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterManager.pas, released 2000-04-14.

The Original Code is based on mwHighlighterManager.pas by Primoz Gabrijelcic,
part of the mwEdit component suite.
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

$Id: SynHighlighterManager.pas,v 1.7.2.2 2008/03/01 18:32:02 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - does not work when dropped on a frame in Delphi 5
-------------------------------------------------------------------------------}
{
@abstract(Provides a component to manage many highlighters in a single project.)
@author(Primoz Gabrijelcic)
@created(1999, converted to SynEdit 2000-04-14)
@lastmod(2000-04-14)
Provides a component to manage many highlighters in a single project.
}

unit SynHighlighterManager;

{$I SynEdit.inc}

interface

uses
  Classes;

type
  {:Highlighter manager.<p>
    Design-only component, designed to simplify work with highlighter components.<p>
    When placed on the form, SynHighlighterManager scans the form for highlighter
    components (descendants of TSynCustomHighlighter). Next it presents the user with
    small form containing checkboxed list and some buttons. User can select (by
    checking/unchecking list items) highlighter that should be placed onto the
    form. After user clicks OK, SynHighlighterManager synchronises highlighter
    components on the form with required state.<p>
    Built-in tricks:<br>
    - SynHighlighterManager never covers existing TComponent with a highlighter.<br>
    - SynHighlighterManager scans the form for TSynCustomHighlighter descendants and
      uses topmost and leftmost component as a starting point for insertion. If
      no TSynCustomHighlighter components are found, first highlighter will be placed
      at coordinates (8,8).<p>
    Known issues:<br>
    - If you place TSynHighlighterManager by double-clicking its icon in
      component palette, it will function normally, except that when all is
      done, Delphi will disply small window with title "Error" and message
      "Operation aborted". Purely cosmetic issue for which there is no obvious
      workaround. Live with it.<p>
    Last change: 2000-01-21

    @author Primoz Gabrijelcic
    @version 0.1
    @component
    @see TSynEditHighlighter
  :}
  TSynHighlighterManager = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
  published
  end;

implementation

uses
  DesignIntf,
  Windows,
  Forms,
  Controls,
  StdCtrls,
  CheckLst,
  SynEditHighlighter,
  SynEditStrConst,
  SysUtils;

type
  TSynHighlighterForm = class(TForm)
    clbHighlighters: TCheckListBox;
    btnSelectAll: TButton;
    btnDeselectAll: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    Highlight: TSynHighlighterList;
    constructor Create(highlighters: TSynHighlighterList); reintroduce;
    procedure   LoadForm;
    procedure   SelectAll(Sender: TObject);
    procedure   DeselectAll(Sender: TObject);
  end;

  TDesignerClass = IDesigner;

{ TSynHighlighterManager }

constructor TSynHighlighterManager.Create(AOwner: TComponent);
var
  form: TCustomForm;
  dsgn: TDesignerClass;
  highlight: TSynHighlighterList;
  synForm: TSynHighlighterForm;

  procedure CheckExisting;
  var
    Int: Integer;
    j: Integer;
  begin
    for Int := 0 to form.ComponentCount-1 do begin
      j := highlight.FindByClass(form.Components[Int]);
      if j >= 0 then begin
        j := synForm.clbHighlighters.Items.IndexOf(highlight[j].GetFriendlyLanguageName);
        if j >= 0 then
          synForm.clbHighlighters.Checked[j] := True;
      end;
    end; //for
  end;

  function FindHighlighterComp(hlClass: TSynCustomHighlighterClass): Integer;
  var
    Int: Integer;
  begin
    Result := -1;
    for Int := 0 to form.ComponentCount-1 do begin
      if form.Components[Int] is hlClass then begin
        Result := Int;
        Exit;
      end;
    end; //for
  end;

  procedure PlaceNew;
  var
    Int: Integer;
    High: Integer;
    comp: Integer;
    xpos, ypos: Integer;
    xstart: Integer;

    procedure GetStartCoordinates;
    var
      compTop: Integer;
      compLeft: Integer;
      Int: Integer;
    begin
      xpos := -1;
      ypos := -1;
      for Int := 0 to form.ComponentCount-1 do begin
        if form.Components[Int] is TSynCustomHighlighterClass then begin
          compLeft := LongRec(form.Components[Int].DesignInfo).Lo;
          compTop  := LongRec(form.Components[Int].DesignInfo).Hi;
          if (xpos < 0) or (compLeft < xpos) then
            xpos := compLeft;
          if (ypos < 0) or (compTop < ypos) then
            ypos := compTop;
        end;
      end; //for
      if xpos < 0 then
        xpos := 8;
      if ypos < 0 then
        ypos := 8;
      xstart := xpos;
    end;

    procedure IncCoordinates;
    begin
      Inc(xpos,32);
      if (xpos+32) >= form.ClientWidth then begin
        xpos := xstart;
        Inc(ypos,32);
      end;
    end;

    function CoordinatesTaken: Boolean;
    var
      compTop: Integer;
      compLeft: Integer;
      compRect: TRect;
      testRect: TRect;
      interRect: TRect;
      Int: Integer;
    begin
      Result := False;
      testRect := Rect(xpos,ypos,xpos+31,ypos+31);
      for Int := 0 to form.ComponentCount-1 do begin
        if (form.Components[Int] <> Self) and (not (form.Components[Int] is TControl)) then begin
          compLeft := LongRec(form.Components[Int].DesignInfo).Lo;
          compTop  := LongRec(form.Components[Int].DesignInfo).Hi;
          compRect := Rect(compLeft,compTop,compLeft+31,compTop+31);
          if IntersectRect(interRect,testRect,compRect) then begin
            Result := True;
            Exit;
          end;
        end;
      end; //for
    end;

    procedure GetFreeCoordinates;
    begin
      while CoordinatesTaken do
        IncCoordinates;
    end;

  begin
    GetStartCoordinates;
    // Iterate over TCheckListBox, not over GetPlaceableHighlighters to ensure
    // inserted highlighters to be sorted by name.
    // Iterate twice - delete highlighters in first pass (to make place), create
    // in second.
    for Int := 0 to synForm.clbHighlighters.Items.Count-1 do begin
      if not synForm.clbHighlighters.Checked[Int] then begin // unchecked - remove
        High := highlight.FindByFriendlyName(synForm.clbHighlighters.Items[Int]);
        if High >= 0 then begin
          comp := FindHighlighterComp(highlight[High]);
          if comp >= 0 then
            form.Components[comp].Free;
        end;
      end;
    end; //for
    for Int := 0 to synForm.clbHighlighters.Items.Count-1 do begin
      if synForm.clbHighlighters.Checked[Int] then begin // checked - add
        High := highlight.FindByFriendlyName(synForm.clbHighlighters.Items[Int]);
        if High >= 0 then begin
          if FindHighlighterComp(highlight[High]) < 0 then begin
            GetFreeCoordinates;
            dsgn.CreateComponent(highlight[High],AOwner,xpos,ypos,24,24);
            IncCoordinates;
          end;
        end;
      end;
    end; //for
  end;

begin
  inherited;
  if (csDesigning in ComponentState) and (AOwner is TCustomForm) then begin
    form := TCustomForm(AOwner);
    dsgn := form.Designer as TDesignerClass;
    highlight := GetPlaceableHighlighters;
    if highlight.Count = 0 then
      Application.MessageBox('No highlighters found!','Highlighter Manager', MB_OK + MB_ICONEXCLAMATION)
    else
    begin
      synForm := TSynHighlighterForm.Create(highlight);
      try
        CheckExisting;
        if synForm.ShowModal = mrOk then
          PlaceNew;
      finally
        synForm.Free;
      end;
    end;
  end;
  SysUtils.Abort;
end;

{ TSynHighlighterForm }

constructor TSynHighlighterForm.Create(highlighters: TSynHighlighterList);
begin
  CreateNew(nil);
  Caption := 'Highlighter Manager';
  Width  := 410;
  Height := 243;
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  Highlight := highlighters;
  
//object clbHighlighters: TCheckListBox
//  Left = 8
//  Top = 8
//  Width = 305
//  Height = 201
//  ItemHeight = 13
//  TabOrder = 0
//end

//object btnSelectAll: TButton
//  Left = 320
//  Top = 8
//  Width = 75
//  Height = 25
//  Caption = '&Select All'
//  TabOrder = 1
//end

//object btnDeselectAll: TButton
//  Left = 320
//  Top = 40
//  Width = 75
//  Height = 25
//  Caption = '&Deselect All'
//  TabOrder = 2
//end

//object btnOK: TButton
//  Left = 320
//  Top = 152
//  Width = 75
//  Height = 25
//  Caption = 'OK'
//  Default = True
//  ModalResult = 1
//  TabOrder = 3
//end

//object btnCancel: TButton
//  Left = 320
//  Top = 184
//  Width = 75
//  Height = 25
//  Caption = 'Cancel'
//  ModalResult = 2
//  TabOrder = 4
//end

  clbHighlighters := TCheckListBox.Create(Self);
  btnSelectAll := TButton.Create(Self);
  btnDeselectAll := TButton.Create(Self);
  btnOK := TButton.Create(Self);
  btnCancel := TButton.Create(Self);
  with clbHighlighters do
  begin
    Name := 'clbHighlighters';
    Parent := Self;
    Left := 8;
    Top := 8;
    Width := 305;
    Height := 201;
    ItemHeight := 13;
    Sorted := True;
    TabOrder := 0;
  end;
  with btnSelectAll do
  begin
    Name := 'btnSelectAll';
    Parent := Self;
    Left := 320;
    Top := 8;
    Width := 75;
    Height := 25;
    Caption := '&Select All';
    TabOrder := 1;
    OnClick := SelectAll;
  end;
  with btnDeselectAll do
  begin
    Name := 'btnDeselectAll';
    Parent := Self;
    Left := 320;
    Top := 40;
    Width := 75;
    Height := 25;
    Caption := '&Deselect All';
    TabOrder := 2;
    OnClick := DeselectAll;
  end;
  with btnOK do
  begin
    Name := 'btnOK';
    Parent := Self;
    Left := 320;
    Top := 152;
    Width := 75;
    Height := 25;
    Caption := 'OK';
    Default := True;
    ModalResult := 1;
    TabOrder := 3;
  end;
  with btnCancel do
  begin
    Name := 'btnCancel';
    Parent := Self;
    Left := 320;
    Top := 184;
    Width := 75;
    Height := 25;
    Caption := 'Cancel';
    ModalResult := 2;
    TabOrder := 4;
  end;
  LoadForm;
end;

procedure TSynHighlighterForm.DeselectAll(Sender: TObject);
var
  Int: Integer;
begin
  for Int := 0 to clbHighlighters.Items.Count-1 do
    clbHighlighters.Checked[Int] := False;
end;

procedure TSynHighlighterForm.LoadForm;
var
  Int: Integer;
begin
  clbHighlighters.Clear;
  for Int := 0 to Highlight.Count-1 do begin
    clbHighlighters.Items.Add(Highlight[Int].GetFriendlyLanguageName); 
  end; //for
end;

procedure TSynHighlighterForm.SelectAll(Sender: TObject);
var
  Int: Integer;
begin
  for Int := 0 to clbHighlighters.Items.Count-1 do
    clbHighlighters.Checked[Int] := True;
end;

end.
