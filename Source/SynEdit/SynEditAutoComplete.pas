{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditAutoComplete.pas, released 2000-06-25.

The Initial Author of the Original Code is Michael Hieke.
Portions written by Michael Hieke are Copyright 2000 Michael Hieke.
Portions written by Cyrille de Brebisson (from mwCompletionProposal.pas) are
Copyright 1999 Cyrille de Brebisson.
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

$Id: SynEditAutoComplete.pas,v 1.10.2.4 2008/09/14 16:24:58 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditAutoComplete;

{$I SynEdit.inc}

interface

uses
  Windows,
  Menus,
  SynEdit,
  SynEditTypes,
  SynEditKeyCmds,
  SynUnicode,
  Classes;

type
  TCustomSynAutoComplete = class(TComponent)
  protected
    fAutoCompleteList: TStrings;
    fCompletions: TStrings;
    fCompletionComments: TStrings;
    fCompletionValues: TStrings;
    fEditor: TCustomSynEdit;
    fEditors: TList;
    fEOTokenChars: string;
    fCaseSensitive: Boolean;
    fParsed: Boolean;
    procedure CompletionListChanged(Sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;    
    function GetCompletions: TStrings;
    function GetCompletionComments: TStrings;
    function GetCompletionValues: TStrings;
    function GetEditorCount: Integer;
    function GetNthEditor(Index: Integer): TCustomSynEdit;
    procedure SetAutoCompleteList(Value: TStrings); virtual;
    procedure SetEditor(Value: TCustomSynEdit);
    procedure SynEditCommandHandler(Sender: TObject; AfterProcessing: Boolean;
      var Handled: Boolean; var Command: TSynEditorCommand; var AChar: WideChar;
      Data: pointer; HandlerData: pointer);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddEditor(AEditor: TCustomSynEdit): Boolean;
    function RemoveEditor(AEditor: TCustomSynEdit): Boolean;

    procedure AddCompletion(const AToken, AValue, AComment: string);
    procedure Execute(AEditor: TCustomSynEdit); virtual;
    procedure ExecuteCompletion(const AToken: string; AEditor: TCustomSynEdit);
      virtual;
    procedure ParseCompletionList; virtual;
  public
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property CaseSensitive: Boolean read fCaseSensitive write fCaseSensitive;
    property Completions: TStrings read GetCompletions;
    property CompletionComments: TStrings read GetCompletionComments;
    property CompletionValues: TStrings read GetCompletionValues;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property EditorCount: Integer read GetEditorCount;
    property Editors[Index: Integer]: TCustomSynEdit read GetNthEditor;
    property EndOfTokenChr: string read fEOTokenChars write fEOTokenChars;
  end;

  TSynAutoComplete = class(TCustomSynAutoComplete)
  published
    property AutoCompleteList;
    property CaseSensitive;
    property Editor;
    property EndOfTokenChr;
  end;

implementation

uses
  SysUtils;

{ TCustomSynAutoComplete }

procedure TCustomSynAutoComplete.AddCompletion(const AToken, AValue, AComment: string);
begin
  if AToken <> '' then
  begin
    if (fAutoCompleteList.Count = 0) and (fCompletions.Count = 0) then
      fParsed := True;
    fCompletions.Add(AToken);
    fCompletionComments.Add(AComment);
    fCompletionValues.Add(AValue);
  end;
end;

function TCustomSynAutoComplete.AddEditor(AEditor: TCustomSynEdit): Boolean;
var
  Int: Integer;
begin
  if AEditor <> nil then
  begin
    Int := fEditors.IndexOf(AEditor);
    if Int = -1 then
    begin
      AEditor.FreeNotification(Self);
      fEditors.Add(AEditor);
      AEditor.RegisterCommandHandler(SynEditCommandHandler, nil);
    end;
    Result := True;
  end
  else
    Result := False;
end;

procedure TCustomSynAutoComplete.CompletionListChanged(Sender: TObject);
begin
  fParsed := False;
end;

constructor TCustomSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAutoCompleteList := TStringList.Create;
  TStringList(fAutoCompleteList).OnChange := CompletionListChanged;
  fCompletions := TStringList.Create;
  fCompletionComments := TStringList.Create;
  fCompletionValues := TStringList.Create;
  fEditors := TList.Create;
  fEOTokenChars := '()[]{}.';
end;

destructor TCustomSynAutoComplete.Destroy;
begin
  Editor := nil;
  while EditorCount > 0 do
    RemoveEditor(TCustomSynEdit(fEditors.Last));

  inherited Destroy;
  fEditors.Free;
  fCompletions.Free;
  fCompletionComments.Free;
  fCompletionValues.Free;
  fAutoCompleteList.Free;
end;

procedure TCustomSynAutoComplete.DefineProperties(Filer: TFiler);
begin
  inherited;
end;

procedure TCustomSynAutoComplete.Execute(AEditor: TCustomSynEdit);
var
  Str: string;
  Int, j: Integer;
begin
  if AEditor <> nil then
  begin
    // get token
    Str := AEditor.LineText;
    j := AEditor.CaretX;
    Int := j - 1;
    if Int <= Length(Str) then
    begin
      while (Int > 0) and (Str[Int] > ' ') and (Pos(Str[Int], fEOTokenChars) = 0) do
        Dec(Int);
      Inc(Int);
      Str := Copy(Str, Int, j - Int);
      ExecuteCompletion(Str, AEditor);
    end;
  end;
end;

procedure TCustomSynAutoComplete.ExecuteCompletion(const AToken: string;
  AEditor: TCustomSynEdit);
var
  Int, j, Len, IndentLen: Integer;
  Str: string;
  IdxMaybe, NumMaybe: Integer;
  Posi: TBufferCoord;
  NewCaretPos: Boolean;
  Temp: TStringList;
begin
  if not fParsed then
    ParseCompletionList;
  Len := Length(AToken);
  if (Len > 0) and (AEditor <> nil) and not AEditor.ReadOnly
    and (fCompletions.Count > 0) then
  begin
    // find completion for this token - not all chars necessary if unambiguous
    Int := fCompletions.Count - 1;
    IdxMaybe := -1;
    NumMaybe := 0;
    if fCaseSensitive then
    begin
      while Int > -1 do
      begin
        Str := fCompletions[Int];
        if CompareStr(Str, AToken) = 0 then
          Break
        else if CompareStr(Copy(Str, 1, Len), AToken) = 0 then
        begin
          Inc(NumMaybe);
          IdxMaybe := Int;
        end;
        Dec(Int);
      end;
    end
    else
    begin
      while Int > -1 do
      begin
        Str := fCompletions[Int];
        if CompareText(Str, AToken) = 0 then
          Break
        else if CompareText(Copy(Str, 1, Len), AToken) = 0 then
        begin
          Inc(NumMaybe);
          IdxMaybe := Int;
        end;
        Dec(Int);
      end;
    end;
    if (Int = -1) and (NumMaybe = 1) then
      Int := IdxMaybe;
    if Int > -1 then
    begin
      // select token in editor
      Posi := AEditor.CaretXY;
      AEditor.BeginUpdate;
      try
        AEditor.BlockBegin := BufferCoord(Posi.Char - Len, Posi.Line);
        AEditor.BlockEnd := Posi;
        // indent the completion string if necessary, determine the caret pos
        IndentLen := Posi.Char - Len - 1;
        Posi := AEditor.BlockBegin;
        NewCaretPos := False;
        Temp := TStringList.Create;
        try
          Temp.Text := fCompletionValues[Int];
          // indent lines
          if (IndentLen > 0) and (Temp.Count > 1) then
          begin
            Str := StringofChar(' ', IndentLen);
            for Int := 1 to Temp.Count - 1 do
              Temp[Int] := Str + Temp[Int];
          end;
          // find first '|' and use it as caret position
          for Int := 0 to Temp.Count - 1 do
          begin
            Str := Temp[Int];
            j := Pos('|', Str);
            if j > 0 then
            begin
              Delete(Str, j, 1);
              Temp[Int] := Str;
//              if j > 1 then
//                Dec(j);
              NewCaretPos := True;
              Inc(Posi.Line, Int);
              if Int = 0 then
//                Inc(p.x, j)
                Inc(Posi.Char, j - 1)
              else
                Posi.Char := j;
              Break;
            end;
          end;
          Str := Temp.Text;
          // strip the trailing #13#10 that was appended by the stringlist
          Int := Length(Str);
          if (Int >= 2) and (Str[Int - 1] = #13) and (Str[Int] = #10) then
            SetLength(Str, Int - 2);
        finally
          Temp.Free;
        end;
        // replace the selected text and position the caret
        AEditor.SelText := Str;
        if NewCaretPos then
          AEditor.CaretXY := Posi;
      finally
        AEditor.EndUpdate;                                                
      end;
    end;
  end;
end;

function TCustomSynAutoComplete.GetCompletions: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletions;
end;

function TCustomSynAutoComplete.GetCompletionComments: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionComments;
end;

function TCustomSynAutoComplete.GetCompletionValues: TStrings;
begin
  if not fParsed then
    ParseCompletionList;
  Result := fCompletionValues;
end;

function TCustomSynAutoComplete.GetEditorCount: Integer;
begin
  Result := fEditors.Count;
end;

function TCustomSynAutoComplete.GetNthEditor(Index: Integer): TCustomSynEdit;
begin
  if (Index >= 0) and (Index < fEditors.Count) then
    Result := fEditors[Index]
  else
    Result := nil;
end;

procedure TCustomSynAutoComplete.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Editor then
      Editor := nil
    else if AComponent is TCustomSynEdit then
      RemoveEditor(TCustomSynEdit(AComponent));
  end;
end;

procedure TCustomSynAutoComplete.ParseCompletionList;
var
  BorlandDCI: Boolean;
  Int, j, Len: Integer;
  Str, sCompl, sComment, sComplValue: string;

  procedure SaveEntry;
  begin
    fCompletions.Add(sCompl);
    sCompl := '';
    fCompletionComments.Add(sComment);
    sComment := '';
    fCompletionValues.Add(sComplValue);
    sComplValue := '';
  end;

begin
  fCompletions.Clear;
  fCompletionComments.Clear;
  fCompletionValues.Clear;

  if fAutoCompleteList.Count > 0 then
  begin
    Str := fAutoCompleteList[0];
    BorlandDCI := (Str <> '') and (Str[1] = '[');

    sCompl := '';
    sComment := '';
    sComplValue := '';
    for Int := 0 to fAutoCompleteList.Count - 1 do
    begin
      Str := fAutoCompleteList[Int];
      Len := Length(Str);
      if BorlandDCI then
      begin
        // the style of the Delphi32.dci file
        if (Len > 0) and (Str[1] = '[') then
        begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          j := 2;
          while (j <= Len) and (Str[j] > ' ') do
            Inc(j);
          sCompl := Copy(Str, 2, j - 2);
          // start of comment in DCI file
          while (j <= Len) and (Str[j] <= ' ') do
            Inc(j);
          if (j <= Len) and (Str[j] = '|') then
            Inc(j);
          while (j <= Len) and (Str[j] <= ' ') do
            Inc(j);
          sComment := Copy(Str, j, Len);
          if sComment[Length(sComment)] = ']' then
            SetLength(sComment, Length(sComment) - 1);
        end
        else
        begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + Str;
        end;
      end
      else
      begin
        // the original style
        if (Len > 0) and (Str[1] <> '=') then
        begin
          // save last entry
          if sCompl <> '' then
            SaveEntry;
          // new completion entry
          sCompl := Str;
        end
        else if (Len > 0) and (Str[1] = '=') then
        begin
          if sComplValue <> '' then
            sComplValue := sComplValue + #13#10;
          sComplValue := sComplValue + Copy(Str, 2, Len);
        end;
      end;
    end;
    if sCompl <> '' then                                                        //mg 2000-11-07
      SaveEntry;
  end;
  fParsed := True;
end;

function TCustomSynAutoComplete.RemoveEditor(AEditor: TCustomSynEdit): Boolean;
var
  Int: Integer;
begin
  if AEditor <> nil then
  begin
    Int := fEditors.IndexOf(AEditor);
    if (Int > -1) then
    begin
      if fEditor = AEditor then
        fEditor := nil;
      fEditors.Delete(Int);
      AEditor.UnregisterCommandHandler(SynEditCommandHandler);
      RemoveFreeNotification(AEditor);
    end;
  end;
  Result := False;
end;

procedure TCustomSynAutoComplete.SetAutoCompleteList(Value: TStrings);
begin
  fAutoCompleteList.Assign(Value);
  fParsed := False;
end;

procedure TCustomSynAutoComplete.SetEditor(Value: TCustomSynEdit);
begin
  if Value <> fEditor then
  begin
    if fEditor <> nil then
      RemoveEditor(fEditor);
    fEditor := Value;
    if (Value <> nil) then
      AddEditor(Value);
  end;
end;

procedure TCustomSynAutoComplete.SynEditCommandHandler(Sender: TObject;
  AfterProcessing: Boolean; var Handled: Boolean;
  var Command: TSynEditorCommand; var AChar: WideChar; Data: pointer;
  HandlerData: pointer);
begin
  if not AfterProcessing and not Handled and (Command = ecAutoCompletion) then
  begin
    Handled := True;
    Execute(Sender as TCustomSynEdit);
  end;
end;

end.

