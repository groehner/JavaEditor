{ Copyright by Przemyslaw Jankowski }
{ e-mail: pjank@home.pl             }

(***********************************************************************************)
(*                                                                                 *)
(*   Classes defined in this unit allow you to parse (and update!) any HTML data   *)
(*                                                                                 *)
(* To use this unit you must first:                                                *)
(*  - create a THtmlParser object                                                  *)
(*  - set its >Text< property to the HTML text you want to parse                   *)
(* Then you can "move around" this text with two methods:                          *)
(*  - NextTag - moves you to the next tag from current position                    *)
(*              (after setting Text current position is the beginning of the text) *)
(*  - PrevTag - moves to the previous tag ("goes back")                            *)
(* The current tag (the tag at current position) is returned by Tag property       *)
(* You have also access to the text between two tags - it's in TextBetween prop.   *)
(* There are also some useful methods:                                             *)
(*  - LoadFromFile  - loads Text from the specified file from disk                 *)
(*  - GotoBeginning - sets current position at the beginning of the text           *)
(*                    (note: Tag and TextBetween are set to nothing)               *)
(*  - RemoveTag     - deletes the current tag                                      *)
(*  - InsertTag     - inserts a new tag before the current one                     *)
(*                    (the current position "moves" behind the new tag)            *)
(*  - InsertText    - inserts some text in the current position                    *)
(*                                                                                 *)
(*                                                                                 *)
(* The TTag class provides you access to everything between two brackets: < and >  *)
(*  - Name - this is the tag's name (e.g. 'TABLE', 'IMG' or '/BODY')               *)
(*           (when you read it, it always returns uppercase)                       *)
(*  - Params - this is a TStringList with all parameters                           *)
(*             (each line is something like: 'width=100' or 'ALT="my image"')      *)
(*             hint: you may use the TStringList's Names, Values properties        *)
(*                                                                                 *)
(*                                                                                 *)
(* Take a look at the Demo1.pas (Button1Click) to see an example.                  *)
(*                                                                                 *)
(***********************************************************************************)
(*                                                                                 *)
(*  version 1.0 -  18.03.2000                                                      *)
(*   - fixed adding empty lines in Tag.Params                                      *)
(*     (thanks to: JulianWEB <julian@clubdelphi.com>)                              *)
(*   - changed the name TParser to THtmlParser because of a conflict               *)
(*     with Classes.pas unit  (thanks: Michael Belmont)                            *)
(*   - a little improved demo project - now shows, what's inside all TTag objects  *)
(*                                                                                 *)
(*  version 0.9  -  30.12.1999                                                     *)
(*   - first released version                                                      *)
(*                                                                                 *)
(***********************************************************************************)
(*                                                                                 *)
(* Everything here is FREE                                                         *)
(* I wrote it in Delphi5 and don't know if it works in other versions              *)
(*                                                                                 *)
(* If you find any bugs or have any comments, please let me know                   *)
(* (the e-mail is at the top of this file)                                         *)
(*                                                                                 *)
(* The newest version is always at "Delphi Super Page" - http://delphi.icm.edu.pl  *)
(*                                                                                 *)
(***********************************************************************************)

unit UHTMLParser;

interface

uses
  Classes;

type
  TSimpleEvent = procedure of object;

  TTag = class
  private
    fName: string;
    fParams: TStrings;
    fOnChanged: TSimpleEvent;
    procedure Changed;
    function GetName:string;
    function GetText:string;
    procedure SetName(const NewName:string);
    procedure SetText(const text:string);
    property OnChanged:TSimpleEvent read fOnChanged write fOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
    property Text:string read GetText write SetText;  // this is all the stuff
                                                      // between "<" and ">"
    property Name:string read GetName write SetName;  // tag name (returns uppercase)
    property Params:TStrings read fParams;            // parameters list
  end;

  THtmlParser = class
  private
    fText: string;
    fTextBetween: string;
    fTag: TTag;
    fPos: Integer;               // current position in Text
    fTagPos, fTagLen: Integer;   // Tag position and length (including brackets)
    fTBPos: Integer;             // TextBetween position
    function GetTag:TTag;
    procedure SetText(const NewText:string);
    procedure SetTextBetween(const text:string);
    procedure TagChanged;
    procedure ClearTag;
    procedure ClearTB;
    procedure CheckPos;
    procedure SetTagText(const text:string);
    function FindTag(next:Boolean):Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemoveTag;                // remove the current tag
    procedure InsertTag(NewTag: TTag);  // insert a new tag BEFORE the current one
    procedure InsertText(const text: string); // insert some text before the current tag
    function NextTag: Boolean;          // find next tag from current pos.
    function PrevTag: Boolean;          // find previous tag from current pos.
    procedure GotoBeginning;
    procedure GotoPos(p: integer);
    function NextTR: boolean;
    function NextDIV: boolean;
    function GotoTag(const Tag: string): boolean;
    function getTDCell: string;
    function getDIVCell: string;
    function getCompleteClass: string;
    function getNextLink: string;
    function DeleteLinebreaks(s: string): string;

    property Position: integer read fPos;
    property Text: string read fText write SetText;     // here is all the HTML file
    property Tag: TTag read GetTag;     // current tag
    property TextBetween:string        // this is the text between two tags:
             read fTextBetween         // - the last one - before calling NextTag/PrevTag
             write SetTextBetween;     // - and the new (current) one
  end;

implementation

uses SysUtils, StrUtils;

{ TParams }

type
  TParams = class (TStringList)
  public
    fTag: TTag;
    procedure Changed; override;
  end;

procedure TParams.Changed;
begin
  inherited;
  if Assigned(fTag) then fTag.Changed;
end;

{ TTag }

constructor TTag.Create;
begin
  fName:= '';
  fParams:= TParams.Create;
  TParams(fParams).fTag:= Self;
  fOnChanged:= nil;
end;

destructor TTag.Destroy;
begin
  FreeAndNil(fParams);
  inherited Destroy;
end;

procedure TTag.Changed;
begin
  if Assigned(fOnChanged) then fOnChanged;
end;

function TTag.GetName: string;
begin
  Result:= UpperCase(fName);
end;

procedure TTag.SetName(const NewName: string);
begin
  if NewName<>fName then begin
    fName:= NewName;
    Changed;
  end;
end;

function TTag.GetText: string;
begin
  Result:= fName;
  for var i:= 0 to fParams.Count-1 do
    Result:= Result + ' ' + fParams[i];
end;

procedure TTag.SetText(const text: string);
var i,k: Integer;
    len: Integer;
    q1,q2: Boolean;

  procedure AddParam;
  var s: string;
  begin
    s:= Trim(Copy(text,k,i-k+1));
    if s<>'' then fParams.Add(s);
    k:= i+1;
  end;

begin
  q1:= False;
  q2:= False;
  len:= Length(text);

  // getting name
  i:= 1;
  while not ((i>len) or (text[i]=' ')) do Inc(i);
  fName:= Copy(text, 1, i-1);

  k:= i+1;  i:= k;
  fParams.Clear;
  // getting parameters
  while not (i>len) do begin
    if CharInSet(text[i], ['''', '"']) then begin
      if (text[i]='"')
       then begin if not q1 then q2:= not q2 end
       else begin if not q2 then q1:= not q1 end;
      if not (q1 or q2) then AddParam;
    end else
    if (text[i]=' ') and not (q1 or q2) then AddParam;
    Inc(i);
  end;
  if k<i then AddParam;
end;

{ THtmlParser }

constructor THtmlParser.Create;
begin
  fTag:= TTag.Create;
  SetText('');
end;

procedure THtmlParser.SetTagText(const text: string);
begin
  fTag.OnChanged:= nil;
  fTag.Text:= text;
  fTag.OnChanged:= TagChanged;
end;

destructor THtmlParser.Destroy;
begin
  FreeAndNil(fTag);
  inherited Destroy;
end;

function THtmlParser.GetTag:TTag;
begin
  if fTagPos=0
   then Result:= nil
   else Result:= fTag;
end;

procedure THtmlParser.ClearTag;
begin
  SetTagText('');
  fTagPos:= 0;
  fTagLen:= 0;
end;

procedure THtmlParser.ClearTB;
begin
  fTextBetween:= '';
  fTBPos:= 0;
end;

procedure THtmlParser.CheckPos;
begin
  if fPos<1 then fPos:= 1  else
  if fPos>Length(fText) then fPos:= Length(fText);
end;

procedure THtmlParser.InsertTag(NewTag: TTag);
begin
  CheckPos;
  Insert('<'+NewTag.Text+'>', fText, fPos);
  NextTag;
end;

procedure THtmlParser.InsertText(const text: string);
begin
  CheckPos;
  ClearTB;
  Insert(text, fText, fPos);
  Inc(fPos, Length(text));
end;

procedure THtmlParser.RemoveTag;
begin
  if fTagPos=0 then Exit;
  Delete(fText, fTagPos, fTagLen);
  ClearTag;
  ClearTB;
end;

procedure THtmlParser.SetText(const NewText: string);
begin
  fText:= NewText;
  GotoBeginning;
end;

procedure THtmlParser.SetTextBetween(const text: string);
begin
  if fTBPos=0 then Exit;
  if text<>fTextBetween then begin
    if (fTBPos<>0) and (fTagPos>fTBPos) then
      Inc(fTagPos, Length(text)-Length(fTextBetween));
    Delete(fText, fTBPos, Length(fTextBetween));
    Insert(text, fText, fTBPos);
  end;
end;

procedure THtmlParser.TagChanged;
begin
  if fTagPos=0 then Exit;
  Delete(fText, fTagPos+1, fTagLen-2);
  var s:= fTag.Text;
  if (fTBPos>fTagPos) then Inc(fTBPos, Length(s)+2-fTagLen);
  fTagLen:= Length(s)+2;
  Insert(s, fText, fTagPos+1);
end;

function THtmlParser.NextTag: Boolean;
begin
  Result:= FindTag(True);
end;

function THtmlParser.PrevTag: Boolean;
begin
  Result:= FindTag(False);
end;

function FindNext(const text: string; ch:char; startfrom:Integer; var pos:Integer):Boolean;
begin
  pos:= startfrom;
  while (pos < Length(text)) and (text[pos] <> ch) do Inc(pos);
  Result:= (text[pos] = ch);
end;

function FindPrev(const text: string; ch:char; startfrom:Integer; var pos:Integer):Boolean;
begin
  pos:= startfrom;
  while (pos>0) and (text[pos]<>ch) do Dec(pos);
  Result:= (text[pos]=ch);
end;

function THtmlParser.FindTag(next: Boolean): Boolean;
var tag1, tag2,         // first/last char of the new tag
    tb1, tb2: Integer;  // first/last char of new TextBetween
begin

  if Length(fText)=0 then begin
    Result:= False;
    Exit;
  end;

  if fTagPos<>0 then
    if next then Inc(fPos) else Dec(fPos);

  CheckPos;

  if next then begin
    // find next tag
    Result:= FindNext(fText, '<', fPos, tag1) and FindNext(fText, '>', tag1, tag2);
    // find end of current tag
    if FindNext(fText, '>', fPos, tb1) and (tb1<tag1)
     then tb1:= tb1+1
     else tb1:= fPos;
    tb2:= 0; //this is just to get rid of a stupid warning
  end
  else begin
    tb2:= fPos;
    // find previous tag
    Result:= FindPrev(fText, '>', tb2, tag2) and FindPrev(fText, '<', tag2, tag1);
  end;

  if Result then begin
    fPos:= tag1;
    if next
     then tb2:= tag1-1
     else tb1:= tag2+1;
  end
  else begin
    if next then begin
      fPos:= Length(fText);
      tb2:= Length(fText);
    end
    else begin
      fPos:= 1;
      tb1:= 1;
    end;
    tag1:= 0;
    tag2:= 0;
  end;

  fTagPos:= tag1;
  fTagLen:= tag2-tag1+1;
  SetTagText(Copy(fText, fTagPos+1, fTagLen-2));
  fTBPos:= tb1;
  fTextBetween:= Copy(fText, fTBPos, tb2-tb1+1);
end;

procedure THtmlParser.GotoBeginning;
begin
  fPos:= 0;
  ClearTag;
  ClearTB;
end;

procedure THtmlParser.GotoPos(p: integer);
begin
  fPos:= p;
  ClearTag;
  ClearTB;
end;

function THtmlParser.NextTR: boolean;
begin
  while NextTag do
    if (fTag.Name = 'TR') or (fTag.Name = '/TABLE') then break;
  Result:= (fTag.Name = 'TR');
end;

function THtmlParser.NextDIV: boolean;
begin
  Result:= false;
  while NextTag do
    if (fTag.Name = 'DIV') then
      Exit(true);
end;

function THtmlParser.GotoTag(const Tag: string): boolean;
begin
  while NextTag do
    if (fTag.Name = Tag) or (fPos = Length(Text)) then break;
  Result:= (ftag.Name = Tag);
end;

function THtmlParser.DeleteLinebreaks(s: string): string;
begin
  s:= ReplaceStr(s, #13#10, '');
  s:= ReplaceStr(s, #10, '');
  s:= ReplaceStr(s, '&nbsp;', ' ');
  s:= ReplaceStr(s, '&gt;', '>');
  s:= ReplaceStr(s, '&lt;', '<');
  Result:= trim(s);
end;

function THtmlParser.getTDCell: string;
begin
  var s:= '';
  repeat
    NextTag;
    if fTag.Name='DIV'
      then s:= s + ' ' + TextBetween
      else s:= s + TextBetween;
    if fTag.Name = 'TABLE' then
      repeat
        NextTag;
        s:= s+ TextBetween;
      until fTag.Name = '/TABLE';
  until (fTag.Name = '/TD') or (fPos >= length(FText));
  Result:= DeleteLinebreaks(s);
end;

function THtmlParser.getDIVCell: string;
begin
  var s:= '';
  repeat
    NextTag;
    if fTag.Name='DIV'
      then s:= s + ' ' + TextBetween
      else s:= s + TextBetween;
  until (fTag.Name = '/DIV') or (fPos >= length(FText));
  Result:= DeleteLinebreaks(s);
end;

function THtmlParser.getCompleteClass: string;
begin
  while (fTag.Name <> 'A') and (fPos < Length(fText)) do
    NextTag;
  var s:= fTag.Params.Text;
  var p:= Pos('href="', s);
  if p > 0 then begin
    delete(s, 1, p+5);
    delete(s, length(s), 1);
    p:= Pos('#', s);
    if p > 0 then
      delete(s, p, length(s));
    while Pos('../', s) > 0 do
      delete(s, 1, 3);
    s:= ChangeFileExt(s, '');
    s:= ReplaceStr(s, '/', '.');
    p:= Pos('.api.', s);
    if p > 0  then
      Delete(s, 1, p + 4);
    Result:= s;
  end
  else
    Result:= '';
end;

function THtmlParser.getNextLink: string;
begin
  Result:= '';
  while NextTag do
    if fTag.Name = 'A' then begin
      var s:= fTag.Params.Strings[0];
      if Pos('HREF="', s) = 1 then begin
        delete(s, 1, 6);
        delete(s, length(s), 1);
        exit(s);
      end;
    end;
end;

end.
