{ Copyright by Przemyslaw Jankowski }
{ e-mail: pjank@home.pl }

(* ********************************************************************************* *)
(* *)
(* Classes defined in this unit allow you to parse (and update!) any HTML data *)
(* *)
(* To use this unit you must first: *)
(* - create a THtmlParser object *)
(* - set its >Text< property to the HTML text you want to parse *)
(* Then you can "move around" this text with two methods: *)
(* - NextTag - moves you to the next tag from current position *)
(* (after setting Text current position is the beginning of the text) *)
(* - PrevTag - moves to the previous tag ("goes back") *)
(* The current tag (the tag at current position) is returned by Tag property *)
(* You have also access to the text between two tags - it's in TextBetween prop. *)
(* There are also some useful methods: *)
(* - LoadFromFile  - loads Text from the specified file from disk *)
(* - GotoBeginning - sets current position at the beginning of the text *)
(* (note: Tag and TextBetween are set to nothing) *)
(* - RemoveTag     - deletes the current tag *)
(* - InsertTag     - inserts a new tag before the current one *)
(* (the current position "moves" behind the new tag) *)
(* - InsertText    - inserts some text in the current position *)
(* *)
(* *)
(* The TTag class provides you access to everything between two brackets: < and > *)
(* - Name - this is the tag's name (e.g. 'TABLE', 'IMG' or '/BODY') *)
(* (when you read it, it always returns uppercase) *)
(* - Params - this is a TStringList with all parameters *)
(* (each line is something like: 'width=100' or 'ALT="my image"') *)
(* hint: you may use the TStringList's Names, Values properties *)
(* *)
(* *)
(* Take a look at the Demo1.pas (Button1Click) to see an example. *)
(* *)
(* ********************************************************************************* *)
(* *)
(* version 1.0 -  18.03.2000 *)
(* - fixed adding empty lines in Tag.Params *)
(* (thanks to: JulianWEB <julian@clubdelphi.com>) *)
(* - changed the name TParser to THtmlParser because of a conflict *)
(* with Classes.pas unit  (thanks: Michael Belmont) *)
(* - a little improved demo project - now shows, what's inside all TTag objects *)
(* *)
(* version 0.9  -  30.12.1999 *)
(* - first released version *)
(* *)
(* ********************************************************************************* *)
(* *)
(* Everything here is FREE *)
(* I wrote it in Delphi5 and don't know if it works in other versions *)
(* *)
(* If you find any bugs or have any comments, please let me know *)
(* (the e-mail is at the top of this file) *)
(* *)
(* The newest version is always at "Delphi Super Page" - http://delphi.icm.edu.pl *)
(* *)
(* ********************************************************************************* *)

unit UHTMLParser;

interface

uses
  Classes;

type
  TSimpleEvent = procedure of object;

  TTag = class
  private
    FName: string;
    FParams: TStrings;
    FOnChanged: TSimpleEvent;
    procedure Changed;
    function GetName: string;
    function GetText: string;
    procedure SetName(const NewName: string);
    procedure SetText(const Text: string);
    property OnChanged: TSimpleEvent read FOnChanged write FOnChanged;
  public
    constructor Create;
    destructor Destroy; override;
    property Text: string read GetText write SetText; // this is all the stuff
    // between "<" and ">"
    property Name: string read GetName write SetName;
    // tag name (returns uppercase)
    property Params: TStrings read FParams; // parameters list
  end;

  THtmlParser = class
  private
    FText: string;
    FTextBetween: string;
    FTag: TTag;
    FPos: Integer; // current position in Text
    FTagPos, FTagLen: Integer; // Tag position and length (including brackets)
    FTBPos: Integer; // TextBetween position
    function GetTag: TTag;
    procedure SetText(const NewText: string);
    procedure SetTextBetween(const Text: string);
    procedure TagChanged;
    procedure ClearTag;
    procedure ClearTB;
    procedure CheckPos;
    procedure SetTagText(const Text: string);
    function FindTag(Next: Boolean): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RemoveTag; // remove the current tag
    procedure InsertTag(NewTag: TTag);
    // insert a new tag BEFORE the current one
    procedure InsertText(const Text: string);
    // insert some text before the current tag
    function NextTag: Boolean; // find next tag from current pos.
    function PrevTag: Boolean; // find previous tag from current pos.
    procedure GotoBeginning;
    procedure GotoPos(Posi: Integer);
    function NextTR: Boolean;
    function NextDIV: Boolean;
    function GotoTag(const Tag: string): Boolean;
    function GetTDCell: string;
    function GetDIVCell: string;
    function GetCompleteClass: string;
    function GetNextLink: string;
    function DeleteLinebreaks(Str: string): string;

    property Position: Integer read FPos;
    property Text: string read FText write SetText; // here is all the HTML file
    property Tag: TTag read GetTag; // current tag
    property TextBetween: string // this is the text between two tags:
      read FTextBetween // - the last one - before calling NextTag/PrevTag
      write SetTextBetween; // - and the new (current) one
  end;

implementation

uses
  SysUtils,
  StrUtils;

{ TParams }

type
  TParams = class(TStringList)
  private
    FTag: TTag;
  public
    procedure Changed; override;
    property Tag: TTag read FTag;
  end;

procedure TParams.Changed;
begin
  inherited;
  if Assigned(FTag) then
    FTag.Changed;
end;

{ TTag }

constructor TTag.Create;
begin
  FName := '';
  FParams := TParams.Create;
  TParams(FParams).FTag := Self;
  FOnChanged := nil;
end;

destructor TTag.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TTag.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged;
end;

function TTag.GetName: string;
begin
  Result := UpperCase(FName);
end;

procedure TTag.SetName(const NewName: string);
begin
  if NewName <> FName then
  begin
    FName := NewName;
    Changed;
  end;
end;

function TTag.GetText: string;
begin
  Result := FName;
  for var I := 0 to FParams.Count - 1 do
    Result := Result + ' ' + FParams[I];
end;

procedure TTag.SetText(const Text: string);
var
  Int, Kidx: Integer;
  Len: Integer;
  Qv1, Qv2: Boolean;

  procedure AddParam;
  var
    Str: string;
  begin
    Str := Trim(Copy(Text, Kidx, Int - Kidx + 1));
    if Str <> '' then
      FParams.Add(Str);
    Kidx := Int + 1;
  end;

begin
  Qv1 := False;
  Qv2 := False;
  Len := Length(Text);

  // getting name
  Int := 1;
  while not((Int > Len) or (Text[Int] = ' ')) do
    Inc(Int);
  FName := Copy(Text, 1, Int - 1);

  Kidx := Int + 1;
  Int := Kidx;
  FParams.Clear;
  // getting parameters
  while not(Int > Len) do
  begin
    if CharInSet(Text[Int], ['''', '"']) then
    begin
      if (Text[Int] = '"') then
      begin
        if not Qv1 then
          Qv2 := not Qv2;
      end
      else
      begin
        if not Qv2 then
          Qv1 := not Qv1;
      end;
      if not(Qv1 or Qv2) then
        AddParam;
    end
    else if (Text[Int] = ' ') and not(Qv1 or Qv2) then
      AddParam;
    Inc(Int);
  end;
  if Kidx < Int then
    AddParam;
end;

{ THtmlParser }

constructor THtmlParser.Create;
begin
  FTag := TTag.Create;
  SetText('');
end;

procedure THtmlParser.SetTagText(const Text: string);
begin
  FTag.OnChanged := nil;
  FTag.Text := Text;
  FTag.OnChanged := TagChanged;
end;

destructor THtmlParser.Destroy;
begin
  FreeAndNil(FTag);
  inherited Destroy;
end;

function THtmlParser.GetTag: TTag;
begin
  if FTagPos = 0 then
    Result := nil
  else
    Result := FTag;
end;

procedure THtmlParser.ClearTag;
begin
  SetTagText('');
  FTagPos := 0;
  FTagLen := 0;
end;

procedure THtmlParser.ClearTB;
begin
  FTextBetween := '';
  FTBPos := 0;
end;

procedure THtmlParser.CheckPos;
begin
  if FPos < 1 then
    FPos := 1
  else if FPos > Length(FText) then
    FPos := Length(FText);
end;

procedure THtmlParser.InsertTag(NewTag: TTag);
begin
  CheckPos;
  Insert('<' + NewTag.Text + '>', FText, FPos);
  NextTag;
end;

procedure THtmlParser.InsertText(const Text: string);
begin
  CheckPos;
  ClearTB;
  Insert(Text, FText, FPos);
  Inc(FPos, Length(Text));
end;

procedure THtmlParser.RemoveTag;
begin
  if FTagPos = 0 then
    Exit;
  Delete(FText, FTagPos, FTagLen);
  ClearTag;
  ClearTB;
end;

procedure THtmlParser.SetText(const NewText: string);
begin
  FText := NewText;
  GotoBeginning;
end;

procedure THtmlParser.SetTextBetween(const Text: string);
begin
  if FTBPos = 0 then
    Exit;
  if Text <> FTextBetween then
  begin
    if (FTBPos <> 0) and (FTagPos > FTBPos) then
      Inc(FTagPos, Length(Text) - Length(FTextBetween));
    Delete(FText, FTBPos, Length(FTextBetween));
    Insert(Text, FText, FTBPos);
  end;
end;

procedure THtmlParser.TagChanged;
begin
  if FTagPos = 0 then
    Exit;
  Delete(FText, FTagPos + 1, FTagLen - 2);
  var
  Str := FTag.Text;
  if (FTBPos > FTagPos) then
    Inc(FTBPos, Length(Str) + 2 - FTagLen);
  FTagLen := Length(Str) + 2;
  Insert(Str, FText, FTagPos + 1);
end;

function THtmlParser.NextTag: Boolean;
begin
  Result := FindTag(True);
end;

function THtmlParser.PrevTag: Boolean;
begin
  Result := FindTag(False);
end;

function FindNext(const Text: string; Chr: Char; StartFrom: Integer;
  var Pos: Integer): Boolean;
begin
  Pos := StartFrom;
  while (Pos < Length(Text)) and (Text[Pos] <> Chr) do
    Inc(Pos);
  Result := (Text[Pos] = Chr);
end;

function FindPrev(const Text: string; Chr: Char; StartFrom: Integer;
  var Pos: Integer): Boolean;
begin
  Pos := StartFrom;
  while (Pos > 0) and (Text[Pos] <> Chr) do
    Dec(Pos);
  Result := (Text[Pos] = Chr);
end;

function THtmlParser.FindTag(Next: Boolean): Boolean;
var
  Tag1, Tag2, // first/last char of the new tag
  Tb1, Tb2: Integer; // first/last char of new TextBetween
begin

  if Length(FText) = 0 then
  begin
    Result := False;
    Exit;
  end;

  if FTagPos <> 0 then
    if Next then
      Inc(FPos)
    else
      Dec(FPos);

  CheckPos;

  if Next then
  begin
    // find next tag
    Result := FindNext(FText, '<', FPos, Tag1) and
      FindNext(FText, '>', Tag1, Tag2);
    // find end of current tag
    if FindNext(FText, '>', FPos, Tb1) and (Tb1 < Tag1) then
      Tb1 := Tb1 + 1
    else
      Tb1 := FPos;
    Tb2 := 0; // this is just to get rid of a stupid warning
  end
  else
  begin
    Tb2 := FPos;
    // find previous tag
    Result := FindPrev(FText, '>', Tb2, Tag2) and
      FindPrev(FText, '<', Tag2, Tag1);
  end;

  if Result then
  begin
    FPos := Tag1;
    if Next then
      Tb2 := Tag1 - 1
    else
      Tb1 := Tag2 + 1;
  end
  else
  begin
    if Next then
    begin
      FPos := Length(FText);
      Tb2 := Length(FText);
    end
    else
    begin
      FPos := 1;
      Tb1 := 1;
    end;
    Tag1 := 0;
    Tag2 := 0;
  end;

  FTagPos := Tag1;
  FTagLen := Tag2 - Tag1 + 1;
  SetTagText(Copy(FText, FTagPos + 1, FTagLen - 2));
  FTBPos := Tb1;
  FTextBetween := Copy(FText, FTBPos, Tb2 - Tb1 + 1);
end;

procedure THtmlParser.GotoBeginning;
begin
  FPos := 0;
  ClearTag;
  ClearTB;
end;

procedure THtmlParser.GotoPos(Posi: Integer);
begin
  FPos := Posi;
  ClearTag;
  ClearTB;
end;

function THtmlParser.NextTR: Boolean;
begin
  while NextTag do
    if (FTag.Name = 'TR') or (FTag.Name = '/TABLE') then
      Break;
  Result := (FTag.Name = 'TR');
end;

function THtmlParser.NextDIV: Boolean;
begin
  Result := False;
  while NextTag do
    if (FTag.Name = 'DIV') then
      Exit(True);
end;

function THtmlParser.GotoTag(const Tag: string): Boolean;
begin
  while NextTag do
    if (FTag.Name = Tag) or (FPos = Length(Text)) then
      Break;
  Result := (FTag.Name = Tag);
end;

function THtmlParser.DeleteLinebreaks(Str: string): string;
begin
  Str := ReplaceStr(Str, #13#10, '');
  Str := ReplaceStr(Str, #10, '');
  Str := ReplaceStr(Str, '&nbsp;', ' ');
  Str := ReplaceStr(Str, '&gt;', '>');
  Str := ReplaceStr(Str, '&lt;', '<');
  Result := Trim(Str);
end;

function THtmlParser.GetTDCell: string;
begin
  var
  Str := '';
  repeat
    NextTag;
    if FTag.Name = 'DIV' then
      Str := Str + ' ' + TextBetween
    else
      Str := Str + TextBetween;
    if FTag.Name = 'TABLE' then
      repeat
        NextTag;
        Str := Str + TextBetween;
      until FTag.Name = '/TABLE';
  until (FTag.Name = '/TD') or (FPos >= Length(FText));
  Result := DeleteLinebreaks(Str);
end;

function THtmlParser.GetDIVCell: string;
begin
  var
  Str := '';
  repeat
    NextTag;
    if FTag.Name = 'DIV' then
      Str := Str + ' ' + TextBetween
    else
      Str := Str + TextBetween;
  until (FTag.Name = '/DIV') or (FPos >= Length(FText));
  Result := DeleteLinebreaks(Str);
end;

function THtmlParser.GetCompleteClass: string;
begin
  while (FTag.Name <> 'A') and (FPos < Length(FText)) do
    NextTag;
  var
  Str := FTag.Params.Text;
  var
  Posi := Pos('href="', Str);
  if Posi > 0 then
  begin
    Delete(Str, 1, Posi + 5);
    Delete(Str, Length(Str), 1);
    Posi := Pos('#', Str);
    if Posi > 0 then
      Delete(Str, Posi, Length(Str));
    while Pos('../', Str) > 0 do
      Delete(Str, 1, 3);
    Str := ChangeFileExt(Str, '');
    Str := ReplaceStr(Str, '/', '.');
    Posi := Pos('.api.', Str);
    if Posi > 0 then
      Delete(Str, 1, Posi + 4);
    Result := Str;
  end
  else
    Result := '';
end;

function THtmlParser.GetNextLink: string;
begin
  Result := '';
  while NextTag do
    if FTag.Name = 'A' then
    begin
      var
      Str := FTag.Params[0];
      if Pos('HREF="', Str) = 1 then
      begin
        Delete(Str, 1, 6);
        Delete(Str, Length(Str), 1);
        Exit(Str);
      end;
    end;
end;

end.
