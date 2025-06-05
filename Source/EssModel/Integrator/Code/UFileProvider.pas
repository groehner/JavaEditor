{
  ESS-Model
  Copyright (C) 2002  Eldean AB, Peter Söderman, Ville Krumlinde

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}

unit UFileProvider;

interface

uses
  Classes,
  UCodeProvider,
  UBaseForm;

type

  {
    Implementation of a TCodeProvider using the physical filesystem.
  }
  TFileProvider = class(TCodeProvider)
  protected
    procedure HookChanges; override;
    procedure UnhookChanges; override;
  public
    function LoadStream(const AName: string; Form: TFForm = nil)
      : TStream; override;
    procedure SaveStream(const AName: string; AStream: TStream); override;
    function LocateFile(const AName: string): string; override;
  end;

implementation

uses
  SysUtils,
  UJava,
  UEditorForm,
  UUtils,
  SynUnicode;

{ TFileProvider }

function TFileProvider.LoadStream(const AName: string;
  Form: TFForm = nil): TStream;
var
  FStream: TFileStream;
  MStream: TMemoryStream;
  SStream: TStringStream;
  Lines: TStringList;
  Source: string;
  EditForm: TFEditForm;
  WithBOM: Boolean;
  Encoding: TEncoding;
begin
  Result := nil;
  try
    if AName <> '' then
    begin
      if HasClassExtension(AName) then
      begin
        FStream := TFileStream.Create(AName, fmOpenRead);
        try
          MStream := TMemoryStream.Create;
          MStream.CopyFrom(FStream, FStream.Size);
          MStream.WriteData(#0);
        finally
          FreeAndNil(FStream);
        end;
        MStream.Seek(0, soFromBeginning);
        Result := MStream;
      end
      else
      begin
        if not Assigned(Form) then
          EditForm := TFEditForm(FJava.GetTDIWindowType(AName, '%E%'))
        else
          EditForm := Form as TFEditForm;
        if Assigned(EditForm) then
        begin
          Source := EditForm.Editor.Text + #0;
          if Source = #0 then
            Source := 'null' + #0;
          Result := TStringStream.Create(Source, TEncoding.Unicode);
        end
        else if FileExists(AName) and ValidFilename(AName) then
        begin
          // used by ClassInsert
          try
            try
              Lines := TStringList.Create;
              FStream := TFileStream.Create(AName, fmOpenRead or
                fmShareDenyWrite);
              Encoding := SynUnicode.GetEncoding(FStream, WithBOM);
              Lines.LoadFromStream(FStream, Encoding);
              SStream := TStringStream.Create(Lines.Text + #0,
                TEncoding.Unicode, True);
              Result := SStream;
            except
              on e: Exception do
                ErrorMsg(e.Message);
            end;
          finally
            FreeAndNil(FStream);
            FreeAndNil(Lines);
          end;
        end
        else
          // Result:= TStringStream.Create('null' + #0, TEncoding.Unicode)
          Result := TStringStream.Create('null' + #0);
      end;
      Inc(FLoadedCount);
      AddChangeWatch(AName);
      AddSearchPath(ExtractFilePath(AName));
    end;
  except
    on e: Exception do
    begin
      ErrorMsg(e.Message);
      Result := nil;
    end;
  end;
end;

procedure TFileProvider.SaveStream(const AName: string; AStream: TStream);
var
  FileStream: TFileStream;
begin
  { TODO : Make a backup before we save the file. }
  FileStream := TFileStream.Create(AName, fmOpenWrite + fmShareExclusive);
  try
    try
      FileStream.CopyFrom(AStream, 0);
    except
      on e: Exception do
        ErrorMsg(e.Message);
    end;
  finally
    FreeAndNil(FileStream);
  end;
end;

procedure TFileProvider.HookChanges;
begin
  { TODO : Attach a filesystem listener. }

end;

procedure TFileProvider.UnhookChanges;
begin
  { TODO : Dettach the filesystem listener. }
end;

function TFileProvider.LocateFile(const AName: string): string;
var
  Posi: string;
begin
  Result := '';
  if (Pos(Copy(AName, 1, 1), '\/') > 0) or (Copy(AName, 2, 1) = ':') then
  begin
    // Filename with an absolute path
    if FileExists(AName) then
      Result := AName;
  end
  else
  begin
    // Filename without a path, use searchpath to locate it.
    for var I := 0 to SearchPath.Count - 1 do
    begin
      if FileExists(SearchPath[I] + AName) then
      begin
        Result := SearchPath[I] + AName;
        Break;
      end;
    end;
  end;

  // Add the searchpath of the file to searchpaths.
  // It will only be added if it doesn't already exist in searchpaths
  Posi := ExtractFilePath(Result);
  if (Posi <> '') and (SearchPath.IndexOf(Posi) < 0) then
    SearchPath.Add(Posi);
end;

end.
