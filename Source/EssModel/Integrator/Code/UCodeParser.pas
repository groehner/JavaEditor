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

unit UCodeParser;

interface

uses
  Classes,
  UModel,
  UParseThread;

type

  TNeedPackageEvent = procedure(var AName: string; Packagename: string;
    var AStream: TStream; OnlyLookUp: Boolean = False) of object;

  {
    Baseclass for a code parser.
  }
  TCodeParser = class(TObject)
  private
    FNeedPackage: TNeedPackageEvent;
    FThread: TParseThread;
  protected
    FModel: TAbstractPackage;
    // Moves data from a stream to a memory stream and adds a terminating #0 for codeparsers.
    function StreamToMemory(AStream: TStream): TMemoryStream;
  public
    // Parse the given stream into the AOM model with AModel as a 'dump' for unresolved objects.
    procedure ParseStream(AStream: TStream; AModel: TAbstractPackage;
      AOM: TObjectModel; Filename: string; Inner: Boolean;
      WithoutNeedSource: Boolean); virtual; abstract;

    property Thread: TParseThread read FThread write FThread;
    // Used to call the providers function to retrieve a package in a stream.
    property NeedPackage: TNeedPackageEvent read FNeedPackage
      write FNeedPackage;
  end;

implementation

uses SysUtils;

{ TCodeParser }

function TCodeParser.StreamToMemory(AStream: TStream): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.LoadFromStream(AStream);
  FreeAndNil(AStream);
  Result.SetSize(Result.Size + 1);
  PChar(Result.Memory)[Result.Size] := #0;
end;

end.
