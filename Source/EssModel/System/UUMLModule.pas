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

unit UUMLModule;

interface

uses
  Classes,
  ExtCtrls,
  UModel,
  UViewIntegrator;

type
  TDMUMLModule = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure SaveDiagramActionExecute(Sender: TObject);
    function OpenFolderActionExecute(Sender: TObject): Boolean;
  private
    FModel: TObjectModel;
    FDiagram: TDiagramIntegrator;
    // TRtfdDiagram is descendent of TDiagramIntegrator
    FOpendFolder: string;
  public
    constructor Create(AOwner: TComponent; Panel: TPanel); reintroduce;
    procedure LoadProject(Filenames: TStrings); overload;
    procedure LoadProject(const Filename: string); overload;
    procedure AddToProject(const Filename: string); overload;
    procedure AddToProject(Filenames: TStrings); overload;
    procedure ShowAllOpenedFiles;
    procedure SaveUML(const Pathname: string);
    procedure LoadUML(const Pathname: string);
    function GetUMLFilename: string;
    function GetClasses: TStringList;
    procedure InitShowParameter(Num: Integer);
    procedure SetShowIcons(Num: Integer);
    procedure RefreshDiagram;
    procedure DoLayout;
    procedure EditSelectedElement;
    procedure UnSelectAllElements;
    procedure SelectAssociation;
    function HasEditableClass: Boolean;
    procedure Print;

    property Diagram: TDiagramIntegrator read FDiagram;
    property Model: TObjectModel read FModel;
    property OpendFolder: string read FOpendFolder;
  end;

implementation

uses
  System.IOUtils,
  Dialogs,
  SysUtils,
  Graphics,
  Controls,
  Forms,
  Contnrs,
  UUtils,
  UFileProvider,
  UDlgOpenFolder,
  UIntegrator,
  UConfiguration,
  URtfdDiagram,
  UJava;

{$R *.DFM}

constructor TDMUMLModule.Create(AOwner: TComponent; Panel: TPanel);
begin
  inherited Create(AOwner);
  FModel := TObjectModel.Create;
  FDiagram := TRtfdDiagram.Create(FModel, Panel);
end;

procedure TDMUMLModule.DataModuleDestroy(Sender: TObject);
begin
  FDiagram.Free;
  FModel.Free;
end;

procedure TDMUMLModule.LoadProject(Filenames: TStrings);
var
  Ext: string;
  Imp: TImportIntegrator;
  Exts: TStringList;
  JPos: Integer;
  SikCursor: TCursor;
  OtherFiles: TStringList;

  function SeparateFiles(Ext: string): TImportIntegrator;
  var Ints: TClassList;
  begin
    Result := nil;
    Ints := Integrators.Get(TImportIntegrator);
    OtherFiles := TStringList.Create;
    for var I := 0 to Ints.Count - 1 do
    begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        if Exts.IndexOfName(Ext) > -1 then
        begin
          Result := TImportIntegratorClass(Ints[I])
            .Create(Model, TFileProvider.Create);
          JPos := 0;
          while JPos < Filenames.Count do
            if Exts.IndexOfName(LowerCase(ExtractFileExt(Filenames[JPos]))) = -1
            then
            begin
              OtherFiles.Add(Filenames[JPos]);
              Filenames.Delete(JPos);
            end
            else
              Inc(JPos);
          Break;
        end;
      finally
        Exts.Free;
        Ints.Free;
      end;
    end;
  end;

begin
  // Examine fileextension and call the correct integrator
  SikCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Ext := '';
  for var Path in Filenames do
    if LowerCase(ExtractFileExt(Path)) = '.java' then
      Ext := '.java';
  if (Ext = '') and (Filenames.Count > 0) then
    Ext := LowerCase(ExtractFileExt(Filenames[0]));

  Imp := nil;
  OtherFiles := nil;
  try
    Imp:= SeparateFiles(Ext);
    if Assigned(Imp) then
      Imp.BuildModelFrom(Filenames);
    AddToProject(OtherFiles);
  finally
    Imp.Free;
    OtherFiles.Free;
    Screen.Cursor := SikCursor;
  end;
end;

procedure TDMUMLModule.LoadProject(const Filename: string);
begin
  var
  StringList := TStringList.Create;
  StringList.Add(Filename);
  LoadProject(StringList);
  StringList.Free;
end;

procedure TDMUMLModule.AddToProject(const Filename: string);
var
  Ext: string;
  Imp: TImportIntegrator;
  Ints: TClassList;
  Exts: TStringList;
begin
  try
    if Assigned(FModel) and Assigned(FModel.ModelRoot) and
      Assigned(FModel.ModelRoot.Files) and (FModel.ModelRoot.Files.Text = '')
    then
    begin
      LoadProject(Filename);
      FDiagram.ResolveAssociations;
      FDiagram.ResolveObjectAssociations;
      Exit;
    end;
  except
    on e: Exception do
      FConfiguration.Log('TDMUMLModule.AddToProject ', e);
  end;
  Ext := LowerCase(ExtractFileExt(Filename));
  Imp := nil;
  Ints := Integrators.Get(TImportIntegrator);
  try
    for var I := 0 to Ints.Count - 1 do
    begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        if Exts.IndexOfName(Ext) > -1 then
          Imp := TImportIntegratorClass(Ints[I])
            .Create(Model, TFileProvider.Create);
      finally
        Exts.Free;
      end;
    end;

    if Assigned(Imp) then
      try
        Imp.AddFileToModel(Filename);
        FDiagram.ResolveAssociations;
        FDiagram.ResolveObjectAssociations;
      finally
        Imp.Free;
      end;
  finally
    Ints.Free;
  end;
end;

procedure TDMUMLModule.AddToProject(Filenames: TStrings);
begin
  for var Path in Filenames do
    AddToProject(Path);
end;

procedure TDMUMLModule.FileOpenActionExecute(Sender: TObject);
var
  Ints: TClassList;
  Exts: TStringList;
  AnyFilter, AFilter: string;
begin
  AFilter := '';
  Ints := Integrators.Get(TImportIntegrator);
  try
    for var I := 0 to Ints.Count - 1 do
    begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        for var J := 0 to Exts.Count - 1 do
        begin
          if AFilter <> '' then
            AFilter := AFilter + '|';
          AFilter := AFilter + Exts.Values[Exts.Names[J]] + ' (*' +
            Exts.Names[J] + ')|*' + Exts.Names[J];
          if AnyFilter <> '' then
            AnyFilter := AnyFilter + ';';
          AnyFilter := AnyFilter + '*' + Exts.Names[J];
        end;
      finally
        Exts.Free;
      end;
    end;
  finally
    Ints.Free;
  end;
  AFilter := 'All types (' + AnyFilter + ')|' + AnyFilter + '|' + AFilter;
  with TOpenDialog.Create(Self) do
  begin
    Filter := AFilter;
    Options := Options + [ofAllowMultiSelect];
    if Execute then
      LoadProject(Files);
    Free;
  end;
end;

procedure TDMUMLModule.SaveDiagramActionExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do
  begin
    InitialDir := ExtractFilePath(Model.ModelRoot.GetConfigFile);
    Filter := 'SVG files (*.svg)|*.svg|PNG files (*.png)|*.png|All files (*.*)|*.*';
    Options := Options + [ofOverwritePrompt];
    if Execute then
    begin
      if ExtractFileExt(FileName) = '' then
      begin
        if FilterIndex = 2 then
          FileName := ChangeFileExt(FileName, '.png')
        else
          FileName := ChangeFileExt(FileName, '.svg');
      end;
      Diagram.SaveAsPicture(FileName);
    end;
    Free;
  end;
end;

procedure TDMUMLModule.ShowAllOpenedFiles;
var
  Files: TStringList;
  AFile: string;
begin
  Files := TStringList.Create;
  for var I := 0 to FJava.TDIEditFormCount - 1 do
  begin
    AFile := FJava.TDIEditFormGet(I).Pathname;
    if HasJavaExtension(AFile) or HasPascalExtension(AFile) then
      Files.Add(AFile);
  end;
  LoadProject(Files);
  Files.Free;
end;

procedure TDMUMLModule.SaveUML(const Pathname: string);
begin
  Diagram.StoreDiagram(Pathname);
end;

procedure TDMUMLModule.LoadUML(const Pathname: string);
begin
  Diagram.FetchDiagram(Pathname);
end;

function TDMUMLModule.GetUMLFilename: string;
begin
  if Assigned(Diagram.Package) then
    Result := Diagram.Package.GetConfigFile
  else
    Result := '';
end;

procedure TDMUMLModule.InitShowParameter(Num: Integer);
begin
  Diagram.InitShowParameter(Num);
end;

procedure TDMUMLModule.SetShowIcons(Num: Integer);
begin
  Diagram.ShowIcons := Num;
end;

function TDMUMLModule.OpenFolderActionExecute(Sender: TObject): Boolean;

  procedure _AddFileNames(Files: TStringList; const Path, Ext: string;
    Recursive: Boolean);
  var
    SearchOption: TSearchOption;
  begin
    if Recursive
      then SearchOption := TSearchOption.soAllDirectories
      else SearchOption := TSearchOption.soTopDirectoryOnly;
    if DirectoryExists(Path) then begin
      var Filenames := TDirectory.GetFiles(Path, '*' + Ext, SearchOption);
      Files.AddStrings(Filenames);
    end;
  end;

begin
  Result := False;
  var
  StringList := TStringList.Create;
  var
  Ints := Integrators.Get(TImportIntegrator);
  var
  OpenFolderForm := TFOpenFolderDialog.Create(nil);
  try
    for var I := 0 to Ints.Count - 1 do
    begin
      var
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        OpenFolderForm.CBFiletype.Items.Add('*' + Exts.Names[0]);
      finally
        Exts.Free;
      end;
    end;
    OpenFolderForm.CBFiletype.ItemIndex := 0;
    if OpenFolderForm.ShowModal = mrOk then
    begin
      Result := True;
      _AddFileNames(StringList, OpenFolderForm.PathTreeView.Path,
        Copy(OpenFolderForm.CBFiletype.Items
        [OpenFolderForm.CBFiletype.ItemIndex], 2, 10),
        OpenFolderForm.CBWithSubFolder.Checked);
      if StringList.Count > 0 then
        LoadProject(StringList)
      else
        ShowMessage('No files found.');
      FOpendFolder := OpenFolderForm.PathTreeView.Path;
    end
    else
      FOpendFolder := '';
  finally
    StringList.Free;
    Ints.Free;
    OpenFolderForm.Release;
  end;
end;

procedure TDMUMLModule.RefreshDiagram;
begin
  Diagram.RefreshDiagram;
end;

procedure TDMUMLModule.DoLayout;
begin
  Diagram.DoLayout;
end;

procedure TDMUMLModule.SelectAssociation;
begin
  Diagram.SelectAssociation;
end;

procedure TDMUMLModule.EditSelectedElement;
begin
  Diagram.ClassEditSelectedDiagramElements;
end;

procedure TDMUMLModule.UnSelectAllElements;
begin
  Diagram.UnSelectAllElements;
end;

function TDMUMLModule.HasEditableClass: Boolean;
begin
  if Assigned(Diagram) then
    Result := Diagram.HasEditableClass
  else
    Result := False;
end;

procedure TDMUMLModule.Print;
var
  DBX, DBY: Integer;
  FormBitmap: TBitmap;
begin
  FormBitmap := TBitmap.Create;
  Diagram.GetDiagramSize(DBX, DBY);
  FormBitmap.Width := DBX - 1;
  FormBitmap.Height := DBY - 1;
  try
    Diagram.PaintTo(FormBitmap.Canvas, 0, 0, False);
    PrintBitmap(FormBitmap, TForm(Owner).PixelsPerInch);
  finally
    FormBitmap.Free;
  end;
end;

function TDMUMLModule.GetClasses: TStringList;
var
  Dir, Filename: string;
begin
  Result := Diagram.GetClasses;
  if FModel.ModelRoot.Files.Count = 0 then
    Exit;

  Dir := ExtractFilePath(FModel.ModelRoot.Files[0]);
  for var I := 0 to Result.Count - 1 do
  begin
    Filename := Dir + Result[I] + '.java';
    if (FModel.ModelRoot.Files.IndexOf(Filename) = -1) and FileExists(Filename)
    then
    begin
      AddToProject(Filename);
      FModel.ModelRoot.Files.Add(Filename);
    end;
  end;
end;

end.
