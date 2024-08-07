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
  Classes, ExtCtrls, uModel, uViewIntegrator;

type
  TDMUMLModule = class(TDataModule)
    procedure DataModuleDestroy(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure SaveDiagramActionExecute(Sender: TObject);
    function OpenFolderActionExecute(Sender: TObject): boolean;
  private
    FModel  : TObjectModel;
    FDiagram: TDiagramIntegrator;  // TRtfdDiagram is descendent of TDiagramIntegrator
  public
    OpendFolder: string;
    property Diagram: TDiagramIntegrator read FDiagram;
    property Model: TObjectModel read FModel;

    constructor Create(AOwner: TComponent; Panel: TPanel); reintroduce;
    procedure LoadProject(FileNames : TStrings); overload;
    procedure LoadProject(const FileName : string); overload;
    procedure AddToProject(const FileName: string); overload;
    procedure AddToProject(FileNames: TStrings); overload;
    procedure ShowAllOpenedFiles;
    procedure SaveUML(const Pathname: string);
    procedure LoadUML(const Pathname: string);
    function  getUMLFilename: string;
    function  getClasses: TStringList;
    procedure InitShowParameter(i: integer);
    procedure SetShowIcons(i: integer);
    procedure RefreshDiagram;
    procedure DoLayout;
    procedure EditSelectedElement;
    procedure UnSelectAllElements;
    procedure SelectAssociation;
    function  hasEditableClass: boolean;
    procedure Print;
  end;

implementation

uses Dialogs, SysUtils, Graphics, Controls, Forms, Clipbrd, Printers, Contnrs, IniFiles,
  UFileProvider, UJava, UDlgOpenFolder, UIntegrator,
  UUtils, UConfiguration, URtfdDiagram;

{$R *.DFM}

constructor TDMUMLModule.Create(AOwner: TComponent; Panel: TPanel);
begin
  inherited Create(AOwner);
  FModel := TObjectModel.Create;
  FDiagram:= TRtfdDiagram.Create(FModel, Panel);
end;

procedure TDMUMLModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FDiagram);
  FreeAndNil(FModel);
  inherited;
end;

procedure TDMUMLModule.LoadProject(FileNames : TStrings);
var
  Ext : string;
  Imp : TImportIntegrator;
  Ints : TClassList;
  Exts : TStringList;
  I,J : integer;
  SikCursor: TCursor;
  OtherFiles: TStringList;
begin
  // Examine fileextension and call the correct integrator
  SikCursor:= Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Ext:= '';
  for i:= 0 to Filenames.Count - 1 do
    if LowerCase(ExtractFileExt(FileNames[i])) = '.java' then
      Ext:= '.java';
  if (Ext = '') and (Filenames.Count > 0) then
    Ext:= LowerCase(ExtractFileExt(FileNames[0]));

  Imp := nil;
  Ints := Integrators.Get(TImportIntegrator);
  OtherFiles:= TStringList.Create;
  try
    for I := 0 to Ints.Count - 1 do begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        if Exts.IndexOfName(Ext)>-1 then begin
          Imp := TImportIntegratorClass(Ints[I]).Create(Model, TFileProvider.Create);
          J := 0;
          while J < FileNames.Count do
            if Exts.IndexOfName(LowerCase(ExtractFileExt(FileNames[J]))) = -1
              then begin
                OtherFiles.Add(FileNames[J]);
                FileNames.Delete(J)
              end
              else Inc(J);
          Break;
        end;
      finally
        FreeAndNil(Exts);
      end;
    end;
    if Imp <> nil then
      try
        Imp.BuildModelFrom(FileNames);
      finally
        FreeAndNil(Imp);
      end;
    AddToProject(OtherFiles);
  finally
    FreeAndNil(Ints);
    FreeAndNil(OtherFiles);
    Screen.Cursor := SikCursor;
  end;
end;

procedure TDMUMLModule.LoadProject(const FileName: string);
begin
  var SL:= TStringList.Create;
  SL.Add(FileName);
  LoadProject(SL);
  FreeAndNil(SL);
end;

procedure TDMUMLModule.AddToProject(const FileName: string);
var
  Ext : string;
  Imp : TImportIntegrator;
  Ints : TClassList;
  Exts : TStringList;
  I: integer; 
begin
  try
    if assigned(FModel) and assigned(FModel.ModelRoot) and
       assigned(FModel.ModelRoot.Files) and (FModel.ModelRoot.Files.Text = '') then begin
      LoadProject(Filename);
      FDiagram.ResolveAssociations;
      FDiagram.ResolveObjectAssociations;
      exit;
    end;
  except
    on e: exception do
      FConfiguration.Log('TDMUMLModule.AddToProject ', e);
  end;
  Ext:= LowerCase(ExtractFileExt(FileName));
  Imp := nil;
  Ints := Integrators.Get(TImportIntegrator);
  try
    for I := 0 to Ints.Count - 1 do begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        if Exts.IndexOfName(Ext)>-1 then
          Imp := TImportIntegratorClass(Ints[I]).Create(Model, TFileProvider.Create);
      finally
        FreeAndNil(Exts);
      end;
    end;

    if Imp<>nil then
      try
        Imp.AddFileToModel(FileName);
        FDiagram.ResolveAssociations;
        FDiagram.ResolveObjectAssociations;
      finally
        FreeAndNil(Imp);
      end;
  finally
    FreeAndNil(Ints);
  end;
end;

procedure TDMUMLModule.AddToProject(Filenames: TStrings);
begin
  for var i:= 0 to Filenames.Count - 1 do
    AddToProject(Filenames[i]);
end;

procedure TDMUMLModule.FileOpenActionExecute(Sender: TObject);
var
  Ints : TClassList;
  Exts : TStringList;
  I, J : integer;
  AnyFilter, aFilter : string;
begin
  aFilter := '';
  Ints := Integrators.Get(TImportIntegrator);
  try
    for I := 0 to Ints.Count - 1 do begin
      Exts := TImportIntegratorClass(Ints[I]).GetFileExtensions;
      try
        for J := 0 to Exts.Count - 1 do begin
          if aFilter <> '' then
            aFilter := aFilter + '|';
          aFilter := aFilter + Exts.Values[Exts.Names[J]] + ' (*' + Exts.Names[J] + ')|*' + Exts.Names[J];
          if AnyFilter <> '' then
            AnyFilter:= AnyFilter + ';';
          AnyFilter:= AnyFilter + '*' + Exts.Names[J];
        end;
      finally
        FreeAndNil(Exts);
      end;
    end;
  finally
    FreeAndNil(Ints);
  end;
  aFilter := 'All types (' + AnyFilter + ')|' + AnyFilter + '|' + aFilter;
  with TOpenDialog.Create(Self) do begin
    Filter:= aFilter;
    Options:= Options + [ofAllowMultiSelect];
    if Execute then
      LoadProject(Files);
    Free;
  end;
end;

procedure TDMUMLModule.SaveDiagramActionExecute(Sender: TObject);
begin
  with TSaveDialog.Create(Self) do begin
    InitialDir:= ExtractFilePath(Model.ModelRoot.GetConfigFile);
    Filter:= 'SVG files (*.svg)|*.svg|PNG files (*.png)|*.png|All files (*.*)|*.*';
    Options:= Options + [ofOverwritePrompt];
    if Execute then begin
      if ExtractFileExt(FileName) = '' then begin
        if FilterIndex = 2
          then FileName:= ChangeFileExt(FileName, '.png')
          else FileName:= ChangeFileExt(FileName, '.svg');
      end;
      Diagram.SaveAsPicture(FileName);
    end;
    Free;
  end;
end;

procedure TDMUMLModule.ShowAllOpenedFiles;
  var Files: TStringList; i: integer; s: string;
begin
  Files:= TStringList.Create;
  with FJava do
    for i:= 0 to FJava.TDIEditFormCount -1 do begin
      s:= TDIEditFormGet(i).Pathname;
      if hasJavaExtension(s) or hasPascalExtension(s) then
        Files.Add(s);
    end;
  LoadProject(Files);
  FreeAndNil(Files);
end;

procedure TDMUMLModule.SaveUML(const Pathname: string);
begin
  Diagram.StoreDiagram(Pathname);
end;

procedure TDMUMLModule.LoadUML(const Pathname: string);
begin
  Diagram.FetchDiagram(Pathname);
end;

function TDMUMLModule.getUMLFilename: string;
begin
  if Assigned(Diagram.Package)
    then Result:= Diagram.Package.GetConfigFile
    else Result:= '';
end;

procedure TDMUMLModule.InitShowParameter(i: integer);
begin
  Diagram.InitShowParameter(i);
end;

procedure TDMUMLModule.SetShowIcons(i: integer);
begin
  Diagram.ShowIcons:= i;
end;

{$WARNINGS OFF}
function TDMUMLModule.OpenFolderActionExecute(Sender: TObject): boolean;

  procedure _AddFileNames(Files: TStringList; const Path, Ext: string; rekursiv: boolean);
    var Sr: TSearchRec;
  begin
    if FindFirst(Path + '\*.*', faReadOnly or faDirectory, Sr) = 0 then begin
      repeat
        if (Sr.Name <> '.') and (Sr.Name <> '..') then
          if ((Sr.Attr and faDirectory) = faDirectory) and rekursiv then
            _AddFileNames(Files, Path + '\' + Sr.Name, Ext, rekursiv)
          else
            if CompareText(ExtractFileExt(Sr.Name), Ext) = 0 then
              Files.Add(Path + '\' + Sr.Name);
      until FindNext(Sr) <> 0;
      FindClose(Sr);
    end;
  end;

begin
  Result:= false;
  var SL:= TStringList.Create;
  var Ints := Integrators.Get(TImportIntegrator);
  var OpenFolderForm:= TFOpenFolderDialog.Create(nil);
  try
    for var i:= 0 to Ints.Count - 1 do begin
      var Exts:= TImportIntegratorClass(Ints[i]).GetFileExtensions;
      try
        OpenFolderForm.CBFiletype.Items.Add( '*' + Exts.Names[0]);
      finally
        FreeAndNil(Exts);
      end;
    end;
    OpenFolderForm.CBFiletype.ItemIndex:= 0;
    if OpenFolderForm.ShowModal = mrOk then begin
      Result:= true;
      _AddFileNames(SL, OpenFolderForm.PathTreeView.Path,
                    Copy(OpenFolderForm.CBFiletype.Items[OpenFolderForm.CBFiletype.ItemIndex], 2, 10),
                    OpenFolderForm.CBWithSubFolder.Checked);
      if SL.Count > 0 then begin
        LoadProject(SL);
        FDiagram.ResolveAssociations;
        DoLayout;
      end else
        ShowMessage('No files found.');
      OpendFolder:= OpenFolderForm.PathTreeView.Path;
    end else
      OpendFolder:= '';
  finally
    FreeAndNil(SL);
    FreeAndNil(Ints);
    OpenFolderForm.Release;
  end;
end;
{$WARNINGS ON}

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

function TDMUMLModule.hasEditableClass: boolean;
  var s: string;
begin
  Result:= false;
  try
    if assigned(Diagram)
      then Result:= Diagram.hasEditableClass
      else Result:= false;
  except on e: exception do begin
    if assigned(Diagram)
       then s:= 'TDMUMLModule.hasEditableClass Diagram assigned'
       else s:= 'TDMUMLModule.hasEditableClass Diagram = nil';
    FConfiguration.Log(s, e);
    end;
  end;
end;

procedure TDMUMLModule.Print;
  var DBx, DBY: Integer;
      FormBitmap: TBitmap;
begin
  FormBitmap:= TBitmap.Create;
  Diagram.GetDiagramSize(DBx, DBy);
  FormBitmap.Width:= DBx - 1;
  FormBitmap.Height:= DBy - 1;
  try
    Diagram.PaintTo(FormBitmap.Canvas, 0, 0, false);
    PrintBitmap(FormBitmap, (Owner as TForm).PixelsPerInch);
  finally
    FreeAndNil(FormBitmap);
  end;
end;

function TDMUMLModule.getClasses: TStringList;
  var i: integer; dir, Filename: string;
begin
  Result:= Diagram.getClasses;
  if FModel.ModelRoot.Files.Count > 0 then begin
    Dir:= ExtractFilePath(FModel.ModelRoot.Files[0]);
    for i:= 0 to Result.Count - 1 do begin
      Filename:= Dir + Result[i] + '.java';
      if (FModel.ModelRoot.Files.IndexOf(Filename) = -1) and FileExists(Filename)
      then begin
        AddToProject(Filename);
        FModel.ModelRoot.Files.Add(Filename);
      end;
    end;
  end;
end;

end.
