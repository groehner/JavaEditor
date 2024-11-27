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

unit UTreeViewIntegrator;

interface

uses {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LclType, {$ENDIF}
      Controls, ComCtrls, uViewIntegrator, uTreeViewFrame, uModel,
      uModelEntity, uListeners, uFeedback;


type
  TTreeViewIntegrator = class(TViewIntegrator, IAfterObjectModelListener)
  private
    Frame: TAFrameTreeView;
  protected
    procedure Change(Sender: TModelEntity);

    procedure BuildAllClassesView(ATreeRoot: TTreeNode; AEntity: TLogicPackage);
    procedure BuildLogicPackageView(ATreeRoot: TTreeNode; AEntity: TLogicPackage);
    procedure BuildUnitPackageView(ATreeRoot: TTreeNode; AEntity: TUnitPackage);
    procedure BuildClassView(ATreeRoot: TTreeNode; AEntity: uModel.TClass);
    procedure BuildInterfaceView(ATreeRoot: TTreeNode; AEntity: uModel.TInterface);

    procedure tvModelCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure tvModelChange(Sender: TObject; Node: TTreeNode);
    procedure tvModelAddition(Sender: TObject; Node: TTreeNode);
    procedure CurrentEntityChanged; override;
  public
    constructor Create(om: TObjectModel; Parent: TWinControl; aFeedback: IEldeanFeedback = nil); override;
    destructor Destroy; override;
    procedure InitFromModel; override;
  end;

  TViewNode = class(TTreeNode)
  private
    FIsImplementation: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    function LocateNode(aData: Pointer; IsImplState: Boolean): TViewNode;

    property IsImplementation: Boolean read FIsImplementation;
  end;

implementation

uses SysUtils, uIntegrator, uIterators, Classes, Contnrs {$IFNDEF LCL}, Types, UITypes {$ENDIF};

const
  ALL_CLASSES_TEXT: string = 'All classes';
  PACKAGES_TEXT: string = 'Packages';
var
  NodesList: TObjectList;

procedure TViewNode.AfterConstruction;
begin
  inherited;
  FIsImplementation := False;
  if not Assigned(NodesList) then
    NodesList := TObjectList.Create(False);
  NodesList.Add(Self);
end;

procedure TViewNode.BeforeDestruction;
begin
  inherited;
  NodesList.Remove(Self);
  if NodesList.Count = 0 then
    FreeAndNil(NodesList);
end;

function TViewNode.LocateNode(aData: Pointer; IsImplState: Boolean): TViewNode;
var
  i: Integer;
begin
  for i := 0 to NodesList.Count - 1 do
  begin
    Result := NodesList[i] as TViewNode;
    if (Result.Data = aData) and (Result.IsImplementation = IsImplState) then exit;
  end;
  Result := nil;
end;

{ TTreeViewIntegrator }

procedure TTreeViewIntegrator.tvModelCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := TViewNode;
end;

procedure TTreeViewIntegrator.tvModelChange(Sender: TObject; Node: TTreeNode);
  // var chkNode: TViewNode;
begin
  // ACHTUNG: Erst nach dem Auskommentieren dieser Methode
  // hat das Ergänzen der UML-Fensters funktioniert!!!

  {chkNode := (Node as TViewNode);
  if Assigned(chkNode) and (chkNode.IsImplementation) then
  begin
    CurrentEntity := TObject(chkNode.Data) as TModelEntity;
    Exit;
  end}
end;

procedure TTreeViewIntegrator.tvModelAddition(Sender: TObject; Node: TTreeNode);
var
  ent: TModelEntity;
  imIndex: Integer;
begin
  if Node.Data = nil then
    Node.ImageIndex := 0
  else begin
    ent := TModelEntity(Node.Data);
    imIndex := 0;
    if ent is TAbstractPackage then
      imIndex := 1
    else if ent is uModel.TClass then
      imIndex := 2
    else if ent is uModel.TInterface then
      imIndex := 3;

    Node.ImageIndex := imIndex;
    Node.SelectedIndex := Node.ImageIndex;
  end;
end;

procedure TTreeViewIntegrator.BuildAllClassesView(ATreeRoot: TTreeNode;
  AEntity: TLogicPackage);
var
  Ci: IModelIterator;
  cent: TModelEntity;
  newRoot: TTreeNode;
begin
  Ci := TModelIterator.Create(Model.ModelRoot.GetAllClassifiers, ioAlpha);
  while Ci.HasNext do
  begin
    cent := Ci.Next;
    if not ((cent is uModel.TClass) or (cent is uModel.TInterface)) then continue;

    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, cent.Name, cent);
    if cent is uModel.TClass then
      BuildClassView(newRoot, cent as uModel.TClass)
    else if cent is uModel.TInterface then
      BuildInterfaceView(newRoot, cent as uModel.TInterface);
  end;
end;

procedure TTreeViewIntegrator.BuildLogicPackageView(ATreeRoot: TTreeNode;
  AEntity: TLogicPackage);
var
  Mi: IModelIterator;
  ent: TModelEntity;
  newRoot: TTreeNode;
begin
  Mi := TModelIterator.Create(AEntity.GetPackages, ioAlpha);
  while Mi.HasNext do
  begin
    ent := Mi.Next;
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, (ent as TAbstractPackage).Name, ent);
    (newRoot as TViewNode).FIsImplementation := True;
    if ent is TUnitPackage then
      BuildUnitPackageView(newRoot, ent as TUnitPackage)
    else
      BuildLogicPackageView(newRoot, ent as TLogicPackage);
  end;
  ATreeRoot.Expand(False);
end;

procedure TTreeViewIntegrator.BuildUnitPackageView(ATreeRoot: TTreeNode;
  AEntity: TUnitPackage);
var
  Mi: IModelIterator;
  ent: TModelEntity;
  newRoot: TTreeNode;
begin
  Mi := TModelIterator.Create(AEntity.GetUnitDependencies, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'dependencies', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, (ent as TUnitDependency).myPackage.Name, (ent as TUnitDependency).myPackage);
    end;
  end;

  Mi := TModelIterator.Create(AEntity.GetClassifiers, ioAlpha);
  while Mi.HasNext do
  begin
    ent := Mi.Next as TClassifier;
    if (ent is uModel.TClass) or (ent is uModel.TInterface) then
    begin
      newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, ent.Name, ent);
      (newRoot as TViewNode).FIsImplementation := True;
      if ent is uModel.TClass then
        BuildClassView(newRoot, ent as uModel.TClass)
      else
        BuildInterfaceView(newRoot, ent as uModel.TInterface)
    end;
  end;
end;

procedure TTreeViewIntegrator.BuildClassView(ATreeRoot: TTreeNode;
  AEntity: TClass);
var
  Mi: IModelIterator;
  newRoot: TTreeNode;
  ent: TModelEntity;
begin
  if Assigned(AEntity.Ancestor) then
    ATreeRoot.Owner.AddChildObject(ATreeRoot, 'Ancestor: ' + AEntity.Ancestor.Name, AEntity.Ancestor);
  Mi := TModelIterator.Create(AEntity.GetImplements, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'interfaces', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;

  Mi := TModelIterator.Create(AEntity.GetDescendants, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'subclasses', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;
end;

procedure TTreeViewIntegrator.BuildInterfaceView(ATreeRoot: TTreeNode;
  AEntity: TInterface);
var
  Mi: IModelIterator;
  newRoot: TTreeNode;
  ent: TModelEntity;
begin
  if Assigned(AEntity.Ancestor) then
    ATreeRoot.Owner.AddChildObject(ATreeRoot, 'Ancestor: ' + AEntity.Ancestor.Name, AEntity.Ancestor);
  Mi := TModelIterator.Create(AEntity.GetImplementingClasses, ioAlpha);
  if Mi.Count > 0 then
  begin
    newRoot := ATreeRoot.Owner.AddChildObject(ATreeRoot, 'implementors', nil);
    while Mi.HasNext do
    begin
      ent := Mi.Next;
      ATreeRoot.Owner.AddChildObject(newRoot, ent.Name, ent);
    end;
  end;
end;

procedure TTreeViewIntegrator.Change(Sender: TModelEntity);
begin
  InitFromModel;
end;

constructor TTreeViewIntegrator.Create(om: TObjectModel;
  Parent: TWinControl; aFeedback: IEldeanFeedback);
begin
  inherited Create(Om, Parent, aFeedback);
  Frame := TAFrameTreeView.Create(Parent);
  Frame.Parent := Parent;
  Model.AddListener(IAfterObjectModelListener(Self));

  Frame.tvModel.OnCreateNodeClass := {$IFDEF LCL}@{$ENDIF}tvModelCreateNodeClass;
  Frame.tvModel.OnChange := {$IFDEF LCL}@{$ENDIF}tvModelChange;
  Frame.tvModel.OnAddition := {$IFDEF LCL}@{$ENDIF}tvModelAddition;
end;

destructor TTreeViewIntegrator.Destroy;
begin
  Model.RemoveListener(IAfterObjectModelListener(Self));
  inherited;
end;

procedure TTreeViewIntegrator.InitFromModel;
var
  node: TViewNode;
begin
  Frame.tvModel.Items.Clear;
//  BuildUnitPackageView(Frame.tvModel.Items.Add(nil,'Unknown'),Model.UnknownPackage);
  node := Frame.tvModel.Items.AddObject(nil, PACKAGES_TEXT, Model.ModelRoot) as TViewNode;
  node.FIsImplementation := True;
  BuildLogicPackageView(node, Model.ModelRoot);

  node := Frame.tvModel.Items.AddObject(nil, ALL_CLASSES_TEXT, AllClassesPackage) as TViewNode;
  node.FIsImplementation := True;
  BuildAllClassesView(node, nil);
end;

procedure TTreeViewIntegrator.CurrentEntityChanged;
begin
  inherited;
  Frame.tvModel.Selected := TViewNode(Frame.tvModel.Items.GetFirstNode).LocateNode(CurrentEntity, True);
end;

end.

