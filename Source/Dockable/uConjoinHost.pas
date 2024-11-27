
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uConjoinHost;

interface

uses
  Windows, Controls, Forms;

type
  TConjoinDockHost = class(TForm)
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
    procedure FormUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
  private
    procedure DoFloat(AControl: TControl);
  public
    procedure UpdateCaption(Exclude: TControl);
  end;

var
  ConjoinDockHost: TConjoinDockHost;

implementation

{$R *.dfm}

uses  Messages, Types, uDockForm;

procedure TConjoinDockHost.DoFloat(AControl: TControl);
var
  ARect: TRect;
begin
  //float the control with its original size.
  ARect.TopLeft:= AControl.ClientToScreen(Point(0, 0));
  ARect.BottomRight:= AControl.ClientToScreen(Point(AControl.UndockWidth, AControl.UndockHeight));
  AControl.ManualFloat(ARect);
end;

procedure TConjoinDockHost.FormClose(Sender: TObject; var aAction: TCloseAction);
begin
  if DockClientCount = 1 then
  begin
    DoFloat(DockClients[0]);
    aAction := caFree;
  end
  else
    aAction := caHide;
end;

procedure TConjoinDockHost.UpdateCaption(Exclude: TControl);
begin
  //if a dockable form is undocking, it will pass itself in as Exclude
  //because even it hasn't actually been taken out of the DockClient array
  //at this point.
  Caption := '';
  for var i:= 0 to DockClientCount-1 do
    if DockClients[i].Visible and (DockClients[i] <> Exclude) then
      Caption := Caption + TDockableForm(DockClients[i]).Caption + ' ';
end;

procedure TConjoinDockHost.FormDockDrop(Sender: TObject; Source: TDragDockObject; X, Y: Integer);
begin
  UpdateCaption(nil);
  DockManager.ResetBounds(True);
  //Force DockManager to redraw it's clients.
end;

{
The following example is taken from the docking demo. It shows how the OnUnDock
event handler of the conjoinment docking site re-enables docking in the control
that is undocked (if it is a dockable form). In addition, when the next-to-last
docked control is undocked, the conjoinment docking site sends itself a close
message so that the last docked control is undocked to its old position and size.}
procedure TConjoinDockHost.FormUnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  //only 2 dock clients means the host must be destroyed and
  //the remaining window undocked to its old position and size.
  //(Recall that OnUnDock gets called before the undocking actually occurs)
  if Client is TDockableForm then
    TDockableForm(Client).DockSite := True;
  if (DockClientCount = 2) and (NewTarget <> Self) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
  UpdateCaption(Client);
end;

procedure TConjoinDockHost.FormDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source.Control is TDockableForm;
end;

procedure TConjoinDockHost.FormGetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  CanDock:= DockClient is TDockableForm;
end;

end.

