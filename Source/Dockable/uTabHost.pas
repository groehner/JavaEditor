//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uTabHost;

interface

uses
  Controls, Forms, ComCtrls, Types, System.Classes;

type
  TTabDockHost = class(TForm)
    PageControl1: TPageControl;
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure PageControl1UnDock(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
    procedure PageControl1GetSiteInfo(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure PageControl1DockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
  public
  end;

var
  TabDockHost: TTabDockHost;

implementation

{$R *.dfm}

uses Windows, Messages, uDockForm;

procedure TTabDockHost.FormClose(Sender: TObject; var aAction: TCloseAction);
var
  ARect: TRect;
begin
  if PageControl1.DockClientCount = 1 then
  begin
    with PageControl1.DockClients[0] do
    begin
      ARect.TopLeft := ClientToScreen(Point(0, 0));
      ARect.BottomRight := ClientToScreen(Point(UndockWidth, UndockHeight));
      ManualFloat(ARect);
    end;
    aAction := caFree;
  end
  else
    aAction := caHide;
end;

procedure TTabDockHost.PageControl1UnDock(Sender: TObject;
  Client: TControl; NewTarget: TWinControl; var Allow: Boolean);
begin
  //only 2 dock clients means the host must be destroyed and
  //the remaining window undocked to its old position and size.
  //  if (PageControl1.DockClientCount = 2) and (NewTarget <> Self) then
  if (PageControl1.DockClientCount = 2) and (NewTarget <> PageControl1) then
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
end;

procedure TTabDockHost.PageControl1GetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  CanDock := DockClient is TDockableForm;
end;

procedure TTabDockHost.PageControl1DockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  Accept := Source.Control is TDockableForm;
end;

end.
