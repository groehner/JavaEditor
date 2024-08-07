//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit uDockForm;

interface

uses
  Windows, Messages, Controls, Forms;


type
  TDockableForm = class(TForm)
    procedure FormDockOver(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure FormClose(Sender: TObject; var aAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormStartDock(Sender: TObject; var DragObject: TDragDockObject);
  private
    FUndockLeft: Integer;
    FUndockTop: Integer;
    FFloatOnCloseDock: boolean;
    function ComputeDockingRect(var DockRect: TRect; MousePos: TPoint): TAlign;
    procedure WMNCLButtonDown(var Msg: TMessage); message WM_NCLBUTTONDOWN;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
  public
    property UndockLeft: Integer read FUndockLeft write FUndockLeft;
    property UndockTop: Integer read FUndockTop write FUndockTop;
    property FloatOnCloseDock: boolean read FFloatOnCloseDock write FFloatOnCloseDock;
  end;

implementation

{$R *.dfm}

uses
  ExtCtrls, ComCtrls, Types, DockingUtils, uTabHost, uConjoinHost,
  UJava, UMessages, UFileStructure;

procedure TDockableForm.FormDockOver(Sender: TObject; Source: TDragDockObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
begin
  Accept := (Source.Control is TDockableForm);
  if (Sender is TFMessages) and (Source.Control is TFFileStructure) then Accept:= false;
  //Draw dock preview depending on where the cursor is relative to our client area
  if Accept and (ComputeDockingRect(ARect, Point(X, Y)) <> alNone) then
  begin
    ComputeDockingRect(ARect, Point(X, Y));
    Source.DockRect := ARect;
  end;
end;

function TDockableForm.ComputeDockingRect(var DockRect: TRect; MousePos: TPoint): TAlign;
var
  DockTopRect,
  DockLeftRect,
  DockBottomRect,
  DockRightRect,
  DockCenterRect: TRect;
begin
  Result := alNone;
  //divide form up into docking "Zones"
  DockLeftRect.TopLeft := Point(0, 0);
  DockLeftRect.BottomRight := Point(ClientWidth div 5, ClientHeight);

  DockTopRect.TopLeft := Point(ClientWidth div 5, 0);
  DockTopRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5);

  DockRightRect.TopLeft := Point(ClientWidth div 5 * 4, 0);
  DockRightRect.BottomRight := Point(ClientWidth, ClientHeight);

  DockBottomRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5 * 4);
  DockBottomRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight);

  DockCenterRect.TopLeft := Point(ClientWidth div 5, ClientHeight div 5);
  DockCenterRect.BottomRight := Point(ClientWidth div 5 * 4, ClientHeight div 5 * 4);

  //Find out where the mouse cursor is, to decide where to draw dock preview.
  if PtInRect(DockLeftRect, MousePos) then
  begin
    Result := alLeft;
    DockRect := DockLeftRect;
    DockRect.Right := ClientWidth div 2;
  end
  else
  if PtInRect(DockTopRect, MousePos) then
  begin
    Result := alTop;
    DockRect := DockTopRect;
    DockRect.Left := 0;
    DockRect.Right := ClientWidth;
    DockRect.Bottom := ClientHeight div 2;
  end
  else
  if PtInRect(DockRightRect, MousePos) then
  begin
    Result := alRight;
    DockRect := DockRightRect;
    DockRect.Left := ClientWidth div 2;
  end
  else
  if PtInRect(DockBottomRect, MousePos) then
  begin
    Result := alBottom;
    DockRect := DockBottomRect;
    DockRect.Left := 0;
    DockRect.Right := ClientWidth;
    DockRect.Top := ClientHeight div 2;
 end
  else
  if PtInRect(DockCenterRect, MousePos) then
  begin
    Result := alClient;
    DockRect := DockCenterRect;
  end;
  if Result = alNone then Exit;

  //DockRect is in screen coordinates.
  DockRect.TopLeft := ClientToScreen(DockRect.TopLeft);
  DockRect.BottomRight := ClientToScreen(DockRect.BottomRight);
end;

procedure TDockableForm.FormClose(Sender: TObject;
  var aAction: TCloseAction);
begin
  //the action taken depends on how the form is docked.

  if assigned(HostDockSite) then begin

    if (HostDockSite is TConjoinDockHost) then
    begin
      //remove the form's caption from the conjoin dock host's caption list
      TConjoinDockHost(HostDockSite).UpdateCaption(Self);

      //if we're the last visible form on a conjoined form, hide the form
      if HostDockSite.VisibleDockClientCount <= 1 then
        HostDockSite.Hide;
    end;

    //if docked to a panel, tell the panel to hide itself. If there are other
    //visible dock clients on the panel, it ShowDockPanel won't allow it to
    //be hidden
    if (HostDockSite is TPanel) and Visible and (HostDockSite.VisibleDockClientCount <= 1)
       or (HostDockSite.VisibleDockClientCount = 0) then
      FJava.ShowDockPanel(HostDockSite as TPanel, False, nil);
  end;

  aAction := caHide;

  if not Floating and FloatOnCloseDock then
  begin
    // Float when close docked window
    Hide;
    ManualFloat(Rect(FUndockLeft, FUndockTop, FUndockLeft + UndockWidth,
      FUndockTop + UndockHeight));
  end;
end;

procedure TDockableForm.CMDockClient(var Message: TCMDockClient);
var
  ARect: TRect;
  DockType: TAlign;
  Host: TForm;
  Pt: TPoint;
begin
  //Overriding this message allows the dock form to create host forms
  //depending on the mouse position when docking occurs. If we don't override
  //this message, the form will use VCL's default DockManager.

  //NOTE: the only time ManualDock can be safely called during a drag
  //operation is we override processing of CM_DOCKCLIENT.

  if Message.DockSource.Control is TDockableForm then
  begin
    //Find out how to dock (Using a TAlign as the result of ComputeDockingRect)
    Pt.X := Message.MousePos.X;
    Pt.Y := Message.MousePos.Y;
    DockType := ComputeDockingRect(ARect, Pt);

    //if we are over a dockable form docked to a panel in the
    //main window, manually dock the dragged form to the panel with
    //the correct orientation.
    if (HostDockSite is TPanel) then
    begin
      Message.DockSource.Control.ManualDock(HostDockSite, nil, DockType);
      Exit;
    end;

    //alClient => Create a TabDockHost and manually dock both forms to the PageControl
    //owned by the TabDockHost.
    if DockType = alClient then
    begin
      if (Message.DockSource.Control is TDockableForm) and (HostDockSite is TPageControl) then
        Host := TTabDockHost(HostDockSite.Parent)
      else
      begin
        Host := TTabDockHost.Create(Application);
        Host.BoundsRect := Self.BoundsRect;
      end;
      Self.ManualDock(TTabDockHost(Host).PageControl1, nil, alClient);
      Message.DockSource.Control.ManualDock(TTabDockHost(Host).PageControl1, nil, alClient);
      Host.Visible := True;
    end
    //if DockType <> alClient, create the ConjoinDockHost and manually dock both
    //forms to it. Be sure to make dockable forms non-dockable when hosted by
    // ConjoinDockForm, since it is using the VCL default DockManager.
    else
    begin
      Host := TConjoinDockHost.Create(Application);
      Host.BoundsRect:= Self.BoundsRect;
      Self.ManualDock(Host, nil, alNone);
      Self.DockSite := False;
      Message.DockSource.Control.ManualDock(Host, nil, DockType);
      TDockableForm(Message.DockSource.Control).DockSite := False;
      Host.Visible := True;
    end;
  end;
end;

procedure TDockableForm.FormShow(Sender: TObject);
begin
  if HostDockSite is TConjoinDockHost then
    TConjoinDockHost(HostDockSite).UpdateCaption(nil);
end;

procedure TDockableForm.FormStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
  // create a customized DragDropObject
  DragObject:= TTransparentDragDockObject.Create(Self);
end;

procedure TDockableForm.WMNCLButtonDown(var Msg: TMessage);
begin
  inherited;
  // This event happen when mouse click in caption
  // save initial window position
  FUndockLeft:= Left;
  FUndockTop:= Top;
end;

end.
