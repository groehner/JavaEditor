object TabDockHost: TTabDockHost
  Left = 412
  Top = 306
  Caption = 'TabDockHost'
  ClientHeight = 274
  ClientWidth = 488
  Color = clBtnFace
  DragMode = dmAutomatic
  ParentFont = True
  OnClose = FormClose
  TextHeight = 15
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 488
    Height = 274
    Align = alClient
    DockSite = True
    TabOrder = 0
    OnDockOver = PageControl1DockOver
    OnGetSiteInfo = PageControl1GetSiteInfo
    OnUnDock = PageControl1UnDock
    ExplicitWidth = 492
    ExplicitHeight = 275
  end
end
