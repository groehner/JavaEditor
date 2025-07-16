
// https://theroadtodelphi.com/2011/12/16/exploring-delphi-xe2-vcl-styles-part-ii/
// https://github.com/RRUZ/vcl-styles-utils


(*
VCLStyleSynEdit By SMP3 .
My Blog : DelphiNews.WordPress.Com .
-----------------------------------------------------------
This small unit add the capability to SynEdit to support
VCL Style !
-----------------------------------------------------------

*)
unit VCLStyleSynEdit;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
 System.Classes, Vcl.Graphics,
 Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEdit, Vcl.Themes;

type

  TSynEdit = class(SynEdit.TSynEdit)
  private
    LStyle: TCustomStyleServices;
  protected
    procedure CMSTYLECHANGED(var Message:TMessage); message CM_STYLECHANGED;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangeStyle;  virtual;
  end;

implementation

uses UConfiguration;

{ TSynEdit }

procedure TSynEdit.CMSTYLECHANGED(var Message: TMessage);
begin
  ChangeStyle;
  Invalidate;
end;

constructor TSynEdit.Create(AOwner: TComponent);
begin
  inherited;
  ChangeStyle;
end;

procedure TSynEdit.Loaded;
begin
  inherited;
  ChangeStyle;
end;

procedure TSynEdit.ChangeStyle;
begin
  LStyle:= TStyleManager.ActiveStyle;
  if LStyle.IsSystemStyle then begin
    // background color of SynEdit
    Color:= clWhite;
    Font.Color:= clBlack;
    if FConfiguration.ActiveLineColor <> clNone then
      ActiveLineColor:= clHighlight;
    with Gutter do begin
      GradientStartColor:= clBtnFace;
      GradientEndColor:= clWindow;
      Color:= clBtnFace;
      Font.Color:= clWindowText;
      BorderColor:= clWindow;
      CodeFolding.CollapsedLineColor:= clGrayText;
      CodeFolding.FolderBarLinesColor:= clGrayText;
      //CodeFolding.IndentGuidesColor:= clGray;
    end;
  end else begin
    // background color of SynEdit
    Color:= LStyle.GetStyleColor(scEdit);
    if Font.Color = Color then
      Font.Color:= clBlack;
    if FConfiguration.ActiveLineColor <> clNone then
      ActiveLineColor:= LStyle.GetSystemColor(clHighlight);
    with Gutter do begin
      GradientStartColor:= LStyle.GetSystemColor(clBtnFace);
      GradientEndColor:= LStyle.GetSystemColor(clWindow);
      Color:= LStyle.GetSystemColor(clBtnFace);
      Font.Color:= LStyle.GetSystemColor(clWindowText);
      BorderColor:= LStyle.GetStyleColor(scSplitter);
      CodeFolding.CollapsedLineColor:= Font.Color;
      CodeFolding.FolderBarLinesColor:= Font.Color;
      //CodeFolding.IndentGuidesColor:= Font.Color;
    end;
  end;
end;

initialization
  TStyleManager.Engine.RegisterStyleHook(TSynEdit, TScrollBoxStyleHook);

finalization
  TStyleManager.Engine.UnRegisterStyleHook(TSynEdit, TScrollBoxStyleHook);

end.