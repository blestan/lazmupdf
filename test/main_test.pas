unit main_test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, BGRAView, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, Ruler, types, BGRABitmap, BCTypes,libmupdf;

type

  { TPreviewForm }

  TPreviewForm = class(TForm)
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    PaintBox1: TBGRAView;
    HRuler: TRuler;
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    VRuler: TRuler;
    HScrollBar: TScrollBar;
    VScrollBar: TScrollBar;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBox1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    FPNum: Integer;
    FPCount: Integer;

    FDoc: fz_document;
    FCTX: fz_context;
    FPage: fz_Page;

    procedure GotoPage(N: Integer);
  public
    { public declarations }
  end; 

var
  PreviewForm: TPreviewForm;

implementation

{$R *.lfm}


{ TPreviewForm }

procedure TPreviewForm.PaintBox1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
      w,h: integer;
      b: fz_bbox;
      ctm: fz_matrix;

      pix:fz_pixmap;
      dev: fz_device;

      oldc: TCursor;
  begin
   if (FPNum<0) or (FPNum>=FPCount) then exit;
   try
     oldc:=Screen.Cursor;
     Screen.Cursor:=crHourGlass;


     with ctm do
      begin
        a:=96/72;b:=0;c:=0;d:=96/72;e:=0;f:=0;
      end;

     with b do
      begin
        x0:=0; y0:=0; x1:=PaintBox1.PageWidth; y1:=PaintBox1.PageHeight;
      end;


     pix:=fz_new_pixmap_with_bbox_and_data(FCTX,fz_find_device_colorspace(FCTX,'DeviceBGR'),b,PaintBox1.Bitmap.Data);

     dev:=fz_new_draw_device(FCTX,pix);

     fz_run_page(FDoc,FPage,dev,ctm,nil);


     PaintBox1.Bitmap.VerticalFlip;

   finally
     fz_free_device(dev);
     fz_drop_pixmap(FCTX,pix);
     Screen.Cursor:=oldC;
   end;
end;

procedure TPreviewForm.HScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
 HRuler.ZeroOffset:=-ScrollPos;
 PaintBox1.SetOrigin(-ScrollPos,PaintBox1.OriginY);
end;

procedure TPreviewForm.GotoPage(N: Integer);
var  r: fz_rectangle;
     w,h: Integer;
begin
 if (N<0) or (N>=FPCount) then Exit;
 if FPage<>nil then fz_free_page(FDoc,FPage);
 FPage:=fz_load_page(FDOC,N);
 r:=fz_bound_page(FDoc,FPage);
 w:=round((r.x1-r.x0)*96/72);
 h:=round((r.y1-r.y0)*96/72);
 PaintBox1.BeginUpdateBounds;
 PaintBox1.PageWidth:=w;
 PaintBox1.PageHeight:=h;
 with HScrollBar do
   begin
     Min:=HRuler.Scale(-w);
     Max:=HRuler.Scale(w*2);
     PageSize:=HRuler.Scale(w);
     SmallChange:=HRuler.Scale(1);
     LargeChange:=HRuler.Scale(10);
     HRuler.ZeroOffset:=0;
     Position:=-HRuler.ZeroOffset;
   end;

  with VScrollBar do
   begin
     Min:=VRuler.Scale(-h);
     Max:=VRuler.Scale(h*2);
     PageSize:=VRuler.Scale(h);
     SmallChange:=VRuler.Scale(1);
     LargeChange:=VRuler.Scale(10);
     Position:=0;
     VRuler.ZeroOffset:=0;
   end;
 FPNum:=N;
 PaintBox1.Caption:=IntToStr(FPNUM);
 PaintBox1.EndUpdateBounds;
 PaintBox1.DiscardBitmap;
 PaintBox1.SetOrigin(HRuler.ZeroOffset,VRuler.ZeroOffset);

end;

procedure TPreviewForm.ToolButton1Click(Sender: TObject);
begin
  if not OpenDialog1.Execute  then exit;

  if FCTX=nil then FCTX:=fz_new_context(nil,nil,0);
  if FDoc<>nil then fz_close_document(FDOC);
  FDoc:=fz_open_document(FCTX,PChar(OpenDialog1.FileName));
  FPNum:=0;
  FPCount:=fz_count_pages(FDoc);
  GotoPage(0);
end;

procedure TPreviewForm.ToolButton4Click(Sender: TObject);
begin
  GotoPage(FPNum-1);
end;

procedure TPreviewForm.ToolButton5Click(Sender: TObject);
begin
  GotoPage(FPNum+1);
end;

procedure TPreviewForm.VScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
 VRuler.ZeroOffset:=-ScrollPos;
 PaintBox1.SetOrigin(PaintBox1.OriginX,-ScrollPos);
end;


procedure TPreviewForm.FormCreate(Sender: TObject);
begin
 FCTX:=nil;
 FDoc:=nil;
 FPage:=nil;
 VRuler.ZeroOffset:=0;
 HRuler.ZeroOffset:=0;
end;

procedure TPreviewForm.FormDestroy(Sender: TObject);
begin
  if FPage<>nil then fz_free_page(FDoc,FPage);
  if FDoc<>nil then fz_close_document(FDoc);
  if FCTX<>nil then fz_free_context(FCTX);
end;

procedure TPreviewForm.FormShow(Sender: TObject);
begin
end;

procedure TPreviewForm.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Panel1.Caption:=Format('%d:%d',[x-PaintBox1.OriginX,y-PaintBox1.OriginY]);
  Panel1.Repaint;
end;

procedure TPreviewForm.PaintBox1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  with VScrollBar do
   begin
     Position:=Position - WheelDelta;
     VRuler.ZeroOffset:=-Position;
     PaintBox1.SetOrigin(PaintBox1.OriginX,-Position);
   end;
end;

end.

