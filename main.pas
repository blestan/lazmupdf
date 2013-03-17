unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, libmupdf, BGRABitmapTypes, BGRABitmap;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDLG: TOpenDialog;
    PaintBox1: TPaintBox;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure RenderPage;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FContext: fz_context;
    FDoc: fz_document;
    FBGRA: TBGRABitmap;
    FPage: Integer;
    FPCount: Integer;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not OpenDLG.Execute then exit;
  fz_close_document(FDoc);
  fz_free_context(FContext);
  FContext:=fz_new_context(nil,nil,FZ_STORE_UNLIMITED);
  FDoc:=fz_open_document(FContext,PChar(OpenDLG.FileName));
  fpcount:= fz_count_pages(FDoc);
  Memo1.Lines.Add(format('%d pages found in document',[fpcount]));

  FPage:=0;

  RenderPage;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBGRA:=nil;
  FPage:=-1;
  FPCount:=0;
  FContext:=nil;
  FDoc:=nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBGRA.Free;
  fz_close_document(FDoc);
  fz_free_context(FContext);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  if FBGRA<>nil then  FBGRA.Draw(PaintBox1.Canvas,0,0,True);
end;


Procedure TForm1.RenderPage;
var p: fz_page;
    r: fz_rectangle;
    w,h: integer;
    b: fz_bbox;
    ctm: fz_matrix;

    pix:fz_pixmap;
    dev: fz_device;

    oldc: TCursor;
begin
 if (FPage<0) or (FPage>=FPCount) then exit;
 try
   if FBGRA<>nil Then FreeAndNil(FBGRA);
   oldc:=Screen.Cursor;
   Screen.Cursor:=crHourGlass;
   p:=fz_load_page(FDoc,FPage);
   r:=fz_bound_page(Fdoc,p);
   Memo1.Lines.Add(format('page %d bounds: %fx%f:%fx%f',[FPage,r.x0,r.y0,r.x1,r.y1]));

   w:=round(r.x1-r.x0);
   h:=round(r.y1-r.y0);

   with ctm do
    begin
      a:=1;b:=0;c:=0;d:=1;e:=0;f:=0;
    end;

   with b do
    begin
      x0:=0; y0:=0; x1:=w; y1:=h;
    end;

   FBGRA:=TBGRABitmap.Create(w,h,clWhite);

   pix:=fz_new_pixmap_with_bbox_and_data(FContext,fz_find_device_colorspace(FContext,'DeviceBGR'),b,FBGRA.Data);

   dev:=fz_new_draw_device(FContext,pix);
   Memo1.Lines.Add('Begin DRAW');
   fz_run_page(FDoc,p,dev,ctm,nil);
   Memo1.Lines.Add('End DRAW');

   FBGRA.VerticalFlip;
   PaintBox1.Invalidate;
 finally
   fz_free_device(dev);
   fz_drop_pixmap(FContext,pix);
   fz_free_page(FDoc,p);
   Screen.Cursor:=oldC;
 end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if FPage<FPCount then
  begin
    Inc(FPage);
    RenderPage();
    PaintBox1.Invalidate;
  end;
end;

end.

