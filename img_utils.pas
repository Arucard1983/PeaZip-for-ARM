unit img_utils;
{
 DESCRIPTION     :  Unit providing routines related to handling images

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20101030  G.Tani      Initial version
 0.11     20110816  G.Tani      Improved getting string with image details
 0.12     20130215  G.Tani      Improced decorations, improved some cases of error handling if images are not correctly loaded
 0.13     20130602  G.Tani      Code cleanup, fixed recognition of uncommon jpeg extensions

(C) Copyright 2010 Giorgio Tani giorgio.tani.software@gmail.com
The program is released under GNU LGPL http://www.gnu.org/licenses/lgpl.txt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$mode objfpc}{$H+}
{$INLINE ON}

interface

uses Classes, SysUtils, StrUtils, Forms, ExtCtrls, Graphics, ansiutf8_utils;

type
   TFoundList = array of utf8string; //dynamic array of utf8string, each time a new file is found new space is allocated (function rList)
   TFoundListBool = array of boolean;
   TFoundListAges = array of dword;
   TFoundListAttrib = array of dword;
   TFoundListSizes = array of qword;
   TFoundListArray64 = array of array [0..63] of byte;
   TFileOfByte = File of Byte;

procedure setsize_bitmap(var abitmap:tbitmap; isize, deco:integer);
procedure loadlargeicon(srcbitmap:TBitmap; var destbitmap:Tbitmap; destsize:integer);
function load_bitmap(var abitmap:Tbitmap; s:utf8string; isize, deco:integer; var imginfo:utf8string):integer;
function load_imagefiletopicture(var apicture:Tpicture; s:utf8string):integer;
function rotate_picture(var apicture:Tpicture; rfun:utf8string): integer;
function resize_picture(var apicture:Tpicture; wsize,hsize:integer): integer;
function crop_picture(var apicture:Tpicture; ctop,cbottom,cleft,cright:integer): integer;
function save_picturetoimagefile(var apicture:Tpicture; s:utf8string):integer;
function saveconvert_picturetoimagefile(var apicture:Tpicture; s,convext:utf8string; convopt:integer):integer;

const
DECO_NONE = 0; //no decoration, use all available space for rendering the image
DECO_SHADOW = 1; //light frame around the image, plus shadow
DECO_FRAME = 2; //light frame 6px away from the image
DECO_BORDER = 3; //light frame around the full icon
DECO_SPACE = 4; //6 px empty border

implementation

procedure autoscale_image(aform:Tform; var aimage:Timage; var ascale,iscale:double);
var
  iwidth,iheight : Integer;
  rect : TRect;
begin
with aform do
   begin
   iwidth:=aimage.Picture.Bitmap.Width;
   iheight:=aimage.Picture.Bitmap.Height;
   iscale:=iscale*ascale;
   rect:=aimage.BoundsRect;
   rect.Right:=rect.Left+Round(iwidth*iscale);
   rect.Bottom:=rect.Top+Round(iheight*iscale);
   aimage.BoundsRect:=rect;
   aimage.Stretch:=True;
   aimage.left:=(aform.width-aimage.width)div 2;
   aimage.top:=(aform.height-aimage.height)div 2;
   end;
end;

procedure autosize_image(aform:tform; var aimage:timage; var iscale:double);
var
   wscale,hscale,ascale:double;
begin
with aform do
   begin
   iscale:=1.0;
   if aimage.Picture.Bitmap.Width<>0 then wscale:=aform.Width / aimage.Picture.Bitmap.Width else wscale:=100;
   if aimage.Picture.Bitmap.Height<>0 then hscale:=aform.Height / aimage.Picture.Bitmap.Height else hscale:=100;
   if wscale<hscale then ascale:=wscale
   else ascale:=hscale;
   autoscale_image(aform, aimage, ascale, iscale);
   end;
end;

function load_image_auto(aform:Tform; var aimage:Timage; s:utf8string; var iscale:double):integer;
begin
load_image_auto:=-1;
Try
//iscale:=1.0;
aimage.Picture.LoadFromFile(s);
aform.Caption:=s;
//image_loaded:=1;
autosize_image(aform, aimage, iscale);
load_image_auto:=0;
Except
load_image_auto:=1;
end
end;

procedure scale_bitmap2(var abitmap:tbitmap; isize:integer; var ascale:double);
//scale image, resize the bitmap and align top left
var
  iwidth,iheight : Integer;
  rect : TRect;
begin
   iwidth:=abitmap.Width;
   iheight:=abitmap.Height;
   rect.Left := 0;
   rect.Top := 0;
   rect.Right:=rect.Left+Round(iwidth*ascale);
   rect.Bottom:=rect.Top+Round(iheight*ascale);
   abitmap.Canvas.StretchDraw(rect, abitmap);
   abitmap.width:=Round(iwidth*ascale);
   abitmap.height:=Round(iheight*ascale);
end;

procedure scale_bitmap(var abitmap:tbitmap; isize, deco:integer; var ascale:double);
//scale image (keeping in account border size), place the bitmap centered in a square of given size
var
  iwidth,iheight : Integer;
  rect,rect2 : TRect;
  bbitmap:tbitmap;
begin
   bbitmap:=tbitmap.Create;
   bbitmap.width:=isize;
   bbitmap.height:=isize;
   bbitmap.Transparent:=false;
   bbitmap.canvas.Brush.Color:=clwindow;
   if deco=DECO_BORDER then bbitmap.canvas.Pen.Color:=clbtnface
   else bbitmap.canvas.Pen.Color:=clnone;
   bbitmap.canvas.Rectangle(0,0,isize,isize);

   iwidth:=abitmap.Width;
   iheight:=abitmap.Height;
   rect.Left := (isize-round(iwidth*ascale)) div 2;
   rect.Top := (isize-round(iheight*ascale)) div 2;
   rect.Right:=rect.Left+Round(iwidth*ascale);
   rect.Bottom:=rect.Top+Round(iheight*ascale);

   if deco=DECO_SHADOW then
   begin
   bbitmap.canvas.Brush.Color:=clnone;
   bbitmap.canvas.Pen.Color:=clbtnface;
   bbitmap.canvas.Rectangle(rect.Left-1,rect.Top-1,rect.Right+1,rect.Bottom+1);
   bbitmap.canvas.Brush.Color:=$00c0c0c0;
   bbitmap.canvas.Pen.Color:=$00e0e0e0;
   bbitmap.canvas.Rectangle(rect.Left+1,rect.Top+1,rect.Right+2,rect.Bottom+2);
   end;

   if deco=DECO_FRAME then
   begin
   bbitmap.canvas.Brush.Color:=clnone;
   bbitmap.canvas.Pen.Color:=clbtnface;
   bbitmap.canvas.Rectangle(rect.Left-6,rect.Top-6,rect.Right+6,rect.Bottom+6);
   end;

   bbitmap.Canvas.StretchDraw(rect, abitmap);

   abitmap.Assign(bbitmap);
   bbitmap.free;
end;

procedure setsize_bitmap(var abitmap:tbitmap; isize, deco:integer);
var
   wscale,hscale,ascale:double;
   rect : TRect;
   csize:integer;
begin
   case deco of
       DECO_FRAME: csize:=isize-12;
       DECO_BORDER: csize:=isize-12;
       DECO_SHADOW: csize:=isize-4;
   else csize:=isize; //let room for borders
   end;
   if abitmap.Width<>0 then wscale:=csize / abitmap.Width else wscale:=100;
   if abitmap.Height<>0 then hscale:=csize / abitmap.Height else hscale:=100;
   if wscale<hscale then ascale:=wscale
   else ascale:=hscale;
   if ascale>1 then ascale:=1;
   scale_bitmap(abitmap, isize, deco, ascale)
end;

procedure getimageinfo(aimage:TImage; var imginfo:utf8string);
begin
try imginfo:=inttostr(aimage.Picture.Bitmap.Width)+'*'+inttostr(aimage.Picture.Bitmap.height)+'@';
except imginfo:=''; end;
case aimage.Picture.Bitmap.PixelFormat of
    pfDevice: imginfo:=imginfo+'Device';
    pf1bit: imginfo:=imginfo+'1';
    pf4bit: imginfo:=imginfo+'4';
    pf8bit: imginfo:=imginfo+'8';
    pf15bit: imginfo:=imginfo+'15';
    pf16bit: imginfo:=imginfo+'16';
    pf24bit: imginfo:=imginfo+'24';
    pf32bit: imginfo:=imginfo+'32';
    pfCustom: imginfo:=imginfo+'Custom';
end;
end;

procedure loadlargeicon(srcbitmap:TBitmap; var destbitmap:Tbitmap; destsize:integer);
begin
destbitmap.Assign(srcbitmap);
setsize_bitmap(destbitmap,destsize,DECO_NONE);
destbitmap.TransparentColor:=$00FFFFFF;
destbitmap.Transparent:=true;
end;

function load_bitmap(var abitmap:Tbitmap; s:utf8string; isize, deco:integer; var imginfo:utf8string):integer;
var
   iscale:double;
   aimage:TImage;
begin
load_bitmap:=-1;
Try
aimage:=TImage.Create(nil);
aimage.Parent:=nil;
aimage.Picture.LoadFromFile(s);
getimageinfo(aimage, imginfo);
abitmap.assign(aimage.Picture.Bitmap);
setsize_bitmap(abitmap, isize, deco);
aimage.free;
load_bitmap:=0;
Except
load_bitmap:=1;
end
end;

function load_imagefiletopicture(var apicture:Tpicture; s:utf8string):integer;
begin
load_imagefiletopicture:=-1;
Try
apicture:=Tpicture.Create;
apicture.LoadFromFile(s);
load_imagefiletopicture:=0;
Except
load_imagefiletopicture:=1;
end
end;

function rotate_picture(var apicture:Tpicture; rfun:utf8string): integer;
var
   x,y: Integer;
   rlh,rlw: Integer;
   bpicture:Tpicture;
begin
rlw:=apicture.Width;
rlh:=apicture.Height;
bpicture:=Tpicture.Create;

case rfun of
   'right':
   with bpicture do
   begin
   bitmap.Width:=rlh;
   bitmap.Height:=rlw;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlh-y-1,x]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'left':
   with bpicture do
   begin
   bitmap.Width:=rlh;
   bitmap.Height:=rlw;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[y,rlw-x-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   '180':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlw-x-1,rlh-y-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'flip':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[x,rlh-y-1]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;

   'mirror':
   with bpicture do
   begin
   bitmap.Width:=rlw;
   bitmap.Height:=rlh;
   for x:=0 to rlw-1 do
      for y:=0 to rlh-1 do
         bitmap.Canvas.Pixels[rlw-x-1,y]:=apicture.bitmap.Canvas.Pixels[x,y];
   end;
   end;
apicture.assign(bpicture);
bpicture.Free;
end;

function resize_picture(var apicture:Tpicture; wsize,hsize:integer): integer;
var
   rect : TRect;
   bpicture:Tpicture;
begin
bpicture:=tpicture.Create;
bpicture.bitmap.width:=wsize;
bpicture.bitmap.height:=hsize;
rect.Left := 0;
rect.Top := 0;
rect.Right:=wsize;
rect.Bottom:=hsize;
bpicture.bitmap.Canvas.StretchDraw(rect, apicture.bitmap);
apicture.assign(bpicture);
bpicture.Free;
end;

function crop_picture(var apicture:Tpicture; ctop,cbottom,cleft,cright:integer): integer;
var
   x,y: Integer;
   rlh,rlw: Integer;
   bpicture:Tpicture;
begin
rlw:=apicture.bitmap.Width;
rlh:=apicture.bitmap.Height;
bpicture:= Tpicture.Create;
with bpicture do
   begin
   bitmap.Width:=rlw-cleft-cright;
   bitmap.Height:=rlh-ctop-cbottom;
   for x:=0 to rlw-cleft-cright-1 do
      for y:=0 to rlh-ctop-cbottom-1 do
         bitmap.Canvas.Pixels[x,y]:=apicture.bitmap.Canvas.Pixels[x+cleft,y+ctop];
   end;
apicture.assign(bpicture);
bpicture.Free;
end;

function save_picturetoimagefile(var apicture:Tpicture; s:utf8string):integer;
begin
try
save_picturetoimagefile:=-1;
apicture.SaveToFile(s);
apicture.Free;
save_picturetoimagefile:=0;
except
save_picturetoimagefile:=1;
try apicture.Free; except end;
end;
end;

function saveconvert_picturetoimagefile(var apicture:Tpicture; s,convext:utf8string; convopt:integer):integer;
begin
try
saveconvert_picturetoimagefile:=-1;
convext:=LowerCase(convext);
case convext of
'.jpg', '.jpeg', '.jpe', '.jif', '.jfif', '.jfi': apicture.Jpeg.CompressionQuality:=convopt;
end;
apicture.SaveToFile(s+'.'+convext);
apicture.Free;
saveconvert_picturetoimagefile:=0;
except
saveconvert_picturetoimagefile:=1;
try apicture.Free; except end;
end;
end;

end.

