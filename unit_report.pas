unit Unit_report;
{
 DESCRIPTION     :  Unit providing GUI for display reports in two string grids
                    and four label

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060908  G.Tani      Initial version
 0.11     20060920  G.Tani      removed *_VER; P_RELEASE constant in pea_utils
                                is used to keep track of release level;
                                for porting the application please refer to notes
                                in unit Peach.
 0.12     20060927  G.Tani      changed Win32 transparence code to be compatible
                                with all Win32 versions (no longer needed separate
                                builds);
 0.12b    20070328  G.Tani      Minor visual updates for better integration with
                                PeaZip 1.6 look and feel
 0.13     20070503  G.Tani      Updated look and feel
 0.14     20070802  G.Tani      Accepts new PeaZip theming
 0.15     20070924  G.Tani      Updated according to PeaZip theming improvements
 0.16     20071130  G.Tani      Minor cleanup
 0.17     20080314  G.Tani      Transparency made available for Win64
 0.18     20080707  G.Tani      Updated to work with utf8 LCL
 0.19     20080826  G.Tani      Ask path for saving reports, default is desktop (or current path if desktop is not found)
 0.20     20081026  G.Tani      Autosized/customisable GUI's items height; various graphic updates
                                Form_report that can now close the application if it is the only form needing to be shown
 0.21     20081118  G.Tani      appdata fixed for Windows users with names containing extended characters
                                filemode set to 0 before all reset file operations to avoid possible lock situations (i.e. concurrent instances)
 0.22     20091103  G.Tani      New icons
 0.23     20101105  G.Tani      Updated look and feel

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com
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

uses
  {$IFDEF MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  Grids, StdCtrls, ExtCtrls, ComCtrls,
  ansiutf8_utils, list_utils, pea_utils, Menus;

type

  { TForm_report }

  TForm_report = class(TForm)
    Bevel9: TBevel;
    Button1: TImage;
    Button2: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Notebook1: TPageControl;
    Input: TTabSheet;
    Output: TTabSheet;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end;

procedure save_report(s:utf8string);
  
var
  Form_report: TForm_report;
   t:text;
   //theming
   conf:text;
   opacity,rowheight,itemheight,autosizeitemheight:integer;
   confpath:utf8string;
   executable_path,dummy,color1,color2,color3,color4,color5:string;
   Binfo,Bloadlayout:TBitmap;
   
implementation

function wingetdesk(var dp:utf8string):integer;
{$IFDEF MSWINDOWS}
var
  pidl: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
wingetdesk:=-1;
{$IFDEF MSWINDOWS}
try
   if Succeeded(ShGetSpecialFolderLocation(Form_report.Handle,0,pidl)) then //0 is CSIDL_DESKTOP numerical value
      if ShGetPathfromIDList(pidl, Buf ) then
         begin
         dp:=envtoutf8(Buf);
         CoTaskMemFree(pidl);
         wingetdesk:=0;
         end
      else CoTaskMemFree(pidl);
except
end;
{$ENDIF}
end;

procedure save_report(s:utf8string);
var
x,y:dword;
field_delim:string;
p:utf8string;
begin
field_delim:=chr($09);
{$IFDEF MSWINDOWS}wingetdesk(p);{$ELSE}get_desktop_path(p);{$ENDIF}
if p[length(p)]<>directoryseparator then p:=p+directoryseparator;
s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+s+'.txt';
Form_report.SaveDialog1.FileName:=p+s;
if udirectoryexists(p) then Form_report.SaveDialog1.InitialDir:=p;
if Form_report.SaveDialog1.Execute then
begin
s:=Form_report.SaveDialog1.FileName;
uassigntext(t,s);
rewrite(t);
write_header(t);
for x:=0 to Form_report.StringGrid1.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid1.ColCount-1 do
      write(t,Form_report.StringGrid1.Cells[y,x]+field_delim);
   writeln(t);
   end;
for x:=0 to Form_report.StringGrid2.RowCount-1 do
   begin
   for y:=0 to Form_report.StringGrid2.ColCount-1 do
      write(t,Form_report.StringGrid2.Cells[y,x]+field_delim);
   writeln(t);
   end;
writeln(t,Form_report.Label1.Caption);
writeln(t,Form_report.Label2.Caption);
writeln(t,Form_report.Label3.Caption);
writeln(t,Form_report.Label4.Caption);
closefile(t);
end;
end;

{ TForm_report }

procedure TForm_report.Button1Click(Sender: TObject);
begin
save_report(Form_report.Caption);
end;

procedure conditional_stop;
begin
if Form_report.Caption='List' then Application.Terminate;
if Form_report.Caption='Info' then Application.Terminate;
if Form_report.Caption='Compare' then Application.Terminate;
if Form_report.Caption='Checksum and hash' then Application.Terminate;
if Form_report.Caption='Environment variables' then Application.Terminate;
if Form_report.Caption='Hex preview' then Application.Terminate;
end;

procedure TForm_report.Button2Click(Sender: TObject);
begin
Form_report.Visible:=false;
conditional_stop;
end;

procedure TForm_report.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
conditional_stop;
end;

function wingetappdata(var s:utf8string):integer;
{$IFDEF MSWINDOWS}
var
  pidl: PItemIDList;
  Buf: array [0..MAX_PATH] of Char;
{$ENDIF}
begin
wingetappdata:=-1;
{$IFDEF MSWINDOWS}
try
   if Succeeded(ShGetSpecialFolderLocation(Form_report.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
      if ShGetPathfromIDList(pidl, Buf ) then
         begin
         s:=envtoutf8(Buf)+'\PeaZip\';
         CoTaskMemFree(pidl);
         wingetappdata:=0;
         end
      else CoTaskMemFree(pidl);
except
end;
{$ENDIF}
end;

initialization
  {$I unit_report.lrs}

end.

