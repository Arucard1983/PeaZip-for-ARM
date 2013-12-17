unit Unit_pea;
{
 DESCRIPTION     :  Unit providing PEA, UnPEA, Raw File Split/Join features.
                    Can either be compiled as a standalone GUI application with
                    parameters passed by Comman Line or can be used within
                    another application calling *_lib_procedure procedures
                    with appropriate parameters

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060915  G.Tani
 0.11     20060920  G.Tani
 0.12     20060925  G.Tani
 0.12b    20061130  G.Tani
 0.12c    20070122  G.Tani
 0.12d    20070224  G.Tani
 0.13     20070503  G.Tani
 0.14     20070605  G.Tani
 0.15     20070804  G.Tani
 0.16     20071001  G.Tani
 0.17     20071028  G.Tani
 0.17b    20071124  G.Tani
 0.18     20080124  G.Tani
 0.19     20080318  G.Tani
 0.19b    20080511  G.Tani
 0.20     20080730  G.Tani
 0.21     20080922  G.Tani
 0.22     20081030  G.Tani
 0.23     20081118  G.Tani
 0.24     20090116  G.Tani
 0.25     20090215  G.Tani
 0.26     20090324  G.Tani
 0.27     20090709  G.Tani
 0.28     20091016  G.Tani
 0.29     20091028  G.Tani
 0.30     20091109  G.Tani
 0.31     20100613  G.Tani
 0.32     20101016  G.Tani
 0.33     20101122  G.Tani
 0.34     20101224  G.Tani
 0.35     20110226  G.Tani
 0.36     20110611  G.Tani
 0.37     20110726  G.Tani
 0.38     20110913  G.Tani
 0.39     20111005  G.Tani
 0.40     20120607  G.Tani
 0.41     20120805  G.Tani      Real time approximate calculation of possible compression in advanced List (Info) function
                                Application auto closes accordingly to PeaZip policy for operation needing to automatically close (PEA, UNPEA, file split, file join, secure delete)
          20120805  G.Tani      Uniformed Button Panels design over the application
 0.42     20130221  G.Tani      New theming engine
                                New high resolution application icon
          20130322  G.Tani      Recompiled with Lazarus 1.0.8
 0.43     20130408  G.Tani      Fixed single volume size issue for Pea format on Win64
 0.44     20130617  G.Tani      Code cleanup
          20130718  G.Tani      Recompiled with Lazarus 1.0.10
 0.45     20130928  G.Tani      Secure delete changes system files attribute to allow operation
                                Recompiled with Lazarus 1.0.12
 0.46     20131122  G.Tani      Secure deletion: added VERY_FAST mode (single pass, random pattern) and ZERO (single pass overwriting data with zero)
                                Addes Sanitize function (free space deletion) with ZERO mode and VERY_FAST to VERY_SLOW secure deletion modes

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com
Official PeaZip site http://www.peazip.org
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
{$INLINE ON}{$UNITPATH ./we}

interface

uses
{$IFDEF MSWINDOWS}
Windows, activex,
{$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Process, Spin,
  Buttons, ComCtrls, StdCtrls, strutils, zuncompr, zcompres,
  hash, adler32, CRC16, CRC24, CRC32, CRC64, ED2K, MD4, MD5, RMD160, SHA1, SHA224, SHA256, SHA384, SHA512, Whirl512, aes_ctr, AES_Type, AES_EAX, fcrypta, FCAES256, mem_util,
  list_utils, pea_utils, rfs_utils, ansiutf8_utils, unit_report;

type

  { TForm_pea }

  TForm_pea = class(TForm)
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel9: TBevel;
    ButtonDone1: TBitBtn;
    ButtonPW1: TBitBtn;
    ButtonPW2: TBitBtn;
    ButtonRFSinteractive: TBitBtn;
    ButtonRFSinteractive1: TBitBtn;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    EditConfirm1: TEdit;
    EditPW1: TEdit;
    Image7: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    ImageList1: TImageList;
    ImageSplit: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LabelConfirm1: TLabel;
    LabelDecrypt2: TLabel;
    LabelDecrypt3: TLabel;
    LabelDecrypt4: TLabel;
    LabelDecrypt5: TLabel;
    LabelDecrypt6: TLabel;
    LabelE1: TLabel;
    LabelEncrypt2: TLabel;
    LabelEncrypt3: TLabel;
    LabelEncrypt4: TLabel;
    LabelEncrypt5: TLabel;
    LabelEncrypt6: TLabel;
    LabelHint1: TLabel;
    LabelKeyFile1: TLabel;
    LabelLog1: TBitBtn;
    LabelOpen: TBitBtn;
    LabelOut1: TLabel;
    LabelPS1: TLabel;
    LabelPW1: TLabel;
    LabelKeyFileName1: TLabel;
    LabelSample1: TLabel;
    LabelSample2: TLabel;
    LabelTime1: TLabel;
    LabelTools3: TLabel;
    LabelTools4: TLabel;
    LabelTools2: TLabel;
    OpenDialog1: TOpenDialog;
    PanelDecrypt1: TPanel;
    PanelEncrypt1: TPanel;
    Panel1: TPanel;
    PanelPW1: TPanel;
    PanelRFSinteractive: TPanel;
    PanelTools: TPanel;
    ProgressBar1: TProgressBar;
    Shape2: TShape;
    ShapeE1: TShape;
    ShapeE2: TShape;
    SpinEdit1: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonDone1Click(Sender: TObject);
    procedure ButtonPW1Click(Sender: TObject);
    procedure ButtonPW2Click(Sender: TObject);
    procedure ButtonRFSinteractive1Click(Sender: TObject);
    procedure ButtonRFSinteractiveClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure EditPW1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LabelE1Click(Sender: TObject);
    procedure LabelKeyFile1Click(Sender: TObject);
    procedure LabelLog1Click(Sender: TObject);
    procedure LabelOpenClick(Sender: TObject);
    procedure PanelPW1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  Type fileofbyte = file of byte;

{$IFDEF MSWINDOWS}
type
  TSetLayeredWindowAttributes = function(hwnd: HWND; crKey: COLORREF; bAlpha: Byte; dwFlags: Longint): Longint;
  stdcall;
{$ENDIF}

const
  P_RELEASE          = '0.45'; //declares release version for the whole build
  PEA_FILEFORMAT_VER = 1;
  PEA_FILEFORMAT_REV = 0; //version and revision declared to be implemented must match with the ones in pea_utils, otherwise a warning will be raised (form caption)
  SBUFSIZE           = 32768;
  {32KB of size for reading small buffers, used for ciphers and hashes}
  WBUFSIZE           = 1048576;
  {1MB of size for reading whide buffers, used for compression.
  Decompression may read arbitrarily sized buffers up to array size used for
  wide buffers -64KB (left for possible data expansion)}
  {$IFDEF MSWINDOWS}
  DEFAULT_THEME = 'seven-embedded';
  {$ELSE}
  DEFAULT_THEME = 'firecrystal-embedded';
  {$ENDIF}
  RED                = $000000DD;
  ORANGE             = $0000AAFF;
  YELLOW             = $0000DDFF;
  OLIVE              = $0000DDDD;
  GREEN              = $0000DD80;
  VIOLET             = $00C08080;
  WS_EX_LAYERED      = $80000;
  LWA_ALPHA          = $2;

var
  Form_pea: TForm_pea;
   wbuf1,wbuf2:array[0..1114111] of byte; //>1MB wide buffers (1MB+ 64KB)
   fun,pw,keyfile_name,output,vol_algo,graphicsfolder,caption_build,delimiter,confpath:utf8string;
   vol_size:qword;
   desk_env:byte;
   interacting,control,details,height_set:boolean;
   ment,kent,fent,ment_sample: THashContext;
   mentd: TWhirlDigest;
   mentd_sample: TSHA256Digest;
   fingerprint: TSHA512Digest;
   in_param,in_files,exp_files,status_objects,status_volumes,exp_fattr_dec,fattr_dec:TFoundList;
   status_files:TFoundListBool;
   fsizes,exp_fsizes:TFoundListSizes;
   ftimes,exp_ftimes:TFoundListAges;
   fattr,exp_fattr:TFoundListAttrib;
   obj_tags,exp_obj_tags,volume_tags,exp_volume_tags:TFoundListArray64;
   Bfd,Bmail,Bhd,Bdvd,Binfo,Blog,Bok,Bcancel:TBitmap;
   //theming
   conf:text;
   opacity,rowheight,stdbtnheight,closepolicy:integer;
   executable_path,persistent_source,color1,color2,color3,color4,color5:string;
   {$IFDEF MSWINDOWS}
   hUser32: HMODULE;
   SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
   osVerInfo: TOSVersionInfo;
   {$ENDIF}

{
PEA features can be called using different modes of operation:
INTERACTIVE the form is visible, user's input is requested if needed (can be used only calling PEA from command line, it's not allowed in *_lib_procedure procedures)
BATCH       the form is visible, user's input not requested: if passphrase/keyfile are needed are got from next two parameters of command line
HIDDEN      the form is not visible, user input not requested (as for BATCH)
*_REPORT    can be applied to each mode, the program operates as described for the mode used and then an automated job report is saved at the end of the operation
mode of operation is declared as opmode in *_lib_procedure, then passed to *_procedure as pw_param
INTERACTIVE* modes can be used only for PEA and UnPEA (since only those features may require keying), other modes can be used also for RFS and RFJ
}

//procedure to call pea within another application
procedure pea_lib_procedure ( out_param: utf8string;                            //archive qualified name (without .(volume number).PEA suffix) or AUTONAME
                              ch_size: qword;                                   //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              compr: utf8string;                                //compression scheme to use
                              volume_algo:utf8string;                           //algorithm for volume integrity check
                              obj_algo: utf8string;                             //algorithm for object integrity check
                              algo:utf8string;                                  //algorithm for stream integrity check
                              password,keyf_name:utf8string;                    //password and keyfile qualified name (if needed by stream algo)
                              in_param:TFoundList;                              //array of utf8string containing input qualified names
                              opmode:utf8string);                               //mode of operation

procedure pea_procedure ( out_param: utf8string;
                          ch_size: qword;
                          compr: utf8string;
                          compr_level: byte;
                          volume_algo:utf8string;
                          volume_authsize:byte;
                          obj_algo: utf8string;
                          obj_authsize: byte;
                          algo:utf8string;
                          headersize,authsize: byte;
                          pwneeded: boolean;
                          pw_param,password,keyf_name:utf8string;
                          in_param:TFoundList);

//procedure to call unpea within another application
procedure unpea_lib_procedure ( in_qualified_name,                              //archive qualified name
                                out_param,                                      //dir were extracting the archive (or AUTONAME)
                                date_param,                                     //actually only supported RESETDATE, reset date of extracted files
                                attr_param,                                     //RESETATTR (or SETATTR only on Windows to set object's attributes as on original ojects)
                                struct_param,                                   //actually only supported EXTRACT2DIR, create a dir and extract archive in the dir using shortest paths for archived objects
                                password,keyf_name:utf8string;                  //password and keyfile qualified name (if needed)
                                opmode:utf8string);                             //mode of operation

procedure unpea_procedure ( in_qualified_name,
                            out_param,
                            date_param,
                            attr_param,
                            struct_param,
                            pw_param,
                            password,
                            keyf_name:utf8string);

//procedure to call raw file split within another application
procedure rfs_lib_procedure ( out_param:utf8string;                             //qualified name for output volumes (without .(volume number) suffix) or AUTONAME
                              ch_size:qword;                                    //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              volume_algo,                                      //algorithm for volume integrity check
                              in_qualified_name:utf8string;                     //qualified name of input file
                              opmode:utf8string);                               //mode of operation

procedure rfs_procedure ( out_param:utf8string;
                          ch_size:qword;
                          volume_algo:utf8string;
                          volume_authsize:byte;
                          pw_param:utf8string;
                          in_qualified_name:utf8string);

//procedure to call raw file join within another application
procedure rfj_lib_procedure ( in_qualified_name,                                //qualified name of first volume of the splitted file
                              out_param,                                        //qualified name to give to the output rejoined file (or AUTONAME)
                              opmode:utf8string);                               //mode of operation

procedure rfj_procedure ( in_qualified_name,
                          pw_param,
                          out_param:utf8string);
                          
implementation

{
misc procedures
}

//timing
procedure timing(tsin:TTimeStamp; size:qword);
var tsout:TTimeStamp;
time,speed:qword;
begin
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if time<=0 then time:=100000;
speed:=(size * 1000) div time;
Form_pea.LabelTime1.Caption:='Processed '+nicenumber(inttostr(size))+' in '+nicetime(inttostr(time))+' @ '+nicenumber(inttostr(speed))+'/s';
Form_pea.ButtonDone1.Visible:=true;
end;

//if an error is encountered calling a PEA_utils procedure, show error description then halt, otherwise (error code is 0) continue
procedure test_pea_error ( s:utf8string;
                           err:integer);
var
   decoded_err:utf8string;
begin
if err<>0 then
   begin
   decode_pea_error(err,decoded_err);
   showmessage('Error '+s+': '+inttostr(err)+' '+decoded_err);
   Application.Terminate;
   end;
end;

//when an internal error is encountered, show error description then halt
procedure internal_error (s:utf8string);
begin
showmessage(s);
halt;
end;

procedure clean_global_vars;
begin
SetLength(in_param,0);
SetLength(in_files,0);
SetLength(exp_files,0);
SetLength(status_objects,0);
SetLength(status_volumes,0);
SetLength(exp_fattr_dec,0);
SetLength(fattr_dec,0);
SetLength(status_files,0);
SetLength(fsizes,0);
SetLength(exp_fsizes,0);
SetLength(ftimes,0);
SetLength(exp_ftimes,0);
SetLength(fattr,0);
SetLength(exp_fattr,0);
SetLength(obj_tags,0);
SetLength(exp_obj_tags,0);
SetLength(volume_tags,0);
SetLength(exp_volume_tags,0);
output:='';
vol_size:=0;
vol_algo:='';
end;

procedure checkspace(outpath:utf8string; chsize:qword);
var size_ok:boolean;
begin
size_ok:=false;
repeat
   if ((chsize>diskfree(0)) and (chsize<>1024*1024*1024*1024*1024)) then
      if MessageDlg('Output path '+outpath+' seems to not have enough free space for an output volume, try to free some space on it or exchange it with an empty one if it''s a removable media. Do you want to test the path another time?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate
   else size_ok:=true;
until size_ok=true;
end;

procedure checkspacepea(outpath:utf8string; chsize,volume_authsize:qword);
var size_ok:boolean;
begin
size_ok:=false;
repeat
   if ((chsize>diskfree(0)) and (chsize<>1024*1024*1024*1024*1024-volume_authsize)) then
      if MessageDlg('Output path '+outpath+' seems to not have enough free space for an output volume, try to free some space on it or exchange it with an empty one if it''s a removable media. Do you want to test the path another time?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate
   else size_ok:=true;
until size_ok=true;
end;

procedure check_chunk ( in_folder:utf8string;
                        j:dword;
                        var chunks_ok:boolean);
begin
chunks_ok:=false;
if MessageDlg('The path "'+in_folder+'" seem not containing volume '+inttostr(j)+' (i.e. volumes are on multiple removable media and you have to change the media). Check again?',mtWarning,[mbYes, mbNo],0)=6 then
else internal_error('Impossible to read requested volume(s). Not found volume '+inttostr(j));
end;

procedure read_from_chunks ( in_folder,in_name:utf8string;                      //path and base name of input file; actual PEA filename get updated by update_pea_filename procedure
                             byte_to_read:dword;                                //size to be read from chunks
                             var buf: array of byte;                            //buffer with output data
                             var tmp_buf: array of byte;                        //buffer used to temporarily store the data to compose in the output buffer
                             volume_tag_size:byte;                              //size of volume tag, data to be skipped at the end of each volume;
                             maxsize:dword;                                     //max size to read at once
                             singlevolume:boolean);
var
   i,j,k,ind,numread:dword;
   total:qword;
   chunks_ok:boolean;
   in_file:utf8string;
   f_in:file of byte;
begin
j:=1;
ind:=0;
chunks_ok:=true;
in_file:=in_name;
while ((chunks_ok=true) and (ind<byte_to_read)) do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file);
   repeat
      if ufileexists(in_folder+in_file) then
         begin
         chunks_ok:=true;
         uassignfile(f_in,in_folder+in_file);
         filemode:=0;
         reset(f_in);
         if IOResult<>0 then internal_error('IO error opening '+in_folder+in_file);
         srcfilesize(in_folder+in_file,total);
         total:=total-volume_tag_size;
         //total:=system.filesize(f_in)-volume_tag_size;
         while ((total>0) and (ind<byte_to_read)) do
            begin
            if total>maxsize then i:=maxsize else i:=total;
            blockread (f_in,tmp_buf,i,numread);
            if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
            dec(total,numread);
            for k:=0 to numread-1 do buf[ind+k]:=tmp_buf[k];
            inc(ind,numread);
            end;
         close(f_in);
         if IOResult<>0 then internal_error('IO error closing '+in_folder+in_file);
         j:=j+1;
         end
      else check_chunk(in_folder,j,chunks_ok);
   until chunks_ok=true;
   end;
end;

procedure gen_rand(var arr: array of byte);
var
   ment1,kent1,fent1: THashContext;
begin
ment1:=ment;
kent1:=kent;
fent1:=fent;
generate_keyf (arr,persistent_source,fingerprint,ment1,kent1,fent1);
end;

procedure shl_rand(var arr: array of byte);
var
   randf: file of byte;
   randarr: TKey2048;
   i,j: integer;
begin
try
//read current rand seed file
uAssignFile(randf,persistent_source);
filemode:=0;
reset(randf);
blockread(randf,randarr,256,j);
closefile(randf);
//left shift by one byte the array of the rand seed
for i:=0 to 254 do arr[i]:=randarr[i+1];
arr[255]:=randarr[0];
except
end;
end;

{
PEA: Pack (archive, compress and split) Encrypt and Authenticate
The program accept n objects (files, dirs) as input, merge them into a single
archive and give m output chunks of desired size.
Number of objects to be archived is acutally only memory limited, not format
limited (PEA format allow unlimited input objects); each object can be up to 2^64
byte in size.
PEA file format version 1 revision 0 can create a single stream, optionally
encrypted and authenticated, containing all objects to be archived, keyed by
passphrase and optionally keyfile (two factor authentication).
Metadata associated to archived objects are: qualified name, last modification
time, attributes; if more advanced archiving/restoring/backup features are
needed it's recommended using asyncronously tar or similar programs more focused
on that needs before sending the resulting file to PEA.

Notes:
- W.Ehrhardt's hash and crypto libraries are used for hashes, checksums, cyphers
  and key scheduling (PBKDF2);
- Lazarus paszlib compression libraries were used to build a custom compression
  scheme (PCOMPESS*);
}

procedure PEA;
var
   out_param,compr,volume_algo,obj_algo,algo,pw_param,password,keyf_name,list_param,listfile_param:utf8string;
   ch_size:qword;
   compr_level,volume_authsize,obj_authsize,headersize,authsize:byte;
   pwneeded:boolean;

procedure parse_pea_cl; //exit at first error with descriptive message, including parameters passed if relevant
var i,k:dword;
begin
i:=0;
try
   out_param:=envtoutf8(paramstr(2));
   //// control volume size
   try
      ch_size:=strtoqword(paramstr(3));
      if ch_size=0 then ch_size:=1024*1024*1024*1024*1024;//high(ch_size); set to 1024 TB// if chunk size is set to 0 no chunks will be done
   except
      internal_error('"'+paramstr(3)+'" is not a valid chunk size; values allowed are 1..2^64, 0 to don''t split the input');
   end;
   //get compression algorithm
   compr:=upcase(paramstr(4));
   if decode_compression_algo(compr,compr_level)<>0 then
      internal_error('"'+compr+'" is not a valid compression algorithm, please refer to the documentation for supported ones');
   //get volume control algorithm
   volume_algo:=upcase(paramstr(5));
   if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
      internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
   if ch_size<volume_authsize+10 then ch_size:=volume_authsize+10;//chunk size is set at least 10 byte over volume size, in order to have at leas 10 byte of data in the first volume to allow to read archive header at once (needed to know volume authsize in UnPEA)
   ch_size:=ch_size-volume_authsize;
   //get object control algorithm
   obj_algo:=upcase(paramstr(6));
   if decode_obj_control_algo(obj_algo,obj_authsize)<>0 then
      internal_error('"'+obj_algo+'" is not a valid control algorithm for object check, please refer to the documentation for supported ones');
   //get control algorithm
   algo:=upcase(paramstr(7));
   if decode_control_algo(algo,headersize,authsize,pwneeded)<>0 then
      internal_error('"'+algo+'" is not a valid control algorithm, please refer to the documentation for supported ones');
   //get operation mode
   inc(i,1);
   pw_param:=upcase(paramstr(7+i));
   if pwneeded=true then
      begin
      if (pw_param<>'INTERACTIVE') and (pw_param<>'INTERACTIVE_REPORT') then
         begin
         inc(i,1);
         password:=envtoutf8(paramstr(7+i));
         inc(i,1);
         keyf_name:=envtoutf8(paramstr(7+i));
         end
      else
         if (pw_param<>'INTERACTIVE') and (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'INTERACTIVE_REPORT') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
            internal_error('"'+pw_param+'" is not a valid operation mode parameter, please refer to the documentation');
      end;
   //get input list (it will be expanded in pea_procedure)
   list_param:=upcase(paramstr(8+i));
   if paramstr(8+i)<>'' then
      if list_param='FROMCL' then //get input files by CL
         begin
         for k:=0 to paramcount-9-i do
            begin
            SetLength(in_param,k+1);
            in_param[k]:=envtoutf8(paramstr(k+9+i));
            end;
         end
      else
         if list_param='FROMFILE' then //get input files from a list file (an ansi text file containing a list of object names, each object in a line)
            begin
            listfile_param:=envtoutf8(paramstr(9+i));
            case read_filelist(listfile_param,in_param) of
              13: internal_error('The list file '+listfile_param+' is empty');
              14: internal_error('Cannot access the specified list file '+listfile_param);
               end;
            end
         else internal_error('Input method '+list_param+' not allowed')
   else internal_error('No accessible input object found');
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_pea_cl;
pea_procedure(out_param,ch_size,compr,compr_level,volume_algo,volume_authsize,obj_algo,obj_authsize,algo,headersize,authsize,pwneeded,pw_param,password,keyf_name,in_param);
end;

procedure pea_lib_procedure ( out_param: utf8string;                            //archive qualified name (without .(volume number).PEA suffix) or AUTONAME
                              ch_size: qword;                                   //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              compr: utf8string;                                //compression scheme to use
                              volume_algo:utf8string;                           //algorithm for volume integrity check
                              obj_algo: utf8string;                             //algorithm for object integrity check
                              algo:utf8string;                                  //algorithm for stream integrity check
                              password,keyf_name:utf8string;                    //password and keyfile qualified name (if needed by stream algo)
                              in_param:TFoundList;                              //array of utf8string containing input qualified names
                              opmode:utf8string);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:utf8string;
   compr_level,volume_authsize,obj_authsize,headersize,authsize:byte;
   pwneeded:boolean;
begin
//// control volume size
if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
//get compression algorithm
if decode_compression_algo(compr,compr_level)<>0 then
   internal_error('"'+compr+'" is not a valid compression algorithm, please refer to the documentation for supported ones');
//get volume control algorithm
if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
   internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
if ch_size<volume_authsize+1 then ch_size:=volume_authsize+1;
ch_size:=ch_size-volume_authsize;
//get object control algorithm
if decode_obj_control_algo(obj_algo,obj_authsize)<>0 then
   internal_error('"'+obj_algo+'" is not a valid control algorithm for object check, please refer to the documentation for supported ones');
//get control algorithm
if decode_control_algo(algo,headersize,authsize,pwneeded)<>0 then
   internal_error('"'+algo+'" is not a valid control algorithm, please refer to the documentation for supported ones');
//input list (will be expanded in pea_procedure) is jet loaded in in_param, TFoundList (array of utf8string)
//get operation mode
if (upcase(opmode)<>'INTERACTIVE') and (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'INTERACTIVE_REPORT') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter, please refer to the documentation');
if (upcase(opmode)='INTERACTIVE') or (upcase(opmode)='INTERACTIVE_REPORT') then
   internal_error('INTERACTIVE* modes are not allowed calling pea_lib_procedure, use BATCH* or HIDDEN* modes');
pw_param:=upcase(opmode);
pea_procedure(out_param,ch_size,compr,compr_level,volume_algo,volume_authsize,obj_algo,obj_authsize,algo,headersize,authsize,pwneeded,pw_param,password,keyf_name,in_param);
end;

procedure pea_procedure ( out_param: utf8string;
                          ch_size: qword;
                          compr: utf8string;
                          compr_level: byte;
                          volume_algo:utf8string;
                          volume_authsize:byte;
                          obj_algo: utf8string;
                          obj_authsize: byte;
                          algo:utf8string;
                          headersize,authsize: byte;
                          pwneeded: boolean;
                          pw_param,password,keyf_name:utf8string;
                          in_param:TFoundList);
var
   hdr : TFCAHdr;
   hdr256 : TFCA256Hdr;
   cxe : TAES_EAXContext;
   cxh : TFCA_HMAC_Context;
   auth : array [0..15] of byte; //valid type conversion for TFCA_AuthBlock and TFCA256_AuthBlock
   HashContext,HashContext_obj,HashContext_volume: THashContext;
   Whirl512Digest,Whirl512Digest_obj,Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest,SHA512Digest_obj,SHA512Digest_volume: TSHA512Digest;
   SHA256Digest,SHA256Digest_obj,SHA256Digest_volume: TSHA256Digest;
   SHA1Digest,SHA1Digest_obj,SHA1Digest_volume: TSHA1Digest;
   RMD160Digest,RMD160Digest_obj,RMD160Digest_volume: TRMD160Digest;
   MD5Digest,MD5Digest_obj,MD5Digest_volume: TMD5Digest;
   crc64,crc64_obj,crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   r: TSearchRec;
   f_in,f_out:file of byte;
   sbuf1,sbuf2:array [0..65535] of byte;
   auth_buf:array [0..63] of byte;
   filename_size,pw_len:word;
   err,adler,crc32,adler_obj,crc32_obj,adler_volume,crc32_volume:longint;
   i,j,k,addr,n_skipped,n_input_files,n_dirs,obj_ok,ch_number_expected,numread,compsize,compsize_d,num_res:dword;
   n_exp,file_size,total,cent_size,prog_size,prog_compsize,in_size,out_size,exp_size,ch_res:qword;
   in_qualified_name,out_file,out_path,out_name,s:utf8string;
   ansi_qualified_name:ansistring;
label 1;

procedure clean_variables;
begin
i:=0;
j:=0;
k:=0;
addr:=0;
n_skipped:=0;
n_input_files:=0;
n_dirs:=0;
obj_ok:=0;
ch_number_expected:=0;
numread:=0;
compsize:=0;
compsize_d:=0;
num_res:=0;
n_exp:=0;
file_size:=0;
total:=0;
cent_size:=0;
prog_size:=0;
prog_compsize:=0;
in_size:=0;
out_size:=0;
exp_size:=0;
ch_res:=0;
clean_global_vars;
end;

procedure expand_inputlist;
var i,k:dword;
fh_overhead:qword;
begin
addr:=0;
n_skipped:=0;
in_size:=0;
fh_overhead:=0;
for i:=0 to length(in_param)-1 do
   begin
   if ufilegetattr(in_param[i]) > 0 then
      if ufilegetattr(in_param[i]) and faDirectory <>0 then //Object is a dir
         begin
         expand(in_param[i],exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,n_exp);
         SetLength(in_files,length(in_files)+n_exp);
         SetLength(status_files,length(status_files)+n_exp);
         SetLength(fsizes,length(fsizes)+n_exp);
         SetLength(ftimes,length(ftimes)+n_exp);
         SetLength(fattr,length(fattr)+n_exp);
         SetLength(fattr_dec,length(fattr_dec)+n_exp);
         if in_param[i][length(in_param[i])]<>DirectorySeparator then in_param[i]:=in_param[i]+DirectorySeparator;
         for k:=0 to n_exp-1 do
            begin
            in_files[addr+k]:=exp_files[k];
            status_files[addr+k]:=true;
            fsizes[addr+k]:=exp_fsizes[k];
            in_size:=in_size+exp_fsizes[k];
            ftimes[addr+k]:=exp_ftimes[k];
            fattr[addr+k]:=exp_fattr[k];
            if (exp_fattr[k] and faDirectory)=0 then fh_overhead:=fh_overhead+length(exp_files[k])+18
            else fh_overhead:=fh_overhead+length(exp_files[k])+10;
            fattr_dec[addr+k]:=exp_fattr_dec[k];
            end;
         addr:=addr+n_exp;
         end
      else //Object is a file
         begin
         expand(in_param[i],exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,n_exp);
         SetLength(in_files,length(in_files)+1);
         SetLength(status_files,length(status_files)+1);
         SetLength(fsizes,length(fsizes)+1);
         SetLength(ftimes,length(ftimes)+1);
         SetLength(fattr,length(fattr)+1);
         SetLength(fattr_dec,length(fattr_dec)+1);
         in_files[addr]:=in_param[i];
         status_files[addr]:=true;
         fsizes[addr]:=exp_fsizes[0];
         fh_overhead:=fh_overhead+length(exp_files[0])+18;
         in_size:=in_size+exp_fsizes[0];
         ftimes[addr]:=exp_ftimes[0];
         fattr[addr]:=exp_fattr[0];
         fattr_dec[addr]:=exp_fattr_dec[0];
         addr:=addr+1;
         end
   else //Object not accessible
      begin
      SetLength(in_files,length(in_files)+1);
      SetLength(status_files,length(status_files)+1);
      SetLength(fsizes,length(fsizes)+1);
      SetLength(ftimes,length(ftimes)+1);
      SetLength(fattr,length(fattr)+1);
      SetLength(fattr_dec,length(fattr_dec)+1);
      in_files[addr]:=in_param[i];
      status_files[addr]:=false;
      inc(n_skipped,1);
      addr:=addr+1;
      end;
   end;
n_input_files:=addr;
exp_size:=in_size+headersize+authsize+6+fh_overhead;
if n_skipped=n_input_files then internal_error('No valid input found');
end;

procedure init_control_algo;
begin
case upcase(algo) of
'EAX256' : test_pea_error('creating stream crypto subheader with '+algo,pea_eax256_subhdr (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr256,sbuf1,num_res));
'EAX' : test_pea_error('creating stream crypto subheader with '+algo,pea_eax_subhdr (cxe,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr,sbuf1,num_res));
'HMAC' : test_pea_error('creating stream crypto subheader with '+algo,pea_hmac_subhdr (cxh,persistent_source,fingerprint,ment,kent,fent,7,sbuf2,pw_len,hdr,sbuf1,num_res));
'WHIRLPOOL' : Whirl_Init(HashContext);
'SHA512' : SHA512Init(HashContext);
'SHA256' : SHA256Init(HashContext);
'SHA1' : SHA1Init(HashContext);
'RIPEMD160' : RMD160Init(HashContext);
'MD5' : MD5Init(HashContext);
'CRC64' : CRC64Init(crc64);
'CRC32' : CRC32Init(crc32);
'ADLER32' : Adler32Init(adler);
end;
end;

procedure init_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_obj);
'SHA512' : SHA512Init(HashContext_obj);
'SHA256' : SHA256Init(HashContext_obj);
'SHA1' : SHA1Init(HashContext_obj);
'RIPEMD160' : RMD160Init(HashContext_obj);
'MD5' : MD5Init(HashContext_obj);
'CRC64' : CRC64Init(crc64_obj);
'CRC32' : CRC32Init(crc32_obj);
'ADLER32' : Adler32Init(adler_obj);
end;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_control_algo(var buf:array of byte; size:word);
begin
case upcase(algo) of
'EAX256' : if FCA_EAX256_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'EAX' : if FCA_EAX_encrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'HMAC' : if FCA_HMAC_encrypt(cxh, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'WHIRLPOOL' : Whirl_Update(HashContext, @buf, size);
'SHA512' : SHA512Update(HashContext, @buf, size);
'SHA256' : SHA256Update(HashContext, @buf, size);
'SHA1' : SHA1Update(HashContext, @buf, size);
'RIPEMD160' : RMD160Update(HashContext, @buf, size);
'MD5' : MD5Update(HashContext, @buf, size);
'CRC64' : CRC64Update(crc64, @buf, size);
'CRC32' : CRC32Update(crc32, @buf, size);
'ADLER32' : Adler32Update(adler, @buf, size);
end;
end;

procedure update_obj_control_algo(buf:array of byte; size:word);
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_obj, @buf, size);
'SHA512' : SHA512Update(HashContext_obj, @buf, size);
'SHA256' : SHA256Update(HashContext_obj, @buf, size);
'SHA1' : SHA1Update(HashContext_obj, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_obj, @buf, size);
'MD5' : MD5Update(HashContext_obj, @buf, size);
'CRC64' : CRC64Update(crc64_obj, @buf, size);
'CRC32' : CRC32Update(crc32_obj, @buf, size);
'ADLER32' : Adler32Update(adler_obj, @buf, size);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_control_algo;
begin
case upcase(algo) of
'EAX256' : FCA_EAX256_final(cxe, auth);
'EAX' : FCA_EAX_final(cxe, auth);
'HMAC' : FCA_HMAC_final(cxh, auth);
'WHIRLPOOL' : Whirl_Final(HashContext,WHIRL512Digest);
'SHA512' : SHA512Final(HashContext,SHA512Digest);
'SHA256' : SHA256Final(HashContext,SHA256Digest);
'SHA1' : SHA1Final(HashContext,SHA1Digest);
'RIPEMD160' : RMD160Final(HashContext,RMD160Digest);
'MD5' : MD5Final(HashContext,MD5Digest);
'CRC64' : CRC64Final(crc64);
'CRC32' : CRC32Final(crc32);
'ADLER32' : Adler32Final(adler);
end;
end;

procedure finish_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_obj,WHIRL512Digest_obj);
'SHA512' : SHA512Final(HashContext_obj,SHA512Digest_obj);
'SHA256' : SHA256Final(HashContext_obj,SHA256Digest_obj);
'SHA1' : SHA1Final(HashContext_obj,SHA1Digest_obj);
'RIPEMD160' : RMD160Final(HashContext_obj,RMD160Digest_obj);
'MD5' : MD5Final(HashContext_obj,MD5Digest_obj);
'CRC64' : CRC64Final(crc64_obj);
'CRC32' : CRC32Final(crc32_obj);
'ADLER32' : Adler32Final(adler_obj);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure write_volume_check;
var k:dword;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do auth_buf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA1Digest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do auth_buf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do auth_buf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,auth_buf,0);
      dword2bytebuf(crc64_volume.hi32,auth_buf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,auth_buf,0);
      'ADLER32' : dword2bytebuf(adler_volume,auth_buf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=auth_buf[k];
   blockwrite (f_out,auth_buf,volume_authsize);
   prog_compsize:=prog_compsize+volume_authsize;
   prog_size:=prog_size+volume_authsize;
   end;
end;

procedure write2chunks ( var num_res: dword;                     //amount of data to write
                         var buf_data: array of byte;            //data buffer
                         var f_out: fileofbyte;                  //output file
                         var out_path,out_name: utf8string;      //name and path for the output;
                         var i: dword;                           //chunk progressive number
                         var ch_size:qword;                      //chunk size
                         var ch_res: qword);                     //residual space in the given chunk
var ci,cj,k,numwritten:dword;
    addr,buf:qword;
    out_file:utf8string;
begin
addr:=0;
numwritten:=0;
while num_res>0 do
   begin
   if num_res<=ch_res then
      begin
      blockwrite (f_out,buf_data,num_res,numwritten);
      if IOResult<>0 then internal_error('IO error writing to volume '+inttostr(i));
      ci:=0;
      while ci<numwritten do
         begin
         if numwritten-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numwritten-ci;
         for k:=0 to cj-1 do sbuf1[k]:=buf_data[ci+k];
         update_volume_control_algo(sbuf1,cj);
         inc(ci,cj);
         end;
      num_res:=num_res-numwritten;
      ch_res:=ch_res-numwritten;
      addr:=0;
      end
   else
      begin
      SetLength(volume_tags,length(volume_tags)+1);
      blockwrite (f_out,buf_data,ch_res,numwritten);
      if IOResult<>0 then internal_error('IO error writing to volume '+inttostr(i));
      ci:=0;
      while ci<numwritten do
         begin
         if numwritten-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numwritten-ci;
         for k:=0 to cj-1 do sbuf1[k]:=buf_data[ci+k];
         update_volume_control_algo(sbuf1,cj);
         inc(ci,cj);
         end;
      finish_volume_control_algo;
      write_volume_check;
      if IOResult<>0 then internal_error('IO error writing volume control tag to volume '+inttostr(i));
      close(f_out);
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(i));
      i:=i+1;
      update_pea_filename(out_name,i,out_file);
      checkspacepea(out_path,ch_size,volume_authsize);
      uassignfile(f_out,out_path+out_file);
      rewrite(f_out); //it will overwrite orphaned files with same name to preserve name coherence
      if IOResult<>0 then internal_error('IO error opening volume '+inttostr(i));
      init_volume_control_algo;
      num_res:=num_res-numwritten;
      if num_res<ch_size then buf:=num_res else buf:=ch_size;
      addr:=addr+numwritten;
      for k:=0 to buf do buf_data[k]:=buf_data[addr+k];
      ch_res:=ch_size;
      end;
   end;
end;

procedure compress_file;
{
PCOMPRESS1..3 is a deflate-based scheme of compression that allows decompression
of single blocks without need of decompressing preceding blocks:
that slightly degrade compression compared to classical schemes but allow fast
access to arbitrary sectors knowing position in input data (feature not used in
this application)
}
var ci,cj,k:dword;
begin
//file data area
while ((numread<>0) and (total<file_size)) do
   begin
   blockread (f_in,wbuf1,WBUFSIZE,numread);
   inc(total,numread);
   inc(prog_size,numread);
   compsize:=numread+65536;
   {leave some room for expansion in compsize (64kb), however expanded blocks
   will be substituted by original blocks and compressed size will be set equal
   to input size, triggering decompression routine to not decompress but rather
   use the block as is (speeding up a bit operations on files that doesn't
   compress well or at all in case of user's misuse)}
   err:=zcompres.compress2(@wbuf2[0], compsize, wbuf1[0], numread, compr_level);
   if (err<>0) or (compsize>=numread) then
      begin
      wbuf2:=wbuf1;
      compsize:=numread;
      end;
   compsize_d:=compsize;
   //check of uncompressed size and data in the order it will be written
   dword2bytebuf(compsize,sbuf1,0);
   update_obj_control_algo(sbuf1,4);
   ci:=0;
   while ci<numread do
      begin
      if numread-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numread-ci;
      for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
      update_obj_control_algo(sbuf1,cj);
      inc(ci,cj);
      end;
   //compressed block size field, dword
   dword2bytebuf(compsize,wbuf1,0);
   //compressed block data field, variable sized
   for k:=0 to compsize_d-1 do wbuf1[k+4]:=wbuf2[k];
   ci:=0;
   while ci<compsize_d+4 do
      begin
      if compsize_d+4-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=compsize_d+4-ci;
      for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
      update_control_algo(sbuf1,cj);
      for k:=0 to cj-1 do wbuf1[ci+k]:=sbuf1[k];
      inc(ci,cj);
      end;
   num_res:=compsize_d+4;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  wbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
// uncompressed size of last buffer field (since it can not match the buffer size), dword
dword2bytebuf(numread,sbuf1,0);
update_obj_control_algo(sbuf1,4);
update_control_algo(sbuf1,4);
num_res:=4;
prog_compsize:=prog_compsize+4;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure nocompress_file;
begin
while ((numread<>0) and (total<file_size)) do
   begin
   blockread (f_in,sbuf1,SBUFSIZE,numread);
   inc(total,numread);
   inc(prog_size,numread);
   update_obj_control_algo(sbuf1,numread);
   update_control_algo(sbuf1,numread);
   num_res:=numread;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
end;

procedure write_eos; //unused in PEA file format 1.0
//write a trigger object that declare the end of the stream
begin
trigger_eos(sbuf1);
update_control_algo(sbuf1,6);
num_res:=6;
prog_size:=prog_size+6;
prog_compsize:=prog_compsize+6;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_eoa;
//write a trigger object that declare the end of the archive (instead of EOS in the last stream of the archive)
begin
trigger_eoa(sbuf1);
update_control_algo(sbuf1,6);
num_res:=6;
prog_size:=prog_size+6;
prog_compsize:=prog_compsize+6;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_auth;
var k:dword;
begin
finish_control_algo;
case upcase(algo) of
   'EAX256','EAX','HMAC': for k:=0 to authsize-1 do sbuf1[k]:=auth[k];
   'WHIRLPOOL' : for k:=0 to authsize-1 do sbuf1[k]:=WHIRL512Digest[k];
   'SHA512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA512Digest[k];
   'SHA256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA256Digest[k];
   'SHA1' : for k:=0 to authsize-1 do sbuf1[k]:=SHA1Digest[k];
   'RIPEMD160' : for k:=0 to authsize-1 do sbuf1[k]:=RMD160Digest[k];
   'MD5' : for k:=0 to authsize-1 do sbuf1[k]:=MD5Digest[k];
   'CRC64' :
   begin
   dword2bytebuf(crc64.lo32,sbuf1,0);
   dword2bytebuf(crc64.hi32,sbuf1,4);
   end;
   'CRC32' : dword2bytebuf(crc32,sbuf1,0);
   'ADLER32' : dword2bytebuf(adler,sbuf1,0);
   end;
s:='';
for k:=0 to authsize-1 do s:=s+hexstr(@sbuf1[k],1);
num_res:=authsize;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
end;

procedure write_obj_check;
var k:dword;
begin
if upcase(obj_algo)<>'NOALGO' then
   begin
   case upcase(obj_algo) of
      'WHIRLPOOL' : for k:=0 to obj_authsize-1 do sbuf1[k]:=WHIRL512Digest_obj[k];
      'SHA512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA512Digest_obj[k];
      'SHA256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA256Digest_obj[k];
      'SHA1' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA1Digest_obj[k];
      'RIPEMD160' : for k:=0 to obj_authsize-1 do sbuf1[k]:=RMD160Digest_obj[k];
      'MD5' : for k:=0 to obj_authsize-1 do sbuf1[k]:=MD5Digest_obj[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_obj.lo32,sbuf1,0);
      dword2bytebuf(crc64_obj.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32_obj,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler_obj,sbuf1,0);
      end;
   for k:=0 to obj_authsize-1 do obj_tags[i,k]:=sbuf1[k];
   update_control_algo(sbuf1,obj_authsize);
   num_res:=obj_authsize;
   prog_size:=prog_size+num_res;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   end;
end;

procedure first_gui_output;
var i,k:integer;
begin
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelEncrypt2.Caption:='Input: ';
if length(in_param)>4 then k:=4 else k:=length(in_param);
for i:=0 to k-1 do Form_pea.LabelEncrypt2.Caption:=Form_pea.LabelEncrypt2.Caption+in_param[i]+', ';
if length(in_param)>4 then Form_pea.LabelEncrypt2.Caption:=Form_pea.LabelEncrypt2.Caption+' ...';
Form_pea.LabelEncrypt3.Caption:='Output: '+out_param+'.*';
Form_pea.LabelEncrypt4.Caption:='Using: '+compr+'; stream: '+algo+', object(s): '+obj_algo+', volume(s): '+volume_algo;
Form_pea.LabelTime1.Caption:='Creating archive...';
Form_pea.Panel1.visible:=true;
Form_pea.LabelE1.Visible:=true;
end;

procedure evaluate_volumes;
begin
if exp_size>0 then
   begin
   ch_number_expected:=(exp_size div ch_size)+1;
   if (exp_size mod ch_size)=0 then ch_number_expected:=ch_number_expected-1;
   end
else ch_number_expected:=0;
if ch_number_expected>9999 then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Expected '+inttostr(ch_number_expected)+' volumes. It seems a lot! Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate
   else
      if MessageDlg('Up to '+inttostr(ch_number_expected)+' volumes are expected. It seems a lot, even if the selected compression scheme may reduce the actual number. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate;
if ch_number_expected>0 then
   if (upcase(compr)<>'PCOMPRESS0') then
      if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt5.Caption:='Volume number and total output size may vary due to the compressibility of the input; volume size: '+inttostr(ch_size+volume_authsize)+' B'
      else Form_pea.LabelEncrypt5.Caption:='Expected a single volume archive, output size may vary due to the compressibility of the input'
   else
      if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt5.Caption:='Expected '+inttostr(ch_number_expected)+' volume(s) of '+inttostr(ch_size+volume_authsize)+' B for a total output size of '+inttostr(exp_size)+' B'
      else Form_pea.LabelEncrypt5.Caption:='Expected a single volume archive of '+inttostr(exp_size)+' B of size'
else Form_pea.LabelEncrypt5.Caption:='Unknown number of volumes expected';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_param[0];
out_file:=uextractfilename(out_param);
out_path:=uextractfilepath(out_param);
if out_file='' then extractdirname(out_param,out_path,out_file); //first input object is a dir, output is set as a file in the same path of the dir and prefixing dir name as name
if out_path='' then out_path:=executable_path;
if usetcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_file+'.*';
if exp_size>diskfree(0) then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate
   else
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media (however the selected compression scheme may reduce the total space needed for the output). Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate;
end;

procedure do_report_PEA;
var
   h,k:dword;
   s:utf8string;
begin
Form_report.Input.Caption:='Input';
Form_report.Output.Caption:='Output';
Form_report.Caption:='Log PEA';
Form_report.StringGrid1.ColCount:=7;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.Cells[3,0]:='Age';
Form_report.StringGrid1.Cells[4,0]:='Attrib';
Form_report.StringGrid1.Cells[5,0]:='Attrib n.';
Form_report.StringGrid1.Cells[6,0]:=obj_algo;
Form_report.StringGrid1.RowCount:=n_input_files+1;
obj_ok:=0;
for k:=0 to n_input_files-1 do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=in_files[k];
    if status_files[k]=true then Form_report.StringGrid1.Cells[1,k+1]:='Archived' else Form_report.StringGrid1.Cells[1,k+1]:='Skipped';
    if status_files[k]=true then
       begin
       Form_report.StringGrid1.Cells[2,k+1]:=inttostr(fsizes[k]);
       if ftimes[k]<>0 then Form_report.StringGrid1.Cells[3,k+1]:=datetimetostr(filedatetodatetime(ftimes[k]));
       Form_report.StringGrid1.Cells[4,k+1]:=fattr_dec[k];
       Form_report.StringGrid1.Cells[5,k+1]:=inttostr(fattr[k]);
       if upcase(obj_algo)<>'NOALGO' then
          begin
          s:='';
          for h:=0 to obj_authsize-1 do s:=s+hexstr(@obj_tags[k,h],1);
          Form_report.StringGrid1.Cells[6,k+1]:=s;
          end;
       inc(obj_ok,1);
       end;
    end;
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=2;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:=volume_algo;
Form_report.StringGrid2.RowCount:=j+1;
for k:=0 to j-1 do
    begin
    update_pea_filename(out_path+out_name,k+1,s);
    Form_report.StringGrid2.Cells[0,k+1]:=s;
    if upcase(volume_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[1,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
//operation parameters
Form_report.Label1.Caption:=Form_pea.LabelEncrypt4.Caption;
//input
Form_report.Label2.Caption:='Archived '+inttostr(obj_ok)+' objects ('+inttostr(n_dirs)+' dirs, '+inttostr(obj_ok-n_dirs)+' files) of '+inttostr(n_input_files)+' ('+inttostr(n_input_files-obj_ok)+' not found); input '+inttostr(in_size)+' B';
//output
Form_report.Label3.Caption:=Form_pea.LabelEncrypt6.Caption;
//ouput name
Form_report.Label4.Caption:=Form_pea.LabelEncrypt3.Caption;
end;

procedure last_gui_output;
begin
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_name+'.*; tag: '+s;
if compr<>'PCOMPRESS0' then out_size:=prog_compsize
else out_size:=prog_size;
if ch_size<>1024*1024*1024*1024*1024-volume_authsize then Form_pea.LabelEncrypt6.Caption:=inttostr(j)+' volume(s), '+inttostr(ch_size+volume_authsize)+' B; total '+inttostr(out_size)+' B'
else Form_pea.LabelEncrypt6.Caption:='Single volume, '+inttostr(out_size)+' B';
if compr<>'PCOMPRESS0' then if in_size<>0 then Form_pea.LabelEncrypt6.Caption:=Form_pea.LabelEncrypt6.Caption+', '+inttostr((out_size * 100) div (in_size+1))+'% of input';
do_report_PEA;
Form_pea.LabelEncrypt5.Caption:=Form_report.Label2.Caption;
Form_pea.LabelOut1.Caption:=inttostr((out_size * 100) div (in_size+1))+'% of input size';
if ((out_size * 200) div (in_size+1))<16 then Form_pea.ShapeE2.Width:=16
else
   if ((out_size * 200) div (in_size+1))>300 then Form_pea.ShapeE2.Width:=300
   else Form_pea.ShapeE2.Width:=(out_size * 200) div (in_size+1);
end;

//clean keying-related variables
procedure clean_keying_vars;
var
   k:integer;
begin
for k:=0 to SBUFSIZE do sbuf2[k]:=0;
pw:='';
password:='';
keyfile_name:='';
keyf_name:='';
pw_len:=0;
k:=0;
end;

begin
clean_variables;
get_fingerprint (fingerprint,false);
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.Caption:='Pea';
Form_pea.PanelDecrypt1.visible:=false;
Form_pea.PanelEncrypt1.visible:=true;
ts_start:=datetimetotimestamp(now);
i:=0;
//give preliminary information on work status to the GUI
first_gui_output;
{
expand the list of input objects and evaluate input and expected uncompressed
output size (taking overheads in account):
if the object is a file add file name to the list;
if the object is a dir, recursively add the content to the list (any object in
the dir and all subdir will be added to the list; if you want only file sin the
root dir to be added to the list, add them as files, don't add the dir);
if the object is not found mark it as skipped in the status list, otherwise mark
it as ok (the different lists indexes must remain sincronized)
}
expand_inputlist;
cent_size:=(exp_size div 100)+1; //1% of expected output size, used for progress indication
{
evaluate volumes number;
at 9999 objects the program will warn and proceed only after user's permission,
however the program has no sort of problem until 999999 chunks (but the host
system may!)
}
evaluate_volumes;
{
get output path and name;
evaluate if the path has enough free space for expected output.
}
evaluate_output;
//check if output path has room for a chunk of given size (mandatory)
checkspacepea(out_path,ch_size,volume_authsize);
{
start the actual operation routine
1) generate the archive header;
2a) generate the stream header (current implementation allow only a stream for archive)
2b) if using AE as stream check algorithm, initialize the encryption and generate additional header data needed for the encryption (similar to FCA file header);
2c) if compression is used write compressed buffer's size
3) add objects to archive; if the object is a non-empty file write the data to the archive, syncronously doing optional compression and control at stream, object and volume level; write object level control tag at the end of each object
4) generate End Of Archive trigger followed by the appropriate control tag
5) write the optional volume control tag at the end of each volume (starting from an appropriate position before volume end, due to the tag size required)
}
//1) generate archive header
out_name:=out_file;
if ch_size=1024*1024*1024*1024*1024-volume_authsize then
   uassignfile(f_out,out_file+'.pea')
else
   uassignfile(f_out,out_file+'.000001.pea');//current dir was jet set to out_path
rewrite(f_out);
if IOResult<>0 then internal_error('IO error opening first volume');
SetLength(volume_tags,length(volume_tags)+1);
init_volume_control_algo;
test_pea_error('creating archive header',pea_archive_hdr(volume_algo,sbuf1,num_res));
j:=1;
ch_res:=ch_size;
prog_size:=num_res;
prog_compsize:=num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to 9 do auth_buf[i]:=sbuf1[i];
//2a) generate stream header
test_pea_error('creating stream header',pea_stream_hdr(compr,algo,obj_algo,sbuf1,num_res));
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
for i:=0 to 9 do auth_buf[i+10]:=sbuf1[i];
// 2b) init stream control algorithm, generate crypto subheader if needed
if pwneeded=true then
   begin
   //get password
   if (upcase(pw_param)='INTERACTIVE') or (upcase(pw_param)='INTERACTIVE_REPORT') then
      begin
      //password is pw string that was already entered in EditPW.Text
      //keyfile name is keyfile_name already entered
      end
   else
      begin
      pw:=password; //pw is got from commandline (not recommended)
      keyfile_name:=keyf_name; //keyfile name is got from command line
      end;
   pw_len:=length(pw);
   if pw_len=0 then internal_error('invalid password length');
   for k:=0 to pw_len-1 do sbuf2[k]:=ord(pw[k+1]);//copy password into an array of byte
   //append headers to password's array (sbuf2)
   for i:=0 to 1 do auth_buf[i+20]:=sbuf1[i];
   for k:=0 to 21 do sbuf2[pw_len+k]:=auth_buf[k];
   pw_len:=pw_len+22;
   //append keyfile to password's array (sbuf2)
   if upcase(keyfile_name)<>'NOKEYFILE' then
      test_pea_error('accessing keyfile',use_keyfile(keyfile_name,2048,numread,sbuf2,pw_len));
   end;
init_control_algo;
clean_keying_vars;
prog_size:=prog_size+num_res;
prog_compsize:=prog_compsize+num_res;
write2chunks ( num_res,
               sbuf1,
               f_out,
               out_path,out_name,
               j,
               ch_size,
               ch_res);
if pwneeded=false then update_control_algo(auth_buf,20); //check the archive and stream headers
// 2c) buffer size field (data to compress at once), dword, stream specific
if upcase(compr)<>'PCOMPRESS0' then
   begin
   dword2bytebuf(WBUFSIZE,sbuf1,0);
   update_control_algo(sbuf1,4);
   num_res:=4;
   prog_compsize:=prog_compsize+num_res;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   end;
//3) for each object: if the object is accessible add it to the archive
n_dirs:=0;
for i:=0 to n_input_files-1 do
   begin
   SetLength(obj_tags,length(obj_tags)+1);
   if status_files[i]=false then goto 1; //the object, during creation of the list, was not accessible
   in_qualified_name:=in_files[i];
   addr:=i;
   k:=check_in(f_in,in_qualified_name,status_files,i);
   if k<>0 then
      begin
      inc(n_skipped,1);
      goto 1; //the object is actually not accessible
      end;
   init_obj_control_algo;
   //2 byte (word) sized field for size of the input object qualified name, if = 0 then the object is a trigger
   ansi_qualified_name:=utf8toansi(in_qualified_name);
   filename_size:=length(ansi_qualified_name);//(in_files[i]);
   word2bytebuf(filename_size,sbuf1,0);
   //variable sized field for input object qualified name
   for k:=0 to filename_size-1 do sbuf1[k+2]:=ord(ansi_qualified_name[k+1]);
   //4 byte (dword) sized field for input object last modification time
   if ufilegetattr(in_files[i]) and faDirectory = 0 then k:=ufileage(in_qualified_name)
   else
      begin
      if uFindFirst(in_files[i]+'.',faDirectory,r) = 0 then k:=r.Time
      else k:=datetimetofiledate(now); //should not happen
      FindClose(r);
      end;
   dword2bytebuf(k,sbuf1,filename_size+2);
   //4 byte (dword) sized field for input object attributes
   k:=ufilegetattr(in_qualified_name);
   dword2bytebuf(k,sbuf1,filename_size+6);
   if ufilegetattr(in_qualified_name) and faDirectory <>0 then //the object is a directory
      begin
      update_obj_control_algo(sbuf1,filename_size+10);
      update_control_algo(sbuf1,filename_size+10);
      num_res:=filename_size+10;
      prog_size:=prog_size+num_res;
      prog_compsize:=prog_compsize+num_res;
      inc(n_dirs,1);
      write2chunks ( num_res,
                     sbuf1,
                     f_out,
                     out_path,out_name,
                     j,
                     ch_size,
                     ch_res);
      finish_obj_control_algo;
      write_obj_check;
      end
   else //the object is a file
      begin
      //8 byte (qword) sized field for input file size
      srcfilesize(in_qualified_name,file_size);
      //file_size:=system.filesize(f_in);
      qword2bytebuf(file_size,sbuf1,filename_size+10);
      update_obj_control_algo(sbuf1,filename_size+18);
      update_control_algo(sbuf1,filename_size+18);
      num_res:=filename_size+18;
      prog_size:=prog_size+num_res;
      prog_compsize:=prog_compsize+num_res;
      write2chunks ( num_res,
                     sbuf1,
                     f_out,
                     out_path,out_name,
                     j,
                     ch_size,
                     ch_res);
      if file_size>0 then //non empty file
         begin
         ////// for each file: 3) mangle and write file data
         total:=0;
         numread:=1;
         if upcase(compr)<>'PCOMPRESS0' then compress_file
         else nocompress_file; //no compression
         closefile(f_in);
         end;
      finish_obj_control_algo;
      write_obj_check;
      end;
   1:
   end;
//4) close stream: write trigger of end of archive (since PEA1.0 files contain a single stream) and write authentication tag (if applicable)
write_eoa;
if upcase(algo)<>'NOALGO' then write_auth
else s:='no control tag';
//5) generate last volume control tag
SetLength(volume_tags,length(volume_tags)+1);
finish_volume_control_algo;
write_volume_check;
closefile(f_out);
if IOResult<>0 then internal_error('IO error closing last volume');
//give final job information to the GUI
last_gui_output;
//calculate operation time
timing(ts_start,in_size);
//make accessible exit button and link to the detailed job log
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log PEA');
Sleep(500);
if closepolicy>0 then Form_pea.Close; //error conditions are intercepted before and handled with internal_error procedure
end;

{
UnPEA
Decrypt, authenticate, join, decompress, extract PEA format archives

Error management:
- errors in objects, stream or volumes are checked by strong functions and
  reported in job log, that can be saved, at the end of the job a popup message
  will warn that such errors were encountered;
- errors that prevent the application to work make the application quit with a
  descriptive message, if the error is of unknown nature application will
  autosave a job log allowing further analysis.

Known issues:
- FPC's set object attributes works only on Windows, set object date seem not
actually working (both are currently not supported on *x);
}

procedure unpea;
var
   in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name:utf8string;
   i:integer;

procedure parse_unpea_cl;
begin
i:=0;
try
   in_qualified_name:=envtoutf8(paramstr(2));
   if not(ufileexists(in_qualified_name)) then
      internal_error('"'+in_qualified_name+'" not exist');
   out_param:=envtoutf8(paramstr(3));
   date_param:=upcase(paramstr(4)); //how to use file age information: SETDATE (not supported on *x) set the output file date to the input file date, RESETDATE gives new file age
   if date_param<>'RESETDATE' then  //(date_param<>'SETDATE') or
      internal_error('"'+date_param+'" is not a valid parameter for file age metadata: RESETDATE (gives new file age) is actually the only option featured by the program');
   attr_param:=upcase(paramstr(5)); //like the previous, about attribute data: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output object attribute as they are by default for the target system and position
   if not ((attr_param='SETATTR') or (attr_param='RESETATTR')) then
      internal_error('"'+attr_param+'" is not a valid parameter for file attributes metadata: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output objects attributes as they are by default for the target system and position');
   struct_param:=upcase(paramstr(6)); //EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures
   if struct_param<>'EXTRACT2DIR' then
      internal_error('"'+struct_param+'" is not a valid parameter for output structure, the only parameter supported is EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures');
   //get operation mode
   pw_param:=upcase(paramstr(7));
   if (pw_param<>'INTERACTIVE') and (pw_param<>'INTERACTIVE_REPORT') then
      begin
      inc(i,1);
      password:=envtoutf8(paramstr(7+i));
      inc(i,1);
      keyf_name:=envtoutf8(paramstr(7+i));
      end
   else if (pw_param<>'INTERACTIVE') and (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'INTERACTIVE_REPORT') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter, please refer to the documentation');
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_unpea_cl; //parse and validate command line
unpea_procedure(in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name);
end;

procedure unpea_lib_procedure ( in_qualified_name,                              //archive qualified name
                                out_param,                                      //dir were extracting the archive (or AUTONAME)
                                date_param,                                     //actually only supported RESETDATE, reset date of extracted files
                                attr_param,                                     //RESETATTR (or SETATTR only on Windows to set object's attributes as on original ojects)
                                struct_param,                                   //actually only supported EXTRACT2DIR, create a dir and extract archive in the dir using shortest paths for archived objects
                                password,keyf_name:utf8string;                  //password and keyfile qualified name (if needed)
                                opmode:utf8string);                             //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:utf8string;
begin
if not(ufileexists(in_qualified_name)) then
   internal_error('"'+in_qualified_name+'" not exist');
//how to use file age information: SETDATE (not supported on *x) set the output file date to the input file date, RESETDATE gives new file age
if date_param<>'RESETDATE' then  //(date_param<>'SETDATE') or
   internal_error('"'+date_param+'" is not a valid parameter for file age metadata: RESETDATE (gives new file age) is actually the only option featured by the program');
//like the previous, about attribute data: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output object attribute as they are by default for the target system and position
if not ((attr_param='SETATTR') or (attr_param='RESETATTR')) then
   internal_error('"'+attr_param+'" is not a valid parameter for file attributes metadata: SETATTR (not supported on *x) set the output objects attributes as saved in the archive; RESETATTR set the output objects attributes as they are by default for the target system and position');
//EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures
if struct_param<>'EXTRACT2DIR' then
   internal_error('"'+struct_param+'" is not a valid parameter for output structure, the only parameter supported is EXTRACT2DIR: make a dir with output object with shortest possible path derived from input structures');
//get operation mode
if (upcase(opmode)<>'INTERACTIVE') and (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'INTERACTIVE_REPORT') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode, please refer to the documentation');
if (upcase(opmode)='INTERACTIVE') or (upcase(opmode)='INTERACTIVE_REPORT') then
   internal_error('INTERACTIVE* modes are not allowed calling unpea_lib_procedure, use BATCH* or HIDDEN* modes');
pw_param:=upcase(opmode);
unpea_procedure(in_qualified_name,out_param,date_param,attr_param,struct_param,pw_param,password,keyf_name);
end;

procedure unpea_procedure ( in_qualified_name,
                            out_param,
                            date_param,
                            attr_param,
                            struct_param,
                            pw_param,
                            password,
                            keyf_name:utf8string);
var
   hdr,hdrd : TFCAHdr;
   hdr256,hdrd256 : TFCA256Hdr;
   cxe : TAES_EAXContext;
   cxh : TFCA_HMAC_Context;
   auth : array [0..15] of byte;//TFCA_AuthBlock;
   HashContext,HashContext_obj,HashContext_volume: THashContext;
   Whirl512Digest,Whirl512Digest_obj,Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest,SHA512Digest_obj,SHA512Digest_volume: TSHA512Digest;
   SHA256Digest,SHA256Digest_obj,SHA256Digest_volume: TSHA256Digest;
   SHA1Digest,SHA1Digest_obj,SHA1Digest_volume: TSHA1Digest;
   RMD160Digest,RMD160Digest_obj,RMD160Digest_volume: TRMD160Digest;
   MD5Digest,MD5Digest_obj,MD5Digest_volume: TMD5Digest;
   crc64,crc64_obj,crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out:file of byte;
   sbuf1,sbuf2:array [0..65535] of byte;
   tagbuf,exp_auth:array [0..63] of byte;
   compr_level,headersize,authsize,obj_authsize,volume_authsize,archive_datetimeencoding:byte;
   pw_len,fns:word;
   adler,crc32,adler_obj,crc32_obj,adler_volume,crc32_volume:longint;
   i,j,ci,cj,h,k,numread,numwritten,n_chunks,n_dirs,n_input_files,compsize,uncompsize,addr,fage,fattrib,buf_size:dword;
   total,wrk_space,exp_space,cent_size,fs,nobj,out_size,qw0,qw1,qw2,qw3,qw4,qw5,qw6,qw7:qword;
   stream_error,obj_error,volume_error,end_of_archive,pwneeded,chunks_ok,filenamed,out_created,no_more_files,readingstream,readingheader,readingfns,readingtrigger,readingfn,readingfs,readingfage,readingfattrib,readingcompsize,fassigned,readingf,readingcompblock,readingobjauth,readingauth,singlevolume:boolean;
   subroot,basedir,s,in_file,in_name,in_folder,out_path,out_file,algo,obj_algo,volume_algo,compr,fn:utf8string;
label 1;

procedure clean_variables;
begin
i:=0;
j:=0;
h:=0;
k:=0;
numread:=0;
numwritten:=0;
n_chunks:=0;
n_dirs:=0;
n_input_files:=0;
compsize:=0;
uncompsize:=0;
addr:=0;
fage:=0;
fattrib:=0;
total:=0;
cent_size:=0;
wrk_space:=0;
exp_space:=0;
fs:=0;
nobj:=0;
out_size:=0;
clean_global_vars;
end;

procedure evaluate_archive_size(var exp_space:qword; var cent_size:qword); //succeed if all chunks are accessible
var qw:qword;
begin
j:=1;
no_more_files:=false;
exp_space:=0;
while no_more_files=false do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file)
   else no_more_files:=true;
   if ufileexists(in_folder+in_file) then
      begin
      uassignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      srcfilesize(in_folder+in_file,qw);
      exp_space:=exp_space+qw;
      //exp_space:=exp_space+system.filesize(f_in);
      closefile(f_in);
      j:=j+1;
      end
   else no_more_files:=true;
   end;
n_chunks:=j-1;
cent_size:=(exp_space div 100)+1;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, expected '+inttostr(n_chunks)+' volume(s), total '+inttostr(exp_space)+' B';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_folder+in_name;
out_file:=uextractfilename(out_param);
out_path:=uextractfilepath(out_param);
if out_file='' then out_file:=in_name; //if no output name is explicitely given, the output name is assumed to be the name of the first input file
if out_path='' then out_path:=in_folder; //if no output path is explicitely given, the output path is assumed to be the path of the first input file
if out_path='' then out_path:=executable_path;
if usetcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
if exp_space>diskfree(0) then
   if (upcase(compr)='PCOMPRESS0') then
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate
   else
      if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media (however the selected compression scheme may reduce the total space needed for the output). Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
      else Application.Terminate;
end;

procedure ansiextract2dir;
var
   afn,fnpath,fnname,aout_path,aout_file:ansistring;
begin
afn:=fn;
aout_path:=utf8toansi(out_path);
aout_file:=utf8toansi(out_file);
if afn[length(afn)]=DirectorySeparator then
   begin
   ansiextractdirname(afn,fnpath,fnname);
   if subroot='' then
      begin
      subroot:=fnpath;
      basedir:=afn;
      end;
   if ansicontainsstr(fnpath,basedir) then
      begin
      s:=copy(fnpath,length(subroot)+1,length(fnpath)-length(subroot)-1);
      end
   else
      begin
      subroot:=fnpath;
      basedir:=afn;
      s:='';
      end;
   try
      if s<>'' then mkdir(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname)
      else mkdir(aout_path+aout_file+directoryseparator+fnname);
   except
      if IOResult<>0 then internal_error('IO error creating dir '+ansitoutf8(fnname));
   end;
   {$IFDEF MSWINDOWS}
   if attr_param='SETATTR' then filesetattr(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fattrib);
   filesetdate(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fage);
   {$ENDIF}
   readingfns:=true;
   end
else
   begin
   fnname:=extractfilename(afn);
   fnpath:=extractfilepath(afn);
   if subroot='' then
      begin
      subroot:=fnpath;
      s:='';
      end
   else s:=copy(fnpath,length(subroot)+1,length(fnpath)-length(subroot)-1);
      if setcurrentdir(aout_path+aout_file+directoryseparator+s)<>true then s:='';
      h:=0;
      filenamed:=false;
      repeat
         if h=0 then
            if fileexists(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname) then inc(h,1)
            else filenamed:=true
         else
            if fileexists(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname+'-'+inttostr(h)+extractfileext(afn)) then inc(h,1)
            else filenamed:=true;
      until filenamed = true;
      if h>0 then fnname:=fnname+'-'+inttostr(h)+extractfileext(afn);
      assignfile(f_out,aout_path+aout_file+directoryseparator+s+directoryseparator+fnname);
      setcurrentdir(aout_path+aout_file);
      rewrite(f_out);
      if IOResult<>0 then internal_error('IO error creating '+ansitoutf8(fnname));
      {$IFDEF MSWINDOWS}
      if attr_param='SETATTR' then filesetattr(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fattrib);
      //filesetdate(aout_path+aout_file+directoryseparator+s+directoryseparator+fnname,fage); fails
      {$ENDIF}
      readingfs:=true;
      fassigned:=true;
      end;
end;

procedure init_AE256_control_algo;
begin
hdr256.FCAsig:=hdr.FCAsig;
hdr256.Flags:=hdr.Flags;
hdr256.Salt:=hdr.Salt;
hdr256.PW_Ver:=hdr.PW_Ver;
hdrd256:=hdr256;
if upcase(algo)='EAX256' then if FCA_EAX256_init(cxe, @sbuf2, pw_len, hdrd256)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if hdr256.PW_ver<>hdrd256.PW_ver then internal_error('Wrong password or keyfile');
end;

procedure init_AE128_control_algo;
begin
hdrd:=hdr;
if upcase(algo)='EAX' then if FCA_EAX_init(cxe, @sbuf2, pw_len, hdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if upcase(algo)='HMAC' then if FCA_HMAC_init(cxh, @sbuf2, pw_len, hdrd)<>0 then internal_error('Stream control algorithm: error in '+algo+' init');
if hdr.PW_ver<>hdrd.PW_ver then internal_error('Wrong password or keyfile');
end;

procedure init_nonAE_control_algo;
begin
case upcase(algo) of
'WHIRLPOOL' : Whirl_Init(HashContext);
'SHA512' : SHA512Init(HashContext);
'SHA256' : SHA256Init(HashContext);
'SHA1' : SHA1Init(HashContext);
'RIPEMD160' : RMD160Init(HashContext);
'MD5' : MD5Init(HashContext);
'CRC64' : CRC64Init(crc64);
'CRC32' : CRC32Init(crc32);
'ADLER32' : Adler32Init(adler);
end;
end;

procedure init_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_obj);
'SHA512' : SHA512Init(HashContext_obj);
'SHA256' : SHA256Init(HashContext_obj);
'SHA1' : SHA1Init(HashContext_obj);
'RIPEMD160' : RMD160Init(HashContext_obj);
'MD5' : MD5Init(HashContext_obj);
'CRC64' : CRC64Init(crc64_obj);
'CRC32' : CRC32Init(crc32_obj);
'ADLER32' : Adler32Init(adler_obj);
end;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_control_algo(var buf:array of byte; size:word);
begin
case upcase(algo) of
'EAX256' : if FCA_EAX256_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'EAX' : if FCA_EAX_decrypt(cxe, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'HMAC' : if FCA_HMAC_decrypt(cxh, buf, size)<>0 then internal_error('Stream control algorithm: error in '+algo+' update');
'WHIRLPOOL' : Whirl_Update(HashContext, @buf, size);
'SHA512' : SHA512Update(HashContext, @buf, size);
'SHA256' : SHA256Update(HashContext, @buf, size);
'SHA1' : SHA1Update(HashContext, @buf, size);
'RIPEMD160' : RMD160Update(HashContext, @buf, size);
'MD5' : MD5Update(HashContext, @buf, size);
'CRC64' : CRC64Update(crc64, @buf, size);
'CRC32' : CRC32Update(crc32, @buf, size);
'ADLER32' : Adler32Update(adler, @buf, size);
end;
end;

procedure update_obj_control_algo(buf:array of byte; size:word);
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_obj, @buf, size);
'SHA512' : SHA512Update(HashContext_obj, @buf, size);
'SHA256' : SHA256Update(HashContext_obj, @buf, size);
'SHA1' : SHA1Update(HashContext_obj, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_obj, @buf, size);
'MD5' : MD5Update(HashContext_obj, @buf, size);
'CRC64' : CRC64Update(crc64_obj, @buf, size);
'CRC32' : CRC32Update(crc32_obj, @buf, size);
'ADLER32' : Adler32Update(adler_obj, @buf, size);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_control_algo;
begin
case upcase(algo) of
'EAX256' : FCA_EAX256_final(cxe, auth);
'EAX' : FCA_EAX_final(cxe, auth);
'HMAC' : FCA_HMAC_final(cxh, auth);
'WHIRLPOOL' : Whirl_Final(HashContext,WHIRL512Digest);
'SHA512' : SHA512Final(HashContext,SHA512Digest);
'SHA256' : SHA256Final(HashContext,SHA256Digest);
'SHA1' : SHA1Final(HashContext,SHA1Digest);
'RIPEMD160' : RMD160Final(HashContext,RMD160Digest);
'MD5' : MD5Final(HashContext,MD5Digest);
'CRC64' : CRC64Final(crc64);
'CRC32' : CRC32Final(crc32);
'ADLER32' : Adler32Final(adler);
end;
end;

procedure finish_obj_control_algo;
begin
case upcase(obj_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_obj,WHIRL512Digest_obj);
'SHA512' : SHA512Final(HashContext_obj,SHA512Digest_obj);
'SHA256' : SHA256Final(HashContext_obj,SHA256Digest_obj);
'SHA1' : SHA1Final(HashContext_obj,SHA1Digest_obj);
'RIPEMD160' : RMD160Final(HashContext_obj,RMD160Digest_obj);
'MD5' : MD5Final(HashContext_obj,MD5Digest_obj);
'CRC64' : CRC64Final(crc64_obj);
'CRC32' : CRC32Final(crc32_obj);
'ADLER32' : Adler32Final(adler_obj);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure authenticate_stream;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(algo)<>'NOALGO' then
   begin
   for k:=0 to authsize-1 do exp_auth[k]:=sbuf1[k];
   case upcase(algo) of
      'EAX256','EAX','HMAC' : for k:=0 to authsize-1 do sbuf1[k]:=auth[k];
      'WHIRLPOOL' : for k:=0 to authsize-1 do sbuf1[k]:=WHIRL512Digest[k];
      'SHA512' : for k:=0 to authsize-1 do sbuf1[k]:=SHA512Digest[k];
      'SHA256' : for k:=0 to authsize-1 do sbuf1[k]:=SHA256Digest[k];
      'SHA1' : for k:=0 to authsize-1 do sbuf1[k]:=SHA1Digest[k];
      'RIPEMD160' : for k:=0 to authsize-1 do sbuf1[k]:=RMD160Digest[k];
      'MD5' : for k:=0 to authsize-1 do sbuf1[k]:=MD5Digest[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64.lo32,sbuf1,0);
      dword2bytebuf(crc64.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler,sbuf1,0);
      end;
   tag_match:=true;
   for k:=0 to authsize-1 do if sbuf1[k]<>exp_auth[k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=false then
      begin
      Form_pea.LabelDecrypt5.Caption:='The archive''s stream of data seem corrupted or tampered! You should not trust the stream''s content!';
      stream_error:=true;
      end
   else
      begin
      s:='';
      for k:=0 to authsize-1 do s:=s+hexstr(@sbuf1[k],1);
      if (upcase(algo)='EAX256') or (upcase(algo)='EAX') or (upcase(algo)='HMAC') then Form_pea.LabelDecrypt5.Caption:='Archive''s stream correctly authenticated, tag: '+s
      else Form_pea.LabelDecrypt5.Caption:='Archive''s stream correctly verified';
      end;
   end;
end;

procedure check_obj;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(obj_algo)<>'NOALGO' then
   begin
   for k:=0 to obj_authsize-1 do exp_obj_tags[nobj,k]:=sbuf1[k];
   case upcase(obj_algo) of
      'WHIRLPOOL' : for k:=0 to obj_authsize-1 do sbuf1[k]:=WHIRL512Digest_obj[k];
      'SHA512' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA512Digest_obj[k];
      'SHA256' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA256Digest_obj[k];
      'SHA1' : for k:=0 to obj_authsize-1 do sbuf1[k]:=SHA1Digest_obj[k];
      'RIPEMD160' : for k:=0 to obj_authsize-1 do sbuf1[k]:=RMD160Digest_obj[k];
      'MD5' : for k:=0 to obj_authsize-1 do sbuf1[k]:=MD5Digest_obj[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_obj.lo32,sbuf1,0);
      dword2bytebuf(crc64_obj.hi32,sbuf1,4);
      end;
      'CRC32' : dword2bytebuf(crc32_obj,sbuf1,0);
      'ADLER32' : dword2bytebuf(adler_obj,sbuf1,0);
      end;
   for k:=0 to obj_authsize-1 do obj_tags[nobj,k]:=sbuf1[k];
   tag_match:=true;
   for k:=0 to obj_authsize-1 do if obj_tags[nobj,k]<>exp_obj_tags[nobj,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_objects[nobj]:='Object is OK'
   else
      begin
      status_objects[nobj]:='Wrong tag!';
      obj_error:=true;
      end;
   end;
end;

procedure check_volume;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   for k:=0 to volume_authsize-1 do exp_volume_tags[j-1,k]:=tagbuf[k];
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do tagbuf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA1Digest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do tagbuf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do tagbuf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,tagbuf,0);
      dword2bytebuf(crc64_volume.hi32,tagbuf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,tagbuf,0);
      'ADLER32' : dword2bytebuf(adler_volume,tagbuf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=tagbuf[k];
   tag_match:=true;
   for k:=0 to volume_authsize-1 do if volume_tags[j-1,k]<>exp_volume_tags[j-1,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_volumes[j-1]:='Volume is OK'
   else
      begin
      status_volumes[j-1]:='Wrong tag!';
      volume_error:=true;
      end;
   end;
end;

procedure do_report_unPEA;
var
   h,k,obj_ok:dword;
   s:utf8string;
   system_datetimeencoding:byte;
begin
get_system_datetimeencoding(system_datetimeencoding);
Form_report.Input.Caption:='Objects';
Form_report.Output.Caption:='Volumes';
Form_report.Caption:='Log UnPEA';
Form_report.StringGrid1.ColCount:=8;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.Cells[3,0]:='Age';
Form_report.StringGrid1.Cells[4,0]:='Attrib';
Form_report.StringGrid1.Cells[5,0]:='Attrib n.';
Form_report.StringGrid1.Cells[6,0]:='calculated ('+obj_algo+')';
Form_report.StringGrid1.Cells[7,0]:='found';
Form_report.StringGrid1.RowCount:=nobj+2;
obj_ok:=0;
for k:=0 to nobj do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=ansitoutf8(in_files[k]);
    Form_report.StringGrid1.Cells[1,k+1]:=status_objects[k];
    Form_report.StringGrid1.Cells[2,k+1]:=inttostr(fsizes[k]);
    if system_datetimeencoding=archive_datetimeencoding then
       begin
       try
          if ftimes[k]<>0 then Form_report.StringGrid1.Cells[3,k+1]:=datetimetostr(filedatetodatetime(ftimes[k]));
       except
          Form_report.StringGrid1.Cells[3,k+1]:='Non valid DateTime';
       end;
       end
    else Form_report.StringGrid1.Cells[3,k+1]:='DateTime conversion not available';
    Form_report.StringGrid1.Cells[4,k+1]:=fattr_dec[k];
    Form_report.StringGrid1.Cells[5,k+1]:=inttostr(fattr[k]);
    if upcase(obj_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to obj_authsize-1 do s:=s+hexstr(@obj_tags[k,h],1);
       Form_report.StringGrid1.Cells[6,k+1]:=s;
       s:='';
       for h:=0 to obj_authsize-1 do s:=s+hexstr(@exp_obj_tags[k,h],1);
       Form_report.StringGrid1.Cells[7,k+1]:=s;
       end;
    inc(obj_ok,1);
    end;
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=4;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:='Status';
Form_report.StringGrid2.Cells[2,0]:='calculated ('+volume_algo+')';
Form_report.StringGrid2.Cells[3,0]:='found';
Form_report.StringGrid2.RowCount:=j;
for k:=0 to j-2 do
    begin
    if singlevolume=false then update_pea_filename((in_name),k+1,s)
    else s:=(in_name);
    Form_report.StringGrid2.Cells[0,k+1]:=s;
    if upcase(volume_algo)<>'NOALGO' then
       begin
       Form_report.StringGrid2.Cells[1,k+1]:=status_volumes[k];
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[2,k+1]:=s;
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@exp_volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[3,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
Form_report.Label1.Caption:=in_folder+in_name+'.* -> '+out_path+out_file+DirectorySeparator;
Form_report.Label2.Caption:=Form_pea.LabelDecrypt4.Caption;
Form_report.Label3.Caption:='Input: '+inttostr(j-1)+' volume(s), '+inttostr(wrk_space)+' B -> Extracted '+inttostr(obj_ok)+' objects ('+inttostr(n_dirs)+' dirs, '+inttostr(obj_ok-n_dirs)+' files) of '+inttostr(n_input_files)+' ('+inttostr(n_input_files-obj_ok)+' not extracted); total output: '+inttostr(out_size)+' B';
Form_report.Label4.Caption:=Form_pea.LabelDecrypt5.Caption+' '+Form_pea.LabelDecrypt6.Caption
end;

//clean keying-related variables
procedure clean_keying_vars;
var
   k:integer;
begin
for k:=0 to pw_len-1 do sbuf2[k]:=0;
pw:='';
password:='';
keyfile_name:='';
keyf_name:='';
pw_len:=0;
k:=0;
end;

function report_errors:integer;
var
   s:utf8string;
begin
result:=0;
if (stream_error=false) and (obj_error=false) and (volume_error=false) then exit;
result:=-1;
s:='Error(s) found in: ';
if stream_error=true then s:=s+'stream; ';
if obj_error=true then s:=s+'object(s); ';
if volume_error=true then s:=s+'volume(s); ';
s:=s+'please check job log!';
showmessage(s);
end;

begin
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=true;
Form_pea.PanelEncrypt1.visible:=false;
Form_pea.Caption:='UnPea';
ts_start:=datetimetotimestamp(now);
stream_error:=false;
obj_error:=false;
volume_error:=false;
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_qualified_name;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_param;
Form_pea.LabelTime1.Caption:='Opening archive...';
in_folder:=uextractfilepath(in_qualified_name);
if in_folder='' then in_folder:=executable_path;
in_file:=uextractfilename(in_qualified_name);
if upcase(copy(in_qualified_name,length(in_qualified_name)-10,11))<>'.000001.PEA' then
   begin
   singlevolume:=true;
   end
else
   begin
   singlevolume:=false;
   delete(in_file,length(in_file)-10,11);
   end;
in_name:=in_file;
//try to evaluate archive size (succeed if all chunks are accessible)
evaluate_archive_size(exp_space,cent_size);
//check output name and path
evaluate_output;
//try to check if the path has enough room for the output (formerly guessed archive size is used, actual output size is unknown unless all data is extracted and all headers are parsed)
usetcurrentdir(uextractfilepath(out_param));
if exp_space>diskfree(0) then
   if MessageDlg('Output path '+uextractfilepath(out_param)+' seems to not have enough free space. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else Application.Terminate;
{blockread 10 byte archive header; since volume tag size is unknown to UnPEA,
PEA set first volume size mandatory at least 10 byte (plus volume tag) in order
to make UnPEA able to blockread the archive header and calculate the volume tag
size}
uassignfile(f_in,in_qualified_name);
filemode:=0;
reset(f_in);
blockread (f_in,sbuf1,10,numread);
if IOResult<>0 then internal_error('IO error reading from '+in_qualified_name);
close(f_in);
test_pea_error('parsing archive header',pea_parse_archive_header(sbuf1,volume_algo,archive_datetimeencoding));
decode_volume_control_algo (volume_algo,volume_authsize);
//read 10 byte archive header plus 10 byte stream header plus other 16 byte crypto subheader (if AE is used) plus 4 byte for compression buffer size (if compression is used)
read_from_chunks ( in_folder,in_name,
                   40,
                   sbuf1,sbuf2,
                   volume_authsize,
                   40,
                   singlevolume);
for i:=0 to 22 do tagbuf[i]:=sbuf1[i]; //write plaintext header
for i:=0 to 29 do sbuf1[i]:=sbuf1[i+10]; //discard 10 byte of archive header
test_pea_error('parsing stream header',pea_parse_stream_header(sbuf1, compr, compr_level, algo, obj_algo));
decode_control_algo ( algo,
                      headersize,
                      authsize,
                      pwneeded);
if compr<>'PCOMPRESS0' then headersize:=headersize+14//stream header size + 10 (archive header size) + 4 (compression buffer field size, if compression is used)
else headersize:=headersize+10;
decode_obj_control_algo (obj_algo,obj_authsize);
for i:=0 to 19 do sbuf1[i]:=sbuf1[i+10]; //discard 10 bytes of stream header
if pwneeded=true then //initialize AE (appending headers to password)
   begin
   //read AE header
   test_pea_error('parsing crypto subheader',pea_parse_crypto_subheader(sbuf1,hdr));
   if (upcase(pw_param)='INTERACTIVE') or (upcase(pw_param)='INTERACTIVE_REPORT') then
      begin
      //password is pw string that was already entered in EditPW.Text
      //keyfile name is keyfile_name already entered
      end
   else
      begin
      pw:=password; //pw is got from commandline (not recommended)
      keyfile_name:=keyf_name; //keyfile name is got from command line
      end;
   pw_len:=length(pw);
   if pw_len=0 then internal_error('invalid password length');
   for k:=0 to pw_len-1 do sbuf2[k]:=ord(pw[k+1]);//copy password into an array of byte
   //append header to password's array (sbuf2)
   for k:=0 to 21 do sbuf2[pw_len+k]:=tagbuf[k];
   pw_len:=pw_len+22;
   //append keyfile to password's array (sbuf2)
   if upcase(keyfile_name)<>'NOKEYFILE' then
      test_pea_error('accessing keyfile',use_keyfile(keyfile_name,2048,numread,sbuf2,pw_len));
   //initialize AE
   if upcase(algo)='EAX256' then init_AE256_control_algo
   else init_AE128_control_algo;
   clean_keying_vars;
   for i:=0 to 3 do sbuf1[i]:=sbuf1[i+16]; //discard 16 bytes of crypto subheader
   end
//if AE is not used, initialize other control algorithms (and check headers)
else
   begin
   init_nonAE_control_algo;
   update_control_algo(tagbuf,20);//check the archive and stream headers
   end;
Form_pea.LabelDecrypt4.Caption:='Using: '+compr+', stream: '+algo+', objects: '+obj_algo+', volume(s): '+volume_algo;
out_created:=false;
if upcase(struct_param)='EXTRACT2DIR' then //save objects with shortest path in a dir with archive's name; actually this is the only output method allowed
   begin
   s:=out_file;
   j:=0;
   repeat
      if not(udirectoryexists(out_path+out_file)) then
         try
         uforcedirectories(out_path+out_file);
         out_created:=true;
         except
         out_file:=s+'output';
         out_created:=true;
         end
      else
         begin
         j:=j+1;
         out_file:=s+'-'+inttostr(j);
         if j=1000 then //to break recursivity if filename is not valid (ie unsupported character encoding)
            begin
            out_file:=s+'output';
            out_created:=true;
            end;
         end;
      {try //no longer works with Lazarus 0.9.30, exception is not returned
         umkdir(out_path+out_file);
         out_created:=true;
      except
         out_file:=s+'-'+inttostr(j);
         j:=j+1;
      end;}
      until out_created=true;
   usetcurrentdir(out_param);
   end;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
//if compression is used, get compression buffer size; since at present revision level a single stream is included in an archive, the stream specific compression buffer size is read as first 4 bytes after the headers area
if compr<>'PCOMPRESS0' then
   begin
   update_control_algo(sbuf1,4);
   buf_size:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
   end;
// process the data
uncompsize:=0;
no_more_files:=false;
chunks_ok:=true;
readingstream:=true;
readingheader:=true;
readingfns:=false;
readingtrigger:=false;
readingfn:=false;
readingfs:=false;
readingfage:=false;
readingfattrib:=false;
readingcompsize:=false;
fassigned:=false;
readingf:=false;
readingcompblock:=false;
readingobjauth:=false;
readingauth:=false;
end_of_archive:=false;
addr:=0;
uncompsize:=0;
j:=1;
n_dirs:=0;
n_input_files:=0;
out_size:=0;
wrk_space:=0;
nobj:=-1;
init_volume_control_algo;
while (chunks_ok=true) and (end_of_archive=false) do
   begin
   if singlevolume=false then update_pea_filename(in_name,j,in_file);
   repeat
      if ufileexists(in_folder+in_file) then
         begin
         try
      chunks_ok:=true;
      uassignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      if IOResult<>0 then internal_error('IO error opening '+in_folder+in_file);
      srcfilesize(in_folder+in_file,total);
      total:=total-volume_authsize;
      //total:=system.filesize(f_in)-volume_authsize;
      while ((total>0) and (readingheader=true)) do //read and discard archive and stream headers
         begin
         if total>headersize-addr then i:=headersize-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         inc(addr,numread);
         if addr>=headersize then
            begin
            addr:=0;
            readingheader:=false;
            readingfns:=true;
            end;
         end;
      1:
      while ((total>0) and (readingfns=true)) do //read filename size;
         begin
         if total>2-addr then i:=2-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=2 then
            begin
            readingfns:=false;
            addr:=0;
            if readingstream=true then
               begin
               init_obj_control_algo;
               update_control_algo(sbuf1,2);
               update_obj_control_algo(sbuf1,2);
               end;
            fns:=sbuf1[0] + (sbuf1[1] shl 8);
            if fns>SBUFSIZE then internal_error('Object name size exceeds '+inttostr(SBUFSIZE));
            {pathnames longer than SBUFSIZE (usually exceeding actual needs,
            SBUFSIZE is originally defined as 32KB), are considered errors}
            if fns=0 then readingtrigger:=true //read a trigger object
            else
               begin
               readingtrigger:=false;
               readingfn:=true;
               inc(nobj,1);
               end;
            end;
         end;
      while ((total>0) and (readingtrigger=true)) do //read 4 byte trigger;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingtrigger:=false;
            addr:=0;
            update_control_algo(sbuf1,4);
            if ((sbuf1[0]=69) and (sbuf1[1]=79) and (sbuf1[2]=65) and (sbuf1[3]=0)) then //EOA
               begin
               if authsize<>0 then readingauth:=true;
               end_of_archive:=true;
               end
            else internal_error('Unrecognized trigger object');
            end;
         end;
      while ((total>0) and (readingfn=true)) do //read object name;
         begin
         if total>fns-addr then i:=fns-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=fns then
            begin
            readingfn:=false;
            readingfage:=true;
            addr:=0;
            fn:='';
            update_control_algo(sbuf1,fns);
            update_obj_control_algo(sbuf1,fns);
            for k:=0 to fns-1 do fn:=fn+char(sbuf1[k]);
            SetLength(in_files,length(in_files)+1);
            SetLength(status_objects,length(in_files)+1);
            SetLength(fsizes,length(in_files)+1);
            SetLength(ftimes,length(in_files)+1);
            SetLength(fattr,length(in_files)+1);
            SetLength(fattr_dec,length(in_files)+1);
            SetLength(obj_tags,length(in_files)+1);
            SetLength(exp_obj_tags,length(in_files)+1);
            in_files[nobj]:=fn;
            end;
         end;
      while ((total>0) and (readingfage=true)) do //read file date and time of last modification;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingfage:=false;
            readingfattrib:=true;
            addr:=0;
            update_control_algo(sbuf1,4);
            update_obj_control_algo(sbuf1,4);
            fage:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
            ftimes[nobj]:=fage;
            end;
         end;
      while ((total>0) and (readingfattrib=true)) do //read file attributes;
         begin
         if total>4-addr then i:=4-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=4 then
            begin
            readingfattrib:=false;
            addr:=0;
            n_input_files:=n_input_files+1;
            update_control_algo(sbuf1,4);
            update_obj_control_algo(sbuf1,4);
            fattrib:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
            fattr[nobj]:=fattrib;
            dword2decodedFileAttributes(fattrib,fattr_dec[nobj]);
            if fassigned=false then
               begin
               //udodirseparators(fn);
               dodirseparators(fn);
               if upcase(struct_param)='EXTRACT2DIR' then
                  begin
                  ansiextract2dir;
                  if (total>0) and (fattrib and faDirectory <> 0) then //object is a dir
                     begin
                     n_dirs:=n_dirs+1;
                     readingobjauth:=true;
                     end;
                  end;
               end;
            end;
         end;
      while ((total>0) and (readingfs=true)) do //read file size;
         begin
         if total>8-addr then i:=8-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=8 then
            begin
            readingfs:=false;
            addr:=0;
            update_control_algo(sbuf1,8);
            update_obj_control_algo(sbuf1,8);
            qw0:=sbuf1[0];
            qw1:=sbuf1[1];
            qw2:=sbuf1[2];
            qw3:=sbuf1[3];
            qw4:=sbuf1[4];
            qw5:=sbuf1[5];
            qw6:=sbuf1[6];
            qw7:=sbuf1[7];
            qw0:=qw0;
            qw1:=qw1 *256;
            qw2:=qw2 *256*256;
            qw3:=qw3 *256*256*256;
            qw4:=qw4 *256*256*256*256;
            qw5:=qw5 *256*256*256*256*256;
            qw6:=qw6 *256*256*256*256*256*256;
            qw7:=qw7 *256*256*256*256*256*256*256;
            fs:=qw0+qw1+qw2+qw3+qw4+qw5+qw6+qw7;
            //fs:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24) + (sbuf1[4] shl 32) + (sbuf1[5] shl 40) + (sbuf1[6] shl 48) + (sbuf1[7] shl 56);
            out_size:=out_size+fs;
            fsizes[nobj]:=fs;
            if fs>0 then
               if compr<>'PCOMPRESS0' then readingcompsize:=true
               else readingf:=true
            else //object is an empty file
               begin
               closefile(f_out);
               fassigned:=false;
               readingobjauth:=true;
               end;
            end;
         end;
      if compr<>'PCOMPRESS0' then //use compression
         begin
         while ((total>0) and (readingcompsize=true)) do
            begin
            if total>4-addr then i:=4-addr else i:=total;
            blockread (f_in,sbuf2,i,numread);
            if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
            update_volume_control_algo(sbuf2,numread);
            dec(total,numread);
            inc(wrk_space,numread);
            for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
            inc(addr,numread);
            if addr>=4 then
               begin
               readingcompsize:=false;
               readingf:=true;
               addr:=0;
               update_control_algo(sbuf1,4);
               update_obj_control_algo(sbuf1,4);
               compsize:=sbuf1[0] + (sbuf1[1] shl 8) + (sbuf1[2] shl 16) + (sbuf1[3] shl 24);
               end;
            end;
         while ((total>0) and (readingf=true)) do
            begin
            while ((total>0) and (addr<compsize+4)) do //read first compsize field for a compressed byte (buffer size was jet read)
               begin
               readingcompblock:=true;
               if total>compsize+4-addr then i:=compsize+4-addr else i:=total;
               blockread (f_in,wbuf2,i,numread);
               if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
               ci:=0;
               while ci<numread do
                  begin
                  if numread-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=numread-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf2[ci+k];
                  update_volume_control_algo(sbuf1,cj);
                  inc(ci,cj);
                  end;
               dec(total,numread);
               inc(wrk_space,numread);
               for k:=0 to i-1 do wbuf1[addr+k]:=wbuf2[k];
               inc(addr,numread);
               if addr=compsize+4 then readingcompblock:=false;
               end;
            if readingcompblock=false then //read a compressed block sized compsize and next 4 byte (next block's compressed size, or uncompressed size for last block)
               begin
               addr:=0;
               ci:=0;
               while ci<compsize+4 do
                  begin
                  if compsize+4-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=compsize+4-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf1[ci+k];
                  update_control_algo(sbuf1,cj);
                  for k:=0 to cj-1 do wbuf1[ci+k]:=sbuf1[k];
                  inc(ci,cj);
                  end;
               if fs>buf_size then k:=buf_size else k:=fs;
               uncompsize:=k;
               if compsize<k then zuncompr.uncompress(@wbuf2[0], uncompsize, wbuf1[0], compsize)
               else wbuf2:=wbuf1;
               ci:=0;
               while ci<uncompsize do
                  begin
                  if uncompsize-ci > SBUFSIZE then cj:=SBUFSIZE else cj:=uncompsize-ci;
                  for k:=0 to cj-1 do sbuf1[k]:=wbuf2[ci+k];
                  update_obj_control_algo(sbuf1,cj);
                  inc(ci,cj);
                  end;
               blockwrite (f_out,wbuf2,uncompsize,numwritten);
               if IOResult<>0 then internal_error('IO error writing data');
               dec(fs,numwritten);
               compsize:=wbuf1[compsize]+(wbuf1[compsize+1] shl 8)+(wbuf1[compsize+2] shl 16)+(wbuf1[compsize+3] shl 24);
               if compsize>WBUFSIZE then internal_error('Decompression error, declared compsize bigger than compression buffer');
               dword2bytebuf(compsize,sbuf1,0);
               update_obj_control_algo(sbuf1,4);
               Form_pea.ProgressBar1.Position:=(wrk_space) div cent_size;
               Application.ProcessMessages;
               end;
            if fs=0 then //end of compressed file, control if uncompsize of last block matches to what expected
               begin
               if compsize<>uncompsize then internal_error('Decompression error, uncompressed size doesn''t match with expected size');
               closefile(f_out);
               fassigned:=false;
               readingf:=false;
               readingobjauth:=true;
               end;
            end;
         end
      else //no compression
         while ((total>0) and (readingf=true)) do
            begin
            if total>SBUFSIZE then i:=SBUFSIZE else i:=total;
            if fs>i then else i:=fs;
            blockread (f_in,sbuf1,i,numread);
            if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
            update_volume_control_algo(sbuf1,numread);
            dec(total,numread);
            inc(wrk_space,numread);
            dec(fs,numread);
            update_control_algo(sbuf1,numread);
            update_obj_control_algo(sbuf1,numread);
            blockwrite (f_out,sbuf1,numread,numwritten);
            if IOResult<>0 then internal_error('IO error writing data');
            Form_pea.ProgressBar1.Position:=(wrk_space) div cent_size;
            Application.ProcessMessages;
            if fs=0 then
               begin
               closefile(f_out);
               fassigned:=false;
               readingf:=false;
               readingobjauth:=true;
               end;
            end;
      //read object check field
      while ((total>0) and (readingobjauth=true)) do
         begin
         if obj_algo='NOALGO' then
            begin
            readingobjauth:=false;
            readingfns:=true;
            addr:=0;
            if total>0 then goto 1;
            end;
         if total>obj_authsize-addr then i:=obj_authsize-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr>=obj_authsize then
            begin
            update_control_algo(sbuf1,obj_authsize);
            readingobjauth:=false;
            readingfns:=true;
            addr:=0;
            finish_obj_control_algo;
            check_obj;
            if total>0 then goto 1;
            end;
         end;
      //read auth block (if any);
      while (total>0) and (readingauth=true) do
         begin
         if total>authsize-addr then i:=authsize-addr else i:=total;
         blockread (f_in,sbuf2,i,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         update_volume_control_algo(sbuf2,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         for k:=0 to numread-1 do sbuf1[addr+k]:=sbuf2[k];
         inc(addr,numread);
         if addr=authsize then
            begin
            finish_control_algo;
            authenticate_stream;
            readingfns:=true;
            addr:=0;
            if total>0 then internal_error('Last volume seem to have wrong size');
            end;
         end;
      //read volume check block (if any);
      if (total=0) then
         begin
         SetLength(status_volumes,length(status_volumes)+1);
         SetLength(volume_tags,length(status_volumes)+1);
         SetLength(exp_volume_tags,length(status_volumes)+1);
         blockread (f_in,tagbuf,volume_authsize,numread);
         if IOResult<>0 then internal_error('IO error reading from '+in_folder+in_file);
         finish_volume_control_algo;
         check_volume;
         dec(total,numread);
         inc(wrk_space,numread);
         init_volume_control_algo;
         end;
      close(f_in);
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(j));
      j:=j+1;
      except
         try
            usetcurrentdir(out_path);
            do_report_unpea;
            save_report('error log');
         except
         end;
      internal_error('Unexpected error working on volume '+inttostr(j)+'; data is either become non accessible or could be corrupted in a way that not allow the current implementation to extract data from the archive (in that case you should try to obtain a new copy of the archive). Tried to extract available output to: '+out_path+out_file+DirectorySeparator+' and to save the error report in: '+out_path);
      end;
      end
      else check_chunk(in_folder,j,chunks_ok);
   until (chunks_ok=true) or (end_of_archive=true);
   end;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, '+inttostr(j-1)+' volume(s), '+inttostr(wrk_space)+' B';
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file+DirectorySeparator;
Form_pea.LabelDecrypt6.Caption:='Done '+struct_param+' on archive';
Form_pea.ProgressBar1.Position:=100;
usetcurrentdir(out_path);
do_report_unpea;
timing(ts_start,wrk_space);
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path+out_file;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log UnPEA');
if report_errors =0 then
   begin
   sleep(500);
   if closepolicy>0 then Form_pea.Close;
   end;
end;

{
Raw File Split
Byte split a single input file in volumes of given size
In an optional separate .check file are saved error checking tags of each volume
The code is closely related to PEA, it's kept distinct for better readability
}

procedure rfs;
var
   out_param,volume_algo,in_qualified_name,pw_param:utf8string;
   ch_size:qword;
   volume_authsize:byte;

procedure parse_rfs_cl;
begin
try
   //output
   out_param:=envtoutf8(paramstr(2));
   //control chunk size
   if (upcase(paramstr(3))='ASK') then
      begin
      ch_size:=vol_size;
      volume_algo:=vol_algo;
      end
   else
      begin
      try
         ch_size:=strtoqword(paramstr(3));
         if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
      except
         internal_error('"'+paramstr(3)+'" is not a valid chunk size; values allowed are 1..2^64, 0 to don''t split the input');
      end;
      //get volume control algorithm
      volume_algo:=upcase(paramstr(4));
      end;
   if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
      internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
   //get operation mode
   pw_param:=upcase(paramstr(5));
   if (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter for RFS, please refer to the documentation');
   //input
   if envtoutf8(paramstr(6))<>'' then
      begin
      in_qualified_name:=envtoutf8(paramstr(6));
      if not ufileexists(in_qualified_name) then
         internal_error('"'+in_qualified_name+'" file is not accessible');
      end
   else
      begin
      internal_error('No accessible input object found');
      end;
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_rfs_cl;
rfs_procedure(out_param,ch_size,volume_algo,volume_authsize,pw_param,in_qualified_name);
end;

procedure rfs_lib_procedure ( out_param:utf8string;                             //qualified name for output volumes (without .(volume number) suffix) or AUTONAME
                              ch_size:qword;                                    //size of volumes, 0 for single volume (current implementation up to 2^64 byte of size for volume)
                              volume_algo,                                      //algorithm for volume integrity check
                              in_qualified_name:utf8string;                     //qualified name of input file
                              opmode:utf8string);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:utf8string;
   volume_authsize:byte;
begin
//control chunk size
if ch_size=0 then ch_size:=1024*1024*1024*1024*1024; // if chunk size is set to 0 no chunks will be done
//get volume control algorithm
if decode_volume_control_algo(volume_algo,volume_authsize)<>0 then
   internal_error('"'+volume_algo+'" is not a valid control algorithm for volume check, please refer to the documentation for supported ones');
//input
if in_qualified_name='' then
   internal_error('No accessible input object found');
if not ufileexists(in_qualified_name) then
   internal_error('"'+in_qualified_name+'" file is not accessible');
//get operation mode
if (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter for rfs_lib_procedure, please refer to the documentation');
pw_param:=upcase(opmode);
rfs_procedure(out_param,ch_size,volume_algo,volume_authsize,pw_param,in_qualified_name);
end;

procedure rfs_procedure ( out_param:utf8string;
                          ch_size:qword;
                          volume_algo:utf8string;
                          volume_authsize:byte;
                          pw_param:utf8string;
                          in_qualified_name:utf8string);
var
   HashContext_volume: THashContext;
   Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest_volume: TSHA512Digest;
   SHA256Digest_volume: TSHA256Digest;
   SHA1Digest_volume: TSHA1Digest;
   RMD160Digest_volume: TRMD160Digest;
   MD5Digest_volume: TMD5Digest;
   crc64_volume:TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out,f_check:file of byte;
   sbuf1:array [0..65535] of byte;
   auth_buf:array [0..63] of byte;
   adler_volume,crc32_volume:longint;
   j,ch_number_expected,numread,num_res:dword;
   file_size,total,cent_size,prog_size,in_size,out_size,check_size,exp_size,ch_res:qword;
   out_file,out_path,out_name:utf8string;

procedure clean_variables;
begin
j:=0;
ch_number_expected:=0;
numread:=0;
num_res:=0;
file_size:=0;
total:=0;
cent_size:=0;
prog_size:=0;
in_size:=0;
out_size:=0;
check_size:=0;
exp_size:=0;
ch_res:=0;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure write_volume_check;
var k:dword;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do auth_buf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do auth_buf[k]:=SHA1Digest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do auth_buf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do auth_buf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,auth_buf,0);
      dword2bytebuf(crc64_volume.hi32,auth_buf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,auth_buf,0);
      'ADLER32' : dword2bytebuf(adler_volume,auth_buf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=auth_buf[k];
   blockwrite (f_check,auth_buf,volume_authsize);
   check_size:=check_size+volume_authsize;
   end;
end;

procedure write2chunks ( var num_res: dword;                     //amount of data to write
                         var sbuf1: array of byte;               //data buffer
                         var f_out:fileofbyte;                   //output file
                         var out_path,out_name: utf8string;      //name and path for the output;
                         var i: dword;                           //chunk progressive number
                         var ch_size:qword;                      //chunk size
                         var ch_res: qword);                     //residual space in the given chunk
var k,numwritten:dword;
    addr,buf:qword;
    out_file:utf8string;
begin
addr:=0;
numwritten:=0;
while num_res>0 do
   begin
   if num_res<=ch_res then
      begin
      blockwrite (f_out,sbuf1,num_res,numwritten);
      if IOResult<>0 then internal_error('IO error writing to volume '+inttostr(i));
      update_volume_control_algo(sbuf1,numwritten);
      num_res:=num_res-numwritten;
      ch_res:=ch_res-numwritten;
      addr:=0;
      end
   else
      begin
      SetLength(volume_tags,length(volume_tags)+1);
      blockwrite (f_out,sbuf1,ch_res,numwritten);
      if IOResult<>0 then internal_error('IO error writing to volume '+inttostr(i));
      update_volume_control_algo(sbuf1,numwritten);
      finish_volume_control_algo;
      write_volume_check;
      if IOResult<>0 then internal_error('IO error writing volume control tag to volume '+inttostr(i));
      close(f_out);
      if IOResult<>0 then internal_error('IO error closing volume '+inttostr(i));
      i:=i+1;
      update_rfs_filename(out_name,i,out_file);
      checkspace(out_path,ch_size);
      uassignfile(f_out,out_path+out_file);
      rewrite(f_out); //it will overwrite orphaned files with same name to preserve name coherence
      if IOResult<>0 then internal_error('IO error opening volume '+inttostr(i));
      init_volume_control_algo;
      num_res:=num_res-numwritten;
      if num_res<ch_size then buf:=num_res else buf:=ch_size;
      addr:=addr+numwritten;
      for k:=0 to buf do sbuf1[k]:=sbuf1[addr+k];
      ch_res:=ch_size;
      end;
   end;
end;

procedure nocompress_file;
begin
while ((numread<>0) and (total<file_size)) do
   begin
   blockread (f_in,sbuf1,SBUFSIZE,numread);
   if IOResult<>0 then internal_error('IO error reading from '+in_qualified_name);
   inc(total,numread);
   inc(prog_size,numread);
   num_res:=numread;
   write2chunks ( num_res,
                  sbuf1,
                  f_out,
                  out_path,out_name,
                  j,
                  ch_size,
                  ch_res);
   Form_pea.ProgressBar1.Position:=prog_size div cent_size;
   Application.ProcessMessages;
   end;
end;

procedure first_gui_output;
begin
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelEncrypt2.Caption:='Input: '+in_qualified_name;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_param;
Form_pea.LabelEncrypt4.Caption:='Integrity check algorithm: '+volume_algo;
Form_pea.LabelTime1.Caption:='Splitting file in volumes...';
Form_pea.Panel1.visible:=false;
Form_pea.LabelE1.Visible:=false;
end;

procedure evaluate_volumes;
begin
ch_number_expected:=(in_size div ch_size)+1;
if (exp_size mod ch_size)=0 then ch_number_expected:=ch_number_expected-1;
if ch_number_expected>9999 then
   if MessageDlg('Expected '+inttostr(ch_number_expected)+' volumes. It seems a lot! Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else Application.Terminate;
if ch_size<>1024*1024*1024*1024*1024 then Form_pea.LabelEncrypt5.Caption:='Expected '+inttostr(ch_number_expected)+' volume(s) of '+inttostr(ch_size+volume_authsize)+' B for a total output size of '+inttostr(exp_size)+' B'
else Form_pea.LabelEncrypt5.Caption:='Expected a single volume of '+inttostr(exp_size)+' B of size';
end;

procedure evaluate_output;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_qualified_name;
out_file:=uextractfilename(out_param);
out_path:=uextractfilepath(out_param);
if out_file='' then out_file:=uextractfilename(in_qualified_name); //if no output name is explicitely given, the output name is assumed to be the name of the input file
if out_path='' then out_path:=uextractfilepath(in_qualified_name); //if no output path is explicitely given, the output path is assumed to be the path of the input file
if out_path='' then out_path:=executable_path;
if usetcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_file;
if exp_size>diskfree(0) then
   if MessageDlg('Output path '+out_path+' seems to not have enough free space, you should continue only if it is a removable support and you have enough removable media. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else Application.Terminate;
end;

procedure do_report_rfs;
var
   k,h:dword;
   s:utf8string;
begin
Form_report.Input.Caption:='Input';
Form_report.Output.Caption:='Output';
Form_report.Caption:='Split file log';
Form_report.StringGrid1.ColCount:=3;
Form_report.StringGrid1.Cells[0,0]:='Original object name';
Form_report.StringGrid1.Cells[1,0]:='Status';
Form_report.StringGrid1.Cells[2,0]:='Size (B)';
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,1]:=in_qualified_name;
Form_report.StringGrid1.Cells[1,1]:='OK';
Form_report.StringGrid1.Cells[2,1]:=inttostr(file_size);
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=2;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:=volume_algo;
Form_report.StringGrid2.RowCount:=j+1;
for k:=0 to j-1 do
    begin
    Form_report.StringGrid2.Cells[0,k+1]:=inttostr(k+1);
    if upcase(volume_algo)<>'NOALGO' then
       begin
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[1,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
//operation parameters
Form_report.Label1.Caption:=Form_pea.LabelEncrypt4.Caption;
//input
Form_report.Label2.Caption:='Splitted '+in_qualified_name+'; input '+inttostr(file_size)+' B';
//output
Form_report.Label3.Caption:=Form_pea.LabelEncrypt6.Caption;
//ouput name
Form_report.Label4.Caption:=Form_pea.LabelEncrypt3.Caption;
end;

procedure last_gui_output;
begin
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelEncrypt3.Caption:='Output: '+out_path+out_name+'.*';
out_size:=prog_size;
if ch_size<>1024*1024*1024*1024*1024 then Form_pea.LabelEncrypt6.Caption:=inttostr(j)+' volume(s) of '+inttostr(ch_size)+' B; total output '+inttostr(out_size)+' B'
else Form_pea.LabelEncrypt6.Caption:='Single volume archive of '+inttostr(out_size)+' B';
if upcase(volume_algo)<>'NOALGO' then Form_pea.LabelEncrypt6.Caption:=Form_pea.LabelEncrypt6.Caption+' + '+inttostr(check_size)+' B (check tags)';
do_report_rfs;
Form_pea.LabelEncrypt5.Caption:=Form_report.Label2.Caption;
Form_pea.LabelEncrypt4.Visible:=true;
Form_pea.LabelEncrypt5.Visible:=true;
Form_pea.LabelEncrypt6.Visible:=true;
end;

begin
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=false;
Form_pea.PanelEncrypt1.visible:=true;
Form_pea.Caption:='Split file';
ts_start:=datetimetotimestamp(now);
//give preliminary information on work status to the GUI
first_gui_output;
uassignfile(f_in,in_qualified_name);
filemode:=0;
reset(f_in);
if IOResult<>0 then internal_error('IO error opening '+in_qualified_name);
srcfilesize(in_qualified_name,file_size);
//file_size:=system.filesize(f_in);
if file_size=0 then internal_error('Cannot split empty files');
if ch_size>file_size then ch_size:=file_size;
cent_size:=(file_size div 100)+1; //1% of expected output size, used for progress indication
//evaluate volumes number;
//at 9999 objects the program will warn and proceed only after user's permission,
//however the program has no sort of problem until 999999 chunks (but the host
//system may!)
evaluate_volumes;
//get output path and name;
//evaluate if the path has enough free space for expected output.
evaluate_output;
//check if output path has room for a chunk of given size (mandatory)
checkspace(out_path,ch_size);
//start the actual operation routine
out_name:=out_file;
uassignfile(f_out,out_file+'.001');//current dir was jet set to out_path
rewrite(f_out);
if IOResult<>0 then internal_error('IO error creating first output volume');
if upcase(volume_algo)<>'NOALGO' then
   begin
   uassignfile(f_check,out_file+'.check');
   rewrite(f_check);
   if IOResult<>0 then internal_error('IO error creating .check file');
   rfs_create_checkfile_hdr(volume_algo,sbuf1);
   blockwrite(f_check,sbuf1,4);
   if IOResult<>0 then internal_error('IO error writing to .check file');
   check_size:=4;
   init_volume_control_algo;
   end;
j:=1;
//1) split file in chunks
total:=0;
numread:=1;
ch_res:=ch_size;
nocompress_file; //no compression
//last volume check
SetLength(volume_tags,length(volume_tags)+1);
finish_volume_control_algo;
write_volume_check;
if IOResult<>0 then internal_error('IO error writing last volume check');
closefile(f_in);
if IOResult<>0 then internal_error('IO error closing '+in_qualified_name);
closefile(f_out);
if IOResult<>0 then internal_error('IO error closing last output volume');
if upcase(volume_algo)<>'NOALGO' then
   begin
   closefile(f_check);
   if IOResult<>0 then internal_error('IO error closing .check file');
   end;
//give final job information to the GUI
last_gui_output;
//calculate operation time
timing(ts_start,out_size);
//make accessible exit button and link to the detailed job log
Form_pea.LabelLog1.Visible:=true;
Form_pea.LabelOpen.Caption:='Explore';
output:=out_path;
Form_pea.LabelOpen.visible:=true;
Form_pea.ButtonDone1.Visible:=true;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log Raw File Split');
Sleep(500);
if closepolicy>0 then Form_pea.Close;
end;

{
Raw File Join
Byte join volumes with same name and progressive counter extension in a single
output file
Optionally error check each volume with informations provided by a separate
.check file
The code is closely related to UnPEA, it's kept distinct for better readability
}

procedure rfj;
var
   in_qualified_name,out_param,pw_param:utf8string;

procedure parse_rfj_cl;
begin
try
   in_qualified_name:=envtoutf8(paramstr(2));
   if not(ufileexists(in_qualified_name)) then
      internal_error('"'+in_qualified_name+'" not exist');
   //get operation mode
   pw_param:=upcase(paramstr(3));
   if (pw_param<>'BATCH') and (pw_param<>'HIDDEN') and (pw_param<>'BATCH_REPORT') and (pw_param<>'HIDDEN_REPORT') then
      internal_error('"'+pw_param+'" is not a valid operation mode parameter for RFJ, please refer to the documentation');
   out_param:=envtoutf8(paramstr(4));
except
   internal_error('Received incorrect Command Line. See the documentation for the correct synopsis.');
end;
end;

begin
parse_rfj_cl;
rfj_procedure(in_qualified_name,pw_param,out_param);
end;

procedure rfj_lib_procedure ( in_qualified_name,                                //qualified name of first volume of the splitted file
                              out_param,                                        //qualified name to give to the output rejoined file (or AUTONAME)
                              opmode:utf8string);                               //mode of operation: VISIBLE the form is visible, HIDDEN the form is not visible, MESSAGE the form is not visible, a message is sent as popup at the end of the operation
var
   pw_param:utf8string;
begin
if not(ufileexists(in_qualified_name)) then
   internal_error('"'+in_qualified_name+'" not exist');
//get operation mode
if (upcase(opmode)<>'BATCH') and (upcase(opmode)<>'HIDDEN') and (upcase(opmode)<>'BATCH_REPORT') and (upcase(opmode)<>'HIDDEN_REPORT') then
   internal_error('"'+upcase(opmode)+'" is not a valid operation mode parameter for rfj_lib_procedure, please refer to the documentation');
pw_param:=upcase(opmode);
rfj_procedure(in_qualified_name,pw_param,out_param);
end;

procedure rfj_procedure ( in_qualified_name,
                          pw_param,
                          out_param:utf8string);
var
   HashContext_volume: THashContext;
   Whirl512Digest_volume: TWhirlDigest;
   SHA512Digest_volume: TSHA512Digest;
   SHA256Digest_volume: TSHA256Digest;
   SHA1Digest_volume: TSHA1Digest;
   RMD160Digest_volume: TRMD160Digest;
   MD5Digest_volume: TMD5Digest;
   crc64_volume: TCRC64;
   ts_start:TTimeStamp;
   f_in,f_out,f_check:file of byte;
   sbuf1:array [0..65535] of byte;
   tagbuf:array [0..63] of byte;
   volume_authsize:byte;
   adler_volume,crc32_volume:longint;
   i,j,numread,numwritten,n_chunks:dword;
   total,prog_size,wrk_space,exp_space:qword;
   chunks_ok,no_more_files,filenamed:boolean;
   in_file,in_name,in_folder,out_path,out_file,volume_algo:utf8string;

procedure clean_variables;
begin
i:=0;
j:=0;
numread:=0;
numwritten:=0;
n_chunks:=0;
total:=0;
prog_size:=0;
wrk_space:=0;
exp_space:=0;
end;

procedure evaluate_file_size(var exp_space:qword; var prog_size:qword); //succeed if all chunks are accessible
var qw:qword;
begin
j:=1;
no_more_files:=false;
exp_space:=0;
while no_more_files=false do
   begin
   update_rfs_filename(in_name,j,in_file);
   if ufileexists(in_folder+in_file) then
      begin
      uassignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      srcfilesize(in_folder+in_file,qw);
      exp_space:=exp_space+qw;
      //exp_space:=exp_space+system.filesize(f_in);
      closefile(f_in);
      j:=j+1;
      end
   else no_more_files:=true;
   end;
n_chunks:=j-1;
prog_size:=(exp_space div 100)+1;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, expected '+inttostr(n_chunks)+' volume(s), total '+inttostr(exp_space)+' B';
end;

procedure evaluate_output;
var
   k:integer;
   name_ok:boolean;
begin
if upcase(out_param) = 'AUTONAME' then out_param:=in_folder+in_name;//the extension was already removed from in_file name
k:=0;
name_ok:=false;
repeat
   if k=0 then
      if ufileexists(out_param) or udirectoryexists(out_param) then inc(k,1)
      else name_ok:=true
   else
      if ufileexists(out_param+'-'+inttostr(k)+uextractfileext(out_param)) or udirectoryexists(out_param+'-'+inttostr(k)+uextractfileext(out_param)) then inc(k,1)
      else name_ok:=true;
until name_ok = true;
if k>0 then out_param:=out_param+'-'+inttostr(k)+uextractfileext(out_param);
out_file:=uextractfilename(out_param);
out_path:=uextractfilepath(out_param);
if out_file='' then out_file:=uextractfilename(in_qualified_name); //if no output name is explicitely given, the output name is assumed to be the name of the input file
if out_path='' then out_path:=uextractfilepath(in_qualified_name); //if no output path is explicitely given, the output path is assumed to be the path of the input file
if out_path='' then out_path:=executable_path;
if usetcurrentdir(out_path)<>true then out_path:=executable_path; //from this point output path is set as current path; if output path is missing or non accessible executable_path (path where the executable is in) is set as output path
if out_path[length(out_path)]<>DirectorySeparator then out_path:=out_path+DirectorySeparator;
Form_pea.LabelDecrypt3.Caption:='Input: '+out_path+out_file;
if exp_space>diskfree(0) then
   if MessageDlg('Output path '+out_path+' seems to not have enough free space. Continue anyway?',mtWarning,[mbYes, mbNo],0)=6 then
   else Application.Terminate;
end;

procedure init_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Init(HashContext_volume);
'SHA512' : SHA512Init(HashContext_volume);
'SHA256' : SHA256Init(HashContext_volume);
'SHA1' : SHA1Init(HashContext_volume);
'RIPEMD160' : RMD160Init(HashContext_volume);
'MD5' : MD5Init(HashContext_volume);
'CRC64' : CRC64Init(crc64_volume);
'CRC32' : CRC32Init(crc32_volume);
'ADLER32' : Adler32Init(adler_volume);
end;
end;

procedure update_volume_control_algo(buf:array of byte; size:word);
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Update(HashContext_volume, @buf, size);
'SHA512' : SHA512Update(HashContext_volume, @buf, size);
'SHA256' : SHA256Update(HashContext_volume, @buf, size);
'SHA1' : SHA1Update(HashContext_volume, @buf, size);
'RIPEMD160' : RMD160Update(HashContext_volume, @buf, size);
'MD5' : MD5Update(HashContext_volume, @buf, size);
'CRC64' : CRC64Update(crc64_volume, @buf, size);
'CRC32' : CRC32Update(crc32_volume, @buf, size);
'ADLER32' : Adler32Update(adler_volume, @buf, size);
end;
end;

procedure finish_volume_control_algo;
begin
case upcase(volume_algo) of
'WHIRLPOOL' : Whirl_Final(HashContext_volume,WHIRL512Digest_volume);
'SHA512' : SHA512Final(HashContext_volume,SHA512Digest_volume);
'SHA256' : SHA256Final(HashContext_volume,SHA256Digest_volume);
'SHA1' : SHA1Final(HashContext_volume,SHA1Digest_volume);
'RIPEMD160' : RMD160Final(HashContext_volume,RMD160Digest_volume);
'MD5' : MD5Final(HashContext_volume,MD5Digest_volume);
'CRC64' : CRC64Final(crc64_volume);
'CRC32' : CRC32Final(crc32_volume);
'ADLER32' : Adler32Final(adler_volume);
end;
end;

procedure check_volume;
var
   k:dword;
   tag_match:boolean;
begin
if upcase(volume_algo)<>'NOALGO' then
   begin
   for k:=0 to volume_authsize-1 do exp_volume_tags[j-1,k]:=tagbuf[k];
   case upcase(volume_algo) of
      'WHIRLPOOL' : for k:=0 to volume_authsize-1 do tagbuf[k]:=WHIRL512Digest_volume[k];
      'SHA512' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA512Digest_volume[k];
      'SHA256' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA256Digest_volume[k];
      'SHA1' : for k:=0 to volume_authsize-1 do tagbuf[k]:=SHA1Digest_volume[k];
      'RIPEMD160' : for k:=0 to volume_authsize-1 do tagbuf[k]:=RMD160Digest_volume[k];
      'MD5' : for k:=0 to volume_authsize-1 do tagbuf[k]:=MD5Digest_volume[k];
      'CRC64' :
      begin
      dword2bytebuf(crc64_volume.lo32,tagbuf,0);
      dword2bytebuf(crc64_volume.hi32,tagbuf,4);
      end;
      'CRC32' : dword2bytebuf(crc32_volume,tagbuf,0);
      'ADLER32' : dword2bytebuf(adler_volume,tagbuf,0);
      end;
   for k:=0 to volume_authsize-1 do volume_tags[j-1,k]:=tagbuf[k];
   tag_match:=true;
   for k:=0 to volume_authsize-1 do if volume_tags[j-1,k]<>exp_volume_tags[j-1,k] then
      begin
      tag_match:=false;
      break;
      end;
   if tag_match=true then status_volumes[j-1]:='Volume is OK'
   else status_volumes[j-1]:='Wrong tag!';
   end;
end;

procedure do_report_rfj;
var
   h,k:dword;
   s:utf8string;
begin
Form_report.Input.Caption:='File';
Form_report.Output.Caption:='Volumes';
Form_report.Caption:='Log Raw File Join';
Form_report.StringGrid1.ColCount:=2;
Form_report.StringGrid1.Cells[0,0]:='File name';
Form_report.StringGrid1.Cells[1,0]:='Size (B)';
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,1]:=out_param;
Form_report.StringGrid1.Cells[1,1]:=inttostr(exp_space);
Form_report.StringGrid1.AutosizeColumns;
Form_report.StringGrid2.ColCount:=4;
Form_report.StringGrid2.Cells[0,0]:='Volume';
Form_report.StringGrid2.Cells[1,0]:='Status';
Form_report.StringGrid2.Cells[2,0]:='calculated ('+volume_algo+')';
Form_report.StringGrid2.Cells[3,0]:='found';
if j>1 then Form_report.StringGrid2.RowCount:=j
else exit;
for k:=0 to j-2 do
    begin
    Form_report.StringGrid2.Cells[0,k+1]:=inttostr(k+1);
    if upcase(volume_algo)<>'NOALGO' then
       begin
       Form_report.StringGrid2.Cells[1,k+1]:=status_volumes[k];
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[2,k+1]:=s;
       s:='';
       for h:=0 to volume_authsize-1 do s:=s+hexstr(@exp_volume_tags[k,h],1);
       Form_report.StringGrid2.Cells[3,k+1]:=s;
       end;
    end;
Form_report.StringGrid2.AutosizeColumns;
Form_report.Label1.Caption:=in_qualified_name+' -> '+out_param;
Form_report.Label2.Caption:=Form_pea.LabelDecrypt4.Caption;
Form_report.Label3.Caption:='Total output '+inttostr(wrk_space)+' B';
Form_report.Label4.Caption:='Joined '+inttostr(j-1)+' volume(s)';
end;

begin
clean_variables;
if (upcase(pw_param)<>'HIDDEN') and (upcase(pw_param)<>'HIDDEN_REPORT') then Form_pea.Visible:=true else Form_pea.Visible:=false;
Form_pea.PanelDecrypt1.visible:=true;
Form_pea.PanelEncrypt1.visible:=false;
Form_pea.Caption:='File join';
ts_start:=datetimetotimestamp(now);
Form_pea.ProgressBar1.Position:=0;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_qualified_name;
if uextractfileext(in_qualified_name)<>'.001' then
  begin
  showmessage('Please select the file with .001 extension to start joining file parts');
  exit;
  end;
Form_pea.LabelDecrypt3.Caption:='Output: '+out_param;
Form_pea.LabelTime1.Caption:='Joining volumes...';
in_folder:=uextractfilepath(in_qualified_name);
in_file:=uextractfilename(in_qualified_name);
delete(in_file,length(in_file)-3,4);
in_name:=in_file;
//try to evaluate archive size (succeed if all chunks are accessible)
evaluate_file_size(exp_space,prog_size);
//evaluate output name and if output path has enough free space
evaluate_output;
// process the data
chunks_ok:=true;
wrk_space:=0;
Form_pea.ProgressBar1.Position:=5;
j:=0;
filenamed:=false;
repeat //avoid to overwrite files
   if j=0 then
      if ufileexists(out_path+out_file) or udirectoryexists(out_path+out_file) then inc(j,1)
      else filenamed:=true
   else
      if ufileexists(out_path+out_file+'-'+inttostr(j)+uextractfileext(out_file)) or udirectoryexists(out_path+out_file+'-'+inttostr(j)+uextractfileext(out_file)) then inc(j,1)
      else filenamed:=true;
   until filenamed = true;
if j>0 then out_file:=out_file+'-'+inttostr(j)+uextractfileext(out_file);
uassignfile(f_out,out_path+out_file);
rewrite(f_out);
if IOResult<>0 then internal_error('IO error creating output file '+out_path+out_file);
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file;
j:=1;
try
   uassignfile(f_check,in_folder+in_name+'.check');
   filemode:=0;
   reset(f_check);
   if IOResult<>0 then internal_error('IO error opening check file '+in_folder+in_name+'.check');
   blockread (f_check,sbuf1,4,numread);
   if IOResult<>0 then internal_error('IO error reading from check file '+in_folder+in_name+'.check');
   if rfs_parse_archive_header (sbuf1,volume_algo)<>0 then volume_algo:='NOALGO';
except
   volume_algo:='NOALGO';
end;
decode_rfs_volume_control_algo (volume_algo, volume_authsize);
Form_pea.LabelDecrypt4.Caption:='Integrity check algorithm: '+volume_algo;
{
Since in raw splitted files there is no extra information about file termination,
the program will assume that the user had copied ALL needed volumes into the same
path
}
while chunks_ok=true do
   begin
   update_rfs_filename(in_name,j,in_file);
   if ufileexists(in_folder+in_file) then
      begin
      init_volume_control_algo;
      chunks_ok:=true;
      uassignfile(f_in,in_folder+in_file);
      filemode:=0;
      reset(f_in);
      if IOResult<>0 then internal_error('IO error opening input file '+in_folder+in_file);
      srcfilesize(in_folder+in_file,total);
      //total:=system.filesize(f_in);
      while (total>0) do
         begin
         if total>SBUFSIZE then i:=SBUFSIZE else i:=total;
         blockread (f_in,sbuf1,i,numread);
         if IOResult<>0 then internal_error('IO error reading from input file '+in_folder+in_file);
         update_volume_control_algo(sbuf1,numread);
         dec(total,numread);
         inc(wrk_space,numread);
         blockwrite (f_out,sbuf1,numread,numwritten);
         if IOResult<>0 then internal_error('IO error writing to output file '+out_path+out_file);
         end;
      close(f_in);
      if IOResult<>0 then internal_error('IO error closing input file '+in_folder+in_file);
      //check volume
      SetLength(status_volumes,length(status_volumes)+1);
      SetLength(volume_tags,length(status_volumes)+1);
      SetLength(exp_volume_tags,length(status_volumes)+1);
      if upcase(volume_algo)<>'NOALGO' then blockread (f_check,tagbuf,volume_authsize,numread);
      finish_volume_control_algo;
      check_volume;
      j:=j+1;
      Form_pea.ProgressBar1.Position:=wrk_space div prog_size;
      Application.ProcessMessages;
      end
   else chunks_ok:=false;
   end;
close(f_out);
if IOResult<>0 then internal_error('IO error closing output file '+out_path+out_file);
if upcase(volume_algo)<>'NOALGO' then
   begin
   closefile(f_check);
   if IOResult<>0 then internal_error('IO error closing check file '+in_folder+in_name+'.check');
   end;
Form_pea.LabelDecrypt2.Caption:='Input: '+in_name+'.*, got '+inttostr(j-1)+' volume(s), total '+inttostr(wrk_space)+' B';
Form_pea.LabelDecrypt3.Caption:='Output: '+out_path+out_file;
Form_pea.LabelDecrypt5.Caption:='Volumes merged succesfully';
Form_pea.ProgressBar1.Position:=100;
usetcurrentdir(uextractfilepath(out_param));
do_report_rfj;
timing(ts_start,wrk_space);
Form_pea.LabelOpen.Caption:='Open';
output:=out_path+out_file;
Form_pea.LabelOpen.visible:=true;
Form_pea.LabelLog1.Visible:=true;
Form_pea.ButtonDone1.Visible:=true;
if (upcase(pw_param)='INTERACTIVE_REPORT') or (upcase(pw_param)='BATCH_REPORT') or (upcase(pw_param)='HIDDEN_REPORT') then save_report('Auto log Raw File Join');
Sleep(500);
if closepolicy>0 then Form_pea.Close;
end;

//procedure to wipe files and folders
procedure wipe ( level: utf8string);
//NONE: delete (quick delete: no overwrite, not sent to recycle bin)
//ZERO: overwrite with zero, flush, delete
//VERY_FAST: 1 * (random data overwrite, flush), cloack file size <4KB, 1 * (rename, flush), delete
//FAST: 2 * (random data overwrite, flush), cloack file size <4KB*2, 2 * (rename, flush), delete
//MEDIUM: 4 * (random data overwrite, flush), cloack file size <4KB*4, 4 * (rename, flush), delete
//SLOW: 8 * (random data overwrite, flush), cloack file size <4KB*8, 8 * (rename, flush), delete
//VERY_SLOW: 16 * (random data overwrite, flush), cloack file size <4KB*16, 16 * (rename, flush), delete
var
   f:file of byte;
   exp_files:TFoundList;
   exp_dirs:TFoundList;
   exp_fsizes:TFoundListSizes;
   exp_ftimes:TFoundListAges;
   exp_fattr:TFoundListAttrib;
   exp_fattr_dec:TFoundList;
   nfound,size,total,ntotalexp:qword;
   i,j,k,errors,dfiles,ddirs,numread,numwritten,nlevel,rc,progressive,attr:integer;
   buf:array[0..65535]of byte;
   aes_ctx:TAESContext;
   aes_iv:array[0..15]of byte;
   randomstring,randomstring2,oldrandomstring:UTF8string;
begin
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
Form_report.Caption:='Secure delete';
Form_pea.Caption:='Secure delete';
Form_pea.LabelTools2.Caption:=inttostr(paramcount-2)+' items(s) sent to '+level+' secure deletion';
Form_pea.ProgressBar1.Position:=0;
Form_pea.PanelTools.Cursor:=crHourGlass;
errors:=0;
dfiles:=0;
ddirs:=0;
ntotalexp:=paramcount-2;
level:=upcase(level);
case level of
   'NONE' : nlevel:=0;
   'ZERO' : nlevel:=1;
   'VERY_FAST' : nlevel:=1;
   'FAST' : nlevel:=2;
   'MEDIUM' : nlevel:=4;
   'SLOW' : nlevel:=8;
   else nlevel:=16;
   end;
progressive:=100 div (paramcount-1);//n objects+1
if progressive=0 then progressive:=1;
randomize;
for j:=3 to paramcount do
   begin
   Form_pea.ProgressBar1.Position:=progressive*(j-2);
   if Form_pea.ProgressBar1.Position>=100 then Form_pea.ProgressBar1.Position:=99;
   try
      expand(envtoutf8(paramstr(j)),exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,nfound);
      if nfound=0 then nfound:=1;
      SetLength(exp_dirs,0);
      ntotalexp:=ntotalexp+nfound-1;
      for k:=0 to nfound-1 do
         begin
         if ufilegetattr(exp_files[k]) and faDirectory = 0 then //file
            begin
            rc:=Form_report.StringGrid1.RowCount+1;
            Form_report.StringGrid1.RowCount:=rc;
            Form_pea.LabelTools3.Caption:='Processing object '+inttostr(rc)+' (file) / '+inttostr(ntotalexp);
            Application.ProcessMessages;
            try
               {$IFDEF MSWINDOWS}
               attr:=ufilegetattr(exp_files[k]);
               attr:=attr and (not faReadOnly);
               attr:=attr and (not faSysFile);
               ufilesetattr(exp_files[k],attr);
               {$ENDIF}
               uassignfile(f,exp_files[k]);
               filemode:=0;
               reset(f);
               srcfilesize(exp_files[k],size);
               closefile(f);
               usetcurrentdir(uextractfilepath((exp_files[k])));
               case level of
               'NONE': udeletefile(exp_files[k]);//quick delete
               'ZERO': //overwrite with zero
               begin
                  uassignfile(f,(exp_files[k]));
                  rewrite(f);
                  total:=0;
                  FillByte(buf,sizeof(buf),0);
                  repeat
                     if size-total>65536 then numread:=65536
                     else numread:=size-total;
                     blockwrite(f,buf,numread,numwritten);
                     inc(total,numwritten);
                  until (total<=size);
                  closefile(f);//causes flush;
                  udeletefile(exp_files[k]);
               end;
               else // secure delete
               begin
               get_fingerprint(fingerprint,false);
               //init encryption
               for i:=0 to 31 do fingerprint[i]:=fingerprint[i];
               for i:=0 to 15 do aes_iv[i]:=fingerprint[i]+random(256);
               AES_CTR_Init(fingerprint, 256, aes_iv, aes_ctx);
               for i:=1 to nlevel do //overwrite nlevel times with random data (AES256 CTR init once by system fingerprint)
                  begin
                  uassignfile(f,(exp_files[k]));
                  rewrite(f);
                  total:=0;
                  repeat
                     if size-total>65536 then numread:=65536
                     else numread:=size-total;
                     AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
                     blockwrite(f,buf,numread,numwritten);
                     inc(total,numwritten);
                  until (total<=size);
                  closefile(f);//causes flush;
                  end;
               numread:=1+random(4096*i);//replace file with random sized block 1B-(4KB*i) to cloak original size
               AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
               uassignfile(f,(exp_files[k]));
               rewrite(f);
               blockwrite(f,buf,numread,numwritten);
               closefile(f);
               randomstring:=(exp_files[k]);
               for i:=1 to nlevel do //rename
                  begin
                  oldrandomstring:=randomstring;
                  uassignfile(f,randomstring);
                  randomstring:=uextractfilepath(randomstring)+inttostr(random(maxint))+'.tmp';
                  urenamefilebyname(oldrandomstring,randomstring);
                  end;
               udeletefile(randomstring);
               end;
               end;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='file successfully deleted';
               dfiles:=dfiles+1;
            except
               try closefile(f); except end;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='FILE NOT DELETED (not writeable/accessible/found)';
               errors:=errors+1;
            end;
            end
         else
            begin
            if not(ufileexists(exp_files[k])) and not(udirectoryexists(exp_files[k])) then //not found
               begin
               rc:=Form_report.StringGrid1.RowCount+1;
               Form_report.StringGrid1.RowCount:=rc;
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_files[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='OBJECT NOT FOUND';
               errors:=errors+1;
               end
            else
               begin
               SetLength(exp_dirs,length(exp_dirs)+1);
               exp_dirs[length(exp_dirs)-1]:=exp_files[k];
               end;
            end;
      end;
      usetcurrentdir(executable_path);
      if length(exp_dirs)>0 then
         for k:=(length(exp_dirs)-1) downto 0 do
            begin
            rc:=Form_report.StringGrid1.RowCount+1;
            Form_report.StringGrid1.RowCount:=rc;
            Form_pea.LabelTools3.Caption:='Processing object '+inttostr(rc)+' (directory) / '+inttostr(ntotalexp);
            Application.ProcessMessages;
            try
               randomstring:=(exp_dirs[k]);
               for i:=1 to nlevel do //rename
                  begin
                  if randomstring[length(randomstring)]=directoryseparator then setlength(randomstring,length(randomstring)-1);
                  randomstring2:=extractfilepath(randomstring)+inttostr(random(maxint));
                  urenamefilebyname(randomstring,randomstring2);
                  randomstring:=randomstring2;
                  end;
               urmdir(randomstring);
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_dirs[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='directory successfully deleted';
               ddirs:=ddirs+1;
            except
               Form_report.StringGrid1.Cells[0,rc-1]:=exp_dirs[k];
               Form_report.StringGrid1.Cells[1,rc-1]:='DIRECTORY NOT DELETED (not writeable/accessible/found)';
               errors:=errors+1;
            end;
            end;
    except
       rc:=Form_report.StringGrid1.RowCount+1;
       Form_report.StringGrid1.RowCount:=rc;
       Form_report.StringGrid1.Cells[0,Form_report.StringGrid1.RowCount-1]:=envtoutf8(paramstr(j));
       Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='OBJECT NOT DELETED';
       errors:=errors+1;
   end;
   end;
Form_pea.ProgressBar1.Position:=100;
Form_pea.PanelTools.Cursor:=crDefault;
Form_report.StringGrid1.Cells[0,0]:='File';
Form_report.StringGrid1.Cells[1,0]:='Result';
Form_report.StringGrid1.AutosizeColumns;
Form_pea.LabelTools3.Caption:='Deleted '+inttostr(dfiles)+' files and '+inttostr(ddirs)+' directories';
Form_pea.LabelTools4.Caption:=inttostr(errors)+' errors';
Form_report.Label1.Caption:='Secure deletion';
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
if errors=0 then
   begin
   Sleep(500);
   if closepolicy>0 then Form_pea.Close;
   end;
end;

//procedure to wipe/sanitize free space
procedure sanitize ( level: utf8string);
//ZERO: overwrite free space with zero, flush
//VERY_FAST: 1 * (random data overwrite filling free space, flush, delete work files)
//FAST: 2 * (random data overwrite filling free space, flush, delete work files)
//MEDIUM: 4 * (random data overwrite filling free space, flush, delete work files)
//SLOW: 8 * (random data overwrite filling free space, flush, delete work files)
//VERY_SLOW: 16 * (random data overwrite filling free space, flush, delete work files)

procedure recoverfreespace(n:integer);
var m:integer;
begin
try
   for m:=1 to n do udeletefile(paramstr(3)+'.ptmp'+directoryseparator+inttostr(m));
except
   sleep(500);
   try
      for m:=1 to n do udeletefile(paramstr(3)+'.ptmp'+directoryseparator+inttostr(m));
   except
      MessageDlg('Cannot delete temporary work files, please manually delete '+paramstr(3)+'.ptmp'+directoryseparator+' to recover free space', mtWarning, [mbOK], 0);
   end;
end;
end;

var
   f:file of byte;
   total,gtotal,maxs:qword;
   n,i,j,numread,numwritten,nlevel,rc,drivenumber,time:integer;
   buf:array[0..65535]of byte;
   aes_ctx:TAESContext;
   aes_iv:array[0..15]of byte;
   wrkfile,wrktitle:UTF8string;
   sizefree,sizetotal,rfree,speed:qword;
   tok:boolean;
   tsin,tsout:TTimestamp;
begin
{$IFDEF MSWINDOWS}
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
level:=upcase(level);
case level of
   'ZERO' : wrktitle:='Zero delete free space';
   else wrktitle:='Secure delete free space, '+level;
   end;
Form_report.Caption:=wrktitle;
Form_pea.Caption:=wrktitle;
Form_pea.ProgressBar1.Position:=0;
Form_pea.PanelTools.Cursor:=crHourGlass;
case level of
   'ZERO' : nlevel:=1;
   'VERY_FAST' : nlevel:=1;
   'FAST' : nlevel:=2;
   'MEDIUM' : nlevel:=4;
   'SLOW' : nlevel:=8;
   else nlevel:=16;
   end;
randomize;
Form_report.StringGrid1.RowCount:=1;
Application.ProcessMessages;
drivenumber:=ord(upcase(paramstr(3)[1]))-64;
if drivenumber>2 then sizefree:=diskfree(drivenumber);
if drivenumber>2 then sizetotal:=DiskSize(drivenumber);
maxs:=2*1024*1024*1024-1;//2GiB-1B
uForceDirectories(paramstr(3)+'.ptmp');
tsin:=datetimetotimestamp(now);
case level of
   'ZERO': //overwrite with zero
   begin
   Form_pea.LabelTools2.Caption:='Processing drive '+envtoutf8(paramstr(3))+', '+nicenumber(inttostr(sizetotal));
   FillByte(buf,sizeof(buf),0);
   uForceDirectories(paramstr(3)+'.ptmp');
   tok:=false;
   gtotal:=0;
   n:=0;
   repeat
      total:=0;
      n:=n+1;
      wrkfile:=paramstr(3)+'.ptmp'+directoryseparator+inttostr(n);
      uassignfile(f,wrkfile);
      rewrite(f);
      try
      repeat
         if maxs-total>65536 then numread:=65536
         else numread:=maxs-total;
         rfree:=diskfree(drivenumber);
         Form_pea.ProgressBar1.Position:=100-((rfree*100) div sizefree);
         Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree))+' free, '+nicenumber(inttostr(rfree))+' remaining';
         tsout:=datetimetotimestamp(now);
         time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
         Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
         Application.ProcessMessages;
         if rfree<=65536 then tok:=true
         else
            begin
            blockwrite(f,buf,numread,numwritten);
            inc(total,numwritten);
            end;
      until (tok=true) or (total>=maxs);
      finally
         closefile(f);//causes flush;
         inc(gtotal,total);
      end;
   until tok=true;
   recoverfreespace(n);
   Form_pea.LabelTools2.Caption:='Done drive '+envtoutf8(paramstr(3))+', '+nicenumber(inttostr(sizetotal));
   end;
   else // secure delete
   begin
   get_fingerprint(fingerprint,false);
   //init encryption
   for i:=0 to 31 do fingerprint[i]:=fingerprint[i];
   for i:=0 to 15 do aes_iv[i]:=fingerprint[i]+random(256);
   AES_CTR_Init(fingerprint, 256, aes_iv, aes_ctx);
   //overwrite nlevel times with random data (AES256 CTR init once by system fingerprint)
   for j:=1 to nlevel do
   begin
   Form_pea.LabelTools2.Caption:='Processing drive '+envtoutf8(paramstr(3))+', '+nicenumber(inttostr(sizetotal))+', pass '+inttostr(j)+' of '+inttostr(nlevel);
   tok:=false;
   gtotal:=0;
   n:=0;
   repeat
      total:=0;
      n:=n+1;
      wrkfile:=paramstr(3)+'.ptmp'+directoryseparator+inttostr(n);
      uassignfile(f,wrkfile);
      rewrite(f);
      try
      repeat
         if maxs-total>65536 then numread:=65536
         else numread:=maxs-total;
         rfree:=diskfree(drivenumber);
         Form_pea.ProgressBar1.Position:=100-((rfree*100) div sizefree);
         Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree))+' free, '+nicenumber(inttostr(rfree))+' remaining';
         tsout:=datetimetotimestamp(now);
         time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
         Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' elapsed';
         Application.ProcessMessages;
         if rfree<=65536 then tok:=true
         else
            begin
            AES_CTR_Encrypt(@buf, @buf, numread, aes_ctx);
            blockwrite(f,buf,numread,numwritten);
            inc(total,numwritten);
            end;
      until (tok=true) or (total>=maxs);
      finally
         closefile(f);//causes flush;
         inc(gtotal,total);
      end;
   until tok=true;
   recoverfreespace(n);
   Form_pea.LabelTools2.Caption:='Done drive '+envtoutf8(paramstr(3))+', '+nicenumber(inttostr(sizetotal))+', pass '+inttostr(j)+' of '+inttostr(nlevel);
   end;
   end;
end;
urmdir(paramstr(3)+'.ptmp');
Form_pea.ProgressBar1.Position:=100;
Form_pea.PanelTools.Cursor:=crDefault;
Form_report.StringGrid1.Cells[0,0]:='File';
Form_report.StringGrid1.Cells[1,0]:='Result';
Form_report.StringGrid1.AutosizeColumns;
rfree:=diskfree(drivenumber);
Form_pea.LabelTools3.Caption:=nicenumber(inttostr(sizefree))+' free when task started, '+nicenumber(inttostr(rfree))+' currently free';
tsout:=datetimetotimestamp(now);
time:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
speed:=(sizefree * 1000) div time;
Form_pea.LabelTools4.Caption:=nicetime(inttostr(time))+' total time @ '+nicenumber(inttostr(speed))+'/s';
Form_report.Label1.Caption:=wrktitle;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
{if errors=0 then
   begin
   Sleep(500);
   if closepolicy>0 then Form_pea.Close;
   end;}
{$ENDIF}
end;

procedure set_form_report_heigths;
begin
with Form_report do
   begin
   StringGrid1.DefaultRowHeight:=rowheight;
   StringGrid2.DefaultRowHeight:=rowheight;
   Button1.Height:=stdbtnheight;
   Button2.Height:=stdbtnheight;
   end;
end;

//procedure to compare files
procedure compare;
var
   fa,fb:file of byte;
   sizea,sizeb,sizemin,total:qword;
   i,d,x,numreada,numreadb,numreadmin,continue:integer;
   bufa,bufb:array[0..65535]of byte;
label 1;
begin
Form_pea.PanelPW1.height:=2;
Form_report.visible:=true;
set_form_report_heigths;
Form_report.Notebook1.PageIndex:=0;
Form_report.StringGrid1.RowCount:=1;
Form_report.Caption:='Compare';
Form_pea.Caption:='Byte to byte compare';
Form_pea.LabelTools2.Caption:='Comparing files...';
Form_pea.LabelTools3.Caption:='First file: '+envtoutf8(paramstr(2));
Form_pea.LabelTools4.Caption:='Second file: '+envtoutf8(paramstr(3));
Form_pea.ProgressBar1.Position:=0;
Form_report.StringGrid1.RowCount:=2;
Form_report.StringGrid1.Cells[0,0]:='Test';
Form_report.StringGrid1.Cells[1,0]:='Result';
continue:=0;
total:=0;
d:=0;
try
uassignfile(fa,envtoutf8(paramstr(2)));
filemode:=0;
reset(fa);
srcfilesize(envtoutf8(paramstr(2)),sizea);
//sizea:=system.filesize(fa);
usetcurrentdir(uextractfilepath(envtoutf8(paramstr(2))));
uassignfile(fb,envtoutf8(paramstr(3)));
filemode:=0;
reset(fb);
srcfilesize(envtoutf8(paramstr(3)),sizeb);
//sizeb:=system.filesize(fb);
if sizea=0 then total:=2;
if sizeb=0 then total:=2;
except
total:=1;
end;
if total<>0 then
   begin
   Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
   Form_report.StringGrid1.Cells[0,1]:='Error';
   Form_report.StringGrid1.Cells[1,1]:='Cannot compare files';
   if paramstr(2)=paramstr(3) then
      Form_pea.LabelTools2.Caption:='Cannot compare a file with itself!'
   else
      begin
      Form_pea.LabelTools2.Caption:='Cannot compare files';
      if total=2 then
         begin
         if sizea=0 then Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' first file is empty';
         if sizeb=0 then Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' second file is empty';
         end
      else
         Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' (i.e. not accessible, not files etc)';
      end;
   Form_pea.ProgressBar1.Position:=100;
   Form_report.StringGrid1.AutosizeColumns;
   Form_report.Label1.Caption:=Form_pea.Caption;
   Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
   Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
   Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
   {$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
   Form_pea.ButtonDone1.Visible:=true;
   Form_pea.LabelOpen.Visible:=true;
   Form_pea.LabelOpen.Enabled:=false;
   Form_pea.LabelLog1.Visible:=true;
   exit;
   end;
if sizeb<sizea then sizemin:=sizeb
else sizemin:=sizea;
if sizea=sizeb then
   begin
   Form_report.StringGrid1.Cells[0,1]:='Size comparison';
   Form_report.StringGrid1.Cells[1,1]:='Files have same size: '+inttostr(sizea)+' B';
   x:=1;
   end
else
   begin
   Form_report.StringGrid1.Cells[0,1]:='Size comparison';
   Form_report.StringGrid1.Cells[1,1]:='Files have different sizes';
   Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+3;
   Form_report.StringGrid1.Cells[0,2]:='- First file';
   Form_report.StringGrid1.Cells[1,2]:=inttostr(sizea)+' B';
   Form_report.StringGrid1.Cells[0,3]:='- Second file';
   Form_report.StringGrid1.Cells[1,3]:=inttostr(sizeb)+' B';
   Form_report.StringGrid1.Cells[0,4]:='- Size difference';
   if sizea>sizeb then Form_report.StringGrid1.Cells[1,4]:=inttostr(sizea-sizeb)+' B'
   else Form_report.StringGrid1.Cells[1,4]:=inttostr(sizeb-sizea)+' B';
   x:=4;
   end;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption+' ('+inttostr(sizea)+' B)';
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption+' ('+inttostr(sizeb)+' B)';
Form_pea.Visible:=false;
repeat
   blockread (fa,bufa,65536,numreada);
   blockread (fb,bufb,65536,numreadb);
   if numreadb<numreada then numreadmin:=numreadb
   else numreadmin:=numreada;
   for i:=0 to (numreadmin - 1) do
      begin
      if bufa[i]=bufb[i] then
      else
         begin
         d:=d+1;
         Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
         Form_report.StringGrid1.Cells[0,d+x]:='Offset '+system.hexstr(((filepos(fa)-numreada+i)div 16)*16,8);
         Form_report.StringGrid1.Cells[1,d+x]:='Byte '+inttostr(filepos(fa)-numreada+i+1)+' is different: Hex '+hexstr(@bufa[i],1)+' vs '+hexstr(@bufb[i],1)+'; Dec '+inttostr(bufa[i])+' vs '+inttostr(bufb[i]);
         if ((d>=100) and (continue=0)) then
            begin
            continue:=1;
            if MessageDlg('More than 100 different bytes, continue anyway?',mtConfirmation,[mbYes, mbNo],0)=6 then continue:=1
            else
               begin
               Form_report.StringGrid1.Cells[0,d+x]:='Comparison terminated by user';
               Form_report.StringGrid1.Cells[1,d+x]:='More than 100 different bytes';
               goto 1;
               end;
            end;
         if (d>=10000) then
            begin
            Form_report.StringGrid1.Cells[0,d+x]:='Comparison automatically terminated';
            Form_report.StringGrid1.Cells[1,d+x]:='More than 10000 different bytes';
            goto 1;
            end;
         end;
      end;
   inc(total,numreadmin);
   Form_pea.ProgressBar1.Position:=(total*100) div sizemin;
   Application.ProcessMessages;
until ((numreada=0) or (numreadb=0));
1:
closefile(fa);
closefile(fb);
Form_report.StringGrid1.RowCount:=Form_report.StringGrid1.RowCount+1;
Form_report.StringGrid1.Cells[0,Form_report.StringGrid1.RowCount-1]:='Byte comparison';
if d=0 then
   if sizea=sizeb then
      begin
      Form_pea.Caption:='Files are identical';
      Form_pea.LabelTools2.Caption:='Same size, no different byte';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='No different byte';
      end
   else
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Different size, no different byte in the shortest file ('+inttostr(sizemin)+' B)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:='No different byte in the shortest file ('+inttostr(sizemin)+' B)';
      end
else
   if sizea=sizeb then
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Same size, '+inttostr(d)+' different byte(s)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:=inttostr(d)+' different byte(s)';
      end
   else
      begin
      Form_pea.Caption:='Files are different';
      Form_pea.LabelTools2.Caption:='Different size, '+inttostr(d)+' different byte(s) in the shortest file ('+inttostr(sizemin)+' B)';
      Form_report.StringGrid1.Cells[1,Form_report.StringGrid1.RowCount-1]:=inttostr(d)+' different byte(s) in the shortest file ('+inttostr(sizemin)+' B)';
      end;
Form_pea.ProgressBar1.Position:=100;
Form_report.StringGrid1.AutosizeColumns;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
end;

//procedure to checksum/hash files
procedure check; //ALL: all algorithms, otherwise specify the algorith to use followed by "on" which marks the input parameters
var
   sbuf:array [1..32767] of byte;
   i,j,n,t:integer;
   k,f_size:qword;
   mode:utf8string;
   pgpsig:TPGPDigest;
   Adler:longint;
   CRC16:word;
   CRC24:longint;
   CRC32:longint;
   CRC64:TCRC64;
   ED2KContext:TED2KContext;
   ED2KRes:TED2KResult;
   MD4Context:THashContext;
   MD4Digest:TMD4Digest;
   MD5Context:THashContext;
   MD5Digest:TMD5Digest;
   RMD160Context:THashContext;
   RMD160Digest:TRMD160Digest;
   SHA1Context:THashContext;
   SHA1Digest:TSHA1Digest;
   SHA224Context:THashContext;
   SHA224Digest:TSHA224Digest;
   SHA256Context:THashContext;
   SHA256Digest:TSHA256Digest;
   SHA384Context:THashContext;
   SHA384Digest:TSHA384Digest;
   SHA512Context:THashContext;
   SHA512Digest:TSHA512Digest;
   WhirlContext:THashContext;
   WhirlDigest:TWhirlDigest;
   Adler32_on,CRC16_on,CRC24_on,CRC32_on,CRC64_on,ED2K_on,MD4_on,MD5_on,RIPEMD160_on,
   SHA1_on,SHA224_on,SHA256_on,SHA384_on,SHA512_on,WHIRLPOOL_on:boolean;
   f:file of byte;
begin
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.Caption:='Checksum and hash';
Form_pea.Caption:='Checksum and hash';
Form_pea.LabelTools2.Caption:='Checking file(s)...';
Form_pea.ProgressBar1.Position:=0;
Form_report.Input.Caption:='Input';
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_report.StringGrid1.ColCount:=18;
Form_report.StringGrid1.Cells[0,0]:='File name';
Form_report.StringGrid1.Cells[1,0]:='Size (B)';
Form_report.StringGrid1.Cells[2,0]:='Modified';
Form_report.StringGrid1.Cells[3,0]:='Adler32';
Form_report.StringGrid1.Cells[4,0]:='CRC16';
Form_report.StringGrid1.Cells[5,0]:='CRC24';
Form_report.StringGrid1.Cells[6,0]:='CRC32';
Form_report.StringGrid1.Cells[7,0]:='CRC64';
Form_report.StringGrid1.Cells[8,0]:='eDonkey';
Form_report.StringGrid1.Cells[9,0]:='MD4';
Form_report.StringGrid1.Cells[10,0]:='MD5';
Form_report.StringGrid1.Cells[11,0]:='RIPEMD160';
Form_report.StringGrid1.Cells[12,0]:='SHA1';
Form_report.StringGrid1.Cells[13,0]:='SHA224';
Form_report.StringGrid1.Cells[14,0]:='SHA256';
Form_report.StringGrid1.Cells[15,0]:='SHA384';
Form_report.StringGrid1.Cells[16,0]:='SHA512';
Form_report.StringGrid1.Cells[17,0]:='Whirlpool';
//read output mode HEX or BASE64
if upcase(paramstr(2))='HEX' then mode:='HEX'
else
   if upcase(paramstr(2))='LSBHEX' then mode:='LSBHEX'
   else
      if upcase(paramstr(2))='BASE64' then mode:='BASE64'
      else
         begin
         showmessage('Mode '+paramstr(2)+' is not valid, use HEX to see output coded as hexadecimal, LSBHEX for LSB hexadecimal or BASE64 for output coded in BASE64');
         Application.Terminate;
         end;
Form_pea.LabelTools3.Caption:='Display results as '+mode;
//read algorithms to be used
j:=3;
Adler32_on:=false;
CRC16_on:=false;
CRC24_on:=false;
CRC32_on:=false;
CRC64_on:=false;
ED2K_on:=false;
MD4_on:=false;
MD5_on:=false;
RIPEMD160_on:=false;
SHA1_on:=false;
SHA224_on:=false;
SHA256_on:=false;
SHA384_on:=false;
SHA512_on:=false;
WHIRLPOOL_on:=false;
repeat
   case upcase(paramstr(j)) of
   'ADLER32': Adler32_on:=true;
   'CRC16': CRC16_on:=true;
   'CRC24': CRC24_on:=true;
   'CRC32': CRC32_on:=true;
   'CRC64': CRC64_on:=true;
   'ED2K': ED2K_on:=true;
   'MD4': MD4_on:=true;
   'MD5': MD5_on:=true;
   'RIPEMD160': RIPEMD160_on:=true;
   'SHA1': SHA1_on:=true;
   'SHA224': SHA224_on:=true;
   'SHA256': SHA256_on:=true;
   'SHA384': SHA384_on:=true;
   'SHA512': SHA512_on:=true;
   'WHIRLPOOL': WHIRLPOOL_on:=true;
   'ALL':
   begin
      Adler32_on:=true;
      CRC16_on:=true;
      CRC24_on:=true;
      CRC32_on:=true;
      CRC64_on:=true;
      ED2K_on:=true;
      MD4_on:=true;
      MD5_on:=true;
      RIPEMD160_on:=true;
      SHA1_on:=true;
      SHA224_on:=true;
      SHA256_on:=true;
      SHA384_on:=true;
      SHA512_on:=true;
      WHIRLPOOL_on:=true;
   end;
   end;
   j:=j+1;
until ((upcase(paramstr(j-1))='ON') or (j>paramcount));
if j=4 then
   begin
   showmessage('No algorithm received');
   Application.Terminate;
   end;
if j>paramcount then
   begin
   showmessage('No input file received');
   Application.Terminate;
   end;
//perform checks
Form_report.StringGrid1.Rowcount:=paramcount-j+2;
t:=0;
for i:=j to paramcount do
   begin
   Form_pea.ProgressBar1.Position:=0;
   Form_pea.LabelTools2.Caption:='Checking file(s) '+inttostr(i-j+1)+' of '+inttostr(paramcount-j+1);
   if i=j then usetcurrentdir(uextractfilepath(envtoutf8(paramstr(i)))); //set path same as the first input file (for saving the report in)
   filemode:=0;
   try
   uassignfile(f,envtoutf8(paramstr(i)));
   filemode:=0;
   reset(f);
   srcfilesize(envtoutf8(paramstr(i)),f_size);
   //f_size:=system.filesize(f);
   except
   Form_report.StringGrid1.Cells[0,i-j+1]:=envtoutf8(paramstr(i))+' (cannot be checked)';
   continue;
   end;
   k:=0;
   t:=t+1;
   RMD160Init(RMD160Context);
   SHA1Init(SHA1Context);
   SHA256Init(SHA256Context);
   SHA224Init(SHA224Context);
   SHA384Init(SHA384Context);
   SHA512Init(SHA512Context);
   Whirl_Init(WhirlContext);
   ED2K_Init(ED2KContext);
   MD4Init(MD4Context);
   MD5Init(MD5Context);
   CRC16Init(CRC16);
   CRC24Init(CRC24);
   CRC32Init(CRC32);
   Adler32Init(adler);
   CRC64Init(CRC64);
   repeat
      blockread(f,sbuf,sizeof(sbuf),n);
      if n<>0 then
         begin
         inc(k,n);
         if Adler32_on then Adler32Update(adler,@sbuf,n);
         if CRC16_on then CRC16Update(CRC16,@sbuf,n);
         if CRC24_on then CRC24Update(CRC24,@sbuf,n);
         if CRC32_on then CRC32Update(CRC32,@sbuf,n);
         if CRC64_on then CRC64Update(CRC64,@sbuf,n);
         if ED2K_on then ED2K_Update(ED2KContext,@sbuf,n);
         if MD4_on then MD4Update(MD4Context,@sbuf,n);
         if MD5_on then MD5Update(MD5Context,@sbuf,n);
         if RIPEMD160_on then RMD160Update(RMD160Context,@sbuf,n);
         if SHA1_on then SHA1Update(SHA1Context,@sbuf,n);
         if SHA224_on then SHA224Update(SHA224Context,@sbuf,n);
         if SHA256_on then SHA256Update(SHA256Context,@sbuf,n);
         if SHA384_on then SHA384Update(SHA384Context,@sbuf,n);
         if SHA512_on then SHA512Update(SHA512Context,@sbuf,n);
         if Whirlpool_on then Whirl_Update(WhirlContext,@sbuf,n);
         Form_pea.ProgressBar1.Position:=(k*100) div f_size;
         Application.ProcessMessages;
         end;
   until n<>sizeof(sbuf);
   close(f);
   if Adler32_on then Adler32Final(adler);
   if CRC16_on then CRC16Final(CRC16);
   if CRC24_on then CRC24Final(CRC24);  Long2PGP(CRC24, pgpsig);
   if CRC32_on then CRC32Final(CRC32);
   if CRC64_on then CRC64Final(CRC64);
   if ED2K_on then ED2K_Final(ED2KContext,ED2KRes);
   if MD4_on then MD4Final(MD4Context,MD4Digest);
   if MD5_on then MD5Final(MD5Context,MD5Digest);
   if RIPEMD160_on then RMD160Final(RMD160Context,RMD160Digest);
   if SHA1_on then SHA1Final(SHA1Context,SHA1Digest);
   if SHA224_on then SHA224Final(SHA224Context,SHA224Digest);
   if SHA256_on then SHA256Final(SHA256Context,SHA256Digest);
   if SHA384_on then SHA384Final(SHA384Context,SHA384Digest);
   if SHA512_on then SHA512Final(SHA512Context,SHA512Digest);
   if Whirlpool_on then Whirl_Final(WhirlContext,WhirlDigest);
   Form_report.StringGrid1.Cells[0,i-j+1]:=envtoutf8(paramstr(i));
   Form_report.StringGrid1.Cells[1,i-j+1]:=inttostr(f_size);
   Form_report.StringGrid1.Cells[2,i-j+1]:=datetimetostr(filedatetodatetime(ufileage(envtoutf8(paramstr(i)))));
   if ((mode='HEX') or (mode='LSBHEX')) then
      begin
      if mode ='HEX' then
         begin
         if CRC16_on then CRC16 := swap(CRC16);
         if CRC24_on then Form_report.StringGrid1.Cells[5,i-j+1]:=hexstr(@pgpsig,sizeof(pgpsig));
         CRC32 := (CRC32 shr 24) or ((CRC32 shr 8) and $FF00) or ((CRC32 shl 8) and $FF0000) or (CRC32 shl 24);
         Adler := (Adler shr 24) or ((Adler shr 8) and $FF00) or ((Adler shl 8) and $FF0000) or (Adler shl 24);
         end
      else
         begin
         if CRC24_on then Form_report.StringGrid1.Cells[5,i-j+1]:=hexstr(@CRC24,sizeof(CRC24));
         end;
      if Adler32_on then Form_report.StringGrid1.Cells[3,i-j+1]:=upcase(hexstr(@adler,sizeof(Adler)));
      if CRC16_on then Form_report.StringGrid1.Cells[4,i-j+1]:=upcase(hexstr(@CRC16,sizeof(CRC16)));
      if CRC32_on then Form_report.StringGrid1.Cells[6,i-j+1]:=upcase(hexstr(@CRC32,sizeof(CRC32)));
      if CRC64_on then Form_report.StringGrid1.Cells[7,i-j+1]:=upcase(hexstr(@CRC64,sizeof(CRC64)));
      if ED2K_on then
         begin
         Form_report.StringGrid1.Cells[8,i-j+1]:=upcase(hexstr(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey)));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[8,i-j+1]:=Form_report.StringGrid1.Cells[8,i-j+1]+' / eMule: '+upcase(hexstr(@ED2KRes.eMule, sizeof(ED2KRes.eMule)));
         end;
      if MD4_on then Form_report.StringGrid1.Cells[9,i-j+1]:=upcase(hexstr(@MD4Digest,sizeof(MD4Digest)));
      if MD5_on then Form_report.StringGrid1.Cells[10,i-j+1]:=upcase(hexstr(@MD5Digest,sizeof(MD5Digest)));
      if RIPEMD160_on then Form_report.StringGrid1.Cells[11,i-j+1]:=upcase(hexstr(@RMD160Digest,sizeof(RMD160Digest)));
      if SHA1_on then Form_report.StringGrid1.Cells[12,i-j+1]:=upcase(hexstr(@SHA1Digest,sizeof(SHA1Digest)));
      if SHA224_on then Form_report.StringGrid1.Cells[13,i-j+1]:=upcase(hexstr(@SHA224Digest,sizeof(SHA224Digest)));
      if SHA256_on then Form_report.StringGrid1.Cells[14,i-j+1]:=upcase(hexstr(@SHA256Digest,sizeof(SHA256Digest)));
      if SHA384_on then Form_report.StringGrid1.Cells[15,i-j+1]:=upcase(hexstr(@SHA384Digest,sizeof(SHA384Digest)));
      if SHA512_on then Form_report.StringGrid1.Cells[16,i-j+1]:=upcase(hexstr(@SHA512Digest,sizeof(SHA512Digest)));
      if Whirlpool_on then Form_report.StringGrid1.Cells[17,i-j+1]:=upcase(hexstr(@WhirlDigest,sizeof(WhirlDigest)));
      end
   else
      begin
      if Adler32_on then Form_report.StringGrid1.Cells[3,i-j+1]:=base64str(@adler,sizeof(Adler));
      if CRC16_on then Form_report.StringGrid1.Cells[4,i-j+1]:=base64str(@CRC16,sizeof(CRC16));
      if CRC24_on then Form_report.StringGrid1.Cells[5,i-j+1]:=base64str(@pgpsig,sizeof(CRC24));
      if CRC32_on then Form_report.StringGrid1.Cells[6,i-j+1]:=base64str(@CRC32,sizeof(CRC32));
      if CRC64_on then Form_report.StringGrid1.Cells[7,i-j+1]:=base64str(@CRC64,sizeof(CRC64));
      if ED2K_on then
         begin
         Form_report.StringGrid1.Cells[8,i-j+1]:=base64str(@ED2KRes.eDonkey, sizeof(ED2KRes.eDonkey));
         if ED2KRes.differ then Form_report.StringGrid1.Cells[8,i-j+1]:=Form_report.StringGrid1.Cells[8,i-j+1]+' / eMule: '+base64str(@ED2KRes.eMule, sizeof(ED2KRes.eMule));
         end;
      if MD4_on then Form_report.StringGrid1.Cells[9,i-j+1]:=base64str(@MD4Digest,sizeof(MD4Digest));
      if MD5_on then Form_report.StringGrid1.Cells[10,i-j+1]:=base64str(@MD5Digest,sizeof(MD5Digest));
      if RIPEMD160_on then Form_report.StringGrid1.Cells[11,i-j+1]:=base64str(@RMD160Digest,sizeof(RMD160Digest));
      if SHA1_on then Form_report.StringGrid1.Cells[12,i-j+1]:=base64str(@SHA1Digest,sizeof(SHA1Digest));
      if SHA224_on then Form_report.StringGrid1.Cells[13,i-j+1]:=base64str(@SHA224Digest,sizeof(SHA224Digest));
      if SHA256_on then Form_report.StringGrid1.Cells[14,i-j+1]:=base64str(@SHA256Digest,sizeof(SHA256Digest));
      if SHA384_on then Form_report.StringGrid1.Cells[15,i-j+1]:=base64str(@SHA384Digest,sizeof(SHA384Digest));
      if SHA512_on then Form_report.StringGrid1.Cells[16,i-j+1]:=base64str(@SHA512Digest,sizeof(SHA512Digest));
      if Whirlpool_on then Form_report.StringGrid1.Cells[17,i-j+1]:=base64str(@WhirlDigest,sizeof(WhirlDigest));
      end;
   end;
Form_report.StringGrid1.AutosizeColumns;
Form_pea.ProgressBar1.Position:=100;
Form_pea.LabelTools2.Caption:='Checked successfully '+inttostr(t)+' of '+inttostr(paramcount-j+1)+' file(s)';
Form_report.StringGrid1.AutosizeColumns;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Form_report.Visible:=true;
Form_pea.Visible:=false;
set_form_report_heigths;
end;

//procedure to display environment variables strings
procedure envstr;
var
   i:integer;
begin
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.Caption:='Environment variables';
Form_pea.Caption:='Environment variables';
Form_pea.LabelTools2.Caption:='';
Form_pea.ProgressBar1.Position:=0;
Form_report.Input.Caption:='Environment variables';
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_report.StringGrid1.ColCount:=2;
Form_report.StringGrid1.Cells[0,0]:='N';
Form_report.StringGrid1.Cells[1,0]:='Variable';
Form_report.StringGrid1.Rowcount:=GetEnvironmentVariableCount+2;
for i:=0 to GetEnvironmentVariableCount do
   begin
   Form_report.StringGrid1.Cells[0,i+1]:=inttostr(i);
   Form_report.StringGrid1.Cells[1,i+1]:=GetEnvironmentString(i);
   end;
Form_report.StringGrid1.AutosizeColumns;
Form_pea.ProgressBar1.Position:=100;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:=Form_pea.LabelTools3.Caption;
Form_report.Label4.Caption:=Form_pea.LabelTools4.Caption;
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
Form_report.Visible:=true;
set_form_report_heigths;
Form_pea.Visible:=false;
end;

procedure listfiles; //list files: mode INFO gives detailed information, LIST plain list
var
   s,mode:utf8string;
   h,k,i:integer;
   exp_files:TFoundList;
   exp_fsizes:TFoundListSizes;
   exp_ftimes:TFoundListAges;
   exp_fattr:TFoundListAttrib;
   exp_fattr_dec:TFoundList;
   nfound,nsize,smax,smin,compsize:qword;
   dmax,dmin,compest:integer;
begin
mode:=envtoutf8(paramstr(2));
s:=envtoutf8(paramstr(3));
Form_pea.Caption:=mode;
Form_pea.LabelTools2.Caption:='Listing '+s+' may take some time, please wait...';
Form_pea.ProgressBar1.Position:=5;
Application.ProcessMessages;
expand(s,exp_files,exp_fsizes,exp_ftimes,exp_fattr,exp_fattr_dec,nfound);
Form_pea.ProgressBar1.Position:=50;
Application.ProcessMessages;
nsize:=0;
compsize:=0;
compest:=0;
dmax:=exp_ftimes[0];
dmin:=exp_ftimes[0];
smax:=exp_fsizes[0];
smin:=exp_fsizes[0];
if nfound=0 then nfound:=1;
Form_pea.LabelTools3.Caption:='Found: '+inttostr(nfound);
Form_pea.ProgressBar1.Position:=60;
Application.ProcessMessages;
if upcase(mode)='INFO' then
   for i:=0 to nfound-1 do
      begin
      nsize:=nsize+exp_fsizes[i];
      compest:=testpcomp(exp_files[i]);
      compsize:=compsize+(exp_fsizes[i]*compest);
      if exp_fsizes[i]>smax then smax:=exp_fsizes[i];
      if exp_fsizes[i]<smin then smin:=exp_fsizes[i];
      try
      if exp_ftimes[i]>dmax then dmax:=exp_ftimes[i];
      if exp_ftimes[i]<dmin then dmin:=exp_ftimes[i];
      except end;
      end;
Form_pea.LabelTools4.Caption:='Total size: '+inttostr(nsize)+' B';
Form_pea.ProgressBar1.Position:=70;
Application.ProcessMessages;
Form_report.Input.Caption:='Input';
if upcase(mode)='INFO' then Form_report.Caption:='Info'
else Form_report.Caption:='List';
Form_report.StringGrid1.ColCount:=4;
Form_report.StringGrid1.Cells[0,0]:='Name';
Form_report.StringGrid1.Cells[1,0]:='Size (B)';
Form_report.StringGrid1.Cells[2,0]:='Date/time';
Form_report.StringGrid1.Cells[3,0]:='Attributes';
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_report.StringGrid1.RowCount:=nfound+1;
for k:=0 to nfound-1 do
    begin
    Form_report.StringGrid1.Cells[0,k+1]:=exp_files[k];
    Form_report.StringGrid1.Cells[1,k+1]:=inttostr(exp_fsizes[k]);
    try Form_report.StringGrid1.Cells[2,k+1]:=datetimetostr(filedatetodatetime(exp_ftimes[k])); except Form_report.StringGrid1.Cells[2,k+1]:='unknown'; end;
    Form_report.StringGrid1.Cells[3,k+1]:=exp_fattr_dec[k];
    end;
Form_pea.ProgressBar1.Position:=100;
Application.ProcessMessages;
Form_report.StringGrid1.AutosizeColumns;
Form_report.Label1.Caption:=s;
Form_report.Label2.Caption:='';
Form_report.Label3.Caption:='';
Form_report.Label4.Caption:='';
if upcase(mode)='INFO' then
   begin
   try Form_report.Label2.Caption:='Found: '+inttostr(nfound)+' objects (newer: '+datetimetostr(filedatetodatetime(dmax))+', older: '+datetimetostr(filedatetodatetime(dmin))+')'; except Form_report.Label2.Caption:='Found: '+inttostr(nfound)+' objects'; end;
   Form_report.Label3.Caption:='Total size: '+nicenumber(inttostr(nsize))+' (larger: '+nicenumber(inttostr(smax))+', smaller: '+nicenumber(inttostr(smin))+');';
   if nsize<>0 then Form_report.Label3.Caption:=Form_report.Label3.Caption+' potential compression: '+inttostr((nsize*100 - compsize) div nsize)+'%';//+nicenumber(inttostr(compsize div 100))+' ('+inttostr(compsize div nsize)+'%)';
   end;
Form_report.Visible:=true;
set_form_report_heigths;
Form_pea.Visible:=false;
end;

//hex preview
procedure hexpreview; //slow, limited to 16MB
var
   hexs,astr,offs,s:utf8string;
   fa:file of byte;
   sizea,total:qword;
   i,x,y,numreada,nrows,prows,noffs,wrbytes:integer;
   bufa:array[0..65535]of byte;
   bufhex:array[0..15,0..4095]of byte;
begin
Form_pea.PanelPW1.height:=2;
Form_report.Notebook1.PageIndex:=0;
Form_report.Caption:='Hex preview';
Form_pea.Caption:='Hex preview';
Form_pea.LabelTools2.Caption:='File: '+envtoutf8(paramstr(2));
Form_pea.LabelTools3.Caption:='';
Form_pea.LabelTools4.Caption:='';
Form_pea.ProgressBar1.Position:=0;
Form_report.StringGrid1.RowCount:=1;
Form_report.StringGrid1.ColCount:=3;
Form_report.StringGrid1.Cells[0,0]:='Offset';
Form_report.StringGrid1.Cells[1,0]:='Hex';
Form_report.StringGrid1.Cells[2,0]:='Possible UTF8';
Form_report.StringGrid1.Font.Name:='Courier';
Form_report.StringGrid1.Font.Size:=10;
try
uassignfile(fa,envtoutf8(paramstr(2)));
filemode:=0;
reset(fa);
srcfilesize(envtoutf8(paramstr(2)),sizea);
//sizea:=system.filesize(fa);
usetcurrentdir(uextractfilepath(envtoutf8(paramstr(2))));
except
showmessage(envtoutf8(paramstr(2))+' is not accessible (or is not a file)');
Application.Terminate;
end;
if sizea>16*1024*1024 then
  begin
  showmessage('Hex preview is currently limited to small files, up to 16MB');
  exit;
  end;
Form_pea.LabelTools2.Caption:=Form_pea.LabelTools2.Caption+' ('+inttostr(sizea)+' B)';
Form_report.StringGrid1.RowCount:=(sizea div 16) +2;
total:=0;
prows:=1;
wrbytes:=0;
repeat
   numreada:=0;
   blockread (fa,bufa,65536,numreada);
   i:=0;
   y:=0;
   repeat
      for x:=0 to 15 do
         begin
         bufhex[x,y]:=bufa[i];
         i:=i+1;
         if i=numreada then break;
         end;
      y:=y+1;
   until i>=numreada;
   nrows:=y;
   i:=0;
   for y:=0 to nrows-1 do
      begin
      noffs:=y+prows-1;
      offs:=hexlong(noffs*16);
      Form_report.StringGrid1.Cells[0,y+prows]:=offs;
      astr:='';
      hexs:='';
      for x:=0 to 15 do
         begin
         astr:=astr+(chr(bufhex[x,y]));
         hexs:=hexs+hexstr(@bufhex[x,y],1)+' ';
         i:=i+1;
         if i=numreada then break;
         end;
      setlength(hexs,length(hexs)-1);
      wrbytes:=wrbytes+16;
      Form_report.StringGrid1.Cells[1,y+prows]:=hexs;
      Form_report.StringGrid1.Cells[2,y+prows]:=ansitoutf8(astr);
      end;
   inc(total,numreada);
   prows:=prows+nrows;
   Form_pea.ProgressBar1.Position:=(total*100) div sizea;
   Application.ProcessMessages;
until (numreada=0) or (total>=sizea);
Form_report.StringGrid1.AutosizeColumns;
closefile(fa);
Form_pea.Visible:=false;
Form_report.visible:=true;
set_form_report_heigths;
Form_pea.ProgressBar1.Position:=100;
Form_report.Label1.Caption:=Form_pea.Caption;
Form_report.Label2.Caption:=Form_pea.LabelTools2.Caption;
Form_report.Label3.Caption:='';
Form_report.Label4.Caption:='';
{$IFDEF MSWINDOWS}Form_report.Output.TabVisible:=false;{$ENDIF}
Form_pea.ButtonDone1.Visible:=true;
Form_pea.LabelOpen.Visible:=true;
Form_pea.LabelOpen.Enabled:=false;
Form_pea.LabelLog1.Visible:=true;
end;

{
GUI procedures
}

procedure set_items_height;
var
   editheight:integer;
begin
if height_set=true then exit;
height_set:=true;
with Form_pea do
   begin
   editheight:=editpw1.height; //this items's height is determined by tedit height
   if editheight>26 then stdbtnheight:=editheight+2 else stdbtnheight:=26;
   rowheight:=editheight-4;
   ButtonDone1.Height:=stdbtnheight;
   LabelOpen.Height:=stdbtnheight;
   LabelLog1.Height:=stdbtnheight;
   ButtonRFSInteractive.Height:=stdbtnheight;
   ButtonRFSInteractive1.Height:=stdbtnheight;
   ButtonPW1.Height:=stdbtnheight;
   ButtonPW2.Height:=stdbtnheight;
   EditPW1.Height:=editheight;
   EditConfirm1.Height:=editheight;
   end;
Form_report.button2.Height:=stdbtnheight;
end;

procedure parse_action;
begin
case upcase(paramstr(1))of
'PEA' : pea;
'UNPEA' : unpea;
'RFS' : rfs;
'RFJ' : rfj;
'WIPE' : wipe(paramstr(2));
'SANITIZE' : sanitize(paramstr(2));
'COMPARE' : compare;
'CHECK' : check;
'ENVSTR' : envstr;
'LIST' : listfiles;
'HEXPREVIEW' : hexpreview;
else internal_error('Incorrect request for Pea, the action "'+paramstr(1)+'" is not supported');
end;
end;

procedure call_pea;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
Form_pea.Panel1.Visible:=true;
Form_pea.LabelE1.Visible:=true;
if (upcase(paramstr(7))='EAX256') or (upcase(paramstr(7))='EAX') or (upcase(paramstr(7))='HMAC') then
   if (upcase(paramstr(8))='INTERACTIVE') or (upcase(paramstr(8))='INTERACTIVE_REPORT') then
      begin
      Form_pea.PanelDecrypt1.visible:=false;
      Form_pea.PanelEncrypt1.visible:=true;
      Form_pea.PanelPW1.Visible:=true;
      Whirl_Init(ment); //only for PEA called as executable (otherwise passphrase/keyfile is passed from main executable): improve seed generation trough mouse movements sampling while entering passwphrase/keyfile
      Form_pea.LabelConfirm1.Visible:=true;
      Form_pea.EditConfirm1.Visible:=true;
      Form_pea.LabelHint1.Visible:=true;
      Form_pea.LabelSample1.Visible:=true;
      Form_pea.LabelSample2.Visible:=true;
      Form_pea.Image5.Visible:=true;
      exit;
      end;
interacting:=false;
end;

procedure call_unpea;
var
   f_in:file of byte;
   in_folder,in_file,in_name,in_qualified_name,compr,algo,obj_algo,volume_algo:utf8string;
   buf,tmp_buf:array [0..19] of byte;
   pwneeded,singlevolume:boolean;
   compr_level,headersize,authsize,volume_authsize,archive_datetimeencoding:byte;
   i,numread:integer;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
//parse archive to see if password is needed
in_qualified_name:=envtoutf8(paramstr(2));
if not(ufileexists(in_qualified_name)) then internal_error('"'+in_qualified_name+'" not exists');
in_folder:=uextractfilepath(in_qualified_name);
in_file:=uextractfilename(in_qualified_name);
if upcase(copy(in_qualified_name,length(in_qualified_name)-10,11))<>'.000001.PEA' then
   begin
   singlevolume:=true;
   end
else
   begin
   singlevolume:=false;
   delete(in_file,length(in_file)-10,11);
   end;
in_name:=in_file;
{blockread 10 byte archive header; since volume tag size is unknown to UnPEA,
PEA set first volume size mandatory at least 10 byte (plus volume tag) in order
to make UnPEA able to blockread the archive header and calculate the volume tag
size}
uassignfile(f_in,in_qualified_name);
filemode:=0;
reset(f_in);
blockread (f_in,buf,10,numread);
if IOResult<>0 then internal_error('IO error reading from '+in_qualified_name);
close(f_in);
test_pea_error('parsing archive header',pea_parse_archive_header(buf,volume_algo,archive_datetimeencoding));
decode_volume_control_algo (volume_algo,volume_authsize);
read_from_chunks ( in_folder,in_name,
                   20,
                   buf,tmp_buf,
                   volume_authsize,
                   20,
                   singlevolume);
for i:=0 to 9 do buf[i]:=buf[i+10];
pea_parse_stream_header(buf, compr, compr_level, algo, obj_algo);
decode_control_algo ( algo,
                      headersize,
                      authsize,
                      pwneeded);
//if password is needed, open the password panel
if pwneeded=true then
   if (upcase(paramstr(7))='INTERACTIVE') or (upcase(paramstr(7))='INTERACTIVE_REPORT') then
      begin
      Form_pea.PanelDecrypt1.visible:=true;
      Form_pea.PanelEncrypt1.visible:=false;
      Form_pea.PanelPW1.Visible:=true;
      exit;
      end;
interacting:=false;
end;

procedure call_rfs;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
if upcase(paramstr(3))='ASK' then
   begin
   Form_pea.PanelRFSinteractive.visible:=true;
   Form_pea.Caption:='Split file';
   exit;
   end;
interacting:=false;
end;

procedure call_rfj;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=false;
interacting:=false;
end;

procedure call_wipe;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_sanitize;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_compare;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_check;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_envstr;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_list;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

procedure call_hexpreview;
begin
Form_pea.PanelRFSinteractive.visible:=false;
Form_pea.PanelTools.visible:=true;
interacting:=false;
end;

{ TForm_pea }

procedure TForm_pea.ButtonDone1Click(Sender: TObject);
begin
Close;
end;

procedure TForm_pea.ButtonPW1Click(Sender: TObject);
begin
if EditPW1.Text='' then
   begin
   showmessage('Cannot accept empty password, please type it in "Passphrase" field');
   exit;
   end
else
   if (upcase(paramstr(1))='PEA') and (EditPW1.Text<>EditConfirm1.Text) then
      begin
      showmessage('Passwords doesn''t match, please retype "Passphrase" and "Confirm" fields');
      exit;
      end
   else pw:=EditPW1.Text;
if LabelKeyFileName1.Caption='<none>' then keyfile_name:='NOKEYFILE'
else keyfile_name:=LabelKeyFileName1.Caption;
PanelPW1.Visible:=false;
interacting:=false;
end;

procedure TForm_pea.ButtonPW2Click(Sender: TObject);
begin
Form_pea.Close;
end;

procedure TForm_pea.ButtonRFSinteractive1Click(Sender: TObject);
begin
Form_pea.Close;
end;

procedure TForm_pea.ButtonRFSinteractiveClick(Sender: TObject);
begin
   case ComboBox1.ItemIndex of
      0: begin
         try
            vol_size:=SpinEdit1.Value;
            case ComboBox2.ItemIndex of
               1: vol_size:=vol_size*1024;
               2: vol_size:=vol_size*1024*1024;
               3: vol_size:=vol_size*1024*1024*1024;
               end;
         except
            MessageDlg('Cannot get volume size', mtWarning, [mbOK], 0);
            exit;
         end;
         end;
      1: vol_size:=1457664;//FD
      2: vol_size:=5*1024*1024;//limit for attachment size of some mail services
      3: vol_size:=10*1024*1024;//limit for attachment size of some mail services
      4: vol_size:=650*1024*1024;//CD 650 MB
      5: vol_size:=700*1024*1024;//CD 700 MB
      6: vol_size:=4*1024*1024*1024;//max file size for FAT32 filesystem
      7: vol_size:=4700372992-(1024*1024);//size DVD+R (slightly smaller than DVD-R), source Wikipedia
      8: vol_size:=8543666176-(1024*1024);//size for DVD-R DL (slightly smaller than DVD+R DL), source Wikipedia
      end;
   case ComboBox3.ItemIndex of //volume checks
      0: vol_algo:='WHIRLPOOL';
      1: vol_algo:='SHA512';
      2: vol_algo:='SHA256';
      3: vol_algo:='RIPEMD160';
      4: vol_algo:='SHA1';
      5: vol_algo:='MD5';
      6: vol_algo:='CRC64';
      7: vol_algo:='CRC32';
      8: vol_algo:='ADLER32';
      9: vol_algo:='NOALGO';
      end;
   interacting:=false;
   PanelRFSinteractive.visible:=false;
end;

procedure change_imagesplit;
begin
with Form_pea do
   begin
   case ComboBox1.ItemIndex of
      0: ImageSplit.Picture.Bitmap:=nil;
      1: ImageSplit.Picture.Bitmap:=Bfd;
      2: ImageSplit.Picture.Bitmap:=Bmail;
      3: ImageSplit.Picture.Bitmap:=Bmail;
      4: ImageSplit.Picture.Bitmap:=Bdvd;
      5: ImageSplit.Picture.Bitmap:=Bdvd;
      6: ImageSplit.Picture.Bitmap:=Bhd;
      7: ImageSplit.Picture.Bitmap:=Bdvd;
      8: ImageSplit.Picture.Bitmap:=Bdvd;
      end;
   end;
end;

procedure ComboBox1_onchange;
begin
with Form_pea do
begin
change_imagesplit;
if ComboBox1.ItemIndex = 0 then
   begin
   SpinEdit1.Visible:=true;
   ComboBox2.Visible:=true;
   end
else
   begin
   SpinEdit1.Visible:=false;
   ComboBox2.Visible:=false;
   end;
end;
end;

procedure TForm_pea.ComboBox1Change(Sender: TObject);
begin
ComboBox1_onchange;
end;

procedure TForm_pea.EditPW1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
   pw,pr:utf8string;
   pw_strength:dword;
   pw_rating:byte;
begin
if LabelKeyFileName1.Caption[1]<>'<' then
   begin
   Shape2.Width:=240;
   Shape2.Brush.Color:=VIOLET;
   LabelPS1.Caption:='Using KeyFile';
   end
else
   begin
   pw:=EditPW1.Text;
   evaluate_password(pw,pw_strength,pw_rating);
   if (pw_strength>8) then
      if (pw_strength<240) then Shape2.Width:=pw_strength
      else Shape2.Width:=240
   else Shape2.Width:=8;
   case pw_rating of
      1: pr:='Weak';
      2: pr:='Quite weak';
      3: pr:='Not too bad';
      4: pr:='Adequate';
      5: pr:='Good';
      6: pr:='Quite strong';
      7: pr:='Strong';
      8: pr:='Very strong';
      end;
   LabelPS1.Caption:=pr+' ('+inttostr(pw_strength div 2)+')';
   if pw_strength<16*2 then Shape2.Brush.Color:=RED
   else
      if pw_strength<32*2 then Shape2.Brush.Color:=ORANGE
      else
         if pw_strength<48*2 then Shape2.Brush.Color:=YELLOW
         else
            if pw_strength<64*2 then Shape2.Brush.Color:=OLIVE
            else Shape2.Brush.Color:=GREEN;
   end;
end;

procedure TForm_pea.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   randf: file of byte;
   randarr: TKey2048;
begin
try
shl_rand(randarr); //read and leftshift of 1 byte data from persistent random seed file
gen_rand(randarr); //create new reandom seed file
uAssignFile(randf,persistent_source); //write keyfile as new seed file
rewrite(randf);
blockwrite(randf,randarr,256);
closefile(randf);
except
end;
end;

procedure TForm_pea.LabelE1Click(Sender: TObject);
begin
if details=true then
   begin
   Panel1.visible:=true;
   LabelE1.Caption:='+';
   details:=false;
   end
else
   begin
   Panel1.visible:=false;
   LabelE1.Caption:='-';
   details:=true;
   end;
end;

procedure TForm_pea.LabelKeyFile1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
   if OpenDialog1.FileName<>'' then
      begin
      LabelKeyFileName1.Caption:=OpenDialog1.FileName;
      Shape2.Width:=240;
      Shape2.Brush.Color:=VIOLET;
      LabelPS1.Caption:='Using KeyFile';
      end;
end;

procedure getthemepath(var thpath:utf8string);
var
   theme_name,s:utf8string;
begin
s:=graphicsfolder;
if s<>'' then setlength(s,length(s)-1);
theme_name:=extractfilename(s);
//default and no graphic themes are in application's path, custome themes are in configuration path (application's path for portable versions, user's home/application data for installable versions)
if (upcase(theme_name)<>upcase(DEFAULT_THEME)) and (upcase(theme_name)<>'NOGRAPHIC') then thpath:=confpath
else thpath:=executable_path;
end;

procedure load_icons; //load icons from bitmaps
var
   thpath:utf8string;
begin
getthemepath(thpath);
//valorize all captions, hints, TStrings Items
   try
   Bfd:=TBitmap.Create;
   Bmail:=TBitmap.Create;
   Bhd:=TBitmap.Create;
   Bdvd:=TBitmap.Create;
   Binfo:=TBitmap.Create;
   Blog:=TBitmap.Create;
   Bok:=TBitmap.Create;
   Bcancel:=TBitmap.Create;
   if graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      Form_pea.imagelist1.getbitmap(1,Bfd);
      Form_pea.imagelist1.getbitmap(2,Bmail);
      Form_pea.imagelist1.getbitmap(3,Bhd);
      Form_pea.imagelist1.getbitmap(4,Bdvd);
      Form_pea.imagelist1.getbitmap(5,Binfo);
      Form_pea.imagelist1.getbitmap(6,Blog);
      Form_pea.imagelist1.getbitmap(7,Bok);
      Form_pea.imagelist1.getbitmap(8,Bcancel);
      end;
   if graphicsfolder='themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      Form_pea.imagelist1.getbitmap(0,Bfd);
      Form_pea.imagelist1.getbitmap(0,Bmail);
      Form_pea.imagelist1.getbitmap(0,Bhd);
      Form_pea.imagelist1.getbitmap(0,Bdvd);
      Form_pea.imagelist1.getbitmap(0,Binfo);
      Form_pea.imagelist1.getbitmap(0,Blog);
      Form_pea.imagelist1.getbitmap(0,Bok);
      Form_pea.imagelist1.getbitmap(0,Bcancel);
      end;
   if (graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator) and (graphicsfolder<>'themes'+directoryseparator+'seven-embedded'+directoryseparator) then
      begin
      Binfo.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-info.bmp');
      Blog.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-paste.bmp');
      Bok.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-test.bmp');
      Bcancel.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-stop.bmp');
      end;
   Bfd.Transparent:=true;
   Bmail.Transparent:=true;
   Bhd.Transparent:=true;
   Bdvd.Transparent:=true;
   Binfo.Transparent:=true;
   Blog.Transparent:=true;
   Bok.Transparent:=true;
   Bcancel.Transparent:=true;
   Form_pea.Image3.Picture.Bitmap:=Binfo;
   Form_pea.Image3.Transparent:=true;
   Form_pea.Image4.Picture.Bitmap:=Binfo;
   Form_pea.Image4.Transparent:=true;
   Form_pea.Image5.Picture.Bitmap:=Binfo;
   Form_pea.Image5.Transparent:=true;
   Form_pea.Image7.Picture.Bitmap:=Binfo;
   Form_pea.Image7.Transparent:=true;
   Form_pea.ImageSplit.Transparent:=true;
   Form_pea.buttonpw1.Glyph:=Bok;
   Form_pea.buttonpw2.Glyph:=Bcancel;
   Form_pea.buttonrfsinteractive.Glyph:=Bok;
   Form_pea.buttonrfsinteractive1.Glyph:=Bcancel;
   Form_report.button1.Picture.Bitmap:=Blog;
   except
   //MessageDlg('some icons not found', mtWarning, [mbOK], 0);  //it's deactivated in final compilation to allow the program to work outside of PeaZip package
   end;
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
   if Succeeded(ShGetSpecialFolderLocation(Form_pea.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
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

procedure TForm_pea.FormCreate(Sender: TObject);

procedure readconf_relativeline(nlines:integer; var dummy:utf8string);
var
   i:integer;
begin
for i:=1 to nlines do readln(conf,dummy);
end;

var
   dummy:utf8string;
begin
executable_path:=uextractfilepath(envtoutf8(paramstr(0)));
if executable_path[length(executable_path)]<>directoryseparator then executable_path:=executable_path+directoryseparator;
usetcurrentdir(executable_path);
SetFocusedControl(EditPW1);
getdesk_env(desk_env,caption_build,delimiter);
height_set:=false;
Form_pea.Caption:='PEA '+P_RELEASE+' / '+inttostr(PEA_FILEFORMAT_VER)+'.'+inttostr(PEA_FILEFORMAT_REV);
if (PEA_FILEFORMAT_VER <> pea_utils.PEA_FILEFORMAT_VER) or (PEA_FILEFORMAT_REV <> pea_utils.PEA_FILEFORMAT_REV) then
   Form_pea.Caption:='PEA '+P_RELEASE+' / Warning: inconsistent file format support level!';
try
   {PEA executable must be in the same path of altconf.txt file, otherwise or if
   theming errors occours, default theming values will be loaded}
   uassigntext(conf,executable_path+'altconf.txt'); //load alternative configuration path
   filemode:=0;
   reset(conf);
   read_header(conf);
   readln(conf,dummy);
   readln(conf,confpath);
   CloseFile(conf);
   if (confpath='same') or (confpath='"same"') or (confpath='''same''') or (confpath=' ') or (confpath='') then confpath:=executable_path; //if confpath parameter is set to 'same' or empty use classic conf location (in res folder)
   {$IFDEF MSWINDOWS}
   if (confpath='appdata') or (confpath='"appdata"') or (confpath='''appdata''') or (confpath='%appdata%') then
   if wingetappdata(confpath)<>0 then confpath:=envtoutf8(GetEnvironmentVariable('APPDATA'))+'\PeaZip\'; //if wingetappdata fails use env variables
   {$ENDIF}
   {$IFDEF LINUX}
   if (confpath='appdata') or (confpath='"appdata"') or (confpath='''appdata''') or (confpath='%appdata%') then confpath:=GetEnvironmentVariable('HOME')+'/.PeaZip/';
   {$ENDIF}
   {$IFDEF FREEBSD}
   if (confpath='appdata') or (confpath='"appdata"') or (confpath='''appdata''') or (confpath='%appdata%') then confpath:=GetEnvironmentVariable('HOME')+'/.PeaZip/';
   {$ENDIF}
   {$IFDEF NETBSD}
   if (confpath='appdata') or (confpath='"appdata"') or (confpath='''appdata''') or (confpath='%appdata%') then confpath:=GetEnvironmentVariable('HOME')+'/.PeaZip/';
   {$ENDIF}
   if not(udirectoryexists(confpath)) then umkdir(confpath);
   if (confpath[1]='.') and (confpath[2]='.') then confpath:='..'+directoryseparator+confpath; //relative path, needs to be adjusted since pea is in a subfolder of peazip path
   confpath:=uexpandfilename(confpath);
   if confpath[length(confpath)]<>directoryseparator then confpath:=confpath+directoryseparator;
   if not(udirectoryexists(confpath)) then confpath:=executable_path; //if alternative configuration directory does not exist or is not accessible, use res path
   persistent_source:=confpath+'rnd';
   uassigntext(conf,(confpath+'conf.txt'));
   filemode:=0;
   reset(conf);
   readln(conf,dummy);
   readln(conf,graphicsfolder);
   if graphicsfolder[1]='r' then graphicsfolder:='themes'+directoryseparator+DEFAULT_THEME+directoryseparator;
   readln(conf,dummy); opacity:=strtoint(dummy);
   readln(conf,color1);
   readln(conf,color2);
   readln(conf,color3);
   readln(conf,color4);
   readln(conf,color5);
   readconf_relativeline(9,dummy); closepolicy:=strtoint(dummy);
   //rowheight:=strtoint(dummy);
   //itemheight:=strtoint(dummy);
   //autosizeitemheight:=strtoint(dummy);
   CloseFile(conf);
   if opacity<0 then opacity:=0;
   if opacity>100 then opacity:=100;
   if (closepolicy<0) or (closepolicy>4) then closepolicy:=1;
   if color1='' then color1:=colortostring(clWindow);
   if color2='' then color2:=colortostring(clBtnFace);
   if color3='' then color3:=colortostring(clBtnFace);
   if color4='' then color4:='$00669999';
   if color5='' then color5:=colortostring(clWindowText);
   //if (rowheight<12) or (rowheight>32) then rowheight:=18;
   //if (itemheight<12) or (itemheight>32) then itemheight:=21;
   //if (autosizeitemheight<0) or (autosizeitemheight>1) then autosizeitemheight:=1;
except
   persistent_source:=executable_path+'rnd';
   graphicsfolder:='themes'+directoryseparator+DEFAULT_THEME+directoryseparator;
   udodirseparators(graphicsfolder);
   opacity:=100;
   color1:=colortostring(clWindow);
   color2:=colortostring(clBtnFace);
   color3:=colortostring(clBtnFace);
   color4:='$00669999';
   color5:=colortostring(clWindowText);
   closepolicy:=1;
   //rowheight:=18;
   //itemheight:=21;
   //autosizeitemheight:=1;
end;
Application.CreateForm(TForm_report, Form_report);
load_icons;
Form_pea.LabelOpen.visible:=false;
{$IFDEF MSWINDOWS}
osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
if GetVersionEx(osVerInfo) then
   begin
   if osVerInfo.dwMajorVersion>4 then //the system is NTx and most recent than NT4
      begin
      {following code make problems on Win9x: while it correctly avoid importing
      user32.dll if SetLayeredWindowAttributes is not supported, it raises randomic
      errors on 9x systems, so the code is made unreachable for those systems and
      for NT4 that will not support that function anyway}
      if opacity<100 then
         begin
         try
            hUser32 := GetModuleHandle(PChar('USER32.DLL'));
            if hUser32 <> 0 then
               begin
               pointer(SetLayeredWindowAttributes) := GetProcAddress(hUser32, 'SetLayeredWindowAttributes');
               if @SetLayeredWindowAttributes <> nil then
                  begin
                  SetWindowLongPtr(Self.Handle, GWL_EXSTYLE, GetWindowLong(Self.Handle, GWL_EXSTYLE) OR WS_EX_LAYERED);
                  SetLayeredWindowAttributes(Self.Handle, 0, 255+opacity-100, LWA_ALPHA);//not supported on 98 and NT4, called dynamically since has no meaning checking it at compile time
                  end;
               end;
         except
         end;
         end;
      end;
   end;
{$ENDIF}
PanelDecrypt1.visible:=false;
PanelEncrypt1.visible:=false;
PanelPW1.visible:=false;
PanelRFSinteractive.visible:=false;
PanelTools.visible:=false;
LabelE1.visible:=false;
Panel1.visible:=false;
details:=false;
control:=false;
interacting:=true;
if paramcount>0 then
   case upcase(paramstr(1)) of
      'PEA' : call_pea;
      'UNPEA' : call_unpea;
      'RFS' : call_rfs;
      'RFJ' : call_rfj;
      'WIPE' : call_wipe;
      'SANITIZE' : call_sanitize;
      'COMPARE' : call_compare;
      'CHECK' : call_check;
      'ENVSTR' : call_envstr;
      'LIST' : call_list;
      'HEXPREVIEW' : call_hexpreview;
      else internal_error('Incorrect request for Pea, the action "'+paramstr(1)+'" is not supported');
      end;
end;

procedure TForm_pea.LabelLog1Click(Sender: TObject);
begin
Form_report.Visible:=true;
set_form_report_heigths;
Form_report.WindowState:=wsNormal;
end;

function cp_open(s:utf8string; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s<>'' then
   {$IFDEF MSWINDOWS}
   w:=utf8decode(s);
   cp_open:=ShellExecuteW(Form_pea.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), 1);//all Windows from 95 and NT3.1
   if cp_open<33 then
      cp_open:=shellexecuteW(Form_pea.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), 1);
   {$ENDIF}
   {$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
   {$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
   {$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TForm_pea.LabelOpenClick(Sender: TObject);
begin
cp_open(output,desk_env);
end;

procedure TForm_pea.PanelPW1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   i:integer;
   st:string;
begin
sample_mouse_ent(ment,x,y);
ment_sample:=ment;
SHA256Final(ment_sample,mentd_sample);
st:='';
for i:=0 to 3 do st:=st+hexstr(@mentd_sample[i],1);
LabelSample1.Caption:=st;
end;

procedure TForm_pea.Timer1Timer(Sender: TObject); //gives the time to draw the UI before the CPU intensive task begin
begin
if Form_pea.visible=true then set_items_height;
if control=true then exit;
if interacting=true then exit;
control:=true;
parse_action;
end;

initialization
  {$I unit_pea.lrs}
  
  {$IFDEF MSWINDOWS}
  OleInitialize(nil);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  OleUninitialize
  {$ENDIF}
  
end.
