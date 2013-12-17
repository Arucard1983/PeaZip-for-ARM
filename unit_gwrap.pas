unit Unit_gwrap;
{
 DESCRIPTION     :  Unit launching 7za console application, providing GUI for:
                    displaying output of console using pipes;
                    displaying and explain job exitcode

 REQUIREMENTS    :  FPC, Lazarus

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20061102  G.Tani      Initial version
 0.11     20061130  G.Tani
 0.12     20070117  G.Tani
 0.13     20070130  G.Tani
 0.14     20070226  G.Tani
 0.15     20070403  G.Tani
 0.16     20070607  G.Tani
 0.17     20070716  G.Tani
 0.18     20070804  G.Tani
 0.19     20071001  G.Tani
 0.20     20071030  G.Tani
 0.20b    20071124  G.Tani
 0.21     20080319  G.Tani
 0.22     20080510  G.Tani
 0.23     20080730  G.Tani
 0.24     20080922  G.Tani
 0.25     20081027  G.Tani
 0.26     20081118  G.Tani
 0.27     20090124  G.Tani
 0.28     20090215  G.Tani
 0.29     20090330  G.Tani
 0.30     20090508  G.Tani
 0.31     20090713  G.Tani
 0.32     20090825  G.Tani
 0.33     20090913  G.Tani
 0.34     20091016  G.Tani
 0.35     20091103  G.Tani
 0.36     20091109  G.Tani
 0.37     20091125  G.Tani
 0.38     20100108  G.Tani
 0.39     20100125  G.Tani
 0.40     20100207  G.Tani
 0.41     20100217  G.Tani
 0.42     20100313  G.Tani
 0.43     20100424  G.Tani
 0.44     20100602  G.Tani
          20100907  G.Tani
 0.45     20101014  G.Tani
 0.46     20101113  G.Tani
 0.47     20101224  G.Tani
 0.48     20110402  G.Tani
 0.49     20110426  G.Tani
 0.50     20110727  G.Tani
 0.51     20110813  G.Tani
 0.52     20110915  G.Tani
 0.53     20111016  G.Tani
 0.54     20111110  G.Tani
          20111224  G.Tani
 0.55     20120115  G.Tani
 0.56     20120315  G.Tani
          20120515  G.Tani
 0.57     20120807  G.Tani      (Windows) Added semaphore to perform one operation at time in order to optimize disk performances for some operations
          20120818  G.Tani      Uniformed Button Panels design over the application
 0.58     20120916  G.Tani      More information given on extraction, list and test jobs
 0.59     20121104  G.Tani      PeaLauncher (in standard mode) inherits previous instances minimized/normal status
                                New high definition Windows icon
 0.60     20130220  G.Tani      New theming engine
 0.61     20130310  G.Tani      Minor fixes in fallback if text file is not found
 0.62     20130617  G.Tani      Code cleanup, various usability improvements (new layout, input and output pats are linked)
 0.63     20130718  G.Tani      (Linux) Fixed opening input/output paths
                                Recompiled with Lazarus 1.0.10
 0.64     20130823  G.Tani      Various minor fixes
 0.65     20130922  G.Tani      Various minor fixes, recompiled with Lazarus 1.0.12
 0.66     20131106  G.Tani      Added context menu for input/output links featuring entries to explore and search the system, and pause/stop the task

(C) Copyright 2006 Giorgio Tani giorgio.tani.software@gmail.com
Official PeaZip site http://www.peazip.org
The program is released under GNU LGPL http://www.gnu.org/licenses/lgpl.txt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, activex,
  {$ENDIF}
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, strutils,
  StdCtrls, Buttons, ComCtrls, Process, Menus,
  pea_utils, list_utils, ansiutf8_utils, Grids, Math;

type

  { TForm_gwrap }

  TForm_gwrap = class(TForm)
    Bevel1: TBevel;
    ButtonEditName10: TSpeedButton;
    ButtonEditName11: TSpeedButton;
    ButtonEditName3: TButton;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOutputArchive: TButton;
    cbAutoClose: TCheckBox;
    cbAutoOpen: TCheckBox;
    CheckBoxFolder: TCheckBox;
    CheckBoxFolder1: TCheckBox;
    CheckBoxHalt: TCheckBox;
    ComboBoxOverwrite: TComboBox;
    EditName3: TEdit;
    EditOpenOut: TEdit;
    EditUn7zaPW: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image4: TImage;
    ImageButton2: TImage;
    ImageList1: TImageList;
    il_dtheme_16: TImageList;
    il_dtheme_big: TImageList;
    Button1: TBitBtn;
    Button3: TBitBtn;
    Button4: TBitBtn;
    ImagePriority: TImage;
    ImageSavePJ: TImage;
    l2: TLabel;
    l3: TLabel;
    l4: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    l1: TLabel;
    labelextto: TLabel;
    labelopenfile1: TLabel;
    Labeli: TLabel;
    LabelInfo1: TLabel;
    LabelInfo2: TLabel;
    LabelInfo3: TLabel;
    Labelo: TLabel;
    labelopenfile2: TLabel;
    labelopenfile3: TLabel;
    labelopenfile0: TLabel;
    Labelt: TLabel;
    LabelWarning1: TLabel;
    LabelWarning2: TLabel;
    LabelWarning3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem11: TMenuItem;
    pmpause: TMenuItem;
    pmstop: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pmei: TMenuItem;
    pmeo: TMenuItem;
    pmexplore: TMenuItem;
    pmsearch: TMenuItem;
    mpathexplore: TMenuItem;
    mpathreset: TMenuItem;
    Notebook1: TPageControl;
    OpenDialog1: TOpenDialog;
    Page1: TTabSheet;
    Page2: TTabSheet;
    Page3: TTabSheet;
    Page4: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    PanelBench: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenupath: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Shape1: TShape;
    Shape2: TShape;
    StringGrid1: TStringGrid;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonEditName10Click(Sender: TObject);
    procedure ButtonEditName11Click(Sender: TObject);
    procedure ButtonEditName3Click(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonOutputArchiveClick(Sender: TObject);
    procedure cbAutoOpenClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ImagePriorityClick(Sender: TObject);
    procedure ImageSavePJClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ImageButton2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure l2Click(Sender: TObject);
    procedure l4Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure labelopenfile0Click(Sender: TObject);
    procedure labelopenfile2Click(Sender: TObject);
    procedure LabelWarning1Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mpathexploreClick(Sender: TObject);
    procedure mpathresetClick(Sender: TObject);
    procedure pmeiClick(Sender: TObject);
    procedure pmeoClick(Sender: TObject);
    procedure pmexploreClick(Sender: TObject);
    procedure pmpauseClick(Sender: TObject);
    procedure pmsearchClick(Sender: TObject);
    procedure pmstopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

{$IFDEF MSWINDOWS}//used for transparence
type
  TSetLayeredWindowAttributes = function(hwnd: HWND; crKey: COLORREF; bAlpha: Byte; dwFlags: Longint): Longint;
  stdcall;
{$ENDIF}

procedure preparefull;

const
  PEAZIPVERSION = '5.2';
  WS_EX_LAYERED = $80000;
  LWA_ALPHA     = $2;
  {$IFDEF MSWINDOWS}
  DEFAULT_THEME = 'seven-embedded';
  {$ELSE}
  DEFAULT_THEME = 'seven-embedded';
  {$ENDIF}
  BARMAX        = 32;
  BARMIN        = 8;
  {$IFDEF MSWINDOWS}
  EXEEXT        = '.exe';
  {$ELSE}
  EXEEXT        = '';
  {$ENDIF}

var
  Form_gwrap: TForm_gwrap;
  Barchive,Binfo,Bloadlayout,Bconsole,Bp1,Bp2,Bp3,Bp4,Bp5,Bp6,Bp7,Bp8,
  Bpriority1,Bpriority2,Bpriority3,Bpriority4,
  Bsuccess,Berror,Binterfaces: TBitmap;
  cl,cl1,outpath,executable_path,graphicsfolder,dummy,Color1,Color2,Color3,
  Color4,Color5,caption_build,delimiter,confpath,peazippath,in_name:utf8string;
  modeofuse,fullmode,automode,max_l,ppriority,rowheight,autoclose,autoopen:integer;
  insize,incstep1,incstep2,incstep3,progress,tinsize:qword;
  opacity,desk_env,pcount,optype:byte;
  patheditor:TEdit;
  T,conf:text;
  f:file of byte;
  launched,stopped,ended,ppause,sizeok,pstarted,launchwithsemaphore:boolean;
  tsin:TTimestamp;
  //translations
  txt_5_0_extract,txt_5_0_from,txt_5_0_in,txt_5_0_to,
  txt_4_5_search,
  txt_4_0_dragorselect,txt_4_0_drag,txt_4_0_select,
  txt_3_6_selectdir,
  txt_3_5_close,
  txt_3_0_details,txt_3_0_hints,txt_3_0_arc,txt_3_0_ext,
  txt_2_8_oop,
  txt_2_7_validatecl,txt_2_7_validatefn,
  txt_2_6_open,
  txt_2_5_ace_missing,
  txt_2_3_pw_errorchar_gwrap,txt_2_3_cancel,txt_2_3_encryption,txt_2_3_extinnew,
  txt_2_3_keyfile,txt_2_3_kf_not_found_gwrap,txt_2_3_moreoptions,txt_2_3_nopaths,
  txt_2_3_pw,txt_2_3_skipexisting,txt_2_3_overexisting,txt_2_3_renameextracted,
  txt_2_3_renameexisting,txt_2_3_options,
  txt_status,txt_jobstatus,txt_rating,txt_threads,txt_input,
  txt_output,txt_time,txt_isrunning,txt_autoclose,txt_halt,txt_report,txt_console,
  txt_explore,txt_ok,txt_stop,txt_pause,txt_rt,txt_high,txt_normal,txt_idle,txt_priority,
  txt_savejob,txt_savelog,txt_bench,txt_saveas,txt_job_success,txt_job1,txt_job2,
  txt_job7,txt_job8,txt_job127,txt_job255,txt_job_unknown,txt_benchscale,txt_lt,txt_extto,
  txt_create,txt_nocl,txt_job_started,txt_jobstopped,txt_jstopped,txt_jpaused,txt_jresumed,
  txt_p_realtime,txt_p_high,txt_p_normal,txt_p_idle,txt_paused,txt_running,txt_speedscale,
  txt_crscale,txt_done,txt_halted,txt_error,txt_hardware,txt_software,txt_resume,
  txt_stdjob,txt_benchmarkjob,txt_defragjob,txt_consolejob,lang_file,lver,wincomspec,
  winver:utf8string;
  {$IFDEF MSWINDOWS}
  //used for transparence
  hUser32: HMODULE;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
  osVerInfo: TOSVersionInfo;
  //semaphore
  psem: THandle;
  {$ENDIF}

implementation

procedure conf_critical_error_msg; //hardcoded
begin
MessageDlg('PeaLauncher cannot find or parse critical configuration files (probably because deleted, moved or corrupted); PeaZip should be reinstalled', mtError, [mbOK], 0);
end;

procedure lang_critical_error_msg; //hardcoded
begin
MessageDlg('PeaLauncher cannot parse language file '+lang_file+' and will now try to fall back to default language file default.txt', mtError, [mbOK], 0);
end;

function valorize_text:integer;
var
   s:utf8string;
begin
valorize_text:=-1;
try
readln(t,s);
readln(t,s); txt_5_0_extract:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_from:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_in:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_5_0_to:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_5_search:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_drag:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_dragorselect:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_4_0_select:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_6_selectdir:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_5_close:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_details:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_hints:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_arc:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_3_0_ext:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_8_oop:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_7_validatefn:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_7_validatecl:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_6_open:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_5_ace_missing:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_pw_errorchar_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_renameexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_renameextracted:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_cancel:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_encryption:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_extinnew:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_keyfile:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_kf_not_found_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_moreoptions:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_nopaths:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_options:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_overexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_pw:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_2_3_skipexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_unknown:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_stdjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_benchmarkjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_defragjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_consolejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job1:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job127:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job2:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job255:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job7:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job8:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_autoclose:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_crscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_console:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_benchscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_create:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_done:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_nocl:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_error:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_explore:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_extto:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_halt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_halted:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_hardware:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_input:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jpaused:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jresumed:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_started:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jobstatus:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_jobstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_job_success:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_lt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_ok:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_output:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_pause:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_paused:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_p_realtime:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_rating:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_rt:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_report:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_resume:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_priority:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_running:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_isrunning:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_saveas:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_savejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_savelog:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_software:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_speedscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_status:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_stop:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_bench:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_threads:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s); txt_time:=copy(s,pos(':',s)+2,length(s)-pos(':',s));
readln(t,s);
readln(t,s); if s<>'=== end PeaLauncher text group ===' then exit;
valorize_text:=0;
except
valorize_text:=-1;
end;
end;

{procedure get_fallback_text(s:utf8string);
begin
if copy(s,0,pos(':',s)-1)='txt_4_5_search' then begin txt_4_5_search:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_4_0_drag' then begin txt_4_0_drag:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_4_0_dragorselect' then begin txt_4_0_dragorselect:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_4_0_select' then begin txt_4_0_select:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_6_selectdir' then begin txt_3_6_selectdir:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_5_close' then begin txt_3_5_close:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_0_details' then begin txt_3_0_details:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_0_hints' then begin txt_3_0_hints:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_0_arc' then begin txt_3_0_arc:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_3_0_ext' then begin txt_3_0_ext:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_8_oop' then begin txt_2_8_oop:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_7_validatefn' then begin txt_2_7_validatefn:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_7_validatecl' then begin txt_2_7_validatecl:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_6_open' then begin txt_2_6_open:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_5_ace_missing' then begin txt_2_5_ace_missing:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_pw_errorchar_gwrap' then begin txt_2_3_pw_errorchar_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_renameexisting' then begin txt_2_3_renameexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_renameextracted' then begin txt_2_3_renameextracted:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_cancel' then begin txt_2_3_cancel:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_encryption' then begin txt_2_3_encryption:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_extinnew' then begin txt_2_3_extinnew:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_keyfile' then begin txt_2_3_keyfile:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_kf_not_found_gwrap' then begin txt_2_3_kf_not_found_gwrap:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_moreoptions' then begin txt_2_3_moreoptions:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_nopaths' then begin txt_2_3_nopaths:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_options' then begin txt_2_3_options:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_overexisting' then begin txt_2_3_overexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_pw' then begin txt_2_3_pw:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_2_3_skipexisting' then begin txt_2_3_skipexisting:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job_unknown' then begin txt_job_unknown:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_stdjob' then begin txt_stdjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_benchmarkjob' then begin txt_benchmarkjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_defragjob' then begin txt_defragjob:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_consolejob' then begin txt_consolejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job1' then begin txt_job1:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job127' then begin txt_job127:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job2' then begin txt_job2:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job255' then begin txt_job255:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job7' then begin txt_job7:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job8' then begin txt_job8:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_autoclose' then begin txt_autoclose:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_crscale' then begin txt_crscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_console' then begin txt_console:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_benchscale' then begin txt_benchscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_create' then begin txt_create:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_done' then begin txt_done:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_nocl' then begin txt_nocl:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_error' then begin txt_error:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_explore' then begin txt_explore:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_extto' then begin txt_extto:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_halt' then begin txt_halt:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_halted' then begin txt_halted:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_hardware' then begin txt_hardware:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_high' then begin txt_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_idle' then begin txt_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_input' then begin txt_input:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_jpaused' then begin txt_jpaused:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_jresumed' then begin txt_jresumed:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job_started' then begin txt_job_started:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_jobstatus' then begin txt_jobstatus:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_jstopped' then begin txt_jstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_jobstopped' then begin txt_jobstopped:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_job_success' then begin txt_job_success:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_lt' then begin txt_lt:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_normal' then begin txt_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_ok' then begin txt_ok:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_output' then begin txt_output:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_pause' then begin txt_pause:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_paused' then begin txt_paused:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_p_high' then begin txt_p_high:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_p_idle' then begin txt_p_idle:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_p_normal' then begin txt_p_normal:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_p_realtime' then begin txt_p_realtime:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_rating' then begin txt_rating:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_rt' then begin txt_rt:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_report' then begin txt_report:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_resume' then begin txt_resume:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_priority' then begin txt_priority:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_running' then begin txt_running:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_isrunning' then begin txt_isrunning:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_saveas' then begin txt_saveas:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_savejob' then begin txt_savejob:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_savelog' then begin txt_savelog:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_software' then begin txt_software:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_speedscale' then begin txt_speedscale:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_status' then begin txt_status:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_stop' then begin txt_stop:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_bench' then begin txt_bench:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_threads' then begin txt_threads:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
if copy(s,0,pos(':',s)-1)='txt_time' then begin txt_time:=copy(s,pos(':',s)+2,length(s)-pos(':',s)); exit; end;
end;

function fallback_valorize_text:integer;
var
   s:utf8string;
begin
fallback_valorize_text:=-1;
repeat
readln(t,s);
get_fallback_text(s);
until eof(t) or (s='=== end PeaLauncher text group ===');
if eof(t)=false then fallback_valorize_text:=0;
end; }

function valorize_headers:integer;
var
   s:utf8string;
begin
valorize_headers:=-1;
readln(t,s);//translator(s)
readln(t,s);//last revision's translator(s)
readln(t,s);//last revision date
readln(t,s);
readln(t,s);//peazip text group
if s<>'=== PeaZip text group ===' then exit
else valorize_headers:=0;
end;

procedure assign_guitext;
begin
with Form_gwrap do
begin
ImagePriority.Hint:=txt_priority;
labelopenfile0.Caption:=txt_4_5_search;
labelopenfile1.Caption:=txt_4_0_drag;
labelopenfile2.Caption:=txt_4_0_select;
labelWarning1.Caption:=txt_3_0_hints;
GroupBox3.Caption:=txt_2_3_options;
CheckBoxFolder.Caption:=txt_2_3_extinnew;
CheckBoxFolder1.Caption:=txt_2_3_nopaths;
Label4.Caption:=txt_2_3_moreoptions;
ButtonCancel.Caption:=txt_2_3_cancel;
GroupBox2.Caption:=txt_2_3_encryption;
LabelWarning3.Caption:=txt_2_3_pw;
LabelWarning2.Caption:=txt_2_3_keyfile;
labelextto.Caption:=txt_extto;
ButtonOk.caption:=txt_ok;
Notebook1.PageIndex:=1; Page2.Caption:=txt_report+'   ';
Notebook1.PageIndex:=2; Page3.Caption:=txt_console+'   ';
Notebook1.PageIndex:=3; Page4.Caption:=txt_2_3_options+'   ';
Notebook1.PageIndex:=0; Page1.Caption:=txt_status+'   ';
Label1.Caption:=txt_isrunning;
Label2.Caption:=txt_rating;
Label3.Caption:=txt_threads;
Labeli.Caption:=txt_input;
Labelo.Caption:=txt_output;
Labelt.Caption:=txt_time;
cbAutoClose.Caption:=txt_autoclose;
CheckBoxHalt.Caption:=txt_halt;
cbAutoOpen.Caption:=txt_2_8_oop;
Button1.Caption:=txt_3_5_close;
Button3.Caption:=txt_stop;
Button4.Caption:=txt_pause;
MenuItem1.Caption:=txt_rt;
MenuItem2.Caption:=txt_high;
MenuItem3.Caption:=txt_normal;
MenuItem4.Caption:=txt_idle;
ImageSavePJ.Hint:=txt_savejob;
ImageButton2.Hint:=txt_savelog;
Label2.Hint:=txt_benchscale;
OpenDialog1.Title:=txt_2_6_open;
SaveDialog1.Title:=txt_saveas;
SaveDialog2.Title:=txt_saveas;
SelectDirectoryDialog1.Title:=txt_3_6_selectdir;
pmexplore.caption:=txt_explore+'...';
pmsearch.caption:=txt_4_5_search;
pmpause.caption:=txt_pause;
pmstop.caption:=txt_stop;
end
end;

function load_texts(lang:utf8string):integer; //valorize localized text strings
var
   s:utf8string;
   i:integer;
begin
load_texts:=-1;
try
   uassigntext(t,(executable_path+'lang'+directoryseparator+lang));
   filemode:=0;
   reset(t);
   read_header(t);
   readln(t,s); //declaration
   if s<>'=== PeaZip language file ===' then
      begin
      closefile(t);
      exit;
      end;
   readln(t,s);//language
   readln(t,lver);//version
   i:=valorize_headers;
   repeat //skip until about text group
      readln(t,s);
   until (eof(t)) or (s='=== end PeaZip text group ===');
   if eof(t)=true then
      begin
      closefile(t);
      exit;
      end;
   readln(t,s);//empty
   readln(t,s);//declaration
   if s<>'=== PeaLauncher text group ===' then
      begin
      closefile(t);
      exit;
      end;
   if lver<>PEAZIPVERSION then
      {if i=0 then i:=fallback_valorize_text
      else}
   else
      if i=0 then i:=valorize_text;
   if i=0 then assign_guitext
   else
      begin
      closefile(t);
      exit;
      end;
   closefile(t);
   load_texts:=0;
except
   try
   closefile(t);
   except
   end;
   load_texts:=-1;
end;
end;

function texts(lang:utf8string):integer;
begin
   //preload default language file to valorize possibly untranslated strings (i.e. older translations)
   texts:=load_texts('default.txt');
   //fallback to english if default language file is not ok
   if texts<>0 then texts:=load_texts('en.txt');
   //fallback to british english if even english language file is not ok
   if texts<>0 then texts:=load_texts('en-gb.txt');
   if lang<>'default.txt' then
      if load_texts(lang)<>0 then //try to load language file sequentially; fallback to a slower recoursive procedure to valorize each known variable from lang file strings if the language file version doesn't match with PeaZip's version
         begin
         lang_file:='default.txt'; //try to load default language file on failure
         load_texts(lang_file);
         end
      else texts:=0;
{if texts<>0 then
   begin
   //conf_critical_error_msg;
   Application.Terminate;
   end;}//use compile time strings if no valid text file is found
end;

function cp_open(s:utf8string; desk_env:byte):integer;
var
   w:widestring;
   //P:TProcess;
begin
cp_open:=-1;
if s='' then exit;
if validatecl(s)<>0 then begin MessageDlg(txt_2_7_validatecl+' '+s, mtWarning, [mbOK], 0); exit; end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(Form_gwrap.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(Form_gwrap.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure set_items_height;
var
   editheight,stdbtnheight:integer;
begin
if sizeok=true then exit;
sizeok:=true;
with Form_gwrap do
begin
editheight:=editname3.height; //this items's height is determined by tedit height
if editheight>26 then stdbtnheight:=editheight+2 else stdbtnheight:=26;
rowheight:=editheight-4;
Button1.Height:=stdbtnheight;
Button3.Height:=stdbtnheight;
Button4.Height:=stdbtnheight;
ButtonOk.Height:=stdbtnheight;
ButtonCancel.Height:=stdbtnheight;
Panel5.Height:=stdbtnheight;
StringGrid1.DefaultRowHeight:=rowheight;
end;
end;

procedure apply_theme;
begin
{$IFDEF MSWINDOWS}
if winver='nt6+' then
   begin
   Form_gwrap.Label1.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.Label5.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.Label6.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.Labeli.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.Labelo.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.Labelt.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.LabelInfo1.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.LabelInfo2.Font.Color:=clgray;//clInactiveCaptionText;
   Form_gwrap.LabelInfo3.Font.Color:=clgray;//clInactiveCaptionText;
   end;
{$ENDIF}
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

procedure load_icons;
var
   thpath:utf8string;
begin
with Form_gwrap do
   try
   //non-themed
   Bpriority1:=TBitmap.Create;
   Bpriority2:=TBitmap.Create;
   Bpriority3:=TBitmap.Create;
   Bpriority4:=TBitmap.Create;
   Bp1:=TBitmap.Create;
   Bp2:=TBitmap.Create;
   Bp3:=TBitmap.Create;
   Bp4:=TBitmap.Create;
   Bp5:=TBitmap.Create;
   Bp6:=TBitmap.Create;
   Bp7:=TBitmap.Create;
   Bp8:=TBitmap.Create;
   Imagelist1.getbitmap(0,ButtonEditname10.Glyph);
   Imagelist1.getbitmap(0,ButtonEditname11.Glyph);
   Imagelist1.getbitmap(1,Bpriority1);
   Imagelist1.getbitmap(2,Bpriority2);
   Imagelist1.getbitmap(3,Bpriority3);
   Imagelist1.getbitmap(4,Bpriority4);
   Imagelist1.getbitmap(5,Bp1);
   Imagelist1.getbitmap(6,Bp2);
   Imagelist1.getbitmap(7,Bp3);
   Imagelist1.getbitmap(8,Bp4);
   Imagelist1.getbitmap(9,Bp5);
   Imagelist1.getbitmap(10,Bp6);
   Imagelist1.getbitmap(11,Bp7);
   Imagelist1.getbitmap(12,Bp8);
   //themed
   Binterfaces:=TBitmap.Create;
   Bsuccess:=TBitmap.Create;
   Binfo:=TBitmap.Create;
   Barchive:=TBitmap.Create;
   Bloadlayout:=TBitmap.Create;
   Bconsole:=TBitmap.Create;
   Berror:=TBitmap.Create;
   getthemepath(thpath);
   if graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      il_dtheme_16.getbitmap(0,Bsuccess);
      il_dtheme_16.getbitmap(1,Binfo);
      il_dtheme_16.getbitmap(2,Bloadlayout);
      il_dtheme_16.getbitmap(3,Bconsole);
      il_dtheme_16.getbitmap(4,Berror);
      il_dtheme_16.getbitmap(5,Binterfaces);
      il_dtheme_big.getbitmap(0,Barchive);
      end;
   if graphicsfolder='themes'+directoryseparator+'nographic'+directoryseparator then
      begin
      imagelist1.getbitmap(13,Bsuccess);
      imagelist1.getbitmap(13,Binfo);
      imagelist1.getbitmap(13,Bloadlayout);
      imagelist1.getbitmap(13,Bconsole);
      imagelist1.getbitmap(13,Berror);
      imagelist1.getbitmap(13,Binterfaces);
      imagelist1.getbitmap(13,Barchive);
      end;
   if (graphicsfolder<>'themes'+directoryseparator+'seven-embedded'+directoryseparator) and (graphicsfolder<>'themes'+directoryseparator+'nographic'+directoryseparator) then
      begin
      Bsuccess.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-test.bmp');
      Binfo.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-info.bmp');
      Bloadlayout.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-paste.bmp');
      Bconsole.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-file-script.bmp');
      Berror.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-stop.bmp');
      Binterfaces.LoadFromFile(thpath+graphicsfolder+'16'+directoryseparator+'16-navigation.bmp');
      Barchive.LoadFromFile(thpath+graphicsfolder+'96'+directoryseparator+'96-folder.bmp');
      end;
   Bsuccess.Transparent:=True;
   Binfo.Transparent:=True;
   Barchive.Transparent:=True;
   Bloadlayout.Transparent:=True;
   Bconsole.Transparent:=True;
   Berror.Transparent:=True;
   Image2.Picture.Bitmap:=Barchive;
   Image2.Transparent:=True;
   Image4.Picture.Bitmap:=Bp1;
   Image4.Transparent:=True;
   Buttonok.Glyph:=Bsuccess;
   ButtonCancel.Glyph:=Berror;
   ImageButton2.Picture.Bitmap:=Bloadlayout;
   ImageSavePJ.Picture.Bitmap:=Bconsole;
   Image1.Picture.Bitmap:=Binterfaces;
   ImagePriority.Picture.Bitmap:=Bpriority2;
   MenuItem1.Bitmap:=Bpriority4;
   MenuItem2.Bitmap:=Bpriority3;
   MenuItem3.Bitmap:=Bpriority2;
   MenuItem4.Bitmap:=Bpriority1;
   except
   end;
end;

procedure save_report;
var
   s,p:utf8string;
   i:integer;
begin
s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+'job_log.txt';
Form_gwrap.SaveDialog1.FileName:=s;
if outpath<>'' then
   if udirectoryexists(outpath) then Form_gwrap.SaveDialog1.InitialDir:=outpath
   else Form_gwrap.SaveDialog1.InitialDir:=uextractfilepath(outpath);
if Form_gwrap.SaveDialog1.Execute then
   begin
   s:=Form_gwrap.SaveDialog1.FileName;
   uassigntext(t,(s));
   rewrite(t);
   write_header(t);
   for i:=0 to Form_gwrap.StringGrid1.Rowcount-1 do writeln(t,Form_gwrap.StringGrid1.Cells[0,i]);
   writeln(t,'');
   writeln(t,Form_gwrap.Label1.Caption);
   if modeofuse=2 then
      begin
      writeln(t,'');
      writeln(t,Form_gwrap.Label2.Caption);
      writeln(t,Form_gwrap.Label3.Caption);
      end;
   closefile(t);
   p:=(ugetcurrentdir);
   if p[length(p)]<>DirectorySeparator then p:=p+DirectorySeparator;
   end;
end;

procedure save_cl;
var
s,p:utf8string;
begin
s:=formatdatetime('yyyymmdd_hh.nn.ss_',now)+'job_definition.txt';
Form_gwrap.SaveDialog1.FileName:=s;
if outpath<>'' then
   if udirectoryexists(outpath) then Form_gwrap.SaveDialog1.InitialDir:=outpath
   else Form_gwrap.SaveDialog1.InitialDir:=uextractfilepath(outpath);
if Form_gwrap.SaveDialog1.Execute then
   begin
   s:=Form_gwrap.SaveDialog1.FileName;
   uassigntext(t,(s));
   rewrite(t);
   write_header(t);
   writeln(t,cl);
   closefile(t);
   p:=(ugetcurrentdir);
   if p[length(p)]<>DirectorySeparator then p:=p+DirectorySeparator;
   end;
end;

procedure decode_exitcode(i:integer; var s:utf8string);
begin
case i of
   0: s:=txt_job_success;
   1: s:=txt_job1;
   2: s:=txt_job2;
   7: s:=txt_job7+' '+cl;
   8: s:=txt_job8;
   127: s:=txt_job127;
   255: s:=txt_job255;
   else s:=inttostr(i)+txt_job_unknown;
   end;
end;

{procedure setspeedgraph(speed:integer);
var
   flog:single;
begin
with Form_gwrap do
begin
ShapeSpeed1.Visible:=true;
ShapeSpeed2.Visible:=true;
ShapeSpeed3.Visible:=true;
ShapeSpeed4.Visible:=true;
ShapeSpeed5.Visible:=true;
ShapeSpeed6.Visible:=true;
ShapeSpeed7.Visible:=true;
ShapeSpeed8.Visible:=true;
ShapeSpeed9.Visible:=true;
ShapeSpeed10.Visible:=true;
ImagePriority.Picture.Bitmap:=nil;
ShapeSpeed10.Height:=ShapeSpeed9.Height;
ShapeSpeed9.Height:=ShapeSpeed8.Height;
ShapeSpeed8.Height:=ShapeSpeed7.Height;
ShapeSpeed7.Height:=ShapeSpeed6.Height;
ShapeSpeed6.Height:=ShapeSpeed5.Height;
ShapeSpeed5.Height:=ShapeSpeed4.Height;
ShapeSpeed4.Height:=ShapeSpeed3.Height;
ShapeSpeed3.Height:=ShapeSpeed2.Height;
ShapeSpeed2.Height:=ShapeSpeed1.Height;
flog:=log10(speed);
ShapeSpeed1.Height:=round(flog*2);
end;
end;}

procedure progress10; //progress counter
var
   outsize,percentout,i:qword;
   tdiff,speed:integer;
   tsout:TTimeStamp;
   tpath:utf8string;
begin
with Form_gwrap do
begin
percentout:=0;
outsize:=0;
speed:=0;
tsout:=datetimetotimestamp(now);
tdiff:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if tdiff<=0 then tdiff:=100000;
Form_gwrap.LabelInfo3.Caption:=nicetime(inttostr(tdiff));
tpath:=outpath;
if (optype=1) and (modeofuse=0) then
  if ufileexists(tpath+'.tmp') then tpath:=outpath+'.tmp';
try
if ufileexists(tpath) then
   if not(udirectoryexists(tpath)) then
      begin
      srcfilesize_multipart(tpath,outsize);
      if insize<>0 then percentout:=(outsize*1000000) div insize;
      if outsize>0 then
         begin
         Form_gwrap.Labelo.Visible:=true;
         Form_gwrap.LabelInfo2.Visible:=true;
         if (percentout>0) then Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize))+' ('+inttostr(percentout div 10000)+'%)'
         else Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize));
         if tdiff<>0 then speed:=outsize div tdiff * 1000;
         if speed>0 then
            begin
            Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' @ '+nicenumber(inttostr(speed))+'/s';
            //setspeedgraph(speed);
            end;
         end
      else
         begin
         Form_gwrap.Labelo.Visible:=false;
         Form_gwrap.LabelInfo2.Visible:=false;
         end;
      end;
except
end;
if optype=0 then percentout:=percentout div 2
else percentout:=percentout * 2;
if percentout>1000000 then percentout:=1000000;

i:=(progress+percentout) div 2;
if i<330000 then progress:=progress+incstep1
else
   if i<660000 then progress:=progress+incstep2
   else progress:=progress+incstep3;

ProgressBar1.Position:=i div 1000;
if ProgressBar1.Position>=1000 then
   begin
   ProgressBar1.Position:=0;
   progress:=0;
   incstep1:=incstep1 div 4;
   incstep2:=incstep2 div 4;
   incstep3:=incstep3 div 4;
   if incstep1=0 then incstep1:=1;
   if incstep2=0 then incstep2:=1;
   if incstep3=0 then incstep3:=1;
   end;
end;
end;

procedure setiomenu;
begin
Form_gwrap.pmei.caption:=Form_gwrap.l2.caption;
Form_gwrap.pmeo.caption:=Form_gwrap.l4.caption;
if Form_gwrap.pmei.caption='' then Form_gwrap.pmei.visible:=false;
if Form_gwrap.pmeo.caption='' then Form_gwrap.pmeo.visible:=false;
Form_gwrap.pmei.caption:=txt_explore+' '+Form_gwrap.l2.caption;
Form_gwrap.pmeo.caption:=txt_explore+' '+Form_gwrap.l4.caption;
end;

procedure set_form_title;
var s,s1,s2:utf8string;
begin
Form_gwrap.l1.Visible:=false;
Form_gwrap.l2.Visible:=false;
Form_gwrap.l3.Visible:=false;
Form_gwrap.l4.Visible:=false;
Form_gwrap.l1.Caption:='';
Form_gwrap.l2.Caption:='';
Form_gwrap.l3.Caption:='';
Form_gwrap.l4.Caption:='';
Form_gwrap.l2.Hint:='';
Form_gwrap.l4.Hint:='';
case modeofuse of
   3 :
   begin
   Form_gwrap.Caption:=cl;
   exit;
   end;
   2 :
   begin
   Form_gwrap.Caption:=txt_bench;
   exit;
   end;
   1 :
   begin
   s1:=txt_lt+' '+uextractfilename(in_name);
   s2:=copy(outpath,1,length(outpath)-1);
   s2:=uextractfilename(s2);
   Form_gwrap.l1.Caption:=s1+' '+txt_5_0_in+' ';
   Form_gwrap.l4.Caption:=s2;
   Form_gwrap.l4.Hint:=uextractfilepath(outpath);
   Form_gwrap.Caption:=Form_gwrap.l1.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   setiomenu;
   exit;
   end;
   end;
if uextractfilename(outpath)='' then optype:=0
else optype:=1;//0 extract, 1 archive
if optype=0 then
   begin
   s2:=copy(outpath,1,length(outpath)-1);
   s2:=uextractfilename(s2);
   s:=uextractfilepath(in_name);
   s:=copy(s,1,length(s)-1);
   s:=uextractfilename(s);
   s1:=txt_5_0_extract+' '+uextractfilename(in_name)+' '+txt_5_0_from+' ';
   Form_gwrap.l1.Caption:=s1;
   Form_gwrap.l2.Caption:=s;
   Form_gwrap.l2.Hint:=uextractfilepath(in_name);
   Form_gwrap.l3.Caption:=' '+txt_5_0_to+' ';
   Form_gwrap.l4.Caption:=s2;
   Form_gwrap.l4.Hint:=uextractfilepath(outpath);
   Form_gwrap.Caption:=Form_gwrap.l1.Caption+Form_gwrap.l2.Caption+Form_gwrap.l3.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l2.Visible:=true;
   Form_gwrap.l3.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   end
else
   begin
   s1:=txt_create+' '+uextractfilename(outpath); //don't work properly for arc since needs no directoryseparator after outpath
   s2:=uextractfilepath(outpath);
   s2:=copy(s2,1,length(s2)-1);
   s2:=uextractfilename(s2);
   Form_gwrap.l1.Caption:=s1+' '+txt_5_0_in+' ';
   Form_gwrap.l4.Caption:=s2;
   Form_gwrap.l4.Hint:=uextractfilepath(outpath);
   Form_gwrap.Caption:=Form_gwrap.l1.Caption+Form_gwrap.l4.Caption;
   Form_gwrap.l1.Visible:=true;
   Form_gwrap.l4.Visible:=true;
   end;
setiomenu;
end;

procedure explore_out;
var
   i:integer;
   s,outpath2:utf8string;
begin
{$IFDEF MSWINDOWS}
outpath2:=outpath;
if modeofuse=1 then outpath2:=in_name;
if (optype=1) and (modeofuse=0) then
   if ufileexists(outpath+'.tmp') then outpath2:=outpath+'.tmp';
s:=outpath2;
if s='' then exit;
if pos(' ',s)<>0 then s:=delimiter+s+delimiter;
winexplorepath(s);
{$ELSE}
i:=ufilegetattr(outpath);
if (i and faDirectory) = 0 then s:=uextractfilepath(outpath)
else s:=outpath;
if s='' then exit;
cp_open(s,desk_env);
{$ENDIF}
end;

procedure explore_in;
var
   i:integer;
   s:utf8string;
begin
{$IFDEF MSWINDOWS}
s:=in_name;
if s='' then exit;
if pos(' ',s)<>0 then s:=delimiter+s+delimiter;
winexplorepath(s);
{$ELSE}
i:=ufilegetattr(in_name);
if (i and faDirectory) = 0 then s:=uextractfilepath(in_name)
else s:=in_name;
if s='' then exit;
cp_open(s,desk_env);
{$ENDIF}
end;

procedure setlabel1text;
begin
with Form_gwrap do
   begin
   if ppause=true then Label1.Caption:=txt_paused+' '
   else Label1.Caption:=txt_running+' ';
   case ppriority of
      1: Label1.Caption:=Label1.Caption+txt_rt;
      2: Label1.Caption:=Label1.Caption+txt_high;
      3: Label1.Caption:=Label1.Caption+txt_normal;
      4: Label1.Caption:=Label1.Caption+txt_idle;
      end;
   case modeofuse of
      2 : Label5.Caption:=txt_benchmarkjob;
      3 : Label5.Caption:=txt_defragjob;//unused
      20 : Label5.Caption:=txt_consolejob;
      end;
   if label5.caption<>'' then Label5.Visible:=true else Label5.Visible:=false;
   end;
end;

procedure updatereport(M:TMemoryStream; var stri1:utf8string);
var
   i:integer;
   stri2:utf8string;
begin
with Form_gwrap do
begin
SetString(stri2, M.Memory, M.Size);
stri2:=AnsiReverseString(stri2);
stri2:=copy(stri2,pos(char($0A)+char($0D),stri2)+2,length(stri2)-(pos(char($0A)+char($0D),stri2)));
stri1:=copy(stri2,1,pos(char($0A)+char($0D),stri2)-1);
stri1:=ReverseString(stri1);
stri2:=ReverseString(stri2);
Form_gwrap.Memo1.Clear;
Form_gwrap.Memo1.Append(stri2);
Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.Memo1.Lines.Count;
for i:=0 to Form_gwrap.Memo1.Lines.Count-1 do Form_gwrap.StringGrid1.Cells[0,i]:=Form_gwrap.Memo1.Lines[i];//envtoutf8(Form_gwrap.Memo1.Lines[i]);
Form_gwrap.Memo1.Clear;
Form_gwrap.StringGrid1.RowCount:=Form_gwrap.StringGrid1.Rowcount-1; //last row may be incomplete
Form_gwrap.StringGrid1.Row:=Form_gwrap.StringGrid1.Rowcount-1;
Form_gwrap.StringGrid1.AutosizeColumns;
end;
end;

procedure fixbringtofront;
begin
{$IFDEF MSWINDOWS}
Form_gwrap.FormStyle:=fsStayOnTop;
Form_gwrap.FormStyle:=fsNormal;
{$ENDIF}
end;

procedure getschedule;
var
  pwait:boolean;
begin
{$IFDEF MSWINDOWS}
if launchwithsemaphore=false then exit;
pwait:=true;
Form_gwrap.WindowState:=wsMinimized;
Form_gwrap.Visible:=false;
while pwait=true do
begin
psem:=CreateSemaphore(nil, 0, 1, 'PeaLauncher semaphore');
if ((psem<>0) and (GetLastError=ERROR_ALREADY_EXISTS)) then
   begin
   CloseHandle(psem);
   sleep(500);
   end
else pwait:=false;
end;
{$ENDIF}
end;

procedure launch_cl;
var
   P: TProcess;
   tsout,tevent:TTimeStamp;
   tdiff,speed1,speed2,i,exit_code,w,BytesRead,prevpriority,minline:integer;
   outsize,cratio:qword;
   s,dummy,scltest,delimstr,stri,astri,bstri:utf8string;
   prevpause:boolean;
   M:TmemoryStream;
begin
pstarted:=true;
if (autoclose=2) or (autoclose=4) then
else fixbringtofront;
Form_gwrap.ProgressBar1.Position:=0;//procedure is launched
progress:=0;
cl:=envtoutf8(cl);
Form_gwrap.Memo2.Lines.Append(cl);
ppause:=false;
if ppriority=0 then ppriority:=3;//default: normal priority
prevpause:=false;
prevpriority:=ppriority;
if cl='' then
   begin
   MessageDlg(txt_nocl,mtError, [mbOK], 0);
   Application.Terminate;
   end;
P:=TProcess.Create(nil);
P.CommandLine:=utf8toenv(cl);
if insize>0 then
   if (tinsize>0) and (tinsize<>insize) then Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(insize))+' / '+nicenumber(inttostr(tinsize))
   else Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(insize))
else Form_gwrap.LabelInfo1.Caption:=nicenumber(inttostr(tinsize));
set_form_title;
//Form_gwrap.StringGrid1.Rowcount:=1;
//Form_gwrap.StringGrid1.Cells[0,Form_gwrap.StringGrid1.Rowcount-1]:=datetimetostr(timestamptodatetime(tsin))+' - '+txt_job_started;
//Form_gwrap.Stringgrid1.Cursor:=crHourGlass;
if Form_gwrap.Visible=true then Application.ProcessMessages;
M := TMemoryStream.Create;
BytesRead := 0;
if modeofuse=20 then
else
   begin
   P.Options := [poUsePipes, poNoConsole];
   end;
try
if validatecl(cl)<>0 then begin MessageDlg(txt_2_7_validatecl+' '+cl, mtWarning, [mbOK], 0); ended:=true; exit; end;
tsin:=datetimetotimestamp(now);
P.Execute;
pcount:=1;
stri:='';
astri:='';
bstri:='';
while P.Running do
   begin
   if Form_gwrap.Visible=true then Application.ProcessMessages;
   if stopped=true then
      begin
      P.Terminate(255);
      MessageDlg(txt_jobstopped, mtWarning, [mbOK], 0);
      tevent:=datetimetotimestamp(now);
      //Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.StringGrid1.Rowcount+1;
      //Form_gwrap.StringGrid1.Cells[0,Form_gwrap.StringGrid1.Rowcount-1]:=datetimetostr(timestamptodatetime(tevent))+' - '+txt_jstopped;
      break;
      end;
   if ppause=true then
      begin
      if ppause<>prevpause then
         begin
         P.Suspend;
         tevent:=datetimetotimestamp(now);
         //Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.StringGrid1.Rowcount+1;
         //Form_gwrap.StringGrid1.Cells[0,Form_gwrap.StringGrid1.Rowcount-1]:=datetimetostr(timestamptodatetime(tevent))+' - '+txt_jpaused;
         prevpause:=true;
         end;
      end
   else
      begin
      if ppause<>prevpause then
         begin
         P.Resume;
         tevent:=datetimetotimestamp(now);
         //Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.StringGrid1.Rowcount+1;
         //Form_gwrap.StringGrid1.Cells[0,Form_gwrap.StringGrid1.Rowcount-1]:=datetimetostr(timestamptodatetime(tevent))+' - '+txt_jresumed;
         prevpause:=false;
         end;
      end;
   if ppriority<>prevpriority then //if priority has changed, update process priority and log the event
      begin
      case ppriority of
         1: begin
            P.Priority:=ppRealTime;
            Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority4;
            tevent:=datetimetotimestamp(now);
            end;
         2: begin
            P.Priority:=ppHigh;
            Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority3;
            tevent:=datetimetotimestamp(now);
            end;
         3: begin
            P.Priority:=ppNormal;
            Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority2;
            tevent:=datetimetotimestamp(now);
            end;
         4: begin
            P.Priority:=ppIdle;
            Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority1;
            tevent:=datetimetotimestamp(now);
            end;
         end;
      prevpriority:=ppriority;
      end;
   setlabel1text;
   if modeofuse>=20 then Sleep(100)
   else
      begin
      if P.output.NumBytesAvailable>0 then
         begin
         M.SetSize(BytesRead + max_l);
         i := P.Output.Read((M.Memory + BytesRead)^, max_l);
         if i>0 then
            begin
            astri:=bstri;
            SetString(stri, M.Memory, M.Size);
            bstri:=stri;
            if astri<>bstri then
               begin
               updatereport(M,stri);
               Form_gwrap.Label6.Caption:=stri;
               if Form_gwrap.label6.caption<>'' then Form_gwrap.Label6.Visible:=true else Form_gwrap.Label6.Visible:=false;
               end;
            end;
         end
      else i:=0;
      if i > 0 then Inc(BytesRead, i)
      else Sleep(100);
      end;
   end;
pstarted:=false;
Form_gwrap.Image4.Cursor:=crDefault;
Form_gwrap.Image4.Hint:='';
tsout:=datetimetotimestamp(now);
Form_gwrap.ProgressBar1.Position:=(Form_gwrap.ProgressBar1.Position+900) div 2; //process terminated
Application.ProcessMessages;
if modeofuse>=20 then
   begin
   exit_code:=P.ExitStatus;
   P.Free;
   end
else
   begin
   repeat
      M.SetSize(BytesRead + max_l);
      i := P.Output.Read((M.Memory + BytesRead)^, max_l);
      if i>0 then
            begin
            astri:=bstri;
            SetString(stri, M.Memory, M.Size);
            bstri:=stri;
            if astri<>bstri then
               begin
               updatereport(M,stri);
               Form_gwrap.Label6.Caption:=stri;
               if Form_gwrap.label6.caption<>'' then Form_gwrap.Label6.Visible:=true else Form_gwrap.Label6.Visible:=false;
               end;
            end;
      if i > 0 then Inc(BytesRead, i)
      else Sleep(100);
   until i <= 0;
   M.SetSize(BytesRead);
   exit_code:=P.ExitStatus;
   P.Free;
   Form_gwrap.Memo1.Clear;
   Form_gwrap.Memo1.Lines.LoadFromStream(M);

            SetString(stri, M.Memory, M.Size);
               stri:=AnsiReverseString(stri);
               stri:=copy(stri,pos(char($0A)+char($0D),stri)+2,length(stri)-(pos(char($0A)+char($0D),stri)));
               stri:=copy(stri,1,pos(char($0A)+char($0D),stri)-1);
               stri:=ReverseString(stri);
               Form_gwrap.Label6.Caption:=stri;
               if Form_gwrap.label6.caption<>'' then Form_gwrap.Label6.Visible:=true else Form_gwrap.Label6.Visible:=false;

   M.Free;
   Form_gwrap.StringGrid1.Rowcount:=Form_gwrap.Memo1.Lines.Count;
   for i:=0 to Form_gwrap.Memo1.Lines.Count-1 do Form_gwrap.StringGrid1.Cells[0,i]:=Form_gwrap.Memo1.Lines[i];//envtoutf8(Form_gwrap.Memo1.Lines[i]);
   Form_gwrap.Memo1.Clear;
   Form_gwrap.StringGrid1.Row:=Form_gwrap.StringGrid1.Rowcount-1;//0
   Form_gwrap.StringGrid1.AutosizeColumns;
   end;
Form_gwrap.Stringgrid1.Cursor:=crDefault;
Form_gwrap.ProgressBar1.Position:=995; //read from memory stream, if used
Application.ProcessMessages;
except
exit_code:=127; //"cannot execute" error
end;
tdiff:=((tsout.date-tsin.date)*24*60*60*1000)+tsout.time-tsin.time;
if tdiff<=0 then tdiff:=100000;
Form_gwrap.Notebook1.Pageindex:=0;
{$IFDEF MSWINDOWS}
{$ELSE}
if stopped=true then exit_code:=255;
{$ENDIF}
decode_exitcode(exit_code,s);
Form_gwrap.Label1.Caption:=s;
Form_gwrap.Label5.Caption:='';
Form_gwrap.Label6.Caption:='';
Form_gwrap.Label5.Visible:=false;
Form_gwrap.Label6.Visible:=false;
outsize:=0;
try
if ufileexists((outpath)) then
   begin
   uassignfile(f,(outpath));
   filemode:=0;
   reset(f);
   srcfilesize_multipart(outpath,outsize);
   closefile(f);
   end;
except
end;
if (outsize>0) then
   if (insize>0) and (Form_gwrap.labelo.visible=true) then
      begin
      cratio:=outsize * 100;
      cratio:=cratio div insize;
      Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize))+' ('+inttostr(cratio)+'%)';
      Form_gwrap.Shape1.Visible:=true;
      Form_gwrap.Shape2.Visible:=true;
      if cratio<BARMIN then Form_gwrap.Shape2.Width:=BARMIN
      else
         if cratio<100 then Form_gwrap.Shape2.Width:=(cratio * 32) div 100
         else Form_gwrap.Shape2.Width:=BARMAX;
      Form_gwrap.Shape1.Hint:=txt_crscale+' '+inttostr(cratio)+'%';
      Form_gwrap.Shape2.Hint:=Form_gwrap.Shape1.Hint;
      end
   else
      Form_gwrap.LabelInfo2.Caption:=nicenumber(inttostr(outsize));
Form_gwrap.Image4.Picture.Bitmap:=Binfo;
//speed
speed1:=0;
speed2:=0;
if tdiff<>0 then speed1:=tinsize div tdiff * 1000;
if tdiff<>0 then speed2:=outsize div tdiff * 1000;
Form_gwrap.LabelInfo3.Caption:=nicetime(inttostr(tdiff));
if speed1>0 then Form_gwrap.LabelInfo1.Caption:=Form_gwrap.LabelInfo1.Caption+' @ '+nicenumber(inttostr(speed1))+'/s';
if speed2>0 then Form_gwrap.LabelInfo2.Caption:=Form_gwrap.LabelInfo2.Caption+' @ '+nicenumber(inttostr(speed2))+'/s';
if modeofuse>=20 then
   begin
   Form_gwrap.StringGrid1.Rowcount:=5;
   Form_gwrap.StringGrid1.Cells[0,0]:=datetimetostr(timestamptodatetime(tsout))+' - '+s;
   Form_gwrap.StringGrid1.Cells[0,1]:='';
   Form_gwrap.StringGrid1.Cells[0,2]:=Form_gwrap.Labeli.Caption+' '+Form_gwrap.LabelInfo1.Caption;
   Form_gwrap.StringGrid1.Cells[0,3]:=Form_gwrap.Labelo.Caption+' '+Form_gwrap.LabelInfo2.Caption;
   Form_gwrap.StringGrid1.Cells[0,4]:=Form_gwrap.Labelt.Caption+' '+Form_gwrap.LabelInfo3.Caption;
   Form_gwrap.StringGrid1.AutosizeColumns;
   end;
Form_gwrap.ImagePriority.Visible:=false;
if exit_code=0 then Form_gwrap.Image4.Picture.Bitmap:=Bsuccess
else Form_gwrap.Image4.Picture.Bitmap:=Berror;
Form_gwrap.Button3.Visible:=false;
Form_gwrap.Button1.Visible:=true;
Form_gwrap.ImageButton2.Visible:=true;
Form_gwrap.ImageSavePJ.Visible:=true;
Form_gwrap.Button4.Visible:=false;
case exit_code of
   0: Form_gwrap.Caption:=txt_done+' '+Form_gwrap.Caption;
   255: Form_gwrap.Caption:=txt_halted+' '+Form_gwrap.Caption;
   else
      begin
      Form_gwrap.Caption:=txt_error+' '+Form_gwrap.Caption;
      if (modeofuse<>2) and (modeofuse<>3) then
         Form_gwrap.LabelWarning1.Visible:=true;
      end;
   end;
if Form_gwrap.LabelInfo2.width>Form_gwrap.LabelInfo3.width then i:=Form_gwrap.LabelInfo2.width
else i:=Form_gwrap.LabelInfo3.width;
Form_gwrap.shape1.left:=Form_gwrap.LabelInfo2.left+i+32;
Form_gwrap.shape2.left:=Form_gwrap.shape1.left;
case modeofuse of
   0: begin
      end;
   1: begin
      Form_gwrap.Notebook1.Pageindex:=1;
      end;
   2: begin
      Form_gwrap.Panel2.Visible:=true;
      Form_gwrap.PanelBench.Visible:=true;
      Form_gwrap.Shape1.Visible:=false;
      Form_gwrap.Shape2.Visible:=false;
      {$IFDEF MSWINDOWS}
      minline:=3;
      {$ELSE}
      minline:=4;
      {$ENDIF}
      try
      Form_gwrap.Label2.Caption:=txt_rating+' '+inttostr(strtoint(copy(Form_gwrap.Stringgrid1.Cells[0,minline+13],25,7)))+' ('+inttostr(strtoint(copy(Form_gwrap.Stringgrid1.Cells[0,minline+13],18,7)))+' MIPS)';
      Form_gwrap.Label3.Caption:=txt_threads+' '+inttostr(strtoint(copy(Form_gwrap.Stringgrid1.Cells[0,minline],47,4)))+' '+txt_hardware+'; '+inttostr(strtoint(copy(Form_gwrap.Stringgrid1.Cells[0,minline+1],47,4)))+' '+txt_software;
      except
      end;
      end;
   3: begin
      Form_gwrap.Notebook1.Pageindex:=1;
      Form_gwrap.Visible:=true;
      Form_gwrap.WindowState:=wsNormal;
      Form_gwrap.SetFocus;
      end;
   end;
Form_gwrap.ProgressBar1.Position:=1000; //presentation completated
ended:=true;
Application.ProcessMessages;
Form_gwrap.StringGrid1.AutosizeColumns;
if Form_gwrap.CheckBoxHalt.State=cbChecked then
   begin
   P:=TProcess.Create(nil);
   P.Options := [poUsePipes, poNoConsole];
   {$IFDEF MSWINDOWS}P.CommandLine:='shutdown /s /t 10';{$ENDIF}
   {$IFDEF LINUX}P.CommandLine:='halt';{$ENDIF}
   {$IFDEF FREEBSD}P.CommandLine:='halt';{$ENDIF}
   {$IFDEF NETBSD}P.CommandLine:='halt';{$ENDIF}
   if Form_gwrap.Visible=true then Application.ProcessMessages;
   scltest:=P.Commandline;
   if validatecl(scltest)<>0 then begin MessageDlg(txt_2_7_validatecl+' '+scltest, mtWarning, [mbOK], 0); exit; end;
   P.Execute;
   P.Free;
   end;
if autoopen=1 then
   if (modeofuse<>1) and (modeofuse<>2) then explore_out;
Form_gwrap.Page4.TabVisible:=false;
sleep(500);
case autoclose of
   1: begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>2) then
            Form_gwrap.Close
         else
            Form_gwrap.SetFocus
      else
         Form_gwrap.SetFocus;
      end;
   2: begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>2) then
            Form_gwrap.Close
         else
            begin
            Form_gwrap.Visible:=true;
            Form_gwrap.WindowState:=wsNormal;
            Form_gwrap.SetFocus;
            end
      else
         begin
         Form_gwrap.Visible:=true;
         Form_gwrap.WindowState:=wsNormal;
         Form_gwrap.SetFocus;
         end;
      end;
   3: Form_gwrap.Close;
   4: begin
      if exit_code=0 then
         if (modeofuse<>1) and (modeofuse<>2) then
            Form_gwrap.Close
         else
            begin
            Form_gwrap.Visible:=true;
            Form_gwrap.WindowState:=wsNormal;
            Form_gwrap.SetFocus;
            end
      else
         begin
         Form_gwrap.Visible:=true;
         Form_gwrap.WindowState:=wsNormal;
         Form_gwrap.SetFocus;
         end;
      end;
   end;
end;

{ TForm_gwrap }

procedure TForm_gwrap.Timer1Timer(Sender: TObject);
begin
if Form_gwrap.visible=true then set_items_height;
if launched=true then exit;
if fullmode=1 then exit;
launched:=true;
launch_cl;
end;

procedure TForm_gwrap.Timer2Timer(Sender: TObject);
begin
if stopped=true then exit;
if ppause=true then exit;
if ended=true then exit;
if pstarted=true then
   begin
      if modeofuse<20 then progress10;
      case pcount of
         1: begin Form_gwrap.Image4.Picture.Bitmap:=Bp2; pcount:=2; end;
         2: begin Form_gwrap.Image4.Picture.Bitmap:=Bp3; pcount:=3; end;
         3: begin Form_gwrap.Image4.Picture.Bitmap:=Bp4; pcount:=4; end;
         4: begin Form_gwrap.Image4.Picture.Bitmap:=Bp5; pcount:=5; end;
         5: begin Form_gwrap.Image4.Picture.Bitmap:=Bp6; pcount:=6; end;
         6: begin Form_gwrap.Image4.Picture.Bitmap:=Bp7; pcount:=7; end;
         7: begin Form_gwrap.Image4.Picture.Bitmap:=Bp8; pcount:=8; end;
         else begin Form_gwrap.Image4.Picture.Bitmap:=Bp1; pcount:=1; end;
         end;
   end;
end;

procedure TForm_gwrap.Button1Click(Sender: TObject);
begin
Close;
end;

procedure TForm_gwrap.ImageSavePJClick(Sender: TObject);
begin
save_cl;
end;

procedure TForm_gwrap.ButtonCancelClick(Sender: TObject);
begin
Form_gwrap.Close;
end;

procedure TForm_gwrap.ButtonEditName10Click(Sender: TObject);
begin
patheditor:=EditOpenOut;
popupmenupath.popup();
end;

procedure TForm_gwrap.ButtonEditName11Click(Sender: TObject);
begin
patheditor:=EditName3;
popupmenupath.popup();
end;

procedure TForm_gwrap.ButtonEditName3Click(Sender: TObject);
begin
if SaveDialog2.Execute then
   if SaveDialog2.FileName<>'' then EditName3.Text:=SaveDialog2.FileName
   else exit
else exit;
end;

procedure getoutfolder(var out_param:utf8string);
var
   out_created:boolean;
   s,s1:utf8string;
   i:integer;
begin
out_created:=false;
if Form_gwrap.Checkboxfolder.State=cbchecked then
   begin
   s1:=Form_gwrap.Caption;
   cutextension(s1);
   s:=out_param+s1;
   i:=0;
   repeat
      if not(udirectoryexists(s)) then
         try
         uforcedirectories(s);
         out_created:=true;
         except
         s:=out_param+'output';
         out_created:=true;
         end
      else
         begin
         i:=i+1;
         s:=out_param+s1+'-'+inttostr(i);
         if i=1000 then //to break recursivity if filename is not valid (ie unsupported character encoding)
            begin
            s:=out_param+'output';
            out_created:=true;
            end;
         end;
      {try
      umkdir(s); //no longer works in Lazarus 0.9.30, mkdir does not raise exception
      out_created:=true;
      except
      i:=i+1;
      s:=out_param+s1+'-'+inttostr(i);
      if i=1000 then //to break recursivity if filename is not valid (ie unsupported character encoding)
         begin
         s:=out_param+'unsupported_name';
         out_created:=true;
         end;
      end;}
   until out_created=true;
   if copy(s,length(s)-1,1)<>directoryseparator then s:=s+directoryseparator;
   out_param:=s;
   end;
end;

procedure clfromname(var in_param,out_param,pw: utf8string);
var
   j:integer;
   P:TProcess;
   s,jobcode,bin_name,overwrite_policy,archive_function,paq_ver,lpaq_ver,ext:utf8string;
   f: file of byte;
begin
   j:=testinput(cl1,true);
   case j of
      0: exit; //unsupported, since custom types can be handled by PeaZip only, and are not passed to PeaLauncher
      1: begin
         out_param:=Form_gwrap.EditOpenOut.Text;
         outpath:=out_param;
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'pea'+EXEEXT+delimiter;
         cl:=utf8toenv(bin_name)+' UNPEA '+in_param+' '+utf8toenv(out_param)+' RESETDATE SETATTR EXTRACT2DIR INTERACTIVE';
         end;
      2: begin
         out_param:=Form_gwrap.EditOpenOut.Text;
         outpath:=out_param;
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'pea'+EXEEXT+delimiter;
         cl:=utf8toenv(bin_name)+' RFJ '+in_param+' BATCH '+utf8toenv(out_param);
         end;
      5: begin
         s:=envtoutf8(cl);
         paq_ver:=copy(uextractfileext(s),2,length(uextractfileext(s))-1);
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         if out_param[length(out_param)]=directoryseparator then out_param:=copy(out_param,1,length(out_param)-1);
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'paq'+DirectorySeparator+paq_ver+EXEEXT+delimiter;
         cl:=utf8toenv(bin_name)+' -d '+in_param+' '+utf8toenv(out_param);
         end;
      6: begin
         s:=envtoutf8(cl);
         ext:=copy(uextractfileext(s),2,length(uextractfileext(s))-1);
         s:=uextractfilename(s);
         cutextension(s);
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         out_param:=out_param+s;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'quad'+DirectorySeparator+ext+EXEEXT+delimiter;
         if ext='quad' then cl:=utf8toenv(bin_name)+' -d '+in_param+' '+utf8toenv(out_param)
         else cl:=utf8toenv(bin_name)+' d '+in_param+' '+utf8toenv(out_param);
         end;
      7: begin
         if not(ufileexists((peazippath+'res'+directoryseparator+'unace'+directoryseparator+'unace'+EXEEXT))) then
            begin
            MessageDlg(txt_2_5_ace_missing, mtWarning, [mbOK], 0);
            Halt;
            end;
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'unace'+DirectorySeparator+'unace'+EXEEXT+delimiter;
         cl:=utf8toenv(bin_name)+' x '+in_param;
         if out_param<>'' then usetcurrentdir(out_param);
         end;
      8: begin
         s:=envtoutf8(cl);
         lpaq_ver:=copy(uextractfileext(s),2,length(uextractfileext(s))-1);
         s:=uextractfilename(s);
         cutextension(s);
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         out_param:=out_param+s;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'lpaq'+DirectorySeparator+lpaq_ver+EXEEXT+delimiter;
         cl:=utf8toenv(bin_name)+' d '+in_param+' '+utf8toenv(out_param);
         end;
      9: begin
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         out_param[length(out_param)]:='/';
         out_param:=delimiter+'-dp'+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'arc'+DirectorySeparator+'arc'+EXEEXT+delimiter;
         if Form_gwrap.Checkboxfolder1.State=cbchecked then archive_function:='e' else archive_function:='x';
         case Form_gwrap.ComboBoxOverwrite.ItemIndex of
            0: overwrite_policy:='-o+';//overwrite
            1: overwrite_policy:='-o-';//skip
            end;
         cl:=utf8toenv(bin_name)+' '+archive_function+' '+overwrite_policy+' '+in_param;//for security and avoid blocking out of console mode, skip existing files is preset as oeverwrite policy
         if pw<>'' then cl:=cl+' '+utf8toenv(pw);
         cl:=cl+' '+utf8toenv(out_param);
         end;
     10: begin
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         if out_param[length(out_param)]=directoryseparator then out_param:=copy(out_param,1,length(out_param)-1);
         out_param:=delimiter+escapefilename(out_param,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'zpaq'+DirectorySeparator+'zpaq'+EXEEXT+delimiter;
         cl:=bin_name+' x '+in_param;
         end;
      else
         begin
         case Form_gwrap.ComboBoxOverwrite.ItemIndex of
            0: overwrite_policy:='-aou';//auto rename extracting files
            1: overwrite_policy:='-aot';//auto rename existing files
            2: overwrite_policy:='-aoa';//overwrite all existing files
            3: overwrite_policy:='-aos';//skip existing files
            end;
         out_param:=Form_gwrap.EditOpenOut.Text;
         if Form_gwrap.Checkboxfolder.State=cbchecked then getoutfolder(out_param);
         outpath:=out_param;
         out_param:=delimiter+'-o'+escapefilename(out_param,desk_env)+delimiter;
         in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
         bin_name:=delimiter+escapefilename(peazippath,desk_env)+'res'+DirectorySeparator+'7z'+DirectorySeparator+'7z'+EXEEXT+delimiter;
         if Form_gwrap.Checkboxfolder1.State=cbchecked then archive_function:='e' else archive_function:='x';
         cl:=utf8toenv(bin_name)+' '+archive_function;
         cl:=cl+' '+overwrite_policy;
         cl:=cl+' '+utf8toenv(out_param);
         cl:=cl+' '+utf8toenv(pw);
         cl:=cl+' '+in_param;
         end;
      end;
end;

procedure msg_pw_errorchar;
var
   errchar:utf8string;
begin
{$IFDEF MSWINDOWS}
errchar:='"';
{$ELSE}
errchar:='''';
{$ENDIF}
MessageDlg(errchar+' '+txt_2_3_pw_errorchar_gwrap, mtWarning, [mbOK], 0);
end;

procedure launch_fullmode;
var
   in_param,out_param,pw:utf8string;
begin
pw:=Form_gwrap.EditUn7zaPW.Text;
if Form_gwrap.EditName3.Text<>'' then
   if prepend_keyfile(pw,Form_gwrap.EditName3.Text)<>0 then
      begin
      MessageDlg(Form_gwrap.EditName3.Text+' '+txt_2_3_kf_not_found_gwrap, mtError,[mbOk],0);
      exit;
      end;
if pw<>'' then
   begin
   if pw4cl('-p',pw)<>0 then
      begin
      msg_pw_errorchar;
      exit;
      end;
   end
else pw:='-pdefault';
outpath:=Form_gwrap.EditOpenOut.Text;
clfromname(in_param,out_param,pw);
Form_gwrap.Panel4.Visible:=false;
Form_gwrap.Notebook1.visible:=true;
Form_gwrap.Button3.Visible:=true;
Form_gwrap.Button4.Visible:=true;
Form_gwrap.ButtonOK.Visible:=false;
Form_gwrap.ButtonCancel.Visible:=false;
fullmode:=0;
end;

procedure TForm_gwrap.ButtonOkClick(Sender: TObject);
begin
if cl<>'' then launch_fullmode;
end;

procedure TForm_gwrap.ButtonOutputArchiveClick(Sender: TObject);
begin
if SelectDirectoryDialog1.Execute then
   if SelectDirectoryDialog1.FileName<>'' then
      begin
      EditOpenOut.Text:=SelectDirectoryDialog1.FileName;
      if EditOpenOut.Text[length(EditOpenOut.Text)]<>directoryseparator then
         EditOpenOut.Text:=EditOpenOut.Text+directoryseparator;
      end
   else exit
else exit;
end;

procedure TForm_gwrap.cbAutoOpenClick(Sender: TObject);
begin
if autoopen=1 then autoopen:=0
else autoopen:=1;
end;

procedure TForm_gwrap.FormDestroy(Sender: TObject);
begin
{$IFDEF MSWINDOWS}CloseHandle(psem);{$ENDIF}
end;

procedure TForm_gwrap.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var s:utf8string;
begin
s:=escapefilename(FileNames[0],desk_env);
if s<>'' then
   begin
   OpenDialog1.FileName:=s;
   cl:=utf8toenv(OpenDialog1.FileName);
   outpath:=uextractfilepath(envtoutf8(cl));
   try
      uassignfile(f,OpenDialog1.FileName);
      filemode:=0;
      reset(f);
      srcfilesize_multipart(OpenDialog1.FileName,insize);
      closefile(f);
   except
      insize:=0;
   end;
   preparefull;
   end;
end;

procedure TForm_gwrap.FormWindowStateChange(Sender: TObject);
begin
if pstarted=false then exit;
case autoclose of
   1: if Form_gwrap.WindowState=wsMinimized then
         begin
         autoclose:=4;
         umkdir(confpath+'.minimized')
         end;
   4: begin
      autoclose:=1;
      if udirectoryexists(confpath+'.minimized') then urmdir(confpath+'.minimized');
      Form_gwrap.Visible:=true;
      Form_gwrap.WindowState:=wsNormal;
      Form_gwrap.Position:=poScreenCenter;
      Form_gwrap.Width:=560;
      Form_gwrap.Height:=260;
      Form_gwrap.WindowState:=wsMaximized;
      Form_gwrap.WindowState:=wsNormal;
      fixbringtofront;
      end;
   end;
end;

procedure TForm_gwrap.ImagePriorityClick(Sender: TObject);
begin
popupmenu1.popup();
end;

procedure TForm_gwrap.ImageButton2Click(Sender: TObject);
begin
save_report;
end;

procedure TForm_gwrap.Button3Click(Sender: TObject);
begin
stopped:=true;
end;

procedure toggle_pause;
begin
ppause:=not(ppause);
if ppause=false then
   begin
   Form_gwrap.Button4.Caption:=txt_pause;
   Form_gwrap.pmpause.Caption:=txt_pause;
   end
else
   begin
   Form_gwrap.Button4.Caption:=txt_resume;
   Form_gwrap.pmpause.Caption:=txt_resume;
   end;
end;

procedure TForm_gwrap.Button4Click(Sender: TObject);
begin
toggle_pause;
end;

procedure prepare_7zdrop;
begin
with form_gwrap do
   begin
   ComboBoxOverwrite.Clear;
   ComboBoxOverwrite.DropDownCount:=4;
   ComboBoxOverwrite.Items.Append(txt_2_3_renameextracted);
   ComboBoxOverwrite.Items.Append(txt_2_3_renameexisting);
   ComboBoxOverwrite.Items.Append(txt_2_3_overexisting);
   ComboBoxOverwrite.Items.Append(txt_2_3_skipexisting);
   ComboBoxOverwrite.ItemIndex:=0;
   ComboBoxOverwrite.Visible:=true;
   end;
end;

procedure prepare_arcdrop;
begin
with form_gwrap do
   begin
   ComboBoxOverwrite.Clear;
   ComboBoxOverwrite.DropDownCount:=2;
   ComboBoxOverwrite.Items.Append(txt_2_3_overexisting);
   ComboBoxOverwrite.Items.Append(txt_2_3_skipexisting);
   ComboBoxOverwrite.ItemIndex:=1;
   ComboBoxOverwrite.Visible:=true;
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
   if Succeeded(ShGetSpecialFolderLocation(Form_gwrap.Handle,26,pidl)) then //26 is CSIDL_APPDATA numerical value
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

procedure preparefull;
var
   j:integer;
begin
with form_gwrap do
begin
Form_gwrap.Visible:=true;
Form_gwrap.WindowState:=wsNormal;
cl1:=envtoutf8(cl);
j:=testinput(cl1,true);
case j of
   3: prepare_7zdrop;
   4: prepare_7zdrop;
   9: prepare_arcdrop;
   else ComboBoxOverwrite.Visible:=false;
end;
Panel4.Visible:=true;
Notebook1.visible:=false;
Button3.Visible:=false;
Button4.Visible:=false;
ButtonOK.Visible:=true;
if cl<>'' then ButtonOk.Enabled:=true else ButtonOk.Enabled:=false;
ButtonCancel.Visible:=true;
EditOpenOut.Text:=outpath;
Form_gwrap.caption:=uextractfilename(envtoutf8(cl));
end;
end;

procedure TForm_gwrap.FormCreate(Sender: TObject);
var
   i,j:integer;
   s,theme_name:utf8string;
begin
launched:=true;//prevent the timer to launch cl, in case it starts before variables are valorized
pstarted:=false;
ended:=false;
sizeok:=false;
launchwithsemaphore:=false;
Button1.Visible:=false;
ImageButton2.Visible:=false;
ImageSavePJ.Visible:=false;
Form_gwrap.Label5.Visible:=false;
Form_gwrap.Label6.Visible:=false;
ppriority:=0;
Form_gwrap.Notebook1.Pageindex:=0;
getdesk_env(desk_env,caption_build,delimiter);
executable_path:=uextractfilepath(envtoutf8(paramstr(0)));
if executable_path[length(executable_path)]<>directoryseparator then executable_path:=executable_path+directoryseparator;
usetcurrentdir(executable_path);
peazippath:=executable_path;
if peazippath[length(peazippath)]=directoryseparator then setlength(peazippath,length(peazippath)-1);
peazippath:=uextractfilepath(peazippath);

//get job type 0 archive/extract 1 list/test; 2 benchmark; 20 archive/extract, not using pipes, visible console (mode 10 was removed)
try
modeofuse:=strtoint(paramstr(1));
fullmode:=0;
automode:=0;
insize:=0;
if modeofuse>=1000 then
   begin
   launchwithsemaphore:=true;
   modeofuse:=modeofuse-1000;
   end;
case modeofuse of
   100: begin fullmode:=1; modeofuse:=0; end;
   120: begin fullmode:=1; modeofuse:=20; end;
   end;
except
   fullmode:=1;
   modeofuse:=0;
   automode:=1;
end;
case modeofuse of
   0: max_l:=1024;//1024
   1: max_l:=1024;
   2: max_l:=32;
   3: max_l:=32;
   end;
//use semaphore if needed to limit job execution to one instance
getschedule;

//get selected input size and input file size
tinsize:=0;
if automode=0 then
   try
      if paramstr(3)<>'na' then
         begin
         srcfilesize_multipart(envtoutf8(paramstr(3)),tinsize);
         in_name:=paramstr(3);
         end
      else in_name:='';
      insize:=strtoqword(paramstr(2));
   except
      insize:=0;
   end
else
   if paramcount>0 then
      begin
      if paramstr(1)='-ext2here' then //automode: set s = paramstr 2 if performing "extract here", else s = paramstr 1
         if paramcount>1 then s:=paramstr(2)
         else
      else s:=paramstr(1);
      try
         uassignfile(f,envtoutf8(s));
         filemode:=0;
         reset(f);
         srcfilesize_multipart(envtoutf8(s),insize);
         closefile(f);
      except
         insize:=0;
      end;
      end;
ProgressBar1.Max:=1000;
if insize > 0 then
   begin
   incstep1:=80000000000 div insize;
   incstep2:=120000000000 div insize;
   incstep3:=40000000000 div insize;
   if incstep1=0 then incstep1:=1;
   if incstep2=0 then incstep2:=1;
   if incstep3=0 then incstep3:=1;
   end
else
   begin
   incstep1:=2000;
   incstep2:=4000;
   incstep3:=1000;
   end;
if insize=0 then
   begin
   Form_gwrap.Labeli.Visible:=false;
   Form_gwrap.LabelInfo1.Visible:=false;
   end;
Form_gwrap.Labelo.Visible:=false;
Form_gwrap.LabelInfo2.Visible:=false; //set visible during progress

//get output path (finalized only when invoked)
if automode=0 then
   outpath:=envtoutf8(paramstr(4))
else
   if paramcount>0 then
      outpath:=uextractfilepath(envtoutf8(s));

//get cl
cl:='';
if automode=0 then
   begin
   if fullmode<>1 then
      begin
      for i:=5 to paramcount do
         if pos(' ',paramstr(i))<>0 then cl:=cl+delimiter+paramstr(i)+delimiter+' '
         else cl:=cl+paramstr(i)+' ';
      end
   else
      cl:=paramstr(5);
   end
else
   if paramcount>0 then cl:=s;
   {else //in PeaZip 4.0 starting gwrap allows dropping files to the application rather than starting the file selection dialog
      if OpenDialog1.execute then
         if OpenDialog1.FileName<>'' then
            begin
            cl:=utf8toenv(OpenDialog1.FileName);
            outpath:=uextractfilepath(envtoutf8(cl));
            try
               uassignfile(f,OpenDialog1.FileName);
               filemode:=0;
               reset(f);
               srcfilesize_multipart(OpenDialog1.FileName,insize);
               closefile(f);
            except
               insize:=0;
            end;
            end
         else Form_gwrap.Close
      else Form_gwrap.Close;}

try
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
   if not(udirectoryexists(confpath)) then uforcedirectories(confpath);
   if (confpath[1]='.') and (confpath[2]='.') then confpath:='..'+directoryseparator+confpath; //relative path, needs to be adjusted since pealauncher is in a subfolder of peazip path
   confpath:=uexpandfilename(confpath);
   if confpath[length(confpath)]<>directoryseparator then confpath:=confpath+directoryseparator;
   if not(udirectoryexists(confpath)) then confpath:=executable_path; //if alternative configuration directory does not exist or is not accessible, use res path
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
   readln(conf,dummy);
   //rowheight:=strtoint(dummy);
   readln(conf,dummy);
   //itemheight:=strtoint(dummy);
   readln(conf,dummy);
   //autosizeitemheight:=strtoint(dummy);
   readln(conf,dummy);
   readln(conf,dummy);
   readln(conf,lang_file);
   readln(conf,dummy);
   readln(conf,dummy);
   readln(conf,dummy);
   autoclose:=strtoint(dummy);
   readln(conf,dummy);
   readln(conf,dummy);
   readln(conf,dummy);
   autoopen:=strtoint(dummy);
   CloseFile(conf);
   if opacity<0 then opacity:=0;
   if opacity>100 then opacity:=100;
   if color1='' then color1:=colortostring(clWindow);
   if color2='' then color2:=colortostring(clBtnFace);
   if color3='' then color3:=colortostring(clBtnFace);
   if color4='' then color4:='$00669999';
   if color5='' then color5:=colortostring(clWindowText);
   //if (rowheight<12) or (rowheight>32) then rowheight:=18;
   //if (itemheight<12) or (itemheight>32) then itemheight:=21;
   //if (autosizeitemheight<0) or (autosizeitemheight>1) then autosizeitemheight:=1;
   if (autoclose<0) or (autoclose>3) then autoclose:=1;
   if (autoopen<0) or (autoopen>1) then autoopen:=0;
except
   graphicsfolder:='themes'+directoryseparator+DEFAULT_THEME+directoryseparator;
   udodirseparators(graphicsfolder);
   opacity:=100;
   color1:=colortostring(clWindow);
   color2:=colortostring(clBtnFace);
   color3:=colortostring(clBtnFace);
   color4:='$00669999';
   color5:=colortostring(clWindowText);
   //rowheight:=18;
   //itemheight:=21;
   //autosizeitemheight:=1;
   autoclose:=1;
   autoopen:=0;
   cbAutoClose.State:=cbUnchecked;
   lang_file:='default.txt';
end;
if autoclose=1 then if udirectoryexists(confpath+'.minimized') then autoclose:=4;
if (autoclose<>2) and (autoclose<>4) then
   begin
   Form_gwrap.Visible:=true;
   Form_gwrap.WindowState:=wsNormal;
   Form_gwrap.Position:=poScreenCenter;
   Form_gwrap.Width:=560;
   Form_gwrap.Height:=260;
   end;
{$IFDEF MSWINDOWS}getwinenv(wincomspec,winver);{$ENDIF}
apply_theme;
load_icons;
texts(lang_file);
if fullmode=1 then preparefull;
Form_gwrap.caption:=txt_4_0_dragorselect;
if automode=1 then Label4.visible:=false;
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
stopped:=false;
launched:=false;
if automode=1 then
   if paramstr(1)='-ext2here' then launch_fullmode; //automode: launch process immediately if "extract here" is requested
end;

procedure TForm_gwrap.l2Click(Sender: TObject);
begin
explore_in;
end;

procedure TForm_gwrap.l4Click(Sender: TObject);
begin
explore_out;
end;

procedure TForm_gwrap.Label4Click(Sender: TObject);
var
   P:TProcess;
   bin_name,in_param:utf8string;
begin
P:=TProcess.Create(nil);
in_param:=delimiter+escapefilename(cl,desk_env)+delimiter;
bin_name:=delimiter+escapefilename(peazippath,desk_env)+'peazip'+EXEEXT+delimiter;
{$IFDEF MSWINDOWS}P.Options := [poNoConsole];{$ELSE}P.Options := [poNoConsole, poWaitOnExit];{$ENDIF}
cl:=bin_name+' -ext2open '; //ext2open handles a single input in open interface
cl:=utf8toenv(cl);
cl:=cl+in_param;//(cl was not trasformed in utf8 before)
P.CommandLine:=cl;
if validatecl(cl)<>0 then begin MessageDlg(txt_2_7_validatecl+' '+cl, mtWarning, [mbOK], 0); exit; end;
P.Execute;
P.Free;
Application.Terminate;
end;

procedure cp_search(desk_env:byte);
begin
{$IFDEF MSWINDOWS}
if winver='nt6+' then
   shellexecutew(Form_gwrap.handle, PWideChar('find'), PWideChar(''), PWideChar(''), PWideChar (''), SW_SHOWNORMAL)
else
   cp_open(executable_path+'res'+directoryseparator+'empty.fnd',desk_env);
{$ENDIF}
{$IFDEF LINUX}cp_search_linuxlike(desk_env);{$ENDIF}//try to search via Gnome or KDE
{$IFDEF FREEBSD}cp_search_linuxlike(desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_search_linuxlike(desk_env);{$ENDIF}
end;

procedure TForm_gwrap.labelopenfile0Click(Sender: TObject);
begin
cp_search(desk_env);
end;

procedure TForm_gwrap.labelopenfile2Click(Sender: TObject);
begin
if OpenDialog1.execute then
   if OpenDialog1.FileName<>'' then
      begin
      cl:=utf8toenv(OpenDialog1.FileName);
      outpath:=uextractfilepath(envtoutf8(cl));
      try
         uassignfile(f,OpenDialog1.FileName);
         filemode:=0;
         reset(f);
         srcfilesize_multipart(OpenDialog1.FileName,insize);
         closefile(f);
      except
         insize:=0;
      end;
      preparefull;
      end;
end;

procedure TForm_gwrap.LabelWarning1Click(Sender: TObject);
var s:utf8string;
begin
if modeofuse=1 then s:=txt_3_0_arc+char($0D)+char($0A)+txt_3_0_ext
else
   if uextractfilename(outpath)='' then s:=txt_3_0_arc+char($0D)+char($0A)+txt_3_0_ext
   else s:=txt_3_0_arc;
s:=s+char($0D)+char($0A)+txt_3_0_details;
MessageDlg(s, mtWarning, [mbOK], 0);
end;

procedure TForm_gwrap.MenuItem1Click(Sender: TObject);
begin
ppriority:=1;
Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority4;
end;

procedure TForm_gwrap.MenuItem2Click(Sender: TObject);
begin
ppriority:=2;
Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority3;
end;

procedure TForm_gwrap.MenuItem3Click(Sender: TObject);
begin
ppriority:=3;
Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority2;
end;

procedure TForm_gwrap.MenuItem4Click(Sender: TObject);
begin
ppriority:=4;
Form_gwrap.ImagePriority.Picture.Bitmap:=Bpriority1;
end;

procedure TForm_gwrap.mpathexploreClick(Sender: TObject);
var
   s:utf8string;
begin
s:=patheditor.Text;
if s='' then exit;
if s[1]='<' then exit;
s:=uextractfilepath(s);
cp_open(s,desk_env);
end;

procedure TForm_gwrap.mpathresetClick(Sender: TObject);
begin
patheditor.Text:='';
if patheditor=EditOpenOut then EditOpenOut.Text:=outpath;
end;

procedure TForm_gwrap.pmeiClick(Sender: TObject);
begin
explore_in;
end;

procedure TForm_gwrap.pmeoClick(Sender: TObject);
begin
explore_out;
end;

procedure do_explorepath;
begin
{$IFDEF MSWINDOWS}
if winver<>'nt6+' then
   ShellExecute(Form_gwrap.Handle, PChar ('open'), PChar('Explorer'), PChar (''), PChar (''), SW_SHOWNORMAL)
else
   ShellExecute(Form_gwrap.Handle, PChar ('open'), PChar('Explorer'), PChar ('/E,::{20D04FE0-3AEA-1069-A2D8-08002B30309D}'), PChar (''), SW_SHOWNORMAL);
{$ELSE}
cp_open('/',desk_env);
{$ENDIF}
end;

procedure TForm_gwrap.pmexploreClick(Sender: TObject);
begin
do_explorepath;
end;

procedure TForm_gwrap.pmpauseClick(Sender: TObject);
begin
toggle_pause;
end;

procedure TForm_gwrap.pmsearchClick(Sender: TObject);
begin
cp_search(desk_env);
end;

procedure TForm_gwrap.pmstopClick(Sender: TObject);
begin
stopped:=true;
end;

initialization
  {$I unit_gwrap.lrs}

  {$IFDEF MSWINDOWS}
  OleInitialize(nil);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  OleUninitialize
  {$ENDIF}

end.

