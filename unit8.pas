unit Unit8; 

{$mode objfpc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, Menus, ButtonPanel, StdCtrls, ExtCtrls, Buttons, ComCtrls,
  pea_utils,list_utils, ansiutf8_utils;

type

  { TFormPM }

  TFormPM = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonEditName3: TButton;
    ButtonEditName4: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    EditName3: TEdit;
    EditName4: TEdit;
    EditUn7zaPW: TEdit;
    EditUn7zaPW1: TEdit;
    EditUn7zaPW2: TEdit;
    ImageInfoPM1: TImage;
    LableListPath1: TLabel;
    LableListPath2: TLabel;
    LableListPath3: TLabel;
    LablePMHint: TLabel;
    MemoPM: TMemo;
    MenuItem1: TMenuItem;
    pmexpenc: TMenuItem;
    pmexpplain: TMenuItem;
    pmexp: TMenuItem;
    mpw: TMenuItem;
    mnote: TMenuItem;
    MenuItem2: TMenuItem;
    mpwexplore: TMenuItem;
    mpwreset: TMenuItem;
    mremove: TMenuItem;
    MenuItem5: TMenuItem;
    mreset: TMenuItem;
    mnew: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialogKFM: TOpenDialog;
    PageControl1: TPageControl;
    PanelPWMaster: TPanel;
    pmPM: TPopupMenu;
    PopupMenupwm: TPopupMenu;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StringGridPM: TStringGrid;
    StringGridPM1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonEditName3Click(Sender: TObject);
    procedure ButtonEditName4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImageInfoPM1Click(Sender: TObject);
    procedure pmexpcsvClick(Sender: TObject);
    procedure mnoteClick(Sender: TObject);
    procedure mpwClick(Sender: TObject);
    procedure mnewClick(Sender: TObject);
    procedure mpwexploreClick(Sender: TObject);
    procedure mpwresetClick(Sender: TObject);
    procedure mremoveClick(Sender: TObject);
    procedure mresetClick(Sender: TObject);
    procedure pmexpencClick(Sender: TObject);
    procedure pmexpplainClick(Sender: TObject);
    procedure StringGridPMColRowMoved(Sender: TObject; IsColumn: Boolean;
      sIndex, tIndex: Integer);
    procedure StringGridPMDblClick(Sender: TObject);
    procedure StringGridPMKeyPress(Sender: TObject; var Key: char);
    procedure StringGridPMMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormPM: TFormPM;
  txt_edit,caption_build,delimiter,wincomspec,winver,validate_txt,local_desktop,confpath:utf8string;
  Column,Row:integer;
  desk_env:byte;
  pmfilet:text;

implementation

{ TFormPM }

procedure new_pm;
var
  i,rc:integer;
  s:utf8string;
begin
if InputQuery(txt_edit+' '+FormPM.mpw.Caption, '', s) then
   begin
   rc:=FormPM.StringGridPM.Rowcount+1;
   FormPM.StringGridPM.RowCount:=rc;
   FormPM.StringGridPM.Cells[1,rc-1]:=s;
   FormPM.StringGridPM.Cells[2,rc-1]:='';
   if rc>2 then
      for i:=1 to rc-2 do FormPM.StringGridPM.Cells[3,i]:='0'
   else
      i:=0;
   FormPM.StringGridPM.Cells[3,rc-1]:='1';
   FormPM.StringGridPM.Cells[0,rc-1]:=inttostr(i+1);
   FormPM.StringGridPM.AutosizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure editpw_pm;
var s:utf8string;
begin
if FormPM.StringGridPM.Row=0 then
   begin
   new_pm;
   exit;
   end;
s:=FormPM.StringGridPM.Cells[1,FormPM.StringGridPM.Row];
if InputQuery(txt_edit+' '+FormPM.mpw.Caption, '', s) then
   begin
   FormPM.StringGridPM.Cells[1,FormPM.StringGridPM.Row]:=s;
   FormPM.StringGridPM.AutoSizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure setdescription_pm;
var s:utf8string;
begin
if FormPM.StringGridPM.Row=0 then
   begin
   new_pm;
   exit;
   end;
s:=FormPM.StringGridPM.Cells[2,FormPM.StringGridPM.Row];
if InputQuery(txt_edit+' '+FormPM.mnote.Caption, '', s) then
   begin
   FormPM.StringGridPM.Cells[2,FormPM.StringGridPM.Row]:=s;
   FormPM.StringGridPM.AutoSizeColumns;
   FormPM.StringGridPM.ColWidths[3]:=0;
   end;
end;

procedure TFormPM.mpwClick(Sender: TObject);
begin
editpw_pm;
end;

procedure TFormPM.mnoteClick(Sender: TObject);
begin
setdescription_pm;
end;

procedure TFormPM.ButtonEditName4Click(Sender: TObject);
var p:tpoint;
begin
p.x:=buttoneditname4.left;
p.y:=panelpwmaster.top+buttoneditname4.top+buttoneditname4.height;
p:=clienttoscreen(p);
popupmenupwm.popup(p.x,p.y);
end;

procedure TFormPM.FormCreate(Sender: TObject);
begin
getdesk_env(desk_env,caption_build,delimiter);
{$IFDEF MSWINDOWS}
getwinenv(wincomspec,winver);
{$ENDIF}
end;

procedure TFormPM.ImageInfoPM1Click(Sender: TObject);
begin
MessageDlg(ImageInfoPM1.Hint, mtInformation, [mbOK], 0);
end;

procedure TFormPM.pmexpcsvClick(Sender: TObject);
begin

end;

procedure TFormPM.ButtonEditName3Click(Sender: TObject);
begin
if OpenDialogKFM.Execute then
   if OpenDialogKFM.FileName<>'' then EditName3.Text:=OpenDialogKFM.FileName
   else exit
else exit;
end;

procedure TFormPM.Button1Click(Sender: TObject);
begin
new_pm;
end;

procedure TFormPM.Button2Click(Sender: TObject);
var
   i:integer;
begin
if StringGridPM.RowCount<2 then exit;
StringGridPM.DeleteColRow(false,Row);
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.mnewClick(Sender: TObject);
begin
new_pm;
end;

//open, cross platform, with sanitization of string passed to the function
function cp_open(s:utf8string; desk_env:byte):integer;
var
   w:widestring;
begin
cp_open:=-1;
if s='' then exit;
if validatecl(s)<>0 then begin MessageDlg(validate_txt+' '+s, mtWarning, [mbOK], 0); exit; end;
{$IFDEF MSWINDOWS}
w:=utf8decode(s);
cp_open:=ShellExecuteW(FormPM.Handle, PWideChar ('open'), PWideChar(w), PWideChar (''), PWideChar (''), SW_SHOWNORMAL);
if cp_open<33 then
   cp_open:=shellexecuteW(FormPM.handle,PWideChar('open'),PWideChar('RUNDLL32.EXE'),PWideChar('shell32.dll,OpenAs_RunDLL '+w),PWideChar (''), SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF LINUX}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}//try to open via Gnome or KDE
{$IFDEF FREEBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
{$IFDEF NETBSD}cp_open:=cp_open_linuxlike(s,desk_env);{$ENDIF}
end;

procedure TFormPM.mpwexploreClick(Sender: TObject);
var
   s:utf8string;
begin
s:=uextractfilepath(EditName3.Text);
cp_open(s,desk_env);
end;

procedure TFormPM.mpwresetClick(Sender: TObject);
begin
EditName3.Text:='';
end;

procedure TFormPM.mremoveClick(Sender: TObject);
var
   i:integer;
begin
if StringGridPM.RowCount<2 then exit;
StringGridPM.DeleteColRow(false,Row);
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.mresetClick(Sender: TObject);
begin
StringGridPM.RowCount:=1;
end;

procedure TFormPM.pmexpencClick(Sender: TObject);
var
   numread,numwritten:integer;
   fin,fout:file of byte;
   read_data:array[0..65535]of byte;
begin
try
SelectDirectoryDialog1.InitialDir:=local_desktop;
if SelectDirectoryDialog1.Execute then
   if SelectDirectoryDialog1.FileName<>'' then
      begin
      uassignfile(fin,(confpath+'pm'));
      filemode:=0;
      reset(fin);
      uassignfile(fout,(SelectDirectoryDialog1.FileName+directoryseparator+'pm'+formatdatetime('-yyyymmdd-hh.nn.ss',now)+'.bak'));
      rewrite(fout);
      repeat
         blockread (fin,read_data,65536,numread);
         blockwrite (fout,read_data,numread,numwritten);
      until ((numread=0) or (numread<>numwritten));
      closefile(fin);
      closefile(fout);
      end;
except
   try
   closefile(fin);
   closefile(fout);
   except
   end;
end;
end;

procedure TFormPM.pmexpplainClick(Sender: TObject);
var
   i,j,rc:integer;
begin
try
SelectDirectoryDialog1.InitialDir:=local_desktop;
if SelectDirectoryDialog1.Execute then
   if SelectDirectoryDialog1.FileName<>'' then
      begin
      uassigntext(pmfilet,(SelectDirectoryDialog1.FileName+directoryseparator+'pm'+formatdatetime('-yyyymmdd-hh.nn.ss',now)+'.txt'));
      rewrite(pmfilet);
      write_header(pmfilet);
      rc:=FormPM.StringGridPM.RowCount;
      for i:=1 to rc-1 do
          begin
          for j:=1 to 2 do writeln(pmfilet,FormPM.StringGridPM.Cells[j,i]);
          writeln(pmfilet,'');
          writeln(pmfilet,'');
          writeln(pmfilet,'');//3 free slots for each entry, for future use
          end;
      CloseFile(pmfilet);
      end;
except
   try
   CloseFile(pmfilet);
   except
   end;
end;
end;

procedure TFormPM.StringGridPMColRowMoved(Sender: TObject; IsColumn: Boolean;
  sIndex, tIndex: Integer);
var
   i:integer;
begin
for i:=1 to (StringGridPM.RowCount-1) do StringGridPM.Cells[0,i]:=inttostr(i);
end;

procedure TFormPM.StringGridPMDblClick(Sender: TObject);
begin
case StringGridPM.Col of
   1: editpw_pm;
   2: setdescription_pm;
   end;
end;

procedure TFormPM.StringGridPMKeyPress(Sender: TObject; var Key: char);
var
   i:integer;
begin
if StringGridPM.Rowcount<2 then exit;
//search char
if ((Column<1) or (Column>2)) then Column:=1;
for i:=StringGridPM.Row+1 to StringGridPM.Rowcount-1 do
   if upcase(copy(StringGridPM.Cells[Column,i],1,1))=upcase(Key) then
      begin
      StringGridPM.Row:=i;
      exit;
      end;
for i:=1 to StringGridPM.Row do
   if upcase(copy(StringGridPM.Cells[Column,i],1,1))=upcase(Key) then
      begin
      StringGridPM.Row:=i;
      exit;
      end;
end;

procedure TFormPM.StringGridPMMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
StringGridPM.MouseToCell(X, Y, Column, Row);
StringGridPM.Row:=Row;
end;

initialization
  {$I unit8.lrs}

end.

