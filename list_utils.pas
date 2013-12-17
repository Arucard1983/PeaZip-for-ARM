unit list_utils;

{
 DESCRIPTION     :  Unit providing UI-neutral routines related to listing,
                    counting and check accessibility of objects in a given path

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20060831  G.Tani      Initial version
 0.11     20060917  G.Tani      removed *_VER; P_RELEASE constant in pea_utils
                                is used to keep track of release level;
                                changed extractdirname: if the path is a root (i.e.
                                disk name in Windows) the path becomes the dir path
                                and the dir name is blanked;
 0.12     20060925  G.Tani      fixed a bug in extractdirname showing up only
                                with Lazarus 0.9.18
 0.13     20080721  G.Tani      Enabled utf8
 0.14     20080825  G.Tani      Added srcfilesize to get file size using TSearchRec
 0.15     20090430  G.Tani      Added rCount for counting files in a path (optionally recursive)
 0.16     20090530  G.Tani      Added srcfilesize_multipart to get size of spanned archives (found on the same path of .001 file)
 0.17     20090601  G.Tani      Added rCountSize for counting number of files and total size in a path (optionally recursive)
 0.18     20091026  G.Tani      Added checksingle
 0.19     20091211  G.Tani      Fixed reporting the number of directories in rList
 0.20     20100124  G.Tani      Added nicenumber function to display a numeric string with size suffixes
 0.21     20101105  G.Tani      Added nicetime to display time in ms, or seconds with one decimal
 0.22     20110115  G.Tani      Added ShellTreeViewSetTextPath to update a specified shelltreeview object selecting the specified path, if exists
                                Added ComboBoxSetPaths to update a combobox to display up to 8 parent paths of the directory set as item 0
                                Moved from pea_utils various file manager functions and procedures not depending on the crypto library framework
                                129 extensions supported
 0.23     20110413  G.Tani      Improved handling of bz2 extensions variants
                                132 extensions supported
 0.24     20110413  G.Tani      Added support to IPA (iPhone application archive file, .zip variant)
                                133 extensions supported
 0.25     20110618  G.Tani      Added support to ZIPX, 134 extensions supported
 0.26     20110825  G.Tani      Minor fix: detection of .squashfs extension
 0.27     20110913  G.Tani      Modified speed calculation to display minutes if needed
 0.28     20111208  G.Tani      Added support to EPUB file format, 135 extensions supported
 0.29     20120115  G.Tani      Added support to OXT file format, 136 extensions supported
 0.30     20120414  G.Tani      added support to:
                                legacy OOo 1.x file formats SX* and ST* files
                                R00 rar files
                                PCV MozBackup files
                                BSZ (BS Player), RMSKIN (Rainmeter), WSZ/WAL (Winamp), and WMZ (Windows Media Player) skin files
 0.30     20120604  G.Tani      added support to IMA, IMG, and IMZ disk images, and AIR Adobe installers files (Adobe Integrated Runtime)
                                getwinenvadv for improved system version/subversion detection
 0.31     20120717  G.Tani      added testpcomp and dirsizetc for a very quick test of possible compression potential of a given file
 0.32     20121014  G.Tani      added explicit support for Open Packaging Conventions file formats:
                                MS Windows 8 App Package .appx
                                MS Windows Azure C# Package .cspkg
                                Autodesk AutoCAD .dwfx
                                Family.Show .familyx
                                Field Device .fdix
                                NuGet Package .nupkg
                                Siemens PLM Software file format .jtx
                                Open XML Paper Specification .oxps
                                MS Semblio .semblio
                                SMPTE Media Package .smpk
                                MS Visual Studio 2010 Extensions .vsix
                                Microsoft XML Paper Specification .xps
 0.33     20130602  G.Tani      Code cleanup
                                added support to Open Packaging Conventions formats .cddx and .appv (Microsoft Application Virtualization)
                                added support to .mdf (Alcohol 120 image file)
                                added support to .crx Google Chrome extension
                                added support to .maff Mozilla archive format
 0.34     20130822  G.Tani      added support for .ipsw iOS devices firmware packages
                                175 file extensions supported
 0.35     20131029  G.Tani      added support for .msu (Microsoft update) and .mpp (Microsoft Project file)
                                177 file extensions supported

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

uses {$IFDEF MSWINDOWS}Windows,{$ENDIF} Classes, SysUtils, StrUtils,
  StdCtrls, ComCtrls, ShellCtrls, ansiutf8_utils, Process;

type
  TFoundList = array of utf8string;
  //dynamic array of utf8string, each time a new file is found new space is allocated (function rList)
  TFoundListBool = array of boolean;
  TFoundListAges = array of dword;
  TFoundListAttrib = array of dword;
  TFoundListSizes = array of qword;
  TFoundListArray64 = array of array [0..63] of byte;
  TFileOfByte = file of byte;
  TCopyDataProc = procedure(oldnode, newnode : TTreenode);

const
  SUCCESS = 0;
  INCOMPLETE_FUNCTION = 1;
  LIST_ERROR = 2;
  LIST_RECURSION_ERROR = 3;
  CALL_ERROR = 4;
  DWORD_DECODE_TO_ATTRIBUTES_ERROR = 5;
  STRING_DECODE_TO_ATTRIBUTES_ERROR = 6;
  OBJECT_NOT_ACCESSIBLE = 7;

//represent a time string with suffixes
function nicetime(s: utf8string): utf8string;

//represent a numeric string with size suffixes
function nicenumber(s: utf8string): utf8string;

//return the name of first object found in directory, check if dir contains 1 object or more (break if contains more than 1 file, nfiles=2)
function checksingle(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var oname: utf8string
  //name of the first object found (excluding . and ..)
  ): integer;

//optionally recursive function to count files
function rCount(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs:
  qword                                       //number of files and dirs found
  ): integer;

//optionally recursive function to count files and total size
function rCountSize(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize:
  qword                              //number of files and dirs found, total size
  ): integer;

{
if an input object is accessible, assign it as a file of byte
otherwise mark it as skipped at the given address in the status_files TFoundList
}
function check_in(var f_in: TFileOfByte;
  //file of byte to assign
  in_qualified_name: utf8string;
  //qualified name of the object to test
  var status_files: TFoundListBool;
  //TFoundListBool containing accessibility boolean value of the object to test
  addr: dword
  //address of the object to test in the TFoundList
  ): integer;

function srcfilesize(s: utf8string; var fsize: qword): integer;

function srcfilesize_multipart(s: utf8string;
  var fsize: qword): integer;

{
list files, dirs and size matching the given path, mask and file attributes;
optionally include main dir as first object;
optionally list object with or without recursion
}
function dirsize(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size: qword
  //total size (Byte)
  ): integer;

{
decode file attributes given attributes as a dword
}
function dword2decodedFileAttributes(d: dword;
  //input dword
  var fattrib: utf8string
  //string of file attributes abbreviations
  ): integer;

{
extract directory name and path were the directory is in (nesting level
unlimited by implementation, only memory limited, whidely exceeding needs)
}
function extractdirname(inpath: utf8string;
  //input path
  var dirpath: utf8string;
  //path were the directory is in
  var dirname: utf8string
  //name of the dir
  ): integer;

function ansiextractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;

{
expand an object, given it's name, to a list of objects (TFoundList).
If the object is a file, its name is returned as sole element of the list;
otherwise if the object is a directory it's name and, recursively, all the
content, will be added to the list
}
function expand(s: utf8string;
  //input object
  var exp_files: TFoundList;
  //expanded list of objects
  var exp_fsizes: TFoundListSizes;
  //expanded list of object sizes
  var exp_ftimes: TFoundListAges;
  //expanded list of object ages
  var exp_fattr: TFoundListAttrib;
  //expanded list of object attributes
  var exp_fattr_dec: TFoundList;
  //expanded list of object decoded attributes
  var nfound: qword
  //number of objects found
  ): integer;
//take the exit code from ListFile function that it calls

{
list files and optionally dirs in a path (optionally with recursion in subdirs),
list objects details to other separate lists
}
function listdetails(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec: TFoundList
  //list of file decoded attributes
  ): integer;

//copy treeview
procedure CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil);

//update the specified treeview or shelltreeview object pointing to the specified path, if exists
function TreeViewSetTextPath(atreeview: TTreeView;
  anode: TTreeNode; Path: utf8string): integer;
function ShellTreeViewSetTextPath(ashelltreeview: TShellTreeView;
  Path: utf8string): integer;

//set parent paths as entries in a combobox
procedure ComboBoxSetPaths(acombobox: TComboBox; apath:utf8string);

//check full name against reserved and illegal characters, and reserved filenames
function checkfiledirname(s: utf8string): integer;

//escape filenames in *x environments likely using Gnome or KDE as desktop environment (Linux, *BSD)
function escapefilenamelinuxlike(s: UTF8string; desk_env: byte): UTF8string;

//cross platform filename escaping
function escapefilename(s: UTF8string; desk_env: byte): UTF8string;

//open Explorer selecting specified file
procedure winexplorepath(s: utf8string);

//open files in *x environments likely using Gnome or KDE as desktop environment (Linux, *BSD)
function cp_open_linuxlike(s: utf8string; desk_env: byte): integer;

//open Gnome or KDE search interface
procedure cp_search_linuxlike(desk_env: byte);

//get desktop environment
procedure getdesk_env(var bytedesk: byte; var caption_build, delimiter: utf8string);

//get desktop path
procedure get_desktop_path(var s: utf8string);

//get home path (*x) or profile path (win)
procedure get_home_path(var s: utf8string);

//get a temporary work path writeable from current user
procedure get_usrtmp_path(var s: utf8string);

//cut extension from filename
procedure cutextension(var s: utf8string);

//check file name against reserved and illegal characters, and reserved filenames
function checkfilename(s: utf8string): integer;

//generic command string sanitization
function validatecl(var s: utf8string): integer;

//get comspec and Windows version
{$IFDEF MSWINDOWS}
procedure getwinenv(var wincomspec, winver: utf8string);
{$ENDIF}

//get comspec and Windows version (advanced)
{$IFDEF MSWINDOWS}
procedure getwinenvadv(var wincomspec, winver, majmin: utf8string);
{$ENDIF}

//assign a code to all supported filetypes
function testext(s: utf8string): integer;

//test what backend should handle a given file
function testinput(infile: utf8string; testdir: boolean): integer;

//quickly test potential compression ratio of a given file
function testpcomp(var s:utf8string):integer;

//dirsize function variant with quick compression test
function dirsizetc(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size, tcsize: qword
  //total size (Byte)
  ): integer;

implementation

function nicetime(s: utf8string): utf8string;
var
  ntime, ints, decs: qword;
  s1: utf8string;
begin
  try
    ntime := strtoqword(s);
  except
    Result := s;
    exit;
  end;
  if ntime < 1000 then
    s1 := s + ' ms'
  else
  if ntime < 60000 then
     begin
        ints := ntime div 1000;
        decs := ((ntime * 10) div 1000) - ints * 10;
       s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' s';
     end
  else
     begin
        ints := ntime div 1000 div 60;
        decs := (ntime div 1000) - (ints*60); //((ntime * 10) div 1000) - ints * 10;
        s1 := IntToStr(ints) + ' m ' + IntToStr(decs) + ' s';
     end;
  Result := s1;
end;

//represent a numeric string with size suffixes
function nicenumber(s: utf8string): utf8string;
var
  fs, ints, decs: qword;
  s1: utf8string;
begin
  try
    fs := strtoqword(s);
  except
    Result := s;
    exit;
  end;
  if fs < 1024 then
    s1 := s + ' B'
  else
  if fs < 1024 * 1024 then
  begin
    ints := fs div 1024;
    decs := ((fs * 10) div 1024) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' KB';
  end
  else
  if fs < 1024 * 1024 * 1024 then
  begin
    ints := fs div (1024 * 1024);
    decs := ((fs * 10) div (1024 * 1024)) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' MB';
  end
  else
  begin
    ints := fs div (1024 * 1024 * 1024);
    decs := ((fs * 10) div (1024 * 1024 * 1024)) - ints * 10;
    s1 := IntToStr(ints) + '.' + IntToStr(decs) + ' GB';
  end;
  Result := s1;
end;

//optionally recursive function used internally to list files (optionally including dirs) and object details
function rList(mode: string;
  //mode of operation, currently implemented only DETAILS: give detailed output (1+4 lists);
  dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec:
  TFoundList                                     //list of file decoded attributes
  ): integer;
var
  r: TSearchRec;
begin
  rList := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if uFindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      if upcase(mode) = 'DETAILS' then
        repeat
          if ((r.Name <> '.') and (r.Name <> '..')) then
          begin
            SetLength(flist, nfiles + 1);
            SetLength(fsizes, nfiles + 1);
            SetLength(ftimes, nfiles + 1);
            SetLength(fattr, nfiles + 1);
            SetLength(fattr_dec, nfiles + 1);
            flist[nfiles] := dir + envtoutf8(r.Name);
            if (r.Attr and faDirectory <> 0) then
            begin
              if flist[nfiles][length(flist[nfiles])] <> DirectorySeparator then
                flist[nfiles] := flist[nfiles] + DirectorySeparator;
              Inc(ndirs, 1);
            end;
            fsizes[nfiles] := r.Size;
            ftimes[nfiles] := r.Time;
            fattr[nfiles] := r.Attr;
            dword2decodedFileAttributes(r.Attr, fattr_dec[nfiles]);
            Inc(nfiles, 1);
          end;
        until FindNext(r) <> 0;
    except
      FindClose(r);
      rList := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if uFindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
          begin
            rList(mode, dir + envtoutf8(r.Name) +
              DirectorySeparator, mask, fattrib, recur, nfiles, ndirs, flist, fsizes, ftimes, fattr, fattr_dec);
            Dec(ndirs, 1);
          end;
        until FindNext(r) <> 0;
      except
        FindClose(r);
        rList := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rList = INCOMPLETE_FUNCTION then
    rList := SUCCESS;
end;

//return the name of first object found in directory, check if dir contains 1 object or more (break if contains more than 1 file, nfiles=2)
function checksingle(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var oname: utf8string
  //name of the first object found (excluding . and ..)
  ): integer;
var
  r: TSearchRec;
begin
  checksingle := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if uFindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          if nfiles = 1 then
            oname := r.Name;
          if nfiles > 1 then
            break;
        end;
      until FindNext(r) <> 0;
    except
      FindClose(r);
      checksingle := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if checksingle = INCOMPLETE_FUNCTION then
    checksingle := SUCCESS;
end;

//optionally recursive fucntion to count files
function rCount(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs:
  qword                                      //number of files and dirs found
  ): integer;
var
  r: TSearchRec;
begin
  rCount := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if uFindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
        end;
      until FindNext(r) <> 0;
    except
      FindClose(r);
      rCount := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if uFindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
            rCount(dir + envtoutf8(r.Name) + DirectorySeparator, mask, fattrib, recur, nfiles, ndirs);
        until FindNext(r) <> 0;
      except
        FindClose(r);
        rCount := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rCount = INCOMPLETE_FUNCTION then
    rCount := SUCCESS;
end;

//optionally recursive function to count files and total size
function rCountSize(dir, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //true: recursive (search in all subfolders); false not recursive (search only in main folder)
  var nfiles, ndirs, tsize:
  qword                                 //number of files and dirs found, total size
  ): integer;
var
  r: TSearchRec;
begin
  rCountSize := INCOMPLETE_FUNCTION;
  Inc(ndirs, 1);
  if uFindFirst(dir + mask, fattrib, r) = 0 then
  begin
    try
      repeat
        if ((r.Name <> '.') and (r.Name <> '..')) then
        begin
          Inc(nfiles, 1);
          tsize := tsize + r.Size;
        end;
      until FindNext(r) <> 0;
    except
      FindClose(r);
      rCountSize := LIST_ERROR;
      exit;
    end;
    FindClose(r);
  end;
  if recur = True then
    if uFindFirst(dir + '*', fattrib or faDirectory, r) = 0 then
    begin
      try
        repeat
          if ((r.Attr and faDirectory) <> 0) and (r.Name <> '.') and
            (r.Name <> '..') then
            rCountSize(dir + envtoutf8(r.Name) + DirectorySeparator, mask, fattrib,
              recur, nfiles, ndirs, tsize);
        until FindNext(r) <> 0;
      except
        FindClose(r);
        rCountSize := LIST_RECURSION_ERROR;
        exit;
      end;
      FindClose(r);
    end;
  if rCountSize = INCOMPLETE_FUNCTION then
    rCountSize := SUCCESS;
end;

function check_in(var f_in: TFileOfByte;
  //file of byte to assign
  in_qualified_name: utf8string;
  //qualified name of the object to test
  var status_files: TFoundListBool;
  //TFoundListBool containing accessibility boolean value of the object to test
  addr: dword
  //address of the object to test in the TFoundList
  ): integer;
var
  in_ok: boolean;
begin
  check_in := INCOMPLETE_FUNCTION;
  status_files[addr] := False;
  try
    if ufilegetattr(in_qualified_name) > 0 then
      if ufilegetattr(in_qualified_name) and faDirectory = 0 then
      begin
        uassignfile(f_in, in_qualified_name);
        filemode := 0;
        reset(f_in);
        in_ok := True;
      end
      else
        in_ok := True
    else
      in_ok := False;
  except
    in_ok := False;
  end;
  if in_ok = False then
  begin
    status_files[addr] := False;
    check_in := OBJECT_NOT_ACCESSIBLE;
  end
  else
  begin
    status_files[addr] := True;
    check_in := SUCCESS;
  end;
end;

function srcfilesize(s: utf8string; var fsize: qword): integer;
var
  r: TSearchRec;
begin
  srcfilesize := -1;
  if ufilegetattr(s) and faDirectory = 0 then //object is a file
  begin
    if uFindFirst(s, faAnyFile, r) = 0 then
    begin
      fsize := r.Size;
      srcfilesize := 0;
    end;
    FindClose(r);
  end;
end;

function srcfilesize_multipart(s: utf8string;
  var fsize: qword): integer;
var
  s_ext, s_name, s_path: utf8string;
  i, j: integer;
  r: TSearchRec;
begin
  srcfilesize_multipart := -1;
  fsize := 0;
  if ufilegetattr(s) and faDirectory = 0 then //object is a file
  begin
    s_ext := uextractfileext(s);
    s_name := uextractfilename(s);
    setlength(s_name, length(s_name) - length(s_ext));
    s_path := uextractfilepath(s);
    i := 1;
    if s_ext = '.001' then
      repeat
        j := uFindFirst(s_path + s_name + s_ext, faAnyFile, r);
        if j = 0 then
          fsize := fsize + r.Size;
        FindClose(r);
        i := i + 1;
        if i < 10 then
          s_ext := '.00' + IntToStr(i)
        else
        if i < 100 then
          s_ext := '.0' + IntToStr(i)
        else
          s_ext := '.' + IntToStr(i);
      until j <> 0
    else
    begin
      if uFindFirst(s, faAnyFile, r) = 0 then
      begin
        fsize := r.Size;
        srcfilesize_multipart := 0;
      end;
      FindClose(r);
    end;
  end;
end;

function dirsize(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size: qword
  //total size (Byte)
  ): integer;
var
  i: integer;
  dir: utf8string;
  flist: TFoundList;
  fsizes: TFoundListSizes;
  ftimes: TFoundListAges;
  fattr: TFoundListAttrib;
  fattr_dec: TFoundList;
begin
  DirSize := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  size := 0;
  try
    //check for valid path, else set executable path as path
    if length(path) = 0 then
      dir := uextractfilepath(ParamStr(0))
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir[length(dir)] <> DirectorySeparator then
      dir := dir + DirectorySeparator;
    DirSize := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs, flist,
      fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
    for i := 0 to length(fsizes) - 1 do
      size := size + fsizes[i];
  except
    DirSize := CALL_ERROR;
    exit;
  end;
  if DirSize = INCOMPLETE_FUNCTION then
    DirSize := SUCCESS;
end;

function dword2decodedFileAttributes(d: dword;
  //input dword
  var fattrib: utf8string
  //string of file attributes abbreviations
  ): integer;
begin
  dword2decodedFileAttributes := INCOMPLETE_FUNCTION;
  try
    fattrib := '';
    if (d and $01) <> 0 then
      fattrib := fattrib + 'R';
    if (d and $02) <> 0 then
      fattrib := fattrib + 'H';
    if (d and $04) <> 0 then
      fattrib := fattrib + 'S';
    if (d and $08) <> 0 then
      fattrib := fattrib + 'V';
    if (d and $10) <> 0 then
      fattrib := fattrib + 'D';
    if (d and $20) <> 0 then
      fattrib := fattrib + 'A';
  except
    dword2decodedFileAttributes := DWORD_DECODE_TO_ATTRIBUTES_ERROR;
  end;
  if dword2decodedFileAttributes = INCOMPLETE_FUNCTION then
    dword2decodedFileAttributes := SUCCESS;
end;

function extractdirname(inpath: utf8string;
  //input path
  var dirpath: utf8string;
  //path were the directory is in
  var dirname: utf8string
  //name of the dir
  ): integer;
var
  s: utf8string;
  i, j: integer;
  dirarr: array of utf8string;
begin
  extractdirname := INCOMPLETE_FUNCTION;
  s := uextractfilepath(inpath);
  i := 0;
  while length(s) > 0 do
  begin
    setlength(dirarr, i + 1);
    dirarr[i] := copy2symbdel(s, directoryseparator);
    if length(s) > 0 then
      if s[1] = directoryseparator then
        s := copy(s, 2, length(s) - 1);
    Inc(i, 1);
  end;
  dirname := dirarr[i - 1];
  dirpath := '';
  for j := 0 to i - 2 do
    dirpath := dirpath + dirarr[j] + directoryseparator;
  if dirpath = '' then
  begin
    dirpath := dirname;
    dirname := '';
  end;
  extractdirname := SUCCESS;
end;

function ansiextractdirname(inpath: ansistring;
  //input path
  var dirpath: ansistring;
  //path were the directory is in
  var dirname: ansistring
  //name of the dir
  ): integer;
var
  s: ansistring;
  i, j: integer;
  dirarr: array of ansistring;
begin
  ansiextractdirname := INCOMPLETE_FUNCTION;
  s := extractfilepath(inpath);
  i := 0;
  while length(s) > 0 do
  begin
    setlength(dirarr, i + 1);
    dirarr[i] := copy2symbdel(s, directoryseparator);
    if length(s) > 0 then
      if s[1] = directoryseparator then
        s := copy(s, 2, length(s) - 1);
    Inc(i, 1);
  end;
  dirname := dirarr[i - 1];
  dirpath := '';
  for j := 0 to i - 2 do
    dirpath := dirpath + dirarr[j] + directoryseparator;
  if dirpath = '' then
  begin
    dirpath := dirname;
    dirname := '';
  end;
  ansiextractdirname := SUCCESS;
end;

function expand(s: utf8string;
  //input object
  var exp_files: TFoundList;
  //expanded list of objects
  var exp_fsizes: TFoundListSizes;
  //expanded list of object sizes
  var exp_ftimes: TFoundListAges;
  //expanded list of object ages
  var exp_fattr: TFoundListAttrib;
  //expanded list of object attributes
  var exp_fattr_dec: TFoundList;
  //expanded list of object decoded attributes
  var nfound: qword
  //number of objects found
  ): integer;
  //take the exit code from ListFile function that it calls
var
  i, j: qword;
  r: TSearchRec;
begin
  nfound := 0;
  i := 0;
  j := 0;
  SetLength(exp_files, i + 1);
  SetLength(exp_fsizes, i + 1);
  SetLength(exp_ftimes, i + 1);
  SetLength(exp_fattr, i + 1);
  SetLength(exp_fattr_dec, i + 1);
  exp_files[0] := s;
  dword2decodedFileAttributes(ufilegetattr(s), exp_fattr_dec[0]);
  if ufilegetattr(s) and faDirectory = 0 then //object is a file
  begin
    if uFindFirst(s, faAnyFile, r) = 0 then
    begin
      exp_fsizes[0] := r.Size;
      exp_ftimes[0] := r.Time;
      exp_fattr[0] := r.Attr;
      dword2decodedFileAttributes(r.Attr, exp_fattr_dec[0]);
    end;
    FindClose(r);
  end
  else
  begin
    if exp_files[0][length(exp_files[0])] <> DirectorySeparator then
      exp_files[0] := exp_files[0] + DirectorySeparator;
    exp_fsizes[0] := 0;
    if uFindFirst(s + '.', faDirectory, r) = 0 then
    begin
      exp_fsizes[0] := r.Size;
      exp_ftimes[0] := r.Time;
      exp_fattr[0] := r.Attr;
      dword2decodedFileAttributes(r.Attr, exp_fattr_dec[0]);
    end;
    FindClose(r);
    expand := ListDetails(s, '*', faAnyFile,
      True, //recursive listing
      True, //add objects in list after the main one
      i, j, exp_files,
      exp_fsizes, exp_ftimes, exp_fattr, exp_fattr_dec);
  end;
  nfound := i;
end;

function listdetails(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var flist: TFoundList;
  //list of file names
  var fsizes: TFoundListSizes;
  //list of file sizes
  var ftimes: TFoundListAges;
  //list of file ages
  var fattr: TFoundListAttrib;
  //list of file attributes
  var fattr_dec: TFoundList
  //list of file decoded attributes
  ): integer;
var
  dir: utf8string;
begin
  ListDetails := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  try
    //check for valid path, else set executable path as path
    if length(path) = 0 then
      dir := uextractfilepath(ParamStr(0))
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir[length(dir)] <> DirectorySeparator then
      dir := dir + DirectorySeparator;
    ListDetails := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs,
      flist, fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
  except
    ListDetails := CALL_ERROR;
    exit;
  end;
  if ListDetails = INCOMPLETE_FUNCTION then
    ListDetails := SUCCESS;
end;

procedure DefaultCopyDataProc(oldnode, newnode : TTreenode);
begin
  newnode.Assign(oldnode);
  newnode.ImageIndex:=3;
  newnode.SelectedIndex:=3;
end;

procedure CopySubtree(sourcenode : TTreenode; target : TTreeview;
  targetnode : TTreenode; CopyProc : TCopyDataProc = nil);
var
  anchor : TTreenode;
  child : TTreenode;
begin { CopySubtree }

  if not Assigned(CopyProc) then
    CopyProc := @DefaultCopyDataProc;

  anchor := target.Items.AddChild(targetnode, sourcenode.Text);
  CopyProc(sourcenode, anchor);
  child := sourcenode.GetFirstChild;
  while Assigned(child) do
  begin
    CopySubtree(child, target, anchor, CopyProc);
    child := child.getNextSibling;
  end;
end;

function TreeViewSetTextPath(atreeview: TTreeView;
  anode: TTreeNode; Path: utf8string): integer;
var
  Str: TStringList;
  i, sc: integer;
begin
  Result := -1;
  if not uDirectoryExists(Path) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := Path;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  //{$IFDEF WINDOWS}
  //Str[0] := Str[0] + PathDelim; //needed in 0.9.28.2, not needed in 0.9.30
  //{$ENDIF}
  atreeview.BeginUpdate;
  for i := 0 to sc - 1 do
  begin
    while (ANode <> nil) and (ANode.Text <> Str[i]) do
      ANode := ANode.GetNextSibling;
    if Anode <> nil then
    begin
      Anode.Expanded := True;
      ANode.Selected := True;
      Anode := ANode.GetFirstChild;
    end
    else
    begin
      atreeview.EndUpdate;
      str.Free;
      Result := 1;
      Exit;
    end;
  end;
  atreeview.EndUpdate;
  str.Free;
  Result := 0;
end;

function ShellTreeViewSetTextPath(ashelltreeview: TShellTreeView;
  Path: utf8string): integer;
var
  Str: TStringList;
  ANode: TTreeNode;
  i, sc: integer;
begin
  Result := -1;
  if not uDirectoryExists(Path) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := Path;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  //{$IFDEF WINDOWS}
  //Str[0] := Str[0] + PathDelim; //needed in 0.9.28.2, not needed in 0.9.30
  //{$ENDIF}
  ashelltreeview.BeginUpdate;
  ANode := ashelltreeview.Items[0];
  for i := 0 to sc - 1 do
  begin
    while (ANode <> nil) and (ANode.Text <> Str[i]) do
      ANode := ANode.GetNextSibling;
    if Anode <> nil then
    begin
      Anode.Expanded := True;
      ANode.Selected := True;
      Anode := ANode.GetFirstChild;
    end
    else
    begin
      ashelltreeview.EndUpdate;
      str.Free;
      Result := 1;
      Exit;
    end;
  end;
  ashelltreeview.EndUpdate;
  str.Free;
  Result := 0;
end;

procedure ComboBoxSetPaths(acombobox: TCombobox; apath:utf8string);
var
  Str: TStringList;
  i, j, sc: integer;
  s: array [0..8] of utf8string;
begin
  if not uDirectoryExists(apath) then exit;
  Str := TStringList.Create;
  str.StrictDelimiter := True;
  Str.Delimiter := PathDelim;
  Str.DelimitedText := apath;
  s[0] := apath;
  for i := Str.Count - 1 downto 0 do
    if Str[i] = '' then
      Str.Delete(i);
  sc := Str.Count;
  for j := 8 downto 1 do
    if (sc - 1 - (8 - j)) >= 0 then
      for i := 0 to sc - 1 - (8 - j) do
        s[j] := s[j] + str[i] + PathDelim;
  acombobox.Clear;
  acombobox.Caption := s[0];
  for i := 8 downto 1 do
    if s[i] <> '' then
      acombobox.Items.Add(s[i]);
  str.Free;
end;

function checkfiledirname(s: utf8string): integer;
var
  sf: utf8string;
  i: integer;
begin
  checkfiledirname := -1;
  if s = '' then
    exit;
  //illegal characters, full name
  for i := 0 to 31 do
    if pos(char(i), s) <> 0 then
      exit;
  //reserved characters, full name
  if pos('*', s) <> 0 then
    exit;
  if pos('?', s) <> 0 then
    exit;
{$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then
    exit;
{$ELSE}
  if pos('''', s) <> 0 then
    exit;
{$ENDIF}
  if pos('<', s) <> 0 then
    exit;
  if pos('>', s) <> 0 then
    exit;
  if pos('|', s) <> 0 then
    exit;
  if pos('     ', s) <> 0 then
    exit;
  sf := uextractfilename(s);
  //reserved characters, filename only (others are checked for the full name)
  if pos('\', sf) <> 0 then
    exit;
  if pos('/', sf) <> 0 then
    exit;
  if pos(':', sf) <> 0 then
    exit;
  //reserved filenames (Windows)
{$IFDEF MSWINDOWS}
  cutextension(sf);
  sf := upcase(sf);
  if (sf = 'CON') or (sf = 'PRN') or (sf = 'AUX') or (sf = 'NUL') or
    (sf = 'COM1') or (sf = 'COM2') or (sf = 'COM3') or (sf = 'COM4') or
    (sf = 'COM5') or (sf = 'COM6') or (sf = 'COM7') or (sf = 'COM8') or
    (sf = 'COM9') or (sf = 'LPT1') or (sf = 'LPT2') or (sf = 'LPT3') or
    (sf = 'LPT4') or (sf = 'LPT5') or (sf = 'LPT6') or (sf = 'LPT7') or
    (sf = 'LPT8') or (sf = 'LPT9') then
    exit;
{$ENDIF}
  checkfiledirname := 0;
end;

function escapefilenamelinuxlike(s: UTF8string; desk_env: byte): UTF8string;
var
  varstr: UTF8string;
  i: integer;
begin
  varstr := s;
  // replace ' with ? (quick way to to preserve string delimitation in command line)
  i := 1;
  repeat
    i := pos('''', varstr);
    if i > 0 then
      varstr[pos('''', varstr)] := '?';
  until i = 0;
  // find and delete 'file://' (and any part before) if it is passed as part of filename (it happens sometimes in Gnome, i.e. using "open with" context menu entry)
  i := pos('file://', varstr);
  if i > 0 then
    varstr := copy(varstr, i + 7, length(varstr) - i - 6);
  //replace %20 with space (if inverse replacement was done by desktop environment, happens in Gnome passing input from desktop environment rather than from application's dialogs)
  if desk_env = 1 then //apply it strictly only on Gnome
    if ufilegetattr(s) <= 0 then
      repeat
        i := pos('%20', varstr);
        if i > 0 then
        begin
          Delete(varstr, i, 3);
          insert(' ', varstr, i);
        end;
      until i = 0;
  escapefilenamelinuxlike := varstr;
end;

{in Linux and BSDs if filename contains delimiter ' change the character in ?, and checking special cases for Gnome and KDE
in Windows delimiter is " and it's not a valid character in filenames, so this control returns the input string (which doesn't need to be variable)
on other systems filenames are not escaped
}
function escapefilename(s: UTF8string; desk_env: byte): UTF8string;
begin
{$IFDEF MSWINDOWS}
  escapefilename := s;
{$ENDIF}
{$IFDEF LINUX}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF FREEBSD}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
{$IFDEF NETBSD}
  escapefilename := escapefilenamelinuxlike(s, desk_env);
{$ENDIF}
end;

function cp_open_linuxlike(s: utf8string; desk_env: byte): integer;
var
  P: TProcess;
begin
  cp_open_linuxlike := -1;
  if s = '' then
    exit;
  if (desk_env = 0) or (desk_env = 10) or (desk_env = 20) then
    exit;
  P := TProcess.Create(nil);
  P.Options := [poNoConsole, poWaitOnExit];
  if desk_env = 1 then
    if s[1] = '''' then
      P.CommandLine := 'gnome-open ' + escapefilename(s, desk_env)
    else
      P.CommandLine := 'gnome-open ' + '''' + escapefilename(s, desk_env) + '''';
  if desk_env = 2 then
    if s[1] = '''' then
      P.CommandLine := 'kioclient exec ' + escapefilename(s, desk_env)//kioclient replaces kfmclient (no longer supported on KDE 4+)
    else
      P.CommandLine := 'kioclient exec ' + '''' + escapefilename(s, desk_env) + '''';
  P.Execute;
  cp_open_linuxlike := P.ExitStatus;
  P.Free;
end;

procedure winexplorepath(s: utf8string);
var
  P: TProcess;
  cl: utf8string;
begin
{$IFDEF MSWINDOWS}
  P := TProcess.Create(nil);
  if ufileexists(s) then
    if s[1] = '"' then
      cl := 'explorer /select,' + s
    else
      cl := 'explorer /select,"' + s + '"'
  else
  if s[1] = '"' then
    cl := 'explorer ' + uextractfilepath(s)
  else
    cl := 'explorer "' + uextractfilepath(s) + '"';
  cl := utf8toenv(cl);
  P.CommandLine := cl;
  P.Execute;
  P.Free;
{$ENDIF}
end;

procedure cp_search_linuxlike(desk_env: byte);
var
  P: TProcess;
begin
  if (desk_env = 0) or (desk_env = 10) or (desk_env = 20) then
    exit;
  P := TProcess.Create(nil);
  P.Options := [poNoConsole, poWaitOnExit];
  if desk_env = 1 then
    P.CommandLine := 'gnome-search-tool';
  if desk_env = 2 then
    P.CommandLine := 'kfind';
  P.Execute;
  P.Free;
end;

procedure getdesk_env(var bytedesk: byte; var caption_build, delimiter: utf8string);
//0 unknown, 1 Gnome, 2 KDE, 10 MS Windows, 20 Darwin, 30 OSX
begin
  caption_build := 'Unknown OS Build';
  delimiter := '''';
  //defaults, overwritten by specifical values if target system is recognized: unknown OS, ' as delimiter character (*x like)
{$IFDEF WIN32}
  caption_build := 'Windows Build';
  delimiter := '"';
{$ENDIF}
{$IFDEF WIN64}
  caption_build := 'Win64 Build';
  delimiter := '"';
{$ENDIF}
{$IFDEF LINUX}
  caption_build := 'Linux Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF FREEBSD}
  caption_build := 'FreeBSD Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF NETBSD}
  caption_build := 'NetBSD Build';
  delimiter := '''';
{$ENDIF}
{$IFDEF DARWIN}
  caption_build := 'OSX Build';
  delimiter := '''';
{$ENDIF}
  bytedesk := 0; //unrecognized desktop environment
{$IFDEF MSWINDOWS}
  bytedesk := 10;
{$ENDIF}
{$IFDEF DARWIN}
  bytedesk := 20;
{$ENDIF}
  if getenvironmentvariable('GNOME_DESKTOP_SESSION_ID') <> '' then
    bytedesk := 1; //if this Gnome specific env variable is set, probably the user is running Gnome
  if getenvironmentvariable('KDE_FULL_SESSION') <> '' then
    bytedesk := 2; //if this KDE specific env variable is set, probably the user is running KDE
  if getenvironmentvariable('DESKTOP_SESSION') = 'gnome' then
    bytedesk :=
      1 //if gnome or kde is explicitely declared in DESKTOP_SESSION env variable, override previously assumed result
  else
  if getenvironmentvariable('DESKTOP_SESSION') = 'kde' then
    bytedesk := 2;
  case bytedesk of
    1: caption_build := caption_build + ' (Gnome)';
    2: caption_build := caption_build + ' (KDE)';
  end;
end;

procedure get_desktop_path(var s: utf8string); //superseeded in Windows
begin
{$IFDEF MSWINDOWS}
  s := envtoutf8(GetEnvironmentVariable('USERPROFILE') + '\Desktop\');
{$ENDIF}
{$IFDEF LINUX}
  s := GetEnvironmentVariable('HOME') + '/Desktop/';
{$ENDIF}
{$IFDEF FREEBSD}
  s := GetEnvironmentVariable('HOME') + '/Desktop/';
{$ENDIF}
{$IFDEF NETBSD}
  s := GetEnvironmentVariable('HOME') + '/Desktop/';
{$ENDIF}
  if s = '' then
    s := (ugetcurrentdir);
  //generic, superseeded by system specific values or value provided to the procedure, if not empty
  if s[length(s)] <> directoryseparator then
    s := s + directoryseparator;
end;

procedure get_home_path(var s: utf8string); //superseeded in Windows
begin
{$IFDEF MSWINDOWS}
  s := envtoutf8(GetEnvironmentVariable('USERPROFILE'));
{$ENDIF}
{$IFDEF LINUX}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF FREEBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF NETBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
  if s = '' then
    s := (ugetcurrentdir);
  //generic, superseeded by system specific values or value provided to the procedure, if not empty
  if s[length(s)] <> directoryseparator then
    s := s + directoryseparator;
end;

procedure get_usrtmp_path(var s: utf8string);
//works fine in Windows even if username contains extended characters
begin
{$IFDEF MSWINDOWS}
  s := envtoutf8(GetEnvironmentVariable('TMP'));
{$ENDIF}
{$IFDEF LINUX}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF FREEBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
{$IFDEF NETBSD}
  s := GetEnvironmentVariable('HOME');
{$ENDIF}
  if s = '' then
    s := (ugetcurrentdir);
  //generic, superseeded by system specific values or value provided to the procedure, if not empty
  if s[length(s)] <> directoryseparator then
    s := s + directoryseparator;
end;

procedure cutextension(var s: utf8string);
begin
  setlength(s, length(s) - length(uextractfileext(s)));
end;

function checkfilename(s: utf8string): integer;
var
  s1: utf8string;
  i: integer;
begin
  checkfilename := -1;
  if s = '' then
    exit;
  //illegal characters (no problem for additional UTF8 bytes since have MSB set to 1 to avoid conflict with those control characters)
  for i := 0 to 31 do
  begin
    if pos(char(i), s) <> 0 then
      exit;
  end;
  //reserved characters
  if pos('\', s) <> 0 then
    exit;
  if pos('/', s) <> 0 then
    exit;
  if pos(':', s) <> 0 then
    exit;
  if pos('*', s) <> 0 then
    exit;
  if pos('?', s) <> 0 then
    exit;
{$IFDEF MSWINDOWS}
  if pos('"', s) <> 0 then
    exit;
{$ELSE}
  if pos('''', s) <> 0 then
    exit;
{$ENDIF}
  if pos('<', s) <> 0 then
    exit;
  if pos('>', s) <> 0 then
    exit;
  if pos('|', s) <> 0 then
    exit;
  if pos('     ', s) <> 0 then
    exit;
  //reserved filenames (Windows)
{$IFDEF MSWINDOWS}
  s1 := uextractfilename(s);
  cutextension(s1);
  s1 := upcase(s1);
  if (s1 = 'CON') or (s1 = 'PRN') or (s1 = 'AUX') or (s1 = 'NUL') or
    (s1 = 'COM1') or (s1 = 'COM2') or (s1 = 'COM3') or (s1 = 'COM4') or
    (s1 = 'COM5') or (s1 = 'COM6') or (s1 = 'COM7') or (s1 = 'COM8') or
    (s1 = 'COM9') or (s1 = 'LPT1') or (s1 = 'LPT2') or (s1 = 'LPT3') or
    (s1 = 'LPT4') or (s1 = 'LPT5') or (s1 = 'LPT6') or (s1 = 'LPT7') or
    (s1 = 'LPT8') or (s1 = 'LPT9') then
    exit;
{$ENDIF}
  checkfilename := 0;
end;

function validatecl(var s: utf8string): integer;
var
  i: integer;
begin
  validatecl := -1;
  if s = '' then
    exit;
  //illegal characters
  for i := 0 to 31 do
    if pos(char(i), s) <> 0 then
      exit;
{if pos('<',s)<>0 then exit;
if pos('>',s)<>0 then exit;}
  if pos('|', s) <> 0 then
    exit;
  if pos('     ', s) <> 0 then
    exit; //more than 4 consecutive spaces may be intentional attempt to hamper readability (as in 7-Zip)
  validatecl := 0;
end;

{$IFDEF MSWINDOWS}
procedure getwinenv(var wincomspec, winver: utf8string);
var
  osVerInfo: TOSVersionInfo;
begin
  wincomspec := uextractfilename(GetEnvironmentVariable('COMSPEC'));
  if (upcase(wincomspec)<> 'COMMAND.COM') and (upcase(wincomspec)<> 'CMD.EXE') then wincomspec:='CMD.EXE';
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    if osVerInfo.dwMajorVersion <= 4 then
      if upcase(wincomspec) = 'CMD.EXE' then
        winver := 'nt4'
      else
        winver := '9x';
    if osVerInfo.dwMajorVersion = 5 then
      winver := 'nt5';
    if osVerInfo.dwMajorVersion >= 6 then
      winver := 'nt6+';
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure getwinenvadv(var wincomspec, winver, majmin: utf8string);
var
  osVerInfo: TOSVersionInfo;
begin
  wincomspec := uextractfilename(GetEnvironmentVariable('COMSPEC'));
  if (upcase(wincomspec)<> 'COMMAND.COM') and (upcase(wincomspec)<> 'CMD.EXE') then wincomspec:='CMD.EXE';
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(osVerInfo) then
  begin
    majmin:=inttostr(osVerInfo.dwMajorVersion)+'.'+inttostr(osVerInfo.dwMinorVersion);
    if osVerInfo.dwMajorVersion <= 4 then
      if upcase(wincomspec) = 'CMD.EXE' then
        winver := 'nt4'
      else
        winver := '9x';
    if osVerInfo.dwMajorVersion = 5 then
      winver := 'nt5';
    if osVerInfo.dwMajorVersion >= 6 then
      winver := 'nt6+';
  end;
end;
{$ENDIF}

function testext(s: utf8string): integer;
var
  ext: utf8string;
begin
  testext := -1;
  ext := lowercase(uextractfileext(s));
  //file types supported thorugh 7z backend
  case ext of
    '.7z', '.cb7': testext := 0;
    '.bz', '.bz2', '.bzip2', '.bzip', '.tbz2', '.tbz', '.tbzip2', '.tbzip', '.tb2': testext := 1;
    '.gz', '.gzip', '.tgz', '.tpz' : testext := 2;
    '.tar', '.cbt': testext := 5;
    '.zip', '.cbz', '.z01', '.smzip': testext := 6;
    '.arj': testext := 7;
    '.cab', '.imf': testext := 8; //cab and derived formats
    '.chm', '.chi', '.chq', '.chw', '.hxs', '.hxi', '.hxr', '.hxq', '.hxw', '.lit': testext := 9;
    '.cpio': testext := 10;
    '.deb': testext := 11;
    '.lzh': testext := 12;
    '.rar', '.cbr', '.r00', '.r01': testext := 13;
    '.00':
       begin
       cutextension(s);
       if lowercase(uextractfileext(s)) = '.rar' then
       testext := 13;
       end;//.rar.00
    '.rpm': testext := 14;
    '.z', '.taz', '.tz': testext := 16;
    '.iso': testext := 19;
    '.jar', '.ear', '.sar', '.war', '.apk': testext := 20;//Java and Android package (.zip derived)
    '.lha': testext := 21;
    '.pet', '.pup': testext := 22;//PuppyLinux packages
    '.pak', '.pk3', '.pk4': testext := 23; //package format used in Quake3 (pk3) and Quake 4 and Doom3 (pk4), zip with checksum appended
    '.slp': testext := 24;//Stampede Linux packages
    '': if uextractfilename(s) = '[Content]' then testext := 25;//in case of [Content] filename as when extracting RPM or SLP files
    '.xpi': testext := 26;//Mozilla installer package
    '.wim': testext := 27;//Windows Vista image package
    '.u3p': testext := 28;//U3P portable application package
    '.lzma86', '.lzma': testext := 29;
    '.udf': testext := 30;
    '.xar': testext := 31;
    '.dmg': testext := 32;
    '.hfs': testext := 33;
    '.part1', '.split': testext := 34; //generic spanned archive, open with 7z binary
    '.swm':  testext := 35;
    '.zipx': testext := 36;//non-legacy WinZip archives (supported for reading as for 7z 9.22)
    '.kmz': testext := 37;
    '.xz', '.txz': testext := 38;
    '.vhd': testext := 39;
    '.mslz': testext := 40;
    '.apm': testext := 41; //Apple Partition Map disk images
    '.ipa', '.ipsw': testext := 42; //.ipa iPhone application archive file .ipsw iOS devices firmware packages (.zip variants)
    '.bsz': testext := 43;
    '.rmskin': testext := 44;
    '.pcv': testext := 45;
    '.wal', '.wsz': testext := 46;
    '.wmz': testext := 47;
    '.air': testext := 48;
    '.ima', '.img': testext := 49;
    '.imz': testext := 50;
    '.mdf': testext := 51; //Alchool 120 image file
    '.crx': testext := 52; //Chrome extension
    '.appx', '.appv', '.smpk', '.nupkg': testext := 53;//OPC files treated as archives
    //mbr and filesystems that can be browsed by 7z backend
    '.mbr': testext := 100;
    '.fat': testext := 101;
    '.ntfs': testext := 102;
    '.sfs': testext := 103;
    '.image': testext := 104;
    '.squashfs': testext := 105;
    //file types usually not handled as archives
    '.exe', '.dll', '.sys': testext := 500; //most executables can be opened
    '.msi', '.msp', '.msu': testext := 500;
    '.sxc', '.sxd', '.sxi', '.sxw', '.stc', '.std', '.sti', '.stw', '.sxg', '.sxm': testext := 501; //OOo 1.x legacy filetypes
    '.ods', '.ots', '.odm', '.oth', '.oxt', '.odb', '.odf', '.odg', '.otg', '.odp', '.otp', '.odt', '.ott': testext := 501; //OOo filetypes
    '.gnm': testext := 501; //Gnumeric spreadsheet
    '.doc', '.dot', '.xls', '.xlt', '.ppt', '.pps', '.pot': testext := 502; //non executable COMPOUND files
    '.docx', '.dotx', '.xlsx', '.xltx', '.pptx': testext := 502; //OPC MS Office 2007 compressed formats, treated as othes MS Office formats
    '.mpp': testext := 502; //misc MS formats
    //any other format to be handled primarily as non-archive:
    //misc
    '.swf', '.fla', '.flv',
    //EPUB ebook (.zip variant)
    '.epub',
    //Mozilla web archive
    '.maff',
    //other Open Packaging Conventions filetypes
    '.dwfx', '.familyx', '.fdix', '.semblio', '.vsix', '.cspkg', '.xps', '.oxps', '.jtx', '.cddx': testext := 503;
    //files supported through other backends
    '.quad': testext := 1001;
    '.balz': testext := 1002;
    '.zpaq': testext := 2000;
    '.paq8f': testext := 2001;
    '.paq8jd': testext := 2002;
    '.paq8l': testext := 2003;
    '.paq8o': testext := 2004;
    '.lpaq1': testext := 2501;
    '.lpaq5': testext := 2505;
    '.lpaq8': testext := 2508;
    '.ace', '.cba': testext := 3001;
    '.arc': testext := 4001;
    '.wrc': testext := 4002;
    '.001': testext := 10000;//generic spanned archive
    '.pea': testext := 10001;
  end;
end;

function testinput(infile: utf8string; testdir: boolean): integer;
var
  i: integer;
begin
  testinput := 0;//not supported filetype
  if testdir = True then
    if udirectoryexists((infile)) then
      testinput := 1000;
  i := testext(infile);
  if i >= 0 then
    testinput := 3;//7z archive
  //specific conditions which overwrites previous result
  case i of
    2000: testinput := 10; //zpaq
    2001, 2002, 2003, 2004: testinput := 5;//paq archive
    2501, 2505, 2508: testinput := 8;//lpaq
    1001, 1002: testinput := 6;//quad
    3001: testinput := 7;//ace
    4001, 4002: testinput := 9;//freearc
    10000: testinput := 4; //use 7z backend to handle split archives
    10001: testinput := 1; //Pea
  end;
end;

function testpcomp(var s:utf8string):integer;
var
   ext:utf8string;
begin
ext:=lowercase(uextractfileext(s));
case ext of
   '.lnk','.txt','.rtf','.wri','.ini','.log','.mid': begin result:=10; exit; end;
   '.htm','.html','.xml','.mht','.url','.css','.xhtml': begin result:=10; exit; end;
   '.bat','.pif','.scr','.vbs','.cmd','.reg': begin result:=10; exit; end;
   '.pas','.pp','.h','.c','.hh','.cpp','.cc','.cxx','.hxx','.cs','.java','.pl','.pm','.php','.py','.p','rb',
   '.js','.asp','.aspx','.vb','.manifest': begin result:=10; exit; end;
   '.xls','.xlt','.gnm','.csv': begin result:=30; exit; end;
   '.ani','.cur','.ico','.icl': begin result:=35; exit; end;
   '.eml': begin result:=40; exit; end;
   '.doc','.dot': begin result:=40; exit; end;
   '.dll','.sys','.so': begin result:=40; exit; end;
   '.bmp','.tga','.tif','.tiff': begin result:=40; exit; end;
   '.wav': begin result:=45; exit; end;
   '.exe': begin result:=50; exit; end;
   '.db','.dbf','.mdb','.nsf': begin result:=50; exit; end;
   '.pps','.ppt','.odp': begin result:=50; exit; end;
   '.gif': begin result:=70; exit; end;
   '.xlsx','.ods': begin result:=80; exit; end;
   '.docx','.odt': begin result:=80; exit; end;
   '.svg','.ps','.eps','.cdr','.ai','.psd','.psp': begin result:=80; exit; end;
   '.pdf': begin result:=90; exit; end;
   '.png': begin result:=90; exit; end;
   '.jpg','.jpe','.jpeg','.jif', '.jfif', '.jfi','.jpx','.jp2','.j2k': begin result:=90; exit; end;
   '.avi','.mpg','.mpeg','.xvid','.divx','.mp4','.mov','.3gp','.wmv','.swf','.flv','.fla': begin result:=99; exit; end;
   '.mp3','.wma','.aiff','.ogg': begin result:=99; exit; end;
  end;
case testext(s) of //most used special formats are intercepted before
   -1: result:=50; //unknown
   else
     begin
     result:=95;//archives, guess medium/high compression ratio unless differently specified after
     if (ext='.tar') or (ext='.iso') or (ext='.wim') then begin result:=55; exit; end;
     if (ext='.rar') or (ext='.zipx') or (ext='.7z') or (ext='.xz') or (ext='.arc') or (ext='.wrc') then begin result:=100; exit; end;
     end;
   end;
end;

function dirsizetc(path, mask: utf8string;
  //path and mask for search
  fattrib: qword;
  //file attributes to filter the search
  recur: boolean;
  //if true: recursive listing (search in all subfolders); if false: not recursive listing (search in main folder)
  addtofirstobj: boolean;
  //if true start to update list from second element (the first one is left available for list the main object, i.e. list folder name and then the content of the folder); if false start listing from first array element (0, list content of the folder)
  var nfiles, ndirs: qword;
  //number of files and dirs found
  var size, tcsize: qword
  //total size (Byte)
  ): integer;
var
  i,tpcomp: integer;
  dir: utf8string;
  flist: TFoundList;
  fsizes: TFoundListSizes;
  ftimes: TFoundListAges;
  fattr: TFoundListAttrib;
  fattr_dec: TFoundList;
begin
  dirsizetc := INCOMPLETE_FUNCTION;
  if addtofirstobj = True then
    nfiles := 1
  else
    nfiles := 0;
  ndirs := 0;
  size := 0;
  tcsize := 0;
  try
    //check for valid path, else set executable path as path
    if length(path) = 0 then
      dir := uextractfilepath(ParamStr(0))
    else
      dir := path;
    //check for directoryseparator at the end of the path (needed)
    if dir[length(dir)] <> DirectorySeparator then
      dir := dir + DirectorySeparator;
    dirsizetc := rList('DETAILS', dir, mask, fattrib, recur, nfiles, ndirs, flist,
      fsizes, ftimes, fattr, fattr_dec);
    if addtofirstobj = False then
      ndirs := ndirs - 1;
    for i := 0 to length(fsizes) - 1 do
      begin
      size := size + fsizes[i];
      tpcomp:=testpcomp(flist[i]);
      tcsize := tcsize+ (fsizes[i] * tpcomp);
      end;
  except
    dirsizetc := CALL_ERROR;
    exit;
  end;
  if dirsizetc = INCOMPLETE_FUNCTION then
    dirsizetc := SUCCESS;
end;

end.

