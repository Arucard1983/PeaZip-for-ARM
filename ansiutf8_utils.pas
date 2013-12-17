unit ansiutf8_utils;
{
 DESCRIPTION     :  Unit providing wrapper for ansi-based FPC functions, in order
                    to, when applicable, receive utf8input and give utf8output

 REQUIREMENTS    :  FPC

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  ---

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     20080730  G.Tani      Initial version
 0.18     20080818  G.Tani      ucreatedir
 0.19     20081118  G.Tani      removed ucreatedir (replaced with umkdir); added urmdir and urenamefilebyname
 0.20     20081201  G.Tani      convertwincp procedure for Windows to convert oem codepages (as in text received from Windows console) to UTF-8
 0.20     20090906  G.Tani      uforcedirectories
 0.21     20091116  G.Tani      uextractrelativepath
 0.22     20100130  G.Tani      ufilesetattr
 0.23     20100228  G.Tani      functions rewritten to be full UTF8: uextractfilepath, uextractfilename, uextractfileext, udodirseparators
 0.24     20110122  G.Tani      removed urenamefile
                                added uStringReplace
                                added functions to get paramstr in widestring form on Windows
 0.25     20111001  G.Tani      added udeletefile_simple that simply maps deletefile function
                                udeletefile modified in order to reset read only flag before attempting deletion

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
Windows,
{$ENDIF}
Classes, SysUtils;

type
   fobyte = file of byte;
   Twarglist = array of utf8string;

procedure write_header(var tf:text); //write utf-8 text file header
function read_header(var tf:text):boolean; //read utf-8 text file header

function uextractfilepath(s:utf8string):utf8string;
function uextractfilename(s:utf8string):utf8string;
function uextractfileext(s:utf8string):utf8string;
procedure udodirseparators (var s: utf8string);
function uexpandfilename(s:utf8string):utf8string;
function uextractrelativepath(s1,s2:utf8string):utf8string;

function ufilesetattr(s:utf8string;Attr:longint):longint;
function ufilegetattr(s:utf8string):longint;
function ufileage(s:utf8string):longint;

function ugetcurrentdir:utf8string;
function usetcurrentdir(s:utf8string):boolean;
procedure umkdir(s:utf8string);
procedure uforcedirectories(s:utf8string);
procedure urmdir(s:utf8string);

procedure uassignfile(var f:fobyte; const s:utf8string);
procedure uassigntext(var t:text; const s:utf8string);
function udeletefile_simple(s:utf8string):boolean;
function udeletefile(s:utf8string):boolean;
function urenamefilebyname(s1,s2:utf8string):boolean;

function ufileexists(s:utf8string):boolean;
function udirectoryexists(s:utf8string):boolean;

function uFindFirst(s:utf8string; attr:Word; var f:TSearchRec):longint;
{can be followed with plain FindNext and FindClose
TSearchRecord.Name string needs to be translated with ansitoutf8 on Windows}

function envtoutf8({$IFDEF MSWINDOWS}a:ansistring{$ELSE}s:utf8string{$ENDIF}):utf8string;
function utf8toenv(s:utf8string):{$IFDEF MSWINDOWS}ansistring;{$ELSE}utf8string;{$ENDIF}

{$IFDEF MSWINDOWS}
//get widestring parameters from widestring command line (Windows)
procedure parsewparams(var warglist:Twarglist; var countwarg:integer);
//get n-th parameter from command line as utf8string (from widestring command line in Windows)
function wparamstr(n:integer):utf8string;
//get parameter count from command line (from widestring command line in Windows)
function wparamcount:integer;
{$ENDIF}

function uStringReplace(const S, OldPattern, NewPattern: utf8string;  Flags: TReplaceFlags): utf8string; //ignores rfIgnoreCase

procedure convertwincp(var s:utf8string);

implementation

procedure write_header(var tf:text);
begin
write(tf,char($ef));
write(tf,char($bb));
write(tf,char($bf));
end;

function read_header(var tf:text):boolean;
var
   c:char;
begin
read_header:=false;
read(tf,c);
if c<>char($ef) then exit;
read(tf,c);
if c<>char($bb) then exit;
read(tf,c);
if c<>char($bf) then exit;
read_header:=true;
end;

function uextractfilepath(s:utf8string):utf8string;
//uextractfilepath:=ansitoutf8(extractfilepath(utf8toansi(s)));
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(s);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (i > 0) and not (s[i] in EndSep) do
    Dec(i);
  If I>0 then
    Result := Copy(s, 1, i)
  else
    Result:='';
end;

function uextractfilename(s:utf8string):utf8string;
//uextractfilename:=ansitoutf8(extractfilename(utf8toansi(s)));
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(s);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not (s[I] in EndSep) do
    Dec(I);
  Result := Copy(s, I + 1, MaxInt);
end;

function uextractfileext(s:utf8string):utf8string;
//uextractfileext:=ansitoutf8(extractfileext(utf8toansi(s)));
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(s);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not (s[I] in EndSep) do
    Dec(I);
  if (I > 0) and (s[I] = ExtensionSeparator) then
    Result := Copy(s, I, MaxInt)
  else
    Result := '';
end;

procedure udodirseparators (var s: utf8string);
var i : longint;
begin
  For i:=1 to Length(s) do
    If s[i] in AllowDirectorySeparators then
      s[i]:=DirectorySeparator;
end;

function uexpandfilename(s:utf8string):utf8string;
begin
uexpandfilename:=ansitoutf8(expandfilename(utf8toansi(s)));
end;

function uextractrelativepath(s1,s2:utf8string):utf8string;
begin
uextractrelativepath:=ansitoutf8(extractrelativepath(utf8toansi(s1),utf8toansi(s2)));
end;

function ufilesetattr(s:utf8string;Attr:longint):longint;
begin
ufilesetattr:=filesetattr(utf8toansi(s),Attr);
end;

function ufilegetattr(s:utf8string):longint;
begin
ufilegetattr:=filegetattr(utf8toansi(s));
end;

function ufileage(s:utf8string):longint;
begin
ufileage:=fileage(utf8toansi(s));
end;

function ugetcurrentdir:utf8string;
begin
ugetcurrentdir:=ansitoutf8(getcurrentdir);
end;

function usetcurrentdir(s:utf8string):boolean;
begin
usetcurrentdir:=setcurrentdir(utf8toansi(s));
end;

procedure umkdir(s:utf8string);
begin
mkdir(utf8toansi(s));
end;

procedure uforcedirectories(s:utf8string);
begin
forcedirectories(utf8toansi(s));
end;

procedure urmdir(s:utf8string);
begin
rmdir(utf8toansi(s));
end;

procedure uassignfile(var f:fobyte; const s:utf8string);
begin
assignfile(f,utf8toansi(s));
end;

procedure uassigntext(var t:text; const s:utf8string);
begin
assignfile(t,utf8toansi(s));
end;

function udeletefile_simple(s:utf8string):boolean;
begin
udeletefile_simple:=deletefile(utf8toansi(s));
end;

function udeletefile(s:utf8string):boolean;
var attr:integer;
begin
{$IFDEF MSWINDOWS}
attr:=ufilegetattr(s);
attr:=attr and (not faReadOnly);
attr:=attr and (not faSysFile);
ufilesetattr(s,attr);
{$ENDIF}
udeletefile:=deletefile(utf8toansi(s));
end;

function urenamefilebyname(s1,s2:utf8string):boolean;
begin
urenamefilebyname:=renamefile(utf8toansi(s1),utf8toansi(s2));
end;

function ufileexists(s:utf8string):boolean;
begin
ufileexists:=fileexists(utf8toansi(s));
end;

function udirectoryexists(s:utf8string):boolean;
begin
udirectoryexists:=directoryexists(utf8toansi(s));
end;

function uFindFirst(s:utf8string;attr:Word; var f:TSearchRec):longint;
begin
uFindFirst:=FindFirst(utf8toansi(s),attr,f);
end;

function envtoutf8({$IFDEF MSWINDOWS}a:ansistring{$ELSE}s:utf8string{$ENDIF}):utf8string;
begin
{$IFDEF MSWINDOWS}
envtoutf8:=ansitoutf8(a);
{$ELSE}
envtoutf8:=s;
{$ENDIF}
end;

function utf8toenv(s:utf8string):{$IFDEF MSWINDOWS}ansistring;{$ELSE}utf8string;{$ENDIF}
begin
{$IFDEF MSWINDOWS}
utf8toenv:=utf8toansi(s);
{$ELSE}
utf8toenv:=s;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
//get widestring parameters from command line (Windows)
procedure parsewparams(var warglist:Twarglist; var countwarg:integer);
var
  count,i,argstart:integer;
  w,warg:widestring;
  quote:Boolean;

begin
  count:=0;
  setlength(warglist,0);
  i:=1;
  w:=(WideString(GetCommandLineW));
  while (w[i]<>#0) do
   begin
     if (i=length(w)) then break;
     while w[i] in [#1..#32] do inc(i);
     if w[i]=#0 then break;
     quote:=False;
     argstart:=i;
     warg:='';
     for i:=i to length(w) do
      begin
        case ord(w[i]) of
          0: break;
          1..32 :
            begin
              if quote=true then
               warg:=warg+w[i]
              else
               break;
            end;
          34 : if i=argstart then quote:=true
               else
                  quote:=false;
          else warg:=warg+w[i];
        end;
      end;
     setlength(warglist,length(warglist)+1);
     warglist[count]:=utf8encode(warg);
     inc(count);
   end;
  countwarg:=count-1;
end;

//get n-th parameter from command line as utf8string (from widestring command line in Windows)
function wparamstr(n:integer):utf8string;
var
   warglist:Twarglist;
   countwarg:integer;
begin
result:=paramstr(n);
parsewparams(warglist,countwarg);
if n<=countwarg then
   result:=warglist[n];
end;

//get parameter count from command line (from widestring command line in Windows)
function wparamcount:integer;
var
   warglist:Twarglist;
   countwarg:integer;
begin
result:=paramcount;
parsewparams(warglist,countwarg);
result:=countwarg;
end;
{$ENDIF}

function uStringReplace(const S, OldPattern, NewPattern: utf8string;  Flags: TReplaceFlags): utf8string; //ignores rfIgnoreCase
var
  Srch,OldP,RemS: utf8string;
  P : Integer;
begin
  Srch:=S;
  OldP:=OldPattern;
  {if rfIgnoreCase in Flags then
    begin
    Srch:=AnsiUpperCase(Srch);
    OldP:=AnsiUpperCase(OldP);
    end;}
  RemS:=S;
  Result:='';
  while (Length(Srch)<>0) do
    begin
    P:=Pos(OldP, Srch);
    if P=0 then
      begin
      Result:=Result+RemS;
      Srch:='';
      end
    else
      begin
      Result:=Result+Copy(RemS,1,P-1)+NewPattern;
      P:=P+Length(OldP);
      RemS:=Copy(RemS,P,Length(RemS)-P+1);
      if not (rfReplaceAll in Flags) then
        begin
        Result:=Result+RemS;
        Srch:='';
        end
      else
         Srch:=Copy(Srch,P,Length(Srch)-P+1);
      end;
    end;
end;

procedure convertwincp(var s:utf8string);
{translates Windows OEM codepages to UTF-8:
CP437 (Microsoft's variant)
CP720 Arabic
CP737 Greek
CP775 Baltic
CP850 Multilingual Latin I
CP852 Latin II
CP855 Cyrillic
CP857 Turkish
CP858 Multilingual Latin I + Euro
CP860 Portuguese
CP861 Icelandic
CP862 Hebrew
CP863 French-Canadian
CP865 Danish/Norwegian
CP866 Russian
CP869 Greek (alternative)
CP874 Thai
CP1258 Vietnam
CP65001 UTF-8, let s unaltered
in case of unsupported codepage, replaces character over 126 with jolly "?" character}
var
 i,j:integer;
 s1:utf8string;
begin
{$IFDEF MSWINDOWS}
j:=GetOEMCP;
s1:='';
case j of
437: //(Microsoft's variant)
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'ì';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'ò';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'ÿ';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'¢';
            156: s1:=s1+'£';
            157: s1:=s1+'¥';
            158: s1:=s1+'₧';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'⌐';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß'; //β
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ'; //∅
            238: s1:=s1+'ε'; //∈
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
720: //Arabic
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            //128: s1:=s1+'';
            //129: s1:=s1+'';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            //132: s1:=s1+'';
            133: s1:=s1+'à';
            //134: s1:=s1+'';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            //141: s1:=s1+'';
            //142: s1:=s1+'';
            //143: s1:=s1+'';
            //144: s1:=s1+'';
            145: s1:=s1+'ّ';
            146: s1:=s1+'ْ';
            147: s1:=s1+'ô';
            148: s1:=s1+'¤';
            149: s1:=s1+'ـ';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'ء';
            153: s1:=s1+'آ';
            154: s1:=s1+'أ';
            155: s1:=s1+'ؤ';
            156: s1:=s1+'£';
            157: s1:=s1+'إ';
            158: s1:=s1+'ئ';
            159: s1:=s1+'ا';
            160: s1:=s1+'ب';
            161: s1:=s1+'ة';
            162: s1:=s1+'ت';
            163: s1:=s1+'ث';
            164: s1:=s1+'ج';
            165: s1:=s1+'ح';
            166: s1:=s1+'خ';
            167: s1:=s1+'د';
            168: s1:=s1+'ذ';
            169: s1:=s1+'ر';
            170: s1:=s1+'ز';
            171: s1:=s1+'س';
            172: s1:=s1+'ش';
            173: s1:=s1+'ص';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'ض';
            225: s1:=s1+'ط';
            226: s1:=s1+'ظ';
            227: s1:=s1+'ع';
            228: s1:=s1+'غ';
            229: s1:=s1+'ف';
            230: s1:=s1+'µ';
            231: s1:=s1+'ق';
            232: s1:=s1+'ك';
            233: s1:=s1+'ل';
            234: s1:=s1+'م';
            235: s1:=s1+'ن';
            236: s1:=s1+'ه';
            237: s1:=s1+'و';
            238: s1:=s1+'ى';
            239: s1:=s1+'ي';
            240: s1:=s1+'≡';
            241: s1:=s1+'ً';
            242: s1:=s1+'ٌ';
            243: s1:=s1+'ٍ';
            244: s1:=s1+'َ';
            245: s1:=s1+'ُ';
            246: s1:=s1+'ِ';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
737: //Greek
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Α';
            129: s1:=s1+'Β';
            130: s1:=s1+'Γ';
            131: s1:=s1+'Δ';
            132: s1:=s1+'Ε';
            133: s1:=s1+'Ζ';
            134: s1:=s1+'Η';
            135: s1:=s1+'Θ';
            136: s1:=s1+'Ι';
            137: s1:=s1+'Κ';
            138: s1:=s1+'Λ';
            139: s1:=s1+'Μ';
            140: s1:=s1+'Ν';
            141: s1:=s1+'Ξ';
            142: s1:=s1+'Ο';
            143: s1:=s1+'Π';
            144: s1:=s1+'Ρ';
            145: s1:=s1+'Σ';
            146: s1:=s1+'Τ';
            147: s1:=s1+'Υ';
            148: s1:=s1+'Φ';
            149: s1:=s1+'Χ';
            150: s1:=s1+'Ψ';
            151: s1:=s1+'Ω';
            152: s1:=s1+'α';
            153: s1:=s1+'β';
            154: s1:=s1+'γ';
            155: s1:=s1+'δ';
            156: s1:=s1+'ε';
            157: s1:=s1+'ζ';
            158: s1:=s1+'η';
            159: s1:=s1+'θ';
            160: s1:=s1+'ι';
            161: s1:=s1+'κ';
            162: s1:=s1+'λ';
            163: s1:=s1+'μ';
            164: s1:=s1+'ν';
            165: s1:=s1+'ξ';
            166: s1:=s1+'ο';
            167: s1:=s1+'π';
            168: s1:=s1+'ρ';
            169: s1:=s1+'σ';
            170: s1:=s1+'ς';
            171: s1:=s1+'τ';
            172: s1:=s1+'υ';
            173: s1:=s1+'φ';
            174: s1:=s1+'χ';
            175: s1:=s1+'ψ';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'ω';
            225: s1:=s1+'ά';
            226: s1:=s1+'έ';
            227: s1:=s1+'ή';
            228: s1:=s1+'ϊ';
            229: s1:=s1+'ί';
            230: s1:=s1+'ό';
            231: s1:=s1+'ύ';
            232: s1:=s1+'ϋ';
            233: s1:=s1+'ώ';
            234: s1:=s1+'Ά';
            235: s1:=s1+'Έ';
            236: s1:=s1+'Ή';
            237: s1:=s1+'Ί';
            238: s1:=s1+'Ό';
            239: s1:=s1+'Ύ';
            240: s1:=s1+'Ώ';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'Ϊ';
            245: s1:=s1+'Ϋ';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
775: //Baltic
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ć';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'ā';
            132: s1:=s1+'ä';
            133: s1:=s1+'ģ';
            134: s1:=s1+'å';
            135: s1:=s1+'ć';
            136: s1:=s1+'ł';
            137: s1:=s1+'ē';
            138: s1:=s1+'Ŗ';
            139: s1:=s1+'ŗ';
            140: s1:=s1+'ŗ';
            141: s1:=s1+'Ź';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ō';
            148: s1:=s1+'ö';
            149: s1:=s1+'Ģ';
            150: s1:=s1+'¢';
            151: s1:=s1+'Ś';
            152: s1:=s1+'ś';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'×';
            159: s1:=s1+'¤';
            160: s1:=s1+'Ā';
            161: s1:=s1+'Ī';
            162: s1:=s1+'ó';
            163: s1:=s1+'Ż';
            164: s1:=s1+'ż';
            165: s1:=s1+'ź';
            166: s1:=s1+'”';
            167: s1:=s1+'¦';
            168: s1:=s1+'©';
            169: s1:=s1+'®';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'Ł';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Ą';
            182: s1:=s1+'Č';
            183: s1:=s1+'Ę';
            184: s1:=s1+'Ė';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'Į';
            190: s1:=s1+'Š';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'Ų';
            199: s1:=s1+'Ū';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'Ž';
            208: s1:=s1+'ą';
            209: s1:=s1+'č';
            210: s1:=s1+'ę';
            211: s1:=s1+'ė';
            212: s1:=s1+'į';
            213: s1:=s1+'š';
            214: s1:=s1+'ų';
            215: s1:=s1+'ū';
            216: s1:=s1+'ž';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'Ó';
            225: s1:=s1+'ß';
            226: s1:=s1+'Ō';
            227: s1:=s1+'Ń';
            228: s1:=s1+'õ';
            229: s1:=s1+'Õ';
            230: s1:=s1+'µ';
            231: s1:=s1+'ń';
            232: s1:=s1+'Ķ';
            233: s1:=s1+'ķ';
            234: s1:=s1+'Ļ';
            235: s1:=s1+'ļ';
            236: s1:=s1+'ņ';
            237: s1:=s1+'Ē';
            238: s1:=s1+'Ņ';
            239: s1:=s1+'’';
            //240: s1:=s1+'';
            241: s1:=s1+'±';
            242: s1:=s1+'“';
            243: s1:=s1+'¾';
            244: s1:=s1+'¶';
            245: s1:=s1+'§';
            246: s1:=s1+'÷';
            247: s1:=s1+'„';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'¹';
            252: s1:=s1+'³';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
850: //Multilingual Latin I
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'ì';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'ò';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'ÿ';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'×';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'®';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Á';
            182: s1:=s1+'Â';
            183: s1:=s1+'À';
            184: s1:=s1+'©';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'¢';
            190: s1:=s1+'¥';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'ã';
            199: s1:=s1+'Ã';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'¤';
            208: s1:=s1+'ð';
            209: s1:=s1+'Ð';
            210: s1:=s1+'Ê';
            211: s1:=s1+'Ë';
            212: s1:=s1+'È';
            213: s1:=s1+'ı';
            214: s1:=s1+'Í';
            215: s1:=s1+'Î';
            216: s1:=s1+'Ï';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'¦';
            222: s1:=s1+'Ì';
            223: s1:=s1+'▀';
            224: s1:=s1+'Ó';
            225: s1:=s1+'ß';
            226: s1:=s1+'Ô';
            227: s1:=s1+'Ò';
            228: s1:=s1+'õ';
            229: s1:=s1+'Õ';
            230: s1:=s1+'µ';
            231: s1:=s1+'þ';
            232: s1:=s1+'Þ';
            233: s1:=s1+'Ú';
            234: s1:=s1+'Û';
            235: s1:=s1+'Ù';
            236: s1:=s1+'ý';
            237: s1:=s1+'Ý';
            238: s1:=s1+'¯';
            239: s1:=s1+'´';
            //240: s1:=s1+'';
            241: s1:=s1+'±';
            242: s1:=s1+'‗';
            243: s1:=s1+'¾';
            244: s1:=s1+'¶';
            245: s1:=s1+'§';
            246: s1:=s1+'÷';
            247: s1:=s1+'¸';
            248: s1:=s1+'°';
            249: s1:=s1+'¨';
            250: s1:=s1+'·';
            251: s1:=s1+'¹';
            252: s1:=s1+'³';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
852: //Latin II
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'ů';
            134: s1:=s1+'ć';
            135: s1:=s1+'ç';
            136: s1:=s1+'ł';
            137: s1:=s1+'ë';
            138: s1:=s1+'Ő';
            139: s1:=s1+'ő';
            140: s1:=s1+'î';
            141: s1:=s1+'Ź';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Ć';
            144: s1:=s1+'É';
            145: s1:=s1+'Ĺ';
            146: s1:=s1+'ĺ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'Ľ';
            150: s1:=s1+'ľ';
            151: s1:=s1+'Ś';
            152: s1:=s1+'ś';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'Ť';
            156: s1:=s1+'ť';
            157: s1:=s1+'Ł';
            158: s1:=s1+'×';
            159: s1:=s1+'č';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'Ą';
            165: s1:=s1+'ą';
            166: s1:=s1+'Ž';
            167: s1:=s1+'ž';
            168: s1:=s1+'Ę';
            169: s1:=s1+'ę';
            170: s1:=s1+'¬';
            171: s1:=s1+'ź';
            172: s1:=s1+'Č';
            173: s1:=s1+'ş';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Á';
            182: s1:=s1+'Â';
            183: s1:=s1+'Ě';
            184: s1:=s1+'Ş';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'Ż';
            190: s1:=s1+'ż';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'Ă';
            199: s1:=s1+'ă';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'¤';
            208: s1:=s1+'đ';
            209: s1:=s1+'Ð';
            210: s1:=s1+'Ď';
            211: s1:=s1+'Ë';
            212: s1:=s1+'ď';
            213: s1:=s1+'Ň';
            214: s1:=s1+'Í';
            215: s1:=s1+'Î';
            216: s1:=s1+'ě';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'Ţ';
            222: s1:=s1+'Ů';
            223: s1:=s1+'▀';
            224: s1:=s1+'Ó';
            225: s1:=s1+'ß';
            226: s1:=s1+'Ô';
            227: s1:=s1+'Ń';
            228: s1:=s1+'ń';
            229: s1:=s1+'ň';
            230: s1:=s1+'Š';
            231: s1:=s1+'š';
            232: s1:=s1+'Ŕ';
            233: s1:=s1+'Ú';
            234: s1:=s1+'ŕ';
            235: s1:=s1+'Ű';
            236: s1:=s1+'ý';
            237: s1:=s1+'Ý';
            238: s1:=s1+'ţ';
            239: s1:=s1+'´';
            //240: s1:=s1+'';
            241: s1:=s1+'˝';
            242: s1:=s1+'˛';
            243: s1:=s1+'ˇ';
            244: s1:=s1+'˘';
            245: s1:=s1+'§';
            246: s1:=s1+'÷';
            247: s1:=s1+'¸';
            248: s1:=s1+'°';
            249: s1:=s1+'¨';
            250: s1:=s1+'·';
            251: s1:=s1+'ű';
            252: s1:=s1+'Ř';
            253: s1:=s1+'ř';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
855: //Cyrillic
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'ђ';
            129: s1:=s1+'Ђ';
            130: s1:=s1+'ѓ';
            131: s1:=s1+'Ѓ';
            132: s1:=s1+'ё';
            133: s1:=s1+'Ё';
            134: s1:=s1+'є';
            135: s1:=s1+'Є';
            136: s1:=s1+'ѕ';
            137: s1:=s1+'Ѕ';
            138: s1:=s1+'і';
            139: s1:=s1+'І';
            140: s1:=s1+'ї';
            141: s1:=s1+'Ї';
            142: s1:=s1+'ј';
            143: s1:=s1+'Ј';
            144: s1:=s1+'љ';
            145: s1:=s1+'Љ';
            146: s1:=s1+'њ';
            147: s1:=s1+'Њ';
            148: s1:=s1+'ћ';
            149: s1:=s1+'Ћ';
            150: s1:=s1+'ќ';
            151: s1:=s1+'Ќ';
            152: s1:=s1+'ў';
            153: s1:=s1+'Ў';
            154: s1:=s1+'џ';
            155: s1:=s1+'Џ';
            156: s1:=s1+'ю';
            157: s1:=s1+'Ю';
            158: s1:=s1+'ъ';
            159: s1:=s1+'Ъ';
            160: s1:=s1+'а';
            161: s1:=s1+'А';
            162: s1:=s1+'б';
            163: s1:=s1+'Б';
            164: s1:=s1+'ц';
            165: s1:=s1+'Ц';
            166: s1:=s1+'д';
            167: s1:=s1+'Д';
            168: s1:=s1+'е';
            169: s1:=s1+'Е';
            170: s1:=s1+'ф';
            171: s1:=s1+'Ф';
            172: s1:=s1+'г';
            173: s1:=s1+'Г';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'х';
            182: s1:=s1+'Х';
            183: s1:=s1+'и';
            184: s1:=s1+'И';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'й';
            190: s1:=s1+'Й';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'к';
            199: s1:=s1+'К';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'¤';
            208: s1:=s1+'л';
            209: s1:=s1+'Л';
            210: s1:=s1+'м';
            211: s1:=s1+'М';
            212: s1:=s1+'н';
            213: s1:=s1+'Н';
            214: s1:=s1+'о';
            215: s1:=s1+'О';
            216: s1:=s1+'п';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'П';
            222: s1:=s1+'я';
            223: s1:=s1+'▀';
            224: s1:=s1+'Я';
            225: s1:=s1+'р';
            226: s1:=s1+'Р';
            227: s1:=s1+'с';
            228: s1:=s1+'С';
            229: s1:=s1+'т';
            230: s1:=s1+'Т';
            231: s1:=s1+'у';
            232: s1:=s1+'У';
            233: s1:=s1+'ж';
            234: s1:=s1+'Ж';
            235: s1:=s1+'в';
            236: s1:=s1+'В';
            237: s1:=s1+'ь';
            238: s1:=s1+'Ь';
            239: s1:=s1+'№';
            //240: s1:=s1+'';
            241: s1:=s1+'ы';
            242: s1:=s1+'Ы';
            243: s1:=s1+'з';
            244: s1:=s1+'З';
            245: s1:=s1+'ш';
            246: s1:=s1+'Ш';
            247: s1:=s1+'э';
            248: s1:=s1+'Э';
            249: s1:=s1+'щ';
            250: s1:=s1+'Щ';
            251: s1:=s1+'ч';
            252: s1:=s1+'Ч';
            253: s1:=s1+'§';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
857: //Turkish
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'ı';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'ò';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'İ';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'Ş';
            159: s1:=s1+'ş';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'Ğ';
            167: s1:=s1+'ğ';
            168: s1:=s1+'¿';
            169: s1:=s1+'®';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Á';
            182: s1:=s1+'Â';
            183: s1:=s1+'À';
            184: s1:=s1+'©';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'¢';
            190: s1:=s1+'¥';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'ã';
            199: s1:=s1+'Ã';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'¤';
            208: s1:=s1+'º';
            209: s1:=s1+'ª';
            210: s1:=s1+'Ê';
            211: s1:=s1+'Ë';
            212: s1:=s1+'È';
            //213: s1:=s1+'';
            214: s1:=s1+'Í';
            215: s1:=s1+'Î';
            216: s1:=s1+'Ï';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'¦';
            222: s1:=s1+'Ì';
            223: s1:=s1+'▀';
            224: s1:=s1+'Ó';
            225: s1:=s1+'ß';
            226: s1:=s1+'Ô';
            227: s1:=s1+'Ò';
            228: s1:=s1+'õ';
            229: s1:=s1+'Õ';
            230: s1:=s1+'µ';
            //231: s1:=s1+'';
            232: s1:=s1+'×';
            233: s1:=s1+'Ú';
            234: s1:=s1+'Û';
            235: s1:=s1+'Ù';
            236: s1:=s1+'ì';
            237: s1:=s1+'ÿ';
            238: s1:=s1+'¯';
            239: s1:=s1+'´';
            240: s1:=s1+'­';
            241: s1:=s1+'±';
            242: s1:=s1+'';
            243: s1:=s1+'¾';
            244: s1:=s1+'¶';
            245: s1:=s1+'§';
            246: s1:=s1+'÷';
            247: s1:=s1+'¸';
            248: s1:=s1+'°';
            249: s1:=s1+'¨';
            250: s1:=s1+'·';
            251: s1:=s1+'¹';
            252: s1:=s1+'³';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
858: //Multilingual Latin I + Euro
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'ì';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'ò';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'ÿ';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'×';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'®';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Á';
            182: s1:=s1+'Â';
            183: s1:=s1+'À';
            184: s1:=s1+'©';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'¢';
            190: s1:=s1+'¥';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'ã';
            199: s1:=s1+'Ã';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'¤';
            208: s1:=s1+'ð';
            209: s1:=s1+'Ð';
            210: s1:=s1+'Ê';
            211: s1:=s1+'Ë';
            212: s1:=s1+'È';
            213: s1:=s1+'€';
            214: s1:=s1+'Í';
            215: s1:=s1+'Î';
            216: s1:=s1+'Ï';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'¦';
            222: s1:=s1+'Ì';
            223: s1:=s1+'▀';
            224: s1:=s1+'Ó';
            225: s1:=s1+'ß';
            226: s1:=s1+'Ô';
            227: s1:=s1+'Ò';
            228: s1:=s1+'õ';
            229: s1:=s1+'Õ';
            230: s1:=s1+'µ';
            231: s1:=s1+'þ';
            232: s1:=s1+'Þ';
            233: s1:=s1+'Ú';
            234: s1:=s1+'Û';
            235: s1:=s1+'Ù';
            236: s1:=s1+'ý';
            237: s1:=s1+'Ý';
            238: s1:=s1+'¯';
            239: s1:=s1+'´';
            //240: s1:=s1+'';
            241: s1:=s1+'±';
            242: s1:=s1+'‗';
            243: s1:=s1+'¾';
            244: s1:=s1+'¶';
            245: s1:=s1+'§';
            246: s1:=s1+'÷';
            247: s1:=s1+'¸';
            248: s1:=s1+'°';
            249: s1:=s1+'¨';
            250: s1:=s1+'·';
            251: s1:=s1+'¹';
            252: s1:=s1+'³';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
860: //Portuguese
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ã';
            133: s1:=s1+'à';
            134: s1:=s1+'Á';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'Ê';
            138: s1:=s1+'è';
            139: s1:=s1+'Í';
            140: s1:=s1+'Ô';
            141: s1:=s1+'ì';
            142: s1:=s1+'Ã';
            143: s1:=s1+'Â';
            144: s1:=s1+'É';
            145: s1:=s1+'À';
            146: s1:=s1+'È';
            147: s1:=s1+'ô';
            148: s1:=s1+'õ';
            149: s1:=s1+'ò';
            150: s1:=s1+'Ú';
            151: s1:=s1+'ù';
            152: s1:=s1+'Ì';
            153: s1:=s1+'Õ';
            154: s1:=s1+'Ü';
            155: s1:=s1+'¢';
            156: s1:=s1+'£';
            157: s1:=s1+'Ù';
            158: s1:=s1+'₧';
            159: s1:=s1+'Ó';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'Ò';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß';
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ';
            238: s1:=s1+'ε';
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
861: //Icelandic
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'Ð';
            140: s1:=s1+'ð';
            141: s1:=s1+'Þ';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'þ';
            150: s1:=s1+'û';
            151: s1:=s1+'Ý';
            152: s1:=s1+'ý';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'₧';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'Á';
            165: s1:=s1+'Í';
            166: s1:=s1+'Ó';
            167: s1:=s1+'Ú';
            168: s1:=s1+'¿';
            169: s1:=s1+'⌐';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß';
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ';
            238: s1:=s1+'ε';
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
862: //Hebrew
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'א';
            129: s1:=s1+'ב';
            130: s1:=s1+'ג';
            131: s1:=s1+'ד';
            132: s1:=s1+'ה';
            133: s1:=s1+'ו';
            134: s1:=s1+'ז';
            135: s1:=s1+'ח';
            136: s1:=s1+'ט';
            137: s1:=s1+'י';
            138: s1:=s1+'ך';
            139: s1:=s1+'כ';
            140: s1:=s1+'ל';
            141: s1:=s1+'ם';
            142: s1:=s1+'מ';
            143: s1:=s1+'ן';
            144: s1:=s1+'נ';
            145: s1:=s1+'ס';
            146: s1:=s1+'ע';
            147: s1:=s1+'ף';
            148: s1:=s1+'פ';
            149: s1:=s1+'ץ';
            150: s1:=s1+'צ';
            151: s1:=s1+'ק';
            152: s1:=s1+'ר';
            153: s1:=s1+'ש';
            154: s1:=s1+'ת';
            155: s1:=s1+'¢';
            156: s1:=s1+'£';
            157: s1:=s1+'¥';
            158: s1:=s1+'₧';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'⌐';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß'; //β
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ'; //∅
            238: s1:=s1+'ε'; //∈
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
863: //French-Canadian
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'Â';
            133: s1:=s1+'à';
            134: s1:=s1+'¶';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'‗';
            142: s1:=s1+'À';
            143: s1:=s1+'§';
            144: s1:=s1+'É';
            145: s1:=s1+'È';
            146: s1:=s1+'Ê';
            147: s1:=s1+'ô';
            148: s1:=s1+'Ë';
            149: s1:=s1+'Ï';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'¤';
            153: s1:=s1+'Ô';
            154: s1:=s1+'Ü';
            155: s1:=s1+'¢';
            156: s1:=s1+'£';
            157: s1:=s1+'Ù';
            158: s1:=s1+'Û';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'¦';
            161: s1:=s1+'´';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'¨';
            165: s1:=s1+'¸';
            166: s1:=s1+'³';
            167: s1:=s1+'¯';
            168: s1:=s1+'Î';
            169: s1:=s1+'⌐';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¾';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß'; //β
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ'; //∅
            238: s1:=s1+'ε'; //∈
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
865: //Danish/Norwegian
   begin
   for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'Ç';
            129: s1:=s1+'ü';
            130: s1:=s1+'é';
            131: s1:=s1+'â';
            132: s1:=s1+'ä';
            133: s1:=s1+'à';
            134: s1:=s1+'å';
            135: s1:=s1+'ç';
            136: s1:=s1+'ê';
            137: s1:=s1+'ë';
            138: s1:=s1+'è';
            139: s1:=s1+'ï';
            140: s1:=s1+'î';
            141: s1:=s1+'ì';
            142: s1:=s1+'Ä';
            143: s1:=s1+'Å';
            144: s1:=s1+'É';
            145: s1:=s1+'æ';
            146: s1:=s1+'Æ';
            147: s1:=s1+'ô';
            148: s1:=s1+'ö';
            149: s1:=s1+'ò';
            150: s1:=s1+'û';
            151: s1:=s1+'ù';
            152: s1:=s1+'ÿ';
            153: s1:=s1+'Ö';
            154: s1:=s1+'Ü';
            155: s1:=s1+'ø';
            156: s1:=s1+'£';
            157: s1:=s1+'Ø';
            158: s1:=s1+'₧';
            159: s1:=s1+'ƒ';
            160: s1:=s1+'á';
            161: s1:=s1+'í';
            162: s1:=s1+'ó';
            163: s1:=s1+'ú';
            164: s1:=s1+'ñ';
            165: s1:=s1+'Ñ';
            166: s1:=s1+'ª';
            167: s1:=s1+'º';
            168: s1:=s1+'¿';
            169: s1:=s1+'⌐';
            170: s1:=s1+'¬';
            171: s1:=s1+'½';
            172: s1:=s1+'¼';
            173: s1:=s1+'¡';
            174: s1:=s1+'«';
            175: s1:=s1+'¤';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'α';
            225: s1:=s1+'ß'; //β
            226: s1:=s1+'Γ';
            227: s1:=s1+'π';
            228: s1:=s1+'Σ';
            229: s1:=s1+'σ';
            230: s1:=s1+'µ';
            231: s1:=s1+'τ';
            232: s1:=s1+'Φ';
            233: s1:=s1+'Θ';
            234: s1:=s1+'Ω';
            235: s1:=s1+'δ';
            236: s1:=s1+'∞';
            237: s1:=s1+'φ'; //∅
            238: s1:=s1+'ε'; //∈
            239: s1:=s1+'∩';
            240: s1:=s1+'≡';
            241: s1:=s1+'±';
            242: s1:=s1+'≥';
            243: s1:=s1+'≤';
            244: s1:=s1+'⌠';
            245: s1:=s1+'⌡';
            246: s1:=s1+'÷';
            247: s1:=s1+'≈';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'ⁿ';
            253: s1:=s1+'²';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
866: //Russian
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'А';
            129: s1:=s1+'Б';
            130: s1:=s1+'В';
            131: s1:=s1+'Г';
            132: s1:=s1+'Д';
            133: s1:=s1+'Е';
            134: s1:=s1+'Ж';
            135: s1:=s1+'З';
            136: s1:=s1+'И';
            137: s1:=s1+'Й';
            138: s1:=s1+'К';
            139: s1:=s1+'Л';
            140: s1:=s1+'М';
            141: s1:=s1+'Н';
            142: s1:=s1+'О';
            143: s1:=s1+'П';
            144: s1:=s1+'Р';
            145: s1:=s1+'С';
            146: s1:=s1+'Т';
            147: s1:=s1+'У';
            148: s1:=s1+'Ф';
            149: s1:=s1+'Х';
            150: s1:=s1+'Ц';
            151: s1:=s1+'Ч';
            152: s1:=s1+'Ш';
            153: s1:=s1+'Щ';
            154: s1:=s1+'Ъ';
            155: s1:=s1+'Ы';
            156: s1:=s1+'Ь';
            157: s1:=s1+'Э';
            158: s1:=s1+'Ю';
            159: s1:=s1+'Я';
            160: s1:=s1+'а';
            161: s1:=s1+'б';
            162: s1:=s1+'в';
            163: s1:=s1+'г';
            164: s1:=s1+'д';
            165: s1:=s1+'е';
            166: s1:=s1+'ж';
            167: s1:=s1+'з';
            168: s1:=s1+'и';
            169: s1:=s1+'й';
            170: s1:=s1+'к';
            171: s1:=s1+'л';
            172: s1:=s1+'м';
            173: s1:=s1+'н';
            174: s1:=s1+'о';
            175: s1:=s1+'п';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'╡';
            182: s1:=s1+'╢';
            183: s1:=s1+'╖';
            184: s1:=s1+'╕';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'╜';
            190: s1:=s1+'╛';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'╞';
            199: s1:=s1+'╟';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'╧';
            208: s1:=s1+'╨';
            209: s1:=s1+'╤';
            210: s1:=s1+'╥';
            211: s1:=s1+'╙';
            212: s1:=s1+'╘';
            213: s1:=s1+'╒';
            214: s1:=s1+'╓';
            215: s1:=s1+'╫';
            216: s1:=s1+'╪';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'▌';
            222: s1:=s1+'▐';
            223: s1:=s1+'▀';
            224: s1:=s1+'р';
            225: s1:=s1+'с';
            226: s1:=s1+'т';
            227: s1:=s1+'у';
            228: s1:=s1+'ф';
            229: s1:=s1+'х';
            230: s1:=s1+'ц';
            231: s1:=s1+'ч';
            232: s1:=s1+'ш';
            233: s1:=s1+'щ';
            234: s1:=s1+'ъ';
            235: s1:=s1+'ы';
            236: s1:=s1+'ь';
            237: s1:=s1+'э';
            238: s1:=s1+'ю';
            239: s1:=s1+'я';
            240: s1:=s1+'Ё';
            241: s1:=s1+'ё';
            242: s1:=s1+'Є';
            243: s1:=s1+'є';
            244: s1:=s1+'Ї';
            245: s1:=s1+'ї';
            246: s1:=s1+'Ў';
            247: s1:=s1+'ў';
            248: s1:=s1+'°';
            249: s1:=s1+'∙';
            250: s1:=s1+'·';
            251: s1:=s1+'√';
            252: s1:=s1+'№';
            253: s1:=s1+'¤';
            254: s1:=s1+'■';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
869: //Greek (alternative)
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            //128: s1:=s1+'';
            //129: s1:=s1+'';
            //130: s1:=s1+'';
            //131: s1:=s1+'';
            //132: s1:=s1+'';
            //133: s1:=s1+'';
            134: s1:=s1+'Ά';
            //135: s1:=s1+'';
            136: s1:=s1+'·';
            137: s1:=s1+'¬';
            138: s1:=s1+'¦';
            139: s1:=s1+'‘';
            140: s1:=s1+'’';
            141: s1:=s1+'Έ';
            142: s1:=s1+'―';
            143: s1:=s1+'Ή';
            144: s1:=s1+'Ί';
            145: s1:=s1+'Ϊ';
            146: s1:=s1+'Ό';
            //147: s1:=s1+'';
            //148: s1:=s1+'';
            149: s1:=s1+'Ύ';
            150: s1:=s1+'Ϋ';
            151: s1:=s1+'©';
            152: s1:=s1+'Ώ';
            153: s1:=s1+'²';
            154: s1:=s1+'³';
            155: s1:=s1+'ά';
            156: s1:=s1+'£';
            157: s1:=s1+'έ';
            158: s1:=s1+'ή';
            159: s1:=s1+'ί';
            160: s1:=s1+'ϊ';
            161: s1:=s1+'ΐ';
            162: s1:=s1+'ό';
            163: s1:=s1+'ύ';
            164: s1:=s1+'Α';
            165: s1:=s1+'Β';
            166: s1:=s1+'Γ';
            167: s1:=s1+'Δ';
            168: s1:=s1+'Ε';
            169: s1:=s1+'Ζ';
            170: s1:=s1+'Η';
            171: s1:=s1+'½';
            172: s1:=s1+'Θ';
            173: s1:=s1+'Ι';
            174: s1:=s1+'«';
            175: s1:=s1+'»';
            176: s1:=s1+'░';
            177: s1:=s1+'▒';
            178: s1:=s1+'▓';
            179: s1:=s1+'│';
            180: s1:=s1+'┤';
            181: s1:=s1+'Κ';
            182: s1:=s1+'Λ';
            183: s1:=s1+'Μ';
            184: s1:=s1+'Ν';
            185: s1:=s1+'╣';
            186: s1:=s1+'║';
            187: s1:=s1+'╗';
            188: s1:=s1+'╝';
            189: s1:=s1+'Ξ';
            190: s1:=s1+'Ο';
            191: s1:=s1+'┐';
            192: s1:=s1+'└';
            193: s1:=s1+'┴';
            194: s1:=s1+'┬';
            195: s1:=s1+'├';
            196: s1:=s1+'─';
            197: s1:=s1+'┼';
            198: s1:=s1+'Π';
            199: s1:=s1+'Ρ';
            200: s1:=s1+'╚';
            201: s1:=s1+'╔';
            202: s1:=s1+'╩';
            203: s1:=s1+'╦';
            204: s1:=s1+'╠';
            205: s1:=s1+'═';
            206: s1:=s1+'╬';
            207: s1:=s1+'Σ';
            208: s1:=s1+'Τ';
            209: s1:=s1+'Υ';
            210: s1:=s1+'Φ';
            211: s1:=s1+'Χ';
            212: s1:=s1+'Ψ';
            213: s1:=s1+'Ω';
            214: s1:=s1+'α';
            215: s1:=s1+'β';
            216: s1:=s1+'γ';
            217: s1:=s1+'┘';
            218: s1:=s1+'┌';
            219: s1:=s1+'█';
            220: s1:=s1+'▄';
            221: s1:=s1+'δ';
            222: s1:=s1+'ε';
            223: s1:=s1+'▀';
            224: s1:=s1+'ζ';
            225: s1:=s1+'η';
            226: s1:=s1+'θ';
            227: s1:=s1+'ι';
            228: s1:=s1+'κ';
            229: s1:=s1+'λ';
            230: s1:=s1+'μ';
            231: s1:=s1+'ν';
            232: s1:=s1+'ξ';
            233: s1:=s1+'ο';
            234: s1:=s1+'π';
            235: s1:=s1+'ρ';
            236: s1:=s1+'σ';
            237: s1:=s1+'ς';
            238: s1:=s1+'τ';
            239: s1:=s1+'΄';
            240: s1:=s1+'­';
            241: s1:=s1+'±';
            242: s1:=s1+'υ';
            243: s1:=s1+'φ';
            244: s1:=s1+'χ';
            245: s1:=s1+'§';
            246: s1:=s1+'ψ';
            247: s1:=s1+'΅';
            248: s1:=s1+'°';
            249: s1:=s1+'¨';
            250: s1:=s1+'ω';
            251: s1:=s1+'ϋ';
            252: s1:=s1+'ΰ';
            253: s1:=s1+'ώ';
            254: s1:=s1+'■';
            //255: s1:=s1+'';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
874: //Thai
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'€';
            //129: s1:=s1+'';
            //130: s1:=s1+'';
            //131: s1:=s1+'';
            //132: s1:=s1+'';
            133: s1:=s1+'…';
            //134: s1:=s1+'';
            //135: s1:=s1+'';
            //136: s1:=s1+'';
            //137: s1:=s1+'';
            //138: s1:=s1+'';
            //139: s1:=s1+'';
            //140: s1:=s1+'';
            //141: s1:=s1+'';
            //142: s1:=s1+'';
            //143: s1:=s1+'';
            //144: s1:=s1+'';
            145: s1:=s1+'‘';
            146: s1:=s1+'’';
            147: s1:=s1+'“';
            148: s1:=s1+'”';
            149: s1:=s1+'•';
            150: s1:=s1+'–';
            151: s1:=s1+'—';
            //152: s1:=s1+'';
            //153: s1:=s1+'';
            //154: s1:=s1+'';
            //155: s1:=s1+'';
            //156: s1:=s1+'';
            //157: s1:=s1+'';
            //158: s1:=s1+'';
            //159: s1:=s1+'';
            //160: s1:=s1+'';
            161: s1:=s1+'ก';
            162: s1:=s1+'ข';
            163: s1:=s1+'ฃ';
            164: s1:=s1+'ค';
            165: s1:=s1+'ฅ';
            166: s1:=s1+'ฆ';
            167: s1:=s1+'ง';
            168: s1:=s1+'จ';
            169: s1:=s1+'ฉ';
            170: s1:=s1+'ช';
            171: s1:=s1+'ซ';
            172: s1:=s1+'ฌ';
            173: s1:=s1+'ญ';
            174: s1:=s1+'ฎ';
            175: s1:=s1+'ฏ';
            176: s1:=s1+'ฐ';
            177: s1:=s1+'ฑ';
            178: s1:=s1+'ฒ';
            179: s1:=s1+'ณ';
            180: s1:=s1+'ด';
            181: s1:=s1+'ต';
            182: s1:=s1+'ถ';
            183: s1:=s1+'ท';
            184: s1:=s1+'ธ';
            185: s1:=s1+'น';
            186: s1:=s1+'บ';
            187: s1:=s1+'ป';
            188: s1:=s1+'ผ';
            189: s1:=s1+'ฝ';
            190: s1:=s1+'พ';
            191: s1:=s1+'ฟ';
            192: s1:=s1+'ภ';
            193: s1:=s1+'ม';
            194: s1:=s1+'ย';
            195: s1:=s1+'ร';
            196: s1:=s1+'ฤ';
            197: s1:=s1+'ล';
            198: s1:=s1+'ฦ';
            199: s1:=s1+'ว';
            200: s1:=s1+'ศ';
            201: s1:=s1+'ษ';
            202: s1:=s1+'ส';
            203: s1:=s1+'ห';
            204: s1:=s1+'ฬ';
            205: s1:=s1+'อ';
            206: s1:=s1+'ฮ';
            207: s1:=s1+'ฯ';
            208: s1:=s1+'ะ';
            209: s1:=s1+'ั';
            210: s1:=s1+'า';
            211: s1:=s1+'ำ';
            212: s1:=s1+'ิ';
            213: s1:=s1+'ี';
            214: s1:=s1+'ึ';
            215: s1:=s1+'ื';
            216: s1:=s1+'ุ';
            217: s1:=s1+'ู';
            218: s1:=s1+'ฺ';
            //219: s1:=s1+'';
            //220: s1:=s1+'';
            //221: s1:=s1+'';
            //222: s1:=s1+'';
            223: s1:=s1+'฿';
            224: s1:=s1+'เ';
            225: s1:=s1+'แ';
            226: s1:=s1+'โ';
            227: s1:=s1+'ใ';
            228: s1:=s1+'ไ';
            229: s1:=s1+'ๅ';
            230: s1:=s1+'ๆ';
            231: s1:=s1+'็';
            232: s1:=s1+'่';
            233: s1:=s1+'้';
            234: s1:=s1+'๊';
            235: s1:=s1+'๋';
            236: s1:=s1+'์';
            237: s1:=s1+'ํ';
            238: s1:=s1+'๎';
            239: s1:=s1+'๏';
            240: s1:=s1+'๐';
            241: s1:=s1+'๑';
            242: s1:=s1+'๒';
            243: s1:=s1+'๓';
            244: s1:=s1+'๔';
            245: s1:=s1+'๕';
            246: s1:=s1+'๖';
            247: s1:=s1+'๗';
            248: s1:=s1+'๘';
            249: s1:=s1+'๙';
            250: s1:=s1+'๚';
            251: s1:=s1+'๛';
            //252: s1:=s1+'';
            //253: s1:=s1+'';
            //254: s1:=s1+'';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
1258: //Vietnam
   begin
      for i:=1 to length(s) do
         case ord(s[i]) of
            127: s1:=s1+'⌂';
            128: s1:=s1+'€';
            //129: s1:=s1+'';
            130: s1:=s1+'‚';
            131: s1:=s1+'ƒ';
            132: s1:=s1+'„';
            133: s1:=s1+'…';
            134: s1:=s1+'†';
            135: s1:=s1+'‡';
            136: s1:=s1+'ˆ';
            137: s1:=s1+'‰';
            //138: s1:=s1+'';
            139: s1:=s1+'‹';
            140: s1:=s1+'Œ';
            //141: s1:=s1+'';
            //142: s1:=s1+'';
            //143: s1:=s1+'';
            //144: s1:=s1+'';
            145: s1:=s1+'‘';
            146: s1:=s1+'’';
            147: s1:=s1+'“';
            148: s1:=s1+'”';
            149: s1:=s1+'•';
            150: s1:=s1+'–';
            151: s1:=s1+'—';
            152: s1:=s1+'˜';
            153: s1:=s1+'™';
            //154: s1:=s1+'';
            155: s1:=s1+'›';
            156: s1:=s1+'œ';
            //157: s1:=s1+'';
            //158: s1:=s1+'';
            159: s1:=s1+'Ÿ';
            //160: s1:=s1+'';
            161: s1:=s1+'¡';
            162: s1:=s1+'¢';
            163: s1:=s1+'£';
            164: s1:=s1+'¤';
            165: s1:=s1+'¥';
            166: s1:=s1+'¦';
            167: s1:=s1+'§';
            168: s1:=s1+'¨';
            169: s1:=s1+'©';
            170: s1:=s1+'ª';
            171: s1:=s1+'«';
            172: s1:=s1+'¬';
            //173: s1:=s1+'';
            174: s1:=s1+'®';
            175: s1:=s1+'¯';
            176: s1:=s1+'°';
            177: s1:=s1+'±';
            178: s1:=s1+'²';
            179: s1:=s1+'³';
            180: s1:=s1+'´';
            181: s1:=s1+'µ';
            182: s1:=s1+'¶';
            183: s1:=s1+'·';
            184: s1:=s1+'¸';
            185: s1:=s1+'¹';
            186: s1:=s1+'º';
            187: s1:=s1+'»';
            188: s1:=s1+'¼';
            189: s1:=s1+'½';
            190: s1:=s1+'¾';
            191: s1:=s1+'¿';
            192: s1:=s1+'À';
            193: s1:=s1+'Á';
            194: s1:=s1+'Â';
            195: s1:=s1+'Ă';
            196: s1:=s1+'Ä';
            197: s1:=s1+'Å';
            198: s1:=s1+'Æ';
            199: s1:=s1+'Ç';
            200: s1:=s1+'È';
            201: s1:=s1+'É';
            202: s1:=s1+'Ê';
            203: s1:=s1+'Ë';
            204: s1:=s1+'◌̀';
            205: s1:=s1+'Í';
            206: s1:=s1+'Î';
            207: s1:=s1+'Ï';
            208: s1:=s1+'Đ';
            209: s1:=s1+'Ñ';
            210: s1:=s1+'◌̉';
            211: s1:=s1+'Ó';
            212: s1:=s1+'Ô';
            213: s1:=s1+'Ơ';
            214: s1:=s1+'Ö';
            215: s1:=s1+'×';
            216: s1:=s1+'Ø';
            217: s1:=s1+'Ù';
            218: s1:=s1+'Ú';
            219: s1:=s1+'Û';
            220: s1:=s1+'Ü';
            221: s1:=s1+'Ư';
            222: s1:=s1+'◌̃';
            223: s1:=s1+'ß';
            224: s1:=s1+'à';
            225: s1:=s1+'á';
            226: s1:=s1+'â';
            227: s1:=s1+'ă';
            228: s1:=s1+'ä';
            229: s1:=s1+'å';
            230: s1:=s1+'æ';
            231: s1:=s1+'ç';
            232: s1:=s1+'è';
            233: s1:=s1+'é';
            234: s1:=s1+'ê';
            235: s1:=s1+'ë';
            236: s1:=s1+'◌́';
            237: s1:=s1+'í';
            238: s1:=s1+'î';
            239: s1:=s1+'ï';
            240: s1:=s1+'đ';
            241: s1:=s1+'ñ';
            242: s1:=s1+'◌̣';
            243: s1:=s1+'ó';
            244: s1:=s1+'ô';
            245: s1:=s1+'ơ';
            246: s1:=s1+'ö';
            247: s1:=s1+'÷';
            248: s1:=s1+'ø';
            249: s1:=s1+'ù';
            250: s1:=s1+'ú';
            251: s1:=s1+'û';
            252: s1:=s1+'ü';
            253: s1:=s1+'ư';
            254: s1:=s1+'₫';
            255: s1:=s1+'ÿ';
            else s1:=s1+s[i];
         end;
      s:=s1;
   end;
65001: //UTF-8
   begin
   end;//let s unaltered
else
   for i:=1 to length(s) do
      begin
      if comparestr(s[i],'~')>0 then s[i]:='?';
      end;
end;
{$ENDIF}
end;

end.
