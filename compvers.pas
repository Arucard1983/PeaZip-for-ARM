unit CompVers;


{Return compiler version as string or symbol}


interface


(*************************************************************************

 DESCRIPTION     :  Return compiler version as string or symbol

 REQUIREMENTS    :  TP4-7, D1-D7/D9-D12/D17, FPC1/2, VP, (and others)

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  ---

 REMARK          :  D8, D11, D14, D15, D16, BCB5 are untested


 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 0.10     15.05.05  W.Ehrhardt  Initial version
 0.11     27.05.05  we          FPC 2.0
 0.12     21.11.05  we          BCB3/4, Fix BP7-DMPI
 0.13     22.03.06  we          D10 aka D2006
 0.14     13.04.06  we          BCB5, BCB6, D8, FPC202
 0.15     05.11.06  we          FPC204
 0.15     05.11.06  we          FPC204
 0.16     25.05.07  we          D11 aka Delphi 2007, FPC2.1.4
 0.17     12.09.07  we          FPC 2.2.0
 0.18     19.06.08  we          FPC 2.2.2
 0.19     04.10.08  we          D12 aka D2009, TCompString
 0.20     22.03.09  we          FPC 2.2.4
 0.21     15.12.09  we          D14, FPC 2.4.0
 0.22     19.11.10  we          FPC 2.4.2
 0.23     24.05.11  we          FPC 2.4.4
 0.24     01.01.12  we          FPC 2.6.0
 0.25     25.12.12  we          D15, D16, D17, FPC 2.6.2
**************************************************************************)


(*-------------------------------------------------------------------------
 (C) Copyright 2005-2012 Wolfgang Ehrhardt

 This software is provided 'as-is', without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.

 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:

 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

 3. This notice may not be removed or altered from any source distribution.
----------------------------------------------------------------------------*)

type
  TCompiler = ( _Unknown,
                _TP4, _TP5, _TP55, _TP6, _BP7, _BP7DPMI, _BP7WIN, _TPW10, _TPW15,
                _D1, _D2, _D3, _D4, _D5, _D6, _D7, _D9,
                _BCB3, _BCB4,
                _VP21, _FPC, _FPC10, _FPC19, _FPC20, _FPC202,
                _D8, _D10, _D11,
                _BCB5, _BCB6, _BCB7,
                _FPC204, _FPC214, _FPC220, _FPC222,
                _D12,
                _FPC224,
                _D14,
                _FPC240, _FPC242, _FPC244, _FPC260, _FPC262,
                _D15, _D16, _D17
              );

type
  TCompString = string[15];

function Compiler_Sym: TCompiler;
  {-Return compiler version as symbol}

function Compiler_Str: TCompString;
  {-Return compiler version as string}


implementation

{$define unknown}

{$ifdef VER10}
  {$undef unknown}
{$endif}

{$ifdef VER15}
  {$undef unknown}
{$endif}

{$ifdef VER40}
  {$undef unknown}
{$endif}

{$ifdef VER50}
  {$undef unknown}
{$endif}

{$ifdef VER55}
  {$undef unknown}
{$endif}

{$ifdef VER60}
  {$undef unknown}
{$endif}

{$ifdef VER70}
  {$ifdef windows}
    {$undef unknown}
  {$else}
    {$ifdef DPMI}
      {$undef unknown}
    {$else}
      {$undef unknown}
    {$endif}
  {$endif}
{$endif}

{$ifdef VER80}
  {$undef unknown}
{$endif}

{$ifdef VER90}
  {$undef unknown}
{$endif}

{$ifdef VER100}
  {$undef unknown}
{$endif}

{$ifdef VER110}
  {$undef unknown}
{$endif}

{$ifdef VER120}
  {$undef unknown}
{$endif}

{$ifdef VER125}
  {$undef unknown}
{$endif}

{$ifdef VER130}
  {$undef unknown}
{$endif}

{$ifdef VER140}
  {$undef unknown}
{$endif}

{$ifdef VER150}
  {$undef unknown}
{$endif}

{$ifdef VER170}
  {$undef unknown}
{$endif}

{$ifdef VER160}
  {$undef unknown}
{$endif}

{$ifdef Ver180}
  {$undef unknown}
{$endif}

{$ifdef Ver190}
  {$undef unknown}
{$endif}

{$ifdef Ver200}
  {$undef unknown}
{$endif}

{$ifdef Ver210}
  {$undef unknown}
{$endif}

{$ifdef Ver220}
  {$undef unknown}
{$endif}

{$ifdef Ver230}
  {$undef unknown}
{$endif}

{$ifdef Ver240}
  {$undef unknown}
{$endif}

{$ifdef VirtualPascal}
  {$undef unknown}
{$endif}

{$ifdef FPC}
  {$ifdef VER1}
    {$undef unknown}
  {$else}
    {$ifdef VER2}
      {$ifdef VER2_0_2}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_0_4}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_1_4}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_2_0}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_2_2}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_2_4}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_4_0}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_4_2}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_4_4}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_6_0}
        {$undef unknown}
      {$endif}
      {$ifdef VER2_6_2}
        {$undef unknown}
      {$endif}
      {$ifdef unknown}
        {$define unkFPC}
        {$undef unknown}
      {$endif}
    {$else}
      {$ifdef unknown}
        {$define unkFPC}
        {$undef unknown}
      {$endif}
    {$endif}
  {$endif}

{$endif}

{---------------------------------------------------------------------------}
function Compiler_Sym: TCompiler;
  {-Return compiler version as symbol}
begin
  {$ifdef unknown}
    Compiler_Sym := _Unknown;
  {$endif}

  {$ifdef VER10}
    Compiler_Sym := _TPW10;
  {$endif}

  {$ifdef VER15}
    Compiler_Sym := _TPW15;
  {$endif}

  {$ifdef VER40}
    Compiler_Sym := _TP4;
  {$endif}

  {$ifdef VER50}
    Compiler_Sym := _TP5;
  {$endif}

  {$ifdef VER55}
    Compiler_Sym := _TP55;
  {$endif}

  {$ifdef VER60}
    Compiler_Sym := _TP6;
  {$endif}

  {$ifdef VER70}
    {$ifdef windows}
      Compiler_Sym := _BP7WIN;
    {$else}
      {$ifdef DPMI}
        Compiler_Sym := _BP7DPMI;
      {$else}
        Compiler_Sym := _BP7;
      {$endif}
    {$endif}
  {$endif}

  {$ifdef VER80}
    Compiler_Sym := _D1;
  {$endif}

  {$ifdef VER90}
    Compiler_Sym := _D2;
  {$endif}

  {$ifdef VER100}
    Compiler_Sym := _D3;
  {$endif}

  {$ifdef VER110}
    Compiler_Sym := _BCB3;
  {$endif}

  {$ifdef VER120}
    Compiler_Sym := _D4;
  {$endif}

  {$ifdef VER125}
    Compiler_Sym := _BCB4;
  {$endif}

  {$ifdef VER130}
    {$ifdef BCB}
      Compiler_Sym := _BCB5;
    {$else}
      Compiler_Sym := _D5;
    {$endif}
  {$endif}

  {$ifdef VER140}
    {$ifdef BCB}
      Compiler_Sym := _BCB6;
    {$else}
      Compiler_Sym := _D6;
    {$endif}
  {$endif}

  {$ifdef VER150}
    Compiler_Sym := _D7;
  {$endif}

  {$ifdef VER160}
    Compiler_Sym := _D8;
  {$endif}

  {$ifdef VER170}
    Compiler_Sym := _D9;
  {$endif}

  {$ifdef Ver180}
    {$ifdef Ver185}
      Compiler_Sym := _D11;
    {$else}
      Compiler_Sym := _D10;
    {$endif}
  {$endif}

  {$ifdef Ver190}
    Compiler_Sym := _D11;  {D11/2007 for .NET}
  {$endif}

  {$ifdef Ver200}
    Compiler_Sym := _D12;
  {$endif}

  {$ifdef Ver210}
    Compiler_Sym := _D14;
  {$endif}

  {$ifdef Ver220}
    Compiler_Sym := _D15;
  {$endif}

  {$ifdef Ver230}
    Compiler_Sym := _D16;
  {$endif}

  {$ifdef Ver240}
    Compiler_Sym := _D17;
  {$endif}

  {$ifdef VirtualPascal}
    Compiler_Sym := _VP21;
  {$endif}

  {$ifdef FPC}
    {$ifdef VER1}
      {$ifndef VER1_0}
        Compiler_Sym := _FPC19;
      {$else}
        Compiler_Sym := _FPC10;
      {$endif}
    {$else}
      {$ifdef VER2}
        {$ifdef VER2_0_2}
          Compiler_Sym := _FPC202;
        {$endif}
        {$ifdef VER2_0_4}
          Compiler_Sym := _FPC204;
        {$endif}
        {$ifdef VER2_1_4}
          Compiler_Sym := _FPC214;
        {$endif}
        {$ifdef VER2_2_0}
          Compiler_Sym := _FPC220;
        {$endif}
        {$ifdef VER2_2_2}
          Compiler_Sym := _FPC222;
        {$endif}
        {$ifdef VER2_2_4}
          Compiler_Sym := _FPC224;
        {$endif}
        {$ifdef VER2_4_0}
          Compiler_Sym := _FPC240;
        {$endif}
        {$ifdef VER2_4_2}
          Compiler_Sym := _FPC242;
        {$endif}
        {$ifdef VER2_4_4}
          Compiler_Sym := _FPC244;
        {$endif}
        {$ifdef VER2_6_0}
          Compiler_Sym := _FPC260;
        {$endif}
        {$ifdef VER2_6_2}
          Compiler_Sym := _FPC262;
        {$endif}
        {$ifdef unkFPC}
          Compiler_Sym := _FPC;
        {$endif}
      {$else}
        {$ifdef unkFPC}
          Compiler_Sym := _FPC;
        {$endif}
      {$endif}
    {$endif}
  {$endif}
end;


{---------------------------------------------------------------------------}
function Compiler_Str: TCompString;
  {-Return compiler version as string}
begin
  {$ifdef unknown}
    Compiler_Str := '(unknown)';
  {$endif}

  {$ifdef VER10}
    Compiler_Str := 'TPW10';
  {$endif}

  {$ifdef VER15}
    Compiler_Str := 'TPW15';
  {$endif}

  {$ifdef VER40}
    Compiler_Str := 'TP4';
  {$endif}

  {$ifdef VER50}
    Compiler_Str := 'TP5';
  {$endif}

  {$ifdef VER55}
    Compiler_Str := 'TP55';
  {$endif}

  {$ifdef VER60}
    Compiler_Str := 'TP6';
  {$endif}

  {$ifdef VER70}
    {$ifdef windows}
      Compiler_Str := 'BP7WIN';
    {$else}
      {$ifdef DPMI}
        Compiler_Str := 'BP7DMPI';
      {$else}
        Compiler_Str := 'BP70';
      {$endif}
    {$endif}
  {$endif}

  {$ifdef VER80}
    Compiler_Str := 'D1';
  {$endif}

  {$ifdef VER90}
    Compiler_Str := 'D2';
  {$endif}

  {$ifdef VER100}
    Compiler_Str := 'D3';
  {$endif}

  {$ifdef VER110}
    Compiler_Str := 'BCB3';
  {$endif}

  {$ifdef VER120}
    Compiler_Str := 'D4';
  {$endif}

  {$ifdef VER125}
    Compiler_Str := 'BCB4';
  {$endif}

  {$ifdef VER130}
    {$ifdef BCB}
      Compiler_Str := 'BCB5';
    {$else}
      Compiler_Str := 'D5';
    {$endif}
  {$endif}

  {$ifdef VER140}
    {$ifdef BCB}
      Compiler_Str := 'BCB6';
    {$else}
      Compiler_Str := 'D6';
    {$endif}
  {$endif}

  {$ifdef VER150}
    Compiler_Str := 'D7';
  {$endif}

  {$ifdef VER160}
    Compiler_Str := 'D8';
  {$endif}

  {$ifdef VER170}
    Compiler_Str := 'D9';      {2005}
  {$endif}

  {$ifdef Ver180}
    {$ifdef Ver185}
      Compiler_Str := 'D11';   {2007.Win32}
    {$else}
      Compiler_Str := 'D10';   {BDS2006}
    {$endif}
  {$endif}

  {$ifdef VER190}
    Compiler_Str := 'D11';     {2007.NET}
  {$endif}

  {$ifdef VER200}
    Compiler_Str := 'D12';     {2009}
  {$endif}

  {$ifdef VER210}
    Compiler_Str := 'D14';     {2010}
  {$endif}

  {$ifdef Ver220}
    Compiler_Str := 'D15';     {XE}
  {$endif}

  {$ifdef Ver230}
    Compiler_Str := 'D16';     {XE2}
  {$endif}

  {$ifdef Ver240}
    Compiler_Str := 'D17';     {XE3}
  {$endif}

  {$ifdef VirtualPascal}
    Compiler_Str := 'VP21';
  {$endif}


  {$ifdef FPC}
    {$ifdef VER1}
      {$ifndef VER1_0}
         Compiler_Str := 'FPC19x';
      {$else}
         Compiler_Str := 'FPC10';
      {$endif}
    {$else}
      {$ifdef VER2}
        {$ifdef VER2_0_2}
          Compiler_Str := 'FPC202';
        {$endif}
        {$ifdef VER2_0_4}
          Compiler_Str := 'FPC204';
        {$endif}
        {$ifdef VER2_1_4}
          Compiler_Str := 'FPC214';
        {$endif}
        {$ifdef VER2_2_0}
          Compiler_Str := 'FPC220';
        {$endif}
        {$ifdef VER2_2_2}
          Compiler_Str := 'FPC222';
        {$endif}
        {$ifdef VER2_2_4}
          Compiler_Str := 'FPC224';
        {$endif}
        {$ifdef VER2_4_0}
          Compiler_Str := 'FPC240';
        {$endif}
        {$ifdef VER2_4_2}
          Compiler_Str := 'FPC242';
        {$endif}
        {$ifdef VER2_4_4}
          Compiler_Str := 'FPC244';
        {$endif}
        {$ifdef VER2_6_0}
          Compiler_Str := 'FPC260';
        {$endif}
        {$ifdef VER2_6_2}
          Compiler_Str := 'FPC262';
        {$endif}
        {$ifdef unkFPC}
          Compiler_Str := 'FPC2';
        {$endif}
      {$else}
        {$ifdef unkFPC}
          Compiler_Str := 'FPC';
        {$endif}
      {$endif}
    {$endif}
  {$endif}
end;

end.

