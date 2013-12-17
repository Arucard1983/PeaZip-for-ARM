unit HMACWHIR;

{HMAC Whirl - message authentication with Wirlpool, obsolete: use HMAC unit!}


interface

(*************************************************************************

 DESCRIPTION     :  HMAC Whirl - message authentication with Whirlpool hash

 REQUIREMENTS    :  TP5-7, D1-D7/D9, FPC, VP

 EXTERNAL DATA   :  ---

 MEMORY USAGE    :  ---

 DISPLAY MODE    :  ---

 REFERENCES      :  http://csrc.nist.gov/publications/fips/fips198/fips-198a.pdf

 REMARKS         :  "Truncate" not implemented, mac: full digest

 Version  Date      Author      Modification
 -------  --------  -------     ------------------------------------------
 1.00     11.12.05  W.Ehrhardt  Initial version (like HMAC SHA512)
 1.01     17.01.06  we          Obsolete/legacy, shell for HMAC unit
 1.02     12.11.08  we          uses BTypes, Str255
************************************************************************)

(*-------------------------------------------------------------------------
 (C) Copyright 2005-2008 Wolfgang Ehrhardt

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

{$i STD.INC}

uses
  BTypes,Hash,HMAC,Whirl512;


procedure hmac_Whirl_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}
  {$ifdef DLL} stdcall; {$endif}

procedure hmac_Whirl_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}
  {$ifdef DLL} stdcall; {$endif}

procedure hmac_Whirl_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
  {$ifdef DLL} stdcall; {$endif}

procedure hmac_Whirl_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
  {$ifdef DLL} stdcall; {$endif}

procedure hmac_Whirl_final(var ctx: THMAC_Context; var mac: TWhirlDigest);
  {-end data input, calculate HMAC digest}
  {$ifdef DLL} stdcall; {$endif}


implementation


{---------------------------------------------------------------------------}
procedure hmac_Whirl_init(var ctx: THMAC_Context; key: pointer; klen: word);
  {-initialize HMAC context with key}
var
  phash: PHashDesc;
begin
  phash := FindHash_by_ID(_Whirlpool);
  hmac_init(ctx, phash, key, klen);
end;


{---------------------------------------------------------------------------}
procedure hmac_Whirl_inits(var ctx: THMAC_Context; skey: Str255);
  {-initialize HMAC context with skey}
begin
  hmac_Whirl_init(ctx, @skey[1], length(skey));
end;


{---------------------------------------------------------------------------}
procedure hmac_Whirl_update(var ctx: THMAC_Context; data: pointer; dlen: word);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_Whirl_updateXL(var ctx: THMAC_Context; data: pointer; dlen: longint);
  {-HMAC data input, may be called more than once}
begin
  hmac_updateXL(ctx, data, dlen);
end;


{---------------------------------------------------------------------------}
procedure hmac_Whirl_final(var ctx: THMAC_Context; var mac: TWhirlDigest);
  {-end data input, calculate HMAC digest}
var
  d: THashDigest;
begin
  hmac_final(ctx, d);
  move(d, mac, sizeof(mac));
end;

end.

