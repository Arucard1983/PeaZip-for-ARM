{-Test prog for crcmodel, we Mar.2010}

program t_cmodel;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}

uses
  {$ifdef USEDLL}
    {$ifdef VirtualPascal}
      ch_intv,
    {$else}
      ch_intf,
    {$endif}
  {$else}
    crcmodel,
  {$endif}
  {$ifdef WINCRT}
    WinCRT,
  {$endif}
  BTypes, crcm_cat;

var
  CTab: TCRC32Tab;

var
  ctx: TCRC_ctx;
  buf: array[0..$1000-1] of byte;


{---------------------------------------------------------------------------}
procedure Test(Para: TCRCParam);
var
  CRCF,CRCT,c1,c2,cc: longint;
  Err: word;
const
  p1: array[0..4] of char8 = '12345';
  p2: array[0..3] of char8 = '6789';
  cs: array[0..8] of char8 = '123456789';
begin
  cm_Create(Para,nil,ctx);
  cm_File('digs',ctx,CRCF,buf,sizeof(buf),Err);
  write(Para.name:19, cm_SelfTest(Para):10);
  write((Err=0) and (ctx.check=CRCF):10);
  cm_CalcTab(Para, CTab);
  cm_Create(Para,@CTab,ctx);
  cm_Full(ctx,CRCT,@cs,9);
  cm_Full(ctx, c1, @p1, sizeof(p1));
  cm_Full(ctx, c2, @p2, sizeof(p2));
  cc := cm_combine(Para,c1,c2,sizeof(p2));
  writeln(ctx.check=CRCT:10, ctx.check=cc:10);
end;



begin
  {$ifdef USEDLL}
    writeln('CH_DLL Version ',CH_DLL_Version);
  {$endif}

  {Selftest, File no table, String with table}

  writeln('CRC name':19,'Selftest':10, 'File/NT':10, 'Str/Tab':10,'Combine':10 );
  writeln('-----------------------------------------------------------');

  Test(CRC3_ROHC);
  Test(CRC4_ITU);
  Test(CRC5_EPC);
  Test(CRC5_ITU);
  Test(CRC5_USB);
  Test(CRC6_DARC);
  Test(CRC6_ITU);
  Test(CRC7);
  Test(CRC7_ROHC);
  Test(CRC8);
  Test(CRC8_DARC);
  Test(CRC8_ICODE);
  Test(CRC8_ITU);
  Test(CRC8_MAXIM);
  Test(CRC8_ROHC);
  Test(CRC8_WCDMA);
  Test(CRC10);
  Test(CRC11);
  Test(CRC12);
  Test(CRC14_DARC);
  Test(CRC15);
  Test(CRC16_ARC);
  Test(CRC16_ATOM);
  Test(CRC16_AUG2_CITT);
  Test(CRC16_AUG_CITT);
  Test(CRC16_BT_CHIP);
  Test(CRC16_BUYPASS);
  Test(CRC16_CITT);
  Test(CRC16_DDS110);
  Test(CRC16_DNP);
  Test(CRC16_EN_13757);
  Test(CRC16_ICODE);
  Test(CRC16_KERMIT);
  Test(CRC16_MAXIM);
  Test(CRC16_MCRF4XX);
  Test(CRC16_MODBUS);
  Test(CRC16_R);
  Test(CRC16_RIELLO);
  Test(CRC16_T10_DIF);
  Test(CRC16_TELEDISK);
  Test(CRC16_USB);
  Test(CRC16_X25);
  Test(CRC16_XKERMIT);
  Test(CRC16_ZMODEM);
  Test(CRC24_FLEXRAYA);
  Test(CRC24_FLEXRAYB);
  Test(CRC24_PGP);
  Test(CRC32_BZIP2);
  Test(CRC32_C);
  Test(CRC32_D);
  Test(CRC32_JAMCRC);
  Test(CRC32_MPEG2);
  Test(CRC32_POSIX);
  Test(CRC32_Q);
  Test(CRC32_XFER);
  Test(CRC32_Zip);

end.
