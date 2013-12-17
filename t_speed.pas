{-Test speed prog for CRC/HASH, we 2003-2012}

program t_speed;

{$i STD.INC}

{$ifdef APPCONS}
  {$apptype console}
{$endif}


{$ifndef FPC}
  {$B-,N+}
{$endif}

uses
  {$ifdef WINCRT} WinCRT, {$endif}
  {$ifdef WIN32or64}
    {$ifdef UNIT_SCOPE}
      winapi.windows,
    {$else}
      windows,
    {$endif}
  {$endif}
  BTypes,
  hrtimer,
  hash,
  adler32,
  bcrc32,
  bcrc64,
  crc64,
  rmd160,
  sha1,
  sha224,
  sha256,
  sha384,
  sha512,
  sha5_224,
  sha5_256,
  whirl512,
  ED2K,
  md4,
  md5,
  fcrc32,
  crc32,
  crc24,
  crc16;

const
  NUMBYTES  = 20000;
  NUMROUNDS = 50;

var
  tst: array[1..NUMBYTES] of byte;
  start, stop: comp;
  HR: THRTimer;

{---------------------------------------------------------------------------}
procedure ShowResult(name: Str255);
var
  MB,sec: double;
  cnt,diff: comp;
begin
  cnt := NUMBYTES*NUMROUNDS;
  MB  := cnt/1E6;
  diff:= stop-start;
  sec := diff/CPUFrequency;
  writeln(name:11,'   Cnt/Byte: ', diff/cnt:7:1, ',     MB/s: ',MB/sec:7:2);
end;


{---------------------------------------------------------------------------}
procedure CRC16_Test;
var
  bc: word;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do CRC16Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('CRC16');
end;


{---------------------------------------------------------------------------}
procedure CRC24_Test;
var
  bc: longint;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do CRC24Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('CRC24');
end;


{---------------------------------------------------------------------------}
procedure CRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do CRC32Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('CRC32');
end;


{---------------------------------------------------------------------------}
procedure FCRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do FCRC32Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('FCRC32');
end;


{---------------------------------------------------------------------------}
procedure bCRC32_Test;
var
  bc: longint;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do bCRC32Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('bCRC32');
end;


{---------------------------------------------------------------------------}
procedure Adler32_Test;
var
  bc: longint;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do Adler32Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('Adler32');
end;


{---------------------------------------------------------------------------}
procedure CRC64_Test;
var
  bc: TCRC64;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do CRC64Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('CRC64');
end;


{---------------------------------------------------------------------------}
procedure bCRC64_Test;
var
  bc: TCRC64b;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do bCRC64Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('bCRC64');
end;


{---------------------------------------------------------------------------}
procedure ED2K_Test;
var
  bc: TED2KResult;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do ED2K_Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('eDonkey');
end;


{---------------------------------------------------------------------------}
procedure MD4_Test;
var
  bc: TMD4Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do MD4Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('MD4');
end;


{---------------------------------------------------------------------------}
procedure MD5_Test;
var
  bc: TMD5Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do MD5Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('MD5');
end;


{---------------------------------------------------------------------------}
procedure RMD160_Test;
var
  bc: TRMD160Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do RMD160Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('RIPEMD160');
end;


{---------------------------------------------------------------------------}
procedure SHA1_Test;
var
  bc: TSHA1Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA1Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA1');
end;


{---------------------------------------------------------------------------}
procedure SHA224_Test;
var
  bc: TSHA224Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA224Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA224');
end;


{---------------------------------------------------------------------------}
procedure SHA256_Test;
var
  bc: TSHA256Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA256Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA256');
end;


{---------------------------------------------------------------------------}
procedure SHA384_Test;
var
  bc: TSHA384Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA384Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA384');
end;


{---------------------------------------------------------------------------}
procedure SHA512_Test;
var
  bc: TSHA512Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA512Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA512');
end;


{---------------------------------------------------------------------------}
procedure SHA5_224_Test;
var
  bc: TSHA5_224Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA5_224Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA512/224');
end;


{---------------------------------------------------------------------------}
procedure SHA5_256_Test;
var
  bc: TSHA5_256Digest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do SHA5_256Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('SHA512/256');
end;


{---------------------------------------------------------------------------}
procedure Whirl_Test;
var
  bc: TWhirlDigest;
  rounds: integer;
begin
  start := ReadCycles(HR);
  for rounds:=1 to NUMROUNDS do Whirl_Full(bc, @tst, sizeof(tst));
  stop := ReadCycles(HR);
  ShowResult('Whirlpool');
end;


var
  i: word;
begin
  {$ifdef WIN32or64}
    SetPriorityClass(GetCurrentProcess,HIGH_PRIORITY_CLASS);
  {$endif}
  for i:=1 to NUMBYTES do tst[i] := random(256);
  CRC16_Test;
  CRC24_Test;
  CRC32_Test;
  bCRC32_Test;
  FCRC32_Test;
  Adler32_Test;
  CRC64_Test;
  bCRC64_Test;
  ED2K_Test;
  MD4_Test;
  MD5_Test;
  RMD160_Test;
  SHA1_Test;
  SHA224_Test;
  SHA256_Test;
  SHA384_Test;
  SHA512_Test;
  SHA5_224_Test;
  SHA5_256_Test;
  Whirl_Test;
end.
