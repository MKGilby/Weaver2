// Core procedures to compress or decompress a file or a stream.

// Version info:
//
//  V1.00: Gilby
//    * Initial creation
//  V1.01: Gilby - 2012.07.30
//    * Skipheader is false by default
//  V1.02: Gilby - 2013.04.12
//    + Compress with untyped data

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MyZStreamUnit;

interface

uses Classes, ZStream{Light}, Logger;

{type
  TCompressionStream2=class(TCompressionStream)
    procedure CopyFrom(Source:TStream;size:integer);
  end;

{  TDeCompressionStream2=class(TDeCompressionStream)
    procedure CopyFrom(Source:TStream;size:integer);
  end;}

// Skipheader=false is required when compressing PDF stream!!!
procedure CompressStream(src,trg:TStream;size:longint;skipheader:boolean=false);
procedure UnCompressStream(src,trg:TStream;skipheader:boolean=false);
procedure CompressFile(inname,outname:string;skipheader:boolean=false);
procedure UnCompressFile(inname,outname:string;skipheader:boolean=false);
procedure Compress(var src;trg:TStream;size:longint;skipheader:boolean=false);

implementation

uses SysUtils;

const
  Fstr='MyZStreamUnit.pas, ';
  Version='1.02';
  BufSize=32768;

var 
  Buffer:pointer;

{procedure TCompressionStream2.CopyFrom(Source:TStream;size:integer);
var i:integer;
begin
  for i:=0 to (size div BufSize)-1 do begin
    Source.Read(buffer^,BufSize); 
    Self.Write(buffer^,BufSize);
  end;
  i:=Size mod BufSize;
  if i>0 then begin   
    Source.Read(buffer^,i); 
    Self.Write(buffer^,i);
  end;  
end;}

procedure CompressStream(src,trg:TStream;size:longint;skipheader:boolean=false);
var CS:TCompressionStream;
begin
//  trg.Write(src.Size,4);
//  CS:=TCompressionStream2.Create(clMax,trg,skipheader);
  CS:=TCompressionStream.Create(clMax,trg,skipheader);
//  CS:=TCompressionStream2.Create(cldefault,trg,true);
  CS.CopyFrom(src,size);
  CS.Free;
end;

procedure Compress(var src;trg:TStream;size:longint;skipheader:boolean=false);
var CS:TCompressionStream;
begin
  CS:=TCompressionStream.Create(clmax,trg,skipheader);
  CS.Write(src,size);
  CS.Free;
end;

procedure UnCompressStream(src,trg:TStream;skipheader:boolean=false);
var
  dc:TDecompressionStream;
  len:integer;
begin
  dc:=TDecompressionStream.Create(src,skipheader);
//  writeln('Decompression started');
  len:=dc.Read(Buffer^,BufSize);
//  writeln('len='+inttostr(len));
  while len>0 do begin
    trg.Write(Buffer^,len);
    len:=dc.Read(Buffer^,BufSize);
//    writeln('len='+inttostr(len));
  end;
  dc.Free;
end;

procedure CompressFile(inname,outname:string;skipheader:boolean);
var s1,s2:TFileStream;
begin
  s1:=TFileStream.Create(inname,fmOpenRead);
  s2:=TFileStream.Create(outname,fmCreate);
  CompressStream(s1,s2,s1.Size,skipheader);
  s1.Free;
  s2.Free;
end;

procedure UnCompressFile(inname,outname:string;skipheader:boolean);
var s1,s2:TFileStream;
begin
  s1:=TFileStream.Create(inname,fmOpenRead);
  s2:=TFileStream.Create(outname,fmCreate);
  UnCompressStream(s1,s2,skipheader);
  s1.Free;
  s2.Free;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  getmem(Buffer,BufSize);
  
finalization
  freemem(Buffer,BufSize);

end.
