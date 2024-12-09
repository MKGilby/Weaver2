{ -[Name]-------------------------------------------

                 TCompressor class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2009-2024

  --------------------------------------------------

  -[Description]------------------------------------

   Data compressor class allowing compression of
   streams to streams with variable compression
   level (and speed).
   
   Also handles the decompression (from stream and
   string).

   Compression level settings (used methods, see below):
     0 - No compression (0)
     1 - Only C2 
     2 - PP+C2
     3 - C1+C2
     4 - PP+C1+C2
    
   Prepocessing methods [PP]: 
     0 - None
     1 - Interleave 2
     2 - Interleave 3
     3 - Delta
     4 - MTF
     5 - Interleave 2 + Delta 
     6 - Interleave 3 + Delta
     7 - Delta+MTF
     
   First compression methods [C1]:
     0 - Not compressed
     1 - RLE
     2 - RLE + HUFF
     3 - RLE + AHUFF
     4 - HUFF
     5 - AHUFF
     
   Second compression method [C2]: 
     0 - Not compressed
     1 - ZLib  

   Compression marker adds up:
     C2*64+C1*8+PP
     
   Examples:
      0 - Not compressed at all 
     65 - Interleave 2 + ZLIB
     91 - Delta + RLE + AHUFF + ZLIB
  --------------------------------------------------

}

// Version info:
//
//  V1.00: Gilby
//    * Initial creation
//    + Methods 0..6 added
//  V1.01: Gilby - 2009.09.13
//    + Methods 7..10 added (Interleave+Delta seems to
//      work greatly on WAVs and some 24 bit TGAs)
//  V1.02: Gilby - 2009.09.16
//    + Uncompress(string):TStream added.
//  V1.03: Gilby - 2009.12.30
//    * 2-MKComp and 3-MKComp now have distinct method id
//    * Compress have a third parameter: CompressionMethod (as written above)
//    + LastCompressionMethod property added
//  V1.10: Gilby - 2010.03.22
//    * Methods now splitted up as Preprocessing, First Compression and
//      Second Compression methods. One method can be picked from each group.
//    + CompressFile and UnCompressFile added.
//  V1.11: Gilby - 2010.10.28
//    * Unit made Delphi compatible.
//  V1.12: Gilby - 2011.08.08
//    * Uncompress: Removed unnecessary stream copying.
//  V1.20: Gilby - 2011.08.24
//    * Using ZStream unit shipped with FreePascal instead of MyZLib
//  V1.21: Gilby - 2011.08.24
//    * Using ZStream unit shipped with FreePascal instead of MyZLib
//  V1.21a: Gilby - 2015.12.09
//    * Fixes to suppress hints in Lazarus
//  V1.22: Gilby - 2018.12.20
//    * Fixed a memory leak.
//  V1.23: Gilby - 2019.12.04
//    * Added ifdefs to disable/enable my compression methods
//  V1.23a: Gilby - 2021.03.15
//    * Fixes to suppress hints in Lazarus
//  V1.24: Gilby - 2021.11.18
//    * Stop using ZLib compression when CompressionLevel=0 passed
//  V1.25: Gilby - 2022.05.16
//    * BUGFIX: Global compressionlevel was not used.
//  V1.26: Gilby - 2024.04.24
//    * Stopped using MKToolBox.Replace since it's deprecated.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}
{$define disableadaptivehuffman}
{define MyComp}

unit CompressorUnit;

interface

uses Classes;

type TCompressor=class
       constructor Create;
       destructor Free;
       function Compress(iSource:TStream;{%H-}iCompressionLevel:integer=-1;{%H-}iCompressionMethod:integer=-1):TStream;
       procedure CompressFile(iSource,iTarget:string;iCompressionLevel:integer=-1;iCompressionMethod:integer=-1);
       function UnCompress(iSource:TStream):TStream; overload;
       function UnCompress(iSource:String):TStream; overload;
       procedure UnCompressFile(iSource,iTarget:String);
       procedure SetCompressionLevel(iLevel:integer);
     private
       fCompressionLevel:integer;
       fLastCompression:string;
       fLastCompressionMethod:integer;
     public
       property CompressionLevel:integer read fCompressionLevel write SetCompressionLevel;
       property LastCompression:string read fLastCompression;
       property LastCompressionMethod:integer read fLastCompressionMethod;
     end;

var Compressor:TCompressor;

implementation

uses
     MyZStreamUnit, {$ifdef MyComp}RLEUnit, HuffmanUnit,
{$ifndef disableadaptivehuffman}
     AdaptiveHuffmanUnit,
{$endif}
     CodingUnit, {$endif} Logger, SysUtils, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.26';

constructor TCompressor.Create;
begin
  fCompressionLevel:=1;
end;

destructor TCompressor.Free;
begin
end;

procedure TCompressor.SetCompressionLevel(iLevel:integer);
begin
  if iLevel in [0..4] then fCompressionLevel:=iLevel;
end;

{$ifdef MyComp}
function NowRLE(src:TStream;var RLEStream:TStream):TStream;
begin
  if RLEStream=nil then begin
    src.Seek(0,soFromBeginning);
    RLEStream:=TMemoryStream.Create;
    RLE(src,RLEStream,src.Size);
  end;
  Result:=TMemoryStream.Create;
  RLEStream.Seek(0,soFromBeginning);
  Result.CopyFrom(RLEStream,RLEStream.Size);
end;
{$endif}

function TCompressor.Compress(iSource:TStream;iCompressionLevel:integer=-1;iCompressionMethod:integer=-1):TStream;
const
//  Istr=Fstr+'TCompressor.Compress';
  PPNames:array[0..6] of string[16]=('','Intl2','Intl3','Delta','MTF','Intl2+Delta','Intl3+Delta');
  C1Names:array[0..5] of string[16]=('','RLE','RLE+Huff','RLE+AHuff','Huff','AHuff');
  C2Names:array[0..1] of string[16]=('','ZLib');
var
  op : cardinal;
  Head0:string[5];
  AfterPP, AfterC1, AfterC2, Xs, Ys:TStream;
  BestStream:TStream;
{$ifdef MyComp}
  TempCompL:integer;
  AfterRLE:TStream;
{$endif}
  ipp,ic1,ic2:integer;
  PPMethods, C1Methods, C2Methods: set of byte;
begin
  if iCompressionLevel=-1 then iCompressionLevel:=fCompressionLevel;
  PPMethods:=[0];
  C1Methods:=[0];
  {$ifdef MyComp}
  TempCompL:=fCompressionLevel;
  if iCompressionLevel in [0..4] then TempCompL:=iCompressionLevel;
  if iCompressionMethod in [0..255] then TempCompL:=-1;
  C2Methods:=[0];
//  Log.Trace('TempCompL='+inttostr(tempcompl));
  case TempCompL of
   -1:begin
        PPMethods:=PPMethods+[iCompressionMethod and 7];
        C1Methods:=C1Methods+[(iCompressionMethod shr 3) and 7];
        C2Methods:=C2Methods+[(iCompressionMethod shr 6) and 1];
      end;
    1:begin
        C2Methods:=C2Methods+[1];
      end;
    2:begin
        PPMethods:=PPMethods+[1,2,3,4,5,6,7];
        C2Methods:=C2Methods+[1];
      end;
    3:begin
        C1Methods:=C1Methods+[1,2,3,4,5];
        C2Methods:=C2Methods+[1];
      end;
    4:begin
        PPMethods:=PPMethods+[1,2,3,4,5,6,7];
        C1Methods:=C1Methods+[1,2,3,4,5];
        C2Methods:=C2Methods+[1];
      end;
  end;
  {$else}
//  TempCompL:=1;
  C2Methods:=[0];
  if iCompressionLevel>0 then C2Methods:=C2Methods+[1];
  {$endif}

  op:=iSource.Position;
  iSource.Seek(0,soFromBeginning);
  Head0:=#0#0#0#0;
  ipp:=iSource.Size;
  move(ipp,Head0[1],4);
  Ys:=TMemoryStream.Create;
  Ys.Write(Head0[1],4);
  Ys.Write(Head0[1],1);
  Ys.CopyFrom(iSource,iSource.Size);

  BestStream:=Ys;

  for ipp:=0 to 6 do if ipp in PPMethods then begin
//    Log.Trace('IPP='+inttostr(ipp));
    iSource.Seek(0,soFromBeginning);
    {$ifdef MyComp}
    case ipp of
      1:AfterPP:=Interleave(iSource,2);
      2:AfterPP:=Interleave(iSource,3);
      3:AfterPP:=Delta(iSource);
      4:AfterPP:=MTFEncode(iSource);
      5:begin
          Xs:=Interleave(iSource,2);
          AfterPP:=Delta(Xs);
          Xs.Free;
        end;
      6:begin
          Xs:=Interleave(iSource,3);
          AfterPP:=Delta(Xs);
          Xs.Free;
        end;
      7:begin
          Xs:=Delta(iSource);
          AfterPP:=MTFEncode(iSource);
          Xs.Free;
        end;
      else begin {$endif}
        AfterPP:=TMemoryStream.Create;
        AfterPP.CopyFrom(iSource,iSource.Size);
      {$ifdef MyComp}end;
    end;{$endif}
//    AfterRLE:=nil;
//    Log.Trace('AfterPP Size='+inttostr(AfterPP.Size));
    for ic1:=0 to 5 do if ic1 in C1Methods then begin
//      Log.Trace('IPP='+inttostr(ipp)+' IC1='+inttostr(ic1));

      AfterPP.Seek(0,soFromBeginning);
      {$ifdef MyComp}
      case ic1 of
        1:AfterC1:=NowRLE(AfterPP, AfterRLE);
        2:begin
            Xs:=NowRLE(AfterPP, AfterRLE);
            AfterC1:=TMemoryStream.Create;
            Huffman.Encode(Xs, AfterC1);
            Xs.Free;
          end;
{$ifndef disableadaptivehuffman}
        3:begin
            Xs:=NowRLE(AfterPP, AfterRLE);
            AfterC1:=TMemoryStream.Create;
            AdaptiveHuffman.Encode(Xs, AfterC1);
            Xs.Free;
          end;
{$endif}
        4:begin
            AfterC1:=TMemoryStream.Create;
            Huffman.Encode(AfterPP, AfterC1);
          end;  
{$ifndef disableadaptivehuffman}
        5:begin
            AfterC1:=TMemoryStream.Create;
            AdaptiveHuffman.Encode(AfterPP, AfterC1);
          end;
{$endif}          
        else begin{$endif}
          AfterC1:=TMemoryStream.Create;
          AfterC1.CopyFrom(AfterPP,AfterPP.Size);
        {$ifdef MyComp}end;
      end;{$endif}
//      Log.Trace('AfterC1 Size='+inttostr(AfterC1.Size));
      for ic2:=0 to 1 do if ic2 in C2Methods then begin
//        Log.Trace('IPP='+inttostr(ipp)+' IC1='+inttostr(ic1)+' IC2='+inttostr(ic2));

        AfterC1.Seek(0,soFromBeginning);
        AfterC2:=TMemoryStream.Create;
        case ic2 of
          1:CompressStream(AfterC1,AfterC2,AfterC1.Size);
          else begin
            AfterC2.Write(Head0[1],4);
            AfterC2.CopyFrom(AfterC1,AfterC1.Size);
          end;
        end;
//        Log.Trace('AfterC2 Size='+inttostr(AfterC2.Size));
        if AfterC2.Size<BestStream.Size then begin
          Xs:=BestStream;
          BestStream:=AfterC2;
          fLastCompressionMethod:=IC2*64+IC1*8+IPP;
          fLastCompression:='';
          if ipp>0 then fLastCompression+=PPNames[ipp]+'+';
          if ic1>0 then fLastCompression+=C1Names[ic1]+'+';
          if ic2>0 then fLastCompression+=C2Names[ic1];

          if (length(fLastCompression)>0) and (fLastCompression[length(fLastCompression)]='+') then delete(fLastCompression,length(fLastCompression),1);
          if length(fLastCompression)=0 then fLastCompression:='None';
          AfterC2:=Xs;
        end;
        AfterC2.Free;
      end;
      AfterC1.Free;
    end;
    AfterPP.Free;
  end;
//  Log.Trace('COMP 3');

  BestStream.Seek(0,soFromBeginning);
  Result:=TMemoryStream.Create;
  Result.Write(fLastCompressionMethod,1);
  Result.CopyFrom(BestStream,BestStream.Size);
  BestStream.Free;
//  Log.Trace('COMP 4');

  iSource.Seek(op,soFromBeginning);
end;

function TCompressor.UnCompress(iSource:TStream):TStream;
//const Istr=Fstr+'TCompressor.UnCompress';
var
  b:byte;
  afterC2,afterC1:TStream;
  {$ifdef MyComp}Xs:TStream;{$endif}
begin
  b:=0;
  iSource.Seek(0,soFromBeginning);
  iSource.Read(b,1);
//  Log.LogDebug('Compression method byte: '+inttostr(b),Istr);

  afterC2:=TMemoryStream.Create;
//  Log.Trace((b shr 6) and 1);
  // C2
  case (b shr 6) and 1 of
    0:begin
//        Log.LogDebug('No ZLib',Istr);
        iSource.Seek(5,soFromBeginning);
        afterC2.CopyFrom(iSource,iSource.Size-5);
      end;
    1:begin
//        Log.LogDebug('ZLib',Istr);
        iSource.Seek(1,soFromBeginning);
        UncompressStream(iSource,afterC2);
      end;
  end;

  afterC2.Seek(0,soFromBeginning);
  afterC1:=TMemoryStream.Create;
  case (b shr 3) and 7 of
    0:begin
        FreeAndNil(afterC1);
        afterC1:=afterC2;
        afterC2:=nil;
      end;
    {$ifdef MyComp}
    1:begin
        UnRLE(afterC2,afterC1);
      end;
    2:begin
        Xs:=TMemoryStream.Create;
        Huffman.Decode(afterC2,Xs);
        Xs.Seek(0,soFromBeginning);
        UnRLE(Xs,afterC1);
        Xs.Free;
      end;
{$ifndef disableadaptivehuffman}
    3:begin
        Xs:=TMemoryStream.Create;
        AdaptiveHuffman.Decode(afterC2,Xs);
        Xs.Seek(0,soFromBeginning);
        UnRLE(Xs,afterC1);
        Xs.Free;
      end;
{$endif}
    4:begin
        Huffman.Decode(afterC2,afterC1);
      end;
{$ifndef disableadaptivehuffman}
    5:begin
        AdaptiveHuffman.Decode(afterC2,afterC1);
      end;
{$endif}
    {$else}
      else raise Exception.Create(Format('Not supported compression method! (%d)',[b]));
    {$endif}
  end;
  if AfterC2<>nil then AfterC2.Free;
  AfterC1.Seek(0,soFromBeginning);
  {$ifdef MyComp}
  case b and 7 of
    1:Result:=DeInterleave(afterC1,2);
    2:Result:=DeInterleave(afterC1,3);
    3:Result:=UnDelta(afterC1);
    4:Result:=MTFDecode(afterC1);
    5:begin
        Xs:=UnDelta(afterC1);
        Result:=DeInterleave(Xs,2);
        Xs.Free;
      end;
    6:begin
        Xs:=UnDelta(afterC1);
        Result:=DeInterleave(Xs,3);
        Xs.Free;
      end;
    7:begin
        Xs:=MTFDecode(afterC1);
        Result:=UnDelta(Xs);
        Xs.Free;
      end;
    else begin  { None } {$endif}
      Result:=AfterC1;
      AfterC1:=nil;
    {$ifdef MyComp} end;
  end;{$endif}
  if AfterC1<>nil then AfterC1.Free;
//  Log.Trace('AfterPP Size='+inttostr(Result.Size));
  Result.Seek(0,soFromBeginning);
end;

function TCompressor.UnCompress(iSource:String):TStream;
var Xs:TStream;
begin
  Xs:=TMemoryStream.Create;
  Xs.Write(iSource[1],length(iSource));
  Xs.Seek(0,soFromBeginning);
  Result:=UnCompress(Xs);
  Xs.Free;
end;

procedure TCompressor.CompressFile(iSource,iTarget:string;iCompressionLevel:integer=-1;iCompressionMethod:integer=-1);
var Xs,Ys:TStream;
begin
  Xs:=TFileStream.Create(iSource,fmOpenRead or fmShareDenyNone);
  Ys:=Compressor.Compress(Xs,iCompressionLevel,iCompressionMethod);
  Xs.Free;
  Ys.Seek(0,soFromBeginning);
  Xs:=TFileStream.Create(iTarget,fmCreate);
  Xs.CopyFrom(Ys,Ys.Size);
  Ys.Free;
  Xs.Free;
end;

procedure TCompressor.UnCompressFile(iSource,iTarget:String);
var Xs,Ys:TStream;
begin
  Xs:=TFileStream.Create(iSource,fmOpenRead or fmShareDenyNone);
  Ys:=Compressor.UnCompress(Xs);
  Xs.Free;
  Ys.Seek(0,soFromBeginning);
  Xs:=TFileStream.Create(iTarget,fmCreate);
  Xs.CopyFrom(Ys,Ys.Size);
  Ys.Free;
  Xs.Free;
end;

initialization
begin
//  Log.LogStatus('Creating Compressor class...','CompressorUnit.pas, Initialization');
  Log.LogStatus(Fstr+'version '+Version,'uses');
  Compressor:=TCompressor.Create;
//  Log.LogStatus('Compressor class created...','CompressorUnit.pas, Initialization');
end;

finalization
begin
//  Log.LogStatus('Freeing Compressor class...','CompressorUnit.pas, Finalization');
  Compressor.Free;
//  Log.LogStatus('Compressor class freed...','CompressorUnit.pas, Finalization');
end;

end.
