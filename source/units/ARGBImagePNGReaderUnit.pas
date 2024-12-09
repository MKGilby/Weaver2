{ -[Name]-------------------------------------------

              PNG reader for TARGBImage

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ     Hungary, 2020-2024

  -[Description]------------------------------------

    PNG Reader add-on for ARGBImage unit.
    Simply put this unit into uses to extend
    TARGBImage with ability to read PNG images.

  --------------------------------------------------
}

// Version info:
//   1.00 - Gilby - 2020.03.11
//     * Initial creation from ARGBImagePNGUnit
//   1.01 - Gilby - 2020.03.16-30
//     * Following changes in ARGBImageUnit
//     + Reading animation chunks (anIM, anMZ)
//     + Reading fontdata chunks (fnTZ)
//   1.02 - Gilby - 2020.04.01
//     * Following changes in ARGBImageUnit (TAnimationData -> TAnimationDatas)
//   1.03 - Gilby - 2020.06.18
//     * Bugfix in DecodeScanLine24 with FilterType3
//   1.04 - Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit
//   1.04a - Gilby - 2021.03.15
//     * Changes to suppress hints in Lazarus
//   1.05: Gilby - 2022.03.18
//     * Following changes in AnimationDataUnit
//   1.06: Gilby - 2022.07.19
//     + Reading ReverseAnim flag
//   1.07: Gilby - 2023.11.29
//     * Fix in reading unnamed animations.
//   1.08: Gilby - 2023.12.13
//     * Following changes in AnimationDataUnit.
//     * Ability to read time-based animation data.
//   1.09: Gilby - 2024.03.14
//     * Added loading V3 and V4 animation data.
//   1.10: Gilby - 2024.05.03
//     * Added loading V5 animation data.

unit ARGBImagePNGReaderUnit;

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

interface

implementation

uses Classes, SysUtils, ARGBImageUnit, CRC32Unit, MyZStreamUnit, Logger,
  AnimationDataUnit, FontDataUnit, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.10';

  HEADER=#137#80#78#71#13#10#26#10;

function ReadNBO4(pSource:TStream):uint32;
var i:uint32;
begin
  i:=0;
  pSource.Read(i,4);
  Result:=
     (i and $ff000000)>>24 +
     (i and $ff0000)>>8 +
     (i and $ff00)<<8 +
     (i and $ff)<<24;
end;

// Reads chunk, gives back data as TMemoryStream and type in pType
// If Skip is true then skips chunks that are not used by decoder
function ReadChunk(pSource:TStream;out pType:string;Skip:boolean=true):TMemoryStream;
const CHUNKS='IHDR'+'PLTE'+'tRNS'+'IDAT'+'IEND'+'fnTZ'+'anIM'+'anMZ';
var clen,crc1,crc2:uint32;
begin
  repeat
    clen:=ReadNBO4(pSource);
    pType:=#0#0#0#0;
    pSource.Read(pType[1],4);
    if Skip and (pos(pType,CHUNKS) mod 4<>1) then begin
      pSource.Position:=pSource.Position+clen+4;
    end;
  until not Skip or (pos(pType,CHUNKS) mod 4=1);
  crc2:=CRCSeed;
  crc2:=_CRC32(ord(pType[1]),crc2);
  crc2:=_CRC32(ord(pType[2]),crc2);
  crc2:=_CRC32(ord(pType[3]),crc2);
  crc2:=_CRC32(ord(pType[4]),crc2);
  Result:=TMemoryStream.Create;
  if clen>0 then Result.CopyFrom(pSource,clen);
  crc1:=ReadNBO4(pSource);
  crc2:=CRC32Stream(Result,crc2);
  if crc1<>crc2 then
    raise Exception.Create(Format('CRC error in chunk (%s, expected: %x, got: %x)',[pType,crc1,crc2]));
  Result.Position:=0;
end;

function GetScanLines(pSource:TStream):TMemoryStream;
begin
  Result:=TMemoryStream.Create;
  pSource.Position:=0;
  UnCompressStream(pSource,Result);
  Result.Position:=0;
end;

function Paeth(a,b,c:integer):integer;
var p,pa,pb,pc:integer;
begin
  p:=a+b-c;
  pa:=abs(p-a);
  pb:=abs(p-b);
  pc:=abs(p-c);
  if (pa<=pb) and (pa<=pc) then Result:=a
  else if (pb<=pc) then Result:=b else Result:=c;
end;

procedure DecodeScanLine8(FilterType:byte;ScanLine,PreScanLine,Target:pointer;ScanLineSize:integer);
var i:integer;
begin
  case FilterType of
    0:move(ScanLine^,Target^,ScanLineSize);
    1:begin
        byte(Target^):=byte(ScanLine^);
        for i:=1 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((Target+i-1)^)) and $ff;
      end;
    2:begin
        for i:=0 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((PreScanLine+i)^)) and $ff;
      end;
    3:begin
        byte(Target^):=byte(PreScanLine^) div 2;
        for i:=1 to ScanLineSize-1 do
          byte((Target+i)^):=((byte((ScanLine+i)^)+byte((PreScanLine+i)^)) div 2) and $ff;
      end;
    4:begin
        byte(Target^):=(byte((Scanline)^)+Paeth(0,byte((PreScanLine)^),0)) and $ff;
        for i:=1 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+Paeth(byte((Target+i-1)^),byte((PreScanLine+i)^),byte((PreScanLine+i-1)^))) and $ff;
      end;
  end;
end;

procedure DecodeScanLine16(FilterType:byte;ScanLine,PreScanLine,Target:pointer;ScanLineSize:integer);
var i:integer;
begin
  case FilterType of
    0:move(ScanLine^,Target^,ScanLineSize);
    1:begin
        move(ScanLine^,Target^,2);
        for i:=2 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((Target+i-2)^)) and $ff;
      end;
    2:begin
        for i:=0 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((PreScanLine+i)^)) and $ff;
      end;
    3:begin
        byte(Target^):=((byte((ScanLine)^)+byte(PreScanLine^)) div 2) and $ff;
        byte((Target+1)^):=((byte((ScanLine+1)^)+byte((PreScanLine+1)^)) div 2) and $ff;
        for i:=2 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+(byte((Target+i-2)^)+byte((PreScanLine+i)^)) div 2) and $ff;
      end;
    4:begin
        byte(Target^):=(byte((Scanline)^)+Paeth(0,byte((PreScanLine)^),0)) and $ff;
        byte((Target+1)^):=(byte((ScanLine+1)^)+Paeth(0,byte((PreScanLine+1)^),0)) and $ff;
        for i:=2 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+Paeth(byte((Target+i-2)^),byte((PreScanLine+i)^),byte((PreScanLine+i-2)^))) and $ff;
      end;
  end;
end;

procedure DecodeScanLine24(FilterType:byte;ScanLine,PreScanLine,Target:pointer;ScanLineSize:integer);
var i:integer;
begin
  case FilterType of
    0:move(ScanLine^,Target^,ScanLineSize);
    1:begin
        move(ScanLine^,Target^,3);
        for i:=3 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((Target+i-3)^)) and $ff;
      end;
    2:begin
        for i:=0 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((PreScanLine+i)^)) and $ff;
      end;
    3:begin
        byte(Target^):=(byte((ScanLine)^)+(byte(PreScanLine^)) div 2) and $ff;
        byte((Target+1)^):=(byte((ScanLine+1)^)+(byte((PreScanLine+1)^)) div 2) and $ff;
        byte((Target+2)^):=(byte((ScanLine+2)^)+(byte((PreScanLine+2)^)) div 2) and $ff;
//        byte(Target^):=((byte((ScanLine)^)+byte(PreScanLine^)) div 2) and $ff;
//        byte((Target+1)^):=((byte((ScanLine+1)^)+byte((PreScanLine+1)^)) div 2) and $ff;
//        byte((Target+2)^):=((byte((ScanLine+2)^)+byte((PreScanLine+2)^)) div 2) and $ff;
        for i:=3 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+(byte((Target+i-3)^)+byte((PreScanLine+i)^)) div 2) and $ff;
      end;
    4:begin
        byte(Target^):=(byte((Scanline)^)+Paeth(0,byte((PreScanLine)^),0)) and $ff;
        byte((Target+1)^):=(byte((ScanLine+1)^)+Paeth(0,byte((PreScanLine+1)^),0)) and $ff;
        byte((Target+2)^):=(byte((ScanLine+2)^)+Paeth(0,byte((PreScanLine+2)^),0)) and $ff;
        for i:=3 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+Paeth(byte((Target+i-3)^),byte((PreScanLine+i)^),byte((PreScanLine+i-3)^))) and $ff;
      end;
  end;
end;

procedure DecodeScanLine32(FilterType:byte;ScanLine,PreScanLine,Target:pointer;ScanLineSize:integer);
var i:integer;
begin
  case FilterType of
    0:move(ScanLine^,Target^,ScanLineSize);
    1:begin
        move(ScanLine^,Target^,4);
        for i:=4 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((Target+i-4)^)) and $ff;
      end;
    2:begin
        for i:=0 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+byte((PreScanLine+i)^)) and $ff;
      end;
    3:begin
        byte(Target^):=((byte((ScanLine)^)+byte(PreScanLine^)) div 2) and $ff;
        byte((Target+1)^):=((byte((ScanLine+1)^)+byte((PreScanLine+1)^)) div 2) and $ff;
        byte((Target+2)^):=((byte((ScanLine+2)^)+byte((PreScanLine+2)^)) div 2) and $ff;
        byte((Target+3)^):=((byte((ScanLine+3)^)+byte((PreScanLine+3)^)) div 2) and $ff;
        for i:=4 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+(byte((Target+i-4)^)+byte((PreScanLine+i)^)) div 2) and $ff;
      end;
    4:begin
        byte(Target^):=(byte((Scanline)^)+Paeth(0,byte((PreScanLine)^),0)) and $ff;
        byte((Target+1)^):=(byte((ScanLine+1)^)+Paeth(0,byte((PreScanLine+1)^),0)) and $ff;
        byte((Target+2)^):=(byte((ScanLine+2)^)+Paeth(0,byte((PreScanLine+2)^),0)) and $ff;
        byte((Target+3)^):=(byte((ScanLine+3)^)+Paeth(0,byte((PreScanLine+3)^),0)) and $ff;
        for i:=4 to ScanLineSize-1 do
          byte((Target+i)^):=(byte((ScanLine+i)^)+Paeth(byte((Target+i-4)^),byte((PreScanLine+i)^),byte((PreScanLine+i-4)^))) and $ff;
      end;
  end;
end;

procedure ReadANIMV1(pSource:TStream;pAnimations:TAnimationDatas);
var cnt:integer;b,b2:byte;atm:TBaseAnimationData;
begin
  cnt:=0;
  pSource.Read(cnt,2);
  b:=0;b2:=0;
  while cnt>0 do begin
    pSource.Read(b,1);
    case b of
      1:atm:=TFrameBasedAnimationData.CreateFromStreamLegacy(pSource);
      2:atm:=TTimeBasedAnimationData.CreateFromStreamV2(pSource);
      3:atm:=TFrameBasedAnimationData.CreateFromStreamV3(pSource);
      4:atm:=TTimeBasedAnimationData.CreateFromStreamV4(pSource);
      5:begin
          pSource.Read(b2,1);
          pSource.Seek(-1,soFromCurrent);
          if b2 and AF_TIMEBASED=0 then
            atm:=TFrameBasedAnimationData.CreateFromStreamV5(pSource)
          else
            atm:=TTimeBasedAnimationData.CreateFromStreamV5(pSource);
        end
      else raise Exception.Create(Format('Unknown animation data version! (%d)',[b]));
    end;
    pAnimations.AddObject(atm.name,atm);
    dec(cnt);
  end;
end;

procedure ReadANIM(pSource:TStream;pAnimations:TAnimationDatas);
var b:Byte;cnt:integer;atm:TBaseAnimationData;
begin
  cnt:=0;
  pSource.Read(cnt,2);
  if cnt>0 then begin  // Legacy format, only frame-based animations
    while cnt>0 do begin
      atm:=TFrameBasedAnimationData.CreateFromStreamLegacy(pSource);
      pAnimations.AddObject(atm.name,atm);
      dec(cnt);
    end;
  end else begin
    b:=0;
    pSource.Read(b,1);
    if b=1 then ReadANIMV1(pSource,pAnimations)
    else raise Exception.Create(Format('Unknown version of ANIM chunk! (%d)',[b]));
  end;
end;

procedure ReadANMZ(pSource:TStream;pAnimations:TAnimationDatas);
var Xs:TMemoryStream;
begin
  Xs:=TMemoryStream.Create;
  try
    pSource.Position:=0;
    UnCompressStream(pSource,Xs);
//    Xs.Position:=0;
//    Xs.SaveToFile('anmz.dat');
    Xs.Position:=0;
    ReadAnim(Xs,pAnimations);
  finally
    FreeAndNil(Xs);
  end;
end;

function ReadFNTZ(pSource:TStream):TFontData;
const ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
var Xs:TMemoryStream;buf:array[0..31] of byte;
  i:integer;x,y,w,h:integer;
begin
  Xs:=TMemoryStream.Create;
  pSource.Position:=0;
  UnCompressStream(pSource,Xs);
  Xs.Position:=0;
  Result:=TFontData.Create;
  buf[0]:=0;
  Xs.Read(buf[0],32);
  x:=0;y:=0;w:=0;h:=0;
  for i:=0 to 255 do
    if buf[i div 8] and ad2[i mod 8]>0 then begin
      Xs.Read(x,2);
      Xs.Read(y,2);
      Xs.Read(w,2);
      Xs.Read(h,2);
      Result.SetCharBox(i,x,y,w,h);
//      Log.LogDebug(Format('Char exists %x, %d, %d, %d, %d',[i,x,y,w,h]));
    end;
  FreeAndNil(Xs);
end;

procedure ProcessColourType0(pSource:TStream;Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  AlphaColourValue:integer;
  FilterType:byte;
  s:string;
  IDAT,ScanLines,ch:TMemoryStream;
  ScanLine,PreScanLine,ReconstScanLine,p:pointer;
  i,j,ScanLineSize:integer;
begin
  FilterType:=0;
  AlphaColourValue:=-1;

  IDAT:=TMemoryStream.Create;
  GetMem(RawData,Width*Height*4);
  ScanLineSize:=Width;
  getmem(ScanLine,ScanLineSize);
  getmem(PreScanLine,ScanLineSize);
  getmem(ReconstScanLine,ScanLineSize);
  FontData:=nil;

  try
    try
      ch:=ReadChunk(pSource,s);
      if s='PLTE' then begin
        FreeAndNil(ch);
        raise Exception.Create('PLTE chunk is not expected with colour mode 0!');
      end;
      if s='tRNS' then begin
        AlphaColourValue:=0;
        ch.Read(AlphaColourValue,2);
        AlphaColourValue:=(AlphaColourValue and $ff00)>>8;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;
      while s='IDAT' do begin
        IDAT.CopyFrom(ch,ch.Size);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      // Read and process Animation or Font data and IEND chunk.
      repeat
        i:=strtoint(decode(s,'anIM,1,anMZ,2,fnTZ,3,IEND,4,0'));
        case i of
          1:ReadANIM(ch,Animations);
          2:ReadANMZ(ch,Animations);
          3:if not(Assigned(FontData)) then
              FontData:=ReadFNTZ(ch)
            else
              raise Exception.Create('Only one font data chunk allowed! (fnTZ)');
          4:break;
          else
            raise Exception.Create('IEND chunk expected, got '+s+'!');
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      until i=4;
      ScanLines:=GetScanLines(IDAT);
      fillchar(PreScanLine^,ScanLineSize,0);
      p:=RawData;
      for j:=0 to Height-1 do begin
        ScanLines.Read(FilterType,1);
        ScanLines.Read(ScanLine^,ScanLineSize);
        DecodeScanLine8(FilterType,ScanLine,PreScanLine,ReconstScanLine,ScanLineSize);
        move(ReconstScanLine^,PreScanLine^,ScanLineSize);
        for i:=0 to Width-1 do begin
          byte(p^):=byte((ReconstScanLine+i)^);
          byte((p+1)^):=byte((ReconstScanLine+i)^);
          byte((p+2)^):=byte((ReconstScanLine+i)^);
          if (AlphaColourValue>-1) and (AlphaColourValue and $ff=byte((ReconstScanLine+i)^)) then
            byte((p+3)^):=0
          else
            byte((p+3)^):=255;
          inc(p,4);
        end;
      end;
    finally
      if Assigned(ch) then FreeAndNil(ch);
      FreeAndNil(IDAT);
      Freemem(ReconstScanLine);
      Freemem(PreScanLine);
      Freemem(ScanLine);
      if Assigned(ScanLines) then FreeAndNil(ScanLines);
    end;
  except
    on exception do begin
      Animations.Clear;
      if Assigned(FontData) then FreeAndNil(FontData);
      Freemem(RawData);
      raise;
    end;
  end;
end;

procedure ProcessColourType2(pSource:TStream;Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  AlphaColourValue:integer;
  FilterType:byte;
  s:string;
  IDAT,ScanLines,ch:TMemoryStream;
  ScanLine,PreScanLine,ReconstScanLine,p:pointer;
  i,j,ScanLineSize:integer;
begin
  FilterType:=0;
  AlphaColourValue:=-1;
  IDAT:=TMemoryStream.Create;
  GetMem(RawData,Width*Height*4);
  ScanLineSize:=Width*3;
  getmem(ScanLine,ScanLineSize);
  getmem(PreScanLine,ScanLineSize);
  getmem(ReconstScanLine,ScanLineSize);
  FontData:=nil;

  try
    try
      ch:=ReadChunk(pSource,s);
      if s='PLTE' then begin  // in true colour mode PLTE is optional and not used by us.
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;
      if s='tRNS' then begin
        AlphaColourValue:=0;
        i:=0;
        ch.Read(i,2);
        AlphaColourValue:=(i and $ff)<<16;
        ch.Read(i,2);
        AlphaColourValue+=(i and $ff)<<8;
        ch.Read(i,2);
        AlphaColourValue+=(i and $ff);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;
      while s='IDAT' do begin
        IDAT.CopyFrom(ch,ch.Size);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;
      // Read and process Animation or Font data and IEND chunk.
      repeat
        i:=strtoint(decode(s,'anIM,1,anMZ,2,fnTZ,3,IEND,4,0'));
        case i of
          1:ReadANIM(ch,Animations);
          2:ReadANMZ(ch,Animations);
          3:if not(Assigned(FontData)) then
              FontData:=ReadFNTZ(ch)
            else
              raise Exception.Create('Only one font data chunk allowed! (fnTZ)');
          4:break;
          else
            raise Exception.Create('IEND chunk expected, got '+s+'!');
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      until i=4;

      ScanLines:=GetScanLines(IDAT);

      fillchar(PreScanLine^,ScanLineSize,0);
      p:=RawData;
      for j:=0 to Height-1 do begin
        ScanLines.Read(FilterType,1);
        ScanLines.Read(ScanLine^,ScanLineSize);
//        Log.Trace(Format('%d. %d',[j,FilterType]));
        DecodeScanLine24(FilterType,ScanLine,PreScanLine,ReconstScanLine,ScanLineSize);
        move(ReconstScanLine^,PreScanLine^,ScanLineSize);
        for i:=0 to Width-1 do begin
          byte(p^):=byte((ReconstScanLine+i*3+2)^);
          byte((p+1)^):=byte((ReconstScanLine+i*3+1)^);
          byte((p+2)^):=byte((ReconstScanLine+i*3)^);
          if (AlphaColourValue>-1) and
             (AlphaColourValue and $ff=byte((p^))) and
             ((AlphaColourValue and $ff00)>>8=byte((p+1)^)) and
             ((AlphaColourValue and $ff0000)>>16=byte((p+2)^)) then
            byte((p+3)^):=0
          else
            byte((p+3)^):=255;
          inc(p,4);
        end;
      end;

    finally
      if Assigned(ch) then FreeAndNil(ch);
      FreeAndNil(IDAT);
      Freemem(ReconstScanLine);
      FreeMem(PreScanLine);
      Freemem(ScanLine);
      if Assigned(ScanLines) then FreeAndNil(ScanLines);
    end;
  except
    on exception do begin
      Animations.Clear;
      if Assigned(FontData) then FreeAndNil(FontData);
      Freemem(RawData);
      raise;
    end;
  end;
end;

procedure ProcessColourType3(pSource:TStream;Width,Height:integer;BitDepth:byte;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  FilterType:byte;
  s:string;
  IDAT,ScanLines,ch:TMemoryStream;
  ScanLine,PreScanLine,ReconstScanLine,Palette,p:pointer;
  i,j,ScanLineSize,PalCount:integer;
  b:byte;
begin
  FilterType:=0;
  ch:=ReadChunk(pSource,s);
  Getmem(RawData,Width*Height*4);
  Getmem(Palette,1024);
  ScanLineSize:=(Width*BitDepth+7) div 8;
  Getmem(ScanLine,ScanLineSize);
  Getmem(PreScanLine,ScanLineSize);
  Getmem(ReconstScanLine,ScanLineSize);
  IDAT:=TMemoryStream.Create;
  FontData:=nil;
  try
    try
      if s='PLTE' then begin
        if ch.Size mod 3<>0 then
          raise Exception.Create('PLTE chunk size not divisible by 3!');
        if ch.Size>768 then
          raise Exception.Create('PLTE chunk size too big!');
        PalCount:=ch.Size div 3;
        for i:=0 to PalCount-1 do begin
          ch.Read((Palette+i*4+2)^,1);
          ch.Read((Palette+i*4+1)^,1);
          ch.Read((Palette+i*4+0)^,1);
          byte((Palette+i*4+3)^):=255;
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      if s='tRNS' then begin
        if ch.Size>PalCount then
          raise Exception.Create('tRNS chunk size too big!');
        for i:=0 to ch.Size-1 do
          ch.Read((Palette+i*4+3)^,1);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      while s='IDAT' do begin
        IDAT.CopyFrom(ch,ch.Size);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      // Read and process Animation or Font data and IEND chunk.
      repeat
        i:=strtoint(decode(s,'anIM,1,anMZ,2,fnTZ,3,IEND,4,0'));
        case i of
          1:ReadANIM(ch,Animations);
          2:ReadANMZ(ch,Animations);
          3:if not(Assigned(FontData)) then
              FontData:=ReadFNTZ(ch)
            else
              raise Exception.Create('Only one font data chunk allowed! (fnTZ)');
          4:break;
          else
            raise Exception.Create('IEND chunk expected, got '+s+'!');
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      until i=4;

      ScanLines:=GetScanLines(IDAT);
      fillchar(PreScanLine^,ScanLineSize,0);
      p:=RawData;
      for j:=0 to Height-1 do begin
        ScanLines.Read(FilterType,1);
        ScanLines.Read(ScanLine^,ScanLineSize);
        DecodeScanLine8(FilterType,ScanLine,PreScanLine,ReconstScanLine,ScanLineSize);
        move(ReconstScanLine^,PreScanLine^,ScanLineSize);
        case BitDepth of
          1:begin
              for i:=0 to Width-1 do begin
                if i mod 8=0 then b:=byte((ReconstScanLine+i div 8)^);
                uint32(p^):=uint32((Palette+((b and $80)>>7)*4)^);
                b:=(b and $7f)<<1;
                inc(p,4);
              end;
            end;
          2:begin
              for i:=0 to Width-1 do begin
                if i mod 4=0 then b:=byte((ReconstScanLine+i div 4)^);
                uint32(p^):=uint32((Palette+((b and $C0)>>6)*4)^);
                b:=(b and $3f)<<2;
                inc(p,4);
              end;
            end;
          4:begin
              for i:=0 to Width-1 do begin
                if i mod 2=0 then b:=byte((ReconstScanLine+i div 2)^);
                uint32(p^):=uint32((Palette+(b and $F0)>>2)^);
                b:=(b and $0f)<<4;
                inc(p,4);
              end;
            end;
          8:begin
              for i:=0 to Width-1 do begin
                b:=byte((ReconstScanLine+i)^);
                uint32(p^):=uint32((Palette+b*4)^);
                inc(p,4);
              end;
            end;
        end;
      end;
    finally
      if Assigned(ch) then FreeAndNil(ch);
      FreeAndNil(IDAT);
      Freemem(Palette);
      Freemem(ReconstScanLine);
      FreeMem(PreScanLine);
      Freemem(ScanLine);
      if Assigned(ScanLines) then FreeAndNil(ScanLines);
    end;
  except
    on exception do begin
      Animations.Clear;
      if Assigned(FontData) then FreeAndNil(FontData);
      Freemem(RawData);
      raise;
    end;
  end;
end;

procedure ProcessColourType4(pSource:TStream;Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  FilterType:byte;
  s:string;
  IDAT,ScanLines,ch:TMemoryStream;
  ScanLine,PreScanLine,ReconstScanLine,p:pointer;
  i,j,ScanLineSize:integer;
begin
  IDAT:=TMemoryStream.Create;
  FilterType:=0;
  GetMem(RawData,Width*Height*4);
  ScanLineSize:=Width*2;
  getmem(ScanLine,ScanLineSize);
  getmem(PreScanLine,ScanLineSize);
  getmem(ReconstScanLine,ScanLineSize);
  FontData:=nil;

  try
    try
      ch:=ReadChunk(pSource,s);
      if s='PLTE' then
        raise Exception.Create('PLTE chunk is not expected with colour mode 4!');
      if s='tRNS' then
        raise Exception.Create('tRNS chunk is not expected with colour mode 4!');
      while s='IDAT' do begin
        IDAT.CopyFrom(ch,ch.Size);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      // Read and process Animation or Font data and IEND chunk.
      repeat
        i:=strtoint(decode(s,'anIM,1,anMZ,2,fnTZ,3,IEND,4,0'));
        case i of
          1:ReadANIM(ch,Animations);
          2:ReadANMZ(ch,Animations);
          3:if not(Assigned(FontData)) then
              FontData:=ReadFNTZ(ch)
            else
              raise Exception.Create('Only one font data chunk allowed! (fnTZ)');
          4:break;
          else
            raise Exception.Create('IEND chunk expected, got '+s+'!');
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      until i=4;

      ScanLines:=GetScanLines(IDAT);
      fillchar(PreScanLine^,ScanLineSize,0);
      p:=RawData;
      for j:=0 to Height-1 do begin
        ScanLines.Read(FilterType,1);
        ScanLines.Read(ScanLine^,ScanLineSize);
        DecodeScanLine16(FilterType,ScanLine,PreScanLine,ReconstScanLine,ScanLineSize);
        move(ReconstScanLine^,PreScanLine^,ScanLineSize);
        for i:=0 to Width-1 do begin
          byte(p^):=byte((ReconstScanLine+i*2)^);
          byte((p+1)^):=byte((ReconstScanLine+i*2)^);
          byte((p+2)^):=byte((ReconstScanLine+i*2)^);
          byte((p+3)^):=byte((ReconstScanLine+i*2+1)^);
          inc(p,4);
        end;
      end;

    finally
      FreeAndNil(IDAT);
      Freemem(ReconstScanLine);
      FreeMem(PreScanLine);
      Freemem(ScanLine);
      if Assigned(ScanLines) then FreeAndNil(ScanLines);
      if Assigned(ch) then FreeAndNil(ch);
    end;

  except
    on exception do begin
      Animations.Clear;
      if Assigned(FontData) then FreeAndNil(FontData);
      Freemem(RawData);
      raise;
    end;
  end;
end;

procedure ProcessColourType6(pSource:TStream;Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  FilterType:byte;
  s:string;
  IDAT,ScanLines,ch:TMemoryStream;
  ScanLine,PreScanLine,ReconstScanLine,p:pointer;
  i,j,ScanLineSize:integer;
begin
  IDAT:=TMemoryStream.Create;
  FilterType:=0;
  GetMem(RawData,Width*Height*4);
  ScanLineSize:=Width*4;
  getmem(ScanLine,ScanLineSize);
  getmem(PreScanLine,ScanLineSize);
  getmem(ReconstScanLine,ScanLineSize);
  FontData:=nil;

  try
    try
      ch:=ReadChunk(pSource,s);
      if s='PLTE' then begin  // in true colour mode PLTE is optional and not used by us.
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;
      if s='tRNS' then
        raise Exception.Create('tRNS chunk is not expected with colour mode 6!');
      while s='IDAT' do begin
        IDAT.CopyFrom(ch,ch.Size);
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      end;

      // Read and process Animation or Font data and IEND chunk.
      repeat
        i:=strtoint(decode(s,'anIM,1,anMZ,2,fnTZ,3,IEND,4,0'));
        case i of
          1:ReadANIM(ch,Animations);
          2:ReadANMZ(ch,Animations);
          3:if not(Assigned(FontData)) then
              FontData:=ReadFNTZ(ch)
            else
              raise Exception.Create('Only one font data chunk allowed! (fnTZ)');
          4:break;
          else
            raise Exception.Create('IEND chunk expected, got '+s+'!');
        end;
        FreeAndNil(ch);
        ch:=ReadChunk(pSource,s);
      until i=4;

      ScanLines:=GetScanLines(IDAT);
      fillchar(PreScanLine^,ScanLineSize,0);
      p:=RawData;
      for j:=0 to Height-1 do begin
        ScanLines.Read(FilterType,1);
        ScanLines.Read(ScanLine^,ScanLineSize);
        DecodeScanLine32(FilterType,ScanLine,PreScanLine,ReconstScanLine,ScanLineSize);
        move(ReconstScanLine^,PreScanLine^,ScanLineSize);
        for i:=0 to Width-1 do begin
          byte(p^):=byte((ReconstScanLine+i*4+2)^);
          byte((p+1)^):=byte((ReconstScanLine+i*4+1)^);
          byte((p+2)^):=byte((ReconstScanLine+i*4)^);
          byte((p+3)^):=byte((ReconstScanLine+i*4+3)^);
          inc(p,4);
        end;
      end;

    finally
      FreeAndNil(IDAT);
      Freemem(ReconstScanLine);
      FreeMem(PreScanLine);
      Freemem(ScanLine);
      if Assigned(ScanLines) then FreeAndNil(ScanLines);
      if Assigned(ch) then FreeAndNil(ch);
    end;

  except
    on exception do begin
      Animations.Clear;
      if Assigned(FontData) then FreeAndNil(FontData);
      Freemem(RawData);
      raise;
    end;
  end;
end;

procedure ReadPNG(pSource:TStream;out Width,Height:integer;out RawData:pointer;Animations:TAnimationDatas;out FontData:TFontData);
var
  s:string;
  ch:TMemoryStream;
  BitDepth,ColourType,CompressionMethod,FilterMethod,InterlaceMethod:byte;
begin
//  Log.Trace(Paeth(132,144,140));
  s:=#0#0#0#0#0#0#0#0;
  pSource.Read(s[1],8);
  if s<>Header then
    raise Exception.Create('Not a PNG file!');
  ch:=ReadChunk(pSource,s,false);
  try
    if s<>'IHDR' then
      raise Exception.Create('IHDR chunk should be first!');
    Width:=ReadNBO4(ch);
    Height:=ReadNBO4(ch);
    BitDepth:=0;
    ch.Read(BitDepth,1);
    if not (BitDepth in [1,2,4,8,16]) then
      raise Exception.Create(Format('Invalid BitDepth value! (%d)',[BitDepth]));
    if BitDepth=16 then
      raise Exception.Create('BitDepth value of 16 is not supported!');
    ColourType:=0;
    ch.Read(ColourType,1);
//    Log.Trace(ColourType);
    if not (ColourType in [0,2,3,4,6]) then
      raise Exception.Create(Format('Invalid ColourType value! (%d)',[ColourType]));
    CompressionMethod:=0;
    ch.Read(CompressionMethod,1);
    if CompressionMethod<>0 then
      raise Exception.Create(Format('Invalid CompressionMethod value! (%d)',[CompressionMethod]));
    FilterMethod:=0;
    ch.Read(FilterMethod,1);
    if FilterMethod<>0 then
      raise Exception.Create(Format('Invalid FilterMethod value! (%d)',[FilterMethod]));
    InterlaceMethod:=0;
    ch.Read(InterlaceMethod,1);
    if InterlaceMethod<>0 then
      raise Exception.Create(Format('Invalid InterlaceMethod value! (%d)',[InterlaceMethod]));
  finally
    FreeAndNil(ch);
  end;
  case ColourType of
    0:ProcessColourType0(pSource,Width,Height,RawData,Animations,FontData);
    2:ProcessColourType2(pSource,Width,Height,RawData,Animations,FontData);
    3:ProcessColourType3(pSource,Width,Height,BitDepth,RawData,Animations,FontData);
    4:ProcessColourType4(pSource,Width,Height,RawData,Animations,FontData);
    6:ProcessColourType6(pSource,Width,Height,RawData,Animations,FontData);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('PNG',@ReadPNG,true);

end.

