// PNG writer for TARGBImage
// ------------------------------------------------------------------
// You can freely distribute the sources
//
// Written by Gilby/MKSZTSZ
// Hungary, 2020-2024
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2020.03.11
//     * Initial creation from ARGBImagePNGUnit
//   1.01 - Gilby - 2020.03.16-30
//     * Following changes in ARGBImageUnit
//     + Writing animation chunks (anIM, anMZ)
//     + Writing fontdata chunks (fnTZ)
//   1.02 - Gilby - 2020.04.01-06
//     * Following changes in ARGBImageUnit (TAnimationData -> TAnimationDatas)
//   1.03 - Gilby - 2020.05.28
//     * Bugfix with writing anMZ chunk
//   1.04 - Gilby - 2020.06.25
//     * Following changes in TAnimData
//   1.05 - Gilby - 2022.07.19
//     + Added ReverseAnim flag
//   1.06 - Gilby - 2023.02.12
//     * Bugfix with writing colormode 3 images with odd width and 4, 2 or 1 bitdepth.
//   1.07 - Gilby - 2023.06.28
//     * Bugfix with writing unnamed animations.
//   1.08 - Gilby - 2023.07.17
//     * Bugfix with writing colormode 3 images with odd width and 4, 2 or 1 bitdepth.
//   1.09 - Gilby - 2023.12.14
//     * Following changes in AnimationDataUnit.
//     * Added new format for ANIM and ANMZ chunks.
//   1.10 - Gilby - 2024.08.16
//     * Following changes in FontDataUnit.
//   1.11 - Gilby - 2024.12.04
//     * Removing unused variables.

unit ARGBImagePNGWriterUnit;

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

interface

{$ifdef Debug}
var
  LastColourType,LastAlphaMode,LastBitDepth:integer;
  LastGreyscaled:boolean;
{$endif}

implementation

uses Classes, SysUtils, ARGBImageUnit, CRC32Unit, FastPaletteUnit,
  MyZStreamUnit, Logger, AnimationDataUnit, FontDataUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='1.10';

  HEADER=#137#80#78#71#13#10#26#10;
  IHDR:uint32=$52444849; //  #82#68#72#73;
  PLTE:uint32=$45544c50; // #69#84#76#80;
  tRNS:uint32=$534e5274; // #83#78#82#116;
  IDAT:uint32=$54414449; // #84#65#68#73;
  IEND:uint32=$444e4549; // #68#78#69#73;
  anIM:uint32=$4d496e61; // #77#73#110#97;
  anMZ:uint32=$5a4d6e61; // #90#73#110#97;
//  foNT:uint32=$544e6f66; // #84#78#111#102;
  fnTZ:uint32=$5a546e66; // #90#84#110#102;

// Greyscale and True colour
// 0-all opaque, 1-colourkey (1 fully transparent, rest opaque), 2-full alpha data
function ScanAlpha(pRawData:pointer;pPixelCount:integer;out AlphaPixelIndex:integer):integer;
var p:pointer;i:integer;AlphaColour:uint32;
begin
  p:=pRawData;
  AlphaPixelIndex:=-1;
  AlphaColour:=$55555555;
  for i:=0 to pPixelCount-1 do begin
    if byte((p+3)^)=0 then begin
      if AlphaPixelIndex=-1 then begin
        AlphaPixelIndex:=i;
        AlphaColour:=uint32(p^);
      end else
      if AlphaColour<>uint32(p^) then begin
        Result:=2;
        exit;
      end;
    end
    else if byte((p+3)^)<>255 then begin
      Result:=2;
      exit;
    end
    else if (AlphaPixelIndex<>-1) and (AlphaColour and $ffffff=uint32(p^) and $ffffff) then begin
      Result:=2;
      exit;
    end;
    inc(p,4);
  end;
  if AlphaPixelIndex=-1 then Result:=0 else Result:=1;
end;

procedure SwapColours(palettedimage,pal:pointer;pPixelcount,index1,index2:uint32);
var i:uint32;p:pointer;
begin
  p:=palettedimage;
  for i:=0 to pPixelcount-1 do begin
    if byte(p^)=index1 then byte(p^):=index2
    else if byte(p^)=index2 then byte(p^):=index1;
    inc(p);
  end;
  i:=uint32((pal+index1*4)^);
  uint32((pal+index1*4)^):=uint32((pal+index2*4)^);
  uint32((pal+index2*4)^):=i;
end;

procedure WriteNBO4(pTarget:TStream;pValue:uint32);
var i:uint32;
begin
  i:=(pValue and $ff000000)>>24 +
     (pValue and $ff0000)>>8 +
     (pValue and $ff00)<<8 +
     (pValue and $ff)<<24;
  pTarget.Write(i,4);
end;

procedure AddChunk(pTarget,pSource:TStream);
var i:uint32;
begin
  WriteNBO4(pTarget,pSource.Size-4);
  pSource.Position:=0;
//  Log.DumpStream(pSource,0,pSource.Size,'','');
  pTarget.CopyFrom(pSource,pSource.Size);
  i:=CRC32Stream(pSource);
  WriteNBO4(pTarget,i);
end;

procedure WriteAnimations(pTarget:TStream;pAnimations:TAnimationDatas);
var Xs,ch:TMemoryStream;i,j:integer;b:byte;
begin
  Xs:=TMemoryStream.Create;
  ch:=TMemoryStream.Create;
  try
    j:=0;
    Xs.Write(j,2);  // To indicate new format animation data
    b:=1;
    Xs.Write(b,1);
    j:=pAnimations.Count;
    Xs.Write(j,2);
    for i:=0 to pAnimations.Count-1 do pAnimations[i].SavetoStream(Xs);
    Xs.Position:=0;
    ch.Write(anMZ,4);
    CompressStream(Xs,ch,Xs.Size);
    if ch.Size>=Xs.Size then begin
      ch.Clear;
      ch.Write(anIM,4);
      Xs.Position:=0;
      ch.CopyFrom(Xs,Xs.Size);
    end;
    AddChunk(pTarget,ch);
  finally
    ch.Free;
    Xs.Free;
  end;
end;

procedure WriteFontData(pTarget:TStream;pFontData:TFontData);
const ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
var ch:TMemoryStream;p:pointer;i,j:integer;
begin
  ch:=TMemoryStream.Create;
  Getmem(p,32+256*8);

  try
    ch.Write(fnTZ,4);
    for i:=0 to 31 do byte((p+i)^):=0;
    j:=0;
    for i:=0 to 255 do
      if pFontData.Charboxes[i].Width>0 then begin
        byte((p+i div 8)^):=byte((p+i div 8)^) or ad2[i mod 8];
        word((p+32+j*8+0)^):=pFontData.CharBoxes[i].Left;
        word((p+32+j*8+2)^):=pFontData.CharBoxes[i].Top;
        word((p+32+j*8+4)^):=pFontData.CharBoxes[i].Width;
        word((p+32+j*8+6)^):=pFontData.CharBoxes[i].Height;
        inc(j);
      end;
    compress(p^,ch,32+j*8);
    AddChunk(pTarget,ch);
  finally
    Freemem(p);
    FreeAndNil(ch);
  end;
end;

procedure WritePNG(pTarget:TStream;pWidth,pHeight:integer;pRawData:pointer;pAnimations:TAnimationDatas;pFontData:TFontData);
var
  i,j,bitdepth:integer;
  b:byte;
  pp,pr:pointer;
  ch:TMemoryStream;
  pal,palettedimage,scanlines:pointer;
  scanlinesize,palcount:integer;
  greyscaled:boolean;
  AlphaMode,AlphaPixelIndex:integer;
  ColourType:integer;

begin
  pTarget.Write(HEADER[1],8);
  ch:=TMemoryStream.Create;
  GetPalettedImage32(pWidth, pHeight, pRawData, palettedimage, pal, palcount, greyscaled);
//  palcount:=256;

  // IHDR
  ch.Write(IHDR,4);
  WriteNBO4(ch,pWidth);
  WriteNBO4(ch,pHeight);
  if (palettedimage=nil) or greyscaled then bitdepth:=8
  else if palcount>16 then bitdepth:=8
  else if palcount>4 then bitdepth:=4
  else if palcount>2 then bitdepth:=2
  else bitdepth:=1;
  ch.Write(bitdepth,1);
  if greyscaled then begin
    ColourType:=0;
    AlphaMode:=ScanAlpha(pRawData,pWidth*pHeight,AlphaPixelIndex);
    if AlphaMode=2 then ColourType:=4;
  end
  else if palettedimage=nil then begin
    ColourType:=2;
    AlphaMode:=ScanAlpha(pRawData,pWidth*pHeight,AlphaPixelIndex);
    if AlphaMode=2 then ColourType:=6;
  end else begin
    ColourType:=3;
    AlphaMode:=ScanAlpha(pal,palcount,AlphaPixelIndex);
    if AlphaMode=1 then SwapColours(palettedimage,pal,pWidth*pHeight,AlphaPixelindex,0);
  end;
  ch.Write(ColourType,4);
{  i:=0; ch.Write(i,3);}
  AddChunk(pTarget,ch);

{$ifdef debug}
  Log.LogDebug(Format('Got:      ColourType=%d, AlphaMode=%d, BitDepth=%d, Greyscaled=%s',[ColourType,AlphaMode,bitdepth,BoolToStr(greyscaled,true)]));
  LastColourType:=ColourType;
  LastAlphaMode:=AlphaMode;
  LastBitDepth:=bitdepth;
  LastGreyscaled:=greyscaled;
{$endif}

  case ColourType of
    0:begin
        if AlphaMode=1 then begin
          ch.Clear;
          ch.Write(tRNS,4);
          i:=256*byte((pRawData+AlphaPixelIndex*4)^);
          ch.Write(i,2);
          AddChunk(pTarget,ch);
//          ch.SaveToFile('trns.dat');
        end;

//        Log.DumpMemory(palettedimage^,0,pWidth*pHeight,'','');
        scanlinesize:=pWidth;
        getmem(scanlines,(scanlinesize+1)*pHeight);
        pp:=pRawdata;
        pr:=scanlines;
        for j:=0 to pHeight-1 do begin
          byte(pr^):=0;
          inc(pr);
          for i:=0 to pWidth-1 do begin
            byte(pr^):=byte(pp^);
            inc(pr);
            inc(pp,4);
          end;
        end;
//        Log.DumpMemory(Scanlines^,0,(scanlinesize+1)*pHeight,'','');

      end;
    2:begin
        if AlphaMode=1 then begin
          ch.Clear;
          ch.Write(tRNS,4);
          i:=0;
          i:=byte((pRawData+AlphaPixelIndex*4+2)^);
          ch.Write(i,2);
          i:=byte((pRawData+AlphaPixelIndex*4+1)^);
          ch.Write(i,2);
          i:=byte((pRawData+AlphaPixelIndex*4)^);
          ch.Write(i,2);
          AddChunk(pTarget,ch);
//          ch.SaveToFile('trns.dat');
        end;

        scanlinesize:=pWidth*3;
        getmem(scanlines,(scanlinesize+1)*pHeight);
        pp:=pRawData;
        pr:=scanlines;
        for j:=0 to pHeight-1 do begin
          byte(pr^):=0;
          inc(pr);
          for i:=0 to pWidth-1 do begin
            byte(pr^):=byte((pp+2)^);
            byte((pr+1)^):=byte((pp+1)^);
            byte((pr+2)^):=byte(pp^);
            pp+=4;pr+=3;
          end;
        end;
      end;
    3:begin
        ch.Clear;
        ch.Write(PLTE,4);
        for i:=0 to palcount-1 do begin
          ch.Write((pal+i*4+2)^,1);
          ch.Write((pal+i*4+1)^,1);
          ch.Write((pal+i*4+0)^,1);
        end;
        AddChunk(pTarget,ch);
//        ch.SaveToFile('pale.dat');

        case AlphaMode of
          1:begin
              ch.Clear;
              ch.Write(tRNS,4);
              i:=0;
              ch.Write(i,1);
              AddChunk(pTarget,ch);
//              ch.SaveToFile('trns.dat');
            end;
          2:begin
              ch.Clear;
              ch.Write(tRNS,4);
              for i:=0 to palcount-1 do
                ch.Write((pal+i*4+3)^,1);
              AddChunk(pTarget,ch);
//              ch.SaveToFile('trns.dat');
            end;
        end;
//        Log.DumpMemory(palettedimage^,0,pWidth*pHeight,'','');

        scanlinesize:=(pWidth-1) div (8 div bitdepth)+1;
        getmem(scanlines,(scanlinesize+1)*pHeight);
        pp:=palettedimage;
        pr:=scanlines;
        for j:=0 to pHeight-1 do begin
          byte(pr^):=0;
          inc(pr);
          if bitdepth=8 then begin
            move(pp^,pr^,pWidth);
            pp+=pWidth;
            pr+=pWidth;
          end else begin
            b:=0;
            for i:=0 to pWidth-1 do begin
              case bitdepth of
                1:b:=b or (byte(pp^) and 1);
                2:b:=b or (byte(pp^) and 3);
                4:b:=b or (byte(pp^) and $f);
              end;
              inc(pp);
              if i mod (8 div bitdepth)=8 div bitdepth-1 then begin
                byte(pr^):=b;
                inc(pr);
                b:=0;
              end;
              b:=(b<<bitdepth) and $ff;
            end;
            if pWidth mod (8 div bitdepth)<>0 then begin
              b:=(b<<(bitdepth*((8 div bitdepth)-(pWidth mod (8 div bitdepth))-1))) and $ff;
              byte(pr^):=b;
              inc(pr);
            end;
          end;
        end;
//        Log.DumpMemory(Scanlines^,0,(scanlinesize+1)*pHeight,'','');
      end;
    4:begin
        scanlinesize:=pWidth*2;
        getmem(scanlines,(scanlinesize+1)*pHeight);
        pp:=pRawdata;
        pr:=scanlines;
        for j:=0 to pHeight-1 do begin
          byte(pr^):=0;
          inc(pr);
          for i:=0 to pWidth-1 do begin
            byte(pr^):=byte(pp^);
            byte((pr+1)^):=byte((pp+3)^);
            inc(pr,2);
            inc(pp,4);
          end;
        end;
      end;
    6:begin
        scanlinesize:=pWidth*4;
        getmem(scanlines,(scanlinesize+1)*pHeight);
        pp:=pRawData;
        pr:=scanlines;
        for j:=0 to pHeight-1 do begin
          byte(pr^):=0;
          inc(pr);
          for i:=0 to pWidth-1 do begin
            byte(pr^):=byte((pp+2)^);
            byte((pr+1)^):=byte((pp+1)^);
            byte((pr+2)^):=byte(pp^);
            byte((pr+3)^):=byte((pp+3)^);
            pp+=4;pr+=4;
          end;
        end;
      end;
  end;

  if palettedimage<>nil then begin   // Paletted
    Freemem(pal,1024);
    Freemem(palettedimage,pWidth*pHeight);
  end;

  ch.Clear;
  ch.Write(IDAT,4);
  compress(scanlines^,ch,(scanlinesize+1)*pHeight);
  Freemem(scanlines,(scanlinesize+1)*pHeight);
  AddChunk(pTarget,ch);

  if pAnimations.Count>0 then WriteAnimations(pTarget,pAnimations);
  if Assigned(pFontData) then WriteFontData(pTarget,pFontData);

  ch.Clear;
  ch.Write(IEND,4);
  AddChunk(pTarget,ch);
  FreeAndNil(ch);

  pTarget.Position:=0;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageWriter('PNG',@WritePNG);

end.

