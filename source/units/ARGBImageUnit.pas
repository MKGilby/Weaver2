// MKSZTSZ ARGB image class
// ------------------------------------------------------------------
// ARGB image to use with SDL2
// ------------------------------------------------------------------
// You can freely distribute the sources.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2017-2024
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2017.03.07
//     * Initial creation from RawPicture
//     * New ARGB version for SDL2
//     * Pixel order p^     = b
//                   (p+1)^ = g
//                   (p+2)^ = r
//                   (p+3)^ = a
//   1.01 - Gilby - 2017.03.23
//     * FIX: ReadTGA with paletted image
//     * FIX: WriteTGA with TrueColor image
//     * FIX: ReadCEL (garbaged palette)
//     * FIX: ReadBMP (garbaged palette)
//   1.02 - Gilby - 2017.05.25
//     * ResizeN added
//     * ReplaceColor added
//   1.03 - Gilby - 2020.02.07
//     * Range check error fix in RecolorRGB
//     * Range check error fix in ReadTGA
//   1.04 - Gilby - 2020.02.09-11
//     * Writers and readers are in separated units, they register themselves
//     + ReadFile from stream added
//     + AddPadding added
//     + SetColorkey added. Sets the specified color to transparent, all others to opaque
//     - Removed unused units
//     * BUG in ResizeN
//   1.05 - Gilby - 2020.02.14-17
//     + Bar added (with clipping)
//   1.06 - Gilby - 2020.02.20
//     + Crop(r,g,b,a) added. It crops the image to the smallest size possible
//       containing all pixels not equal the given color.
//   1.07 - Gilby - 2020.03.04
//     * Clear now clears to opaque black.
//     + VLine added (with clipping)
//     + HLine added (with clipping)
//     + Rectangle added (with clipping)
//     * Optimized bar when all channels have values
//   1.08 - Gilby - 2020.03.05
//     + Create(iFilename:string) added
//     * Fix in ReadFile when memory was already allocated
//   1.09 - Gilby - 2020.03.16
//     * TExtra record replaced with TAnimationData and TFontData classes.
//   1.10 - Gilby - 2020.04.01
//     * TAnimationData replaced with TAnimationDatas as PNG can store
//       multiple animations.
//   1.11 - Gilby - 2020.04.27
//     * Copy only frees target image if target image exists.
//     * CopyTo checks if target assigned and clips width and height with
//       both source and target.
//   1.12 - Gilby - 2021.02.18
//     * Copy and CopyTo now has a new boolean parameter: useColorkey.
//       If it is true, the pixels having zero alpha value will not be copied.
//       (Default is false.)
//   1.12a - Gilby - 2021.03.12
//     * BUGFIX: Clipping check in HLine. (y=0 was considered invalid.)
//   1.13 - Gilby - 2021.05.02
//     * Copy<R|G|B>ToAlpha added.
//   1.13a - Gilby - 2021.05.17
//     * BUGFIX: Clipping check in VLine. (x=0 was considered invalid.)
//   1.14 - Gilby - 2021.06.06
//     + UseAlpha:boolean property added.
//       Sets if alpha is used in supported methods.
//       (Currently copy and copyto)
//   1.15 - Gilby - 2021.11.09
//     * + GetPixel added.
//   1.16 - Gilby - 2022.03.16
//     * Copy no longer creates new ARGBImage if target is nil.
//   1.17 - Gilby - 2022.04.27
//     + IsIdentical:boolean added.
//   1.18 - Gilby - 2022.05.13
//     + FillImage added.
//   1.19 - Gilby - 2022.06.23
//     + Line added.
//   1.20 - Gilby - 2022.08.30
//     * Rotate(2) was not implemented. Added it now.
//   1.21 - Gilby - 2022.10.27
//     + HalveImage(n:integer) added. It shrinks the image to 2^-n
//     * Changed freemems to sizeless versions.
//   1.22 - Gilby - 2023.01.13-16
//     + Added Bar with uint32 color parameter.
//     + Added PutPixel with uint32 color parameter.
//     + Added HLine with uint32 color parameter.
//     + Added VLine with uint32 color parameter.
//     + Added Rectangle with uint32 color parameter.
//   1.22a - Gilby - 2023.01.25
//     * Tidying code.
//   1.23 - Gilby - 2023.07.31-08.02
//     * Changed dwords to uint32.
//     + Added Line with uint32 color parameter.
//     + Added Circle with uint32 color parameter.
//     + Added FilledCircle with uint32 color parameter.
//     + Added FloodFill with uint32 color parameter.
//     + Added PutImage with uint32 color parameter.
//     + Added PutImagePart with uint32 color parameter.
//     * Renamed ResizeN to Magnify.
//   1.24 - Gilby - 2023.08.10
//     * Fixed FlipV.
//   1.25 - Gilby - 2023.11.02
//     * Fixed line. All parameters was uint32, but only color should be.
//   1.26 - Gilby - 2023.11.20
//     * Clear now can have a color parameter.
//     * Added clipping to PutPixel.
//     * The split color Line now calls the uint32 color version.
//     * Simplified FilledCircle.
//   1.27 - Gilby - 2023.12.05
//     + Added Resize, it changes changes the image resolution but keeps the image
//       either cut to size (shrinking) or padded with empty space (enlarging).
//   1.28 - Gilby - 2024.04.24
//     * Changed MKToolBox.Replace to SysUtils.StringReplace.
//   1.29 - Gilby - 2024.08.28
//     + Added even parameter to Circle and FilledCircle.
//       If it is set to true, circle will be 1 pixel wider and taller to occupy
//       an even*even area.
//   1.30 - Gilby - 2024.09.13
//     * Bugfix in GetClipBox.
//     * GetClipBox is now a class static function.
//   1.31 - Gilby - 2024.09.18
//     * Bugfix in WeightedMatrix.


{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit ARGBImageUnit;

interface

uses Classes, Lists, AnimationDataUnit, FontDataUnit;

type
  TARGBImageFileReaderProc=procedure(pSource:TStream;out Width,Height:integer;out RawData:pointer;AnimationData:TAnimationDatas;out FontData:TFontData);
  TARGBImageFileWriterProc=procedure(pTarget:TStream;pWidth,pHeight:integer;pRawData:pointer;pAnimationData:TAnimationDatas;pFontData:TFontData);

  TARGBImageReaderItem=class
    constructor Create(iType:string;iProc:TARGBImageFileReaderProc;iAffectsImage:boolean);
  private
    fType:string;
    fProc:TARGBImageFileReaderProc;
    fAffectsImage:boolean;
  public
    property Proc:TARGBImageFileReaderProc read fProc;
    property AffectsImage:boolean read fAffectsImage;
  end;

  TARGBImageWriterItem=class
    constructor Create(iType:string;iProc:TARGBImageFileWriterProc);
  private
    fType:string;
    fProc:TARGBImageFileWriterProc;
  public
    property Proc:TARGBImageFileWriterProc read fProc;
  end;

  TARGBImageReaderList=TNamedList<TARGBImageReaderItem>;
  TARGBImageWriterList=TNamedList<TARGBImageWriterItem>;

  TARGBPalette=array[0..255,0..3] of byte;

  TClipBox=record
    x1,x2,y1,y2:integer;
    wi,he:integer;
  end;

  TLMatrix=array[-1..1,-1..1] of integer;

  { TARGBImage }

  TARGBImage=class
    constructor Create; overload;
    constructor Create(iWidth,iHeight:integer); overload;
    constructor Create(iFilename:string); overload;

    destructor Destroy; override;

    // Checks if the two rectangle (1 and 2) overlaps. If yes, gives back the
    // overlapping area in the TClipBox record. If not, gives back -1 in x1
    // of the result.
    class function GetClipBox(x1,y1,w1,h1,x2,y2,w2,h2:integer):TClipBox; static;

    // Delete pixels from image where iMask has black (0,0,0) pixels
    // iMask must have the same size as image.
    procedure CombineMask(iMask:TARGBImage);

    // Multiplies all distinct color channel values in Self by iSource
    // then stores it in Self
    // Self and iSource must have the same size.
    procedure CombineMUL(iSource:TARGBImage);

    // Fills the image the specified color (defaults to (0,0,0,255))
    procedure Clear(color:uint32=$FF000000);

    // Copies a portion of Self to iTarget and sets iTarget size
    // according to the copied area
    procedure Copy(x1,y1,wi,he:integer;iTarget:TARGBImage;usecolorkey:boolean=false);

    // Copies a portion of Self to a specified position on iTarget
    procedure CopyTo(x1,y1,wi,he,x2,y2:integer;iTarget:TARGBImage;usecolorkey:boolean=false);

    // Copies source onto Self at x,y
    procedure PutImage(x,y:integer;source:TARGBImage;usecolorkey:boolean=false);

    // Copies a part of Source onto Self at x,y
    procedure PutImagePart(x,y,sx,sy,w,h:integer; source:TARGBImage;usecolorkey:boolean=false);

    // Inverts the image
    procedure Invert;

    // Processes the image with the given 3x3 matrix
    procedure WeightedMatrix(matrix:TLMatrix);

    // Recolors the image to a given HSV value
    procedure RecolorHSV(h,s,v:integer);

    // Recolors the image to a given RGB value
    procedure RecolorRGB(r,g,b:integer);

    // Rotates iSource by amount*90° clockwise.
    procedure Rotate(iAmount:word);

    // Puts a pixel.
    procedure PutPixel(x,y,r,g,b,a:integer); overload;

    // Puts a pixel.
    procedure PutPixel(x,y:integer;color32:uint32); overload;

    // Gets a pixel.
    function GetPixel(x,y:integer):uint32;

    // Draws a bar. If any color value is -1, it leaves that channel unchanged
    procedure Bar(x,y,w,h,r,g,b:integer;a:integer=255); overload;

    // Draws a bar. Fills all channels with given pixel data.
    procedure Bar(x,y,w,h:integer;color32:uint32); overload;

    // Draws a horizontal line. If any color value is -1, it leaves that channel unchanged
    procedure HLine(x,y,w,r,g,b:integer;a:integer=255); overload;

    // Draws a horizontal line. Fills all channels with given pixel data.
    procedure HLine(x,y,w:integer;color32:uint32); overload;

    // Draws a vertical line. If any color value is -1, it leaves that channel unchanged
    procedure VLine(x,y,h,r,g,b:integer;a:integer=255); overload;

    // Draws a vertical line. Fills all channels with given pixel data.
    procedure VLine(x,y,h:integer;color32:uint32); overload;

    // Draws a line between two points. (using Bresenham's line drawing algorithm)
    // If any color value is -1, it leaves that channel unchanged
    procedure Line(x1,y1,x2,y2,r,g,b:integer;a:integer=255); overload;

    // Draws a line between two points. (using Bresenham's line drawing algorithm)
    // Fills all channels with given pixel data.
    procedure Line(x1,y1,x2,y2:integer;color32:uint32); overload;

    // Draws a rectangle. If any color value is -1, it leaves that channel unchanged
    procedure Rectangle(x,y,w,h,r,g,b:integer;a:integer=255); overload;

    // Draws a rectangle. Fills all channels with given pixel data.
    procedure Rectangle(x,y,w,h:integer;color32:uint32); overload;

    // Draws a circle. (Using Bresenham's circle drawing algorithm.)
    // Fills all channels with given pixel data.
    // Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
    // Set hack to true if you want the circle's width and height to be an even number.
    procedure Circle(cx,cy,r:integer;color32:uint32;even:boolean=false);

    // Draws a filled circle. (Using Bresenham's circle drawing algorithm.)
    // Fills all channels with given pixel data.
    // Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
    // Set hack to true if you want the circle's width and height to be an even number.
    procedure FilledCircle(cx,cy,r:integer;color32:uint32;even:boolean=false);

    // Replaces a continuous color patch starting at the selected pixel.
    procedure FloodFill(x,y:integer;color32:uint32);

    // Crops the image to the smallest size possible containing all pixels
    // not equal the given color.
    procedure Crop(r,g,b,a:integer);

    // Resizes the image to the given size. Data outside the new dimensions is lost.
    procedure Resize(pNewWidth,pNewHeight:integer);

    // Grayscales the image
    procedure Grayscale;

    // Flips the image horizontally
    procedure FlipH;

    // Flips the image vertically
    procedure FlipV;

    // Makes the image double sized.
    procedure Resize2x;

    // Makes the image N times bigger.
    procedure Magnify(n:integer);

    // Shrinks the image to 2^-n  (1=/2, 2=/4, etc...)
    procedure HalveImage(n:integer);

    // Replace one exact color in image
    procedure ReplaceColor(sR,sG,sB,sA,tR,tG,tB,tA:uint32); overload;

    // Replace one exact color (disregarding alpha) in image
    procedure ReplaceColor(sR,sG,sB,tR,tG,tB,tA:uint32); overload;

    // Tilefills the image with another image
    procedure FillImage(pSource:TARGBImage);

    // Adds padding between frames to avoid artifacts with fullscreen scaling
    procedure AddPadding(pHorizontalFrameCount,pVerticalFrameCount:integer;pR:integer=0;pG:integer=0;pB:integer=0;pA:integer=255);

    // Sets the specified color to transparent, all others to opaque
    procedure SetColorkey(r,g,b:integer);

    // Copy one channel to alpha channel
    procedure CopyRToAlpha;
    procedure CopyGToAlpha;
    procedure CopyBToAlpha;

    // Performs multiple transactions on iSource
    procedure MultiTransform(iTransform:string);

    // Get one channel as GSD stream (2 word (width and height) then channel byte stream)
    function GetChannelAAsGSD:TStream;
    function GetChannelRAsGSD:TStream;
    function GetChannelGAsGSD:TStream;
    function GetChannelBAsGSD:TStream;

    // Compares the to ARGBImage and gives back true if the images are identical
    // (both size and every pixel including alpha)
    function IsIdentical(pOtherImage:TARGBImage):boolean;

    procedure WriteFile(pFilename:string;pFormat:string); overload;
    procedure WriteFile(pTarget:TStream;pFormat:string); overload;
    function WriteFile(pFormat:string):TStream; overload;

    procedure ReadFile(iFileName:string); overload;
    Procedure ReadFile(pStream:TStream;pFileType:string); overload;

  protected
    fWidth:integer;
    fHeight:integer;
    fRawdata:pointer;
  private
    fAnimations:TAnimationDatas;
    fUseAlpha:boolean;
    fFontData:TFontData;
    procedure MultiTransformEx(iTransform:string);
    function fGetChannelAsGSD(pOffset:integer):TStream;
    procedure fBarFull(x,y,w,h:integer;r,g,b,a:uint32);
    procedure fBarChannel(x,y,w,h,value,offset:integer);
    procedure fHLineFull(x,y,w:integer;r,g,b,a:uint32);
    procedure fHLineChannel(x,y,w,value,offset:integer);
    procedure fVLineFull(x,y,h:integer;r,g,b,a:uint32);
    procedure fVLineChannel(x,y,h,value,offset:integer);
    procedure fSetFontData(pFontData:TFontData);
  public
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property Rawdata:pointer read fRawData;
    property Animations:TAnimationDatas read fAnimations;
    property FontData:TFontData read fFontData write fSetFontData;
    property UseAlpha:boolean read fUseAlpha write fUseAlpha;
  end;

  procedure RegisterARGBImageReader(pType:string;pProc:TARGBImageFileReaderProc;pAffectsImage:boolean);
  procedure RegisterARGBImageWriter(pType:string;pProc:TARGBImageFileWriterProc);

implementation

uses SysUtils, MKToolBox, Logger, MKStream;

const
  Fstr={$I %FILE%}+', ';
  Version='1.31';
  POSTPROCESSCOLOR=$00FF00FF;  // Fully transparent magenta is the magic color!

var
  ARGBImageReaders:TARGBImageReaderList;
  ARGBImageWriters:TARGBImageWriterList;

constructor TARGBImageReaderItem.Create(iType:string;iProc:TARGBImageFileReaderProc;iAffectsImage:boolean);
begin
  fType:=iType;
  fProc:=iProc;
  fAffectsImage:=iAffectsImage;
end;

constructor TARGBImageWriterItem.Create(iType:string;iProc:TARGBImageFileWriterProc);
begin
  fType:=iType;
  fProc:=iProc;
end;

// ---------------------------------------------- Creating and manipulating ---

constructor TARGBImage.Create;
begin
  fWidth:=0;
  fHeight:=0;
  fRawdata:=nil;
  fAnimations:=TAnimationDatas.Create;
  fFontData:=nil;
end;

constructor TARGBImage.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  fRawData:=GetMem(fWidth*fHeight*4);
  fAnimations:=TAnimationDatas.Create;
  fFontData:=nil;
end;

constructor TARGBImage.Create(iFilename:string);
begin
  Create;
  ReadFile(iFilename);
end;

destructor TARGBImage.Destroy;
begin
  if (fRawdata<>nil) then FreeMem(fRawdata);
  if Assigned(fAnimations) then FreeAndNil(fAnimations);
  if Assigned(fFontData) then FreeAndNil(fFontData);
  inherited;
end;

class function TARGBImage.GetClipBox(x1,y1,w1,h1,x2,y2,w2,h2:integer):TClipBox;
begin
  if x1<x2 then begin
    if x1+w1<x2 then begin
      Result.x1:=-1;  // 1
    end else begin
      Result.x2:=0;
      Result.x1:=x2-x1;
      if x1+w1<=x2+w2 then begin  // 2
        Result.wi:=x1+w1-x2;
      end else begin  // 3
        Result.wi:=w2;
      end;
    end;
  end else begin
    if x1<x2+w2 then begin
      Result.x1:=0;
      Result.x2:=x1-x2;
      if x1+w1<=x2+w2 then begin
        Result.wi:=w1;
      end else begin
        Result.wi:=x2+w2-x1;
      end;
    end else begin
      Result.x1:=-1;  // 6
    end;
  end;
  if y1<y2 then begin
    if y1+h1<y2 then begin
      Result.x1:=-1;  // 1 - Must be x1, it indicates when the areas are not overlapping
    end else begin
      Result.y2:=0;
      Result.y1:=y2-y1;
      if y1+h1<=y2+h2 then begin  // 2
        Result.he:=y1+h1-y2;
      end else begin  // 3
        Result.he:=h2;
      end;
    end;
  end else begin
    if y1<y2+h2 then begin
      Result.y1:=0;
      Result.y2:=y1-y2;
      if y1+h1<=y2+h2 then begin
        Result.he:=h1;
      end else begin
        Result.he:=y2+h2-y1;
      end;
    end else begin
      Result.x1:=-1;  // 6 - Must be x1, it indicates when the areas are not overlapping
    end;
  end;
end;

procedure TARGBImage.CombineMask(iMask:TARGBImage);
const Istr=Fstr+'TRawPicture.CombineMask';
var cnt:integer;s,m:pointer;sx:uint32;
begin
  if (fWidth<>iMask.Width) or (fHeight<>iMask.Height) then begin
    Log.LogWarning('The two RawPictures must have the same size!',Istr);
    exit;
  end;
  s:=fRawdata;
  m:=iMask.Rawdata;
  sx:=$ff000000;

  for cnt:=(fWidth*fHeight)-1 downto 0 do begin
    if (byte(m^)=0) and (byte((m+1)^)=0) and (byte((m+2)^)=0) then uint32(s^):=sx;
    s+=4;
    m+=4;
  end;
end;

procedure TARGBImage.CombineMUL(iSource:TARGBImage);
const Istr=Fstr+'TRawPicture.CombineMUL';
var cnt:integer;s1,s2:pointer;b1,b2:word;
begin
  if (fWidth<>iSource.Width) or (fHeight<>iSource.Height) then begin
    Log.LogWarning('The two RawPictures must have the same size!',Istr);
    exit;
  end;
  s1:=iSource.Rawdata;
  s2:=fRawdata;

  for cnt:=(4*fWidth*fHeight)-1 downto 0 do begin
    b1:=byte(s1^);
    b2:=byte(s2^);
    byte(s2^):=b1*b2 div 256;
    inc(s1);inc(s2);
  end;
end;

procedure TARGBImage.Clear(color: uint32);
const Istr=Fstr+'TARGBImage.Clear';
var i:integer;
begin
  if (fRawdata<>nil) and (fWidth<>0) and (fHeight<>0) then begin
    for i:=0 to fWidth*fHeight-1 do
      uint32((fRawData+i*4)^):=color;
  end
  else
    Log.LogWarning('Attempt to clear an uninitialized rawpicture!',Istr);
end;

procedure TARGBImage.Copy(x1, y1, wi, he: integer; iTarget: TARGBImage;
  usecolorkey: boolean);
const Istr=Fstr+'TARGBImage.Copy';
var x,y:integer;s1,t:pointer;
begin
  if (wi<1) or (he<1) then begin
    Log.LogWarning('Illegal parameters!',Istr);
    exit;
  end;
  if (iTarget.Width<>wi) or (iTarget.Height<>he) then begin
    Freemem(fRawData);
    fRawData:=Getmem(wi*he*4);
  end;
  s1:=fRawdata;
  s1+=(y1*fWidth+x1)*4;
  t:=iTarget.Rawdata;

  if not useColorkey and not fUseAlpha then   // Simply overwrite
    for y:=0 to he-1 do begin
      move(s1^,t^,wi*4);
      s1+=fWidth*4;
      t+=wi*4;
    end
  else if useColorkey and not fUseAlpha then  // Only colorkey
    for y:=0 to he-1 do begin
      for x:=0 to wi-1 do begin
        if byte((s1+3)^)<>0 then uint32(t^):=uint32(s1^);
        inc(s1,4);
        inc(t,4);
      end;
      s1+=(fWidth-wi)*4;
    end
  else begin  // Use alpha. (This means auto colorkey, since 0 alpha means leave
              //original colors intact.)
    for y:=0 to he-1 do begin
      for x:=0 to wi-1 do begin
        if byte((s1+3)^)<>0 then begin
          if byte((s1+3)^)=255 then begin
            uint32(t^):=uint32(s1^);
          end else begin
            byte(t^):=byte(s1^)*byte((s1+3)^) div 255+byte(t^)*(255-byte((s1+3)^)) div 255;
            byte((t+1)^):=byte((s1+1)^)*byte((s1+3)^) div 255+byte((t+1)^)*(255-byte((s1+3)^)) div 255;
            byte((t+2)^):=byte((s1+2)^)*byte((s1+3)^) div 255+byte((t+2)^)*(255-byte((s1+3)^)) div 255;
          end;
        end;
        inc(s1,4);
        inc(t,4);
      end;
      s1+=(fWidth-wi)*4;
    end
  end;
end;

procedure TARGBImage.CopyTo(x1, y1, wi, he, x2, y2: integer;
  iTarget: TARGBImage; usecolorkey: boolean);
var x,y:integer;s1,t:pointer;
begin
  if Assigned(iTarget) then begin
    if not((wi>0) and (he>0)) then exit;
    // Clip width and height with source
    if x1<0 then begin wi+=x1;x1:=0;end;
    if x1+wi>fWidth then wi:=fWidth-x1;
    if y1<0 then begin he+=y1;y1:=0;end;
    if y1+he>fHeight then he:=fHeight-y1;
    if not((wi>0) and (he>0)) then exit;  // Still has something to copy
    // Clip width and height with target
    if x2<0 then begin wi+=x2;x2:=0;end;
    if x2+wi>iTarget.Width then wi:=iTarget.Width-x2;
    if y2<0 then begin he+=y2;y2:=0;end;
    if y2+he>iTarget.Height then he:=iTarget.Height-y2;
    if not((wi>0) and (he>0)) then exit; // Still has something to copy
    s1:=fRawdata;
    s1+=(y1*fWidth+x1)*4;
    with iTarget do
      t:=Rawdata+(y2*Width+x2)*4;

    if not useColorkey and not fUseAlpha then
      for y:=0 to he-1 do begin
        move(s1^,t^,wi*4);
        s1+=fWidth*4;
        t+=iTarget.Width*4;
      end
    else if useColorkey and not fUseAlpha then // Only colorkey
      for y:=0 to he-1 do begin
        for x:=0 to wi-1 do begin
          if byte((s1+3)^)<>0 then uint32(t^):=uint32(s1^);
          inc(s1,4);
          inc(t,4);
        end;
        s1+=(fWidth-wi)*4;
        t+=(iTarget.Width-wi)*4;
      end
    else begin
      for y:=0 to he-1 do begin
        for x:=0 to wi-1 do begin
          if byte((s1+3)^)<>0 then begin
            if byte((s1+3)^)=255 then begin
              uint32(t^):=uint32(s1^);
            end else begin
              byte(t^):=(byte(s1^)*byte((s1+3)^)) div 255+byte(t^)*(255-byte((s1+3)^)) div 255;
              byte((t+1)^):=(byte((s1+1)^)*byte((s1+3)^)) div 255+byte((t+1)^)*(255-byte((s1+3)^)) div 255;
              byte((t+2)^):=(byte((s1+2)^)*byte((s1+3)^)) div 255+byte((t+2)^)*(255-byte((s1+3)^)) div 255;
            end;
          end;
          inc(s1,4);
          inc(t,4);
        end;
        s1+=(fWidth-wi)*4;
        t+=(iTarget.Width-wi)*4;
      end
    end;
  end else
    raise Exception.Create('TARGBImage.CopyTo: Target is not assigned!');
end;

procedure TARGBImage.PutImage(x,y:integer;source:TARGBImage;usecolorkey:boolean);
var clipbox:TClipBox;i,j:integer;c:uint32;
begin
  if Assigned(source) then begin
    clipbox:=GetClipBox(0,0,Width,Height,x,y,source.Width,source.Height);
    if clipbox.x1<>-1 then with clipbox do begin
      if usecolorkey then begin
        for i:=0 to he-1 do
          for j:=0 to wi-1 do begin
            c:=uint32((source.RawData+(x2+j+(y2+i)*source.Width)*4)^);
            if c and $ff000000<>0 then uint32((fRawdata+(x1+j+(y1+i)*Width)*4)^):=c;
          end;
      end else begin
        for i:=0 to he-1 do
          move((source.RawData+(x2+(y2+i)*source.Width)*4)^,(fRawdata+(x1+(y1+i)*fWidth)*4)^,wi*4);
      end;
    end;
  end else
    raise Exception.Create('TARGBImage.PutImage: Source is not assigned!');
end;

procedure TARGBImage.PutImagePart(x,y,sx,sy,w,h:integer; source:TARGBImage;
  usecolorkey:boolean);
var clip1,clip2:TClipBox;i,j:integer;c:uint32;
begin
  // 1. Clip the source image with the specified area
  clip1:=GetClipBox(0,0,source.Width,source.Height,sx,sy,w,h);
  if clip1.x1=-1 then exit;  // Specified an area that is is not overlapping with source image.
  // 2. Clip the target image with remaining area
  clip2:=GetClipBox(0,0,Width,Height,x,y,clip1.wi,clip1.he);
  if clip2.x1<>-1 then begin
    // Still left some pixels to work with
    if usecolorkey then begin
      for i:=0 to clip2.he-1 do
        for j:=0 to clip2.wi-1 do begin
          c:=uint32((source.RawData+(clip1.x1+j+(clip1.y1+i)*source.Width)*4)^);
          if c and $FF000000<>0 then
            uint32((fRawdata+(clip2.x1+j+(clip2.y1+i)*fWidth)*4)^):=c;
        end;
    end else begin
      for i:=0 to clip2.he-1 do
        move((source.RawData+(clip1.x1+(clip1.y1+i)*source.Width)*4)^,
             (fRawdata+(clip2.x1+(clip2.y1+i)*fWidth)*4)^,clip2.wi*4);
    end;
  end;
end;

procedure TARGBImage.Invert;
var y:integer;s1:pointer;
begin
  s1:=fRawdata;
  for y:=0 to fWidth*fHeight-1 do begin
    byte(s1^):=255-byte(s1^);
    byte((s1+1)^):=255-byte((s1+1)^);
    byte((s1+2)^):=255-byte((s1+2)^);
    inc(s1,4);
  end;
end;

procedure TARGBImage.WeightedMatrix(matrix:TLMatrix);
var r,g,b,w:longint;
    x,y:longint;
    i,j:longint;
    s1,s2,p:pointer;
begin
  s1:=fRawdata;
  p:=getmem(fWidth*fHeight*4);
  s2:=p;
  for y:=0 to fHeight-1 do begin
    for x:=0 to fWidth-1 do begin
      r:=0;g:=0;b:=0;w:=0;
      for j:=-1 to 1 do
        for i:=-1 to 1 do
          if (y+j>=0) and (y+j<fHeight) and (x+i>=0) and (x+i<fWidth) then begin
            b+=byte((s1+(j*fWidth+i)*4)^)*Matrix[i,j];
            g+=byte((s1+(j*fWidth+i)*4+1)^)*Matrix[i,j];
            r+=byte((s1+(j*fWidth+i)*4+2)^)*Matrix[i,j];
            w+=Matrix[i,j];
          end;
      b:=b div w;
      g:=g div w;
      r:=r div w;
      byte(s2^):=b;
      byte((s2+1)^):=g;
      byte((s2+2)^):=r;
      byte((s2+3)^):=255;
      inc(s1,4);
      inc(s2,4);
    end;
  end;
  freemem(fRawData);
  fRawdata:=p;
end;

procedure TARGBImage.RecolorHSV(h,s,v:integer);
var r,g,b:integer;
begin
  r:=0;g:=0;b:=0;
  HSV2RGB(h,s,v,r,g,b);
  RecolorRGB(r,g,b);
end;

procedure TARGBImage.RecolorRGB(r,g,b:integer);
var i:integer;
    y:integer;s1:pointer;
begin
  s1:=fRawdata;
  for y:=0 to fWidth*fHeight-1 do begin
    i:=(byte(s1^)*11+byte((s1+1)^)*59+byte((s1+2)^)*30) div 100;
    byte(s1^):=(b*i)>>8 and $ff;
    byte((s1+1)^):=(g*i)>>8 and $ff;
    byte((s1+2)^):=(r*i)>>8 and $ff;
    inc(s1,4);
  end;
end;

procedure TARGBImage.Rotate(iAmount:word);
var x,y:integer;s,t,p:pointer;
begin
  p:=GetMem(fWidth*fHeight*4);

  case iAmount mod 4 of
    0:exit;   // No rotate
    1:begin // Rotate 90°
        s:=fRawdata+(fHeight-1)*fWidth*4;
        t:=p;

        for x:=0 to fWidth-1 do begin
          for y:=0 to fHeight-1 do begin
            move(s^,t^,4);
            t+=4;
            s-=fWidth*4;
          end;
          s:=s+(fHeight*fWidth*4)+4;
        end;
        x:=fWidth;fWidth:=fHeight;fHeight:=x;
      end;
    2:begin  // Rotate 180°
        s:=fRawData+(fWidth*fHeight-1)*4;
        t:=p;
        for x:=0 to fWidth*fHeight-1 do begin
          move(s^,t^,4);
          s-=4;
          t+=4;
        end;
      end;
    3:begin                   // Rotate 270° (or 90° anti-clockwise)
        s:=fRawdata+(fWidth-1)*4;
        t:=p;

        for x:=0 to fWidth-1 do begin
          for y:=0 to fHeight-1 do begin
            move(s^,t^,4);
            t+=4;
            s+=fWidth*4;
          end;
          s:=s-(fHeight*fWidth*4)-4;
        end;
        x:=fWidth;fWidth:=fHeight;fHeight:=x;
      end;
  end;
  freemem(fRawData);
  fRawdata:=p;
end;

procedure TARGBImage.PutPixel(x,y,r,g,b,a:integer);
begin
  byte((fRawdata+(x+y*fWidth)*4)^):=b;
  byte((fRawdata+(x+y*fWidth)*4+1)^):=g;
  byte((fRawdata+(x+y*fWidth)*4+2)^):=r;
  byte((fRawdata+(x+y*fWidth)*4+3)^):=a;
end;

procedure TARGBImage.PutPixel(x,y:integer; color32:uint32);
begin
  if (x>=0) and (x<Width) and (y>=0) and (y<Height) then
    uint32((fRawdata+(x+y*fWidth)*4)^):=color32;
end;

function TARGBImage.GetPixel(x,y:integer):uint32;
begin
  Result:=uint32((fRawData+(x+y*fWidth)*4)^);
end;

procedure TARGBImage.Bar(x, y, w, h, r, g, b: integer; a: integer);
begin
  // If has valid dimensions and overlaps our image
  if (w>0) and (h>0) and (x<fWidth) and (x+w>0) and (y<fHeight) and (y+h>0) then begin
    // Still check for clipping
    if x<0 then begin w+=x;x:=0;end;
    if x+w>fWidth then w:=fWidth-x;
    if y<0 then begin h+=y;y:=0;end;
    if y+h>fHeight then h:=fHeight-y;

    // And now draw
    if (b>=0) and (b<=255) and (g>=0) and (g<=255) and
       (r>=0) and (r<=255) and (a>=0) and (a<=255) then fBarFull(x,y,w,h,r,g,b,a)
    else begin
      if (b>=0) and (b<=255) then fBarChannel(x,y,w,h,b,0);
      if (g>=0) and (g<=255) then fBarChannel(x,y,w,h,g,1);
      if (r>=0) and (r<=255) then fBarChannel(x,y,w,h,r,2);
      if (a>=0) and (a<=255) then fBarChannel(x,y,w,h,a,3);
    end;
  end;
end;

procedure TARGBImage.Bar(x,y,w,h:integer; color32:uint32);
var i,j:integer;p:pointer;
begin
  // If has valid dimensions and overlaps our image
  if (w>0) and (h>0) and (x<fWidth) and (x+w>0) and (y<fHeight) and (y+h>0) then begin
    // Still check for clipping
    if x<0 then begin w+=x;x:=0;end;
    if x+w>fWidth then w:=fWidth-x;
    if y<0 then begin h+=y;y:=0;end;
    if y+h>fHeight then h:=fHeight-y;
    for j:=y to y+h-1 do begin
      p:=fRawdata+(j*fWidth+x)*4;
      for i:=0 to w-1 do begin
        uint32(p^):=color32;
        inc(p,4);
      end;
    end;

  end;
end;

procedure TARGBImage.HLine(x, y, w, r, g, b: integer; a: integer);
begin
  // If has valid dimensions and overlaps our image
  if (w>0) and (x<fWidth) and (x+w>0) and (y<fHeight) and (y>=0) then begin
    // Still check for clipping
    if x<0 then begin w+=x;x:=0;end;
    if x+w>fWidth then w:=fWidth-x;

    // And now draw
    if (b>=0) and (b<=255) and (g>=0) and (g<=255) and
       (r>=0) and (r<=255) and (a>=0) and (a<=255) then fHLineFull(x,y,w,r,g,b,a)
    else begin
      if (b>=0) and (b<=255) then fHLineChannel(x,y,w,b,0);
      if (g>=0) and (g<=255) then fHLineChannel(x,y,w,g,1);
      if (r>=0) and (r<=255) then fHLineChannel(x,y,w,r,2);
      if (a>=0) and (a<=255) then fHLineChannel(x,y,w,a,3);
    end;
  end;
end;

procedure TARGBImage.HLine(x,y,w:integer; color32:uint32);
var p:pointer;
begin
  // If has valid dimensions and overlaps our image
  if (w>0) and (x<fWidth) and (x+w>0) and (y<fHeight) and (y>=0) then begin
    // Still check for clipping
    if x<0 then begin w+=x;x:=0;end;
    if x+w>fWidth then w:=fWidth-x;

    // And now draw
    p:=fRawdata+(y*fWidth+x)*4;
    while w>0 do begin
      uint32(p^):=color32;
      dec(w);
      inc(p,4);
    end;
  end;
end;

procedure TARGBImage.VLine(x, y, h, r, g, b: integer; a: integer);
begin
  // If has valid dimensions and overlaps our image
  if (h>0) and (x<fWidth) and (x>=0) and (y<fHeight) and (y+h>0) then begin
    // Still check for clipping
    if y<0 then begin h+=y;y:=0;end;
    if y+h>fHeight then h:=fHeight-y;

    // And now draw
    if (b>=0) and (b<=255) and (g>=0) and (g<=255) and
       (r>=0) and (r<=255) and (a>=0) and (a<=255) then fVLineFull(x,y,h,r,g,b,a)
    else begin
      if (b>=0) and (b<=255) then fVLineChannel(x,y,h,b,0);
      if (g>=0) and (g<=255) then fVLineChannel(x,y,h,g,1);
      if (r>=0) and (r<=255) then fVLineChannel(x,y,h,r,2);
      if (a>=0) and (a<=255) then fVLineChannel(x,y,h,a,3);
    end;
  end;
end;

procedure TARGBImage.VLine(x,y,h:integer; color32:uint32);
var p:pointer;
begin
  // If has valid dimensions and overlaps our image
  if (h>0) and (x<fWidth) and (x>=0) and (y<fHeight) and (y+h>0) then begin
    // Still check for clipping
    if y<0 then begin h+=y;y:=0;end;
    if y+h>fHeight then h:=fHeight-y;

    // And now draw
    p:=fRawdata+(y*fWidth+x)*4;
    while h>0 do begin
      uint32(p^):=color32;
      dec(h);
      inc(p,fWidth*4);
    end;
  end;
end;

procedure TARGBImage.Line(x1, y1, x2, y2, r, g, b: integer; a: integer);
begin
  Line(x1,y1,x2,y2,
    (a and $ff)<<24+(b and $ff)<<16+(g and $ff)<<8+(r and $ff));
end;

// Taken from http://www.efg2.com/Lab/Library/Delphi/Graphics/Bresenham.txt
// Stripped a few comments, variable names changed here and there...
procedure TARGBImage.Line(x1, y1, x2, y2: integer; color32: uint32);
var
  _a,_b,_d : integer;
  diag_inc, nondiag_inc : integer;
  dx_diag, dx_nondiag, dy_diag, dy_nondiag : integer;
  i,swap,x,y : integer;
begin
  x := x1;
  y := y1;
  {Determine drawing direction and step to the next pixel.}
  _a := x2 - x1;
  _b := y2 - y1;
  {Determine whether end point lies to right or left of start point.}
  if _a < 0 then begin
    _a := -_a;
    dx_diag := -1;
  end else
    dx_diag := 1;
  {Determine whether end point lies above or below start point.}
  if _b < 0 then begin
    _b := -_b;
    dy_diag := -1
  end else
    dy_diag := 1;
  {Identify octant containing end point.}
  if _a < _b then begin
    swap := _a;
    _a := _b;
    _b := swap;
    dx_nondiag := 0;
    dy_nondiag := dy_diag
  end else begin
    dx_nondiag := dx_diag;
    dy_nondiag := 0
  end;
  _d := _b + _b - _a;
  nondiag_inc := _b + _b;
  diag_inc    := _b + _b - _a - _a;
  for i := 0 to _a do begin   {draw the a+1 pixels}
    PutPixel(x,y,color32);
    if _d < 0 then begin
      x := x + dx_nondiag;
      y := y + dy_nondiag;
      _d := _d + nondiag_inc
    end else begin
      x := x + dx_diag;
      y := y + dy_diag;
      _d := _d + diag_inc
    end;
  end;
end;

procedure TARGBImage.Rectangle(x, y, w, h, r, g, b: integer; a: integer);
begin
  VLine(x,y,h,r,g,b,a);
  VLine(x+w-1,y,h,r,g,b,a);
  HLine(x,y,w,r,g,b,a);
  HLine(x,y+h-1,w,r,g,b,a);
end;

procedure TARGBImage.Rectangle(x,y,w,h:integer; color32:uint32);
begin
  VLine(x,y,h,color32);
  VLine(x+w-1,y,h,color32);
  HLine(x,y,w,color32);
  HLine(x,y+h-1,w,color32);
end;

// Taken from https://www.geeksforgeeks.org/bresenhams-circle-drawing-algorithm/
procedure TARGBImage.Circle(cx,cy,r:integer; color32:uint32; even:boolean);

  procedure PutPixel8(x,y:integer);
  begin
    PutPixel(cx+x, cy+y, color32);
    PutPixel(cx-x, cy+y, color32);
    PutPixel(cx+x, cy-y, color32);
    PutPixel(cx-x, cy-y, color32);
    PutPixel(cx+y, cy+x, color32);
    PutPixel(cx-y, cy+x, color32);
    PutPixel(cx+y, cy-x, color32);
    PutPixel(cx-y, cy-x, color32);
  end;

  procedure PutPixel8_even(x,y:integer);
  begin
    PutPixel(cx+x,   cy+y,   color32);
    PutPixel(cx-x-1, cy+y,   color32);
    PutPixel(cx+x,   cy-y-1, color32);
    PutPixel(cx-x-1, cy-y-1, color32);
    PutPixel(cx+y,   cy+x,   color32);
    PutPixel(cx-y-1, cy+x,   color32);
    PutPixel(cx+y,   cy-x-1, color32);
    PutPixel(cx-y-1, cy-x-1, color32);
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  if not even then PutPixel8(x,y) else PutPixel8_even(x,y);
  while (y>=x) do begin
    inc(x);
    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    if not even then PutPixel8(x,y) else PutPixel8_even(x,y);
  end;
end;

procedure TARGBImage.FilledCircle(cx,cy,r:integer; color32:uint32; even:boolean);

  procedure CHLine(x,y:integer);
  begin
    HLine(cx-x,cy+y,2*x+1,color32);
    HLine(cx-x,cy-y,2*x+1,color32);
    HLine(cx-y,cy+x,2*y+1,color32);
    HLine(cx-y,cy-x,2*y+1,color32);
  end;

  procedure CHLine_even(x,y:integer);
  begin
    HLine(cx-x-1,cy+y  ,2*x+2,color32);
    HLine(cx-x-1,cy-y-1,2*x+2,color32);
    HLine(cx-y-1,cy+x  ,2*y+2,color32);
    HLine(cx-y-1,cy-x-1,2*y+2,color32);
  end;

var x,y,d:integer;

begin
  x:=0;
  y:=r;
  d:=3-2*r;
  if not even then CHLine(x,y) else CHLine_even(x,y);
  while (y>=x) do begin
    inc(x);
    // check for decision parameter and correspondingly update d, x, y
    if d>0 then begin
      dec(y);
      d:=d+4*(x-y)+10;
    end else
      d:=d+4*x+6;
    if not even then CHLine(x,y) else CHLine_even(x,y);
  end;
end;

procedure TARGBImage.FloodFill(x,y:integer; color32:uint32);
var i,j:integer;w:boolean;cc:uint32;

  function FFCheckPixel(x,y:integer;src,trg:uint32):boolean;
  begin
    Result:=false;
    if (y>0) and (GetPixel(x,y-1)=src) then begin
      PutPixel(x,y-1,trg);
      Result:=true;
    end;
    if (x<Width-1) and (GetPixel(x+1,y)=src) then begin
      PutPixel(x+1,y,trg);
      Result:=true;
    end;
    if (y<Height-1) and (GetPixel(x,y+1)=src) then begin
      PutPixel(x,y+1,trg);
      Result:=true;
    end;
    if (x>0) and (GetPixel(x-1,y)=src) then begin
      PutPixel(x-1,y,trg);
      Result:=true;
    end;
  end;

begin
  cc:=GetPixel(x,y);
  if cc=color32 then exit;  // The two color is identical.
  PutPixel(x,y,POSTPROCESSCOLOR);
  repeat
    w:=false;
    for j:=0 to Height-1 do
      for i:=0 to Width-1 do
        if GetPixel(i,j)=POSTPROCESSCOLOR then
          if FFCheckPixel(i,j,cc,POSTPROCESSCOLOR) then w:=true;
    if w then begin
      w:=false;
      for j:=Height-1 downto 0 do
        for i:=0 to Width-1 do
          if GetPixel(i,j)=POSTPROCESSCOLOR then
            if FFCheckPixel(i,j,cc,POSTPROCESSCOLOR) then w:=true;
    end;
    if w then begin
      w:=false;
      for j:=Height-1 downto 0 do
        for i:=Width-1 downto 0 do
          if GetPixel(i,j)=POSTPROCESSCOLOR then
            if FFCheckPixel(i,j,cc,POSTPROCESSCOLOR) then w:=true;
    end;
    if w then begin
      w:=false;
      for j:=0 to Height-1 do
        for i:=Width-1 downto 0 do
          if GetPixel(i,j)=POSTPROCESSCOLOR then
            if FFCheckPixel(i,j,cc,POSTPROCESSCOLOR) then w:=true;
    end;
  until not w;
  if color32<>POSTPROCESSCOLOR then
    for j:=0 to Height-1 do
      for i:=0 to Width-1 do
        if GetPixel(i,j)=POSTPROCESSCOLOR then PutPixel(i,j,color32);
end;

procedure TARGBImage.Crop(r, g, b, a: integer);
var i,j,x1,y1,x2,y2,w,h:integer;p:pointer;
begin
  // 1. Determine smaller image
  x1:=fWidth;
  y1:=fHeight;
  x2:=0;
  y2:=0;
  p:=fRawdata;
  for j:=0 to fHeight-1 do
    for i:=0 to fWidth-1 do begin
      if (byte(p^)<>b) or (byte((p+1)^)<>g) or (byte((p+2)^)<>r) or (byte((p+3)^)<>a) then begin
        if i<x1 then x1:=i;
        if i>x2 then x2:=i;
        if j<y1 then y1:=j;
        if j>y2 then y2:=j;
      end;
      inc(p,4);
    end;
  w:=x2-x1+1;
  h:=y2-y1+1;
  // 2. Allocate memory for smaller image
  p:=GetMem(w*h*4);
  // 3. Copy cropped data to allocated memory
  for j:=0 to h-1 do
    move((fRawData+((y1+j)*fWidth+x1)*4)^,(p+(j*w)*4)^,w*4);
  // 4. Assign new data to image
  freemem(fRawData);
  fRawData:=p;
  // 5. Adjust size
  fWidth:=w;
  fHeight:=h;
end;

procedure TARGBImage.Resize(pNewWidth,pNewHeight:integer);
var tmp:pointer;i:integer;
begin
  tmp:=Getmem(pNewWidth*pNewHeight*4);
  fillchar(tmp^,pNewWidth*pNewHeight*4,0);
  for i:=0 to min(fHeight,pNewHeight)-1 do
    Move((fRawdata+i*Width*4)^,(tmp+i*pNewWidth*4)^,min(Width,pNewWidth)*4);
  Freemem(fRawdata);
  fWidth:=pNewWidth;
  fHeight:=pNewHeight;
  fRawdata:=tmp;
end;

procedure TARGBImage.Grayscale;
var y,gw:integer;s1:pointer;
begin
  s1:=fRawdata;
  for y:=0 to fWidth*fHeight-1 do begin
    gw:=(byte(s1^)*11+byte((s1+1)^)*59+byte((s1+2)^)*30) div 100;
    byte(s1^):=gw;
    byte((s1+1)^):=gw;
    byte((s1+2)^):=gw;
    inc(s1,4);
  end;
end;

procedure TARGBImage.FlipH;
var x,y:integer;s,t,p:pointer;
begin
  p:=getmem(fWidth*fHeight*4);
  t:=p;
  s:=fRawdata+(fWidth-1)*4;

  for y:=0 to fHeight-1 do begin
    for x:=0 to fWidth-1 do begin
      move(s^,t^,4);
      t+=4;
      s-=4;
    end;
    s+=fWidth*4*2;
  end;
  freemem(fRawData);
  fRawdata:=p;
end;

procedure TARGBImage.FlipV;
var y:integer;s,t,p:pointer;
begin
  p:=getmem(fWidth*fHeight*4);
  s:=fRawdata+(fHeight-1)*fWidth*4;
  t:=p;

  for y:=0 to fHeight-1 do begin
    move(s^,t^,fWidth*4);
    t+=fwidth*4;
    s-=fWidth*4;
  end;
  freemem(fRawData);
  fRawdata:=p;
end;

procedure TARGBImage.Resize2x;
var s,t:pointer;x,y:integer;p:pointer;
begin
  p:=getmem(fWidth*fHeight*4*4);
  t:=p;
  s:=fRawdata;
  for y:=0 to fHeight<<1-1 do
    for x:=0 to fWidth<<1-1 do begin
      move((s+(y>>1*fWidth+x>>1)*4)^,t^,4);
      t+=4;
    end;
  freemem(fRawData);
  fRawdata:=p;
  fWidth:=fWidth<<1;
  fHeight:=fHeight<<1;
end;

procedure TARGBImage.Magnify(n:integer);
var s,t:pointer;x,y,i,j:integer;p:pointer;
begin
  if n<2 then exit;
  p:=getmem(fWidth*fHeight*4*n*n);
  s:=fRawdata;

  for y:=0 to fHeight-1 do
    for x:=0 to fWidth-1 do begin
      t:=p+(y*fWidth*n+x)*n*4;
      for i:=0 to n-1 do
        for j:=0 to n-1 do
          move(s^,(t+(j*fWidth*n+i)*4)^,4);
      inc(s,4);
    end;

  freemem(fRawData);
  fRawdata:=p;
  fWidth:=fWidth*n;
  fHeight:=fHeight*n;
end;

procedure TARGBImage.HalveImage(n:integer);
var i,j,x,y,r,g,b,a,f:integer;s,t:pointer;
begin
  if (n>0) and (n<16) then begin
    f:=1 << n;
    if (fWidth mod f<>0) then begin
      Log.LogError(Format('HalveImage: Cannot divide %d with %d!',[fWidth,f]));
      exit;
    end;
    if (fHeight mod f<>0) then begin
      Log.LogError(Format('HalveImage: Cannot divide %d with %d!',[fHeight,f]));
      exit;
    end;
    fWidth:=fWidth div f;
    fHeight:=fHeight div f;
    t:=GetMem(fHeight*fWidth*4);
    for j:=0 to fHeight-1 do begin
      for i:=0 to fWidth-1 do begin
        r:=0;g:=0;b:=0;a:=0;
        for y:=0 to f-1 do begin
          s:=fRawdata+(j*f+y)*fWidth*f*4+i*f*4;
          for x:=0 to f-1 do begin
            b+=byte((s)^);
            g+=byte((s+1)^);
            r+=byte((s+2)^);
            a+=byte((s+3)^);
            s+=4;
          end;
        end;
        byte((t)^):=b div (f*f);
        byte((t+1)^):=g div (f*f);
        byte((t+2)^):=r div (f*f);
        byte((t+3)^):=a div (f*f);
        t+=4;
      end;
    end;
    Freemem(fRawdata);
    fRawdata:=t-fWidth*fHeight*4;
  end;
end;

procedure TARGBImage.ReplaceColor(sR,sG,sB,sA,tR,tG,tB,tA:uint32);
var p:pointer;i,s,t:uint32;
begin
  p:=fRawData;
  s:=sA<<24+sR<<16+sG<<8+sB;
  t:=tA<<24+tR<<16+tG<<8+tB;
  for i:=0 to fWidth*fHeight-1 do begin
    if uint32(p^)=s then uint32(p^):=t;
    inc(p,4);
  end;
end;

procedure TARGBImage.ReplaceColor(sR,sG,sB,tR,tG,tB,tA:uint32);
var p:pointer;i:integer;s,t:uint32;
begin
  p:=fRawData;
  s:=sR<<16+sG<<8+sB;
  t:=tA<<24+tR<<16+tG<<8+tB;
  for i:=0 to fWidth*fHeight-1 do begin
    if uint32(p^) and $00ffffff=s then uint32(p^):=t;
    inc(p,4);
  end;
end;

procedure TARGBImage.FillImage(pSource: TARGBImage);
var i,j:integer;
begin
  for j:=0 to (Height div pSource.Height)-1 do begin
    for i:=0 to (Width div pSource.Width)-1 do
      pSource.CopyTo(0,0,pSource.Width,pSource.Height,i*pSource.Width,j*pSource.Height,Self);
    if Width mod pSource.Width>0 then
      pSource.CopyTo(0,0,Width mod pSource.Width,pSource.Height,i*pSource.Width,j*pSource.Height,Self);
  end;
  if Height mod pSource.Height>0 then begin
    for i:=0 to (Width div pSource.Width)-1 do
      pSource.CopyTo(0,0,pSource.Width,Height mod pSource.Height,i*pSource.Width,j*pSource.Height,Self);
    if Width mod pSource.Width>0 then
      pSource.CopyTo(0,0,Width mod pSource.Width,Height mod pSource.Height,i*pSource.Width,j*pSource.Height,Self);
  end;
end;

procedure TARGBImage.MultiTransformEx(iTransform:string);
const Istr=Fstr+'TRawPicture.MultiTransformEx';
var i:integer;
begin
  for i:=1 to length(iTransform) do begin
    Log.LogDebug('['+inttostr(i)+'/'+inttostr(length(iTransform))+'] '+iTransform[i],Istr);
    case iTransform[i] of
      'R':Rotate(1);
      'r':Rotate(3);
      'H':FlipH;
      'V':FlipV;
      'I':Invert;
      else Log.LogWarning('Unknown transformation command ('+iTransform[1]+')','RAWTRANSFORM.II, TransformRawImage');
    end;
  end;
  Log.LogDebug('Finished.',Istr);
end;

procedure TARGBImage.MultiTransform(iTransform:string);
const Istr=Fstr+'TRawPicture.MultiTransform';
var d:integer;
begin
  if length(alltrim(iTransform))=0 then begin
    Log.LogDebug('No transformation needed.',Istr);
    exit;
  end;
  Log.LogDebug('Starting...',Istr);
  Log.LogDebug('Transformation string: '+iTransform,Istr);
  d:=CountChar('D',iTransform);
  iTransform:=StringReplace(iTransform,'D','',[rfReplaceAll]);
  Log.LogDebug('Size: '+inttostr(fWidth)+'x'+inttostr(fHeight),Istr);
  if fAnimations.Count>0 then begin
    Log.LogWarning('Transforming animated images no longer supported.',Istr);
  end else begin
    Log.LogDebug('Not animated.',Istr);
    MultiTransformEx(iTransform);
  end;
  while d>0 do begin
    Resize2x;
    dec(d);
  end;
end;

procedure TARGBImage.AddPadding(pHorizontalFrameCount,pVerticalFrameCount:integer;pR:integer=0;pG:integer=0;pB:integer=0;pA:integer=255);
var
  newRawData,p,q:pointer;
  newWidth,newHeight:integer;
  i,j,y:integer;
  frameWidth,frameHeight:integer;
begin
  if fWidth mod pHorizontalFrameCount<>0 then
    raise Exception.Create(Format('Fractional horizontal frame! (Width=%d, FrameCount=%d)',[fWidth, pHorizontalFrameCount]));
  if fHeight mod pVerticalFrameCount<>0 then
    raise Exception.Create(Format('Fractional vertical frame! (Height=%d, FrameCount=%d)',[fHeight, pVerticalFrameCount]));
  frameWidth:=fWidth div pHorizontalFrameCount;
  frameHeight:=fHeight div pVerticalFrameCount;
  newWidth:=fWidth+pHorizontalFrameCount+1;
  newHeight:=fHeight+pVerticalFrameCount+1;
  newRawData:=GetMem(newWidth*newHeight*4);
  p:=newRawData;
  for i:=0 to newWidth*newHeight-1 do begin
    byte(p^):=pB;
    byte((p+1)^):=pG;
    byte((p+2)^):=pR;
    byte((p+3)^):=pA;
    inc(p,4);
  end;
  for i:=0 to pHorizontalFrameCount-1 do
    for j:=0 to pVerticalFrameCount-1 do begin
      p:=fRawData+(i*frameWidth+j*frameHeight*fWidth)*4;
      q:=newRawData+((i*(frameWidth+1)+1)+(j*(frameHeight+1)+1)*newWidth)*4;
      for y:=0 to frameHeight-1 do
        move((p+(y*fWidth*4))^,(q+(y*newWidth*4))^,frameWidth*4);
    end;
  freemem(fRawData);
  fRawdata:=newRawData;
  fWidth:=newWidth;
  fHeight:=newHeight;
end;

procedure TARGBImage.SetColorkey(r,g,b:integer);
var p:pointer;i:integer;
begin
  p:=fRawdata;
  for i:=0 to fWidth*fHeight-1 do begin
    if (byte(p^)=b) and (byte((p+1)^)=g) and (byte((p+2)^)=r) then
      byte((p+3)^):=0
    else
      byte((p+3)^):=255;
    inc(p,4);
  end;
end;

function TARGBImage.GetChannelAAsGSD:TStream;
begin
  Result:=fGetChannelAsGSD(0);
end;

function TARGBImage.GetChannelRAsGSD:TStream;
begin
  Result:=fGetChannelAsGSD(1);
end;

function TARGBImage.GetChannelGAsGSD:TStream;
begin
  Result:=fGetChannelAsGSD(2);
end;

function TARGBImage.GetChannelBAsGSD:TStream;
begin
  Result:=fGetChannelAsGSD(3);
end;

function TARGBImage.IsIdentical(pOtherImage: TARGBImage): boolean;
var i:integer;
begin
  Result:=false;
  if Assigned(pOtherImage) then begin
    if (Width=pOtherImage.Width) and (Height=pOtherImage.Height) then begin
      Result:=true;
      for i:=0 to Width*height-1 do
        if uint32((Rawdata+i*4)^)<>uint32((pOtherImage.Rawdata+i*4)^) then begin
          Result:=false;
          exit;
        end;
    end;
  end;
end;

procedure TARGBImage.CopyRToAlpha;
var source,target:pointer;i:integer;
begin
  source:=fRawData+2;
  target:=fRawData+3;
  for i:=0 to fWidth*fHeight-1 do
    byte((target+i*4)^):=byte((source+i*4)^);
end;

procedure TARGBImage.CopyGToAlpha;
var source,target:pointer;i:integer;
begin
  source:=fRawData+1;
  target:=fRawData+3;
  for i:=0 to fWidth*fHeight-1 do
    byte((target+i*4)^):=byte((source+i*4)^);
end;

procedure TARGBImage.CopyBToAlpha;
var source,target:pointer;i:integer;
begin
  source:=fRawData;
  target:=fRawData+3;
  for i:=0 to fWidth*fHeight-1 do
    byte((target+i*4)^):=byte((source+i*4)^);
end;

function TARGBImage.fGetChannelAsGSD(pOffset:integer):TStream;
var i:integer;p:pointer;
begin
  Result:=TMemoryStream.Create;
  Result.Write(fWidth,2);
  Result.Write(fHeight,2);
  p:=fRawdata+pOffset;
  for i:=0 to fWidth*fHeight-1 do begin
    Result.Write(p,1);
    inc(p,4);
  end;
end;

procedure TARGBImage.fBarFull(x,y,w,h:integer;r,g,b,a:uint32);
var p:pointer;i,j:integer;v:uint32;
begin
  v:=b and $ff+(g and $ff)<<8+(r and $ff)<<16+(a and $ff)<<24;
  for j:=y to y+h-1 do begin
    p:=fRawdata+(j*fWidth+x)*4;
    for i:=0 to w-1 do begin
      uint32(p^):=v;
      inc(p,4);
    end;
  end;

end;

procedure TARGBImage.fBarChannel(x,y,w,h,value,offset:integer);
var p:pointer;i,j:integer;b:byte;
begin
  if (offset>=0) and (offset<=3) then begin
    b:=value;
    for j:=y to y+h-1 do begin
      p:=fRawdata+((j*fWidth+x)*4)+offset;
      for i:=0 to w-1 do begin
        byte(p^):=b;
        inc(p,4);
      end;
    end;
  end;
end;

procedure TARGBImage.fHLineFull(x,y,w:integer;r,g,b,a:uint32);
var p:pointer;v:uint32;
begin
  p:=fRawdata+(y*fWidth+x)*4;
  v:=b and $ff+(g and $ff)<<8+(r and $ff)<<16+(a and $ff)<<24;
  while w>0 do begin
    uint32(p^):=v;
    dec(w);
    inc(p,4);
  end;
end;

procedure TARGBImage.fHLineChannel(x,y,w,value,offset:integer);
var p:pointer;
begin
  if (offset>=0) and (offset<=3) then begin
    p:=fRawdata+(y*fWidth+x)*4+offset;
    while w>0 do begin
      byte(p^):=value;
      dec(w);
      inc(p,4);
    end;
  end;
end;

procedure TARGBImage.fVLineFull(x,y,h:integer;r,g,b,a:uint32);
var p:pointer;v:uint32;
begin
  p:=fRawdata+(y*fWidth+x)*4;
  v:=b and $ff+((g and $ff)<<8)+((r and $ff)<<16)+((a and $ff)<<24);
  while h>0 do begin
    uint32(p^):=v;
    dec(h);
    inc(p,fWidth*4);
  end;
end;

procedure TARGBImage.fVLineChannel(x,y,h,value,offset:integer);
var p:pointer;
begin
  if (offset>=0) and (offset<=3) then begin
    p:=fRawdata+(y*fWidth+x)*4+offset;
    while h>0 do begin
      byte(p^):=value;
      dec(h);
      inc(p,fWidth*4);
    end;
  end;
end;

procedure TARGBImage.fSetFontData(pFontData:TFontData);
begin
  if Assigned(fFontData) then FreeAndNil(fFontData);
  fFontData:=pFontData;
end;

procedure TARGBImage.ReadFile(iFileName: string);
var ext:string;i:integer;s:string;Xs:TStream;
begin
  s:=iFilename;
  if ExtractFileExt(s)='.ZL' then s:=ChangeFileExt(s,'');
  ext:=uppercase(ExtractFileExt(s));
  if length(ext)>1 then delete(ext,1,1);
  i:=ARGBImageReaders.IndexOf(ext);
  if i=-1 then raise Exception.Create('Extension not recognized! ('+ext+')');
  Xs:=MKStreamOpener.OpenStream(iFileName);
  if ARGBImageReaders[i].AffectsImage then begin
    if (fRawdata<>nil) then Freemem(fRawdata);
    fAnimations.Clear;
    if Assigned(fFontData) then FreeAndNil(fFontData);
  end;
  ARGBImageReaders[i].proc(Xs,fWidth,fHeight,fRawdata,fAnimations,fFontData);
  FreeAndNil(Xs);
end;

procedure TARGBImage.ReadFile(pStream: TStream; pFileType: string);
var i:integer;
begin
  i:=ARGBImageReaders.IndexOf(uppercase(pFileType));
  if i=-1 then raise Exception.Create('Filetype not recognized! ('+pFileType+')');
  if ARGBImageReaders[i].AffectsImage then begin
    if (fRawdata<>nil) then Freemem(fRawdata);
    fAnimations.Clear;
    if Assigned(fFontData) then FreeAndNil(fFontData);
  end;
  ARGBImageReaders[i].proc(pStream,fWidth,fHeight,fRawdata,fAnimations,fFontData);
end;

procedure TARGBImage.WriteFile(pFilename:string;pFormat:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  WriteFile(Xs,pFormat);
  FreeAndNil(Xs);
end;

procedure TARGBImage.WriteFile(pTarget:TStream;pFormat:string);
var i:integer;
begin
  if ARGBImageWriters.Count=0 then raise Exception.Create('No RawPicture writers are registered!');
  i:=ARGBImageWriters.IndexOf(pFormat);
  if i=-1 then raise Exception.Create(pFormat+' writer is not registered!');
  ARGBImageWriters[i].Proc(pTarget,fWidth,fHeight,fRawdata,fAnimations,fFontData);
end;

function TARGBImage.WriteFile(pFormat:string):TStream;
begin
  Result:=TMemoryStream.Create;
  WriteFile(Result,pFormat);
end;

procedure RegisterARGBImageReader(pType:string;pProc:TARGBImageFileReaderProc;pAffectsImage:boolean);
var atm:TARGBImageReaderItem;
begin
  atm:=TARGBImageReaderItem.Create(pType,pProc,pAffectsImage);
  ARGBImageReaders.AddObject(pType,atm);
end;

procedure RegisterARGBImageWriter(pType:string;pProc:TARGBImageFileWriterProc);
var atm:TARGBImageWriterItem;
begin
  atm:=TARGBImageWriterItem.Create(pType,pProc);
  ARGBImageWriters.AddObject(pType,atm);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  ARGBImageReaders:=TARGBImageReaderList.Create;
  ARGBImageWriters:=TARGBImageWriterList.Create;

finalization
  ARGBImageReaders.Free;
  ARGBImageWriters.Free;

end.
