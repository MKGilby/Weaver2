{ -[Name]-------------------------------------------

                 Font class for SDL2

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020

  --------------------------------------------------

  -[Description]------------------------------------

   TFont class usable with SDL2

  -------------------------------------------------- }

// Version info:
//   V1.00: Gilby - 2020.02.07
//      - Initial creation from FontUnit
//   V1.01: Gilby - 2020.06.25
//      + Added ARGBImageGSDReaderUnit to uses
//   V1.02: Gilby - 2021.05.17
//      + Added checking in Destroy if the classes are assigned.
//      * Class now holds a copy of the ARGBImage version of the font,
//        to use it with ARGBImages.
//   V1.03: Gilby - 2021.08.12
//      + Create from TARGBImage added.
//      * Original image is held in an ARGBImage to allow re-rendering coloured
//        fonts. So the ARGBImageGSDReaderUnit is no longer needed.
//   V1.04: Gilby - 2021.09.17
//      * Fix in fAddPadding when the letter order in the image is not
//        ascii ascending.
//   V1.05: Gilby - 2021.11.22
//      + Added logging of chars and charboxes.
//   V1.06: Gilby - 2022.05.01
//      * Followed change in TARGBImage.Copy
//   V1.07: Gilby - 2023.02.17
//      * BUGFix in creating from ARGBImage. Missed fRerender.
//   V1.08: Gilby - 2023.04.05
//      * You can specify chars to exclude from recoloring.
//   V1.09: Gilby - 2023.11.16
//      * You can specify if you want ARGBImage and/or Texture font.
//   V1.10: Gilby - 2024.08.21
//      * Following changes in FontDataUnit.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit Font2Unit;

interface

uses Classes, mk_sdl2, ARGBImageUnit;

const
  mjLeft=0;
  mjCenter=1;
  mjRight=2;

  FONT_CREATE_ARGB=1;
  FONT_CREATE_TEXTURE=2;
  FONT_CREATE_BOTH=FONT_CREATE_ARGB+FONT_CREATE_TEXTURE;

type
  TCharRect=record
    Left,Top,Width,Height:integer;
  end;

  { TFont }

  TFont=class
    constructor Create(iCreateFlags:integer=FONT_CREATE_BOTH); overload;
    constructor Create(iImage:TARGBImage;iCreateFlags:integer=FONT_CREATE_BOTH); overload;
    destructor Destroy; override;
    procedure OutText(Text:string;x,y,align:integer); overload;
    procedure OutText(Target:TARGBImage;text:string;x,y,align:integer); overload;
    procedure SetColorKey(r,g,b:byte);
    procedure SetAlpha(alpha:byte);
    procedure ClearColorKey;
    procedure SetColor(r,g,b:integer);
    procedure SetBackgroundColor(r,g,b:integer);
    procedure SetRecolorExcludeChars(chars:string);
    function TextWidth(text:string):integer;
    procedure LogChars;
  protected
    fOriginalImage:TARGBImage;
    fTexture:TStaticTexture;
    fARGBImage:TARGBImage;
    fOrigDefs, fDefs:array [0..255] of TCharRect;
    fIsColorKey:boolean;
    fCKr,fCKg,fCKb:integer;
    fR,fG,fB:integer;
    fAlpha:integer;
    fBR,fBG,fBB:integer;
    fSize:integer;
    fFixedWidth:integer;
    fName:string;
    fDontRecolorChars:string;
    fHeight:integer;
    fLetterSpace:integer;
    fSpaceSpace:integer;
    fCreateFlags:integer;
    // Adds one pixel padding between chars (modifies char definitions too)
    function fAddPadding(pImage:TARGBImage):TARGBImage;
    procedure fReRender;
    procedure fSetSize(newsize:integer);
  public
    property Size:integer read fSize write fSetSize;
    property LetterSpace:integer read fLetterSpace write fLetterSpace;
    property SpaceSpace:integer read fSpaceSpace write fSpaceSpace;
    property FixedWidth:integer read fFixedWidth write fFixedWidth;
    property Height:integer read fHeight;
    property Image:TStaticTexture read fTexture;
    property Name :string read fName write fName;
  end;

implementation

uses SDL2, Logger, MKToolBox, SysUtils;

const
  Fstr={$I %FILE%}+', ';
  Version='1.10';

// --------------------------------------------------------------- [ TFont ]---

constructor TFont.Create(iCreateFlags:integer);
begin
  fillchar(fDefs,sizeof(fDefs),0);
  fCreateFlags:=iCreateFlags;
  LetterSpace:=1;
  SpaceSpace:=10;
  fIsColorKey:=false;
  fFixedWidth:=0;
  fR:=-1;fG:=255;fB:=255;
  fBR:=0;fBG:=0;fBB:=0;
  fAlpha:=255;
  fSize:=1;
  fTexture:=nil;
  fOriginalImage:=nil;
end;

constructor TFont.Create(iImage:TARGBImage; iCreateFlags:integer);
var i:integer;
begin
  Create(iCreateFlags);
  fOriginalImage:=TARGBImage.Create(iImage.Width,iImage.Height);
  iImage.Copy(0,0,iImage.Width,iImage.Height,fOriginalImage);
  if Assigned(iImage.FontData) then begin
    for i:=0 to 255 do begin
      fOrigDefs[i].Left:=iImage.FontData.CharBoxes[i].Left;
      fOrigDefs[i].Top:=iImage.FontData.CharBoxes[i].Top;
      fOrigDefs[i].Width:=iImage.FontData.CharBoxes[i].Width;
      fOrigDefs[i].Height:=iImage.FontData.CharBoxes[i].Height;
      fDefs[i].Left:=fOrigDefs[i].Left;
      fDefs[i].Top:=fOrigDefs[i].Top;
      fDefs[i].Width:=fOrigDefs[i].Width;
      fDefs[i].Height:=fOrigDefs[i].Height;
    end;
  end;
  fOriginalImage:=fAddPadding(fOriginalImage);
  fHeight:=fOriginalImage.Height;
  fIsColorKey:=false;
  fFixedWidth:=0;
  fRerender;
end;

destructor TFont.Destroy;
begin
  if Assigned(fOriginalImage) then fOriginalImage.Free;
  if Assigned(fARGBImage) then fARGBImage.Free;
  if Assigned(fTexture) then fTexture.Free;
  inherited;
end;

procedure TFont.OutText(text:string;x,y,align:longint);
var i:longint;
begin
  if fCreateFlags or FONT_CREATE_TEXTURE<>0 then begin
    case align of
      mjLeft:;
      mjCenter:x-=TextWidth(text) div 2;
      mjRight:x-=TextWidth(text);
    end;
    for i:=1 to length(text) do with fDefs[ord(text[i])] do begin
      if fFixedWidth=0 then begin
        if text[i]<>#32 then begin
          PutTexturePart(x,y+Top,Left,Top,Width,Height,fTexture);
          x+=Width+fLetterSpace*fSize;
        end else
          x+=fSpaceSpace*fSize;
      end else begin
        if text[i]<>#32 then
          PutTexturePart(x+(fFixedWidth-Width)>>1,y+Top,Left,Top,Width,Height,fTexture);
        x+=fFixedWidth+fLetterSpace*fSize;
      end;
    end;
  end else
    raise Exception.Create('Font was not created with FONT_CREATE_TEXTURE flag!');
end;

procedure TFont.OutText(Target:TARGBImage;text:string;x,y,align:longint);
var i:longint;
begin
  if fCreateFlags or FONT_CREATE_ARGB<>0 then begin
    case align of
      mjLeft:;
      mjCenter:x-=TextWidth(text) div 2;
      mjRight:x-=TextWidth(text);
    end;
    for i:=1 to length(text) do with fDefs[ord(text[i])] do begin
      if fFixedWidth=0 then begin
        if text[i]<>#32 then begin
          fARGBImage.CopyTo(Left,Top,Width,Height,x,y+Top,Target,true);
          x+=Width+fLetterSpace*fSize;
        end else begin
          x+=fSpaceSpace*fSize;
        end;
      end else begin
        if text[i]<>#32 then
          fARGBImage.CopyTo(Left,Top,Width,Height,x+(fFixedWidth-Width)>>1,y+Top,Target,true);
        x+=fFixedWidth+fLetterSpace*fSize;
      end;
    end;
  end else
    raise Exception.Create('Font was not created with FONT_CREATE_ARGB flag!');
end;

procedure TFont.SetColorKey(r,g,b:byte);
begin
  fIsColorKey:=true;
  fCKr:=r;fCKg:=g;fCKb:=b;
  fReRender;
end;

procedure TFont.ClearColorKey;
begin
  fIsColorKey:=false;
  fReRender;
end;

procedure TFont.SetAlpha(alpha:byte);
begin
  fAlpha:=alpha;
  if SDL_SetTextureAlphaMod(fTexture.Texture,alpha)=-1 then
    Log.LogWarning('SetAlpha failed! (Font: '+fName+')');
end;

procedure TFont.SetColor(r,g,b:integer);
const Istr=Fstr+'TFont.SetColor';
begin
  if ((r<>fR) or (g<>fG) or (b<>fB)) then  // if any color component changed
    if (r>=0) and (r<=255) and  // and all of them are in 0..255 range
       (g>=0) and (g<=255) and
       (b>=0) and (b<=255) then begin
      fR:=r;fG:=G;fB:=b;
      fReRender;
    end else
      Log.LogWarning('Color is out of range! ('+inttostr(r)+', '+inttostr(g)+', '+inttostr(b)+')',Istr);
end;

procedure TFont.SetBackgroundColor(r,g,b:integer);
const Istr=Fstr+'TFont.SetBackgroundColor';
begin
  if ((r<>fBR) or (g<>fBG) or (b<>fBB)) then  // if any color component changed
    if (r>=0) and (r<=255) and  // and all of them are in 0..255 range
       (g>=0) and (g<=255) and
       (b>=0) and (b<=255) then begin
      fBR:=r;fBG:=G;fBB:=b;
      fReRender;
    end else
      Log.LogWarning('Color is out of range! ('+inttostr(r)+', '+inttostr(g)+', '+inttostr(b)+')',Istr);
end;

procedure TFont.SetRecolorExcludeChars(chars:string);
begin
  fDontRecolorChars:=chars;
end;

procedure TFont.fSetSize(newsize:integer);
const Istr=Fstr+'TFont.SetSize';
var i:integer;
begin
  if (newsize<>fsize) then
    if (newsize>0) and (newsize<=16) then begin
      fSize:=newsize;
      fReRender;
      fHeight:=fTexture.Height;
      for i:=0 to 255 do begin
        fDefs[i].Left:=fOrigDefs[i].Left*newsize;
        fDefs[i].Top:=fOrigDefs[i].Top*newsize;
        fDefs[i].Width:=fOrigDefs[i].Width*newsize;
        fDefs[i].Height:=fOrigDefs[i].Height*newsize;
      end;
    end else
      Log.LogWarning('Invalid size! ('+inttostr(newsize)+')',Istr);
end;

procedure TFont.fReRender;
var i:integer;
begin
  if Assigned(fTexture) then fTexture.Free;
  if Assigned(fARGBImage) then fARGBImage.Free;
  fARGBImage:=TARGBImage.Create(fOriginalImage.Width,fOriginalImage.Height);
  fOriginalImage.Copy(0,0,fOriginalImage.Width,fOriginalImage.Height,fARGBImage);
  if fR>-1 then fARGBImage.RecolorRGB(fR,fG,fB);
  // Copy back chars that shouldn't be recolored.
  for i:=0 to 255 do
    if pos(chr(i),fDontRecolorChars)>0 then with fOrigDefs[i] do
      fOriginalImage.CopyTo(Left,Top,Width,Height,Left,Top,fARGBImage);
  if fIsColorkey then begin
    fARGBImage.ReplaceColor(fCKr,fCKg,fCKb,0,0,0,0);
    fARGBImage.SetColorkey(fCKr,fCKg,fCKb);
  end;
  if fSize<>1 then fARGBImage.Magnify(fSize);
  if fCreateFlags and FONT_CREATE_TEXTURE<>0 then begin
    fTexture:=TStaticTexture.Create(fARGBImage);
    if SDL_SetTextureAlphaMod(fTexture.Texture,fAlpha)=-1 then
      Log.LogWarning('SetAlpha failed! (Font: '+fName+')');
  end;
  if fCreateFlags and FONT_CREATE_ARGB=0 then FreeAndNIL(fARGBImage);
end;

function TFont.fAddPadding(pImage:TARGBImage):TARGBImage;
var i,j,Count:integer;
begin
  Count:=0;
  for i:=0 to 255 do
    if fOrigDefs[i].Width>0 then inc(Count);
  Result:=TARGBImage.Create(pImage.Width+Count+1,pImage.Height);
  Result.Clear;
  j:=1;
  for i:=0 to 255 do
    if fOrigDefs[i].Width>0 then begin
      pImage.CopyTo(
        fOrigDefs[i].Left,     fOrigDefs[i].Top,
        fOrigDefs[i].Width,    fOrigDefs[i].Height,
        j, fOrigDefs[i].Top,
        Result);
      fOrigDefs[i].Left:=j;
      fDefs[i].Left:=fOrigDefs[i].Left*fSize;
      j+=fOrigDefs[i].Width+1;
    end;
  FreeAndNil(pImage);
end;

function TFont.TextWidth(text:string):integer;
var i,j:longint;
begin
  j:=0;
  if fFixedWidth<>0 then begin
    j:=(fFixedWidth+LetterSpace*fSize)*length(text);
  end else begin
    for i:=1 to length(text) do begin
      if text[i]=#32 then j+=SpaceSpace*fSize
                     else j+=fDefs[ord(text[i])].Width+LetterSpace*fSize;
    end;
  end;
  TextWidth:=j-LetterSpace*fSize;
end;

procedure TFont.LogChars;
var i:integer;
begin
  Log.LogStatus('Font logging starts...');
  Log.LogStatus('  Ascii        Charbox (left,top,width,height)');
  for i:=0 to 255 do if fOrigDefs[i].Width>0 then with fOrigDefs[i] do begin
    Log.LogStatus(Format('  chr(%3d) ''%s'' %d,%d,%d,%d',[i,chr(i),Left,Top,Width,Height]));
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
