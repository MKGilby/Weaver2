{ -[Name]-------------------------------------------

              TextureAtlasGenerator class

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ   Hungary, 2020-2024

  -[Description]------------------------------------

    You can add TARGBImages with animations to it, and
    it creates a TextureAtlas from them, with the specified
    padding.

    The result is a TARGBImage with the transformed
    animation data.

    You can feed it to a TMediaManager and simply create
    sprites from MediaManager.Animations.ItemByName['Player'].SpawnAnimation.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.07.09
//     * Initial creation
//  V1.01: Gilby - 2020.07.10
//     + LoadImage added (Loads image from file. PNGs with animations.)
//  V1.02: Gilby - 2022.03.18
//     * Following changes in AnimationDataUnit
//     * AddImage now have an optional second parameter to specify one animation
//       name to add.
//  V1.03: Gilby - 2022.07.19
//     * Leaving out duplicate frames within one image.
//  V1.04: Gilby - 2023.05.12
//     * Changing Lists.TGenericList to fgl.TFPGObjectList.
//  V1.05: Gilby - 2023.12.05
//     * Added Crop method to crop image to the minimum size required.
//  V1.06: Gilby - 2023.12.14
//     * Following changes in AnimationDataUnit.
//  V1.06a: Gilby - 2023.12.14
//     * Bugfix in Addimage.
//  V1.07: Gilby - 2024.06.22
//     * Added full frame deduplication.

unit TextureAtlasGeneratorUnit;

{$mode delphi}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes, ARGBImageUnit, fgl;

type

  { TTextureLine }

  TTextureLine=class
    constructor Create(iTop,iHeight,iMaxWidth,iPadding:integer);
    function IsTherePlaceFor(pWidth:integer):boolean;
    function AddImage(pWidth:integer):boolean;
  private
    fTop:integer;
    fHeight:integer;
    fPadding:integer;
    fCurrentLeft:integer;
    fMaxWidth:integer;
  public
    property CurrentLeft:integer read fCurrentLeft;
    property Height:integer read fHeight;
    property Top:integer read fTop;
  end;

  { TTextureLines }

  TTextureLines=class(TFPGObjectList<TTextureLine>)
    function SearchLine(pWidth,pHeight:integer):TTextureLine;
    function CurrentTop:integer;
  end;

  { TTextureAtlasGenerator }

  TTextureAtlasGenerator=class
    constructor Create(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
    procedure AddImage(pImage:TARGBImage;pAnimationName:string='');
    procedure LoadImage(pFilename:string);
    procedure Crop;
  private
    fTextureAtlas:TARGBImage;
    fLines:TTextureLines;
    fPadding:integer;
    fFreeTextureAtlas:boolean;
    function fGetTextureAtlas:TARGBImage;
    function SearchForIdenticalFrame(fFrame:TARGBImage;out x:integer;out y:integer):boolean;
  public
    property TextureAtlas:TARGBImage read fGetTextureAtlas;
    property FreeImage:boolean read fFreeTextureAtlas write fFreeTextureAtlas;
  end;

implementation

uses sysutils, AnimationDataUnit, Logger, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.07';

constructor TTextureLine.Create(iTop,iHeight,iMaxWidth,iPadding:integer);
begin
  fTop:=iTop;
  fHeight:=iHeight;
  fMaxWidth:=iMaxWidth;
  fPadding:=iPadding;
  fCurrentLeft:=fPadding;
end;

function TTextureLine.IsTherePlaceFor(pWidth:integer):boolean;
begin
  Result:=fCurrentLeft+pWidth+fPadding<=fMaxWidth;
end;

function TTextureLine.AddImage(pWidth:integer):boolean;
begin
  if IsTherePlaceFor(pWidth) then begin
    Result:=true;
    fCurrentLeft+=pWidth+fPadding;
  end else Result:=false;
end;

function TTextureLines.SearchLine(pWidth,pHeight:integer):TTextureLine;
var i:integer;
begin
  for i:=0 to Count-1 do
    if (Self[i].Height=pHeight) and (Self[i].IsTherePlaceFor(pWidth)) then begin
      Result:=Self[i];
      exit;
    end;
  Result:=nil;
end;

function TTextureLines.CurrentTop:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if Self[i].Top+Self[i].Height>Result then Result:=Self[i].Top+Self[i].Height;
end;

constructor TTextureAtlasGenerator.Create(iWidth,iHeight,iPadding:integer);
begin
  fTextureAtlas:=TARGBImage.Create(iWidth,iHeight);
  fTextureAtlas.Clear(0);
  fLines:=TTextureLines.Create;
  fPadding:=iPadding;
  fFreeTextureAtlas:=true;
end;

destructor TTextureAtlasGenerator.Destroy;
begin
  FreeAndNil(fLines);
  if fFreeTextureAtlas then FreeAndNil(fTextureAtlas);
  inherited ;
end;

function TTextureAtlasGenerator.fGetTextureAtlas:TARGBImage;
begin
  Result:=fTextureAtlas;
//  fFreeTextureAtlas:=false;
end;

function TTextureAtlasGenerator.SearchForIdenticalFrame(fFrame: TARGBImage; out
  x: integer; out y: integer): boolean;
var tmp:TARGBImage;i,j:integer;
begin
  Result:=false;
  x:=-1;
  i:=0;
  while not Result and (i<fTextureAtlas.Animations.Count) do begin
    if (fTextureAtlas.Animations[i].Width=fFrame.Width) and
       (fTextureAtlas.Animations[i].Height=fFrame.Height) then begin
      tmp:=TARGBImage.Create(fFrame.Width,fFrame.Height);
      try
        for j:=0 to fTextureAtlas.Animations[i].FrameCount-1 do
          with fTextureAtlas.Animations[i].Frames[j] do begin
            fTextureAtlas.CopyTo(Left,Top,Width,Height,0,0,tmp);
            if fFrame.IsIdentical(tmp) then begin
              Result:=true;
              x:=Left;
              y:=top;
              break;
            end;
          end;
      finally
        tmp.Free;
      end;
    end;
    inc(i);
  end;
end;

procedure TTextureAtlasGenerator.AddImage(pImage: TARGBImage; pAnimationName: string);
var anim,frame:integer;atm:TBaseAnimationData;Line:TTextureLine;
  PrevFrames:TStringList;key:string;
  currentframe:TARGBImage;
  x,y:integer;
begin
  PrevFrames:=TStringList.Create;
  try
    for anim:=0 to pImage.Animations.Count-1 do begin
      if (pAnimationName='') or (pImage.Animations[anim].Name=pAnimationName) then begin
        if pImage.Animations[anim] is TFrameBasedAnimationData then
          atm:=TFrameBasedAnimationData(pImage.Animations[anim]).Clone(true)
        else if pImage.Animations[anim] is TTimeBasedAnimationData then
          atm:=TTimeBasedAnimationData(pImage.Animations[anim]).Clone(true);

        currentframe:=TARGBImage.Create(pImage.Animations[anim].Width,pImage.Animations[anim].Height);
        try
          for frame:=0 to pImage.Animations[anim].FrameCount-1 do begin
            key:=Format('%d,%d',[pImage.Animations[anim].Frames[frame].Left,pImage.Animations[anim].Frames[frame].Top]);
            if PrevFrames.Values[key]='' then begin
              with pImage.Animations[anim] do
                pImage.CopyTo(Frames[frame].Left,Frames[frame].Top,atm.Width,atm.Height,0,0,currentframe);
              if SearchForIdenticalFrame(currentframe,x,y) then begin
                atm.AddFrame(x,y);
                PrevFrames.Add(Format('%s=%d,%d',[key,x,y]));
              end else begin
                Line:=fLines.SearchLine(atm.Width,atm.Height);
                if Line=nil then begin
                  Line:=TTextureLine.Create(fLines.CurrentTop+fPadding,atm.Height,fTextureAtlas.Width,fPadding);
                  fLines.Add(Line);
                end;
                atm.AddFrame(Line.CurrentLeft,Line.Top);
                with pImage.Animations[anim] do
                  pImage.CopyTo(Frames[frame].Left,Frames[frame].Top,atm.Width,atm.Height,Line.CurrentLeft,Line.Top,fTextureAtlas);
                PrevFrames.Add(Format('%s=%d,%d',[key,Line.CurrentLeft,Line.Top]));
                Line.AddImage(atm.Width);
              end;
            end else begin
              key:=PrevFrames.Values[key];
              atm.AddFrame(strtoint(GetNthSegment(key,',',1)),strtoint(GetNthSegment(key,',',2)));
            end;
          end;
        finally
          currentframe.Free;
        end;
        fTextureAtlas.Animations.AddObject(atm.Name,atm);
      end;
    end;

  finally
    PrevFrames.Free;
  end;
end;

procedure TTextureAtlasGenerator.LoadImage(pFilename:string);
var image:TARGBImage;
begin
  image:=TARGBImage.Create(pFilename);
  AddImage(image);
  FreeAndNil(image);
end;

procedure TTextureAtlasGenerator.Crop;
var tmp:TARGBImage;
begin
  fTextureAtlas.Crop(0,0,0,0);
  tmp:=TARGBImage.Create(fTextureAtlas.Width+fPadding*2,fTextureAtlas.Height+fPadding*2);
  tmp.Clear(0);
  fTextureAtlas.CopyTo(0,0,fTextureAtlas.Width,fTextureAtlas.Height,fPadding,fPadding,tmp);

  fTextureAtlas.Resize(tmp.Width,tmp.Height);
  tmp.CopyTo(0,0,tmp.Width,tmp.Height,0,0,fTextureAtlas);
  tmp.Free;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

