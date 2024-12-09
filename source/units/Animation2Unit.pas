{ -[Name]-------------------------------------------

     Animation class for SDL2

  -[Disclaimer]-------------------------------------

     You can freely distribute it.

     Written by Gilby/MKSZTSZ   Hungary, 2020

  -[Description]------------------------------------

    [to be written]

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.02.10
//     * Initial creation from AnimationUnit
//  V1.01: Gilby - 2020.05.28
//     * Reworked based on TAnimationData
//  V1.02: Gilby - 2020.06.25
//     * Following changes in AnimationDataUnit
//     - Name not needed when creating from TAnimationData
//  V1.03: Gilby - 2020.11.23
//     + PutFrame added. Puts the specified frame onto the main window.
//  V1.04: Gilby - 2021.03.03
//     - AddFrame removed it is inherited from TAnimationData
//     + HotPoint data is copied when created from TAnimationData
//  V1.05: Gilby - 2022.06.23
//     * From now TAnimation holds the entire Animation logic
//  V1.06: Gilby - 2022.07.08
//     + Added PutFramePart (with clipping)
//  V1.07: Gilby - 2023.12.13
//     * Frame handling is move to AnimationTimerUnit.
//     * Adopting time based animations.
//  V1.07a: Gilby - 2023.12.14
//     * PutFrame now uses HotPoint coordinates.

{$mode delphi}

unit Animation2Unit;

interface

uses
  Classes, Lists, mk_sdl2, AnimationDataUnit, AnimationTimerUnit;

type

  { TAnimation }

  TAnimation=class
    constructor Create(iTexture:TTexture;iAnimationData:TBaseAnimationData);
    destructor Destroy; override;

    procedure Animate(pTimeUsed:double=1);
    procedure PutFrame(pX,pY:integer;pFrameIndex:integer=-1);
    procedure PutFramePart(tX,tY,sX,sY,sW,sH:integer;pFrameIndex:integer=-1);
    procedure LogData;
  private
    fTexture:TTexture;
    fAnimationTimer:TAnimationTimer;
  public
    Name:string;
    property Texture:TTexture read fTexture;
    property Timer:TAnimationTimer read fAnimationTimer;
  end;

  { TAnimations }

  TAnimations=class(TNamedList<TAnimation>)
    procedure AnimateAll(pTimeUsed:double=1);
  end;

implementation

uses SysUtils, MKStream, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.07a';

// -----------------------------------------------------------[ TAnimation ]---

constructor TAnimation.Create(iTexture:TTexture;iAnimationData:TBaseAnimationData);
begin
  fTexture:=iTexture;
  if iAnimationData is TFrameBasedAnimationData then
    fAnimationTimer:=TFrameBasedAnimationTimer.Create(TFrameBasedAnimationData(iAnimationData))
  else if iAnimationData is TTimeBasedAnimationData then
    fAnimationTimer:=TTimeBasedAnimationTimer.Create(TTimeBasedAnimationData(iAnimationData));
end;

destructor TAnimation.Destroy;
begin
  if Assigned(fAnimationTimer) then fAnimationTimer.Free;
  inherited Destroy;
end;

procedure TAnimation.Animate(pTimeUsed:double);
begin
  fAnimationTimer.Animate(pTimeUsed);
end;

procedure TAnimation.PutFrame(pX, pY: integer; pFrameIndex: integer);
begin
  if (pFrameIndex>=0) and (pFrameIndex<fAnimationTimer.FrameCount) then
    with fAnimationTimer.Frames[pFrameIndex] do PutTexturePart(pX-fAnimationTimer.HotPointX,pY-fAnimationTimer.HotPointY,Left,Top,Width,Height,fTexture)
  else
    with fAnimationTimer.CurrentFrame do PutTexturePart(pX-fAnimationTimer.HotPointX,pY-fAnimationTimer.HotPointY,Left,Top,Width,Height,fTexture);
end;

procedure TAnimation.PutFramePart(tX, tY, sX, sY, sW, sH: integer;
  pFrameIndex: integer);
begin

  if not((pFrameIndex>=0) and (pFrameIndex<fAnimationTimer.FrameCount)) then pFrameIndex:=fAnimationTimer.CurrentFrameIndex;
  with fAnimationTimer.Frames[pFrameIndex] do begin
    // Is the source area valid?
    if (sW>0) and (sH>0) and (sX<Width) and (sX+sW>0) and (sY<Height) and (sY+sH>0) then begin
      // Still do some clipping
      if sX<0 then begin sW+=sX;sX:=0;end;
      if sX+sW>Width then sW:=Width-sX;
      if sY<0 then begin sH+=sY;sY:=0;end;
      if sY+sH>Height then sH:=Height-sY;
      PutTexturePart(tX,tY,Left+sX,Top+sY,sW,sH,fTexture);
    end;
  end;
end;

procedure TAnimation.LogData;
begin
  fAnimationTimer.LogData;
end;

{ TAnimations }

procedure TAnimations.AnimateAll(pTimeUsed:double);
var i:integer;
begin
  for i:=0 to Count-1 do Self[i].Animate(pTimeUsed);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

