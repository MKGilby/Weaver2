{ -[Name]-------------------------------------------

     AnimationTimer classes

  -[Disclaimer]-------------------------------------

     See copyright.txt in project source directory.

     Written by Gilby/MKSZTSZ   Hungary, 2023

  -[Description]------------------------------------

    Animation timer classes for Frame and Time based
    animations. These classes will show you which
    frame to draw in a given point of time, either
    by frame index or frame coordinates.

    It doesn't hold the texture, TAnimation will do
    that.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2023.12.13
//    * Initial creation from Animation2Unit
//  V1.01: Gilby - 2024.10.23
//    * BUGFIX: There was a hang when time based animation finished and there
//              was more time to use.

{$mode delphi}

unit AnimationTimerUnit;

interface

uses
  Classes, Lists, AnimationDataUnit;

type

  { TAnimationTimer }

  TAnimationTimer=class abstract
    constructor Create(iAnimationData:TBaseAnimationData);
    procedure Animate(pInterval:double=0);  virtual; abstract;
    procedure ResetFrameIndex; virtual;
    procedure LogData; virtual;
  private
    fWidth,fHeight:integer;
    fCurrentFrameIndex,
    fFrameAdvance,fEndFrame:integer;
    fFinished:boolean;
    fReverseAnim:boolean;
    fFrames:array of TRect;
    function fGetFrameCount:integer;
    procedure fSetReverseAnim(pNewValue:boolean);
    procedure fSetFrameIndex(pFrameIndex:integer);
    function fGetCurrentFrame:TRect;
    function fGetFrame(pIndex:integer):TRect;
  public
    Name:string;
    HotPointX:integer;
    HotPointY:integer;
    StartFrame:integer;
    RandomStart:boolean;
    Paused:boolean;
    PingPong:boolean;
    Looped:boolean;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property FrameCount:integer read fGetFrameCount;
    property ReverseAnim:boolean read fReverseAnim write fSetReverseAnim;
    property CurrentFrameIndex:integer read fCurrentFrameIndex write fSetFrameIndex;
    property Finished:boolean read fFinished;
    property CurrentFrame:TRect read fGetCurrentFrame;
    property Frames[index:integer]:TRect read fGetFrame;
  end;

  { TFrameBasedAnimationTimer }

  TFrameBasedAnimationTimer=class(TAnimationTimer)
    constructor Create(iAnimationData:TFrameBasedAnimationData);
    procedure Animate(pFrameUsed:double=1);  override;
    procedure ResetFrameIndex; override;
    procedure LogData; override;
  private
    fFrameDelayCount,
    fLoopDelayCount,
    fFrameDelay,
    fLoopDelay:integer;
    procedure fSetFrameDelay(pValue:integer);
    procedure fSetLoopDelay(pValue:integer);
    procedure AnimateOneFrame;
  public
    property FrameDelay:integer read fFrameDelay write fSetFrameDelay;
    property LoopDelay:integer read fLoopDelay write fSetLoopDelay;
  end;

  { TTimeBasedAnimationTimer }

  TTimeBasedAnimationTimer=class(TAnimationTimer)
    constructor Create(iAnimationData:TTimeBasedAnimationData);
    procedure Animate(pTimeUsed:double);  override;
    procedure ResetFrameIndex; override;
    procedure LogData; override;
  private
    fFrameDelayCount,
    fLoopDelayCount,
    fFrameDelay,
    fLoopDelay,
    fFPS:double;
    fState:(sFrame,sLoop,sFinished);
    procedure fSetFPS(pValue:double);
    procedure fSetLoopDelay(pValue:double);
    procedure AnimateEx(var pTimeUsed:double);
    procedure AnimateOneFrame;
  public
    property FPS:double read fFPS write fSetFPS;
    property LoopDelay:double read fLoopDelay write fSetLoopDelay;
  end;

implementation

uses SysUtils, MKStream, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.01';

{ TAnimationTimer }

constructor TAnimationTimer.Create(iAnimationData:TBaseAnimationData);
var i:integer;
begin
  fCurrentFrameIndex:=0;
  Name:=iAnimationData.Name;
  fWidth:=iAnimationData.Width;
  fHeight:=iAnimationData.Height;
  StartFrame:=iAnimationData.StartFrame;
  Looped:=iAnimationData.Looped;
  RandomStart:=iAnimationData.RandomStart;
  ReverseAnim:=iAnimationData.ReverseAnim;
  Paused:=iAnimationData.Paused;
  PingPong:=iAnimationData.PingPong;
  HotPointX:=iAnimationData.HotPointX;
  HotPointY:=iAnimationData.HotPointY;

  fFrameAdvance:=0;
  fEndFrame:=0;
  fFinished:=true;
  fReverseAnim:=false;
  SetLength(fFrames,iAnimationData.FrameCount);
  for i:=0 to iAnimationData.FrameCount-1 do
    fFrames[i]:=iAnimationData.Frames[i];
end;

procedure TAnimationTimer.LogData;
var s:string;
begin
  Log.LogDebug('------ AnimationTimer data starts ------');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug('Type: Time-based');
  Log.LogDebug(Format('Hotpoint: %d, %d',[HotPointX,HotPointY]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[StartFrame]));

  Log.LogDebug(Format('CurrentFrameIndex=%d',[fCurrentFrameIndex]));
  Log.LogDebug(Format('EndFrame=',[fEndFrame]));
  Log.LogDebug(Format('FrameAdvance=',[fFrameAdvance]));
  s:='     ';
  if Looped then s[1]:='X';
  if RandomStart then s[2]:='X';
  if Paused then s[3]:='X';
  if PingPong then s[4]:='X';
  if ReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
end;

function TAnimationTimer.fGetFrameCount:integer;
begin
  result:=length(fFrames);
end;

procedure TAnimationTimer.fSetReverseAnim(pNewValue:boolean);
begin
  fReverseAnim:=pNewValue;
  if fReverseAnim then begin
    fFrameAdvance:=-1;
    fEndFrame:=0;
  end else begin
    fFrameAdvance:=1;
    fEndFrame:=FrameCount-1;
  end;
end;

procedure TAnimationTimer.fSetFrameIndex(pFrameIndex:integer);
begin
  if (pFrameIndex>=0) and (pFrameIndex<FrameCount) then
    fCurrentFrameIndex:=pFrameIndex;
end;

procedure TAnimationTimer.ResetFrameIndex;
begin
  if RandomStart then begin
    fCurrentFrameIndex:=random(FrameCount)
  end else
    if not fReverseAnim then begin
      fCurrentFrameIndex:=StartFrame;
    end else begin
      fCurrentFrameIndex:=FrameCount-1;
    end;
  if not fReverseAnim then begin
    fFrameAdvance:=1;
    fEndFrame:=FrameCount-1;
  end else begin
    fFrameAdvance:=-1;
    fEndFrame:=0;
  end;
  if FrameCount=1 then fFrameAdvance:=0;
  fFinished:=false;
end;

function TAnimationTimer.fGetCurrentFrame:TRect;
begin
  Result:=fFrames[fCurrentFrameIndex];
end;

function TAnimationTimer.fGetFrame(pIndex:integer):TRect;
begin
  if (pIndex>=0) and (pIndex<FrameCount) then
    Result:=fFrames[pIndex]
  else begin
    Result.Left:=0;Result.Top:=0;Result.Width:=1;Result.Height:=1;
  end;
end;

{ TFrameBasedAnimationTimer }

constructor TFrameBasedAnimationTimer.Create(iAnimationData:TFrameBasedAnimationData);
begin
  inherited Create(iAnimationData);
  fFrameDelay:=iAnimationData.FrameDelay;
  fLoopDelay:=iAnimationData.LoopDelay;
  ResetFrameIndex;
end;

procedure TFrameBasedAnimationTimer.Animate(pFrameUsed:double);
begin
  while pFrameUsed>=1 do begin
    AnimateOneFrame;
    pFrameUsed-=1;
  end;
end;

procedure TFrameBasedAnimationTimer.LogData;
begin
  inherited LogData;
  Log.LogDebug(Format('FrameDelay=%d',[fFramedelay]));
  Log.LogDebug(Format('LoopDelay=%d',[fLoopdelay]));
end;

procedure TFrameBasedAnimationTimer.fSetFrameDelay(pValue:integer);
begin
  fFrameDelay:=pValue;
  fFrameDelayCount:=fFrameDelay+1;
end;

procedure TFrameBasedAnimationTimer.fSetLoopDelay(pValue:integer);
begin
  fLoopDelay:=pValue;
  fLoopDelayCount:=fLoopDelay+1;
end;

procedure TFrameBasedAnimationTimer.ResetFrameIndex;
begin
  inherited ResetFrameIndex;
  fFrameDelayCount:=fFrameDelay+1;
  fLoopDelayCount:=fLoopDelay+1;
end;

procedure TFrameBasedAnimationTimer.AnimateOneFrame;
begin
  if not Paused and not Finished then dec(fFrameDelayCount);
  if fFrameDelayCount=0 then begin
    fFrameDelayCount:=fFrameDelay+1;
    if fCurrentFrameIndex=fEndFrame then begin
      if PingPong and (fFrameAdvance=1) then begin
        fFrameAdvance:=-1;
        fCurrentFrameIndex:=FrameCount-2;
        fEndFrame:=0;
      end else
        if Looped then begin
          dec(fLoopDelayCount);
          if fLoopDelayCount=0 then begin
            if not fReverseAnim then begin
              if not PingPong then
                fCurrentFrameIndex:=0
              else begin
                fCurrentFrameIndex:=1;
                fFrameAdvance:=1;
              end;
              fEndFrame:=FrameCount-1;
            end else begin
              fCurrentFrameIndex:=FrameCount-1;
              fEndFrame:=0;
            end;
            fLoopDelayCount:=fLoopDelay+1;
          end;
        end else
          fFinished:=true;
    end else
      inc(fCurrentFrameIndex,fFrameAdvance);
  end;
end;

{ TTimeBasedAnimationTimer }

constructor TTimeBasedAnimationTimer.Create(iAnimationData:TTimeBasedAnimationData);
begin
  inherited Create(iAnimationData);
  fFPS:=iAnimationData.FPS;
  fFrameDelay:=1/FPS;
  fLoopDelay:=iAnimationData.LoopDelay;
  fState:=sFrame;
  ResetFrameIndex;
end;

procedure TTimeBasedAnimationTimer.Animate(pTimeUsed:double);
begin
  if not Paused and not Finished then begin
    while pTimeUsed>0 do AnimateEx(pTimeUsed);
  end;
end;

procedure TTimeBasedAnimationTimer.LogData;
begin
  inherited LogData;
  Log.LogDebug(Format('Frame/sec: %.2f',[FPS]));
  Log.LogDebug(Format('Loopdelay: %.2f secs',[LoopDelay]));
end;

procedure TTimeBasedAnimationTimer.fSetFPS(pValue:double);
begin
  if pValue>0 then begin
    fFPS:=pValue;
    fFrameDelay:=1/fFPS;
  end else raise Exception.Create('Cannot set negative FPS for animation!')
end;

procedure TTimeBasedAnimationTimer.fSetLoopDelay(pValue:double);
begin
  fLoopDelay:=pValue;
  fLoopDelayCount:=fLoopDelay;
end;

procedure TTimeBasedAnimationTimer.ResetFrameIndex;
begin
  inherited ResetFrameIndex;
  fFrameDelayCount:=fFrameDelay;
  fLoopDelayCount:=fLoopDelay;
end;

procedure TTimeBasedAnimationTimer.AnimateEx(var pTimeUsed:double);
begin
  case fState of
    sFrame:begin
      if pTimeUsed>=fFrameDelayCount then begin
        pTimeUsed-=fFrameDelayCount;
        AnimateOneFrame;
        if fFinished then pTimeUsed:=0;
        fFrameDelayCount:=fFrameDelay;
      end else begin
        fFrameDelayCount-=pTimeUsed;
        pTimeUsed:=0;
      end;
    end;
    sLoop:begin
      if pTimeUsed>=fLoopDelayCount then begin
        pTimeUsed-=fLoopDelayCount;
        fLoopDelayCount:=fLoopDelay;
        fState:=sFrame;
        if not fReverseAnim then begin
          if not PingPong then
            fCurrentFrameIndex:=0
          else begin
            fCurrentFrameIndex:=1;
            fFrameAdvance:=1;
          end;
          fEndFrame:=FrameCount-1;
        end else begin
          fCurrentFrameIndex:=FrameCount-1;
          fEndFrame:=0;
        end;
      end else begin
        fLoopDelayCount-=pTimeUsed;
        pTimeUsed:=0;
      end;
    end;
  end;
end;

procedure TTimeBasedAnimationTimer.AnimateOneFrame;
begin
  if fCurrentFrameIndex=fEndFrame then begin
    if PingPong and (fFrameAdvance=1) then begin
      fFrameAdvance:=-1;
      fCurrentFrameIndex:=FrameCount-2;
      fEndFrame:=0;
    end else
      if Looped then begin
        fLoopDelayCount:=fLoopDelay;
        fState:=sLoop;
      end else begin
        fFinished:=true;
        fState:=sFinished;
      end;
  end else
    inc(fCurrentFrameIndex,fFrameAdvance);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

