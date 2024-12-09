{ -[Name]-------------------------------------------

      TAnimationData class. Base for TAnimation.

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ   Hungary, 2020-

  -[Description]------------------------------------

    Contains animation data, except image.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.03.16
//     * Initial creation from Animation2Unit
//  V1.01: Gilby - 2020.03.22
//     + Added StartFrame
//  V1.02: Gilby - 2020.04.01
//     + Added TAnimationDatas type
//  V1.03: Gilby - 2020.04.01
//     + Name property added
//     * FrameCount is now equals the count of added frames,
//       thus not needed at Create.
//  V1.04: Gilby - 2021.03.03
//     * Added HotPointX and HotPointY property. It defines the hot point of
//       the animation, that will be at the x,y point of the sprite (if used in
//       AnimatedSprite) (effectively shifting the image -HotPointX,-HotPointY
//       pixels).
//  V1.05: Gilby - 2021.04.22
//     + Added LogData
//  V1.05a: Gilby - 2021.11.09
//     - Removed MKStream from uses.
//  V1.06: Gilby - 2022.03.18
//     * TAnimationDatas is now TNamedList (to be searchable for names).
//  V1.07: Gilby - 2022.06.23
//     + Added ReverseAnim.
//  V1.08: Gilby - 2022.07.19
//     + Added animation flag constants
//     + Added logging of animation flags
//  V1.09: Gilby - 2023.12.13-14
//     * Reworked to support Time-based animations.
//     * Can be created directly from stream.
//     * Can be saved to stream.
//       - Frame-based version: 1
//       - Time-based version: 2
//  V1.10: Gilby - 2023.12.29
//     + Added LogData to base class.
//     + Added SkipFrames parameter to Clone.
//  V1.11: Gilby - 2024.03.14
//     + Added saving HotPoint data into stream.
//  V1.12: Gilby - 2024.05.03
//     + New combined format as version 5.
//     + Added saving PPS (pixels/second)

{$mode delphi}

unit AnimationDataUnit;

interface

uses
  Classes, Lists;

const
  AF_LOOPED=1;
  AF_RANDOMSTART=2;
  AF_PAUSED=4;
  AF_PINGPONG=8;
  AF_REVERSEANIM=16;
  AF_TIMEBASED=32;

type

  { TBaseAnimationData }

  TBaseAnimationData=class abstract
    constructor Create(iWidth,iHeight:integer);  virtual;
    procedure AddFrame(pX,pY:integer);
    procedure SavetoStream(pStream:TStream); virtual;
    procedure LogData; virtual; abstract;
  private
    function fGetFrame(index:integer):TRect;
  protected
    fWidth,fHeight:integer;
    fFrames:array of TRect;
    function fGetFrameCount:integer;
  public
    HotPointX:integer;
    HotPointY:integer;
    StartFrame:integer;
    Name:string;
    RandomStart:boolean;
    ReverseAnim:boolean;
    Paused:boolean;
    PingPong:boolean;
    Looped:boolean;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property FrameCount:integer read fGetFrameCount;
    property Frames[index:integer]:TRect read fGetFrame;
  end;

  { TFrameBasedAnimationData }

  TFrameBasedAnimationData=class(TBaseAnimationData)
    constructor Create(iWidth,iHeight:integer);  override;
    constructor CreateFromStreamLegacy(iStream:TStream);
    constructor CreateFromStreamV3(iStream:TStream);
    constructor CreateFromStreamV5(iStream:TStream);
    procedure SavetoStream(pStream:TStream); override;
    procedure LogData; override;
    function Clone(pSkipFrames:boolean=false):TFrameBasedAnimationData;
  public
    FrameDelay:integer;
    LoopDelay:integer;
  end;

  { TTimeBasedAnimationData }

  TTimeBasedAnimationData=class(TBaseAnimationData)
    constructor Create(iWidth,iHeight:integer);  override;
    constructor CreateFromStreamV2(iStream:TStream);
    constructor CreateFromStreamV4(iStream:TStream);
    constructor CreateFromStreamV5(iStream:TStream);
    procedure SavetoStream(pStream:TStream); override;
    procedure LogData; override;
    function Clone(pSkipFrames:boolean=false):TTimeBasedAnimationData;
  private
    fDefaultFPS:double;
  public
    FPS:double;  // to allow 1.5 frames per sec = 3 frames/2 sec
    PPS:double;
    LoopDelay:double;  // in seconds
    property DefaultFPS:double read fDefaultFPS;
  end;

  TAnimationDatas=TNamedList<TBaseAnimationData>;

implementation

uses SysUtils, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.12';

  NMD_STARTFRAME=1;
  NMD_FRAMEDELAY=2;
  NMD_FPS=2;
  NMD_LOOPDELAYT=4;
  NMD_LOOPDELAYF=4;
  NMD_HOTPOINT=8;
  NMD_PPS=16;



// -------------------------------------------------------[ TBaseAnimationData ]---

constructor TBaseAnimationData.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  HotPointX:=0;
  HotPointY:=0;
  StartFrame:=0;
  Looped:=false;
  RandomStart:=false;
  Paused:=false;
  PingPong:=false;
  SetLength(fFrames,0);
  Name:='';
end;

procedure TBaseAnimationData.AddFrame(pX,pY:integer);
begin
  SetLength(fFrames,length(fFrames)+1);
  with fFrames[length(fFrames)-1] do begin
    Left:=pX;
    Top:=pY;
    Width:=fWidth;
    Height:=fHeight;
  end;
end;

procedure TBaseAnimationData.SavetoStream(pStream:TStream);
begin
  raise Exception.Create('SaveToStream is not supported!');
end;

function TBaseAnimationData.fGetFrameCount:integer;
begin
  Result:=length(fFrames);
end;

function TBaseAnimationData.fGetFrame(index:integer):TRect;
begin
  if (index>=0) and (index<length(fFrames)) then
    Result:=fFrames[index]
  else begin
    Result.Left:=0;
    Result.Top:=0;
    Result.Width:=1;
    Result.Height:=1;
  end;
end;

{ TFrameBasedAnimationData }

constructor TFrameBasedAnimationData.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  FrameDelay:=0;
  LoopDelay:=0;
end;

constructor TFrameBasedAnimationData.CreateFromStreamLegacy(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  i:=0;
  iStream.Read(i,2);
  FrameDelay:=0;
  iStream.Read(FrameDelay,2);
  LoopDelay:=0;
  iStream.Read(LoopDelay,2);
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
  HotPointX:=0;
  HotPointY:=0;
end;

constructor TFrameBasedAnimationData.CreateFromStreamV3(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);
  i:=0;
  iStream.Read(i,2);
  HotPointX:=0;
  iStream.Read(HotPointX,2);
  HotPointY:=0;
  iStream.Read(HotPointY,2);

  FrameDelay:=0;
  iStream.Read(FrameDelay,2);
  LoopDelay:=0;
  iStream.Read(LoopDelay,2);
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
end;

constructor TFrameBasedAnimationData.CreateFromStreamV5(iStream:TStream);
var i,w,h,flags:integer;streampos:int64;
begin
  // To set default values, since not everything is loaded now.
  Create(0,0);
  // Version already consumed by caller
  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;
  // No need to check AF_TIMEBASED, it was checked by caller.

  // Name, width, height
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  // Framecount and frames
  i:=0;
  iStream.Read(i,2);
  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;

  // Non-mandatory data
  w:=0;
  iStream.Read(w,1);
  i:=0;
  iStream.Read(i,2);
  streampos:=iStream.Position;

  if (i and NMD_STARTFRAME<>0) then iStream.Read(StartFrame,2);
  if (i and NMD_FRAMEDELAY<>0) then iStream.Read(FrameDelay,2);
  if (i and NMD_LOOPDELAYF<>0) then iStream.Read(LoopDelay,2);
  if (i and NMD_HOTPOINT<>0) then begin
    iStream.Read(HotPointX,2);
    iStream.Read(HotPointY,2);
  end;
  iStream.Position:=streampos+w;
end;

procedure TFrameBasedAnimationData.SavetoStream(pStream:TStream);
var b:Byte;i:integer;
begin
  b:=5;
  pStream.Write(b,1);  // Version

  // Flags
  b:=0;
  if Looped then b:=b or AF_LOOPED;
  if RandomStart then b:=b or AF_RANDOMSTART;
  if Paused then b:=b or AF_PAUSED;
  if PingPong then b:=b or AF_PINGPONG;
  if ReverseAnim then b:=b or AF_REVERSEANIM;
  pStream.Write(b,1);

  // Name, width, height
  b:=length(Name);
  pStream.Write(b,1);
  if b>0 then pStream.Write(Name[1],b);
  pStream.Write(Width,2);
  pStream.Write(Height,2);

  // Framecount, frames
  pStream.Write(FrameCount,2);
  for i:=0 to FrameCount-1 do begin
    pStream.Write(Frames[i].Left,2);
    pStream.Write(Frames[i].Top,2);
  end;

  i:=0;b:=0;
  // Non-mandatory data
  if (StartFrame<>0) then begin i:=i or NMD_STARTFRAME;inc(b,2);end;
  if (FrameDelay<>0) then begin i:=i or NMD_FRAMEDELAY;inc(b,2);end;
  if (LoopDelay<>0) then begin i:=i or NMD_LOOPDELAYF;inc(b,2);end;
  if (HotPointX<>0) or (HotPointY<>0) then begin i:=i or NMD_HOTPOINT;inc(b,4);end;
  pStream.Write(b,1);
  pStream.Write(i,2);

  if (i and NMD_STARTFRAME<>0) then pStream.Write(StartFrame,2);
  if (i and NMD_FRAMEDELAY<>0) then pStream.Write(FrameDelay,2);
  if (i and NMD_LOOPDELAYF<>0) then pStream.Write(LoopDelay,2);
  if (i and NMD_HOTPOINT<>0) then begin
    pStream.Write(HotPointX,2);
    pStream.Write(HotPointY,2);
  end;
end;

procedure TFrameBasedAnimationData.LogData;
var i:integer;s:string;
begin
  Log.LogDebug('--- AnimationData logging starts ---');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug('Type: Frame-based');
  Log.LogDebug(Format('Hotpoint: %d, %d',[HotPointX,HotPointY]));
  Log.LogDebug(Format('Frame and loopdelay: %d, %d',[FrameDelay,LoopDelay]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[StartFrame]));
  s:='     ';
  if Looped then s[1]:='X';
  if RandomStart then s[2]:='X';
  if Paused then s[3]:='X';
  if PingPong then s[4]:='X';
  if ReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
  Log.LogDebug('Frames:');
  for i:=0 to length(fFrames)-1 do with fFrames[i] do
    Log.LogDebug(Format('  %d. x=%d, y=%d, w=%d, h=%d',[i,Left,Top,Width,Height]));
end;

function TFrameBasedAnimationData.Clone(pSkipFrames:boolean):TFrameBasedAnimationData;
var i:integer;
begin
  Result:=TFrameBasedAnimationData.Create(fWidth,fHeight);
  Result.Name:=Name;
  Result.FrameDelay:=FrameDelay;
  Result.LoopDelay:=LoopDelay;
  Result.StartFrame:=StartFrame;
  Result.Looped:=Looped;
  Result.RandomStart:=RandomStart;
  Result.Paused:=Paused;
  Result.PingPong:=PingPong;
  Result.ReverseAnim:=ReverseAnim;
  Result.HotPointX:=HotPointX;
  Result.HotPointY:=HotPointY;
  if not pSkipFrames then
    for i:=0 to FrameCount-1 do Result.AddFrame(Frames[i].Left,Frames[i].Top);
end;

{ TTimeBasedAnimationData }

constructor TTimeBasedAnimationData.Create(iWidth,iHeight:integer);
begin
  inherited Create(iWidth,iHeight);
  FPS:=1;
  PPS:=1;
  fDefaultFPS:=FPS;
  LoopDelay:=0;
end;

constructor TTimeBasedAnimationData.CreateFromStreamV2(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  // To set default values, since not everything is loaded now.
  Create(0,0);
  // Version already consumed by caller
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  i:=0;
  iStream.Read(i,2);
  FPS:=0;
  iStream.Read(FPS,sizeof(FPS));
  LoopDelay:=0;
  iStream.Read(LoopDelay,sizeof(LoopDelay));
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
end;

constructor TTimeBasedAnimationData.CreateFromStreamV4(iStream:TStream);
var i,w,h:integer;flags:byte;
begin
  // To set default values, since not everything is loaded now.
  Create(0,0);
  // Version already consumed by caller
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);
  i:=0;
  iStream.Read(i,2);
  HotPointX:=0;
  iStream.Read(HotPointX,2);
  HotPointY:=0;
  iStream.Read(HotPointY,2);

  FPS:=0;
  iStream.Read(FPS,sizeof(FPS));
  LoopDelay:=0;
  iStream.Read(LoopDelay,sizeof(LoopDelay));
  StartFrame:=0;
  iStream.Read(StartFrame,2);

  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;

  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;
end;

constructor TTimeBasedAnimationData.CreateFromStreamV5(iStream:TStream);
var i,w,h,flags:integer;streampos:int64;
begin
  // To set default values, since not everything is loaded now.
  Create(0,0);
  // Version already consumed by caller
  flags:=0;
  iStream.Read(flags,1);
  Looped:=(flags and AF_LOOPED)>0;
  RandomStart:=(flags and AF_RANDOMSTART)>0;
  Paused:=(flags and AF_PAUSED)>0;
  PingPong:=(flags and AF_PINGPONG)>0;
  ReverseAnim:=(flags and AF_REVERSEANIM)>0;
  // No need to check AF_TIMEBASED, it was checked by caller.

  // Name, width, height
  i:=0;
  iStream.Read(i,1);
  SetLength(Name,i);
  if i>0 then iStream.Read(Name[1],i);
  fWidth:=0;
  iStream.Read(fWidth,2);
  fHeight:=0;
  iStream.Read(fHeight,2);

  // Framecount and frames
  i:=0;
  iStream.Read(i,2);
  w:=0;h:=0;
  while i>0 do begin
    iStream.Read(w,2);
    iStream.Read(h,2);
    AddFrame(w,h);
    dec(i);
  end;

  // Non-mandatory data
  w:=0;
  iStream.Read(w,1);
  i:=0;
  iStream.Read(i,2);
  streampos:=iStream.Position;

  if (i and NMD_STARTFRAME<>0) then iStream.Read(StartFrame,2);
  if (i and NMD_FPS<>0) then iStream.Read(FPS,sizeof(FPS));
  if (i and NMD_LOOPDELAYT<>0) then iStream.Read(LoopDelay,sizeof(LoopDelay));
  if (i and NMD_HOTPOINT<>0) then begin
    iStream.Read(HotPointX,2);
    iStream.Read(HotPointY,2);
  end;
  if (i and NMD_PPS<>0) then iStream.Read(PPS,sizeof(PPS));
  iStream.Position:=streampos+w;
end;

procedure TTimeBasedAnimationData.SavetoStream(pStream:TStream);
var b:Byte;i:integer;
begin
  b:=5;
  pStream.Write(b,1);  // Version
  b:=AF_TIMEBASED;
  if Looped then b:=b or AF_LOOPED;
  if RandomStart then b:=b or AF_RANDOMSTART;
  if Paused then b:=b or AF_PAUSED;
  if PingPong then b:=b or AF_PINGPONG;
  if ReverseAnim then b:=b or AF_REVERSEANIM;
  pStream.Write(b,1);  // Flags

  b:=length(Name);
  pStream.Write(b,1);
  if b>0 then pStream.Write(Name[1],b);  // Name

  pStream.Write(Width,2);
  pStream.Write(Height,2);
  pStream.Write(FrameCount,2);
  for i:=0 to FrameCount-1 do begin
    pStream.Write(Frames[i].Left,2);
    pStream.Write(Frames[i].Top,2);
  end;

  i:=0;b:=0;
  if (StartFrame<>0) then begin i:=i or NMD_STARTFRAME;inc(b,2);end;
  if (FPS<>1) then begin i:=i or NMD_FPS;inc(b,sizeof(FPS));end;
  if (LoopDelay<>0) then begin i:=i or NMD_LOOPDELAYT;inc(b,sizeof(LoopDelay));end;
  if (HotPointX<>0) or (HotPointY<>0) then begin i:=i or NMD_HOTPOINT;inc(b,4);end;
  if (PPS<>1) then begin i:=i or NMD_PPS;inc(b,sizeof(PPS));end;
  pStream.Write(b,1);
  pStream.Write(i,2);

  if i and NMD_STARTFRAME<>0 then pStream.Write(StartFrame,2);
  if i and NMD_FPS<>0 then pStream.Write(FPS,sizeof(FPS));
  if i and NMD_LOOPDELAYT<>0 then pStream.Write(LoopDelay,sizeof(LoopDelay));
  if i and NMD_HOTPOINT<>0 then begin
    pStream.Write(HotPointX,2);
    pStream.Write(HotPointY,2);
  end;
  if i and NMD_PPS<>0 then pStream.Write(PPS,sizeof(PPS));
end;

procedure TTimeBasedAnimationData.LogData;
var s:string;i:integer;
begin
  Log.LogDebug('--- AnimationData logging starts ---');
  Log.LogDebug(Format('Name: %s',[name]));
  Log.LogDebug(Format('Dimensions: %dx%d',[fWidth,fHeight]));
  Log.LogDebug('Type: Time-based');
  Log.LogDebug(Format('Hotpoint: %d, %d',[HotPointX,HotPointY]));
  Log.LogDebug(Format('Frame/sec: %.2f',[FPS]));
  Log.LogDebug(Format('Pixel/sec: %.2f',[PPS]));
  Log.LogDebug(Format('Loopdelay: %.2f secs',[LoopDelay]));
  Log.LogDebug(Format('Framecount: %d',[length(fFrames)]));
  Log.LogDebug(Format('StartFrame: %d',[StartFrame]));
  s:='     ';
  if Looped then s[1]:='X';
  if RandomStart then s[2]:='X';
  if Paused then s[3]:='X';
  if PingPong then s[4]:='X';
  if ReverseAnim then s[5]:='X';
  Log.LogDebug('Looped ['+s[1]+']  RandomStart ['+s[2]+']  Paused ['+s[3]+']  PingPong ['+s[4]+']  ReverseAnim ['+s[5]+']');
  Log.LogDebug('Frames:');
  for i:=0 to length(fFrames)-1 do with fFrames[i] do
    Log.LogDebug(Format('  %d. x=%d, y=%d, w=%d, h=%d',[i,Left,Top,Width,Height]));
end;

function TTimeBasedAnimationData.Clone(pSkipFrames:boolean):TTimeBasedAnimationData;
var i:integer;
begin
  Result:=TTimeBasedAnimationData.Create(fWidth,fHeight);
  Result.Name:=Name;
  Result.FPS:=FPS;
  Result.LoopDelay:=LoopDelay;
  Result.StartFrame:=StartFrame;
  Result.Looped:=Looped;
  Result.RandomStart:=RandomStart;
  Result.Paused:=Paused;
  Result.PingPong:=PingPong;
  Result.ReverseAnim:=ReverseAnim;
  Result.HotPointX:=HotPointX;
  Result.HotPointY:=HotPointY;
  Result.PPS:=PPS;
  if not pSkipFrames then
    for i:=0 to FrameCount-1 do Result.AddFrame(Frames[i].Left,Frames[i].Top);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

{

  Stream format V5
  ----------------

    Size     Description
    byte     Version (5)
    byte     Animation flags as added up AF_ constants:
               1 - AF_LOOPED - Animation starts over when played all frames
               2 - AF_RANDOMSTART - Animation starts at a random frame instead of StartFrame
               4 - AF_PAUSED - Animation doesn't animate by default.
               8 - AF_PINGPONG - When reaching last frame, animation played backwards too.
              16 - AF_REVERSEANIM - Animation starts playing backwards.
              32 - AF_TIMEBASED - Animation is time based (instead of frame based).
                                  This means that some values has no meaning and
                                  cannot be included in the next part.
             (This is put to the beginning to help loader decide animation type!)
    string   Name
    word     Width
    word     Height
    word     FrameCount
    FrameCount*dword Frames
      (One frame is two words, specifying x and y coordinates on the textureatlas).
    byte     Non-mandatory data size without the index.
    word     Non-mandatory data index as added up NMD_ constants (see table below)
    ?        Non-mandatory data (only that has the corresponding bit set in index above)

    NMD_STARTFRAME    word
    NMD_FRAMEDELAY    word
    NMD_FPS           double
    NMD_LOOPDELAYT    double
    NMD_LOOPDELAYF    word
    NMD_HOTPOINT      2x word (x,y)
    NMD_PPS           double

  NMD_ constants and their meaning
  --------------------------------
      T*  F*   Value - Constant -     Default value when not included
      X   X    1 - NMD_STARTFRAME - 0
                   Start frame of the animation
          X    2 - NMD_FRAMEDELAY - 0
                   Wait this count of game loops before advancing animation one frame.
      X        2 - NMD_FPS - 1
                   Frame/second.
      X        4 - NMD_LOOPDELAYT - 0
                   Seconds to wait before looping animation.
          X    4 - NMD_LOOPDELAYF - 0
                   Wait this count of game loops before looping animation.
      X   X    8 - NMD_HOTPOINT - (0,0)
                   Hotpoint of the animation.
      X       16 - NMD_PPS - 1
                   Moving speed of the animation (pixel/second) which it seems
                   to move naturally (using the loaded FPS).
                   You can use this to adjust fps if sprite moves slower of faster.

  * The X marks if the value is valid for the animation type
    (T - Time based, F - Frame based)

}
