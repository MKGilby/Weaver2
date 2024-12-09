// MKSZTSZ SDL2 Wrapper
// ------------------------------------------------------------------
// You can freely distribute the sources.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2017
// ------------------------------------------------------------------

// Version info:
//   V1.00 - 2017.03.10 - Gilby
//      * Initial creation from MK_SDL.
//      + Window  (TWindow).
//      + Texture (TTexture, TStaticTexture).
//      + Message Handling.
//      + Basic drawing (bar, rectangle, lines).
//      + Texture drawing to window.
//      * Every drawing function supports only the primary window yet.
//   V1.01 - 2017.05.25
//      + PutTexturePart added.
//   V1.02 - 2020.06.18
//      + TStreamingTexture added. Modify its .ARGBImage, then Update.
//   V1.02a - 2020.06.25
//      * Fix in TStaticTexture.Create (invalid rendeder).
//   V1.03 - 2020.10.02
//      + PutTexture(x,y,w,h,Texture) added.
//   V1.04 - 2020.10.09
//      + PutTexturePart(sx,sy,sw,sh,tx,ty,tw,th,Texture) added.
//   V1.05 - 2020.10.15
//      + Added MouseButton, contains last pressed mouse button number.
//   V1.06 - 2020.11.23
//      + CreateDoubleSized added. Will create a window twice the size specified
//        resolution with the specified logical resolution.
//   V1.07 - 2021.09.03
//      + LogicalWidth and LogicalHeight added to TWindow.
//   V1.08 - 2022.05.13
//      * StreamingTexture update uses SDL_LockTexture now.
//   V1.09 - 2023.03.19
//      + Added Terminate. Check this in every loop, if true, close the program.
//      + Added FlipNoLimit. This fLipping will not limit the fps and updates
//        the FPS counter. Will fully use one cpu core.
//   V1.10 - 2023.06.22
//      + Added FPS counter update to Flip.
//   V1.11 - 2023.07.14
//      + Added TWindow.CreateCustomSized.
//   V1.12 - 2023.07.21
//      + Added FindController.
//   V1.13 - 2023.07.26
//      + Added ControllerButtons array. It works like Keys array.
//   V1.14 - 2023.11.16
//      + Added Bar with Texture instead of color. (Draws bar tiled with texture.)
//   V1.14a - 2023.11.17
//      * Fixed Bar with Texture. Sometimes it missed the last column.
//   V1.15 - 2024.03.18
//      * Removed the hack to SDL_LockTexture. Using ctypes instead.
//   V1.15a - 2024.08.26
//      * Really removed the hack to SDL_LockTexture. :)

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit mk_sdl2;

interface

uses
  Classes, Windows, SDL2, ARGBImageUnit;

const 
  MouseX:word=0;
  MouseY:word=0;
  MouseButtonDown:boolean=false;
  MouseButton:integer=0;

  Terminate:boolean=false;

type

  { TWindow }

  TWindow=class
    constructor Create(Left,Top,Width,Height:integer;Title:string);
    constructor CreateDoubleSized(Left,Top,Width,Height:integer;Title:string);
    constructor CreateFullScreenBordered(Width,Height:integer;Title:string);
    constructor CreateCustomSized(Left,Top,Width,Height,LogicalWidth,LogicalHeight:integer;Title:string);
    destructor Destroy; override;
  private
    fLogicalWidth,fLogicalHeight:integer;
    fWindow:PSDL_Window;
    fRenderer:PSDL_Renderer;
    procedure fSetTitle(title:string);
  public
    property Window:PSDL_Window read fWindow;
    property Renderer:PSDL_Renderer read fRenderer;
    property Title:string write fSetTitle;
    property Width:integer read fLogicalWidth;
    property Height:integer read fLogicalHeight;
  end;

  TTexture=class
    constructor Create(iRenderer:PSDL_Renderer=nil);
    destructor Destroy; override;
  private
    fRenderer:PSDL_Renderer;
    fTexture:PSDL_Texture;
    fWidth,fHeight:integer;
  public
    property Texture:PSDL_Texture read fTexture;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
  end;

  TStaticTexture=class(TTexture)
    // from ARGBImage
    constructor Create(Source:TARGBImage;Renderer:PSDL_Renderer=nil); overload;
    // from file through MKStream
    constructor Create(Source:string;Renderer:PSDL_Renderer=nil); overload;
//    destructor Destroy; override;
  end;

  TStreamingTexture=class(TTexture)
    constructor Create(iWidth,iHeight:integer;Renderer:PSDL_Renderer=nil);
    destructor Destroy; override;
    procedure Update;
  private
    fARGBImage:TARGBImage;
  public
    property ARGBImage:TARGBImage read fARGBImage;
  end;

  TEventHandlerProc=function (Event:PSDL_Event):boolean of object;

const
  PrimaryWindow:TWindow=nil;

var
  keys : array[0..SDL_NUM_SCANCODES] of boolean;
  controllerbuttons : array[0..SDL_CONTROLLER_BUTTON_MAX] of boolean;
  FrameCount : UInt32;
  fps: integer;

// SDL System
  procedure HandleMessages;
  procedure Flip;
  procedure FlipNoLimit;
//  function ReadKeyEx(wait:boolean):char;
//  procedure ClearKeyBuffer;
  procedure ClearKeys;
  procedure ClearControllerButtons;
  function TimeLeft : UInt32;   // From Kichy's Oxygene spriteenginedemo...
  procedure SetFPS(value:uint32);
  function GetDesktopSize:TRect;
  procedure RegisterEventHandler(EventHandlerProc:TEventHandlerProc);
  procedure UnRegisterEventHandler(EventHandlerProc:TEventHandlerProc);

  // Draws a filled rectangle with the given color
  procedure Bar(x,y,w,h,r,g,b:integer;a:integer=255); overload;

  // Draws a filled rectangle, filled with the given texture.
  procedure Bar(x,y,w,h:integer;Texture:TTexture); overload;

  // Draws a rectangle with the given color.
  procedure Rectangle(x,y,w,h,r,g,b:integer;a:integer=255);

  // Draws a line with the given color.
  procedure Line(x1,y1,x2,y2,r,g,b:integer;a:integer=255);

  // Draws a horizontal line with the given color.
  procedure HLine(x1,y1,w,r,g,b:integer;a:integer=255);

  // Draws a vertical line with the given color.
  procedure VLine(x1,y1,h,r,g,b:integer;a:integer=255);

  // Draws the given texture at x,y.
  procedure PutTexture(x,y:integer;Texture:TTexture); overload;

  // Draws the given texture at x,y, resized to w*h logical pixels.
  procedure PutTexture(x,y,w,h:integer;Texture:TTexture); overload;

  // Draws a part of the given texture at x,y.
  procedure PutTexturePart(x,y,sx,sy,w,h:integer;Texture:TTexture); overload;

  // Draws a part of the given texture at tx,ty, resized to tw*th.
  procedure PutTexturePart(sx,sy,sw,sh,tx,ty,tw,th:integer;Texture:TTexture); overload;

  function FindController:PSDL_GameController;

implementation

uses SysUtils, Logger, ctypes;

const
  Fstr={$I %FILE%}+', ';
  Version='1.15a';

type
  TEventHandlers=array of TEventHandlerProc;

var
  Event : TSDL_Event;
//  KeyBuffer : array[0..254] of char;
//  KBiP,KBoP : byte;
  FullTime : UInt32;
  EventHandlers:TEventHandlers;
  prevTicks:integer;

// ------------------------------------------------------------ [ TWindow ] ---

constructor TWindow.Create(Left,Top,Width,Height:integer;Title:string);
begin
  fWindow:=SDL_CreateWindow(PChar(Title), Left, Top, Width, Height, {SDL_WINDOW_OPENGL}0);
  if fWindow=nil then raise Exception.Create('Could not create window!');
  fRenderer:=SDL_CreateRenderer(fWindow, -1, 0);
  if fRenderer=nil then raise Exception.Create('Could not create renderer!');
  if PrimaryWindow=nil then PrimaryWindow:=Self;
  fLogicalWidth:=Width;
  fLogicalHeight:=Height;
  FrameCount:=0;
  fps:=0;
  prevTicks:=SDL_GetTicks;
end;

constructor TWindow.CreateDoubleSized(Left,Top,Width,Height:integer;Title:string);
begin
  fWindow:=SDL_CreateWindow(PChar(Title), Left, Top, Width*2, Height*2, SDL_WINDOW_OPENGL);
  if fWindow=nil then raise Exception.Create('Could not create window!');
  fRenderer:=SDL_CreateRenderer(fWindow, -1, 0);
  if fRenderer=nil then raise Exception.Create('Could not create renderer!');
//  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');  // make the scaled rendering look smoother.
  SDL_RenderSetLogicalSize(fRenderer, Width, Height);
  if PrimaryWindow=nil then PrimaryWindow:=Self;
  fLogicalWidth:=Width;
  fLogicalHeight:=Height;
  FrameCount:=0;
  fps:=0;
  prevTicks:=SDL_GetTicks;
end;

constructor TWindow.CreateFullScreenBordered(Width,Height:integer;Title:string);
begin
  fWindow:=SDL_CreateWindow(PChar(Title),
    SDL_WINDOWPOS_UNDEFINED,
    SDL_WINDOWPOS_UNDEFINED,
    0, 0,
    SDL_WINDOW_FULLSCREEN_DESKTOP or SDL_WINDOW_OPENGL);
  if fWindow=nil then raise Exception.Create('Could not create window!');
  fRenderer:=SDL_CreateRenderer(fWindow, -1, 0);
  if fRenderer=nil then raise Exception.Create('Could not create renderer!');
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');  // make the scaled rendering look smoother.
  SDL_RenderSetLogicalSize(fRenderer, Width, Height);
  if PrimaryWindow=nil then PrimaryWindow:=Self;
  fLogicalWidth:=Width;
  fLogicalHeight:=Height;
  FrameCount:=0;
  fps:=0;
  prevTicks:=SDL_GetTicks;
end;

constructor TWindow.CreateCustomSized(Left,Top,Width,Height,
  LogicalWidth,LogicalHeight:integer; Title:string);
begin
  fWindow:=SDL_CreateWindow(PChar(Title), Left, Top, Width, Height, SDL_WINDOW_OPENGL);
  if fWindow=nil then raise Exception.Create('Could not create window!');
  fRenderer:=SDL_CreateRenderer(fWindow, -1, 0);
  if fRenderer=nil then raise Exception.Create('Could not create renderer!');
  SDL_RenderSetLogicalSize(fRenderer, LogicalWidth, LogicalHeight);
  if PrimaryWindow=nil then PrimaryWindow:=Self;
  fLogicalWidth:=LogicalWidth;
  fLogicalHeight:=LogicalHeight;
  FrameCount:=0;
  fps:=0;
  prevTicks:=SDL_GetTicks;
end;

destructor TWindow.Destroy;
begin
  SDL_DestroyRenderer(fRenderer);
  SDL_DestroyWindow(fWindow);
  if PrimaryWindow=Self then PrimaryWindow:=nil;
  inherited ;
end;

procedure TWindow.fSetTitle(title:string);
begin
  SDL_SetWindowTitle(fWindow, PChar(title));
end;

// ----------------------------------------------------------- [ TTexture ] ---

constructor TTexture.Create(iRenderer:PSDL_Renderer);
begin
  fRenderer:=iRenderer;
  if fRenderer=nil then fRenderer:=PrimaryWindow.Renderer;
end;

destructor TTexture.Destroy;
begin
  SDL_DestroyTexture(fTexture);
  inherited ;
end;

// ----------------------------------------------------- [ TStaticTexture ] ---

constructor TStaticTexture.Create(Source:TARGBImage;renderer:PSDL_Renderer);
begin
  inherited Create(Renderer);

  fWidth:=Source.Width;
  fHeight:=Source.Height;
  fTexture:=SDL_CreateTexture(fRenderer,
                              SDL_PIXELFORMAT_ARGB8888,
                              SDL_TEXTUREACCESS_STATIC,
                              fWidth, fHeight);

  if fTexture=nil then raise Exception.Create(Format('CreateTexture failed! (%s)',[SDL_GetError]));

  if SDL_UpdateTexture(fTexture, nil, Source.RawData, Source.Width * sizeof (Uint32))<>0 then
    raise Exception.Create(Format('UpdateTexture failed! (%s)',[SDL_GetError]));
  SDL_SetTextureBlendMode(fTexture, SDL_BLENDMODE_BLEND);
end;

constructor TStaticTexture.Create(Source:string;Renderer:PSDL_Renderer);
var RawPict:TARGBImage;
begin
  RawPict:=TARGBImage.Create;
  RawPict.ReadFile(Source);
  Create(RawPict,Renderer);
  FreeAndNil(RawPict);
end;

constructor TStreamingTexture.Create(iWidth,iHeight:integer;Renderer:PSDL_Renderer=nil);
begin
  inherited Create(Renderer);
  fWidth:=iWidth;
  fHeight:=iHeight;
  fARGBImage:=TARGBImage.Create(fWidth,fHeight);
  fTexture:=SDL_CreateTexture(fRenderer,
                              SDL_PIXELFORMAT_ARGB8888,
                              SDL_TEXTUREACCESS_STREAMING,
                              fWidth, fHeight);
  if fTexture=nil then raise Exception.Create(Format('CreateTexture failed! (%s)',[SDL_GetError]));
end;

procedure TStreamingTexture.Update;
var p:ppointer;pitch:pcint;
begin
//  SDL_UpdateTexture(fTexture, nil, fARGBImage.Rawdata, fWidth * sizeof (Uint32));
  new(p);
  new(pitch);
  try
    SDL_LockTexture(fTexture, nil, p{%H-}, pitch{%H-});
    move(fARGBImage.Rawdata^,p^^,fWidth*fHeight*sizeof(UInt32));
    SDL_UnLockTexture(fTexture);
  finally
    dispose(pitch);
    dispose(p);
  end;
end;

destructor TStreamingTexture.Destroy;
begin
  FreeAndNil(fARGBImage);
  inherited ;
end;

procedure PutTexture(x,y:integer;Texture:TTexture);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x;
  DestRect.y:=y;
  DestRect.w:=Texture.Width;
  DestRect.h:=Texture.Height;
  SDL_RenderCopy(PrimaryWindow.Renderer,
                 Texture.Texture,
                 nil,
                 @DestRect);

end;

procedure PutTexture(x,y,w,h:integer;Texture:TTexture);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x;
  DestRect.y:=y;
  DestRect.w:=w;
  DestRect.h:=h;
  SDL_RenderCopy(PrimaryWindow.Renderer,
                 Texture.Texture,
                 nil,
                 @DestRect);

end;

procedure PutTexturePart(x,y,sx,sy,w,h:integer;Texture:TTexture);
var SourceRect,DestRect:TSDL_Rect;
begin
  SourceRect.x:=sx;
  SourceRect.y:=sy;
  SourceRect.w:=w;
  SourceRect.h:=h;
  DestRect.x:=x;
  DestRect.y:=y;
  DestRect.w:=w;
  DestRect.h:=h;
  SDL_RenderCopy(PrimaryWindow.Renderer,
                 Texture.Texture,
                 @SourceRect,
                 @DestRect);
end;

procedure PutTexturePart(sx,sy,sw,sh,tx,ty,tw,th:integer;Texture:TTexture);
var SourceRect,DestRect:TSDL_Rect;
begin
  SourceRect.x:=sx;
  SourceRect.y:=sy;
  SourceRect.w:=sw;
  SourceRect.h:=sh;
  DestRect.x:=tx;
  DestRect.y:=ty;
  DestRect.w:=tw;
  DestRect.h:=th;
  SDL_RenderCopy(PrimaryWindow.Renderer,
                 Texture.Texture,
                 @SourceRect,
                 @DestRect);
end;

function FindController: PSDL_GameController;
var i,n:integer;
begin
  n:=SDL_NumJoysticks;
//  if n<0
  Result:=nil;
  for i:=0 to n-1 do
    if SDL_IsGameController(i) then
      Result:=SDL_GameControllerOpen(i);

end;

procedure Flip;
var i:integer;
begin
  i:=TimeLeft;
  FullTime+=i;
  while Timeleft>0 do SDL_Delay(TimeLeft);
  i:=SDL_GetTicks;
  if i-prevTicks>1000 then begin
    fps:=FrameCount;
    FrameCount:=0;
    prevTicks:=i;
  end;
  inc(FrameCount);
  SDL_RenderPresent(PrimaryWindow.Renderer);
end;

procedure FlipNoLimit;
var i:integer;
begin
  i:=SDL_GetTicks;
  if i-prevTicks>1000 then begin
    fps:=FrameCount;
    FrameCount:=0;
    prevTicks:=i;
  end;
  inc(FrameCount);
  SDL_RenderPresent(PrimaryWindow.Renderer);
end;

procedure HandleMessages;
var i:integer;EventHandled:boolean;
begin
  while SDL_PollEvent(@event) > 0 do begin
    i:=0;EventHandled:=false;
    if event.type_=SDL_MOUSEMOTION then begin
      MouseX:=Event.Motion.X;
      MouseY:=Event.Motion.Y;
    end;
    while (i<length(EventHandlers)) do begin
      if EventHandlers[i](@event) then begin
        EventHandled:=true;
        break;
      end;
      inc(i);
    end;
//    if EventHandled then Log.LogDebug('true') else Log.LogDebug('false');
    if not EventHandled then begin
      case event.type_ of
        SDL_QUITEV : begin
          Terminate:=true;
//          SDL_Quit;
        end;
        SDL_KEYDOWN: begin
          keys[Event.Key.keysym.scancode]:=true;
        end;
        SDL_KEYUP: begin
          keys[Event.Key.keysym.scancode]:=false;
        end;
        SDL_MOUSEMOTION: begin
          MouseX:=Event.Motion.X;
          MouseY:=Event.Motion.Y;
        end;
        SDL_MOUSEBUTTONDOWN:begin
          MouseButtonDown:=true;
          MouseButton:=Event.button.button;
        end;
        SDL_MOUSEBUTTONUP:begin
          MouseButtonDown:=false;
        end;
        SDL_CONTROLLERBUTTONDOWN:begin
          controllerbuttons[Event.cbutton.button]:=true;
        end;
        SDL_CONTROLLERBUTTONUP:begin
          controllerbuttons[Event.cbutton.button]:=false;
        end;
      end;
    end;
  end;
end;

procedure ClearKeys;
var i:integer;
begin
  for i:=0 to SDL_NUM_SCANCODES do keys[i]:=false;
end;

const next_time:UInt32=0;
const TICK_INTERVAL:UInt32=1000 div 40;

procedure ClearControllerButtons;
var i:integer;
begin
  for i:=0 to SDL_CONTROLLER_BUTTON_MAX do controllerbuttons[i]:=false;
end;

function TimeLeft : UInt32;   // From Kichy's Oxygene spriteenginedemo...
var
  now : cardinal;
begin
  now := SDL_GetTicks;
  if next_time <= now then begin
    next_time := now + TICK_INTERVAL;
    TimeLeft:=0;
    exit;
  end;
  TimeLeft := next_time - now;
end;

procedure SetFPS(value:uint32);
begin
  Tick_Interval:=1000 div value;
end;

function GetDesktopSize:TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0) ;
end;

function GetScreenSize:TRect;
var r:windows.TRect;
begin
  r.Left:=0;
  GetWindowRect(GetDesktopWindow(), r);
  Result.Top:=r.Top;
  Result.Left:=r.Left;
  Result.Right:=r.Right;
  Result.Bottom:=r.Bottom;
end;

procedure RegisterEventHandler(EventHandlerProc:TEventHandlerProc);
begin
  SetLength(EventHandlers,length(EventHandlers)+1);
  EventHandlers[length(EventHandlers)-1]:=EventHandlerProc;
end;

procedure UnRegisterEventHandler(EventHandlerProc:TEventHandlerProc);
var i,j:integer;
begin
  for i:=length(EventHandlers)-1 downto 0 do
    if @EventHandlers[i]=@EventHandlerProc then begin
      for j:=i+1 to length(EventHandlers)-1 do
        EventHandlers[j-1]:=EventHandlers[j];
      SetLength(EventHandlers,length(EventHandlers)-1);
    end;
end;

procedure Bar(x,y,w,h,r,g,b:integer;a:integer=255);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x;
  DestRect.y:=y;
  DestRect.w:=w;
  DestRect.h:=h;
  SDL_SetRenderDrawColor(PrimaryWindow.Renderer, r, g, b, a);
  SDL_RenderFillRect(PrimaryWindow.Renderer,@DestRect);
end;

procedure Bar(x,y,w,h:integer; Texture:TTexture);
var i,j:integer;
begin
  for j:=0 to (h div Texture.Height)-1 do begin
    i:=0;
    while i<(w div Texture.Width) do begin
      PutTexture(x+i*Texture.Width,y+j*Texture.Height,Texture);
      inc(i);
    end;
    if w mod Texture.Width>0 then
      PutTexturePart(x+i*Texture.Width,y+j*Texture.Height,0,0,w mod Texture.Width,Texture.Height,Texture);
  end;
  if h mod Texture.Height>0 then begin
    j:=h div Texture.Height;
    i:=0;
    while i<(w div Texture.Width) do begin
      PutTexturePart(x+i*Texture.Width,y+j*Texture.Height,0,0,Texture.Width,h mod Texture.Height,Texture);
      inc(i);
    end;
    if w mod Texture.Width>0 then
      PutTexturePart(x+i*Texture.Width,y+j*Texture.Height,0,0,w mod Texture.Width,h mod Texture.Height,Texture);
  end;
end;

procedure Rectangle(x,y,w,h,r,g,b:integer;a:integer=255);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x;
  DestRect.y:=y;
  DestRect.w:=w;
  DestRect.h:=h;
  SDL_SetRenderDrawColor(PrimaryWindow.Renderer, r, g, b, a);
  SDL_RenderDrawRect(PrimaryWindow.Renderer,@DestRect);
end;

procedure Line(x1,y1,x2,y2,r,g,b:integer;a:integer=255);
begin
  SDL_SetRenderDrawColor(PrimaryWindow.Renderer, r, g, b, a);
  SDL_RenderDrawLine(PrimaryWindow.Renderer,x1,y1,x2,y2);
end;

procedure HLine(x1,y1,w,r,g,b:integer;a:integer=255);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x1;
  DestRect.y:=y1;
  DestRect.w:=w;
  DestRect.h:=1;
  SDL_SetRenderDrawColor(PrimaryWindow.Renderer, r, g, b, a);
  SDL_RenderFillRect(PrimaryWindow.Renderer,@DestRect);
end;

procedure VLine(x1,y1,h,r,g,b:integer;a:integer=255);
var DestRect:TSDL_Rect;
begin
  DestRect.x:=x1;
  DestRect.y:=y1;
  DestRect.w:=1;
  DestRect.h:=h;
  SDL_SetRenderDrawColor(PrimaryWindow.Renderer, r, g, b, a);
  SDL_RenderFillRect(PrimaryWindow.Renderer,@DestRect);
end;

{procedure GetImage(x,y:word;Image:TImage);
begin
  if useSecondarySurface then
    Image.PutImagePart(0,0,x,y,Image.Surface.w,Image.Surface.h,SecondarySurfaceImage)
  else
    Image.PutImagePart(0,0,x,y,Image.Surface.w,Image.Surface.h,PrimarySurfaceImage);
end;

procedure PutImage(x,y:word;Image:TImage);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.PutImage(x,y,Image)
  else
    PrimarySurfaceImage.PutImage(x,y,Image);
end;

procedure PutImagePart(x,y,x1,y1,x2,y2:word;Image:TImage);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.PutImagePart(x,y,x1,y1,x2-x1+1,y2-y1+1,Image)
  else
    PrimarySurfaceImage.PutImagePart(x,y,x1,y1,x2-x1+1,y2-y1+1,Image)
end;

procedure PutImagePartWH(tx,ty,sx,sy,w,h:integer;Image:TImage);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.PutImagePart(tx,ty,sx,sy,w,h,Image)
  else
    PrimarySurfaceImage.PutImagePart(tx,ty,sx,sy,w,h,Image);
end;

procedure PutImage(x,y:word;name:String);
var atm:TImage;
begin
  atm:=TImage.Create(name);
  PutImage(x,y,atm);
  FreeAndNIL(atm);
end;

procedure SaveImage(image:PImage;filename:string);
var p:pchar;
begin
  p:=strAlloc(length(filename)+2);
  StrPCopy(p,filename);
  SDL_SaveBMP(image^._surface,p);
  StrDispose(p);
end;

function CreateGSDFromImage(Source:TImage):TStream;
var i,j:integer;d:dword;
begin
  Result:=TMemoryStream.Create;
  i:=Source.Surface.w;
  Result.Write(i,2);
  i:=Source.Surface.h;
  Result.Write(i,2);
  for j:=0 to Source.Surface.h-1 do
    for i:=0 to Source.Surface.w-1 do begin
      d:=Source.GetPixel(i,j);
      d:=(((d>>16) and $ff)*30+((d>>8) and $ff*59)+(d and $ff)*11) div 100;
      Result.Write(d,1);
    end;
end;

function CreateGSDFromImage(Source:string):TStream; overload;
var atm:TImage;
begin
  atm:=TImage.Create(Source);
  Result:=CreateGSDFromImage(atm);
  FreeAndNIL(atm);
end;

function CreateRawPictureFromImage(Source:TImage):TRawPicture;
var i,j:integer;d:dword;
begin
  Result:=TRawPicture.Create(Source.Surface.w,Source.Surface.h);
  for j:=0 to Source.Surface.h-1 do
    for i:=0 to Source.Surface.w-1 do begin
      d:=Source.GetPixel(i,j);
      move(d,(Result.Rawdata+(i+j*Source.Surface.w)*3)^,3);
    end;
end;

procedure ImageLine(x1,y1,x2,y2:integer;image:TImage);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.ImageLine(x1,y1,x2,y2,image)
  else
    PrimarySurfaceImage.ImageLine(x1,y1,x2,y2,image);
end;

procedure ClearScreen(r,g,b:byte);
begin
  if useSecondarySurface then
    SDL_FillRect(SecondarySurface,nil,SDL_MapRGB(SecondarySurface^.Format,r,g,b))
  else
    SDL_FillRect(PrimarySurface,nil,SDL_MapRGB(PrimarySurface^.Format,r,g,b));
end;

function GetPixel(x,y:integer):Uint32;
begin
  if useSecondarySurface then
    Result:=SecondarySurfaceImage.GetPixel(x,y)
  else
    Result:=PrimarySurfaceImage.GetPixel(x,y);
end;

procedure PutPixel(x,y:integer;r,g,b:byte);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.PutPixel(x,y,r,g,b)
  else
    PrimarySurfaceImage.PutPixel(x,y,r,g,b);
end;

procedure PutPixel(x,y:integer;Pixel:UInt32);
begin
  if useSecondarySurface then
    SecondarySurfaceImage.PutPixel(x,y,Pixel)
  else
    PrimarySurfaceImage.PutPixel(x,y,Pixel);
end;}

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

