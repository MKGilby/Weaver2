{
  This file is part of the source code of Weaver 2.
  See "copyright.txt" for details.
}

unit W2Main;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, mk_sdl2;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
    fVersion:String;
  end;

implementation

uses Logger, MKStream, SDL2{, MKAudio}, MAD4MidLevelUnit,
  W2Shared, W2Map, W2Play1Map;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
var DesktopSize:TSDL_Rect;
    {$ifndef DEBUG}MAD4:TMAD4MidLevel;{$endif}
begin
  randomize;
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
  MKStreamOpener.AddDirectory('..\data',0);
{$ELSE}
  // Set logging level
  Log.SetLogLevel(llStatus);
  MKStreamOpener.AddDirectory('.',0);
  MAD4:=TMAD4MidLevel.Create(ExtractFilePath(paramstr(0))+DATAFILENAME);
  MKStreamOpener.AddOtherSource(MAD4,100);
{$ENDIF}

  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_GAMECONTROLLER);
  SDL_SetHint(SDL_HINT_RENDER_VSYNC,'1');
  SDL_GetDisplayBounds(0,@DesktopSize);
  if (DesktopSize.w>WINDOWWIDTH*2) and (DesktopSize.h>WINDOWHEIGHT*2) then begin
    fMainWindow:=TWindow.CreateDoubleSized(
      SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED,
      WINDOWWIDTH,
      WINDOWHEIGHT,
      Format(WINDOWCAPTION,[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])]));
  end else begin
    fMainWindow:=TWindow.Create(
      SDL_WINDOWPOS_CENTERED,
      SDL_WINDOWPOS_CENTERED,
      WINDOWWIDTH,
      WINDOWHEIGHT,
      Format(WINDOWCAPTION,[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])]));
  end;
{  fMainWindow:=TWindow.CreateFullScreenBordered(
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format(WINDOWCAPTION,[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])])
  );
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY,'0');}

  Controller:=FindController;

//  Init_Audio;

  LoadAssets;

  fVersion:=iVersion;
end;

destructor TMain.Destroy;
begin
  FreeAssets;
  fMainWindow.Free;
  SDL_Quit;
  inherited Destroy;
end;

procedure TMain.Run;
var Map:TMap;Play1Map:TPlay1Map;res,currentmap:integer;
begin
  currentmap:=4;
  Map:=TMap.Create;
  try
    repeat
      Map.LoadFromFile(currentmap);
      Play1Map:=TPlay1Map.Create(Map);
      try
        res:=Play1Map.Run;
        if res=1 then inc(currentmap);
      finally
        Play1Map.Free;
      end;
    until res<0;
  finally
    Map.Free;
  end;
end;

end.

