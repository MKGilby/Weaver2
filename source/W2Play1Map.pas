unit W2Play1Map;

{$mode Delphi}

interface

uses
  Classes, SysUtils, mk_sdl2, W2Map, W2MapEntities;

type

  { TPlay1Map }

  TPlay1Map=class
    constructor Create(iMap:TMap);
    destructor Destroy; override;
    function Run:integer;
  private
    fMap:TMap;
    fBack:TTexture;
    procedure CreateBack;
  end;

implementation

uses
  sdl2, ARGBImageUnit, W2Shared;

const
  // Put these in the order of tiles in 'tiles.png'
  TILES:array[0..5] of string=('Color1','Color2','Color3','Floor','Zapper','Wall');

{ TPlay1Map }

constructor TPlay1Map.Create(iMap: TMap);
var x,y:integer;
begin
  fMap:=iMap;
  Entities:=TMapEntities.Create;
  CreateBack;
  BlockCount:=0;
  for y:=-1 to MAPHEIGHT do
    for x:=-1 to MAPWIDTH do begin
      case fMap.OrigTiles[x,y] of
        LOADED_TILE_FLOOR:fMap.Tiles[x,y]:=TILE_FLOOR;
        LOADED_TILE_WALL:fMap.Tiles[x,y]:=TILE_WALL or MOVEBLOCKALL;
        LOADED_TILE_BLOCK1:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR1,fMap));
        end;
        LOADED_TILE_BLOCK2:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR2,fMap));
        end;
        LOADED_TILE_BLOCK3:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR1 or COLOR2,fMap));
        end;
        LOADED_TILE_BLOCK4:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR3,fMap));
        end;
        LOADED_TILE_BLOCK5:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR1 or COLOR3,fMap));
        end;
        LOADED_TILE_BLOCK6:begin
          fMap.Tiles[x,y]:=MOVEBLOCKALL or TILE_BLOCK;
          Entities.Add(TBlock.Create(x,y,COLOR2 or COLOR3,fMap));
        end;
        LOADED_TILE_COLOR1:fMap.Tiles[x,y]:=TILE_COLOR1;
        LOADED_TILE_COLOR2:fMap.Tiles[x,y]:=TILE_COLOR2;
        LOADED_TILE_COLOR3:fMap.Tiles[x,y]:=TILE_COLOR3;
        LOADED_TILE_ZAPPER1:begin
          fMap.Tiles[x,y]:=TILE_ZAPPER;
          Entities.Add(TZapper.Create(x,y,'100'));
        end;
        LOADED_TILE_ZAPPER2:begin
          fMap.Tiles[x,y]:=TILE_ZAPPER;
          Entities.Add(TZapper.Create(x,y,'010'));
        end;
        LOADED_TILE_ZAPPER3:begin
          fMap.Tiles[x,y]:=TILE_ZAPPER;
          Entities.Add(TZapper.Create(x,y,'001'));
        end;
        LOADED_TILE_ZAPPER4:begin
          fMap.Tiles[x,y]:=TILE_ZAPPER;
          Entities.Add(TZapper.Create(x,y,'10'));
        end;
        LOADED_TILE_EXIT:begin
          fMap.Tiles[x,y]:=TILE_EXIT;
          Entities.Add(TExit.Create(x,y));
        end;
      end;
    end;
  Entities.Add(TPlayer.Create(fMap));
end;

destructor TPlay1Map.Destroy;
begin
  Entities.Free;
  fBack.Free;
  inherited Destroy;
end;

function TPlay1Map.Run:integer;
var pre,now:QWord;
begin
  Result:=0;
  ClearKeys;
  ClearControllerButtons;
  pre:=GetTickCount64;
  repeat
    now:=GetTickCount64;
    Entities.Move((now-pre)/1000);
    pre:=now;
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    MM.Fonts.OutText(#1'BLOCKS: '#0+inttostr(BlockCount),568,128,1);

    PutTexture(MAPLEFT-8,MAPTOP-8,fBack);
    Entities.Draw;

    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_X] then Result:=-1;
    if Terminate then Result:=-1;
  until Result<>0;

end;

procedure TPlay1Map.CreateBack;
var tmpI,tmpT:TARGBImage;i,j,ti:integer;s:string;

  function IndexOfTile(pName:string):integer;
  var i:integer;
  begin
    Result:=-1;
    for i:=Low(TILES) to High(TILES) do
      if pName=TILES[i] then begin
        Result:=i;
        Break;
      end;
  end;

begin
  tmpT:=MM.Images.ItemByName['Tiles'];
  if Assigned(tmpT) then begin
    tmpI:=TARGBImage.Create(MAPWIDTH*TILESIZE+16,MAPHEIGHT*TILESIZE+16);
    try
      tmpI.Clear;
      for j:=0 to MAPHEIGHT-1 do
        for i:=0 to MAPWIDTH-1 do begin
          case fMap.OrigTiles[i,j] of
            LOADED_TILE_ZAPPER1,LOADED_TILE_ZAPPER2,LOADED_TILE_ZAPPER3,LOADED_TILE_ZAPPER4:s:='Zapper';
            LOADED_TILE_WALL:s:='Wall';
            LOADED_TILE_COLOR1:s:='Color1';
            LOADED_TILE_COLOR2:s:='Color2';
            LOADED_TILE_COLOR3:s:='Color3';
            else s:='Floor';
          end;
          ti:=IndexOfTile(s);
          if (ti>-1) then
            tmpI.PutImagePart(i*TILESIZE+8,j*TILESIZE+8,ti*TILESIZE,0,TILESIZE,TILESIZE,tmpT);
        end;
      tmpI.HLine(0,0,tmpI.Width,MonoColor32);
      tmpI.HLine(1,1,tmpI.Width-2,MonoColor32);
      tmpI.VLine(tmpI.Width-1,0,tmpI.Height,MonoColor32);
      tmpI.VLine(tmpI.Width-2,1,tmpI.Height-2,MonoColor32);

      tmpI.VLine(5,6,tmpI.Height-10,MonoColor32);
      tmpI.VLine(6,7,tmpI.Height-12,MonoColor32);
      tmpI.HLine(5,tmpI.Height-6,tmpI.Width-11,MonoColor32);
      tmpI.HLine(6,tmpI.Height-7,tmpI.Width-13,MonoColor32);
      for i:=2 to tmpI.Width-3 do
        for j:=2 to tmpI.Height-3 do
          if ((i<5) or (i>tmpI.Width-6)) or ((j<5) or (j>tmpI.Height-6)) then
            if (i+j) mod 2=0 then tmpI.PutPixel(i,j,MonoColor32);
      fBack:=TStaticTexture.Create(tmpI);
    finally
      tmpI.Free;
    end;
  end;
end;

end.

