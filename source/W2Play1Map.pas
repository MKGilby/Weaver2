{
  This file is part of the source code of Weaver 2.
  See "copyright.txt" for details.
}

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
    fPlayer:TPlayer;
    procedure CreateBack;
  end;

implementation

uses
  sdl2, ARGBImageUnit, W2Shared, MKStream, Math;

const
  // Put these in the order of tiles in 'tiles.png'
  TILES:array[0..11] of string=
    ('Color1','Color2','Color3','Floor','Zapper','Wall',
     'Button1','Button2','Button3','Button4','Button5','Button6');

{ TPlay1Map }

constructor TPlay1Map.Create(iMap: TMap);
var x,y:integer;
begin
  fMap:=iMap;
  Entities:=TMapEntities.Create;
  CreateBack;
  BlockCount:=0;
{  for y:=-1 to MAPHEIGHT do
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
        LOADED_TILE_TELEPORT1:begin
          fMap.Tiles[x,y]:=TILE_TELEPORT;
          Entities.Add(TTeleport.Create(x,y,0,fMap));
        end;
        LOADED_TILE_TELEPORT2:begin
          fMap.Tiles[x,y]:=TILE_TELEPORT;
          Entities.Add(TTeleport.Create(x,y,1,fMap));
        end;
        LOADED_TILE_DOOR1:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,1,fMap));
        end;
        LOADED_TILE_DOOR2:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,2,fMap));
        end;
        LOADED_TILE_DOOR3:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,3,fMap));
        end;
        LOADED_TILE_DOOR4:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,4,fMap));
        end;
        LOADED_TILE_DOOR5:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,5,fMap));
        end;
        LOADED_TILE_DOOR6:begin
          fMap.Tiles[x,y]:=TILE_DOOR or MOVEBLOCKALL;
          Entities.Add(TDoor.Create(x,y,6,fMap));
        end;
        LOADED_TILE_DOORBUTTON1:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,1,fMap));
        end;
        LOADED_TILE_DOORBUTTON2:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,2,fMap));
        end;
        LOADED_TILE_DOORBUTTON3:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,3,fMap));
        end;
        LOADED_TILE_DOORBUTTON4:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,4,fMap));
        end;
        LOADED_TILE_DOORBUTTON5:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,5,fMap));
        end;
        LOADED_TILE_DOORBUTTON6:begin
          fMap.Tiles[x,y]:=TILE_DOORBUTTON;
          Entities.Add(TDoorButton.Create(x,y,6,fMap));
        end;
      end;
    end;
  fPlayer:=TPlayer.Create(fMap);
  Entities.Add(fPlayer);
  for x:=0 to fMap.MonsterCount-1 do
    Entities.Add(TMonster.Create(fMap,x));}
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
//    Entities.Move((now-pre)/1000);
    pre:=now;
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,0,0,0,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
//    if Entities.IsPlayerCollidedWithEnemies(fPlayer.CollisionData) then
//      MM.Fonts.OutText('COLLIDE!',568,0,1);
//    MM.Fonts.OutText(#1'BLOCKS: '#0+inttostr(BlockCount),568,128,1);

    PutTexture(MAPLEFT-8,MAPTOP-8,fBack);
//    Entities.Draw;

    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_X] then Result:=-1;
    if Terminate then Result:=-1;
//    if fPlayer.State=psExit then Result:=1;
  until Result<>0;

end;

procedure TPlay1Map.CreateBack;
const
  DARK=$FF303030;
  MID=$FF585858;
  BRIGHT=$FF808080;
var tmpI,tmpT:TARGBImage;i,j,ti:integer;s:string;
    floor,zapper,wall:TARGBImage;

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

  function CreateFromBIN(const pFilename:string):TARGBImage;
  var grmap:pointer;Xs:TStream;x,y:integer;
  begin
    Result:=TARGBImage.Create(TILESIZE,TILESIZE);
    grmap:=getmem(TILESIZE*TILESIZE);
    try
      Xs:=MKStreamOpener.OpenStream(pFilename+'.bin');
      try
        Xs.Read(grmap^,min(Xs.Size,TILESIZE*TILESIZE));
      finally
        Xs.Free;
      end;
      for y:=0 to TILESIZE-1 do
        for x:=0 to TILESIZE-1 do
          Result.PutPixel(x,y,fMap.Gradient.GetColorAt(byte((grmap+y*TILESIZE+x)^)/256));
    finally
      freemem(grmap);
    end;
  end;

begin
//  tmpT:=MM.Images.ItemByName['Tiles'];
//  if Assigned(tmpT) then begin
    floor:=CreateFromBIN('floor');
    zapper:=CreateFromBIN('zapper_floor');
    wall:=CreateFromBIN('wall');
    try
      tmpI:=TARGBImage.Create(MAPWIDTH*TILESIZE+16,MAPHEIGHT*TILESIZE+16);
      try
        tmpI.Clear;
        for j:=0 to MAPHEIGHT-1 do
          for i:=0 to MAPWIDTH-1 do begin
            tmpI.PutImage(i*TILESIZE+8,j*TILESIZE+8,floor);
            case fMap.OrigTiles[i,j] of
              LOADED_TILE_ZAPPER1,LOADED_TILE_ZAPPER2,LOADED_TILE_ZAPPER3,LOADED_TILE_ZAPPER4:begin
                s:='';
                tmpI.PutImage(i*TILESIZE+8,j*TILESIZE+8,zapper);
              end;
              LOADED_TILE_WALL:begin
                s:='';
                tmpI.PutImage(i*TILESIZE+8,j*TILESIZE+8,wall);
              end;
              LOADED_TILE_COLOR1:s:='Color1';
              LOADED_TILE_COLOR2:s:='Color2';
              LOADED_TILE_COLOR3:s:='Color3';
              LOADED_TILE_DOORBUTTON1:s:='Button1';
              LOADED_TILE_DOORBUTTON2:s:='Button2';
              LOADED_TILE_DOORBUTTON3:s:='Button3';
              LOADED_TILE_DOORBUTTON4:s:='Button4';
              LOADED_TILE_DOORBUTTON5:s:='Button5';
              LOADED_TILE_DOORBUTTON6:s:='Button6';
              otherwise begin
                s:='';
              end;
            end;
{            ti:=IndexOfTile(s)*TILESIZE;
            if (ti>-1) then
              tmpI.PutImagePart(i*TILESIZE+8,j*TILESIZE+8,ti mod tmpT.Width,ti div tmpT.Width*TILESIZE,TILESIZE,TILESIZE,tmpT);
}
          end;
        tmpI.HLine(0,0,tmpI.Width,BRIGHT);
        tmpI.HLine(1,1,tmpI.Width-2,BRIGHT);
        tmpI.VLine(0,1,tmpI.Height-1,DARK);
        tmpI.VLine(1,2,tmpI.Height-3,DARK);
        tmpI.VLine(tmpI.Width-1,0,tmpI.Height,BRIGHT);
        tmpI.VLine(tmpI.Width-2,1,tmpI.Height-2,BRIGHT);
        tmpI.HLine(0,tmpI.Height-1,tmpI.Width-1,DARK);
        tmpI.HLine(1,tmpI.Height-2,tmpI.Width-3,DARK);

        tmpI.VLine(5,6,tmpI.Height-10,BRIGHT);
        tmpI.VLine(6,7,tmpI.Height-12,BRIGHT);
        tmpI.HLine(5,5,tmpI.Width-10,DARK);
        tmpI.HLine(6,6,tmpI.Width-12,DARK);

        tmpI.HLine(5,tmpI.Height-6,tmpI.Width-11,BRIGHT);
        tmpI.HLine(6,tmpI.Height-7,tmpI.Width-13,BRIGHT);
        tmpI.VLine(tmpI.Width-6,5,tmpI.Height-11,DARK);
        tmpI.VLine(tmpI.Width-7,6,tmpI.Height-13,DARK);
        tmpI.Rectangle(2,2,tmpI.Width-4,tmpI.Height-4,MID);
        tmpI.Rectangle(3,3,tmpI.Width-6,tmpI.Height-6,MID);
        tmpI.Rectangle(4,4,tmpI.Width-8,tmpI.Height-8,MID);
        fBack:=TStaticTexture.Create(tmpI);
      finally
        tmpI.Free;
      end;
    finally
      floor.Free;
      zapper.Free;
      wall.Free;
    end;
//  end;
end;

end.

