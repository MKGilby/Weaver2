unit W2Shared;

{$mode Delphi}

interface

uses sdl2, MediaManagerUnit;

const
  WINDOWCAPTION='Weaver 2 V%s (%s)';
  WINDOWWIDTH=640;
  WINDOWHEIGHT=400;
  DATAFILENAME='Weaver2.data';

  TILEWIDTH=32;
  TILEHEIGHT=32;
  MAPWIDTH=15;
  MAPHEIGHT=12;
  MAPLEFT=(WINDOWWIDTH-(TILEWIDTH*MAPWIDTH)) div 2;
  MAPTOP=(WINDOWHEIGHT-(TILEHEIGHT*MAPHEIGHT)) div 2;

  LOADED_TILE_FLOOR=ord('.');
  LOADED_TILE_WALL=ord('#');
  LOADED_TILE_COLOR1=ord('A');
  LOADED_TILE_COLOR2=ord('B');
  LOADED_TILE_COLOR3=ord('C');
  LOADED_TILE_EXIT=ord('e');
  LOADED_TILE_ZAPPER1=ord('X');
  LOADED_TILE_ZAPPER2=ord('Y');
  LOADED_TILE_ZAPPER3=ord('Z');
  LOADED_TILE_ZAPPER4=ord('z');
  LOADED_TILE_BLOCK1=ord('1');
  LOADED_TILE_BLOCK2=ord('2');
  LOADED_TILE_BLOCK3=ord('3');
  LOADED_TILE_BLOCK4=ord('4');
  LOADED_TILE_BLOCK5=ord('5');
  LOADED_TILE_BLOCK6=ord('6');

  MOVEBLOCKFROMABOVE=$0100;
  MOVEBLOCKFROMBELOW=$0400;
  MOVEBLOCKFROMLEFT=$0800;
  MOVEBLOCKFROMRIGHT=$0200;
  MOVEBLOCKALL=MOVEBLOCKFROMABOVE or MOVEBLOCKFROMBELOW or MOVEBLOCKFROMLEFT or MOVEBLOCKFROMRIGHT;

  TILE_FLOOR=0;
  TILE_WALL=$1000;
  TILE_COLOR1=1;
  TILE_COLOR2=2;
  TILE_COLOR3=3;
  TILE_EXIT=4;
  TILE_ZAPPER=5;
  TILE_BLOCK=6;

  ZAPPERLIFECYCLE=3;
  MAXTIMESLICE=1/10;

{  BLOCKCOLOR1=$10;
  BLOCKCOLOR1MASK=$7FFFFFFF xor BLOCKCOLOR1;
  BLOCKCOLOR2=$20;
  BLOCKCOLOR2MASK=$7FFFFFFF xor BLOCKCOLOR2;
  BLOCKCOLOR3=$40;
  BLOCKCOLOR3MASK=$7FFFFFFF xor BLOCKCOLOR3;
  BLOCKCOLORS=BLOCKCOLOR1 or BLOCKCOLOR2 or BLOCKCOLOR3;

  COLOR1=$100;
  COLOR2=$200;
  COLOR3=$300;}



var
  Controller:PSDL_GameController;
  MM:TMediaManager;
  MonoColor32:uint32;
  MonoColorR,MonoColorG,MonoColorB:byte;

procedure LoadAssets;
procedure FreeAssets;

implementation

uses ARGBImageUnit, AnimationDataUnit, TextureAtlasGeneratorUnit;

procedure CreateAnim1(
            pSourceBlockAnimName,
            pTargetBlockAnimName,
            pTargetAnimName:string;
            pSteps:integer;
            pSourceImage:TARGBImage;
            pAtlas:TTextureAtlasGenerator);
var
  Target,TargetAnim:TARGBImage;
  grid:array[0..TILEWIDTH-1,0..TILEHEIGHT-1] of integer;
  i,j,x,y:integer;
  tmpA:TTimeBasedAnimationData;
begin
  grid[0,0]:=0;
  fillchar(grid,sizeof(grid),0);
  Target:=TARGBImage.Create(TILEWIDTH,TILEHEIGHT);
  try
    with pSourceImage.Animations.ItemByName[pTargetBlockAnimName].Frames[0] do
      pSourceImage.CopyTo(Left,Top,Width,Height,0,0,Target);

    TargetAnim:=TARGBImage.Create(TILEWIDTH*pSteps,TILEHEIGHT);
    try
      tmpA:=TTimeBasedAnimationData.Create(TILEWIDTH,TILEHEIGHT);
      tmpA.Name:=pTargetAnimName;
      tmpA.FPS:=pSteps;
      tmpA.Looped:=false;
      tmpA.StartFrame:=0;
      tmpA.RandomStart:=false;
      tmpA.Paused:=true;

      for i:=0 to pSteps-1 do begin
        if i=0 then
          with pSourceImage.Animations.ItemByName[pSourceBlockAnimName].Frames[0] do
            pSourceImage.CopyTo(Left,Top,Width,Height,0,0,TargetAnim)
        else
          TargetAnim.CopyTo((i-1)*TILEWIDTH,0,TILEWIDTH,TILEHEIGHT,i*TILEWIDTH,0,TargetAnim);
        for j:=0 to (TILEWIDTH*TILEHEIGHT div 2) div pSteps-1 do begin
          repeat
            x:=random(TILEWIDTH) div 2;
            y:=random(TILEHEIGHT);
          until grid[x,y]=0;
          grid[x,y]:=1;
          Target.CopyTo(x,y,1,1,i*TILEWIDTH+x,y,TargetAnim);
        end;
        tmpA.AddFrame(i*TILEWIDTH,0);
      end;
      TargetAnim.Animations.AddObject(pTargetAnimName,tmpA);
//      TargetAnim.WriteFile(pTargetAnimName+'.png','PNG');
      pAtlas.AddImage(TargetAnim);
    finally
      TargetAnim.Free;
    end;
  finally
    Target.Free;
  end;
end;

procedure CreateAnim2(
            pSourceBlockAnimName,
            pTargetBlockAnimName,
            pTargetAnimName:string;
            pSteps:integer;
            pSourceImage:TARGBImage;
            pAtlas:TTextureAtlasGenerator);
var
  Target,TargetAnim:TARGBImage;
  grid:array[0..TILEWIDTH-1,0..TILEHEIGHT-1] of integer;
  i,j,x,y:integer;
  tmpA:TTimeBasedAnimationData;
begin
  grid[0,0]:=0;
  fillchar(grid,sizeof(grid),0);
  Target:=TARGBImage.Create(TILEWIDTH,TILEHEIGHT);
  try
    with pSourceImage.Animations.ItemByName[pTargetBlockAnimName].Frames[0] do
      pSourceImage.CopyTo(Left,Top,Width,Height,0,0,Target);

    TargetAnim:=TARGBImage.Create(TILEWIDTH*pSteps,TILEHEIGHT);
    try
      tmpA:=TTimeBasedAnimationData.Create(TILEWIDTH,TILEHEIGHT);
      tmpA.Name:=pTargetAnimName;
      tmpA.FPS:=pSteps;
      tmpA.Looped:=false;
      tmpA.StartFrame:=0;
      tmpA.RandomStart:=false;
      tmpA.Paused:=true;

      for i:=0 to pSteps-1 do begin
        if i=0 then
          with pSourceImage.Animations.ItemByName[pSourceBlockAnimName].Frames[0] do
            pSourceImage.CopyTo(Left,Top,Width,Height,0,0,TargetAnim)
        else
          TargetAnim.CopyTo((i-1)*TILEWIDTH,0,TILEWIDTH,TILEHEIGHT,i*TILEWIDTH,0,TargetAnim);
        for j:=0 to (TILEWIDTH*TILEHEIGHT div 2) div pSteps-1 do begin
          repeat
            x:=random(TILEWIDTH) div 2+TILEWIDTH div 2;
            y:=random(TILEHEIGHT);
          until grid[x,y]=0;
          grid[x,y]:=1;
          Target.CopyTo(x,y,1,1,i*TILEWIDTH+x,y,TargetAnim);
        end;
        tmpA.AddFrame(i*TILEWIDTH,0);
      end;
      TargetAnim.Animations.AddObject(pTargetAnimName,tmpA);
//      TargetAnim.WriteFile(pTargetAnimName+'.png','PNG');
      pAtlas.AddImage(TargetAnim);
    finally
      TargetAnim.Free;
    end;

  finally
    Target.Free;
  end;
end;

procedure CreateAnim3(
            pSourceAnimName,
            pTargetAnimName:string;
            pSteps:integer;
            pSourceImage:TARGBImage;
            pAtlas:TTextureAtlasGenerator);
var
  TargetAnim:TARGBImage;
  grid:array[0..TILEWIDTH-1,0..TILEHEIGHT-1] of integer;
  i,j,x,y:integer;
  tmpA:TTimeBasedAnimationData;
begin
  grid[0,0]:=0;
  fillchar(grid,sizeof(grid),0);

  TargetAnim:=TARGBImage.Create(TILEWIDTH*pSteps,TILEHEIGHT);
  try
    tmpA:=TTimeBasedAnimationData.Create(TILEWIDTH,TILEHEIGHT);
    tmpA.Name:=pTargetAnimName;
    tmpA.FPS:=pSteps;
    tmpA.Looped:=false;
    tmpA.StartFrame:=0;
    tmpA.RandomStart:=false;
    tmpA.Paused:=true;

    for i:=0 to pSteps-1 do begin
      if i=0 then
        with pSourceImage.Animations.ItemByName[pSourceAnimName].Frames[0] do
          pSourceImage.CopyTo(Left,Top,Width,Height,0,0,TargetAnim)
      else
        TargetAnim.CopyTo((i-1)*TILEWIDTH,0,TILEWIDTH,TILEHEIGHT,i*TILEWIDTH,0,TargetAnim);
      for j:=0 to TILEWIDTH*TILEHEIGHT div pSteps-1 do begin
        repeat
          x:=random(TILEWIDTH);
          y:=random(TILEHEIGHT);
        until grid[x,y]=0;
        grid[x,y]:=1;
        TargetAnim.Putpixel(i*TILEWIDTH+x,y,0,0,0,0);
      end;
      tmpA.AddFrame(i*TILEWIDTH,0);
    end;
    TargetAnim.Animations.AddObject(pTargetAnimName,tmpA);
//    TargetAnim.WriteFile(pTargetAnimName+'.png','PNG');
    pAtlas.AddImage(TargetAnim);
  finally
    TargetAnim.Free;
  end;
end;

procedure LoadAssets;
var Atlas:TTextureAtlasGenerator;Sprites:TARGBImage;
begin
  MonoColor32:=$FFFF8000;  // Amber
//  MonoColor32:=$FF00FF00;  // Green
//  MonoColor32:=$FFFFFFFF;  // White

  MonoColorR:=(MonoColor32 and $FF0000)>>16;
  MonoColorG:=(MonoColor32 and $FF00)>>8;
  MonoColorB:=(MonoColor32 and $FF);
  MM:=TMediaManager.Create;
  MM.Load('tiles.png','Tiles');
  MM.Images.ItemByName['Tiles'].RecolorRGB(MonoColorR,MonoColorG,MonoColorB);
  Sprites:=TARGBImage.Create('sprites.png');
  try
//  MM.Load('sprites.png','Sprites');
//  MM.Images.ItemByName['Sprites'].RecolorRGB(MonoColorR,MonoColorG,MonoColorB);
    Atlas:=TTextureAtlasGenerator.Create(1024,1024,1);
    try
      Atlas.AddImage(Sprites);
      CreateAnim3('Block1','Block1Destroy',32,Sprites,Atlas);
      CreateAnim3('Block2','Block2Destroy',32,Sprites,Atlas);
      CreateAnim3('Block4','Block4Destroy',32,Sprites,Atlas);
      CreateAnim1('Block3','Block1','Block3to1',32,Sprites,Atlas);
      CreateAnim2('Block3','Block2','Block3to2',32,Sprites,Atlas);
      CreateAnim1('Block5','Block4','Block5to4',32,Sprites,Atlas);
      CreateAnim2('Block5','Block1','Block5to1',32,Sprites,Atlas);
      CreateAnim1('Block6','Block2','Block6to2',32,Sprites,Atlas);
      CreateAnim2('Block6','Block4','Block6to4',32,Sprites,Atlas);
      Atlas.Crop;
      Atlas.TextureAtlas.RecolorRGB(MonoColorR,MonoColorG,MonoColorB);
      MM.AddImage(Atlas.TextureAtlas,'Sprites');
      Atlas.FreeImage:=false;
//      Atlas.TextureAtlas.WriteFile('atlas.png','PNG');
    finally
      Atlas.Free
    end;
  finally
    Sprites.Free;
  end;
end;

procedure FreeAssets;
begin
  MM.Free;
end;

end.

