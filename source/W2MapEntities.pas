{
  This file is part of the source code of Weaver 2.
  See "copyright.txt" for details.
}

unit W2MapEntities;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  SysUtils, fgl, Animation2Unit, W2Map, CollisionChecker2Unit, Mask2Unit;

type

  { TMapEntity }

  TMapEntity=class
    constructor Create(ipX,ipY:integer);
    // Move entity by dx,dy tiles
    procedure MoveRel(iDeltaX,iDeltaY:integer);
    procedure Draw; virtual;
    procedure Move(pElapsedTime:double); virtual;
  private
    fpX,fpY:integer;
    fX,fY:integer;
    fCollisionData:PCollisionData;
    function fGetCollisionData:PCollisionData; virtual;
  public
    Enemy:boolean;
    property pX:integer read fpX;
    property pY:integer read fpY;
    property CollisionData:PCollisionData read fGetCollisionData;
  end;

  { TMapEntities }

  TMapEntities=class(TFPGObjectList<TMapEntity>)
    procedure Draw;
    procedure Move(pElapsedTime:double);
    function IsPlayerCollidedWithEnemies(pPlayerCollisionData:PCollisionData):boolean;
  private
    procedure MoveEx(pElapsedTime:double);
    function fGetEntityAt(x,y:integer):TMapEntity;
  public
    property EntityAt[x,y:integer]:TMapEntity read fGetEntityAt;
  end;

  { TBlock }

  TBlock=class(TMapEntity)
    constructor Create(ipX,ipY,iColor:integer;iMap:TMap);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
    // Block has been hit by player holding pColor
    procedure Hit(pColor:integer);
  private
    fColor:integer;
    fMap:TMap;
    fAnimation:TAnimation;
  end;

  { TZapper }

  TZapper=class(TMapEntity)
    constructor Create(ipX,ipY:integer;iProgram:string);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
  private
    fProgram:string;
    fPosition:integer;
    fTimeLeft:double;
    fAnimation:TAnimation;
    fZapperMask,fEmptyMask:TMask;
    function fGetCollisionData:PCollisionData; override;
  end;

  TPlayerExternalState=(psNone,psExit,psDead);

  { TPlayer }

  TPlayer=class(TMapEntity)
    constructor Create(iMap:TMap);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
  private
    fAnimation:TAnimation;  // a frame for each color
    fMap:TMap;
    fDir:integer;
    fWantedDir:integer;
    fColor:integer;
    fShield:double;
    fPixelMoveRemainingTime:double;
    fState:TPlayerExternalState;
    procedure Move1Pixel;
    function fGetCollisionData:PCollisionData; override;
  public
    property State:TPlayerExternalState read fState;
  end;

  { TExit }

  TExit=class(TMapEntity)
    constructor Create(ipX,ipY:integer);
    destructor Destroy; override;
    procedure Draw; override;
  private
    fAnimation:TAnimation;
  end;

  { TTeleport }

  TTeleport=class(TMapEntity)
    constructor Create(ipX,ipY,iGroup:integer;iMap:TMap);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
    procedure Hit;
  private
    fAnimation:TAnimation;
    fPairX,fPairY:integer;
    fCoolDown:double;
    fMap:TMap;
  public
    property PairX:integer read fPairX;
    property PairY:integer read fPairY;
  end;

  TMonsterMove1pxProc=procedure of object;

  { TEnemy }

  TEnemy=class(TMapEntity)
    constructor Create(iMap:TMap;iNumber:integer);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
  private
    fMap:TMap;
    fAnimation:TAnimation;
    fTimePerPixel:double;
    fDir:integer;
    fMoveProc:TMonsterMove1pxProc;
    fPixelMoveRemainingTime:double;
    fMasks:array of TMask;
    procedure Move1pxVertical;
    procedure Move1pxHorizontal;
    procedure Move1pxRoamer;
    function NewDirVertical(pCurrentDir:integer):integer;
    function NewDirHorizontal(pCurrentDir:integer):integer;
    function NewDirRoamer(pCurrentDir:integer):integer;
    function fGetCollisionData:PCollisionData; override;
  end;

  { TDoor }

  TDoor=class(TMapEntity)
    constructor Create(ipX,ipY,iColor:integer;iMap:TMap);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Open;
  private
    fColor:integer;
    fMap:TMap;
    fAnimation:TAnimation;
  end;

  { TDoorButton }

  TDoorButton=class(TMapEntity)
    constructor Create(ipX,ipY,iColor:integer;iMap:TMap);
    procedure Hit;
  private
    fPairX,fPairY:integer;
    fMap:TMap;
  end;

implementation

uses W2Shared, mk_sdl2, sdl2, Logger;

{ TMapEntity }
{$region /fold}

constructor TMapEntity.Create(ipX,ipY:integer);
begin
  fpX:=ipX;
  fpY:=ipY;
  fX:=fpX*TILESIZE;
  fY:=fpY*TILESIZE;
  Enemy:=false;
end;

procedure TMapEntity.MoveRel(iDeltaX,iDeltaY:integer);
begin
  inc(fpX,iDeltaX);
  if (fpX<0) then fpX:=0
  else if (fpX>=MAPWIDTH) then fpX:=MAPWIDTH-1;
  inc(fpY,iDeltaY);
  if (fpY<0) then fpY:=0
  else if (fpY>=MAPHEIGHT) then fpY:=MAPHEIGHT-1;
end;

procedure TMapEntity.Draw;
begin
  { Nothing to do, override if want draw something. }
end;

procedure TMapEntity.Move(pElapsedTime: double);
begin
  { Nothing to do, override if want to do something based on ellapsed time. }
end;

function TMapEntity.fGetCollisionData:PCollisionData;
begin
  { Nothing to do, override if want to provide collision data. }
  Result:=nil;
end;

{$endregion}

{ TMapEntities }
{$region /fold}

procedure TMapEntities.Draw;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Draw;
end;

procedure TMapEntities.Move(pElapsedTime: double);
begin
  // Feed only MAXTIMESLICE a time to entities.
  while pElapsedTime>MAXTIMESLICE do begin
    MoveEx(MAXTIMESLICE);
    pElapsedTime:=pElapsedTime-MAXTIMESLICE;
  end;
  MoveEx(pElapsedTime);
end;

function TMapEntities.IsPlayerCollidedWithEnemies(pPlayerCollisionData:PCollisionData):boolean;
var i:integer;
begin
  Result:=false;
  for i:=0 to Count-1 do
    Result:=Result or
      (Items[i].Enemy and
       TCollisionChecker.IsCollideNonZero(pPlayerCollisionData,Items[i].CollisionData));
end;

procedure TMapEntities.MoveEx(pElapsedTime:double);
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Move(pElapsedTime);
end;

function TMapEntities.fGetEntityAt(x,y:integer):TMapEntity;
var i:integer;
begin
  Result:=nil;
  for i:=0 to Count-1 do
    if (Items[i].pX=x) and (Items[i].pY=y) then begin
      Result:=Items[i];
      break;
    end;
end;
{$endregion}

{ TBlock }
{$region /fold}

constructor TBlock.Create(ipX,ipY,iColor:integer; iMap:TMap);
begin
  inherited Create(ipX,ipY);
  fColor:=iColor;
  fMap:=iMap;
  fAnimation:=MM.Animations.ItemByName[Format('Block%d',[fColor])].SpawnAnimation;
  inc(BlockCount);
end;

destructor TBlock.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBlock.Draw;
begin
  if Assigned(fAnimation) then fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
end;

procedure TBlock.Move(pElapsedTime:double);
begin
  if Assigned(fAnimation) then begin
    fAnimation.Animate(pElapsedTime);
    if fAnimation.Timer.Finished then begin
      FreeAndNil(fAnimation);
      if fColor>0 then
        fAnimation:=MM.Animations.ItemByName[Format('Block%d',[fColor])].SpawnAnimation
      else begin
        fMap.Tiles[fpX,fpY]:=TILE_FLOOR;
        dec(BlockCount);
      end;
    end;
  end;
end;

procedure TBlock.Hit(pColor:integer);
var s:string;
begin
  if (fColor and pColor)<>0 then begin
    FreeAndNil(fAnimation);
    // Start animation here
    s:='Block'+chr(fColor+48);
    if fColor=pColor then
      s:=s+'Destroy'
    else
      s:=s+'to'+chr(fColor xor pColor+48);
    fAnimation:=MM.Animations.ItemByName[s].SpawnAnimation;
    fAnimation.Name:=s;
    fAnimation.Timer.Paused:=false;
    fColor:=(fColor xor pColor) and fColor;
  end;
end;

{$endregion}

{ TZapper }
{$region /fold}

constructor TZapper.Create(ipX, ipY: integer; iProgram: string);
begin
  inherited Create(ipX,ipY);
  fProgram:=iProgram;
  if fProgram='' then fProgram:='01';
  fPosition:=1;
  fTimeLeft:=ZAPPERLIFECYCLE;
  fAnimation:=MM.Animations.ItemByName['Zapper'].SpawnAnimation;
  Enemy:=true;
  fZapperMask:=MM.Masks.ItemByName['ZapperMask0'];
  fEmptyMask:=MM.Masks.ItemByName['EmptyMask0'];
  fCollisionData:=TCollisionChecker.CreateCollisionData(fZapperMask,fX,fY);
end;

destructor TZapper.Destroy;
begin
  TCollisionChecker.DestroyCollisionData(fCollisionData);
  fAnimation.Free;
  inherited Destroy;
end;

procedure TZapper.Draw;
begin
  if (fProgram[fPosition]='1') and Assigned(fAnimation) then
    fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
end;

procedure TZapper.Move(pElapsedTime: double);
begin
  fAnimation.Animate(pElapsedTime);
  if fTimeLeft>pElapsedTime then begin
    fTimeLeft:=fTimeLeft-pElapsedTime;
  end else begin
    fTimeLeft:=ZAPPERLIFECYCLE-(pElapsedTime-fTimeLeft);
    inc(fPosition);
    if fPosition>length(fProgram) then fPosition:=1;
  end;
end;

function TZapper.fGetCollisionData:PCollisionData;
begin
  if fProgram[fPosition]='1' then
    fCollisionData._mask:=fZapperMask
  else
    fCollisionData._mask:=fEmptyMask;
  fCollisionData._x:=fX;
  fCollisionData._y:=fY;
  Result:=fCollisionData;
end;

{$endregion}

{ TPlayer }
{$region /fold}

constructor TPlayer.Create(iMap:TMap);
begin
  inherited Create(iMap.PlayerStartX,iMap.PlayerStartY);
  fMap:=iMap;
  fDir:=DIRECTION_NONE;
  fColor:=COLOR1;
  fAnimation:=MM.Animations.ItemByName['Ship'].SpawnAnimation;
  fShield:=3;
  fPixelMoveRemainingTime:=PLAYERTIMEPERPIXEL;
  fState:=psNone;
  fCollisionData:=TCollisionChecker.CreateCollisionData(MM.Masks.ItemByName['ShipMask0'],fX,fY);
end;

destructor TPlayer.Destroy;
begin
  TCollisionChecker.DestroyCollisionData(fCollisionData);
  fAnimation.Free;
  inherited Destroy;
end;

procedure TPlayer.Draw;
begin
  if trunc(fShield*40) mod 10<5 then
    fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP,fColor-1);
end;

procedure TPlayer.Move(pElapsedTime:double);
begin
  // If there are remaining shield time, decrease it.
  if fShield>0 then begin
    fShield:=fShield-pElapsedTime;
    if fShield<0 then fShield:=0;  // Handle underflow
  end;

  // While ellapsed time greater than remaining time until next pixel move, do pixel move.
  while pElapsedTime>=fPixelMoveRemainingTime do begin
    Move1Pixel;
    pElapsedTime:=pElapsedTime-fPixelMoveRemainingTime;
    fPixelMoveRemainingTime:=PLAYERTIMEPERPIXEL;
  end;
  fPixelMoveRemainingTime:=fPixelMoveRemainingTime-pElapsedTime;
end;

procedure TPlayer.Move1Pixel;
var newDir:integer;tile:integer;
begin
  fpX:=fX div TILESIZE;
  fpY:=fY div TILESIZE;
  newDir:=0;
  if keys[SDL_SCANCODE_UP] then begin
    if fx mod 32=0 then begin
      tile:=fMap.Tiles[pX,(fY+31) div TILESIZE-1];
      if (tile and BLOCKPLAYERMOVEFROMBELOW<>0) then begin
        fWantedDir:=DIRECTION_UP;
        newDir:=0;
        if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,(fY+31) div TILESIZE-1]).Hit(fColor);
      end else
        newDir:=DIRECTION_UP;
    end else newDir:=fDir;
  end else
  if keys[SDL_SCANCODE_RIGHT] then begin
    if fy mod 32=0 then begin
      tile:=fMap.Tiles[pX+1,pY];
      if (tile and BLOCKPLAYERMOVEFROMLEFT<>0) then begin
        fWantedDir:=DIRECTION_RIGHT;
        newDir:=0;
        if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX+1,pY]).Hit(fColor);
      end else
        newDir:=DIRECTION_RIGHT;
    end else newDir:=fDir;
  end else
  if keys[SDL_SCANCODE_DOWN] then begin
    if fx mod 32=0 then begin
      tile:=fMap.Tiles[pX,pY+1];
      if (tile and BLOCKPLAYERMOVEFROMABOVE<>0) then begin
        fWantedDir:=DIRECTION_DOWN;
        newDir:=0;
        if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,pY+1]).Hit(fColor);
      end else
        newDir:=DIRECTION_DOWN;
    end else newDir:=fDir;
  end else
  if keys[SDL_SCANCODE_LEFT] then begin
    if fy mod 32=0 then begin
      tile:=fMap.Tiles[(fX+31) div TILESIZE-1,pY];
      if (tile and BLOCKPLAYERMOVEFROMRIGHT<>0) then begin
        fWantedDir:=DIRECTION_LEFT;
        newDir:=0;
        if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[(fX+31) div TILESIZE-1,pY]).Hit(fColor);
      end else
        newDir:=DIRECTION_LEFT;
    end else newDir:=fDir;
  end else begin
    // No direction key pressed
    if (fDir=0) and (fWantedDir<>0) then fDir:=fWantedDir;
    case fdir of
      DIRECTION_UP:begin
        if fY mod 32=0 then begin
          tile:=fMap.Tiles[pX,(fY+31) div TILESIZE-1];
          if (tile and BLOCKPLAYERMOVEFROMBELOW<>0) then begin
            if (fMap.Tiles[pX,pY+1] and BLOCKPLAYERMOVEFROMABOVE<>0) then begin
              fWantedDir:=DIRECTION_UP;
              newDir:=0;
            end else newDir:=DIRECTION_DOWN;
            if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,(fY+31) div TILESIZE-1]).Hit(fColor);
          end else
            newDir:=DIRECTION_UP;
        end else newDir:=fDir;
      end;
      DIRECTION_RIGHT:begin
        if fX mod 32=0 then begin
          tile:=fMap.Tiles[pX+1,pY];
          if (tile and BLOCKPLAYERMOVEFROMLEFT<>0) then begin
            if (fMap.Tiles[(fX+31) div TILESIZE-1,pY] and BLOCKPLAYERMOVEFROMRIGHT<>0) then begin
              fWantedDir:=DIRECTION_RIGHT;
              newDir:=0;
            end else newDir:=DIRECTION_LEFT;
            if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX+1,pY]).Hit(fColor);
          end else
            newDir:=DIRECTION_RIGHT;
        end else newDir:=fDir;
      end;
      DIRECTION_DOWN:begin
        if fY mod 32=0 then begin
          tile:=fMap.Tiles[pX,pY+1];
          if (tile and BLOCKPLAYERMOVEFROMABOVE<>0) then begin
            if (fMap.Tiles[pX,(fY+31) div TILESIZE-1] and BLOCKPLAYERMOVEFROMBELOW<>0) then begin
              fWantedDir:=DIRECTION_DOWN;
              newDir:=0;
            end else newDir:=DIRECTION_UP;
            if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,pY+1]).Hit(fColor);
          end else
            newDir:=DIRECTION_DOWN;
        end else newDir:=fDir;
      end;
      DIRECTION_LEFT:begin
        if fX mod 32=0 then begin
          tile:=fMap.Tiles[(fX+31) div TILESIZE-1,pY];
          if (tile and BLOCKPLAYERMOVEFROMRIGHT<>0) then begin
            if (fMap.Tiles[pX+1,pY] and BLOCKPLAYERMOVEFROMLEFT<>0) then begin
              fWantedDir:=DIRECTION_LEFT;
              newDir:=0;
            end else newDir:=DIRECTION_RIGHT;
            if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[(fX+31) div TILESIZE-1,pY]).Hit(fColor);
          end else
            newDir:=DIRECTION_LEFT;
        end else newDir:=fDir;
      end;
    end;
  end;
  fDir:=newDir;
  case fDir of
    DIRECTION_UP:dec(fY);
    DIRECTION_RIGHT:inc(fX);
    DIRECTION_DOWN:inc(fY);
    DIRECTION_LEFT:dec(fX);
  end;
  if (fX mod 32=0) and (fY mod 32=0) then begin
    case fMap.Tiles[fX div TILESIZE,fY div TILESIZE] of
      TILE_COLOR1:fColor:=COLOR1;
      TILE_COLOR2:fColor:=COLOR2;
      TILE_COLOR3:fColor:=COLOR3;
      TILE_EXIT:if BlockCount=0 then fState:=psExit;
      TILE_TELEPORT:begin
                      fpX:=TTeleport(Entities.EntityAt[fX div TILESIZE,fY div TILESIZE]).PairX;
                      fpY:=TTeleport(Entities.EntityAt[fX div TILESIZE,fY div TILESIZE]).PairY;
                      fX:=fpX*TILESIZE;
                      fY:=fpY*TILESIZE;
                      TTeleport(Entities.EntityAt[fX div TILESIZE,fY div TILESIZE]).Hit;
                    end;
      TILE_DOORBUTTON:begin
                        TDoorButton(Entities.EntityAt[fX div TILESIZE,fY div TILESIZE]).Hit;
                      end;
    end;
  end;
end;

function TPlayer.fGetCollisionData:PCollisionData;
begin
  fCollisionData._x:=fX;
  fCollisionData._y:=fY;
  Result:=fCollisionData;
end;

{$endregion}

{ TExit }
{$region /fold}

constructor TExit.Create(ipX,ipY:integer);
begin
  Inherited Create(ipX,ipY);
  fAnimation:=MM.Animations.ItemByName['Exit'].SpawnAnimation;
end;

destructor TExit.Destroy;
begin
  fAnimation.Free;
end;

procedure TExit.Draw;
begin
  if BlockCount=0 then begin
    fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
  end;
end;

{$endregion}

{ TTeleport }
{$region /fold}

constructor TTeleport.Create(ipX,ipY,iGroup:integer; iMap:TMap);

  // Searches another teleport of same group and puts coords into fPairX/Y.
  procedure SearchPair(pLoadedTile:integer);
  var x,y:integer;
  begin
    Log.LogDebug(Format('Searching for value: %d',[pLoadedTile]));
    for y:=0 to iMap.Height-1 do
      for x:=0 to iMap.Width-1 do
        if (iMap.OrigTiles[x,y]=pLoadedTile) and ((x<>ipX) or (y<>ipY)) then begin
          fPairX:=x;
          fPairY:=y;
          Log.LogDebug('Found!');
          exit;
        end;
  end;

begin
  inherited Create(ipX,ipY);
  fMap:=iMap;
  iGroup:=iGroup and 1;
  if iGroup=0 then begin
    SearchPair(LOADED_TILE_TELEPORT1);
    fAnimation:=MM.Animations.ItemByName['Teleport1'].SpawnAnimation;
  end else begin
    SearchPair(LOADED_TILE_TELEPORT2);
    fAnimation:=MM.Animations.ItemByName['Teleport2'].SpawnAnimation;
  end;
  Log.LogDebug(Format('This: %d,%d    Pair: %d,%d',[ipX,ipY,fPairX,fPairY]));
  if (fPairY>ipY) or ((fPairY=ipY) and (fPairX>ipX)) then fAnimation.Timer.CurrentFrameIndex:=1;
  fAnimation.LogData;
  fCoolDown:=0;
end;

destructor TTeleport.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TTeleport.Draw;
begin
  if fCoolDown=0 then fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
end;

procedure TTeleport.Move(pElapsedTime:double);
begin
  fAnimation.Animate(pElapsedTime);
  if fCoolDown>0 then begin
    fCoolDown:=fCoolDown-pElapsedTime;
    if fCoolDown<0 then fCoolDown:=0;
  end;
  if fCoolDown>0 then
    fMap.Tiles[pX,pY]:=TILE_FLOOR
  else
    fMap.Tiles[pX,pY]:=TILE_TELEPORT;
end;

procedure TTeleport.Hit;
begin
  if fCoolDown=0 then begin
    fCoolDown:=TELEPORTCOOLDOWN;
    fMap.Tiles[pX,pY]:=TILE_FLOOR;
    TTeleport(Entities.EntityAt[fPairX,fPairY]).Hit;
  end;
end;


{$endregion}

{ TEnemy }
{$region /fold}

constructor TEnemy.Create(iMap:TMap; iNumber:integer);
var tmp:TMonsterData;i:integer;
begin
  fMap:=iMap;
  tmp:=fMap.Monsters[iNumber];
  inherited Create(tmp._x,tmp._y);
  case tmp._type of
    mtVertical:begin
      fMoveProc:=Move1pxVertical;
      fAnimation:=MM.Animations.ItemByName['VerticalEnemy'].SpawnAnimation;
      fDir:=NewDirVertical(DIRECTION_NONE);
    end;
    mtHorizontal:begin
      fMoveProc:=Move1pxHorizontal;
      fAnimation:=MM.Animations.ItemByName['HorizontalEnemy'].SpawnAnimation;
      fDir:=NewDirHorizontal(DIRECTION_NONE);
    end;
    mtRoamer:begin
      fMoveProc:=Move1pxRoamer;
      fAnimation:=MM.Animations.ItemByName['RoamerEnemy'].SpawnAnimation;
      fDir:=NewDirRoamer(DIRECTION_NONE);
    end;
  end;
  case tmp._speed of
    msSlow:fTimePerPixel:=1/(TILESIZE*1.5);
    msFast:fTimePerPixel:=1/(TILESIZE*3);
  end;
  SetLength(fMasks,fAnimation.Timer.FrameCount);
  for i:=0 to fAnimation.Timer.FrameCount-1 do
    fMasks[i]:=MM.Masks.ItemByName[fAnimation.Name+inttostr(i)];
  fPixelMoveRemainingTime:=fTimePerPixel;
  fCollisionData:=TCollisionChecker.CreateCollisionData(fMasks[0],fX,fY);
  Enemy:=true;
end;

destructor TEnemy.Destroy;
begin
  TCollisionChecker.DestroyCollisionData(fCollisionData);
  fAnimation.Free;
  inherited Destroy;
end;

procedure TEnemy.Draw;
begin
  fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
end;

procedure TEnemy.Move(pElapsedTime:double);
begin
  fAnimation.Animate(pElapsedTime);
  // While ellapsed time greater than remaining time until next pixel move, do pixel move.
  while pElapsedTime>=fPixelMoveRemainingTime do begin
    fMoveProc;
    pElapsedTime:=pElapsedTime-fPixelMoveRemainingTime;
    fPixelMoveRemainingTime:=fTimePerPixel;
  end;
  fPixelMoveRemainingTime:=fPixelMoveRemainingTime-pElapsedTime;
end;

procedure TEnemy.Move1pxVertical;
begin
  if fY mod 32=0 then begin
    fpY:=fY div 32;
    fDir:=NewDirVertical(fDir);
  end;
  case fDir of
    DIRECTION_UP:dec(fY);
    DIRECTION_DOWN:inc(fY);
  end;
end;

procedure TEnemy.Move1pxHorizontal;
begin
  if fX mod 32=0 then begin
    fpX:=fX div 32;
    fDir:=NewDirHorizontal(fDir);
  end;
  case fDir of
    DIRECTION_LEFT:dec(fX);
    DIRECTION_RIGHT:inc(fX);
  end;
end;

procedure TEnemy.Move1pxRoamer;
begin
  if (fX mod 32=0) and (fy mod 32=0) then begin
    fpX:=fX div 32;
    fpY:=fY div 32;
    fDir:=NewDirRoamer(fDir);
  end;
  case fDir of
    DIRECTION_UP:dec(fY);
    DIRECTION_LEFT:dec(fX);
    DIRECTION_DOWN:inc(fY);
    DIRECTION_RIGHT:inc(fX);
  end;
end;

function TEnemy.NewDirVertical(pCurrentDir:integer):integer;
begin
  if pCurrentDir=DIRECTION_NONE then pCurrentDir:=VERTICALDIRS[random(length(VERTICALDIRS))];
  case pCurrentDir of
    DIRECTION_UP:begin
      // Is something blocking the way up?
      if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW<>0 then begin
        // Can move down instead?
        if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE=0 then
          Result:=DIRECTION_DOWN
        else
          // Do not move then.
          Result:=DIRECTION_NONE;
      end else
        Result:=DIRECTION_UP;
    end;
    DIRECTION_DOWN:begin
      // Is something blocking the way down?
      if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE<>0 then begin
        // Can move up instead?
        if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW=0 then
          Result:=DIRECTION_UP
        else
          // Do not move then.
          Result:=DIRECTION_NONE;
      end else
        Result:=DIRECTION_DOWN;
    end;
    otherwise Result:=DIRECTION_NONE;
  end;
end;

function TEnemy.NewDirHorizontal(pCurrentDir:integer):integer;
begin
  if pCurrentDir=DIRECTION_NONE then pCurrentDir:=HORIZONTALDIRS[random(length(HORIZONTALDIRS))];
  case pCurrentDir of
    DIRECTION_LEFT:begin
      // Is something blocking the way left?
      if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT<>0 then begin
        // Can move right instead?
        if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT=0 then
          Result:=DIRECTION_RIGHT
        else
          // Do not move then.
          Result:=DIRECTION_NONE;
      end else
        Result:=DIRECTION_LEFT;
    end;
    DIRECTION_RIGHT:begin
      // Is something blocking the way right?
      if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT<>0 then begin
        // Can move left instead?
        if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT=0 then
          Result:=DIRECTION_LEFT
        else
          // Do not move then.
          Result:=DIRECTION_NONE;
      end else
        Result:=DIRECTION_RIGHT;
    end;
    otherwise Result:=DIRECTION_NONE;
  end;
end;

function TEnemy.NewDirRoamer(pCurrentDir:integer):integer;
var dirs:string;i:integer;
begin
  if pCurrentDir=DIRECTION_NONE then pCurrentDir:=ALLDIRS[random(length(ALLDIRS))];
  case pCurrentDir of
    DIRECTION_UP:begin
      // Is something blocking the way up?
      if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW<>0 then begin  // Yes
        dirs:='0000';
        if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT=0 then dirs[2]:='1';
        if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT=0 then dirs[4]:='1';
        // Can go left or right?
        if dirs<>'0000' then begin  // Yes
          // Choose randomly
          repeat
            i:=random(4);
          until dirs[i+1]='1';
          Result:=ALLDIRS[i];
        end else begin  // No
          // Can go down?
          if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE=0 then
            Result:=DIRECTION_DOWN
          else
            Result:=DIRECTION_NONE;
        end;
      end else  // No
        Result:=DIRECTION_UP;
    end;
    DIRECTION_RIGHT:begin
      // Is something blocking the way right?
      if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT<>0 then begin  // Yes
        dirs:='0000';
        if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW=0 then dirs[1]:='1';
        if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE=0 then dirs[3]:='1';
        // Can go up or down?
        if dirs<>'0000' then begin  // Yes
          // Choose randomly
          repeat
            i:=random(4);
          until dirs[i+1]='1';
          Result:=ALLDIRS[i];
        end else begin  // No
          // Can go left?
          if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT=0 then
            Result:=DIRECTION_LEFT
          else
            Result:=DIRECTION_NONE;
        end;
      end else  // No
        Result:=DIRECTION_RIGHT;
    end;
    DIRECTION_DOWN:begin
      // Is something blocking the way down?
      if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE<>0 then begin  // Yes
        dirs:='0000';
        if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT=0 then dirs[2]:='1';
        if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT=0 then dirs[4]:='1';
        // Can go left or right?
        if dirs<>'0000' then begin  // Yes
          // Choose randomly
          repeat
            i:=random(4);
          until dirs[i+1]='1';
          Result:=ALLDIRS[i];
        end else begin  // No
          // Can go up?
          if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW=0 then
            Result:=DIRECTION_UP
          else
            Result:=DIRECTION_NONE;
        end;
      end else  // No
        Result:=DIRECTION_DOWN;
    end;
    DIRECTION_LEFT:begin
      // Is something blocking the way left?
      if fMap.Tiles[pX-1,pY] and BLOCKENEMYMOVEFROMRIGHT<>0 then begin  // Yes
        dirs:='0000';
        if fMap.Tiles[pX,pY-1] and BLOCKENEMYMOVEFROMBELOW=0 then dirs[1]:='1';
        if fMap.Tiles[pX,pY+1] and BLOCKENEMYMOVEFROMABOVE=0 then dirs[3]:='1';
        // Can go up or down?
        if dirs<>'0000' then begin  // Yes
          // Choose randomly
          repeat
            i:=random(4);
          until dirs[i+1]='1';
          Result:=ALLDIRS[i];
        end else begin  // No
          // Can go right?
          if fMap.Tiles[pX+1,pY] and BLOCKENEMYMOVEFROMLEFT=0 then
            Result:=DIRECTION_RIGHT
          else
            Result:=DIRECTION_NONE;
        end;
      end else  // No
        Result:=DIRECTION_LEFT;
    end;
    otherwise Result:=DIRECTION_NONE;
  end;
end;

function TEnemy.fGetCollisionData:PCollisionData;
begin
  fCollisionData._mask:=fMasks[fAnimation.Timer.CurrentFrameIndex];
  fCollisionData._x:=fX;
  fCollisionData._y:=fY;
  Result:=fCollisionData;
end;

{$endregion}

{ TDoor }
{$region /fold}

constructor TDoor.Create(ipX,ipY,iColor:integer; iMap:TMap);
begin
  inherited Create(ipX,ipY);
  fColor:=iColor;
  fMap:=iMap;
  fAnimation:=MM.Animations.ItemByName[Format('Door%d',[fColor])].SpawnAnimation;
end;

destructor TDoor.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TDoor.Draw;
begin
  if Assigned(fAnimation) then fAnimation.PutFrame(fX+MAPLEFT,fY+MAPTOP);
end;

procedure TDoor.Open;
begin
  FreeAndNil(fAnimation);
  fMap.Tiles[fpX,fpY]:=TILE_FLOOR;
end;

{$endregion}

{ TDoorButton }
{$region /fold}

constructor TDoorButton.Create(ipX,ipY,iColor:integer; iMap:TMap);

  // Searches door of same color and puts coords into fPairX/Y.
  procedure SearchDoor(pLoadedTile:integer);
  var x,y:integer;
  begin
    Log.LogDebug(Format('Searching for value: %d',[pLoadedTile]));
    for y:=0 to iMap.Height-1 do
      for x:=0 to iMap.Width-1 do
        if (iMap.OrigTiles[x,y]=pLoadedTile) and ((x<>ipX) or (y<>ipY)) then begin
          fPairX:=x;
          fPairY:=y;
          Log.LogDebug('Found!');
          exit;
        end;
  end;

begin
  inherited Create(ipX,ipY);
  fMap:=iMap;
  case iColor of
    1:SearchDoor(LOADED_TILE_DOOR1);
    2:SearchDoor(LOADED_TILE_DOOR2);
    3:SearchDoor(LOADED_TILE_DOOR3);
    4:SearchDoor(LOADED_TILE_DOOR4);
    5:SearchDoor(LOADED_TILE_DOOR5);
    6:SearchDoor(LOADED_TILE_DOOR6);
  end;
  Log.LogDebug(Format('This: %d,%d    Pair: %d,%d',[ipX,ipY,fPairX,fPairY]));
end;

procedure TDoorButton.Hit;
begin
  TDoor(Entities.EntityAt[fPairX,fPairY]).Open;
  fMap.Tiles[fpX,fpY]:=TILE_FLOOR;  // To prevent hitting again.
end;

{$endregion}

end.

