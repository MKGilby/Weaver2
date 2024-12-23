unit W2MapEntities;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  SysUtils, fgl, Animation2Unit, W2Map;

type

  { TMapEntity }

  TMapEntity=class
    constructor Create(ipX,ipY:integer);
    // Move entity by dx,dy tiles
    procedure MoveRel(iDeltaX,iDeltaY:integer);
    procedure Draw; virtual; abstract;
    procedure Move(pElapsedTime:double); virtual;
  private
    fpX,fpY:integer;
    fX,fY:integer;
  public
    property pX:integer read fpX;
    property pY:integer read fpY;
  end;

  { TMapEntities }

  TMapEntities=class(TFPGObjectList<TMapEntity>)
    procedure Draw;
    procedure Move(pElapsedTime:double);
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
  end;

  TPlayerExternalState=(psNone,psExit,psDead);

  { TPlayer }

  TPlayer=class(TMapEntity)
    constructor Create(iMap:TMap);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pElapsedTime:double); override;
    procedure Move1Pixel;
  private
    fAnimation:TAnimation;  // a frame for each color
    fMap:TMap;
    fDir:integer;
    fNewDir:integer;
    fColor:integer;
    fShield:double;
    fPixelMoveRemainingTime:double;
    fState:TPlayerExternalState;
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

procedure TMapEntity.Move(pElapsedTime: double);
begin
  { Nothing to do, override if want to do something based on ellapsed time. }
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
end;

destructor TZapper.Destroy;
begin
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
{$endregion}

{ TPlayer }
{$region /fold}

constructor TPlayer.Create(iMap:TMap);
begin
  inherited Create(iMap.PlayerStartX,iMap.PlayerStartY);
  fMap:=iMap;
  fDir:=0;
  fNewDir:=0;
  fColor:=COLOR1;
  fAnimation:=MM.Animations.ItemByName['Ship'].SpawnAnimation;
  fShield:=3;
  fPixelMoveRemainingTime:=TIMEPERPIXEL;
  fState:=psNone;
end;

destructor TPlayer.Destroy;
begin
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
    fPixelMoveRemainingTime:=TIMEPERPIXEL;
  end;
  fPixelMoveRemainingTime:=fPixelMoveRemainingTime-pElapsedTime;
end;

procedure TPlayer.Move1Pixel;
var
  k:array[1..4] of boolean;
  dirs:string;
  tile:integer;
begin
  // First we check keypresses for changing direction
  k[DIRECTION_UP]:=keys[SDL_SCANCODE_UP];
  k[DIRECTION_RIGHT]:=keys[SDL_SCANCODE_RIGHT];
  k[DIRECTION_DOWN]:=keys[SDL_SCANCODE_DOWN];
  k[DIRECTION_LEFT]:=keys[SDL_SCANCODE_LEFT];

  // Joystick simulation. If opposite direction keys are both pressed clear them.
  // (Since you cannot turn the joystick left/right or up/down in the same time.)
  if k[DIRECTION_UP] and k[DIRECTION_DOWN] then begin k[DIRECTION_UP]:=false;k[DIRECTION_DOWN]:=false;end;
  if k[DIRECTION_RIGHT] and k[DIRECTION_LEFT] then begin k[DIRECTION_RIGHT]:=false;k[DIRECTION_LEFT]:=false;end;

  // Check keys, store available directions in 'k'.
  k[DIRECTION_UP]:=k[DIRECTION_UP] and ((fDir in [0,DIRECTION_UP,DIRECTION_DOWN]) or ((fDir in [DIRECTION_RIGHT,DIRECTION_LEFT]) and (fX mod TILESIZE=0)));
  k[DIRECTION_RIGHT]:=k[DIRECTION_RIGHT] and ((fDir in [0,DIRECTION_RIGHT,DIRECTION_LEFT]) or ((fDir in [DIRECTION_UP,DIRECTION_DOWN]) and (fY mod TILESIZE=0)));
  k[DIRECTION_DOWN]:=k[DIRECTION_DOWN] and ((fDir in [0,DIRECTION_UP,DIRECTION_DOWN]) or ((fDir in [DIRECTION_RIGHT,DIRECTION_LEFT]) and (fX mod TILESIZE=0)));
  k[DIRECTION_LEFT]:=k[DIRECTION_LEFT] and ((fDir in [0,DIRECTION_RIGHT,DIRECTION_LEFT]) or ((fDir in [DIRECTION_UP,DIRECTION_DOWN]) and (fY mod TILESIZE=0)));

  // If no key pressed the last direction is forced.
  if not(k[DIRECTION_UP] or k[DIRECTION_RIGHT] or k[DIRECTION_DOWN] or k[DIRECTION_LEFT]) and (fDir in [1..4]) then k[fDir]:=true;

  fpX:=fX div TILESIZE;
  fpY:=fY div TILESIZE;
  dirs:='0000';
  if k[DIRECTION_UP] then begin
    tile:=fMap.Tiles[pX,(fY+31) div TILESIZE-1];
    if (tile and MOVEBLOCKFROMBELOW<>0) then begin
      if (fMap.Tiles[pX,pY+1] and MOVEBLOCKFROMABOVE=0) then
        dirs[DIRECTION_UP]:='B';
    end else
      dirs[DIRECTION_UP]:='F';
    if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,(fY+31) div TILESIZE-1]).Hit(fColor);
  end;
  if k[DIRECTION_RIGHT] then begin
    tile:=fMap.Tiles[pX+1,pY];
    if (tile and MOVEBLOCKFROMLEFT<>0) then begin
      if (fMap.Tiles[(fX+31) div TILESIZE-1,pY] and MOVEBLOCKFROMRIGHT=0) then
        dirs[DIRECTION_RIGHT]:='B';
    end else
      dirs[DIRECTION_RIGHT]:='F';
    if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX+1,pY]).Hit(fColor);
  end;
  if k[DIRECTION_DOWN] then begin
    tile:=fMap.Tiles[pX,pY+1];
    if tile and MOVEBLOCKFROMABOVE<>0 then begin
      if (fMap.Tiles[pX,(fY+31) div TILESIZE-1] and MOVEBLOCKFROMBELOW=0) then
        dirs[DIRECTION_DOWN]:='B';
    end else
      dirs[DIRECTION_DOWN]:='F';
    if (tile and TILE_MASK=TILE_BLOCK) then TBlock(Entities.EntityAt[pX,pY+1]).Hit(fColor);
  end;
  if k[DIRECTION_LEFT] then begin
    tile:=fMap.Tiles[(fX+31) div TILESIZE-1,pY];
    if tile and MOVEBLOCKFROMRIGHT<>0 then begin
      if (fMap.Tiles[px+1,pY] and MOVEBLOCKFROMLEFT=0) then
        dirs[DIRECTION_LEFT]:='B';
    end else
      dirs[DIRECTION_LEFT]:='F';
    if (tile and TILE_MASK=TILE_BLOCK) then
      TBlock(Entities.EntityAt[(fX+31) div TILESIZE-1,pY]).Hit(fColor);
  end;
  fDir:=pos('F',dirs);
  if fDir<>0 then begin
    case fDir of
      DIRECTION_UP:dec(fY);
      DIRECTION_RIGHT:inc(fX);
      DIRECTION_DOWN:inc(fY);
      DIRECTION_LEFT:dec(fX);
    end;
  end else begin
    fDir:=pos('B',dirs);
    if fDir>0 then fDir:=((fDir+1) and 3)+1;  // Bounce: 1->3 2->4 3->1 4->2
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
    end;
  end;

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

end.

