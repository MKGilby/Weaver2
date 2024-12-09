unit W2MapEntities;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}
interface

uses
  SysUtils, fgl, Animation2Unit;

type

  { TMapEntity }

  TMapEntity=class
    constructor Create(ipX,ipY:integer);
    // Move entity by dx,dy tiles
    procedure MoveRel(iDeltaX,iDeltaY:integer);
    procedure Draw; virtual; abstract;
    procedure Move(pTimeUsed:double); virtual;
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
    procedure Move(pTimeUsed:double);
  private
    function fGetEntityAt(x,y:integer):TMapEntity;
  public
    property EntityAt[x,y:integer]:TMapEntity read fGetEntityAt;
  end;

  { TBlock }

  TBlock=class(TMapEntity)
    constructor Create(ipX,ipY,iColor:integer);
    destructor Destroy; override;
    procedure Draw; override;
  private
    fColor:integer;
    fAnimation:TAnimation;
  end;

  { TZapper }

  TZapper=class(TMapEntity)
    constructor Create(ipX,ipY:integer;iProgram:string);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Move(pTimeUsed:double); override;
  private
    fProgram:string;
    fPosition:integer;
    fTimeLeft:double;
    fAnimation:TAnimation;
  end;

implementation

uses W2Shared;

{ TMapEntity }

constructor TMapEntity.Create(ipX,ipY:integer);
begin
  fpX:=ipX;
  fpY:=ipY;
  fX:=MAPLEFT+fpX*TILEWIDTH;
  fY:=MAPTOP+fpY*TILEHEIGHT;
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

procedure TMapEntity.Move(pTimeUsed: double);
begin
  { Nothing to do, override if want to do something based on ellapsed time. }
end;

{ TMapEntities }

procedure TMapEntities.Draw;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Draw;
end;

procedure TMapEntities.Move(pTimeUsed: double);
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Move(pTimeUsed);
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

{ TBlock }

constructor TBlock.Create(ipX,ipY,iColor:integer);
begin
  inherited Create(ipX,ipY);
  fColor:=iColor;
  fAnimation:=MM.Animations.ItemByName[Format('Block%d',[iColor])].SpawnAnimation;
end;

destructor TBlock.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBlock.Draw;
begin
  if Assigned(fAnimation) then fAnimation.PutFrame(fX,fY);
end;

{ TZapper }

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
  if (fProgram[fPosition]='1') and Assigned(fAnimation) then fAnimation.PutFrame(fX,fY);
end;

procedure TZapper.Move(pTimeUsed: double);
begin
  fAnimation.Animate(pTimeUsed);
  if fTimeLeft>pTimeUsed then begin
    fTimeLeft:=fTimeLeft-pTimeUsed;
  end else begin
    fTimeLeft:=ZAPPERLIFECYCLE-(pTimeUsed-fTimeLeft);
    inc(fPosition);
    if fPosition>length(fProgram) then fPosition:=1;
  end;
end;

end.

