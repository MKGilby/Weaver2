{
  This file is part of the source code of Weaver 2.
  See "copyright.txt" for details.
}

unit W2Map;

{$mode Delphi}

interface

uses
  Classes, SysUtils, TileMapUnit, fpjson, jsonparser, GradientUnit;

type

  TMonsterType=(mtNone,mtVertical,mtHorizontal,mtRoamer);
  TMonsterSpeed=(msSlow,msFast);

  TMonsterData=record
    _type:TMonsterType;
    _speed:TMonsterSpeed;
    _x,_y:integer;
  end;

  { TMap }

  TMap=class(TTileMap)
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(pMapNo:integer);
  private
    fPlayerStartX,fPlayerStartY:integer;
    fColor1,fColor2:uint32;
    fMapNo:integer;
    fMonsterData:array of TMonsterData;
    fGradient:TGradient;
    function fGetMonsterCount:integer;
    function fGetMonster(index:integer):TMonsterData;
  public
    property PlayerStartX:integer read fPlayerStartX;
    property PlayerStartY:integer read fPlayerStartY;
    property MonsterCount:integer read fGetMonsterCount;
    property Monsters[index:integer]:TMonsterData read fGetMonster;
    property MapNo:integer read fMapNo;
    property Gradient:TGradient read fGradient;
  end;

implementation

uses MKStream, W2Shared, MKToolbox;

{ TMap }

constructor TMap.Create;
var i,j:integer;
begin
  inherited Create(MAPWIDTH+2,MAPHEIGHT+2);
  for j:=0 to Height-1 do
    for i:=0 to Width-1 do
      if (i=0) or (j=0) or (i=Width-1) or (j=Height-1) then
        OrigTiles[i,j]:=LOADED_TILE_WALL
      else
        OrigTiles[i,j]:=LOADED_TILE_FLOOR;

  OriginX:=-1;
  OriginY:=-1;
  fPlayerStartX:=0;
  fPlayerStartY:=0;
  fMapNo:=0;
  fGradient:=TGradient.Create($ff202020,$ff909090);
end;

destructor TMap.Destroy;
begin
  fGradient.Free;
  inherited Destroy;
end;

procedure TMap.LoadFromFile(pMapNo:integer);
var J,JD:TJSONData;Xs:TStream;JA:TJSONArray;x,y:integer;s:string;

  function min(i1,i2:integer):integer; inline;
  begin
    if i1>i2 then Result:=i2 else Result:=i1;
  end;

  function GetColor(const pPath:string;const pDefault:uint32):uint32;
  begin
    if Assigned(J.FindPath(pPath)) then
      Result:=HexToInt('FF'+J.FindPath(pPath).AsString)
    else
      Result:=pDefault;
  end;

begin
  Xs:=MKStreamOpener.OpenStream(format(MAPFILEFORMAT,[pMapNo]));
  try
    J:=GetJSON(Xs);
  finally
    Xs.Free;
  end;
  try
    if Assigned(J.FindPath('Tiles')) then begin
      JA:=TJSONArray(J.FindPath('Tiles'));
      for y:=0 to min(JA.Count-1,MAPHEIGHT-1) do begin
        s:=JA[y].AsString;
        for x:=1 to min(length(s),MAPWIDTH) do begin
          OrigTiles[x-1,y]:=ord(s[x]);
        end;
      end;
    end;
    if Assigned(J.FindPath('Player.X')) then fPlayerStartX:=J.FindPath('Player.X').AsInteger;
    if Assigned(J.FindPath('Player.Y')) then fPlayerStartY:=J.FindPath('Player.Y').AsInteger;
    fGradient.Colors[1]:=$ff000000;
    fGradient.Colors[2]:=$ffeeeeee;
    fGradient.Colors[3]:=GetColor('Color1',$ff202020);
    fGradient.Colors[4]:=GetColor('Color2',$ff909090);
    fGradient.ColorPositions[3]:=0.02;
    fGradient.ColorUsed[3]:=true;
    fGradient.ColorPositions[4]:=0.98;
    fGradient.ColorUsed[4]:=true;
    if Assigned(J.FindPath('Monsters')) then begin
      JA:=TJSONArray(J.FindPath('Monsters'));
      SetLength(fMonsterData,JA.Count);
      for y:=0 to JA.Count-1 do begin
        JD:=JA.Items[y];
        if Assigned(JD.FindPath('Type')) then begin
          if JD.FindPath('Type').AsString='Vertical' then fMonsterData[y]._type:=mtVertical
          else if JD.FindPath('Type').AsString='Horizontal' then fMonsterData[y]._type:=mtHorizontal
          else if JD.FindPath('Type').AsString='Roamer' then fMonsterData[y]._type:=mtRoamer
          else raise Exception.Create(Format('Unknown monster type: %s (Monster number: %d)',[JD.FindPath('Type').AsString,y]));
        end else
          raise Exception.Create(Format('Type not specified! (Monster number: %d)',[y]));
        if Assigned(JD.FindPath('Speed')) then begin
          if JD.FindPath('Speed').AsString='Slow' then fMonsterData[y]._speed:=msSlow
          else if JD.FindPath('Speed').AsString='Fast' then fMonsterData[y]._speed:=msFast
          else raise Exception.Create(Format('Unknown monster speed: %s (Monster number: %d)',[JD.FindPath('Speed').AsString,y]));
        end else
          raise Exception.Create(Format('Speed not specified! (Monster number: %d)',[y]));
        if Assigned(JD.FindPath('X')) then
          fMonsterData[y]._x:=JD.FindPath('X').AsInteger
        else
          raise Exception.Create(Format('X not specified! (Monster number: %d)',[y]));
        if Assigned(JD.FindPath('Y')) then
          fMonsterData[y]._y:=JD.FindPath('Y').AsInteger
        else
          raise Exception.Create(Format('Y not specified! (Monster number: %d)',[y]));
      end;
    end;
  finally
    J.Free;
  end;
end;

function TMap.fGetMonsterCount: integer;
begin
  Result:=length(fMonsterData);
end;

function TMap.fGetMonster(index: integer): TMonsterData;
begin
  if (index>=0) and (index<length(fMonsterData)) then
    Result:=fMonsterData[index]
  else begin
    Result._type:=mtNone;
  end;
end;

end.

