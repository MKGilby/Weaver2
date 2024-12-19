unit W2Map;

{$mode Delphi}

interface

uses
  Classes, SysUtils, TileMapUnit, fpjson, jsonparser;

type

  { TMap }

  TMap=class(TTileMap)
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(pMapNo:integer);
  private
    fPlayerStartX,fPlayerStartY:integer;
    fMapNo:integer;
  public
    property PlayerStartX:integer read fPlayerStartX;
    property PlayerStartY:integer read fPlayerStartY;
    property MapNo:integer read fMapNo;
  end;

implementation

uses MKStream, W2Shared;

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
  LogContent('---- MAP Data follows:');
end;

destructor TMap.Destroy;
begin
  inherited Destroy;
end;

procedure TMap.LoadFromFile(pMapNo:integer);
var J:TJSONData;Xs:TStream;JA:TJSONArray;x,y:integer;s:string;

  function max(i1,i2:integer):integer; inline;
  begin
    if i1>i2 then Result:=i2 else Result:=i1;
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
      for y:=0 to max(JA.Count-1,MAPHEIGHT-1) do begin
        s:=JA[y].AsString;
        for x:=1 to max(length(s),MAPWIDTH) do begin
          OrigTiles[x-1,y]:=ord(s[x]);
        end;
      end;
    end;
    if Assigned(J.FindPath('Player.X')) then fPlayerStartX:=J.FindPath('Player.X').AsInteger;
    if Assigned(J.FindPath('Player.Y')) then fPlayerStartY:=J.FindPath('Player.Y').AsInteger;
  finally
    J.Free;
  end;
end;

end.

