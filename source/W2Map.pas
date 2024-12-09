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
    procedure LoadFromFile(iFilename:string);
  private
    fPlayerStartX,fPlayerStartY:integer;
  public
    property PlayerStartX:integer read fPlayerStartX;
    property PlayerStartY:integer read fPlayerStartY;
  end;

implementation

uses MKStream, W2Shared;

{ TMap }

constructor TMap.Create;
var i,j:integer;
begin
  inherited Create(MAPWIDTH,MAPHEIGHT);
  for j:=0 to MAPHEIGHT-1 do
    for i:=0 to MAPWIDTH-1 do
      OrigTiles[i,j]:=LOADED_TILE_FLOOR;
  fPlayerStartX:=0;
  fPlayerStartY:=0;
end;

destructor TMap.Destroy;
begin
  inherited Destroy;
end;

procedure TMap.LoadFromFile(iFilename:string);
var J:TJSONData;Xs:TStream;JA:TJSONArray;x,y:integer;s:string;

  function max(i1,i2:integer):integer; inline;
  begin
    if i1>i2 then Result:=i2 else Result:=i1;
  end;

begin
  Xs:=MKStreamOpener.OpenStream(iFilename);
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

