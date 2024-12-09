{ -[Name]-------------------------------------------

                     Tile Map Class 

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2012-2015

  --------------------------------------------------

  -[Versions]---------------------------------------

   1.00: Gilby - 2012.10.15
      * Base tile map class created from XMap
      * No load or save just the base functions
   1.01: Gilby - 2015.05.20
      * MKStream3 removed from uses
   1.02: Gilby - 2015.07.02
      * Tiles is the default property
   1.03: Gilby - 2015.10.26
      + OriginX and OriginY property 
        It shows the map's top-left corner. So if origin is (-2,1) and you 
        issue Get[Orig]Tile(0,0), you will get the tile at (2,1) from the map.
        It is useful for offscreen borders.
   1.04: Gilby - 2016.01.27
      + LogContent now logs both OrigTiles and Tiles
      * Fix in ApplyLogicalWrap
   1.04a: Gilby - 2021.08.10
      - Removed unused MKINIFiles unit
   1.05: Gilby - 2023.11.28
      + Added Create(width,height)
   1.06: Gilby - 2023.12.05
      * Getmems and freemems changed to new version.
      + Added pMessage parameter to LogContent.
      * BUGFix in Move.
}

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit TileMapUnit;

interface

uses Classes;

type
  TLogicalWrap=record
    _originalvalue:integer;
    _newvalue:integer;
  end;

  { TTileMap }

  TTileMap=class
    constructor Create; overload;
    constructor Create(iWidth,iHeight:integer); overload;
    destructor Destroy; override;
       
    procedure Clear; virtual;
    procedure NewMap(iWidth,iHeight:integer); virtual;
       
    function CheckTiles:boolean;

    procedure ApplyLogicalWrap;
    procedure AddLogicalWrap(iOrig,iNew:integer);
    
    function SizeIs(x,y:integer):boolean;
    procedure Resize(neww,newh:integer); virtual;
    procedure Move(dx,dy:integer); virtual;

    procedure LogContent(pMessage:string='');
  private
    function GetTile(x,y:integer):integer;
    function GetOrigTile(x,y:integer):integer;
    procedure SetTile(x,y:integer;value:integer);
    procedure SetOrigTile(x,y:integer;value:integer);
  protected
    fWidth, fHeight : integer;
    fMapData, fOrigMapData : pointer;
//    fLastError : string;
    fExtras : TStringList;
    fLogicalWrap : array of TLogicalWrap;
    fOriginX,fOriginY:integer;
  public
    MapName, Author, Game: string;
    property OriginX:integer read fOriginX write fOriginX;
    property OriginY:integer read fOriginY write fOriginY;
    property Tiles[x,y:integer]:integer read GetTile write SetTile; default;
    property OrigTiles[x,y:integer]:integer read GetOrigTile write SetOrigTile;
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property Extras:TStringList read fExtras write fExtras;
//    property LastError:string read fLastError;
  end;
     
implementation

uses SysUtils, Logger, MKToolBox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.06';

constructor TTileMap.Create;
begin
  inherited ;
  Game:='Unknown';
  Author:='Unknown';
  MapName:='Unknown';
  fWidth:=0;
  fHeight:=0;
  SetLength(fLogicalWrap,0);
  fExtras:=TStringList.Create;
  fOriginX:=0;
  fOriginY:=0;
end;

constructor TTileMap.Create(iWidth,iHeight:integer);
begin
  Create;
  NewMap(iWidth,iHeight);
end;

destructor TTileMap.Destroy;
begin
  Clear;
  FreeAndNil(fExtras);
  inherited ;
end;

procedure TTileMap.Clear;
begin
  if (fWidth>0) and (fHeight>0) then begin
    freemem(fMapData);
    freemem(fOrigMapData);
    fWidth:=0;
    fHeight:=0;
  end;
  Game:='Unknown';
  Author:='Unknown';
  MapName:='Unknown';
  fExtras.Clear;
  SetLength(fLogicalWrap,0);
  fOriginX:=0;
  fOriginY:=0;
end;

procedure TTileMap.NewMap(iWidth,iHeight:integer);
begin
  Clear;
  fWidth:=iWidth;
  fHeight:=iHeight;
  fOriginX:=0;
  fOriginY:=0;
  fOrigMapData:=getmem(fWidth*fHeight*4);
  fillchar(fOrigMapData^,fWidth*fHeight*4,0);
  fMapData:=getmem(fWidth*fHeight*4);
  fillchar(fMapData^,fWidth*fHeight*4,0);
end;

procedure TTileMap.ApplyLogicalWrap;
var x,y,z,w2:integer;
begin
//  Log.DumpMemory(fMapData^,0,fHeight*fWidth*4,'Before','ApplyLogicalWrap');
  for y:=0 to fHeight-1 do
    for x:=0 to fWidth-1 do begin
      w2:=integer((fOrigMapData+(x+y*fWidth)*4)^);
      integer((fMapData+(x+y*fWidth)*4)^):=w2;
      for z:=0 to length(fLogicalWrap)-1 do begin
        if fLogicalWrap[z]._OriginalValue=w2 then begin
          integer((fMapData+(x+y*fWidth)*4)^):=fLogicalWrap[z]._NewValue;
          break;
        end;
      end;
    end;
//  Log.DumpMemory(fMapData^,0,fHeight*fWidth*4,'After','ApplyLogicalWrap');
end;

function TTileMap.CheckTiles:boolean;
var i,j:integer;
begin
  for j:=0 to fHeight-1 do
    for i:=0 to fWidth-1 do
      if OrigTiles[i,j]>255 then begin
        Result:=false;
        exit;
      end;
  Result:=true;
end;

function TTileMap.GetTile(x,y:integer):integer;
begin
  Result:=integer((fMapData+((y-OriginY)*fWidth+x-OriginX)*4)^);
end;

function TTileMap.GetOrigTile(x,y:integer):integer;
begin
  Result:=integer((fOrigMapData+((y-OriginY)*fWidth+x-OriginX)*4)^);
end;


procedure TTileMap.SetTile(x,y,value:integer);
begin
  integer((fMapData+((y-OriginY)*fWidth+x-OriginX)*4)^):=value;
end;

procedure TTileMap.SetOrigTile(x,y,value:integer);
begin
  integer((fOrigMapData+((y-OriginY)*fWidth+x-OriginX)*4)^):=value;
end;

function TTileMap.SizeIs(x,y:integer):boolean;
begin
  Result:= (x=fWidth) and (y=fHeight);
end;

procedure TTileMap.Resize(neww,newh:integer);
var atm:pointer;y:integer;
//const Istr=Fstr+'TTileMap.Resize';
begin
  atm:=fMapData;
//  Log.DumpMemory(atm^,0,fWidth*fHeight,'Logical map data before resize',Istr);
  fMapData:=getmem(neww*newh*4);
  fillchar(fMapData^,neww*newh*4,0);
  for y:=0 to fHeight-1 do
    system.move((atm+(y*fWidth)*4)^,(fMapData+(y*neww)*4)^,fWidth*4);
  freemem(atm);
//  Log.DumpMemory(fMapData^,0,neww*newh,'Logical map data after resize',Istr);

  atm:=fOrigMapData;
//  Log.DumpMemory(atm^,0,fWidth*fHeight,'Original map data before resize',Istr);
  fOrigMapData:=getmem(neww*newh*4);
  fillchar(fOrigMapData^,neww*newh*4,0);
  for y:=0 to fHeight-1 do
    system.move((atm+(y*fWidth)*4)^,(fOrigMapData+(y*neww*4))^,fWidth*4);
  freemem(atm);
//  Log.DumpMemory(fOrigMapData^,0,neww*newh,'Original map data after resize',Istr);

  fWidth:=neww;
  fHeight:=newh;
end;

procedure TTileMap.Move(dx,dy:integer);
var atm:pointer;x,y,px,py:integer;
begin
  atm:=fMapData;
  fMapData:=getmem(fWidth*fHeight*4);
  px:=dx;
  py:=dy;
  for y:=0 to fHeight-1 do
    for x:=0 to fWidth-1 do begin
      integer((fMapData+(py*fWidth+px)*4)^):=integer((atm+(y*fWidth+x)*4)^);
      inc(px);
      if px=fWidth then begin
        px:=0;
        inc(py);
        if py=fHeight then py:=0;
      end;
    end;
  freemem(atm);

  atm:=fOrigMapData;
  fOrigMapData:=getmem(fWidth*fHeight*4);
  px:=dx;
  py:=dy;
  for y:=0 to fHeight-1 do
    for x:=0 to fWidth-1 do begin
      integer((fOrigMapData+(py*fWidth+px)*4)^):=integer((atm+(y*fWidth+x)*4)^);
      inc(px);
      if px=fWidth then begin
        px:=0;
        inc(py);
        if py=fHeight then py:=0;
      end;
    end;
  freemem(atm);
end;

procedure TTileMap.LogContent(pMessage:string);
var i,j:integer;s:String;
begin
  if (fWidth>0) and (fHeight>0) then begin
    if pMessage<>'' then Log.Trace('Message: '+pMessage);
    Log.Trace('Size: '+inttostr(fWidth)+'x'+inttostr(fHeight));
    Log.Trace('Origin: '+inttostr(fOriginX)+','+inttostr(fOriginY));
    Log.Trace('Origtiles:');
    for j:=0 to fHeight-1 do begin
      s:='';
      for i:=0 to fWidth-1 do
        s:=s+inttohex(GetOrigTile(i+OriginX,j+OriginY),8)+' ';
      Log.Trace(s);
    end;
    Log.Trace('Tiles:');
    for j:=0 to fHeight-1 do begin
      s:='';
      for i:=0 to fWidth-1 do
        s:=s+inttohex(GetTile(i+OriginX,j+OriginY),8)+' ';
      Log.Trace(s);
    end;
  end;
end;

procedure TTileMap.AddLogicalWrap(iOrig,iNew:integer);
const Istr=Fstr+'AddLogicalWrap';
begin
  Log.LogDebug(inttostr(iOrig)+', '+inttostr(iNew),Istr);
  SetLength(fLogicalWrap,length(fLogicalWrap)+1);
  with fLogicalWrap[length(fLogicalWrap)-1] do begin
    _OriginalValue:=iOrig;
    _NewValue:=iNew;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
