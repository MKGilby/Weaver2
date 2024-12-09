// MKSZTSZ Fast palette class
// ------------------------------------------------------------------
// Helps you out when you are counting the distinct colors of a picture
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2013-2019
// ------------------------------------------------------------------

// Version info:
//   1.00 - Gilby - 2013.02.01
//     * Initial creation
//   1.01 - Gilby - 2013.02.05
//     + Caching the last added color to increase performance
//   1.02 - Gilby - 2013.12.03
//     * BugFix: fPrevC and fPrevI was uninitialized
//     * Padding color values in ListItem
//     * Added index listing in ListItem
//   1.03 - Gilby - 2014.01.15
//     * Dec2Hex changed to IntToHex
//   1.03a - Gilby - 2019.08.12
//     * >>1 changed to div 2
//   1.04 - Gilby - 2019.08.14
//     + GetPalettedImage added. You must freemem(img,width*height) and
//       freemem(pal,768) when the output is no longer used!
//   1.05 - Gilby - 2019.11.08
//     * REWORK: TFastPaletteEntry is a record instead of class. (Speedup!)
//     * REWORK: Preallocated all Entries for more speed.
//   1.06 - Gilby - 2020.02.14
//     * GetPalettedImage32 added. It processes 32 bit ARGB rawdata with
//       BGRA ordering. It can be used with ARGBImage.RawData.
//       Still you have to free memory but pal size is 1024 instead of 768.
//       (It contains alpha value too)


{$mode delphi}

unit FastPaletteUnit;

interface

uses Classes;

type
  TFastPaletteEntry=record
    _index:integer;
    _color:uint32;
  end;

  TFastPalette=class
    constructor Create;
//    destructor Destroy; override;
    procedure Clear;
    function AddColor(r,g,b:byte):integer; overload;
    function AddColor(color:uint32):integer; overload;
    procedure ListItems;
    procedure GetAsRGBPalette(var p);
    procedure GetAsBGRAPalette(var p);
    function Clone:TFastPalette;
  private
    fPrevC:uint32;
    fCount,fPrevI:integer;
    fEntries:array [0..255] of TFastPaletteEntry;
    function SearchPlace(Value:uint32):integer;
    function fGetPaletteEntry(index:integer):TFastPaletteEntry;
    procedure Insert(pPlace,pIndex:integer;pColor:uint32);
  public
    property Entries[index:integer]:TFastPaletteEntry read fGetPaletteEntry; default;
    property Count:integer read fCount;
  end;

{  TAlphaCompactData=record
    _r:word;
    _g:word;
    _b:word;
  end;}

procedure GetPalettedImage(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);
procedure GetPalettedImage2(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);
procedure GetPalettedImage32(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);

implementation

uses SysUtils, MKToolBox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.06a';

constructor TFastPalette.Create;
begin
  fPrevI:=-1;
  fCount:=0;
end;

{destructor TFastPalette.Destroy;
begin
  Clear;
  inherited ;
end;}

procedure TFastPalette.Clear;
begin
  fPrevI:=-1;
  fCount:=0;
{  for i:=0 to Count-1 do
    TFastPaletteEntry(Items[i]).Free;}
  inherited ;
end;

function TFastPalette.SearchPlace(Value:uint32):integer;
var First,Last,Now:integer;
begin
  First:=0;
  Last:=fCount-1;
  Now:=(First+Last) div 2;
  repeat
//    Log.Trace(inttostr(First)+','+inttostr(Last)+','+inttostr(Now));
    with fEntries[Now] do
      if Value>_color then First:=Now+1
      else if Value<_color then Last:=Now-1
      else begin
        result:=Now;
        exit;
      end;
    Now:=(First+Last) div 2;
  until First>=Last;
//  Log.Trace(inttostr(First)+','+inttostr(Last)+','+inttostr(Now));
  Result:=First;
end;

function TFastPalette.fGetPaletteEntry(index:integer):TFastPaletteEntry;
begin
  if (index>=0) and (index<fCount) then
    Result:=fEntries[index]
  else begin
    Result._color:=0;
    Result._index:=-1;
  end;
end;

function TFastPalette.AddColor(r,g,b:byte):integer;
var c:uint32;
begin
//  c:=(r and $FF)+(g and $FF)<<8+(b and $FF)<<16+$ff000000;
  c:=r+g<<8+b<<16+$ff000000;
{  if (fPrevC=c) and (fPrevI>-1) then begin
    Result:=fPrevI;
    exit;
  end;}
  Result:=AddColor(c);
end;

function TFastPalette.AddColor(color:uint32):integer;
var i:integer;
begin
  if (fPrevC=color) and (fPrevI>-1) then begin
    Result:=fPrevI;
    exit;
  end;
  if fCount>0 then begin
    if fCount>1 then i:=SearchPlace(color) else i:=0;
    if color=Self[i]._color then begin
      Result:=Self[i]._index;
    end else begin
      if fCount<256 then begin
        Result:=fCount;
        if color<Self[i]._color then Insert(i,fCount,color)
        else if color>Self[i]._color then
          if i<fCount-1 then Insert(i+1,fCount,color) else begin
            fEntries[fCount]._index:=fCount;
            fEntries[fCount]._color:=color;
            inc(fCount);
          end;
      end else begin
        Result:=-1;
      end;
    end;
  end else begin
    fEntries[0]._index:=0;
    fEntries[0]._color:=color;
    Result:=0;
    fCount:=1;
  end;
  fPrevC:=color;
  fPrevI:=Result;
end;

procedure TFastPalette.Insert(pPlace,pIndex:integer;pColor:uint32);
var i:integer;
begin
  for i:=fCount downto pPlace+1 do fEntries[i]:=fEntries[i-1];
  fEntries[pPlace]._index:=pIndex;
  fEntries[pPlace]._color:=pColor;
  inc(fCount);
end;

procedure TFastPalette.ListItems;
var i:integer;
begin
  Log.Trace('-------------');
  for i:=0 to fCount-1 do with Self[i] do
    Log.Trace(inttostr(i)+'. Index: '+inttohex(_index,2)+' Value: '+inttohex(_color,6));
end;

procedure TFastPalette.GetAsRGBPalette(var p);
var pp:pointer;i:integer;
begin
  pp:=@p;
  for i:=0 to fCount-1 do
    system.move(fEntries[i]._color,(pp+Self[i]._index*3)^,3);
end;

procedure TFastPalette.GetAsBGRAPalette(var p);
var pp:pointer;i:integer;
begin
  pp:=@p;
  for i:=0 to fCount-1 do
    system.move(fEntries[i]._color,(pp+Self[i]._index*4)^,4);
end;

function TFastPalette.Clone:TFastPalette;
var i:integer;
begin
  Result:=TFastPalette.Create;
  Result.fCount:=fCount;
  for i:=0 to fCount-1 do
    Result.fEntries[i]:=fEntries[i];
end;

procedure GetPalettedImage(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);
var FastPalette:TFastPalette;pq,pp:pointer;i,j:integer;r,g,b:byte;
begin
  getmem(img,pWidth*pHeight);
  grayscaled:=true;
  pq:=img;
  pp:=pRawData;
  i:=0;
  FastPalette:=TFastPalette.Create;
  repeat
    r:=byte(pp^);
    g:=byte((pp+1)^);
    b:=byte((pp+2)^);
//    Log.Trace(inttostr(r)+', '+inttostr(g)+', '+inttostr(b));
    if (r<>g) or (r<>b) then Grayscaled:=false;
    j:=FastPalette.AddColor(r,g,b);
    if j>-1 then byte(pq^):=j;
    inc(pp,3);
    inc(i);
    inc(pq);
  until (i=pWidth*pHeight) or (j=-1);
  if j=-1 then begin
    freemem(img,pWidth*pHeight);
    img:=nil;
    palette:=nil;
  end else begin
    getmem(palette,768);
    palcount:=FastPalette.fCount;
    FastPalette.GetAsRGBPalette(palette^);
  end;
  FreeAndNil(FastPalette);
end;

procedure GetPalettedImage2(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);
var FastPalette:TFastPalette;pq,pp:pointer;i,j:integer;r,g,b:byte;
begin
  getmem(img,pWidth*pHeight);
  grayscaled:=true;
  pq:=img;
  pp:=pRawData;
  i:=0;
  FastPalette:=TFastPalette.Create;
  repeat
    r:=byte(pp^);
    g:=byte((pp+1)^);
    b:=byte((pp+2)^);
//    Log.Trace(inttostr(r)+', '+inttostr(g)+', '+inttostr(b));
    if (r<>g) or (r<>b) then Grayscaled:=false;
    j:=FastPalette.AddColor(r,g,b);
    if j>-1 then byte(pq^):=j;
    inc(pp,4);
    inc(i);
    inc(pq);
  until (i=pWidth*pHeight) or (j=-1);
  if j=-1 then begin
    freemem(img,pWidth*pHeight);
    img:=nil;
    palette:=nil;
  end else begin
    getmem(palette,768);
    palcount:=FastPalette.fCount;
    FastPalette.GetAsRGBPalette(palette^);
  end;
  FreeAndNil(FastPalette);
end;

procedure GetPalettedImage32(const pWidth,pHeight:integer;const pRawData:pointer;out img:pointer;out palette:pointer;out palcount:integer;out grayscaled:boolean);
var FastPalette:TFastPalette;pq,pp:pointer;i,j:integer;
begin
  getmem(img,pWidth*pHeight);
  grayscaled:=true;
  pq:=img;
  pp:=pRawData;
  i:=0;
  FastPalette:=TFastPalette.Create;
  repeat
//    b:=byte(pp^);
//    g:=byte((pp+1)^);
//    r:=byte((pp+2)^);
//    Log.Trace(inttostr(r)+', '+inttostr(g)+', '+inttostr(b));
    if (byte(pp^)<>byte((pp+1)^)) or (byte(pp^)<>byte((pp+2)^)) then Grayscaled:=false;
    j:=FastPalette.AddColor(uint32(pp^));
    if j>-1 then byte(pq^):=j;
    inc(pp,4);
    inc(i);
    inc(pq);
  until (i=pWidth*pHeight) or (j=-1);
  if j=-1 then begin
    freemem(img,pWidth*pHeight);
    img:=nil;
    palette:=nil;
  end else begin
    getmem(palette,1024);
    palcount:=FastPalette.fCount;
    FastPalette.GetAsBGRAPalette(palette^);
  end;
  FreeAndNil(FastPalette);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
