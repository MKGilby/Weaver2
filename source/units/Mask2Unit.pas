// Mask2 class
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2021-2023
// ------------------------------------------------------------------

// Version info:
//   V1.00 - 2021.04.16
//     + Created from MaskUnit.
//   V1.01 - 2022.09.21
//     + Removed unnecessary logging.
//   V1.01a - 2023.01.25
//     * Changed getmems to p:=getmem.
//     * Changed freemem to sizeless version.
//     * Tidying code.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit Mask2Unit;

interface

uses
  Classes, ARGBImageUnit;

type
  TMask=class
    constructor Create(iWidth,iHeight:integer); overload;
    constructor Create(iFilename:string); overload;
    constructor Create(iStream:TStream); overload;
    constructor CreateFromImage(iImage:TARGBImage;nzvalue:byte=1);
    constructor CreateFromImagePart(iImage:TARGBImage;x,y,w,h:integer;nzvalue:byte=1);
    constructor CreateFromImage2(iImage:TARGBImage;shrvalue:integer=0);
    destructor Destroy; override;
    procedure PutMask(aX,aY:integer;aSource:TMask);
    procedure PutMaskKey(aX,aY:integer;aSource:TMask);
    procedure PutMaskPart(aX,aY,aSX,aSY,aSW,aSH:integer;aSource:TMask);
    procedure BarMaskWH(aX,aY,aW,aH:integer;aValue:byte);
    function GetCombinedMaskValue(aLeft,aTop,aWidth,aHeight:integer):byte;
    function CheckMaskValue(aLeft,aTop,aWidth,aHeight:integer;aValue:byte):boolean;
    procedure DebugMask;
    procedure DebugMaskHex;
  private
    fWidth, fHeight: integer;  // Width and height of Mask
    fData:pointer;  // Mask data (width*height bytes)
    procedure LoadMask_MASK(iSource:TStream);
  public
    property Width:integer read fWidth;
    property Height:integer read fHeight;
    property Data:pointer read fData;
  end;

implementation

uses SysUtils, Logger, MKToolBox, MKStream;

const 
  Fstr={$I %FILE%}+', ';
  Version='1.01a';

constructor TMask.Create(iWidth,iHeight:integer);
begin
  fWidth:=iWidth;
  fHeight:=iHeight;
  fData:=getmem(fWidth*fHeight);
  fillchar(fData^,fWidth*fHeight,0);
end;

constructor TMask.Create(iFilename:string);
var Xs:TStream;s:string;atm:TARGBImage;
begin
  if pos('.',iFileName)>0 then
    s:=copy(iFileName,rpos('.',iFileName,1)+1,length(iFileName)-rpos('.',iFileName,1))
  else s:='';
  s:=decode(uppercase(s),'CEL,1,TGA,2,GSD,3,PNG,4,X');
  if s[1]='X' then begin
    Xs:=MKStreamOpener.OpenStream(iFileName);
    if Xs<>nil then begin
      Create(Xs);
      FreeAndNil(Xs);
    end else fData:=nil;
  end else begin
    atm:=TARGBImage.Create(iFilename);
    CreateFromImage(atm);
    FreeAndNIL(atm);
  end;
end;

constructor TMask.Create(iStream:TStream);
var s:String;
begin
  s:=#0#0#0#0;
  iStream.Read(s[1],4);
  if s='MASK' then LoadMask_MASK(iStream)
  else begin
    fWidth:=16;
    fHeight:=16;
    fData:=Getmem(fWidth*fHeight);
  end;
end;

constructor TMask.CreateFromImage(iImage:TARGBImage;nzvalue:byte=1);
var i,j:integer;p,q:pointer;
begin
  fWidth:=iImage.Width;
  fHeight:=iImage.Height;
  fData:=getmem(fHeight*fWidth);
  p:=fData;
  q:=iImage.Rawdata;
  for j:=0 to iImage.Height-1 do
    for i:=0 to iImage.Width-1 do begin
      if dword(q^)<>0 then byte(p^):=nzvalue else byte(p^):=0;
      inc(p);
      inc(q,4);
    end;
end;

constructor TMask.CreateFromImagePart(iImage:TARGBImage;x,y,w,h:integer;nzvalue:byte=1);
var i,j:integer;p,q:pointer;
begin
//  Log.LogDebug(Format('TMask.CreateFromImagePart(%d,%d,%d,%d)',[x,y,w,h]));
  fWidth:=w;
  fHeight:=h;
  fData:=getmem(fHeight*fWidth);
  p:=fData;
  for j:=0 to h-1 do begin
    q:=iImage.Rawdata+((y+j)*iImage.Width+x)*4;
    for i:=0 to w-1 do begin
      if dword(q^)<>0 then byte(p^):=nzvalue else byte(p^):=0;
      inc(p);
      inc(q,4);
    end;
  end;
end;

// Creates a mask from an image using one color channel.
constructor TMask.CreateFromImage2(iImage:TARGBImage;shrvalue:integer=0);
var i,j:integer;p,q:pointer;
begin
  fWidth:=iImage.Width;
  fHeight:=iImage.Height;
  fData:=getmem(fHeight*fWidth);
  p:=fData;
  q:=iImage.Rawdata;
  if (shrvalue=1) or (shrvalue=8) then inc(q)
  else if (shrvalue=2) or (shrvalue=16) then inc(q,2)
  else if (shrvalue=3) or (shrvalue=24) then inc(q,3);
  for j:=0 to iImage.Height-1 do
    for i:=0 to iImage.Width-1 do begin
      if byte(q^)<>0 then byte(p^):=byte(q^) else byte(p^):=0;
      inc(p);
      inc(q,4);
    end;
end;

destructor TMask.Destroy;
begin
  freemem(fData);
end;

procedure TMask.LoadMask_MASK(iSource:TStream);
var i,b:byte;j:cardinal;
begin
  fWidth:=0;
  fHeight:=0;
  iSource.Read(fWidth,2);
  iSource.Read(fHeight,2);
  fData:=getmem(fWidth*fHeight);
  i:=0;b:=0;
  for j:=0 to fWidth*fHeight-1 do begin
    if i=0 then begin
      iSource.Read(b,1);
      i:=8;
    end;
    byte(ptr(0,ofs(fData^)+j)^):=b and 1;
    b:=b>>1;
    i-=1;
  end;
end;

procedure TMask.PutMask(aX,aY:integer;aSource:TMask);
var i,w,h:integer;
begin
  w:=aSource.Width;
  h:=aSource.Height;
  if aX+w>fWidth then w:=fWidth-aX;
  if aY+h>fHeight then h:=fHeight-aY;
  for i:=0 to h-1 do
    move((aSource.Data+i*aSource.Width)^,(fData+(aY+i)*fWidth+aX)^,w);
end;

procedure TMask.PutMaskKey(aX,aY:integer;aSource:TMask);
var i,j,w,h:integer;p,q:pointer;
begin
  w:=aSource.Width;
  h:=aSource.Height;
  if aX+w>fWidth then w:=fWidth-aX;
  if aY+h>fHeight then h:=fHeight-aY;
  p:=aSource.Data;
  for j:=0 to h-1 do begin
    q:=fData+(aY+j)*fWidth+aX;
    for i:=0 to w-1 do
      if byte((p+i)^)<>0 then
        byte((q+i)^):=byte((p+i)^);
    p:=p+aSource.Width;
  end;
end;

procedure TMask.PutMaskPart(aX,aY,aSX,aSY,aSW,aSH:integer;aSource:TMask);
var i:integer;
begin
  if aX+aSW>fWidth then aSW:=fWidth-aX;
  if aY+aSH>fHeight then aSH:=fHeight-aY;
  for i:=0 to aSH-1 do
    move((aSource.Data+(aSY+i)*aSource.fWidth+aSX)^,
         (fData+(aY+i)*fWidth+aX)^,aSW);
end;

procedure TMask.BarMaskWH(aX,aY,aW,aH:integer;aValue:byte);
var j:integer;
begin
  if aX+aW>fWidth then aW:=fWidth-aX;
  if aY+aH>fHeight then aH:=fHeight-aY;
  for j:=0 to aH-1 do
    fillchar((fData+(aY+j)*fWidth+aX)^,aW,aValue);
end;

function TMask.GetCombinedMaskValue(aLeft,aTop,aWidth,aHeight:integer):byte;
var x,y:integer;p:pointer;
begin
  Result:=0;
  if aLeft<0 then begin aWidth+=aLeft;aLeft:=0;end;
  if aTop<0 then begin aHeight+=aTop;aTop:=0;end;
  if (aLeft+aWidth)>fWidth then aWidth:=fWidth-aLeft;
  if (aTop+aHeight)>fHeight then aHeight:=fHeight-aTop;
  for y:=aHeight-1 downto 0 do begin
    p:=fData+(aTop+y)*fWidth+aLeft;
    for x:=aWidth-1 downto 0 do begin
      Result:=Result or byte(p^);
      inc(p);
    end;
  end;
end;

function TMask.CheckMaskValue(aLeft,aTop,aWidth,aHeight:integer;aValue:byte):boolean;
var x,y:integer;p:pointer;
begin
  if aLeft<0 then begin aWidth+=aLeft;aLeft:=0;end;
  if aTop<0 then begin aHeight+=aTop;aTop:=0;end;
  if (aLeft+aWidth)>fWidth then aWidth:=fWidth-aLeft;
  if (aTop+aHeight)>fHeight then aHeight:=fHeight-aTop;
  for y:=aHeight-1 downto 0 do begin
    p:=fData+(aTop+y)*fWidth+aLeft;
    for x:=aWidth-1 downto 0 do begin
      if byte(p^)=aValue then begin
        Result:=true;
        exit;
      end;
      inc(p);
    end;
  end;
  Result:=false;
end;

procedure TMask.DebugMask;
const Istr=Fstr+'DebugMask';
var s:string;i,j:integer;
begin
  Log.LogDebug('Width='+inttostr(fWidth),Istr);
  Log.LogDebug('Height='+inttostr(fHeight),Istr);
  for j:=0 to fHeight-1 do begin
    s:='';
    for i:=0 to fWidth-1 do
      if byte((fData+i+j*fWidth)^)=0 then s+='.' else s+='X';
    Log.LogDebug(s,Istr);
  end;
end;

procedure TMask.DebugMaskHex;
const Istr=Fstr+'DebugMaskHex';
var s:string;i,j:integer;
begin
  Log.LogDebug('Width='+inttostr(fWidth),Istr);
  Log.LogDebug('Height='+inttostr(fHeight),Istr);
  for j:=0 to fHeight-1 do begin
    s:='';
    for i:=0 to fWidth-1 do
      s+=inttohex(byte((fData+i+j*fWidth)^),2)+' ';
    Log.LogDebug(s,Istr);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

