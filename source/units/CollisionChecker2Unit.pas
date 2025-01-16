{ -[Name]-------------------------------------------

           Collision checker class for SDL2

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2021

  --------------------------------------------------

  -[Description]------------------------------------

   You can use this class' static methods to check
   if two collision data record is collides each other.

   Masks come from Masks2Unit.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2021.04.29
//     - Initial creation from CollisionCheckerUnit
//  V1.01: Gilby - 2021.05.02
//     - BUGFIX: When GetCollisionBox swapped sprites it mixed up the coordinates.

{$mode delphi}

unit CollisionChecker2Unit;

interface

uses Mask2Unit;

type 
  PCollisionData=^TCollisionData;
  TCollisionData=record
    _mask:TMask;           // Mask of the object
    _x,_y:integer;         // x,y position of the object on the screen
    _extra:integer;        // You can store some extra info here...
  end;

  TCollisionBox=record
    x1,x2,y1,y2:integer;
    wi,he:integer;
  end;

  TCollisionChecker=class
    // Returns true if Sprite1 and Sprite2 collides.
    class function IsCollideBit1(Sprite1,Sprite2:PCollisionData):boolean; static;
    class function IsCollideAnd(Sprite1,Sprite2:PCollisionData):boolean; static;
    class function IsCollideNonZero(Sprite1,Sprite2:PCollisionData):boolean; static;
    class function IsCollideValue(Sprite1,Sprite2:PCollisionData):boolean; static;

    // Create collision data record
    class function CreateCollisionData(aMask:TMask;aX,aY:integer;aExtra:integer=0):PCollisionData; static;

    // Destroys the data, but not the mask in it!. (Releases memory...)
    class procedure DestroyCollisionData(data:PCollisionData); static;

  private
    class function GetCollisionBox(Sprite1,Sprite2:PCollisionData):TCollisionBox; static;
  end;

//var
//  CollisionChecker:TCollisionChecker;

implementation

uses SysUtils, Logger, MKToolbox;

const Fstr='CollisionChecker2Unit.pas, ';
      Version='1.01';

class function TCollisionChecker.GetCollisionBox(Sprite1,Sprite2:PCollisionData):TCollisionBox;
var
  tmp:PCollisionData;
  wasswap:boolean;
  i:integer;
begin
  Result.x1:=-1;
{  Log.LogDebug('Sprite 1: '+inttostr(Sprite1^._x)+','+inttostr(Sprite1^._y)+','+
               inttostr(Sprite1^._mask.Width)+','+inttostr(Sprite1^._mask.Height));
  Log.LogDebug('Sprite 2: '+inttostr(Sprite2^._x)+','+inttostr(Sprite2^._y)+','+
               inttostr(Sprite2^._mask.Width)+','+inttostr(Sprite2^._mask.Height));}
  if Sprite1^._x>Sprite2^._x then begin
    tmp:=Sprite1;
    Sprite1:=Sprite2;
    Sprite2:=tmp;
    wasswap:=true;
  end else wasswap:=false;
  if (Sprite1^._y<Sprite2^._y) then begin
    if (Sprite1^._x+Sprite1^._mask.Width-1<Sprite2^._x) or
       (Sprite1^._y+Sprite1^._mask.Height-1<Sprite2^._y) then
         exit;  // Not touching each other
    Result.x2:=0;Result.y2:=0;
    Result.x1:=Sprite2^._x-Sprite1^._x;
    Result.y1:=Sprite2^._y-Sprite1^._y;
    if Sprite1^._x+Sprite1^._mask.Width<Sprite2^._x+Sprite2^._mask.Width then
      if Sprite1^._y+Sprite1^._mask.Height<Sprite2^._y+Sprite2^._mask.Height then begin
        // variation 1
        Result.wi:=Sprite1^._x+Sprite1^._mask.Width-Sprite2^._x;
        Result.he:=Sprite1^._y+Sprite1^._mask.Height-Sprite2^._y;
      end else begin
        // variation 2
        Result.wi:=Sprite1^._x+Sprite1^._mask.Width-Sprite2^._x;
        Result.he:=Sprite2^._mask.Height;
      end
    else
      if Sprite1^._y+Sprite1^._mask.Height<Sprite2^._y+Sprite2^._mask.Height then begin
        // variation 3
        Result.wi:=Sprite2^._mask.Width;
        Result.he:=Sprite1^._y+Sprite1^._mask.Height-Sprite2^._y;
      end else begin
        // variation 4
        Result.wi:=Sprite2^._mask.Width;
        Result.he:=Sprite2^._mask.Height;
      end
  end else begin
    if (Sprite1^._x+Sprite1^._mask.Width-1<Sprite2^._x) or
       (Sprite2^._y+Sprite2^._mask.Height-1<Sprite1^._y) then
         exit;  // Not touching each other
    Result.x1:=Sprite2^._x-Sprite1^._x;
    Result.y1:=0;
    Result.x2:=0;
    Result.y2:=Sprite1^._y-Sprite2^._y;
    if Sprite1^._x+Sprite1^._mask.Width<Sprite2^._x+Sprite2^._mask.Width then
      if Sprite1^._y+Sprite1^._mask.Height<Sprite2^._y+Sprite2^._mask.Height then begin
        // variation 7
        Result.wi:=Sprite1^._x+Sprite1^._mask.Width-Sprite2^._x;
        Result.he:=Sprite1^._mask.Height;
      end else begin
        // variation 6
        Result.wi:=Sprite1^._x+Sprite1^._mask.Width-Sprite2^._x;
        Result.he:=Sprite2^._y+Sprite2^._mask.Height-Sprite1^._y;
      end
    else
      if Sprite1^._y+Sprite1^._mask.Height<Sprite2^._y+Sprite2^._mask.Height then begin
        // variation 8
        Result.wi:=Sprite2^._mask.Width;
        Result.he:=Sprite1^._mask.Height;
      end else begin
        // variation 5
        Result.wi:=Sprite2^._mask.Width;
        Result.he:=Sprite2^._y+Sprite2^._mask.Height-Sprite1^._y;
      end
  end;

  if wasswap then begin
    i:=Result.x1;Result.x1:=Result.x2;Result.x2:=i;
    i:=Result.y1;Result.y1:=Result.y2;Result.y2:=i;
  end;

{  Log.LogDebug(inttostr(Result.x1)+','+inttostr(Result.y1)+' - '+
              inttostr(Result.x2)+','+inttostr(Result.y2)+' - '+
              inttostr(Result.wi)+','+inttostr(Result.he));}
end;

class function TCollisionChecker.IsCollideBit1(Sprite1,Sprite2:PCollisionData):boolean;
var i,j:integer;p1,p2:pointer;tmp:TCollisionBox;
begin
  Result:=false;
  tmp:=GetCollisionBox(Sprite1,Sprite2);
  if tmp.x1<>-1 then with tmp do begin
    for j:=0 to he-1 do begin
      p1:=Sprite1^._mask.Data+(j+y1)*Sprite1^._mask.Width+x1;
      p2:=Sprite2^._mask.Data+(j+y2)*Sprite2^._mask.Width+x2;
      for i:=0 to wi-1 do begin
        if (byte(p1^) and byte(p2^) and 1)<>0 then begin Result:=true;exit;end;
        inc(p1);inc(p2);
      end;
    end;
  end;
end;

class function TCollisionChecker.IsCollideNonZero(Sprite1,Sprite2:PCollisionData):boolean;
var i,j:integer;p1,p2:pointer;tmp:TCollisionBox;
begin
  Result:=false;
  tmp:=GetCollisionBox(Sprite1,Sprite2);
  if tmp.x1<>-1 then with tmp do begin
//    Sprite1^._Mask.DebugMask;
//    Sprite2^._Mask.DebugMask;
    for j:=0 to he-1 do begin
      p1:=Sprite1^._mask.Data+(j+y1)*Sprite1^._mask.Width+x1;
      p2:=Sprite2^._mask.Data+(j+y2)*Sprite2^._mask.Width+x2;
      for i:=0 to wi-1 do begin
        if (byte(p1^)<>0) and (byte(p2^)<>0) then begin
          Result:=true;
          exit;
        end;
        inc(p1);inc(p2);
      end;
    end;
  end;
end;

class function TCollisionChecker.IsCollideValue(Sprite1,Sprite2:PCollisionData):boolean;
var i,j:integer;p1,p2:pointer;tmp:TCollisionBox;
begin
  Result:=false;
  tmp:=GetCollisionBox(Sprite1,Sprite2);
  if tmp.x1<>-1 then with tmp do begin
    for j:=0 to he-1 do begin
      p1:=Sprite1^._mask.Data+(j+y1)*Sprite1^._mask.Width+x1;
      p2:=Sprite2^._mask.Data+(j+y2)*Sprite2^._mask.Width+x2;
      for i:=0 to wi-1 do begin
        if (byte(p1^)=byte(p2^)) then begin Result:=true;exit;end;
        inc(p1);inc(p2);
      end;
    end;
  end;
end;

class function TCollisionChecker.IsCollideAnd(Sprite1,Sprite2:PCollisionData):boolean;
var i,j:integer;p1,p2:pointer;tmp:TCollisionBox;
begin
  Result:=false;
  tmp:=GetCollisionBox(Sprite1,Sprite2);
  if tmp.x1<>-1 then with tmp do begin
    for j:=0 to he-1 do begin
      p1:=Sprite1^._mask.Data+(j+y1)*Sprite1^._mask.Width+x1;
      p2:=Sprite2^._mask.Data+(j+y2)*Sprite2^._mask.Width+x2;
      for i:=0 to wi-1 do begin
        if (byte(p1^) and byte(p2^))<>0 then begin Result:=true;exit;end;
        inc(p1);inc(p2);
      end;
    end;
  end;
end;

class function TCollisionChecker.CreateCollisionData(aMask:TMask;aX,aY:integer;aExtra:integer=0):PCollisionData;
//var atm:PCollisionData;
begin
  new(Result);
  with Result^ do begin
    _Mask:=aMask;
    _x:=aX;
    _y:=aY;
    _extra:=aExtra;
  end;
//  Result:=atm;
end;

class procedure TCollisionChecker.DestroyCollisionData(data:PCollisionData);
begin
  dispose(data);
  data:=nil;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
