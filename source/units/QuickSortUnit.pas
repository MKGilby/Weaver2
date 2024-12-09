{ -[Name]-------------------------------------------

               QuickSort Algorythm V1.00

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2006

  --------------------------------------------------

  -[Description]------------------------------------

   QuickSort algorythm with callbacks, to sort
   anything you want.
   
   Usage: You must provide a function to determine
   the relation of two elements (greater,equal,less),
   and a procedure to swap two elements.
   
   Then just call Sort and voila'!
   
  --------------------------------------------------

  -[Requirements]-----------------------------------

   Nope...

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//     - Initial creation
//  V1.01: 2010.11.25 - Gilby
//     + Added support for "of object" callbacks

{$mode delphi}
{$smartlink on}

unit QuickSortUnit;

interface

type 
  TSortCallBackBigger=function(index1,index2:integer):byte;
  TSortCallBackBiggerF=function(index1,index2:integer):byte of Object;
  // Result=1 the first value is biggee
  //        2 the second value is bigger
  //        0 the two values are equal
  TSortCallBackSwap=procedure(index1,index2:integer);
  TSortCallBackSwapF=procedure(index1,index2:integer) of Object;

procedure Sort(First,Last:integer;
               iCallBackB:TSortCallBackBigger;
               iCallBackS:TSortCallBackSwap); overload;

procedure Sort(First,Last:integer;
               iCallBackB:TSortCallBackBiggerF;
               iCallBackS:TSortCallBackSwapF); overload;

implementation

uses Logger;

const 
  Fstr='QuickSortUnit.pas, ';
  Version='1.01';

function Partition(First,Last:integer;
                   iCallBackB:TSortCallBackBigger;
                   iCallBackS:TSortCallBackSwap):integer; overload;
var Pivot,Up,Down:integer;
begin
//  if Last>=First then exit;
  if Last<=First then exit;
  Pivot:=First;
  Up:=First;Down:=Last;
  repeat
    while (Up<Last) and (iCallBackB(Pivot,Up)<>2) do inc(Up);
//    writeln(up);
    while (Down>First) and (iCallBackB(Pivot,Down)<>1) do dec(Down);
//    writeln(down);
    if Up<Down then begin
      iCallBacks(Up,Down);
      if Pivot=Up then Pivot:=Down
      else if Pivot=Down then Pivot:=Up;
    end;
  until Up>=Down;
  iCallBackS(First,Down);
  Partition:=Down;
end;

function Partition(First,Last:integer;
                   iCallBackB:TSortCallBackBiggerF;
                   iCallBackS:TSortCallBackSwapF):integer; overload;
var Pivot,Up,Down:integer;
begin
//  if Last>=First then exit;
  if Last<=First then exit;
  Pivot:=First;
  Up:=First;Down:=Last;
  repeat
    while (Up<Last) and (iCallBackB(Pivot,Up)<>2) do inc(Up);
//    writeln(up);
    while (Down>First) and (iCallBackB(Pivot,Down)<>1) do dec(Down);
//    writeln(down);
    if Up<Down then begin
      iCallBacks(Up,Down);
      if Pivot=Up then Pivot:=Down
      else if Pivot=Down then Pivot:=Up;
    end;
  until Up>=Down;
  iCallBackS(First,Down);
  Partition:=Down;
end;

procedure Sort(First,Last:integer;
               iCallBackB:TSortCallBackBigger;
               iCallBackS:TSortCallBackSwap);
var PivIndex:integer;
begin
//  writeln(First,',',Last);readkey;
  if First<Last then begin
    PivIndex:=Partition(First,Last,iCallBackB,iCallBackS);
//    writeln(PivIndex);
    Sort(First,PivIndex-1,iCallBackB,iCallBackS);
    Sort(PivIndex+1,Last,iCallBackB,iCallBackS);
  end;
end;

procedure Sort(First,Last:integer;
               iCallBackB:TSortCallBackBiggerF;
               iCallBackS:TSortCallBackSwapF);
var PivIndex:integer;
begin
//  writeln(First,',',Last);readkey;
  if First<Last then begin
    PivIndex:=Partition(First,Last,iCallBackB,iCallBackS);
//    writeln(PivIndex);
    Sort(First,PivIndex-1,iCallBackB,iCallBackS);
    Sort(PivIndex+1,Last,iCallBackB,iCallBackS);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
