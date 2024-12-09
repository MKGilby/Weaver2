{ -[Name]-------------------------------------------

                 Parameters Class Unit 

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2010-2012

  --------------------------------------------------

  -[Description]------------------------------------

   Provides a list of command line parameters and
   easy to use functions for check switches, etc.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//      - Initial creation
//  V1.01: 2010.11.15 - Gilby
//      * Now Items[0] contains the full qualified path to the executable.
//        Previously Items[0] contained paramstr(1) and so on.
//  V1.02: 2010.11.17 - Gilby
//      + Count:integer property
//      * Items is set to default property so you can access parameters like
//        Parameters[1]
//  V1.03: 2010.11.25 - Gilby
//      + IndexOfSwitch function. Checks for both -x and /x
//  V1.04: 2011.09.08 - Gilby
//      + IndexOfSwitch function checks for uppercase too.
//      + IndexOfSwitch has a hidden second parameter, which you can set to
//           true if you want to remove the parameter 
//  V1.05: 2011.12.21 - Gilby
//      * BUGFIX: IndexOfSwitch
//      + GetNextOfSwitch function added 
//  V1.06: 2012.08.17 - Gilby
//      * Destructor is now Destroy

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit ParametersUnit;

interface

type TStringArray=array of string;

type TParameters=class
       constructor Create;
       destructor Destroy; override;
       function IndexOf(s:String):integer;
       function GetNextOf(s:String):string;
       function DeleteIfExists(s:string):boolean;
       function Delete(i:integer):boolean;
       function IndexOfSwitch(s:String;remove:boolean=false):integer;
       function GetNextOfSwitch(s:String;remove:boolean=false):string;
     private
       fParms:TStringArray;
       function fGetCount:integer;
       function fGetParm(index:integer):string;
     public
       property Items[index:integer]:string read fGetParm; default;
       property Count:integer read fGetCount;
     end;
    
var Parameters:TParameters;    
     
implementation

uses SysUtils;

constructor TParameters.Create;
var i:integer;
begin
  SetLength(fParms,paramcount+1);
  for i:=0 to paramcount do
    fParms[i]:=paramstr(i);
end;

destructor TParameters.Destroy;
begin
  SetLength(fParms,0);
  inherited ;
end;

function TParameters.IndexOf(s:String):integer;
var i:integer;
begin
  for i:=0 to length(fParms)-1 do
    if uppercase(s)=uppercase(fParms[i]) then begin
      Result:=i;
      exit;
    end;
  Result:=-1; 
end;

function TParameters.GetNextOf(s:String):string;
var i:integer;
begin
  i:=IndexOf(s);
  if (i>-1) and (i<length(fParms)-1) then Result:=fParms[i+1] else Result:=''; 
end;

function TParameters.DeleteIfExists(s:string):boolean;
var i:integer;
begin
  i:=IndexOf(s);
  if i>-1 then begin
    Delete(i);
    Result:=true;
  end else Result:=false;
end;

function TParameters.Delete(i:integer):boolean;
var j:integer;
begin
  if (i>-1) and (i<length(fParms)) then begin
    for j:=i to length(fParms)-2 do
      fParms[j]:=fParms[j+1];
    SetLength(fParms,length(fParms)-1);   
    Result:=true;
  end else Result:=false;
end;

function TParameters.fGetCount:integer;
begin
  Result:=length(fParms);
end;

function TParameters.fGetParm(index:integer):string;
begin
  Result:=fParms[index];
end;

function TParameters.IndexOfSwitch(s:String;remove:boolean=false):integer;
begin
  Result:=IndexOf('-'+s);
  if Result=-1 then Result:=IndexOf('/'+s);
  if Result=-1 then Result:=IndexOf('-'+uppercase(s));
  if Result=-1 then Result:=IndexOf('/'+uppercase(s));
  if remove and (Result>-1) then Delete(Result);
end;

function TParameters.GetNextOfSwitch(s:String;remove:boolean=false):string;
var i:integer;
begin
  i:=IndexOfSwitch(s);
  if (i>-1) and (i<length(fParms)-1) then begin
    Result:=fParms[i+1];
    if remove then begin
      Delete(i+1);
      Delete(i);
    end;  
  end else begin
    Result:='';
  end;   
end;

initialization
  Parameters:=TParameters.Create;

finalization
  FreeAndNil(Parameters);

end.
