{ -[Name]-------------------------------------------

               MKSZTSZ WaveList Class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2003-2013

  --------------------------------------------------

  -[Description]------------------------------------

   WaveList class to hold sound effects.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.01.15
//     - Initial creation from WaveCollectionUnit

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit WaveListUnit;

interface

uses Classes, Bass, WaveUnit, Lists;

const PlayFailed=$FFFFFFFF;

type
  SamplePlayHandle=DWORD;

  TWaveListItem=class(TObject)
    destructor Destroy; override;
  public
    _name:string[64];
    _wave:TWave;
    _defaultvolume:float;
  end;

  TWaveList=class(TNamedList<TWaveListItem>)
//    destructor Destroy; override;
    // Destroys the list, free up memory and the samples too!
    
    procedure Add(iWave:TWave;iName:String;iVolume:Float); overload;
    // Add a sample to the list with the specified name.

    procedure Add(iFilename,iName:String;iVolume:Float); overload;
    // Add a sample to the list from file.

    procedure List;
    // List the loaded sample names to the LOG.
  private
    fGlobalVolume:float;
    function fGetWaveIndex(iName:string):integer;
    function fGetWave(index:string):TWaveListItem;
    function fGetWave2(index:integer):TWaveListItem;
    procedure fSetGlobalVolume(Value:float);
  public
    property WaveByIndex[index:integer]:TWaveListItem read fGetWave2;
    property Waves[index:String]:TWaveListItem read fGetWave; default;
    property GlobalVolume:float read fGlobalVolume write fSetGlobalVolume;
  end;

implementation

uses SysUtils, MKToolBox, Logger, MKStream;

const Fstr='WaveListUnit.pas, ';
      Version='1.00';

destructor TWaveListItem.Destroy;
const Istr=Fstr+'TWaveListItem.Destroy';
begin
  Log.LogDebug('Destroying wave '+_name, Istr);
  FreeAndNil(_wave);
end;

{constructor TWaveList.Create;
begin

end;}

{destructor TWaveList.Destroy;
begin
  inherited ;
end;}

procedure TWaveList.Add(iWave:TWave;iName:String;iVolume:Float);
var atm:TWaveListItem;
begin
  atm:=TWaveListItem.Create;
  atm._wave:=iWave;
  atm._name:=iName;
  atm._defaultvolume:=iVolume;
  AddObject(iName,atm);
end;

procedure TWaveList.Add(iFilename,iName:string;iVolume:Float);
begin
  Add(TWave.Create(iFilename),iName,iVolume);
end;

procedure TWaveList.List;
const Istr=Fstr+'TWaveList.List';
var i:longint;
begin
  Log.LogStatus('WaveList name listing start',Istr);
  for i:=0 to Count-1 do
    Log.LogStatus(WaveByIndex[i]._name,Istr);
  Log.LogStatus('WaveList name listing end',Istr);
end;

function TWaveList.fGetWaveIndex(iName:string):integer;
var i:integer;
begin
  for i:=0 to Count-1 do
    if WaveByIndex[i]._name=iName then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

function TWaveList.fGetWave(index:string):TWaveListItem;
var i:integer;
begin
  for i:=0 to Count-1 do
    if WaveByIndex[i]._name=index then begin
      Result:=WaveByIndex[i];
      exit;
    end;
  Result:=nil;
end;

function TWaveList.fGetWave2(index:integer):TWaveListItem;
begin
  if (index>=0) and (index<Count) then
    Result:=TWaveListItem(Items[index])
  else
    Result:=nil;
end;

procedure TWaveList.fSetGlobalVolume(value:float);
var i:integer;
begin
  if value<0 then value:=0
  else if value>1 then value:=1;
  fGlobalVolume:=value;
//  if fEnabled then
    for i:=0 to Count-1 do with WaveByIndex[i] do
      _wave.Volume:=fGlobalVolume*_defaultvolume;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
