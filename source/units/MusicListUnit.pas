{ -[Name]-------------------------------------------

               MKSZTSZ MusicList class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020-2023

  --------------------------------------------------

  -[Description]------------------------------------

   MusicList class to store TMusic

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.01.15
//     - Initial creation from MusicCollectionUnit.
//  V1.01: Gilby - 2021.10.10
//     - Tidying up.
//  V1.02: Gilby - 2023.01.25
//     * Changed Traces to LogDebug in List method.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MusicListUnit;

interface

uses Lists, MusicUnit, Bass {needed for float};

type
  TMusicListItem=class(TObject)
    destructor Destroy; override;
  public
    _name:string[24];
    _music:TMusic;
    _title:string;
    _author:string;
    _originalfilename:string;
  end;

  TMusicList=class(TNamedList<TMusicListItem>)
  public
    // Creates the list.
    constructor Create;

    // Destroys the list, free up memory and the musics too!
    destructor Destroy; override;

    // Add a music to the list with the specified name.
    procedure Add(iMusic:TMusic;iName:String); overload;

    // Add a music to the list from external file.
    procedure Add(iFilename,iName:String); overload;

    // Enables if disabled, disables if enabled
    procedure ToggleMusic;

    // List items into Log
    procedure List;
  protected
    fGlobalVolume:float;
    fEnabled:boolean;
    function SearchByName(iName:string):longint;
    function fGetMusicListItem(index:integer):TMusicListItem;
    function fGetMusicListItem2(index:string):TMusicListItem;
    procedure fSetEnabled(value:boolean);
    procedure fSetGlobalVolume(value:float);
  public
    property MusicByIndex[index:integer]:TMusicListItem read fGetMusicListItem;
    property Music[index:string]:TMusicListItem read fGetMusicListItem2; default;
    property Enabled:boolean read fEnabled write fSetEnabled;
    property GlobalVolume:float read fGlobalVolume write fSetGlobalVolume;
  end;

implementation

uses SysUtils, MKToolBox, Logger, MKStream;

const
  Fstr={$I %FILE%}+', ';
  Version='1.02';

destructor TMusicListItem.Destroy;
const Istr=Fstr+'TMusicListItem.Destroy';
begin
  Log.LogDebug('Destroying music '+_name, Istr);
  FreeAndNil(_music);
end;

constructor TMusicList.Create;
begin
  inherited ;
  fGlobalVolume:=1;
  fEnabled:=true;
end;

destructor TMusicList.Destroy;
begin
  Clear;
  inherited ;
end;

procedure TMusicList.Add(iMusic:TMusic;iName:String);
var atm:TMusicListItem;
begin
  atm:=TMusicListItem.Create;
  atm._music:=iMusic;
  atm._name:=iName;
  AddObject(iName,atm);
end;

function TMusicList.SearchByName(iName:string):longint;
var i:longint;
begin
  for i:=0 to Count-1 do
    if MusicByIndex[i]._name=iName then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

procedure TMusicList.Add(iFilename,iName:string);
begin
  Add(TMusic.Create(iFilename),iName);
end;

function TMusicList.fGetMusicListItem(index:integer):TMusicListItem;
begin
  if (index>=0) and (index<Count) then
    Result:=TMusicListItem(Items[index])
  else
    Result:=nil;
end;

function TMusicList.fGetMusicListItem2(index:string):TMusicListItem;
var i:integer;
begin
  i:=SearchByName(index);
  if i>-1 then
    Result:=TMusicListItem(Items[i])
  else
    Result:=nil;
end;

procedure TMusicList.fSetEnabled(value:boolean);
begin
  if fEnabled<>value then ToggleMusic;
end;

procedure TMusicList.ToggleMusic;
var i:integer;
begin
  fEnabled:=not fEnabled;
  if not fEnabled then begin
    for i:=0 to Count-1 do
      MusicByIndex[i]._music.Volume:=0;
  end else begin
    for i:=0 to Count-1 do
      MusicByIndex[i]._music.Volume:=fGlobalVolume;
  end;
end;

procedure TMusicList.fSetGlobalVolume(value:float);
var i:integer;
begin
  if value<0 then value:=0
  else if value>1 then value:=1;
  fGlobalVolume:=value;
  if fEnabled then
    for i:=0 to Count-1 do
      MusicByIndex[i]._music.Volume:=fGlobalVolume;
end;

procedure TMusicList.List;
var i:integer;
begin
  Log.LogDebug('MusicList listing starts.');
  for i:=0 to Count-1 do begin
    Log.LogDebug(inttostr(i+1)+'. '+MusicByIndex[i]._Name);
  end;
  Log.LogDebug('MusicList listing ends.');
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

