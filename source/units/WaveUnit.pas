{ -[Name]-------------------------------------------

                   MKSZTSZ Wave class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2012

  --------------------------------------------------

  -[Description]------------------------------------

   Music class

  --------------------------------------------------
}

// Version info:
//
//  V1.00: 2012.09.03 - Gilby:
//     - Initial creation
//  V1.01: 2012.09.04 - Gilby:
//     - Only one stop is enough
//  V1.01a: Gilby - 2017.02.21
//     * Using MKStream instead of MKStream3
//  V1.02: Gilby - 2020.01.15
//     * Improved logging in Create

{$mode delphi}
{$smartlink on}

unit WaveUnit;

interface

uses Bass;

type
  TWave=class
    constructor Create(iFilename:string;iMaxPlayback:integer=1;iLooped:boolean=false);
    destructor Destroy; override;
    function Play:HChannel;
    procedure Stop; overload;
  private
    fWave:HSample;
    fData:pointer;
    fSize:integer;
    function fGetFrequency:DWORD;
    procedure fSetFrequency(iValue:DWORD);
    function fGetVolume:float;
    procedure fSetVolume(iValue:float);
    function fGetPanning:float;
    procedure fSetPanning(iValue:float);
    function fGetFlags:DWORD;
    procedure fSetFlags(iValue:DWORD);
    function fGetLength:DWORD;
    function fGetMaxPlayback:DWORD;
    function fGetOrigRes:DWORD;
    function fGetChannels:DWORD;
    function fGetMinGap:DWORD;
    procedure fSetMinGap(iValue:DWORD);
  public
    property Frequency:DWORD read fGetFrequency write fSetFrequency;
    property Volume:float read fGetVolume write fSetVolume;
    property Panning:float read fGetPanning write fSetPanning;
    property Flags:DWORD read fGetFlags write fSetFlags;
    property Length:DWORD read fGetFlags;
    property MaxPlayback:DWORD read fGetMaxPlayback ;
    property OrigRes:DWORD read fGetOrigRes;
    property Channels:DWORD read fGetChannels;
  end;

const
  PlayFailed=$FFFFFFFF;

implementation

uses Classes, SysUtils, MKStream, Logger;

const
  Fstr='WaveUnit.pas, ';
  Version='1.02';

constructor TWave.Create(iFilename:string;iMaxPlayback:integer;iLooped:boolean=false);
const Istr=Fstr+'TWave.Create';
var
  Xs:TStream;
  atm:TMKFileInfo;
  pc:PChar;
begin
  atm:=MKStreamOpener.GetFileInfo(iFilename);
  pc:=pchar(atm._filename);
  if not atm._compressed then begin
    Log.LogDebug(Format('Not compressed (%s,%d,%d)',[atm._filename,atm._start,atm._size]),Istr);
    fSize:=-1;
    fData:=nil;
    if not iLooped then
      fWave:=BASS_SampleLoad(false, pc, atm._start, atm._size, iMaxPlayback, BASS_SAMPLE_OVER_POS)
    else
      fWave:=BASS_SampleLoad(false, pc, atm._start, atm._size, iMaxPlayback, BASS_SAMPLE_OVER_POS or BASS_SAMPLE_LOOP);
  end else begin
    Log.LogDebug('Compressed',Istr);
    Xs:=MKStreamOpener.OpenStream(iFilename);
    fSize:=Xs.Size;
    getmem(fData,fSize);
    Xs.Read(fData^,fSize);
    FreeAndNil(Xs);
    if not iLooped then
      fWave:=BASS_SampleLoad(true, fData, 0, fSize, iMaxPlayback, BASS_SAMPLE_OVER_POS)
    else
      fWave:=BASS_SampleLoad(true, fData, 0, fSize, iMaxPlayback, BASS_SAMPLE_OVER_POS or BASS_SAMPLE_LOOP);
  end;

  if fWave = 0 then
    Log.LogError(Format('BASS: Loading %s failed! (Errorcode: %d)',[iFilename,BASS_ErrorGetCode]),Istr)
  else
    Log.LogDebug(Format('BASS: Loading %s was successful. (fWave=%d)',[iFilename,fWave]),Istr);
end;

destructor TWave.Destroy;
const Istr=Fstr+'TWave.Destroy';
begin
  if fWave<>0 then begin
    BASS_SampleFree(fWave);
    if (fData<>nil) and (fSize<>-1) then freemem(fData,fSize);
  end;
  Log.LogStatus('BASS: Wave freed.',Istr);
  inherited ;
end;

function TWave.Play:HChannel;
const Istr=Fstr+'TWave.Play';
begin
  if fWave<>0 then begin
    Result:=BASS_SampleGetChannel(fWave,false);
    if not(Bass_ChannelPlay(Result,false)) then
      Log.LogError('BASS: Could not play sample!',Istr)
    else
      Result:=PlayFailed;
  end else Result:=PlayFailed;
end;

procedure TWave.Stop;
//const Istr=Fstr+'TWave.Stop';
begin
  if fWave<>0 then BASS_SampleStop(fWave);
end;

function TWave.fGetFrequency:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.freq;
end;

procedure TWave.fSetFrequency(iValue:DWORD);
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  s.freq:=iValue;
  BASS_SampleSetInfo(fWave, s);
end;

function TWave.fGetVolume:float;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.volume;
end;

procedure TWave.fSetVolume(iValue:float);
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  s.volume:=iValue;
  BASS_SampleSetInfo(fWave, s);
end;

function TWave.fGetPanning:float;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.pan;
end;

procedure TWave.fSetPanning(iValue:float);
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  s.pan:=iValue;
  BASS_SampleSetInfo(fWave, s);
end;

function TWave.fGetFlags:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.flags;
end;

procedure TWave.fSetFlags(iValue:DWORD);
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  s.flags:=iValue;
  BASS_SampleSetInfo(fWave, s);
end;

function TWave.fGetLength:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.length;
end;

function TWave.fGetMaxPlayback:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.max;
end;

function TWave.fGetOrigRes:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.origres;
end;

function TWave.fGetChannels:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.chans;
end;

function TWave.fGetMinGap:DWORD;
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  Result:=s.mingap;
end;

procedure TWave.fSetMinGap(iValue:DWORD);
var s:BASS_SAMPLE;
begin
  BASS_SampleGetInfo(fWave, s{%H-});
  s.mingap:=iValue;
  BASS_SampleSetInfo(fWave, s);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
