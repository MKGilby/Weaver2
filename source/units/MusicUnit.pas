{ -[Name]-------------------------------------------

                  MKSZTSZ Music class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2012-2017

  --------------------------------------------------

  -[Description]------------------------------------

   Music class

  --------------------------------------------------
}

// Version info:
//
//  V1.00: 2012.08.30 - Gilby:
//     * Initial creation
//  V1.01: 2014.09.16 - Gilby:
//     + Handle property
//  V1.01a: Gilby - 2017.02.21
//     * Using MKStream instead of MKStream3
//  V1.02: Gilby - 2018.01.15
//     + Loading from stream (by specifying start and length)
//     * Destroy now frees MP3 streams too

{$mode delphi}
{$smartlink on}

unit MusicUnit;

interface

uses Bass, Classes;

type
  TMusic=class
    constructor Create(iFilename:string);
    constructor CreateMP3(iStream:TStream;iStart,iLength:integer;iLoop:boolean=false);
    destructor Destroy; override;
    procedure Play;
    procedure Stop;
    procedure Continue;
  private
    fType:(mtMOD,mtMP3);
    fMusic:dword;
    fData:pointer;
    fSize:integer;
    function fGetVolume:float;
    procedure fSetVolume(iVolume:float);
  public
    property Volume:float read fGetVolume write fSetVolume;
    property Handle:dword read fMusic;
  end;

implementation

uses SysUtils, MKStream, Logger;

const
  Fstr='MusicUnit.pas, ';
  Version='1.02';

constructor TMusic.Create(iFilename:string);
const Istr=Fstr+'TMusic.Create';
var
  Xs:TStream;
  atm:TMKFileInfo;
  pc:PChar;
begin
  if (length(iFilename)>4) and (uppercase(copy(iFilename,length(iFilename)-3,4))='.MP3') then begin
// Loading an mp3. It is stored uncompressed in a MAD4 or on disk.
// 1. Get source of the file.
    atm:=MKStreamOpener.GetFileInfo(iFilename);
//    if atm._compressed then Log.Trace('!Comp') else Log.Trace('!Rawd');
//    Log.Trace(atm._filename);
    pc:=pchar(atm._filename);
    fType:=mtMP3;
    if not atm._compressed then begin
      fSize:=-1;
      fData:=nil;
      fMusic:= BASS_StreamCreateFile(false, pc, atm._start, atm._size, BASS_SAMPLE_LOOP);
    end else begin
      Xs:=MKStreamOpener.OpenStream(iFilename);
      fSize:=Xs.Size;
      getmem(fData,fSize);
      Xs.Read(fData^,fSize);
      FreeAndNil(Xs);
      fMusic := BASS_StreamCreateFile(TRUE, fData, 0, fSize, BASS_SAMPLE_LOOP);
    end;
  end else begin
    fType:=mtMOD;
    Xs:=MKStreamOpener.OpenStream(iFilename);
    fSize:=Xs.Size;
    getmem(fData,fSize);
    Xs.Read(fData^,fSize);
    FreeAndNil(Xs);

    fMusic:=BASS_MusicLoad(TRUE, fData, 0, fSize, BASS_MUSIC_LOOP or BASS_MUSIC_RAMPS, 0);
//    if _music>0 then Log.LogDebug('Music file...',Istr);
  end;

  if fMusic = 0 then
    Log.LogError('BASS: Loading '+iFilename+' failed',Istr)
  else
    Log.LogStatus('BASS: Loading '+iFilename+' was successful. (fMusic='+inttostr(fMusic)+')',Istr);
end;

constructor TMusic.CreateMP3(iStream:TStream;iStart,iLength:integer;iLoop:boolean);
begin
  fType:=mtMP3;
  fSize:=iLength;
  getmem(fData,fSize);
  iStream.Seek(iStart,soFromBeginning);
  iStream.Read(fData^,fSize);
  if iLoop then
    fMusic := BASS_StreamCreateFile(TRUE, fData, 0, fSize, BASS_SAMPLE_LOOP)
  else
    fMusic := BASS_StreamCreateFile(TRUE, fData, 0, fSize, 0);
end;

destructor TMusic.Destroy;
const Istr=Fstr+'TMusic.Destroy';
begin
  if fMusic<>0 then begin
    case fType of
      mtMOD:BASS_MusicFree(fMusic);
      mtMP3:BASS_StreamFree(fMusic);
    end;
    if (fData<>nil) and (fSize<>-1) then freemem(fData,fSize);
  end;
  Log.LogStatus('BASS: Music freed.',Istr);
  inherited ;
end;

procedure TMusic.Play;
const Istr=Fstr+'TMusic.Play';
begin
  if fMusic<>0 then begin
    BASS_ChannelPlay(fMusic,true);
    Log.LogStatus('BASS: Music started.',Istr);
  end;
end;

procedure TMusic.Stop;
const Istr=Fstr+'TMusic.Stop';
begin
  if fMusic<>0 then begin
    BASS_ChannelStop(fMusic);
    Log.LogStatus('BASS: Music stopped.',Istr);
  end;
end;

procedure TMusic.Continue;
const Istr=Fstr+'TMusic.Continue';
begin
  if fMusic<>0 then begin
    BASS_ChannelPlay(fMusic,false);
    Log.LogStatus('BASS: Music continued.',Istr);
  end;
end;

function TMusic.fGetVolume:float;
begin
  Result:=0;
  BASS_ChannelGetAttribute(fMusic,BASS_ATTRIB_Vol,Result);
end;

procedure TMusic.fSetVolume(iVolume:float);
begin
  if fMusic<>0 then begin
    if iVolume<0 then iVolume:=0
    else if iVolume>1 then iVolume:=1;
//    Log.Trace('Hot! '+inttostr(fMusic));
    BASS_ChannelSetAttribute(fMusic,BASS_ATTRIB_Vol,iVolume);
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
