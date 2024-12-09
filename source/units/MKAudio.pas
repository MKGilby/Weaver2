// MKSZTSZ BASS Wrapper
// ------------------------------------------------------------------
// You can freely distribute the sources under the GNU GPL Version 2.
//
// Written by Gilby/MKSZTSZ
// Hungary, 2017
// ------------------------------------------------------------------

// Version info:
//   V1.00 - 2017.02.19 - Gilby
//     * Initial creation from MK_SDL
//   V1.00a - 2021.10.10 - Gilby
//     - Removed Classes from uses and moved Windows to implementation

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MKAudio;

interface

uses
  Bass;

var
  SoundEnabled : boolean;

// Audio
  procedure Init_Audio;

implementation

uses Windows, SysUtils, Logger;

const 
  Fstr={$I %FILE%}+', ';
  Version='1.00a';

procedure Finalize;
const Istr=Fstr+'Finalize';
begin
  if SoundEnabled then begin
    BASS_Free;
    Log.LogStatus('BASS: Shutdown...',Istr);
  end;
  SoundEnabled:=false;
end;

procedure Init_Audio;
const Istr=Fstr+'Init_Audio';
begin
  if (HIWORD(BASS_GetVersion) <> BASSVERSION) then begin
    Log.LogError('An incorrect version of BASS.DLL was loaded',Istr);
    SoundEnabled:=false;
    Exit;
  end;
  Log.LogStatus('BASS V2.4 loaded.',Istr);

  if not BASS_Init(-1, 44100, 0, 0, nil) then begin
    Log.LogError('BASS: Can''t initialize device!',Istr);
    SoundEnabled:=false;
    exit;
  end;
  Log.LogStatus('BASS: Device initialized.',Istr);

  SoundEnabled:=true;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  SoundEnabled:=false;

finalization
  Finalize;

end.

