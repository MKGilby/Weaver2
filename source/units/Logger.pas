{$IFDEF FPC}  
{$MODE DELPHI}
{$smartlink on}
{$ENDIF}

unit Logger;
{******************************************************************************}
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominique Louis.                                   }
{                                                                              }
{******************************************************************************}

// Version info:
//
//  V1.00: Gilby
//    * Initial creation based on Dominique Louis' work. 
//  V1.01: Gilby
//    + Delphi compatibility.
//  V1.02: Gilby - 2010.11.03
//    + Create now can have a Filename parameter. If it is empty, file
//      will be created as in previous versions.
//    + Using ParametersUnit to evaluate run time switches.
//  V1.03: Gilby - 2011.02.08
//    + Class is no longer a descendant of TFileStream.
//    + Now the file can be read by external apps if the host app is still running.      
//  V1.04: Gilby - 2011.02.20
//    + LogDebug added
//  V1.05: Gilby - 2011.06.21
//    * Create has one more parameter: Append:boolean
//  V1.06: Gilby - 2011.06.24
//    + You can specify which units or procedures/functions/methods can log
//      messages. Use Filter property (TStringList) to set filters.
//      The filter is applied to Location parameter of LogAnything.
//      If no filter specified everything will be logged.
//      Filters starting with - are disabling filters.
//      If multiple filters are added, the list is treated as a whitelist
//      '*' in the first or the last position means wildcard. 
//      Examples:
//         Log.Filter.Add('MKFonts.pas*')
//           -> enables all log lines from MKFonts.pas
//         Log.Filter.Add('-MKFonts.pas, LoadFont')
//           -> disables lines from MKFonts.pas, Loadfont
//  V1.07: Gilby - 2012.02.15
//    + Delphi XE2 compatibility (Widechars...)
//  V1.08: Gilby - 2012.02.29
//    * Slight rework
//    + Write added (just write anything into the log)
//  V1.09: Gilby - 2012.03.27
//    * Delphi XE2 compatibility fixes
//  V1.10: Gilby - 2012.05.29
//    * Append mode didn't work when the file didn't exist
//  V1.11: Gilby - 2012.08.16
//    * Destructor is now Destroy
//  V1.12: Gilby - 2013.11.25
//    * Minor modifications (nothing in the functionality)
//  V1.13: Gilby - 2015.04.14
//    * Write and Writeln can write to screen too.
//  V1.13a: Gilby - 2015.04.29
//    * Write and Writeln can write to screen too. (FIX)
//  V1.14: Gilby - 2019.05.07
//    * SetLogLevel reworked. Now it really sets the minimum severity to log.
//  V1.15: Gilby - 2023.02.13
//    * Removed filtering options.
//    * Added indenting. Indenting works with everything.

interface

uses
  Classes;

type
  TLogLevel=(llAll,llDebug,llStatus,llWarning,llError,llNone);

  { TLogger }

  TLogger = class
  private
    fStream:TFileStream;
    fApplicationName : string;
    fApplicationPath : string;
//    fCreated:boolean;
{$ifndef fpc}
    fPreTick:integer;
{$endif}
    fScreenOutput:boolean;
    fLogLevel:TLogLevel;
    fIndent:integer;
  public
    constructor Create( FileName : string = ''; Append : boolean = false );
    destructor Destroy; override;
    procedure LogAppHead( Head :  string; Location : string = '' );
    procedure LogError( ErrorMessage : string; Location : string = '' );
    procedure LogWarning( WarningMessage : string; Location : string = '' );
    procedure LogStatus( StatusMessage : string; Location : string = '' );
    procedure LogDebug( DebugMessage : string; Location : string = '' );
{$ifndef fpc}
    procedure LogTick( TickMessage : string );
{$endif}
    procedure DumpStream( stream : TStream;
                          start , size : integer;
                          Message , Location : string );
{$ifdef fpc}
    procedure DumpMemory( var p ;
                          start , size : integer;
                          Message , Location : string );
{$endif}
    procedure Trace( TraceMessage : String ); overload;
    procedure Trace( l : longint ); overload;
    procedure SetScreenOutputOn;
    procedure SetScreenOutputOff;
    procedure SetLogLevel(pLogLevel:TLogLevel);
    procedure Write( s : string );
    procedure WriteLN( s : string );
    procedure IncreaseIndent(value:integer);
    procedure DecreaseIndent(value:integer);
  public
    property ApplicationName : string read fApplicationName;
    property ApplicationPath : string read fApplicationPath;
  end;

var
  Log : TLogger;

implementation

uses SysUtils, Windows, ParametersUnit;

const
  Fstr={$I %FILE%}+', ';
  sysstr='..system..';
  Version='1.15';
  HexChars='0123456789ABCDEF';

{ Helper functions borrowed from MKToolBox }
function LPad(c:char;s:String;num:integer):String;
begin
  while length(s)<num do s:=c+s;
  Result:=s;
end;

function Dec2Hex(l:cardinal):string;overload;
var x:cardinal;s:string;
begin
  x:=$10000000;
  s:='';
  repeat
    s:=s+HexChars[(l div x)+1];
    l:=l mod x;
    x:=x shr 4;
  until x=0;
  s:=s+'h';
  while s[1]='0' do delete(s,1,1);
  if length(s)<2 then s:='0'+s;
  delete(s,length(s),1);
  if (length(s) and 1)=1 then s:='0'+s;
  Result:=s;
end;

function Dec2Hex(l:cardinal;fix:byte):string;overload;
begin
  Result:=lpad('0',Dec2Hex(l),fix);
end;

function GetIndentSpaces(count:integer):string;
const SPACES='                                ';  // 32
begin
  Result:='';
  while count>32 do begin
    Result+=SPACES;
    dec(count,32);
  end;
  if Count>0 then
    Result+=copy(SPACES,1,count);
end;

{ TLogger }
constructor TLogger.Create( FileName : string; Append : boolean );
begin
  fScreenOutput:=false;
  fApplicationName := ExtractFileName( Parameters[0] );
  fApplicationPath := ExtractFilePath( Parameters[0] );
  
  if (Parameters.IndexOfSwitch('nolog',true)>-1) then exit;

  if (Parameters.IndexOfSwitch('waitlog',true)>-1) then Sleep(1000);

  if (Parameters.IndexOfSwitch('appendlog',true)>-1) then Append:=true;

  if FileName = '' then begin
    FileName:=Parameters.GetNextOfSwitch('mainlog');
    if FileName = '' then
      FileName := fApplicationPath
                  + Copy( fApplicationName, 1, pos('.',fApplicationName+'.')-1)
                  + '.log';
  end;
  
  if (Parameters.IndexOfSwitch('safelog')>-1) then FileName := fApplicationPath + 'run.log';

  if not Append or not fileexists(Filename) then begin
    fStream:=TFileStream.Create( FileName, fmCreate or fmShareDenyNone );
    FreeAndNil(fStream);
  end;
  fStream:=TFileStream.Create( FileName, fmOpenWrite or fmShareDenyWrite );
  fStream.Seek(0,soFromEnd);

  SetLogLevel(llAll);
  fIndent:=0;
end;

destructor TLogger.Destroy;
begin
  if Assigned(fStream) then FreeAndNil(fStream);
  inherited ;
end;

procedure TLogger.Write( s : string );
begin
  if Assigned(fStream) then begin
    s:=GetIndentSpaces(fIndent)+s;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.write(s);
  end;
end;

procedure TLogger.WriteLN( s : string );
begin
  if Assigned(fStream) then begin
    s:=GetIndentSpaces(fIndent)+s+#13#10;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.write(s);
  end;
end;

procedure TLogger.IncreaseIndent(value:integer);
begin
  inc(fIndent,value);
end;

procedure TLogger.DecreaseIndent(value:integer);
begin
  dec(fIndent,value);
  if fIndent<0 then fIndent:=0;
end;

// Everybody can log error messages, so no filtering here. 
procedure TLogger.LogError(ErrorMessage:string; Location:string);
var s:string;
begin
  if Assigned(fStream) and (fLogLevel<=llError) then begin
    s:='{'+Location+'} ' + GetIndentSpaces(fIndent) + ErrorMessage + #13#10;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.writeln('Error: ',ErrorMessage);
  end;
end;

procedure TLogger.LogAppHead(Head:string; Location:string);
var
  S,S2 : string;
begin
  if Assigned(fStream) then begin
    S := '<'+Location+'> ' + GetIndentSpaces(fIndent) + Head;
    S2:='-';
    while length(S2)<length(S) do S2:=S2+'-';
    S:=S+#13#10;
    S2:=S2+#13#10;
    fStream.write(S2[1], length(S2)*sizeof(char));
    fStream.write(S[1], length(S)*sizeof(char));
    fStream.write(S2[1], length(S2)*sizeof(char));
  end;
end;

procedure TLogger.LogStatus(StatusMessage:string; Location:string);
var s:string;
begin
  if Assigned(fStream) and (fLogLevel<=llStatus) then begin
    s:= '('+Location+') ' + GetIndentSpaces(fIndent) + StatusMessage + #13#10;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.writeln(StatusMessage);
  end;
end;

procedure TLogger.LogDebug(DebugMessage:string; Location:string);
var s:String;
begin
  if Assigned(fStream) and (fLogLevel<=llDebug) then begin
    s:='<'+Location+'> ' + GetIndentSpaces(fIndent) + DebugMessage+#13#10;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.writeln(DebugMessage);
  end;
end;

procedure TLogger.LogWarning(WarningMessage:string; Location:string);
var s:String;
begin
  if Assigned(fStream) and (fLogLevel<=llWarning) then begin
    s:='['+Location+'] ' + GetIndentSpaces(fIndent) + WarningMessage+#13#10;
    fStream.write(S[1], length(S)*sizeof(char));
    if fScreenOutput then system.writeln('Warning: ',WarningMessage);
  end;
end;

procedure TLogger.DumpStream(stream:TStream;start,size:integer;Message,Location:string);
var
  s,r : string;
  t,i : integer;
    b : byte;

begin
  if Assigned(fStream) then begin
    WriteLN('');
    WriteLN('Stream dump invoked from "'+Location+'".');
    WriteLN('Comment: ' + Message);
    WriteLN('Stream full size: ' + inttostr(stream.Size));
    WriteLN('Dump start: ' + inttostr(start));
    WriteLN('Dump length: ' + inttostr(size));
    WriteLN('----------------------------- Dump start --------------------------------');
    t:=stream.Position;
    stream.Seek(start,soFromBeginning);
    i:=start;
    s:=dec2hex(i,8)+' ';
    r:='';
    b:=0; // To supress warning in Lazarus
    while size>0 do begin
      stream.Read(b,1);
      s:=s+dec2hex(b,2)+' ';
      if b in [6,7,10,13,9,8,0,255] then b:=46;
      r:=r+chr(b);
      inc(i);
      if (i-start) mod 16=0 then begin
        WriteLN(s+r);
        s:=dec2hex(i,8)+' ';
        r:='';
      end;
      dec(size);
    end;
    if r<>'' then begin
      while length(s)<57 do s:=s+' ';
      WriteLN(s+r);
    end;
    stream.Seek(t,soFromBeginning);
    WriteLN('-----------------------------  Dump end  --------------------------------');
  end;
end;

{$ifdef fpc}
procedure TLogger.DumpMemory(var p;start,size:integer;Message,Location:string);
var
  S,r : string;
    i : dword;
    b : byte;
    t : pointer;

begin
  if Assigned(fStream) then begin
    WriteLN ('Memory dump invoked from "'+Location+'".');
    WriteLN('Comment: ' + Message);
    WriteLN('Dump start: ' + inttostr(start));
    WriteLN('Dump length: ' + inttostr(size));
    WriteLN('----------------------------- Dump start --------------------------------');
    t:=@p+start;
    i:=start;
    s:=dec2hex(i,8)+' ';
    r:='';

    while size>0 do begin
      b:=byte(t^);
      inc(t);
      s+=dec2hex(b,2)+' ';
      if b in [6,7,10,13,9,8,0,255] then b:=46;
      r+=chr(b);
      inc(i);
      if (t-@p) mod 16=0 then begin
        WriteLN(s+r);
        s:=dec2hex(i,8)+' ';
        r:='';
      end;
      dec(size);
    end;
    if r<>'' then begin
      while length(s)<57 do s+=' ';
      WriteLN(s+r);
    end;
    WriteLN('-----------------------------  Dump end  --------------------------------');
  end;
end;
{$endif}

procedure TLogger.Trace(TraceMessage:String);
var s:String;
begin
  if Assigned(fStream) then begin
    s:='< Trace > ' + GetIndentSpaces(fIndent) + TraceMessage+#13#10;
    fStream.write(s[1], length(s)*sizeof(char));
    if fScreenOutput then system.writeln('Warning: ',TraceMessage);
  end;
end;

procedure TLogger.Trace(l:longint);
begin
  Trace(inttostr(l));
end;

{$ifndef fpc}
procedure TLogger.LogTick(TickMessage:string);
var
//  S : string;
  Now:integer;
begin
  now:=GetTickCount;
  if fPreTick>0 then 
    WriteLN('- TICK - : ' + inttostr(now-fPreTick) + ' ms. - MSG : ' + TickMessage);
  fPreTick:=now;
end;
{$endif}

procedure TLogger.SetScreenOutputOn;
begin
  fScreenOutput:=true;
end;

procedure TLogger.SetScreenOutputOff;
begin
  fScreenOutput:=false;
end;

procedure TLogger.SetLogLevel(pLogLevel:TLogLevel);
begin
  fLogLevel:=pLogLevel;
  Self.Write(GetIndentSpaces(fIndent)+'Minimum logging level set to ');
  case fLogLevel of
    llAll:Self.WriteLN('ALL');
    llDebug:Self.Writeln('DEBUG');
    llStatus:Self.Writeln('STATUS');
    llWarning:Self.Writeln('WARNING');
    llError:Self.Writeln('ERROR');
    llNone:Self.Writeln('NONE');
  end;
end;

initialization
  Log := TLogger.Create;
  Log.LogStatus('Starting Application. '+DateToStr(Date)+' '+TimeToStr(Time),sysstr);
  Log.LogStatus('(Status) [Warning] {Error} <Debug>',sysstr);
  Log.LogStatus(Fstr+'version '+Version,'uses');

finalization
  Log.LogStatus('Terminating Application. '+DateToStr(Date)+' '+TimeToStr(Time),sysstr);
  FreeAndNil(Log);

end.
