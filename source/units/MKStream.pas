{ -[Name]-------------------------------------------

                 MKOpenStream function 

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2008-2021

  --------------------------------------------------

  -[Description]------------------------------------

   One function for opening stream from multiple
   sources.

   You can add directories and other sources with
   their weight, then open stream from any source
   with a single method.
   When the same file exists in multiple sources,
   the file from the source with the highest weight
   will be opened.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby
//    * Initial creation
//  V1.02: 2010.06.22 - Gilby
//    + Uses MADHandlerUnit
//  V1.03: 2010.06.29 - Gilby
//    + Uses MAD4_VolumeLevel
//  V1.04: 2010.07.15 - Gilby
//    - Removed MKVD, MAD4 can do that too
//  V1.05: 2011.04.27 - Gilby
//    + Added loading from current workdir
//  V1.06: 2011.08.19 - Gilby
//    + MKGetFileInfo(filename):TMKFileInfo function added.
//  V1.07: 2012.02.27 - Gilby
//    * MAD4_VolumeLevel replaced by MAD4_HighLevel
//  V1.08: 2013.05.01 - Gilby
//  V1.09: 2015.01.13 - Gilby
//    * Files with .zl extension will be automatically uncompressed when
//        MKOpenStreamed from disk
//  V1.10: 2015.03.16 - Gilby
//    * Throws exception when cannot open file in MKOpenStream
//  V2.00: 2015.05.19 - Gilby
//    * It's now a class (TMKStreamOpener)
//    * BaseDir is for disk based operations
//    * CD is applied both disk and MAD4 based operations
//  V2.01: 2015.06.22 - Gilby
//    * BUGFIX with opening network files (starting with \\)
//  V2.02: 2016.02.17 - Gilby
//    + Added caching of last opened files
//  V2.03: 2016.07.04 - Gilby
//    * FileExists checks in cache too
//  V2.04: 2017.02.22 - Gilby
//    + TMKFileInfo added back
//    * GetFileInfo method added
//  V2.05: 2018.06.05 - Gilby
//    + The Fullpath check on disk is now tries the full original filename
//      instead the stripped one.
//  V2.06: 2018.12.16 - Gilby
//    + Option to disable caching (Gives back bad file when mounting a MAD4
//      file containing a file already in the cache.)
//  V2.07: 2018.12.20 - Gilby
//    * Fixed a memory leak
//  V2.08: 2019.05.29 - Gilby
//    * Fixed another memory leak
//  V2.09: 2020.01.29 - Gilby
//    * Fixed a range check error
//  V3.00: 2021.11.15 - Gilby
//    * Complete rework
//  V3.01: 2022.10.06 - Gilby
//    * Using fgl.TFGObjectList for TStreams instead of Lists.TGenericList
//  V3.01a: 2023.06.26 - Gilby
//    * Removed an unneeded variable declaration and two commented out lines.

{$ifdef fpc}
  {$mode delphi}
{$endif}

unit MKStream;

interface

uses Classes, Lists, fgl;

type
  TMKFileInfo=record
    _filename:string;
    _start:integer;
    _size:integer;
    _compressed:boolean;
  end;

  TStreamOpener=class
    function OpenStream(aFilename:string):TStream; virtual; abstract;
    function FileExists(aFilename:string):boolean; virtual; abstract;
    function GetFileInfo(pFilename:String):TMKFileInfo; virtual; abstract;
  end;

  TMKCacheItem=class
    destructor Destroy; override;
  public
    _filename:string;
    _stream:TStream;
  end;
  
  TMKCache=TNamedList<TMKCacheItem>;

  TStreamsItem=class
    destructor Destroy; override;
  public
    _Weight:integer;
    _StreamOpener:TStreamOpener;
  end;

  TStreams=TFPGObjectList<TStreamsItem>;

  TMKStreamOpener=class
    constructor Create;
    destructor Destroy; override;

    procedure AddDirectory(pSourceDir:string;pWeight:integer);
    procedure AddOtherSource(pSource:TStreamOpener;pWeight:integer);

//    procedure SetBaseDir(aPath:string);
//    procedure CD(aPath:string);
    function OpenStream(aFilename:string):TStream;
    function FileExists(aFilename:string):boolean;
    function GetFileInfo(aFilename:string):TMKFileInfo;
  private
    fBaseDir:string;
    fCurrentDir:string;
    fVerbose:boolean;
    fStreams:TStreams;
    fCache:TMKCache;
    fUseCache:boolean;
//    function GetRealFilename(aFilename:string):string;
  public
    property BaseDir:string read fBaseDir;
    property CurrentDir:string read fCurrentDir;
    property Verbose:boolean read fVerbose write fVerbose;
    property UseCache:boolean read fUseCache write fUseCache;
  end;

  TFileStreamOpener=class(TStreamOpener)
    constructor Create(iBaseDir:string='');
    function OpenStream(pFilename:string):TStream; override;
    function FileExists(pFilename:string):boolean; override;
    function GetFileInfo(pFilename:String):TMKFileInfo; override;
  private
    fBaseDir:string;
  end;

var
  MKStreamOpener:TMKStreamOpener;

implementation

uses SysUtils, MKToolBox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='3.01a';

var ExePath:string;

destructor TMKCacheItem.Destroy;
begin
  FreeAndNil(_stream);
  inherited ;
end;

destructor TStreamsItem.Destroy;
begin
  FreeAndNil(_StreamOpener);
  inherited ;
end;

constructor TMKStreamOpener.Create;
begin
  fBaseDir:=ExePath;
  fStreams:=TStreams.Create;
  fCache:=TMKCache.Create;
  fUseCache:=true;
end;

destructor TMKStreamOpener.Destroy;
begin
  FreeAndNil(fStreams);
  FreeAndNil(fCache);
  inherited ;
end;

procedure TMKStreamOpener.AddDirectory(pSourceDir:string;pWeight:integer);
var tmp:TFileStreamOpener;
begin
  tmp:=TFileStreamOpener.Create(pSourceDir);
  AddOtherSource(tmp,pWeight);
end;

procedure TMKStreamOpener.AddOtherSource(pSource:TStreamOpener;pWeight:integer);
var tmp:TStreamsItem;i:integer;
begin
  tmp:=TStreamsItem.Create;
  tmp._Weight:=pWeight;
  tmp._StreamOpener:=pSource;
  if fStreams.Count=0 then fStreams.Add(tmp)
  else begin
    i:=0;
    while (i<fStreams.Count) and (pWeight<fStreams[i]._Weight) do inc(i);
    if i=fStreams.Count then fStreams.Add(tmp) else fStreams.Insert(i,tmp);
  end;
end;

{procedure TMKStreamOpener.SetBaseDir(aPath:string);
begin
  if (length(aPath)>0) and (aPath[length(aPath)]<>'\') then aPath+='\';
  fBaseDir:=aPath;
end;}

{procedure TMKStreamOpener.CD(aPath:string);
begin
  if (length(aPath)>0) and (aPath[length(aPath)]<>'\') then aPath+='\';
  fCurrentDir:=aPath;
end;}

{function TMKStreamOpener.GetRealFilename(aFilename:string):string;
var s:string;
begin
  if length(aFilename)>0 then begin
    if copy(aFilename,1,2)='\\' then Result:=aFilename
    else if aFilename[1]='\' then result:=copy(aFilename,2,length(aFilename)-1)
                        else begin
      s:=fCurrentDir;
      while copy(aFilename,1,3)='..\' do begin
        if length(s)>0 then
          delete(s,rpos('\',s,2)+1,length(s)-rpos('\',s,2));
        delete(aFilename,1,3);
      end;
      Result:=s+aFileName;
    end;
  end else Result:='';
end;}

function TMKStreamOpener.OpenStream(aFilename:string):TStream;
const Istr=Fstr+'TMKStreamOpener.OpenStream';
var atm:TMKCacheItem;i:integer;
begin
  Log.LogDebug('OpenStream('''+aFilename+''')',Istr);
  Result:=NIL;

  if fUseCache then begin
    Log.LogDebug('Check in cache ('+aFilename+')',Istr);
    i:=fCache.IndexOf(aFilename);
    if i>-1 then begin
      atm:=fCache[i];
      Result:=TMemoryStream.Create;
      Result.CopyFrom(atm._stream,atm._stream.Size);
      atm._stream.Seek(0,soFromBeginning);
      Result.Seek(0,soFromBeginning);
      // Move the object to the end of the cache to keep it from dropping out.
      fCache.Exchange(i,fCache.Count-1);
//      fCache.Delete(i);
//      fCache.AddObject(atm._filename,atm);
      exit;
    end;
  end;

  i:=0;
  while (i<fStreams.Count) and (Result=nil) do begin
    Result:=fStreams[i]._StreamOpener.OpenStream(aFilename);
    inc(i);
  end;

  if Result<>nil then begin
    if fUseCache then begin
      if fCache.Count>64 then begin
        fCache.Items[0]._stream.Free;
        fCache.Items[0].Free;
        fCache.Delete(0);
      end;
      atm:=TMKCacheItem.Create;
      atm._filename:=aFilename;
      atm._stream:=TMemoryStream.Create;
      atm._stream.CopyFrom(Result,Result.Size);
      atm._stream.Seek(0,soFromBeginning);
      fCache.AddObject(aFilename,atm);
      Result.Seek(0,soFromBeginning);
    end;
  end else begin
    Log.LogError('Openstream failed: '+aFilename,Istr);
    raise Exception.Create('OpenStream failed: '+aFileName);
  end;
end;

function TMKStreamOpener.FileExists(aFilename:string):boolean;
const Istr=Fstr+'TMKStreamOpener.FileExists';
var i:integer;
begin
//  fn:=GetRealFilename(aFilename);
  Log.LogDebug('Check in cache ('+aFilename+')',Istr);
  Result:=false;
  if fCache.IndexOf(aFilename)=-1 then begin
    i:=0;
    while (i<fStreams.Count) and not Result do begin
      Result:=fStreams[i]._StreamOpener.FileExists(aFilename);
      inc(i);
    end;
  end else
    Result:=true;
end;

function TMKStreamOpener.GetFileInfo(aFilename:string):TMKFileInfo;
const Istr=Fstr+'TMKStreamOpener.OpenStream';
var i:integer;
begin
  Log.LogDebug('GetFileInfo('''+aFilename+''')',Istr);
//  fn:=GetRealFilename(aFilename);

  i:=0;
  Result._filename:='';
  Result._start:=-1;
  Result._size:=-1;
  while (i<fStreams.Count) and (Result._start=-1) do begin
    Result:=fStreams[i]._StreamOpener.GetFileInfo(aFilename);
    inc(i);
  end;

end;

{ TFileStreamOpener }

constructor TFileStreamOpener.Create(iBaseDir: string='');
begin
  fBaseDir:=iBaseDir;
  if (fBaseDir>'') and (fBaseDir[length(fBaseDir)]<>'\') then fBaseDir+='\';
end;

function TFileStreamOpener.OpenStream(pFilename: string): TStream;
begin
  if Sysutils.FileExists(fBaseDir+pFilename) then
    Result:=TFileStream.Create(fBaseDir+pFilename,fmOpenRead or fmShareDenyNone)
  else
    Result:=nil;
end;

function TFileStreamOpener.FileExists(pFilename: string): boolean;
begin
  Result:=SysUtils.FileExists(fBaseDir+pFilename);
end;

function TFileStreamOpener.GetFileInfo(pFilename: String): TMKFileInfo;
begin
  if SysUtils.FileExists(fBaseDir+pFilename) then begin
    Result._filename:=fBaseDir+pFilename;
    Result._start:=0;
    Result._size:=SizeOfFile(fBaseDir+pFilename);
    Result._compressed:=(length(pFilename)>3) and (uppercase(copy(pFilename,length(pFilename)-2,3))='.ZL');
  end else begin
    Result._filename:='';
    Result._start:=-1;
    Result._size:=-1;
    Result._compressed:=false;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  ExePath:=ExtractFilePath(paramstr(0));
  MKStreamOpener:=TMKStreamOpener.Create;

finalization
  if MKStreamOpener<>nil then FreeAndNIL(MKStreamOpener);

end.
