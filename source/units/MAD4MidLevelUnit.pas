{ -[Name]-------------------------------------------

           MKSZTSZ Advanced Datafile Handler
                Middle Level operations

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2011-2014

  --------------------------------------------------

  -[Description]------------------------------------

   Lets you mount .MAD4 files and read/write files 
   into them.
   
   MAD4 file format:
   
     Name     Size  Desc.
     FourCC     4   'MAD4' (or 'MADU' to indicate that this is an update pack)
     Size       4   Full file size - 8 
     [Chunk1]
     [Chunk2]
     .
     .
     [ChunkN]


   There are 6 types of chunks:
     - Compressed (COMP)
     - Not compressed (RAWD)
     - Link to another chunk (LINK)
     - Empty (NULL) (used by TMAD4LowLevel)
     - Version (FVER)
     - Closing (ENDS) (used by TMAD4LowLevel)

   Compressed chunk format:
     FNLen      1   Length of Filename
     Filename FNLEN Filename
     [Compressed data, use CompressorUnit to uncompress)

   Uncompressed chunk format:
     FNLen      1   Length of Filename
     Filename FNLEN Filename
     [Uncompressed data]

   Version segment format:
     VSLen      1   Length of Version string
     Version  VSLEN Version string
     BDLen      1   Length of Build string
     Build    BDLEN Build string
     
   Link segment format:
     FNLen      1   Length of Filename
     Filename FNLEN Filename
     LCStart    4   Linked chunk start
     
   Trying to add a NULL or an ENDS chunk will result an error.

   Deleting issues:
   
     LINK links to COMP
     
     Deleting LINK: removes LINK segment.  (it the easier)
     Deleting COMP: Should remove all segments AND create a new COMP with the 
       first LINK name, and modify the remaining links to point new COMP.

  --------------------------------------------------

}
// Version info:
//
//  V1.00: Gilby - 2011.10.19
//    * Initial creation from MAD4_LowLevelUnit
//  V1.01: Gilby - 2012.03.05
//    * File deduplication option added
//      Set Deduplication to turn it on/off (Default on).
//      Files already in the archive won't be deduplicated.
//      Deduplication will only work on files added with deduplication turned on.
//      (but they will be checked against the whole archive)
//  V1.02: Gilby - 2012.03.06
//    * GetFileList added (gives back a TStringList, with filename;chunk;size data)
//  V1.03: Gilby - 2012.03.29-30
//    * Added ifdefs to make a compressionless version under Delphi XE2
//    * More modifications for Delphi XE2 compatibility
//    * FourCC type changed to UInt32
//  V1.04: Gilby - 2012.04.04
//    * Bugfix in deduplication. 
//  V1.05: Gilby - 2012.05.09
//    * GetFullChunk(filename) added
//  V1.06: Gilby - 2012.08.17
//    * Destructor is now Destroy
//  V1.07: Gilby - 2012.08.22
//    - Deduplication disabled, something wrong with it
//  V1.08: Gilby - 2012.08.29
//    * Using MD5 unit provided with FreePascal instead of the one found on net.
//  V1.09: Gilby - 2014.01.14
//    * Dec2Hex changed to IntToHex
//  V1.10: Gilby - 2015.05.20
//    * Don't try to read file list if TMAD4LowLevel fails to initialize.
//  V1.10a: Gilby - 2015.12.09
//    * Fixes to suppress hints in Lazarus
//  V1.11: Gilby - 2019.12.04
//    * Removing deprecated methods
//  V1.11a: Gilby - 2019.12.04
//    * Changes to suppress hints in Lazarus
//  V2.00: Gilby - 2021.08.27
//    * Big rework: Get rid of mount. One entity for one file.
//  V2.01: Gilby - 2021.11.15
//    * Changes to comply TStreamOpener


{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MAD4MidLevelUnit;

interface

uses Classes, MAD4LowLevelUnit, MKStream;

type
  TMAD4FileData=class(TObject)
    _filename:AnsiString;
    _start:integer;  // File start, not the chunk!
    _size:integer;   // File size, not the chunk!
    _chunkstart:integer;
    _linkto:integer;
  end;

  TMAD4MidLevel=class(TMAD4LowLevel)
    constructor Create(iFilename:AnsiString;iReadOnly:boolean=true); override;
    destructor Destroy; override;

    function Add(iFileName:AnsiString;var p;iSize:integer;const iCompMethod:integer=-1):integer; overload;
    function Add(iFileName:AnsiString;iXs:TStream;const iCompMethod:integer=-1):integer; overload;
    function Add(iFilename,SourceFileName:AnsiString;const iCompMethod:integer=-1):integer; overload;
    // Adds a file to the volume.

    function OpenStream(iFileName:AnsiString):TStream; override;
    // Reads whole specified file into a TMemoryStream.

    function Delete(iFilename:AnsiString;iInnerCall:boolean=false):boolean; overload;
    // Deletes iFilename is exists. Returns true if successful, false if not.

    function FileExists(iFileName:AnsiString):boolean; override;
    // Returns TRUE if iFileName exists in the mounted volume

    function SaveFileAs(iFileName,iTargetFileName:AnsiString):boolean;
    // Extracts a file from the volume to disk. Returns true if successful.

    function GetFullChunk(iFilename:AnsiString):TStream;
    // Gets a full chunk

    function GetFileList:TStringList;
    // Gives back the list of contained files with chunk and size info

    function GetShortFileList:TStringList;
    // Gives back the list of contained files (filename only)
{
    procedure SetVersion(Version,Build:AnsiString);
    // Set version data
}
//    procedure GetFileInfo(var iFilename:AnsiString;var iStart,iSize:integer;var iCompressed:boolean); overload;
    function GetFileInfo(pFilename:String):TMKFileInfo; override;
    procedure ListFiles;
  private
    fFileList:TStringList;
    fCompressionLevel:integer;
    fLastCompressionMethod:integer;
    fLastCompression:Ansistring;
    fVersion, fBuild:Ansistring;

    function Search(iFilename:Ansistring):integer; overload;
    function SearchChunkStart(iValue:integer):integer;
    function GetFile(index:integer):TMAD4FileData;
    property Files[index:integer]:TMAD4FileData read GetFile;
  public
    property LastCompressionMethod:integer read fLastCompressionMethod;
    property LastCompression:AnsiString read fLastCompression;
    property Version:AnsiString read fVersion;
    property Build:AnsiString read fBuild;
    {$ifdef fpc}
    property CompressionLevel:integer read fCompressionLevel write fCompressionLevel;
    {$endif}
  end;

implementation

uses SysUtils, Logger, {$ifdef fpc}CompressorUnit, {$endif}MKToolBox;

const
  Fstr='MAD4MidLevelUnit.pas, ';
  Version='2.01';
  _comp=$504D4F43;
  _rawd=$44574152;
  _fver=$52455646;
  _link=$4B4E494C;

constructor TMAD4MidLevel.Create(iFilename:AnsiString;iReadOnly:boolean=true);
var
  i,j:integer;
  b:byte;
  atm:TMAD4FileData;
  Xs:TFileStream;
begin
  inherited Create(iFilename,iReadOnly);
  fCompressionLevel:=1;
  fFileList:=TStringList.Create;
  b:=0;j:=0;
  for i:=0 to fChunkList.Count-1 do
    with TMAD4ChunkData(fChunkList.Objects[i]) do begin
      if (_fourcc=_comp) or (_fourcc=_rawd) then begin
        atm:=TMAD4FileData.Create;
        atm._chunkstart:=_start;
        Xs:=ReadFile(_start,j);
        Xs.Read(b,1);
        SetLength(atm._filename,b);
        Xs.Read(atm._filename[1],b);
        atm._start:=Xs.Position;
        atm._size:=j-b-1;
        atm._linkto:=0;
        FreeAndNil(Xs);
        fFileList.AddObject(string(atm._filename),atm);
      end else if (_fourcc=_fver) then begin
        atm:=TMAD4FileData.Create;
        atm._chunkstart:=_start;
        atm._filename:='*V';
        Xs:=ReadFile(_start,j);
        Xs.Read(b,1);
        SetLength(fVersion,b);
        Xs.Read(fVersion[1],b);
        Xs.Read(b,1);
        SetLength(fBuild,b);
        Xs.Read(fBuild[1],b);
        atm._start:=_start;
        atm._size:=length(fVersion)+length(fBuild)+2;
        atm._linkto:=0;
        FreeAndNil(Xs);
        fFileList.AddObject(string(atm._filename),atm);
      end else if (_fourcc=_link) then begin
        atm:=TMAD4FileData.Create;
        atm._chunkstart:=_start;
        Xs:=ReadFile(_start,j);
        Xs.Read(b,1);
        SetLength(atm._filename,b);
        Xs.Read(atm._filename[1],b);
        atm._start:=Xs.Position;
        atm._size:=j-b-1;
        Xs.Read(atm._linkto,4);
        atm._linkto:=atm._linkto+fFileStart;
        FreeAndNil(Xs);
        fFileList.AddObject(string(atm._filename),atm);
      end;
    end;
end;

destructor TMAD4MIDLevel.Destroy;
var i:integer;
begin
  if Assigned(fFileList) then begin
    for i:=0 to fFileList.Count-1 do
      TMAD4FileData(fFileList.Objects[i]).Free;
    FreeAndNil(fFileList);
  end;
  inherited ;
end;

function TMAD4MidLevel.Search(iFilename:AnsiString):integer;
var i:integer;
begin
//  iFilename:=uppercase(iFilename);
//  *** ITT megáll ha kész a pálya!
  for i:=0 to fFileList.Count-1 do with Files[i] do
    if uppercase(string(_filename))=uppercase(string(iFilename)) then begin
      Result:=i;
      exit;
    end;
  Result:=-1; 
end;

function TMAD4MidLevel.SearchChunkStart(iValue:integer):integer;
var i:integer;
begin
  for i:=0 to fFileList.Count-1 do with Files[i] do
    if _chunkstart=iValue then begin
      Result:=i;
      exit;
    end;
  Result:=-1; 
end;

function TMAD4MidLevel.OpenStream(iFileName:AnsiString):TStream;
//const Istr=Fstr+'TMAD4_LowLevel.OpenStream';
var i,j:integer;
{$ifdef fpc}Xs:TStream;{$endif}
begin
//  Result:=TMemoryStream.Create;
  i:=Search(iFileName);
  if i>-1 then begin
//    writeln(i,', ',Files[i]._linkto);
    if Files[i]._linkto<>0 then i:=SearchChunkStart(Files[i]._linkto);
//    writeln(i);
    j:=Search(Files[i]._chunkstart);
    if j>-1 then begin
      fStream.Seek(Files[i]._start,soFromBeginning);
      if Chunks[j]._fourcc=_comp then begin
{$ifdef fpc}
        Xs:=TMemoryStream.Create;
        Xs.CopyFrom(fStream,Files[i]._size);
        Result:=Compressor.UnCompress(Xs);
        FreeAndNil(Xs);
        Result.Seek(0,soFromBeginning);
{$else}
        Result:=TMemoryStream.Create;
{$endif}
      end
      else if Chunks[j]._fourcc=_rawd then begin
        Result:=TMemoryStream.Create;
        Result.CopyFrom(fStream,Files[i]._size);
        Result.Seek(0,soFromBeginning);
      end else Result:=nil;
    end else Result:=nil;
  end else Result:=nil;
end;

function TMAD4MIDLevel.Add(iFileName:ansistring;var p;iSize:integer;const iCompMethod:integer=-1):integer;
var Xs:TMemoryStream;
begin
  Xs:=TMemoryStream.Create;
  Xs.Write(p,iSize);
  Xs.Seek(0,soFromBeginning);
  Result:=Add(iFileName,Xs,iCompMethod);
  FreeAndNil(Xs);
end;

function TMAD4MIDLevel.Add(iFilename,SourceFileName:ansiString;const iCompMethod:integer=-1):integer;
const Istr=Fstr+'TMAD4MIDLevel.Add';
var Xs:TStream;
begin
  if FileExists(string(SourceFileName)) then begin
    Xs:=TFileStream.Create(string(SourceFileName),fmOpenRead);
    Result:=Add(iFileName,Xs,iCompMethod);
    FreeAndNil(Xs);
  end else begin
    Result:=-1;
    Log.LogWarning('File not found: '+string(SourceFileName),Istr);
  end;
end;

function TMAD4MidLevel.Add(iFileName:ansistring;iXs:TStream;const iCompMethod:integer=-1):integer;
const Istr=Fstr+'TMAD4MidLevel.Add';
var Ys:TStream;
    i,o:integer;fcc:cardinal;
    atm:TMAD4FileData;
//    s:AnsiString;
{$ifdef fpc}Xs:TStream;{key:AnsiString;}{$endif}

begin
  Log.LogDebug('ADD '+string(iFileName)+' (Size='+inttostr(iXs.Size)+')',Istr);
  Delete(iFileName,true);
  o:=iXs.Position;

//     1. Compress file
{$ifdef fpc}
//  writeln('M4M 1, ',iXs.Size,', ',iXs.Position);
  Xs:=Compressor.Compress(iXs,fCompressionLevel,iCompMethod);
  fLastCompressionMethod:=Compressor.LastCompressionMethod;
  fLastCompression:=Compressor.LastCompression;

//  writeln('M4M 2, ',fLastCompression);
  if fLastCompressionMethod=0 then begin
    Ys:=TMemoryStream.Create;
    Xs.Seek(5,soFromBeginning);
    i:=length(iFilename);
    Ys.Write(i,1);
    Ys.Write(iFilename[1],i);
    Ys.CopyFrom(Xs,Xs.Size-5);
    i:=Xs.Size-5;
    FreeAndNil(Xs);
    fcc:=_rawd;
  end else begin
    Ys:=TMemoryStream.Create;
    Xs.Seek(0,soFromBeginning);
    i:=length(iFilename);
    Ys.Write(i,1);
    Ys.Write(iFilename[1],i);
    Ys.CopyFrom(Xs,Xs.Size);
    i:=Xs.Size;
    FreeAndNil(Xs);
    fcc:=_comp;
  end;
{$else}
  {$ifdef ver230}
  Ys:=TMemoryStream.Create;
  iXs.Seek(0,soFromBeginning);
  s:=iFilename;
  i:=length(s);
  Ys.Write(i,1);
  Ys.Write(s[1],i);
  Ys.CopyFrom(iXs,iXs.Size);
  i:=iXs.Size;
  fcc:=_rawd;
  {$endif}
{$endif}
//  writeln('M4M 3');

  iXs.Seek(o,soFromBeginning);
  Result:=round(i*10000/iXs.Size);

  Ys.Seek(0,soFromBeginning);
  atm:=TMAD4FileData.Create;
  atm._filename:=iFilename;
  atm._chunkstart:=inherited Add(fcc,Ys);
//  writeln('M4M 4');
{$ifdef fpc}
//  fDedupeList.Add(key+'='+inttostr(atm._chunkstart));
{$endif}
  atm._start:=atm._chunkstart+8+length(iFilename)+1;
  atm._size:=Ys.Size;
  fFileList.AddObject(string(iFilename),atm);
  FreeAndNil(Ys);
//  writeln('M4M 5');
end;

function TMAD4MidLevel.Delete(iFilename:AnsiString;iInnerCall:boolean=false):boolean;
const Istr=Fstr+'TMAD4MidLevel.Delete';
var fi,ci{,j},cnt{,csize,orgchunkstart}:integer;{Xs,Ys:TStream;fcc:cardinal;}
{$ifdef fpc} {i:integer;} {$endif}
begin
  if iInnerCall then
    Log.LogDebug('DELETE (inner) "'+string(iFilename)+'"',Istr)
  else
    Log.LogDebug('DELETE "'+string(iFilename)+'"',Istr);
  fi:=Search(iFilename);
  if fi=-1 then begin
    Log.LogDebug('  Not found!',Istr);
    Result:=false;
    exit;
  end;

  ci:=Search(Files[fi]._chunkstart);
  if ci=-1 then begin
    Result:=false;
    exit;
  end;

// If the deleted file is a link, simply delete it.
  Result:=true;
  if Chunks[ci]._fourcc=_link then begin
    Delete(Files[fi]._chunkstart);
    TMAD4FileData(fFileList.Objects[fi]).Free;
    fFileList.Delete(fi);
    exit;
  end;

  cnt:=1;

  if cnt=1 then begin  // No link was found to the file
    Delete(Files[fi]._chunkstart);
    Files[fi].Free;
    fFileList.Delete(fi);
  end;
end;

function TMAD4MidLevel.GetFile(index:integer):TMAD4FileData;
begin
  if (index>=0) and (index<fFileList.Count) then 
    Result:=TMAD4FileData(fFileList.Objects[index])
  else
    Result:=nil;
end;

function TMAD4MidLevel.FileExists(iFileName:AnsiString):boolean;
begin
  Result:= (Search(iFileName)>-1);
end;

function TMAD4MidLevel.SaveFileAs(iFileName,iTargetFileName:AnsiString):boolean;
var Xs:TStream;
begin
  if Search(iFileName)>-1 then begin
    Xs:=OpenStream(iFileName);
    TMemoryStream(Xs).SaveToFile(string(iTargetFileName));
    Result:=true;
  end else Result:=false;
end;

function TMAD4MidLevel.GetFileList:TStringList;
var i,j:integer;
begin
  Result:=TStringList.Create;
  Result.Add('Name;Chunk;Size');
  for i:=0 to fFileList.Count-1 do with Files[i] do begin
    j:=Search(_chunkstart);
    if j>-1 then begin
      Result.Add(string(_filename)+';'+inttohex(Chunks[j]._fourcc,8)+';'+inttostr(_size));
    end;
  end;
end;

function TMAD4MidLevel.GetShortFileList:TStringList;
var i,j:integer;
begin
  Result:=TStringList.Create;
  for i:=0 to fFileList.Count-1 do with Files[i] do begin
    j:=Search(_chunkstart);
    if j>-1 then
      Result.Add(string(_filename));
  end;
end;

procedure TMAD4MidLevel.ListFiles;
const Istr=Fstr+'TMAD4MidLevel.ListFiles';
var i,j:integer;s:string;
begin
  if fWriteOnly then exit;
  Log.LogDebug('File listing starts ('+string(fFileName)+'):',Istr);
  s:=#0#0#0#0;
  for i:=0 to fFileList.Count-1 do with Files[i] do begin
    j:=Search(_chunkstart);
    if j>-1 then begin
      move(Chunks[j]._fourcc,s[1],4);

      Log.Trace(inttostr(i+1)+'. '+_filename+' Start: '+inttostr(_start)+
        ' Size: '+inttostr(_size)+' Chunkstart: '+inttostr(_chunkstart)+
        ' Chunk type: '+s+' Start: '+inttostr(Chunks[j]._start)+
        ' Size: '+inttostr(Chunks[j]._size));
    end;
  end;
end;

function TMAD4MidLevel.GetFullChunk(iFilename:AnsiString):TStream;
var i,j:integer;
begin
  i:=Search(iFileName);
  if i>-1 then with Files[i] do begin
    j:=Search(_chunkstart);
    if j>-1 then begin
      Result:=TMemoryStream.Create;
      fStream.Seek(Chunks[j]._start,soFromBeginning);
      Result.CopyFrom(fStream,int64(Chunks[j]._size)+8);
      exit;
    end;
  end;
  Result:=nil;
end;

{procedure TMAD4MidLevel.GetFileInfo(var iFilename:AnsiString;var iStart,iSize:integer;var iCompressed:boolean);
var i,j:integer;
begin
  i:=Search(iFilename);
  if i>-1 then with Files[i] do begin
    iFilename:=fFileName;
    istart:=_start;
    isize:=_size;
//    Log.Trace(_fourcc);
    j:=Search(_chunkstart);
    if j>-1 then begin
      icompressed:=(Chunks[j]._fourcc=_comp);
      exit;
    end;  
  end;
  iStart:=-1;
end;}

function TMAD4MidLevel.GetFileInfo(pFilename:String):TMKFileInfo;
var i,j:integer;
begin
  i:=Search(pFilename);
  if i>-1 then with Files[i] do begin
    Result._filename:=fFileName;
    Result._start:=_start;
    Result._size:=_size;
//    Log.Trace(_fourcc);
    j:=Search(_chunkstart);
    if j>-1 then begin
      Result._compressed:=(Chunks[j]._fourcc=_comp);
      exit;
    end;
  end;
  Result._Start:=-1;
end;

initialization
   Log.LogStatus(Fstr+'version '+Version,'uses');

end.
