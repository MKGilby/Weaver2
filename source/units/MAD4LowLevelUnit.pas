{ -[Name]-------------------------------------------

           MKSZTSZ Advanced Datafile Handler
                 Low Level operations

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2011-2015

  --------------------------------------------------

  -[Description]------------------------------------

   Lets you mount .MAD4 files and read/write chunks 
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
     [Closing chunk (ENDS)]                   


   There three types of chunks: 
     - Empty (NULL) (this one created when a chunk deleted. Later it can be reused)
     - Closing (ENDS)
     - Not empty (any fourcc except NULL and ENDS)

   If a new chunk has to be created, the algo is the following: 
     AddChunk(type,stream) called: (size is without fourCC and chunksize)
       1. Search a 'NULL' chunk matching size with the new size.
       2. If found: Replace the NULL chunk with the new chunk. Goto 7
       3. Search a NULL chunk bigger or equal than the new size+8.
       4. If found: Write the new chunk over the NULL chunk and create a new
          NULL chunk for the remaining space. Fill the space with 0s. Goto 7
       5. Put the file at the end:
            Write the new chunk over the ENDS chunk and create new ENDS chunk.
       6. END          

  --------------------------------------------------

}
// Version info:
//
//  V1.00: Gilby - 2011.10.19
//    * Initial creation from MAD4_LowLevelUnit
//  V1.01: Gilby - 2012.03.05
//    * Better reusing of NULL chunks
//    + fFileStart:integer added, it holds the real file start of archive (it 
//      can be handy when MAD4 is copied after the game.exe)
//  V1.02: Gilby - 2012.03.29-30
//    - CompressorUnit removed from uses
//    * FourCC changed to integer
//    * Delphi XE2 compatibility fixes
//  V1.03: Gilby - 2012.04.04
//    * Bugfix: Crash in CombineNulls
//  V1.04: Gilby - 2012.05.09
//    * Speedup in WriteNULL
//  V1.05: Gilby - 2012.08.17
//    * Destructor is now Destroy
//  V1.06: Gilby - 2012.08.22
//    * ListChunks now lists chunk type as string instead of uint32
//    * Bugfix: Adding files to the end when there is a NULL chunk before ENDS
//              f*cked up the file structure.
//  V1.07: Gilby - 2013.04.12
//    + Add(fourcc:integer;var p;size:integer) but only when in WriteOnly mode
//  V1.08: Gilby - 2014.01.14
//    * Dec2Hex changed to IntToHex
//  V1.09: Gilby - 2015.05.20
//    * Logging error when the file tried to mount is not a MAD4 file.
//  V1.10: Gilby - 2015.05.29
//    * Logging change, chunks are logged as string not as hex values.
//    + Logs chunk start and size too.
//  V1.10a: Gilby - 2015.12.09
//    * Fixes to suppress hints in Lazarus
//  V1.10b: Gilby - 2021.03.15
//    * More fixes to suppress hints in Lazarus
//  V2.00: Gilby - 2021.08.27
//    * Big rework: Get rid of Mount. One entity for one file.
//    + Compact added. It relocates all files to the beginning to the volume
//      and leaves one big free chunk at the end.
//  V2.01: Gilby - 2021.11.15
//    * Class now based on TStreamOpener
//  V2.02: Gilby - 2021.11.15
//    * Fix when adding a chunk at the end but not entirely write over the
//      existing NULL/ENDS chunk. The ENDS added was not at the correct position,
//      there was a hole in the file, not used by any chunk.


{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit MAD4LowLevelUnit;

interface

uses Classes, MKStream;

type 
  TMAD4ChunkData=class
    _fourcc:cardinal;
    _size:integer;
    _start:integer;
  end;

  TMAD4LowLevel=class(TStreamOpener)
    // Creates a MAD4 volume for low level read/write or read only
    constructor Create(iFilename:AnsiString;iReadOnly:boolean=true); virtual;
    // Creates a MAD4 volume for low level writing only.
    // Doesn't updates ENDS until Destroy.
    constructor CreateWriteOnly(iFilename:AnsiString);
    // The usual destructor.
    destructor Destroy; override;

    function Add(iFourCC:cardinal;iData:TStream;iSkipExactCheck:boolean=false):integer; overload; virtual;
    // Adds a chunk to the volume. Returns it start position in file.

    function Add(iFourCC:cardinal;var p;iSize:integer):integer; overload; virtual;
    // Adds a chunk to the volume. Returns it start position in file.

    procedure Delete(iStart:integer); overload;
    // Changes the chunk to NULL.

    function ReadMem(iStart:integer):TMemoryStream;
    // Gives back a TMemoryStream containing the chunk data

    function ReadFile(iStart:integer;var iSize:integer):TFileStream;
    // Gives back a read-only TFileStream positioned at the start of the 
    // chunk data. Sets iSize to the size of the data. 
    
    function GetFullChunk(iStart:integer):TStream;
    // Gives back a TStream containing the full chunk 

    procedure AddFullChunk(iChunk:TStream;pSkipExactCheck:boolean=false);
    // Add a full chunk to the archive.

    procedure CombineNulls;
    // Search for NULL chunks beside each other and combines them into one
    // bigger NULL chunk.
    
    procedure ListChunks;
    // List chunks into log file for debugging purposes. If log level 'D' is
    // not set then this procedure will do nothing.
    
    procedure Compact;
    // Moving not NULL chunks closer to the beginning of the archive.
    // In other words it combines all NULL chunks into a big one at the
    // end of the file, by moving the data closer to the start of the archive.
    
  protected
    fFileName:AnsiString;
    fStream:TStream;
    fChunkList:TStringList;
    fUpdate:boolean;
    fReadOnly,fWriteOnly:boolean;
    fFileStart:integer;

    function Search(iPosition:integer):integer; overload;
    // Search for chunk at iPosition and give back its index in fChunkList 

  private
    function GetChunk(index:integer):TMAD4ChunkData;
    procedure UpdateENDS;
    procedure WriteNull(iStart,iSize:integer);
    
    function ReadChunkData(iPosition:integer):TMAD4ChunkData;
    // Reads chunk data and positions stream to the next chunk
    
    function GetFirstNotNullChunkAfter(pStart:integer):TMAD4ChunkData;
    function GetFirstNullChunk:TMAD4ChunkData;
    function GetLastChunk:TMAD4ChunkData;
  public
//    property Status:integer read fStatus;
    property FileName:AnsiString read fFileName;
    property IsUpdate:boolean read fUpdate;

  protected  
    property Chunks[index:integer]:TMAD4ChunkData read GetChunk;
  end;

implementation

uses SysUtils, Logger, MKToolBox;

const 
  Fstr='MAD4LowLevelUnit.pas, ';
  Version='2.02';
//  _mad4=$3444414D;
  _ends=$53444E45;
  _null=$4C4C554E;
  
var NullBuffer:pointer;  

constructor TMAD4LowLevel.Create(iFilename:AnsiString;iReadOnly:boolean=true);
var
  s:AnsiString;
  i:integer;
  atm:TMAD4ChunkData;
begin
  fFileName:=iFilename;
  fChunkList:=TStringList.Create;
  fReadOnly:=iReadOnly;
  fWriteOnly:=false;

  if SysUtils.FileExists(string(fFileName)) then begin
    if fReadOnly then fStream:=TFileStream.Create(string(fFileName),fmOpenRead or fmShareDenyNone)
                 else fStream:=TFileStream.Create(string(fFileName),fmOpenReadWrite or fmShareDenyWrite);
    fStream.Seek(-12,soFromEnd);
    s:=#0#0#0#0;
    fStream.Read(s[1],4);
    i:=0;
    fStream.Read(i,4);
    if (s<>'ENDS') or (i<>4) then fStream.Seek(0,soFromBeginning)
    else begin
      fStream.Read(i,4);
      if i>fStream.Size then fStream.Seek(0,soFromBeginning)
                        else fStream.Seek(-i,soFromEnd);
    end;
    fFileStart:=fStream.Position;

    SetLength(s,4);
    fStream.Read(s[1],4);
    if s<>'MAD4' then begin
      FreeAndNil(fStream);
      raise Exception.Create(Format('Not a MAD4 file! (%s)',[fFileName]));
    end else begin
      if s='MADU' then fUpdate:=true;
      fStream.Read(i,4);
      while fStream.Position<=fStream.Size-8 do begin
        atm:=ReadChunkData(fStream.Position);
        fChunkList.AddObject(st(atm._start,9,' '),atm);
      end;
    end;
  end else begin
    fStream:=TFileStream.Create(string(fFileName),fmCreate);
    FreeAndNil(fStream);
    fStream:=TFileStream.Create(string(fFileName),fmOpenReadWrite or fmShareDenyWrite);
    s:='MAD4'#8#0#0#0;
    fStream.Write(s[1],length(s));
    UpdateENDS;
  end;
end;

constructor TMAD4LowLevel.CreateWriteOnly(iFilename:AnsiString);
var s:AnsiString;
begin
  fWriteOnly:=true;
  fReadOnly:=false;
  fFilename:=iFilename;
  fStream:=TFileStream.Create(string(fFileName),fmCreate);
  s:='MAD4'#8#0#0#0;
  fStream.Write(s[1],length(s));
  fChunkList:=TStringList.Create;
end;

destructor TMAD4LowLevel.Destroy;
var i:integer;
begin
  UpdateENDS;
  if not fWriteOnly then begin
    CombineNulls;
    for i:=fChunkList.Count-1 downto 0 do
      TMAD4ChunkData(fChunklist.Objects[i]).Free;
  end;
  FreeAndNil(fStream);
  if Assigned(fChunkList) then FreeAndNil(fChunkList);
  inherited ;
end;

function TMAD4LowLevel.ReadChunkData(iPosition:integer):TMAD4ChunkData;
const Istr=Fstr+'TMAD4LowLevel.ReadChunkData';
var s:string;
begin
  Result:=TMAD4ChunkData.Create;
  fStream.Seek(iPosition,soFromBeginning);
  with Result do begin
    _start:=fStream.Position;
    fStream.Read(_fourcc,4);
    s:=#0#0#0#0;
    move(_fourcc,s[1],4);
    fStream.Read(_size,4);
    Log.LogDebug('Chunk found: '+s+' (start: '+inttostr(_start)+', size: '+inttostr(_size)+')',Istr);
    fStream.Seek(_size,soFromCurrent);
  end;
end;

procedure TMAD4LowLevel.UpdateENDS;
var s:AnsiString;i,j:integer;atm:TMAD4ChunkData;
begin
  fStream.Seek(-12,soFromEnd);
  s:=#0#0#0#0;
  fStream.Read(s[1],4);
  i:=0;
  fStream.Read(i,4);
  if (s<>'ENDS') or (i<>4) then begin
    j:=fStream.Size;
    i:=j+12;
    fStream.Seek(0,soFromEnd);
    s:='ENDS'#4#0#0#0;
    fStream.Write(s[1],8);
    fStream.Write(i,4);
  end else begin
    i:=fStream.Size;
    fStream.Write(i,4);
    j:=i-12;
  end;
  if not fWriteOnly then begin
    if (fChunkList.Count>0) and
       (TMAD4ChunkData(fChunkList.Objects[fChunkList.Count-1])._fourcc=_ends) then begin
      TMAD4ChunkData(fChunkList.Objects[fChunkList.Count-1])._start:=j;
    end else begin
      atm:=TMAD4ChunkData.Create;
      atm._start:=j;
      atm._size:=4;
      atm._fourcc:=_ends;
      fChunkList.AddObject(st(atm._start,9,' '),atm);
    end;
  end;
  i:=fStream.Size-8;
  fStream.Seek(4,soFromBeginning);
  fStream.Write(i,4);
end;

function TMAD4LowLevel.Add(iFourCC:cardinal;iData:TStream;iSkipExactCheck:boolean=false):integer;
const Istr=Fstr+'TMAD4LowLevel.Add';
var i,j,k:integer;atm:TMAD4ChunkData;
begin
  Log.LogDebug(inttohex(iFourCC,8),Istr);
  if (iFourCC=_null) or (iFourCC=_ends) then begin
    Log.LogError('Invalid FourCC! ('+IntToHex(iFourCC,8)+')',Istr);
    Result:=-1;
    exit;
  end;
  iData.Seek(0,soFromBeginning);

  if not fWriteOnly then begin
//   1. Search a 'NULL' chunk matching size with the new size.
    if not iSkipExactCheck then
      for i:=0 to fChunkList.Count-1 do
        with TMAD4ChunkData(fChunkList.Objects[i]) do begin
//   2. If found: Replace the NULL chunk with the new chunk. Goto 7
          if (_fourcc=_null) and (_size=iData.Size) then begin
            fStream.Seek(_start,soFromBeginning);
            fStream.Write(iFourCC,4);
            fStream.Seek(4,soFromCurrent); // Size already good...
            fStream.CopyFrom(iData,iData.Size);
            _fourcc:=iFourCC;
            Result:=_start;
            Log.LogDebug('  Replace whole NULL (Start='+inttostr(_start)+', Size='+inttostr(_size)+')',Istr);
            exit;
          end;
        end;

//   3. Search a NULL chunk bigger or equal than the new size+8.
    for i:=0 to fChunkList.Count-1 do
//   4. If found: Write the new chunk over the NULL chunk and create a new
//      NULL chunk for the remaining space. Fill the space with 0s. Goto 7
      with TMAD4ChunkData(fChunkList.Objects[i]) do begin
        if (_fourcc=_null) and (_size>=iData.Size+8) then begin
          atm:=TMAD4ChunkData.Create;
          atm._fourcc:=iFourCC;
          atm._start:=_start;
          Result:=_start;
          atm._size:=iData.Size;
          Log.LogDebug('  Overwrite partial NULL (Start='+inttostr(atm._start)+', Size='+inttostr(atm._size)+')',Istr);
          fStream.Seek(_start,soFromBeginning);
          fStream.Write(iFourCC,4);
          fStream.Write(atm._size,4);
          fStream.CopyFrom(iData,iData.Size);
          _start:=fStream.Position;
          _size:=_size-(iData.Size+8);
          WriteNull(_start,_size);
          fChunkList[i]:=st(_start,9,' ');
          fChunkList.InsertObject(i,st(atm._start,9,' '),atm);
          exit;
        end;
      end;

//   5. Put the file at the end:
//        Write the new chunk over the closing NULL chunks and
//        ENDS chunk and create new ENDS chunk.
     i:=fChunkList.Count-1;
     
     while (i>=0) and ((Chunks[i]._fourcc=_null) or (Chunks[i]._fourcc=_ends)) do
       dec(i);

     inc(i);
     if (i=0) and (fChunkList.Count=0) then
       j:=8
     else
       j:=Chunks[i]._start;  // There is always one chunk at the end (ENDS)

     for k:=fChunkList.Count-1 downto i do begin
       Chunks[k].Free;
       fChunkList.Delete(k);
     end;
     
    atm:=TMAD4ChunkData.Create;
    atm._fourcc:=iFourCC;
    atm._start:=j;
    Result:=j;
    atm._size:=iData.Size;
    Log.LogDebug('  Put at the end (Start='+inttostr(atm._start)+', Size='+inttostr(atm._size)+')',Istr);
    fStream.Seek(j,soFromBeginning);
    fStream.Write(iFourCC,4);
    fStream.Write(atm._size,4);
    fStream.CopyFrom(iData,iData.Size);
    fChunkList.AddObject(st(atm._start,9,' '),atm);
    fStream.Size:=fStream.Position;  // Cut file just after the stream end.

    Log.LogDebug('Start='+inttostr(atm._start)+', Size='+inttostr(atm._size)+', Streamsize='+inttostr(fStream.Size),Istr);
{    if atm._start+atm._size<fStream.Size then begin  // ENDS was overwritten partially
      i:=fStream.Size-(atm._start+atm._size)-8;
      if i<0 then i:=0;
      atm:=TMAD4ChunkData.Create;
      atm._fourcc:=_null;
      atm._start:=fStream.Position;
      atm._size:=i;
      Log.LogDebug('  Insert NULL (Start='+inttostr(atm._start)+', Size='+inttostr(atm._size)+')',Istr);
      fStream.Write(atm._fourcc,4);
      fStream.Write(atm._size,4);
      fChunkList.AddObject(st(atm._start,9,' '),atm);
    end;}
  
    UpdateENDS;
  end else begin
    fStream.Seek(fStream.Size,soFromBeginning);
    Result:=fStream.Size;
    fStream.Write(iFourCC,4);
    i:=iData.Size;
    fStream.Write(i,4);
    if iData.Size>0 then fStream.CopyFrom(iData,iData.Size);
  end;
end;

function TMAD4LowLevel.Add(iFourCC:cardinal;var p;iSize:integer):integer;
const Istr=Fstr+'TMAD4LowLevel.Add';
var i:integer;
begin
  if (iFourCC=_null) or (iFourCC=_ends) then begin
    Log.LogError('Invalid FourCC! ('+inttohex(iFourCC,8)+')',Istr);
    Result:=-1;
    exit;
  end;
  if not fWriteOnly then begin
    Log.LogWarning('Add untyped data currently only supported with WriteOnly archives!',Istr);
  end else begin
    fStream.Seek(fStream.Size,soFromBeginning);
    Result:=fStream.Size;
    fStream.Write(iFourCC,4);
    i:=iSize;
    fStream.Write(i,4);
    if iSize>0 then fStream.Write(p,iSize);
  end;
end;

procedure TMAD4LowLevel.Delete(iStart:integer);
var i:integer;
begin
  if fWriteOnly then exit;
  for i:=0 to fChunkList.Count-1 do
    with TMAD4ChunkData(fChunkList.Objects[i]) do
      if _start=iStart then begin
        _fourcc:=_null;
        WriteNull(_start,_size);
        exit;
      end;
end;

function TMAD4LowLevel.ReadMem(iStart:integer):TMemoryStream;
var i:integer;
begin
  if not fWriteOnly then
    for i:=0 to fChunkList.Count-1 do
      with TMAD4ChunkData(fChunkList.Objects[i]) do
        if _start=iStart then begin
          Result:=TMemoryStream.Create;
          fStream.Seek(_start+8,soFromBeginning);
          Result.CopyFrom(fStream,_size);
          Result.Seek(0,soFromBeginning);
          exit;
        end;
  Result:=nil;
end;

function TMAD4LowLevel.ReadFile(iStart:integer;var iSize:integer):TFileStream;
var i:integer;
begin
  if not fWriteOnly then
    for i:=0 to fChunkList.Count-1 do
      with TMAD4ChunkData(fChunkList.Objects[i]) do
        if _start=iStart then begin
          Result:=TFileStream.Create(string(fFileName),fmOpenRead or fmShareDenyNone);
          Result.Seek(_start+8,soFromBeginning);
          iSize:=_size;
          exit;
        end;
  Result:=nil;
end;

function TMAD4LowLevel.GetFullChunk(iStart:integer):TStream;
const Istr=Fstr+'TMAD4LowLevel.GetFullChunk';
var i:integer;
begin
  if not fWriteOnly then
    for i:=0 to fChunkList.Count-1 do
      with TMAD4ChunkData(fChunkList.Objects[i]) do
        if _start=iStart then begin
          Log.LogDebug('Full chunk data:',Istr);
          Log.LogDebug('  Start: '+inttostr(_start),Istr);
          Log.LogDebug('  Size: '+inttostr(_size),Istr);
          fStream.Seek(_start,soFromBeginning);
          Result:=TMemoryStream.Create;
          Result.CopyFrom(fStream,int64(_size)+8);
          exit;
        end;
  Result:=nil;
end;

procedure TMAD4LowLevel.WriteNull(iStart,iSize:integer);
const Istr=Fstr+'TMAD4LowLevel.WriteNull';
var s:AnsiString;i:integer;
begin
  Log.LogDebug('Start: '+inttostr(iStart)+', Size: '+inttostr(iSize),Istr);
  fStream.Seek(iStart,soFromBeginning);
  s:='NULL';
  fStream.Write(s[1],4);
  fStream.Write(iSize,4);
  for i:=0 to (iSize div 4096)-1 do fStream.Write(NullBuffer^,4096);
  if iSize mod 4096>0 then fStream.Write(NullBuffer^,iSize mod 4096);
end;

procedure TMAD4LowLevel.AddFullChunk(iChunk:TStream;pSkipExactCheck:boolean=false);
//const Istr=Fstr+'TMAD4LowLevel.AddFullChunk';
var fcc:cardinal;Xs:TStream;
begin
  if fWriteOnly then exit;
  iChunk.Seek(0,soFromBeginning);
  fcc:=0;
  iChunk.Read(fcc,4);
  iChunk.Seek(8,soFromBeginning);
  Xs:=TMemoryStream.Create;
  Xs.CopyFrom(iChunk,iChunk.Size-8);
  Add(fcc,Xs,pSkipExactCheck);
  FreeAndNil(Xs);
end;

procedure TMAD4LowLevel.CombineNulls;
const Istr=Fstr+'TMAD4LowLevel.CombineNulls';
var i:integer;s:AnsiString;
begin
  if fWriteOnly then exit;
  Log.LogDebug('Before combining NULLs',Istr);
//  fChunkList.Sort;
//  ListChunks;
  for i:=fChunkList.Count-2 downto 0 do begin
//    Log.Trace(i);
    if (Chunks[i]._fourcc=_null) and
       (Chunks[i+1]._fourcc=_null) then begin
         s:=#0#0#0#0#0#0#0#0;
         Chunks[i]._size:=Chunks[i]._size+Chunks[i+1]._size+8;
         fStream.Seek(Chunks[i]._start+4,soFromBeginning);
         fStream.Write(Chunks[i]._size,4);
         fStream.Seek(Chunks[i+1]._start,soFromBeginning);
         fStream.Write(s[1],8);
         Chunks[i+1].Free;
         fChunkList.Delete(i+1);
       end;
  end;      
  Log.LogDebug('After combining NULLs',Istr);
end;

procedure TMAD4LowLevel.ListChunks;
const Istr=Fstr+'TMAD4_LowLevel.ListChunks';
var i:integer;s:string;
begin
  if fWriteOnly then exit;
  Log.LogDebug('Chunk listing starts ('+string(fFileName)+'):',Istr);
  s:=#0#0#0#0;
  for i:=0 to fChunkList.Count-1 do
    with TMAD4ChunkData(fChunkList.Objects[i]) do begin
      move(_fourcc,s[1],4);
      Log.LogDebug(inttostr(i+1)+'. '+s+' Start: '+inttostr(_start)+
        ', Size: '+inttostr(_size),Istr);
    end;
end;

function TMAD4LowLevel.Search(iPosition:integer):integer;
var i:integer;
begin
  for i:=0 to fChunkList.Count-1 do  
    with TMAD4ChunkData(fChunkList.Objects[i]) do
      if _start=iPosition then begin Result:=i;exit;end;
  Result:=-1;    
end;

function TMAD4LowLevel.GetChunk(index:integer):TMAD4ChunkData;
begin
  if (index>=0) and (index<fChunkList.Count) then 
    Result:=TMAD4ChunkData(fChunkList.Objects[index])
  else
    Result:=nil;
end;

function TMAD4LowLevel.GetFirstNullChunk:TMAD4ChunkData;
var i,min:integer;atm:TMAD4ChunkData;
begin
  min:=MaxLongint;
  Result:=nil;
  for i:=0 to fChunkList.Count-1 do begin
    atm:=GetChunk(i);
    if (atm._fourcc=_null) and (atm._start<min) then begin
      min:=atm._start;
      Result:=atm;
    end;
  end;
  if Assigned(Result) then
    Log.Trace(Format('First NULL chunk is at %d (size: %d)',[Result._start,Result._size]));
end;

function TMAD4LowLevel.GetFirstNotNullChunkAfter(pStart:integer):TMAD4ChunkData;
var i,min:integer;atm:TMAD4ChunkData;
begin
  min:=MaxLongint;
  Result:=nil;
  for i:=0 to fChunkList.Count-1 do begin
    atm:=GetChunk(i);
    if (atm._fourcc<>_null) and (atm._fourcc<>_ends) and (atm._start>pStart) and (atm._start-pStart<min) then begin
      min:=atm._start-pStart;
      Result:=atm;
    end;
  end;
  if Assigned(Result) then
    Log.Trace(Format('First not NULL chunk after %d is at %d (size: %d)',[pStart,Result._start,Result._size]));
end;

function TMAD4LowLevel.GetLastChunk:TMAD4ChunkData;
var i,max:integer;atm:TMAD4ChunkData;
begin
  max:=0;
  Result:=nil;
  for i:=0 to fChunkList.Count-1 do begin
    atm:=GetChunk(i);
    if (atm._fourcc<>_ends) and (atm._start>max) then begin
      max:=atm._start;
      Result:=atm;
    end;
  end;
  if Assigned(Result) then
    Log.Trace(Format('Last not ENDS chunk is at %d (size: %d)',[Result._start,Result._size]));
end;

procedure TMAD4LowLevel.Compact;
var atm:TMAD4ChunkData;tmpS:TStream;i:integer;
begin
  atm:=GetFirstNullChunk;
  if assigned(atm) then begin
    i:=atm._start;
    atm:=GetFirstNotNullChunkAfter(i);
    while Assigned(atm) do begin
      tmpS:=GetFullChunk(atm._start);
      Delete(atm._start);
      CombineNulls;
      AddFullChunk(tmpS,true);
      FreeAndNil(tmpS);
      atm:=GetFirstNullChunk;
      i:=atm._start;
      atm:=GetFirstNotNullChunkAfter(i);
    end;
  end;
  atm:=GetLastChunk;
  if atm._fourcc=_null then begin
    fStream.Size:=atm._start;
    UpdateENDS;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  getmem(NullBuffer,4096);
  fillchar(NullBuffer^,4096,0);
  
finalization
  freemem(NullBuffer,4096);  

end.
