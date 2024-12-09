{
  -[Name]-------------------------------------------

     Lists:

       - TFileSearchList
       - TFIFOStringList
       - TIntList

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2005-2018

  --------------------------------------------------

  -[Description]------------------------------------

  --------------------------------------------------
}
// Version info
// V1.00
//   * Initial creation
// V1.10
//   * Item count is unlimited now.
//   + GetItemIndex function added.
// V1.11
//   + Clear method added.
// V1.20
//   - Checking for ($define nolog) added
// V2.00 - Gilby - 2007.02.27
//   * It's now a unit, containing three types of lists.
// V2.10 - Gilby - 2007.04.20
//   + Objectlist added. (Can contain objects, compare callback proc reqd.)
// V3.00 - Gilby - 2007.10.10
//   - TStringList and DistincStringList is removed, use classes in
//     Classes unit instead
//   - TObjectList is removed, use TList or TFPList from Classes unit instead
//   * TFileSearchList is converted to be a class (descendant of TStringList)
//   * TFIFOStringList is converted to be a class (descendant of TStringList)
// V3.01 - Gilby - 2008.04.08
//   - TFileSearchList now can search recursive
// V3.02 - Gilby - 2010.11.02
//   + TIntList added
// V3.03 - Gilby - 2011.07.26
//   + TDoubleValueList added
//     This is a special TStringList, where Values[Name] is enhanced to
//     Values1[Name] and Values2[Name].
//     Default format of a double value line is "Name=Value1;Value2"
// V3.04 - Gilby - 2013.11.25
//   + TCallbackStringList introduced
//   + ForEach method inherited to TFileSearchList, TFIFOStringList
//     and TDoubleValueList
// V3.05 - Gilby - 2014.01.29
//   + A new Create introduced to TFileSearchList for instant filling
// V3.05a - Gilby - 2014.02.05
//   * FIX: The new Create in TFileSearchList hidden the original Create
// V3.06 - Gilby - 2014.02.28
//   * TIntList.Delete added
//   * TIntList.Items is now a read only property and not giving back
//     the whole array
//   * TIntList destructor is now Destroy
// V3.07 - Gilby - 2014.06.24
//   + GenericNamedList added
// V3.08 - Gilby - 2015.01.05
//   + FIX: DoubleValueList
// V3.09 - Gilby - 2015.03.12
//   + TCounterList added
// V3.10 - Gilby - 2016.07.08
//   * TNamedList is usable under FPC
// V3.11 - Gilby - 2017.06.08
//   + Sorted property is added to TIntList
//   * TIntList is not sorted by default
// V3.12 - Gilby - 2018.12.19
//   * TCounterList.Increase got a second parameter, the value to increase the
//     counter. (Default=1)
// V3.13 - Gilby - 2019.05.10
//   + TGenericList added
// V3.14 - Gilby - 2019.07.??
//   * TGenericList.Items and TNamedList.Items are no longer read only.
// V3.15 - Gilby - 2019.08.30
//   * TGenericList.Clear and TNamedList.Clear nils items.
// V3.16 - Gilby - 2020.06.26
//   + TNamedList.ItemByName added
// V3.17 - Gilby - 2020.10.13
//   - TCallBackStringList removed, TStringList already have a ForEach callback.
// V3.18 - Gilby - 2022.10.06
//   - Removed TGenericList, use fgl unit instead.
// V3.19 - Gilby - 2024.03.22
//   - Fix in TDoubleValueList.

{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

unit Lists;

interface

uses Classes;

type
  TFileSearchList=class(TStringList)
//    constructor Create;
    constructor Create(iPath:string;iAttr:integer); overload;
    procedure SearchAndFill(aPath:String;aAttr:integer);
    procedure SearchAndFillRecursive(aPath,aFileName:String;aAttr:integer);
  end;

  TFIFOStringList=class(TStringList)
    function GetNextItem:string;
  end;

  TIntList=class
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(aIndex:integer);
    procedure Add(aValue:integer);
    function IndexOf(aValue:integer):integer;
    procedure Sort;
  private
    fItems:array of integer;
    fSorted:boolean;
    function fGetCount:integer;
    function fGetItem(index:integer):integer;
    procedure fSetSorted(Value:boolean);
    function fSortCompare(i1,i2:integer):byte;
    procedure fSortSwap(i1,i2:integer);
  public
    property Items[index:integer]:integer read fGetItem; default;
    property Count:integer read fGetCount;
    property Sorted:boolean read fSorted write fSetSorted;
  end;
  
  { TDoubleValueList }

  TDoubleValueList=class(TStringList)
  private
    function GetValue1(name:string):string;
    function GetValue2(name:string):string;
    procedure SetValue1(name,value:string);
    procedure SetValue2(name,value:string);
  public
    property Values1[name:string]:string read GetValue1 write SetValue1;
    property Values2[name:string]:string read GetValue2 write SetValue2;
    function Value1FromIndex(index:integer):string;
    function Value2FromIndex(index:integer):string;
  end;

{$ifdef fpc}
  TNamedList<T>=class(TStringList)
  public
    destructor Destroy; override;
    procedure Clear; override;
  private
    function fGetItem(index:integer):T;
    procedure fSetItem(index:integer;item:T);
    function fGetItemS(index:string):T;
    procedure fSetItemS(index:string;item:T);
  public
    property Items[index:integer]:T read fGetItem write fSetItem; default;
    property ItemByName[index:string]:T read fGetItemS write fSetItemS;
  end;
{$endif}

  TCounterList=class(TStringList)
  public
    function Increase(pItemName:string;pValue:integer=1):boolean;  // true:new item, false:already exist
  private
    function fGetItem(index:string):integer;
  public
    property Counters[index:string]:integer read fGetItem; default;
  end;


implementation

uses SysUtils, Logger, QuickSortUnit;

const
  Fstr={$I %FILE%}+', ';
  Version='3.19';

constructor TFileSearchList.Create(iPath:string;iAttr:integer);
begin
  inherited Create;
  SearchAndFill(iPath,iAttr);
end;

procedure TFileSearchList.SearchAndFill(aPath:String;aAttr:longint);
var Info:TSearchRec;
begin
  if FindFirst(aPath,aAttr,Info)=0 then begin
    repeat
      if Info.Attr and aAttr<>0 then
        Add(Info.Name);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
end;

procedure TFileSearchList.SearchAndFillRecursive(aPath,aFileName:String;aAttr:longint);
var Info:TSearchRec;
begin
//  writeln('Scanning directory '''+apath+#39'...');
  if FindFirst(aPath+'\'+aFileName,aAttr or faDirectory,Info)=0 then begin
    repeat
      if (Info.Attr and aAttr<>0) and (Info.Attr and faDirectory=0) then
        Add(aPath+'\'+Info.Name);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
  if FindFirst(aPath+'\*',faDirectory,Info)=0 then begin
    repeat
      if (Info.Name[1]<>'.') and (Info.Attr and faDirectory<>0) then
        SearchAndFillRecursive(aPath+'\'+Info.name,aFileName,aAttr);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
end;

function TFIFOStringList.GetNextItem:string;
begin
  if Count>0 then begin
    Result:=Strings[0];
    Delete(0);
  end else Result:='';
end;

constructor TIntList.Create;
begin
  Clear;
  fSorted:=false;
end;

destructor TIntList.Destroy;
begin
  Clear;
  inherited ;
end;

procedure TIntList.Clear;
begin
  SetLength(fItems,0);
end;
    
procedure TIntList.Add(aValue:integer);
var i,j:integer;
begin
  if length(fItems)=0 then begin
    SetLength(fItems,1);
    fItems[0]:=aValue;
  end else begin
    if fSorted then begin
      i:=0;
      while (i<length(fItems)) and (fItems[i]<aValue) do inc(i);
      Setlength(fItems,length(fItems)+1);
      for j:=length(fItems)-2 downto i do fItems[j+1]:=fItems[j];
      fItems[i]:=aValue;
    end else begin
      Setlength(fItems,length(fItems)+1);
      fItems[length(fItems)-1]:=aValue;
    end;
  end;
end;

procedure TIntList.Delete(aIndex:integer);
var i:integer;
begin
  for i:=aIndex to length(fItems)-2 do
    fItems[i]:=fItems[i+1];
  SetLength(fItems,length(fItems)-1);
end;

function TIntList.IndexOf(aValue:integer):integer;
var i:integer;
begin
  for i:=0 to length(fItems)-1 do
    if fItems[i]=aValue then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

function TIntList.fSortCompare(i1,i2:integer):byte;
begin
  if (fItems[i1]>fItems[i2]) then Result:=1
  else if (fItems[i1]<fItems[i2]) then Result:=2
  else Result:=0;
end;

procedure TIntList.fSortSwap(i1,i2:integer);
var tmp:integer;
begin
  tmp:=fItems[i1];
  fItems[i1]:=fItems[i2];
  fItems[i2]:=tmp;
end;

procedure TIntList.Sort;
begin
  QuickSortUnit.Sort(0,fGetCount-1,fSortCompare,fSortSwap);
end;

function TIntList.fGetCount:integer;
begin
  Result:=length(fItems);
end;

function TIntList.fGetItem(index:integer):integer;
begin
  if (index>=0) and (index<length(fItems)) then
    Result:=fItems[index]
  else
    Result:=-1;
end;

procedure TIntList.fSetSorted(Value:boolean);
begin
  fSorted:=Value;
  if fSorted then Sort;
end;

function TDoubleValueList.GetValue1(name:string):string;
var s:string;
begin
  s:=Values[name];
  if s<>'' then begin
    if pos(';',s)>0 then Result:=copy(s,1,pos(';',s)-1)
                    else Result:=s;
  end else Result:='';
end;

function TDoubleValueList.GetValue2(name:string):string;
var s:string;
begin
  s:=Values[name];
  if s<>'' then begin
    if pos(';',s)>0 then Result:=copy(s,pos(';',s)+1,length(s)-pos(';',s))
                    else Result:='';
  end else Result:='';
end;

procedure TDoubleValueList.SetValue1(name,value:string);
var s:string;
begin
  s:=Values[name];
  if s<>'' then begin
    if pos(';',s)>0 then Values[name]:=value+copy(s,pos(';',s),length(s)-pos(';',s)+1)
                    else Values[name]:=value;
  end else Values[name]:=value;
end;

procedure TDoubleValueList.SetValue2(name,value:string);
var s:string;
begin
  s:=Values[name];
  if s<>'' then begin
    if pos(';',s)>0 then Values[name]:=copy(s,1,pos(';',s))+value
                    else Values[name]:=Values[name]+';'+value;
  end else Values[name]:=';'+value;
end;

function TDoubleValueList.Value1FromIndex(index:integer):string;
var s:string;
begin
  s:=ValueFromIndex[index];
  if s<>'' then begin
    if pos(';',s)>0 then Result:=copy(s,1,pos(';',s)-1)
                    else Result:=s;
  end else Result:='';
end;

function TDoubleValueList.Value2FromIndex(index:integer):string;
var s:string;
begin
  s:=ValueFromIndex[index];
  if s<>'' then begin
    if pos(';',s)>0 then Result:=copy(s,pos(';',s)+1,length(s)-pos(';',s))
                    else Result:='';
  end else Result:='';
end;

{$ifdef fpc}
destructor TNamedList<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TNamedList<T>.Clear;
var i:integer;
begin
  for i:=0 to Self.Count-1 do begin
    Objects[i].Free;
    Objects[i]:=nil;
  end;
  inherited;
end;

function TNamedList<T>.fGetItem(index:integer):T;
begin
  if (index>=0) and (index<Count) then
    Result:=T(Objects[index])
  else
    Result:=nil;
end;

procedure TNamedList<T>.fSetItem(index:integer;item:T);
begin
  if (index>=0) and (index<Count) then
    Objects[index]:=item;
end;

function TNamedList<T>.fGetItemS(index:string):T;
begin
  Result:=fGetItem(IndexOf(index));
end;

procedure TNamedList<T>.fSetItemS(index:string;item:T);
begin
  fSetItem(IndexOf(index),item);
end;

{$endif}

function TCounterList.Increase(pItemName:string;pValue:integer):boolean;  // true:new item, false:already exist
var i:integer;s:String;
begin
  Result:=true;
  s:=Values[pItemName];
  if s='' then begin
    Result:=false;
    i:=0;
  end else
    i:=strtoint(s);
  Values[pItemName]:=inttostr(i+pValue);
end;

function TCounterList.fGetItem(index:string):integer;
//var s:string;
begin
  if Values[index]>'' then
    Result:=strtoint(Values[index])
  else
    Result:=-1;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
