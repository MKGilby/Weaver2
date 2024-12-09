{$ifdef fpc}
  {$smartlink on}
  {$mode delphi}
{$endif}

// Version date info:
// * before V1.13 is not available
// V1.13 - 2005.10.26 - Gilby
// V1.14 - 2005.11.19 - Gilby
// V1.15 - 2006.02.15 - Gilby
// V1.16 - 2006.05.15 - Gilby
// V1.17 - 2006.09.25 - Gilby
// V1.18 - 2007.06.13 - Gilby
// V2.00 - 2008.03.14 - Gilby - Using Classes instead of objects
//                                (use mkwincrt_o for objects)
// V2.01 - 2008.03.28 - Gilby
// V2.02 - 2008.04.28 - Gilby
// V3.00 - 2008.08.18 - Gilby - Renamed to MK_Toolbox, left out some deprecated thing
// V3.01 - 2008.08.22 - Gilby
// V3.02 - 2009.01.05 - Gilby
// V3.03 - 2009.03.09 - Gilby
// V3.04 - 2009.04.09 - Gilby
// V3.05 - 2009.07.10 - Gilby
//   * Replace rewritten to work with (path,'/','//') too...
// V3.06 - 2009.07.22 - Gilby
//   * Replace fix
//   * Function results changed to Result instead of function name
// V3.07 - 2009.08.27 - Gilby
//   + function CountBitsInByte(byte):integer added
//   + function AddExtension(string,string):string added
// V3.08 - 2009.09.09 - Gilby
//   + function IsRGB(string):boolean added
// V3.09 - 2009.11.23 - Gilby
//   + function DecimalSeparate(i:integer;separator:char=' '):string
// V3.10 - 2009.11.25 - Gilby
//   * BUGFIX: IsNumeric now detects negative numbers correctly
// V3.11 - 2010.02.05 - Gilby
//   + procedure ReadStreamStringUNIX(source:TStream;var s:String); added
// V3.12 - 2010.05.27 - Gilby
//   + function Dec2Bin(d,len:integer):string; added
//   + function Bin2Dec(s:string):integer; added
// V3.13 - 2010.06.16 - Gilby
//   * Modified FirstCapital to accept a second parameter (boolean)
//     if it is false it converts all words first letter to capital (default)
//     if it is true it convert only the first letter of the string to capital
// V3.14 - 2010.10.18 - Gilby
//   + function CreateRect(x1,y1,x2,y2:integer):TRect added.
//     The difference between this and windows.rect function that this one
//     orders the points, so you will always get a topleft and bottomright point.
// V3.15 - 2010.10.20 - Gilby
//   * CopyFile function is changed. When you specify 0 bytes for length (or
//     leave it out), the whole file is copied.
// V3.16 - 2010.10.21 - Gilby
//   + ToUTF8 function added.
// V3.17 - 2010.10.29 - Gilby
//   + function GetNthSegment(s:String;sep:char;n:integer):string added
// V3.18 - 2011.02.04 - Gilby
//   + function Dec2Oct(d,len:integer):string added
// V3.19 - 2011.03.04 - Gilby
//   + function UnTab(s:string;tabsize:integer):string added
// V3.20 - 2011.05.31 - Gilby
//   * IsNumeric function has an extra parameter (DecSep = decimalseparator)
//     Decimal separator default is #0, which means that
//     SysUtils.DecimalSeparator will be used.
// V3.21 - 2011.06.08 - Gilby
//   * GetNthSegment bugfix.
//   + FromUTF8(s:String):string added
// V3.22 - 2011.07.25 - Gilby
//   + UnBase64(input:String):string added.
//   + DecodeUTF(s:String):string added
//   + DecodeUTF7(input:String):string added
// V3.23 - 2012.03.10 - Gilby
//   * Addextension only add extension when no extension present
// V3.24 - 2012.03.27 - Gilby
//   * Delphi XE2 compatibility fixes
// V3.25 - 2012.04.03 - Gilby
//   * DOSChars fix...
// V3.26 - 2012.07.30-31 - Gilby
//   * ReadStreamString and ReadStreamStringUnix speedup... 
// V3.27 - 2013.03.27 - Gilby
//   * Oct2Dec(s:String):integer added
// V3.28 - 2013.10.14 - Gilby
//   * Oct2Dec Delphi XE2 optimization
//   * Added compiler directives for unused variables is Delphi mode
// V3.29 - 2014.01.14 - Gilby
//   - Removed Dec2Hex as it is implemented in SysUtils
// V3.30 - 2014.04.22 - Gilby
//   + ReplaceNthSegment added
// V3.31 - 2015.02.08 - Gilby
//   + Modifications to suppress warnings in Lazarus
// V3.32 - 2015.05.21 - Gilby
//   * Copyfile modified to use streams (to handle read-only files)
// V3.33 - 2015.10.09 - Gilby
//   + Max added
//   + Min added
// V3.33a - 2015.12.09 - Gilby
//   * Fixes to suppress hints in Lazarus
// V3.34 - 2016.01.18 - Gilby
//   + Added global FS:TFormatSettings value populated with Hungarian settings.
// V3.35 - 2016.02.26 - Gilby
//   - Dec2Bin is removed, use SysUtils.BinStr instead
//   - Dec2Oct is removed, use SysUtils.OctStr instead
//   * Hex2Dec is renamed to HexToInt
//   * Oct2Dec is renamed to OctToInt
//   * Bin2Dec is renamed to BinToInt
//   * OctToInt, HexToInt and BinToInt throw an EConvertError exception when
//     encounter an invalid char.
// V3.35a- 2016.05.13 - Gilby
//   * Added typecasts to remove hints in Lazarus
// V3.36 - 2016.07.13 - Gilby
//   - CreateStreamFromString is removed, Classes.TStringStream does the same
// V3.37 - 2017.07.13 - Gilby
//   * You can specify brackets in Trimbrackets now (default = '()')
// V3.38 - 2018.03.22 - Gilby
//   + IsValidUTF8(s:string):boolean is added.
// V3.39 - 2018.10.28 - Gilby
//   * Decode is accepts a third parameter (pIsCaseSensitive, default true).
// V3.40 - 2019.01.16 - Gilby
//   + Added ƒ to char conversion
// V3.41 - 2019.02.05 - Gilby
//   + Added proper codepage-852 -> UTF-8 conversion:
//     Cp852ToUTF8(s:string):string;
//   + Added proper Windows-1250 -> UTF-8 conversion:
//     Win1250ToUTF8(s:string):string;
//   * ToUTF8 is deprecated
// V3.41a - 2019.02.11 - Gilby
//   * Changed += to proper := + in new functions
// V3.42 - 2019.05.16 - Gilby
//   * Set the decimal separator to '.' in FS
// V3.42a - 2020.06.25 - Gilby
//   * Removed a debug writeln.
// V3.43 - 2020.09.09 - Gilby
//   + GetPassword added.
// V3.44 - 2021.01.20
//   + CreateTimeStamp added.
// V3.45 - 2021.08.17
//   - Removed UnBase64. Use base64 unit and DecodeStringBase64 instead.
//   - Removed DecodeUTF and DecodeUTF7. It was used in some Delphi stuff,
//     moved them there.
// V3.46 - 2021.08.26
//   - Removed AddExtension, use SysUtils.ChangeFileExt instead.
//   - Removed RenameFile, use SysUtils.MoveFile instead.
// V3.47 - 2021.12.18
//   - cp852_unicode.inc and win1250_unicode.inc files are embedded into pas file
//     to make my life easier when adding units to projects.
// V3.48 - 2023.09.01-04
//   - Removed RGB16, RGB15, c16i, c15i, c16c and c16to15.
//   - Removed RecCount.
//   + Added HSLtoRGB and RGBtoHSL
// V3.48a - 2023.09.05
//   - Fix in HSLtoRGB
// V3.49 - 2024.04.23
//   + Added ExchangeSegments
//   * Replace is deprecated, use StringReplace in SysUtils instead.

unit MKToolbox;

interface

uses Windows, SysUtils, Classes;

// ---------------------------
// String and Number functions
// ---------------------------

{V1.00}  function St(l:longint;b:Byte;c:char):string; overload;
         function Std(d:double;b:Byte;c:char):string;
{V1.05}  function AddStr(s:String;b:Byte):string;
         function AddStrx(s:String;b:Byte):string;
{V1.07}  function Spc(db:integer):string;
         function Replicate(c:char;db:integer):string;
{V1.08}  function HexToInt(s:string):cardinal;
{V1.09}  function Alltrim(s:String):string;
         function Rtrim(s:String):string;
         function Ltrim(s:String):string;
{V1.10}  function Win2Dos(s:String):string;
         function Dos2Win(s:String):string;
{V1.12}  function Replace(s,src,trg:string):string; deprecated; // Use StringReplace in SysUtils instead!
{V1.15}  function CountChar(c:char;s:String):byte;
{V1.18}  function TrimBrackets(var s:string;brackets:string):boolean;
{V2.01}  function IsNumeric(s:string;DecSep:char=#0):boolean;
         function LPad(c:char;s:String;num:integer):String;
{V2.02}  function Decode(input,decodestring:String;pIsCaseSensitive:boolean=true):String;
{V3.01}  function RPos(what,where:string;nth:integer):integer;
{V3.02}  function RPad(c:char;s:String;num:integer):String;
{V3.03}  function FirstCapital(s:string;onlyfirst:boolean=false):String;
{V3.04}  function EvalStringToBoolean(s:string):boolean;
{V3.07}  function CountBitsInByte(b:byte):integer;
{V3.09}  function DecimalSeparate(i:integer;separator:char=' '):string;
{V3.12}  function BinToInt(s:string):integer;
{V3.16}  function ToUTF8(s:String):string; deprecated;
{V3.17}  function GetNthSegment(s:String;sep:char;n:integer):string;
{V3.19}  function UnTab(s:string;tabsize:integer):string;
{V3.21}  function FromUTF8(s:String):string;
{V3.27}  function OctToInt(s:string):integer;
{V3.30}  function ReplaceNthSegment(s:String;sep:char;n:integer;value:string):string;
{V3.33}  function Max(i1,i2:integer):integer;
         function Min(i1,i2:integer):integer;
{V3.38}  function IsValidUTF8(s:string):boolean;
{V3.41}  function Cp852ToUTF8(s:string):string;
         function Win1250ToUTF8(s:string):string;
{V3.43}  function GetPassword(const InputMask: Char = '*'): string;
{V3.49}  function ExchangeSegments(s:String;sep:char;n1,n2:integer):string;

// -------------------------------------
// File related functions and procedures
// -------------------------------------

{V3.30}  procedure CopyFile(srcfn,trgfn:string;start,size:integer); overload;
         procedure CopyFile(srcfn,trgfn:string;size:integer=0); overload;
{V1.08}  procedure ReadStreamString(source:TStream;var s:String);
{V3.11}  procedure ReadStreamStringUNIX(source:TStream;var s:String);
{V1.11}  function SizeOfFile(fname:string):longint;  // You don't need to open it

// --------------------------
// Color conversion functions
// --------------------------

{V1.04}  function RGB32(r,g,b:cardinal):cardinal;
         function C32i(col,intensity:cardinal):cardinal;
{V1.06}  function HSV2RGB(h,s,v:byte):longint; overload;
         procedure HSV2RGB(h,s,v:integer;var r,g,b:integer); overload;
{V1.16}  procedure ExtractHTMLRGB(s:String;var r,g,b:integer);
{V3.08}  function IsRGB(s:string):boolean;
         // These functions use the following value ranges:
         //   h:0..359   s:0..100   l:0..100
{V3.48}  procedure HSLtoRGB(h:word;s,l:integer;out r,g,b:byte); overload;
         function HSLtoRGB(h:word;s,l:integer):uint32; overload;
         procedure RGBtoHSL(r,g,b:byte;out h:word;out s,l:integer);

// -----------------------
// Miscalleanous functions
// -----------------------

{$ifdef fpc}
{V1.05}  function ByteCount(var p;size:integer):byte;  // How many type of bytes...
{$endif}
{V1.17}  {function CreateStreamFromString(s:string):TStream;}
{V3.14}  function CreateRect(x1,y1,x2,y2:integer):TRect;

// -------------------
// Date-time functions
// -------------------

{V1.14}  function Year(dt:TDateTime):integer;
{V3.44}  function CreateTimeStamp:string;

var FS:TFormatSettings;

implementation

uses Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='3.49';

  HexChars='0123456789ABCDEF';
  DOSChars=#$a0#$b5#$82#$90#$a1#$d6#$a2#$e0#$94#$99#$8b#$8a#$a3#$e9#$81#$9a#$fb#$eb#$8e;
  WINChars='·¡È…ÌÕÛ”ˆ÷ı’˙⁄¸‹˚€'#$c4#$e4;
  UTF8s=  // in order of '·¡È…ÌÕ....'
  //·       ¡       È       …       Ì       Õ       Û       ”       ˆ
    #$c3#$a1#$c3#$81#$c3#$a9#$c3#$89#$c3#$ad#$c3#$8d#$c3#$b3#$c3#$93#$c3#$b6+
  //÷       ı       ’       ˙       ⁄       ¸       ‹       ˚       €
    #$c3#$96#$c5#$91#$c5#$90#$c3#$ba#$c3#$9a#$c3#$bc#$c3#$9c#$c5#$b1#$c5#$b0+
  //ƒ       ‰
    #$c3#$84#$c3#$a4;
//  UTFChars= // in order of '·¡È…ÌÕ....'
//    #$00#$e1#$00#$c1#$00#$e9#$00#$c9#$00#$ed#$00#$cd#$00#$f3#$00#$d3#$00#$f6+
//    #$00#$d6#$01#$51#$01#$50#$00#$fa#$00#$da#$00#$fc#$00#$dc#$01#$71#$01#$70+
//    #$00#$22#$00#$3c#$00#$3d#$00#$3e;
//  UTFTargetChars= WINChars+'"<=>';

const
  {%H-}Cp852_Unicode:array[128..255] of integer=
    ($00c7,$00fc,$00e9,$00e2,$00e4,$016f,$0107,$00e7,
     $0142,$00eb,$0150,$0151,$00ee,$0179,$00c4,$0106,
     $00c9,$0139,$013a,$00f4,$00f6,$013d,$013e,$015a,
     $015b,$00d6,$00dc,$0164,$0165,$0141,$00d7,$010d,
     $00e1,$00ed,$00f3,$00fa,$0104,$0105,$017d,$017e,
     $0118,$0119,$00ac,$017a,$010c,$015f,$00ab,$00bb,
     $2591,$2592,$2593,$2502,$2524,$00c1,$00c2,$011a,
     $015e,$2563,$2551,$2557,$255d,$017b,$017c,$2510,
     $2514,$2534,$252c,$251c,$2500,$253c,$0102,$0103,
     $255a,$2554,$2569,$2566,$2560,$2550,$256c,$00a4,
     $0111,$0110,$010e,$00cb,$010f,$0147,$00cd,$00ce,
     $011b,$2518,$250c,$2588,$2584,$0162,$016e,$2580,
     $00d3,$00df,$00d4,$0143,$0144,$0148,$0160,$0161,
     $0154,$00da,$0155,$0170,$00fd,$00dd,$0163,$00b4,
     $00ad,$02dd,$02db,$02c7,$02d8,$00a7,$00f7,$00b8,
     $00b0,$00a8,$02d9,$0171,$0158,$0159,$25a0,$00a0);

  Cp852_UTF8:array [128..255] of AnsiString=(
    #$C3#$87,#$C3#$BC,#$C3#$A9,#$C3#$A2,#$C3#$A4,#$C5#$AF,#$C4#$87,#$C3#$A7,
    #$C5#$82,#$C3#$AB,#$C5#$90,#$C5#$91,#$C3#$AE,#$C5#$B9,#$C3#$84,#$C4#$86,
    #$C3#$89,#$C4#$B9,#$C4#$BA,#$C3#$B4,#$C3#$B6,#$C4#$BD,#$C4#$BE,#$C5#$9A,
    #$C5#$9B,#$C3#$96,#$C3#$9C,#$C5#$A4,#$C5#$A5,#$C5#$81,#$C3#$97,#$C4#$8D,
    #$C3#$A1,#$C3#$AD,#$C3#$B3,#$C3#$BA,#$C4#$84,#$C4#$85,#$C5#$BD,#$C5#$BE,
    #$C4#$98,#$C4#$99,#$C2#$AC,#$C5#$BA,#$C4#$8C,#$C5#$9F,#$C2#$AB,#$C2#$BB,
    #$E2#$96#$91,#$E2#$96#$92,#$E2#$96#$93,#$E2#$94#$82,#$E2#$94#$A4,#$C3#$81,
    #$C3#$82,#$C4#$9A,#$C5#$9E,#$E2#$95#$A3,#$E2#$95#$91,#$E2#$95#$97,#$E2#$95#$9D,
    #$C5#$BB,#$C5#$BC,#$E2#$94#$90,#$E2#$94#$94,#$E2#$94#$B4,#$E2#$94#$AC,
    #$E2#$94#$9C,#$E2#$94#$80,#$E2#$94#$BC,#$C4#$82,#$C4#$83,#$E2#$95#$9A,
    #$E2#$95#$94,#$E2#$95#$A9,#$E2#$95#$A6,#$E2#$95#$A0,#$E2#$95#$90,#$E2#$95#$AC,
    #$C2#$A4,#$C4#$91,#$C4#$90,#$C4#$8E,#$C3#$8B,#$C4#$8F,#$C5#$87,#$C3#$8D,
    #$C3#$8E,#$C4#$9B,#$E2#$94#$98,#$E2#$94#$8C,#$E2#$96#$88,#$E2#$96#$84,
    #$C5#$A2,#$C5#$AE,#$E2#$96#$80,#$C3#$93,#$C3#$9F,#$C3#$94,#$C5#$83,#$C5#$84,
    #$C5#$88,#$C5#$A0,#$C5#$A1,#$C5#$94,#$C3#$9A,#$C5#$95,#$C5#$B0,#$C3#$BD,
    #$C3#$9D,#$C5#$A3,#$C2#$B4,#$C2#$AD,#$CB#$9D,#$CB#$9B,#$CB#$87,#$CB#$98,
    #$C2#$A7,#$C3#$B7,#$C2#$B8,#$C2#$B0,#$C2#$A8,#$CB#$99,#$C5#$B1,#$C5#$98,
    #$C5#$99,#$E2#$96#$A0,#$C2#$A0);

  {%H-}Win1250_Unicode:array[128..255] of integer=
    ($20ac,$0000,$201a,$0000,$201e,$2026,$2020,$2021,
     $0000,$2030,$0160,$2039,$015a,$0164,$017d,$0179,
     $0000,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
     $0000,$2122,$0161,$203a,$015b,$0165,$017e,$017a,
     $00a0,$02c7,$02db,$0141,$00a4,$0104,$00a6,$00a7,
     $00a8,$00a9,$015e,$00ab,$00ac,$00ad,$00ae,$017b,
     $00b0,$00b1,$02db,$0142,$00b4,$00b5,$00b6,$00b7,
     $00b8,$0105,$015f,$00bb,$013d,$02dd,$013e,$017c,
     $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
     $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
     $0110,$0143,$0147,$00d3,$00d4,$0150,$00d6,$00d7,
     $0158,$016e,$00da,$0170,$00dc,$00dd,$0162,$00df,
     $0155,$00e1,$00e2,$0103,$00e4,$013a,$0107,$00e7,
     $010d,$00e9,$0119,$00eb,$011b,$00ed,$00ee,$010f,
     $0111,$0144,$0148,$00f3,$00f4,$0151,$00f6,$00f7,
     $0159,$016f,$00fa,$0171,$00fc,$00fd,$0163,$02d9);

  Win1250_UTF8:array [128..255] of AnsiString=(
    #$E2#$82#$AC,#$00,#$E2#$80#$9A,#$00,#$E2#$80#$9E,#$E2#$80#$A6,#$E2#$80#$A0,
    #$E2#$80#$A1,#$00,#$E2#$80#$B0,#$C5#$A0,#$E2#$80#$B9,#$C5#$9A,#$C5#$A4,
    #$C5#$BD,#$C5#$B9,#$00,#$E2#$80#$98,#$E2#$80#$99,#$E2#$80#$9C,#$E2#$80#$9D,
    #$E2#$80#$A2,#$E2#$80#$93,#$E2#$80#$94,#$00,#$E2#$84#$A2,#$C5#$A1,#$E2#$80#$BA,
    #$C5#$9B,#$C5#$A5,#$C5#$BE,#$C5#$BA,#$C2#$A0,#$CB#$87,#$CB#$9B,#$C5#$81,
    #$C2#$A4,#$C4#$84,#$C2#$A6,#$C2#$A7,#$C2#$A8,#$C2#$A9,#$C5#$9E,#$C2#$AB,
    #$C2#$AC,#$C2#$AD,#$C2#$AE,#$C5#$BB,#$C2#$B0,#$C2#$B1,#$CB#$9B,#$C5#$82,
    #$C2#$B4,#$C2#$B5,#$C2#$B6,#$C2#$B7,#$C2#$B8,#$C4#$85,#$C5#$9F,#$C2#$BB,
    #$C4#$BD,#$CB#$9D,#$C4#$BE,#$C5#$BC,#$C5#$94,#$C3#$81,#$C3#$82,#$C4#$82,
    #$C3#$84,#$C4#$B9,#$C4#$86,#$C3#$87,#$C4#$8C,#$C3#$89,#$C4#$98,#$C3#$8B,
    #$C4#$9A,#$C3#$8D,#$C3#$8E,#$C4#$8E,#$C4#$90,#$C5#$83,#$C5#$87,#$C3#$93,
    #$C3#$94,#$C5#$90,#$C3#$96,#$C3#$97,#$C5#$98,#$C5#$AE,#$C3#$9A,#$C5#$B0,
    #$C3#$9C,#$C3#$9D,#$C5#$A2,#$C3#$9F,#$C5#$95,#$C3#$A1,#$C3#$A2,#$C4#$83,
    #$C3#$A4,#$C4#$BA,#$C4#$87,#$C3#$A7,#$C4#$8D,#$C3#$A9,#$C4#$99,#$C3#$AB,
    #$C4#$9B,#$C3#$AD,#$C3#$AE,#$C4#$8F,#$C4#$91,#$C5#$84,#$C5#$88,#$C3#$B3,
    #$C3#$B4,#$C5#$91,#$C3#$B6,#$C3#$B7,#$C5#$99,#$C5#$AF,#$C3#$BA,#$C5#$B1,
    #$C3#$BC,#$C3#$BD,#$C5#$A3,#$CB#$99);

{$ifndef VER230}
var
  Buf:array[0..512] of byte;
  BufLen:integer;
{$endif}

function St(l:longint;b:Byte;c:char):string;
begin
  Result:=inttostr(l);
  while length(Result)<b do Result:=c+Result;
end;

function Std(d:double;b:Byte;c:char):string;
begin
  Result:=floattostr(d);
  while length(Result)<b do Result:=c+Result;
end;

procedure CopyFile(srcfn,trgfn:string;start,size:integer);
var src,trg:TStream;
begin
  src:=TFileStream.Create(srcfn,fmOpenRead or fmShareDenyNone);
  trg:=TFileStream.Create(trgfn,fmCreate);
  if start<src.Size then begin
    if int64(start)+int64(size)>src.Size then size:=src.Size-start;
    src.Seek(start,soFromBeginning);
    trg.CopyFrom(src,size);
  end;
  FreeAndNIL(trg);
  FreeAndNIL(src);
end;

procedure CopyFile(srcfn,trgfn:string;size:integer=0);
begin
  CopyFile(srcfn,trgfn,0,size);
end;

function RGB32(r,g,b:cardinal):cardinal;
begin
  Result:=(r and $ff) shl 16+(g and $ff) shl 8+b and $ff;
end;

function C32i(col,intensity:cardinal):cardinal;
var r,g,b:word;
begin
  r:=(col and $ff0000) shr 16;
  g:=(col and $ff00) shr 8;
  b:=col and $ff;
  r:=(r*intensity) shr 8;
  g:=(g*intensity) shr 8;
  b:=(b*intensity) shr 8;
  Result:=rgb32(r,g,b);
end;

function AddStr(s:String;b:Byte):string;
var i:integer;
begin
  for i:=1 to length(s) do s[i]:=chr(ord(s[i])+b);
  Result:=s;
end;

function AddStrx(s:String;b:Byte):string;
var i:integer;
begin
  for i:=1 to length(s) do
    if s[i]<>' ' then s[i]:=chr(ord(s[i])+b);
  Result:=s;
end;

{$ifdef fpc}
function ByteCount(var p;size:integer):byte;
var bytes:pointer;//bytes:array[0..255] of byte;
    i:integer;pp:pointer;
begin
  getmem(bytes,256);
  fillchar(bytes^,256,0);
  pp:=@p;
  for i:=0 to size-1 do
    byte((bytes+byte((pp+i)^))^):=1;
  size:=0;
  for i:=0 to 255 do
    if byte((bytes+i)^)=1 then inc(size);
  Result:=size;
  freemem(bytes,256);
end;
{$endif}

function HSV2RGB(h,s,v:Byte):longint;
var r,g,b:byte;
begin
  r:=0;g:=0;b:=0;
  case h of
     0..15:begin   { Red -> Yellow }
             r:=255;g:=h*16;b:=0;
           end;
    16..31:begin   { Yellow -> Green }
             r:=(31-h)*16;g:=255;b:=0;
           end;
    32..47:begin   { Green -> Cyan }
             r:=0;g:=255;b:=h*16;
           end;
    48..63:begin   { Cyan -> Blue }
             r:=0;g:=(63-h)*16;b:=255;
           end;
    64..79:begin   { Blue -> Magenta }
             r:=h*16;g:=0;b:=255;
           end;
    80..95:begin   { Magenta -> Red }
             r:=255;g:=0;b:=(95-h)*16;
           end;
        96:begin
             r:=255;g:=0;b:=0;
           end;
  end;
  r:=r+(128-r)*s div 32;
  g:=g+(128-g)*s div 32;
  b:=b+(128-b)*s div 32;
  if v<32 then begin
    r:=r*v div 32;
    g:=g*v div 32;
    b:=b*v div 32;
  end else begin
    v:=v-32;
    r:=r+(255-r)*v div 32;
    g:=g+(255-g)*v div 32;
    b:=b+(255-b)*v div 32;
  end;
  Result:=r shl 16+g shl 8+b;
end;

function Spc(db:integer):string;
begin
  Result:='';
  while db>0 do begin
    Result:=Result+' ';
    dec(db);
  end;
end;

function Replicate(c:char;db:integer):string;
var i:integer;
begin
  Result:='';
  for i:=1 to db do Result:=Result+c;
end;

{$ifdef VER230}
procedure ReadStreamString(source:TStream;var s:String);
var c:char;
begin
  s:='';
  repeat
    Source.Read(c,1);
    if c=#13 then break;
    s:=s+c;
  until Source.Position=Source.Size;
  Source.Read(c,1);
end;
{$else}
procedure ReadStreamString(source:TStream;var s:String);  
// Non XE2 compatible, but fast!
var i:integer;
begin
  s:='';
  buflen:=source.Read(buf,512);
  while buflen>0 do begin
    for i:=0 to buflen-1 do
      if buf[i]=10 then begin
        source.Seek(int64(i)-int64(buflen)+1,soCurrent);
        if i>0 then begin 
          Setlength(s,length(s)+i);
          move(buf[0],s[length(s)-i+1],i);
          if s[length(s)]=#13 then delete(s,length(s),1);
          exit;
        end;  
      end;
    Setlength(s,length(s)+buflen);
    move(buf[0],s[length(s)-buflen+1],buflen);
    buflen:=source.Read(buf,512);
  end;
end;
{$endif}

//{$ifdef VER230}
procedure ReadStreamStringUNIX(source:TStream;var s:String);
var c:char;
begin
  s:='';
  c:=#0;
  repeat
    Source.Read(c,1);
    if c=#10 then break;
    s:=s+c;
  until Source.Position=Source.Size;
end;
{else
procedure ReadStreamStringUNIX(source:TStream;var s:String);  
// Non XE2 compatible, but fast!
var i:integer;
begin
  Log.Trace('RSSU IN: '+inttostr(source.Position));
  s:='';
  buflen:=source.Read(buf,256);
  while buflen>0 do begin
    for i:=0 to buflen-1 do
      if buf[i]=10 then begin
        Log.Trace('RSSU HIT POS: '+inttostr(i));
        source.Seek(int64(i)-int64(buflen)+1,soCurrent);
        if i>0 then begin
          Setlength(s,length(s)+i);
          move(buf[0],s[length(s)-i+1],i);
          Log.Trace('RSSU OUT: '+inttostr(source.Position));
          exit;
        end;
      end;
    Setlength(s,length(s)+buflen);
    move(buf[0],s[length(s)-buflen+1],buflen);
    buflen:=source.Read(buf,256);
  end;
  Log.Trace('RSSU OUT: '+inttostr(source.Position));
end;
$endif}

function HexToInt(s:string):cardinal;
var v,d:cardinal;i:byte;
begin
  s:=uppercase(s);
  d:=1;
  v:=0;
  for i:=length(s) downto 1 do
    if s[i] in ['0'..'9','A'..'F'] then begin
      v:=v+d*word(pos(s[i],HexChars)-1);
      d:=d shl 4;
    end else
      raise EConvertError.Create('Not a hexadecimal expression! ('''+s+''')');
  Result:=v;
end;

function Alltrim(s:String):string;
begin
  Result:=RTrim(LTrim(s));
end;

function Rtrim(s:String):string;
begin
  while (length(s)>0) and (s[length(s)]=' ') do delete(s,length(s),1);
  Result:=s;
end;

function Ltrim(s:String):string;
begin
  while (length(s)>0) and (s[1]=' ') do delete(s,1,1);
  Result:=s;
end;

function Win2Dos(s:String):string;
var i:integer;
begin
  for i:=1 to length(s) do
    if pos(s[i],WINChars)<>0 then s[i]:=DOSChars[pos(s[i],WINChars)];
  Result:=s;
end;

function Dos2Win(s:String):string;
var i:integer;
begin
  for i:=1 to length(s) do
    if pos(s[i],DOSChars)<>0 then s[i]:=WINChars[pos(s[i],DOSChars)];
  Result:=s;
end;

function SizeOfFile(fname:string):longint;
var f:file;
begin
  assign(f,fname);
  {$i-} reset(f,1); {$i+}
  if ioresult=0 then begin
    Result:=filesize(f);
    close(f);
  end else Result:=-1;
end;

procedure HSV2RGB(h,s,v:integer;var r,g,b:integer);
var rm,gm,bm,gr:integer;
begin
  rm:=0;gm:=0;bm:=0;
  case h of
    0..42:begin
            rm:=255;
            gm:=h*255 div 43;
            bm:=0;
          end;
    43..85:begin
             rm:=(85-h)*255 div 43;
             gm:=255;
             bm:=0;
           end;
    86..128:begin
              rm:=0;
              gm:=255;
              bm:=(h-86)*255 div 43;
            end;
    129..170:begin
               rm:=0;
               gm:=(170-h)*255 div 42;
               bm:=255;
             end;
    171..213:begin
               rm:=(h-171)*255 div 43;
               gm:=0;
               bm:=255;
             end;
    214..255:begin
               rm:=255;
               gm:=0;
               bm:=(255-h)*255 div 42;
             end;
  end;
  gr:=(gm*30+rm*59+bm*11) div 100;
  r:=(gr+(rm-gr)*s div 255)*v div 255;
  g:=(gr+(gm-gr)*s div 255)*v div 255;
  b:=(gr+(bm-gr)*s div 255)*v div 255;
end;

function Replace(s,src,trg:string):string;
var i:integer;
begin
  Result:=s;
  repeat
    s:=Result;
    Result:='';
    i:=1;
    while i<=length(s) do begin
//      write(i);
      if (i<=length(s)-length(src)+1) and (copy(s,i,length(src))=src) then begin
        Result:=Result+trg;
        i:=i+length(src);
      end else begin
        Result:=Result+s[i];
        inc(i);
      end;
    end;
    if i<length(s) then Result:=Result+copy(s,i+1,length(s)-i);
//    writeln('"',s,'","',result,'"');
  until result=s;
end;

function Year(dt:TDateTime):integer;
var y,m,d:word;
begin
  DecodeDate(dt,y,m,d);
  Result:=y;
end;

function CountChar(c:char;s:String):byte;
var i:integer;
begin
  Result:=0;
  for i:=1 to length(s) do
    if s[i]=c then inc(Result);
end;

procedure ExtractHTMLRGB(s:String;var r,g,b:integer);
begin
  s:=lpad('0',uppercase(alltrim(s)),6);
  r:=HexToInt(copy(s,1,2));
  g:=HexToInt(copy(s,3,2));
  b:=HexToInt(copy(s,5,2));
end;

function TrimBrackets(var s:string;brackets:string):boolean;
var i,lvl:integer;
begin
  Result:=false;
  if length(brackets)<>2 then exit;
  if (s[1]=brackets[1]) and (s[length(s)]=brackets[2]) then begin
    lvl:=0;
    for i:=1 to length(s) do begin
      if s[i]=brackets[1] then inc(lvl);
      if s[i]=brackets[2] then dec(lvl);
      if (lvl=0) and (i<>length(s)) then exit;
    end;
    if (lvl=0) then begin
      s:=copy(s,2,length(s)-2);
      Result:=true;
    end;
  end;
end;

{$ifdef VER230}
function IsNumeric(s:string;decsep:char):boolean;
var i:integer;
begin
  if decsep=#0 then decsep:=FormatSettings.DecimalSeparator;
  if (length(s)>0) and (s[1]='-') then delete(s,1,1);
  if length(s)>0 then begin
    for i:=1 to length(s) do
      if not CharInSet(s[i],['0'..'9',decsep]) then begin
        Result:=false;
        exit;
      end;
    Result:=true;
  end else
    Result:=false;
end;
{$else}
function IsNumeric(s:string;decsep:char):boolean;
var i:integer;
begin
  if decsep=#0 then decsep:=FormatSettings.DecimalSeparator;
  if (length(s)>0) and (s[1]='-') then delete(s,1,1);
  if length(s)>0 then begin
    for i:=1 to length(s) do
      if not (s[i] in ['0'..'9',decsep]) then begin
        Result:=false;
        exit;
      end;
    Result:=true;
  end else
    Result:=false;
end;
{$endif}

function LPad(c:char;s:String;num:integer):String;
begin
  while length(s)<num do s:=c+s;
  Result:=s;
end;

// Like oracle decode...
function Decode(input,decodestring:String;pIsCaseSensitive:boolean=true):String;
var s,s2:string;
begin
  if not pIsCaseSensitive then begin
    while (length(decodestring)>0) and (pos(',',decodestring)>0) do begin
      s:=copy(decodestring,1,pos(',',decodestring)-1);
      delete(decodestring,1,length(s)+1);
      if pos(',',decodestring)>0 then begin
        s2:=copy(decodestring,1,pos(',',decodestring)-1);
        delete(decodestring,1,length(s2)+1);
      end else s2:=decodestring;
      if input=s then begin
        Result:=s2;
        exit;
      end;
    end;
  end else begin
    input:=uppercase(input);
    while (length(decodestring)>0) and (pos(',',decodestring)>0) do begin
      s:=UpperCase(copy(decodestring,1,pos(',',decodestring)-1));
      delete(decodestring,1,length(s)+1);
      if pos(',',decodestring)>0 then begin
        s2:=copy(decodestring,1,pos(',',decodestring)-1);
        delete(decodestring,1,length(s2)+1);
      end else s2:=decodestring;
      if input=s then begin
        Result:=s2;
        exit;
      end;
    end;
  end;
  Result:=decodestring;
end;

function RPos(what,where:string;nth:integer):integer;
var i:integer;
begin
  i:=length(where)-length(what)+1;
  while i>0 do begin
    if copy(where,i,length(what))=what then begin
      dec(nth);
      if nth=0 then begin
        Result:=i;
        exit;
      end;
    end;
    dec(i);
  end;
  Result:=0;
end;

function RPad(c:char;s:String;num:integer):String;
begin
  while length(s)<num do s:=s+c;
  Result:=s;
end;

function FirstCapital(s:string;onlyfirst:boolean):String;
const Low='a·bcdeÈfghiÌjklmnoÛˆıpqrstu˙¸˚vwxyz';
const Upp='A¡BCDE…FGHIÕJKLMNO”÷’PQRSTU⁄‹€VWXYZ';
var pre:byte;i:integer;
begin
  for i:=1 to length(s) do
    if pos(s[i],Low)>0 then s[i]:=Upp[pos(s[i],Low)];
  pre:=0;
  for i:=1 to length(s) do begin
    if pos(s[i],Upp)>0 then begin
      if pre<>0 then s[i]:=Low[pos(s[i],Upp)];
      pre:=1;
    end else 
      if not onlyfirst then pre:=0;
  end;
  Result:=s;
end;

function EvalStringToBoolean(s:string):boolean;
begin
  s:=uppercase(s);
  Result:=(s='TRUE') or (s='1') or (s='Y') or (s='I') or (s='YES');
end;

function CountBitsInByte(b:byte):integer;
var i:integer;
begin
  Result:=0;
  for i:=7 downto 0 do begin
    if b and 1=1 then inc(Result);
    b:=b shr 1;
  end;
end;

function IsRGB(s:string):boolean;
var i:integer;
begin
  Result:=true;
  if length(s)<>6 then Result:=false
  else for i:=1 to length(s) do if pos(s[i],HexChars)=0 then Result:=false;
end;

// Method taken from rapidtables.com/convert/color/hsl-to-rgb.html
procedure HSLtoRGB(h:word; s,l:integer; out r,g,b:byte);
var c,x,m,ss,ll:double;

  function RealMod(x,y:double):double;
  begin
    Result:=x-y*trunc(x/y);
  end;

begin
  if h>360 then h:=h mod 360;
  if s>100 then s:=100;
  if l>100 then l:=100;
  ss:=s/100;
  ll:=l/100;
  c:=(1-abs(2*ll-1))*ss;
  x:=c*(1-abs(RealMod(h/60,2)-1));
  m:=ll-c/2;
  if (h<60) then begin r:=trunc((c+m)*255);g:=trunc((x+m)*255);b:=trunc(m*255);end
  else if (h<120) then begin r:=trunc((x+m)*255);g:=trunc((c+m)*255);b:=trunc(m*255);end
  else if (h<180) then begin r:=trunc(m*255);g:=trunc((c+m)*255);b:=trunc((x+m)*255);end
  else if (h<240) then begin r:=trunc(m*255);g:=trunc((x+m)*255);b:=trunc((c+m)*255);end
  else if (h<300) then begin r:=trunc((x+m)*255);g:=trunc(m*255);b:=trunc((c+m)*255);end
  else begin r:=trunc((c+m)*255);g:=trunc(m*255);b:=trunc((x+m)*255);end;
end;

function HSLtoRGB(h:word; s,l:integer):uint32;
var r,g,b:byte;
begin
  HSLtoRGB(h,s,l,r,g,b);
  Result:=$FF000000+r<<16+g<<8+b;
end;

// Method taken from rapidtables.com/convert/color/rgb-to-hsl.html
procedure RGBtoHSL(r,g,b:byte; out h:word; out s,l:integer);
var rr,gg,bb,d,cmin,cmax,ll:double;
begin
  rr:=r/255;
  gg:=g/255;
  bb:=b/255;
  if (rr<gg) then begin
    if (rr<bb) then cmin:=rr else cmin:=bb;
    if (gg>bb) then cmax:=gg else cmax:=bb;
  end else begin
    if (gg<bb) then cmin:=gg else cmin:=bb;
    if (rr>bb) then cmax:=rr else cmax:=bb;
  end;
  d:=cmax-cmin;
  ll:=(cmax+cmin)/2;
  if d=0 then begin h:=0;s:=0;end
  else begin
    if cmax=rr then h:=trunc(60*(gg-bb)/d+360) mod 360
    else if cmax=gg then h:=trunc(60*((bb-rr)/d+2))
    else if cmax=bb then h:=trunc(60*((rr-gg)/d+4));
    s:=trunc(d/(1-abs(2*ll-1))*100);
  end;
  l:=trunc(ll*100);
end;

function DecimalSeparate(i:integer;separator:char=' '):string;
var s:string;
begin
  s:=inttostr(abs(i));
  Result:='';
  while length(s)>3 do begin
    Result:=separator+copy(s,length(s)-2,3)+Result;
    delete(s,length(s)-2,3);
  end;
  Result:=s+Result;
  if i<0 then Result:='-'+Result;
end;

function BinToInt(s:string):integer;
var i:integer;
begin
  Result:=0;
  for i:=1 to length(s) do
    if s[i] in ['0','1'] then begin
      Result:=Result shl 1;
      if s[i]='1' then inc(Result);
    end else
      raise EConvertError.Create('Not a binary expression! ('''+s+''')');
end;

function CreateRect(x1,y1,x2,y2:integer):TRect;
begin
  if x1<=x2 then begin
    Result.Left:=x1;Result.Right:=x2;
  end else begin
    Result.Left:=x2;Result.Right:=x1;
  end;
  if y1<=y2 then begin
    Result.Top:=y1;Result.Bottom:=y2;
  end else begin
    Result.Top:=y1;Result.Bottom:=y2;
  end;
end;

function ToUTF8(s:String):string;
var i,j:integer;   
begin
  for i:=length(s) downto 1 do begin
    j:=pos(s[i],WINChars);
    if j>0 then begin
      delete(s,i,1);
      insert(copy(utf8s,j*2-1,2),s,i);
    end;
  end;
  result:=s;
end;

function FromUTF8(s:String):string;
var i,j:integer;   
begin
  for i:=length(s)-1 downto 1 do begin
    j:=pos(copy(s,i,2),UTF8s);
    if j>0 then begin
      delete(s,i,2);
      insert(WinChars[(j+1) shr 1],s,i);
    end;
  end;
  result:=s;
end;

function GetNthSegment(s:String;sep:char;n:integer):string;
begin
  if (length(s)>0) and (s[length(s)]<>sep) then s:=s+sep;
  if CountChar(sep,s)<n then Result:=''
  else begin
    while (length(s)>0) and (n>1) do begin
      delete(s,1,pos(sep,s));
      dec(n);
    end;
    Result:=copy(s,1,pos(sep,s)-1);
  end;
end;

function ReplaceNthSegment(s:String;sep:char;n:integer;value:string):string;
begin
  if (length(s)>0) and (s[length(s)]<>sep) then s:=s+sep;
  if CountChar(sep,s)<n then Result:=''
  else begin
//    writeln('value=',value);
    Result:='';
    while (length(s)>0) and (n>1) do begin
      Result:=Result+copy(s,1,pos(sep,s));
      delete(s,1,pos(sep,s));
      dec(n);
    end;
    Result:=Result+value+sep;
    delete(s,1,pos(sep,s));
    while (length(s)>0) do begin
      Result:=Result+copy(s,1,pos(sep,s));
      delete(s,1,pos(sep,s));
    end;
  end;
end;

function UnTab(s:string;tabsize:integer):string;
begin
  Result:='';
  while pos(#9,s)>0 do begin
    Result:=Result+copy(s,1,pos(#9,s)-1);
//    if length(Result) mod tabsize=0 then 
//      Result:=Result+spc(tabsize)
//    else
    Result:=Result+spc(tabsize-length(Result) mod tabsize); 
    delete(s,1,pos(#9,s));
  end;
  Result:=Result+s;
end;

function OctToInt(s:string):integer;
var i:byte;
begin
  Result:=0;
  for i:=1 to length(s) do
    if s[i] in ['0'..'7'] then begin
      Result:=Result shl 3;
      Result:=Result+(ord(s[i])-48);
    end else
      Raise EConvertError.Create('Not an octal expression! ('''+s+''')');
end;

function Max(i1,i2:integer):integer;
begin
  if i1>i2 then Result:=i1 else Result:=i2;
end;

function Min(i1,i2:integer):integer;
begin
  if i1<i2 then Result:=i1 else Result:=i2;
end;

function IsValidUTF8(s:string):boolean;
var i,w:integer;
begin
  i:=1;
  Result:=false;
  w:=0;
  while i<=length(s) do begin
    if (i and $80)<>0 then begin
      if (i and $c0)=$80 then begin // 10xxxxxx
        if w>0 then dec(w)
               else exit;   // not expected
      end else
      if (i and $e0)=$c0 then begin // 110xxxxx
        if w=0 then w:=1
               else exit;  // not expected
      end else
      if (i and $f0)=$e0 then begin // 1110xxxx
        if w=0 then w:=2
               else exit;  // not expected
      end else
      if (i and $f8)=$f0 then begin // 11110xxx
        if w=0 then w:=3
               else exit;  // not expected
      end else exit;  // this bit combo is not valid utf-8
    end;
    inc(i);
  end;
  Result:=true;
end;

function Cp852ToUTF8(s:string):string;
var i:integer;
begin
  result:='';
  for i:=1 to length(s) do
    if ord(s[i])<127 then Result:=Result+s[i]
    else Result:=Result+Cp852_UTF8[ord(s[i])];
end;

function Win1250ToUTF8(s:string):string;
var i:integer;
begin
  result:='';
  for i:=1 to length(s) do
    if ord(s[i])<127 then Result:=Result+s[i]
    else Result:=Result+Win1250_UTF8[ord(s[i])];
end;

function GetPassword(const InputMask: Char = '*'): string;
var
  OldMode: Cardinal;
  c: char;
begin
  Result:='';
  OldMode:=0;
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode and not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  try
    while not Eof do
    begin
      Read(c);
      if c = #13 then // Carriage Return
        Break;
      if c = #8 then begin // Back Space
        if length(Result)>0 then begin
          delete(Result,length(Result),1);
          Write(#8);
        end;
      end else begin
        Write(InputMask);
        Result := Result + c;
      end;
    end;
  finally
    SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), OldMode);
  end;
end;

function ExchangeSegments(s:String; sep:char; n1,n2:integer):string;
var s2:string;
begin
  s2:=GetNthSegment(s,sep,n1);
  Result:=ReplaceNthSegment(s,sep,n1,GetNthSegment(s,sep,n2));
  Result:=ReplaceNthSegment(Result,sep,n2,s2);
end;

function CreateTimeStamp:string;
var i:integer;
begin
  Result:=DateTimeToStr(Now,FS);
  for i:=length(Result) downto 1 do
    if not(Result[i] in ['0'..'9']) then delete(Result,i,1);
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  {%H-}GetLocaleFormatSettings( 1038, FS );
  FS.ShortDateFormat:='yyyy.mm.dd';
  FS.LongTimeFormat:='hh:nn:ss';
  FS.DecimalSeparator:='.';

end.
