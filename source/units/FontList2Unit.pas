{ -[Name]-------------------------------------------

             MKSZTSZ SDL2 FontList Class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

   Public domain. No warranty.

   Written by Gilby/MKSZTSZ           Hungary, 2020

  --------------------------------------------------

  -[Description]------------------------------------

   A list of TFont+name. Allows outtext with
   switching beetween fonts with #0..#15 chars.

  --------------------------------------------------
}
// Version info:
//
//  V1.00: Gilby - 2020.02.07
//    * Initial creation from FontListUnit
//  V1.01: Gilby - 2021.10.10
//    * Simplified
//  V1.02: Gilby - 2022.09.05
//    + Added Outtext(TARGBImage,text,x,y,align);
//  V1.02a: Gilby - 2023.01.25
//    * Tidying code

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit FontList2Unit;

interface

uses Classes, Font2Unit, Lists, ARGBImageUnit;

type

  { TFontList }

  TFontList=class(TNamedList<TFont>)
    procedure Add(aFont:TFont;aName:String); overload;
    // Adds Font to the list with the aname name.

    procedure Delete(aName:string); reintroduce;

    procedure ListItems;
    procedure DemoFonts;

    procedure OutText(text:string;x,y,align:integer); overload;
    procedure OutText(Target:TARGBImage;text:string;x,y,align:integer); overload;

  protected
    function fGetFont2(index:string):TFont;
  public
    property Fonts[index:string]:TFont read fGetFont2; default;
  end;
       
implementation

uses SysUtils, MKToolBox, Logger, MKStream;

const
  Fstr={$I %FILE%}+', ';
  Version='1.02a';

// ---------------------------------------------------------- [ TFontList ] ---

procedure TFontList.Add(aFont:TFont;aName:String);
begin
  AddObject(aName,aFont);
end;

procedure TFontList.Delete(aName: string);
var i:longint;
begin
  i:=IndexOf(aName);
  if i>-1 then begin
    Items[i].Free;
    inherited Delete(i);
  end;
end;

function TFontList.fGetFont2(index:string):TFont;
var i:integer;
begin
  i:=IndexOf(index);
  if i>-1 then
    Result:=Items[i]
  else
    Result:=nil;
end;

procedure TFontList.ListItems;
const Istr=Fstr+'TFontList.ListItems';
var i:integer;
begin
  Log.LogStatus('FontList listing starts...',Istr);
  for i:=0 to Count-1 do
    Log.LogStatus(st(i,3,' ')+'. '+Strings[i]);
  Log.LogStatus('FontList listing ends...',Istr);
end;

procedure TFontList.OutText(text:string;x,y,align:integer);
var i,wi,af:integer;
begin
  if not ((Self.Count=0) or (text='')) then begin
    wi:=0;
    af:=0;
    for i:=1 to length(text) do begin
      if text[i] in [#0..#15] then begin
        af:=ord(text[i]);
        if af>Self.Count then af:=Self.Count-1;
      end
      else wi+=Items[af].TextWidth(text[i])+Items[af].LetterSpace*Items[af].size;
    end;
    wi-=Items[af].LetterSpace*Items[af].Size;
    case align of
      mjLeft:;
      mjCenter:x-=wi div 2;
      mjRight:x-=wi;
    end;
    af:=0;
    for i:=1 to length(text) do
      if text[i] in [#0..#15] then begin
        af:=ord(text[i]);
        if af>Self.Count then af:=Self.Count-1;
      end else begin
        Items[af].OutText(text[i],x,y,mjLeft);
        x+=Items[af].TextWidth(text[i])+Items[af].LetterSpace*Items[af].Size;
      end;
  end;
end;

procedure TFontList.OutText(Target: TARGBImage; text: string; x, y,
  align: integer);
var i,wi,af:integer;
begin
  if not ((Self.Count=0) or (text='')) then begin
    wi:=0;
    af:=0;
    for i:=1 to length(text) do begin
      if text[i] in [#0..#15] then begin
        af:=ord(text[i]);
        if af>Self.Count then af:=Self.Count-1;
      end
      else wi+=Items[af].TextWidth(text[i])+Items[af].LetterSpace*Items[af].size;
    end;
    wi-=Items[af].LetterSpace*Items[af].Size;
    case align of
      mjLeft:;
      mjCenter:x-=wi div 2;
      mjRight:x-=wi;
    end;
    af:=0;
    for i:=1 to length(text) do
      if text[i] in [#0..#15] then begin
        af:=ord(text[i]);
        if af>Self.Count then af:=Self.Count-1;
      end else begin
        Items[af].OutText(Target,text[i],x,y,mjLeft);
        x+=Items[af].TextWidth(text[i])+Items[af].LetterSpace*Items[af].Size;
      end;
  end;
end;

procedure TFontList.DemoFonts;
var i,t:integer;
begin
  t:=0;
  for i:=0 to Count-1 do begin
    Items[i].OutText('Demo Text 123! . :)',0,t,mjLeft);
    t+=Items[i].Height+2;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
