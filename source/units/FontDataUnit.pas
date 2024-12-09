{ -[Name]-------------------------------------------

                  TFontData class

  --------------------------------------------------

  -[Disclaimer]-------------------------------------

             You can freely distribute it
             under the GNU GPL Version 2.

  Written by Gilby/MKSZTSZ
  Hungary, 2020

  --------------------------------------------------

  -[Description]------------------------------------

   Base for TFont class, but without images

  -------------------------------------------------- }

// Version info:
//   V1.00: Gilby - 2020.03.16
//      - Initial creation from Font2Unit
//   V1.01: Gilby - 2024.08.15
//      - Get rid of sdl2 TRect.

{$ifdef fpc}
  {$mode delphi}
  {$smartlink on}
{$endif}

unit FontDataUnit;

interface

type
  TCharRect=record
    Left,Top,Width,Height:integer;
  end;

  TFontData=class
    constructor Create;
    procedure SetCharBox(c,x,y,w,h:integer);
  protected
    // Character box for each char. Set width to 0 if the char is not present in font.
    fCharBoxes:array [0..255] of TCharRect;
    function fGetCharBox(index:integer):TCharRect;
  public
    property CharBoxes[index:integer]:TCharRect read fGetCharBox;
  end;

implementation

uses sysutils, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.01';

// ----------------------------------------------------------- [ TFontData ]---

constructor TFontData.Create;
var i:integer;
begin
  for i:=0 to 255 do
    fCharBoxes[i].Width:=0;
end;

procedure TFontData.SetCharBox(c,x,y,w,h:integer);
begin
  if (c>=0) and (c<256) then begin
    fCharBoxes[c].Left:=x;
    fCharBoxes[c].Top:=y;
    fCharBoxes[c].Width:=w;
    fCharBoxes[c].Height:=h;
  end else raise Exception.Create(Format('Invalid character index in TFontData.SetCharBox! (%d)',[c]));
end;

function TFontData.fGetCharBox(index:integer):TCharRect;
begin
  if (index>=0) and (index<256) then
    Result:=fCharBoxes[index]
  else
    Result:=fCharBoxes[0];
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.
