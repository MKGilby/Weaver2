{
  -[Name]-------------------------------------------

                    TGradient class

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ   Hungary, 2024

  -[Description]------------------------------------

    You can give a start and end color and three
    interim colors (with their position).

    You can get the color of the gradient at any
    position.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2024.09.13
//     * Initial creation


unit GradientUnit;

{$mode Delphi}

interface

uses SysUtils;

type

  { TGradient }

  TGradient=class
    // Create gradient.
    constructor Create(iColor1,iColor2:uint32);

    // Gives back the color in the gradient at pValue on a scale to 0..1
    function GetColorAt(pValue:double):uint32;

    // Gives back the color in the gradient at pValue on a scale to 0..1
    // discarding reverse or pingpong flags.
    function GetColorAtRaw(pValue:double):uint32;

    // Log contents
    procedure LogContents;
  private
    fColors:array[1..5] of uint32;
    fColorPositions:array[1..5] of double;
    fPingpong:boolean;
    fReversed:boolean;
    fUsed:array[1..5] of boolean;
    fR,fG,fB,fA:array[1..5] of integer;

    fOrder:array of integer;

    procedure fSetColor(index:integer;value:uint32);
    function fGetColor(index:integer):uint32;
    procedure fSetColorPos(index:integer;value:double);
    function fGetColorPos(index:integer):double;
    function fGetColorUsed(index:integer):boolean;
    procedure fSetColorUsed(index:integer;value:boolean);
    procedure SortColors;
  public
    // Color values for start color (index=1)
    //                  end color (index=2)
    //                  interim colors (indexed 3 to 5).
    // For interim colors set ColorUsed[n] to true to use color.
    property Colors[index:integer]:uint32 read fGetColor write fSetColor;
    // Position of the interim color (indexed 3 to 5)
    property ColorPositions[index:integer]:double read fGetColorPos write fSetColorPos;
    // Is the interim color (indexed 3 to 5) is used in the gradient?
    property ColorUsed[index:integer]:boolean read fGetColorUsed write fSetColorUsed;
    // Reverse the gradient.
    property Reversed:boolean read fReversed write fReversed;
    // Extend the gradient with the reversed gradient.
    property PingPong:boolean read fPingpong write fPingpong;
  end;

implementation

uses Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';


{ TGradient }

constructor TGradient.Create(iColor1,iColor2:uint32);
begin
  fUsed[1]:=true;
  fUsed[2]:=true;
  fUsed[3]:=false;
  fUsed[4]:=false;
  fUsed[5]:=false;
  fColorPositions[1]:=0;
  fColorPositions[2]:=1;
  fColorPositions[3]:=0.25;
  fColorPositions[4]:=0.5;
  fColorPositions[5]:=0.75;
  fSetColor(1,iColor1);
  fSetColor(2,iColor2);
  fSetColor(3,0);
  fSetColor(4,0);
  fSetColor(5,0);
end;

function TGradient.GetColorAt(pValue:double):uint32;
var i:integer;
begin
  if pValue<0 then pValue:=0
  else if pValue>1 then pValue:=1;
  if fPingpong then begin
    pValue:=pValue*2;
    if pValue>1 then pValue:=2-pValue;
  end;
  if fReversed then pValue:=1-pValue;
  Result:=0;
  for i:=0 to length(fOrder)-2 do
    if (pValue>=fColorPositions[fOrder[i]]) and
       (pValue<=fColorPositions[fOrder[i+1]]) then begin
     Result:=
       (round(fA[fOrder[i]]+(fA[fOrder[i+1]]-fA[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 24+
       (round(fR[fOrder[i]]+(fR[fOrder[i+1]]-fR[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 16+
       (round(fG[fOrder[i]]+(fG[fOrder[i+1]]-fG[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 8+
       (round(fB[fOrder[i]]+(fB[fOrder[i+1]]-fB[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff);
     exit;
   end;
end;

function TGradient.GetColorAtRaw(pValue:double):uint32;
var i:integer;
begin
  if pValue<0 then pValue:=0
  else if pValue>1 then pValue:=1;
  Result:=0;
  for i:=0 to length(fOrder)-2 do
    if (pValue>=fColorPositions[fOrder[i]]) and
       (pValue<=fColorPositions[fOrder[i+1]]) then begin
     Result:=
       (round(fA[fOrder[i]]+(fA[fOrder[i+1]]-fA[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 24+
       (round(fR[fOrder[i]]+(fR[fOrder[i+1]]-fR[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 16+
       (round(fG[fOrder[i]]+(fG[fOrder[i+1]]-fG[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff) << 8+
       (round(fB[fOrder[i]]+(fB[fOrder[i+1]]-fB[fOrder[i]])*(pValue-fColorPositions[fOrder[i]])/(fColorPositions[fOrder[i+1]]-fColorPositions[fOrder[i]])) and $ff);
     exit;
   end;
end;

procedure TGradient.LogContents;
var c:char;i:integer;
begin
  Log.LogStatus('-------------------------------');
  Log.LogStatus('Gradient content logging starts');
  Log.LogStatus('');
  Log.LogStatus(Format('Start color: %.8x',[fColors[1]]));
  Log.LogStatus(Format('End color  : %.8x',[fColors[2]]));
  for i:=3 to 5 do begin
    if fUsed[i] then c:='X' else c:=' ';
    Log.LogStatus(Format('[%s] Color %d: %.8x, Position: %4.2f',[c,i,fColors[i],fColorPositions[i]]));
  end;
  Log.LogStatus('Gradient content logging ends');
  Log.LogStatus('-----------------------------');
end;

procedure TGradient.fSetColor(index:integer; value:uint32);
begin
  if index in [1..5] then begin
    if fColors[index]<>value then begin
      fColors[index]:=value;
      fA[index]:=(fColors[index] and $FF000000)>>24;
      fR[index]:=(fColors[index] and $FF0000)>>16;
      fG[index]:=(fColors[index] and $FF00)>>8;
      fB[index]:=(fColors[index] and $FF);
      SortColors;
    end;
  end;
end;

function TGradient.fGetColor(index:integer):uint32;
begin
  if index in [1..5] then
    Result:=fColors[index]
  else
    Result:=0;
end;

procedure TGradient.fSetColorPos(index:integer; value:double);
begin
  if (index in [3..5]) and (value>0) and (value<1) then
    if fColorPositions[index]<>value then begin
      fColorPositions[index]:=value;
      SortColors;
    end;
end;

function TGradient.fGetColorPos(index:integer):double;
begin
  if index in [1..5] then
    Result:=fColorPositions[index]
  else
    Result:=-1;
end;

function TGradient.fGetColorUsed(index:integer):boolean;
begin
  if index in [1..5] then
    Result:=fUsed[index]
  else
    Result:=false;
end;

procedure TGradient.fSetColorUsed(index:integer; value:boolean);
begin
  if index in [3..5] then
    if fUsed[index]<>value then begin
      fUsed[index]:=value;
      SortColors;
    end;
end;

procedure TGradient.SortColors;
var i,j,k:integer;
begin
  SetLength(fOrder,0);
  for i:=1 to 5 do if fUsed[i] then begin
    j:=0;
    while (j<length(fOrder)) and (fColorPositions[fOrder[j]]<fColorPositions[i]) do inc(j);
    if j=length(fOrder) then begin  // Add at end
      SetLength(fOrder,length(fOrder)+1);
      fOrder[length(fOrder)-1]:=i;
    end else begin  // Insert at j
      SetLength(fOrder,length(fOrder)+1);
      for k:=length(fOrder)-1 downto j+1 do
        fOrder[k]:=fOrder[k-1];
      fOrder[j]:=i;
    end;
  end;
//  Log.Trace('Order:');
//  for i:=0 to length(fOrder)-1 do
//    Log.Trace(Format('  %d. %d',[i+1,fOrder[i]]));
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

