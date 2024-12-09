{
  This file is part of the source code of EarthShaker Redux.
  See "copyright.txt" for details.
}

program Weaver2;

{$ifndef DEBUG}{$apptype GUI}{$endif}

uses
  // For Format tool
  SysUtils,
  // For ProgramVersion
  FileInfo,
  winpeimagereader,
  // For reading PNG files in TARGBImage
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  // The game itself.
  W2Main;

const
  BDATE={$i %DATE%};

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then
    Result:=Format('%d.%d build %d',[PV.Major,PV.Minor,PV.Build])
  else
    Result:=Format('%d.%d.%d build %d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
end;

{$R *.res}

begin
  with TMain.Create(GetVersionString,BDATE) do try
    Run;
  finally
    Free;
  end;
end.

