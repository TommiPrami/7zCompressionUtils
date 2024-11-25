unit FCUnit.FileCompressor;

interface

uses
  System.Classes, System.SyncObjs, System.IOUtils, FCUnit.CommandLine, CUUnit.Custom.Compressor;

type
  TFileCompress7z = class(TCustomCompressor7z)
  strict private
  strict protected
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); override;
    function GetItemsToBeCompressed: TArray<string>; override;
  public
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

function TFileCompress7z.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetFiles(FCompressorCommandLineOptions.SourceRoot, FCompressorCommandLineOptions.FileNameFilter,
    TSearchOption.soTopDirectoryOnly)
end;

procedure TFileCompress7z.PrepareITemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LFileNameOnly: string;
  LDestinationDir: string;
  LCommandLine: string;
begin
  LFileNameOnly := GetFileNameOnly(ACurrentItemName);
  LDestinationDir := FCompressorCommandLineOptions.SourceRoot + LFileNameOnly;

  if Lock then
  try
    LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md1024m -mfb256 -mmt=off -v1000m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
      + FCompressorCommandLineOptions.SourceRoot + ACurrentItemName + '"';
  finally
    Unlock
  end;
end;

end.
