unit DCUnit.DirectoryCompressor;

interface

uses
  System.Classes, System.SyncObjs, System.IOUtils, DCUnit.CommandLine, CUUnit.Custom.Compressor;

type
  TDirectoryCompressor = class(TCustomCompressor7z)
  strict private
  strict protected
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); override;
    function GetItemsToBeCompressed: TArray<string>; override;
  public
    // procedure Execute;
  end;

implementation

uses
  Winapi.Windows, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

function TDirectoryCompressor.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetDirectories(FCompressorCommandLineOptions.SourceRoot, '*.*', TSearchOption.soTopDirectoryOnly);
end;

procedure TDirectoryCompressor.PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LDestinationName: string;
begin
  LDestinationName := GetFileNameOnly(GetFileNameWithFilter(IncludeTrailingPathDelimiter(ACurrentItemName),
    FCompressorCommandLineOptions.FileNameFilter));

  if LDestinationName.IsEmpty then
    Exit;

  ADestinationRoot := IncludeTrailingPathDelimiter(TDirectoryCompressLineOptions(FCompressorCommandLineOptions).DestinationRoot + LDestinationName);

  if Lock then
  try
    ACommandLine := EXE_7Z.QuotedString('"') + ' ' + 'a -r'
      + GetCompressionCommandlineOptions(FCompressorCommandLineOptions.CompressionLevel) + '-v1000m '
      + '"' + ADestinationRoot + LDestinationName + '.7z" "'
      + IncludeTrailingPathDelimiter(ACurrentItemName) + '*.*' +  '"';
  finally
    Unlock
  end;
end;

end.
