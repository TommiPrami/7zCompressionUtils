unit DCUnit.DirectoryCompressor;

interface

uses
  System.Classes, System.SyncObjs, System.IOUtils, DCUnit.CommandLine, CUUnit.Custom.Compressor, CUUnit.Types;

type
  TDirectoryCompressor = class(TCustomCompressor7z)
  strict protected
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); override;
    procedure AfterItemCompressed(const AFileName: string); override;
    function GetItemsToBeCompressed: TArray<string>; override;
    function GetDestinationItemName(const ACurrentItemName: string): string; override;
    function GetDestinationDirectory(const ACurrentDestinationItemName: string): string; override;
  public
    // procedure Execute;
  end;

implementation

uses
  Winapi.Windows, System.Math, System.SysUtils, CUUnit.Utils;

function TDirectoryCompressor.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetDirectories(FCompressorCommandLineOptions.SourceRoot, '*.*', TSearchOption.soTopDirectoryOnly);
end;

function TDirectoryCompressor.GetDestinationItemName(const ACurrentItemName: string): string;
begin
  Result := GetFileNameOnly(GetFileNameWithFilter(IncludeTrailingPathDelimiter(ACurrentItemName),
    FCompressorCommandLineOptions.FileNameFilter));
end;

procedure TDirectoryCompressor.AfterItemCompressed(const AFileName: string);
begin
  inherited;

  // Nothing to do, so far
end;

function TDirectoryCompressor.GetDestinationDirectory(const ACurrentDestinationItemName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(TDirectoryCompressLineOptions(FCompressorCommandLineOptions).DestinationRoot + ACurrentDestinationItemName);
end;

procedure TDirectoryCompressor.PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LDestinationItemName: string;
begin
  ADestinationRoot := '';
  ACommandLine := '';

  LDestinationItemName := GetDestinationItemName(ACurrentItemName);

  if LDestinationItemName.IsEmpty then
    Exit;

  ADestinationRoot := GetDestinationDirectory(LDestinationItemName);

  // TODO: Should not need to lock here
  if Lock then
  try
    ACommandLine := EXE_7Z.QuotedString('"') + ' ' + 'a -r'
      + GetCompressionCommandlineOptions(FCompressorCommandLineOptions.CompressionLevel, FCompressorCommandLineOptions.VolumeSize,
        FCompressorCommandLineOptions.CoresToUse)
      + '"' + ADestinationRoot + LDestinationItemName + '.7z" "'
      + IncludeTrailingPathDelimiter(ACurrentItemName) + '*.*' +  '"';
  finally
    Unlock;
  end;
end;


end.
