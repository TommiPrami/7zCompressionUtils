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
    function GetCompressionLevel: TCompressionLevel; override;
    function GetCoreCount: Integer; override;
    function GetVolumeSizeMB: Integer; override;
    function GetSourceRoot: string; override;
    function ThrottleBySystemResources: Boolean; override;
  public
    // procedure Execute;
  end;

implementation

uses
  Winapi.Windows, System.Math, System.SysUtils, CUUnit.Utils;

function TDirectoryCompressor.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetDirectories(GetSourceRoot, '*.*', TSearchOption.soTopDirectoryOnly);
end;

function TDirectoryCompressor.GetSourceRoot: string;
begin
  Result := TDirectoryCompressLineOptions(FCompressorCommandLineOptions).SourceRoot;
end;

function TDirectoryCompressor.GetVolumeSizeMB: Integer;
begin
  Result := TDirectoryCompressLineOptions(FCompressorCommandLineOptions).VolumeSize;
end;

function TDirectoryCompressor.GetDestinationItemName(const ACurrentItemName: string): string;
begin
  Result := GetFileNameOnly(GetFileNameWithFilter(IncludeTrailingPathDelimiter(ACurrentItemName),
    TDirectoryCompressLineOptions(FCompressorCommandLineOptions).FileNameFilter));
end;

procedure TDirectoryCompressor.AfterItemCompressed(const AFileName: string);
begin
  inherited;

  // Nothing to do, so far
end;

function TDirectoryCompressor.GetCompressionLevel: TCompressionLevel;
begin
  Result := TDirectoryCompressLineOptions(FCompressorCommandLineOptions).CompressionLevel;
end;

function TDirectoryCompressor.GetCoreCount: Integer;
begin
  Result := TDirectoryCompressLineOptions(FCompressorCommandLineOptions).CoresToUse;
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
      + GetCompressionCommandlineOptions(GetCompressionLevel, GetVolumeSizeMB, GetCoreCount)
      + '"' + ADestinationRoot + LDestinationItemName + '.7z" "'
      + IncludeTrailingPathDelimiter(ACurrentItemName) + '*.*' +  '"';
  finally
    Unlock
  end;
end;

function TDirectoryCompressor.ThrottleBySystemResources: Boolean;
begin
  Result := TDirectoryCompressLineOptions(FCompressorCommandLineOptions).ThrottleBySystemResources;
end;

end.
