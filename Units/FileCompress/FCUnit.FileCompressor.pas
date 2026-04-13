unit FCUnit.FileCompressor;

interface

uses
  System.Classes, System.IOUtils, System.SyncObjs, CUUnit.Custom.Compressor, FCUnit.CommandLine, CUUnit.Types;

type
  TFileCompress7z = class(TCustomCompressor7z)
  strict protected
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); override;
    procedure AfterItemCompressed(const AFileName: string); override;
    function GetItemsToBeCompressed: TArray<string>; override;
    function GetDestinationItemName(const ACurrentItemName: string): string; override;
    function GetDestinationDirectory(const ACurrentDestinationItemName: string): string; override;
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Utils;

function TFileCompress7z.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetFiles(TFileCompressOptions(FCompressorCommandLineOptions).SourceRoot,
    TFileCompressOptions(FCompressorCommandLineOptions).FileNameFilter, TSearchOption.soTopDirectoryOnly);
end;

procedure TFileCompress7z.AfterItemCompressed(const AFileName: string);
begin
  inherited;

  if TFileCompressOptions(FCompressorCommandLineOptions).DeleteSourceItemWhenDone then
    if FileExists(AFileName) then
      if not DeleteFile(AFileName) then
        LockingWriteLn('Could not delete source file ' + AFileName.QuotedString('"'));
end;

function TFileCompress7z.GetDestinationItemName(const ACurrentItemName: string): string;
begin
  Result := GetFileNameOnly(ACurrentItemName);
end;

function TFileCompress7z.GetDestinationDirectory(const ACurrentDestinationItemName: string): string;
begin
  Result := FCompressorCommandLineOptions.SourceRoot + ACurrentDestinationItemName;
end;

procedure TFileCompress7z.PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string);
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
    ACommandLine := EXE_7Z + ' ' + 'a '
      + GetCompressionCommandlineOptions(FCompressorCommandLineOptions.CompressionLevel, FCompressorCommandLineOptions.VolumeSize,
       FCompressorCommandLineOptions.CoresToUse)
      + '"' + IncludeTrailingPathDelimiter(ADestinationRoot) + LDestinationItemName + '.7z" '
      + ACurrentItemName.QuotedString('"');
  finally
    Unlock
  end;
end;

end.
