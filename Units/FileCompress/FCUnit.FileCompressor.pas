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
    function GetCompressionLevel: TCompressionLevel; override;
    function GetCoreCount: Integer; override;
    function GetVolumeSizeMB: Integer; override;
    function GetSourceRoot: string; override;
    function ThrottleBySystemResources: Boolean; override;
  public
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Utils;

function TFileCompress7z.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetFiles(TFileCompressOptions(FCompressorCommandLineOptions).SourceRoot,
    TFileCompressOptions(FCompressorCommandLineOptions).FileNameFilter, TSearchOption.soTopDirectoryOnly);
end;

function TFileCompress7z.GetSourceRoot: string;
begin
  Result := TFileCompressOptions(FCompressorCommandLineOptions).SourceRoot;
end;

function TFileCompress7z.GetVolumeSizeMB: Integer;
begin
  Result := TFileCompressOptions(FCompressorCommandLineOptions).VolumeSize;
end;

function TFileCompress7z.GetDestinationItemName(const ACurrentItemName: string): string;
begin
  Result := GetFileNameOnly(ACurrentItemName);
end;

procedure TFileCompress7z.AfterItemCompressed(const AFileName: string);
begin
  inherited;

  if TFileCompressOptions(FCompressorCommandLineOptions).DeleteSourceItemWhenDone then
    if FileExists(AFileName) then
      if not DeleteFile(AFileName) then
        LockingWriteLn('Could not delete source file ' + AFileName.QuotedString('"'));
end;

function TFileCompress7z.GetCompressionLevel: TCompressionLevel;
begin
  Result := TFileCompressOptions(FCompressorCommandLineOptions).CompressionLevel;
end;

function TFileCompress7z.GetCoreCount: Integer;
begin
  Result := ScaleCoreCount(TFileCompressOptions(FCompressorCommandLineOptions).CoresToUse);
end;

function TFileCompress7z.GetDestinationDirectory(const ACurrentDestinationItemName: string): string;
begin
  Result := GetSourceRoot + ACurrentDestinationItemName;
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
      + GetCompressionCommandlineOptions(GetCompressionLevel, GetVolumeSizeMB, GetCoreCount)
      + '"' + IncludeTrailingPathDelimiter(ADestinationRoot) + LDestinationItemName + '.7z" '
      + ACurrentItemName.QuotedString('"');
  finally
    Unlock
  end;
end;

function TFileCompress7z.ThrottleBySystemResources: Boolean;
begin
  Result := TFileCompressOptions(FCompressorCommandLineOptions).ThrottleBySystemResources;
end;

end.
