﻿unit FCUnit.FileCompressor;

interface

uses
  System.Classes, System.SyncObjs, System.IOUtils, FCUnit.CommandLine, CUUnit.Custom.Compressor;

type
  TFileCompress7z = class(TCustomCompressor7z)
  strict protected
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); override;
    function GetItemsToBeCompressed: TArray<string>; override;
    function GetDestinationItemName(const ACurrentItemName: string): string; override;
    function GetDestinationDirectory(const ACurrentDestinationItemName: string): string; override;
    function GetCoreCount: Integer; override;
    function GetSourceRoot: string; override;
  public
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

function TFileCompress7z.GetItemsToBeCompressed: TArray<string>;
begin
  Result := TDirectory.GetFiles(TFileCompressLineOptions(FCompressorCommandLineOptions).SourceRoot,
    TFileCompressLineOptions(FCompressorCommandLineOptions).FileNameFilter, TSearchOption.soTopDirectoryOnly);
end;

function TFileCompress7z.GetSourceRoot: string;
begin
  Result := TFileCompressLineOptions(FCompressorCommandLineOptions).SourceRoot;
end;

function TFileCompress7z.GetDestinationItemName(const ACurrentItemName: string): string;
begin
  Result := GetFileNameOnly(ACurrentItemName);
end;

function TFileCompress7z.GetCoreCount: Integer;
begin
  Result := ScaleCoreCount(TFileCompressLineOptions(FCompressorCommandLineOptions).CoresToUse)
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
    ACommandLine := EXE_7Z + ' ' + 'a -mx9 -md1024m -mfb256 -mmt=off -v1000m "'
      + IncludeTrailingPathDelimiter(ADestinationRoot) + LDestinationItemName + '.7z" '
      + ACurrentItemName.QuotedString('"');
  finally
    Unlock
  end;
end;

end.
