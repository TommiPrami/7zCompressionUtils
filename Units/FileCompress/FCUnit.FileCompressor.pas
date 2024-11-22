﻿unit FCUnit.FileCompressor;

interface

uses
  System.Classes, System.IOUtils, System.SyncObjs, OtlParallel, OtlCommon, OtlCollections, OtlTask, FCUnit.CommandLine,
  CUUnit.Custom.Compressor;

type
  TFileCompress7z = class(TCustomCompressor7z)
  strict private
    procedure CompressFile(const ARootDirectory, AFilename: string);
  public
    procedure Execute;
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

procedure TFileCompress7z.Execute;
var
  LFiles: TStringList;
begin
  LFiles := TStringList.Create;
  try
    LFiles.AddStrings(TDirectory.GetFiles(FCompressorCommandLineOptions.SourceRoot, FCompressorCommandLineOptions.FileNameFilter,
      TSearchOption.soTopDirectoryOnly));

    FilterByDirectories(LFiles, FCompressorCommandLineOptions.SourceRoot);

    if LFiles.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LFiles)
        .NumTasks(ScaleCoreCount(FCompressorCommandLineOptions.CoresToUse))
        .OnStop(
          procedure
          begin
            if Lock then
            try
              FRunningTasks := False;
            finally
              Unlock;
            end;
          end
        )
        .NoWait
        .Execute(
          procedure(const AFileName: TOmniValue)
          var
            LCurrentFile: string;
          begin
            LCurrentFile := AFileName;

            CompressFile(FCompressorCommandLineOptions.SourceRoot, ExtractFileName(LCurrentFile));
          end
        );

      while True do
      begin
        Sleep(200);
        ProcessMessages;

        if Lock then
        try
          if not FRunningTasks then
            Break;
        finally
          Unlock;
        end;
      end;

      LStopWatch.Stop;
      LockingWriteLn(' Elapsed time: ' + LStopWatch.Elapsed.ToString);
    end
    else
    begin
      LockingWriteLn('No files found from directory "' + FCompressorCommandLineOptions.SourceRoot + '" with search pattern "' + FCompressorCommandLineOptions.FileNameFilter + '"');

      Exit;
    end;
  finally
    LFiles.Free;
  end;
end;

procedure TFileCompress7z.CompressFile(const ARootDirectory, AFilename: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LFileNameOnly: string;
  LDestinationDir: string;
  LCommandLine: string;
begin
  LFileNameOnly := GetFileNameOnly(AFilename);
  LDestinationDir := ARootDirectory + LFileNameOnly;

  if Lock then
  try
    LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md1024m -mfb256 -mmt=off -v1000m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
      + ARootDirectory + AFilename + '"';

    WriteLn('Executing: ' + LCommandLine + '...');
    Inc(FTaksStarted);
  finally
    Unlock
  end;

  WaitForSystemStatus(IfThen(FTaksStarted <= 1, 100, 4000), 76.66, 76.66);

  if not DirectoryExists(LDestinationDir) then
    ForceDirectories(LDestinationDir);

  ExecuteAndWait(LCommandLine, fcpcIdle);
end;

end.
