unit DCUnit.DirectoryCompressor;

interface

uses
  System.Classes, System.Diagnostics, System.IOUtils, System.SyncObjs, DCUnit.CommandLine,
  OtlParallel, OtlCommon, OtlCollections, OtlTask, CUUnit.Custom.Compressor;

type
  TDirectoryCompressor = class(TCustomCompressor7z)
  strict private
    procedure CompressFile(const ACurrentDirectoryName, ADestinationRoot: string);
  public
    procedure Execute;
  end;

implementation

uses
  Winapi.Windows, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

procedure TDirectoryCompressor.Execute;
var
  LDirectories: TStringList;
begin
  LDirectories := TStringList.Create;
  try
    LDirectories.AddStrings(TDirectory.GetDirectories(FCompressorCommandLineOptions.SourceRoot, '*.*', TSearchOption.soTopDirectoryOnly));

    FilterByDirectories(LDirectories, TDirectoryCompressLineOptions(FCompressorCommandLineOptions).DestinationRoot);

    if LDirectories.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LDirectories)
        .NumTasks(GetMaxThreadCount)
        .OnStop(
          procedure
          begin
            if Lock then
            try
              FRunningTasks := False;
            finally
              Unlock;
            end;
          end)
        .NoWait
        .Execute(
          procedure(const ADirectoryName: TOmniValue)
          var
            LCurrentDirectoryName: string;
          begin
            LCurrentDirectoryName := ADirectoryName;

            CompressFile(LCurrentDirectoryName, TDirectoryCompressLineOptions(FCompressorCommandLineOptions).DestinationRoot);
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
      LockingWriteLn('No files found from directory "' + FCompressorCommandLineOptions.SourceRoot);

      Exit;
    end;
  finally
    LDirectories.Free;
  end;
end;

procedure TDirectoryCompressor.CompressFile(const ACurrentDirectoryName, ADestinationRoot: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LDestinationDirName: string;
  LDestinationRoot: string;
  LCommandLine: string;
begin
  LDestinationDirName := GetFileNameOnly(GetFileNameWithFilter(IncludeTrailingPathDelimiter(ACurrentDirectoryName),
    FCompressorCommandLineOptions.FileNameFilter));

  if LDestinationDirName.IsEmpty then
    Exit;

  LDestinationRoot := IncludeTrailingPathDelimiter(ADestinationRoot + LDestinationDirName);

  if Lock then
  try
    LCommandLine := EXE_7Z.QuotedString('"') + ' ' + 'a -r'
      + GetCompressionCommandlineOptions(FCompressorCommandLineOptions.CompressionLevel) + '-v1000m '
      + '"' + LDestinationRoot + LDestinationDirName + '.7z" "'
      + IncludeTrailingPathDelimiter(ACurrentDirectoryName) + '*.*' +  '"';

    WriteLn('Executing: ' + LCommandLine + '...');

    Inc(FTaksStarted);
  finally
    Unlock
  end;

  WaitForSystemStatus(IfThen(FTaksStarted <= 1, 333, 10 * 666), 76.66, 76.66);

  if not DirectoryExists(LDestinationRoot) then
    ForceDirectories(LDestinationRoot);

  ExecuteAndWait(LCommandLine, fcpcIdle);
end;

end.
