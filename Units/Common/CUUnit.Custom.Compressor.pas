unit CUUnit.Custom.Compressor;

interface

uses
  System.Classes, System.IOUtils, System.SyncObjs, OtlParallel, OtlCommon, OtlCollections, OtlTask,
  CUUnit.Custom.Commandline;

type
  TCustomCompressor7z = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
  strict protected
    FCompressorCommandLineOptions: TCustomCompressLineOptions;
    FRunningTasks: Boolean;
    FTaksStarted: Int64;
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure LockingWriteLn(const ALine: string);
    // procedure CompressFile(const ARootDirectory, AFilename: string);
    procedure FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
  public
    constructor Create(const ACompressorCommandLineOptions: TCustomCompressLineOptions);
    destructor Destroy; override;

    // procedure Execute;
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

function TCustomCompressor7z.Lock: Boolean;
begin
  Result := False;

  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;

  Result := True;
end;

procedure TCustomCompressor7z.Unlock;
begin
  FCriticalSection.Release;
end;

procedure TCustomCompressor7z.LockingWriteLn(const ALine: string);
begin
  if Lock then
  try
    WriteLn(ALine);
  finally
    Unlock;
  end;
end;

(*
procedure TCustomCompressor7z.Execute;
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
*)

constructor TCustomCompressor7z.Create(const ACompressorCommandLineOptions: TCustomCompressLineOptions);
begin
  inherited Create;

  FCriticalSection := TCriticalSection.Create;
  FCompressorCommandLineOptions := ACompressorCommandLineOptions;

  { Need one or more calls to stabilize, it seems... one round could be enough, hard to say
    So we warm CPU usage code up }
  for var LIndex := 1 to 5 do
  begin
    TotalCpuUsagePercentage;
    Sleep(Random(42));
  end;
end;

destructor TCustomCompressor7z.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

(*
procedure TCustomCompressor7z.CompressFile(const ARootDirectory, AFilename: string);
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
*)

procedure TCustomCompressor7z.FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
var
  LIndex: Integer;
  LFileNameOnly: string;
  LDestinationDir: string;
begin
  for LIndex := AFiles.Count - 1 downto 0 do
  begin
    LFileNameOnly := GetFileNameOnly(AFiles[LIndex]);
    LDestinationDir := ARootDirectory + LFileNameOnly;

    if not DirEmpty(LDestinationDir) then
    begin
      WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));
      AFiles.Delete(LIndex);
    end;
  end;
end;

end.
