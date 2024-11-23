unit FCUnit.FileCompressor;

interface

uses
  System.Classes, System.IOUtils, System.SyncObjs, OtlParallel, OtlCommon, OtlCollections, OtlTask, FCUnit.CommandLine;

type
  TFileCompress7z = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
    FRunningTasks: Boolean;
    FTaksStarted: Int64;
    FFileCompressLineOptions: TFileCompressLineOptions;
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure LockingWriteLn(const ALine: string);
    procedure CompressFile(const ARootDirectory, AFilename: string);
    procedure FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
  public
    constructor Create(const AFileCompressLineOptions: TFileCompressLineOptions);
    destructor Destroy; override;

    procedure Execute;
  end;

implementation

uses
  System.Diagnostics, System.Math, System.SysUtils, CUUnit.Types, CUUnit.Utils;

function TFileCompress7z.Lock: Boolean;
begin
  Result := False;

  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;

  Result := True;
end;

procedure TFileCompress7z.Unlock;
begin
  FCriticalSection.Release;
end;

procedure TFileCompress7z.LockingWriteLn(const ALine: string);
begin
  if Lock then
  try
    WriteLn(ALine);
  finally
    Unlock;
  end;
end;

procedure TFileCompress7z.Execute;
var
  LFiles: TStringList;
begin
  LFiles := TStringList.Create;
  try
    LFiles.AddStrings(TDirectory.GetFiles(FFileCompressLineOptions.SourceRoot, FFileCompressLineOptions.FileNameFilter,
      TSearchOption.soTopDirectoryOnly));

    FilterByDirectories(LFiles, FFileCompressLineOptions.SourceRoot);

    if LFiles.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LFiles)
        .NumTasks(ScaleCoreCount(FFileCompressLineOptions.CoresToUse))
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

            CompressFile(FFileCompressLineOptions.SourceRoot, ExtractFileName(LCurrentFile));
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
      LockingWriteLn('No files found from directory "' + FFileCompressLineOptions.SourceRoot + '" with search pattern "' + FFileCompressLineOptions.FileNameFilter + '"');

      Exit;
    end;
  finally
    LFiles.Free;
  end;
end;

constructor TFileCompress7z.Create(const AFileCompressLineOptions: TFileCompressLineOptions);
begin
  inherited Create;

  FCriticalSection := TCriticalSection.Create;
  FFileCompressLineOptions := AFileCompressLineOptions;

  { Need one or more calls to stabilize, it seems... one round could be enough, hard to say
    So we warm CPU usage code up }
  for var LIndex := 1 to 5 do
  begin
    TotalCpuUsagePercentage;
    Sleep(Random(42));
  end;
end;

destructor TFileCompress7z.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
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

procedure TFileCompress7z.FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
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
