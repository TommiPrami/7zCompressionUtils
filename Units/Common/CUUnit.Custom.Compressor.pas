unit CUUnit.Custom.Compressor;

interface

uses
  System.Classes, System.SyncObjs, OtlParallel, OtlCommon, OtlCollections, OtlTask,
  CUUnit.Custom.Commandline;

type
  TCustomCompressor7z = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
    FTaksStarted: Int64;
    FTaskTotal: Integer;
    FRunningTasks: Boolean;
    procedure CompressItem(const ACurrentItem, ADestinationRoot, ACommandLine: string);
  strict protected
    FCompressorCommandLineOptions: TCustomCompressLineOptions;
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure LockingWriteLn(const ALine: string; const AIndent: Integer = 0);
    procedure FilterByDirectories(const AItems: TStringList; const ARootDirectory: string);
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); virtual; abstract;
    function GetItemsToBeCompressed: TArray<string>; virtual; abstract;
  public
    constructor Create(const ACompressorCommandLineOptions: TCustomCompressLineOptions);
    destructor Destroy; override;

    procedure Execute;
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

procedure TCustomCompressor7z.LockingWriteLn(const ALine: string; const AIndent: Integer = 0);
begin
  if Lock then
  try
    WriteLn(StringOfChar(' ', AIndent * 2) + ALine);
  finally
    Unlock;
  end;
end;

procedure TCustomCompressor7z.CompressItem(const ACurrentItem, ADestinationRoot, ACommandLine: string);
var
  LTasksStarted: Integer;
begin
  if not Lock then
    Exit;

  try
    Inc(FTaksStarted);
    LTasksStarted := FTaksStarted;
  finally
    UnLock;
  end;

  WaitForSystemStatus(IfThen(LTasksStarted <= 1, 333, 10 * 666), 76.66, 76.66);

  if not DirectoryExists(ADestinationRoot) then
    ForceDirectories(ADestinationRoot);

  var LIntFormat := GetIntFormat(FTaskTotal);

  LockingWriteLn(Format(LIntFormat, [LTasksStarted]) + '/' + Format(LIntFormat, [FTaskTotal]) + 'Executing: '
    + ACurrentItem + '...', 2);
  ExecuteAndWait(ACommandLine, fcpcIdle);
end;

constructor TCustomCompressor7z.Create(const ACompressorCommandLineOptions: TCustomCompressLineOptions);
begin
  inherited Create;

  FCriticalSection := TCriticalSection.Create;
  FCompressorCommandLineOptions := ACompressorCommandLineOptions;

  { Need one or more calls to stabilize, it seems... one round could be enough, hard to say
    So we warm CPU usage code up }
  for var LIndex := 1 to 3 do
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

procedure TCustomCompressor7z.Execute;
var
  LItemsToBeCompressed: TStringList;
begin
  LItemsToBeCompressed := TStringList.Create;
  try
    LItemsToBeCompressed.AddStrings(GetItemsToBeCompressed);

    FilterByDirectories(LItemsToBeCompressed, FCompressorCommandLineOptions.SourceRoot);

    if LItemsToBeCompressed.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LItemsToBeCompressed)
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
            LCurrentItem: string;
            LDestinationRoot: string;
            LCommandLine: string;
          begin
            LCurrentItem := AFileName;

            PrepareITemForCompression(LCurrentItem, LDestinationRoot, LCommandLine);
            CompressItem(LCurrentItem, LDestinationRoot, LCommandLine)
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
      LockingWriteLn(' Elapsed time: ' + LStopWatch.Elapsed.ToString, 1);
    end
    else
    begin
      LockingWriteLn('No files found from directory "' + FCompressorCommandLineOptions.SourceRoot, 3);

      Exit;
    end;
  finally
    LItemsToBeCompressed.Free;
  end;
end;

procedure TCustomCompressor7z.FilterByDirectories(const AItems: TStringList; const ARootDirectory: string);
var
  LIndex: Integer;
  LFileNameOnly: string;
  LDestinationDir: string;
begin
  if not Lock then
    Exit;

  try
    for LIndex := AItems.Count - 1 downto 0 do
    begin
      // TODO: Tämä ei ole abstaktoitu vielä...
      LFileNameOnly := GetFileNameOnly(AItems[LIndex]);
      LDestinationDir := ARootDirectory + LFileNameOnly;

      if not DirEmpty(LDestinationDir) then
      begin
        LockingWriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'), 3);
        AItems.Delete(LIndex);
      end;
    end;

    FTaskTotal := AItems.Count;
  finally
    UnLock;
  end;
end;

end.
