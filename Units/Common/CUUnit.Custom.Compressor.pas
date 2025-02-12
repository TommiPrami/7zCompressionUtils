unit CUUnit.Custom.Compressor;

interface

uses
  System.Classes, System.SyncObjs, CUUnit.Custom.Commandline, OtlCollections, OtlCommon, OtlParallel, OtlTask;

type
  TCustomCompressor7z = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
    FTaksStarted: Int64;
    FTaskTotal: Integer;
    FRunningTasks: Boolean;
    function CompressItem(const ACurrentItem, ADestinationRoot, ACommandLine: string): Boolean;
  strict protected
    FCompressorCommandLineOptions: TCustomCompressLineOptions;
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure FilterByDirectories(const AItems: TStringList);
    procedure PrepareItemForCompression(const ACurrentItemName: string; var ADestinationRoot, ACommandLine: string); virtual; abstract;
    procedure AfterItemCompressed(const AFileName: string); virtual; abstract;
    procedure WaitForTasks;
    function GetItemsToBeCompressed: TArray<string>; virtual; abstract;
    function GetDestinationItemName(const ACurrentItemName: string): string; virtual; abstract;
    function GetDestinationDirectory(const ACurrentDestinationItemName: string): string; virtual; abstract;
    function GetSourceRoot: string; virtual; abstract;
    function GetCoreCount: Integer; virtual; abstract;
    function ThrottleBySystemResources: Boolean; virtual; abstract;
  public
    constructor Create(const ACompressorCommandLineOptions: TCustomCompressLineOptions);
    destructor Destroy; override;

    procedure Execute;
    procedure LockingWriteLn(const ALine: string; const AIndent: Integer = 0);
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

procedure TCustomCompressor7z.WaitForTasks;
var
  LTasksRunning: Boolean;
begin
  LTasksRunning := True;

  while LTasksRunning do
  begin
    Sleep(200);
    ProcessMessages;

    if Lock then
    try
      LTasksRunning := FRunningTasks;
    finally
      Unlock;
    end
    else
      LTasksRunning := False;
  end;
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

function TCustomCompressor7z.CompressItem(const ACurrentItem, ADestinationRoot, ACommandLine: string): Boolean;

  function GetItemOfMaxStr(const AItemIndex: Integer): string;
  begin
    var LIntFormat := GetIntFormat(FTaskTotal);

    Result := Format(LIntFormat, [AItemIndex]) + '/' + Format(LIntFormat, [FTaskTotal])
  end;

var
  LTasksStarted: Integer;
begin
  Result := False;

  if not Lock then
    Exit;

  try
    Inc(FTaksStarted);
    LTasksStarted := FTaksStarted;
  finally
    Unlock;
  end;

  if ThrottleBySystemResources then
    WaitForSystemStatus(IfThen(LTasksStarted <= 1, 333, 10 * 666), 76.66, 76.66);

  if not DirectoryExists(ADestinationRoot) then
    ForceDirectories(ADestinationRoot);

  LockingWriteLn(GetItemOfMaxStr(LTasksStarted) + ' Executing: ' + ACurrentItem + '...', 2);

  try
    Result := ExecuteAndWait(ACommandLine, fcpcIdle, True) = 0;
  except
    on E: Exception do
    begin
      Result := False;

      LockingWriteLn('Exception ' + E.ClassName + ' occurred, while compressing item ' + ACurrentItem.QuotedString('"')
        + ' with message ' + E.Message.QuotedString('"'));
    end;
  end;
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

    FilterByDirectories(LItemsToBeCompressed);

    if LItemsToBeCompressed.Count > 0 then
    begin
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LItemsToBeCompressed)
        .NumTasks(GetCoreCount)
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

            PrepareItemForCompression(LCurrentItem, LDestinationRoot, LCommandLine);

            if not LCommandLine.IsEmpty and not LDestinationRoot.IsEmpty  then
              if CompressItem(LCurrentItem, LDestinationRoot, LCommandLine) then
              begin
                if Lock then
                try
                  AfterItemCompressed(LCurrentItem);
                finally
                  Unlock;
                end;
              end;
          end
        );

      WaitForTasks;

      LStopWatch.Stop;
      LockingWriteLn(' Elapsed time: ' + LStopWatch.Elapsed.ToString, 1);
    end
    else
    begin
      LockingWriteLn('Nothing to compress was found from directory ' + GetSourceRoot.QuotedString('"') , 3);

      Exit;
    end;
  finally
    LItemsToBeCompressed.Free;
  end;
end;

procedure TCustomCompressor7z.FilterByDirectories(const AItems: TStringList);
var
  LIndex: Integer;
  LDestinationItemName: string;
  LDestinationDir: string;
begin
  if not Lock then
    Exit;

  try
    for LIndex := AItems.Count - 1 downto 0 do
    begin
      LDestinationItemName := GetDestinationItemName(AItems[LIndex]);
      LDestinationDir := GetDestinationDirectory(LDestinationItemName);

      if not DirEmpty(LDestinationDir) then
      begin
        LockingWriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'), 3);
        AItems.Delete(LIndex);
      end;
    end;

    FTaskTotal := AItems.Count;
  finally
    Unlock;
  end;
end;

end.
