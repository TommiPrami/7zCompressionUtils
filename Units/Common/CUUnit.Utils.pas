﻿unit CUUnit.Utils;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Diagnostics, System.SysUtils, CUUnit.Types;

  function DirEmpty(const ADirectory: string): Boolean;
  function FileTimeToInt64(const FileTime: TFileTime): Int64;
  function GetAvailableMemoryPercentage: Integer;
  function GetMaxThreadCount: Integer;
  function ScaleCoreCount(const AWishfulCoreCount: Integer): Integer;
  function TotalCpuUsagePercentage: Double;
  procedure ProcessMessages;
  procedure WaitForSystemStatus(const APreWaitMillisecs: Integer; const AMaxTotalCpuUsagePercentage, AMaxAValilableMemoryPercentage: Double);
  function ExecuteAndWait(const ACommandLine: string; const APriorityClass: TFCPriorityClass = fcpcNormal;
    const AUsePerformanceCoresOnly: Boolean = True): Cardinal; overload;
  function ExecuteAndWait(const ACommandLine: string; var AErrorMessage: string; const APriorityClass: TFCPriorityClass = fcpcNormal;
    const AUsePerformanceCoresOnly: Boolean = True): Cardinal; overload;
  function GetFileNameOnly(const AFilename: string): string;
  function GetFileNameWithFilter(const ADirectory, AFileNameFilter: string): string;
  function GetLastDirectoryName(const ADirectory: string): string;
  function GetCompressionCommandlineOptions(const ACompressionLevel: TCompressionLevel; const AVolumeSizeInMb, ACores: Integer): string;
  function GetIntFormat(const AMaxItems: Integer): string;

implementation

uses
  System.IOUtils, System.Math, System.Types, Delphi.ProcessAffinity.Utils;

const
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;

var
  GLastIdleTime: Int64;
  GLastKernelTime: Int64;
  GLastUserTime: Int64;

function PriorityClassToNumeric(const APriorityClass: TFCPriorityClass): Cardinal;
begin
  Result := 0; // Shut up the compiler

  case APriorityClass of
    fcpcIdle: Result := IDLE_PRIORITY_CLASS;
    fcpcBelowNormal: Result := BELOW_NORMAL_PRIORITY_CLASS;
    fcpcNormal: Result := NORMAL_PRIORITY_CLASS;
    fcpcAboveNormal: Result := ABOVE_NORMAL_PRIORITY_CLASS;
    fcpcHigh: Result := HIGH_PRIORITY_CLASS;
    fcpcRealTime: Result := REALTIME_PRIORITY_CLASS;
  end;
end;

function DirEmpty(const ADirectory: string): Boolean;
var
  LFiles: TStringDynArray;
begin
  if not DirectoryExists(ADirectory) then
    Exit(True);

  LFiles := TDirectory.GetFiles(ADirectory, '*.*', TSearchOption.soTopDirectoryOnly);

  Result := Length(LFiles) = 0;
end;

function FileTimeToInt64(const FileTime: TFileTime): Int64;
begin
  Result := Int64(FileTime.dwHighDateTime) shl 32 or FileTime.dwLowDateTime;
end;

procedure SetGlobalTimes(const ALastIdleTime, ALastKernelTime, ALastUserTime: Int64);
begin
  GLastIdleTime := ALastIdleTime;
  GLastKernelTime := ALastKernelTime;
  GLastUserTime := ALastUserTime;
end;

function TotalCpuUsagePercentage: Double;
var
  LIdleTime, LKernelTime, LUserTime: TFileTime;
  LIdleDiff, LKernelDiff, LUserDiff, LTotalCpuTime: Int64;
begin
  if GetSystemTimes(LIdleTime, LKernelTime, LUserTime) then
  begin
    LIdleDiff := FileTimeToInt64(LIdleTime) - GLastIdleTime;
    LKernelDiff := FileTimeToInt64(LKernelTime) - GLastKernelTime;
    LUserDiff := FileTimeToInt64(LUserTime) - GLastUserTime;

    LTotalCpuTime := LKernelDiff + LUserDiff;

    SetGlobalTimes(FileTimeToInt64(LIdleTime), FileTimeToInt64(LKernelTime), FileTimeToInt64(LUserTime));

    if LTotalCpuTime > 0 then
      Result := 100.0 - ((LIdleDiff * 100.0) / LTotalCpuTime)
    else
      Result := 0.00;
  end
  else
    Result := 0.00;
end;

function GetMaxThreadCount: Integer;
begin
  Result := EnsureRange(CPUCount - 1, 1, CPUCount);
end;

function ScaleCoreCount(const AWishfulCoreCount: Integer): Integer;
begin
  Result := EnsureRange(AWishfulCoreCount, 1, CPUCount);
end;

procedure ProcessMessages;
var
  LMsg: TMsg;
begin
  while PeekMessage(LMsg, 0, 0, 0, PM_REMOVE) do
  begin
    if LMsg.Message = WM_QUIT then
      Exit;

    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

function GetAvailableMemoryPercentage: Integer;
var
  LMemoryStatus: TMemoryStatus;
begin
  LMemoryStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(LMemoryStatus);

  Result := LMemoryStatus.dwMemoryLoad;
end;

procedure WaitForSystemStatus(const APreWaitMillisecs: Integer; const AMaxTotalCpuUsagePercentage, AMaxAValilableMemoryPercentage: Double);
const
  SLEEP_TIME = 100;
var
  LStopWatch: TStopwatch;
begin
  LStopWatch := TStopwatch.StartNew;

  while LStopWatch.Elapsed.TotalMilliseconds <= APreWaitMillisecs do
    Sleep(SLEEP_TIME);

 { TODO: This potentiaally can hang the whole app, so not too smart :) But let's see does it help or not
         in GUI app wold nice to have some global Shit-down flag, but in command line app it might we
         futile, because no way to terminate anyhow... Maybe, have to see... I think there could be way to do it.
         Like old DOS-apps had ctrl+c/ctrl+x termination }
 while ((TotalCpuUsagePercentage > AMaxTotalCpuUsagePercentage)
    or (GetAvailableMemoryPercentage > AMaxAValilableMemoryPercentage)) do
  begin
    Sleep(SLEEP_TIME);
  end;
end;

function ExecuteAndWait(const ACommandLine: string; var AErrorMessage: string;
  const APriorityClass: TFCPriorityClass = fcpcNormal; const AUsePerformanceCoresOnly: Boolean = True): Cardinal;
var
  LStartupInfo: TStartupInfo;
  LProcessInformation: TProcessInformation;
  LCommandLine: string;
  LExitCode: DWORD;
  LCreationFlags: DWORD;
begin
  AErrorMessage := '';
  Result := 0;

  LCommandLine := Trim(ACommandLine);

  FillChar(LStartupInfo, SizeOf(LStartupInfo), 0);

  LStartupInfo.cb := SizeOf(TStartupInfo);
  LStartupInfo.wShowWindow := SW_SHOW;

  LCreationFlags := PriorityClassToNumeric(APriorityClass) or CREATE_NEW_CONSOLE;

  if CreateProcess(nil, PChar(LCommandLine), nil, nil, True, LCreationFlags, nil, nil, LStartupInfo, LProcessInformation) then
  try
    if AUsePerformanceCoresOnly then
      if not SetProcessAffinityMask(LProcessInformation.hProcess, GetPerformanceAffinityMask(LProcessInformation.hProcess)) then
        AErrorMessage := 'Could  not set process affinity mask';

    repeat
      Sleep(10);

      ProcessMessages;

      GetExitCodeProcess(LProcessInformation.hProcess, LExitCode);
    until LExitCode <> STILL_ACTIVE;
  finally
    CloseHandle(LProcessInformation.hProcess);
    CloseHandle(LProcessInformation.hThread);
  end
  else
    RaiseLastOSError;
end;

function ExecuteAndWait(const ACommandLine: string; const APriorityClass: TFCPriorityClass = fcpcNormal;
  const AUsePerformanceCoresOnly: Boolean = True): Cardinal;
begin
  var LErrorMessage: string;

  Result := ExecuteAndWait(ACommandLine, LErrorMessage, APriorityClass, AUsePerformanceCoresOnly);
end;

function GetFileNameOnly(const AFilename: string): string;
var
  LExtension: string;
begin
  Result := ExtractFileName(AFilename);
  LExtension := ExtractFileExt(Result);

  if not LExtension.IsEmpty then
    Result := Copy(Result, 1, Result.Length - LExtension.Length);
end;

function GetFileNameWithFilter(const ADirectory, AFileNameFilter: string): string;
begin
  Result := '';

  var LFiles := TDirectory.GetFiles(IncludeTrailingPathDelimiter(ADirectory), AFileNameFilter, TSearchOption.soTopDirectoryOnly);

  for var LFileName in LFiles do
  begin
    if not LFileName.IsEmpty then
      Exit(LFileName);
  end;
end;

function GetLastDirectoryName(const ADirectory: string): string;
begin
  Result := '';

  var LDirectoryArray := ADirectory.Split(['\', '/']);

  for var LIndex := High(LDirectoryArray) downto Low(LDirectoryArray) do
  begin
    var LCurrentDirectoryPart := LDirectoryArray[LIndex];

    if not LCurrentDirectoryPart.IsEmpty then
      Exit(LCurrentDirectoryPart);
  end;
end;

function GetCompressionCommandlineOptions(const ACompressionLevel: TCompressionLevel; const AVolumeSizeInMb, ACores: Integer): string;
begin
  case ACompressionLevel of
    Store: Result := '-mx0';
    Fastest: Result := '-mx1';
    Fast: Result := '-mx3';
    Normal: Result := '-mx5';
    Maximum: Result := '-mx7';
    Ultra: Result := '-mx9';
    UltraDeluxe: Result := '-mx9 -md768m -mfb128';
    UltraCreamDeluxe: Result := '-mx9 -md1024m -mfb128';
  end;

  if ACores <= 1 then
    Result := Result  + ' -mmt=off'
  else
    Result := Result  + ' -mmt' + ACores.ToString;

  if AVolumeSizeInMb >= 1 then
    Result := Result  + ' -v' + AVolumeSizeInMb.ToString + 'm';

  Result := ' ' + Result + ' ';
end;

function GetDigitCount(const ANumber: Integer): Integer;

  procedure CheckParameter(const AParameter: Integer);
  begin
    if AParameter <= 0 then
      raise Exception.Create('Number must be greater than 0'); // Handle edge cases
  end;

begin
  CheckParameter(ANumber);

  Result := Trunc(Log10(ANumber)) + 1;
end;

function GetIntFormat(const AMaxItems: Integer): string;
begin
  Result := '%.' + GetDigitCount(AMaxItems).ToString + 'd';
end;

initialization

  SetGlobalTimes(0, 0, 0);

end.
