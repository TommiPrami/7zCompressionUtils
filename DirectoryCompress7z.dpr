program DirectoryCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Types,
  CUUnit.Consts in 'Units\Common\CUUnit.Consts.pas',
  CUUnit.Custom.Commandline in 'Units\Common\CUUnit.Custom.Commandline.pas',
  CUUnit.Custom.Compressor in 'Units\Common\CUUnit.Custom.Compressor.pas',
  CUUnit.Types in 'Units\Common\CUUnit.Types.pas',
  CUUnit.Utils in 'Units\Common\CUUnit.Utils.pas',
  DCUnit.CommandLine in 'Units\DirectoryCompress\DCUnit.CommandLine.pas',
  DCUnit.DirectoryCompressor in 'Units\DirectoryCompress\DCUnit.DirectoryCompressor.pas',
  Delphi.ProcessAffinity.Utils in 'Units\Common\Delphi.ProcessAffinity.Utils.pas';

// main program body
var
  LDirectoryCompress: TDirectoryCompressor;
begin
  var LCommandLineOptions := TDirectoryCompressLineOptions.Create;
  try
    if not ParseCommandLine(LCommandLineOptions) then
    begin
      ExitCode := EXIT_CODE_ERROR_IN_COMMANDLINE_PARAMS;

      Exit;
    end;

    LDirectoryCompress := TDirectoryCompressor.Create(LCommandLineOptions);
    try
      LDirectoryCompress.Execute;

      {$IFDEF DEBUG}
      LDirectoryCompress.LockingWriteLn('');
      LDirectoryCompress.LockingWriteLn('Press [Enter] to continue');

      ReadLn;
      {$ENDIF}
    finally
      LDirectoryCompress.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
