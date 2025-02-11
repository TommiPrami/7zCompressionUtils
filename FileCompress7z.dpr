program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  CUUnit.Consts in 'Units\Common\CUUnit.Consts.pas',
  CUUnit.Types in 'Units\Common\CUUnit.Types.pas',
  CUUnit.Utils in 'Units\Common\CUUnit.Utils.pas',
  CUUnit.Custom.Commandline in 'Units\Common\CUUnit.Custom.Commandline.pas',
  CUUnit.Custom.Compressor in 'Units\Common\CUUnit.Custom.Compressor.pas',
  FCUnit.CommandLine in 'Units\FileCompress\FCUnit.CommandLine.pas',
  FCUnit.FileCompressor in 'Units\FileCompress\FCUnit.FileCompressor.pas',
  Delphi.ProcessAffinity.Utils in 'Units\Common\Delphi.ProcessAffinity.Utils.pas';

var
  LFileCompress: TFileCompress7z;
begin
  var LCommandLineOptions := TFileCompressLineOptions.Create;
  try
    if not ParseCommandLine(LCommandLineOptions) then
    begin
      ExitCode := EXIT_CODE_ERROR_IN_COMMANDLINE_PARAMS;

      Exit;
    end;

    LFileCompress := TFileCompress7z.Create(LCommandLineOptions);
    try
      LFileCompress.Execute;

      {$IFDEF DEBUG}
      LFileCompress.LockingWriteLn('');
      LFileCompress.LockingWriteLn('Press [Enter] to continue');

      ReadLn;
      {$ENDIF}
    finally
      LFileCompress.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
