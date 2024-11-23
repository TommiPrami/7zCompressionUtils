program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  CUUnit.Consts in 'Units\Common\CUUnit.Consts.pas',
  CUUnit.Types in 'Units\Common\CUUnit.Types.pas',
  CUUnit.Utils in 'Units\Common\CUUnit.Utils.pas',
  FCUnit.CommandLine in 'Units\FileCompress\FCUnit.CommandLine.pas',
  FCUnit.FileCompressor in 'Units\FileCompress\FCUnit.FileCompressor.pas';

var
  LFileCompress: TFileCompress7z;
begin
  var LCommandLineOptions := TFileCompressLineOptions.Create;
  try
    if not ParseCommandLine(LCommandLineOptions) then
    begin
      ExitCode := 1;
      Exit;
    end;

    LFileCompress := TFileCompress7z.Create(LCommandLineOptions);
    try
      LFileCompress.Execute;
    finally
      LFileCompress.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
