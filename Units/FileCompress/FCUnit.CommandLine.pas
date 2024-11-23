unit FCUnit.CommandLine;

interface

uses
  Delphi.CommandLineParser, CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils;

type
  TFileCompressLineOptions = class(TCustomCompressLineOptions)
  strict private
  public
  end;

  function ParseCommandLine(const ACommandlineOptions: TFileCompressLineOptions): Boolean;

implementation

function ParseCommandLine(const ACommandlineOptions: TFileCompressLineOptions): Boolean;
var
  LParser: ICommandLineParser;
begin
  LParser := CreateCommandLineParser;

  Result := LParser.Parse(ACommandlineOptions);

  if not Result then
    DefaultUsageConsoleOutput(LParser);
end;

end.
