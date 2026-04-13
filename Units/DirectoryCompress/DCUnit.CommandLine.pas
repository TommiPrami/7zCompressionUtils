unit DCUnit.CommandLine;

interface

uses
  CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils, Delphi.CommandLineParser;

type
  TDirectoryCompressLineOptions = class(TCustomCompressLineOptions)
  strict private
    FDestinationRoot: string;
  public
    [CLPLongName('DestinationRoot'), CLPDescription('Destination Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property DestinationRoot: string read FDestinationRoot write FDestinationRoot;
  end;

  function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;

implementation

uses
  DCUnit.DirectoryCompressor;

function ParseCommandLine(const ACommandlineOptions: TDirectoryCompressLineOptions): Boolean;
var
  LParser: ICommandLineParser;
begin
  LParser := CreateCommandLineParser;

  Result := LParser.Parse(ACommandlineOptions);

  if not Result then
    DefaultUsageConsoleOutput(LParser);
end;


end.
