unit DCUnit.CommandLine;

interface

uses
  Delphi.CommandLineParser,CUUnit.Types, CUUnit.Custom.Commandline, CUUnit.Utils;

type
  TDirectoryCompressLineOptions = class(TCustomCompressLineOptions)
  strict private
    FDestinationRoot: string;
  public
    [CLPLongName('DestinationRoot'), CLPDescription('Destination Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
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
