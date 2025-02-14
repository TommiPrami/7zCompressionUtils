unit DCUnit.CommandLine;

interface

uses
  CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils, Delphi.CommandLineParser;

type
  TDirectoryCompressLineOptions = class(TCustomCompressLineOptions)
  strict private
    FSourceRoot: string;
    FFileNameFilter: string;
    FCompressionLevel: TCompressionLevel;
    FCoresToUse: Integer;
    FDestinationRoot: string;
    FThrottleBySystemResources: Boolean;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('FileNameFilter'), CLPDescription('Filter (like *.vbox) to take name for result', '<wild card>'), CLPDefault(''), CLPRequired]
    property FileNameFilter: string read FFileNameFilter write FFileNameFilter;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra, UltraCreamDeluxe, UltraCreamDeluxe2>'), CLPDefault('UltraCreamDeluxe2')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;

    [CLPLongName('CoresToUse'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('4')]
    property CoresToUse: Integer read FCoresToUse write FCoresToUse;

    [CLPLongName('DestinationRoot'), CLPDescription('Destination Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property DestinationRoot: string read FDestinationRoot write FDestinationRoot;

    [CLPLongName('ThrottleBySystemResources'), CLPDescription('Throttle by System resources', '<Boolean>'), CLPDefault('False')]
    property ThrottleBySystemResources: Boolean read FThrottleBySystemResources write FThrottleBySystemResources;
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
