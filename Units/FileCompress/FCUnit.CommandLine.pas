unit FCUnit.CommandLine;

interface

uses
  CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils, Delphi.CommandLineParser;

type
  TFileCompressOptions = class(TCustomCompressLineOptions)
  strict private
    FSourceRoot: string;
    FFileNameFilter: string;
    FCompressionLevel: TCompressionLevel;
    FCoresToUse: Integer;
    FVolumeSize: Integer;
    FDeleteSourceItemWhenDone: Boolean;
    FThrottleBySystemResources: Boolean;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('FileNameFilter'), CLPDescription('Filter (like *.vbox) to take name for result', '<wild card>'), CLPDefault(''), CLPRequired]
    property FileNameFilter: string read FFileNameFilter write FFileNameFilter;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra, UltraDeluxe, UltraCreamDeluxe>'), CLPDefault('UltraDeluxe')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;

    [CLPLongName('CoresToUse'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('1')]
    property CoresToUse: Integer read FCoresToUse write FCoresToUse;

    [CLPLongName('VolumeSize'), CLPDescription('Compression VolumeSize size in megobytes', '<Integer>'), CLPDefault('4096')]
    property VolumeSize: Integer read FVolumeSize write FVolumeSize;

    [CLPLongName('DeleteSourceItemWhenDone'), CLPDescription('Delete source file when done', '<Boolean>'), CLPDefault('False')]
    property DeleteSourceItemWhenDone: Boolean read FDeleteSourceItemWhenDone write FDeleteSourceItemWhenDone;

    [CLPLongName('ThrottleBySystemResources'), CLPDescription('Throttle by System resources', '<Boolean>'), CLPDefault('False')]
    property ThrottleBySystemResources: Boolean read FThrottleBySystemResources write FThrottleBySystemResources;
  end;

  function ParseCommandLine(const ACommandlineOptions: TFileCompressOptions): Boolean;

implementation

function ParseCommandLine(const ACommandlineOptions: TFileCompressOptions): Boolean;
var
  LParser: ICommandLineParser;
begin
  LParser := CreateCommandLineParser;

  Result := LParser.Parse(ACommandlineOptions);

  if not Result then
    DefaultUsageConsoleOutput(LParser);
end;

end.
