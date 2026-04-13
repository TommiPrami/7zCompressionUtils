unit CUUnit.Custom.Commandline;

interface

uses
  Delphi.CommandLineParser, CUUnit.Types, CUUnit.Utils;

type
  // TODO: Have to fix commandline parser that would work on inhherited command line settings
  // Like this the custom command line base class is basically futile.
  TCustomCompressLineOptions = class(TObject)
  strict private
    FCompressionLevel: TCompressionLevel;
    FCoresToUse: Integer;
    FDeleteFilesFromDestination: Boolean;
    FFileNameFilter: string;
    FParallelCompressorCount: Integer;
    FSourceRoot: string;
    FThrottleBySystemResources: Boolean;
    FVolumeSize: Integer;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('FileNameFilter'), CLPDescription('Filter (like *.vbox) to take name for result', '<wild card>'), CLPDefault(''), CLPRequired]
    property FileNameFilter: string read FFileNameFilter write FFileNameFilter;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra, UltraDeluxe, UltraCreamDeluxe>'), CLPDefault('UltraDeluxe')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;

    [CLPLongName('CoresToUse'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('1')]
    property CoresToUse: Integer read FCoresToUse write FCoresToUse;

    [CLPLongName('ParallelCompressorCount'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('1')]
    property ParallelCompressorCount: Integer read FParallelCompressorCount write FParallelCompressorCount;

    [CLPLongName('VolumeSize'), CLPDescription('Compression VolumeSize size in megobytes', '<Integer>'), CLPDefault('4096')]
    property VolumeSize: Integer read FVolumeSize write FVolumeSize;

    [CLPLongName('ThrottleBySystemResources'), CLPDescription('Throttle by System resources', '<Boolean>'), CLPDefault('False')]
    property ThrottleBySystemResources: Boolean read FThrottleBySystemResources write FThrottleBySystemResources;

    [CLPLongName('DeleteFilesFromDestination'), CLPDescription('If destination folder has files, can those be deleted.', '<Boolean>'), CLPDefault('False')]
    property DeleteFilesFromDestination: Boolean read FDeleteFilesFromDestination write FDeleteFilesFromDestination;
  end;

implementation


end.
