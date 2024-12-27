unit FCUnit.CommandLine;

interface

uses
  Delphi.CommandLineParser, CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils;

type
  TFileCompressLineOptions = class(TCustomCompressLineOptions)
  strict private
    FSourceRoot: string;
    FFileNameFilter: string;
    FCompressionLevel: TCompressionLevel;
    FCoresToUse: Integer;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directory', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('FileNameFilter'), CLPDescription('Filter (like *.vbox) to take name for result', '<wild card>'), CLPDefault(''), CLPRequired]
    property FileNameFilter: string read FFileNameFilter write FFileNameFilter;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra>'), CLPDefault('Ultra')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;

    [CLPLongName('CoresToUse'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('4')]
    property CoresToUse: Integer read FCoresToUse write FCoresToUse;
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
