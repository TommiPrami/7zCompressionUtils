unit CUUnit.Custom.Commandline;

interface

uses
  Delphi.CommandLineParser,CUUnit.Types, CUUnit.Utils;

type
  TCustomCompressLineOptions = class(TObject)
  strict private
    FSourceRoot: string;
    FFileNameFilter: string;
    FCompressionLevel: TCompressionLevel;
    FCoresToUse: Integer;
  public
    [CLPLongName('SourceRoot'), CLPDescription('Source Root directry', '<directory>'), CLPDefault(''), CLPRequired, CLPDirectoryMustExist]
    property SourceRoot: string read FSourceRoot write FSourceRoot;

    [CLPLongName('FileNameFilter'), CLPDescription('Filter (like *.vbox) to take name for result', '<wild card>'), CLPDefault(''), CLPRequired]
    property FileNameFilter: string read FFileNameFilter write FFileNameFilter;

    [CLPLongName('CompressionLevel'), CLPDescription('Compression Level', '<Store, Fastest, Fast, Normal, Maximum, Ultra>'), CLPDefault('Ultra')]
    property CompressionLevel: TCompressionLevel read FCompressionLevel write FCompressionLevel;

    [CLPLongName('CoresToUse'), CLPDescription('In how many processes is used, scaled to 1..max cores', '<Integer>'), CLPDefault('4')]
    property CoresToUse: Integer read FCoresToUse write FCoresToUse;
  end;

implementation


end.
