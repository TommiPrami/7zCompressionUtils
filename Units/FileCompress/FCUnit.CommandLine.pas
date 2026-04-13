unit FCUnit.CommandLine;

interface

uses
  CUUnit.Custom.Commandline, CUUnit.Types, CUUnit.Utils, Delphi.CommandLineParser;

type
  TFileCompressOptions = class(TCustomCompressLineOptions)
  strict private
    FDeleteSourceItemWhenDone: Boolean;
  public
    [CLPLongName('DeleteSourceItemWhenDone'), CLPDescription('Delete source file when done', '<Boolean>'), CLPDefault('False')]
    property DeleteSourceItemWhenDone: Boolean read FDeleteSourceItemWhenDone write FDeleteSourceItemWhenDone;
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
