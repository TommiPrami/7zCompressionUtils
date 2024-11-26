unit CUUnit.Custom.Commandline;

interface

uses
  Delphi.CommandLineParser, CUUnit.Types, CUUnit.Utils;

type
  // TODO: Have to fix commandline parser that would work on inhherited command line settings
  // Like this the custom command line base class is basically futile.
  TCustomCompressLineOptions = class(TObject)
  strict private
  public
  end;

implementation


end.
