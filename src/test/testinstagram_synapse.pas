unit testinstagram_synapse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testinstagram;

implementation

uses
  FileUtil, synapsehttpclientbroker;

initialization
  //TSynapseHTTPClient.RegisterClientClass;
  RegisterTests([TTestInstagram, TTestInstagramWithProxy, TTestAuthorize]);

end.

