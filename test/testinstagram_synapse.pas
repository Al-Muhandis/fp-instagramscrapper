unit testinstagram_synapse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, testinstagram;

type

  { TTestSynapseInstagram }

  TTestSynapseInstagram= class(TTestInstagram)
  protected
    procedure SetUp; override;
  end;

  { TTestSynapseWithProxy }

  TTestSynapseWithProxy= class(TTestInstagramWithProxy)
  protected
    procedure SetUp; override;
  end;

implementation

uses
  FileUtil, synapsehttpclientbroker;

{ TTestSynapseWithProxy }

procedure TTestSynapseWithProxy.SetUp;
begin
  TSynapseHTTPClient.UnregisterClientClass;
  TSynapseHTTPClient.RegisterClientClass; // Register synapse HTTP client
  inherited SetUp;
end;

{ TTestSynapseInstagram }

procedure TTestSynapseInstagram.SetUp;
begin
  TSynapseHTTPClient.UnregisterClientClass;
  TSynapseHTTPClient.RegisterClientClass; // Register synapse HTTP client
  inherited SetUp;
end;

initialization
  RegisterTests([TTestSynapseInstagram, TTestSynapseWithProxy]);

end.

