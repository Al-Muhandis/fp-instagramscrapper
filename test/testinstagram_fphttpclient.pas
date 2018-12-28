unit testinstagram_fphttpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, testinstagram;

type

  { TTestFPClientInstagram }

  TTestFPClientInstagram= class(TTestInstagram)
  protected
    procedure SetUp; override;
  end;

  { TTestFPClientAuthoruze }

  TTestFPClientAuthoruze= class(TTestAuthorise)
  protected
    procedure SetUp; override;
  end;

implementation

uses
  FileUtil, fphttpclientbroker;

{ TTestFPClientAuthoruze }

procedure TTestFPClientAuthoruze.SetUp;
begin
  TbFPHTTPClient.UnregisterClientClass;
  TbFPHTTPClient.RegisterClientClass; // Register FCL (native) HTTP client
  inherited SetUp;
end;

{ TTestFPClientInstagram }

procedure TTestFPClientInstagram.SetUp;
begin
  TbFPHTTPClient.UnregisterClientClass;
  TbFPHTTPClient.RegisterClientClass; // Register FCL (native) HTTP client
  inherited SetUp;
end;

initialization
  RegisterTests([TTestFPClientInstagram, TTestFPClientAuthoruze]);

end.

