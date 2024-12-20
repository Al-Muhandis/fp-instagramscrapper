unit testinstagram_fphttpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpcunit, testregistry, testinstagram;

implementation

uses
  FileUtil, fphttpclientbroker;

initialization
  //TbFPHTTPClient.RegisterClientClass;
  RegisterTests([TTestInstagram, TTestAuthorize]);

end.

