unit testinstagram_indy;

{$mode objFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, testinstagram;

implementation

uses
  FileUtil, indyhttpclientbroker;

initialization
  TIndyHTTPClient.RegisterClientClass;
  RegisterTests([TTestInstagram, TTestInstagramWithProxy]);

end.

