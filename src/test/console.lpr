program console;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner,
  testinstagram_fphttpclient, fphttpclientbroker
  ;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
