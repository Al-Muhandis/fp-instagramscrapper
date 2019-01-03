program console_indy;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, consoletestrunner,
  testinstagram_indy, indyhttpclientbroker, indylaz
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
