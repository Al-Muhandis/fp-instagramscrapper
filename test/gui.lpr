program gui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testinstagram;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

