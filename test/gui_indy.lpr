program gui_indy;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testinstagram_indy, InstagramScrapper;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

