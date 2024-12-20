program gui_synapse;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, testinstagram_synapse, InstagramScrapper;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

