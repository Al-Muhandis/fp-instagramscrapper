program AccountScrapping;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, InstagramScrapper, eventlog, SysUtils, fphttpclientbroker;

var
  Instagram: TInstagramParser;
  S: String;
  ALogger: TEventLog;

begin
  TbFPHTTPClient.RegisterClientClass; // You must register any http client class (now available only fphttpclient)
  ALogger:=TEventLog.Create(nil);
  ALogger.FileName:=ChangeFileExt(ApplicationName, '.log');
  ALogger.LogType:=ltFile;
  Instagram:=TInstagramParser.Create;
  Instagram.Logger:=ALogger;
  try
    try
      repeat
        WriteLn('Please input Instagram user alias (Username)');
        ReadLn(S);
        Instagram.ParseGetAccount(S);
        Writeln('Follows: '+IntToStr(Instagram.Follows)+', FolowedBy: '+IntToStr(Instagram.FollowedBy));
        Write('Are You want to continue? (Y/N): ');
        Readln(S);
      until lowercase(S)<>'y';
    finally
      Instagram.Free;
    end;
  except
    on E: Exception do
      ALogger.Log(etError, '[Unhandled exception] '+e.ClassName+': '+e.Message);
  end;
  ALogger.Free;
end.

