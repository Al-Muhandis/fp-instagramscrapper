unit testinstagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, InstagramScrapper, fpjson, IniFiles;

type

  { TTestInstagramBase }

  TTestInstagramBase= class(TTestCase)
  private
    FConf: TMemIniFile;
    FInstagramParser: TInstagramParser;
    FTargetMediaShortCode: String;
    FTargetUserName: String;
    procedure SaveJSONObject(AData: TJSONData; const AFileName: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure AccountProperties;
    procedure MediaProperties;
    property TargetUsername: String read FTargetUserName;
    property TargetMedia: String read FTargetMediaShortCode;
  end;

  { TTestInstagram }

  TTestInstagram=class(TTestInstagramBase)
  published
    procedure TestGetParseJSONAccount;
    procedure TestGetParseJSONMedia;
    procedure TestGetParseComments;
    procedure TestGetParseMultiple;
  end;

  { TTestInstagramWithProxy }
  { It is not working when using fphttpclient  }
  TTestInstagramWithProxy=class(TTestInstagram)
  protected
    procedure SetUp; override;
  end;

  { TTestAuthorize }

  TTestAuthorize = class(TTestInstagramBase)
  protected
    procedure SetUp; override;
  published
    procedure Authorise;
    procedure TestGetStories;
  end;

implementation

uses
  FileUtil, eventlog, URIParser;

const
  s_SampleAccount='natgeo';
  s_SampleMedia='BqRpCX2gfsq';
  s_NotParsed='Not parsed';
  s_NilJSON='JSON data is nil!';
  s_ConfTarget='Target';
  s_Media='Media';
  s_Username='Username';
  s_Session='Session';
  s_Password='Password';
  s_Proxy='Proxy';
  s_Host='Host';
  s_Port='Port';
  s_Uri='Uri';

{ TTestInstagramWithProxy }

procedure TTestInstagramWithProxy.SetUp;
var
  AHost, AUsername, APassword: String;
  APort: Word;
  URI: TURI;
begin
  inherited SetUp;
  AHost:=    FConf.ReadString(s_Proxy,  s_Host,     EmptyStr);
  AUsername:=FConf.ReadString(s_Proxy,  s_Username, EmptyStr);
  APassword:=FConf.ReadString(s_Proxy,  s_Password, EmptyStr);
  APort:=    FConf.ReadInteger(s_Proxy, s_Port,     0);
  if AHost=EmptyStr then
  begin
    URI:=URIParser.ParseURI('https://'+FConf.ReadString(s_Proxy, s_Uri, EmptyStr));
    AHost:=URI.Host;
    APort:=URI.Port;
    AUsername:=URI.Username;
    APassword:=URI.Password;
  end;
  FInstagramParser.HTTPClient.HTTPProxyHost:=AHost;
  FInstagramParser.HTTPClient.HTTPProxyPort:=APort;
  FInstagramParser.HTTPClient.HTTPProxyUsername:=AUsername;
  FInstagramParser.HTTPClient.HTTPProxyPassword:=APassword;
end;

{ TTestInstagram }

procedure TTestInstagram.TestGetParseJSONAccount;
begin
  AssertTrue(s_NotParsed, FInstagramParser.ParseGetAccount(TargetUsername));
  SaveJSONObject(FInstagramParser.jsonUser, '~account.json');
  AccountProperties;
end;

procedure TTestInstagram.TestGetParseJSONMedia;
begin
  FInstagramParser.ParseComments:=True;
  AssertTrue(s_NotParsed, FInstagramParser.ParseGetPost(TargetMedia));
  SaveJSONObject(FInstagramParser.jsonPost, '~media.json');
  MediaProperties;
end;

procedure TTestInstagram.TestGetParseComments;
begin
  FInstagramParser.Url:=FInstagramParser.UrlFromShortcode(TargetMedia);
  FInstagramParser.GetCommentsFromUrl;  // We must get gisToken and End_Cursor for futher requests
  Sleep(1000);
  FInstagramParser.Shortcode:=TargetMedia;
  while FInstagramParser.CommentHasPrev do
  begin
    if not FInstagramParser.getMediaCommentsByCodeHash(FInstagramParser.EndCursor) then
      Fail('Failed to retrieve the data');
    Sleep(3000);
  end;
  SaveJSONObject(FInstagramParser.CommentList, '~comments.json');
end;

procedure TTestInstagram.TestGetParseMultiple;
begin
  TestGetParseJSONAccount;
  Sleep(300);
  TestGetParseJSONMedia;
end;

{ TTestAuthorize }

procedure TTestAuthorize.SetUp;
var
  AFileName: String;
begin
  inherited SetUp;
  FInstagramParser.SessionUserName:=FConf.ReadString(s_Session, s_Username, '');
  FInstagramParser.SessionPassword:=FConf.ReadString(s_Session, s_Password, '');
  AssertTrue('Username or password not specified! See readme.md',
    (FInstagramParser.SessionUserName<>EmptyStr) and (FInstagramParser.SessionPassword<>EmptyStr));
  AFileName:='~cookies_'+FInstagramParser.SessionUserName+'.txt';
  FInstagramParser.HTTPClient.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0';
  if FileExists(AFileName) then
    FInstagramParser.UserSession.LoadFromFile(AFileName); // No need authorise every test...
  FInstagramParser._Login();
  FInstagramParser.UserSession.SaveToFile(AFileName);
  AssertTrue('Login is not succesful!', FInstagramParser.Logged);
  Sleep(1000); // to avoid ban from Instagram
end;

procedure TTestAuthorize.Authorise;
begin
  // Just only authorise
end;

procedure TTestAuthorize.TestGetStories;
var
  jsonStories: TJSONArray;
begin
  FInstagramParser.ParseGetAccount(TargetUsername);
  jsonStories:=FInstagramParser._getStoriesForUser(FInstagramParser.UserID);
  if Assigned(jsonStories) then
  begin
    SaveJSONObject(jsonStories, '~Stories.json');
    jsonStories.Free;
  end
  else
    Fail('json stories array is nil!');
end;

procedure TTestInstagramBase.AccountProperties;
var
  AProperties: TStringList;
  i: Integer;
begin
  AProperties:=TStringList.Create;
  try
    AProperties.Values['biography']:=FInstagramParser.Biography;
    AProperties.Values['HomePage']:=FInstagramParser.HomePage;
    AProperties.Values['followed_by']:=IntToStr(FInstagramParser.FollowedBy);
    AProperties.Values['follows']:=IntToStr(FInstagramParser.Follows);
    AProperties.Values['FullName']:=FInstagramParser.FullName;
    AProperties.Values['Username']:=FInstagramParser.Username;
    AProperties.Values['UserID']:=IntToStr(FInstagramParser.UserID);
    AProperties.Values['ProfilePic']:=IntToStr(FInstagramParser.UserID);
    for i:=0 to FInstagramParser.Images.Count-1 do
      AProperties.Values['Image'+IntToStr(i)]:=FInstagramParser.Images[i];
    for i:=0 to FInstagramParser.Videos.Count-1 do
      AProperties.Values['Video'+IntToStr(i)]:=FInstagramParser.Videos[i];
    AProperties.SaveToFile('~AccountProperties.txt');
    AssertTrue('Empty account properties', FInstagramParser.Username<>EmptyStr);
  finally
    AProperties.Free;
  end;
end;

procedure TTestInstagramBase.MediaProperties;
var
  AProperties: TStringList;
  i: Integer;
begin
  AProperties:=TStringList.Create;
  try
    AProperties.Values['text']:=FInstagramParser.PostCaption;
    AProperties.Values['CommentCount']:=IntToStr(FInstagramParser.CommentCount);
    AProperties.Values['Likes']:=IntToStr(FInstagramParser.Likes);

// properties of owner of the media post
    AProperties.Values['biography']:=FInstagramParser.Biography;
    AProperties.Values['HomePage']:=FInstagramParser.HomePage;
    AProperties.Values['followed_by']:=IntToStr(FInstagramParser.FollowedBy);
    AProperties.Values['follows']:=IntToStr(FInstagramParser.Follows);
    AProperties.Values['FullName']:=FInstagramParser.FullName;
    AProperties.Values['Username']:=FInstagramParser.Username;
    AProperties.Values['UserID']:=IntToStr(FInstagramParser.UserID);
    AProperties.Values['ProfilePic']:=IntToStr(FInstagramParser.UserID);


    for i:=0 to FInstagramParser.Images.Count-1 do
      AProperties.Values['Image'+IntToStr(i)]:=FInstagramParser.Images[i];
    for i:=0 to FInstagramParser.Videos.Count-1 do
      AProperties.Values['Video'+IntToStr(i)]:=FInstagramParser.Videos[i];
    AProperties.SaveToFile('~MediaProperties.txt');
    CheckNotEquals(FInstagramParser.Images.Count+FInstagramParser.Videos.Count, 0,
      'Empty media content');
    SaveJSONObject(FInstagramParser.CommentList, '~comments.json');
  finally
    AProperties.Free;
  end;
end;

procedure TTestInstagramBase.SaveJSONObject(AData: TJSONData; const AFileName: String);
var
  AStrings: TStringList;
begin
  if not Assigned(AData) then
  begin
    Fail(s_NilJSON);
    Exit;
  end;
  AStrings:=TStringList.Create;
  try
    AStrings.Text:=AData.FormatJSON;
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

procedure TTestInstagramBase.SetUp;
begin
  FConf:=TMemIniFile.Create('testinstagram.ini');
  FInstagramParser:=TInstagramParser.Create;
  FInstagramParser.Logger:=TEventLog.Create(nil);
  FInstagramParser.Logger.AppendContent:=True;
  FInstagramParser.Logger.LogType:=ltFile;
  FInstagramParser.Logger.Active:=True;
  FInstagramParser.HTTPClient.UserAgent:='Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0';
  FTargetUserName:=FConf.ReadString(s_ConfTarget, s_Username, s_SampleAccount);
  FTargetMediaShortCode:=FConf.ReadString(s_ConfTarget, s_Media, s_SampleMedia);
  Sleep(200); // to avoid ban from Instagram
end;

procedure TTestInstagramBase.TearDown;
begin
  FreeAndNil(FConf);
  FInstagramParser.Logger.Free;
  FInstagramParser.Logger:=nil;
  FreeAndNil(FInstagramParser);
end;

end.

