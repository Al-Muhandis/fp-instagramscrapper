unit testinstagram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, InstagramScrapper, fpjson;

type

  { TTestInstagram }

  TTestInstagram= class(TTestCase)
  private
    FInstagramParser: TInstagramParser;
    procedure SaveJSONObject(AnObject: TJSONObject; const AFileName: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    procedure AccountProperties;
    procedure MediaProperties;
  published
    procedure TestGetParseJSONAccount;
    procedure TestGetParseJSONMedia;
  end;

implementation

uses
  FileUtil;

const
  s_SampleAccount='natgeo';
  s_SampleMedia='BqRpCX2gfsq';
  s_NotParsed='Not parsed';
  s_NilJSON='JSON object not assigned!';

procedure TTestInstagram.TestGetParseJSONAccount;
begin
  AssertTrue(s_NotParsed, FInstagramParser.ParseGetAccount(s_SampleAccount));
  SaveJSONObject(FInstagramParser.jsonUser, '~account.json');
  AccountProperties;
end;

procedure TTestInstagram.AccountProperties;
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

procedure TTestInstagram.MediaProperties;
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
    AssertTrue('Empty media content',
      FInstagramParser.Images.Count+FInstagramParser.Images.Count>0);
  finally
    AProperties.Free;
  end;
end;

procedure TTestInstagram.TestGetParseJSONMedia;
begin
  AssertTrue(s_NotParsed, FInstagramParser.ParseGetPost(s_SampleMedia));
  SaveJSONObject(FInstagramParser.jsonPost, '~media.json');
  MediaProperties;
end;

procedure TTestInstagram.SaveJSONObject(AnObject: TJSONObject; const AFileName: String);
var
  AStrings: TStringList;
begin
  if not Assigned(AnObject) then
  begin
    Fail(s_NilJSON);
    Exit;
  end;
  AStrings:=TStringList.Create;
  try
    AStrings.Text:=AnObject.FormatJSON;
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

procedure TTestInstagram.SetUp;
begin
  FInstagramParser:=TInstagramParser.Create;
end;

procedure TTestInstagram.TearDown;
begin
  FreeAndNil(FInstagramParser);
end;

initialization
  RegisterTest('Account', TTestInstagram);
end.

