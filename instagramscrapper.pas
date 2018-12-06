unit InstagramScrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, eventlog, fphttpclient;

type
  TArrayOfString = array of String;

  Tjson_HLStories = TJSONArray;
  TAnsiCharSet = set of TAnsiChar;

  { TInstagramParser }

  TInstagramParser = class
  private
    FBiography: String;
    FCommentHasPrev: Boolean;
    FCountryCode: String;
    FFollowedBy: Integer;
    FFollows: Integer;
    FFullName: String;
    FHTTPCode: Integer;
    FjsonMedias: TJSONObject;
    FjsonPost: TJSONObject;
    FLogged: Boolean;
    FParseComments: Boolean;
    FPostCaption: String;
    FProfilePic: String;
    FResponse: String;
    FCommentCount: Integer;
    FHomePage: String;
    FImages: TStrings;
    FLanguageCode: String;
    FLikes: Integer;
    FLogger: TEventLog;
    FMaxCommentID: Int64;
    FMaxCommentsGet: Integer;
    FParsed: Boolean;
    FShortcode: String;
    FSessionPassword: String;
    FSessionUserName: String;
    FUrl: String;
    FUsername: String;
    FUserID: Int64;
    FUserSession: TStrings;
    FVideos: TStrings;
    FThumbVideos: TStrings;
    FCommentList: TJSONArray;
    FHTTPClient: TFPHTTPClient;
    FJSON_Data: TJSONObject;
    FjsonUser: TJSONObject;
    FEndCursor: String;
    FMaxID: Int64;
    FrhxGis: String;
    Fcsrf_token: String;
    procedure AddMediaUrl(ANode: TJSONObject);
    function ExtractCommentsFromMediaJSONData(AMediaJSON: TJSONObject;
      out hasPrevious: Boolean; out AMaxID: Int64): Boolean;
    function ExtractSharedData: Boolean;
    procedure ExtractSharedData_Profile;
    procedure ExtractSharedData_Post;
    function IsLoggenIn(ASession: TStrings = nil): Boolean;
    procedure generateHeaders(ASession: TStrings; gisToken: String = '');
    class function getAccountJsonLink(const AnUserName: String): String;
    function getEdgesFromGraphQueryHash(const AHash: String; variables: TJSONObject;
      const ResPath: String): TJSONArray;
    class function getGraphQlUrl(const QueryId: String; const Parameters: String): String;
    class function getGraphQlQueryHashUrl(const QueryHash: String; const Parameters: String): String;
    class function getMediaLink(const ACode: String): String;
    class function GetEndCursor(PageInfo: TJSONObject): String;
    function getUserStoriesLink: String;
    function getStoriesLink(const variables: TJSONObject): String;
    function getHighlightStoriesLinkIDs(const variables: TJSONObject): String;
    function getHighlightStoriesLink1(const variables: TJSONObject): String;
    class function getAccountMediasJsonLink(AUserID: Int64; const After: String): String;
    procedure LogMesage(EventType: TEventType; const Msg: String);
    procedure ParseCookies(AHTTPClient: TFPHTTPClient; ASession: TStrings);
    procedure ParseSetCookie(AHeaders: TStrings; ASession: TStrings; const AName: String);
    function Parse_SharedData(const JSONData: String): Boolean;
    procedure Parse_SharedData_Profile(const JSONData: String);
    procedure Parse_SharedData_Post(const JSONData: String);
    function Parse_jsonUser(ParseMedia: Boolean = True): Boolean;
    function Parse_jsonPost: Boolean;
    function Parse_jsonMedias: Boolean;
    procedure SetBiography(AValue: String);
    procedure SetCommentHasPrev(AValue: Boolean);
    procedure SetEndCursor(AValue: String);
    procedure SetHTTPCode(AValue: Integer);
    procedure SetjsonMedias(AValue: TJSONObject);
    procedure SetjsonPost(AValue: TJSONObject);
    procedure SetJSON_Data(AValue: TJSONObject);
    procedure SetjsonUser(AValue: TJSONObject);
    procedure SetLogged(AValue: Boolean);
    procedure SetLogger(AValue: TEventLog);
    procedure SetMaxCommentID(AValue: Int64);
    procedure SetMaxCommentsGet(AValue: Integer);
    procedure SetMaxID(AValue: Int64);
    procedure SetPostCaption(AValue: String);
    procedure SetShortcode(AValue: String);
    procedure SetProfilePic(AValue: String);
    procedure SetSessionPassword(AValue: String);
    procedure SetSessionUserName(AValue: String);
    procedure SetUrl(AValue: String);
    function getCommentsBeforeCommentIdByCode(ACount: Integer;
      ACommentId: Int64): String;
  public
    constructor Create;
    constructor Create(const AnUrl: String);
    destructor Destroy; override;
    function getAccount(const AUserName: String = ''): Boolean;  // Only authorised!!!
    function getMediasByPostCode(const ACode: String): Boolean;
    function getMediasByUserID(AUserID: Int64; var After: Int64): Boolean;
    function ParseGetAccount(const AUserName: String = ''): Boolean;
    function ParseGetPost(ACode: String = ''): Boolean;
    function GetDataFromUrl: Boolean;
    function GetJSONDataFromUrl: Boolean;
    function GetCommentsFromUrl: Boolean; overload;
    function getMediaCommentsByCode(ACount: Integer = 10; AMaxID: Int64 = 0): Boolean;
    function GetSrcsFromHTML: Boolean;
    function IsInstagram: Boolean;
    function IsInstagramUrl(const AnUrl: String): Boolean; overload;
    function HTTPGetText(AnURL: string = ''): Boolean;
    function HTTPGetJSON(AnURL: string = ''): TJSONObject;
    function Login(Force: Boolean = False): Boolean;
    function CheckLogin(): Boolean;
    function GetPostUrl(APostID: String = ''): String;
    function getStories(AReel_ids: TJSONArray = nil): TJSONArray;
    function getStoriesForUser(AUserID: Int64 = 0): TJSONArray;
    function getHLStoriesForUser(AUserID: Int64 = 0): Tjson_HLStories;
    function getHLStoriesForUser_internal(AUserID: Int64 = 0): Tjson_HLStories;
    function LoginNGetStories(reel_ids: TJSONArray = nil): TJSONArray;
    function LoginNGetHLStories(AUserID: Int64 = 0): Tjson_HLStories;
    function LoginNGetMediaCommentsByCode(ACount: Integer = 10; AMaxID: Int64 = 0): Boolean;
    function PrivateInfoByID(AccountID: Int64): Boolean;
    procedure SetUrlFromProfile(const Username: String);
    class function ThumbUrlFromMedias(ANode: TJSONObject): String;
    function UrlFromUsername(const AUsername: String = ''): String;
    class function UrlFromShortcode(const AShortcode: String): String;
    procedure withCredentials(const User, Password: String);
    property UserID: Int64 read FUserID;
    property Username: String read FUsername;
    property FollowedBy: Integer read FFollowedBy;
    property Follows: Integer read FFollows;
    property FullName: String read FFullName;
    property Parsed: Boolean read FParsed;
    property Shortcode: String read FShortcode write SetShortcode;
    property Images: TStrings read FImages;
    property Videos: TStrings read FVideos;
    property ThumbVideos: TStrings read FThumbVideos;
    property CommentCount: Integer read FCommentCount;
    property HomePage: String read FHomePage;
    property Likes: Integer read FLikes;
    property Url: String read FUrl write SetUrl;
    property MaxCommentsGet: Integer read FMaxCommentsGet write SetMaxCommentsGet;
    property CountryCode: String read FCountryCode;
    property LanguageCode: String read FLanguageCode;
    property Logger: TEventLog read FLogger write SetLogger;
    property CommentList: TJSONArray read FCommentList;
    property ParseComments: Boolean read FParseComments write FParseComments;
    property MaxCommentID: Int64 read FMaxCommentID write SetMaxCommentID;
    property CommentHasPrev: Boolean read FCommentHasPrev write SetCommentHasPrev;
    property SessionUserName: String read FSessionUserName write SetSessionUserName;
    property SessionPassword: String read FSessionPassword write SetSessionPassword;
    property UserSession: TStrings read FUserSession;
    property JSON_Data: TJSONObject read FJSON_Data write SetJSON_Data;
    property jsonUser: TJSONObject read FjsonUser write SetjsonUser;
    property jsonPost: TJSONObject read FjsonPost write SetjsonPost;
    property jsonMedias: TJSONObject read FjsonMedias write SetjsonMedias;
    property PostCaption: String read FPostCaption write SetPostCaption;
    property ProfilePic: String read FProfilePic write SetProfilePic;
    property Biography: String read FBiography write SetBiography;
    property EndCursor: String read FEndCursor write SetEndCursor;
    property MaxID: Int64 read FMaxID write SetMaxID;
    property Logged: Boolean read FLogged write SetLogged;
    property HTTPCode: Integer read FHTTPCode write SetHTTPCode;
  end;

const
  MediaIDValidChars = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
  UserNameValidChars = MediaIDValidChars+['.'];

function ExtractBetweenKeys(const ASource, Key1, Key2: String;
  var APos: Integer; out ADest: String): Boolean;
function HttpGetText(const URL: string; out Response: String;
  Logger: TEventLog = nil): Boolean;
function HTTPPostData(const URL, URLData: string; out Response: String): Boolean;
function JSONStringToString(const S: TJSONStringType): UnicodeString;
function GetThumbnailUrl(ADim: Integer; const APhotoUrl: String): String; deprecated;
function IsValidString(const S: String;
  ValidChars: TAnsiCharSet = UserNameValidChars): Boolean;

const
  InstgrmStart = 'https://instagram.com/';
  InstgrmStart1 = 'https://www.instagram.com/';

  s_TakenAtTimestamp = 'taken_at_timestamp';
  s_ID = 'id';
  s_Node = 'node';
  s_Owner = 'owner';
  s_Text = 'text';
  s_Username = 'username';
  s_Shortcode = 'shortcode';
  s_CreatedAt = 'created_at';

implementation

uses
  strutils, jsonparser, jsonscanner;

const
  MAX_COMMENTS_PER_REQUEST = 300;
  COMMENTS_BEFORE_COMMENT_ID_BY_CODE = 'https://www.instagram.com/graphql/query/?query_id=17852405266163336&shortcode={{shortcode}}&first={{count}}&after={{commentId}}';
  //'https://www.instagram.com/graphql/query/?query_hash=33ba35852cb50da46f5b5e889df7d159&shortcode={{shortcode}}&first={{count}}&after={{commentId}}';
  BASE_URL = 'https://www.instagram.com';
  LOGIN_URL = 'https://www.instagram.com/accounts/login/ajax/';
  InstagramQueryIdUSER_STORIES = '17890626976041463';
  InstagramQueryIdSTORIES = '17873473675158481';
  GRAPH_QL_QUERY_URL = 'https://www.instagram.com/graphql/query/?query_id={{queryId}}';
  ACCOUNT_JSON_INFO = 'https://www.instagram.com/{username}/?__a=1';
//  ACCOUNT_MEDIAS = 'https://www.instagram.com/{username}/?__a=1&max_id={max_id}';
  MEDIA_JSON_INFO = 'https://www.instagram.com/p/{code}/?__a=1';
  GRAPH_QL_QUERY_URL1 = 'https://www.instagram.com/graphql/query/?query_hash={{queryHash}}';
  ACCOUNT_MEDIAS2 = 'https://instagram.com/graphql/query/?query_id=17888483320059182&id={user_id}&first=12&after={end_cursor}';
  //ACCOUNT_MEDIAS3 = 'https://www.instagram.com/graphql/query/?query_hash=a5164aed103f24b03e7b7747a2d94e3c&variables={"id":"1775246512","first":12,"after":"AQBVsfylyZxyuP11Oeb51dK4MAC1ptz9s69g8KhW3qZfjf-iZKkeSlf15o292ChEy3sK3gPQMc738xS2BImRipymLlVOguRcZxRQULVNNH9cmg"}';
  QryHash_HLStoriesIDs = '7c16654f22c819fb63d1183034a5162f';
  QryHash_HLStories1 = '45246d3fe16ccc6577e0bd297a5db1ab';

  url_privateinfo_by_id='https://i.instagram.com/api/v1/users/{user_id}/info/';


procedure StrToFile(const S, AFileName: String);
var
  AStrings: TStringList;
begin
  AStrings:=TStringList.Create;
  AStrings.Text:=S;
  try
    AStrings.SaveToFile(AFileName);
  finally
    AStrings.Free;
  end;
end;

function ExtractBetweenKeys(const ASource, Key1, Key2: String;
  var APos: Integer; out ADest: String): Boolean;
var
  AStart, AnEnd: Integer;
begin
  Result := False;
  AStart := PosEx(Key1, ASource, APos);
  if AStart <> 0 then
  begin
    Inc(AStart, Length(Key1));
    AnEnd := PosEx(Key2, ASource, AStart);
    if AnEnd <> 0 then
    begin
      ADest := copy(ASource, AStart, AnEnd - AStart);
      Result := True;
      APos := AnEnd + Length(Key2)
    end
  end
end;

function HttpGetText(const URL: string; out Response: String;
  Logger: TEventLog = nil): Boolean;
var
  HTTP: TFPHTTPClient;
  StatusCode: Integer;

begin
  HTTP := TFPHTTPClient.Create(nil);
  try
    HTTP.AllowRedirect:=True;
    try
      Response:=HTTP.Get(URL);
    except
      on E: Exception do
        if Assigned(Logger) then
          Logger.Error('Error while GET request: '+E.Message);
    end;
    StatusCode:=HTTP.ResponseStatusCode;
    Result:=StatusCode=200;
  finally
    HTTP.Free;
  end;
end;

function HTTPPostData(const URL, URLData: string; out Response: String): Boolean;
var
  HTTP: TFPHTTPClient;
begin
  HTTP:=TFPHTTPClient.Create(nil);
  try
    HTTP.RequestBody:=TStringStream.Create(URLData);
    try
      HTTP.AddHeader('Content-Type','application/json');
      Response:=HTTP.Post(URL);
    finally
      HTTP.RequestBody.Free;
    end;
    Result:=True;
  except
    Result:=False;
  end;
  HTTP.Free;
end;
  // Own JSONStringToString... Bug with emoji in function in fcl-json library at least in stable version of fpc
function JSONStringToString(const S: TJSONStringType): UnicodeString;

Var
  I,J,L : Integer;
  P : PJSONCharType;
  w : String;

begin
  I:=1;
  J:=1;
  L:=Length(S);
  Result:={%H-}EmptyStr;
  P:=PJSONCharType(S);
  While (I<=L) do
    begin
    if (P^='\') then
      begin
      Result:=Result+{%H-}Copy(S,J,I-J);
      Inc(P);
      If (P^<>#0) then
        begin
        Inc(I);
        Case AnsiChar(P^) of
          '\','"','/'
              : Result:=Result+P{%H-}^;
          'b' : Result:=Result+#8;
          't' : Result:=Result+#9;
          'n' : Result:=Result+#10;
          'f' : Result:=Result+#12;
          'r' : Result:=Result+#13;
          'u' : begin
                W:=Copy(S,I+1,4);
                Inc(I,4);
                Inc(P,4);
                Result:=Result+WideChar(StrToInt('$'+W));
                end;
        end;
        end;
      J:=I+1;
      end;
    Inc(I);
    Inc(P);
    end;
  Result:=Result+{%H-}Copy(S,J,I-J+1);
end;

function GetThumbnailUrl(ADim: Integer; const APhotoUrl: String): String;
var
  i, j, k, l: Integer;
  s: String;
begin
  i:=strutils.PosEx('.com/vp/', APhotoUrl);
  if i=0 then
    Exit('');
  j:=strutils.PosEx('/', APhotoUrl, i+10);
  if j=0 then
    Exit('');
  Result:=LeftStr(APhotoUrl, i-1)+'.com/'+RightStr(APhotoUrl, Length(APhotoUrl)-j);
  s:=IntToStr(ADim);
  s:='s'+s+'x'+s;
  k:=strutils.RPos('/', Result);
  if k=0 then
    Exit;
  l:=strutils.RPos('/s', Result);
  if l>=k then
    Exit;
  if l>length('https://scontent') then
    Result:=LeftStr(Result, l)+s+'/'+RightStr(Result, Length(Result)-k)
  else 
    Result:=LeftStr(Result, k)+s+'/'+RightStr(Result, Length(Result)-k)
end;

function IsValidString(const S: String; ValidChars: TAnsiCharSet): Boolean;
var
  i: Integer;
begin
  if S=EmptyStr then
    Exit(False);
  i:=0;
  Result:=True;
  while Result and (i<Length(S)) do
  begin
    if not (S[i+1] in ValidChars) then
      Result:=False;
    Inc(i);
  end;
end;

{ TInstagramParser }

procedure TInstagramParser.SetMaxCommentsGet(AValue: Integer);
begin
  if FMaxCommentsGet=AValue then Exit;
  FMaxCommentsGet:=AValue;
end;

procedure TInstagramParser.SetMaxID(AValue: Int64);
begin
  if FMaxID=AValue then Exit;
  FMaxID:=AValue;
end;

procedure TInstagramParser.SetPostCaption(AValue: String);
begin
  if FPostCaption=AValue then Exit;
  FPostCaption:=AValue;
end;

procedure TInstagramParser.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger:=AValue;
end;

procedure TInstagramParser.generateHeaders(ASession: TStrings; gisToken: String
  );
begin
  if Assigned(ASession) then
  begin
    FHTTPClient.Cookies.Assign(ASession);
    FHTTPClient.AddHeader('referer', BASE_URL+'/');
    FHTTPClient.AddHeader('x-csrftoken', Fcsrf_token);
    if not (gisToken=EmptyStr) then
      FHTTPClient.AddHeader('x-instagram-gis', gisToken);
  end
  else
    FHTTPClient.RequestHeaders.Clear;
end;

class function TInstagramParser.getAccountJsonLink(const AnUserName: String): String;
begin
  Result:=ReplaceStr(ACCOUNT_JSON_INFO, '{username}', AnUserName);
end;

class function TInstagramParser.getGraphQlUrl(const QueryId: String;
  const Parameters: String): String;
begin
  Result := ReplaceStr(GRAPH_QL_QUERY_URL, '{{queryId}}', QueryId);
  if Parameters<>EmptyStr then
    Result+='&' + 'variables='+EncodeURLElement(Parameters);
end;

class function TInstagramParser.getGraphQlQueryHashUrl(const QueryHash: String;
  const Parameters: String): String;
begin
  Result := ReplaceStr(GRAPH_QL_QUERY_URL1, '{{queryHash}}', QueryHash);
  if Parameters<>EmptyStr then
    Result+='&' + 'variables='+EncodeURLElement(Parameters);
end;

class function TInstagramParser.getMediaLink(const ACode: String): String;
begin
  Result:=ReplaceStr(MEDIA_JSON_INFO, '{code}', ACode);
end;

class function TInstagramParser.GetEndCursor(PageInfo: TJSONObject): String;
begin
  if PageInfo.Booleans['has_next_page'] then
    Result:=PageInfo.Strings['end_cursor']
  else
    Result:='';
end;

function TInstagramParser.getUserStoriesLink: String;
begin
  Result := getGraphQlUrl(InstagramQueryIdUSER_STORIES, '');
end;

function TInstagramParser.getStoriesLink(const variables: TJSONObject): String;
begin
  Result := getGraphQlUrl(InstagramQueryIdSTORIES, variables.AsJSON);
end;

function TInstagramParser.getHighlightStoriesLinkIDs(const variables: TJSONObject
  ): String;
begin
  Result := getGraphQlQueryHashUrl(QryHash_HLStoriesIDs, variables.AsJSON);
end;

function TInstagramParser.getHighlightStoriesLink1(const variables: TJSONObject
  ): String;
begin
  Result := getGraphQlQueryHashUrl(QryHash_HLStories1, variables.AsJSON);
end;

function TInstagramParser.getEdgesFromGraphQueryHash(const AHash: String;
  variables: TJSONObject; const ResPath: String): TJSONArray;
var
  jsonResponse: TJSONObject;
  jsonArray: TJSONArray;
  u: String;
begin
  Result:=nil;
  try
    variables.CompressedJSON:=True;
    generateHeaders(UserSession);
    u:=getGraphQlQueryHashUrl(AHash, variables.AsJSON);
    jsonResponse:=HTTPGetJSON(u);
    if not Assigned(jsonResponse) then
      Exit;
    try
      {$IFDEF DEBUG}LogMesage(etDebug, 'Queryhash: '+jsonResponse.AsJSON);{$ENDIF}
      jsonArray:=jsonResponse.FindPath(ResPath) as TJSONArray;
      if Assigned(jsonArray) then
        Result:=jsonArray.Clone as TJSONArray;
    finally
      jsonResponse.Free;
    end;
  except
    Result:=nil;
  end;
end;

class function TInstagramParser.getAccountMediasJsonLink(AUserID: Int64; const After: String): String;
begin
  Result:=ReplaceStr(ACCOUNT_MEDIAS2, '{user_id}', IntToStr(AUserID));
  Result:=ReplaceStr(Result, '{end_cursor}', After);
end;

procedure TInstagramParser.LogMesage(EventType: TEventType; const Msg: String);
begin
  if Assigned(FLogger) then
    FLogger.Log(EventType, Msg);
end;

procedure TInstagramParser.ParseCookies(AHTTPClient: TFPHTTPClient; ASession: TStrings);
begin
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'csrftoken');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'mid');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'sessionid');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'ds_user_id');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'rur');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'mcd');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'target');
  ParseSetCookie(AHTTPClient.ResponseHeaders, ASession, 'urlgen');
end;

procedure TInstagramParser.ParseSetCookie(AHeaders: TStrings;
  ASession: TStrings; const AName: String);
var
  APos: Integer;
  S: String;
begin
  APos:=1;
  while ExtractBetweenKeys(AHeaders.Text, AName+'=', ';', APos, S) do
    if S<>'""' then
    begin
      ASession.Values[AName]:=S;
      Exit;
    end;
end;

function TInstagramParser.Parse_SharedData(const JSONData: String): Boolean;
var
  p: TJSONParser;
  d: TJSONData;
begin
  try
    p:=TJSONParser.Create(JSONData, DefaultOptions);
    try
      JSON_Data:=nil;
      JSON_Data:=p.Parse as TJSONObject;
      FrhxGis:=FJSON_Data.Get('rhx_gis', EmptyStr);
      d:=FJSON_Data.FindPath('config.csrf_token');
      if Assigned(d) then
        Fcsrf_token:=d.AsString
      else
        Fcsrf_token:=EmptyStr;
      Result:=True;
    finally
      p.Free;
    end;
  except
    LogMesage(etError, 'Failed to parse shared JSON data: "'+JSONData+'"');
    Result:=False;
  end;
end;

procedure TInstagramParser.Parse_SharedData_Profile(const JSONData: String);
begin
  if Parse_SharedData(JSONData) then
    try
      jsonUser:=FJSON_Data.Objects['entry_data'].Arrays['ProfilePage'].Objects[0].Objects['graphql'].Objects['user'].Clone as TJSONObject;
      if not Parse_jsonUser then
        LogMesage(etError, 'Failed to parse json user data: '+jsonUser.AsJSON);
    except
      on E: Exception do
      begin
        LogMesage(etError, 'Unable to parse jsonUser. '+ e.ClassName+': '+e.Message);
        LogMesage(etDebug, FJSON_Data.AsJSON);
        FjsonUser:=nil;
      end;
    end;
end;

procedure TInstagramParser.Parse_SharedData_Post(const JSONData: String);
begin
  if Parse_SharedData(JSONData) then
    try
      jsonPost:=FJSON_Data.Objects['entry_data'].Arrays['PostPage'].Objects[0].Objects['graphql'].Objects['shortcode_media'].Clone as TJSONObject;
      if not Parse_jsonPost then
        LogMesage(etError, 'Failed to parse json media post data: '+jsonPost.AsJSON);
    except
      FjsonPost:=nil;
    end;
end;

function TInstagramParser.Parse_jsonUser(ParseMedia: Boolean): Boolean;
var
  jo: TJSONObject;
  jsonEnum: TJSONEnum;
  s: String;
begin
  try
    FBiography:=FjsonUser.Get('biography', '');
    FHomePage:=FjsonUser.Get('external_url', '');
    if FjsonUser.Find('edge_followed_by',jo) then             //followed_by
      FFollowedBy:=jo.Integers['count'];
    if FjsonUser.Find('edge_follow', jo) then          //follows
      FFollows:=jo.Integers['count'];
    FFullName:=FjsonUser.Get('full_name', '');
    S:=FjsonUser.Get('id', EmptyStr);
    FUserID:=StrToInt64Def(S, 0);

    FProfilePic:=FjsonUser.Get('profile_pic_url_hd', '');
    if FProfilePic=EmptyStr then
       FProfilePic:=FjsonUser.Get('profile_pic_url', '');
    FUsername:=FjsonUser.Strings['username'];

    if ParseMedia then
      if FjsonUser.Find('edge_owner_to_timeline_media', jo) then //media
      begin
        FMaxID:=0;
        FEndCursor:=GetEndCursor(jo.Objects['page_info']);
        for jsonEnum in jo.Arrays['edges'] do
          AddMediaUrl((jsonEnum.Value as TJSONObject).Objects['node']);
      end;

    Result:=True;
  except
    Result:=False
  end;
end;

function TInstagramParser.Parse_jsonPost: Boolean;
var
  jo: TJSONObject;
  jsonEnum: TJSONEnum;
begin
  try
    FPostCaption:=EmptyStr;
    if FjsonPost.Find('edge_media_to_caption', jo) then
      for jsonEnum in jo.Arrays['edges'] do
        FPostCaption+=(jsonEnum.Value as TJSONObject).Objects['node'].Strings['text'];
    FMaxID:=0;
    if FjsonPost.Find('edge_sidecar_to_children', jo) then
      for jsonEnum in jo.Arrays['edges'] do
        AddMediaUrl((jsonEnum.Value as TJSONObject).Objects['node'])
    else
      AddMediaUrl(FjsonPost);

    FCommentCount:=FjsonPost.Objects['edge_media_to_comment'].Integers['count'];
    FLikes:=FjsonPost.Objects['edge_media_preview_like'].Integers['count'];

    jsonUser:=FjsonPost.Objects['owner'].Clone as TJSONObject;
    Parse_jsonUser(False);

    if FParseComments then
      ExtractCommentsFromMediaJSONData(jsonPost, FCommentHasPrev, FMaxCommentID);

    Result:=True;
  except
    Result:=False
  end;
end;

function TInstagramParser.Parse_jsonMedias: Boolean;
var
  ja: TJSONArray;
  jsonEnum: TJSONEnum;
begin
  try
    FMaxID:=0;
    FEndCursor:=GetEndCursor(jsonMedias.Objects['page_info']);
    ja:=jsonMedias.Find('edges', jtArray) as TJSONArray;
    if Assigned(ja) then
      for jsonEnum in ja do
        AddMediaUrl((jsonEnum.Value as TJSONObject).Objects['node']);

    Result:=True;
  except
    Result:=False
  end;
end;

procedure TInstagramParser.SetBiography(AValue: String);
begin
  if FBiography=AValue then Exit;
  FBiography:=AValue;
end;

procedure TInstagramParser.SetCommentHasPrev(AValue: Boolean);
begin
  if FCommentHasPrev=AValue then Exit;
  FCommentHasPrev:=AValue;
end;

procedure TInstagramParser.SetEndCursor(AValue: String);
begin
  if FEndCursor=AValue then Exit;
  FEndCursor:=AValue;
end;

procedure TInstagramParser.SetHTTPCode(AValue: Integer);
begin
  if FHTTPCode=AValue then Exit;
  FHTTPCode:=AValue;
end;

procedure TInstagramParser.SetjsonMedias(AValue: TJSONObject);
begin
  if FjsonMedias=AValue then Exit;
  FjsonMedias.Free;
  FjsonMedias:=AValue;
end;

procedure TInstagramParser.SetjsonPost(AValue: TJSONObject);
begin
  if FjsonPost=AValue then Exit;
  if Assigned(FjsonPost) then
    FjsonPost.Free;
  FjsonPost:=AValue;
end;

procedure TInstagramParser.SetJSON_Data(AValue: TJSONObject);
begin
  if FJSON_Data=AValue then Exit;
  if Assigned(FJSON_Data) then
    FJSON_Data.Free;
  FJSON_Data:=AValue;
end;

procedure TInstagramParser.SetjsonUser(AValue: TJSONObject);
begin
  if FjsonUser=AValue then Exit;
  if Assigned(FjsonUser) then
    FjsonUser.Free;
  FjsonUser:=AValue;
end;

procedure TInstagramParser.SetLogged(AValue: Boolean);
begin
  if FLogged=AValue then Exit;
  FLogged:=AValue;
end;

procedure TInstagramParser.SetMaxCommentID(AValue: Int64);
begin
  if FMaxCommentID=AValue then Exit;
  FMaxCommentID:=AValue;
end;

procedure TInstagramParser.SetShortcode(AValue: String);
begin
  if FShortcode=AValue then Exit;
  FShortcode:=Trim(AValue);
  FUrl:=GetPostUrl;
end;

procedure TInstagramParser.SetProfilePic(AValue: String);
begin
  if FProfilePic=AValue then Exit;
  FProfilePic:=AValue;
end;

procedure TInstagramParser.SetSessionPassword(AValue: String);
begin
  if FSessionPassword=AValue then Exit;
  FSessionPassword:=AValue;
end;

procedure TInstagramParser.SetSessionUserName(AValue: String);
begin
  if FSessionUserName=AValue then Exit;
  FSessionUserName:=AValue;
end;

procedure TInstagramParser.SetUrl(AValue: String);
var
  i: Integer;
begin
  if FUrl=AValue then Exit;
  FUrl:=Trim(AValue);
  i:=Pos('?', FUrl);
  if i>0 then
    FUrl:=LeftStr(FUrl, i-1);
  if AnsiStartsText('http://', FUrl) then
    FUrl:='https://'+RightStr(FUrl, Length(FUrl)-Length('http://'));
  if AnsiStartsText('instagram.com/', FUrl) then
    FUrl:='https://'+FUrl;
  FUrl:=IncludeTrailingPathDelimiter(FUrl);
end;

function TInstagramParser.getCommentsBeforeCommentIdByCode(ACount: Integer;
  ACommentId: Int64): String;
var
  s: String;
begin
  Result := ReplaceStr(COMMENTS_BEFORE_COMMENT_ID_BY_CODE, '{{shortcode}}', FShortcode);
  Result := ReplaceStr(Result, '{{count}}', IntToStr(ACount));
  if ACommentId=0 then
    s:=''
  else
    s:=IntToStr(ACommentId);
  Result:= ReplaceStr(Result, '{{commentId}}', S);
end;

constructor TInstagramParser.Create;
begin
  FMaxCommentsGet:=50;
  FImages:=TStringList.Create;
  FVideos:=TStringList.Create;
  FThumbVideos:=TStringList.Create;
  with TStringList(FImages) do
  begin
    Sorted:=True;
    Duplicates:=dupIgnore;
  end;
  with TStringList(FVideos) do
  begin
    Sorted:=True;
    Duplicates:=dupIgnore;
  end;
  with TStringList(FThumbVideos) do
  begin
    Sorted:=True;
    Duplicates:=dupIgnore;
  end;
  FCommentList:=TJSONArray.Create;
  FSessionUserName:=EmptyStr;
  FSessionPassword:=EmptyStr;
  FHTTPClient := TFPHTTPClient.Create(nil);
  FHTTPClient.AllowRedirect:=True;

  FUserSession := TStringList.Create;

  FJSON_Data:=nil;
  FjsonUser:=nil;
  FjsonPost:=nil;
  FjsonMedias:=nil;

  FEndCursor:=EmptyStr;
  Fcsrf_token:=EmptyStr;

  FLogged:=False;
  FParseComments:=False;
end;

constructor TInstagramParser.Create(const AnUrl: String);
begin
  Create;
  FUrl:=AnUrl;
  if AnUrl<>EmptyStr then
    FParsed:=GetDataFromUrl;
end;

destructor TInstagramParser.Destroy;
begin
  jsonMedias:=nil;
  jsonPost:=nil;
  jsonUser:=nil;
  JSON_Data:=nil;
  FUserSession.Free;
  FHTTPClient.Free;
  FCommentList.Free;
  FImages.Free;
  FVideos.Free;
  FThumbVideos.Free;
  inherited Destroy;
end;

function TInstagramParser.getAccount(const AUserName: String): Boolean;
var
  jsonResponce: TJSONObject;
begin
  try
    Result:=False;
    if AUserName<>EmptyStr then
      FUsername:=AUserName;
    jsonResponce:=HTTPGetJSON(getAccountJsonLink(FUsername)) as TJSONObject;
    try
      jsonUser:=jsonResponce.Objects['graphql'].Objects['user'].Clone as TJSONObject;
      if Assigned(jsonUser) then
        Result:=Parse_jsonUser;
    finally
      jsonResponce.Free;
    end;
  except
    Result:=False;
  end;
end;

function TInstagramParser.getMediasByPostCode(const ACode: String): Boolean;
var
  jsonResponce: TJSONObject;
begin
  try
    Result:=False;
    jsonResponce:=HTTPGetJSON(getMediaLink(ACode)) as TJSONObject;
    try
      jsonPost:=jsonResponce.Objects['graphql'].Objects['shortcode_media'].Clone as TJSONObject;
      if Assigned(jsonPost) then
        Result:=Parse_jsonPost;
    finally
      jsonResponce.Free;
    end;
  except
    Result:=False;
  end;
end;

function TInstagramParser.getMediasByUserID(AUserID: Int64; var After: Int64): Boolean;
var
  AnUrl: String;
begin
  try
    Result:=False;
    AnUrl:=getAccountMediasJsonLink(AUserID, IntToStr(After));
    jsonMedias:=(HTTPGetJSON(AnUrl) as TJSONObject).Objects['data'].Objects['user'].Objects['edge_owner_to_timeline_media']; //user
    if Assigned(jsonMedias) then
    begin
      Result:=Parse_jsonMedias;
      After:=FMaxID;
    end;
  except
    Result:=False;
    After:=0;
  end;
end;

function TInstagramParser.ParseGetAccount(const AUserName: String): Boolean;
begin
  Result:=False;
  if AUserName=EmptyStr then
    SetUrlFromProfile(FUserName)
  else
    SetUrlFromProfile(AUserName);
  if GetDataFromUrl then
    Result:=Assigned(jsonUser)
end;

function TInstagramParser.ParseGetPost(ACode: String): Boolean;
begin
  if ACode=EmptyStr then
    getMediasByPostCode(FShortcode)
  else
    getMediasByPostCode(ACode);
  Result:=Assigned(jsonPost)
end;

function TInstagramParser.GetDataFromUrl: Boolean;
begin
  Result:=False;
  if HttpGetText then
    Result:=GetSrcsFromHTML;
end;

function TInstagramParser.GetJSONDataFromUrl: Boolean;
begin
  if Shortcode=EmptyStr then
    Result:=ParseGetAccount(EmptyStr)
  else
    Result:=ParseGetPost;
end;

function TInstagramParser.GetCommentsFromUrl: Boolean;
var
  AParseComments: Boolean;
begin
  Result:=False;
  AParseComments:=FParseComments;
  FParseComments:=True;
  if HttpGetText then
    Result:=GetSrcsFromHTML;
  FParseComments:=AParseComments;
end;

function TInstagramParser.getMediaCommentsByCode(ACount: Integer; AMaxID: Int64
  ): Boolean;
var
  Remain, NumberOfCommentsToRetreive: Integer;
  AnIndex: Integer;
  HasPrevious: Boolean;
  CommentsUrl: String;
  jsonResponse, AjsonPost: TJSONObject;
begin
  Result:=False;
  FCommentCount:=0;
  FCommentList.Clear;
  Remain := ACount;
  AnIndex := 0;
  HasPrevious := true;
  while (HasPrevious and (AnIndex < ACount)) do
  begin
    if (Remain > MAX_COMMENTS_PER_REQUEST) then
    begin
      NumberOfCommentsToRetreive := MAX_COMMENTS_PER_REQUEST;
      Remain -= MAX_COMMENTS_PER_REQUEST;
      AnIndex += MAX_COMMENTS_PER_REQUEST;
    end else
    begin
      NumberOfCommentsToRetreive := Remain;
      AnIndex += Remain;
      Remain := 0;
    end;
    CommentsUrl := getCommentsBeforeCommentIdByCode(NumberOfCommentsToRetreive, AMaxID);
    generateHeaders(UserSession);
    jsonResponse:=HTTPGetJSON(CommentsUrl);
    try
      if not Assigned(jsonResponse) then
        Exit(False);
      AjsonPost:=jsonResponse.FindPath('data.shortcode_media') as TJSONObject;
      if not Assigned(AjsonPost) then
        Exit(False);
      ExtractCommentsFromMediaJSONData(AjsonPost, HasPrevious, AMaxID);
      if ACount > FCommentCount then
        ACount := FCommentCount;
    finally
      jsonResponse.Free;
    end;
  end;
  FMaxCommentID:=AMaxID;
  FCommentHasPrev:=HasPrevious;
end;

function TInstagramParser.GetSrcsFromHTML: Boolean;
var
  APos: Integer;
  S, S1: String;


  function ExtractStr(const Key1, Key2: String; out AValue: String): Boolean;
  var
    p: Integer;
    S: String;
  begin
    p:=1;
    Result:=ExtractBetweenKeys(FResponse, Key1, Key2, p, S);
    if Result then
      AValue:=String(JSONStringToString(S));
  end;

  procedure ExtractProfileData;
  var
    js: String;
  begin
    APos:=1;
    if ExtractBetweenKeys(FResponse, 'window._sharedData = ', ';</script>', APos, js) then
    begin
      Parse_SharedData_Profile(js);
      Result:=True;
      Exit;
    end;
  end;

  procedure ExtractPostData;
  var
    js: String;
  begin
    APos:=1;
    if ExtractBetweenKeys(FResponse, 'window._sharedData = ', ';</script>', APos, js) then
    begin
      Parse_SharedData_Post(js);
      Result:=True;
      Exit;
    end;
  end;

begin
  Result:=False;
  APos:=1;
  if ExtractBetweenKeys(FResponse, '<meta property="og:url"', '/>', APos, S) then
  begin
    APos:=1;
    if ExtractBetweenKeys(S, 'content="', '"', APos, S1) then
      if AnsiContainsStr(S1, '/p/') or AnsiContainsStr(S1, '/tv/') then
        ExtractPostData
      else
        if AnsiContainsStr(S1, 'https://www.instagram.com/') then
          ExtractProfileData;
  end;
end;

function TInstagramParser.IsInstagram: Boolean;
var
  i: Integer;
  AUrl, p: String;
begin
  if AnsiStartsStr(InstgrmStart, FUrl) or AnsiStartsStr(InstgrmStart1, FUrl) then
  begin
    Result:=True;
    i:=1;
    AUrl:=IncludeTrailingPathDelimiter(FUrl);
    if AnsiContainsStr(AUrl, '/p/') then
      p:='/p/'
    else
      if AnsiContainsStr(AUrl, '/tv/') then
        p:='/tv/'
      else
        p:=EmptyStr;
    if (p=EmptyStr) or not ExtractBetweenKeys(AUrl, p, '/', i, FShortcode) then
    begin
      FShortcode:='';
      i:=1;
      if not ExtractBetweenKeys(AUrl, 'instagram.com/', '/', i, FUsername) then
        FUsername:=''
    end;
  end
  else
    Result:=False;
end;

function TInstagramParser.IsInstagramUrl(const AnUrl: String): Boolean;
begin
  Url:=AnUrl;
  Result:=IsInstagram;
end;

procedure TInstagramParser.AddMediaUrl(ANode: TJSONObject);
var
  Adrs, S: String;
  AMediaID: Int64;
begin
  AMediaID:=0;
  AMediaID:=StrToInt64Def(ANode.Get('id', ''), 0);
  if (FMaxID=0) or (AMediaID<FMaxID) then
    FMaxID:=AMediaID;
  Adrs:='url';
  if ANode.Booleans['is_video'] then
  begin
    S:=ANode.Get('video_'+Adrs, '');
    if S<>EmptyStr then
      FVideos.AddObject(S, ANode)
    else
      FVideos.AddObject(ANode.Get('shortcode', ''), ANode);
    FThumbVideos.AddObject(ANode.Get('display_'+Adrs, ''), ANode);
  end
  else
    FImages.AddObject(ANode.Strings['display_'+Adrs], ANode);
end;

function TInstagramParser.ExtractCommentsFromMediaJSONData(
  AMediaJSON: TJSONObject; out hasPrevious: Boolean; out AMaxID: Int64
  ): Boolean;
var
  nodes: TJSONArray;
  jsonEnum: TJSONEnum;
begin
  Result:=False;
  try
    FCommentCount:=AMediaJSON.FindPath('edge_media_to_comment.count').AsInteger;
    nodes:=AMediaJSON.FindPath('edge_media_to_comment.edges') as TJSONArray;
    for jsonEnum in nodes do
      FCommentList.Add(jsonEnum.Value.Clone);
    hasPrevious:=
      AMediaJSON.FindPath('edge_media_to_comment.page_info.has_next_page').AsBoolean;
    if nodes.Count=0 then
      Exit(True);
    AMaxID := nodes[0].FindPath('node.id').AsInt64;
    Result:=True;
  except
    on E: Exception do
      LogMesage(etError, 'Failed parse comments: '+AMediaJSON.AsJSON);
  end;
end;

function TInstagramParser.ExtractSharedData: Boolean;
var
  js: String;
  APos: Integer;
begin
  APos:=1;
  Result:=False;
  if ExtractBetweenKeys(FResponse, 'window._sharedData = ', ';</script>', APos, js) then
    Result:=Parse_SharedData(js);
end;

procedure TInstagramParser.ExtractSharedData_Profile;
var
  js: String;  
  APos: Integer;
begin
  APos:=1;
  if ExtractBetweenKeys(FResponse, 'window._sharedData = ', ';</script>', APos, js) then
    Parse_SharedData_Profile(js);
end;

procedure TInstagramParser.ExtractSharedData_Post;
var
  js: String;  
  APos: Integer;
begin
  APos:=1;
  if ExtractBetweenKeys(FResponse, 'window._sharedData = ', ';</script>', APos, js) then
    Parse_SharedData_Post(js);
end;

function TInstagramParser.IsLoggenIn(ASession: TStrings): Boolean;
begin
  if not Assigned(ASession) or (ASession.Values['sessionid']=EmptyStr) then
    Exit(False);
  generateHeaders(ASession);
  HTTPGetText(BASE_URL+'/');
  ParseCookies(FHTTPClient, ASession);
  if FHTTPClient.ResponseStatusCode <> 200 then
    Exit(False);
  if ASession.Values['ds_user_id']=EmptyStr then
    Exit(false);
  Result:=True;
end;

function TInstagramParser.HTTPGetText(AnURL: string): Boolean;
begin
  if AnURL=EmptyStr then
    AnURL:=FUrl;
  try
    FHTTPCode:=0;
    try
      FResponse:=FHTTPClient.Get(AnURL);
    finally
      FHTTPCode:=FHTTPClient.ResponseStatusCode;
      {$IFDEF DEBUG}
      FHTTPClient.Cookies.SaveToFile('~HTTPGetText_Cookies.txt');
      FHTTPClient.ResponseHeaders.SaveToFile('~HTTPGetText_Response_Headers.txt');
      {$ENDIF}
    end;
  except
    on E: Exception do
      LogMesage(etError, 'Error while GET ('+AnUrl+') request: '+E.Message);
  end;
  Result:=FHTTPCode=200;
end;

function TInstagramParser.HTTPGetJSON(AnURL: string): TJSONObject;
var
  jsonParser: TJSONParser;
begin
  Result:=nil;
  if HTTPGetText(AnURL) then
  begin
    try
      jsonParser:=TJSONParser.Create(FResponse, DefaultOptions);
      try
        Result:=jsonParser.Parse as TJSONObject;
      finally
        jsonParser.Free;
      end;
    except
      on E: Exception do
        begin
          Result:=nil;
          LogMesage(etError, 'Error while parse JSON by URL ('+AnUrl+'): '+E.Message);
        end;
    end;
  end;
end;

function TInstagramParser.Login(Force: Boolean): Boolean;
var
  AFormData: TStrings;
begin
  if (FSessionUserName = EmptyStr) or (FSessionPassword = EmptyStr) then
  begin
    LogMesage(etError, 'Username or password not specified!');
    Exit(False);
  end;
  if Force or not IsLoggenIn(UserSession) then
  begin
    HTTPGetText(BASE_URL+'/');
    ExtractSharedData;
    ParseCookies(FHTTPClient, FUserSession);
    if FUserSession.Values['csrf_token']=EmptyStr then
      FUserSession.Values['csrf_token']:=Fcsrf_token
    else
      Fcsrf_token:=FUserSession.Values['csrf_token'];
    if (FHTTPClient.ResponseStatusCode<>200) then
      Exit(False);
    if FUserSession.IndexOfName('sessionid')>-1 then
      FUserSession.Delete(FUserSession.IndexOfName('sessionid'));
    generateHeaders(FUserSession, FrhxGis);
{$IFDEF DEBUG}     FHTTPClient.Cookies.SaveToFile('~cookies.txt');
    FHTTPClient.RequestHeaders.SaveToFile('~request_headers.txt'); {$ENDIF}
    AFormData:=TStringList.Create;
    AFormData.AddStrings(['username='+SessionUserName, 'password='+SessionPassword]);
    try
      FResponse:=FHTTPClient.FormPost(LOGIN_URL, AFormData);
    finally
      AFormData.Free;
    end;
    {$IFDEF DEBUG}
    StrToFile(FResponse, '~response.html');
    FHTTPClient.ResponseHeaders.SaveToFile('~response_header1.txt');
    {$ENDIF}
    if FHTTPClient.ResponseStatusCode<>200 then
      if FHTTPClient.ResponseStatusCode = 400 then        // todo
        Exit(False)
      else
        Exit(False);
    ParseCookies(FHTTPClient, FUserSession);
  end;
  Result:=True;
  FLogged:=True;
  generateHeaders(UserSession);
end;

function TInstagramParser.CheckLogin(): Boolean;
begin
  Result:=Logged;
  if not Result then
    Result:=Login();
end;

function TInstagramParser.GetPostUrl(APostID: String = ''): String;
begin
  if APostID=EmptyStr then
    APostID:=FShortcode;
  Result:=UrlFromShortcode(APostID);
end;

function TInstagramParser.getStories(AReel_ids: TJSONArray): TJSONArray;
var
  jsonResponse, variables: TJSONObject;
  edges, reels_media: TJSONArray;
  edge: TJSONEnum;
begin
  Result:=nil;
  variables:=TJSONObject.Create(['precomposed_overlay', False, 'reel_ids', TJSONArray.Create]);
  variables.CompressedJSON:=True;
  try
    if not Assigned(AReel_ids) then
    begin
      generateHeaders(userSession);
      jsonResponse:=HTTPGetJSON(getUserStoriesLink);
      if not Assigned(jsonResponse) then
        Exit;
      edges:=jsonResponse.FindPath('data.user.feed_reels_tray.edge_reels_tray_to_reel.edges') as TJSONArray;
      if not Assigned(edges) then
        Exit;
      for edge in edges do
        variables.Arrays['reel_ids'].Add((edge.Value as TJSONObject).Objects['node'].Int64s['id']);
      FreeAndNil(jsonResponse);
    end
    else
      variables.Arrays['reel_ids'] := AReel_ids.Clone as TJSONArray;
    generateHeaders(UserSession);
    jsonResponse:=HTTPGetJSON(getStoriesLink(variables));
    if not Assigned(jsonResponse) then
      Exit;
    {$IFDEF DEBUG}StrToFile(jsonResponse.FormatJSON, '~getStoriesLink.json');{$ENDIF}
    reels_media:=jsonResponse.Objects['data'].Arrays['reels_media'];
    {$IFDEF DEBUG}StrToFile(reels_media.FormatJSON, '~reels_media.json');{$ENDIF}
    if not Assigned(reels_media) then
      Exit;
    Result:=reels_media.Clone as TJSONArray;
  finally
    variables.Free;
    if Assigned(jsonResponse) then
      jsonResponse.Free;
  end;
end;

function TInstagramParser.getStoriesForUser(AUserID: Int64): TJSONArray;
var
  reel_ids, reels_media: TJSONArray;
begin
  Result:=nil;
  reel_ids:=TJSONArray.Create;
  if AUserID=0 then
    AUserID:=FUserID;
  reel_ids.Add(AUserID);
  try
    reels_media:=LoginNGetStories(reel_ids);
    try
      if Assigned(reels_media) then
        if reels_media.Count>0 then
          Result:=reels_media.Objects[0].Arrays['items'].Clone as TJSONArray;
    except
      Result:=nil;
    end;
  finally
    reels_media.Free;
    reel_ids.Free;
  end;
end;

function TInstagramParser.getHLStoriesForUser(AUserID: Int64): Tjson_HLStories;
begin
  Result:=nil;
  if AUserID=0 then
    AUserID:=FUserID;
  Result:=LoginNGetHLStories(AUserID);
end;

function TInstagramParser.getHLStoriesForUser_internal(AUserID: Int64): Tjson_HLStories;
var
  variables: TJSONObject;
  HLIds, HL_reels: TJSONArray;
  edge: TJSONEnum;
begin
  Result:=nil;
  variables:=TJSONObject.Create(['user_id', AUserID, 'include_chaining', False,
    'include_reel', True, 'include_suggested_users', False, 'include_logged_out_extras', False,
    'include_highlight_reels', True]);
  HL_reels:=getEdgesFromGraphQueryHash(QryHash_HLStoriesIDs, variables,
      'data.user.edge_highlight_reels.edges');
  variables.Free;
  if Assigned(HL_reels) then
  begin
    try
      HLIds:=TJSONArray.Create;
      for edge in HL_reels do
        HLIds.Add((edge.Value as TJSONObject).Objects['node'].Int64s['id']);
      variables:=TJSONObject.Create(['reel_ids', TJSONArray.Create,
        'tag_names', TJSONArray.Create, 'location_ids', TJSONArray.Create,
        'highlight_reel_ids', HLIds, 'precomposed_overlay', False]);
      Result:=getEdgesFromGraphQueryHash(QryHash_HLStories1, variables, 'data.reels_media');
      variables.Free;
    finally
      HL_reels.Free;
    end;
  end;
end;

function TInstagramParser.LoginNGetStories(reel_ids: TJSONArray): TJSONArray;
begin
  Result:=nil;
  if Login() then
    Result:=getStories(reel_ids)
  else
    LogMesage(etError, 'Failed to login');
end;

function TInstagramParser.LoginNGetHLStories(AUserID: Int64): Tjson_HLStories;
begin
  Result:=nil;
  if Login() then
    Result:=getHLStoriesForUser_internal(AUserID);
end;

function TInstagramParser.LoginNGetMediaCommentsByCode(ACount: Integer;
  AMaxID: Int64): Boolean;
begin
  Result:=False;
  if CheckLogin() then
    Result:=getMediaCommentsByCode(ACount, AMaxID);
end;

function TInstagramParser.PrivateInfoByID(AccountID: Int64): Boolean;
var
  jsonResponce: TJSONObject;
  AnUrl: String;
begin
  try
    Result:=False;
    AnUrl:=ReplaceStr(url_privateinfo_by_id, '{user_id}', IntToStr(AccountID));
    jsonResponce:=HTTPGetJSON(AnUrl) as TJSONObject;
    try
      jsonUser:=jsonResponce.Objects['user'].Clone as TJSONObject;
      if Assigned(jsonUser) then
        Result:=Parse_jsonUser(False);
      if Result then
        FUserID:=AccountID;
    finally
      jsonResponce.Free;
    end;
  except
    Result:=False;
  end;
end;

procedure TInstagramParser.SetUrlFromProfile(const Username: String);
begin
  FUsername:=Username;
  FUrl:=UrlFromUsername(FUsername);
end;

class function TInstagramParser.ThumbUrlFromMedias(ANode: TJSONObject): String;
var
  jArray: TJSONArray;
begin
  try
    if ANode.Find('thumbnail_resources', jArray) then
      Result:=ANode.Arrays['thumbnail_resources'].Objects[0].Strings['src']
    else
      Result:=ANode.Arrays['display_resources'].Objects[0].Strings['src'];
  except
    Result:='';
  end;
end;

function TInstagramParser.UrlFromUsername(const AUsername: String): String;
var
  S: String;
begin
  if AUsername=EmptyStr then
    S:=FUsername
  else
    S:=AUsername;
  Result:=InstgrmStart1+Trim(S)+'/';
end;

class function TInstagramParser.UrlFromShortcode(const AShortcode: String
  ): String;
begin
  Result:=InstgrmStart1+'p/'+AShortcode+'/';
end;

procedure TInstagramParser.withCredentials(const User, Password: String);
begin
  FSessionUserName:=User;
  FSessionPassword:=Password;
end;

end.

