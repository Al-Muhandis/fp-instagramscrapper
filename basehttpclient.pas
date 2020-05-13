unit basehttpclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  EHTTPClient = class(Exception);

  TBaseClientClass = class of TBaseHTTPClient;

  { TBaseHTTPClient }

  TBaseHTTPClient=class(TComponent)
  private
    FUserAgent: String;
  protected
    function GetAllowRedirect: Boolean; virtual; abstract;
    function GetCookies: TStrings; virtual; abstract;
    function GetHTTPProxyHost: String; virtual; abstract;
    function GetHTTPProxyPassword: String; virtual; abstract;
    function GetHTTPProxyPort: Word; virtual; abstract;
    function GetHTTPProxyUsername: String; virtual; abstract;
    function GetInternalHTTPClient: TObject; virtual; abstract;
    function GetRequestBody: TStream; virtual; abstract;
    function GetRequestHeaders: TStrings; virtual; abstract;
    function GetResponseHeaders: TStrings; virtual; abstract;
    function GetResponseStatusCode: Integer; virtual; abstract;
    function GetResponseStatusText: String; virtual; abstract;
    function GetIOTimeout: Integer; virtual; abstract;
    procedure SetAllowRedirect(AValue: Boolean); virtual; abstract;
    procedure SetCookies(AValue: TStrings); virtual; abstract;
    procedure SetHTTPProxyHost(AValue: String); virtual; abstract;
    procedure SetHTTPProxyPassword(AValue: String); virtual; abstract;
    procedure SetHTTPProxyPort(AValue: Word); virtual; abstract;
    procedure SetHTTPProxyUsername(AValue: String); virtual; abstract;
    procedure SetRequestBody(AValue: TStream); virtual; abstract;
    procedure SetRequestHeaders(AValue: TStrings); virtual; abstract;
    procedure SetIOTimeout(AValue: Integer); virtual; abstract;
  public
    procedure AddHeader(Const AHeader,AValue : String); virtual; abstract;
    class function EncodeUrlElement(S: String): String; virtual; abstract;
    procedure FileFormPost(const AURL: string; FormData: TStrings; AFieldName, AFileName: string;
      const Response: TStream); virtual; abstract;
    function FormPost(const URL: string; FormData : TStrings): String; virtual; abstract;
    function Get(const AUrl: String): String; virtual; abstract;
    function Post(const URL: string) : String; virtual; abstract;
    procedure StreamFormPost(const AURL: string; FormData: TStrings;
      const AFieldName, AFileName: string; const AStream: TStream;
      const Response: TStream); virtual; abstract;
    class function GetClientClass: TBaseClientClass;
    class procedure RegisterClientClass;         
    procedure SaveHTTPHeaders(const aFile: String); virtual; abstract;
    class procedure UnregisterClientClass;
    property AllowRedirect: Boolean read GetAllowRedirect write SetAllowRedirect;
    property Cookies: TStrings read GetCookies write SetCookies;
    property RequestBody: TStream read GetRequestBody write SetRequestBody;
    property RequestHeaders: TStrings read GetRequestHeaders write SetRequestHeaders;
    property ResponseHeaders: TStrings read GetResponseHeaders;
    property ResponseStatusCode: Integer read GetResponseStatusCode;
    property ResponseStatusText: String read GetResponseStatusText;
    property HTTPProxyHost: String read GetHTTPProxyHost write SetHTTPProxyHost;
    property HTTPProxyPort: Word read GetHTTPProxyPort write SetHTTPProxyPort;
    property HTTPProxyUsername: String read GetHTTPProxyUsername write SetHTTPProxyUsername;
    property HTTPProxyPassword: String read GetHTTPProxyPassword write SetHTTPProxyPassword;
    Property IOTimeout : Integer read GetIOTimeout write SetIOTimeout;
    property InternalHTTPClient: TObject read GetInternalHTTPClient;
    property UserAgent: String read FUserAgent write FUserAgent;
  end;

implementation

var
  _BaseHTTPClientClass: TBaseClientClass = nil;

{ TBaseHTTPClient }

class procedure TBaseHTTPClient.RegisterClientClass;
begin
  if Assigned(_BaseHTTPClientClass) then
    raise EHTTPClient.Create('HTTP client class already registered!');
  _BaseHTTPClientClass := Self;
end;

class procedure TBaseHTTPClient.UnregisterClientClass;
begin
  _BaseHTTPClientClass := nil;
end;

class function TBaseHTTPClient.GetClientClass: TBaseClientClass;
begin
  if not Assigned(_BaseHTTPClientClass) then
    raise EHTTPClient.Create('No HTTP client class registered! Please use RegisterClientClass procedure');
  Result:=_BaseHTTPClientClass;
end;

end.
