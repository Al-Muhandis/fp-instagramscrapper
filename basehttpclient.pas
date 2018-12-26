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
  protected
    function GetCookies: TStrings; virtual; abstract;
    function GetHTTPProxyHost: String; virtual; abstract;
    function GetHTTPProxyPassword: String; virtual; abstract;
    function GetHTTPProxyPort: Word; virtual; abstract;
    function GetHTTPProxyUsername: String; virtual; abstract;
    function GetRequestHeaders: TStrings; virtual; abstract;
    function GetResponseHeaders: TStrings; virtual; abstract;
    function GetResponseStatusCode: Integer; virtual; abstract;
    function GetResponseStatusText: String; virtual; abstract;
    procedure SetCookies(AValue: TStrings); virtual; abstract;
    procedure SetHTTPProxyHost(AValue: String); virtual; abstract;
    procedure SetHTTPProxyPassword(AValue: String); virtual; abstract;
    procedure SetHTTPProxyPort(AValue: Word); virtual; abstract;
    procedure SetHTTPProxyUsername(AValue: String); virtual; abstract;
    procedure SetRequestHeaders(AValue: TStrings); virtual; abstract;
  public
    procedure AddHeader(Const AHeader,AValue : String); virtual; abstract;
    class function EncodeUrlElement(S: String): String; virtual; abstract;
    function FormPost(const URL: string; FormData : TStrings): String; virtual; abstract;
    function Get(const AUrl: String): String; virtual; abstract;
    class function GetClientClass: TBaseClientClass;
    class procedure RegisterClientClass;
    class procedure UnregisterClientClass;
    property Cookies: TStrings read GetCookies write SetCookies;
    property RequestHeaders: TStrings read GetRequestHeaders write SetRequestHeaders;
    property ResponseHeaders: TStrings read GetResponseHeaders;
    property ResponseStatusCode: Integer read GetResponseStatusCode;
    property ResponseStatusText: String read GetResponseStatusText;
    property HTTPProxyHost: String read GetHTTPProxyHost write SetHTTPProxyHost;
    property HTTPProxyPort: Word read GetHTTPProxyPort write SetHTTPProxyPort;
    property HTTPProxyUsername: String read GetHTTPProxyUsername write SetHTTPProxyUsername;
    property HTTPProxyPassword: String read GetHTTPProxyPassword write SetHTTPProxyPassword;
  end;

implementation

var
  _BaseHTTPClientClass: TBaseClientClass = nil;

{ TBaseHTTPClient }

class procedure TBaseHTTPClient.RegisterClientClass;
begin
  if Assigned(_BaseHTTPClientClass) then
    raise EHTTPClient.Create('HTTP client class already regitered!');
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

