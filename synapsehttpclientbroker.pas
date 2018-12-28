unit synapsehttpclientbroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basehttpclient, httpsend;

type

  { TSynapseHTTPClient }

  TSynapseHTTPClient = class(TBaseHTTPClient)
  private
    FHTTPClient: THTTPSend;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
  protected
    function GetCookies: TStrings; override;
    function GetHTTPProxyHost: String; override;
    function GetHTTPProxyPassword: String; override;
    function GetHTTPProxyPort: Word; override;
    function GetHTTPProxyUsername: String; override;
    function GetRequestHeaders: TStrings; override;
    function GetResponseHeaders: TStrings; override;
    function GetResponseStatusCode: Integer; override;
    function GetResponseStatusText: String; override;
    procedure SetCookies(AValue: TStrings); override;
    procedure SetHTTPProxyHost(AValue: String); override;
    procedure SetHTTPProxyPassword(AValue: String); override;
    procedure SetHTTPProxyPort(AValue: Word); override;
    procedure SetHTTPProxyUsername(AValue: String); override;
    procedure SetRequestHeaders(AValue: TStrings); override;
  public
    procedure AddHeader(const AHeader, AValue: String); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function EncodeUrlElement(S: String): String; override;
    function FormPost(const URL: string; FormData: TStrings): String; override;
    function Get(const AUrl: String): String; override;
  end;

implementation

uses
  synacode, synautil,
  ssl_openssl // ?
  ;

{ TSynapseHTTPClient }

procedure TSynapseHTTPClient.AddHeader(const AHeader, AValue: String);
begin
  FRequestHeaders.Add(AHeader+': '+AValue);
end;

constructor TSynapseHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTTPClient:=THTTPSend.Create;
  FRequestHeaders:=TStringList.Create;
  FRequestHeaders.NameValueSeparator:=':';
  FResponseHeaders:=TStringList.Create;
  FResponseHeaders.NameValueSeparator:=':';
end;

destructor TSynapseHTTPClient.Destroy;
begin
  FreeAndNil(FResponseHeaders);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FHTTPClient);
  inherited Destroy;
end;

class function TSynapseHTTPClient.EncodeUrlElement(S: String): String;
begin
  Result:=synacode.EncodeURLElement(S);
end;

function TSynapseHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
var
  Response: TStringList;
begin
  FHTTPClient.Document.Clear;
  FHTTPClient.Headers.AddStrings(FRequestHeaders, True);
  FResponseHeaders.Clear;
  FormData.SaveToStream(FHTTPClient.Document);
  if FHTTPClient.HTTPMethod('POST', URL) then
  begin
    Response:=TStringList.Create;
    try
      Response.LoadFromStream(FHTTPClient.Document);
      FResponseHeaders.AddStrings(FHTTPClient.Headers, True);
      Result:=Response.Text;
      {$IFDEF DEBUG}FResponseHeaders.SaveToFile('~ResponseHeaders.txt');{$ENDIF}
    finally
      Response.Free;
    end
  end
  else
    Result:=EmptyStr;
end;

function TSynapseHTTPClient.Get(const AUrl: String): String;
var
  Response: TStringList;
begin
  FHTTPClient.Document.Clear;
  FHTTPClient.Headers.AddStrings(FRequestHeaders, True);
  FResponseHeaders.Clear;
  if FHTTPClient.HTTPMethod('GET', AUrl) then
  begin
    Response:=TStringList.Create;
    try
      Response.LoadFromStream(FHTTPClient.Document);
      FResponseHeaders.AddStrings(FHTTPClient.Headers, True);
      Result:=Response.Text;
      {$IFDEF DEBUG}FResponseHeaders.SaveToFile('~ResponseHeaders.txt');{$ENDIF}
    finally
      Response.Free;
    end
  end
  else
    Result:=EmptyStr;
end;

function TSynapseHTTPClient.GetCookies: TStrings;
begin
  Result:=FHTTPClient.Cookies;
end;

function TSynapseHTTPClient.GetHTTPProxyHost: String;
begin
  Result:=FHTTPClient.ProxyHost;
end;

function TSynapseHTTPClient.GetHTTPProxyPassword: String;
begin
  Result:=FHTTPClient.ProxyPass;
end;

function TSynapseHTTPClient.GetHTTPProxyPort: Word;
begin
  Result:=StrToInt(FHTTPClient.ProxyPort);
end;

function TSynapseHTTPClient.GetHTTPProxyUsername: String;
begin
  Result:=FHTTPClient.ProxyUser;
end;

function TSynapseHTTPClient.GetRequestHeaders: TStrings;
begin
  Result:=FRequestHeaders;
end;

function TSynapseHTTPClient.GetResponseHeaders: TStrings;
begin
  Result:=FResponseHeaders;
end;

function TSynapseHTTPClient.GetResponseStatusCode: Integer;
begin
  Result:=FHTTPClient.ResultCode;
end;

function TSynapseHTTPClient.GetResponseStatusText: String;
begin
  Result:=FHTTPClient.ResultString;
end;

procedure TSynapseHTTPClient.SetCookies(AValue: TStrings);
begin
  FHTTPClient.Cookies.Clear;
  FHTTPClient.Cookies.AddStrings(AValue);
end;

procedure TSynapseHTTPClient.SetHTTPProxyHost(AValue: String);
begin
  FHTTPClient.ProxyHost:=AValue;
end;

procedure TSynapseHTTPClient.SetHTTPProxyPassword(AValue: String);
begin
  FHTTPClient.ProxyPass:=AValue;
end;

procedure TSynapseHTTPClient.SetHTTPProxyPort(AValue: Word);
begin
  FHTTPClient.ProxyPort:=IntToStr(AValue);
end;

procedure TSynapseHTTPClient.SetHTTPProxyUsername(AValue: String);
begin
  FHTTPClient.ProxyUser:=AValue;
end;

procedure TSynapseHTTPClient.SetRequestHeaders(AValue: TStrings);
begin
  FRequestHeaders.Assign(AValue);
end;

end.

