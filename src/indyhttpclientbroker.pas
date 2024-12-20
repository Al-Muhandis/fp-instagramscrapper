unit indyhttpclientbroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basehttpclient, IdHTTP, IdMultipartFormData,
  IdSSL, IdSSLOpenSSL;

type

  { TIndyHTTPClient }

  TIndyHTTPClient = class(TBaseHTTPClient)
  private
    FCookies: TStrings;
    FHTTPClient: TIdHTTP;
    FIDSSL: TIdSSLIOHandlerSocketOpenSSL;
    FData: TIdMultiPartFormDataStream;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
  protected
    function GetAllowRedirect: Boolean; override;
    function GetCookies: TStrings; override;
    function GetHTTPProxyHost: String; override;
    function GetHTTPProxyPassword: String; override;
    function GetHTTPProxyPort: Word; override;
    function GetHTTPProxyUsername: String; override;
    function GetInternalHTTPClient: TObject; override;
    function GetRequestHeaders: TStrings; override;
    function GetResponseHeaders: TStrings; override;
    function GetResponseStatusCode: Integer; override;
    function GetResponseStatusText: String; override;
    procedure SetAllowRedirect(AValue: Boolean); override;
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
  IdURI, IdException
  ;

{ TIndyHTTPClient }

procedure TIndyHTTPClient.AddHeader(const AHeader, AValue: String);
begin
  FRequestHeaders.Add(AHeader+': '+AValue);
end;

constructor TIndyHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTTPClient:=TIdHTTP.Create;
  FIDSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FHTTPClient.IOHandler:=FIDSSL;
  FIDSSL.ConnectTimeout:=10000;
  FIDSSL.ReadTimeout:=16000;
  FHTTPClient.ReadTimeout:=16000;
  FHTTPClient.ConnectTimeout:=10000;
//  FHTTPClient.CookieManager:=TIdCookieManager.Create(nil);
  FRequestHeaders:=TStringList.Create;
  FRequestHeaders.NameValueSeparator:=':';
  FResponseHeaders:=TStringList.Create;
  FResponseHeaders.NameValueSeparator:=':';
  FCookies:=TStringList.Create;
end;

destructor TIndyHTTPClient.Destroy;
begin
  FreeAndNil(FCookies);
  FreeAndNil(FIDSSL);
  FreeAndNil(FData);
//  FHTTPClient.CookieManager.Free;
//  FHTTPClient.CookieManager:=nil;
  FreeAndNil(FResponseHeaders);
  FreeAndNil(FRequestHeaders);
  FreeAndNil(FHTTPClient);
  inherited Destroy;
end;

class function TIndyHTTPClient.EncodeUrlElement(S: String): String;
begin
  Result:=TIdURI.URLEncode(S);
end;

function TIndyHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
var
  i: Integer;
  AData: TIdMultiPartFormDataStream;
begin
//  FHTTPClient.Document.Clear;
  FHTTPClient.Request.CustomHeaders.AddStrings(FRequestHeaders);
  FResponseHeaders.Clear;
  AData:=TIdMultiPartFormDataStream.Create;
  try
    for i:=0 to FormData.Count-1 do
      FData.AddFormField(FormData.Names[i], FormData.ValueFromIndex[i]);
    Result:=FHTTPClient.Post(URL, FData);
  finally
    FreeAndNil(AData);
  end;
  if FHTTPClient.ResponseCode=200 then
  begin
    FResponseHeaders.AddStrings(FHTTPClient.Response.CustomHeaders, True);
    {$IFDEF DEBUG}FResponseHeaders.SaveToFile('~ResponseHeaders.txt');{$ENDIF}
  end
  else begin
    Result:=EmptyStr;
  end;
end;

function TIndyHTTPClient.Get(const AUrl: String): String;
begin
  FHTTPClient.Request.CustomHeaders.AddStrings(FRequestHeaders);
  FResponseHeaders.Clear;
  try
    try
      Result:=FHTTPClient.Get(AUrl);
    except
      on E: EIdException do
        raise EHTTPClient.Create(E.ClassName+': '+E.Message);
    end;
  finally
    if FHTTPClient.ResponseCode=200 then
    begin
      FResponseHeaders.AddStrings(FHTTPClient.Response.CustomHeaders, True);
      {$IFDEF DEBUG}FResponseHeaders.SaveToFile('~ResponseHeaders.txt');{$ENDIF}
    end
    else
      Result:=EmptyStr;
  end;
end;

function TIndyHTTPClient.GetAllowRedirect: Boolean;
begin
  Result:=FHTTPClient.HandleRedirects;
end;

function TIndyHTTPClient.GetCookies: TStrings;
begin
  Result:=FCookies;
end;

function TIndyHTTPClient.GetHTTPProxyHost: String;
begin
  Result:=FHTTPClient.ProxyParams.ProxyServer;
end;

function TIndyHTTPClient.GetHTTPProxyPassword: String;
begin
  Result:=FHTTPClient.ProxyParams.ProxyPassword;
end;

function TIndyHTTPClient.GetHTTPProxyPort: Word;
begin
  Result:=FHTTPClient.ProxyParams.ProxyPort;
end;

function TIndyHTTPClient.GetHTTPProxyUsername: String;
begin
  Result:=FHTTPClient.ProxyParams.ProxyUsername;
end;

function TIndyHTTPClient.GetInternalHTTPClient: TObject;
begin
  Result:=FHTTPClient;
end;

function TIndyHTTPClient.GetRequestHeaders: TStrings;
begin
  Result:=FRequestHeaders;
end;

function TIndyHTTPClient.GetResponseHeaders: TStrings;
begin
  Result:=FResponseHeaders;
end;

function TIndyHTTPClient.GetResponseStatusCode: Integer;
begin
  Result:=FHTTPClient.ResponseCode;
end;

function TIndyHTTPClient.GetResponseStatusText: String;
begin
  Result:=FHTTPClient.ResponseText;
end;

procedure TIndyHTTPClient.SetAllowRedirect(AValue: Boolean);
begin
  FHTTPClient.HandleRedirects:=AValue;
end;

procedure TIndyHTTPClient.SetCookies(AValue: TStrings);
begin
  FCookies.AddStrings(AValue, True);
end;

procedure TIndyHTTPClient.SetHTTPProxyHost(AValue: String);
begin
  FHTTPClient.ProxyParams.ProxyServer:=AValue;
end;

procedure TIndyHTTPClient.SetHTTPProxyPassword(AValue: String);
begin
  FHTTPClient.ProxyParams.ProxyPassword:=AValue;
end;

procedure TIndyHTTPClient.SetHTTPProxyPort(AValue: Word);
begin
  FHTTPClient.ProxyParams.ProxyPort:=AValue;
end;

procedure TIndyHTTPClient.SetHTTPProxyUsername(AValue: String);
begin
  FHTTPClient.ProxyParams.ProxyUsername:=AValue;
end;

procedure TIndyHTTPClient.SetRequestHeaders(AValue: TStrings);
begin
  FRequestHeaders.Assign(AValue);
end;

end.

