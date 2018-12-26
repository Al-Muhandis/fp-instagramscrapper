unit fphttpclientbroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basehttpclient, fphttpclient;

type

  { TbFPHTTPClient }

  TbFPHTTPClient=class(TBaseHTTPClient)
  private
    FHTTPClient: TFPHTTPClient;
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

{ TbFPHTTPClient }

function TbFPHTTPClient.GetCookies: TStrings;
begin
  Result:=FHTTPClient.Cookies;
end;

function TbFPHTTPClient.GetHTTPProxyHost: String;
begin
  Result:=FHTTPClient.Proxy.Host;
end;

function TbFPHTTPClient.GetHTTPProxyPassword: String;
begin
  Result:=FHTTPClient.Proxy.Password;
end;

function TbFPHTTPClient.GetHTTPProxyPort: Word;
begin
  Result:=FHTTPClient.Proxy.Port;
end;

function TbFPHTTPClient.GetHTTPProxyUsername: String;
begin
  Result:=FHTTPClient.Proxy.UserName;
end;

function TbFPHTTPClient.GetRequestHeaders: TStrings;
begin
  Result:=FHTTPClient.RequestHeaders;
end;

function TbFPHTTPClient.GetResponseHeaders: TStrings;
begin
  Result:=FHTTPClient.ResponseHeaders;
end;

function TbFPHTTPClient.GetResponseStatusCode: Integer;
begin
  Result:=FHTTPClient.ResponseStatusCode;
end;

function TbFPHTTPClient.GetResponseStatusText: String;
begin
  Result:=FHTTPClient.ResponseStatusText;
end;

procedure TbFPHTTPClient.SetCookies(AValue: TStrings);
begin
  FHTTPClient.Cookies:=AValue;
end;

procedure TbFPHTTPClient.SetHTTPProxyHost(AValue: String);
begin
  FHTTPClient.Proxy.Host:=AValue;
end;

procedure TbFPHTTPClient.SetHTTPProxyPassword(AValue: String);
begin
  FHTTPClient.Proxy.Password:=AValue;
end;

procedure TbFPHTTPClient.SetHTTPProxyPort(AValue: Word);
begin
  FHTTPClient.Proxy.Port:=AValue;
end;

procedure TbFPHTTPClient.SetHTTPProxyUsername(AValue: String);
begin
  FHTTPClient.Proxy.UserName:=AValue;
end;

procedure TbFPHTTPClient.SetRequestHeaders(AValue: TStrings);
begin
  FHTTPClient.RequestHeaders:=AValue;
end;

procedure TbFPHTTPClient.AddHeader(const AHeader, AValue: String);
begin
  FHTTPClient.AddHeader(AHeader, AValue);
end;

constructor TbFPHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTTPClient:=TFPHTTPClient.Create(AOwner);
end;

destructor TbFPHTTPClient.Destroy;
begin
  FreeAndNil(FHTTPClient);
  inherited Destroy;
end;

class function TbFPHTTPClient.EncodeUrlElement(S: String): String;
begin
  Result:=fphttpclient.EncodeURLElement(S);
end;

function TbFPHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
begin
  Result:=FHTTPClient.FormPost(URL, FormData);
end;

function TbFPHTTPClient.Get(const AUrl: String): String;
begin
  Result:=FHTTPClient.Get(AUrl);
end;

end.

