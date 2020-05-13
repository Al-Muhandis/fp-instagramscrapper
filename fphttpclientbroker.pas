unit fphttpclientbroker;

{$mode objfpc}{$H+}

interface

{$IF FPC_FULLVERSION < 30300}{$DEFINE ExplSSL}{$else}{$DEFINE SSLOpenSockets}{$ENDIF}

uses
  Classes, SysUtils{$IFDEF ExplSSL}, ssockets{$ENDIF}, basehttpclient, fphttpclient;

type

  { TFCLHTTPClient }

  TFCLHTTPClient=class(TBaseHTTPClient)
  private
    FHTTPClient: TFPHTTPClient;
    procedure PrepareHeaders;   {$IFDEF ExplSSL}
    procedure HttpClientGetSocketHandler(Sender: TObject; const {%H-}UseSSL: Boolean;
      out {%H-}AHandler: TSocketHandler);{$ENDIF}
  protected
    function GetAllowRedirect: Boolean; override;
    function GetCookies: TStrings; override;
    function GetHTTPProxyHost: String; override;
    function GetHTTPProxyPassword: String; override;
    function GetHTTPProxyPort: Word; override;
    function GetHTTPProxyUsername: String; override;
    function GetInternalHTTPClient: TObject; override;
    function GetIOTimeout: Integer; override;
    function GetRequestBody: TStream; override;
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
    procedure SetIOTimeout(AValue: Integer); override;
    procedure SetRequestBody(AValue: TStream); override;
    procedure SetRequestHeaders(AValue: TStrings); override;
  public
    procedure AddHeader(const AHeader, AValue: String); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function EncodeUrlElement(S: String): String; override;
    procedure FileFormPost(const AURL: string; FormData: TStrings; AFieldName, AFileName: string;
      const Response: TStream); override;
    function FormPost(const URL: string; FormData: TStrings): String; override;
    function Get(const AUrl: String): String; override;
    function Post(const URL: string): String; override;
    procedure StreamFormPost(const AURL: string; FormData: TStrings; const AFieldName,
      AFileName: string; const AStream: TStream; const Response: TStream); override;
  end;

implementation

uses
  {$IFDEF ExplSSL}sslsockets, fpopenssl{$ENDIF}
  {$IFDEF SSLOpenSockets}opensslsockets{$endif}
  ;

{ TFCLHTTPClient }

procedure TFCLHTTPClient.PrepareHeaders;
begin
  if UserAgent<>EmptyStr then
    FHTTPClient.AddHeader('User-Agent', UserAgent);
end;
{$IFDEF ExplSSL}
procedure TFCLHTTPClient.HttpClientGetSocketHandler(Sender: TObject;
  const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  {$IFDEF LINUX}
    if UseSSL then begin
      AHandler:=TSSLSocketHandler.Create;
      TSSLSocketHandler(AHandler).SSLType:=stTLSv1_2;  // <--
    end;
  {$ENDIF}
end;
{$ENDIF}
function TFCLHTTPClient.GetAllowRedirect: Boolean;
begin
  Result:=FHTTPClient.AllowRedirect;
end;

function TFCLHTTPClient.GetCookies: TStrings;
begin
  Result:=FHTTPClient.Cookies;
end;

function TFCLHTTPClient.GetHTTPProxyHost: String;
begin
  Result:=FHTTPClient.Proxy.Host;
end;

function TFCLHTTPClient.GetHTTPProxyPassword: String;
begin
  Result:=FHTTPClient.Proxy.Password;
end;

function TFCLHTTPClient.GetHTTPProxyPort: Word;
begin
  Result:=FHTTPClient.Proxy.Port;
end;

function TFCLHTTPClient.GetHTTPProxyUsername: String;
begin
  Result:=FHTTPClient.Proxy.UserName;
end;

function TFCLHTTPClient.GetInternalHTTPClient: TObject;
begin
  Result:=FHTTPClient;
end;

function TFCLHTTPClient.GetIOTimeout: Integer;
begin
  Result:=FHTTPClient.IOTimeout;
end;

function TFCLHTTPClient.GetRequestBody: TStream;
begin
  Result:=FHTTPClient.RequestBody;
end;

function TFCLHTTPClient.GetRequestHeaders: TStrings;
begin
  Result:=FHTTPClient.RequestHeaders;
end;

function TFCLHTTPClient.GetResponseHeaders: TStrings;
begin
  Result:=FHTTPClient.ResponseHeaders;
end;

function TFCLHTTPClient.GetResponseStatusCode: Integer;
begin
  Result:=FHTTPClient.ResponseStatusCode;
end;

function TFCLHTTPClient.GetResponseStatusText: String;
begin
  Result:=FHTTPClient.ResponseStatusText;
end;

procedure TFCLHTTPClient.SetAllowRedirect(AValue: Boolean);
begin
  FHTTPClient.AllowRedirect:=AValue;
end;

procedure TFCLHTTPClient.SetCookies(AValue: TStrings);
begin
  FHTTPClient.Cookies:=AValue;
end;

procedure TFCLHTTPClient.SetHTTPProxyHost(AValue: String);
begin
  FHTTPClient.Proxy.Host:=AValue;
end;

procedure TFCLHTTPClient.SetHTTPProxyPassword(AValue: String);
begin
  FHTTPClient.Proxy.Password:=AValue;
end;

procedure TFCLHTTPClient.SetHTTPProxyPort(AValue: Word);
begin
  FHTTPClient.Proxy.Port:=AValue;
end;

procedure TFCLHTTPClient.SetHTTPProxyUsername(AValue: String);
begin
  FHTTPClient.Proxy.UserName:=AValue;
end;

procedure TFCLHTTPClient.SetIOTimeout(AValue: Integer);
begin
  FHTTPClient.IOTimeout:=AValue;
end;

procedure TFCLHTTPClient.SetRequestBody(AValue: TStream);
begin
  FHTTPClient.RequestBody:=AValue;
end;

procedure TFCLHTTPClient.SetRequestHeaders(AValue: TStrings);
begin
  FHTTPClient.RequestHeaders:=AValue;
end;

procedure TFCLHTTPClient.AddHeader(const AHeader, AValue: String);
begin
  FHTTPClient.AddHeader(AHeader, AValue);
end;

constructor TFCLHTTPClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHTTPClient:=TFPHTTPClient.Create(AOwner);
  {$IFDEF ExplSSL}FHTTPClient.OnGetSocketHandler:=@HttpClientGetSocketHandler;{$ENDIF}
end;

destructor TFCLHTTPClient.Destroy;
begin
  FreeAndNil(FHTTPClient);
  inherited Destroy;
end;

class function TFCLHTTPClient.EncodeUrlElement(S: String): String;
begin
  Result:=fphttpclient.EncodeURLElement(S);
end;

procedure TFCLHTTPClient.FileFormPost(const AURL: string; FormData: TStrings; AFieldName,
  AFileName: string; const Response: TStream);
begin
  FHTTPClient.FileFormPost(AURL, FormData, AFieldName, AFileName, Response);
end;

function TFCLHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
begin
  PrepareHeaders;
  Result:=FHTTPClient.FormPost(URL, FormData);
end;

function TFCLHTTPClient.Get(const AUrl: String): String;
begin
  PrepareHeaders;
  Result:=FHTTPClient.Get(AUrl);
end;

function TFCLHTTPClient.Post(const URL: string): String;
begin
  PrepareHeaders;
  Result:=FHTTPClient.Post(URL);
end;

procedure TFCLHTTPClient.StreamFormPost(const AURL: string; FormData: TStrings; const AFieldName,
  AFileName: string; const AStream: TStream; const Response: TStream);
begin
  FHTTPClient.StreamFormPost(AURL, FormData, AFieldName, AFileName, AStream, Response);
end;

initialization
  TFCLHTTPClient.UnregisterClientClass;
  TFCLHTTPClient.RegisterClientClass;

end.

