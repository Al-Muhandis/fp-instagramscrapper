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
    FRequestBody: TStream;
    FRequestHeaders: TStrings;
    FResponseHeaders: TStrings;
  protected
    procedure BeforeRequest;
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
    procedure SetAllowRedirect({%H-}AValue: Boolean); override;
    procedure SetCookies(AValue: TStrings); override;
    procedure SetHTTPProxyHost(AValue: String); override;
    procedure SetHTTPProxyPassword(AValue: String); override;
    procedure SetHTTPProxyPort(AValue: Word); override;
    procedure SetHTTPProxyUsername(AValue: String); override;
    procedure SetIOTimeout(AValue: Integer); override;
    procedure SetRequestBody(AValue: TStream); override;
    procedure SetRequestHeaders(AValue: TStrings); override;
    function HttpPostFile(const URL: string; FormData: TStrings; const AFieldName, AFileName: String;
      InputFileData: TStream; Response: TStream): Boolean;
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
  synacode, synautil, ssl_openssl, blcksock
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

procedure TSynapseHTTPClient.FileFormPost(const AURL: string; FormData: TStrings; AFieldName,
  AFileName: string; const Response: TStream);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    StreamFormPost(AURL, FormData, AFieldName, ExtractFileName(AFileName), F, Response);
  finally
    F.Free;
  end;
end;

function TSynapseHTTPClient.FormPost(const URL: string; FormData: TStrings
  ): String;
var
  Response: TStringList;
begin
  BeforeRequest;
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
  else begin
    Result:=EmptyStr;
    raise EHTTPClient.Create('HTTP client. '+FHTTPClient.ResultCode.ToString+' '+
      FHTTPClient.ResultString);
  end;
end;

function TSynapseHTTPClient.Get(const AUrl: String): String;
var
  Response: TStringList;
begin
  BeforeRequest;
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
  else begin
    Result:=EmptyStr;
    raise EHTTPClient.Create('HTTP client. '+FHTTPClient.ResultCode.ToString+' '+
      FHTTPClient.ResultString);
  end;
end;

function TSynapseHTTPClient.Post(const URL: string): String;
var
  Response: TStringList;
begin
  BeforeRequest;
  if Assigned(RequestBody) then
    FHTTPClient.Document.LoadFromStream(RequestBody);
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
  else begin
    Result:=EmptyStr;
    raise EHTTPClient.Create('HTTP client. '+FHTTPClient.ResultCode.ToString+' '+
      FHTTPClient.ResultString);
  end;
end;

procedure TSynapseHTTPClient.StreamFormPost(const AURL: string; FormData: TStrings;
  const AFieldName, AFileName: string; const AStream: TStream; const Response: TStream);
begin
  HttpPostFile(AURL, FormData, AFieldName, AFileName, AStream, Response);
end;

procedure TSynapseHTTPClient.BeforeRequest;
begin
  FHTTPClient.Document.Clear;
  FHTTPClient.Headers.AddStrings(FRequestHeaders, True);
  FResponseHeaders.Clear;
  FHTTPClient.Sock.SSL.SSLType:=LT_TLSv1_2;
end;

function TSynapseHTTPClient.{%H-}GetAllowRedirect: Boolean;
begin
  { todo }
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

function TSynapseHTTPClient.GetInternalHTTPClient: TObject;
begin
  Result:=FHTTPClient;
end;

function TSynapseHTTPClient.{%H-}GetIOTimeout: Integer;
begin
  Result:=FHTTPClient.Timeout;
end;

function TSynapseHTTPClient.GetRequestBody: TStream;
begin
  Result:=FRequestBody;
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

procedure TSynapseHTTPClient.SetAllowRedirect(AValue: Boolean);
begin
  { todo }
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

procedure TSynapseHTTPClient.SetIOTimeout(AValue: Integer);
begin
  FHTTPClient.Timeout:=AValue;
end;

procedure TSynapseHTTPClient.SetRequestBody(AValue: TStream);
begin
  FRequestBody:=AValue;
end;

procedure TSynapseHTTPClient.SetRequestHeaders(AValue: TStrings);
begin
  FRequestHeaders.Assign(AValue);
end;

function TSynapseHTTPClient.HttpPostFile(const URL: string; FormData: TStrings; const AFieldName,
  AFileName: String; InputFileData: TStream; Response: TStream): Boolean;
var
  Bound, N, V, S: string;
  I: Integer;
  SS: TStringStream;
begin
  BeforeRequest;
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  SS:=TStringStream.Create('');
  try
    if (FormData<>Nil) then
      for I:=0 to FormData.Count -1 do
      begin
        FormData.GetNameValue(I,N,V);
        S :='--'+Bound+CRLF;
        S:=S+Format('Content-Disposition: form-data; name="%s"'+CRLF+CRLF+'%s'+CRLF,[N, V]);
        SS.WriteBuffer(S[1],Length(S));
      end;
    S:='--'+Bound+CRLF;
    s:=s+Format('Content-Disposition: form-data; name="%s"; filename="%s"'+CRLF,[AFieldName,ExtractFileName(AFileName)]);
    s:=s+'Content-Type: application/octet-string'+CRLF+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    InputFileData.Seek(0, soFromBeginning);
    SS.CopyFrom(InputFileData, InputFileData.Size);
    S:=CRLF+'--'+Bound+'--'+CRLF;
    SS.WriteBuffer(S[1],Length(S));
    SS.Position:=0;

    FHTTPClient.Document.LoadFromStream(SS);
    FHTTPClient.MimeType := 'multipart/form-data; boundary=' + Bound;
    Result := FHTTPClient.HTTPMethod('POST', URL);
    if Result then
    begin
      FHTTPClient.Document.SaveToStream(Response);
      FResponseHeaders.AddStrings(FHTTPClient.Headers, True);
      {$IFDEF DEBUG}FResponseHeaders.SaveToFile('~ResponseHeaders.txt');{$ENDIF}
    end
    else
      raise EHTTPClient.Create('HTTP client. '+FHTTPClient.ResultCode.ToString+' '+
            FHTTPClient.ResultString);
  finally
    SS.Free;
  end;
end;

initialization
  TSynapseHTTPClient.UnregisterClientClass;
  TSynapseHTTPClient.RegisterClientClass;

end.
