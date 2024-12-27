unit Netel.Soap.NLRIO;
{-------------------------------------------------------------------------------
  这是封装了 TLinkedRIO 的作为 RIO 客户端的一个 RIO

  TNLRioClient 对象通过事件输出SOAP请求数据，然后阻塞；外界通过调用它的公共方法输入 SOAP 应答，然后给信号。

  pcplayer 2007-8-14
-------------------------------------------------------------------------------}

interface

uses Classes, SysUtils, Rio, TypInfo, WebNode, SOAPPasInv, IntfInfo, WSDLIntf, SyncObjs,
     SOAPAttachIntf, Soap.SOAPLinked;

type
  TNLRioRequestEvent = procedure(Sender: TObject; Request: TStream) of object;

  //add by pcplayer 2022-5-19
  TNLClientLinkedWebNode = class(TLinkedWebNode)
  private
    FOnRequestEvent: TOnRequestEvent;
  public
    procedure Execute(const Request: TStream; Response: TStream); override;
  published
    property NLOnRequestEvent: TOnRequestEvent read FOnRequestEvent write FOnRequestEvent;
  end;

  TNLRio = class(TLinkedRIO)
  public
    constructor CreateNetel(AOwner: TComponent; OnRequestEvent: TOnRequestEvent); overload;
  end;

  TNLRioClient = class(TNLRio) // class(TLinkedRIO) //
  private
    FEvent: TSimpleEvent;
    FStream: TStream;
    FOnRequestEvent: TNLRioRequestEvent;

    procedure DoOnRequest(Request, Response: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteSoapResponse(Response: TStream); overload;
    procedure ResetEvent;

    property NLOnRioRequest: TNLRioRequestEvent read FOnRequestEvent write FOnRequestEvent;
  end;

  TNLRioServer = class(TLinkedRIO)
  private
  public
    procedure Execute(const Request: TStream; Response: TStream);
  end;


implementation

uses Soap.OPConvert;

{ TNLRioClient }

constructor TNLRioClient.Create;
begin
  FEvent := TSimpleEvent.Create;
  FEvent.ResetEvent;

  inherited CreateNetel(nil, DoOnRequest);
end;

destructor TNLRioClient.Destroy;
begin
  //FEvent.ResetEvent;
  FEvent.Free;

  inherited;
end;

procedure TNLRioClient.DoOnRequest(Request, Response: TStream);
begin
{-------------------------------------------------------------------------
  作为 SOAP 客户端的 RIO 在这里输出 Request，等待输入 Response
  在这个事件里，把两个 Stream 都送给外界，一个拿去发送，一个让外界把收到的数据填进去。
  所以，这里要做的事仅仅是产生一个事件，然后等待。因此这个类仅仅是起到阻塞的作用。
---------------------------------------------------------------------------}
  FStream := Response;
  if Assigned(FOnRequestEvent) then FOnRequestEvent(Self, Request);
  FEvent.WaitFor(120000); //2分钟，对于大型应用，可能需要改为5分钟甚至10分钟。
  FEvent.ResetEvent;
  //if Resp.Size = 0 then raise Exception.Create('SOAP 请求没有获得应答。');
end;

procedure TNLRioClient.ResetEvent;
begin
  try
    if Assigned(FEvent) then
    begin
      FEvent.SetEvent;
      FEvent.ResetEvent;
    end;
  except
  end;
end;

procedure TNLRioClient.WriteSOAPResponse(Response: TStream);
begin
  Response.Position := 0;
  FStream.Position := 0;
  FStream.CopyFrom(Response, Response.Size);

  FEvent.SetEvent;
  //FEvent.ResetEvent;
end;



{ TNLClientLinkedWebNode }

procedure TNLClientLinkedWebNode.Execute(const Request: TStream;
  Response: TStream);
begin
  if Assigned(FOnRequestEvent) then FOnRequestEvent(Request, Response);
end;

{ TNLRio }

constructor TNLRio.CreateNetel(AOwner: TComponent;
  OnRequestEvent: TOnRequestEvent);
begin
  inherited Create(nil);

  if Assigned(FLinkedWebNode) then FLinkedWebNode.Free;

  Self.FLinkedWebNode := TNLClientLinkedWebNode.Create(nil);
  TNLClientLinkedWebNode(FLinkedWebNode).NLOnRequestEvent := OnRequestEvent;

  FLinkedWebNode.IntfInfo :=  IntfMD.Info;
  FWebNode := FLinkedWebNode as IWebNode;

  { Converter }
  FDOMConverter := GetDefaultConverter;
  FConverter := FDOMConverter as IOPConvert;
end;

{ TNLRioServer }

procedure TNLRioServer.Execute(const Request: TStream; Response: TStream);
begin
  Self.FLinkedWebNode.Execute(Request, Response);
end;

end.
