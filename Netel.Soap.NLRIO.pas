unit Netel.Soap.NLRIO;
{-------------------------------------------------------------------------------
  ���Ƿ�װ�� TLinkedRIO ����Ϊ RIO �ͻ��˵�һ�� RIO

  TNLRioClient ����ͨ���¼����SOAP�������ݣ�Ȼ�����������ͨ���������Ĺ����������� SOAP Ӧ��Ȼ����źš�

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
  ��Ϊ SOAP �ͻ��˵� RIO ��������� Request���ȴ����� Response
  ������¼�������� Stream ���͸���磬һ����ȥ���ͣ�һ���������յ����������ȥ��
  ���ԣ�����Ҫ�����½����ǲ���һ���¼���Ȼ��ȴ���������������������������á�
---------------------------------------------------------------------------}
  FStream := Response;
  if Assigned(FOnRequestEvent) then FOnRequestEvent(Self, Request);
  FEvent.WaitFor(120000); //2���ӣ����ڴ���Ӧ�ã�������Ҫ��Ϊ5��������10���ӡ�
  FEvent.ResetEvent;
  //if Resp.Size = 0 then raise Exception.Create('SOAP ����û�л��Ӧ��');
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
