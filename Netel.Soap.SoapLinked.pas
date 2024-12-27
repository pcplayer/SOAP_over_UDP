unit Netel.Soap.SoapLinked;
  //完全复制 Soap.SoapLinked 然后做轻微修改，比较麻烦，需要复制的代码太多，容易出问题。
  //还是直接修改 soap.SoapLinked.pas 里面的代码比较容易。所以这个单元放弃。pcplayer 2022-5-20
interface

uses
  System.Classes, System.SysUtils, System.TypInfo,
  Soap.IntfInfo, Soap.InvokeRegistry, Soap.OPToSOAPDomConv, Soap.Rio,
  Soap.SOAPAttachIntf, Soap.SOAPPasInv, Soap.WebNode, Soap.WSDLIntf,
  Soap.SOAPLinked;

type
  TOnRequestEvent = procedure(Req, Resp: TStream) of object;

  //这个也不能从 TLinkedWebNode 继承，因为 TLinkedWebNode 的私有变量没有公开
  //因此，这里也只能复制它的代码。
  TNLClientLinkedWebNode = class(TComponent, IWebNode)
  private
    FOnRequestEvent: TOnRequestEvent;

    FInvoker: TSoapPascalInvoker;
    IntfInfo: PTypeInfo;
    FClass: TClass;
    FMimeBoundary: string;
    FWebNodeOptions: WebNodeOptions;
    FMethIntf: TIntfMethEntry;
    FStaticRequest: TBytes;
    FStaticResponse: TBytes;
  protected
    function  GetMimeBoundary: string;
    procedure SetMimeBoundary(const Value: string);

    function  GetWebNodeOptions: WebNodeOptions;
    procedure SetWebNodeOptions(Value: WebNodeOptions);

    function  GetResponseStream: TStream; virtual;
    procedure InvokeImplementation(const Request: TStream; Response: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   BeforeExecute(const IntfMetaData: TIntfMetaData;
                              const MethodMetaData: TIntfMethEntry;
                              MethodIndex: Integer;
                              AttachHandler: IMimeAttachmentHandler); virtual;

    //IWebNode
    procedure Execute(const Request: TStream; Response: TStream); overload; virtual;
    procedure Execute(const DataMsg: String; Resp: TStream); overload; virtual;
    function    Execute(const Request: TStream): TStream; overload;
  published
    property NLOnRequestEvent: TOnRequestEvent read FOnRequestEvent write FOnRequestEvent;

    property Invoker: TSoapPascalInvoker read FInvoker;
    property StaticRequest: TBytes read FStaticRequest write FStaticRequest;
    property StaticResponse: TBytes read FStaticResponse write FStaticResponse;
    property MimeBoundary: string read GetMimeBoundary write SetMimeBoundary;
  end;

  //直接复制 TLinkedRIO 而不是继承，因为没法继承。
  TNLRio = class(TRIO)
  private
    FLinkedWebNode: TNLClientLinkedWebNode;
    FDOMConverter:  TOPToSoapDomConvert;
    FDefaultConverter: TOPToSoapDomConvert;

    function  GetDomConverter: TOpToSoapDomConvert;
    procedure SetDomConverter(Value: TOPToSoapDomConvert);
    function  GetDefaultConverter: TOPToSoapDomConvert;
  protected
    function  GetResponseStream(BindingType: TWebServiceBindingType): TStream; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;

    constructor CreateNetel(AOwner: TComponent; OnRequestEvent: TOnRequestEvent); overload;
    destructor  Destroy; override;
    property    WebNode: TNLClientLinkedWebNode read FLinkedWebNode;
  published
    property Converter: TOPToSoapDomConvert read GetDomConverter write SetDOMConverter;
  end;

implementation

uses
  Soap.OPConvert, Soap.SOAPAttach, Soap.SOAPConst, Soap.WebServExp, Xml.XMLDoc;

{ TNLClientLinkedWebNode }

procedure TNLClientLinkedWebNode.BeforeExecute(
  const IntfMetaData: TIntfMetaData; const MethodMetaData: TIntfMethEntry;
  MethodIndex: Integer; AttachHandler: IMimeAttachmentHandler);
begin
  FMethIntf := MethodMetaData; //复制的代码
end;

constructor TNLClientLinkedWebNode.Create(AOwner: TComponent);
begin
  inherited;

  FInvoker := TSoapPascalInvoker.Create(nil); //复制的代码
end;

destructor TNLClientLinkedWebNode.Destroy;
begin
  FInvoker.Free;

  inherited;
end;

function TNLClientLinkedWebNode.Execute(const Request: TStream): TStream;
begin

end;

procedure TNLClientLinkedWebNode.Execute(const DataMsg: String; Resp: TStream);
begin

end;

procedure TNLClientLinkedWebNode.Execute(const Request: TStream;
  Response: TStream);
begin
  if Assigned(FOnRequestEvent) then FOnRequestEvent(Request, Response);
end;

function TNLClientLinkedWebNode.GetMimeBoundary: string;
begin
  Result := FMimeBoundary;
end;

function TNLClientLinkedWebNode.GetResponseStream: TStream;
begin
  Result := TMemoryStream.Create;
end;

function TNLClientLinkedWebNode.GetWebNodeOptions: WebNodeOptions;
begin
  Result := FWebNodeOptions;
end;

procedure TNLClientLinkedWebNode.InvokeImplementation(const Request: TStream;
  Response: TStream);
var
  BindingType: TWebServiceBindingType;
  XMLReq: TMemoryStream;
  AttachHandler: IMimeAttachmentHandler;
begin
  InvRegistry.GetClassFromIntfInfo(IntfInfo, FClass);
  if FClass = nil then
    raise Exception.CreateFmt(SNoClassRegistered, [IntfInfo.Name]);

  { Check what Input is expecting }
  BindingType := GetBindingType(FMethIntf, True);

  AttachHandler := nil;
  AttachHandler := GetMimeAttachmentHandler(BindingType);
  try
    { Create MIME stream if we're MIME bound }
    if (BindingType = btMIME) then
    begin
      XMLReq := TMemoryStream.Create;
      try
        FMimeBoundary := SBorlandMimeBoundary;
        AttachHandler.ProcessMultiPartForm(Request, XMLReq,
                                           FMimeBoundary, Nil,
                                           FInvoker.Converter.Attachments,
                                           FInvoker.Converter.TempDir);
        FInvoker.Invoke(FClass, IntfInfo, '', XMLReq, Response, BindingType);
      finally
        XMLReq.Free;
      end;
    end else
    begin
      FMimeBoundary := '';
      FInvoker.Invoke(FClass, IntfInfo, '', Request, Response, BindingType);
    end;
    if FInvoker.Converter.Attachments.Count > 0 then
    begin
      AttachHandler.CreateMimeStream(Response, FInvoker.Converter.Attachments);
      AttachHandler.FinalizeStream;
      FMimeBoundary := SBorlandMimeBoundary;
      Response.Position := 0;
      Response.CopyFrom(AttachHandler.GetMIMEStream, 0);
    end else
      FMimeBoundary := '';
  finally
    AttachHandler := nil;
  end;
end;

procedure TNLClientLinkedWebNode.SetMimeBoundary(const Value: string);
begin
  FMimeBoundary := Value;
end;

procedure TNLClientLinkedWebNode.SetWebNodeOptions(Value: WebNodeOptions);
begin
  FWebNodeOptions := Value;
end;

{ TNLRio }

constructor TNLRio.CreateNetel(AOwner: TComponent;
  OnRequestEvent: TOnRequestEvent);
begin
  Self.FLinkedWebNode := TNLClientLinkedWebNode.Create(nil);
  TNLClientLinkedWebNode(FLinkedWebNode).NLOnRequestEvent := OnRequestEvent;

  FLinkedWebNode.IntfInfo :=  IntfMD.Info;
  FWebNode := FLinkedWebNode as IWebNode;

  { Converter }
  FDOMConverter := GetDefaultConverter;
  FConverter := FDOMConverter as IOPConvert;

  inherited Create(AOwner);
end;

destructor TNLRio.Destroy;
begin
  FConverter := nil;
  FWebNode := nil;
  WebNode.Free;
  inherited;
end;

function TNLRio.GetDefaultConverter: TOPToSoapDomConvert;
begin
  if (FDefaultConverter = nil) then
  begin
    FDefaultConverter := TLinkedOPToSoapDomConvert.CreateLinked(Self, Self);
    FDefaultConverter.Name := 'Converter1';                 { do not localize }
    FDefaultConverter.SetSubComponent(True);
  end;
  Result := FDefaultConverter;
end;

function TNLRio.GetDomConverter: TOpToSoapDomConvert;
begin
  if not Assigned(FDOMConverter) then
  begin
    FDOMConverter := GetDefaultConverter;
    FConverter := FDOMConverter as IOPConvert;
  end;
  Result := FDOMConverter;
end;

function TNLRio.GetResponseStream(BindingType: TWebServiceBindingType): TStream;
begin
  Result := FLinkedWebNode.GetResponseStream;
end;

procedure TNLRio.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDomConverter) then
  begin
    FConverter := nil;
    FDomConverter := nil;
  end;
end;

function TNLRio.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
  FLinkedWebNode.IntfInfo := IntfMD.Info;
end;

procedure TNLRio.SetDomConverter(Value: TOPToSoapDomConvert);
begin
  if Assigned(FDOMConverter) and (FDomConverter.Owner = Self) then
  begin
    FConverter := nil;
    if FDomConverter <> FDefaultConverter then
      FDomConverter.Free;
  end;
  FDomConverter := Value;
  if Value <> nil then
  begin
    FConverter := Value as IOPConvert;
    FDomConverter.FreeNotification(Self);
  end;
end;

end.
