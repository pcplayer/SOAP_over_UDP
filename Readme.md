# SOAP over UDP 的架构

## 首先
假设你知道如何在 Delphi 里面，创建 WebService 的服务器端和客户端，并理解它如何工作。
Delphi 提供的 WebService 框架，通讯层是基于 HTTP 的。
本项目对 Delphi 的 WebService 框架，进行简单的修改，让它能够支持 UDP 或者任何其它的通讯方式。

### 概念
拦截 Delphi 的 WebService 框架的通讯数据的接收和发送，然后通过 UDP 传输。

## 服务器端：

### 服务器端 SOAP 服务的基本调用方法

```
Request, Response: TMemoryStream;

FRIO := TLinkSrvRIO.Create(nil);

FRIO.Execute(Request, Response); //服务器端执行客户端请求，输入是来自客户端的 Request，输出是服务器端返回给客户端的 Response

```
这里的 TLinkSrvRIO 是我自己写的：

```
  TLinkSrvRIO = class(TLinkedRIO)
  public
    procedure   Execute(const Request: TStream; Response: TStream);
  end;

  procedure TLinkSrvRIO.Execute(const Request: TStream; Response: TStream);
  begin
    Self.FLinkedWebNode.Execute(Request, Response);
  end;
```
  
而上述 TLinkedRIO 是 D7 原本就有的：
```
  TLinkedRIO = class(TRIO)
  private
    FLinkedWebNode: TLinkedWebNode;
  protected
    function  GetResponseStream(BindingType: TWebServiceBindingType): TStream; override;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    constructor Create(AOwner: TComponent);  overload; override;
    constructor CreateFile(AOwner: TComponent; ReqFile, RespFile: string); overload;
    constructor CreateUDP(OnRequestEvent: TOnRequestEvent); overload;

    //procedure   Execute(const Request: TStream; Response: TStream);

    destructor  Destroy; override;
    property    WebNode: TLinkedWebNode read FLinkedWebNode;
  end;
```
里面的 TLinkedWebNode 也是 D7 原本就有的：
```
  TLinkedWebNode = class(TComponent, IWebNode)
  private
    FInvoker: TSoapPascalInvoker;
    IntfInfo: PTypeInfo;
    FClass: TClass;
    FMimeBoundary: string;
    FMethIntf: TIntfMethEntry;
  protected
    function  GetMimeBoundary: string;
    procedure SetMimeBoundary(Value: string);
    function  GetResponseStream: TStream; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    { IWebNode }
    procedure   BeforeExecute(const IntfMetaData: TIntfMetaData;
                              const MethodMetaData: TIntfMethEntry;
                              MethodIndex: Integer;
                              AttachHandler: IMimeAttachmentHandler); virtual;
    procedure   Execute(const DataMsg: String; Resp: TStream); overload; virtual;
    procedure   Execute(const Request: TStream; Response: TStream); overload; virtual;
    function Execute(const Request: TStream): TStream; overload;
    property Invoker: TSoapPascalInvoker read FInvoker;
    property MimeBoundary: string read GetMimeBoundary write SetMimeBoundary;
  end;
```
 上述代码都在 D7 原本的 SOAPLinked.pas 里面
 
 因此，服务器端比较简单，就是直接调用 TLinkedRIO 内部的 **Self.FLinkedWebNode.Execute(Request, Response);** 就算是执行完服务器端实现的方法，同时搞定输入和输出。
 TLinkSrvRIO = class(TLinkedRIO) 这样继承，仅仅是因为 TLinkedRIO 没有公开 Excute 方法。
 
 ### 服务器端对于客户端并发请求的支持
 
 TNLSOAPServerList 这个玩意，用一个 List 来作为线程池，存放多个 TNLSOAPServerThread 线程。
 因此，客户端的请求，丢给 TNLSOAPServerList，它从线程池取一个线程出来，让线程去执行真正的服务器端方法。也就是上面写的执行 FRIO.Execute(Request, Response); 这个方法。
 
 线程里面要执行 FRIO.Execute 方法之前，需要告诉 RIO 究竟是执行哪个接口方法（服务器端可能有实现多个不同的接口），必须要先执行：
 IntfTest := FRIO as IUDPTest; -- 这句话只是让 FRIO 知道自己要执行哪个接口的方法。取得这个 IntfTest 变量，最终释放掉，IntfTest := nil；并没有调用这个接口变量。
 
 服务器端的线程类只有一个，多实例是同一个类。那么，它究竟应该执行哪个接口？这里是通过线程类的公开方法：
 function WriteSOAPData(const PeerNumb: LongWord; const CMD: TSOAPClientCMD; const SoapRequest: string): Boolean;
 上述方法里面的 CMD 告诉线程用哪个接口。这个 CMD 实际上就是网络通讯的命令值。这样就把命令值转换为了执行不同的接口方法。
 上述方法，同时让线程类的实例记住当前执行的命令是来自哪个 Peer，最终输出时，也要输出 PeerNumb，然后输出的数据才会知道发送回给哪个 Peer；
 
 这里的线程类，如果采用新版 Delphi 的异步方法，可能会节约非常多的代码。
 
 
 
 ## 客户端
 
 客户端作为通讯的发起者，首先执行一个接口方法：
```
  SoClient := SoClientList.GetSOAPClient;
  SoClient.NLMainHandle := Self.Handle;
  SoClient.NLOnSendData := DoSendData;
  SoClient.echoMyEmployee(Obj); //接口方法
```
 接口的获取：
   **function TNLSOAPClientList.GetSOAPClient: IUDPTestClient;**
   
   上述方法，是从接口缓冲池里面取一个，如果没有，则创建：
```
   Obj := TNLSOAPClient.Create(nil);
```
   而 TNLSOAPClient 这个类，内部有一个 FThread: TNLSOAPClientThread; 线程。真正执行，是由线程去执行。
```
  procedure TNLSOAPClient.echoMyEmployee(const PeerNumb: LongWord;
  const Value: TMyEmployee);
  begin
    FThread.FPeerNumb := PeerNumb;
    FThread.FInterfaceNumb := niIUDPTest;
    FThread.FFunctionNumb := 1;
    FThread.FMyEmployee := Value;
    FThread.Resume; // 这里启动线程
  end;
```
在 procedure TNLSOAPClientThread.Execute; 里面真正执行的代码是：
```
  FRIO := TNLRioClient.Create;
  FRIO.NLOnRioRequest := Self.DoOnRIORequest; //客户端接口方法调用的输出，这个输出要拿去作为服务器端的输入。
  IntfTest := FRIO as IUDPTest;
  IntfTest.echoMyEmployee(FMyEmployee);  //这里的 FMyEmpoloyy 是调用客户端接口方法的输入的参数;
```
  那么，客户端接口方法被调用，它的输出是要发送给客户端去执行真正的调用的。它的输出是：
```
  procedure TNLSOAPClientThread.DoOnRIORequest(Sender: TObject; Request: TStream);
```
  FRIO.NLOnRioRequest 定义在 NLRIO.pas 里面
```
  TNLRioClient = class(TLinkedRIO)
  private
    FEvent: TSimpleEvent;
    FStream: TStream;
    FOnRequestEvent: TNLRioRequestEvent;

    procedure DoOnRequest(AStream, Resp: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteSOAPResponse(const Response: string);overload;
    procedure WriteSoapResponse(Response: TStream); overload;
    procedure ResetEvent;

    property NLOnRioRequest: TNLRioRequestEvent read FOnRequestEvent write FOnRequestEvent;
  end;
```
  真正的客户端调用的输出数据就是发给服务器端的 Request，绑定输出数据的事件在：

```
  constructor TLinkedRIO.CreateUDP(OnRequestEvent: TOnRequestEvent); //这里的 OnRequestEvent 就是输出数据的事件绑定
begin
  FLinkedWebNode := TClientLinkedWebNode.Create(nil);
  TClientLinkedWebNode(FLinkedWebNode).NLOnRequestEvent := OnRequestEvent;

  FLinkedWebNode.IntfInfo :=  IntfMD.Info;
  FWebNode := FLinkedWebNode as IWebNode;
  FConverter := TOPToSoapDomConvert.Create(nil) as IOpConvert;

  inherited Create(nil);
end;
```
  实际上，这里绑定的还是我写的那个类：
```
  TClientLinkedWebNode = class(TLinkedWebNode)
  private
    FOnRequestEvent: TOnRequestEvent;
  public
    procedure Execute(const Request: TStream; Response: TStream); override;
  published
    property NLOnRequestEvent: TOnRequestEvent read FOnRequestEvent write FOnRequestEvent;
  end;

  procedure TClientLinkedWebNode.Execute(const Request: TStream;
  Response: TStream);
begin
  if Assigned(FOnRequestEvent) then FOnRequestEvent(Request, Response);  //到这里，就把输入的 Request 变成了输出的 Response 了；
  //inherited;
end;
```
#### 注意 TNLRioClient
```
procedure TNLRioClient.WriteSOAPResponse(const Response: string);
begin
  if Length(Response) > 0 then
  begin
    FStream.Position := 0;
    FStream.Write(Response[1], Length(Response));
  end;

  FEvent.SetEvent;   //收到来自服务器端的 Response，解除阻塞，让 DoOnRequest 方法继续往下执行。因此，WriteSOAPResponse 这个方法必须是其它线程（通讯数据进来的线程）调用。
  //FEvent.ResetEvent; //因此，客户端接口方法的调用时一个线程，而进来的通讯数据的推动，调用 TNLRioClient.WriteSOAPResponse 的是另外一个线程。
end;
```
#### 客户端的调用顺序流程
```
FRIO := TNLRioClient.Create;
IntfTest := FRIO as IUDPTest;
FMyEmployeeReturn := IntfTest.echoMyEmployee(FMyEmployee);
```
接口方法被调用，因此就调用到：TNLRioClient 继承自 TLinkedRIO，
而 TLinkedRIO 我做了修改，内部的 FLinkedWebNode: TLinkedWebNode; 在创建时
```
FLinkedWebNode := TClientLinkedWebNode.Create(nil); 创建的是我专门做的 TClientLinkedWebNode
```
因此，客户端的接口方法调用时，最终会调用到 TClientLinkedWebNode.Excute 方法；这个方法内部，是 Delphi 内部的东西，看不到代码了，这方法最终就是输入 Rqeuest, 输出 Response；
最终就通过事件，调用到了 procedure TNLRioClient.DoOnRequest(AStream, Resp: TStream);
而上述方法，里面，则取出 Request 的数据，发送给服务器，等待服务器返回数据；收到返回数据，把它当作 Response 输入进去。客户端的接口方法调用完成。

# SOAP over UDP Delphi 10.4.2 版架构

首先，不应该修改 TLinkedRIO，而是应该继承它。在继承的类里面，创建内部的 FLinkedWebNode: TLinkedWebNode; 实例时，选择创建 TClientLinkedWebNode = class(TLinkedWebNode) 这个类。当然，这个类也是新增加的。

其次，继承的类，放到一个单独的单元。这样就避免了去修改 Delphi 原本的 SoapLinked.pas 单元。但是，看起来，因为 TLinkedRIO 内部的 FLinkedWebNode 是私有的，在另外一个单元，无法改写它。

看起来，还是只好改写 Delphi 原本的单元了。

## 仍然要修改 Delphi 原本的单元。
该单元在 D7 里面是 SoapLinked.pas，在 Delphi 10.4.2 里面是 Soap.SOAPLinked.pas

要修改它的原因是，我需要使用到它内部私有的 FLinkedWebNode: TLinkedWebNode; 如果在新的单元里面继承，无法访问到这个私有的对象。

修改 Soap.SOAPLinked.pas 并没有修改它原来的代码，仅仅是增加了 3 个类。没有在其它单元增加而是修改这个 pas 文件的理由就是需要调用那个私有的对象。
增加的三个单元是：
```
  TNLClientLinkedWebNode = class(TLinkedWebNode)
  TNLRio = class(TLinkedRIO)
  TNLRioServer = class(TLinkedRIO)
```
另外，新建的一个单元是：**Netel.Soap.NLRIO.pas；**


## 可以在 Delphi 10.4.2. 下运行的基于包通讯的 SOAP 的代码架构说明
### 目标： 
1. 客户端调用 SOAP 接口方法，SOAP 框架输出 Request 的内容，让应用层获得这个内容；
2. 应用层获得 Request 内容后，可以在服务器端，将 Request 的内容输入到 SOAP 框架里面，让 SOAP 框架自动执行接口函数调用的函数实现类，然后将函数执行的结果作为 Response，输出给应用层；
3. 应用层获得服务器端的 Response 后，将其内容传输给客户端，客户端收到 Response 以后，将其从 SOAP 框架输入，让客户端完成接口方法的调用。

### 实现：
1. Delphi 的 SOAP 框架的代码，最外层我们可以输入和输出数据的地方，是 Soap.SOAPLinked.pas；
2. 客户端调用一个 SOAP 接口方法，具体执行的是 TLinkedWebNode.Excute；因此，我们要从这里获取数据。
3. 这个单元里面，还有一个 TLogLinkedWebNode = class(TLinkedWebNode)，它的代码是执行 SOAP 接口方法的时候，将数据写入文件作为 LOG；
4. 学习这个 TLogLinkedWebNode，我们自己做一个 TNLClientLinkedWebNode，给它增加一个事件，在事件里面将客户端的 Request 的数据，输出给应用层； 
5. TLinkedRIO 包装了 FLinkedWebNode: TLinkedWebNode; 提供了两个 constructor，constructor Create(AOwner: TComponent); 和 constructor CreateFile，在 constructor CreateFile 里面，它为 FLinkedWebNode 创建的实例是 TLogLinkedWebNode；
6.学习上述做法，给 TLinkedRIO 增加一个新的 constructor CreateNetel(AOwner: TComponent; OnRequestEvent: TOnRequestEvent); overload; 在这里，应用层可以把事件对应的方法放进来给到 TNLClientLinkedWebNode。
6.1. 当 TLinkedRIO.FLinkedWebNode.Execute 执行时，实际上是 TNLClientLinkedWebNode 在执行，而 TNLClientLinkedWebNode.Exectue 里面，调用事件方法，将 Request 送给绑定这个事件的应用层的方法。
7. 架构问题：为啥修改 TLinkedRIO 而不是继承它，是因为：
7.1. FLinkedWebNode 是它私有的，要修改它，只能在同一个单元里做。在另外一个单元，无法修改它；
7.2. 如果不修改 TLinkedRIO 而是继承它，TNLRio = class(TLinkedRIO)，则继承的类的 constructor Create 里面，的确可以把 FLinkedWebNode 实例化为 TNLClientLinkedWebNode，但不能 inherited Create(nil)，因为会调用父类 TLinkedRIO 的 create 而它的这个 Create 里面，又会重新将 FLinkedWebNode 创建一个 TLinkedWebNode 的实例；但是如果不 inherited 就会导致 TLinkedRIO 的父类 TRIO 的 Create 不能被执行，最终导致客户端调用 SOAP 接口，当获得服务器端的 Response 时，执行完毕内部自动释放（SOAP 框架自己内部的功能）时，会出现 AV 错误。TLinkedRIO 自己的 Create 里面是 inherited Create 了的，也就是调用了其父类的 Create 的。因此，这里的问题其实是，如果继承，新的 TNLRio 是 TRIO 的孙类，语法上需要它能跳过调用父类的 Create，又能调用到其父类的父类 TRIO 的 Create。如果语法上做不到，就只能直接修改 TLinkedRIO 类。
7.3. 对以上问题的解决，目前暂时是修改 TLinkedRIO 类。
8. 增加一个 Netel.Soap.NLRIO.pas 单元，里面有一个 TNLRioClient = class(TLinkedRIO) 也就是用于 SOAP 客户端调用的类。
8.1. 这个类解决一个问题：实现前面提到的事件对应的方法 procedure DoOnRequest(Request, Response: TStream);
8.2. 上述方法里面，将 Response 赋予这个类私有的 FStream ，本质是记住这次调用中，SOAP 框架需要的 Response 的指针，收到来自服务器端的 Response 数据时，可以把数据写入 SOAP 框架需要的 Response 对象。
8.3 因为需要等待服务器端的应答，这里阻塞等待（可以试试不阻塞会不会有问题）
9. TNLRioClient 同时实现一个写入 Response 数据的方法给应用层调用。应用层收到来自服务器端的 Response 数据，调用 procedure WriteSoapResponse(Response: TStream); 方法，将数据输入给 SOAP 客户端框架，完成客户端的接口方法调用过程；
10. 服务器端：服务端非常简单，将 TLinkedRIO 私有的 FLinkedWebNode.Execute 暴露出来，让应用层可以输入 Request 数据并获得 Response 数据。代码：
```
  TNLRioServer = class(TLinkedRIO)
  private
  public
    procedure Execute(const Request: TStream; Response: TStream);
  end;
```
#### 使用注意
1. 客户端如同普通的 THTTPRIO 一样使用。
1.1. 客户端执行一次接口调用后，其 RIO 对象会被自动释放掉。因此，每次调用，都需要创建一次。
2. 服务器端：服务器端在创建了 FRioServer := TNLRioServer.Create(nil); 对象实例后，需要做一次接口转换，Intf := FRioServer as IMyTest; 当然获得的接口 Intf 本身这里不会用到，但这样当写入 Request 数据时，服务器端才知道应该去调用哪个接口。至于接口里面的方法，在 Request 的内容里面了。

## 用途：
随便什么通讯方式都能使用 Delphi 的 SOAP 远程调用框架，而不会仅仅只是 HTTP；


### 架构问题：
这里修改了 Delphi 原本的 TLinkedRIO, 为了不修改它，可能需要另外做一个全新的类，大部分代码直接复制它的，而不是继承它 -- 因为没法继承。--- 试做了一下，发现因为架构上的不合理，导致基本上要复制所有代码。那就不如直接修改 Soap.SoapLinked.pas 单元了。

### 到目前为止，通过修改 Soap.SoapLinked.pas 单元，达到目的。初步测试通过。测试程序是两个独立的程序，通过 UDP 互相通讯。

## 最终修改方案

### 既然要修改 Delphi 自己的 Soap.SOAPLinked.pas：
那么，只修改里面的两个类有关继承的时候在另外一个单元看不见的私有段的变量和函数，挪到 Protected 段，则可以在另外一个新的单元里面做继承新的类来实现我要的功能，而不是把新的继承类写在 Soap.SOAPLinked.pas 里面。这样对这个单元的改动是最小的；

### 继承 TNLRio = class(TLinkedRIO) 的 Inherited Create 的问题
其实非常简单，因为  TLinkedRIO.Create 会创建 FLinkedWebNode，所以把这个 inherited 放到最前面，先执行 TLinkedRIO.Create 然后把它创建的 FLinkedWebNode 释放掉，再创建我自己的。问题完美解决。

### 最终代码框架
1. 对 Soap.SoapLinked.pas 只做简单修改，也就是将几个需要的变量和函数，从 Private 段，挪到 Protected 段，让另外一个单元里面的继承类能看见；
2. Netel.Soap.NLRIO.pas 单元，有 4 个我自己写的类，其实就是继承后做一点点改变，然后封装，使得我可以插入事件，截取 Request 和 Response，以及将收到的 Response 写入客户端 RIO。
3. 综上，实现在应用层包通讯方式的 SOAP 远程调用，核心部分只需要 2 个单元，一个 Soap.SoapLinked.pas 是 Delphi 本身的，需要做一点点修改；另外一个是 Netel.Soap.NLRIO.pas

###以上修改方案，测试通过。2022-5-20

## 服务器端的使用
在正式代码里面，当有网络命令进来，调用服务器端，大概代码：
```
FRioServer := TNLRioServer.Create(nil);

  Intf := FRioServer as IMyTest;

  ARequest := TMemoryStream.Create;
  AResponse := TMemoryStream.Create;
  AStrStream := TStringStream.Create('');
  try
    ARequest.Write(AData[0], Length(AData));
    FRioServer.Execute(ARequest, AResponse); //这里是服务器端在执行远程调用并返回调用结果。正式代码，因为可能是一个耗时操作，这里需要使用多线程。
  finally
    //释放前面创建的 Stream;
  end;
```
这里，不能让处理进来的数据命令包的工作线程来直接调用，因为 SOAP 调用可能比较慢，阻塞工作线程不好。
旧版的 Netel 是在这里封装了一个线程类和一个线程 List 缓冲池。当有命令过来，就从 List 里面取一个线程来执行；并且线程内部，有 CASE 命令，根据命令来知道需要取哪个接口，如果服务器端有多个接口的话。

新的代码，可以考虑直接把 FRioServer.Execute 这个耗时的操作，放到一个 TTask.Run 里面，也就等于丢给一个独立的线程去操作了。代码比旧版就会简单很多。
当然，这个 TTask.Run 里面，还需要输入 TPeer 以及发送命令的接口函数，让它获得 SOAP 执行结果后，可以将命令再发回去。

## 客户端调用方式：
```
procedure TForm1.Button1Click(Sender: TObject);
var
  Numb: Double;
begin
  FRioClient := TNLRioClient.Create;
  FRioClient.NLOnRioRequest := Self.DoOnClientRequest3; //调用接口方法，产生的 Request XML 文档，从这个事件输出；

  FIntf := FRioClient as IMyTest;

  Numb := StrToInt(Edit1.Text);

  TTask.Run(
    procedure
    begin
      CoInitialize(nil);
      try
        Numb := FIntf.echoDouble(Numb);  //调用接口方法，内部被阻塞，直到有 Response 进来。

        TThread.Synchronize(nil,
          procedure
          begin
            Label1.Caption := Numb.ToString;
          end
        );
      finally
        CoUninitialize;
      end;
    end
  );
end;
 
procedure TForm1.DoOnClientRequest3(Sender: TObject; Request: TStream);
var
  ARequest: TIdBytes;
begin
  //SOAP 客户端接口方法调用产生的事件，客户端方法调用在这里输出要发给服务器端的 Request

  Request.Position := 0;
  SetLength(ARequest, Request.Size);
  Request.Read(ARequest[0], Request.Size);

  IdUDPServer1.SendBuffer('127.0.0.1', 664, ARequest);
end;

// 有 Response 进来，从这里输入：

FRioClient.WriteSOAPResponse(AResponse);
```

## 最新测试过的 Delphi 版本
Delphi 11 CE Version.
