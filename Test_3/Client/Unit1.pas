unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent,
  IdComponent, IdUDPBase, IdUDPServer, Netel.Soap.NLRIO, {MyTestIntf,} IdGlobal,
  IdSocketHandle, IMyTest1;

type
  TForm1 = class(TForm)
    IdUDPServer1: TIdUDPServer;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    FRioClient: TNLRioClient;
    FIntf: IMyTest;

    procedure DoOnClientRequest3(Sender: TObject; Request: TStream);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses System.Threading, WinAPI.ActiveX;


{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Numb: Double;
begin
  if not Assigned(FRioClient) then
  begin
    FRioClient := TNLRioClient.Create;
    FRioClient.NLOnRioRequest := Self.DoOnClientRequest3;
  end;

  FIntf := FRioClient as IMyTest;

  Numb := StrToInt(Edit1.Text);

  TTask.Run(
    procedure
    begin
      CoInitialize(nil);
      try
        Numb := FIntf.echoDouble(Numb);

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

procedure TForm1.Button2Click(Sender: TObject);
begin
  IdUDPServer1.Active := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not Assigned(FRioClient) then
  begin
    FRioClient := TNLRioClient.Create;
    FRioClient.NLOnRioRequest := Self.DoOnClientRequest3;
  end;

  FIntf := FRioClient as IMyTest;

  TTask.Run(
    procedure
    begin
      CoInitialize(nil);
      try
        FIntf.SetLabel1Caption(Edit1.Text);

        TThread.Synchronize(nil,
          procedure
          begin
            Label1.Caption := '设置对方的 Label1 成功！';
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

procedure TForm1.IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  AResponse: TMemoryStream;
  AStrStream: TStringStream;
begin
  //客户端收到来自服务器端的 SOAP 执行结果的 UDP 包

  AResponse := TMemoryStream.Create;
  try
    AResponse.Write(AData[0], Length(AData));

    TThread.Synchronize(nil,
      procedure
      begin
        FRioClient.WriteSOAPResponse(AResponse);
      end
    )
  finally
    AResponse.Free;
  end;
end;

end.
