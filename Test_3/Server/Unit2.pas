unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPServer, Vcl.StdCtrls, Netel.Soap.NLRIO, IdGlobal, IdSocketHandle;

type
  TForm2 = class(TForm)
    IdUDPServer1: TIdUDPServer;
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
  private
    { Private declarations }
    FRioServer: TNLRioServer;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses MyTestIntf;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
begin
  IdUDPServer1.Active := True;
end;

procedure TForm2.IdUDPServer1UDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  ARequest, AResponse: TMemoryStream;

  BResponse: TIdBytes;
  Intf: IMyTest;

  AStrStream: TStringStream;
begin
  //服务器端收到客户端的请求 UDP Package
  FRioServer := TNLRioServer.Create(nil);

  Intf := FRioServer as IMyTest;

  ARequest := TMemoryStream.Create;
  AResponse := TMemoryStream.Create;
  AStrStream := TStringStream.Create('');
  try
    ARequest.Write(AData[0], Length(AData));
    FRioServer.Execute(ARequest, AResponse); //这里是服务器端在执行远程调用并返回调用结果。正式代码，因为可能是一个耗时操作，这里需要使用多线程。

    SetLength(BResponse, AResponse.Size);
    AResponse.Position := 0;
    AResponse.Read(BResponse[0], AResponse.Size);

    IdUDPServer1.SendBuffer('127.0.0.1', 654, BResponse); //将服务器端的执行结果发送回客户端。

    AResponse.Position := 0;
    AStrStream.CopyFrom(AResponse, AResponse.Size);
    Memo1.Lines.Add(AStrStream.DataString);
  finally
    ARequest.Free;
    AResponse.Free;
  end;

end;

end.
