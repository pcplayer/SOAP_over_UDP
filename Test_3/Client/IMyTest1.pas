// ************************************************************************ //
// The types declared in this file were generated from data read from the
// WSDL File described below:
// WSDL     : http://127.0.0.1:8080/wsdl/IMyTest
//  >Import : http://127.0.0.1:8080/wsdl/IMyTest>0
// Version  : 1.0
// (2022/5/19 10:30:58 - - $Rev: 103843 $)
// ************************************************************************ //

unit IMyTest1;

interface

uses Soap.InvokeRegistry, Soap.SOAPHTTPClient, System.Types, Soap.XSBuiltIns;

type

  // ************************************************************************ //
  // The following types, referred to in the WSDL document are not being represented
  // in this file. They are either aliases[@] of other types represented or were referred
  // to but never[!] declared in the document. The types from the latter category
  // typically map to predefined/known XML or Embarcadero types; however, they could also 
  // indicate incorrect WSDL documents that failed to declare or import a schema type.
  // ************************************************************************ //
  // !:string          - "http://www.w3.org/2001/XMLSchema"[Gbl]
  // !:double          - "http://www.w3.org/2001/XMLSchema"[Gbl]

  TMyEmployee          = class;                 { "urn:MyTestIntf"[GblCplx] }

  {$SCOPEDENUMS ON}
  { "urn:MyTestIntf"[GblSmpl] }
  TEnumTest = (etNone, etAFew, etSome, etAlot);

  {$SCOPEDENUMS OFF}



  // ************************************************************************ //
  // XML       : TMyEmployee, global, <complexType>
  // Namespace : urn:MyTestIntf
  // ************************************************************************ //
  TMyEmployee = class(TRemotable)
  private
    FLastName: string;
    FFirstName: string;
    FSalary: Double;
  published
    property LastName:  string  read FLastName write FLastName;
    property FirstName: string  read FFirstName write FFirstName;
    property Salary:    Double  read FSalary write FSalary;
  end;

  TDoubleArray = array of Double;               { "urn:MyTestIntf"[GblCplx] }

  // ************************************************************************ //
  // Namespace : urn:MyTestIntf-IMyTest
  // soapAction: urn:MyTestIntf-IMyTest#%operationName%
  // transport : http://schemas.xmlsoap.org/soap/http
  // style     : rpc
  // use       : encoded
  // binding   : IMyTestbinding
  // service   : IMyTestservice
  // port      : IMyTestPort
  // URL       : http://127.0.0.1:8080/soap/IMyTest
  // ************************************************************************ //
  IMyTest = interface(IInvokable)
  ['{025AEBCD-99EA-A97D-2269-3D0F4D2BC7DA}']
    function  echoEnum(const Value: TEnumTest): TEnumTest; stdcall;
    function  echoDoubleArray(const Value: TDoubleArray): TDoubleArray; stdcall;
    function  echoMyEmployee(const Value: TMyEmployee): TMyEmployee; stdcall;
    function  echoDouble(const Value: Double): Double; stdcall;
    procedure SetLabel1Caption(const Value: string); stdcall;
  end;

function GetIMyTest(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): IMyTest;


implementation
  uses System.SysUtils;

function GetIMyTest(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): IMyTest;
const
  defWSDL = 'http://127.0.0.1:8080/wsdl/IMyTest';
  defURL  = 'http://127.0.0.1:8080/soap/IMyTest';
  defSvc  = 'IMyTestservice';
  defPrt  = 'IMyTestPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as IMyTest);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  { IMyTest }
  InvRegistry.RegisterInterface(TypeInfo(IMyTest), 'urn:MyTestIntf-IMyTest', '');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(IMyTest), 'urn:MyTestIntf-IMyTest#%operationName%');
  RemClassRegistry.RegisterXSInfo(TypeInfo(TEnumTest), 'urn:MyTestIntf', 'TEnumTest');
  RemClassRegistry.RegisterXSClass(TMyEmployee, 'urn:MyTestIntf', 'TMyEmployee');
  RemClassRegistry.RegisterXSInfo(TypeInfo(TDoubleArray), 'urn:MyTestIntf', 'TDoubleArray');

end.