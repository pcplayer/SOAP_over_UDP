{ Invokable implementation File for TMyTest which implements IMyTest }

unit MyTestImpl;

interface

uses Soap.InvokeRegistry, System.Types, Soap.XSBuiltIns, MyTestIntf;

type

  { TMyTest }
  TMyTest = class(TInvokableClass, IMyTest)
  public
    function echoEnum(const Value: TEnumTest): TEnumTest; stdcall;
    function echoDoubleArray(const Value: TDoubleArray): TDoubleArray; stdcall;
    function echoMyEmployee(const Value: TMyEmployee): TMyEmployee; stdcall;
    function echoDouble(const Value: Double): Double; stdcall;
    procedure SetLabel1Caption(const Value: string); stdcall;
  end;

implementation

uses Unit2;

function TMyTest.echoEnum(const Value: TEnumTest): TEnumTest; stdcall;
begin
  { TODO : Implement method echoEnum }
  Result := Value;
end;

function TMyTest.echoDoubleArray(const Value: TDoubleArray): TDoubleArray; stdcall;
begin
  { TODO : Implement method echoDoubleArray }
  Result := Value;
end;

function TMyTest.echoMyEmployee(const Value: TMyEmployee): TMyEmployee; stdcall;
begin
  { TODO : Implement method echoMyEmployee }
  Value.LastName := 'Li';
  Value.FirstName := 'James';
  Result := Value;
end;

procedure TMyTest.SetLabel1Caption(const Value: string);
begin
  Unit2.Form2.Label1.Caption := Value;
end;

function TMyTest.echoDouble(const Value: Double): Double; stdcall;
begin
  { TODO : Implement method echoDouble }
  Result := Value + 120;
end;


initialization
{ Invokable classes must be registered }
   InvRegistry.RegisterInvokableClass(TMyTest);
end.

