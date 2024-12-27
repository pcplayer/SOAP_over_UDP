program SoapOverUDPTestClient;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Netel.Soap.NLRIO in '..\..\Netel.Soap.NLRIO.pas',
  Soap.SOAPLinked in '..\..\Soap.SOAPLinked.pas',
  IMyTest1 in 'IMyTest1.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
