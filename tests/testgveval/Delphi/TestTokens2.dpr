program TestTokens2;

uses
  Vcl.Forms,
  main in 'main.pas' {Form1},
  gvtokens2 in '..\Tokens2\gvtokens2.pas',
  GVConsts in '..\..\..\units\GVConsts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
