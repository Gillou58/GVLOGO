program ProjectStack;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Golden Graphite');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
