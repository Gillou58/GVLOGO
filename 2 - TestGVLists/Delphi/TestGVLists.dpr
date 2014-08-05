program TestGVLists;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {MainFormGVLists},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Ruby Graphite');
  Application.CreateForm(TMainFormGVLists, MainFormGVLists);
  Application.Run;
end.
