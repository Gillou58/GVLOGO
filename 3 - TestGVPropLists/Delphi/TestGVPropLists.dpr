program TestGVPropLists;

uses
  Vcl.Forms,
  MainGVPropLists in 'MainGVPropLists.pas' {MainForm} ,
  Vcl.Themes,
  Vcl.Styles,
  GVPropLists in 'GVPropLists.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Auric');
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
