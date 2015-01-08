program TestGVPropLists;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, GVConsts, GVLists, GVPropLists, GVWords, GVErrors;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainFormGVPropLists, MainFormGVPropLists);
  Application.Run;
end.

