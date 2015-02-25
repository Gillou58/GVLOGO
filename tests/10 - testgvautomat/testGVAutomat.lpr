program testGVAutomat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, main, GVAutomat, GVTurtles, GVConsts, GVErrConsts,
  GVErrors, GVEval, GVKernel, GVLists, GVLocVars, GVPrimConsts, GVPropLists,
  GVStacks, GVWords
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

