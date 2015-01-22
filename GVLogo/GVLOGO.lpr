program GVLOGO;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, main, FrmTurtle, GVAutomat, GVConsts, GVErrConsts,
  GVErrors, GVEval, GVKernel, GVLists, GVLocVars, GVPrimConsts, GVPropLists,
  GVStacks, GVTurtles, GVWords, FrmAbout, FrmError;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTurtleForm, TurtleForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.Run;
end.

