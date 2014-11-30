program testfvinterpreter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, main, GVConsts, GVEval, GVKernel, GVLists, GVPropLists,
  GVStacks, GVTurtles2, GVInterpreter, GVWords;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

