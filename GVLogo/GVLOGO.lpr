program GVLOGO;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, richmemopackage, main, FrmTurtle, GVAutomat, GVConsts,
  GVErrConsts, GVErrors, GVEval, GVKernel, GVLists, GVLocVars, GVPrimConsts,
  GVPropLists, GVStacks, GVTurtles, GVWords, FrmAbout, FrmError, FrmText, 
FrmEdit, FrmInfo, FrmProcs;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTurtleForm, TurtleForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.Run;
end.

