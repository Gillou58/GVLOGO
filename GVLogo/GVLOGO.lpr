program GVLOGO;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, richmemopackage, main, FrmTurtle, GVAutomat, GVConsts,
  GVErrConsts, GVErrors, GVEval, GVKernel, GVLists, GVLocVars, GVPrimConsts,
  GVPropLists, GVStacks, GVTurtles, GVWords, GVHighlighter, FrmAbout, FrmError,
  FrmText, FrmEdit, FrmInfo, FrmProcs, GVLogoConsts, FrmEditor, FrmFind, 
FrmpHelpPrims;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTurtleForm, TurtleForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TEditorForm, EditorForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.Run;
end.

