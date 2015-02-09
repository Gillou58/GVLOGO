{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Programme GVLOGO                        |
  |                  Unité : GVLOGO.lpr                                    |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVLOGO.LPR - GVLOGO
// Copyright (C) 2014-2015 Gilles VASSEUR
//
// This program is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation, either version 3 of the License,
// or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.
// If not, see <http://www.gnu.org/licenses/>.

{$I GVDefines.inc} // fichier des définitions préalables

program GVLOGO;

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, bgrabitmappack, richmemopackage, printer4lazarus, main,
  FrmTurtle, GVAutomat, GVConsts, GVErrConsts, GVErrors, GVEval, GVKernel,
  GVLists, GVLocVars, GVPrimConsts, GVPropLists, GVStacks, GVTurtles, GVWords,
  GVHighlighter, FrmAbout, FrmError, FrmText, FrmEdit, FrmInfo, FrmProcs,
  GVLogoConsts, FrmEditor, FrmFind, FrmpHelpPrims, FrmFollow, FrmDump, FrmVars,
  FrmLocVars, FrmPcks, FrmNewProc, FrmOptions, FrmTurtleShow, GetText,
  Translations; // traduction française de la LCL

{$R *.res}

procedure TranslateLCL;
// *** traduction ***
var
  Lang, DefLang: string;
begin
  GetLanguageIDs({%H-}Lang, {%H-}DefLang);
  // utilisation du fichier corrigé
  TranslateUnitResourceStrings('LCLStrConsts',
      '..\3rdparty\lclstrconsts.fr.po', Lang, DefLang);
end;

begin
  RequireDerivedFormResource := True;
  TranslateLCL; // traduction
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TTurtleForm, TurtleForm);
  Application.CreateForm(TErrorForm, ErrorForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TEditorForm, EditorForm);
  Application.CreateForm(TFindForm, FindForm);
  Application.CreateForm(TFollowForm, FollowForm);
  Application.CreateForm(TDumpForm, DumpForm);
  Application.CreateForm(TVarsForm, VarsForm);
  Application.CreateForm(TLocVarsForm, LocVarsForm);
  Application.CreateForm(TPcksForm, PcksForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TTurtleShowForm, TurtleShowForm);
  Application.Run;
end.

