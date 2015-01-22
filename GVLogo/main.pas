{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche principale de GVLOGO              |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// MAIN - part of GVLOGO
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

unit main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, StdActns;

type

  { TMainForm }

  TMainForm = class(TForm)
    ShowAll: TAction;
    ShowPcks: TAction;
    ShowProcs: TAction;
    ShowLovVars: TAction;
    ShowVars: TAction;
    ShowText: TAction;
    ShowTurtle: TAction;
    ShowEdit: TAction;
    ShowCmdLine: TAction;
    HelpAbout: TAction;
    HelpPrims: TAction;
    ExecDeepFollow: TAction;
    ExecFollow: TAction;
    ExecClear: TAction;
    ExecInterpret: TAction;
    ExecWait: TAction;
    ExeStop: TAction;
    ExecExecute: TAction;
    MenuClear: TMenuItem;
    SearchNextTo: TAction;
    SearchPreviousTo: TAction;
    SearchNextEnd: TAction;
    SearchPreviousEnd: TAction;
    EditTextState: TAction;
    EditScreenState: TAction;
    EditTurtleState: TAction;
    EditWordSelect: TAction;
    EditLineSelect: TAction;
    EditUnIndent: TAction;
    EditIndent: TAction;
    EditRedo: TAction;
    FilePrint: TAction;
    FileClose: TAction;
    FileSave: TAction;
    FileNewProc: TAction;
    FileNew: TAction;
    FileQuit: TAction;
    ActionList: TActionList;
    EditCopy: TEditCopy;
    EditCut: TEditCut;
    EditPaste: TEditPaste;
    EditSelectAll: TEditSelectAll;
    EditUndo: TEditUndo;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;
    HelpOnHelp: THelpOnHelp;
    ImageListMenu: TImageList;
    MainMenu: TMainMenu;
    MenuFIle: TMenuItem;
    MenuEdit: TMenuItem;
    MenuExec: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItem1: TMenuItem;
    MenuIndent: TMenuItem;
    MenuIndentOff: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuInter: TMenuItem;
    MenuGO: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuHelp2: TMenuItem;
    MenuItem15: TMenuItem;
    MenuAbout: TMenuItem;
    MenuItem16: TMenuItem;
    MenuOpen: TMenuItem;
    MenuTextState: TMenuItem;
    MenuScreenState: TMenuItem;
    MenuTurtleState: TMenuItem;
    MenuWait: TMenuItem;
    MenuPrimsHelp: TMenuItem;
    MenuWin1: TMenuItem;
    MenuStop: TMenuItem;
    MenuShowCmdLine: TMenuItem;
    MenuShowAll: TMenuItem;
    MenuShowPck: TMenuItem;
    MenuShowProcs: TMenuItem;
    MenuShowLocVars: TMenuItem;
    MenuShowVars: TMenuItem;
    MenuShowText: TMenuItem;
    MenuShowTurtle: TMenuItem;
    MenuShowDebugger: TMenuItem;
    MenuShowEditor: TMenuItem;
    MenuNextEnd: TMenuItem;
    MenuPreviousEnd: TMenuItem;
    MenuPreviousTo: TMenuItem;
    MenuNextTo: TMenuItem;
    MenuReplace: TMenuItem;
    MenuSearchNext: TMenuItem;
    MenuSearch2: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuWord: TMenuItem;
    MenuSelectLine: TMenuItem;
    MenuSelectAll: TMenuItem;
    MenuPaste: TMenuItem;
    MenuItem2: TMenuItem;
    MenuCloseEditor: TMenuItem;
    MenuExit: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuDoAgain: TMenuItem;
    MenuItem7: TMenuItem;
    MenuCut: TMenuItem;
    MenuCopy: TMenuItem;
    MenuUndo: TMenuItem;
    MenuPrint: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuSave: TMenuItem;
    MenuNewProc: TMenuItem;
    MenuNew: TMenuItem;
    MenuWindows: TMenuItem;
    MenuSee: TMenuItem;
    MenuSearch: TMenuItem;
    SearchFind: TSearchFind;
    SearchFindNext: TSearchFindNext;
    SearchReplace: TSearchReplace;
    procedure FileQuitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ShowTurtleExecute(Sender: TObject);
  private
    fModified: Boolean; // drapeau de modification
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  GVConsts, // constantes
  FrmTurtle, // fiche de la tortue
  FrmAbout; // boîte à propos

{ TMainForm }

procedure TMainForm.FileQuitExecute(Sender: TObject);
// *** demande de fermeture du logiciel ***
begin
  Close; // on tente de fermer la fenêtre principale
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** travail avant la fermeture du logiciel ***
begin
  // vérification de l'enregistrement des modifications
  if fModified then
  begin
    // demande d'enregistrement des modifications
  end;
  // fermeture des fenêtres en cours
  CanClose := True; // fermeture autorisée
end;

procedure TMainForm.FormCreate(Sender: TObject);
// *** création de la fiche principale ***
begin
  fModified := False; // pas de modification
  Caption :=  CE_GVTitle + ' ' + CE_GVVersion + ' ' + CE_GVAuthor +
    ' ' + CE_GVDate;  // entête
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
// *** affichage de la boîte à propos ***
begin
  ShowAboutForm; // on montre la fiche
end;

procedure TMainForm.ShowTurtleExecute(Sender: TObject);
// *** affichage de l'écran de la tortue ***
begin
  TurtleForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  TurtleForm.Show; // on la voit
end;

end.

