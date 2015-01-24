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
  ActnList, StdActns, ComCtrls,
  GVConsts, // constantes générales
  GVErrConsts, // constantes des erreurs
  GVAutomat; // interpréteur

type

  { TMainForm }

  TMainForm = class(TForm)
    CoolBarMain: TCoolBar;
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
    tbFile: TToolBar;
    tbFileNew: TToolButton;
    tbFileNewProc: TToolButton;
    tbFileSave: TToolButton;
    tbFilePrint: TToolButton;
    tbFileOpen: TToolButton;
    tbFileQuit: TToolButton;
    tbExec: TToolBar;
    tbExecInter: TToolButton;
    tbExecExec: TToolButton;
    tbExecWait: TToolButton;
    tbExecStop: TToolButton;
    tbExecClear: TToolButton;
    tbDiv2: TToolButton;
    tbExecFollow: TToolButton;
    tbDeepFollow: TToolButton;
    tbShowLocVars: TToolButton;
    tbShowProcs: TToolButton;
    tbShowPcks: TToolButton;
    tbShowCmdLine: TToolButton;
    tbShowEdit: TToolButton;
    tbShowTurtle: TToolButton;
    tbShowText: TToolButton;
    tbShowAll: TToolButton;
    tbDiv4: TToolButton;
    tbDiv3: TToolButton;
    tbShowVars: TToolButton;
    tbEdit: TToolBar;
    tbEditCut: TToolButton;
    tbEditCopy: TToolButton;
    tbEditPaste: TToolButton;
    tbDiv5: TToolButton;
    tbSearch: TToolButton;
    tbReplace: TToolButton;
    tbSelectAll: TToolButton;
    procedure ExecDeepFollowExecute(Sender: TObject);
    procedure ExecExecuteExecute(Sender: TObject);
    procedure ExecFollowExecute(Sender: TObject);
    procedure ExeStopExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure ShowCmdLineExecute(Sender: TObject);
    procedure ShowTextExecute(Sender: TObject);
    procedure ShowTurtleExecute(Sender: TObject);
  private
    fModified: Boolean; // drapeau de modification
    fDeepFollow: Boolean; // drapeau de trace approfondie
    fGVAutomat: TGVAutomat; // interpréteur
    // recherche d'une couleur normalisée
    function GetAColor(const AValue: TColor): TGVAutomatMessage;
    // conversion d'une couleur
    function SetAColor(const St: string): TColor;
    // recherche de fonte
    function GetFont: TGVAutomatMessage;
    // taille de la fonte
    function GetFontSize: TGVAutomatMessage;
  public
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec); // erreurs
    procedure GetMessage(Sender: TObject); // messages
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  GVPrimConsts, // primitives
  GVErrors, // constantes des erreurs
  FrmTurtle, // fiche de la tortue
  FrmText, // fiche du texte
  FrmEdit, // ligne de commande
  FrmError, // fiche des erreurs
  FrmInfo, // information
  FrmAbout; // boîte à propos

{ TMainForm }

procedure TMainForm.FileQuitExecute(Sender: TObject);
// *** demande de fermeture du logiciel ***
begin
  Close; // on tente de fermer la fenêtre principale
end;

procedure TMainForm.FormActivate(Sender: TObject);
// *** activation de la fenêtre principale ***
begin
  fGVAutomat.Turtle := TurtleForm.GVTurtle; // tortue liée à l'automate
end;

procedure TMainForm.ExecFollowExecute(Sender: TObject);
// *** trace ***
begin
  fGVAutomat.Follow := fGVAutomat.Follow; // on inverse sa valeur
end;

procedure TMainForm.ExeStopExecute(Sender: TObject);
// *** stop ***
begin
  fGVAutomat.Stop := True; // arrêt demandé
end;

procedure TMainForm.FileOpenAccept(Sender: TObject);
// *** ouverture d'un fichier ***
begin
  // exécute le chargement du fichier choisi
  fGVAutomat.Process(CBeginList + P_LoadAll + CBlank + CQuote +
    FileOpen.Dialog.FileName + CEndList);
  // pas d'erreur et interpréteur en attente ?
  if (fGVAutomat.Error.Ok) and (fGVAutomat.State = asWaiting) then
    FrmInfo.ShowInfoForm(Format(GVM_Load, [FileOpen.Dialog.FileName]));
end;

procedure TMainForm.ExecDeepFollowExecute(Sender: TObject);
// *** trace approfondie ***
begin
  fDeepFollow := not fDeepFollow; // on inverse le drapeau
  fGVAutomat.Follow := fDeepFollow; // interpréteur à jour
end;

procedure TMainForm.ExecExecuteExecute(Sender: TObject);
// *** interprétation ***
begin
  fGVAutomat.Clear; // nettoyage
  ExecExecute.Enabled := False; // action inactive
  try
    fGVAutomat.Process(CBeginList + FrmEdit.EditForm.EditCmdLine.Text +
      CEndList); // ligne à exécuter
  finally
    ExecExecute.Enabled := True; // action active
  end;
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
  fDeepFollow := False; // pas de trace approfondie
  Caption :=  CE_GVTitle + ' ' + CE_GVVersion + ' ' + CE_GVAuthor +
    ' ' + CE_GVDate;  // entête
  fGVAutomat := TGVAutomat.Create; // création de l'interpréteur
  fGVAutomat.Error.OnError := @GetError; // gestionnaire d'erreurs
  fGVAutomat.OnNewLine := @GetMessage; // gestionnaire de messages
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVAutomat.Free; // libération de l'interpréteur
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
// *** affichage de la boîte à propos ***
begin
  ShowAboutForm; // on montre la fiche
end;

procedure TMainForm.ShowCmdLineExecute(Sender: TObject);
// *** montre la ligne de commande ***
begin
  EditForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  EditForm.Show; // on la voit
end;

procedure TMainForm.ShowTextExecute(Sender: TObject);
// *** affichage de la fenêtre de texte ***
begin
  TextForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  TextForm.Show; // on la voit
end;

procedure TMainForm.ShowTurtleExecute(Sender: TObject);
// *** affichage de l'écran de la tortue ***
begin
  TurtleForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  TurtleForm.Show; // on la voit
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// *** gestion des erreurs ***
begin
  // préparation des données
  with fGVAutomat.Datas do
    ErrorForm.SetError((Sender as TGVErrors).ErrorMessage, ErrorRec.ErrItem,
      fLine, fItem, fPrim, fProc, fNum, fLevel, ErrorRec.ErrPos);
  // affichage de la fenêtre appropriée
  ErrorForm.ShowModal;
end;

procedure TMainForm.GetMessage(Sender: TObject);
// *** gestionnaire des messages ***
begin
  case fGVAutomat.Message.fCommand of
    // écriture avec retour chariot
    acWrite: TextForm.WriteTextLN(fGVAutomat.Message.fMessage);
    acClear: TextForm.Clear; // nettoyage
    acReadList: ; // fGVAutomat.Message := GetValue; ### TODO ###
    acConfirm: ; // fGVAutomat.Message := GetBool; ### TODO ###
    // écriture sans retour chariot
    acType: TextForm.WriteText(fGVAutomat.Message.fMessage);
    acReadChar: ; // ### TODO ###
    // styles de caractères
    acBold: TextForm.Bold := True;
    acUnderline: TextForm.Underline := True;
    acItalic: TextForm.Italic := True;
    acNoBold: TextForm.Bold := False;
    acNoUnderline: TextForm.Underline := False;
    acNoItalic: TextForm.Italic := False;
    // couleurs
    acColor: fGVAutomat.Message := GetAColor(TextForm.FontColor);
    acBackColor: fGVAutomat.Message := GetAColor(TextForm.BackColor);
    acSetColor: TextForm.FontColor := SetAColor(fGVAutomat.Message.fMessage);
    acSetBackColor: TextForm.BackColor :=
      SetAColor(fGVAutomat.Message.fMessage);
    // fontes
    acFont: fGVAutomat.Message := GetFont;
    acSetFont: TextForm.Font.Name := fGVAutomat.Message.fMessage;
    acSetFontSize: TextForm.FontSize := StrToInt(fGVAutomat.Message.fMessage);
    acFontSize: fGVAutomat.Message := GetFontSize;
  end;
end;

function TMainForm.GetAColor(const AValue: TColor): TGVAutomatMessage;
// *** recherche d'une couleur normalisée ***
var
  Li: Integer;
  Lr: Integer;
begin
  Lr := -1;
  for Li := 0 to sizeOf(CColors) do
    if CColors[Li] = AValue then // trouvée ?
    begin
      Lr := Li; // couleur enregistrée
      Break; // on arrête de boucler
    end;
  Result.fMessage := IntToStr(Lr);
end;

function TMainForm.SetAColor(const St: string): TColor;
// *** conversion d'une couleur ***
var
  Li: Integer;
begin
  Result := clBlack; // couleur noire par défaut
  if TryStrToInt(St, Li) then // on essaye de convertir en nombre
    if (Li >= 0) and (Li <= SizeOf(CColors)) then // dans les bornes autorisées ?
      Result := CColors[Li]; // renvoi de la couleur
end;

function TMainForm.GetFont: TGVAutomatMessage;
// *** recherche de la fonte en cours (texte) ***
begin
  Result.fMessage := TextForm.Font.Name;
end;

function TMainForm.GetFontSize: TGVAutomatMessage;
// *** recherche de la taille de la fonte en cours (texte ***
begin
  Result.fMessage := IntToStr(TextForm.FontSize);
end;

end.

