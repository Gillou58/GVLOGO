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
  // *** TMainForm ***

  { TMainForm }

  TMainForm = class(TForm)
    SearchNext: TAction;
    SearchReplace: TAction;
    SearchFind: TAction;
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
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure EditCopyExecute(Sender: TObject);
    procedure EditCutExecute(Sender: TObject);
    procedure EditIndentExecute(Sender: TObject);
    procedure EditLineSelectExecute(Sender: TObject);
    procedure EditPasteExecute(Sender: TObject);
    procedure EditRedoExecute(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure EditUndoExecute(Sender: TObject);
    procedure EditUnIndentExecute(Sender: TObject);
    procedure EditWordSelectExecute(Sender: TObject);
    procedure ExecClearExecute(Sender: TObject);
    procedure ExecDeepFollowExecute(Sender: TObject);
    procedure ExecExecuteExecute(Sender: TObject);
    procedure ExecFollowExecute(Sender: TObject);
    procedure ExecInterpretExecute(Sender: TObject);
    procedure ExeStopExecute(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileQuitExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure SearchFindUpdate(Sender: TObject);
    procedure SearchNextExecute(Sender: TObject);
    procedure SearchNextUpdate(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure ShowCmdLineExecute(Sender: TObject);
    procedure ShowProcsExecute(Sender: TObject);
    procedure ShowProcsUpdate(Sender: TObject);
    procedure ShowTextExecute(Sender: TObject);
    procedure ShowTurtleExecute(Sender: TObject);
  private
    fDeepFollow: Boolean; // drapeau de trace approfondie
    fGVAutomat: TGVAutomat; // interpréteur
    // recherche d'une couleur normalisée
    function GetAColor(const AValue: TColor): string;
    // conversion d'une couleur
    function SetAColor(const St: string): TColor;
  public
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec); // erreurs
    procedure GetMessage(Sender: TObject); // messages
    property Automat: TGVAutomat read fGVAutomat write fGVAutomat;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  {%H-}StrUtils, // chaînes
  GVPrimConsts, // primitives
  GVErrors, // constantes des erreurs
  GVLogoConsts, // constantes du projet
  FrmTurtle, // fiche de la tortue
  FrmText, // fiche du texte
  FrmEdit, // ligne de commande
  FrmError, // fiche des erreurs
  FrmInfo, // information
  FrmProcs, // procédures
  FrmEditor, // édition
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
  Automat.Turtle := TurtleForm.GVTurtle; // tortue liée à l'automate
end;

procedure TMainForm.ExecFollowExecute(Sender: TObject);
// *** trace ***
begin
  Automat.Follow := Automat.Follow; // on inverse sa valeur
end;

procedure TMainForm.ExecInterpretExecute(Sender: TObject);
// *** interprétation ***
var Err: Integer;
begin
  if Automat.Kernel.EditToProc(EditorForm.SynEditEditor.Lines, 0, 0, Err) then
    FrmInfo.ShowInfoForm(CrsInterpreter); // information affichée si OK
end;

procedure TMainForm.ExeStopExecute(Sender: TObject);
// *** stop ***
begin
  Automat.Stop := True; // arrêt demandé
end;

procedure TMainForm.FileOpenAccept(Sender: TObject);
// *** ouverture d'un fichier ***
begin
  // ### confirmation TODO ###
  // exécute le chargement du fichier choisi
  Automat.Process(CBeginList + P_LoadAll + CBlank + CQuote +
    FileOpen.Dialog.FileName + CEndList);
  // pas d'erreur et interpréteur en attente ?
  if (Automat.Error.Ok) and (Automat.State = asWaiting) then
  begin
    if Automat.Kernel.AllProcsToEdit(FrmEditor.EditorForm.SynEditEditor.Lines)
      then
      begin
        // nom de fichier en titre
        EditorForm.Caption := FileOpen.Dialog.FileName;
        EditorForm.ShowOnTop; // éditeur en vue
        // boîte d'information
        FrmInfo.ShowInfoForm(Format(CrsLoad, [FileOpen.Dialog.FileName]));
      end;
  end;
end;

procedure TMainForm.ExecDeepFollowExecute(Sender: TObject);
// *** trace approfondie ***
begin
  fDeepFollow := not fDeepFollow; // on inverse le drapeau
  Automat.Follow := fDeepFollow; // interpréteur à jour
end;

procedure TMainForm.ExecClearExecute(Sender: TObject);
// *** nettoyage de l'interpréteur ***
begin
  // ### confirmation TODO ###
  Automat.ClearAll;
end;

procedure TMainForm.EditUndoExecute(Sender: TObject);
// *** défaire (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.Undo;
end;

procedure TMainForm.EditUnIndentExecute(Sender: TObject);
// *** retirer l'indentation (éditeur) ***
begin
  // ### TODO ###
end;

procedure TMainForm.EditWordSelectExecute(Sender: TObject);
// *** sélectionner le mot en cours (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.SelectWord;
end;

procedure TMainForm.EditRedoExecute(Sender: TObject);
// *** refaire (éditeur) ***
begin
  EditorForm.SynEditEditor.Redo;
end;

procedure TMainForm.EditSelectAllExecute(Sender: TObject);
// *** tout sélectionner (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.SelectAll;
end;

procedure TMainForm.EditIndentExecute(Sender: TObject);
// *** indentation (éditeur) ***
begin
  // ### TODO ###
end;

procedure TMainForm.EditLineSelectExecute(Sender: TObject);
// *** sélectionner la ligne (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.SelectLine();
end;

procedure TMainForm.EditPasteExecute(Sender: TObject);
// *** coller (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.PasteFromClipboard;
end;

procedure TMainForm.EditCutExecute(Sender: TObject);
// *** couper (éditeur) ***
begin
  EditorForm.SynEditEditor.CutToClipboard;
end;

procedure TMainForm.EditCopyExecute(Sender: TObject);
// *** copier (éditeur) ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.SynEditEditor.CopyToClipboard;
end;

procedure TMainForm.SearchFindExecute(Sender: TObject);
// *** recherche dans l'éditeur ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skFind); // on cherche
end;

procedure TMainForm.EditUndoUpdate(Sender: TObject);
// *** activation/ désactivation de défaire ***
begin
  EditUndo.Enabled := EditorForm.SynEditEditor.CanUndo; // possible ?
end;

procedure TMainForm.EditRedoUpdate(Sender: TObject);
// *** activation/ désactivation de refaire ***
begin
  EditRedo.Enabled := EditorForm.SynEditEditor.CanRedo; // possible ?
end;

procedure TMainForm.ExecExecuteExecute(Sender: TObject);
// *** interprétation ***
begin
  Automat.Clear; // nettoyage
  ExecExecute.Enabled := False; // action inactive
  try
    ShowTextExecute(nil); // on montre la fenêtre de texte
    ShowTurtleExecute(nil); // on montre la fenêtre de la tortue
    Automat.Process(CBeginList + FrmEdit.EditForm.EditCmdLine.Text +
      CEndList); // ligne à exécuter
  finally
    ExecExecute.Enabled := True; // action active
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** travail avant la fermeture du logiciel ***
begin
  // vérification de l'enregistrement des modifications
  if EditorForm.SynEditEditor.Modified then
  begin
    // demande d'enregistrement des modifications  ### TODO ###
  end;
  // fermeture des fenêtres en cours
  CanClose := True; // fermeture autorisée
end;

procedure TMainForm.FormCreate(Sender: TObject);
// *** création de la fiche principale ***
begin
  fDeepFollow := False; // pas de trace approfondie
  Caption :=  CE_GVTitle + ' ' + CE_GVVersion + ' ' + CE_GVAuthor +
    ' ' + CE_GVDate;  // entête
  Automat := TGVAutomat.Create; // création de l'interpréteur
  Automat.Error.OnError := @GetError; // gestionnaire d'erreurs
  Automat.Message.OnMessageChange := @GetMessage; // gestionnaire de messages
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  Automat.Free; // libération de l'interpréteur
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
// *** affichage de la boîte à propos ***
begin
  ShowAboutForm; // on montre la fiche
end;

procedure TMainForm.SearchFindUpdate(Sender: TObject);
// *** activation / désactivation de la recherche ***
begin
  // actif si l'éditeur est non vide
  (Sender as TAction).Enabled := not ((EditorForm.SynEditEditor.Lines.Count = 1)
    and (EditorForm.SynEditEditor.Lines[1] = EmptyStr));
end;

procedure TMainForm.SearchNextExecute(Sender: TObject);
// *** poursuite d'une recherche aboutie ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skFindNext); // on cherche
end;

procedure TMainForm.SearchNextUpdate(Sender: TObject);
// *** activation/ désactivation de la recherche suivante ***
begin
  SearchNext.Enabled := (SearchFind.Enabled) and (EditorForm.SearchOK);
end;

procedure TMainForm.SearchReplaceExecute(Sender: TObject);
// *** remplacement dans l'éditeur ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skReplace); // on cherche
end;

procedure TMainForm.ShowCmdLineExecute(Sender: TObject);
// *** montre la ligne de commande ***
begin
  EditForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  EditForm.Show; // on la voit
end;

procedure TMainForm.ShowProcsExecute(Sender: TObject);
// *** affichage des procédures disponibles
begin
  ShowProcsForm; // on montre la fiche
end;

procedure TMainForm.ShowProcsUpdate(Sender: TObject);
// *** procédures disponibles ? ***
begin
  // action active s'il y a des procédures à éditer
  ShowProcs.Enabled := (Automat.Kernel.ProcsCount <> 0);
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
  with Automat.Datas do
    ErrorForm.SetError((Sender as TGVErrors).ErrorMessage, ErrorRec.ErrItem,
      fLine, fItem, fPrim, fProc, fNum, fLevel, ErrorRec.ErrPos);
  // affichage de la fenêtre appropriée
  ErrorForm.ShowModal;
end;

procedure TMainForm.GetMessage(Sender: TObject);
// *** gestionnaire des messages ***
begin
  case Automat.Message.Cmd of
    // écriture avec retour chariot
    acWrite: TextForm.WriteTextLN(Automat.Message.Message);
    acClear: TextForm.Clear; // nettoyage
    // lecture d'une liste
    acReadList: Automat.Message.Message := InputBox('TestGVAutomat',
    'Entrez la valeur demandée ici :', EmptyStr);
    // demande de confirmation
    acConfirm: if MessageDlg(Automat.Message.Message , mtConfirmation,
      mbYesNo, 0) = mrYes then
        Automat.Message.Message := CStTrue // vrai en retour
      else
        Automat.Message.Message := CStFalse; // faux en retour
    // écriture sans retour chariot
    acType: TextForm.WriteText(Automat.Message.Message);
    acReadChar: ; // ### TODO ###
    // styles de caractères
    acBold: TextForm.Bold := True;
    acUnderline: TextForm.Underline := True;
    acItalic: TextForm.Italic := True;
    acNoBold: TextForm.Bold := False;
    acNoUnderline: TextForm.Underline := False;
    acNoItalic: TextForm.Italic := False;
    // couleurs
    acColor: Automat.Message.Message := GetAColor(TextForm.FontColor);
    acBackColor: Automat.Message.Message := GetAColor(TextForm.BackColor);
    acSetColor: TextForm.FontColor := SetAColor(Automat.Message.Message);
    acSetBackColor: TextForm.BackColor :=
      SetAColor(Automat.Message.Message);
    // fontes
    acFont: Automat.Message.Message := TextForm.Font.Name;
    acSetFont: TextForm.Font.Name := Automat.Message.Message;
    acSetFontSize: TextForm.FontSize := StrToInt(Automat.Message.Message);
    acFontSize: Automat.Message.Message := IntToStr(TextForm.FontSize);
  end;
end;

function TMainForm.GetAColor(const AValue: TColor): string;
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
  Result := IntToStr(Lr);
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

end.

