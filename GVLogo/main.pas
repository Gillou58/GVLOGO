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
    ShowFollow: TAction;
    MenuItem10: TMenuItem;
    MenuFollow: TMenuItem;
    MenuPrevTo: TMenuItem;
    MenuNextEnd: TMenuItem;
    MenuPrevEnd: TMenuItem;
    MenuNextTo: TMenuItem;
    MenuReplace: TMenuItem;
    MenuSearchFirst: TMenuItem;
    MenuSearchNext: TMenuItem;
    MenuItem17: TMenuItem;
    MenuSearch: TMenuItem;
    SaveDialog: TSaveDialog;
    SearchNext: TAction;
    SearchReplace: TAction;
    SearchFind: TAction;
    CoolBarMain: TCoolBar;
    ShowAll: TAction;
    ShowPcks: TAction;
    ShowProcs: TAction;
    ShowLocVars: TAction;
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
    MenuFile: TMenuItem;
    MenuEdit: TMenuItem;
    MenuExec: TMenuItem;
    MenuHelp: TMenuItem;
    MenuItem1: TMenuItem;
    MenuIndent: TMenuItem;
    MenuIndentOff: TMenuItem;
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
    tbShowFollow: TToolButton;
    procedure EditRedoUpdate(Sender: TObject);
    procedure EditUndoUpdate(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure HelpPrimsExecute(Sender: TObject);
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
    procedure SearchNextEndExecute(Sender: TObject);
    procedure SearchNextExecute(Sender: TObject);
    procedure SearchNextToExecute(Sender: TObject);
    procedure SearchNextUpdate(Sender: TObject);
    procedure SearchPreviousEndExecute(Sender: TObject);
    procedure SearchPreviousToExecute(Sender: TObject);
    procedure SearchReplaceExecute(Sender: TObject);
    procedure ShowAllExecute(Sender: TObject);
    procedure ShowCmdLineExecute(Sender: TObject);
    procedure ShowEditExecute(Sender: TObject);
    procedure ShowFollowExecute(Sender: TObject);
    procedure ShowProcsExecute(Sender: TObject);
    procedure ShowProcsUpdate(Sender: TObject);
    procedure ShowTextExecute(Sender: TObject);
    procedure ShowTurtleExecute(Sender: TObject);
  private
    fDeepFollow: Boolean; // drapeau de trace approfondie
    fGVAutomat: TGVAutomat; // interpréteur
    fRunning: Boolean; // drapeau d'exécution en cours
    fModified: Boolean; // éditeur modifié
    // recherche d'une couleur normalisée
    function GetAColor(const AValue: TColor): string;
    // conversion d'une couleur
    function SetAColor(const St: string): TColor;
    procedure SetModified(AValue: Boolean);
    procedure SetRunning(AValue: Boolean);
    // enregistrement d'un fichier
    function SaveFile(const St: string): TModalResult;
    // vérification avant nouveau fichier
    function FileSaved(const St: string): Boolean;
  public
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec); // erreurs
    procedure GetMessage(Sender: TObject); // messages
    procedure GetStateChange(Sender: TObject); // changement d'état
    property Automat: TGVAutomat read fGVAutomat write fGVAutomat;
    // drapeau d'exécution en cours
    property Running: Boolean read fRunning write SetRunning;
    // drapeau d'éditeur modifié
    property Modified: Boolean read fModified write SetModified;
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
  FrmpHelpPrims, // aide sur les primitives
  FrmFollow, // suivi
  FrmDump, // contenu du noyau
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
  Automat.Follow := not Automat.Follow; // on inverse sa valeur
end;

procedure TMainForm.ExecInterpretExecute(Sender: TObject);
// *** interprétation ***
begin
  ShowEditExecute(nil); // éditeur montré
  EditorForm.Process; // on exécute
end;

procedure TMainForm.ExeStopExecute(Sender: TObject);
// *** stop ***
begin
  Automat.Stop := True; // arrêt demandé
  ShowInfoForm(CrsStop); // message affiché
end;

procedure TMainForm.FileOpenAccept(Sender: TObject);
// *** ouverture d'un fichier ***
begin
  // enregistrement avorté ou erroné ?
  if not FileSaved(EditorForm.Caption) then
    Exit; // on sort
  // exécute le chargement du fichier choisi
  try
    EditorForm.ShowOnTop; // éditeur en vue
    EditorForm.SynEditEditor.Lines.LoadFromFile(FileOpen.Dialog.FileName);
    EditorForm.Caption := FileOpen.Dialog.FileName;
  except
    // erreur
    FrmInfo.ShowInfoForm(Format(CrsErrLoad, [FileOpen.Dialog.FileName]));
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
  // enregistrement OK ?
  if FileSaved(EditorForm.Caption) then
    Automat.ClearAll; // on nettoie
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
  EditUndo.Enabled := EditorForm.SynEditEditor.CanUndo // possible ?
    and not Running; // et pas de programme en cours
end;

procedure TMainForm.FileNewExecute(Sender: TObject);
// *** nouveau fichier ***
begin
  if FileSaved(EditorForm.Caption) then // sauvegarde nécessaire ?
  begin
    with EditorForm do
    begin
      SynEditEditor.Lines.Clear; // on vide l'éditeur
      Caption := CrsUnknownFile; // fichier par défaut
      WindowState := wsNormal; // apparence normale
      ShowOnTop; // on montre la fenêtre
    end;
  end;
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
// *** sauvegarde ***
begin
  // nouveau fichier ?
  if EditorForm.Caption = CrsUnknownFile then
    FileSaved(EditorForm.Caption) // sauvegarde sous
  else
  begin
    try
      // sauvegarde effectuée
      EditorForm.SynEditEditor.Lines.SaveToFile(EditorForm.Caption);
      Modified := False; // drapeau de modification à jour
    except
      // message d'erreur
      FrmInfo.ShowInfoForm(Format(CrsErrSaved, [EditorForm.Caption]));
    end;
  end;
end;

procedure TMainForm.FileSaveUpdate(Sender: TObject);
// *** activation/ désactivation de la sauvegarde ***
begin
  FileSave.Enabled := (not Running) and Modified;
  FileSaveAs.Enabled := FileSave.Enabled;
end;

procedure TMainForm.HelpPrimsExecute(Sender: TObject);
// *** aide sur les primitives ***
begin
  FrmpHelpPrims.ShowPrimsHelp; // on montre la fiche
end;

procedure TMainForm.EditRedoUpdate(Sender: TObject);
// *** activation/ désactivation de refaire ***
begin
  EditRedo.Enabled := (EditorForm.SynEditEditor.CanRedo) // possible ?
    and not Running; // sans exécution en cours
end;

procedure TMainForm.ExecExecuteExecute(Sender: TObject);
// *** interprétation ***
begin
  Automat.Clear; // nettoyage
  try
    Running := True; // drapeau d'exécution
    ShowTextExecute(nil); // on montre la fenêtre de texte
    ShowTurtleExecute(nil); // on montre la fenêtre de la tortue
    ShowCmdLineExecute(nil); // on montre la ligne de commande
    Automat.Process(CBeginList + FrmEdit.EditForm.cbEditCmdLine.Text +
      CEndList); // ligne à exécuter
  finally
    Running := False; // drapeau d'exécution
  end;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** travail avant la fermeture du logiciel ***
begin
  // fermeture autorisée suivant l'état du fichier
  CanClose := FIleSaved(EditorForm.Caption);
end;

procedure TMainForm.FormCreate(Sender: TObject);
// *** création de la fiche principale ***
begin
  fDeepFollow := False; // pas de trace approfondie
  Caption :=  CE_GVTitle + ' ' + CE_GVVersion + ' ' + CE_GVAuthor +
    ' ' + CE_GVDate;  // entête
  Automat := TGVAutomat.Create; // création de l'interpréteur
  Automat.OnStateChange := @GetStateChange; // changement d'état
  Automat.Error.OnError := @GetError; // gestionnaire d'erreurs
  // gestionnaire des messages
  Automat.Message.OnMessageChange := @GetMessage;
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
  // actif si l'éditeur est non vide et pas de programme en cours
  (Sender as TAction).Enabled := not (((EditorForm.SynEditEditor.Lines.Count = 1)
    and (EditorForm.SynEditEditor.Lines[1] = EmptyStr)) or Running);
end;

procedure TMainForm.SearchNextEndExecute(Sender: TObject);
// *** recherche FIN suivant ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skNextEnd); // on cherche
end;

procedure TMainForm.SearchNextExecute(Sender: TObject);
// *** poursuite d'une recherche aboutie ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skFindNext); // on cherche
end;

procedure TMainForm.SearchNextToExecute(Sender: TObject);
// *** recherche POUR suivant ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skNextTo); // on cherche
end;

procedure TMainForm.SearchNextUpdate(Sender: TObject);
// *** activation/ désactivation de la recherche suivante ***
begin
  SearchNext.Enabled := (SearchFind.Enabled) and (EditorForm.SearchOK);
end;

procedure TMainForm.SearchPreviousEndExecute(Sender: TObject);
// *** recherche FIN précédent ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skPrevEnd); // on cherche
end;

procedure TMainForm.SearchPreviousToExecute(Sender: TObject);
// *** recherche POUR précédent ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skPrevTo); // on cherche
end;

procedure TMainForm.SearchReplaceExecute(Sender: TObject);
// *** remplacement dans l'éditeur ***
begin
  EditorForm.ShowOnTop; // on montre l'éditeur
  EditorForm.Search(skReplace); // on cherche
end;

procedure TMainForm.ShowAllExecute(Sender: TObject);
// *** affichage du contenu du noyau ***
begin
  DumpForm.Dump; // remplissage
  DumpForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  DumpForm.Show; // on la voit
end;

procedure TMainForm.ShowCmdLineExecute(Sender: TObject);
// *** montre la ligne de commande ***
begin
  EditForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  EditForm.Show; // on la voit
end;

procedure TMainForm.ShowEditExecute(Sender: TObject);
// *** affichage de l'éditeur ***
begin
  EditorForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  EditorForm.Show; // on la voit
end;

procedure TMainForm.ShowFollowExecute(Sender: TObject);
// *** affichage de la fenêtre de suivi ***
begin
  FollowForm.WindowState := wsNormal; // la fenêtre est redimensionnée
  FollowForm.Show; // on la voit
end;

procedure TMainForm.ShowProcsExecute(Sender: TObject);
// *** affichage des procédures disponibles ***
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
  with Automat.Message do
    case Cmd of
      // écriture avec retour chariot
      acWrite: TextForm.WriteTextLN(Message);
      acClear: TextForm.Clear; // nettoyage
      // lecture d'une liste
      acReadList: Message := InputBox(CrsAsk, CrsAskForValue, EmptyStr);
      // demande de confirmation
      acConfirm: if MessageDlg(Message , mtConfirmation, mbYesNo,
        0) = mrYes then
          Message := CStTrue // vrai en retour
        else
          Message := CStFalse; // faux en retour
      // écriture sans retour chariot
      acType: TextForm.WriteText(Message);
      acReadChar: ; // ### TODO ###
      // styles de caractères
      acBold: TextForm.Bold := True;
      acUnderline: TextForm.Underline := True;
      acItalic: TextForm.Italic := True;
      acNoBold: TextForm.Bold := False;
      acNoUnderline: TextForm.Underline := False;
      acNoItalic: TextForm.Italic := False;
      // couleurs
      acColor: Message := GetAColor(TextForm.FontColor);
      acBackColor: Message := GetAColor(TextForm.BackColor);
      acSetColor: TextForm.FontColor := SetAColor(Message);
      acSetBackColor: TextForm.BackColor := SetAColor(Message);
      // fontes
      acFont: Message := TextForm.Font.Name;
      acSetFont: TextForm.Font.Name := Message;
      acSetFontSize: TextForm.FontSize := StrToInt(Message);
      acFontSize: Message := IntToStr(TextForm.FontSize);
      // éditeur
      acWriteEdit: EditorForm.SynEditEditor.Lines.Add(Message);
    end;
end;

procedure TMainForm.GetStateChange(Sender: TObject);
// *** gestion du changement d'état ***
var
  LS: string;
begin
  if Automat.Follow then  // suivi ?
  begin
    case Automat.State of // message suivant l'état
      asWord, asList, asVar, asNumber, asEval, asPushing, asProc,
      asPrim, asPrimValue: LS := Automat.Datas.fItem;
      asExePrim, asPrimDone: LS := Automat.Datas.fPrim;
      asExeProc, asProcDone: LS := Automat.Datas.fProc;
    else
      LS := EmptyStr;
    end;
    // état affiché
    FollowForm.Write(Format('[%d]  '+ CStatesArray[Automat.State],
      [Automat.Datas.fLevel, LS]), 1);
   end;
   if fDeepFollow and (not (Automat.State in [asWaiting, asPreparing,
        asEnding])) then  // trace ?
          with Automat.Datas do
          begin
            FollowForm.Write(CrsLine + fLine, 2);
            FollowForm.Write(Format(CrsFollowLine,
              [fLevel, fItem, fNum, fPrim, fProc]), 1);
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

procedure TMainForm.SetModified(AValue: Boolean);
// *** éditeur modifié ***
begin
  if fModified = AValue then // pas de changement ?
    Exit; /// on sort
  fModified := AValue; // nouvelle valeur
  EditorForm.SynEditEditor.Modified := fModified; // éditeur à jour
  FileSave.Enabled := fModified; // état sauvegarde adapté
  FileSaveAs.Enabled := fModified;
end;

procedure TMainForm.SetRunning(AValue: Boolean);
// *** exécution en cours ***
begin
  if fRunning = AValue then
    Exit;
  fRunning := AValue; // nouvelle valeur affectée
  ExeStop.Enabled := fRunning; // interruption
  ExecClear.Enabled := not fRunning; // nettoyage
  ExecWait.Enabled := fRunning; // pause
  ExecExecute.Enabled := not fRunning; // exécution
  ExecInterpret.Enabled := not fRunning; // interprétation
  FileQuit.Enabled := not fRunning; // sortie
  FileOpen.Enabled := not fRunning; // ouverture de fichier
  FileSaveAs.Enabled := not fRunning; // sauvegarde sous
  FileNew.Enabled := not fRunning; // nouveau fichier
  FileNewProc.Enabled := not fRunning; // nouvelle procédure
  FileClose.Enabled := not fRunning; // fermeture de fichier
  FilePrint.Enabled := not fRunning; // impression
  FileSave.Enabled := not fRunning; // sauvegarde
  EditCut.Enabled := not fRunning; // couper
  EditCopy.Enabled := not fRunning; // ccpier
  EditPaste.Enabled := not fRunning; // coller
  EditSelectAll.Enabled := not fRunning; // tout sélectionner
  SearchNext.Enabled := not fRunning; // recherche suivante
  SearchFind.Enabled := not fRunning; // recherche
  SearchReplace.Enabled := not fRunning; // rempalcement
  EditForm.cbEditCmdLine.Enabled := not fRunning; // ligne de commande
  HelpAbout.Enabled := not fRunning; // aide sur l'aide
  HelpPrims.Enabled := not fRunning; // aide sur les primitives
  MenuFile.Enabled := not fRunning; // menus
  MenuEdit.Enabled := not fRunning;
  MenuSearch.Enabled := not fRunning;
  MenuWindows.Enabled := fRunning;
  MenuHelp.Enabled := not fRunning;
end;

function TMainForm.SaveFile(const St: string): TModalResult;
// *** enregistrement d'un fichier ***
var
  LSt: string;
begin
  Result := mrNone; // résultat neutre
  LSt := ChangeFileExt(St, CExt); // on force l'extension
  // est-ce le fichier par défaut ?
  if LSt = CrsUnknownFile then
  begin
    if SaveDialog.Execute then // sauvegarde admise ?
      LSt := SaveDialog.FileName // nouveau nom récupéré
    else
    begin
      Result := mrCancel; // abandon
      Exit;
    end;
  end;
  if FileExistsUTF8(LSt) then // le fichier existe-t-il déjà ?
    // on confirme ou non son remplacement
    Result := ShowConfirmForm(Format(CrsReplaceFile, [LSt]));
  if Result in [mrYes, mrNone] then // sauvegarde demandée
  begin
    try
      // sauvegarde effectuée
      EditorForm.SynEditEditor.Lines.SaveToFile(LSt);
      Modified := False; // drapeau de modification à jour
      Result := mrOk;
    except
      Result := mrAbort; // erreur de sauvegarde
      // message d'erreur
      FrmInfo.ShowInfoForm(Format(CrsErrSaved, [LSt]));
    end;
  end;
end;

function TMainForm.FileSaved(const St: string): Boolean;
// *** vérification avant nouveau fichier ***
begin
  if EditorForm.SynEditEditor.Modified then
  begin
    // demande d'enregistrement des modifications
    case ShowConfirmForm(Format(CrsSave, [St])) of
      mrYes: Result := (SaveFile(St) = mrOk); // on enregistre
      mrNo: Result := True; // on n'enregistre pas
      mrCancel: Result := False; // on abandonne l'opération
    end;
  end
  else
    Result := True; // pas besoin d'enregistrer
end;

end.

