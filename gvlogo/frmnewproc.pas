{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche pour une nouvelle procédure       |
  |                  Unité : FrmNewProc.pas                                |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMNEWPROC - part of GVLOGO
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

unit FrmNewProc;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons,
   GVHighlighter; // coloration syntaxique

type
  // *** TNewProcForm ***
  TNewProcForm = class(TForm)
    btnClose: TBitBtn;
    btnClear: TBitBtn;
    btnSave: TBitBtn;
    lbledtParams: TLabeledEdit;
    lbledtName: TLabeledEdit;
    pnlNewProc2: TPanel;
    pnlNewProc: TPanel;
    sbNewProc: TStatusBar;
    SynEditNewProc: TSynEdit;
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbledtNameExit(Sender: TObject);
    procedure lbledtParamsExit(Sender: TObject);
    procedure SynEditNewProcChange(Sender: TObject);
    procedure SynEditNewProcExit(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // coloration syntaxique
    fParams: string;  // paramètres actuels
    fBody: string; // corps de la procédure
    fEdit: Boolean; // drapeau d'édition
    procedure SetNameEdit(AValue: Boolean);
  public
    property NameEdit: Boolean read fEdit write SetNameEdit;
  end;

  function ShowNewProcForm: TModalResult; // affichage

implementation

{$R *.lfm}

uses
  Main, // fiche principale
  FrmInfo, // boîtes d'information
  GVLists, // listes
  GVConsts, // constantes générales
  GVErrConsts, // constantes d'erreurs
  GVLOGOConsts; // constantes du projet

function ShowNewProcForm: TModalResult;
// *** affiche la fiche ***
var
  GVNewProcForm: TNewProcForm;
begin
  GVNewProcForm := TNewProcForm.Create(nil); // fiche créée
  try
    Result := GVNewProcForm.ShowModal; // fiche affichée
  finally
    GVNewProcForm.Free; // fiche libérée
  end;
end;

{ TNewProcForm }

procedure TNewProcForm.SynEditNewProcChange(Sender: TObject);
// *** éditeur modifié ***
begin
  // message suivant l'état du texte
  if SynEditNewProc.Modified then
    sbNewProc.Panels[1].Text := CrsModified
  else
    sbNewProc.Panels[1].Text := CrsOk;
  // bouton de sauvegarde adapté à l'état du texte
  btnSave.Enabled := SynEditNewProc.Modified;
end;

procedure TNewProcForm.SynEditNewProcExit(Sender: TObject);
// sortie de l'éditeur
var
  LS: string;
begin
  fBody := CBeginList; // chaîne initialisée
  try
    // on construit la liste de sortie
    for LS in SynEditNewProc.Lines do
      fBody := fBody + CBeginList + LS + CEndList + CBlank;
  finally
    fBody := TrimRight(fBody) + CEndList; // chaîne finalisée
  end;
  // on teste le corps de la définition
  if not MainForm.Automat.Kernel.IsValidBodyDef(fBody) then
    fBody := CEmptyList; // corps de la procédure vidé si erreur
end;

procedure TNewProcForm.SetNameEdit(AValue: Boolean);
// *** édition du nom de la procédure ***
begin
  if fEdit = AValue then // pas de changement ?
    Exit; // on sort
  fEdit := AValue; // valeur pour l'édition
  lbledtName.Enabled := not fEdit; // pas de changement de nom si édition
end;

procedure TNewProcForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditNewProc.Highlighter := fGVHighlighter; // éditeur lié
  SynEditNewProc.Lines.Clear; // nettoyage
  btnSave.Enabled := False; // sauvegarde désactivée
  fParams := CEmptyList; // pas de paramètres
  fBody := CEmptyList; // ni de corps
  lbledtName.Text := ME_Nothing; // ligne d'édition du nom nettoyée
end;

procedure TNewProcForm.btnClearClick(Sender: TObject);
// *** nettoyage de la fenêtre ***
begin
  SynEditNewProc.Lines.Clear;
  lbledtName.Clear;
  lbledtParams.Clear;
  NameEdit := False; // pas d'édition
end;

procedure TNewProcForm.btnSaveClick(Sender: TObject);
// *** enregistrement de la procédure ***
begin
  SynEditNewProc.Modified := not MainForm.Automat.Kernel.AddProc(
    lbledtName.Text, CBeginList + fParams + fBody + CEndList); // enregistrement
end;

procedure TNewProcForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** fermeture de la fenêtre ***
begin
  if SynEditNewProc.Modified then // éditeur modifié ?
  begin
    // confirmation
    case FrmInfo.ShowConfirmForm(Format(CrsSave,
      [lbledtName.Text])) of
        // on veut enregistrer la procédure
        mrYes: CanClose := MainForm.Automat.Kernel.AddProc(lbledtName.Text,
          CBeginList + fParams + fBody + CEndList); // enregistrement
        mrNo: CanClose := True; // pas d'enregistrement
        mrCancel: CanClose := False; // abandon
      end;
  end
  else
    CanClose := True; // on ferme la fenêtre
end;

procedure TNewProcForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVHighlighter.Free; // libération de la colorisation
end;

procedure TNewProcForm.lbledtNameExit(Sender: TObject);
// *** sortie de la ligne d'édition du nom ***
var
  Li: Integer;
  LU: TGVListUtils;
begin
  // déjà une procédure et pas d'édition en cours
  if (MainForm.Automat.Kernel.IsProc(lbledtName.Text)) and not NameEdit then
  begin
    LU := TGVListUtils.Create; // création de la liste de travail
    try
      // message de confirmation d'édition
      case ShowConfirmForm(Format(CrsAlreadyAProc, [lbledtName.Text])) of
        mrYes: begin
          // on ajoute la ligne de paramètres sans les crochets
          lbledtParams.Text := LU.ListToStr(MainForm.Automat.Kernel.ParamsLine(
            lbledtName.Text));
          SynEditNewProc.Lines.Clear; // on efface l'éditeur
          // on balaie les lignes de la définition de la procédure
          for Li := 1 to MainForm.Automat.Kernel.ProcLinesCount(
            lbledtName.Text) do
          begin
            // on ajoute la ligne à l'éditeur sans les crochets
            SynEditNewProc.Lines.Add(LU.ListToStr(
              MainForm.Automat.Kernel.ProcLine(lbledtName.Text, Li)));
          end;
          NameEdit := True; // mode édition actif
        end;
        mrNo, mrCancel: begin
          lbledtName.Text := ME_Nothing; // ligne d'édition du nom nettoyée
          lbledtName.SetFocus; // on reste sur la ligne
        end;
      end;
    finally
      LU.Free; // libération de la liste de travail
    end;
  end;
end;

procedure TNewProcForm.lbledtParamsExit(Sender: TObject);
// *** sortie de la ligne d'édition des paramètres ***
begin
  // liste construite
  fParams := CBeginList + Trim(lbledtParams.Text) + CEndList;
  // paramètres incorrects ?
  if not MainForm.Automat.Kernel.IsValidParamsLine(fParams) then
    fParams := CEmptyList; // paramètres à zéro
end;

end.

