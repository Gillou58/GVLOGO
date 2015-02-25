{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de recherche/remplacement         |
  |                  Unité : FrmFind.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMFIND - part of GVLOGO
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

unit FrmFind;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  SynEditTypes; // types pour l'éditeur

type
  //*** TFindForm ***
  TFindForm = class(TForm)
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    btnCancel: TBitBtn;
    btnFind: TBitBtn;
    cboxCase: TCheckBox;
    cboxReplace: TCheckBox;
    cboxWholeWord: TCheckBox;
    cbFind: TComboBox;
    cbReplace: TComboBox;
    lblReplace: TLabel;
    lblFind: TLabel;
    rgOptions: TCheckGroup;
    rbSelected: TRadioButton;
    rbBackward: TRadioButton;
    rbForward: TRadioButton;
    rgHeading: TRadioGroup;
    rbGlobal: TRadioButton;
    rgWhere: TRadioGroup;
    procedure btnCancelClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure cbFindChange(Sender: TObject);
    procedure cboxCaseChange(Sender: TObject);
    procedure cboxReplaceChange(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure cboxWholeWordChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbSelectedChange(Sender: TObject);
  private
    fSynSearch: TSynSearchOptions; // options de recherche
    fFind, fReplace: string; // textes de travail
    procedure NextPrevPrim(const APrim: string; Backwards: Boolean = False);
  public
    procedure Find; // chercher
    procedure FindNext; // continuer la recherche
    procedure Replace; // remplacer
    procedure NextTo; // POUR suivant
    procedure PrevTo; // POUR précédent
    procedure NextEnd; // FIN suivant
    procedure PrevEnd; // FIN précédent
    property FindText: string read fFind write fFind;
    property ReplaceText: string read fReplace write fReplace;
  end;

var
  FindForm: TFindForm;

implementation

uses
  GVLogoConsts, // constantes de GVLOGO
  GVPrimConsts, // constantes des primitives
  FrmInfo, // boîte d'information
  FrmEditor; // fenêtre d'édition

{$R *.lfm}

{ TFindForm }

procedure TFindForm.btnCancelClick(Sender: TObject);
// *** abandon de la recherche ***
begin
  Close; // on ferme la fenêtre
end;

procedure TFindForm.btnFindClick(Sender: TObject);
// *** bouton de recherche ***
begin
  if fFind = EmptyStr then // chaîne de recherche vide ?
    Exit; // on sort
  // non trouvée ?
  if EditorForm.SynEditEditor.SearchReplace(fFind, fReplace,
    fSynSearch) = 0 then
  begin
    ShowInfoForm(Format(CrsNotFound, [fFind])); // on informe
    fSynSearch := fSynSearch - [ssoFindContinue]; // on ne continue pas
    EditorForm.SearchOK := False; // drapeau d'échec
  end
  else
  begin
    EditorForm.SearchOK := True; // drapeau de réussite
    fSynSearch := fSynSearch + [ssoFindContinue]; // recherche poursuivie
  end;
end;

procedure TFindForm.btnReplaceAllClick(Sender: TObject);
// *** remplacer tout ***
begin
  fSynSearch := fSynSearch + [ssoReplaceAll]; // drapeau de remplacement total
  btnFindClick(nil); // changement effectué
end;

procedure TFindForm.btnReplaceClick(Sender: TObject);
// *** remplacement ***
begin
  // drapeau de remplacement
  fSynSearch := fSynSearch + [ssoReplace] - [ssoReplaceAll];
  btnFindClick(nil); // changement effectué
end;

procedure TFindForm.cbFindChange(Sender: TObject);
// *** texte à chercher ***
begin
  fFind := cbFind.Text;
end;

procedure TFindForm.cboxCaseChange(Sender: TObject);
// *** changement de casse ***
begin
  if cboxCase.Checked then
    fSynSearch := fSynSearch + [ssoMatchCase]
  else
    fSynSearch := fSynSearch - [ssoMatchCase];
end;

procedure TFindForm.cboxReplaceChange(Sender: TObject);
// *** remplacement actif ou non ***
begin
  // contrôles activés/désactivés
  lblReplace.Enabled := cboxReplace.Checked;
  cbReplace.Enabled := cboxReplace.Checked;
  btnFind.Visible := not cboxReplace.Checked;
  btnReplace.Visible := cboxReplace.Checked;
  btnReplaceAll.Visible := cboxReplace.Checked;;
  if cboxReplace.Checked then
  begin
    cbReplace.Cursor := crNo; // curseur adapté
    fSynSearch := fSynSearch + [ssoReplace]; // recherche
  end
  else
  begin
    cbReplace.Cursor := crHandPoint; // curseur adapté
    fSynSearch := fSynSearch - [ssoReplace, ssoReplaceAll]; // remplacement
  end;
end;

procedure TFindForm.cbReplaceChange(Sender: TObject);
// *** texte de remplacement ***
begin
  fReplace := cbReplace.Text;
end;

procedure TFindForm.cboxWholeWordChange(Sender: TObject);
// *** mot entier ou non ***
begin
  if cboxWholeWord.Checked then
    fSynSearch := fSynSearch + [ssoWholeWord]
  else
    fSynSearch := fSynSearch - [ssoWholeWord];
end;

procedure TFindForm.FormActivate(Sender: TObject);
// *** activation de la fenêtre ***
begin
  // mise à jour des contrôles
  cbFind.Text := fFind;
  cboxCase.Checked := (ssoMatchCase in fSynSearch);
  cboxWholeWord.Checked := (ssoWholeWord in fSynSearch);
  rbSelected.Checked := (ssoSelectedOnly in fSynSearch);
  rbBackward.Checked := (ssoBackwards in fSynSearch);
  fSynSearch := fSynSearch - [ssoFindContinue];
  cboxReplaceChange(nil);
end;

procedure TFindForm.rbBackwardChange(Sender: TObject);
// *** en arrière ou en avant ***
begin
  if rbBackward.Checked then
     fSynSearch := fSynSearch + [ssoBackwards]
   else
     fSynSearch := fSynSearch - [ssoBackwards];
end;

procedure TFindForm.rbSelectedChange(Sender: TObject);
// *** opération sur texte sélectionné ou globale ***
begin
  if rbSelected.Checked then
    fSynSearch := fSynSearch + [ssoSelectedOnly] - [ssoEntireScope]
  else
    fSynSearch := fSynSearch + [ssoEntireScope] - [ssoSelectedOnly];
end;

procedure TFindForm.NextPrevPrim(const APrim: string; Backwards: Boolean);
// *** recherche d'un mot donné (avant ou arrière) ***
var
  LOldFind: string;
  LOldOptions: TSynSearchOptions;
begin
  LOldFind := FindText; // on conserve les données précédentes
  LOldOptions := fSynSearch;
  try
    if Backwards then // en arrière ?
      fSynSearch := [ssoBackwards, ssoWholeWord] // en arrière
    else
      fSynSearch := [ssoWholeWord]; // mots entiers seulement
    FindText := APrim; // primitive à rechercher
    btnFindClick(nil); // recherche effectuée
  finally
    FindText := LOldFind; // on retrouve les données originales
    fSynSearch := LOldOptions;
  end;
end;

procedure TFindForm.Find;
// *** recherche ***
begin
  cboxReplace.Checked := False;
  ShowOnTop; // fenêtre montrée
end;

procedure TFindForm.FindNext;
// *** continuation de la recherche ***
begin
  fSynSearch := fSynSearch + [ssoFindContinue]; // recherche poursuivie
  btnFindClick(nil); // on cherche sans la fenêtre
end;

procedure TFindForm.Replace;
// *** remplacement ***
begin
  cboxReplace.Checked := True;
  ShowOnTop;
end;

procedure TFindForm.NextTo;
// *** POUR suivant ***
begin
  NextPrevPrim(P_To);
end;

procedure TFindForm.PrevTo;
// *** POUR précédent ***
begin
  NextPrevPrim(P_To, True);
end;

procedure TFindForm.NextEnd;
// *** FIN suivant ***
begin
  NextPrevPrim(P_End);
end;

procedure TFindForm.PrevEnd;
// *** FIN précédent ***
begin
  NextPrevPrim(P_End, True);
end;


end.

