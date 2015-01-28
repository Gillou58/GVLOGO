{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de recherche/remplacement         |
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
    cboxPrompt: TCheckBox;
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
    procedure cboxPromptChange(Sender: TObject);
    procedure cbReplaceChange(Sender: TObject);
    procedure cboxWholeWordChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rbBackwardChange(Sender: TObject);
    procedure rbSelectedChange(Sender: TObject);
  private
    fSynSearch: TSynSearchOptions; // options de recherche
    fFind, fReplace: string; // textes de travail
  public
    procedure Find; // chercher
    procedure Replace; // remplacer
  end;

var
  FindForm: TFindForm;

implementation

uses
  GVLogoConsts, // constantes de GVLOGO
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
  // non trouvée ?
  if EditorForm.SynEditEditor.SearchReplace(fFind, fReplace,
    fSynSearch) = 0 then
  begin
    ShowInfoForm(Format(CrsNotFound, [fFind])); // on informe
    fSynSearch := fSynSearch - [ssoFindContinue]; // on ne continue pas
  end
  else
    fSynSearch := fSynSearch + [ssoFindContinue]; // recherche poursuivie
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
  fSynSearch := fSynSearch + [ssoReplace] - [ssoReplaceAll]; // drapeau de remplacement
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
  cboxPrompt.Enabled := cboxReplace.Checked;
  btnFind.Visible := not cboxReplace.Checked;
  btnReplace.Visible := cboxReplace.Checked;
  btnReplaceAll.Visible := cboxReplace.Checked;;
  if cboxReplace.Checked then
  begin
    cbReplace.Cursor := crNo; // curseur adapté
    cboxPrompt.Cursor := crNo;
    fSynSearch := fSynSearch + [ssoReplace]; // recherche
  end
  else
  begin
    cbReplace.Cursor := crHandPoint; // curseur adapté
    cboxPrompt.Cursor := crHandPoint;
    fSynSearch := fSynSearch - [ssoReplace, ssoReplaceAll]; // remplacement
  end;
end;

procedure TFindForm.cboxPromptChange(Sender: TObject);
// *** demande sur remplacement ***
begin
  if cboxPrompt.Checked then
    fSynSearch := fSynSearch + [ssoPrompt]
  else
    fSynSearch := fSynSearch - [ssoPrompt];
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
  cboxPrompt.Checked := (ssoPrompt in fSynSearch);
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

procedure TFindForm.Find;
// *** recherche ***
begin
  cboxReplace.Checked := False;
  ShowOnTop; // fenêtre montrée
end;

procedure TFindForm.Replace;
// *** remplacement ***
begin
  cboxReplace.Checked := True;
  ShowOnTop;
end;


end.

