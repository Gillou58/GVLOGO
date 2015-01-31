{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche des procédures disponibles        |
  |                  Unité : FrmProcs.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMPROCS - part of GVLOGO
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

unit FrmProcs;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, Buttons,
  GVHighlighter; // coloration syntaxique

type
  // *** TProcsForm ***
  TProcsForm = class(TForm)
    btnSave: TBitBtn;
    btnQuit: TBitBtn;
    cbProcs: TComboBox;
    sbProcs: TStatusBar;
    SynEditProcs: TSynEdit;
    procedure btnQuitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure cbProcsEnter(Sender: TObject);
    procedure cbProcsKeyPress(Sender: TObject; var Key: char);
    procedure cbProcsSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditProcsChange(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // coloration syntaxique
  public
  end;

  function ShowProcsForm: TModalResult;

implementation

uses
  Main, // fiche principale
  GVLogoConsts, // constantes GVLOGO
  FrmInfo, // messages
  GVLists; // listes

function ShowProcsForm: TModalResult;
// *** affiche la fiche ***
var
  GVProcsForm: TProcsForm;
begin
  GVProcsForm := TProcsForm.Create(nil); // fiche créée
  try
    Result := GVProcsForm.ShowModal; // fiche affichée
  finally
    GVProcsForm.Free; // fiche libérée
  end;
end;

{$R *.lfm}

{ TProcsForm }

procedure TProcsForm.btnSaveClick(Sender: TObject);
// *** enregistrement des modifications ***
begin
  SynEditProcs.Modified := not MainForm.Automat.Kernel.EditToProc(
    SynEditProcs.Lines, 0, 0); // enregistrement
  SynEditProcsChange(nil); // changement notifié
end;

procedure TProcsForm.btnQuitClick(Sender: TObject);
// *** fermeture de la fenêtre
begin
  Close; // on ferme la fenêtre
end;

procedure TProcsForm.cbProcsEnter(Sender: TObject);
// *** traitement pour l'affichage dans la boîte ***
var
  LL: TGVList;
  LS: string;
begin
  cbProcs.Clear; // on nettoie la boîte
  LL := TGVList.Create; // création de la liste
  try
    // affectation des procédures
    LL.Text := MainForm.Automat.Kernel.ProcsToList;
    Self.Text := Format(CrsProcs ,[LL.Count]); // entête de la fenêtre
    // on balaie la liste
    for LS in LL do
      cbProcs.AddItem(LS, nil); // ajout à la boîte
    cbProcs.ItemIndex := 0; // pointe sur le premier élément
    cbProcsSelect(nil); // on l'édite
  finally
    LL.Free; // liste libérée
  end;
end;

procedure TProcsForm.cbProcsKeyPress(Sender: TObject; var Key: char);
// *** touche pressée ***
begin
  if key = #13 then
    cbProcsSelect(nil);
end;

procedure TProcsForm.cbProcsSelect(Sender: TObject);
// *** affichage de la procédure choisie ***
var
  LChange: Boolean;
begin
  LChange := True; // changement par défaut
  if SynEditProcs.Modified then // éditeur modifié ?
  begin
    // confirmation
    case FrmInfo.ShowConfirmForm(Format(CrsSave,
      [cbProcs.Items[cbProcs.ItemIndex]])) of
        // on veut enregistrer la procédure
        mrYes: LChange := MainForm.Automat.Kernel.EditToProc(SynEditProcs.Lines,
            0, 0);
        mrNo: LChange := True; // pas d'enregistrement
        mrCancel: LChange := False; // abandon
      end;
  end;
  if LChange then // changement autorisé ?
  begin
    SynEditProcs.Lines.Clear; // nettoyage de l'éditeur
    // édition de la procédure
    with cbProcs do
      if MainForm.Automat.Kernel.ProcToEdit(Items[ItemIndex],
        SynEditProcs.Lines) then
          SynEditProcs.Modified := False; // drapeau de modification à jour
  end;
end;

procedure TProcsForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
// *** fermeture de la fenêtre ***
begin
  if SynEditProcs.Modified then // éditeur modifié ?
  begin
    // confirmation
    case FrmInfo.ShowConfirmForm(Format(CrsSave,
      [cbProcs.Items[cbProcs.ItemIndex]])) of
        // on veut enregistrer la procédure
        mrYes: CanClose := MainForm.Automat.Kernel.EditToProc(SynEditProcs.Lines,
          0, 0);
        mrNo: CanClose := True; // pas d'enregistrement
        mrCancel: CanClose := False; // abandon
      end;
  end
  else
    CanClose := True; // on ferme la fenêtre
end;

procedure TProcsForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditProcs.Highlighter := fGVHighlighter; // éditeur lié
  SynEditProcs.Lines.Clear; // nettoyage
  btnSave.Enabled := False; // sauvegarde désactivée
end;

procedure TProcsForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVHighlighter.Free; // libération de la colorisation
end;

procedure TProcsForm.SynEditProcsChange(Sender: TObject);
// *** éditeur modifié ***
begin
  // message suivant l'état du texte
  if SynEditProcs.Modified then
    sbProcs.Panels[1].Text := CrsModified
  else
    sbProcs.Panels[1].Text := CrsOk;
  // bouton de sauvegarde adapté à l'état du texte
  btnSave.Enabled := SynEditProcs.Modified;
end;

end.

