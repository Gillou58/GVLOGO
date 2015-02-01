{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de l'éditeur de GVLOGO            |
  |                  Unité : FrmEditor.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMEDITOR - part of GVLOGO
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

unit FrmEditor;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynPluginSyncroEdit, Forms, Controls,
  Graphics, Dialogs, ComCtrls,
  GVLogoConsts, // constantes de GVLOGO
  FrmFind, // recherche
  GVHighlighter; // colorisation de la syntaxe

type
  // *** TEditorForm ***

  { TEditorForm }

  TEditorForm = class(TForm)
    sbEdit: TStatusBar;
    SynEditEditor: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditEditorChange(Sender: TObject);
    procedure SynEditEditorKeyDown(Sender: TObject; var {%H-}Key: Word;
      {%H-}Shift: TShiftState);
  private
    fGVHighlighter: TGVHighlighter; // colorisation
    fSearchOk: Boolean; // drapeau de recherche aboutie
  public
    // recherche et remplacement
    procedure Search(Kind: TSearchKind);
    // interprétation de l'éditeur
    procedure Process;
    // drapeau de recherche aboutie
    property SearchOK: Boolean read fSearchOk write fSearchOk;

  end;

var
  EditorForm: TEditorForm;

implementation

uses
  GVConsts, // constantes
  GVPrimConsts, // conqtantes de primitives
  GVLists, // listes
  Main, // fiche principale
  FrmEdit, // ligne de commande
  FrmInfo, // fenêtre d'information
  StrUtils;

{$R *.lfm}

{ TEditorForm }

procedure TEditorForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditEditor.Highlighter := fGVHighlighter; // éditeur affecté
  Caption := CrsUnknownFile; // entête par défaut
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVHighlighter.Free; // libération de la colorisation
end;

procedure TEditorForm.SynEditEditorChange(Sender: TObject);
// *** changement de l'éditeur ***
begin
  with SynEditEditor do
    sbEdit.Panels[1].Text := Format(CrsLine + '%.3d - ' + CrsCol + '%.3d - %s',
      [CaretY, CaretX, IfThen(Modified, CrsModified, CrsOk)]);
  MainForm.Modified := SynEditEditor.Modified;
end;

procedure TEditorForm.SynEditEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// *** touche pressée ***
begin
  SynEditEditorChange(nil);
end;

procedure TEditorForm.Search(Kind: TSearchKind);
// *** recherche dans le texte ***
begin
  case Kind of
    skFind: FindForm.Find; // recherche simple
    skFindNext: FindForm.FindNext; // recherche suivante
    skReplace: FindForm.Replace; // remplacement
    skNextTo: FindForm.NextTo; // POUR suivant
    skPrevTo: FindForm.PrevTo; // POUR précédent
    skNextEnd: FindForm.NextEnd; // FIN suivant
    skPrevEnd: FindForm.PrevEnd; // FIN précédent
  end;
end;

procedure TEditorForm.Process;
// *** interprétation ***
var
  Li, LBg: Integer;
  LL: TGVList;

  function ValidLine: Boolean;
  // traitement d'une ligne
  begin
    Result := True; // suppose que tout va bien
    Inc(Li); // ligne suivante
    // ligne transformée en liste
    LL.Text := CBeginList + Trim(SynEditEditor.Lines[Li]) +
      CEndList;
    if not LL.IsValid then // une erreur ?
    begin
      // on positionne le curseur sur l'erreur
      SynEditEditor.CaretX := LL.Error.Error.ErrPos;
      SynEditEditor.CaretY := Li + 1;
      Result := False; // erreur signalée
    end;
  end;

begin
  LL := TGVList.Create; // création de la liste de travail
  try
    // gestionnaire d'erreur en place
    LL.Error.OnError := @MainForm.GetError;
    Li := -1; // avant la première ligne
    // on balaie les lignes de l'éditeur tant qu'il n'y a pas d'erreur
    while (Li <= SynEditEditor.Lines.Count) and (MainForm.Automat.Error.Ok) do
    begin
      if (not ValidLine) then // test de la validité de la ligne
        Break; // on arrête en cas d'erreur
      // *** ligne vide ou premier élément commentaire ? ***
      if LL.IsEmptyList or (LL.First = CComment) then
        Continue // on force le bouclage
      else
      // *** le premier élément est-il POUR ? ***
      if AnsiSameText(LL.First, P_To) then
      begin
        LBg := Li + 1; // marque le début de la procédure
        // on boucle pour trouver le mot FIN
        while (Li <= SynEditEditor.Lines.Count) do
        begin
          if not ValidLine then // ligne incorrecte ?
            Exit // on sort
          else
          begin
            // FIN ? => on sort de la boucle
            if (not LL.IsEmptyList) and AnsiSameText(LL.First, P_End) then
              Break; // on sort de la boucle
          end;
        end;
        // on enregistre la procédure (erreur si FIN non trouvé)
        MainForm.Automat.Kernel.EditToProc(SynEditEditor.Lines, LBg,
          Li + 1);
      end
      else
      begin
        // *** on tente d'exécuter la ligne ***
        // affectation à la ligne de commande
        FrmEdit.EditForm.cbEditCmdLine.Text := SynEditEditor.Lines[Li];
        MainForm.ExecExecuteExecute(nil); // exécution
      end;
    end;
    if (MainForm.Automat.Error.Ok and LL.Error.Ok) then // pas d'erreur ?
      ShowInfoForm(CrsInterpreter) // message de réussite
    else
      ShowOnTop; // si erreur, on montre l'éditeur
      MainForm.Automat.Clear; // on nettoie l'erreur
  finally
    LL.Free; // libération de la liste de travail
  end;
end;

end.

