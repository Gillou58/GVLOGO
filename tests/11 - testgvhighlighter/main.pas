{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : test de la coloration de l'éditeur      |
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
// 27/02/2015 - 1.0.1
// * nouvelles primitives : F.CARRE F.RECTANGLE F.ELLIPSE F.CERCLE
//   F.RECTANGLE.ARRONDI REMPLIS LAISSE.VOIR
// * acceptation des mots VRAI et FAUX dans les listes attendant des
//   valeurs booléennes : FIXE.ETAT.TORTUE [100 100 90 8 100 VRAI FAUX]

// TESTGVHIGHLIGHTER - part of GVLOGO
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
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Buttons, ExtCtrls,
  GVHighlighter; // unité de coloration

type
  // TMainForm
  TMainForm = class(TForm)
    btnUnfoldAll: TBitBtn;
    btnFoldAll: TBitBtn;
    btnExit: TBitBtn;
    btnHighlight: TBitBtn;
    pnlMain: TPanel;
    sbMain: TStatusBar;
    SynEditMain: TSynEdit;
    procedure btnExitClick(Sender: TObject);
    procedure btnFoldAllClick(Sender: TObject);
    procedure btnHighlightClick(Sender: TObject);
    procedure btnUnfoldAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // colorisation
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnExitClick(Sender: TObject);
// logiciel fermé
begin
  Close;
end;

procedure TMainForm.btnFoldAllClick(Sender: TObject);
// tout plier
begin
  SynEditMain.FoldAll(0, True);
end;

procedure TMainForm.btnHighlightClick(Sender: TObject);
// éditeur coloré
begin
  if not Assigned(SynEditMain.Highlighter) then
    SynEditMain.Highlighter := fGVHighlighter // colorisation affectée
  else
    SynEditMain.Highlighter := nil; // pas de coloration
end;

procedure TMainForm.btnUnfoldAllClick(Sender: TObject);
// tout déplier
begin
  SynEditMain.UnfoldAll;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // colorisation créée
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fGVHighlighter.Free; // colorisation libérée
end;

end.

