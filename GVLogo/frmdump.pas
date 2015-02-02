{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Affichage du contenu du noyau           |
  |                  Unité : FrmDump.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMDUMP - part of GVLOGO
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

unit FrmDump;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  GVHighlighter; // coloration syntaxique

type
  // *** TDumpForm ***

  { TDumpForm }

  TDumpForm = class(TForm)
    SynEditDump: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fGVHighlighter: TGVHighlighter; // coloration syntaxique
  public
    procedure Dump; // affichage
  end;

var
  DumpForm: TDumpForm;

implementation

{$R *.lfm}

uses
  Main; // fiche principale

{ TDumpForm }

procedure TDumpForm.FormCreate(Sender: TObject);
// *** création de la fenêtre ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditDump.Highlighter := fGVHighlighter; // éditeur lié
  SynEditDump.Lines.Clear; // nettoyage
end;

procedure TDumpForm.FormDestroy(Sender: TObject);
// *** destruction de la fenêtre ***
begin
  fGVHighlighter.Free; // coloration libérée
end;

procedure TDumpForm.FormShow(Sender: TObject);
// *** affichage de la fenêtre ***
begin
  Dump; // on met à jour la fenêtre
end;

procedure TDumpForm.Dump;
// *** affichage du contenu du noyau ***
var
  Lst: TStringList;
  LS: string;
begin
  Lst := TStringList.Create; // création de la fiche
  try
    MainForm.Automat.Kernel.Dump(Lst); // recherche du contenu
    SynEditDump.Lines.Clear; // éditeur nettoyé
    for LS in Lst do // on balaie la liste
      SynEditDump.Lines.Add(LS);
  finally
    Lst.Free; // libération de la liste
  end;
end;

end.

