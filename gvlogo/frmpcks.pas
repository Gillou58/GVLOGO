{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Affichage des paquets                   |
  |                  Unité : FrmPcks.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMPCKS - part of GVLOGO
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

unit FrmPcks;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit;

type
  // *** TPcksForm ***
  TPcksForm = class(TForm)
    ValueListEditor: TValueListEditor;
    procedure FormShow(Sender: TObject);
  private
  public
    procedure Pcks; // affichage des paquets
  end;

var
  PcksForm: TPcksForm;

implementation

{$R *.lfm}

uses
  GVConsts, // constantes
  GVLists, // listes
  Main; // fiche principale

{ TPcksForm }

procedure TPcksForm.FormShow(Sender: TObject);
// *** affichage de la fenêtre ***
begin
  Pcks; // on affiche
end;

procedure TPcksForm.Pcks;
// *** paquets en cours ***
var
  Lst: TGVList;
  LS, LValue: string;
begin
  Lst := TGVList.Create; // création de la liste de travail
  try
    ValueListEditor.Strings.Clear; // on nettoie les valeurs anciennes
    Lst.Text := MainForm.Automat.Kernel.PcksToList; // on récupère les valeurs
    for LS in Lst do // on balaie les variables
    begin
      LValue := MainForm.Automat.Kernel.PckToList(Ls); // on récupère la valeur
      ValueListEditor.Strings.Add(LS + CEqual + LValue);
    end;
  finally
    Lst.Free; // libération de la liste
  end;
end;

end.

