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
  Classes, SysUtils, FileUtil, BCButton, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TProcsForm }

  TProcsForm = class(TForm)
    btnQuit: TBCButton;
    cbProcs: TComboBox;
    procedure btnQuitClick(Sender: TObject);
    procedure cbProcsChange(Sender: TObject);
    procedure cbProcsGetItems(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  function ShowProcsForm: TModalResult;

implementation

uses
  Main, // fiche principale
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

procedure TProcsForm.btnQuitClick(Sender: TObject);
// *** fermeture de la fenêtre ***
begin
  Close; // on ferme la fenêtre
end;

procedure TProcsForm.cbProcsChange(Sender: TObject);
// *** affichage de la procédure choisie ***
begin
  // ### TODO ###
end;

procedure TProcsForm.cbProcsGetItems(Sender: TObject);
// *** traitement pour l'affichage ***
var
  LL: TGVList;
  LS: string;
begin
  LL := TGVList.Create; // création de la liste
  try
    // affectation des procédures
    LL.Text := MainForm.Automat.Kernel.ProcsToList;
    Self.Text := Self.Text + ' (' + IntToStr(LL.Count) + ')';
    // on balaie la liste
    for LS in LL do
      cbProcs.AddItem(LS, nil); // ajout à la boîte
  finally
    LL.Free; // liste libérée
  end;
end;

end.

