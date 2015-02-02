{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fenêtre de la ligne de commande         |
  |                  Unité : FrmEdit.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMEDIT - part of GVLOGO
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

unit FrmEdit;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type
  // *** TEditForm ***
  TEditForm = class(TForm)
    cbEditCmdLine: TComboBox;
    sbWait: TSpeedButton;
    sbExec: TSpeedButton;
    spStop: TSpeedButton;
    procedure EditCmdLineKeyPress(Sender: TObject; var Key: char);
  private
  public
  end;

var
  EditForm: TEditForm;

implementation

uses
  GVLogoConsts, // constantes du projet
  Main; // fiche principale

{$R *.lfm}

{ TEditForm }

procedure TEditForm.EditCmdLineKeyPress(Sender: TObject; var Key: char);
// *** test des touches ***
begin
  if Key = #13 then // touche Entrée ?
  begin
    // mémorisation de l'entrée
    cbEditCmdLine.AddHistoryItem(cbEditCmdLine.Text, CMaxHistoryEntries, True,
      True);
    MainForm.ExecExecuteExecute(nil); // exécution des commandes
  end;
end;

end.

