{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche d'aide sur les primitives         |
  |                  Unité : FrmpHelpPrims.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMPHELPRIMS - part of GVLOGO
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

unit FrmpHelpPrims;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type
  // *** TPrimsHelpForm ***
  TPrimsHelpForm = class(TForm)
    btnClose: TBitBtn;
    cbPrims: TComboBox;
    mmoDefs: TMemo;
    procedure cbPrimsSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Lst: TStringList; // liste de travail
  public
  end;

  procedure ShowPrimsHelp; // montre la fiche

implementation

uses
  FrmInfo, // fenêtre d'information
  GVLogoConsts; // constantes du projet

procedure ShowPrimsHelp;
// *** fiche active ***
var
  LPrimsHForm: TPrimsHelpForm;
begin
  LPrimsHForm := TPrimsHelpForm.Create(nil); // fiche créée
  try
    LPrimsHForm.ShowModal; // fiche montrée
  finally
    LPrimsHForm.Free; // fenêtre libérée
  end;
end;

{$R *.lfm}

{ TPrimsHelpForm }

procedure TPrimsHelpForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
var
  Li: Integer;
begin
  Lst := TStringList.Create; // création de la liste de travail
  try
    Lst.LoadFromFile(CPrimDefs); // chargement du fichier de définitions
    for Li := 1 to (Lst.Count - 1) do // on balaie la liste
      cbPrims.Items.Add(Lst.Names[Li]); // on ajoute les noms à la combobox
    cbPrims.ItemIndex := 0; // pointe sur la première primitive
  except
    FrmInfo.ShowInfoForm(CPrimDefs); // erreur affichée
    cbPrims.Enabled := False; // boîte inactive
  end;
end;

procedure TPrimsHelpForm.cbPrimsSelect(Sender: TObject);
// *** sélection d'un nom de primitive ***
begin
  mmoDefs.Lines.Clear; // zone effacée
  // on écrit la définition qui correspond à la primitive choisie
  mmoDefs.Lines.Add(Lst.Values[cbPrims.Items[cbPrims.ItemIndex]]);
end;

procedure TPrimsHelpForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  Lst.Free; // on libère la liste de travail
end;

end.

