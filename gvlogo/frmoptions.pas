{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fenêtre des options                     |
  |                  Unité : FrmOptions.pas                                |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMOPTIONS - part of GVLOGO
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

unit FrmOptions;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  // *** TOptionsForm ***
  TOptionsForm = class(TForm)
    edtFiles: TEdit;
    edtSetup: TEdit;
    gbDirs: TGroupBox;
    lblFiles: TLabel;
    lblSetup: TLabel;
    procedure FormActivate(Sender: TObject);
  private
    fConfigFile: string; // fichier de configuration
    fUserDir: string; // répertoire des projets
    procedure SetConfigFile(const AValue: string);
    procedure SetUserDir(const AValue: string);
  public
    property ConfigFile: string read fConfigFile write SetConfigFile;
    property UserDir: string read fUserDir write SetUserDir;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  GVLOGOCOnsts; // constantes du projet

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.FormActivate(Sender: TObject);
// *** activation de la fiche ***
begin
  ConfigFile := EmptyStr; // fichier de configuration par défaut
  UserDir := EmptyStr; // idem pour l'emplacement des fichiers
end;

procedure TOptionsForm.SetConfigFile(const AValue: string);
// *** fichier de configuration ***
var
  LS: string;
begin
  if AValue = EmptyStr then // chaîne vide ?
    LS := GetAppConfigDir(False) // bon fichier
  else
    LS := AValue; // nouvelle valeur
  if not DirectoryExists(LS) then // le dossier n'existe pas ?
    if not ForceDirectory(LS) then // on le crée si possible
      Exit; // on sort en cas d'erreur
  fConfigFile := LS; // valeur changée
  edtSetup.Text := fConfigFile; // affichage mis à jour
end;

procedure TOptionsForm.SetUserDir(const AValue: string);
// *** répertoire de travail ***
begin
  if AValue = EmptyStr then // chaîne vide ?
    fUserDir := GetUserDir + CrsGVLOGO // bon répertoire
  else
    fUserDir := AValue; // nouvelle valeur
  if not DirectoryExists(fUserDir) then // le répertoire existe ?
    if not ForceDirectories(fUserDir) then // on force sa création
      // si erreur on prend le répertoire par défaut
      fUserDir := GetUserDir;
  SetCurrentDir(fUserDir); // répertoire actif mis à jour
  edtFiles.Text := fUserDir; // affichage à jour
end;

end.

