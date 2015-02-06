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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Inifiles;

type
  // *** TOptionsForm ***

  { TOptionsForm }

  TOptionsForm = class(TForm)
    btnAbort: TBitBtn;
    btnClear: TBitBtn;
    btnClose: TBitBtn;
    edtFiles: TEdit;
    edtSetup: TEdit;
    gbDirs: TGroupBox;
    lblFiles: TLabel;
    lblSetup: TLabel;
    pnlOptions: TPanel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    sbOptions: TStatusBar;
    procedure btnAbortClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtFilesDblClick(Sender: TObject);
    procedure edtFilesExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fAbort: Boolean; // drapeau d'abandon
    fConfigFile: string; // fichier de configuration
    fUserDir: string; // répertoire des projets
    fIniFile: TIniFile; // fichier de stockage
    procedure SetConfigFile(const AValue: string);
    procedure SetUserDir(const AValue: string);
  public
    procedure InitAll; // initialisation
    property ConfigFile: string read fConfigFile write SetConfigFile;
    property UserDir: string read fUserDir write SetUserDir;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  FrmInfo, // fenêtre d'information
  GVConsts, // constantes générales
  GVLOGOConsts; // constantes du projet

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.FormCreate(Sender: TObject);
// *** activation de la fiche ***
begin
  InitAll; // initilisation
  // on crée le fichier de configuration
  fIniFile := TIniFile.Create(ConfigFile + CrsConfigFile);
  // le fichier de configuration existe-t-il ?
  if FileExists(ConfigFile + CrsConfigFile) then
  begin
    with fIniFile do // fichier de configuration
    begin
      // fichiers utilisateur
      UserDir := ReadString('directories', 'userdir', UserDir);
    end;
  end;
end;

procedure TOptionsForm.edtFilesExit(Sender: TObject);
// *** changement de dossier d'enregistrement de dossiers
begin
  UserDir := edtFiles.Text; // on tente de changer le répaertoire de travail
end;

procedure TOptionsForm.edtFilesDblClick(Sender: TObject);
// *** choix d'un répertoire ***
begin
  SelectDirectoryDialog.InitialDir := UserDir; // répertoire d'origine
  if SelectDirectoryDialog.Execute then // dialogue
    UserDir := SelectDirectoryDialog.FileName; // nouveau répertoire de travail
end;

procedure TOptionsForm.btnClearClick(Sender: TObject);
// *** nettoyage ***
begin
  InitAll; // réinitialisation complète
end;

procedure TOptionsForm.btnAbortClick(Sender: TObject);
// *** abandon des modifications ***
begin
  fAbort := True; // drapeau levé
  Close; // on ferme la fenêtre
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
// *** désactivation de la fiche ***
begin
  if not fAbort then
  begin
    // sauvegarde de la configuration
    with fIniFile do // fichier de configuration
    begin
      // identification
      WriteString('general', 'copyright', CE_GVTitle + ' ' + CE_GVVersion + ' '
        + CE_GVAuthor + ' ' +  CE_GVDate);
      // fichiers utilisateur
      WriteString('directories', 'userdir', UserDir);
    end;
  end;
  fIniFile.Free; // libération du fichier de configuration
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
    begin
      ShowInfoForm(Format(CrsCreateDirErr, [LS])); // message d'erreur
      Exit; // on sort
    end;
  fConfigFile := LS; // valeur changée
  edtSetup.Text := fConfigFile; // affichage mis à jour
end;

procedure TOptionsForm.SetUserDir(const AValue: string);
// *** répertoire de travail ***
var
  LS: string;
begin
  if AValue = EmptyStr then // chaîne vide ?
    LS := GetUserDir + CrsGVLOGO // bon répertoire
  else
    LS := AValue; // nouvelle valeur
  if not DirectoryExists(LS) then // le répertoire existe ?
    if not ForceDirectories(LS) then // on force sa création
    begin
      ShowInfoForm(Format(CrsCreateDirErr, [LS])); // message d'erreur
      Exit; // on sort
    end;
  fUserDir := LS; // nouveau répertoire de travail
  edtFiles.Text := fUserDir; // affichage à jour
end;

procedure TOptionsForm.InitAll;
// *** initialisation ***
begin
  ConfigFile := EmptyStr; // fichier de configuration par défaut
  UserDir := EmptyStr; // idem pour l'emplacement des fichiers
end;

end.

