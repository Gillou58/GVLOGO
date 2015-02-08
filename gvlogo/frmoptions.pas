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
    cbHistFiles: TComboBox;
    edtFiles: TEdit;
    edtSetup: TEdit;
    gbDirs: TGroupBox;
    gbHisto: TGroupBox;
    lblFiles: TLabel;
    lblSetup: TLabel;
    pnlOptions: TPanel;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    sbOptions: TStatusBar;
    procedure btnAbortClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure edtFilesDblClick(Sender: TObject);
    procedure edtFilesExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
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
    procedure ClearHist; // nettoyage de l'historique
    procedure AddHistFile(const St: string); // liste historique
    property ConfigFile: string read fConfigFile write SetConfigFile;
    property UserDir: string read fUserDir write SetUserDir;
  end;

var
  OptionsForm: TOptionsForm;

implementation

uses
  FrmInfo, // fenêtre d'information
  Main, // fiche principale
  GVConsts, // constantes générales
  GVErrConsts, // erreurs
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

procedure TOptionsForm.FormActivate(Sender: TObject);
// *** fenêtre montrée ***
begin
  cbHistFiles.Enabled := (cbHistFiles.Items.Count > 0); // des éléments ?
  if cbHistFiles.Enabled then
    cbHistFiles.ItemIndex := 0  // on pointe sur le premier élément
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
  ClearHist; // nettoyage de l'historique
  cbHistFiles.Clear; // nettoyage de l'historique
end;

procedure TOptionsForm.ClearHist;
// *** nettoyage de l'historique ***
begin
  // mise à jour du menu principal
  with MainForm do
  begin
    HF1.Caption := ME_Nothing; HF1.Enabled := False;
    HF2.Caption := EmptyStr; HF2.Visible := False;
    HF3.Caption := EmptyStr; HF3.Visible := False;
    HF4.Caption := EmptyStr; HF4.Visible := False;
    HF5.Caption := EmptyStr; HF5.Visible := False;
    HF6.Caption := EmptyStr; HF6.Visible := False;
    HF7.Caption := EmptyStr; HF7.Visible := False;
    HF8.Caption := EmptyStr; HF8.Visible := False;
  end;
end;

procedure TOptionsForm.AddHistFile(const St: string);
// *** fichier récent ***
var
  Li: Integer;
  LB: Boolean;
begin
  LB := False; // drapeau baissé
  for Li := 0 to cbHistFiles.Items.Count - 1 do // on balaie la liste
    if CompareText(cbHistFiles.Items[Li], St) = 0 then // existe déjà ?
    begin
      cbHistFiles.Items.Exchange(0, Li); // replacé en premier
      LB := True; // drapeau levé
      Break; // on sort
    end;
  if not LB then // non trouvé ?
  begin
    cbHistFiles.Items.Insert(0, St); // place en première position
    // on supprime l'élément le plus ancien
    cbHistFiles.Items.Delete(CMaxHistoryEntries);
  end;
  ClearHist; // menu nettoyé puis mis à jour
  with MainForm do
  begin
    if (cbHistFiles.Items.Count > 0) then
    begin
      HF1.Caption := cbHistFiles.Items[0]; HF1.Enabled := True;
    end
    else
      HF1.Caption := ME_Nothing;
    if (cbHistFiles.Items.Count > 1) then
    begin
      HF2.Caption := cbHistFiles.Items[1]; HF2.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 2) then
    begin
      HF3.Caption := cbHistFiles.Items[2]; HF3.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 3) then
    begin
      HF4.Caption := cbHistFiles.Items[3]; HF4.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 4) then
    begin
      HF5.Caption := cbHistFiles.Items[4]; HF5.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 5) then
    begin
      HF6.Caption := cbHistFiles.Items[5]; HF6.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 6) then
    begin
      HF7.Caption := cbHistFiles.Items[6]; HF7.Visible := True;
    end;
    if (cbHistFiles.Items.Count > 7) then
    begin
      HF8.Caption := cbHistFiles.Items[7]; HF8.Visible := True;
    end;
  end;
end;

end.

