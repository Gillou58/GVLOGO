{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Boîte à propo de GVLOGO                 |
  |                  Unité : frmAbout.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMABOUT - part of GVLOGO
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

unit frmAbout;

interface

uses
  Classes, SysUtils, FileUtil, BCButton, BCLabel, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    BCBtnClose: TBCButton;
    blblVersion: TBCLabel;
    blblDate: TBCLabel;
    blblSupport: TBCLabel;
    imgPowered: TImage;
    Label1: TLabel;
    lblTarget: TLabel;
    lblDate: TLabel;
    lblVersion: TLabel;
    lblTitle: TBCLabel;
    LblAuthor: TBCLabel;
    imgAbout: TImage;
    procedure BCBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

function ShowAboutForm: TModalResult;

implementation

{$R *.lfm}

uses
  DefineTemplates, // pour la version
  GVConsts; // constantes

function ShowAboutForm: TModalResult;
// *** affiche la fiche ***
var
  GVAboutForm: TAboutForm;
begin
  GVAboutForm := TAboutForm.Create(nil); // fiche créée
  try
    Result := GVAboutForm.ShowModal; // fiche affichée
  finally
    GVAboutForm.Free; // fiche libérée
  end;
end;

{ TAboutForm }

procedure TAboutForm.BCBtnCloseClick(Sender: TObject);
// *** demande fermeture de la fenêtre ***
begin
  Close; // on ferme la fenêtre
end;

procedure TAboutForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  blblVersion.Caption := CE_GVVersion; // version mise à jour
  blblDate.Caption := CE_GVDate; // idem pour la date
  // idem pour la plateforme
  blblSupport.Caption := GetCompiledTargetCPU + CBlank + GetCompiledTargetOS;
end;

end.

