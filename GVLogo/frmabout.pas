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
    ilTurtle: TImageList;
    imgPowered: TImage;
    Label1: TLabel;
    lblLES: TLabel;
    lblURL: TLabel;
    lblAuthor: TLabel;
    lblGVLOGO: TLabel;
    lblTarget: TLabel;
    lblDate: TLabel;
    lblVersion: TLabel;
    lblTitle: TBCLabel;
    blblAuthor: TBCLabel;
    imgAbout: TImage;
    mmoAbout: TMemo;
    procedure BCBtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgAboutMouseEnter(Sender: TObject);
    procedure imgAboutMouseLeave(Sender: TObject);
    procedure lblLESMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure lblLESMouseEnter(Sender: TObject);
    procedure lblLESMouseLeave(Sender: TObject);
  private
  public
  end;

function ShowAboutForm: TModalResult;

implementation

{$R *.lfm}

uses
  DefineTemplates, // pour le support
  LCLIntf, // pour les URL
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
// *** demande de fermeture de la fenêtre ***
begin
  Close; // on ferme la fenêtre
end;

procedure TAboutForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  // auteur mis à jour
  blblAuthor.Caption := TrimLeft(Copy(CE_GVAuthor, 3, Length(CE_GVAuthor)));
  blblVersion.Caption := CE_GVVersion; // idem pour la version
  blblDate.Caption := CE_GVDate; // idem pour la date
  // idem pour la plateforme
  blblSupport.Caption := GetCompiledTargetCPU + CBlank + GetCompiledTargetOS;
  // idem pour la présentation
  lblGVLOGO.Caption := CE_GVTitle + CBlank + CE_GVVersion + CBlank +
    CE_GVAuthor + CBlank + CE_GVDate;
end;

procedure TAboutForm.imgAboutMouseEnter(Sender: TObject);
// *** image changée ***
begin
  ilTurtle.GetBitmap(random(ilTurtle.Count - 1) + 1,imgAbout.Picture.Bitmap);
end;

procedure TAboutForm.imgAboutMouseLeave(Sender: TObject);
// *** image par défaut ***
begin
  ilTurtle.GetBitmap(0,imgAbout.Picture.Bitmap);
end;

procedure TAboutForm.lblLESMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
// *** lien URL cliqué ***
begin
  OpenURL((Sender as TLabel).Caption); // on charge la page
end;

procedure TAboutForm.lblLESMouseEnter(Sender: TObject);
// *** souris sur une URL ***
begin
  (Sender as TLabel).Font.Style := [fsUnderline]; // soulignement
  (Sender as TLabel).Font.Color := clRed; // couleur rouge
  (Sender as TLabel).Cursor := crHandPoint; // pointeur sous forme de main
end;

procedure TAboutForm.lblLESMouseLeave(Sender: TObject);
// *** souris quittant une URL ***
begin
  (Sender as TLabel).Font.Style := []; // pas de style
  (Sender as TLabel).Font.Color := clBlue; // couleur bleue
  (Sender as TLabel).Cursor := crDefault; // pointeur par défaut
end;



end.

