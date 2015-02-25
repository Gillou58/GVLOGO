{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des erreurs          |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVERRORS - part of GVLOGO
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

unit main;

{$I GVDefines.inc} // conditions générales

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,
  GVErrConsts, // constantes d'erreurs
  GVErrors; // gestion des erreurs

type
  { TMainForm }
  TMainForm = class(TForm)
    btnClear: TBitBtn;
    btnExit: TBitBtn;
    btnGenere: TBitBtn;
    mmoMain: TMemo;
    procedure btnClearClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fError: TGVErrors; // gestionnaire d'erreurs
  public
    // traitement des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnSetClick(Sender: TObject);
// génération d'une erreur
var
  LErr: TGVError; // l'erreur
  LErrItem: string; // l'élément fautif
  LErrPos: Integer; // la position de l'erreur
begin
  // génération aléatoire des paramètres
  LErr := TGVError(random(CTotalErrors));
  LErrItem := 'ELEMENT' + IntToStr(Ord(LErr));
  LErrPos := random(Length(LErrItem));
  if LErrPos = 0 then
    LErrPos := CE_NoErr;
  // génération de l'erreur
  fError.SetError(LErr, LErrItem, LErrPos);
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoMain.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  inherited; // on hérite
  fError := TGVErrors.Create; // gestionnaire de travail créé
  // gestionnaire de recherche d'erreur actif
  fError.OnError := @GetError;
  // pour le pseudo-hasard
  Randomize;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fError.Free; // gestionnaire libéré
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// traitement des erreurs
begin
  // message en toutes lettres
  mmoMain.Lines.Add('>>> ' + fError.ErrorMessage);
  with mmoMain.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
  mmoMain.Lines.Add(EmptyStr); // ligne vide
  fError.Clear; // nettoyage de l'erreur
end;

end.

