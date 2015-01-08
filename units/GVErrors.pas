{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Gestion des erreurs                     |
  |                  Unité : GVErrors.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVERRORS - part of GVLOGO
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

unit GVErrors;
//
// Cette unité contient les outils nécessaires à la gestion des erreurs
// de toutes les unités de GVLOGO.
//

interface

uses Classes, SysUtils,
  GVErrConsts; // constantes pour les erreurs

type
  // *** notification d'une erreur ***
  TGVOnErrorEvent = procedure(Sender: TObject; ErrorRec: TGVErrorRec) of object;

  // *** TGVErrors ***
  TGVErrors = class(TObject)
  strict private
    fOnError: TGVOnErrorEvent; // notification de changement
    fErrorRec: TGVErrorRec; // enregistrement en cours
    fErrorMessage: string; // message d'erreur en cours
    fOKFlag: Boolean; // drapeau d'erreur
    function GetOKFlag: Boolean; // récupération du drapeau d'erreur
    procedure SetOKFlag(AValue: Boolean); // mise à jour du drapeau d'erreur
  protected
    // changement notifié
    procedure NotifyError;
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // nettoyage des erreurs
    // procédure concernant la collecte des informations
    procedure SetError(const Code: TGVError; ErrItem: string; ErrPos:
       Integer = CE_NoErr); virtual;
    // centralisation des informations
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
    // message associé à une erreur
    property ErrorMessage: string read fErrorMessage;
    // notification de changement
    property OnError: TGVOnErrorEvent read fOnError write fOnError;
    // drapeau d'erreur
    property Ok: Boolean read GetOKFlag write SetOKFlag default True;
    // enregistrement d'erreur
    property Error: TGVErrorRec read fErrorRec write fErrorRec;
 end;

implementation

{ TGVErrors }

procedure TGVErrors.NotifyError;
// *** notification de changement ***
begin
  if Assigned(fOnError) then // si gestionnaire actif
    fOnError(Self, fErrorRec); // on l'exécute
end;

procedure TGVErrors.Clear;
// *** nettoyage des erreurs ***
begin
  with fErrorRec do // on nettoie l'enregistrement
  begin
    Code := CE_None; // pas d'erreur
    ErrItem := ME_Nothing; // pas d'élément fautif dans la ligne de travail
    ErrPos := CE_NoErr; // pas de position de l'élément fautif
    // message en cours formaté
    fErrorMessage := Format(GVErrorName[CE_None], [CE_GVLogo]);
  end;
  Ok := True; // tout va bien...
end;

procedure TGVErrors.SetError(const Code: TGVError; ErrItem: string;
   ErrPos: Integer = CE_NoErr);
// *** enregistrement d'une erreur ***
begin
  if Code <> CE_None then // code modifié d'erreur s'il existe
    fErrorRec.Code := Code; // on change le code
  if ErrItem <> ME_Nothing then // idem élément en cours
  begin
    if ErrItem = EmptyStr then
      fErrorRec.ErrItem := ME_Nothing
    else
      fErrorRec.ErrItem := ErrItem;
  end;
  if ErrPos <> CE_NoErr then // idem position de l'erreur
    fErrorRec.ErrPos := ErrPos;
  // message en cours formaté avec élément erroné
  fErrorMessage := Format(GVErrorName[Code], [fErrorRec.ErrItem]);
  OK := False; // erreur signalée
  NotifyError; // changement notifié
end;

procedure TGVErrors.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// *** centralisation des erreurs ***
begin
  with ErrorRec do
    SetError(Code, ErrItem, ErrPos);  // erreur récupérée
end;

constructor TGVErrors.Create;
// *** constructeur ***
begin
  inherited Create; // on hérite
  Clear; // nettoyage
end;

destructor TGVErrors.Destroy;
// *** destructeur ***
begin
  inherited Destroy; // on hérite
end;

function TGVErrors.GetOKFlag: Boolean;
// *** renvoi du drapeau d'erreur ***
begin
  Result := fOKFlag;
end;

procedure TGVErrors.SetOKFlag(AValue: Boolean);
// *** drapeau d'erreur ***
begin
  if (AValue <> fOKFlag) then // si un changement
    fOKFlag := AValue; // on le note
end;

end.
