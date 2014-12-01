{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Intepréteur                             |
  |                  Unité : GVInterpreter.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    01-12-2014 07:40:23                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVInterpreter - part of GVLOGO
// Copyright (C) 2014 Gilles VASSEUR
//
// This program is free software: you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
//  If not, see <http://www.gnu.org/licenses/>.

{$I GVDefines.inc}

{$IFNDEF Delphi}
{$mode objfpc}{$H+}
{$ENDIF}

unit GVInterpreter;

// Unité de l'interpréteur
//
// ##############################################################
//
// L'interpréteur utilise les unités précedemment définies
// pour l'interprétation des programmes écrits en GVLOGO.
//

interface

uses
  Classes, SysUtils, ExtCtrls,
  GVConsts, GVStacks, GVKernel, GVEval;

type

  { TGVInterpreter }

  TGVInterpreter = class(TObject)
  private
    fOnChange: TNotifyEvent; // notification de changement
    fError: TGVError; // erreur en cours
    fActualItem: string; // donnée en cours
    fKernel: TGVLogoKernel; // noyau de travail
    fParamsStack: TGVIntegerStack; // pile des paramètres
    fDatasStack: TGVStringStack; // pile des données
    fCommandsStack: TGVStringStack; // pile des commandes
    fExeStack: TGVStringStack; // pile d'exécution
    fEval: TGVEval; // évaluateur d'expressions
    procedure PushConst(const St: string); // empilement d'une constante
    procedure LWord(const AValue: string); // traitement d'un littéral mot
    procedure LNumber(const AValue: string); // traitement d'un nombre
    procedure LList(const AValue: string); // traitement d'une liste
    procedure LVar(const AValue: string); // traitement d'une variable
    procedure LPrim(const AValue: string); // traitement d'une primitive
    procedure LProc(const AValue: string); // traitement d'une procédure
    procedure LCommand(const AValue: string); // traitement d'une commande
    procedure SetError(AValue: TGVError); // traitement d'une erreur
    procedure ExePrim(N: Integer); // exécution d'une primitive
    procedure ExeCommand; // exécution de la commande en attente
  protected
    procedure Change; // changement signalé
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    property Error: TGVError read fError write SetError default C_None; // erreur
    property ActualItem: string read fActualItem write fActualItem; // élément en cours
    property OnChange: TNotifyEvent read fOnChange write fOnChange; // changement notifié
  end;

implementation

{ TGVInterpreter }

procedure TGVInterpreter.PushConst(const St: string);
// *** empilement d'une constante ***
begin
  fDatasStack.Push(St); // on empile la constante
  Change; // changement notifié
  with fParamsStack do
  begin
    Push(Pop -1); // un paramètre a été trouvé
    // trop de paramètres ?
    if (Peek < 0) or ((Peek = 0) and (fCommandsStack.Count = 0)) then
      Error := C_WhatAbout // [Erreur : que faire de ? ]
    else
      ExeCommand; // on exécute la commande en attente
  end;
end;

procedure TGVInterpreter.LWord(const AValue: string);
// *** traitement d'un littéral mot ***
begin
  PushConst(Copy(AValue, 1, Length(AValue)-1)); // empiler la constante sans le "
end;

procedure TGVInterpreter.LNumber(const AValue: string);
// *** traitement d'un nombre ***
var
  St: string;
  Dbl: Double;
begin
  St := AValue;
  if St[1] = CPlus then // retirer + initial
    St := Copy(St, 1, Length(St) -1);
  if TryStrToFloat(St, Dbl) then // nombre correct ?
    PushConst(St) // empiler la constante
  else
    Error := C_BadNumber; // [Erreur: nombre invalide]
end;

procedure TGVInterpreter.LList(const AValue: string);
// *** traitement d'une liste ***
begin
  PushConst(AValue); // empiler la constante
end;

procedure TGVInterpreter.LVar(const AValue: string);
// *** traitement d'une variable ***
var
  St: string;
begin
  St := Copy(AValue, 1, Length(AValue)-1); // : initial retiré
  if fKernel.IsLocVar(St) then // variable locale ?
    PushConst(fKernel.ValLocVar(St)) // on empile sa valeur
  else
  if fKernel.IsVar(St) then // variable globale ?
    PushConst(fKernel.ValVar(St)) // on empile sa valeur
  else
    Error := C_UnknownVar; // [Erreur: variable inconnue]
  if fKernel.Error then // autre erreur du noyau ?
    Error := fKernel.KernelResult; // [Erreur signalée]
end;

procedure TGVInterpreter.LPrim(const AValue: string);
// *** traitement d'une primitive ***
var
  Num, N: Integer;
begin
  // numéro de la primitive
  Num := fKernel.NumPrim(AValue);
  // nombre de paramètres associés
  N := fKernel.NumParamsPrim(AValue);
  fParamsStack.Push(N); // on enregistre le nombre d'arguments attendus
  fParamsStack.Dup; // on double ce nombre
  if N = 0 then // pas de paramètres ?
    ExePrim(Num) // exécute la primitive
  else
    // $ + numéro de la primitive empilé
    fCommandsStack.Push(CLink + IntToStr(Num));
end;

procedure TGVInterpreter.LProc(const AValue: string);
// *** traitement d'une procédure ***
begin
  // ### TODO ###
end;

procedure TGVInterpreter.LCommand(const AValue: string);
// *** traitement d'une commande ***
begin
  if fKernel.IsPrim(AValue) then
    LPrim(AValue) // on traite la primitive
  else
  if fKernel.IsProc(AValue) then
    LProc(AValue) // on traite la procédure
  else
    Error := C_NorPrimNorProc;
end;

procedure TGVInterpreter.SetError(AValue: TGVError);
// *** traitement d'une erreur ***
begin
  if fError = AValue then
    Exit; // on sort si aucun changement
  fError := AValue; // nouvelle valeur d'erreur
  Change; // changement notifié
end;

procedure TGVInterpreter.ExePrim(N: Integer);
// *** exécution d'une primitive
begin
  // ### TODO ###
end;

procedure TGVInterpreter.ExeCommand;
// *** exécution de la commande en attente ***
begin
  // ### TODO ###
end;

procedure TGVInterpreter.Change;
// *** notification de changement ***
begin
  if Assigned(fOnChange) then // gestionnaire assigné ?
    fOnChange(self); // on l'exécute
end;

constructor TGVInterpreter.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fError := C_None; // pas d'erreur
  fKernel := TGVLogoKernel.Create; // noyau de travail
  fParamsStack := TGVIntegerStack.Create; // pile des paramètres
  fDatasStack := TGVStringStack.Create; // pile des données
  fCommandsStack := TGVStringStack.Create; // pile des commandes
  fExeStack := TGVStringStack.Create; // pile d'exécution
  fEval := TGVEval.Create; // évaluateur d'expressions
end;

destructor TGVInterpreter.Destroy;
// *** destruction ***
begin
  // libération des objets de travail
  fKernel.Free; // noyau de travail
  fParamsStack.Free; // pile des paramètres
  fDatasStack.Free; // pile des données
  fCommandsStack.Free; // pile des commandes
  fExeStack.Free; // pile d'exécution
  fEval.Free; // évaluateur d'expressions
  inherited Destroy; // on hérite
end;

end.

