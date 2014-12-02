{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Intepréteur                             |
  |                  Unité : GVInterpreter.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    01-12-2014 18:39:23                          |
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

{$DEFINE Debug}

unit GVInterpreter;

// Unité de l'interpréteur
//
// ##############################################################
//
// L'interpréteur utilise les unités précédemment définies
// pour l'interprétation des programmes écrits en GVLOGO.
//

interface

uses
  Classes, SysUtils, ExtCtrls,
  GVConsts, GVStacks, GVKernel, GVEval, GVLists, GVWords;

type

  { TGVInterpreter }

  TGVInterpreter = class(TObject)
  private
    fLines: TStrings; // lignes de l'éditeur de sortie
    fOnChange: TNotifyEvent; // notification de changement
    fOnError: TNotifyEvent; // notification d'erreur
    fError: TGVError; // erreur en cours
    fActualItem: string; // donnée en cours
    fActualLine: string; // ligne en cours
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
    procedure LEval(const AValue: string); // traitement d'une évaluation
    procedure SetError(AValue: TGVError); // traitement d'une erreur
    procedure ExePrim(N: Integer); // exécution d'une primitive
    procedure ExeProc; // exécution d'une procédure
    procedure ExeCommand; // exécution de la commande en attente
  protected
    procedure Change; // changement signalé
    procedure ErrorEvent; // erreur signalée
  public
    fTextRes: string; // résultat texte ### DEBUG ###
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure ComputeLine(const St: string); // interprète une ligne
    property Error: TGVError read fError write SetError default C_None; // erreur
    property ActualItem: string read fActualItem write fActualItem; // élément en cours
    property ActualLine: string read fActualLine; // ligne en cours
    property OnChange: TNotifyEvent read fOnChange write fOnChange; // changement notifié
    property OnError: TNotifyEvent read fOnError write fOnError; // erreur notifiée
    property Lines: TStrings read fLines write fLines; // éditeur
  end;

implementation

{ TGVInterpreter }

procedure TGVInterpreter.PushConst(const St: string);
// *** empilement d'une constante ***
begin
  fDatasStack.Push(St); // on empile la constante
  with fParamsStack do
  begin
    // pas de paramètres ou pas de commande en attente
    if (Count = 0) or ((Peek > 0) and (fCommandsStack.Count = 0)) then
      Error := C_WhatAbout // [Erreur : que faire de ? ]
    else
    begin
      Push(Pop - 1); // un paramètre a été trouvé
      if (Peek = 0) then // plus de paramètres en attente
        ExeCommand; // on exécute la commande en attente
    end;
  end;
end;

procedure TGVInterpreter.LWord(const AValue: string);
// *** traitement d'un littéral mot ***
var
  St: string;
begin
  St := AValue;
  // empiler la constante non vide sans le "
  if (St <> EmptyStr) and (St[1] = CQuote) then
    St := Copy(St, 2, Length(St));
  PushConst(St);
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
  // $ + numéro de la primitive empilé
  fCommandsStack.Push(CLink + IntToStr(Num));
  if N = 0 then // pas de paramètres ?
    ExePrim(Num); // exécute immédiatement la primitive
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
    Error := C_NorPrimNorProc; // [Erreur: ni une primitive ni une procédure
end;

procedure TGVInterpreter.LEval(const AValue: string);
// *** traitement d'une évaluation ***
begin
  fEval.Text := AValue; // on affecte l'évaluateur
  fEVal.Scan; // on évalue
  if (fEval.Error = C_None) then  // pas d'erreur ?
    PushConst(FloatToStr(fEval.Res)) // valeur empilée
  else
    Error := fEval.Error; // [Erreur signalée]
end;

procedure TGVInterpreter.SetError(AValue: TGVError);
// *** traitement d'une erreur ***
begin
  if fError = AValue then
    Exit; // on sort si aucun changement
  fError := AValue; // nouvelle valeur d'erreur
  ErrorEvent; // notification du changement
end;

procedure TGVInterpreter.ExePrim(N: Integer);
// *** exécution d'une primitive
var
  Prm: Integer;
  St: string;
  L: TGVList;
  W: TGVWord;

  {$I Prims.inc} // fichier des primitives

begin
  L := TGVList.Create; // liste de travail
  W := TGVWord.Create; // mot de travail
  try
    if (N = -1) then // numéro primitive à rechercher ?
    begin
      St := fCommandsStack.Peek; // $ + numéro de la primitive
      N := StrToInt(Copy(St, 2, Length(St) - 1)); // numéro retrouvé
    end;
    fParamsStack.Pop; // on libère les paramètres
    Prm := fParamsStack.Pop; // nombre de paramètres de la primitive
    while Prm <> 0 do // tant qu'il y a des paramètres
    begin
      // on empile les paramètres sur la pile d'exécution
      fExeStack.Push(fDatasStack.Pop);
      Dec(Prm); // paramètre suivant
    end;
    // on retire la primitive de la pile des commandes et on la conserve
    ActualItem := GVPrimName[(StrToInt(Copy(fCommandsStack.Pop, 2, 127)))].Name;
    case N of // exécution

      70: BadTo; // POUR
      71: BadEnd; // FIN
      72, 73: First; // PREM PREMIER
      74, 75: Last;  // DER DERNIER
      76, 77: ButFirst; // SAUFPREMIER SP
      78, 79: ButLast; // SAUFDERNIER SD
      80: PTrue; // VRAI
      81: PFalse; // FAUX
    {$IFDEF Debug}
      82: fLines.Add(fExeStack.Pop); // ECRIS
    {$ENDIF}
      83: WriteAll; // ECRIST
      84, 85: PutFirst; // METSPREMIER
      86, 87: PutLast; // METSDERNIER
      88: Insert; // INSERE
      89: Reverse; // INVERSE
      90: UpperCase; // MAJUSCULES
      91: LowerCase; // MINUSCULES
      92: Shuffle; // MELANGE
      93: Replace; // REMPLACE
      94: Sort; // TRIE
      95: Rotate; // ROTATION
    end;
  finally
    Change; // changement notifié
    L.Free; // libération de la liste des travail
    W.Free; // idem mot de travail
  end;
end;

procedure TGVInterpreter.ExeProc;
// *** exécution d'une procédure ***
begin
  // ### TODO ###
end;

procedure TGVInterpreter.ExeCommand;
// *** exécution de la commande en attente ***
begin
  if fCommandsStack.Peek[1] = CLink then // $ ?
    ExePrim(-1) // c'est une primitive
  else
    ExeProc; // sinon une procédure
end;

procedure TGVInterpreter.ComputeLine(const St: string);
// *** interprète une ligne ***
var
  L: TGVList;
  S: string;
begin
  fActualLine := St; // on conserve la ligne à analyser
  L := TGVList.Create; // création de la liste
  try
    L.Text := CBeginList + St + CEndList;
    for S in L do // on balaie les éléments
    begin
      ActualItem := S; // élément en cours
      case S[1] of // premier caractère déterminant
        CQuote: LWord(S); // un littéral
        CBeginList: LList(S); // une liste
        CColon: LVar(S); // une variable
        CPlus, CMinus, '0'..'9' : LNumber(S); // un nombre
        'a'..'z','A'..'Z',CUnderline, CDot : LCommand(S); // une commande
        CBeginPar: LEval(S); // une valeur à évaluer
      end;
      if Error <> C_None then // une erreur ?
        Break; // on arrête
    end;
  finally
    L.Free; // libération de la liste
  end;
end;

procedure TGVInterpreter.Change;
// *** notification de changement ***
begin
  if Assigned(fOnChange) then // gestionnaire assigné ?
    fOnChange(self); // on l'exécute
end;

procedure TGVInterpreter.ErrorEvent;
// *** notification d'erreur ***
begin
  if Assigned(fOnError) and (Error <> C_None) then // gestionnaire assigné et erreur ?
    fOnError(self); // on l'exécute
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

