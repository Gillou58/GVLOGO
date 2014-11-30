{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Intepréteur                             |
  |                  Unité : GVInterpreter.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    30-11-2014 12:32:23                          |
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
  Classes, SysUtils,
  GVConsts, GVStacks, GVKernel, GVTurtles2, GVEval;

type

  { TGVInterpreter }

  TGVInterpreter = class(TObject)
  private
    fKernel: TGVLogoKernel; // noyau de travail
    fParamsStack: TGVIntegerStack; // pile des paramètres
    fDatasStack: TGVStringStack; // pile des données
    fCommandsStack: TGVStringStack; // pile des commandes
    fExeStack: TGVStringStack; // pile d'exécution
    fTurtle: TGVTurtle; // tortue graphique
    fEval: TGVEval; // évaluateur d'expressions
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
  end;

implementation

{ TGVInterpreter }

constructor TGVInterpreter.Create;
// *** création ***
begin
  fKernel := TGVLogoKernel.Create; // noyau de travail
  fParamsStack := TGVIntegerStack.Create; // pile des paramètres
  fDatasStack := TGVStringStack.Create; // pile des données
  fCommandsStack := TGVStringStack.Create; // pile des commandes
  fExeStack := TGVStringStack.Create; // pile d'exécution
  fTurtle := TGVTurtle.Create(400,400); // tortue graphique
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
  fTurtle.Free; // tortue graphique
  fEval.Free; // évaluateur d'expressions
end;

end.

