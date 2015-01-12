{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Définitions                             |
  |                  Fichier : GVDefines.inc                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVDEFINES.INC - part of GVLOGO
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

// Ce fichier contient les définitions nécessaires
// à la compilation de GVLOGO.
// Il est compris dans toutes les unités du projet.


// Lazarus (défini par défaut)
{$DEFINE CompLazarus }

// options de compilation
// Lazarus ou Delphi 

{$IFDEF CompLazarus}
   {$MODE objfpc}{$H+}
   {$UNDEF CompDelphi}
 {$ELSE}
   {$DEFINE CompDelphi}
{$ENDIF}

// option de débogage (défini par défaut)
{$DEFINE Debug}
