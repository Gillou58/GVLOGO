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
  GVConsts, GVStacks, GVKernel, GVTurtles2;

implementation

end.

