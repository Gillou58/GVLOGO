{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes pour GVLOGO final            |
  |                  Unité : GVLOGOConsts.pas                              |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVLOGOCONSTS - part of GVLOGO
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

unit GVLogoConsts;

interface

uses
  Classes, SysUtils;

type
  // *** types de recherches ***
  TSearchKind = (skFind, skFindNext, skReplace);

resourcestring
  // *** chaînes des messages ***
  CrsLoad = 'Les procédures du fichier "%s" ont été chargées.';
  CrsModified = 'Modifié';
  CrsOk = 'OK';
  CrsInfo = 'Information';
  CrsConfirm = 'Confirmation';
  CrsSave = 'Voulez-vous enregistrer %s ?';
  CrsLine = 'Ligne: ';
  CrsCol = 'Colonne: ';
  CrsClose = '&Fermer';
  CrsCloseHint = 'Fermer la fenêtre';
  CrsInterpreter = ' L''éditeur a été correctement interprété.';
  CrsNotFound = 'Le texte "%s" n''a pas été trouvé.';


implementation

end.
