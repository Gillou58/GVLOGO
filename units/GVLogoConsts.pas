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

const
  CMaxHistoryEntries = 10; // mémorisation des entrées
  CExt = '.GVL'; // extension par défaut
  CPrimDefs = 'GVPrimDefs.txt'; // définitions des primitives

type
  // *** types de recherches ***
  TSearchKind = (skFind, skFindNext, skReplace, skNextTo, skPrevTo,
    skNextEnd, skPrevEnd);

resourcestring
  // *** chaînes des messages ***
  CrsUnknownFile = 'sans_nom' + CExt; // fichier par défaut
  CrsLoad = 'Le fichier "%s" a été chargé.';
  CrsErrLoad = 'Le fichier "%s" n''a pas pu être chargé.';
  CrsSaved = 'Le fichier "%s" a été sauvegardé.';
  CrsErrSaved = 'Le fichier "%s" n''a pas pu être sauvegardé.';
  CrsReplaceFile = '"%s" existe déjà. Voulez-vous le remplacer ?';
  CrsModified = 'Modifié';
  CrsOk = 'OK';
  CrsInfo = 'Information';
  CrsConfirm = 'Confirmation';
  CrsAsk = 'Question';
  CrsSave = 'Voulez-vous enregistrer "%s" ?';
  CrsLine = 'Ligne: ';
  CrsCol = 'Colonne: ';
  CrsClose = '&Fermer';
  CrsCloseHint = 'Fermer la fenêtre';
  CrsInterpreter = ' L''éditeur a été correctement interprété.';
  CrsNotFound = 'Le texte "%s" n''a pas été trouvé.';
  CrsAskForValue = 'Entrez la valeur demandée ici :';
  CrsStop = 'Le programme a été arrêté.';
  CrsProcs = 'Liste des procédures disponibles (%d)';
  CrsNoPrimHelp = 'Le fichier "%s" n''est pas accessible. L''aide sur les ' +
    'primitives ne sera pas disponible.';
  CrsFollowLine = '[%d]  Donnée : %s - Num : %d - Prim : %s - Proc : %s';
  CrsAlreadyAProc = '"%s" existe déjà ! Voulez-vous l''éditer ?';
  CrsBadParams = 'La ligne de paramètres "%s" est incorrecte.';
  CrsEmptyNameProc = 'Le nom vide pour une procédure est incorrect.';
implementation

end.

