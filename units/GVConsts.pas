{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 17:17:49                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


  
// GVConsts - part of GVLOGO
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
  
unit GVConsts;

interface

uses
  Graphics;

const
  { séparateurs }

  CBlank = ' ';
  CBeginList = '[';
  CEndList = ']';
  CBeginPar = '(';
  CEndPar = ')';
  CSeparators = [CBlank, CBeginList, CEndList, CBeginPar, CEndPar];

  { caractères spéciaux }

  CLink = '$';
  CUnderline = '_';
  CDot = '.';
  CAsk = '?';
  CQuote = '"';
  CColon = ':';

  { ensembles de caractères courants }

  CLowAlpha = ['a' .. 'z'];
  CHighAlpha = ['A' .. 'Z'];
  CAlpha = CLowAlpha + CHighAlpha;
  CDigit = ['0' .. '9'];
  CAlphaNum = CAlpha + CDigit;

  { chaînes utiles }

  CEmptyList = CBeginList + CEndList;

  { listes de propriétés}

  // extension pour les fichiers de listes de propriétés
  CExtPl = '.GPL';
  // entête de fichier
  CHeader = '[GPL100 (c) GV 2014]';
  // séparateur de liste de propriétés
  CSep = '|';

  { tortue }

  DgToRad = Pi / 180; // pour les conversions en radians
  RadToDg = 180 / Pi; // pour les conversions en degrés
  CDefaultScale = 100; // échelle par défaut
  CDefaultHeading = 90; // cap par défaut
  CDefaultSize = 8; // taille d'une tortue par défaut
  CMaxSize = 20; // taille maximale de la tortue
  CMaxSpeed = 100; // vitesse maximum de la tortue
  CDefaultPenColor = clWhite; // couleur de fond par défaut
  CDefaultBackColor = clBlack; // couleur du crayon par défaut
  CDefaultPenWidth = 1; // largeur du crayon par défaut

  { piles }

  CMinStack = 8; // minimum d'espace pour une pile

type
  { erreurs }
  TGVError = (
  C_None, // pas d'erreur
  C_BadNumber, // nombre incorrect
  C_BadInt, // entier incorrect
  C_EmptyStr, // mot vide interdit
  C_BadChar, // caractère incorrect
  C_BadList, // erreur dans une liste
  C_DelItem,  // position incorrecte pour une suppression
  C_InsItem, // position incorrecte pour une insertion
  C_ReplaceItem, // position incorrecte pour un remplacement
  C_NoListWord, // ni un mot ni une liste
  C_TwoDelete, // pas assez d'éléments pour en supprimer deux
  C_BadListP, // liste de propriétés incorrecte
  C_BadFormat, // fichier de format erroné
  C_EmptyStack, // pile interne vide
  C_OutOfMemory, // mémoire insuffisante pour la pile
  C_LowStack // pile insuffisante
  );

  { tortue }

  // type d'écrans : enroule, fenêtre illimitée ou champ clos
  TScreenTurtle = (teWin, teGate, teRoll);
  // types de tortue
  TTurtleKind = (tkTriangle, tkPng, tkOwner);

  { piles }

  TGVStackNotification = (stAdded, stRemoved, stChanged, stCleared);

resourcestring
  { message d'erreur }

  ME_None = 'Pas d''erreur à signaler.';
  ME_BadNumber = 'L''objet %s n''est pas un nombre correct.';
  ME_BadInt = 'L''objet %s n''est pas un entier correct.';
  ME_EmptyStr = 'Le mot vide ne convient pas pour la primitive %s.';
  ME_BadChar = 'Le mot %s est trop court pour en traiter l''élément %d.';
  ME_BadList = 'La liste %s est incorrecte.';
  ME_DelItem = 'L''élément %d n''existe pas pour une suppression.';
  ME_InsItem = 'L''élément %d n''existe pas pour une insertion.';
  ME_ReplaceItem = 'L''élément %d n''existe pas pour un remplacement.';
  ME_NoListWord = '%s n''est ni une liste ni un mot corrects.';
  ME_TwoDelete = 'La liste ne contient pas assez d''éléments pour en supprimer deux à partir de %d.';
  ME_BadListP = 'La liste de propriétés %d est introuvable.';
  ME_BadFormat = 'Le format du fichier %s est incorrect : %s.';
  ME_EmptyStack = 'La pile interne est vide.';
  ME_OutOfMemory = 'La mémoire est insuffisante pour la pile.';
  ME_LowStack = 'Pas assez d''éléments dans la pile (%d pour %d).';

  { primitives }

  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

implementation

end.
