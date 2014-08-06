{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    27-07-2014 17:17:49                          |
  |                  Version : 2.0.0                                       |
  |                                                                        |
  |========================================================================| }

unit GVConsts;

interface

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
  CQUote = '"';
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
  CHeader = '[GPL200 (c) GV 2014]';
  // séparateur de liste de propriétés
  CSep = '|';

  { tortue }

  DgToRad = Pi / 180; // pour les conversions en radians
  RadToDg = 180 / Pi; // pour les conversions en degrés
  DefaultScale = 100; // échelle par défaut
  DefaultHeading = 90; // cap par défaut
  TurtleDefaultSize = 8; // taille d'une tortue par défaut
  TurtleMaxSize = 20; // taille maximale de la tortue
  TurtleMaxSpeed = 100; // vitesse maximum de la tortue

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
  C_BadFormat // fichier de format erroné
  );

  { tortue }

  // type d'écrans : enroule, fenêtre illimitée ou champ clos
  TScreenTurtle = (teWin, teGate, teRoll);
  // types de tortue
  TTurtleKind = (tkTriangle, tkPng, tkOwner);

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
  ME_BadFormat = 'Le format du fichier %s est incorrect : %.';

  { primitives }

  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

implementation

end.
