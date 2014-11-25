{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    22-11-2014 17:00:42                          |
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
  CComma = ',';
  CExclamation = '!';
    
  // ************* GVEval *************
type
  // éléments d'une expression à évaluer
  CTokensEnum = (cteInteger, cteReal, cteVar, cteFunction, cteBeginExp, cteEndExp,
    ctePlus, cteMinus, cteMul, cteDiv, ctePower, cteGreater, cteLower, cteEqual,
    cteNotEqual, cteGreaterOrEqual, cteLowerOrEqual, cteMod, cteNot, cteAnd,
    cteOr, cteEnd, cteOrB, cteAndB, cteBoolean, cteUnKnown, cteForbidden,
    cteNotSupported);

  // élément de base de l'expression
  TGVBaseItem = record
    Token: string; // élément
    Kind: CTokensEnum; // type d'élément
  end;

const
  // priorité des éléments d'une expression
  // nombre le plus élevé = priorité la moins élevée
  // -1 : ne s'applique pas
  // ( )
  // 1: non
  // 2: * / % mod
  // 3: + -
  // 4: > < <= >=
  // 5: = <> !=
  // 6: &
  // 7: |
  // 8: et
  // 9: ou
  // 10: ^ puissance
  CTokenPrecedence: array[CTokensEnum] of Integer = (-1, -1, -1, -1, 10, 10, 3,
    3, 2, 2, 10, 4, 4, 5, 5, 4, 4, 2, 1, 8, 9, -1, 7, 6, -1, -1, -1, -1);
  // associativité des éléments (1 = droite 0 = gauche -1 = ne s'applique pas)
  CTokenAssociation: array[CTokensEnum] of Integer =
    (-1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
      0, -1, -1, -1, -1);
const
  CPlus = '+'; // addition
  CMinus = '-'; // soustraction
  CMul = '*'; // multiplication
  CDiv = '/'; // division
  CPower = '^'; // puisssance
  CGreater = '>'; // plus grand
  CLower = '<'; // plus petit
  CEqual = '='; // égal
  CNotEqual = '<>'; // différent
  CNotEqual2 = '!='; // différent (2)
  CNot = '!'; // négation
  COrB = '|'; // ou binaire
  CAndB = '&'; // et binaire
  CGreaterOrEqual = '>='; // plus grand ou égal
  CLowerOrEqual = '<='; // plus petit ou égal
  // caractères spéciaux
  CSpecialChar = [CBlank, CEqual, CPlus, CMinus, CMul, CDiv, CBeginPar, CEndPar,
    CPower, CGreater, CLower];
  CRLower = -1; // résultats de comparaisons (plus petit)
  CREqual = 0;  // (égal)
  CRGreater = 1; // (plus grand)
  CRTrue = 1; // vrai
  CRFalse = 0; // faux
  
  { ensembles de caractères courants }

  CLowAlpha = ['a'..'z'];
  CHighAlpha = ['A'..'Z'];
  CAlpha = CLowAlpha + CHighAlpha;
  CAlphaPlus = CAlpha + [CUnderline, CColon, CDot];
  CDigit = ['0'..'9'];
  CAlphaNum = CAlpha + CDigit;
  CAlphaNumPlus = CAlphaPlus + CDigit;
  CAllValidChars = CAlphaNumPlus + CSpecialChar;

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
  C_InternalError, // erreur interne
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
  C_LowStack, // pile insuffisante
  C_NoInit, // valeur non initialisée
  C_BadChar2, // caractère interdit ou inconnu
  C_ClosePar, // parenthèse fermante absente
  C_BadVar, // variable incorrecte
  C_UnknownVar, // variable inconnue
  C_BadFunction, // fonction inconnue
  C_NoArg, // argument manquant
  C_BadExp, // expression incorrecte
  C_Zero, // division par zéro
  C_NegNumber, // nombre négatif interdit
  C_OutOfRange, // index hors limites pour une expression
  C_OutOfRange2, // élément hors limites d'une expression
  C_NotSupported, // élément non supporté dans une expression
  C_ParMismatch // parenthèses non concordantes
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
  ME_InternalError = 'ERREUR INCONNUE (erreur interne).';
  
  // ************* GVList et GVPropList *************
  
  ME_BadNumber = 'Le nombre "%s" est incorrect.';
  ME_BadInt = '"%s" n''est pas un entier correct.';
  ME_EmptyStr = 'Le mot vide ne convient pas pour la primitive "%s".';
  ME_BadChar = 'Le mot "%s" est trop court pour en traiter l''élément %d.';
  ME_BadList = 'La liste "%s" est incorrecte.';
  ME_DelItem = 'L''élément %d n''existe pas pour une suppression.';
  ME_InsItem = 'L''élément %d n''existe pas pour une insertion.';
  ME_ReplaceItem = 'L''élément %d n''existe pas pour un remplacement.';
  ME_NoListWord = '"%s" n''est ni une liste ni un mot corrects.';
  ME_TwoDelete = 'La liste ne contient pas assez d''éléments pour en supprimer deux à partir de %d.';
  ME_BadListP = 'La liste de propriétés %d est introuvable.';
  ME_BadFormat = 'Le format du fichier "%s" est incorrect : %s.';
  
  // ************* GVStacks *************
  
  ME_EmptyStack = 'La pile interne est vide.';
  ME_OutOfMemory = 'La mémoire est insuffisante pour la pile.';
  ME_LowStack = 'Pas assez d''éléments dans la pile (%d pour %d).';

  // ************* GVEval *************

  ME_NoInit = 'La valeur à évaluer n''a pas été initialisée.';
  ME_BadChar2 = 'Caractère interdit ou inconnu dans "%s".';
  ME_ClosePar = 'Parenthèse fermante absente dans "%s".';
  ME_BadVar = 'La variable "%s" est incorrecte.';
  ME_UnknownVar = 'La variable "%s" est inconnue.';
  ME_BadFunction = 'La fonction "%s" est inconnue.';
  ME_NoArg = 'Il manque un argument pour "%s".';
  ME_BadExp = 'Il y a une expression incorrecte dans "%s".';
  ME_Zero = 'Les divisions par 0 sont impossibles. ("%s")';
  ME_NegNumber = 'Un nombre négatif est interdit pour "%s".';
  ME_OutOfRange = 'Evaluation hors limites : %d pour "%s".';
  ME_OutOfRange2 = 'Evaluation hors limites : élément %d de "%s".';
  ME_NotSupported = 'La fonction "%s" n''est pas utilisable dans une expression.';
  ME_ParMismatch = 'Les parenthèses de l''expression ne sont pas appariées.';

  // ************* PRIMITIVES *************

  { primitives de base }

  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

  { noms des fonctions mathématiques}

  MF_Unknown = ''; // fonction non définie
  MF_DAbs = 'ABS'; // valeur absolue
  MF_DAbs2 = 'ABSOLUE';
  MF_DCos = 'COS'; // cosinus
  MF_DCos2 = 'COSINUS';
  MF_DSin = 'SIN'; // sinus
  MF_DSin2 = 'SINUS';
  MF_DTan = 'TAN'; // tangente
  MF_DTan2 = 'TANGENTE';
  MF_DSqrt = 'RAC'; // racine carrée
  MF_DSqrt2 = 'RACINE';
  MF_DTrunc = 'TRONQUE'; // nombre tronqué
  MF_DRound = 'ARRONDI'; // nombre arrondi
  MF_DSqr = 'CARRE'; // nombre au carré
  MF_DExp = 'EXP'; // exponentielle
  MF_DFrac = 'FRAC'; // partie fractionnelle
  MF_DInt = 'ENT'; // partie entière
  MF_DInt2 = 'ENTIER';
  MF_DLn = 'LN'; // log népérien
  MF_DLog2 = 'LOG2'; // log base 2
  MF_DLog10 = 'LOG10'; // log base 100
  MF_DCoTan = 'COTAN'; // cotangente
  MF_DCoTan2 = 'COTANGENTE';
  MF_DHypot = 'HYPOTHENUSE'; // hypothénuse
  MF_DArcCos = 'ARCCOS'; // arc cosinus
  MF_DArcCos2 = 'ARCCOSINUS';
  MF_DArcSin = 'ARCSIN'; // arc sinus
  MF_DArcSin2 = 'ARCSINUS';
  MF_DMinus = 'NEGATIF'; // nombre négatif
  MF_DPLus = 'POSITIF'; // nombre positif
  MF_DNegate = 'OPPOSE'; // signe inversé
  MF_DSign = 'SIGNE'; // signe
  MF_DRandom = 'HASARD'; // nombre au hasard
  MF_Not = 'NON'; // négation
  // fonctions sans paramètres
  MF_DPi = 'PI'; // PI sur la pile
  MF_True = 'VRAI'; // valeur vrai
  MF_False = 'FAUX'; // valeur faux
  // fonctions infixées
  MF_Or = 'OU'; // ou logique
  MF_And = 'ET'; // et logique
  MF_Mod = 'MOD'; // modulo
  MF_DPower = 'PUISSANCE'; // puissance
  // deux opérateurs
  MF_DMax = 'MAX'; // maximum
  MF_DMax2 = 'MAXIMUM';
  MF_DMin = 'MIN'; // minimum
  MF_DMin2 = 'MINIMUM';

 // ************* GVEval *************
 
 type
  // ensemble des fonctions mathématiques de base
  TGVFunctions = (
    C_Unknown, // fonction inconnue
    C_DAbs, // valeur absolue
    C_DAbs2,
    C_DCos, // cosinus
    C_DCos2,
    C_DSin, // sinus
    C_DSin2,
    C_DTan, // tangente
    C_DTan2,
    C_DSqrt, // racine carrée
    C_DSqrt2,
    C_DTrunc, // nombre tronqué
    C_DRound, // nombre arrondi
    C_DSqr, // nombre au carré
    C_DExp, // exponentielle
    C_DFrac, // partie fractionnelle
    C_DInt, // partie entière
    C_DInt2,
    C_DLn, // log népérien
    C_DLog2, // log base 2
    C_DLog10, // log base 100
    C_DCoTan, // cotangente
    C_DCoTan2,
    C_DHypot, // hypothénuse
    C_DArcCos, // arc cosinus
    C_DArcCos2,
    C_DArcSin, // arc sinus
    C_DArcSin2,
    C_Minus, // négatif
    C_Plus, // positif
    C_DNegate, // signe inversé
    C_DSign, // signe
    C_DRandom, // nombre au hasard,
    C_Not, // not
    C_DPi, // PI sur la pile
    C_True, // vrai
    C_False, // faux
    C_Or, // ou
    C_And, // et
    C_Mod, // mod
    C_DPower, // puissance
    C_DMax, // maximum
    C_DMax2,
    C_DMin, // minimum
    C_DMin2
   );

const
  // tableau du nom des erreurs
  GVErrorName: array[TGVError] of string = (ME_None,ME_InternalError,
    ME_BadNumber, ME_BadInt, ME_EmptyStr, ME_BadChar, ME_BadList, ME_DelItem,
    ME_InsItem, ME_ReplaceItem, ME_NoListWord, ME_TwoDelete, ME_BadListP,
    ME_BadFormat, ME_EmptyStack, ME_OutOfMemory, ME_LowStack, ME_NoInit,
    ME_BadChar2, ME_ClosePar, ME_BadVar, ME_UnknownVar, ME_BadFunction,
    ME_NoArg, ME_BadExp, ME_Zero, ME_NegNumber, ME_OutOfRange, ME_OutOfRange2,
    ME_NotSupported, ME_ParMismatch);

  // tableau du nom des fonctions
  GVFunctionName: array [TGVFunctions] of string = (MF_Unknown, MF_DAbs,
    MF_DAbs2, MF_DCos, MF_DCos2, MF_DSin, MF_DSin2, MF_DTan, MF_DTan2, MF_DSqrt,
    MF_DSqrt2, MF_DTrunc, MF_DRound, MF_DSqr, MF_DExp, MF_DFrac, MF_DInt,
    MF_DInt2, MF_DLn, MF_DLog2, MF_DLog10, MF_DCoTan, MF_DCoTan2, MF_DHypot,
    MF_DArcCos, MF_DArcCos2, MF_DArcSin, MF_DArcSin2,  MF_DMinus,
    MF_DPLus, MF_DNegate,  MF_DSign,
    MF_DRandom, MF_Not, MF_DPi, MF_True, MF_False, MF_Or, MF_And, MF_Mod,
    MF_DPower, MF_DMax, MF_DMax2, MF_DMin, MF_DMin2);
	
implementation

end.
