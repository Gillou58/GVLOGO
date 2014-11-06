{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unit� : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
  |                  Date:    27-10-2014 16:15:42                          |
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
  { s�parateurs }

  CBlank = ' ';
  CBeginList = '[';
  CEndList = ']';
  CBeginPar = '(';
  CEndPar = ')';
  CSeparators = [CBlank, CBeginList, CEndList, CBeginPar, CEndPar];

  { caract�res sp�ciaux }

  CLink = '$';
  CUnderline = '_';
  CDot = '.';
  CAsk = '?';
  CQuote = '"';
  CColon = ':';
  CExclamation = '!';
    
  // ************* GVEval *************
type
  // �l�ments d'une expression � �valuer
  CTokensEnum = (cteNumber, cteVar, cteFunction, cteBeginExp, cteEndExp, ctePlus,
    cteMinus, cteMul, cteDiv, ctePower, cteGreater, cteLower, cteEqual, cteNotEqual,
    cteGreaterOrEqual, cteLowerOrEqual, cteMod, cteAnd, cteOr, cteNot, cteEnd,
    cteUnKnown);

const
  CPlus = '+'; // addition
  CMinus = '-'; // soustraction
  CMul = '*'; // multiplication
  CDiv = '/'; // division
  CPower = '^'; // puisssance
  CGreater = '>'; // plus grand
  CLower = '<'; // plus petit
  CEqual = '='; // �gal
  CNotEqual = '<>'; // diff�rent
  CGreaterOrEqual = '>='; // plus grand ou �gal
  CLowerOrEqual = '<='; // plus petit ou �gal
  // caract�res sp�ciaux
  CSpecialChar = [CBlank, CEqual, CPlus, CMinus, CMul, CDiv, CBeginPar, CEndPar,
    CPower, CGreater, CLower];
  CRLower = -1; // r�sultats de comparaisons (plus petit)
  CREqual = 0;  // (�gal)
  CRGreater = 1; // (plus grand)
  CRTrue = 1; // vrai
  CRFalse = 0; // faux
  
  { ensembles de caract�res courants }

  CLowAlpha = ['a'..'z'];
  CHighAlpha = ['A'..'Z'];
  CAlpha = CLowAlpha + CHighAlpha;
  CAlphaPlus = CAlpha + [CUnderline, CColon, CDot];
  CDigit = ['0'..'9'];
  CAlphaNum = CAlpha + CDigit;
  CAlphaNumPlus = CAlphaPlus + CDigit;
  CAllValidChars = CAlphaNumPlus + CSpecialChar;

  { cha�nes utiles }

  CEmptyList = CBeginList + CEndList;

  { listes de propri�t�s}

  // extension pour les fichiers de listes de propri�t�s
  CExtPl = '.GPL';
  // ent�te de fichier
  CHeader = '[GPL100 (c) GV 2014]';
  // s�parateur de liste de propri�t�s
  CSep = '|';

  { tortue }

  DgToRad = Pi / 180; // pour les conversions en radians
  RadToDg = 180 / Pi; // pour les conversions en degr�s
  CDefaultScale = 100; // �chelle par d�faut
  CDefaultHeading = 90; // cap par d�faut
  CDefaultSize = 8; // taille d'une tortue par d�faut
  CMaxSize = 20; // taille maximale de la tortue
  CMaxSpeed = 100; // vitesse maximum de la tortue
  CDefaultPenColor = clWhite; // couleur de fond par d�faut
  CDefaultBackColor = clBlack; // couleur du crayon par d�faut
  CDefaultPenWidth = 1; // largeur du crayon par d�faut

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
  C_BadChar, // caract�re incorrect
  C_BadList, // erreur dans une liste
  C_DelItem,  // position incorrecte pour une suppression
  C_InsItem, // position incorrecte pour une insertion
  C_ReplaceItem, // position incorrecte pour un remplacement
  C_NoListWord, // ni un mot ni une liste
  C_TwoDelete, // pas assez d'�l�ments pour en supprimer deux
  C_BadListP, // liste de propri�t�s incorrecte
  C_BadFormat, // fichier de format erron�
  C_EmptyStack, // pile interne vide
  C_OutOfMemory, // m�moire insuffisante pour la pile
  C_LowStack, // pile insuffisante
  C_NoInit, // valeur non initialis�e
  C_BadChar2, // caract�re interdit ou inconnu
  C_ClosePar, // parenth�se fermante absente
  C_BadNumber2, // nombre incorrect
  C_BadVar, // variable incorrecte
  C_UnknownVar, // variable inconnue
  C_BadFunction, // fonction inconnue
  C_NoArg, // argument manquant
  C_BadExp, // expression incorrecte
  C_Zero, // division par z�ro
  C_NegNumber, // nombre n�gatif interdit
  C_OutOfRange, // index hors limites pour une expression
  C_OutOfRange2 // �l�ment hors limites d'une expression
  );

  { tortue }

  // type d'�crans : enroule, fen�tre illimit�e ou champ clos
  TScreenTurtle = (teWin, teGate, teRoll);
  // types de tortue
  TTurtleKind = (tkTriangle, tkPng, tkOwner);

  { piles }

  TGVStackNotification = (stAdded, stRemoved, stChanged, stCleared);

resourcestring
  { message d'erreur }

  ME_None = 'Pas d''erreur � signaler.';
  ME_InternalError = 'ERREUR INCONNUE (erreur interne).';
  
  // ************* GVList et GVPropList *************
  
  ME_BadNumber = 'L''objet %s n''est pas un nombre correct.';
  ME_BadInt = 'L''objet %s n''est pas un entier correct.';
  ME_EmptyStr = 'Le mot vide ne convient pas pour la primitive %s.';
  ME_BadChar = 'Le mot %s est trop court pour en traiter l''�l�ment %d.';
  ME_BadList = 'La liste %s est incorrecte.';
  ME_DelItem = 'L''�l�ment %d n''existe pas pour une suppression.';
  ME_InsItem = 'L''�l�ment %d n''existe pas pour une insertion.';
  ME_ReplaceItem = 'L''�l�ment %d n''existe pas pour un remplacement.';
  ME_NoListWord = '%s n''est ni une liste ni un mot corrects.';
  ME_TwoDelete = 'La liste ne contient pas assez d''�l�ments pour en supprimer deux � partir de %d.';
  ME_BadListP = 'La liste de propri�t�s %d est introuvable.';
  ME_BadFormat = 'Le format du fichier %s est incorrect : %s.';
  
  // ************* GVStacks *************
  
  ME_EmptyStack = 'La pile interne est vide.';
  ME_OutOfMemory = 'La m�moire est insuffisante pour la pile.';
  ME_LowStack = 'Pas assez d''�l�ments dans la pile (%d pour %d).';

  // ************* GVEval *************

  ME_NoInit = 'La valeur � �valuer n''a pas �t� initialis�e.';
  ME_BadChar2 = 'Caract�re interdit ou inconnu dans "%s".';
  ME_ClosePar = 'Parenth�se fermante absente dans "%s".';
  ME_BadNumber2 = 'Le nombre "%s" est incorrect.';
  ME_BadVar = 'La variable "%s" est incorrecte.';
  ME_UnknownVar = 'La variable "%s" est inconnue.';
  ME_BadFunction = 'La fonction "%s" est inconnue ou inappropri�e.';
  ME_NoArg = 'Il manque un argument pour "%s".';
  ME_BadExp = 'Il y a une expression incorrecte dans "%s".';
  ME_Zero = 'Les divisions par z�ro sont impossibles. (%s)';
  ME_NegNumber = 'Un nombre n�gatif est interdit pour %s.';
  ME_OutOfRange = 'Evaluation hors limites : %d pour "%s."';
  ME_OutOfRange2 = 'Evaluation hors limites : �l�ment %d de "%s."';

  // ************* PRIMITIVES *************

  { primitives de base }

  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

  { noms des fonctions math�matiques}
  
  MF_DAbs = 'ABS'; // valeur absolue
  MF_DAbs2 = 'ABSOLUE';
  MF_DCos = 'COS'; // cosinus
  MF_DCos2 = 'COSINUS';
  MF_DSin = 'SIN'; // sinus
  MF_DSin2 = 'SINUS';
  MF_DTan = 'TAN'; // tangente
  MF_DTan2 = 'TANGENTE';
  MF_DSqrt = 'RAC'; // racine carr�e
  MF_DSqrt2 = 'RACINE';
  MF_DTrunc = 'TRONQUE'; // nombre tronqu�
  MF_DRound = 'ARRONDI'; // nombre arrondi
  MF_DSqr = 'CARRE'; // nombre au carr�
  MF_DExp = 'EXP'; // exponentielle
  MF_DFrac = 'FRAC'; // partie fractionnelle
  MF_DInt = 'ENT'; // partie enti�re
  MF_DInt2 = 'ENTIER';
  MF_DLn = 'LN'; // log n�p�rien
  MF_DLog2 = 'LOG2'; // log base 2
  MF_DLog10 = 'LOG10'; // log base 100
  MF_DCoTan = 'COTAN'; // cotangente
  MF_DCoTan2 = 'COTANGENTE';
  MF_DHypot = 'HYPOTHENUSE'; // hypoth�nuse
  MF_DArcCos = 'ARCCOS'; // arc cosinus
  MF_DArcCos2 = 'ARCCOSINUS';
  MF_DArcSin = 'ARCSIN'; // arc sinus
  MF_DArcSin2 = 'ARCSINUS';
  MF_DPower = 'PUISSANCE'; // puissance
  MF_DMinus = 'NEGATIF'; // nombre n�gatif
  MF_DPLus = 'POSITIF'; // nombre positif
  MF_DNegate = 'OPPOSE'; // signe invers�
  MF_DMax = 'MAX'; // maximum
  MF_DMax2 = 'MAXIMUM';
  MF_DMin = 'MIN'; // minimum
  MF_DMin2 = 'MINIMUM';
  MF_DSign = 'SIGNE'; // signe
  MF_DRandom = 'HASARD'; // nombre au hasard
  // fonctions sans param�tres
  MF_DPi = 'PI'; // PI sur la pile
  MF_True = 'VRAI'; // valeur vrai
  MF_False = 'FAUX'; // valeur faux

 // ************* GVEval *************
 
 type
  // ensemble des fonctions math�matiques de base
  TGVFunctions = (
    C_DAbs, // valeur absolue
    C_DAbs2,
    C_DCos, // cosinus
    C_DCos2,
    C_DSin, // sinus
    C_DSin2,
    C_DTan, // tangente
    C_DTan2,
    C_DSqrt, // racine carr�e
    C_DSqrt2,
    C_DTrunc, // nombre tronqu�
    C_DRound, // nombre arrondi
    C_DSqr, // nombre au carr�
    C_DExp, // exponentielle
    C_DFrac, // partie fractionnelle
    C_DInt, // partie enti�re
    C_DInt2,
    C_DLn, // log n�p�rien
    C_DLog2, // log base 2
    C_DLog10, // log base 100
    C_DCoTan, // cotangente
    C_DCoTan2,
    C_DHypot, // hypoth�nuse
    C_DArcCos, // arc cosinus
    C_DArcCos2,
    C_DArcSin, // arc sinus
    C_DArcSin2,
    C_DPower, // puissance
    C_Minus, // n�gatif
    C_Plus, // positif
    C_DNegate, // signe invers�
    C_DMax, // maximum
    C_DMax2,
    C_DMin, // minimum
    C_DMin2,
    C_DSign, // signe
    C_DRandom, // nombre au hasard
    C_DPi, // PI sur la pile
    C_True, // vrai
    C_False // faux
   );

const
  // tableau du nom des fonctions
  GVFunctionName: array [TGVFunctions] of string = (MF_DAbs, MF_DAbs2, MF_DCos,
    MF_DCos2, MF_DSin, MF_DSin2, MF_DTan, MF_DTan2, MF_DSqrt, MF_DSqrt2,
    MF_DTrunc, MF_DRound, MF_DSqr, MF_DExp, MF_DFrac, MF_DInt, MF_DInt2,
    MF_DLn, MF_DLog2, MF_DLog10, MF_DCoTan, MF_DCoTan2, MF_DHypot, MF_DArcCos,
    MF_DArcCos2, MF_DArcSin, MF_DArcSin2, MF_DPower, MF_DMinus, MF_DPLus,
    MF_DNegate, MF_DMax, MF_DMax2, MF_DMin, MF_DMin2, MF_DSign, MF_DRandom,
    MF_DPi, MF_True, MF_False);
	
implementation

end.
