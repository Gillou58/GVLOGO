{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    27-11-2014 16:29:42                          |
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
  Graphics; // unité pour la tortue

  //#####################################################
  //
  //                  Chaînes de travail
  //
  //#####################################################

resourcestring
  // *** message d'erreur ***
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
  ME_NoArg = 'Il manque au moins un argument pour "%s".';
  ME_BadExp = 'Il y a une expression incorrecte dans "%s".';
  ME_Zero = 'Les divisions par 0 sont impossibles. ("%s")';
  ME_NegNumber = 'Un nombre négatif est interdit pour "%s".';
  ME_OutOfRange = 'Evaluation hors limites : %d pour "%s".';
  ME_OutOfRange2 = 'Evaluation hors limites : élément %d de "%s".';
  ME_NotSupported = 'La fonction "%s" n''est pas utilisable dans une expression.';
  ME_ParMismatch = 'Les parenthèses de l''expression ne sont pas appariées.';
  ME_NeedsInteger = '"%s" ne fonctionne qu''avec des entiers.';
  ME_Tan = 'La fonction tangente n''est pas définie pour un cosinus nul. ("%s")';
  Me_Ln = 'Les fonctions logarithmiques n''acceptent que des paramètres strictement positifs. ("%s")';
  ME_CoTan = 'La fonction cotangente n''est pas définie pour un sinus nul. ("%s")';
  ME_Arc = 'La fonction "%s" n''est définie que dans l''intervalle [-1,1].';

  // ************* GVKernel *************
  ME_C_Protected = 'L''objet "%s" est protégé.';
  ME_C_BadName = 'Le nom "%s" est incorrect.';
  ME_C_NotVar = 'L''objet "%s" n''est pas une variable.';
  ME_C_AlreadyBurried = 'L''objet "%s" est déjà enterré.';
  ME_C_NotPackage = 'L''objet "%s" n''est pas un paquet.';
  ME_C_NotObject = 'L''objet "%s" n''existe pas.';
  ME_C_NotPrim = 'L''objet "%s" n''est pas une primitive.';
  ME_C_PackageForbidden =
    'Le paquet "%s" ne peut pas être placé dans un autre paquet.';
  ME_C_PrimForbidden = 'La primitive "%s" ne peut pas être modifiée.';
  ME_C_NotBurried = 'L''objet "%s" n''est pas enterré.';
  ME_C_NotProc = 'L''objet "%s" n''est pas une procédure.';
  ME_C_BadParam = 'Le paramètre "%s" est incorrect.';
  ME_C_BadLine = 'La ligne "%s" est incorrecte.';
  ME_C_AlreadyPackage = 'L''objet "%s" est déjà un paquet.';
  ME_C_BadSave = 'Erreur de sauvegarde de "%s".';
  ME_C_NotLProp = 'La liste de propriétés "%s" est inconnue.';
  ME_C_BadDef = 'La définition "%s" est incorrecte.';
  ME_C_BadFile = 'Le fichier "%s" est introuvable ou corrompu.';
  ME_C_NotProp = 'La propriété "%s" est inconnue.';
  ME_C_BadProp = 'La définition de la propriété "%s" est incorrecte.';
  ME_C_BadObj = 'Le nom "%s" est déjà utilisé pour un autre type d''objet.';
  ME_C_NotFor = 'Le mot POUR est introuvable.';
  ME_C_EmptyEdit = 'L''éditeur est vide (lignes : "%s").';
  ME_C_OKProc = 'La procédure "%s" est correcte.';
  ME_C_NotEnd = 'Le mot FIN est introuvable pour "%s".';
  ME_C_Bad = 'Le nombre "%s" est incorrect.';
  ME_C_Inc = 'Oups ! Erreur inconnue !';
  ME_C_EmptyList = 'La liste "%s" est vide.';
  ME_C_FileNotFound = 'Le fichier "%s" est introuvable.';
  ME_C_Version = 'La version du fichier "%s" est incorrecte.';
  ME_C_BadContent = 'Le contenu du fichier "%s" est incorrect.';
  ME_C_NorProcnorList =
    'L''objet "%s" n''est ni une procédure ni une liste de procédures.';
  ME_C_Burried = 'L''objet "%s" est enterré.';
  ME_C_NotInPackage = 'L''objet "%s" n''appartient à aucun paquet.';
  ME_C_NorPrimNorProc = 'L''objet "%s" n''est ni une procédure ni une primitive.';
  ME_C_BadTo = 'Le mot POUR est mal placé dans %s.';
  ME_C_BadEnd = 'Le mot FIN est mal placé dans %s.';
  ME_C_WhatAbout = 'Que faut-il faire de "%s" ?';
  ME_C_BadNum = 'Le nombre %d ne convient pas pour cette opération.';
  ME_C_Nothing = '<vide>';

  // ************* PRIMITIVES *************

  { primitives de base }

  CComment = '//'; // commentaire
  P_For = 'POUR';
  P_End = 'FIN';
  P_First = 'PREMIER';
  P_First2 = 'PREM';
  P_Last = 'DERNIER';
  P_Last2 = 'DER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButFirst2 = 'SP';
  P_ButLast = 'SAUFDERNIER';
  P_ButLast2 = 'SD';
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
  MF_DLog10 = 'LOG'; // log base 10
  MF_DCoTan = 'COTAN'; // cotangente
  MF_DCoTan2 = 'COTANGENTE';
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
  MF_DHypot = 'HYPOTHENUSE'; // hypothénuse

//#####################################################
//
//                  Constantes et types
//
//#####################################################

// ************* Listes *************
const
  CBlank = ' '; // espace
  CBeginList = '['; // début de liste
  CEndList = ']'; // fin de liste
  CBeginPar = '('; // début d'expression
  CEndPar = ')'; // fin d'expression
  // séparateurs
  CSeparators = [CBlank, CBeginList, CEndList, CBeginPar, CEndPar];
  CLink = '$'; // caractère de lien
  CUnderline = '_'; // soulignement
  CDot = '.'; // point
  CAsk = '?'; // point d'interrogation
  CQuote = '"'; // guillemets
  CColon = ':'; // deux points (début de variable)
  CComma = ','; // virgule
  CExclamation = '!'; // #### exclamation ####
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
  CRLower = -1; // résultats de comparaison (plus petit)
  CREqual = 0;  // résultats de comparaison (égal)
  CRGreater = 1; // résultats de comparaison (plus grand)
  CRTrue = -1; // vrai
  CRFalse = 0; // faux
  // ensembles de caractères courants
  CLowAlpha = ['a'..'z']; // caractères alphabétiques en minuscules
  CHighAlpha = ['A'..'Z']; // caractères alphabétiques en majuscules
  CAlpha = CLowAlpha + CHighAlpha; // caractères alphabétiques
  // caractères alphabétiques autorisés pour un identificateur
  CAlphaPlus = CAlpha + [CUnderline, CColon, CDot];
  CDigit = ['0'..'9']; // chiffres
  // caractères alphanumériques
  CAlphaNum = CAlpha + CDigit;
  // caractères autorisés pour un identificateur
  CAlphaNumPlus = CAlphaPlus + CDigit;
  // caractères autorisés hors chaîne de caractères
  CAllValidChars = CAlphaNumPlus + CSpecialChar;
  CEmptyList = CBeginList + CEndList; // liste vide
  // extension pour les fichiers de listes de propriétés
  CExtPl = '.GPL';
  // entête de fichier de listes de propriétés
  CHeader = '[GPL100 (c) GV 2014]';
  CSep = '|'; // séparateur de liste de propriétés

  // ************* GVTurtles *************
const
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
type
  // type d'écrans : enroulement, fenêtre illimitée ou champ clos
  TScreenTurtle = (teWin, teGate, teRoll);
  // types de tortue : triangle, dessin ou personnalisée (réservée)
  TTurtleKind = (tkTriangle, tkPng, tkOwner);

  // ************* GVStacks *************
const
  CMinStack = 8; // minimum d'espace pour une pile
type
  // notifications de la pile : ajout, suppression, changement, effacement
  TGVStackNotification = (stAdded, stRemoved, stChanged, stCleared);

  // ************* GVEval *************
type
  // éléments d'une expression à évaluer
  CTokensEnum = (cteInteger, cteReal, cteVar, cteFunction, cteBeginExp, cteEndExp,
    ctePlus, cteMinus, cteMul, cteDiv, ctePower, cteGreater, cteLower, cteEqual,
    cteNotEqual, cteGreaterOrEqual, cteLowerOrEqual, cteMod, cteNot, cteAnd,
    cteOr, cteEnd, cteOrB, cteAndB, cteBoolean, cteUnKnown, cteForbidden,
    cteNotSupported, cteUnaryMinus, cteUnaryPlus);
  // élément de base d'une expression
  TGVBaseItem = record
    Token: string; // élément
    Kind: CTokensEnum; // type d'élément
  end;
  // états de l'évaluateur
   TGVEvalState = (esWaiting, esTokenizing, esScanning, esComputing, esNoInit);
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
    C_DLog10, // log base 10
    C_DCoTan, // cotangente
    C_DCoTan2,
    C_DArcCos, // arc cosinus
    C_DArcCos2,
    C_DArcSin, // arc sinus
    C_DArcSin2,
    C_Minus, // négatif
    C_Plus, // positif
    C_DNegate, // signe inversé
    C_DSign, // signe
    C_DRandom, // nombre au hasard
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
    C_DMin2,
    C_DHypot // hypothénuse
   );
const
  // priorité des éléments d'une expression
  // nombre le plus élevé = priorité la moins élevée
  // -1 : ne s'applique pas
  // 0: (unaires) - +
  // 1: (réservé)
  // 2: * / % mod
  // 3: + -
  // 4: > < <= >=
  // 5: = <> !=
  // 6: &
  // 7: |
  // 8: et
  // 9: ou
  // 10: non
  // 11: ^ puissance
  // 12: ( )
  CTokenPrecedence: array[CTokensEnum] of Integer = (-1, -1, -1, -1, 12, 12, 3,
    3, 2, 2, 11, 4, 4, 5, 5, 4, 4, 2, 10, 8, 9, -1, 7, 6, -1, -1, -1, -1, 0, 0);
  // associativité des éléments
  // 1 = droite - 0 = gauche  - -1 = ne s'applique pas
  CTokenAssociation: array[CTokensEnum] of Integer =
    (-1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
      0, -1, -1, -1, -1, 0, 0);
  // tableau du nom des fonctions
  GVFunctionName: array [TGVFunctions] of string = (MF_Unknown, MF_DAbs,
    MF_DAbs2, MF_DCos, MF_DCos2, MF_DSin, MF_DSin2, MF_DTan, MF_DTan2, MF_DSqrt,
    MF_DSqrt2, MF_DTrunc, MF_DRound, MF_DSqr, MF_DExp, MF_DFrac, MF_DInt,
    MF_DInt2, MF_DLn, MF_DLog2, MF_DLog10, MF_DCoTan, MF_DCoTan2,
    MF_DArcCos, MF_DArcCos2, MF_DArcSin, MF_DArcSin2,  MF_DMinus,
    MF_DPLus, MF_DNegate,  MF_DSign, MF_DRandom, MF_Not, MF_DPi, MF_True,
    MF_False, MF_Or, MF_And, MF_Mod, MF_DPower, MF_DMax, MF_DMax2, MF_DMin,
    MF_DMin2, MF_DHypot);

  // ************* GVKernel *************
const
  CPrimCount = 56; // nombre de primitives
  CVr = CDot + 'VAR'; // variable
  CBurried = CDot + 'BUR'; // enterré
  CInPackage = CDot + 'INP'; // dans un paquet
  CPackage = CDot + 'PKG'; // un paquet
  CProc = CDot + 'PRC'; // une procédure
  CExtLP = CDot + 'GVE'; // extension d'un espace de travail
type
  GVPrimRec = record  // enregistrement d'une primitive
    Name: string;
    NbParams: Integer;
  end;
const
  // *** tableau des primitives ***
  GVPrimName: array[1..CPrimCount] of GVPrimRec = (
   (Name:P_For; NbParams: -1),
   (Name:P_End; NbParams: 0),
   (Name:P_First; NbParams: 1),
   (Name:P_First2; NbParams: 1),
   (Name:P_Last; NbParams: 1),
   (Name:P_Last2; NbParams: 1),
   (Name:P_ButFirst; NbParams: 1),
   (Name:P_ButFirst2; NbParams: 1),
   (Name:P_ButLast; NbParams: 1),
   (Name:P_ButLast2; NbParams: 1),
   (Name:P_True; NbParams: 0),
   (Name:P_False; NbParams: 0),
   // fonctions
   (Name:MF_DAbs; NbParams: 1),
   (Name:MF_DAbs2; NbParams: 1),
   (Name:MF_DCos; NbParams: 1),
   (Name:MF_DCos2; NbParams: 1),
   (Name:MF_DSin; NbParams: 1),
   (Name:MF_DSin2; NbParams: 1),
   (Name:MF_DTan; NbParams: 1),
   (Name:MF_DTan2; NbParams: 1),
   (Name:MF_DSqrt; NbParams: 1),
   (Name:MF_DSqrt2; NbParams: 1),
   (Name:MF_DTrunc; NbParams: 1),
   (Name:MF_DRound; NbParams: 1),
   (Name:MF_DSqr; NbParams: 1),
   (Name:MF_DExp; NbParams: 1),
   (Name:MF_DFrac; NbParams: 1),
   (Name:MF_DInt; NbParams: 1),
   (Name:MF_DInt2; NbParams: 1),
   (Name:MF_DLn; NbParams: 1),
   (Name:MF_DLog2; NbParams: 1),
   (Name:MF_DLog10; NbParams: 1),
   (Name:MF_DCoTan; NbParams: 1),
   (Name:MF_DCoTan2; NbParams: 1),
   (Name:MF_DArcCos; NbParams: 1),
   (Name:MF_DArcCos2; NbParams: 1),
   (Name:MF_DArcSin; NbParams: 1),
   (Name:MF_DArcSin2; NbParams: 1),
   (Name:MF_DMinus; NbParams: 1),
   (Name:MF_DPLus; NbParams: 1),
   (Name:MF_DNegate; NbParams: 1),
   (Name:MF_DSign; NbParams: 1),
   (Name:MF_DRandom; NbParams: 1),
   (Name:MF_Not; NbParams: 1),
   (Name:MF_DPi; NbParams: 0),
   (Name:MF_True; NbParams: 0),
   (Name:MF_False; NbParams: 0),
   (Name:MF_Or; NbParams: 2),
   (Name:MF_And; NbParams: 2),
   (Name:MF_Mod; NbParams: 2),
   (Name:MF_DPower; NbParams: 2),
   (Name:MF_DMax; NbParams: 2),
   (Name:MF_DMax; NbParams: 2),
   (Name:MF_DMin; NbParams: 2),
   (Name:MF_DMin2; NbParams: 2),
   (Name:MF_DHypot; NbParams: 2)
   );

// ************* ERREURS *************
type
  // *** types d'erreur ***
  TGVError = (
  C_None, // pas d'erreur
  C_InternalError, // erreur interne
  // Listes
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
  // *** GVStack ***
  C_EmptyStack, // pile interne vide
  C_OutOfMemory, // mémoire insuffisante pour la pile
  C_LowStack, // pile insuffisante
  // *** GVEval ***
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
  C_ParMismatch, // parenthèses non concordantes
  C_NeedsInteger, // entiers exigés
  C_Tan, // tangente avec un cosinus nul
  C_Ln, // log avec nombre <= 0
  C_CoTan, // cotangente avec sinus nul
  C_Arc, // arccosinus ou arcsinus non définies
  // *** GVKernel ***
  C_Protected, // objet protégé
  C_BadName, // nom incorrect
  C_NotVar, // pas une variable
  C_AlreadyBurried, // déjà enterré
  C_NotPackage, // pas un paquet
  C_NotObject, // pas un objet
  C_NotPrim, // pas une primitive
  C_PackageForbidden, // paquet interdit
  C_PrimForbidden, // primitive interdite
  C_NotBurried, // non enterré
  C_NotProc, // procédure attendue
  C_BadParam, // mauvais paramètre
  C_BadLine, // mauvaise ligne
  C_AlreadyPackage, // déjà un paquet
  C_BadSave, // erreur de sauvegarde
  C_NotLProp, // liste de propriétés inconnue
  C_BadDef, // mauvaise définition
  C_BadFile, // mauvais fichier
  C_NotProp, // propriété inconnue
  C_BadProp, // mauvaise propriété
  C_BadObj, // erreur interne
  C_NotFor, // mot POUR absent
  C_EmptyEdit, // éditeur vide
  C_OKProc, // procédure OK
  C_NotEnd, // mot FIN absent
  C_Bad, // mauvais nombre
  C_Inc, // erreur inconnue
  C_EmptyList, // liste vide non attendue
  C_FileNotFound, // fichier introuvable
  C_Version, // mauvaise version de fichier
  C_BadContent, // mauvais contenu
  C_NorProcnorList, // ni une procédure ni une liste de procédures
  C_Burried, // objet enterré
  C_NotInPackage, // objet absent d'un paquet
  C_NorPrimNorProc, // ni une procédure ni une primitive
  C_BadTo, // mot POUR mal placé
  C_BadEnd, // mot FIN mal placé
  C_WhatAbout, // que faire de ?
  C_BadNum, // nombre inapproprié pour une opération
  C_Nothing // rien n'a été fourni
  );
const
  // *** tableau du nom des erreurs ***
  GVErrorName: array[TGVError] of string = (ME_None,ME_InternalError,
    ME_BadNumber, ME_BadInt, ME_EmptyStr, ME_BadChar, ME_BadList, ME_DelItem,
    ME_InsItem, ME_ReplaceItem, ME_NoListWord, ME_TwoDelete, ME_BadListP,
    ME_BadFormat, ME_EmptyStack, ME_OutOfMemory, ME_LowStack, ME_NoInit,
    ME_BadChar2, ME_ClosePar, ME_BadVar, ME_UnknownVar, ME_BadFunction,
    ME_NoArg, ME_BadExp, ME_Zero, ME_NegNumber, ME_OutOfRange, ME_OutOfRange2,
    ME_NotSupported, ME_ParMismatch, ME_NeedsInteger, ME_Tan, ME_Ln, ME_CoTan,
    ME_Arc, ME_C_Protected, ME_C_BadName, ME_C_NotVar,
    ME_C_AlreadyBurried, ME_C_NotPackage, ME_C_NotObject, ME_C_NotPrim,
    ME_C_PackageForbidden, ME_C_PrimForbidden, ME_C_NotBurried, ME_C_NotProc,
    ME_C_BadParam, ME_C_BadLine, ME_C_AlreadyPackage, ME_C_BadSave,
    ME_C_NotLProp, ME_C_BadDef, ME_C_BadFile, ME_C_NotProp, ME_C_BadProp,
    ME_C_BadObj, ME_C_NotFor, ME_C_EmptyEdit, ME_C_OKProc, ME_C_NotEnd,
    ME_C_Bad, ME_C_Inc, ME_C_EmptyList, ME_C_FileNotFound,
    ME_C_Version, ME_C_BadContent, ME_C_NorProcnorList, ME_C_Burried,
    ME_C_NotInPackage, ME_C_NorPrimNorProc, ME_C_BadTo, ME_C_BadEnd,
    ME_C_WhatAbout, ME_C_BadNum, ME_C_Nothing);

implementation

end.
