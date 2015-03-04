{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVCONSTS - part of GVLOGO
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

unit GVConsts;
//
// Unité des constantes, chaînes et types communs aux unités
// du projet GVLOGO.
//

interface

uses
  Graphics; // unité pour la tortue

resourcestring
  // *** messages d'identification ***
  CE_GVTitle = 'GVLOGO';
  CE_GVVersion = 'v1.0.3';
  CE_GVAuthor = '© Gilles Vasseur';
  CE_GVDate = '04/03/2015';

const
  // *** listes ***
  CBlank = ' '; // espace
  CTab = #9; // tabulation
  CSlash = '/'; // slash
  CBeginList = '['; // début de liste
  CEndList = ']'; // fin de liste
  CBeginPar = '('; // début d'expression
  CEndPar = ')'; // fin d'expression
  CEmptyList = CBeginList + CEndList; // liste vide

  // *** séparateurs ***
  CBlanks = [CTab, CBlank];
  CBraces = [CBeginList, CEndList, CBeginPar, CEndPar];
  CSeparators = CBlanks + CBraces;

  // *** caractères ***
  CLink = '$'; // caractère de lien
  CUnderline = '_'; // soulignement
  CDot = '.'; // point
  CAsk = '?'; // point d'interrogation
  CQuote = '"'; // guillemets
  CColon = ':'; // deux points (début de variable)
  CComma = ','; // virgule
  CExclamation = '!';
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

  // *** caractères spéciaux ***
  CSpecialChar = [CTab, CBlank, CEqual, CPlus, CMinus, CMul, CDiv, CBeginPar,
    CEndPar, CPower, CGreater, CLower];
  CNonPrintable = [#0..#32];

  // *** ensembles de caractères courants ***
  CLowAlpha = ['a' .. 'z']; // caractères alphabétiques en minuscules
  CHighAlpha = ['A' .. 'Z']; // caractères alphabétiques en majuscules
  CAlpha = CLowAlpha + CHighAlpha; // caractères alphabétiques
  // caractères alphabétiques autorisés pour un identificateur
  CAlphaPlus = CAlpha + [CUnderline, CColon, CDot];
  CDigit = ['0' .. '9']; // chiffres
  CDigitPlus = CDigit + [CPlus, CMinus]; // chiffres et signes
  CDigitPlusDot = CDigitPlus + [CDot]; // point en plus
  CAlphaNum = CAlpha + CDigit;  // caractères alphanumériques
  // caractères autorisés pour un identificateur
  CAlphaNumPlus = CAlphaPlus + CDigit;
  // caractères autorisés hors chaîne de caractères
  CAllValidChars = CAlphaNumPlus + CSpecialChar;

  // *** listes de propriétés ***
  CExtPl = '.GPL'; // extension pour les fichiers de listes de propriétés
  // entête de fichier de listes de propriétés
  CHeader = '[GPL100 (c) GV 2014]';
  CSep = '|'; // séparateur de liste de propriétés

  // *** valeurs remarquables ***
  CRLower = -1; // résultats de comparaison (plus petit)
  CREqual = 0; // résultats de comparaison (égal)
  CRGreater = 1; // résultats de comparaison (plus grand)
  CRTrue = -1; // vrai
  CStTrue = '-1';
  CRFalse = 0; // faux
  CStFalse = '0';

  // *** tortue graphique ***
  // couleurs de base
  CColors: array[0..19] of TColor = (clBlack, clMaroon, clGreen, clOlive,
    clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow,
    clBlue, clFuchsia, clAqua, clWhite, clMoneyGreen, clSkyBlue, clCream,
    clMedGray);
  DgToRad = Pi / 180; // pour les conversions en radians
  RadToDg = 180 / Pi; // pour les conversions en degrés
  CDefaultScale = 100; // échelle par défaut
  CDefaultHeading = 90; // cap par défaut
  CDefaultXY = 600; // taille écran tortue par défaut
  CDefaultSize = 8; // taille d'une tortue par défaut
  CMaxSize = 20; // taille maximale de la tortue
  CMaxSpeed = 100; // vitesse maximum de la tortue
  CDefaultPenColor = clWhite; // couleur de fond par défaut
  CDefaultBackColor = clBlack; // couleur du crayon par défaut
  CDefaultPenWidth = 1; // largeur du crayon par défaut

  // *** piles ***
  CMinStack = 8; // minimum d'espace pour une pile

  // *** noyau ***
  CVr = CDot + 'VAR'; // variable
  CBurried = CDot + 'BUR'; // enterré
  CInPackage = CDot + 'INP'; // dans un paquet
  CPackage = CDot + 'PKG'; // un paquet
  CProc = CDot + 'PRC'; // une procédure
  CExtLP = CDot + 'GVE'; // extension d'un espace de travail

  // *** interpréteur ***
  CDisabledState = 1; // désactivé
  CTrueState = -1; // vrai
  CFalseState = 0; // faux
  CBreak = '#'; // marque de fin

type
  // *** type d'écrans : enroulement, fenêtre illimitée ou champ clos ***
  TScreenTurtle = (teWin, teGate, teRoll);

  // *** types de tortue : triangle, dessin ou personnalisée (réservée) ***
  TTurtleKind = (tkTriangle, tkPng, tkOwner);

  // *** définition d'une tortue ***
  TTurtle = record
    rSaved: Boolean; // drapeau de sauvegarde
    rX: Extended; // abscisse
    rY: Extended; // ordonnée
    rKind: TTurtleKind; // type de tortue
    rSize: Integer; // taille de la tortue
    rVisible: Boolean; // drapeau de visibilité
    rHeading: Extended; // direction
    rPenDown: Boolean; // drapeau de crayon baissé
    rPenRubber: Boolean; // drapeau d'effacement
    rScaleX: Integer; // échelle des X
    rScaleY: Integer; // échelle des Y
    rFilled: Boolean; // remplissage
    rPenWidth: Integer; // largeur de crayon
    rBrush: TBrush; // type de brosse
    rPen: TPen; // type de crayon
    rFont: TFont; // type de fonte
  end;

  // *** notifications de la pile ***
  // ajout, suppression, changement, effacement
  TGVStackNotification = (stAdded, stRemoved, stChanged, stCleared);

  // *** éléments d'une expression à évaluer ***
  CTokensEnum = (cteInteger, cteReal, cteVar, cteFunction, cteBeginExp,
    cteEndExp, ctePlus, cteMinus, cteMul, cteDiv, ctePower, cteGreater,
    cteLower, cteEqual, cteNotEqual, cteGreaterOrEqual, cteLowerOrEqual, cteMod,
    cteNot, cteAnd, cteOr, cteOrB, cteAndB, cteBoolean, cteUnKnown,
    cteForbidden, cteNotSupported, cteUnaryMinus, cteUnaryPlus);

  // *** élément de base d'une expression ***
  TGVBaseItem = record
    Token: string; // élément
    Kind: CTokensEnum; // type d'élément
  end;

  // *** états de l'évaluateur ***
  TGVEvalState = (esWaiting, esTokenizing, esScanning, esComputing,
    esNoInit, esOK);

  // *** ensemble des fonctions mathématiques de base ***
  TGVFunctions = (C_Unknown, // fonction inconnue
    C_DAbs, // valeur absolue
    C_DAbs2, C_DCos, // cosinus
    C_DCos2, C_DSin, // sinus
    C_DSin2, C_DTan, // tangente
    C_DTan2, C_DSqrt, // racine carrée
    C_DSqrt2, C_DTrunc, // nombre tronqué
    C_DRound, // nombre arrondi
    C_DSqr, // nombre au carré
    C_DExp, // exponentielle
    C_DFrac, // partie fractionnelle
    C_DInt, // partie entière
    C_DInt2, C_DLn, // log népérien
    C_DLog2, // log base 2
    C_DLog10, // log base 10
    C_DCoTan, // cotangente
    C_DCoTan2, C_DArcCos, // arc cosinus
    C_DArcCos2, C_DArcSin, // arc sinus
    C_DArcSin2, C_Minus, // négatif
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
    C_DMax2, C_DMin, // minimum
    C_DMin2, C_DHypot // hypothénuse
    );

  // *** états possibles (3 positions) ***
  TThreeStates = CTrueState..CDisabledState;

  // *** description d'un espace d'interprétation ***
  TGVAutomatRec = record
    fNum: Integer; // élément en cours dans la ligne
    fItem: string; // donnée en cours
    fLine: string; // ligne en cours
    fPrim: string; // primitive en cours
    fProc: string; // procédure en cours
    fLevel: Integer; // niveau en cours
  end;

  // *** commandes de l'interpréteur ***
  TGVAutomatCmd = (acNone, acClear, acWrite, acType, acReadList, acReadChar,
    acConfirm, acBold, acUnderline, acItalic, acNoBold, acNoUnderline,
    acNoItalic, acColor, acSetColor, acBackColor, acSetBackColor,
    acSetFontSize, acFontSize, acSetFont, acFont, acWriteEdit);

  // *** état de l'automate ***
  TGVAutomatState = (asWaiting, asEnding, asWorking, asError,
    asWord, asList, asVar, asNumber, asEval, asProc, asPrim,
    asPushing, asStopped, asExePrim, asExeProc, asPreparing,
    asProcDone, asPrimDone, asPrimStop, asPrimValue, asFollowing,
    asEndFollowing);

resourcestring
  // *** chaînes des états de l'automate  
  MasWaiting = 'Attente %s';
  MasEnding = 'Travail terminé %s';
  MasWorking = 'Au travail %s';
  MasError = '===> Erreur : %s <===';
  MasWord = 'Mot "%s" traité';
  MasList = 'Liste "%s" traitée';
  MasVar = 'Variable "%s" traitée';
  MasNumber = 'Nombre "%s" traité';
  MasEval = 'Expression "%s" traitée';
  MasProc = 'Procédure "%s" traitée';
  MasPrim = 'Primitive "%s" traitée';
  MasPushing = 'On empile "%s"';
  MasStopped = 'On s''est arrêté %s';
  MasExePrim = 'On exécute la primitive "%s"';
  MasExeProc = 'On exécute la procédure "%s"';
  MasPreparing = 'Préparation %s';
  MasProcDone = 'La procédure "%s" a été exécutée';
  MasPrimDone = 'La primitive "%s" a été exécutée';
  MasPrimStop = 'STOP demandé %s';
  MasPrimValue = 'Résultat obtenu : %s';
  MasFollowing = 'Début de suivi %s';
  MasEndFollowing = 'Fin de suivi %s';
	
const
  // *** messages pour l'état de l'automate ***
  CStatesArray: array[TGVAutomatState] of string = 
   (MasWaiting, MasEnding, MasWorking, MasError, MasWord, MasList,
	MasVar,	MasNumber, MasEval, MasProc, MasPrim, MasPushing,
	MasStopped, MasExePrim,	MasExeProc, MasPreparing, MasProcDone,
	MasPrimDone, MasPrimStop, MasPrimValue,	MasFollowing,
	MasEndFollowing);

  // *** priorité des éléments d'une expression ***
  // nombre le plus élevé = priorité la moins élevée
  // -1 : ne s'applique pas
  // 0: (unaires) - +
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
  // 11: ( )
  CTokenPrecedence: array [CTokensEnum] of Integer = (-1, -1, -1, -1, 11, 11, 3,
    3, 2, 2, 10, 4, 4, 5, 5, 4, 4, 2, 1, 8, 9, 7, 6, -1, -1, -1, -1, 0, 0);
  // *** associativité des éléments ***
  // 1 = droite - 0 = gauche  - -1 = ne s'applique pas
  CTokenAssociation: array [CTokensEnum] of Integer = (-1, -1, -1, -1, -1, -1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, 0, 0);

implementation

end.
