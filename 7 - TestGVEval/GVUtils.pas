{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, chaînes et fonctions utiles |
  |                  Unité : GVUtils.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    07-05-2014 22:23:54                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

{$I GVDefines.inc}
unit GVUtils;

// constantes, chaînes et fonctions pour GVLOGO
//
// ##############################################################
//
// L'unité regroupe les constantes et chaînes utilisées dans les autres unités
// du projet GVLOGO.
// Elle fournit aussi des fonctions générales.
//

interface

uses SysUtils, Graphics;

const
  // ************* GVLists *************
  // caractère d'échappement dans une liste
  CLink = '$';
  { Ce caractère permet de ne plus considérer le caractère suivant comme un
    séparateur. Il est donc possible d'inclure des crochets dans une liste
    ou des blancs dans un mot. }
  // caractère blanc
  CBlank = ' ';
  // premier caractère d'une liste
  CBeginList = '[';
  // dernier caractère d'une liste
  CEndList = ']';
  // début d'une expression
  CBeginPar = '(';
  // fin d'une expression
  CEndPar = ')';
  // premier caractère d'une variable
  CColon = ':';
  // premier caractère d'une constante
  CQuote = '"';

  // ************* GVInterpreter *************

  // point
  CPeriod = '.';
  // slash
  CSlash = '/';
  // souligné
  CUnderline = '_';

  // ************* GVPropLists *************
  // extension pour les fichiers de listes de propriétés
  CExtPl = '.GPL';
  // entête de fichier
  CHeader = '[GPL100 (c) GV 2014]';
  // séparateur de liste de propriétés
  CSep = '|';

  // ************* GVEval *************
  CPlus = '+'; // addition
  CMinus = '-'; // soustraction
  CMul = '*'; // multiplication
  CDiv = '/'; // division
  CPower = '^'; // puisssance

  // ************* GVKernel *************
  CAsk = '?'; // signe pour les questions
  CDot = '.'; // point pour les propriétés système
  CComment = '//'; // commentaire
  GVSOFT = CDot + 'GVS'; // copyright
  CPrim = CDot + 'PRM'; // primitive
  CParamPrim = CDot + 'PAR'; // paramètres de primitives
  CVr = CDot + 'VAR'; // variable
  CBurried = CDot + 'BUR'; // enterré
  CInPackage = CDot + 'INP'; // dans un paquet
  CPackage = CDot + 'PKG'; // un paquet
  CProc = CDot + 'PRC'; // une procédure
  CExtLP = CDot + 'GVE'; // extension d'un espace de travail

  V_To = 'Pour'; // primitive POUR par défaut
  V_End = 'Fin'; // idem pour FIN
  V_True = 'Vrai'; // valeur vraie
  V_False = 'Faux'; // valeur fausse

  // ************* GVTurtles *************
  DgToRad = Pi / 180; // pour les conversions en radians
  RadToDg = 180 / Pi; // pour les conversions en degrés
  DefaultScale = 100; // échelle par défaut
  DefaultHeading = 90; // cap par défaut
  TurtleDefaultSize = 8; // taille d'une tortue par défaut
  TurtleMaxSize = 20; // taille maximale de la tortue

  // ************* GVTInterpreter *************
type
  TDigit = '0' .. '9';
  TLowCase = 'a' .. 'z';
  TUpperCase = 'A' .. 'Z';

  // ************* ensemble des erreurs gérées *************
  TGVError = (C_General, // erreur générale pour une méthode
    C_None, // pas d'erreur
    C_ProtectPrim, // primitives protégées
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
    C_BadList, // liste incorrecte
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
    C_Nothing, // rien n'a été fourni
    C_BadChar, // mauvais caractère dans une expression
    C_ClosePar, // parenthèse fermante absente dans une expression
    C_BadNumber, // mauvais nombre dans une expression
    C_BadVar, // mauvaise variable dans une expression
    C_BadFunction, // mauvaise fonction dans une expression
    C_NoArg, // argument manquant dans une expression
    C_BadExp, // mauvaise expression
    C_Zero); // division par 0 dans une expression

  // ************* Graphisme *************

  IntColors = 1 .. StandardColorsCount; // intervalle des couleurs standard
{$IFDEF Delphi}
  IntPenStyles = 1 .. 9;
{$ELSE}
  IntPenStyles = 1 .. 8; // intervalle des formes de trait
{$ENDIF}
  IntBrushStyles = 1 .. 8; // intervalle des formes de brosse
  IntPenModes = 1 .. 16; // intervalle des modes du crayon

  // ************* Divers *************

  EGVListException = class(Exception); // exception de liste
  EGVStackException = class(Exception); // exception de pile

  TGVErrors = set of TGVError; // ensemble d'erreurs

resourcestring

  { Les erreurs internes suivantes ne concernent pas l'utilisateur final.
    Elles permettent de repérer des erreurs et doivent subir un
    traitement pour être lisibles et exploitables. }

  // *************  GVLists *************

  E_NoListWord = 'IE 000 GVLIST - NOT A LIST NOR A WORD';
  E_DelItem = 'IE 001 GVLIST - DELETEITEM (%d)';
  E_InsItem = 'IE 002 GVLIST - INSERTAITEM (%d)';
  E_ReplaceItem = 'IE 003 GVLIST - REPLACEITEM (%d)';
  E_TwoDelete = 'IE 004 GVLIST - TWODELETE (%d)';

  // ************* GVPropLists *************

  PL_Load = 'IE 101 PROP - LOADFROMFILE (%s)';
  PL_BadFile = 'IE 102 PROP - BAD FILE (%s)';
  PL_NotFound = 'IE 103 PROP - NOT FOUND (%s)';
  PL_SaveToFile = 'IE 104 PROP - SAVETOFILE (%s)';
  PL_BadName = 'IE 105 PROP - BAD NAME (%s)';
  PL_BadPropName = 'IE 105 PROP - BAD PROP NAME (%s)';
  PL_BadValue = 'IE 106 PROP - BAD VALUE (%s)';
  PL_Unknown = 'IE 107 PROP - UNKNOWN (%s)';
  PL_LoadFromFile = 'IE 108 PROP - LOADFROMFILE (%s)';
  PL_BadFormat = 'IE 109 PROP - BAD FORMAT (File : %s) (Format : %s)';
  PL_BadLineFile = 'IE 110 PROP - BAD LINE FILE (File : %s) (Line : %s)';

  // ************* GVStacks *************

  ME_C_LowStack = 'PILE - Pile insuffisante pour l''opération.';
  ME_C_ZeroDiv = 'PILE - Division par zéro impossible.';

  // ************* GVKernel *************
  { Ces messages d'erreur sont destinés à l'utilisateur final }

  ME_C_General = 'Erreur générique.';
  ME_C_None = 'Pas d''erreur à signaler !';
  ME_C_ProtectPrim =
    'Les primitives sont protégées : ajout de "%s" impossible.';
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
  ME_C_NotFor = 'Le mot "' + V_To + '" est introuvable.';
  ME_C_EmptyEdit = 'L''éditeur est vide (lignes : "%s").';
  ME_C_OKProc = 'La procédure "%s" est correcte.';
  ME_C_NotEnd = 'Le mot "' + V_End + '" est introuvable pour "%s".';
  ME_C_Bad = 'Le nombre "%s" est incorrect.';
  ME_C_Inc = 'Oups ! Erreur inconnue !';
  ME_C_EmptyList = 'La liste "%s" est vide.';
  ME_C_BadList = 'La liste "%s" est incorrecte.';
  ME_C_FileNotFound = 'Le fichier "%s" est introuvable.';
  ME_C_Version = 'La version du fichier "%s" est incorrecte.';
  ME_C_BadContent = 'Le contenu du fichier "%s" est incorrect.';
  ME_C_NorProcnorList =
    'L''objet "%s" n''est ni une procédure ni une liste de procédures.';
  ME_C_Burried = 'L''objet %s est enterré.';
  ME_C_NotInPackage = 'L''objet %s n''appartient à aucun paquet.';
  ME_C_NorPrimNorProc = 'L''objet %S n''est ni une procédure ni une primitive.';
  ME_C_BadTo = 'Le mot ' + V_To + ' est mal placé dans %s.';
  ME_C_BadEnd = 'Le mot ' + V_End + ' est mal placé dans %s.';
  ME_C_WhatAbout = 'Que faut-il faire de %s ?';
  ME_C_BadNum = 'Le nombre %d ne convient pas pour cette opération.';
  ME_C_Nothing = '<vide>';

  // ************* GVEval *************

  ME_BadChar = 'EVALUATION - Caractère interdit ou inconnu dans "%s".';
  ME_ClosePar = 'EVALUATION - Parenthèse fermante absente dans "%s".';
  ME_BadNumber = 'EVALUATION - Le nombre "%s" est incorrect.';
  ME_BadVar = 'EVALUATION - La variable "%s" est inconnue ou incorrecte.';
  ME_BadFunction = 'EVALUATION - La fonction "%s" est inconnue.';
  ME_NoArg = 'EVALUATION - Il manque un argument pour "%s".';
  ME_BadExp = 'EVALUATION - Il y a une expression incorrecte dans "%s".';
  ME_Zero = 'EVALUATION - Les divisions par zéro sont impossibles.';

  { ************* couleurs ************* }

  MC_Black = 'noir';
  MC_Maroon = 'marron';
  MC_Green = 'vert';
  MC_Olive = 'olive';
  MC_Navy = 'bleu marine';
  MC_Purple = 'violet';
  MC_Teal = 'vert d''eau';
  MC_Gray = 'gris';
  MC_Silver = 'argent';
  MC_Red = 'rouge';
  MC_Lime = 'vert clair';
  MC_Yellow = 'jaune';
  MC_Blue = 'bleu';
  MC_Fuchsia = 'fuchsia';
  MC_Aqua = 'bleu clair';
  MC_White = 'blanc';
  MC_UnKnown = 'inconnue';

  { ************* types de traits ************* }

  MC_Solid = 'solide';
  MC_Dash = 'tirets';
  MC_Dot = 'pointillés';
  MC_DashDot = 'tiret pointillé';
  MC_DashDotDot = 'tiret point point';
  MC_Clear = 'nettoyer';
  MC_Insideframe = 'dans le cadre';
{$IFDEF Delphi}
  MC_UserStyle = 'utilisateur';
  MC_Alternate = 'alterné';
{$ELSE}
  MC_Pattern = 'selon le modèle';
{$ENDIF}
  { ************* types de brosses ************* }

  MCB_Solid = 'solide';
  MCB_Clear = 'nettoyer';
  MCB_Horizontal = 'horizontal';
  MCB_Vertical = 'vertical';
  MCB_FDiagonal = 'diagonale 1';
  MCB_BDiagonal = 'diagonale 2';
  MCB_Cross = 'croix';
  MCB_DiagCross = 'croix diagonale';

  { ************* modes du crayon ************* }

  MCP_Black = 'noir';
  MCP_White = 'blanc';
  MCP_Nop = 'rien';
  MCP_Not = 'non';
  MCP_Copy = 'copie';
  MCP_NotCopy = 'non copie';
  MCP_MergePenNot = 'non fusion';
  MCP_MaskPenNot = 'non masque';
  MCP_MergeNotPen = 'non fusion 2';
  MCP_MaskNotPen = 'non masque 2';
  MCP_Merge = 'fusion';
  MCP_NotMerge = 'non fusion 3';
  MCP_Mask = 'masque';
  MCP_NotMask = 'non masque 3';
  MCP_Xor = 'ou exclusif';
  MCP_NotXor = 'non ou exclusif';

const
  GVErrorNames: array [TGVError] of string = (ME_C_General, ME_C_None,
    ME_C_ProtectPrim, ME_C_Protected, ME_C_BadName, ME_C_NotVar,
    ME_C_AlreadyBurried, ME_C_NotPackage, ME_C_NotObject, ME_C_NotPrim,
    ME_C_PackageForbidden, ME_C_PrimForbidden, ME_C_NotBurried, ME_C_NotProc,
    ME_C_BadParam, ME_C_BadLine, ME_C_AlreadyPackage, ME_C_BadSave,
    ME_C_NotLProp, ME_C_BadDef, ME_C_BadFile, ME_C_NotProp, ME_C_BadProp,
    ME_C_BadObj, ME_C_NotFor, ME_C_EmptyEdit, ME_C_OKProc, ME_C_NotEnd,
    ME_C_Bad, ME_C_Inc, ME_C_EmptyList, ME_C_BadList, ME_C_FileNotFound,
    ME_C_Version, ME_C_BadContent, ME_C_NorProcnorList, ME_C_Burried,
    ME_C_NotInPackage, ME_C_NorPrimNorProc, ME_C_BadTo, ME_C_BadEnd,
    ME_C_WhatAbout, ME_C_BadNum, ME_C_Nothing, ME_BadChar, ME_ClosePar,
    ME_BadNumber, ME_BadVar, ME_BadFunction, ME_NoArg, ME_BadExp, ME_Zero);

  GVColors: array [IntColors] of string = (MC_Black, MC_Maroon, MC_Green,
    MC_Olive, MC_Navy, MC_Purple, MC_Teal, MC_Gray, MC_Silver, MC_Red, MC_Lime,
    MC_Yellow, MC_Blue, MC_Fuchsia, MC_Aqua, MC_White);

  GVTColors: array [IntColors] of TColor = (clBlack, clMaroon, clGreen, clOlive,
    clNavy, clPurple, clTeal, clGray, clSilver, clRed, clLime, clYellow, clBlue,
    clFuchsia, clAqua, clWhite);

  { TPENSTYLE
    * Pour Delphi :
    (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame, psUserStyle, psAlternate);
    * Pour FreePascal :
    (psSolid, psDash, psDot, psDashDot, psDashDotDot, psinsideFrame, psPattern,
    psClear);
  }
{$IFDEF Delphi}
  GVPenStyles: array [IntPenStyles] of string = (MC_Solid, MC_Dash, MC_Dot,
    MC_DashDot, MC_DashDotDot, MC_Clear, MC_Insideframe, MC_UserStyle,
    MC_Clear);
{$ELSE}
  GVPenStyles: array [IntPenStyles] of string = (MC_Solid, MC_Dash, MC_Dot,
    MC_DashDot, MC_DashDotDot, MC_Insideframe, MC_Pattern, MC_Clear);
{$ENDIF}
  GVBrushStyles: array [IntBrushStyles] of string = (MCB_Solid, MCB_Clear,
    MCB_Horizontal, MCB_Vertical, MCB_FDiagonal, MCB_BDiagonal, MCB_Cross,
    MCB_DiagCross);

  GVPenModes: array [IntPenModes] of string = (MCP_Black, MCP_White, MCP_Nop,
    MCP_Not, MCP_Copy, MCP_NotCopy, MCP_MergePenNot, MCP_MaskPenNot,
    MCP_MergeNotPen, MCP_MaskNotPen, MCP_Merge, MCP_NotMerge, MCP_Mask,
    MCP_NotMask, MCP_Xor, MCP_NotXor);

  // ************* GVEval *************

const
  SpecialChar = [' ', '=', '+', '-', '*', '/', '(', ')', '^'];
  CharNum = ['0' .. '9'];
  CharAlpha = ['A' .. 'Z', 'a' .. 'z'];
  CharAlphaPlus = CharAlpha + ['_', ':'];
  CharAlphaNum = CharNum + CharAlphaPlus;

resourcestring
  MF_DSum = 'SOMME'; // addition
  MF_DSub = 'DIFF'; // différence
  MF_DSub2 = 'DIFFERNCE';
  MF_DMul = 'PRODUIT'; // multiplication
  MF_DDiv = 'DIV'; // division
  MF_DDiv2 = 'DIVISE';
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
  MF_DPower = 'PUISSANCE'; // puissance
  MF_DMinus = 'NEGATIF'; // nombre négatif
  MF_DPLus = 'POSITIF'; // nombre positif
  MF_DNegate = 'OPPOSE'; // signe inversé
  MF_DEqual = 'EGAL'; // égalité sur pile
  MF_DInf = 'INF'; // infériorité sur pile
  MF_DInf2 = 'INFERIEUR';
  MF_DSup = 'SUP'; // supériorité sur pile
  MF_DSup2 = 'SUPERIEUR';
  MF_DMax = 'MAX'; // maximum
  MF_DMax2 = 'MAXIMUM';
  MF_DMin = 'MIN'; // minimum
  MF_DMin2 = 'MINIMUM';
  MF_DPi = 'PI'; // PI sur la pile
  MF_DSign = 'SIGNE'; // signe
  MF_DRandom = 'HASARD'; // nombre au hasard

type
  // ensemble des fonctions mathématiques
  TGVFunctions = (C_DSum, // addition
    C_DSub, // différence
    C_DSub2, C_DMul, // multiplication
    C_DDiv, // division
    C_DDiv2, C_DAbs, // valeur absolue
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
    C_DLog10, // log base 100
    C_DCoTan, // cotangente
    C_DCoTan2, C_DHypot, // hypothénuse
    C_DArcCos, // arc cosinus
    C_DArcCos2, C_DArcSin, // arc sinus
    C_DArcSin2, C_DPower, // puissance
    C_Minus, // négatif
    C_Plus, // positif
    C_DNegate, // signe inversé
    C_DEqual, // égalité sur pile
    C_DInf, // infériorité sur pile
    C_DInf2, C_DSup, // supériorité sur pile
    C_DSup2, C_DMax, // maximum
    C_DMax2, C_DMin, // minimum
    C_DMin2, C_DPi, // PI sur la pile
    C_DSign, // signe
    C_DRandom); // nombre au hasard

const
  GVFunctionName: array [TGVFunctions] of string = (MF_DSum, MF_DSub, MF_DSub2,
    MF_DMul, MF_DDiv, MF_DDiv2, MF_DAbs, MF_DAbs2, MF_DCos, MF_DCos2, MF_DSin,
    MF_DSin2, MF_DTan, MF_DTan2, MF_DSqrt, MF_DSqrt2, MF_DTrunc, MF_DRound,
    MF_DSqr, MF_DExp, MF_DFrac, MF_DInt, MF_DInt2, MF_DLn, MF_DLog2, MF_DLog10,
    MF_DCoTan, MF_DCoTan2, MF_DHypot, MF_DArcCos, MF_DArcCos2, MF_DArcSin,
    MF_DArcSin2, MF_DPower, MF_DMinus, MF_DPLus, MF_DNegate, MF_DEqual, MF_DInf,
    MF_DInf2, MF_DSup, MF_DSup2, MF_DMax, MF_DMax2, MF_DMin, MF_DMin2, MF_DPi,
    MF_DSign, MF_DRandom);

  { ************* fonctions utiles ************* }

  { chaînes }

  // liste en chaîne (version simple)
function ListToStr(const Value: string): string; overload;
// liste en chaîne (version avec contrôle)
function ListToStr(const Value: string; out OutSt: string): Boolean; overload;
// est-ce une liste ?
function IsList(const Value: string): Boolean;
// retourne la liste vide
function EmptyList: string; inline;
// liste valide ?
function IsValidList(const Value: string): Boolean;
// chaîne en liste (version simple)
function StrToList(const Value: string): string; overload;
// chaîne en liste (avec contrôle)
function StrToList(const Value: string; out OutList: string): Boolean; overload;
// mot valide ?
function IsValidWord(const Value: string): Boolean;
// validation d'une valeur
function IsValidValue(const Value: string): Boolean;
// validation d'une valeur avec exception
procedure TestValue(const Value: string);
// message d'erreur
function ErrCode(const Code: TGVError; const Text: string): string;

{ couleurs }

// couleur en entier
function ColorToInt(const AColor: TColor): Integer;
// entier en couleur
function IntToColor(const N: Integer): TColor;
// couleur en chaîne
function ColorToStr(const AColor: TColor): string;
// entier vers style de crayon
function IntToPenStyle(const N: Integer): TPenStyle;
// style de crayon vers un entier
function PenStyleToInt(const AStyle: TPenStyle): Integer;
// style de crayon en chaîne
function PenStyleToStr(const AStyle: TPenStyle): string;
// entier vers style de brosse
function IntToBrushStyle(const N: Integer): TBrushStyle;
// style de brosse vers un entier
function BrushStyleToInt(const AStyle: TBrushStyle): Integer;
// style de brosse en chaîne
function BrushStyleToStr(const AStyle: TBrushStyle): string;
// entier vers mode de crayon
function IntToPenMode(const N: Integer): TPenMode;
// mode de crayon vers un entier
function PenModeToInt(const AStyle: TPenMode): Integer;
// mode de crayon en chaîne
function PenModeToStr(const AStyle: TPenMode): string;

implementation

uses
  Dialogs;

{ fonctions utiles }

function ListToStr(const Value: string): string;
// *** liste en chaîne (version simple) ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (Value <> EmptyStr) and (Value[1] = CBeginList) and
    (Value[Length(Value)] = CEndList) then // on enlève les crochets si possible
    Result := Copy(Value, 2, Length(Value) - 2);
end;

(* ********************************************************************* *)

function ListToStr(const Value: string; out OutSt: string): Boolean;
// *** liste en chaîne ***
begin
  // on cherche les crochets et on vérifie la validité de la liste
  Result := (Value <> EmptyStr) and (Value[1] = CBeginList) and
    (Value[Length(Value)] = CEndList) and IsValidList(Value);
  if Result then
    OutSt := Copy(Value, 2, Length(Value) - 2); // on retire les crochets
end;

(* ********************************************************************* *)

function StrToList(const Value: string): string;
// *** transforme une chaîne en liste (version simple) ***
begin
  // le résultat est la chaîne entre crochets
  Result := CBeginList + Value + CEndList;
end;

(* ********************************************************************* *)

function StrToList(const Value: string; out OutList: string): Boolean;
// *** transforme une chaîne en liste (version avec contrôle) ***
begin
  Result := IsValidList(CBeginList + Value + CEndList);
  // on vérifie la validité de la liste
  if Result then // si OK
    OutList := CBeginList + Value + CEndList;
  // la valeur de sortie est transmise
end;

(* ********************************************************************* *)

function EmptyList: string;
// *** retourne la liste vide ***
begin
  Result := CBeginList + CEndList; // => []
end;

(* ********************************************************************* *)

procedure TestValue(const Value: string);
// *** test d'une valeur avec exception ***
begin
  if not IsValidValue(Value) then // mot ou liste ?
    raise EGVListException.Create(E_NoListWord); // sinon erreur
end;

(* ********************************************************************* *)

function IsValidList(const Value: string): Boolean;
// *** teste la validité d'une liste ***
var
  St: string; // liste de travail
  I: Integer; // caractère en cours
  Lev: Integer; // niveau interne
begin
  Result := IsList(Value); // est-ce une liste simple ?
  if Result then // si oui
    St := ListToStr(Value); // si possible, on enlève les crochets extérieurs
  I := 1; // on pointe sur le premier caractère
  // on boucle tant qu'il n'y a pas d'erreur et qu'il reste des caractères
  while Result and (I <= Length(St)) do
  begin
    case St[I] of
      CBeginList: // *** début d'une sous-liste ? ***
        begin
          Inc(I); // caractère suivant
          Lev := 1; // premier niveau
          while (Lev <> 0) and (I <= Length(St)) do // autres niveaux ?
          begin
            case St[I] of
              CLink: // un lien ?
                Inc(I); // on le saute
              CBeginList:
                Inc(Lev); // niveau suivant
              CEndList:
                Dec(Lev); // niveau précédent
            end;
            Inc(I); // caractère suivant
          end; // fin des autres niveaux
          Result := (Lev = 0); // OK si niveau = 0
        end;
      CBeginPar: // *** début d'une expression ? ***
        begin
          Inc(I); // prochaine caractère
          Lev := 1; // premier niveau
          while Result and (Lev <> 0) and (I <= Length(St)) do
          // autres niveaux sans erreur ?
          begin
            case St[I] of
              CLink: // un lien ?
                Inc(I); // caractère suivant
              CBeginList, CEndList:
                Result := False; // pas de liste dans une expression
              CBeginPar:
                Inc(Lev); // niveau suivant
              CEndPar:
                Dec(Lev); // niveau précédent
            end;
            Inc(I); // caractère suivant
          end; // fin des autres niveaux
          if Result then
            Result := (Lev = 0); // OK si niveau = 0 et pas d'erreur en amont
        end;
      CLink: // *** un lien ? ***
        Inc(I, 2); // on saute un caractère
      CEndList, CEndPar: // fin de liste ou de parenthèse ? => erreur
        Result := False; // mauvaise liste
    else // autres caractères
      Inc(I); // on les ignore
    end;
  end; // fin de la boucle
end;

(* ********************************************************************* *)

function IsValidValue(const Value: string): Boolean;
// *** validation d'une valeur ***
begin
  Result := IsValidWord(Value) or IsValidList(Value);
end;

(* ********************************************************************* *)

function IsValidWord(const Value: string): Boolean;
// *** le mot Value est-il valide ? ***
var
  Ch: Char; // caractère de travail
  Next: Boolean; // drapeau de caractère à sauter
begin
  Result := (Value = EmptyStr); // chaîne vide acceptée pour un mot
  if not Result then // sinon on examine la chaîne
  begin
    Next := False; // pas de lien
    for Ch in Value do // boucle sur les caractères
    begin
      if not Next then // si pas après le caractère d'échappement
        // [, ], (, ), :, "  et blancs interdits dans un mot
{$IFDEF Delphi}
        Result := not CharInSet(Ch, [CBeginList, CEndList, CBeginPar, CEndPar,
          CQuote, CColon, CBlank]);
{$ELSE}
        Result := not(Ch in [CBeginList, CEndList, CBeginPar, CEndPar, CQuote,
          CColon, CBlank]);
{$ENDIF}
      Next := (Ch = CLink) and not Next; // saute le suivant si <> CLink
      if not Result then // on sort en cas d'erreur
        Break;
    end;
  end;
end;

(* ********************************************************************* *)

function ErrCode(const Code: TGVError; const Text: string): string;
// *** renvoie le message en fonction du code d'erreur ***
begin
  if (Code >= Low(TGVError)) and (Code <= High(TGVError)) then
    Result := Format(GVErrorNames[Code], [Text])
  else
    Result := GVErrorNames[C_Inc];
end;

(* ********************************************************************* *)

function IsList(const Value: string): Boolean;
// *** est-ce une liste ? ***
begin
  Result := (Value <> EmptyStr) and (Value[1] = CBeginList) and
    (Value[Length(Value)] = CEndList);
end;

(* ********************************************************************* *)

function ColorToInt(const AColor: TColor): Integer;
// *** couleur en entier ***
var
  I: Integer;
begin
  Result := -1; // couleur non trouvée
  for I := 1 to StandardColorsCount do // on balaie les couleurs connues
    if (GVTColors[I] = AColor) then
    begin
      Result := I; // on a trouvé la couleur
      Break; // inutile d'aller plus loin
    end;
end;

(* ********************************************************************* *)

function IntToColor(const N: Integer): TColor;
// *** entier en couleur ***
begin
  // le nombre est normalisé
  Result := GVTColors[Abs(N mod (High(IntColors) + 1))];
end;

(* ********************************************************************* *)

function ColorToStr(const AColor: TColor): string;
// *** couleur en chaîne ***
var
  I: Integer;
begin
  I := ColorToInt(AColor); // recherche de la couleur
  if I <> -1 then // couleur trouvée
    Result := GVColors[I]
  else
    Result := MC_UnKnown; // couleur inconnue
end;

(* ********************************************************************* *)

function IntToPenStyle(const N: Integer): TPenStyle;
// *** entier vers style de crayon ***
begin
  // le nombre est normalisé
  Result := TPenStyle(Abs(N mod (High(IntPenStyles) + 1)));
end;

(* ********************************************************************* *)

function PenStyleToInt(const AStyle: TPenStyle): Integer;
// *** style de crayon vers entier ***
begin
  Result := Ord(AStyle);
end;

(* ********************************************************************* *)

function PenStyleToStr(const AStyle: TPenStyle): string;
// *** style de crayon en chaîne ***
begin
  Result := GVPenStyles[Ord(AStyle)];
end;

(* ********************************************************************* *)

function IntToBrushStyle(const N: Integer): TBrushStyle;
// *** entier vers style de brosse ***
begin
  Result := TBrushStyle(Abs(N mod (High(IntBrushStyles) + 1)));
end;

(* ********************************************************************* *)

function BrushStyleToInt(const AStyle: TBrushStyle): Integer;
// *** style de brosse vers entier ***
begin
  Result := Ord(AStyle);
end;

(* ********************************************************************* *)

function BrushStyleToStr(const AStyle: TBrushStyle): string;
// *** style de brosse vers chaîne ***
begin
  Result := GVBrushStyles[Ord(AStyle)];
end;

(* ********************************************************************* *)

function IntToPenMode(const N: Integer): TPenMode;
// *** entier en mode de crayon ***
begin
  // le nombre est normalisé
  Result := TPenMode(Abs(N mod (High(IntPenModes) + 1)));
end;

(* ********************************************************************* *)

function PenModeToInt(const AStyle: TPenMode): Integer;
// *** mode de crayon en entier ***
begin
  Result := Ord(AStyle);
end;

(* ********************************************************************* *)

function PenModeToStr(const AStyle: TPenMode): string;
// *** mode de crayon vers chaîne ***
begin
  Result := GVPenModes[Ord(AStyle)];
end;

(* ********************************************************************* *)

end.
