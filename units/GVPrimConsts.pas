{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Données pour les primitives             |
  |                  Unité : GVPrimConsts.pas                              |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle
// 26/02/2015 - 1.0.1 - ajout des primitives des formes de la tortue
// 03/03/2015 - 1.0.2 - modification de l'enregistrement d'une primitive

// GVPRIMCONSTS - part of GVLOGO
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

unit GVPrimConsts;
// Unité des constantes, chaînes et types utiles pour les primitives
// du projet GVLOGO.
//

interface

uses
  GVConsts, // constantes communes
  GVErrConsts; // constantes d'erreurs

const
  // *** longueur maximale du nom d'une primitive ***
  CMaxLengthPrim = 127;
  // *** nombre de primitives ***
  CPrimCount = 270;

 type
  // *** enregistrement d'une primitive ***
  TGVPrimRec = record
    Key: Integer; // clé d'accès # 1.0.2
    Name: string; // nom
    NbParams: Integer; // nombre de paramètres
  end;

resourcestring
  // toutes les primitives par ordre alphabétique (highlighter)
  CPrimsAll = '.EFFACE.TOUT .SUP.PROCS .SUP.VARS ABS ABSOLUE ' +
    'ANNULE ANPROP APPARTIENT.A ARCCOS ARCCOSINUS ARCSIN ARCSINUS ARRONDI ' +
    'AU.CARRE AV AVANCE BAISSE.CRAYON BAISSE? BC BOUCLE CACHE.TORTUE CAP ' +
    'CC CF CHARGE CHARGE.PROCS CHARGE.VARS CHOSE CLOS COMPTE COMPTE.PROPS ' +
    'CONFIRMEED COPIE.PROC COS COSINUS COTAN COTANGENTE COULEUR.CRAYON ' +
    'COULEUR.FOND COULEUR.FOND.TEXTE COULEUR.TEXTE CT DATE DEF.PROC DEPAQUETTE ' +
    'DER DERNIER DETERRE DIFF DIFFERENCE DISTANCE DIV DONNE DPROP DROITE ' +
    'EC ECHELLE ECRIS ECRIST EDITE.PAQUET EDITE.PROC EDITE.TOUT EGAL? ' +
    'ELEMENT ELEMENTS.PAQUET ENROULE ENT ENTERRE ENTERRE? ENTIER ET ' +
    'ETAT.CRAYON ETAT.ECRAN ETAT.TORTUE EXEC EXECUTE EXP ' +
    'F.CARRE F.CERCLE F.ELLIPSE F.RECTANGLE F.RECTANGLE.ARRONDI ' +
    'FAUX FCAP FCC FCF ' +
    'FCFT FCT FEC FEN FENETRE FIN FIXE.CAP FIXE.COULEUR.CRAYON ' +
    'FIXE.COULEUR.FOND FIXE.COULEUR.FOND.TEXTE FIXE.COULEUR.TEXTE ' +
    'FIXE.CRAYON FIXE.ECHELLE FIXE.ECHELLEX FIXE.ECHELLEY FIXE.ECRAN ' +
    'FIXE.ETAT.CRAYON FIXE.ETAT.TORTUE FIXE.FONTE FIXE.POS FIXE.TAILLE ' +
    'FIXE.TAILLE.CRAYON FIXE.TAILLE.FONTE FIXE.TORTUE FIXE.VITESSE ' +
    'FIXE.XY FIXEX FIXEY FONTE FPOS FRAC FTF FX FXY FY GAUCHE GOMME GRAS ' +
    'GVLOGO HASARD HEURE HYPOTHENUSE IDENTIFICATEUR? INSERE INVERSE INVERSE.CRAYON ' +
    'ITALIQUE ITEM LAISSE.VOIR LC LEVE.CRAYON LIS.CAR LIS.LISTE LISTE LISTE.PROP? ' +
    'LISTE.VERS.PAQUET LISTE? LL LN LOCALE LOCALES LOG LOG2 MAJUSCULES MAX ' +
    'MAXIMUM MD MELANGE MEMBRE? METS.DERNIER METS.PREMIER MIN MINIMUM ' +
    'MINUSCULES MOD MONTRE.TORTUE MOT MOT? MP MT MUL NB.PARAMS.PROC ' +
    'NEGATIF? NETTOIE NIVEAU.SUP NOM? NOMBRE? NON NORMAL OPPOSE ORIGINE ' +
    'OU PAQUET PAQUET.VERS.LISTE PARAMS.PROC PH PHRASE PHRASE.FIN PI PLG? ' +
    'PLISTE PLP? POS POSITIF? POUR PRECEDENT PREM PREMIER PREMS PRIMITIVE? ' +
    'PRIMITIVES PROCEDURE? PROCEDURES PRODUIT PROP? PROPS PROTEGE? PUISSANCE ' +
    'QUOTIENT RAC RACINE RE RECULE REMPLACE REMPLIS RENDS REPETE ROTATION RPROP ' +
    'SAUF.DERNIER SAUF.PREMIER SAUF.PREMS SAUVE SAUVE.PAQUET SAUVE.PROCS ' +
    'SAUVE.VARS SD SI SI.FAUX SI.VRAI SIGNE SIN SINON SINUS SOMME ' +
    'SORTIE.TORTUE SOULIGNE SP STOP SUIVANT SUPPRIME SUPPRIME.PAQUET TAILLE ' +
    'TAILLE.CRAYON TAILLE.FONTE TAN TANGENTE TAPE TD TESTE TF TG ' +
    'TORTUE.DESSIN TORTUE.NORMALE TRACE TRIE TRONQUE VALEUR.CRAYON ' +
    'VALEUR.ECRAN VALEUR.TORTUE VARIABLES VE VERS VERS.PAQUET VIDE.ECRAN ' +
    'VIDE.TEXTE VIDE? VISIBLE? VITESSE VRAI VT XCOOR YCOOR';
	
  // *** intitulés des primitives ***
  CComment = '//'; // commentaire
  P_GVLOGO = 'GVLOGO'; // 1
  P_UpperLevel = 'NIVEAU.SUP';
  P_Break = 'STOP';
  P_If = 'SI';
  P_Return = 'RENDS';
  P_Word = 'MOT';
  P_Repeat = 'REPETE';
  P_Item2 = 'ITEM';
  P_Loop = 'BOUCLE';
  P_EditAllProcs = 'EDITE.TOUT'; // 10
  P_LoadProcs = 'CHARGE.PROCS';
  P_SaveProcs = 'SAUVE.PROCS';
  P_ProcToEdit = 'EDITE.PROC';
  P_ParamLineProc = 'PARAMS.PROC';
  P_DefListProc = 'DEF.PROC';
  P_ParamsCount = 'NB.PARAMS.PROC';
  P_CopyProc = 'COPIE.PROC';
  P_SaveAll = 'SAUVE';
  P_LoadAll = 'CHARGE';
  P_SaveVars = 'SAUVE.VARS'; // 20
  P_LoadVars = 'CHARGE.VARS';
  P_PckToEdit = 'EDITE.PAQUET';
  P_SavePck = 'SAUVE.PAQUET';
  P_ListToPck = 'LISTE.VERS.PAQUET';
  P_DelPck = 'SUPPRIME.PAQUET';
  P_UnPackObj = 'DEPAQUETTE';
  P_PckToList = 'PAQUET.VERS.LISTE';
  P_ProtectedP = 'PROTEGE?';
  P_ToPck = 'VERS.PAQUET';
  P_PckUnBurry = 'DETERRE'; // 30
  P_IsBurriedPck = 'ENTERRE?';
  P_DelProcs = '.SUP.PROCS';
  P_DelVars = '.SUP.VARS';
  P_Else = 'SINON';
  P_Write2 = 'EC';
  P_PkgCreate = 'PAQUET';
  P_PkgItemsCount = 'ELEMENTS.PAQUET';
  P_PckBurry = 'ENTERRE';
  P_BelongsTo = 'APPARTIENT.A';
  P_Give = 'DONNE'; // 40
  P_Thing = 'CHOSE';
  P_SetHeading = 'FIXE.CAP';
  P_SetHeading2 = 'FCAP';
  P_Heading = 'CAP';
  P_Towards = 'VERS';
  P_ShowTurtle = 'MONTRE.TORTUE';
  P_ShowTurtle2 = 'MT';
  P_HideTurtle = 'CACHE.TORTUE';
  P_HideTurtle2 = 'CT';
  P_VisibleP = 'VISIBLE?'; // 50
  P_TurtleState = 'ETAT.TORTUE';
  P_SetTurtleState = 'FIXE.ETAT.TORTUE';
  P_NormalTurtle = 'TORTUE.NORMALE';
  P_GreenTurtle = 'TORTUE.DESSIN';
  P_SetTurtleSize = 'FIXE.TAILLE';
  P_TurtleSize = 'TAILLE';
  P_PenDown = 'BAISSE.CRAYON';
  P_PenDown2 = 'BC';
  P_PenUp = 'LEVE.CRAYON';
  P_PenUp2 = 'LC'; // 60
  P_DownP = 'BAISSE?';
  P_SetPenColor = 'FIXE.COULEUR.CRAYON';
  P_SetPenColor2 = 'FCC';
  P_PenColor = 'COULEUR.CRAYON';
  P_SetPenWidth = 'FIXE.TAILLE.CRAYON';
  P_PenWidth = 'TAILLE.CRAYON';
  P_PenReverse = 'INVERSE.CRAYON';
  P_Rubber = 'GOMME';
  P_Normal = 'NORMAL';
  P_PenState = 'ETAT.CRAYON'; // 70
  P_SetPenState = 'FIXE.ETAT.CRAYON';
  P_SetPenState2 = 'FEC';
  P_SetPos = 'FIXE.POS';
  P_SetPos2 = 'FPOS';
  P_SetXY = 'FIXE.XY';
  P_SetX = 'FIXEX';
  P_SetX2 = 'FX';
  P_SetY = 'FIXEY';
  P_SetY2 = 'FY';
  P_Pos = 'POS'; // 80
  P_X = 'XCOOR';
  P_Y = 'YCOOR';
  P_SetSpeed = 'FIXE.VITESSE';
  P_Speed = 'VITESSE';
  P_Fen = 'CLOS';
  P_Roll = 'ENROULE';
  P_Window = 'FENETRE';
  P_Window2 = 'FEN';
  P_ScreenState = 'ETAT.ECRAN';
  P_SetScale = 'FIXE.ECHELLE'; // 90
  P_SetScaleX = 'FIXE.ECHELLEX';
  P_SetScaleY = 'FIXE.ECHELLEY';
  P_Scale = 'ECHELLE';
  P_ClearScreen = 'VIDE.ECRAN';
  P_ClearScreen2 = 'VE';
  P_Home = 'ORIGINE';
  P_Wipe = 'NETTOIE';
  P_SetBackGroundColor = 'FIXE.COULEUR.FOND';
  P_SetBackGroundColor2 = 'FCF';
  P_BackGroundColor = 'COULEUR.FOND'; // 100
  P_BackGroundColor2 = 'CF';
  P_Distance = 'DISTANCE';
  P_Forward = 'AVANCE';
  P_Forward2 = 'AV';
  P_Backward = 'RECULE';
  P_Backward2 = 'RE';
  P_Left = 'GAUCHE';
  P_Left2 = 'TG';
  P_Right = 'DROITE';
  P_Right2 = 'TD'; // 110
  P_To = 'POUR'; //
  P_End = 'FIN';
  P_First = 'PREMIER';
  P_First2 = 'PREM';
  P_Last = 'DERNIER';
  P_Last2 = 'DER';
  P_ButFirst = 'SAUF.PREMIER';
  P_ButFirst2 = 'SP';
  P_ButLast = 'SAUF.DERNIER';
  P_ButLast2 = 'SD'; // 120
  P_Write = 'ECRIS';
  P_WriteA = 'ECRIST';
  P_PutFirst = 'METS.PREMIER';
  P_PutFirst2 = 'MP';
  P_PutLast = 'METS.DERNIER';
  P_PutLast2 = 'MD';
  P_Insert = 'INSERE';
  P_Reverse = 'INVERSE';
  P_Uppercase = 'MAJUSCULES';
  P_Lowercase = 'MINUSCULES'; // 130
  P_Shuffle = 'MELANGE';
  P_Replace = 'REMPLACE';
  P_Delete = 'SUPPRIME';
  P_Sort = 'TRIE';
  P_Rotate = 'ROTATION';
  P_Item = 'ELEMENT';
  P_ClearAll = '.EFFACE.TOUT';
  P_Sentence = 'PHRASE';
  P_Sentence2 ='PH';
  P_SentenceRight = 'PHRASE.FIN'; // 140
  P_BeforeP = 'PLP?';
  P_AfterP = 'PLG?';
  P_Count = 'COMPTE';
  P_EQUALP = 'EGAL?';
  P_Ident = 'IDENTIFICATEUR?';
  P_MemberP = 'MEMBRE?';
  P_NumberP = 'NOMBRE?';
  P_WordP = 'MOT?';
  P_Previous = 'PRECEDENT';
  P_Next = 'SUIVANT'; // 150
  P_EmptyP = 'VIDE?';
  P_Firsts = 'PREMS';
  P_ButFirsts = 'SAUF.PREMS';
  P_ListP = 'LISTE?';
  P_DProp = 'DPROP';
  P_RProp = 'RPROP';
  P_DelProp = 'ANPROP';
  P_Props = 'PROPS';
  P_Del = 'ANNULE';
  P_CountProps = 'COMPTE.PROPS'; // 160
  P_PropP = 'PROP?';
  P_PropListP = 'LISTE.PROP?';
  P_ProcedureP = 'PROCEDURE?';
  P_Prim = 'PRIMITIVE?';
  P_NameP = 'NOM?';
  P_List = 'LISTE';
  P_ReadList = 'LIS.LISTE';
  P_ReadChar = 'LIS.CAR';
  P_ClearText = 'VIDE.TEXTE';
  P_ClearText2 = 'VT'; // 170
  P_ReadList2 = 'LL';
  P_PropList = 'PLISTE';
  P_Follow = 'TRACE';
  P_Exec = 'EXEC';
  P_Test = 'TESTE';
  P_IfTrue = 'SI.VRAI';
  P_IfFalse = 'SI.FAUX';
  P_Procs = 'PROCEDURES';
  P_Prims = 'PRIMITIVES';
  P_Vars = 'VARIABLES'; // 180
  P_LocVars = 'LOCALES';
  P_Loc = 'LOCALE';
  MF_Unknown = ''; // fonction non définie
  MF_DAbs = 'ABS'; // valeur absolue
  MF_DAbs2 = 'ABSOLUE';
  MF_DCos = 'COS'; // cosinus
  MF_DCos2 = 'COSINUS';
  MF_DSin = 'SIN'; // sinus
  MF_DSin2 = 'SINUS';
  MF_DTan = 'TAN'; // tangente
  MF_DTan2 = 'TANGENTE'; // 190
  MF_DSqrt = 'RACINE'; // racine carrée
  MF_DSqrt2 = 'RAC';
  MF_DTrunc = 'TRONQUE'; // nombre tronqué
  MF_DRound = 'ARRONDI'; // nombre arrondi
  MF_DSqr = 'AU.CARRE'; // nombre au carré
  MF_DExp = 'EXP'; // exponentielle
  MF_DFrac = 'FRAC'; // partie fractionnelle
  MF_DInt = 'ENT'; // partie entière
  MF_DInt2 = 'ENTIER';
  MF_DLn = 'LN'; // log népérien // 200
  MF_DLog2 = 'LOG2'; // log base 2
  MF_DLog10 = 'LOG'; // log base 10
  MF_DCoTan = 'COTAN'; // cotangente
  MF_DCoTan2 = 'COTANGENTE';
  MF_DArcCos = 'ARCCOS'; // arc cosinus
  MF_DArcCos2 = 'ARCCOSINUS';
  MF_DArcSin = 'ARCSIN'; // arc sinus
  MF_DArcSin2 = 'ARCSINUS';
  MF_DMinus = 'NEGATIF?'; // nombre négatif ?
  MF_DPLus = 'POSITIF?'; // nombre positif ? // 210
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
  MF_Mod = 'MOD'; // modulo // 220
  MF_DPower = 'PUISSANCE'; // puissance
  // deux opérateurs
  MF_DMax = 'MAX'; // maximum
  MF_DMax2 = 'MAXIMUM';
  MF_DMin = 'MIN'; // minimum
  MF_DMin2 = 'MINIMUM';
  MF_DHypot = 'HYPOTHENUSE'; // hypothénuse
  MF_Sum = 'SOMME';
  MF_Minus = 'DIFFERENCE';
  MF_Minus2 = 'DIFF';
  MF_Mul = 'MUL';  // 230
  MF_Mul2 = 'PRODUIT';
  MF_Div = 'QUOTIENT';
  MF_DIV2 = 'DIV';
  P_PenColor2 = 'CC';
  P_SetXY2 = 'FXY';
  P_SetTurtle = 'FIXE.TORTUE';
  P_SetScreen = 'FIXE.ECRAN';
  P_GetTurtle = 'VALEUR.TORTUE';
  P_GetScreen = 'VALEUR.ECRAN';
  P_SetPen = 'FIXE.CRAYON'; // 240
  P_GetPen = 'VALEUR.CRAYON';
  P_Confirm = 'CONFIRME';
  P_ProcToEdit2 = 'ED';
  P_Type = 'TAPE';
  P_Exec2 = 'EXECUTE';
  P_Bold = 'GRAS';
  P_Underline = 'SOULIGNE';
  P_Italic = 'ITALIQUE';
  P_TextColor = 'COULEUR.TEXTE';
  P_SetTextColor = 'FIXE.COULEUR.TEXTE'; // 250
  P_SetTextColor2 = 'FCT';
  P_BackTextColor = 'COULEUR.FOND.TEXTE';
  P_SetBackTextColor = 'FIXE.COULEUR.FOND.TEXTE';
  P_SetBackTextColor2 = 'FCFT';
  P_TurtleOutPut = 'SORTIE.TORTUE';
  P_SetFontSize = 'FIXE.TAILLE.FONTE';
  P_SetFontSize2 = 'FTF';
  P_FontSize = 'TAILLE.FONTE';
  P_FontSize2 = 'TF';
  P_Font = 'FONTE';  // 260
  P_SetFont = 'FIXE.FONTE';
  P_FSquare = 'F.CARRE';
  P_FCircle = 'F.CERCLE';
  P_FEllipse = 'F.ELLIPSE';
  P_FRect = 'F.RECTANGLE';
  P_FRoundRect = 'F.RECTANGLE.ARRONDI';
  P_Fill = 'REMPLIS';
  P_Show = 'LAISSE.VOIR';
  P_Date = 'DATE';
  P_Time = 'HEURE'; // 270

const
  // tableau du nom des fonctions supportées par l'évaluation
  GVFunctionName: array [TGVFunctions] of string = (MF_Unknown, MF_DAbs,
    MF_DAbs2, MF_DCos, MF_DCos2, MF_DSin, MF_DSin2, MF_DTan, MF_DTan2, MF_DSqrt,
    MF_DSqrt2, MF_DTrunc, MF_DRound, MF_DSqr, MF_DExp, MF_DFrac, MF_DInt,
    MF_DInt2, MF_DLn, MF_DLog2, MF_DLog10, MF_DCoTan, MF_DCoTan2, MF_DArcCos,
    MF_DArcCos2, MF_DArcSin, MF_DArcSin2, MF_DMinus, MF_DPLus, MF_DNegate,
    MF_DSign, MF_DRandom, MF_Not, MF_DPi, MF_True, MF_False, MF_Or, MF_And,
    MF_Mod, MF_DPower, MF_DMax, MF_DMax2, MF_DMin, MF_DMin2, MF_DHypot);

  // *** tableau des primitives ***
  GVPrimName: array [1 .. CPrimCount] of TGVPrimRec = (
    (Key: 1; Name: P_GVLOGO; NbParams: 0),
    (Key: 2; Name: P_UpperLevel; NbParams: 0),
    (Key: 3; Name: P_Break; NbParams: 0),
    (Key: 4; Name: P_If; NbParams: 2),
    (Key: 5; Name: P_Return; NbParams: 1),
    (Key: 6; Name: P_Word; NbParams: 2),
    (Key: 7; Name: P_Repeat; NbParams: 2),
    (Key: 8; Name: P_Item2; NbParams: 2),
    (Key: 9; Name: P_Loop; NbParams: 4),
    (Key: 10; Name: P_EditAllProcs; NbParams: 0), // 10
    (Key: 11; Name: P_LoadProcs; NbParams: 1),
    (Key: 12; Name: P_SaveProcs; NbParams: 2),
    (Key: 13; Name: P_ProcToEdit; NbParams: 1),
    (Key: 14; Name: P_ParamLineProc; NbParams: 1),
    (Key: 15; Name: P_DefListProc; NbParams: 1),
    (Key: 16; Name: P_ParamsCount; NbParams: 1),
    (Key: 17; Name: P_CopyProc; NbParams: 2),
    (Key: 18; Name: P_SaveAll; NbParams: 1),
    (Key: 19; Name: P_LoadAll; NbParams: 1),
    (Key: 20; Name: P_SaveVars; NbParams: 1), // 20
    (Key: 21; Name: P_LoadVars; NbParams: 1),
    (Key: 22; Name: P_PckToEdit; NbParams: 1),
    (Key: 23; Name: P_SavePck; NbParams: 1),
    (Key: 24; Name: P_ListToPck; NbParams: 2),
    (Key: 25; Name: P_DelPck; NbParams: 1),
    (Key: 26; Name: P_UnPackObj; NbParams: 1),
    (Key: 27; Name: P_PckToList; NbParams: 1),
    (Key: 28; Name: P_ProtectedP; NbParams: 1),
    (Key: 29; Name: P_ToPck; NbParams: 2),
    (Key: 30; Name: P_PckUnBurry; NbParams: 1), // 30
    (Key: 31; Name: P_IsBurriedPck; NbParams: 1),
    (Key: 32; Name: P_DelProcs; NbParams: 1),
    (Key: 33; Name: P_DelVars; NbParams: 1),
    (Key: 34; Name: P_Else; NbParams: 1),
    (Key: 35; Name: P_Write2; NbParams: 1),
    (Key: 36; Name: P_PkgCreate; NbParams: 1),
    (Key: 37; Name: P_PkgItemsCount; NbParams: 1),
    (Key: 38; Name: P_PckBurry; NbParams: 1),
    (Key: 39; Name: P_BelongsTo; NbParams: 1),
    (Key: 40; Name: P_Give; NbParams: 2), // 40
    (Key: 41; Name: P_Thing; NbParams: 1),
    (Key: 42; Name: P_SetHeading; NbParams: 1),
    (Key: 43; Name: P_SetHeading2; NbParams: 1),
    (Key: 44; Name: P_Heading; NbParams: 0),
    (Key: 45; Name: P_Towards; NbParams: 1),
    (Key: 46; Name: P_ShowTurtle; NbParams: 0),
    (Key: 47; Name: P_ShowTurtle2; NbParams: 0),
    (Key: 48; Name: P_HideTurtle; NbParams: 0),
    (Key: 49; Name: P_HideTurtle2; NbParams: 0),
    (Key: 50; Name: P_VisibleP; NbParams: 0), // 50
    (Key: 51; Name: P_TurtleState; NbParams: 0),
    (Key: 52; Name: P_SetTurtleState; NbParams: 1),
    (Key: 53; Name: P_NormalTurtle; NbParams: 0),
    (Key: 54; Name: P_GreenTurtle; NbParams: 0),
    (Key: 55; Name: P_SetTurtleSize; NbParams: 1),
    (Key: 56; Name: P_TurtleSize; NbParams: 0),
    (Key: 57; Name: P_PenDown; NbParams: 0),
    (Key: 58; Name: P_PenDown2; NbParams: 0),
    (Key: 59; Name: P_PenUp; NbParams: 0),
    (Key: 60; Name: P_PenUp2; NbParams: 0), // 60
    (Key: 61; Name: P_DownP; NbParams: 0),
    (Key: 62; Name: P_SetPenColor; NbParams: 1),
    (Key: 63; Name: P_SetPenColor2; NbParams: 1),
    (Key: 64; Name: P_PenColor; NbParams: 0),
    (Key: 65; Name: P_SetPenWidth; NbParams: 1),
    (Key: 66; Name: P_PenWidth; NbParams: 0),
    (Key: 67; Name: P_PenReverse; NbParams: 0),
    (Key: 68; Name: P_Rubber; NbParams: 0),
    (Key: 69; Name: P_Normal; NbParams: 0),
    (Key: 70; Name: P_PenState; NbParams: 0), // 70
    (Key: 71; Name: P_SetPenState; NbParams: 1),
    (Key: 72; Name: P_SetPenState2; NbParams: 1),
    (Key: 73; Name: P_SetPos; NbParams: 1),
    (Key: 74; Name: P_SetPos2; NbParams: 1),
    (Key: 75; Name: P_SetXY; NbParams: 2),
    (Key: 76; Name: P_SetX; NbParams: 1),
    (Key: 77; Name: P_SetX2; NbParams: 1),
    (Key: 78; Name: P_SetY; NbParams: 1),
    (Key: 79; Name: P_SetY2; NbParams: 1),
    (Key: 80; Name: P_Pos; NbParams: 0), // 80
    (Key: 81; Name: P_X; NbParams: 0),
    (Key: 82; Name: P_Y; NbParams: 0),
    (Key: 83; Name: P_SetSpeed; NbParams: 1),
    (Key: 84; Name: P_Speed; NbParams: 0),
    (Key: 85; Name: P_Fen; NbParams: 0),
    (Key: 86; Name: P_Roll; NbParams: 0),
    (Key: 87; Name: P_Window; NbParams: 0),
    (Key: 88; Name: P_Window2; NbParams: 0),
    (Key: 89; Name: P_ScreenState; NbParams: 0),
    (Key: 90; Name: P_SetScale; NbParams: 1), // 90
    (Key: 91; Name: P_SetScaleX; NbParams: 1),
    (Key: 92; Name: P_SetScaleY; NbParams: 1),
    (Key: 93; Name: P_Scale; NbParams: 0),
    (Key: 94; Name: P_ClearScreen; NbParams: 0),
    (Key: 95; Name: P_ClearScreen2; NbParams: 0),
    (Key: 96; Name: P_Home; NbParams: 0),
    (Key: 97; Name: P_Wipe; NbParams: 0),
    (Key: 98; Name: P_SetBackGroundColor; NbParams: 1),
    (Key: 99; Name: P_SetBackGroundColor2; NbParams: 1),
    (Key: 100; Name: P_BackGroundColor; NbParams: 0), // 100
    (Key: 101; Name: P_BackGroundColor2; NbParams: 0),
    (Key: 102; Name: P_Distance; NbParams: 1),
    (Key: 103; Name: P_Forward; NbParams: 1),
    (Key: 104; Name: P_Forward2; NbParams: 1),
    (Key: 105; Name: P_Backward; NbParams: 1),
    (Key: 106; Name: P_Backward2; NbParams: 1),
    (Key: 107; Name: P_Left; NbParams: 1),
    (Key: 108; Name: P_Left2; NbParams: 1),
    (Key: 109; Name: P_Right; NbParams: 1),
    (Key: 110; Name: P_Right2; NbParams: 1), // 110
    (Key: 111; Name: P_To; NbParams: 0),
    (Key: 112; Name: P_End; NbParams: 0),
    (Key: 113; Name: P_First; NbParams: 1),
    (Key: 114; Name: P_First2; NbParams: 1),
    (Key: 115; Name: P_Last; NbParams: 1),
    (Key: 116; Name: P_Last2; NbParams: 1),
    (Key: 117; Name: P_ButFirst; NbParams: 1),
    (Key: 118; Name: P_ButFirst2; NbParams: 1),
    (Key: 119; Name: P_ButLast; NbParams: 1),
    (Key: 120; Name: P_ButLast2; NbParams: 1), // 120
    (Key: 121; Name: P_Write; NbParams: 1),
    (Key: 122; Name: P_WriteA; NbParams: 1),
    (Key: 123; Name: P_PutFirst; NbParams: 1),
    (Key: 124; Name: P_PutFirst2; NbParams: 2),
    (Key: 125; Name: P_PutLast; NbParams: 2),
    (Key: 126; Name: P_PutLast2; NbParams: 2),
    (Key: 127; Name: P_Insert; NbParams: 3),
    (Key: 128; Name: P_Reverse; NbParams: 1),
    (Key: 129; Name: P_Uppercase; NbParams: 1),
    (Key: 130; Name: P_Lowercase; NbParams: 1), // 130
    (Key: 131; Name: P_Shuffle; NbParams: 1),
    (Key: 132; Name: P_Replace; NbParams: 3),
    (Key: 133; Name: P_Delete; NbParams: 2),
    (Key: 134; Name: P_Sort; NbParams: 1),
    (Key: 135; Name: P_Rotate; NbParams: 1),
    (Key: 136; Name: P_Item; NbParams: 2),
    (Key: 137; Name: P_ClearAll; NbParams: 0),
    (Key: 138; Name: P_Sentence; NbParams: 2),
    (Key: 139; Name: P_Sentence2;  NbParams: 2),
    (Key: 140; Name: P_SentenceRight; NbParams: 2), // 140
    (Key: 141; Name: P_BeforeP; NbParams: 2),
    (Key: 142; Name: P_AfterP; NbParams: 2),
    (Key: 143; Name: P_Count; NbParams: 1),
    (Key: 144; Name: P_EqualP; NbParams: 2),
    (Key: 145; Name: P_Ident; NbParams: 1),
    (Key: 146; Name: P_MemberP; NbParams: 2),
    (Key: 147; Name: P_NumberP; NbParams: 1),
    (Key: 148; Name: P_WordP; NbParams: 1),
    (Key: 149; Name: P_Previous; NbParams: 2),
    (Key: 150; Name: P_Next; NbParams: 2), // 150
    (Key: 151; Name: P_EmptyP; NbParams: 1),
    (Key: 152; Name: P_Firsts; NbParams: 1),
    (Key: 153; Name: P_ButFirsts; NbParams: 1),
    (Key: 154; Name: P_ListP; NbParams: 1),
    (Key: 155; Name: P_DProp; NbParams: 3),
    (Key: 156; Name: P_RProp; NbParams: 2),
    (Key: 157; Name: P_DelProp; NbParams: 2),
    (Key: 158; Name: P_Props; NbParams: 1),
    (Key: 159; Name: P_Del; NbParams: 1),
    (Key: 160; Name: P_CountProps; NbParams: 1), // 160
    (Key: 161; Name: P_PropP; NbParams: 2),
    (Key: 162; Name: P_PropListP; NbParams: 0),
    (Key: 163; Name: P_ProcedureP; NbParams: 1),
    (Key: 164; Name: P_Prim; NbParams: 1),
    (Key: 165; Name: P_NameP; NbParams: 1),
    (Key: 166; Name: P_List; NbParams: 2),
    (Key: 167; Name: P_ReadList; NbParams: 0),
    (Key: 168; Name: P_ReadChar; NbParams: 0),
    (Key: 169; Name: P_ClearText; NbParams: 0),
    (Key: 170; Name: P_ClearText2; NbParams: 0), // 170
    (Key: 171; Name: P_ReadList2; NbParams: 0),
    (Key: 172; Name: P_PropList; NbParams: 1),
    (Key: 173; Name: P_Follow; NbParams: 1),
    (Key: 174; Name: P_Exec; NbParams: 1),
    (Key: 175; Name: P_Test; NbParams: 1),
    (Key: 176; Name: P_IfTrue; NbParams: 1),
    (Key: 177; Name: P_IfFalse; NbParams: 1),
    (Key: 178; Name: P_Procs; NbParams: 0),
    (Key: 179; Name: P_Prims; NbParams: 0),
    (Key: 180; Name: P_Vars; NbParams: 0), // 180
    (Key: 181; Name: P_LocVars; NbParams: 0),
    (Key: 182; Name: P_Loc; NbParams: 1),
    // fonctions
    (Key: 183; Name: MF_DAbs; NbParams: 1),
    (Key: 184; Name: MF_DAbs2; NbParams: 1),
    (Key: 185; Name: MF_DCos; NbParams: 1),
    (Key: 186; Name: MF_DCos2; NbParams: 1),
    (Key: 187; Name: MF_DSin; NbParams: 1),
    (Key: 188; Name: MF_DSin2; NbParams: 1),
    (Key: 189; Name: MF_DTan; NbParams: 1),
    (Key: 190; Name: MF_DTan2; NbParams: 1), // 190
    (Key: 191; Name: MF_DSqrt; NbParams: 1),
    (Key: 192; Name: MF_DSqrt2; NbParams: 1),
    (Key: 193; Name: MF_DTrunc; NbParams: 1),
    (Key: 194; Name: MF_DRound; NbParams: 1),
    (Key: 195; Name: MF_DSqr; NbParams: 1),
    (Key: 196; Name: MF_DExp; NbParams: 1),
    (Key: 197; Name: MF_DFrac; NbParams: 1),
    (Key: 198; Name: MF_DInt; NbParams: 1),
    (Key: 199; Name: MF_DInt2; NbParams: 1),
    (Key: 200; Name: MF_DLn; NbParams: 1), // 200
    (Key: 201; Name: MF_DLog2; NbParams: 1),
    (Key: 202; Name: MF_DLog10; NbParams: 1),
    (Key: 203; Name: MF_DCoTan; NbParams: 1),
    (Key: 204; Name: MF_DCoTan2; NbParams: 1),
    (Key: 205; Name: MF_DArcCos; NbParams: 1),
    (Key: 206; Name: MF_DArcCos2; NbParams: 1),
    (Key: 207; Name: MF_DArcSin; NbParams: 1),
    (Key: 208; Name: MF_DArcSin2; NbParams: 1),
    (Key: 209; Name: MF_DMinus; NbParams: 1),
    (Key: 210; Name: MF_DPLus; NbParams: 1), // 210
    (Key: 211; Name: MF_DNegate; NbParams: 1),
    (Key: 212; Name: MF_DSign; NbParams: 1),
    (Key: 213; Name: MF_DRandom; NbParams: 1),
    (Key: 214; Name: MF_Not; NbParams: 1),
    (Key: 215; Name: MF_DPi; NbParams: 0),
    (Key: 216; Name: MF_True; NbParams: 0),
    (Key: 217; Name: MF_False; NbParams: 0),
    (Key: 218; Name: MF_Or; NbParams: 2),
    (Key: 219; Name: MF_And; NbParams: 2),
    (Key: 220; Name: MF_Mod; NbParams: 2), // 220
    (Key: 221; Name: MF_DPower; NbParams: 2),
    (Key: 222; Name: MF_DMax; NbParams: 2),
    (Key: 223; Name: MF_DMax; NbParams: 2),
    (Key: 224; Name: MF_DMin; NbParams: 2),
    (Key: 225; Name: MF_DMin2; NbParams: 2),
    (Key: 226; Name: MF_DHypot; NbParams: 2),
    // opérations
    (Key: 227; Name: MF_Sum; NbParams: 2),
    (Key: 228; Name: MF_Minus; NbParams: 2),
    (Key: 229; Name: MF_Minus2; NbParams: 2),
    (Key: 230; Name: MF_Mul; NbParams: 2),  // 230
    (Key: 231; Name: MF_Mul2; NbParams: 2),
    (Key: 232; Name: MF_Div; NbParams: 2),
    (Key: 233; Name: MF_Div2; NbParams: 2),
    (Key: 234; Name: P_PenColor2; NbParams: 0),
    (Key: 235; Name: P_SetXY2; NbParams: 2),
    (Key: 236; Name: P_SetTurtle; NbParams: 1),
    (Key: 237; Name: P_SetScreen; NbParams: 1),
    (Key: 238; Name: P_GetTurtle; NbParams: 0),
    (Key: 239; Name: P_GetScreen; NbParams: 0),
    (Key: 240; Name: P_SetPen; NbParams: 1),  // 240
    (Key: 241; Name: P_GetPen; NbParams: 0),
    (Key: 242; Name: P_Confirm; NbParams: 1),
    (Key: 243; Name: P_ProcToEdit2; NbParams: 1),
    (Key: 244; Name: P_Type; NbParams: 1),
    (Key: 245; Name: P_Exec2; NbParams: 1),
    (Key: 246; Name: P_Bold; NbParams: 1),
    (Key: 247; Name: P_Underline; NbParams: 1),
    (Key: 248; Name: P_Italic; NbParams: 1),
    (Key: 249; Name: P_TextColor; NbParams: 0),
    (Key: 250; Name: P_SetTextColor; NbParams: 1),  // 250
    (Key: 251; Name: P_SetTextColor2; NbParams: 1),
    (Key: 252; Name: P_BackTextColor; NbParams: 0),
    (Key: 253; Name: P_SetBackTextColor; NbParams: 1),
    (Key: 254; Name: P_SetBackTextColor2; NbParams: 1),
    (Key: 255; Name: P_TurtleOutPut; NbParams: 1),
    (Key: 256; Name: P_SetFontSize; NbParams: 1),
    (Key: 257; Name: P_SetFontSize2; NbParams: 1),
    (Key: 258; Name: P_FontSize; NbParams: 0),
    (Key: 259; Name: P_FontSize2; NbParams: 0),
    (Key: 260; Name: P_Font; NbParams: 0),  // 260
    (Key: 261; Name: P_SetFont; NbParams: 1),
    (Key: 262; Name: P_FSquare; NbParams: 1),
    (Key: 263; Name: P_FCircle; NbParams: 1),
    (Key: 264; Name: P_FEllipse; NbParams: 1),
    (Key: 265; Name: P_FRect; NbParams: 1),
    (Key: 266; Name: P_FRoundRect; NbParams: 1),
    (Key: 267; Name: P_Fill; NbParams: 0),
    (Key: 268; Name: P_Show; NbParams: 0),
    (Key: 269; Name: P_Date; NbParams: 0),
    (Key: 270; Name: P_Time; NbParams: 0)) // 270
    ;

implementation

end.

