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
  CPrimCount = 243;

 type
  // *** enregistrement d'une primitive ***
  TGVPrimRec = record
    Name: string; // nom
    NbParams: Integer; // nombre de paramètres
  end;

resourcestring
  // *** intitulés des primitives ***
  CComment = '//'; // commentaire
  P_GVLOGO = 'GVLOGO'; // 1
  P_UpperLevel = 'NIVEAU.SUP';
  P_Break = 'STOP';
  P_If = 'SI';
  P_Return = 'RENDS';
  P_While = 'TANT.QUE';
  P_Repeat = 'REPETE';
  P_Until = 'JUSQUA';
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
  P_GreenTurtle = ' TORTUE.VERTE';
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
  P_Word = 'MOT?';
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
  P_SetPen = 'FIXE.CRAYON';
  P_GetPen = 'VALEUR.CRAYON';
  P_Confirm = 'CONFIRME';
  P_ProcToEdit2 = 'ED';

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
    (Name: P_GVLOGO; NbParams: 0),
    (Name: P_UpperLevel; NbParams: 0),
    (Name: P_Break; NbParams: 0),
    (Name: P_If; NbParams: 2),
    (Name: P_Return; NbParams: 1),
    (Name: P_While; NbParams: 2),
    (Name: P_Repeat; NbParams: 2),
    (Name: P_Until; NbParams: 2),
    (Name: P_Loop; NbParams: 4),
    (Name: P_EditAllProcs; NbParams: 0),
    (Name: P_LoadProcs; NbParams: 1),
    (Name: P_SaveProcs; NbParams: 2),
    (Name: P_ProcToEdit; NbParams: 1),
    (Name: P_ParamLineProc; NbParams: 1),
    (Name: P_DefListProc; NbParams: 1),
    (Name: P_ParamsCount; NbParams: 1),
    (Name: P_CopyProc; NbParams: 2),
    (Name: P_SaveAll; NbParams: 1),
    (Name: P_LoadAll; NbParams: 1),
    (Name: P_SaveVars; NbParams: 1),
    (Name: P_LoadVars; NbParams: 1),
    (Name: P_PckToEdit; NbParams: 1),
    (Name: P_SavePck; NbParams: 1),
    (Name: P_ListToPck; NbParams: 2),
    (Name: P_DelPck; NbParams: 1),
    (Name: P_UnPackObj; NbParams: 1),
    (Name: P_PckToList; NbParams: 1),
    (Name: P_ProtectedP; NbParams: 1),
    (Name: P_ToPck; NbParams: 2),
    (Name: P_PckUnBurry; NbParams: 1),
    (Name: P_IsBurriedPck; NbParams: 1),
    (Name: P_DelProcs; NbParams: 1),
    (Name: P_DelVars; NbParams: 1),
    (Name: P_Else; NbParams: 1),
    (Name: P_Write2; NbParams: 1),
    (Name: P_PkgCreate; NbParams: 1),
    (Name: P_PkgItemsCount; NbParams: 1),
    (Name: P_PckBurry; NbParams: 1),
    (Name: P_BelongsTo; NbParams: 2),
    (Name: P_Give; NbParams: 2),
    (Name: P_Thing; NbParams: 1),
    (Name: P_SetHeading; NbParams: 1),
    (Name: P_SetHeading2; NbParams: 1),
    (Name: P_Heading; NbParams: 0),
    (Name: P_Towards; NbParams: 1),
    (Name: P_ShowTurtle; NbParams: 0),
    (Name: P_ShowTurtle2; NbParams: 0),
    (Name: P_HideTurtle; NbParams: 0),
    (Name: P_HideTurtle2; NbParams: 0),
    (Name: P_VisibleP; NbParams: 0),
    (Name: P_TurtleState; NbParams: 0),
    (Name: P_SetTurtleState; NbParams: 1),
    (Name: P_NormalTurtle; NbParams: 0),
    (Name: P_GreenTurtle; NbParams: 0),
    (Name: P_SetTurtleSize; NbParams: 1),
    (Name: P_TurtleSize; NbParams: 0),
    (Name: P_PenDown; NbParams: 0),
    (Name: P_PenDown2; NbParams: 0),
    (Name: P_PenUp; NbParams: 0),
    (Name: P_PenUp2; NbParams: 0),
    (Name: P_DownP; NbParams: 0),
    (Name: P_SetPenColor; NbParams: 1),
    (Name: P_SetPenColor2; NbParams: 1),
    (Name: P_PenColor; NbParams: 0),
    (Name: P_SetPenWidth; NbParams: 1),
    (Name: P_PenWidth; NbParams: 0),
    (Name: P_PenReverse; NbParams: 0),
    (Name: P_Rubber; NbParams: 0),
    (Name: P_Normal; NbParams: 0),
    (Name: P_PenState; NbParams: 0),
    (Name: P_SetPenState; NbParams: 1),
    (Name: P_SetPenState2; NbParams: 1),
    (Name: P_SetPos; NbParams: 1),
    (Name: P_SetPos2; NbParams: 1),
    (Name: P_SetXY; NbParams: 2),
    (Name: P_SetX; NbParams: 1),
    (Name: P_SetX2; NbParams: 1),
    (Name: P_SetY; NbParams: 1),
    (Name: P_SetY2; NbParams: 1),
    (Name: P_Pos; NbParams: 0),
    (Name: P_X; NbParams: 0),
    (Name: P_Y; NbParams: 0),
    (Name: P_SetSpeed; NbParams: 1),
    (Name: P_Speed; NbParams: 0),
    (Name: P_Fen; NbParams: 0),
    (Name: P_Roll; NbParams: 0),
    (Name: P_Window; NbParams: 0),
    (Name: P_Window2; NbParams: 0),
    (Name: P_ScreenState; NbParams: 0),
    (Name: P_SetScale; NbParams: 1),
    (Name: P_SetScaleX; NbParams: 1),
    (Name: P_SetScaleY; NbParams: 1),
    (Name: P_Scale; NbParams: 0),
    (Name: P_ClearScreen; NbParams: 0),
    (Name: P_ClearScreen2; NbParams: 0),
    (Name: P_Home; NbParams: 0),
    (Name: P_Wipe; NbParams: 0),
    (Name: P_SetBackGroundColor; NbParams: 1),
    (Name: P_SetBackGroundColor2; NbParams: 1),
    (Name: P_BackGroundColor; NbParams: 0),
    (Name: P_BackGroundColor2; NbParams: 0),
    (Name: P_Distance; NbParams: 1),
    (Name: P_Forward; NbParams: 1),
    (Name: P_Forward2; NbParams: 1),
    (Name: P_Backward; NbParams: 1),
    (Name: P_Backward2; NbParams: 1),
    (Name: P_Left; NbParams: 1),
    (Name: P_Left2; NbParams: 1),
    (Name: P_Right; NbParams: 1),
    (Name: P_Right2; NbParams: 1),
    (Name: P_To; NbParams: 0),
    (Name: P_End; NbParams: 0),
    (Name: P_First; NbParams: 1),
    (Name: P_First2; NbParams: 1),
    (Name: P_Last; NbParams: 1),
    (Name: P_Last2; NbParams: 1),
    (Name: P_ButFirst; NbParams: 1),
    (Name: P_ButFirst2; NbParams: 1),
    (Name: P_ButLast; NbParams: 1),
    (Name: P_ButLast2; NbParams: 1),
    (Name: P_Write; NbParams: 1),
    (Name: P_WriteA; NbParams: 1),
    (Name: P_PutFirst; NbParams: 1),
    (Name: P_PutFirst2; NbParams: 2),
    (Name: P_PutLast; NbParams: 2),
    (Name: P_PutLast2; NbParams: 2),
    (Name: P_Insert; NbParams: 3),
    (Name: P_Reverse; NbParams: 1),
    (Name: P_Uppercase; NbParams: 1),
    (Name: P_Lowercase; NbParams: 1),
    (Name: P_Shuffle; NbParams: 1),
    (Name: P_Replace; NbParams: 3),
    (Name: P_Delete; NbParams: 2),
    (Name: P_Sort; NbParams: 1),
    (Name: P_Rotate; NbParams: 1),
    (Name: P_Item; NbParams: 2),
    (Name: P_ClearAll; NbParams: 0),
    (Name: P_Sentence; NbParams: 2),
    (Name: P_Sentence2;  NbParams: 2),
    (Name: P_SentenceRight; NbParams: 2),
    (Name: P_BeforeP; NbParams: 2),
    (Name: P_AfterP; NbParams: 2),
    (Name: P_Count; NbParams: 1),
    (Name: P_EqualP; NbParams: 2),
    (Name: P_Ident; NbParams: 1),
    (Name: P_MemberP; NbParams: 2),
    (Name: P_NumberP; NbParams: 1),
    (Name: P_Word; NbParams: 1),
    (Name: P_Previous; NbParams: 2),
    (Name: P_Next; NbParams: 2),
    (Name: P_EmptyP; NbParams: 1),
    (Name: P_Firsts; NbParams: 1),
    (Name: P_ButFirsts; NbParams: 1),
    (Name: P_ListP; NbParams: 1),
    (Name: P_DProp; NbParams: 3),
    (Name: P_RProp; NbParams: 2),
    (Name: P_DelProp; NbParams: 2),
    (Name: P_Props; NbParams: 1),
    (Name: P_Del; NbParams: 1),
    (Name: P_CountProps; NbParams: 1),
    (Name: P_PropP; NbParams: 2),
    (Name: P_PropListP; NbParams: 0),
    (Name: P_ProcedureP; NbParams: 1),
    (Name: P_Prim; NbParams: 1),
    (Name: P_NameP; NbParams: 1),
    (Name: P_List; NbParams: 2),
    (Name: P_ReadList; NbParams: 0),
    (Name: P_ReadChar; NbParams: 0),
    (Name: P_ClearText; NbParams: 0),
    (Name: P_ClearText2; NbParams: 0),
    (Name: P_ReadList2; NbParams: 0),
    (Name: P_PropList; NbParams: 1),
    (Name: P_Follow; NbParams: 0),
    (Name: P_Exec; NbParams: 1),
    (Name: P_Test; NbParams: 1),
    (Name: P_IfTrue; NbParams: 1),
    (Name: P_IfFalse; NbParams: 1),
    (Name: P_Procs; NbParams: 0),
    (Name: P_Prims; NbParams: 0),
    (Name: P_Vars; NbParams: 0),
    (Name: P_LocVars; NbParams: 0),
    (Name: P_Loc; NbParams: 1),
    // fonctions
    (Name: MF_DAbs; NbParams: 1),
    (Name: MF_DAbs2; NbParams: 1),
    (Name: MF_DCos; NbParams: 1),
    (Name: MF_DCos2; NbParams: 1),
    (Name: MF_DSin; NbParams: 1),
    (Name: MF_DSin2; NbParams: 1),
    (Name: MF_DTan; NbParams: 1),
    (Name: MF_DTan2; NbParams: 1),
    (Name: MF_DSqrt; NbParams: 1),
    (Name: MF_DSqrt2; NbParams: 1),
    (Name: MF_DTrunc; NbParams: 1),
    (Name: MF_DRound; NbParams: 1),
    (Name: MF_DSqr; NbParams: 1),
    (Name: MF_DExp; NbParams: 1),
    (Name: MF_DFrac; NbParams: 1),
    (Name: MF_DInt; NbParams: 1),
    (Name: MF_DInt2; NbParams: 1),
    (Name: MF_DLn; NbParams: 1),
    (Name: MF_DLog2; NbParams: 1),
    (Name: MF_DLog10; NbParams: 1),
    (Name: MF_DCoTan; NbParams: 1),
    (Name: MF_DCoTan2; NbParams: 1),
    (Name: MF_DArcCos; NbParams: 1),
    (Name: MF_DArcCos2; NbParams: 1),
    (Name: MF_DArcSin; NbParams: 1),
    (Name: MF_DArcSin2; NbParams: 1),
    (Name: MF_DMinus; NbParams: 1),
    (Name: MF_DPLus; NbParams: 1),
    (Name: MF_DNegate; NbParams: 1),
    (Name: MF_DSign; NbParams: 1),
    (Name: MF_DRandom; NbParams: 1),
    (Name: MF_Not; NbParams: 1),
    (Name: MF_DPi; NbParams: 0),
    (Name: MF_True; NbParams: 0),
    (Name: MF_False; NbParams: 0),
    (Name: MF_Or; NbParams: 2),
    (Name: MF_And; NbParams: 2),
    (Name: MF_Mod; NbParams: 2),
    (Name: MF_DPower; NbParams: 2),
    (Name: MF_DMax; NbParams: 2),
    (Name: MF_DMax; NbParams: 2),
    (Name: MF_DMin; NbParams: 2),
    (Name: MF_DMin2; NbParams: 2),
    (Name: MF_DHypot; NbParams: 2),
    // opérations
    (Name: MF_Sum; NbParams: 2),
    (Name: MF_Minus; NbParams: 2),
    (Name: MF_Minus2; NbParams: 2),
    (Name: MF_Mul; NbParams: 2),
    (Name: MF_Mul2; NbParams: 2),
    (Name: MF_Div; NbParams: 2),
    (Name: MF_Div2; NbParams: 2),
    (Name: P_PenColor2; NbParams: 0),
    (Name: P_SetXY2; NbParams: 2),
    (Name: P_SetTurtle; NbParams: 1),
    (Name: P_SetScreen; NbParams: 1),
    (Name: P_GetTurtle; NbParams: 0),
    (Name: P_GetScreen; NbParams: 0),
    (Name: P_SetPen; NbParams: 1),
    (Name: P_GetPen; NbParams: 0),
    (Name: P_Confirm; NbParams: 1),
    (Name: P_ProcToEdit2; NbParams: 1))
    ;

implementation

end.

