{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables d'erreurs|
  |                  Unité : GVErrConsts.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVERRCONSTS - part of GVLOGO
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

unit GVErrConsts;
// Cette unité contient les constantes, variables et types nécessaires
// à la gestion des erreurs de GVLOGO.

interface

const
  // *** message d'identification ***
  CE_GVLogo = 'GVLOGO 1.0.0 Gilles Vasseur 2014';
  // *** pas de position d'erreur ***
  CE_NoErr = -1;

resourcestring
  // *** messages d'erreur ***
  ME_Nothing = '<vide >'; // élément vide
  ME_None = '%s : pas d''erreur à signaler.';

  // *** erreurs internes ***
  IE_TwoDelete =
    'ERREUR INTERNE - La liste "%s" ne contient pas assez d''éléments'
    + ' pour en supprimer deux.';
  IE_TwoDelete2 =
      'ERREUR INTERNE - La valeur "%s" n''est ni une liste ni un mot.';
  IE_EmptyStack = 'ERREUR INTERNE - La pile interne "%s" est vide.';
  IE_OutOfMemory =
    'ERREUR INTERNE - La mémoire est insuffisante pour la pile "%s".';
  IE_LowStack = 'ERREUR INTERNE - Pas assez d''éléments dans la pile "%s".';
  IE_NoInit =
    'ERREUR INTERNE - La valeur à évaluer n''a pas été initialisée pour "%s".';
  IE_OutOfRange = 'ERREUR INTERNE - Evaluation hors limites pour "%s".';
  IE_ListInit = 'ERREUR INTERNE - La liste "%s" n''a pas été initialisée.';
  IE_NoGetVar = 'ERREUR INTERNE - L''évaluation des variables ' +
     'n''est pas initialisée pour "%s".';
  IE_MemLocVar = 'ERREUR INTERNE - L''espace pour les variables locales ' +
     'est erroné ("%s").';

  // *** erreurs communes ***
  // les nombres
  ME_BadBool = '"%s" n''est pas un booléen correct.';
  ME_BadNumber = '"%s" n''est pas un nombre correct.';
  ME_BadInt = '"%s" n''est pas un entier.';
  ME_UnknownFunction = 'La fonction "%s" est inconnue.';
  ME_NoArg = 'Il manque au moins un paramètre pour "%s".';
  ME_ZeroDiv = 'Les divisions par 0 sont impossibles dans "%s".';
  ME_NoNegNumber = 'Un nombre négatif est interdit pour "%s".';
  ME_FunctionNotSupported =
    'La fonction "%s" n''est pas utilisable dans l''expression.';
  ME_NeedsInteger = 'Un nombre entier était attendu ' +
    'et "%s" n''en est pas un.';
  ME_ParMismatch =
    'Les parenthèses de l''expression "%s" ne sont pas appariées.';
  ME_Tan = 'La fonction tangente dans "%s" n''est pas définie '+
     'pour un cosinus nul.';
  ME_Ln =
    '"%s" est négatif : impossible avec les fonctions logarithmique.';
  ME_CoTan =
    'La fonction cotangente dans "%s" n''est pas définie pour un sinus nul.';
  ME_Arc = 'L''expression "%s" n''est définie que dans l''intervalle [-1,1].';
  ME_BadExp = 'Il y a une expression incorrecte dans "%s".';
  ME_BadChar = 'Caractère interdit ou inconnu dans l''expression "%s".';
  ME_MissClosePar = 'Parenthèse fermante absente dans "%s".';

  // les listes
  ME_BadList = 'La liste "%s" est incorrecte.';
  ME_BadList2 = 'La liste "%s" est incorrecte (crochets à vérifier).';
  ME_BadWord = 'Le mot "%s" est incorrect.';
  ME_UnknownListWord = '"%s" n''est ni une liste ni ' +
    'un mot corrects.';
  ME_EmptyList = '"%s" a besoin d''une liste non vide comme donnée.';
  ME_EmptyWord = '"%s" a besoin d''un mot non vide comme donnée.';
  ME_BadItem = 'La position demandée n''existe pas dans "%s".';
  ME_UnknownList = '"%s" n''est pas une liste.';
  ME_ListBadLength = 'La longueur de "%s" est incorrecte.';

  // les listes de propriétés
  ME_UnknownListP = 'La liste de propriétés "%s" est introuvable.';
  ME_UnknownProp = 'La propriété "%s" est inconnue.';
  ME_UnknownNumListP = 'La liste de propriétés numéro %s est introuvable.';
  ME_UnknownNumProp = 'La propriété numéro %s est inconnue.';
  ME_BadProp = 'La définition de la propriété "%s" est incorrecte.';

  // les fichiers
  ME_BadFileFormat = 'Le format du fichier "%s" est incorrect.';
  ME_BadSave = 'Erreur de sauvegarde du fichier "%s".';
  ME_BadFile = 'Le fichier "%s" est introuvable ou corrompu.';
  ME_FileNotFound = 'Le fichier "%s" est introuvable.';
  ME_Version = 'La version du fichier "%s" est incorrecte.';
  ME_BadContent = 'Le contenu du fichier "%s" est incorrect.';

  // la tortue
  ME_BadColor = 'La couleur "%s" n''existe pas.';

  // les primitives
  ME_EmptyWordOrList = 'La donnée "%s" est incorrecte pour la primitive.';
  ME_UnknownPrim = 'La primitive "%s" est inconnue.';
  ME_CantModifyPrim = 'La primitive "%s" ne peut pas être modifiée.';
  ME_NorPrimNorProc = '"%s" n''est ni une procédure ni une primitive.';
  ME_BadElse = 'Le SI accompagnant SINON avec "%s" est introuvable.';
  ME_BadTest = 'Aucun test ne précède "%s".';

  // les variables
  ME_BadVar = 'La variable "%s" est incorrecte.';
  ME_UnknownVar = 'La variable "%s" est inconnue.';
  ME_NotVar = '"%s" n''est pas une variable.';
  ME_LocVarForbidden = '"%s" ne peut pas être utilisée hors d''une procédure.';
  ME_AlreadyExists = '"%s" existe déjà en tant que variable locale.';
  ME_VarNoName = 'Le mot DONNE n''est pas suivi d''un nom ou d''une valeur ' +
    'dans "%s".';
  ME_VarTooManyDatas = 'Trop d''éléments pour DONNE dans "%s".';

  // les paquets
  ME_UnknownPackage = 'Le paquet "%s" est inconnu.';
  ME_PackageForbidden =
    'Le paquet "%s" ne peut pas être placé dans un autre paquet.';
  ME_AlreadyPackage = '"%s" est déjà un paquet.';
  ME_NotInPackage = '"%s" n''appartient à aucun paquet.';

  // les procédures
  ME_UnknownProc = 'La procédure "%s" est inconnue.';
  ME_BadParam = 'Le paramètre "%s" de la procédure en cours est incorrect.';
  ME_DupParam = 'Le paramètre "%s" de la procédure en cours est en double.';
  ME_BadLine = 'La ligne "%s" de la procédure en cours est incorrecte.';
  ME_BadDef = 'La définition "%s" de la procédure en cours est incorrecte.';
  ME_NotTo = 'Le mot POUR est introuvable dans "%s".';
  ME_NotEnd = 'Le mot FIN est introuvable pour "%s".';
  ME_EmptyEdit =
    'L''éditeur pour la procédure en cours est vide (lignes : "%s").';
  ME_NorProcnorList =
    '"%s" n''est ni une procédure ni une liste de procédures.';
  ME_BadTo = 'Le mot POUR est mal placé dans "%s".';
  ME_BadEnd = 'Le mot FIN est mal placé dans "%s".';
  ME_NoName = 'Le mot POUR n''est pas suivi d''un nom dans "%s".';

  // le noyau
  ME_Protected = 'Opération impossible : l''objet "%s" est protégé.';
  ME_BadName = 'Le nom "%s" est incorrect.';
  ME_UnknownObject = '"%s" n''existe pas.';
  ME_BadObj = 'Le nom "%s" est déjà utilisé pour un autre type d''objet.';
  ME_Burried = 'Opération impossible : "%s" est enterré.';

  // l'interprète
  ME_NotEnoughDatas = 'Pas assez de données pour "%s".';
  ME_WhatAbout = 'Que faut-il faire de "%s" ?';

type
  // *** énumération des erreurs ***
  TGVError = (CE_None, CIE_TwoDelete, CIE_EmptyStack, CIE_OutOfMemory,
    CIE_LowStack, CIE_NoInit, CIE_OutOfRange, CIE_ListInit, CIE_NoGetVar,
    CIE_MemLocVar, CE_BadBool, CE_BadNumber, CE_BadInt, CE_UnknownFunction,
    CE_NoArg, CE_ZeroDiv, CE_NoNegNumber, CE_FunctionNotSupported,
    CE_NeedsInteger, CE_ParMismatch, CE_Tan, CE_Ln, CE_CoTan, CE_Arc,
    CE_BadExp, CE_BadChar, CE_MissClosePar, CE_BadList, CE_BadList2,
    CE_BadWord, CE_UnknownListWord, CE_EmptyList, CE_EmptyWord, CE_BadItem,
    CE_UnknownList, CE_ListBadLength, CE_UnknownListP, CE_UnknownProp,
    CE_UnknownNumListP, CE_UnknownNumProp, CE_BadProp, CE_BadFileFormat,
    CE_BadSave, CE_BadFile, CE_FileNotFound, CE_Version, CE_BadContent,
    CE_BadColor, CE_EmptyWordOrList, CE_UnknownPrim, CE_CantModifyPrim,
    CE_NorPrimNorProc, CE_BadElse, CE_BadTest, CE_BadVar, CE_UnknownVar,
    CE_NotVar, CE_LocVarForbidden, CE_AlreadyExists, CE_VarNoName,
    CE_VarTooManyDatas, CE_UnknownPackage, CE_PackageForbidden,
    CE_AlreadyPackage, CE_NotInPackage, CE_UnknownProc, CE_BadParam,
    CE_DupParam, CE_BadLine, CE_BadDef, CE_NotTo, CE_NotEnd, CE_EmptyEdit,
    CE_NorProcnorList, CE_BadTo, CE_BadEnd, CE_NoName,
    CE_Protected, CE_BadName, CE_UnknownObject, CE_BadObj, CE_Burried,
    CE_NotEnoughDatas, CE_WhatAbout);

  // *** enregistrement d'une erreur ***
  TGVErrorRec = record
    Code: TGVError; // code de l'erreur
    ErrItem: string; // élément fautif dans la ligne de travail
    ErrPos: Integer; // position (absolue ou relative) de l'élément fautif
  end;

const
  // *** nombre d'erreurs ***
  CTotalErrors = Ord(CE_WhatAbout);

  // *** tableau des intitulés d'erreurs ***
  GVErrorName: array [TGVError] of string = (ME_None, IE_TwoDelete,
    IE_EmptyStack, IE_OutOfMemory, IE_LowStack, IE_NoInit, IE_OutOfRange,
    IE_ListInit, IE_NoGetVar, IE_MemLocVar, ME_BadBool, ME_BadNumber,
    ME_BadInt, ME_UnknownFunction, ME_NoArg, ME_ZeroDiv, ME_NoNegNumber,
    ME_FunctionNotSupported, ME_NeedsInteger, ME_ParMismatch, ME_Tan, ME_Ln,
    ME_CoTan, ME_Arc, ME_BadExp, ME_BadChar, ME_MissClosePar, ME_BadList,
    ME_BadList2, ME_BadWord, ME_UnknownListWord, ME_EmptyList, ME_EmptyWord,
    ME_BadItem, ME_UnknownList, ME_ListBadLength, ME_UnknownListP, ME_UnknownProp,
    ME_UnknownNumListP, ME_UnknownNumProp, ME_BadProp, ME_BadFileFormat,
    ME_BadSave, ME_BadFile, ME_FileNotFound, ME_Version, ME_BadContent,
    ME_BadColor, ME_EmptyWordOrList, ME_UnknownPrim, ME_CantModifyPrim,
    ME_NorPrimNorProc, ME_BadElse, ME_BadTest, ME_BadVar, ME_UnknownVar,
    ME_NotVar, ME_LocVarForbidden, ME_AlreadyExists, ME_VarNoName,
    ME_VarTooManyDatas, ME_UnknownPackage, ME_PackageForbidden,
    ME_AlreadyPackage,  ME_NotInPackage, ME_UnknownProc, ME_BadParam,
    ME_DupParam, ME_BadLine, ME_BadDef, ME_NotTo,
    ME_NotEnd, ME_EmptyEdit, ME_NorProcnorList, ME_BadTo, ME_BadEnd, ME_NoName,
    ME_Protected, ME_BadName, ME_UnknownObject, ME_BadObj, ME_Burried,
    ME_NotEnoughDatas, ME_WhatAbout);

implementation

end.

