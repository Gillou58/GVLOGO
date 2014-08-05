{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unit� : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
  |                  Date:    18-06-2014 17:17:49                          |
  |                  Version : 2.0.0                                       |
  |                                                                        |
  |========================================================================| }
unit GVConsts;

interface

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
  CQUote = '"';
  CColon = ':';

  { ensembles de caract�res courants }

  CLowAlpha = ['a' .. 'z'];
  CHighAlpha = ['A' .. 'Z'];
  CAlpha = CLowAlpha + CHighAlpha;
  CDigit = ['0' .. '9'];
  CAlphaNum = CAlpha + CDigit;

  { cha�nes utiles }

  CEmptyList = CBeginList + CEndList;

type
  { erreurs }
  TGVError = (
  C_None, // pas d'erreur
  C_BadNumber, // nombre incorrect
  C_BadInt, // entier incorrect
  C_EmptyStr, // mot vide interdit
  c_BadChar, // caract�re incorrect
  C_BadList, // erreur dans une liste
  C_DelItem,  // position incorrecte pour une suppression
  C_InsItem, // position incorrecte pour une insertion
  C_ReplaceItem, // position incorrecte pour un remplacement
  C_NoListWord, // ni un mot ni une liste
  C_TwoDelete // pas assez d'�l�ments pour en supprimer deux
  );

resourcestring
  { message d'erreur }

  ME_None = 'Pas d''erreur � signaler.';
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

  { primitives }

  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

implementation

end.
