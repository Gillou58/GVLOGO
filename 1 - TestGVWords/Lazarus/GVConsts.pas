{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Constantes, types et variables          |
  |                  Unité : GVConsts.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    28-07-2014 18:18:46                          |
  |                  Version : 1.0.0                                       |
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

resourcestring
  { message d'erreur }
  ME_BadNumber = 'L''objet %s n''est pas un nombre correct.';
  ME_BadInt = 'L''objet %s n''est pas un entier correct.';
  ME_EmptyStr = 'Le mot vide ne convient pas pour la primitive %s.';
  ME_BadChar = 'Le mot %s est trop court pour en traiter l''élément %d.';

  { primitives }
  P_First = 'PREMIER';
  P_Last = 'DERNIER';
  P_ButFirst = 'SAUFPREMIER';
  P_ButLast = 'SAUFDERNIER';
  P_True = 'VRAI';
  P_False = 'FAUX';

implementation

end.
