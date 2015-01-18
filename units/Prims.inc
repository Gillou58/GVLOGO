{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : procédures pour les primitives          |
  |                  Fichier : Prims.inc                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// PRIMS.INC - part of GVLOGO
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


function IsList: Boolean;
// *** est-ce une liste ? ***
begin
  Result := (fWkRec.fItem <> EmptyStr) and (fWkRec.fItem[1] = CBeginList);
end;

procedure AData;
// *** dépile et normalise un mot ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  LW.Text := fWkRec.fItem; // mot normalisé
end;

function IsListPop: Boolean;
// *** dépilement et test si liste ***
begin
  AData; // donnée dépilée
  Result := IsList; // test
end;

procedure DoPrimGVLogo;
// *** GVLOGO ***
begin
  PushConst(CBeginList + CE_GVLogo + CEndList); // message retourné
end;

procedure DoPrimTrue;
// *** VRAI ***
begin
  PushConst(CStTrue); // on empile la valeur de VRAI
end;

procedure DoPrimFalse;
// *** FAUX ***
begin
  PushConst(CStFalse); // on empile la valeur de FAUX
end;

procedure DoPrimWriteAll;
// *** ECRIST ***
begin
  fWkRec.fItem := fExeStack.Pop; // élément en cours
  fWkMess.fMessage := fWkRec.fItem; // valeur brute enregistrée
  fWkMess.fCommand := acWrite; // demande d'écriture
  MessageChange; // notification de changement
end;

procedure DoPrimWrite;
// *** ECRIS ***
begin
  if IsListPop then // liste ?
    fWkMess.fMessage := LU.ListToStr(fWkRec.fItem) // on enlève les crochets
  else
  begin
    LW.Text := fWkRec.fItem; // mot normalisé
    fWkMess.fMessage := LW.Text; // sans les caractères d'échappement
  end;
  fWkMess.fCommand := acWrite; // demande d'écriture
  MessageChange; // notification de changement
end;

procedure DoPrimReturn;
// *** RENDS ***
begin
  AData; // valeur récupérée
  fReturnVal := fWkRec.fItem; // valeur de retour
  fReturnFlag := True; // drapeau levé
end;

procedure DoPrimGive;
// *** DONNE ***
begin
  AData; // un mot attendu
  if fLocVars.IsLocVar(LW.Text) then
    fLocVars.DirectUpdateLocVar(fExeStack.Pop) // variable locale
  else
    fKernel.AddVar(LW.Text, fExeStack.Pop); // variable globale
end;

procedure DoPrimThing;
// *** CHOSE ***
begin
  AData; // un mot attendu
  if fLocVars.IsLocVar(LW.Text) then
    PushConst(fLocVars.DirectValLocVar) // variable locale
  else
    PushConst(fKernel.ValVar(LW.Text)); // variable globale
end;

procedure DoPrimTopLevel;
// *** NIVEAU.SUP ***
begin
  Stop := True; // arrêt demandé
  State := asPrimStop; // stop signalé
end;

procedure DoPrimStop;
// *** STOP ***
begin
  while (not fWkStack.IsEmpty) and (fWkStack.Peek <> CBreak) do
    FWkStack.Pop; // on supprime les commandes en attente
end;

procedure DoPrimExec;
// *** EXEC ***
var
  Li: Integer;
begin
  if IsListPop then // une liste ?
  begin
    if fWkRec.fItem <> CEmptyList then // liste vide ?
    begin
      LL.Text := fWkRec.fItem; // on l'analyse
      if LL.IsValid then // liste valide ?
      begin
        for Li := LL.Count downto 1 do // on balaie cette liste à rebours
          // on empile l'élément dans la liste de travail
          fWkStack.Push(LL[Li - 1]);
      end
      else
        // [### Erreur: liste incorrecte ###]
        SetError(CE_BadList, fWkRec.fItem);
    end
    else
      // [### Erreur: liste vide ###]
      SetError(CE_EmptyList, fWkRec.fItem);
  end
  else
    // [### Erreur: pas une liste ###]
    SetError(CE_UnknownList, fWkRec.fItem);
end;

procedure DoPrimShowTurtle;
// *** MONTRE.TORTUE MT ***
begin
  fTurtle.TurtleVisible := True; // la tortue est visible
end;

procedure DoPrimHideTurtle;
// *** CACHE.TORTUE CT ***
begin
  fTurtle.TurtleVisible := False; // la tortue est invisible
end;

procedure DoPrimPenDown;
// *** BAISSE.CRAYON BC ***
begin
  fTurtle.PenDown := True; // le crayon est baissé
end;

procedure DoPrimPenUp;
// *** LEVE.CRAYON LC ***
begin
  fTurtle.PenDown := False; // le crayon est levé
end;

procedure DoPrimGate;
// *** CLOS ***
begin
  fTurtle.Screen := teGate;
end;

procedure DoPrimRoll;
// *** ENROULE ***
begin
  fTurtle.Screen := teRoll;
end;

procedure DoPrimWin;
// *** FENETRE FEN ***
begin
  fTurtle.Screen := teWin;
end;

procedure DoPrimForward;
// *** AVANCE AV ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
   fTurtle.Move(LW.AsNumber)
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimBackward;
// *** RECULE RE ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
   fTurtle.Move(- LW.AsNumber)
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimLeft;
// *** GAUCHE TG ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
   fTurtle.Turn(LW.AsNumber)
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimRight;
// *** DROITE TD ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
   fTurtle.Turn(- LW.AsNumber)
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimProcs;
// *** PROCEDURES ***
begin
  PushConst(fKernel.ProcsToList);
end;

procedure DoPrimPrims;
// *** PRIMITIVES ***
begin
  PushConst(fKernel.PrimsToList);
end;

procedure DoPrimVars;
// *** VARIABLES ***
begin
  PushConst(fKernel.VarsToList);
end;

procedure DoPrimLocalVars;
// *** LOCALES ***
begin
  PushConst(fLocVars.LocVarsToList);
end;

procedure DoPrimSetPenColor;
// *** FIXE.COULEUR.CRAYON FCC ***
begin
  AData; // un mot attendu
  if LW.IsInt then
    // on transforme la couleur
    fTurtle.LocalPenColor := LW.AsInt
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, LW.Text);
end;

procedure DoPrimNot;
// *** NON ***
begin
  AData; // un mot attendu
  if LW.IsInt then // entier ?
    PushConst(IntToStr(not LW.AsInt))
  else
    // [### Erreur: booléen incorrect ###]
    SetError(CE_BadInt, LW.Text);
end;

procedure DoPrimTo;
// *** mauvais POUR ***
begin
  fWkRec.fItem := fWkRec.fLine; // ligne actuelle retrouvée
  // [### Erreur: POUR mal placé ###]
  SetError(CE_BadTo, fWkRec.fItem);
end;

procedure DoPrimEnd;
// *** mauvais FIN ***
begin
  fWkRec.fItem := fWkRec.fLine; // ligne actuelle retrouvée
  // [### Erreur: FIN mal placé ###]
  SetError(CE_BadEnd, fWkRec.fItem);
end;

procedure DoPrimRandom;
// *** HASARD ***
begin
  if IsListPop then  // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.AtRandom); // on renvoie un élément au hasard
  end
  else
  begin
    LW.Text := fWkRec.fItem; // mot normalisé
    if LW.IsInt then
      PushConst(IntToStr(Random(LW.AsInt) + 1)) // nombre au hasard
    else
      PushConst(LW.Atrandom); // on renvoie un caractère au hasard
  end;
end;

procedure DoPrimEmptyP;
// *** VIDE? ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    // vrai ou faux
    PushConst(IfThen(LL.IsEmptyList, CStTrue, CStFalse));
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    // vrai ou faux
    PushConst(IfThen(LW.IsEmptyWord, CStTrue, CStFalse));
  end;
end;

procedure DoPrimProcToEdit;
// *** EDITE.PROC ***
var
  LLst: TStringList;
  LS: string;
begin
  AData; // un mot attendu
  LLst := TStringList.Create; // création de la liste de travail
  try
    // procédure récupérée
    if (fKernel.ProcToEdit(LW.Text, LLst) and (LLst.Count > 0)) then
      for LS in LLst do  // on balaie la définition
      begin
        fWkMess.fMessage := LS; // le message
        fWkMess.fCommand := acWrite; // demande d'écriture
        MessageChange; // changement notifié
      end;
  finally
    LLst.Free; // libération de la liste de travail
  end;
end;

procedure DoPrimLast;
// *** DERNIER DER ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.Last); // on renvoie le dernier élément
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Last); // un mot : on renvoie le dernier caractère
  end;
end;

procedure DoPrimFirst;
// *** PREMIER PREM ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.First); // on renvoie le premier élément
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.First); // un mot : on renvoie le premier caractère
  end;
end;

procedure DoPrimButFirst;
// *** SAUF.PREMIER SP ***
begin
  if IsList then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.ButFirst); // on renvoie tout sauf le premier élément
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    // un mot : on renvoie tout sauf le premier caractère
    PushConst(LW.ButFirst);
  end;
end;

procedure DoPrimButLast;
// *** SAUF.DERNIER SD ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.ButLast); // on renvoie tout sauf le dernier élément
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.ButLast); // un mot : on renvoie tout sauf le dernier caractère
  end;
end;

procedure DoPrimPutFirst;
// *** METS.PREMIER MP ***
var
  LS: string;
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    AData; // seconde donnée
    if LW.IsValid then // un mot ?
      fWkRec.fItem := LW.Text; // mot brut
    PushConst(LL.PutFirst(fWkRec.fItem)); // met en dernier
  end
  else
  begin
    // c'est un mot
    LW.Text := fWkRec.fItem; // analyse
    LS := LW.Text; // mot brut conservé
    if not IsListPop then // pas une liste ?
      PushConst(LW.PutLast(LS))
    else
      // [### Erreur: liste interdite ###]
      SetError(CE_BadWord, fWkRec.fItem);
  end;
end;

procedure DoPrimPutLast;
// *** METS.DERNIER MD ***
var
  LS: string;
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    AData; // seconde donnée
    if LW.IsValid then // un mot ?
      fWkRec.fItem := LW.Text; // mot brut
    PushConst(LL.PutLast(fWkRec.fItem)); // met en dernier
  end
  else
  begin
    // c'est un mot
    LW.Text := fWkRec.fItem; // analyse
    LS := LW.Text; // mot brut conservé
    if not IsListPop then // pas une liste ?
      PushConst(LW.PutFirst(LS))
    else
      // [### Erreur: liste interdite ###]
      SetError(CE_BadWord, fWkRec.fItem);
  end;
end;

Procedure DoPrimReverse;
// *** INVERSE ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.ReverseItems); // inversion des éléments
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Reverse); // inversion des lettres du mot
  end;
end;

procedure DoPrimUpperCase;
// *** MAJUSCULES ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.Uppercase); // en majuscules
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Uppercase); // mot en majuscules
  end;
end;

procedure DoPrimLowerCase;
// *** MINUSCULES ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.Lowercase); // en minuscules
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Lowercase); // mot en minuscules
  end;
end;

procedure DoPrimShuffle;
// *** MELANGE ***
begin
  if IsListPop then  // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.ShuffleItems); // mélange
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Shuffle); // mot mélangé
  end;
end;

procedure DoPrimSort;
// *** TRIE ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.SortItems); // on trie
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Sort); // lettres du mot triées
  end;
end;

procedure DoPrimRotate;
// *** ROTATION ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.Rotate); // rotation
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.Rotate); // rotation des lettres du mot
  end;
end;

procedure DoPrimNumberP;
// *** NOMBRE? ***
begin
  AData; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsNumber, CStTrue, CStFalse));
end;

procedure DoPrimMemberP;
// *** MEMBRE? ***
var
  LS: string;
  LB: Boolean;
begin
  fWkRec.fItem := fExeStack.Pop; // élément dépilé
  LS := fExeStack.Pop; // donnée dépilée
  if (LS <> EmptyStr) and (LS[1] = CBeginList) then // une liste ?
  begin
    LL.Text := LS; // analyse
    LB := LL.IsItem(fWkRec.fItem); // élément?
  end
  else
  begin
    LW.Text := LS;
    LB := LW.IsMember(fWkRec.fItem); // élément?
  end;
  // vrai ou faux
  PushConst(IfThen(LB, CStTrue, CStFalse));
end;

procedure DoPrimInsert;
// *** INSERE ***
var
  LS: string;
  Li: Integer;
begin
  AData; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    Li := StrToInt(LW.Text); // nombre récupéré
    LS := fExeStack.Pop; // donnée à insérer
    if IsListPop then // une liste ?
    begin
      LL.Text := fWkRec.fItem; // analyse
      // on insère dans la liste
      PushConst(LL.InsertAItem(Li, LS));
    end
    else
    begin
      LW.Text := fWkRec.fItem; // analyse
      // on insère dans le mot
      PushConst(LW.Insert(Li, LS));
    end;
  end
  else
    // [### Erreur: entier exigé ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimLowerP;
// *** PLP? ***
begin
  AData; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsLower(fExeStack.Pop),
    CStTrue, CStFalse));
end;

procedure DoPrimGreaterP;
// *** PLG? ***
begin
  AData; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsGreater(fExeStack.Pop),
    CStTrue, CStFalse));
end;

procedure DoPrimEqualP;
// *** EGAL? ***
begin
  AData; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsEqual(fExeStack.Pop),
    CStTrue, CStFalse));
end;

procedure DoPrimIdentP;
// *** IDENTIFICATEUR? ***
begin
  AData; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsValidIdent, CStTrue, CStFalse));
end;

procedure DoPrimCount;
// *** COMPTE ***
begin
  if IsListPop then // une liste ?
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(IntToStr(LL.Count)); // on renvoie le compte
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(IntToStr(LW.Count)); // on renvoie le compte
  end;
end;

procedure DoPrimSentenceRight;
// *** PHRASE.FIN ***
var
  LS: string;
begin
  if IsListPop then // une liste ?
  begin
    LS := fWkRec.fItem; // donnée conservée
    AData; // seconde donnée
    LL.Text := LS; // liste analysée
    LW.Text := fWkRec.fItem;
    if LW.IsValid then // mot valide ?
      fWkRec.fItem := LU.StrToList(LW.Text); // on élimine les liens
    PushConst(LL.SentenceLeft(fWkRec.fItem)); // résultat empilé (second après)
  end
  else
  begin
    // le premier élément est un mot
    LW.Text := fWkRec.fItem; // donnée analysée
    LS := LW.Text; // texte brut dans la liste
    if IsListPop then // une liste ?
    begin
      LL.Text := LU.StrToList(LS); // on analyse le premier élément
      PushConst(LL.SentenceLeft(fWkRec.fItem)); // le second est placé après
    end
    else
    begin
      LW.Text := fWkRec.fItem;
      fWkRec.fItem := LW.Text; // texte brut dans la liste
      // le second élément est aussi un mot
      PushConst(CBeginList + LW.Text + CBlank + LS + CEndList);
    end;
  end;
end;

procedure DoPrimSentence;
// *** PHRASE PH ***
begin
  fExeStack.Swap; // inversion sur la pile
  DoPrimSentenceRight; // comme PHRASE.FIN
end;

procedure DoPrimReplace;
// *** REMPLACE ***
var
  LS: string;
  Li: Integer;
begin
  AData; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    Li := LW.AsInt;
    AData; // nouveau mot (donnée à remplacer)
    fWkRec.fItem := fExeStack.Pop; // donnée où remplacer
    if IsList then // une liste ?
    begin
      LL.Text := fWkRec.fItem; // analyse
      // on remplace dans la liste
      PushConst(LL.ReplaceItem(Li, LW.Text));
    end
    else
    begin
      LS := LW.Text;
      LW.Text := fWkRec.fItem; // mot analysé
      PushConst(LW.Replace(Li, LS)); // on remplace
    end;
  end
  else
    // [### Erreur: entier exigé ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimDelete;
// *** SUPPRIME ***
var
  Li: Integer;
begin
  AData; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    fWkRec.fItem := fExeStack.Pop; // donnée où supprimer
    Li := LW.AsInt;
    if IsList then // une liste ?
    begin
      LL.Text := fWkRec.fItem; // analyse
      PushConst(LL.DeleteItem(Li)); // on remplace dans la liste
    end
    else
    begin
      LW.Text := fWkRec.fItem;
      PushConst(LW.DelItem(Li)); // on remplace
    end;
  end
  else
    // [### Erreur: entier exigé ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimItem;
// *** ELEMENT ***
var
  Li: Integer;
begin
  fWkRec.fItem := fExeStack.Pop; // nombre dépilé
  LW.Text := fWkRec.fItem;
  if LW.IsInt then // entier correct ?
  begin
    fWkRec.fItem := fExeStack.Pop; // donnée dépilée
    Li := LW.AsInt;
    if IsList then // une liste ?
    begin
      LL.Text := fWkRec.fItem; // analyse
      PushConst(LL[Li - 1]); // élément
    end
    else
    begin
      LW.Text := fWkRec.fItem;
      PushConst(LW[Li]); // élément
    end;
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimRepeat;
// *** REPETE ***
var
  Li: Integer;
begin
  fWkRec.fItem := fExeStack.Pop; // nombre dépilé
  LW.Text := fWkRec.fItem;
  if LW.IsInt then // entier correct ?
  begin
    for Li := 1 to LW.AsInt do // on boucle
    begin
      fExeStack.Dup; // on duplique la donnée à exécuter
      DoPrimExec; // ligne à exécuter
      if Stop or (not Error.Ok) then // arrêt demandé ou erreur ?
        Break; // on sort
    end;
    fExeStack.Pop; // nettoyage de la pile
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimDelProcs;
// *** .SUP.PROCS ***
begin
  if IsListPop then // une liste ?
    // on supprime les procédures de la liste
    fKernel.RemoveSomeProcs(fWkRec.fItem)
  else
  begin
    LW.Text := fWkRec.fItem;
    fKernel.RemoveProc(LW.Text); // un mot : on supprime la procédure visée
  end;
end;

procedure DoPrimDelVars;
// *** .SUP.VARS ***
begin
  if IsListPop then // une liste ?
    // on supprime les variables de la liste
    fKernel.RemoveSomeVars(fWkRec.fItem)
  else
  begin
    LW.Text := fWkRec.fItem;
    fKernel.RemoveVar(LW.Text); // un mot : on supprime la variable visée
  end;
end;

procedure DoPrimIf;
// *** SI ***
begin
  fElse := CTrueState; // "sinon" possible et actif
  AData; // un mot attendu
  if LW.IsBoolean then // un booléen ?
  begin
    if LW.Text = CStTrue then // vrai ?
    begin
      DoPrimExec; // exécution
      fElse := CFalseState; // "sinon" possible mais inactif
    end
    else
      fExeStack.Pop; // élément ignoré
  end
  else
    // [### Erreur: booléen incorrect ###]
    SetError(CE_BadBool, fWkRec.fItem);
end;

procedure DoPrimElse;
// *** SINON ***
begin
  fWkRec.fItem := fExeStack.Peek; // donnée enregistrée
  case fElse of // analyse du drapeau de "sinon"
    CFalseState: fExeStack.Pop; // on ignore la liste suivante
    CTrueState: DoPrimExec; // on l'exécute
    CDisabledState: // [### Erreur: SINON mal placé ###]
      SetError(CE_BadElse, fWkRec.fItem);
  end;
  fElse := CDisabledState; // drapeau de "sinon" indisponible
end;

procedure DoPrimAbs;
// *** ABSOLUE ABS ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Abs(LW.AsNumber)))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimCos;
// *** COSINUS COS ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Cos(DegToRad(LW.AsNumber))))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSin;
// *** SINUS SIN ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Sin(DegToRad(LW.AsNumber))))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimTan;
// *** TANGENTE TAN ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    if not IsZero(Cos(DegToRad(LW.AsNumber))) then
      PushConst(FloatToStr(Tan(DegToRad(LW.AsNumber))))
    else
      // [### Erreur: tangente indéfinie ###]
      SetError(CE_Tan, fWkRec.fLine);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSum;
// *** SOMME ***
var
  LDble1, LDble2: Double;
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AData; // encore un nombre
    if LW.IsNumber then
    begin
      LDble2 := LW.AsNumber; // nombre conservé
      PushConst(FloatToStr(LDble1 + LDble2));  // valeur renvoyée
    end
    else
      // [### Erreur: nombre incorrect ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimMinus;
// *** DIFF DIFFERENCE ***
var
  LDble1, LDble2: Double;
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AData; // encore un nombre
    if LW.IsNumber then
    begin
      LDble2 := LW.AsNumber; // nombre conservé
      PushConst(FloatToStr(LDble1 - LDble2)); // valeur renvoyée
    end
    else
      // [### Erreur: nombre incorrect ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimMul;
// *** MUL PRODUIT ***
var
  LDble1, LDble2: Double;
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AData; // encore un nombre
    if LW.IsNumber then
    begin
      LDble2 := LW.AsNumber; // nombre conservé
      PushConst(FloatToStr(LDble1 * LDble2));  // valeur renvoyée
    end
    else
      // [### Erreur: nombre incorrect ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimDiv;
// *** DIV QUOTIENT ***
var
  LDble1, LDble2: Double;
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AData; // encore un nombre
    if LW.IsNumber then
    begin
      LDble2 := LW.AsNumber; // nombre conservé
      if IsZero(LDble2) then
        // [### Erreur: division par zéro ###]
        SetError(CE_ZeroDiv, fWkRec.fLine)
      else
        PushConst(FloatToStr(LDble1 / LDble2)); // valeur renvoyée
    end
    else
      // [### Erreur: nombre incorrect ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSqrt;
// *** RAC RACINE ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
  begin
    if LW.AsNumber >= 0 then  // nombre positif ?
      PushConst(FloatToStr(Sqrt(LW.AsNumber))) // valeur renvoyée
    else
      // [### Erreur: nombre négatif ###]
      SetError(CE_NoNegNumber, MF_DSqrt);
  end
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimPi;
// *** PI ***
begin
  PushConst(FloatToStr(Pi));
end;

procedure DoPrimOr;
// *** OU ***
var
  Li, Lj: Integer;
begin
  AData; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AData; // encore un nombre
    if LW.IsInt then
    begin
      Lj := LW.AsInt; // nombre conservé
      PushConst(FloatToStr(Li or Lj));  // valeur renvoyée
    end
    else
      // [### Erreur: entier incorrect ###]
      SetError(CE_BadInt, fWkRec.fItem);
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimAnd;
// *** ET ***
var
  Li, Lj: Integer;
begin
  AData; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AData; // encore un nombre
    if LW.IsInt then
    begin
      Lj := LW.AsInt; // nombre conservé
      PushConst(FloatToStr(Li and Lj));  // valeur renvoyée
    end
    else
      // [### Erreur: entier incorrect ###]
      SetError(CE_BadInt, fWkRec.fItem);
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimMod;
// *** MOD ***
var
  Li, Lj: Integer;
begin
  AData; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AData; // encore un nombre
    if LW.IsInt then
    begin
      Lj := LW.AsInt; // nombre conservé
      PushConst(FloatToStr(Li mod Lj));  // valeur renvoyée
    end
    else
      // [### Erreur: entier incorrect ###]
      SetError(CE_BadInt, fWkRec.fItem);
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimNegate;
// *** OPPOSE ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(-LW.AsNumber)) // valeur renvoyée
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSetHeading;
// *** FIXE.CAP FCAP ***
begin
  AData; // un mot attendu
  if LW.IsNumber then
    fTurtle.Heading := LW.AsNumber // cap fixé
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimHeading;
// *** CAP ***
begin
  PushConst(FloatToStr(fTurtle.Heading)); // cap renvoyé
end;

procedure DoPrimList;
// *** LISTE ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  LW.Text := CBeginList + fWkRec.fItem + CBlank + fExeStack.Pop + CEndList;
  PushConst(LW.Text); // on renvoie la liste
end;

procedure DoPrimPropP;
// *** PROP? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProp(fExeStack.Pop, fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimPropListP;
// *** LISTE.PROP? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsListP(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimProcP;
// *** PROCEDURE? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProc(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimPrimP;
// *** PRIMITIVE? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsPrim(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimNameP;
// *** NOM? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsVar(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimLoadProcs;
// *** CHARGE.PROCS ***
begin
  AData; // mot attendu
  fKernel.LoadProcs(fWkRec.fItem); // on charge
end;

procedure DoPrimSaveProcs;
// *** SAUVE.PROCS ***
var
  LS: string;
begin
  AData; // mot attendu
  LS := LW.WithoutQuote; // nom récupéré
  if IsListPop then // une liste ?
    fKernel.SaveProcs(LS, fWkRec.fItem) // on sauvegarde
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_UnKnownList, fWkRec.fItem);
end;

procedure DoPrimParamLineProc;
// *** PARAMS.PROC ***
var
  LS: string;
begin
  AData; // mot attendu
  if fKernel.ParamsLine(fWkRec.fItem, LS) then // ligne ?
    PushConst(LS); // on l'empile
end;

procedure DoPrimDefListProc;
// *** DEF.PROC ***
var
  LS: string;
begin
  AData; // mot attendu
  if fKernel.ProcListDef(fWkRec.fItem, LS) then // définition ?
    PushConst(LS); // on l'empile
end;

procedure DoPrimParamsCount;
// *** NB.PARAMS.PROC ***
begin
  AData; // mot attendu
  PushConst(IntToStr(fKernel.NumParamsPrim(fWkRec.fItem))); // nombre empilé
end;

procedure DoPrimCopyProc;
// *** COPIE.PROC ***
begin
  AData; // mot attendu
  fWkRec.fItem := fExeStack.Pop; // où copier ?
  fKernel.CopyDef(LW.Text, fWkRec.fItem); // on copie
end;

procedure DoPrimSaveAll;
// *** SAUVE ***
begin
  AData; // mot attendu
  fKernel.SaveAll(fWkRec.fItem); // on sauvegarde
end;

procedure DoPrimLoadAll;
// *** CHARGE ***
begin
  AData; // mot attendu
  fKernel.LoadAll(fWkRec.fItem); // on charge
end;

procedure DoPrimSaveVars;
// *** SAUVE.VARS ***
begin
  if IsListPop then // une liste ?
    fKernel.SaveVars(LW.Text, fWkRec.fItem) // on sauvegarde
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_UnKnownList, fWkRec.fItem);
end;

procedure DoPrimLoadVars;
// *** CHARGE.VARS ***
begin
  AData; // mot attendu
  fKernel.LoadVars(fWkRec.fItem); // on charge
end;

procedure DoPrimPckToEdit;
// *** EDITE.PAQUET ***
begin
  AData; // mot attendu ######
end;

procedure DoPrimSavePck;
// *** SAUVE.PAQUET ***
begin
  AData; // mot attendu
  fKernel.SavePck(fWkRec.fItem); // on sauvegarde
end;

procedure DoPrimListToPck;
// *** LISTE.VERS.PAQUET ***
begin
  AData; // mot attendu
end;

procedure DoPrimDelPck;
// *** SUPPRIME.PAQUET ***
begin
  AData; // mot attendu
  fKernel.RemovePck(fWkRec.fItem);
end;

procedure DoPrimUnPackObj;
// *** DEPAQUETTE ***
begin
  AData; // mot attendu
  fKernel.UnPackObj(fWkRec.fItem); // objet dépaqueté
end;

procedure DoPrimPckToList;
// *** PAQUET.VERS.LISTE ***
begin
  PushConst(fKernel.PcksToList); // on empile la liste
end;

procedure DoPrimProtectedP;
// *** PROTEGE? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProtected(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimToPck;
// *** VERS.PAQUET ***
begin
  AData; // mot récupéré
  fWkRec.fItem := fExeStack.Pop; // objet récupéré
  fKernel.ToPck(LW.Text, fWkRec.fItem); // dans paquet
end;

procedure DoPrimPckUnBurry;
// *** DETERRE ***
begin
  AData; // mot récupéré
  fKernel.UnBurryPck(fWkRec.fItem); // paquet déterré
end;

procedure DoPrimIsBurriedPck;
// *** ENTERRE? ***
begin
  AData; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsBurriedPck(fWkRec.fItem),
    CStTrue, CStFalse));
end;

procedure DoPrimPkgCreate;
// *** PAQUET ***
begin
  AData; // mot récupéré
  fKernel.CreatePck(fWkRec.fItem); // paquet créé
end;

procedure DoPrimPkgItemsCount;
// *** ELEMENTS.PAQUET ***
begin
  AData; // mot récupéré
  PushConst(IntToStr(fKernel.CountItemsPck((fWkRec.fItem)))); // nombre empilé
end;

procedure DoPrimPckBurry;
// *** ENTERRE ***
begin
  AData; // mot récupéré
  fKernel.BurryPck(fWkRec.fItem); // on enterre
end;

procedure DoPrimBelongsTo;
// *** APPARTIENT.A ***
begin
  AData; // mot récupéré
  PushConst(fKernel.BelongsTo(fWkRec.fItem)); // résultat empilé
end;

procedure DoPrimTowards;
// *** VERS ***
var
  Li: Integer;
begin
  if IsListPop then
  begin
    LL.Text := fWkRec.fItem; // liste analysée
    if LL.Count = 2 then // deux éléments attendus
    begin
      LW.Text := LL.First; // premier élément
      if LW.IsInt then // un nombre ?
      begin
        Li := LW.AsInt; // nombre récupéré
        LW.Text := LL.Last; // second élément
        if LW.IsInt then // encore un nombre ?
          // on empile le résultat
          PushConst(FloatToStr((fTurtle.Towards(Li, LW.AsInt))))
        else
          // [### Erreur: mauvais entier ###]
          SetError(CE_BadInt, LL.Last);
      end
      else
        // [### Erreur: mauvais entier ###]
          SetError(CE_BadInt, LL.First);
    end
    else
      // [### Erreur: mauvaise liste ###]
      SetError(CE_BadList, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_BadList, fWkRec.fItem);
end;

procedure DoPrimWipeScreen;
// *** VIDE.ECRAN VE ***
begin
  fTurtle.ReInit; // initialisation de l'écran
end;

procedure DoPrimFor;
// *** BOUCLE ***
var
  LS, LLst: string;
  Li, Lj: Integer;
begin
  AData; // premier paramètre récupéré
  if LW.IsValidIdent then // nom de variable possible ?
  begin
    LS := LW.Text; // on conserve sa valeur
    AData; // deuxième paramètre
    if LW.IsInt then // un entier ?
    begin
      Li := LW.AsInt; // entier conservé
      AData; // troisième paramètre
      if LW.IsInt then // un entier ?
      begin
        Lj := LW.AsInt; // le conserver
        if IsListPop then // une liste ?
        begin
          LLst := FWkRec.fItem; // liste conservée
          // le premier paramètre est-il le nom d'une variable locale ?
          if fLocVars.IsLocVar(LS) then
            fLocVars.DirectUpdateLocVar(IntToStr(Li)) // mise à jour
          else
          // une variable globale ou une nouvelle variable ?
          if not fKernel.AddVar(LS,IntToStr(Li)) then // une erreur ?
            Exit; // on sort
          for Li := Li to Lj do // on boucle comme demandé
            if Stop or (not Error.Ok) then
              Process(LLst); // exécution de la liste
        end
        else
        // [### Erreur: mauvaise liste ###]
        SetError(CE_BadList, fWkRec.fItem);
      end
      else
        // [### Erreur: mauvais entier ###]
        SetError(CE_BadInt, fWkRec.fItem);
    end
    else
      // [### Erreur: mauvais entier ###]
      SetError(CE_BadInt, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais identificateur ###]
    SetError(CE_BadName, fWkRec.fItem);
end;

procedure DoPrimVisibleP;
// *** VISIBLE? ***
begin
  // vrai ou faux
  PushConst(IfThen(fTurtle.TurtleVisible, CStTrue, CStFalse));
end;

procedure DoPrimSetTurtleSize;
// *** FIXE.TAILLE ***
begin
  AData; // mot récupéré
  if LW.IsInt then // un entier ?
    fTurtle.Size := LW.AsInt // taille de la tortue ajustée
  else
    // [### Erreur: mauvais entier ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimTurtleSize;
// *** TAILLE ***
begin
  PushConst(IntToStr(fTurtle.Size)); // taille retrouvée et empilée
end;

procedure DoPrimNormalTurtle;
// *** TORTUE.NORMALE ***
begin
  fTurtle.Kind := tkTriangle; // un triangle pour la tortue
end;

procedure DoPrimGreenTurtle;
// *** TORTUE.VERTE ***
begin
  fTurtle.Kind := tkPng; // tortue PNG
end;

procedure DoPrimPenDownP;
// *** BAISSE? ***
begin
  // vrai ou faux
  PushConst(IfThen(fTurtle.PenDown, CStTrue, CStFalse));
end;

procedure DoPrimX;
// *** XCOOR ***
begin
  PushConst(FloatToStr(fTurtle.CoordX)); // X
end;

procedure DoPrimY;
// *** YCOOR ***
begin
  PushConst(FloatToStr(fTurtle.CoordY)); // Y
end;

procedure DoPrimSetSpeed;
// *** FIXE.VITESSE ***
begin
  AData; // mot récupéré
  if LW.IsInt then // un entier ?
    fTurtle.Speed := LW.AsInt // vitesse fixée
  else
    // [### Erreur: mauvais entier ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimSpeed;
// *** VITESSE ***
begin
  PushConst(IntToStr(fTurtle.Speed)); // vitesse
end;

procedure DoPrimMax;
// *** MAX ***
begin
  AData; // donnée dépilée
  PushConst(LW.Greatest(fExeStack.Pop)); // le plus grand
end;

procedure DoPrimMin;
// *** MIN ***
begin
  AData; // donnée dépilée
  PushConst(LW.Lowest(fExeStack.Pop)); // le plus petit
end;

procedure DoPrimHypot;
// *** HYPOTHENUSE ***
var
  LDble: Double;
begin
  AData; // donnée empilée
  if LW.IsNumber then // un nombre ?
  begin
    LDble := LW.AsNumber; // nombre conservé
    AData; // second paramètre
    if LW.IsNumber then // un nombre ?
      PushConst(FloatToStr(Hypot(LDble, LW.AsNumber)))
    else
      // [### Erreur: mauvais nombre ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimPower;
// *** PUISSANCE ***
var
  LDble: Double;
begin
  AData; // donnée récupérée
  if LW.IsNumber then // un nombre ?
  begin
    LDble := LW.AsNumber; // nombre conservé
    AData; // second paramètre
    if LW.IsNumber then // un nombre ?
      PushConst(FloatToStr(Power(LDble, LW.AsNumber)))
    else
      // [### Erreur: mauvais nombre ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSign;
// *** SIGNE ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then // un nombre ?
    PushConst(IntToStr(Sign(LW.AsNumber))) // valeur du signe -1 0 +1
  else
    // [### Erreur: mauvais entier ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimMinusP;
// *** NEGATIF? ***
begin
  AData; // un mot attendu
  if LW.IsNumber then // un nombre ?
    // vrai ou faux
    PushConst(IfThen(LW.IsNegate, CStTrue, CStFalse));
end;

procedure DoPrimPlusP;
// *** POSITIF? ***
begin
  AData; // un mot attendu
  if LW.IsNumber then // un nombre ?
    // vrai ou faux
    PushConst(IfThen(not LW.IsNegate, CStTrue, CStFalse));
end;

procedure DoPrimClearAll;
// *** .EFFACE.TOUT ***
begin
  fKernel.Clear; // nettoyage du noyau
  Clear; //... et tout le reste...
end;

procedure DoPrimTurtleState;
// *** ETAT.TORTUE ***
// abscisse, ordonnée, cap, taille, vitesse, visibilité et forme
begin
  PushConst(fTurtle.TurtleState);
end;

procedure DoPrimSetTurtleState;
// *** FIXE.ETAT.TORTUE ***
begin
  AData; // donnée attendue
  fTurtle.TurtleState := fWkRec.fItem;
end;

procedure DoPrimPenColor;
// *** COULEUR.CRAYON CC ***
begin
  PushConst(IntToStr(fTurtle.LocalPenColor));
end;

procedure DoPrimSetPenWidth;
// *** FIXE.TAILLE.CRAYON ***
begin
  AData; // donnée récupérée
  if LW.IsInt then // un entier ?
    fTurtle.PenWidth := LW.AsInt // épaisseur changée
  else
    // [### Erreur: mauvais entier ###]
    SetError(CE_BadInt, fWkRec.fItem);
end;

procedure DoPrimPenWidth;
// *** TAILLE.CRAYON ***
begin
  PushConst(IntToStr(fTurtle.PenWidth));
end;

procedure DoPrimPenReverse;
// *** INVERSE.CRAYON ***
begin
  fTurtle.PenReverse;
end;

procedure DoPrimRubber;
// *** GOMME ***
begin
  fTurtle.PenRubber := True;
end;

procedure DoPrimNormal;
// *** NORMAL ***
begin
  fTurtle.PenRubber := False;;
end;

procedure DoPrimPenState;
// *** ETAT.CRAYON ***
// couleur, épaisseur, baissé?, gomme?
begin
  PushConst(fTurtle.PenState);
end;

procedure DoPrimSetPenState;
// *** FIXE.ETAT.CRAYON FEC ***
begin
  AData; // donnée récupérée
  fTurtle.PenState := fWkRec.fItem;
end;

procedure DoPrimSetPos;
// ***  FIXE.POS FPOS ***
begin
  AData; // donnée récupérée
  fTurtle.PosState := fWkRec.fItem;
end;

procedure DoPrimSetXY;
// ***  FIXE.XY FXY ***
var
  LDble: Double;
begin
  AData; // donnée récupérée
  if LW.IsNumber then // un nombre
  begin
    LDble := LW.AsNumber; // première donnée conservée
    AData; // seconde donnée
    if LW.IsNumber then // un autre nombre ?
      fTurtle.SetPos(LDble, LW.AsNumber) // position fixée
    else
      // [### Erreur: mauvais nombre ###]
      SetError(CE_BadNumber, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSetX;
// ***  FIXEX FX ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    fTurtle.CoordX := LW.AsNumber // abscisse
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSetY;
// ***  FIXEY FY ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    fTurtle.CoordY := LW.AsNumber // ordonnée
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimPos;
// *** POS ***
begin
  PushConst(CBeginList + FloatToStr(fTurtle.CoordX) + CBlank +
    FloatToStr(fTurtle.CoordY) + CEndList);
end;

procedure DoPrimScreenState;
// *** ETAT.ECRAN ***
begin
  PushConst(fTurtle.ScreenState);
end;

procedure DoPrimSetScale;
// *** FIXE.ECHELLE ***
begin
  AData; // donnée récupérée
  fTurtle.ScreenState := fWkRec.fItem;
end;

procedure DoPrimSetScaleX;
// *** FIXE.ECHELLEX ***
begin
  PushConst(IntToStr(fTurtle.ScaleX));
end;

procedure DoPrimSetScaleY;
// *** FIXE.ECHELLEY ***
begin
  PushConst(IntToStr(fTurtle.ScaleY));
end;

procedure DoPrimScale;
// *** ECHELLE ***
begin
  PushConst(fTurtle.ScaleState);
end;

procedure DoPrimHome;
// *** ORIGINE ***
begin
  fTurtle.Home;
end;

procedure DoPrimWipe;
// *** NETTOIE ***
begin
  fTurtle.Wipe;
end;

procedure DoPrimSetBackGroundColor;
// *** FIXE.COULEUR.FOND FCF ***
begin
  AData; // un mot attendu
  if LW.IsInt then
    // on transforme la couleur
    fTurtle.LocalScreenColor := StrToInt(LW.Text)
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, LW.Text);
end;

procedure DoPrimBackGroundColor;
// *** COULEUR.FOND CF ***
begin
  PushConst(IntToStr(fTurtle.LocalScreenColor));
end;

procedure DoPrimDistance;
// *** DISTANCE ***
var
  Li: Integer;
begin
  if IsListPop then
  begin
    LL.Text := fWkRec.fItem; // liste analysée
    if LL.Count = 2 then // deux éléments attendus
    begin
      LW.Text := LL.First; // premier élément
      if LW.IsInt then // un nombre ?
      begin
        Li := LW.AsInt; // nombre récupéré
        LW.Text := LL.Last; // second élément
        if LW.IsInt then // encore un entier ?
          // on empile le résultat
          PushConst(FloatToStr((fTurtle.Distance(Li, LW.AsInt))))
        else
          // [### Erreur: mauvais entier ###]
          SetError(CE_BadInt, LL.Last);
      end
      else
        // [### Erreur: mauvais entier ###]
        SetError(CE_BadInt, LL.First);
    end
    else
      // [### Erreur: mauvaise liste ###]
      SetError(CE_BadList, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_BadList, fWkRec.fItem);
end;

procedure DoPrimTrunc;
// *** TRONQUE ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(Trunc(LW.AsNumber)))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimRound;
// *** ARRONDI ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(Round(LW.AsNumber)))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSqr;
// *** AU.CARRE ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(LW.AsNumber * LW.AsNumber))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimExp;
// *** EXP ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(Exp(LW.AsNumber)))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimFrac;
// *** FRAC ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(Frac(LW.AsNumber)))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimPropList;
// *** PLISTE ***
var
  LS: string;
begin
  AData; // donnée
  if fKernel.PList(fWkRec.fItem, LS) then
    PushConst(LS);
end;

procedure DoPrimClearText;
// *** VIDE.TEXTE VT ***
begin
  fWkMess.fCommand := acClear; // demande d'effacement
  MessageChange; // changement notifié
end;

procedure DoPrimWordP;
// *** MOT? ***
begin
  AData; // récupération d'une donnée
  // vrai ou faux
  PushConst(IfThen(LW.IsValid, CStTrue, CStFalse));
end;

procedure DoPrimPrevious;
// *** PRECEDENT ***
var
  LS: string;
begin
  AData;
  LS := LW.Text; // première donnée
  AData; // seconde donnée
  // empile le premier par ordre alaphabétique
  PushConst(LW.Lowest(LS));
end;

procedure DoPrimNext;
// *** SUIVANT ***
var
  LS: string;
begin
  AData;
  LS := LW.Text; // première donnée
  AData; // seconde donnée
  // empile le dernier par ordre alaphabétique
  PushConst(LW.Greatest(LS));
end;

procedure DoPrimInt;
// *** ENT ENTIER ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
    PushConst(FloatToStr(Int(LW.AsNumber)))
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimLn;
// *** LN ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if LW.IsNegate then
      // [### Erreur: nombre négatif impossible ###]
      SetError(CE_Ln, fWkRec.fItem)
    else
      PushConst(FloatToStr(Ln(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimLog2;
// *** LOG2 ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if LW.IsNegate then
      // [### Erreur: nombre négatif impossible ###]
      SetError(CE_Ln, fWkRec.fItem)
    else
      PushConst(FloatToStr(Log2(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimLog10;
// *** LOG ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if LW.IsNegate then
      // [### Erreur: nombre négatif impossible ###]
      SetError(CE_Ln, fWkRec.fItem)
    else
      PushConst(FloatToStr(Log10(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimCoTan;
// *** COTAN COTANGENTE ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if (Sin(LW.AsNumber) = 0) then
      // [### Erreur: ctangente non définie ###]
      SetError(CE_CoTan, fWkRec.fItem)
    else
      PushConst(FloatToStr(CoTan(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimArcCos;
// *** ARCCOS ARCCOSINUS***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if (LW.AsNumber < -1) or (LW.AsNumber > 1) then
      // [### Erreur: hors intervalle [-1,1] ###]
      SetError(CE_Arc, fWkRec.fItem)
    else
      PushConst(FloatToStr(ArcCos(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimArcSin;
// *** ARCSIN ARSINUS ***
begin
  AData; // donnée récupérée
  if LW.IsNumber then
  begin
    if (LW.AsNumber < -1) or (LW.AsNumber > 1) then
      // [### Erreur: hors intervalle [-1,1] ###]
      SetError(CE_Arc, fWkRec.fItem)
    else
      PushConst(FloatToStr(ArcSin(LW.AsNumber)));
  end
  else
    // [### Erreur: mauvais nombre ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimTest;
// *** TESTE ***
begin
  AData; // un mot attendu
  // booléen ?
  if LW.IsBoolean then
  begin
    if (LW.Text = CStTrue) then  // résultat vrai ?
      fTest := CTrueState // drapeau adapté
    else
      fTest := CFalseState;
  end
  else
    // [### Erreur: booléen incorrect ###]
    SetError(CE_BadBool, fWkRec.fItem);
end;

procedure DoPrimIfTrue;
// *** SI.VRAI ***
begin
  fWkRec.fItem := fExeStack.Peek; // donnée enregistrée
  case fTest of // analyse du drapeau de "sinon"
    CFalseState: fExeStack.Pop; // on ignore la liste suivante
    CTrueState: DoPrimExec; // on l'exécute
    CDisabledState: // [### Erreur: SI.VRAI mal placé ###]
      SetError(CE_BadTest, P_IfTrue);
  end;
end;

procedure DoPrimIfFalse;
// *** SI.FAUX ***
begin
  fWkRec.fItem := fExeStack.Peek; // donnée enregistrée
  case fTest of // analyse du drapeau de "sinon"
    CTrueState: fExeStack.Pop; // on ignore la liste suivante
    CFalseState: DoPrimExec; // on l'exécute
    CDisabledState: // [### Erreur: SI.FAUX mal placé ###]
      SetError(CE_BadTest, P_IfFalse);
  end;
end;

procedure DoPrimSetTurtle;
// *** FIXE.TORTUE ***
begin
  AData; // donnée récupérée
  fTurtle.TurtleState := fWkRec.fItem;
end;

procedure DoPrimSetScreen;
// *** FIXE.ECRAN ***
begin
  AData; // donnée récupérée
  fTurtle.ScreenState := fWkRec.fItem;
end;

procedure DoPrimGetTurtle;
// *** VALEUR.TORTUE ***
begin
  PushConst(fTurtle.TurtleState);
end;

procedure DoPrimGetScreen;
// *** VALEUR.ECRAN ***
begin
  PushConst(fTurtle.ScreenState);
end;

procedure DoPrimSetPen;
// *** FIXE.CRAYON ***
begin
  AData; // donnée récupérée
  fTurtle.PenState := fWkRec.fItem;
end;

procedure DoPrimGetPen;
// *** VALEUR.CRAYON ***
begin
  PushConst(fTurtle.PenState);
end;

procedure DoPrimListP;
// *** LISTE? ***
begin
  LL.Text := fExeStack.Pop; // donnée récupérée
  PushConst(IfThen(LL.IsValid, CStTrue, CStFalse)); // vrai ou faux
end;

procedure DoPrimDProp;
// *** DPROP ***
var
  LName, LProp: string;
begin
  AData; // donnée récupérée
  if LW.IsValidIdent then // mot valide ?
  begin
    LName := LW.Text; // nom conservé
    AData; // donnée suivante
    if LW.IsValidIdent then // mot valide ?
    begin
      LProp := LW.Text; // propriété conservée
      AData; // dernière donnée
      fKernel.DProp(LName, LProp, LW.Text); // propriété définie
    end
    else
      // [### Erreur: mauvais nom ###]
      SetError(CE_BadName, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nom ###]
    SetError(CE_BadName, fWkRec.fItem);
end;

procedure DoPrimRProp;
// *** RPROP ***
var
  LS: string;
begin
  AData; // donnée récupérée
  if LW.IsValidIdent then // mot valide ?
  begin
    LS := LW.Text; // nom conservé
    AData; // donnée suivante
    if LW.IsValidIdent then // mot valide ?
      PushConst(fKernel.RProp(LS, LW.Text)) // valeur empilée
    else
      // [### Erreur: mauvais nom ###]
      SetError(CE_BadName, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nom ###]
    SetError(CE_BadName, fWkRec.fItem);
end;

procedure DoPrimDelProp;
// *** ANPROP ***
var
  LS: string;
begin
  AData; // donnée récupérée
  if LW.IsValidIdent then // mot valide ?
  begin
    LS := LW.Text; // nom conservé
    AData; // donnée suivante
    if LW.IsValidIdent then // mot valide ?
      fKernel.AnProp(LS, LW.Text) // propriété supprimée
    else
      // [### Erreur: mauvais nom ###]
      SetError(CE_BadName, fWkRec.fItem);
  end
  else
    // [### Erreur: mauvais nom ###]
    SetError(CE_BadName, fWkRec.fItem);
end;

procedure DoPrimFirsts;
// *** PREMS ***
begin
  if IsListPop then
  begin
    LL.Text := fWkRec.fItem;
    PushConst(LL.Firsts);
  end
  else
    // [### Erreur: mauvaise liste ###]
      SetError(CE_BadList, fWkRec.fItem);
end;

procedure DoPrimButFirsts;
// *** SAUF.PREMS ***
begin
  if IsListPop then
  begin
    LL.Text := fWkRec.fItem;
    PushConst(LL.ButFirsts);
  end
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_BadList, fWkRec.fItem);
end;

procedure DoPrimProps;
// *** PROPS ***
begin
  AData; // donnée récupérée
  PushConst(fKernel.ListOfProps(fWkRec.fItem));
end;

procedure DoPrimDel;
// *** ANNULE ***
begin
  AData; // donnée récupérée
  fKernel.DelPropList(fWkRec.fItem);
end;

procedure DoPrimCountProps;
// *** COMPTE.PROPS ***
begin
  AData; // donnée récupérée
  PushConst(IntToStr(fKernel.CountProps(fWkRec.fItem)));
end;

procedure DoPrimLocal;
// *** LOCALE ***
begin
  AData; // nom récupéré
  if fLocVars.IsLocVar(fWkRec.fItem) then // le nom est déjà pris ?
    // [### Erreur: existe déjà ###]
    SetError(CE_AlreadyExists, fWkRec.fItem)
  else
  begin
    fLocVars.AddNewLocNumber; // allocation d'un emplacement
    if Error.Ok then // pas d'erreur ?
      fLocVars.AddLocVar(fWkRec.fItem, EmptyStr); // variable enregistrée
  end;
end;

procedure DoPrimFollow;
// *** TRACE ***
begin
  fFollow := not fFollow; // trace inversée
end;

procedure DoPrimReadList;
// *** LIS.LISTE LL ***
begin
  fWkMess.fCommand := acReadList; // demande de lecture
  MessageChange; // notification de changement
  fWkRec.fItem := fWkMess.fMessage; // on récupère le message de retour
  LL.Text := CBeginList + fWkRec.fItem + CEndList; // contrôle de la liste
  if LL.IsValid then // liste correcte ?
    PushConst(fWkRec.fItem) // résultat empilé
  else
    // [### Erreur: pas une liste ###]
    SetError(CE_BadList, fWkRec.fItem);
end;

procedure DoPrimConfirm;
// *** CONFIRME ***
begin
  if IsListPop then // une liste ?
    fWkMess.fMessage := LU.ListToStr(fWkRec.fItem) // message affecté
  else
    fWkMess.fMessage := fWkRec.fItem;
  fWkMess.fCommand := acConfirm; // demande de réponse O/N
  MessageChange; // notification de changement
  fWkRec.fItem := fWkMess.fMessage; // on récupère le message de retour
  LW.Text := fWkRec.fItem; // analyse du résultat
  if LW.IsBoolean then // un booléen ?
    PushConst(fWkRec.fItem) // résultat empilé
  else
    // [### Erreur: pas un booléen ###]
    SetError(CE_BadBool, fWkRec.fItem);
end;

procedure DoPrimAllProcsEdit;
// *** EDITE.TOUT ***
var
  LLst: TStringList;
  LS: string;
begin
  LLst := TStringList.Create; // création de la liste de travail
  try
    // procédures récupérées
    if (fKernel.AllProcsToEdit(LLst) and (LLst.Count > 0)) then
      for LS in LLst do  // on balaie la définition
      begin
        fWkMess.fMessage := LS; // le message
        fWkMess.fCommand := acWrite; // demande d'écriture
        MessageChange; // changement notifié
      end;
  finally
    LLst.Free; // libération de la liste de travail
  end;
end;

procedure DoPrimWord;
// *** MOT ***
var
  LS: string;
begin
  AData; // donnée récupérée
  if LW.IsValid then // mot correct ?
  begin
    LS := fWkRec.fItem; // mot conservé
    AData; // nouvelle donnée
    if LW.IsValid then // correcte aussi ?
      PushConst(LW.PutFirst(LS)) // mot construit
    else
      // [### Erreur: pas un mot ###]
      SetError(CE_BadWord, fWkRec.fItem);
  end
  else
    // [### Erreur: pas un mot ###]
    SetError(CE_BadWord, fWkRec.fItem);
end;

// #################################################### //

procedure DoExePrim(const N: Integer);
// *** exécution d'une primitive ***
begin
  // on exécute la primitive
  case N of
    1: DoPrimGVLogo; // GVLOGO
    2: DoPrimTopLevel; // NIVEAU.SUP
    3: DoPrimStop; // STOP
    4: DoPrimIf; // SI
    5: DoPrimReturn; // RENDS
    6: DoPrimWord; // MOT
    7: DoPrimRepeat; // REPETE
    8:; // ### JUSQUA ###
    9: DoPrimFor; // BOUCLE
    10: DoPrimAllProcsEdit; // EDITE.TOUT
    11: DoPrimLoadProcs; // CHARGE.PROCS
    12: DoPrimSaveProcs; // SAUVE.PROCS
    13, 243: DoPrimProcToEdit; // EDITE.PROC
    14: DoPrimParamLineProc;  // PARAMS.PROC
    15: DoPrimDefListProc; // DEF.PROC
    16: DoPrimParamsCount; // NB.PARAMS.PROC
    17: DoPrimCopyProc; // COPIE.PROC
    18: DoPrimSaveAll; // SAUVE
    19: DoPrimLoadAll; // CHARGE
    20: DoPrimSaveVars; // SAUVE.VARS
    21: DoPrimLoadVars; // CHARGE.VARS
    22: DoPrimPckToEdit; // EDITE.PAQUET
    23: DoPrimSavePck; // SAUVE.PAQUET
    24: DoPrimListToPck; // LISTE.VERS.PAQUET
    25: DoPrimDelPck; // SUPPRIME.PAQUET
    26: DoPrimUnPackObj; // DEPAQUETTE
    27: DoPrimPckToList; // PAQUET.VERS.LISTE
    28: DoPrimProtectedP; // PROTEGE?
    29: DoPrimToPck; // VERS.PAQUET
    30: DoPrimPckUnBurry; // DETERRE
    31: DoPrimIsBurriedPck; // ENTERRE?
    32: DoPrimDelProcs; // .SUP.PROCS
    33: DoPrimDelVars; // .SUP.VARS
    34: DoPrimElse; // SINON
    35: DoPrimWrite; // EC
    36: DoPrimPkgCreate; // PAQUET
    37: DoPrimPkgItemsCount; // ELEMENTS.PAQUET
    38: DoPrimPckBurry; // ENTERRE
    39: DoPrimBelongsTo; // APPARTIENT.A
    40: DoPrimGive; // DONNE
    41: DoPrimThing; // CHOSE
    42, 43: DoPrimSetHeading; // FIXE.CAP FCAP
    44: DoPrimHeading; // CAP
    45: DoPrimTowards; // VERS
    46, 47: DoPrimShowTurtle; // MONTRE.TORTUE MT
    48, 49: DoPrimHideTurtle; // CACHE.TORTUE CT
    50: DoPrimVisibleP; // VISIBLE?
    51: DoPrimTurtleState; // ETAT.TORTUE
    52: DoPrimSetTurtleState; // FIXE.ETAT.TORTUE
    53: DoPrimNormalTurtle; // TORTUE.NORMALE
    54: DoPrimGreenTurtle; // TORTUE.VERTE
    55: DoPrimSetTurtleSize; // FIXE.TAILLE.TORTUE
    56: DoPrimTurtleSize; // TAILLE.TORTUE
    57, 58: DoPrimPenDown; // BAISSE.CRAYON BC
    59, 60: DoPrimPenUp; // LEVE.CRAYON LC
    61: DoPrimPenDownP; // BAISSE?
    62, 63: DoPrimSetPenColor; // FIXE.COULEUR.CRAYON FCC
    64, 234: DoPrimPenColor; // COULEUR.CRAYON CC
    65: DoPrimSetPenWidth; // FIXE.TAILLE.CRAYON
    66: DoPrimPenWidth; // TAILLE.CRAYON
    67: DoPrimPenReverse; // INVERSE.CRAYON
    68: DoPrimRubber; // GOMME
    69: DoPrimNormal; // NORMAL
    70: DoPrimPenState; // ETAT.CRAYON
    71, 72: DoPrimSetPenState; // FIXE.ETAT.CRAYON FEC
    73, 74: DoPrimSetPos; // FIXE.POS FPOS
    75, 235: DoPrimSetXY; // FIXE.XY
    76, 77: DoPrimSetX; // FIXEX FX
    78, 79: DoPrimSetY; // FIXEY FY
    80: DoPrimPos; // POS
    81: DoPrimX; // XCOOR
    82: DoPrimY; // YCOOR
    83: DoPrimSetSpeed; // FIXE.VITESSE
    84: DoPrimSpeed; // VITESSE
    85: DoPrimGate; // CLOS
    86: DoPrimRoll; // ENROULE
    87, 88: DoPrimWin; // FENETRE FEN
    89: DoPrimScreenState; // ETAT.ECRAN
    90: DoPrimSetScale; // FIXE.ECHELLE
    91: DoPrimSetScaleX; // FIXE.ECHELLEX
    92: DoPrimSetScaleY; // FIXE.ECHELLEY
    93: DoPrimScale; // ECHELLE
    94, 95: DoPrimWipeScreen; // VIDE.ECRAN VE
    96: DoPrimHome; // ORIGINE
    97: DoPrimWipe; // NETTOIE
    98, 99: DoPrimSetBackGroundColor; // FIXE.COULEUR.FOND FCF
    100, 101: DoPrimBackGroundColor; // COULEUR.FOND CF
    102: DoPrimDistance; // DISTANCE
    103, 104: DoPrimForward; // AVANCE AV
    105, 106: DoPrimBackward; // RECULE RE
    107, 108: DoPrimLeft; // GAUCHE TG
    109, 110: DoPrimRight; // DROITE TD
    111: DoPrimTo; // POUR
    112: DoPrimEnd; // END
    113, 114: DoPrimFirst; // PREMIER PREM
    115, 116: DoPrimLast; // DERNIER DER
    117, 118: DoPrimButFirst; // SAUF.PREMIER SP
    119, 120: DoPrimButLast; // SAUF.DERNIER SD
    121: DoPrimWrite; // ECRIS
    122: DoPrimWriteAll; // ECRIST
    123, 124: DoPrimPutFirst; // METS.PREMIER MP
    125, 126: DoPrimPutLast; // METS.DERNIER MD;
    127: DoPrimInsert; // INSERE
    128: DoPrimReverse; // INVERSE
    129: DoPrimUppercase; // MAJUSCULES
    130: DoPrimLowercase; // MINUSCULES
    131: DoPrimShuffle; // MELANGE
    132: DoPrimReplace; // REMPLACE
    133: DoPrimDelete; // SUPPRIME
    134: DoPrimSort; // TRIE
    135: DoPrimRotate; // ROTATION
    136: DoPrimItem; // ELEMENT
    137: DoPrimClearAll; // .EFFACE.TOUT
    138, 139: DoPrimSentence; // PHRASE PH
    140: DoPrimSentenceRight; // PHRASE.FIN
    141: DoPrimLowerP; // PLP?
    142: DoPrimGreaterP; // PLG?
    143: DoPrimCount; // COMPTE
    144: DoPrimEqualP; // EGAL?
    145: DoPrimIdentP; // IDENTIFICATEUR?
    146: DoPrimMemberP; // MEMBRE?
    147: DoPrimNumberP; // NOMBRE?
    148: DoPrimWordP;  // MOT?
    149: DoPrimPrevious; // PRECEDENT
    150: DoPrimNext; // SUIVANT
    151: DoPrimEmptyP; // VIDE?
    152: DoPrimFirsts; // PREMS
    153: DoPrimButFirsts; // SAUF.PREMS
    154: DoPrimListP; // LISTE?
    155: DoPrimDProp; // DPROP
    156: DoPrimRProp; // RPROP
    157: DoPrimDelProp; // ANPROP
    158: DoPrimProps; // PROPS
    159: DoPrimDel; // ANNULE
    160: DoPrimCountProps; // COMPTE.PROPS
    161: DoPrimPropP; // PROP?
    162: DoPrimPropListP; // LISTE.PROP?
    163: DoPrimProcP; // PROCEDURE?
    164: DoPrimPrimP;  // PRIMITIVE?
    165: DoPrimNameP;  // NOM?
    166: DoPrimList; // LISTE
    167, 171: DoPrimReadList; // LIS.LISTE LL
    168: ; // ### LIS.CAR ###
    169, 170: DoPrimClearText; // VIDE.TEXTE VT
    172: DoPrimPropList; // PLISTE
    173: DoPrimFollow;  // TRACE
    174: DoPrimExec; // EXEC
    175: DoPrimTest; // TESTE
    176: DoPrimIfTrue; // SI.VRAI
    177: DoPrimIfFalse; // SI.FAUX
    178: DoPrimProcs; // PROCEDURES
    179: DoPrimPrims; // PRIMITIVES
    180: DoPrimVars; // VARIABLES
    181: DoPrimLocalVars; // LOCALES
    182: DoPrimLocal; // LOCALE
    183, 184: DoPrimAbs; // ABSOLUE ABS
    185, 186: DoPrimCos; // COSINUS COS
    187, 188: DoPrimSin; // SINUS SIN
    189, 190: DoPrimTan; // TANGENTE TAN
    191, 192: DoPrimSqrt; // RAC RACINE
    193: DoPrimTrunc; // TRONQUE
    194: DoPrimRound; // ARRONDI
    195: DoPrimSqr; // AU.CARRE
    196: DoPrimExp; // EXP
    197: DoPrimFrac; // FRAC
    198, 199: DoPrimInt; // ENT ENTIER
    200: DoPrimLn; // LN
    201: DoPrimLog2; // LOG2
    202: DoPrimLog10; // LOG
    203, 204: DoPrimCoTan; // COTAN COTANGENTE
    205, 206: DoPrimArcCos; // ARCCOS ARCCOSINUS
    207, 208: DoPrimArcSin; // ARCSIN ARSINUS
    209: DoPrimMinusP; // NEGATIF?
    210: DoPrimPlusP; // POSITIF?
    211: DoPrimNegate; // OPPOSE
    212: DoPrimSign; // SIGNE
    213: DoPrimRandom; // HASARD
    214: DoPrimNot; // NON
    215: DoPrimPi; // PI
    216: DoPrimTrue; // VRAI
    217: DoPrimFalse; // FAUX
    218: DoPrimOr; // OU
    219: DoPrimAnd; // ET
    220: DoPrimMod; // MOD
    221: DoPrimPower; // PUISSANCE
    222, 223: DoPrimMax; // MAX
    224, 225: DoPrimMin; // MIN
    226: DoPrimHypot; // HYPOTHENUSE
    227: DoPrimSum; // SOMME
    228, 229: DoPrimMinus; // DIFF DIFFERENCE
    230, 231: DoPrimMul; // PRODUIT MUL
    232, 233: DoPrimDiv; // DIV QUOTIENT
    236: DoPrimSetTurtle; // FIXE.TORTUE
    237: DoPrimSetScreen; // FIXE.ECRAN
    238: DoPrimGetTurtle; // VALEUR.TORTUE
    239: DoPrimGetScreen; // VALEUR.ECRAN
    240: DoPrimSetPen; // FIXE.CRAYON
    241: DoPrimGetPen; // VALEUR.CRAYON
    242: DoPrimConfirm; // CONFIRME
  end;
end;
