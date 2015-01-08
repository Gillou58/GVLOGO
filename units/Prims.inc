function IsList: Boolean;
// *** est-ce une liste ? ***
begin
  Result := (fWkRec.fItem <> EmptyStr) and (fWkRec.fItem[1] = CBeginList);
end;

procedure AWord;
// *** dépile et normalise un mot
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  LW.Text := fWkRec.fItem; // mot normalisé
end;

procedure DoPrimGVLogo;
// *** GVLOGO ***
begin
  PushConst(CBeginList + CE_GVLogo + CEndList); // message retourné
end;

procedure DoPrimTrue;
// *** VRAI ***
begin
  PushConst(IntToStr(CRTrue)); // on empile la valeur de VRAI
end;

procedure DoPrimFalse;
// *** FAUX ***
begin
  PushConst(IntToStr(CRFalse)); // on empile la valeur de FAUX
end;

procedure DoPrimWriteAll;
// *** ECRIST ***
begin
  fWkRec.fItem := fExeStack.Pop; // élément en cours
  fWkRec.fMessage := fWkRec.fItem; // valeur brute enregistrée
  MessageChange; // notification de changement
end;

procedure DoPrimWrite;
// *** ECRIS ***
begin
  fWkRec.fItem := fExeStack.Pop; // valeur à afficher
  if IsList then // liste ?
    fWkRec.fMessage := LU.ListToStr(fWkRec.fItem) // enlève les crochets
  else
  begin
    LW.Text := fWkRec.fItem; // mot normalisé
    fWkRec.fMessage := LW.Text; // sans les caractères d'échappement
  end;
  MessageChange; // notification de changement
end;

procedure DoPrimReturn;
// *** RENDS ***
begin
  PushConst(fExeStack.Pop); // valeur renvoyée
end;

procedure DoPrimGive;
// *** DONNE ***
begin
  AWord; // un mot attendu
  if fLocVars.IsLocVar(LW.Text) then
    fLocVars.DirectUpdateLocVar(fExeStack.Pop) // variable locale
  else
    fKernel.AddVar(LW.Text, fExeStack.Pop); // variable globale
end;

procedure DoPrimThing;
// *** CHOSE ***
begin
  AWord; // un mot attendu
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
  fLocStop := True; // drapeau levé
end;

procedure DoPrimExec;
// *** EXEC ***
var
  Li: Integer;
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    if fWkRec.fItem <> CEmptyList then // liste vide ?
    begin
      LL.Text := fWkRec.fItem; // on l'analyse
      for Li := LL.Count downto 1 do // on balaie cette liste à rebours
        // on empile l'élément dans la liste de travail
        fWkStack.Push(LL[Li - 1]);
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
  AWord; // un mot attendu
  if LW.IsNumber then
   fTurtle.Move(StrToFloat(LW.Text))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimBackward;
// *** RECULE RE ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then
   fTurtle.Move(- StrToFloat(LW.Text))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimLeft;
// *** GAUCHE TG ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then
   fTurtle.Turn(StrToFloat(LW.Text))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, LW.Text);
end;

procedure DoPrimRight;
// *** DROITE TD ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then
   fTurtle.Turn(- StrToFloat(LW.Text))
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

procedure DoSetPenColor;
// *** FIXE.COULEUR.CRAYON FCC ***
begin
  AWord; // un mot attendu
  if LW.IsInt then
  begin
   // ### transformer la couleur ###
   fTurtle.PenColor := IntColorToRGB(StrToInt(LW.Text));
  end
  else
    // [### Erreur: entier incorrect ###]
    SetError(CE_BadInt, LW.Text);
end;

procedure DoPrimNot;
// *** NON ***
begin
  AWord; // un mot attendu
  // booléen ?
  if LW.IsBoolean then
    PushConst(IntToStr(not StrToInt(fWkRec.fItem)))
  else
    // [### Erreur: booléen incorrect ###]
    SetError(CE_BadBool, LW.Text);
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.AtRandom); // on renvoie un élément au hasard
  end
  else
  begin
    LW.Text := fWkRec.fItem; // mot normalisé
    if LW.IsInt then
      PushConst(IntToStr(Random(StrToInt(LW.Text)) + 1)) // nombre au hasard
    else
      PushConst(LW.Atrandom); // on renvoie un caractère au hasard
  end;
end;

procedure DoPrimEmptyP;
// *** VIDE? ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    // vrai ou faux
    PushConst(IfThen(LL.IsEmptyList, IntToStr(CRTrue), IntToStr(CRFalse)));
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    // vrai ou faux
    PushConst(IfThen(LW.IsEmptyWord, IntToStr(CRTrue), IntToStr(CRFalse)));
  end;
end;

procedure DoPrimProcToEdit;
// *** EDITE.PROC ***
begin
  AWord; // un mot attendu
  // fKernel.ProcToEdit(LW.Text, fLines); // procédure éditée ### TODO ###
end;

procedure DoPrimLast;
// *** DERNIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
// *** PREMIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
// *** SAUFPREMIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.ButFirst); // on renvoie tout sauf le premier élément
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.ButFirst); // un mot : on renvoie tout sauf le premier caractère
  end;
end;

procedure DoPrimButLast;
// *** SAUFDERNIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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

procedure DoPrimPutLast;
// *** METSDERNIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donné dépilée
  if IsList then // liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.PutLast(fExeStack.Pop)); // met en dernier
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.PutLast(fExeStack.Pop)); // donnée à la fin du mot
  end;
end;

procedure DoPrimPutFirst;
// *** METSPREMIER ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  if IsList then // liste ?
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.PutFirst(fExeStack.Pop)); // met en premier
  end
  else
  begin
    LW.Text := fWkRec.fItem;
    PushConst(LW.PutFirst(fExeStack.Pop)); // donnée au début du mot
  end;
end;

Procedure DoPrimReverse;
// *** INVERSE ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // analyse
    PushConst(LL.SortItems); // trie
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  AWord; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsNumber, IntToStr(CRTrue), IntToStr(CRFalse)));
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
  PushConst(IfThen(LB, IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimInsert;
// *** INSERE ***
var
  LS: string;
  Li: Integer;
begin
  AWord; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    Li := StrToInt(LW.Text); // nombre récupéré
    LS := fExeStack.Pop; // donnée à insérer
    fWkRec.fItem := fExeStack.Pop; // donnée où insérer
    if IsList then // une liste ?
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
// *** AVANT? ***
begin
  AWord; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsLower(fExeStack.Pop),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimGreaterP;
// *** APRES? ***
begin
  AWord; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsGreater(fExeStack.Pop),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimEqualP;
// *** EGAL? ***
begin
  AWord; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsEqual(fExeStack.Pop),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimIdentP;
// *** IDENTIFICATEUR? ***
begin
  AWord; // un mot attendu
  // vrai ou faux
  PushConst(IfThen(LW.IsValidIdent, IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimCount;
// *** COMPTE ***
begin
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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

procedure DoPrimSentence;
// *** PHRASE ***
begin
  fExeStack.Swap;
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.SentenceLeft(fExeStack.Pop)); // on renvoie la phrase
  end
  else
  begin
    LW.Text := CBeginList + fExeStack.Pop + CBlank + fWkRec.fItem + CEndList;
    PushConst(LW.Text); // on renvoie la liste
  end;
end;

procedure DoPrimSentenceRight;
// *** PHRASE.FIN ***
begin
  fExeStack.Swap;
  fWkRec.fItem:= fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
  begin
    LL.Text := fWkRec.fItem; // on l'analyse
    PushConst(LL.SentenceRight(fExeStack.Pop)); // on renvoie la phrase
  end
  else
  begin
    LW.Text := CBeginList + fWkRec.fItem  + CBlank + fExeStack.Pop + CEndList;
    PushConst(LW.Text); // on renvoie la liste
  end;
end;

procedure DoPrimReplace;
// *** REMPLACE ***
var
  LS: string;
  Li: Integer;
begin
  AWord; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    Li := StrToInt(LW.Text);
    AWord; // nouveau mot (donnée à remplacer)
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
  AWord; // un mot attendu (emplacement)
  if LW.IsInt then // conversion en entier ?
  begin
    fWkRec.fItem := fExeStack.Pop; // donnée où supprimer
    Li := StrToInt(LW.Text);
    // une liste ?
    if IsList then
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
  // entier correct ?
  if LW.IsInt then
  begin
    fWkRec.fItem := fExeStack.Pop; // donnée dépilée
    Li := StrToInt(LW.Text);
    // une liste ?
    if IsList then
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
  // entier correct ?
  LW.Text := fWkRec.fItem;
  if LW.IsInt then
  begin
    for Li := 1 to StrToInt(Lw.Text) do // on boucle
    begin
      fExeStack.Dup; // on duplique la donnée à exécuter
      DoPrimExec; // ligne à exécuter
      if Stop or Error.Ok then // arrêt demandé ?
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  fWkRec.fItem := fExeStack.Pop; // donnée dépilée
  // une liste ?
  if IsList then
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
  AWord; // un mot attendu
  // booléen ?
  if LW.IsBoolean then
  begin
    if LW.Text = IntToStr(CRTrue) then // vrai ?
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
  AWord; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Abs(LW.AsNumber)))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimCos;
// *** COSINUS COS ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Cos(DegToRad(LW.AsNumber))))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSin;
// *** SINUS SIN ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(Sin(DegToRad(LW.AsNumber))))
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimTan;
// *** TANGENTE TAN ***
begin
  AWord; // un mot attendu
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
  AWord; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsNumber then
  begin
    LDble1 := LW.AsNumber; // nombre conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
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
  AWord; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsInt then
  begin
    Li := LW.AsInt; // entier conservé
    AWord; // encore un nombre
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
  AWord; // un mot attendu
  if LW.IsNumber then
    PushConst(FloatToStr(-LW.AsNumber)) // valeur renvoyée
  else
    // [### Erreur: nombre incorrect ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimSetHeading;
// *** FIXE.CAP FCAP ***
begin
  AWord; // un mot attendu
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
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProp(fExeStack.Pop, fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimPropListP;
// *** LISTE.PROP? ***
begin
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsListP(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimProcP;
// *** PROCEDURE? ***
begin
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProc(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimPrimP;
// *** PRIMITIVE? ***
begin
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsPrim(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimNameP;
// *** NOM? ***
begin
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsVar(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimLoadProcs;
// *** CHARGE.PROCS ***
begin
  AWord; // mot attendu
  fKernel.LoadProcs(fWkRec.fItem); // on charge
end;

procedure DoPrimSaveProcs;
// *** SAUVE.PROCS ***
var
  LS: string;
begin
  AWord; // mot attendu
  LS := LW.WithoutQuote; // nom récupéré
  fWkRec.fItem := fExeStack.Pop; // liste dépilée
  // une liste ?
  if IsList then
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
  AWord; // mot attendu
  if fKernel.ParamsLine(fWkRec.fItem, LS) then // ligne ?
    PushConst(LS); // on l'empile
end;

procedure DoPrimDefListProc;
// *** DEF.PROC ***
var
  LS: string;
begin
  AWord; // mot attendu
  if fKernel.ProcListDef(fWkRec.fItem, LS) then // définition ?
    PushConst(LS); // on l'empile
end;

procedure DoPrimParamsCount;
// *** NB.PARAMS.PROC ***
begin
  AWord; // mot attendu
  PushConst(IntToStr(fKernel.NumParamsPrim(fWkRec.fItem))); // nombre empilé
end;

procedure DoPrimCopyProc;
// *** COPIE.PROC ***
begin
  AWord; // mot attendu
  fWkRec.fItem := fExeStack.Pop; // où copier ?
  fKernel.CopyDef(LW.Text, fWkRec.fItem); // on copie
end;

procedure DoPrimSaveAll;
// *** SAUVE ***
begin
  AWord; // mot attendu
  fKernel.SaveAll(fWkRec.fItem); // on sauvegarde
end;

procedure DoPrimLoadAll;
// *** CHARGE ***
begin
  AWord; // mot attendu
  fKernel.LoadAll(fWkRec.fItem); // on charge
end;

procedure DoPrimSaveVars;
// *** SAUVE.VARS ***
begin
  AWord; // mot attendu
  fWkRec.fItem := fExeStack.Pop; // liste dépilée
  // une liste ?
  if IsList then
    fKernel.SaveVars(LW.Text, fWkRec.fItem) // on sauvegarde
  else
    // [### Erreur: mauvaise liste ###]
    SetError(CE_UnKnownList, fWkRec.fItem);
end;

procedure DoPrimLoadVars;
// *** CHARGE.VARS ***
begin
  AWord; // mot attendu
  fKernel.LoadVars(fWkRec.fItem); // on charge
end;

procedure DoPrimPckToEdit;
// *** EDITE.PAQUET ***
begin
  AWord; // mot attendu ######
end;

procedure DoPrimSavePck;
// *** SAUVE.PAQUET ***
begin
  AWord; // mot attendu
  fKernel.SavePck(fWkRec.fItem); // on sauvegarde
end;

procedure DoPrimListToPck;
// *** LISTE.VERS.PAQUET ***
begin
  AWord; // mot attendu
end;

procedure DoPrimDelPck;
// *** SUPPRIME.PAQUET ***
begin
  AWord; // mot attendu
  fKernel.RemovePck(fWkRec.fItem);
end;

procedure DoPrimUnPackObj;
// *** DEPAQUETTE ***
begin
  AWord; // mot attendu
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
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsProtected(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimToPck;
// *** VERS.PAQUET ***
begin
  AWord; // mot récupéré
  fWkRec.fItem := fExeStack.Pop; // objet récupéré
  fKernel.ToPck(LW.Text, fWkRec.fItem); // dans paquet
end;

procedure DoPrimPckUnBurry;
// *** DETERRE ***
begin
  AWord; // mot récupéré
  fKernel.UnBurryPck(fWkRec.fItem); // paquet déterré
end;

procedure DoPrimIsBurriedPck;
// *** ENTERRE? ***
begin
  AWord; // mot récupéré
  // vrai ou faux
  PushConst(IfThen(fKernel.IsBurriedPck(fWkRec.fItem),
    IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimPkgCreate;
// *** PAQUET ***
begin
  AWord; // mot récupéré
  fKernel.CreatePck(fWkRec.fItem); // paquet créé
end;

procedure DoPrimPkgItemsCount;
// *** ELEMENTS.PAQUET ***
begin
  AWord; // mot récupéré
  PushConst(IntToStr(fKernel.CountItemsPck((fWkRec.fItem)))); // nombre empilé
end;

procedure DoPrimPckBurry;
// *** ENTERRE ***
begin
  AWord; // mot récupéré
  fKernel.BurryPck(fWkRec.fItem); // on enterre
end;

procedure DoPrimBelongsTo;
// *** APPARTIENT.A ***
begin
  AWord; // mot récupéré
  PushConst(fKernel.BelongsTo(fWkRec.fItem)); // résultat empilé
end;

procedure DoPrimTowards;
// *** VERS ***
var
  Li: Integer;
begin
  fWkRec.fItem := fExeStack.Pop; // donnée récupérée
  if IsList then
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
          // [### Erreur: mauvaise liste ###]
          SetError(CE_BadList, fWkRec.fItem);
      end
      else
        // [### Erreur: mauvaise liste ###]
        SetError(CE_BadList, fWkRec.fItem);
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
  AWord; // premier paramètre récupéré
  if LW.IsValidIdent then // nom de variable possible ?
  begin
    LS := LW.Text; // on conserve sa valeur
    AWord; // deuxième paramètre
    if LW.IsInt then // un entier ?
    begin
      Li := LW.AsInt; // entier conservé
      AWord; // troisième paramètre
      if LW.IsInt then // un entier ?
      begin
        Lj := LW.AsInt; // le conserver
        AWord; // quatrième paramètre
        if IsList then // une liste ?
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
  PushConst(IfThen(fTurtle.TurtleVisible, IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimSetTurtleSize;
// *** FIXE.TAILLE ***
begin
  AWord; // mot récupéré
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
  PushConst(IfThen(fTurtle.PenDown, IntToStr(CRTrue), IntToStr(CRFalse)));
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
  AWord; // mot récupéré
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
  AWord; // donnée dépilée
  PushConst(LW.Greatest(fExeStack.Pop)); // le plus grand
end;

procedure DoPrimMin;
// *** MIN ***
begin
  AWord; // donnée dépilée
  PushConst(LW.Lowest(fExeStack.Pop)); // le plus petit
end;

procedure DoPrimHypot;
// *** HYPOTHENUSE ***
var
  LDble: Double;
begin
  AWord; // donnée empilée
  if LW.IsNumber then // un nombre ?
  begin
    LDble := LW.AsNumber; // nombre conservé
    AWord; // second paramètre
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
  AWord; // donnée récupérée
  if LW.IsNumber then // un nombre ?
  begin
    LDble := LW.AsNumber; // nombre conservé
    AWord; // second paramètre
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
  AWord; // donnée récupérée
  if LW.IsNumber then // un nombre ?
    PushConst(IntToStr(Sign(LW.AsNumber))) // valeur du signe -1 0 +1
  else
    // [### Erreur: mauvais entier ###]
    SetError(CE_BadNumber, fWkRec.fItem);
end;

procedure DoPrimMinusP;
// *** NEGATIF? ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then // un nombre ?
    // vrai ou faux
    PushConst(IfThen(LW.IsNegate, IntToStr(CRTrue), IntToStr(CRFalse)));
end;

procedure DoPrimPlusP;
// *** POSITIF? ***
begin
  AWord; // un mot attendu
  if LW.IsNumber then // un nombre ?
    // vrai ou faux
    PushConst(IfThen(not LW.IsNegate, IntToStr(CRTrue), IntToStr(CRFalse)));
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
    6: ; // TANT.QUE
    7: DoPrimRepeat; // REPETE
    8:;
    9: DoPrimFor; // BOUCLE
    10:;
    11: DoPrimLoadProcs; // CHARGE.PROCS
    12: DoPrimSaveProcs; // SAUVE.PROCS
    13: DoPrimProcToEdit; // EDITE.PROC
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
    51:;
    52:;
    53: DoPrimNormalTurtle; // TORTUE.NORMALE
    54: DoPrimGreenTurtle; // TORTUE.VERTE
    55: DoPrimSetTurtleSize; // FIXE.TAILLE.TORTUE
    56: DoPrimTurtleSize; // TAILLE.TORTUE
    57, 58: DoPrimPenDown; // BAISSE.CRAYON BC
    59, 60: DoPrimPenUp; // LEVE.CRAYON LC
    61: DoPrimPenDownP; // BAISSE?
    62, 63: DoSetPenColor; // FIXE.COULEUR.CRAYON FCC
    64:;
    65:;
    66:;
    67:;
    68:;
    69:;
    70:;
    71:;
    72:;
    73:;
    74:;
    75:;
    76:;
    77:;
    78:;
    79:;
    80:;
    81: DoPrimX; // XCOOR
    82: DoPrimY; // YCOOR
    83: DoPrimSetSpeed; // FIXE.VITESSE
    84: DoPrimSpeed; // VITESSE
    85: DoPrimGate; // CLOS
    86: DoPrimRoll; // ENROULE
    87, 88: DoPrimWin; // FENETRE FEN
    89:;
    90:;
    91:;
    92:;
    93:;
    94, 95: DoPrimWipeScreen; // VIDE.ECRAN VE
    96:;
    97:;
    98:;
    99:;
    100:;
    101:;
    102:;
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
    137: ; // ### LIBRE AU.HASARD ###
    138, 139: DoPrimSentence; // PHRASE PH
    140: DoPrimSentenceRight; // PHRASE.FIN
    141: DoPrimLowerP; // PLP?
    142: DoPrimGreaterP; // PLG?
    143: DoPrimCount; // COMPTE
    144: DoPrimEqualP; // EGAL?
    145: DoPrimIdentP; // IDENTIFICATEUR?
    146: DoPrimMemberP; // MEMBRE?
    147: DoPrimNumberP; // NOMBRE?
    148:;
    149:;
    150:;
    151: DoPrimEmptyP; // VIDE?
    152:;
    153:;
    154:;
    155:;
    156:;
    157:;
    158:;
    159:;
    160:;
    161: DoPrimPropP; // PROP?
    162: DoPrimPropListP; // LISTE.PROP?
    163: DoPrimProcP; // PROCEDURE?
    164: DoPrimPrimP;  // PRIMITIVE?
    165: DoPrimNameP;  // NOM?
    166: DoPrimList; // LISTE
    167:;
    168:;
    169:;
    170:;
    171:;
    172:;
    173:;
    174: DoPrimExec; // EXEC
    175:;
    176:;
    177:;
    178: DoPrimProcs; // PROCEDURES
    179: DoPrimPrims; // PRIMITIVES
    180: DoPrimVars; // VARIABLES
    181: DoPrimLocalVars; // LOCALES
    182:;
    183, 184: DoPrimAbs; // ABSOLUE ABS
    185, 186: DoPrimCos; // COSINUS COS
    187, 188: DoPrimSin; // SINUS SIN
    189, 190: DoPrimTan; // TANGENTE TAN
    191, 192: DoPrimSqrt; // RAC RACINE
    193:;
    194:;
    195:;
    196:;
    197:;
    198:;
    199:;
    200:;
    201:;
    202:;
    203:;
    204:;
    205:;
    206:;
    207:;
    208:;
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
    232, 233: DoPrimDiv; //DIV QUOTIENT
  end;
end;


{

procedure ProcEdit;
// *** EDITE.PROC ***
begin
  ActualItem := fExeStack.Pop; // donnée dépilée
  LW.Text := ActualItem;
  fKernel.ProcToEdit(LW.Text, fLines); // procédure éditée
end;

}
