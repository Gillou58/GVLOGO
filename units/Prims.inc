procedure BadTo;
// *** mauvais POUR ***
begin
  ActualItem := ActualLine; // ligne actuelle retrouvée
  Error := C_BadTo; // POUR mal placé
end;

procedure BadEnd;
// *** mauvais FIN ***
begin
  ActualItem := ActualLine; // ligne actuelle retrouvée
  Error := C_BadEnd; // FIN mal placé
end;

procedure Last;
// *** DERNIER ***
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St = EmptyStr) or (St = CEmptyList) then // une liste ou un mot vide ?
    Error := C_EmptyWordOrList // [Erreur : mot ou liste vide]
  else
  if (St[1] = CBeginList) then  // une liste ?
  begin
    L.Text := St; // on l'analyse
    LList(L.Last); // on renvoie le dernier élément
  end
  else
    LWord(W.Last(St)); // un mot : on renvoie le dernier caractère
end;

procedure First;
// *** PREMIER ***
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St = EmptyStr) or (St = CEmptyList) then // une liste ou un mot vide ?
    Error := C_EmptyWordOrList // [Erreur : mot ou liste vide]
  else
  if (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // on l'analyse
    LList(L.First); // on renvoie le premier élément
  end
  else
    LWord(W.First(St)); // un mot : on renvoie le premier caractère
end;

procedure ButFirst;
// SAUFPREMIER
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St = EmptyStr) or (St = CEmptyList) then // une liste ou un mot vide ?
    Error := C_EmptyWordOrList // [Erreur : mot ou liste vide]
  else
  if (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // on l'analyse
    LList(L.ButFirst); // on renvoie tout sauf le premier élément
  end
  else
    LWord(W.ButFirst(St)); // un mot : on renvoie tout sauf le premier caractère
end;

procedure ButLast;
// SAUFDERNIER
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St = EmptyStr) or (St = CEmptyList) then // une liste ou un mot vide ?
    Error := C_EmptyWordOrList // [Erreur : mot ou liste vide]
  else
  if (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // on l'analyse
    LList(L.ButLast); // on renvoie tout sauf le dernier élément
  end
  else
    LWord(W.ButLast(St)); // un mot : on renvoie tout sauf le dernier caractère
end;

procedure PTrue;
// *** VRAI ***
begin
  LNumber(IntToStr(CRTrue)); // on empile la valeur de VRAI
end;

procedure PFalse;
// *** FAUX ***
begin
  LNumber(IntToStr(CRFalse)); // on empile la valeur de FAUX
end;

procedure WriteAll;
// *** ECRIST ***
begin
  fLines.Add(fExeStack.Pop); // on affiche la valeur brute empilée
end;

procedure PutLast;
// *** METSDERNIER ***
var
  St: string;
begin
  St := fExeStack.Pop; // donné dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then  // liste ?
  begin
    L.Text := St; // analyse
    LList(L.PutLast(fExeStack.Pop)); // met en dernier
  end
  else
    LWord(W.PutLast(St, fExeStack.Pop)); // donnée à la fin du mot
end;

procedure PutFirst;
// *** METSPREMIER ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then  // liste ?
  begin
    L.Text := St; // analyse
    LList(L.PutFirst(fExeStack.Pop)); // met en premier
  end
  else
    LWord(W.PutFirst(St, fExeStack.Pop)); // donnée au début du mot
end;

Procedure Insert;
// *** INSERE ***
var
  St1, St2: string;
  N: Integer;
begin
  St1 := fExeStack.Pop; // donnée dépilée (emplacement)
  if TryStrToInt(St1, N) and (N >= 0) then // conversion en nombre ?
  begin
    if N > 0 then // emplacement positif ?
    begin
      St1 := fExeStack.Pop; // donnée à insérer
      St2 := fExeStack.Pop; // donnée où insérer
      if (St2 <> EmptyStr) and (St2[1] = CBeginList) then // une liste ?
      begin
        L.Text := St2; // analyse
        if (N <= L.Count + 1) then // insertion possible ?
          LList(L.InsertAItem(N, St1)) // on insère dans la liste
        else
        begin
          ActualItem := IntToStr(N); // stocke la place fautive
          Error := C_InsItem; // [Erreur: insertion impossible]
        end;
      end
      else
      begin
        if (N <= Length(St2) + 1) then // insertion dans mot possible ?
          LWord(W.Insert(St2, St1, N)) // on insère
        else
        begin
          ActualItem := IntToStr(N); // stocke la place fautive
          Error := C_InsItem;// [Erreur: insertion impossible]
        end;
      end;
    end
    else
    begin
      ActualItem := IntToStr(N); // stocke la place fautive
      Error := C_InsItem;// [Erreur: insertion impossible]
    end;
  end
  else
  begin
    ActualItem := St1; // stocke la place fautive
    Error := C_BadInt; // [Erreur: entier exigé]
  end;
end;

Procedure Reverse;
// *** INVERSE ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then  // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.ReverseItems); // inversion des éléments
  end
  else
    LWord(W.Reverse(St)); // inversion des lettres du mot
end;

procedure UpperCase;
// *** MAJUSCULES ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.Uppercase); // en majuscules
  end
  else
    LWord(W.Uppercase(St)); // mot en majuscules
end;

procedure LowerCase;
// *** MINUSCULES ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.Lowercase); // en minuscules
  end
  else
    LWord(W.Lowercase(St)); // mot en minuscules
end;

Procedure Shuffle;
// *** MELANGE ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.ShuffleItems); // mélange
  end
  else
    LWord(W.Shuffle(St)); // mot mélangé
end;

procedure Replace;
// *** REMPLACE ***
var
  St1, St2: string;
  N: Integer;
begin
  St1 := fExeStack.Pop; // donnée dépilée (emplacement)
  if TryStrToInt(St1, N) then // conversion en nombre ?
  begin
    if N > 0 then // positif ?
    begin
      St1 := fExeStack.Pop; // donnée à remplacer
      St2 := fExeStack.Pop; // donnée où remplacer
      if (St2 <> EmptyStr) and (St2[1] = CBeginList) then // une liste ?
      begin
        L.Text := St2; // analyse
        if (N <= L.Count) then // remplacement possible ?
          LList(L.ReplaceItem(N, St1)) // on remplace dans la liste
        else
        begin
          ActualItem := IntToStr(N); // stocke la place fautive
          Error := C_ReplaceItem; // [Erreur: remplacement impossible]
        end;
      end
      else
      begin
        if (N <= Length(St2)) then // remplacement dans mot possible ?
          LWord(W.Replace(St2, St1, N)) // on remplace
        else
        begin
          ActualItem := IntToStr(N); // stocke la place fautive
          Error := C_ReplaceItem;// [Erreur: remplacement impossible]
        end;
      end;
    end
    else
    begin
      ActualItem := IntToStr(N); // stocke la place fautive
      Error := C_ReplaceItem;// [Erreur: remplacement impossible]
    end;
  end
  else
  begin
    ActualItem := St1; // stocke la place fautive
    Error := C_BadInt; // [Erreur: entier exigé]
  end;
end;

procedure Sort;
// *** TRIE ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.SortItems); // trie
  end
  else
    LWord(W.Sort(St)); // lettres du mot trié
end;

procedure Rotate;
// *** ROTATION ***
var
  St: string;
begin
  St := fExeStack.Pop; // donnée dépilée
  if (St <> EmptyStr) and (St[1] = CBeginList) then // une liste ?
  begin
    L.Text := St; // analyse
    LList(L.Rotate); // rotation
  end
  else
    LWord(W.Rotate(St)); // rotation des lettres du mot
end;








