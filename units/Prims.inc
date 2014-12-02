procedure Last;
// DERNIER
begin
  St := fExeStack.Pop;
  if (St = EmptyStr) or (St = CEmptyList) then
    Error := C_NoListWord
  else
  if (St[1] = CBeginList) then
  begin
    L.Text := St;
    St := L.Last;
  end
  else
    St := W.Last(St);
  LWord(St);
end;

procedure First;
// PREMIER
begin
  St := fExeStack.Pop;
  if (St = EmptyStr) or (St = CEmptyList) then
    Error := C_NoListWord
  else
  if (St[1] = CBeginList) then
  begin
    L.Text := St;
    St := L.First;
  end
  else
    St := W.First(St);
  LWord(St);
end;

procedure ButFirst;
// SAUFPREMIER
begin
  St := fExeStack.Pop;
  if (St = EmptyStr) or (St = CEmptyList) then
    Error := C_NoListWord
  else
  if (St[1] = CBeginList) then
  begin
    L.Text := St;
    St := L.ButFirst;
  end
  else
    St := W.ButFirst(St);
  LWord(St);
end;

procedure ButLast;
// SAUFDERNIER
begin
  St := fExeStack.Pop;
  if (St = EmptyStr) or (St = CEmptyList) then
    Error := C_NoListWord
  else
  if (St[1] = CBeginList) then
  begin
    L.Text := St;
    St := L.ButLast;
  end
  else
    St := W.ButLast(St);
  LWord(St);
end;

procedure PTrue;
// VRAI
begin
  LNumber(IntToStr(CRTrue));
end;

procedure PFalse;
// FAUX
begin
  LNumber(IntToStr(CRFalse));
end;

procedure WriteAll;
// ECRIST
begin
  fLines.Add(fExeStack.Pop);
end;

procedure PutLast;
// METSDERNIER
var
  St: string;
begin
  St := fExeStack.Pop;
  if (St <> EmptyStr) and (St[1] = CBeginList) then
  begin
    L.Text := St;
    LList(L.PutLast(fExeStack.Pop));
  end
  else
    LWord(W.PutLast(St, fExeStack.Pop));
end;

procedure PutFirst;
// METSPREMIER
var
  St: string;
begin
  St := fExeStack.Pop;
  if (St <> EmptyStr) and (St[1] = CBeginList) then
  begin
    L.Text := St;
    LList(L.PutFirst(fExeStack.Pop));
  end
  else
    LWord(W.PutFirst(St, fExeStack.Pop));
end;

Procedure Insert;
// INSERE
begin
   // ### TODO ###
end;

Procedure Reverse;
// INVERSE
var
  St: string;
begin
  St := fExeStack.Pop;
  if (St <> EmptyStr) and (St[1] = CBeginList) then
  begin
    L.Text := St;
    LList(L.ReverseItems);
  end
  else
    LWord(W.Reverse(St));
end;

procedure UpperCase;
// MAJUSCULES
var
  St: string;
begin
  St := fExeStack.Pop;
  if (St <> EmptyStr) and (St[1] = CBeginList) then
  begin
    L.Text := St;
    LList(L.Uppercase);
  end
  else
    LWord(W.Uppercase(St));
end;

procedure LowerCase;
// MINUSCULES
var
  St: string;
begin
  St := fExeStack.Pop;
  if (St <> EmptyStr) and (St[1] = CBeginList) then
  begin
    L.Text := St;
    LList(L.Lowercase);
  end
  else
    LWord(W.Lowercase(St));
end;








