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





