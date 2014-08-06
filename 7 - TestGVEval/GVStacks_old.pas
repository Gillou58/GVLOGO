{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Structures de piles                     |
  |                  Unité : GVStacks.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    07-05-2014 22:23:20                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

{$I GVDefines.inc}
unit GVStacks;

// piles pour GVLOGO
//
// ##############################################################
//
// L'unité définit les piles nécessaires au fonctionnement de GVLOGO.
// Elle comprend des structures pour les entiers, pour des chaînes et
// pour des réels.
//
// A noter : les opérations sont toutes destructrices. Les données à conserver
// sont donc à dupliquer avant leur manipulation.
//

interface

uses Contnrs, GVUtils;

type
  // ************* TGVStack *************

  TGVStack = class(TStack)
  public
    // inversion au sommet de la pile
    procedure Swap;
    // rotation au sommet de la pile (3 éléments)
    procedure Rot;
    // sommet de la pile perdu
    procedure Drop;
    // nettoie la pile
    procedure Clear;
    // pile vide ?
    function IsEmpty: Boolean;
  published
    // liste de travail
    property List;
  end;

  // ************* TGVIntStack *************

  TGVIntStack = class(TGVStack)
    // duplication du sommet de la pile
    procedure Dup;
    // avant-dernier élément dupliqué au sommet de la pile
    procedure Over;
    // place au sommet
    procedure Push(const Value: Integer);
    // retire le sommet
    function Pop: Integer;
    // renvoie le sommet
    function Peek: Integer;
    // ajoute
    procedure IntSum;
    // soustrait
    procedure IntSub;
    // multiplie
    procedure IntMul;
    // divise
    procedure IntDiv;
    // reste de la division
    procedure IntMod;
    // reste et quotient de la division
    procedure IntModDiv;
    // valeur absolue
    procedure IntAbs;
    // opposé
    procedure IntNegate;
    // incrémente
    procedure IntInc;
    // décrémente
    procedure IntDec;
    // sommet = 0 ?
    procedure IntZero;
    // duplique si sommet = 0
    procedure ZeroDup;
    // "ou" logique
    procedure IntOr;
    // "et" logique
    procedure IntAnd;
    // "non" logique
    procedure IntNot;
    // "ou exclusif" logique
    procedure IntXor;
    // maximum
    procedure IntMax;
    // minimum
    procedure IntMin;
    // multiplie par 2
    procedure Int2Mul;
    // divise par 2
    procedure Int2Div;
    // inférieur ?
    procedure IntInf;
    // supérieur ?
    procedure IntSup;
    // égalité ?
    procedure IntEqual;
    // signe sur la pile
    procedure IntSign;
    // nombre au hasard
    procedure IntRandom;
    // nombre au hasard dans un intervalle
    procedure IntRandomRange;
  end;

  // ************* TGVStringStack *************

  TGVStringStack = class(TGVStack)
    // duplication du sommet de la pile
    procedure Dup;
    // avant-dernier élément dupliqué au sommet de la pile
    procedure Over;
    // place au sommet
    procedure Push(const Value: string);
    // retire le sommet
    function Pop: string;
    // renvoie le sommet
    function Peek: string;
  end;

  // ************* TGVRealStack *************

  PReal = ^Real;

  TGVRealStack = class(TGVStack)
    // duplication du sommet de la pile
    procedure Dup;
    // avant-dernier élément dupliqué au sommet de la pile
    procedure Over;
    // place au sommet
    procedure Push(const Value: Real);
    // retire le sommet
    function Pop: Real;
    // renvoie le sommet
    function Peek: Real;
    procedure DSum; // somme
    procedure DSub; // différence
    procedure DMul; // multiplication
    procedure DDiv; // division
    procedure DAbs; // valeur absolue
    procedure DCos; // cosinus
    procedure DSin; // sinus
    procedure DTan; // tangente
    procedure DSqrt; // racine carrée
    procedure DTrunc; // nombre tronqué
    procedure DRound; // nombre arrondi
    procedure DSqr; // nombre au carré
    procedure DExp; // exponentielle
    procedure DFrac; // partie fractionnelle
    procedure DInt; // partie entière
    procedure DLn; // log népérien
    procedure DLog2; // log base 2
    procedure DLog10; // log base 100
    procedure DCoTan; // cotangente
    procedure DHypot; // hypothénuse
    procedure DArcCos; // arc cosinus
    procedure DArcSin; // arc sinus
    procedure DPower; // puissance
    procedure DMinus; // nombre négatif sur la pile ?
    procedure DPlus; // nombre positif sur la pile ?
    procedure DNegate; // signe inversé
    procedure DEqual; // égalité sur pile
    procedure DInf; // infériorité sur pile
    procedure DSup; // supériorité sur pile
    procedure DMax; // maximum
    procedure DMin; // minimum
    procedure DPi; // PI sur la pile
    procedure DSign; // signe
    procedure DRandom; // nombre au hasard
  end;

implementation

uses Math;

{ TGVStack }

procedure TGVStack.Clear;
// nettoie la pile
begin
  while Count > 0 do
    Pop;
end;

(* ********************************************************************* *)

procedure TGVStack.Drop;
// sommet de la pile perdu
begin
  Pop;
end;

(* ********************************************************************* *)

function TGVStack.IsEmpty: Boolean;
// pile vide ?
begin
  Result := (Count = 0);
end;

(* ********************************************************************* *)

procedure TGVStack.Rot;
// rotation au sommet de la pile (3 éléments)
var
  T1, T2, T3: Pointer;
begin
  if AtLeast(3) then
  begin
    T1 := Pop;
    T2 := Pop;
    T3 := Pop;
    Push(T1);
    Push(T3);
    Push(T2);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVStack.Swap;
// inversion au sommet de la pile
var
  T1, T2: Pointer;
begin
  if AtLeast(2) then
  begin
    T1 := Pop;
    T2 := Pop;
    Push(T1);
    Push(T2);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

{ TGVIntStack }

procedure TGVIntStack.Dup;
// duplication du sommet
begin
  Push(Peek);
end;

(* ********************************************************************* *)

procedure TGVIntStack.Int2Div;
// divise par 2
begin
  Push(Pop shr 1);
end;

(* ********************************************************************* *)

procedure TGVIntStack.Int2Mul;
// multiplie par 2
begin
  Push(Pop shl 1);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntAbs;
// valeur absolue
begin
  Push(Abs(Pop));
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntSum;
// addition
begin
  if AtLeast(2) then
    Push(Pop + Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntSup;
// supériorité du sommet
begin
  if AtLeast(2) then
  begin
    Swap;
    if Pop > Pop then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntAnd;
// "et" logique
begin
  if AtLeast(2) then
    Push(Pop and Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntXor;
// "ou exclusif" logique
begin
  if AtLeast(2) then
    Push(Pop xor Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntDec;
// décrémente le sommet de la pile
begin
  if AtLeast(1) then
    Dec(Integer(inherited Peek^))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntZero;
// sommet nul ?
begin
  if Pop = 0 then
    Push(Ord(True))
  else
    Push(Ord(False));
end;

(* ********************************************************************* *)

procedure TGVIntStack.Over;
// avant-dernier élément dupliqué au sommet de la pile
var
  I: Integer;
begin
  if AtLeast(2) then
  begin
    I := Pop;
    Dup;
    Push(I);
    Swap;
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntDiv;
// division entière
begin
  if AtLeast(2) then
  begin
    if Peek <> 0 then
    begin
      Swap;
      Push(Pop div Pop);
    end
    else
      raise EGVStackException.Create(ME_C_ZeroDiv);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntEqual;
// égalité du sommet
begin
  if AtLeast(2) then
  begin
    if Pop = Pop then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntInc;
// incrémente le sommet de la pile
begin
  if AtLeast(1) then
    Inc(Integer(inherited Peek^))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntInf;
// infériorité du sommet
begin
  if AtLeast(2) then
  begin
    Swap;
    if Pop < Pop then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntMax;
// garde le plus grand sur la pile
begin
  if AtLeast(2) then
    Push(Max(Pop, Pop))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntMin;
// garde le plus petit sur la pile
begin
  if AtLeast(2) then
    Push(Min(Pop, Pop))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntMod;
// reste de division
begin
  if AtLeast(2) then
  begin
    if Peek <> 0 then
    begin
      Swap;
      Push(Pop mod Pop);
    end
    else
      raise EGVStackException.Create(ME_C_ZeroDiv);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntModDiv;
// reste et quotient
begin
  if AtLeast(2) then
  begin
    if Peek <> 0 then
    begin
      Over;
      Over;
      IntMod;
      Rot;
      IntDiv;
    end
    else
      raise EGVStackException.Create(ME_C_ZeroDiv);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntMul;
// multiplication
begin
  if AtLeast(2) then
    Push(Pop * Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntNegate;
// inverse le signe du sommet de la pile
begin
  Push(-Pop);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntNot;
// "non" logique
begin
  Push(not Pop);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntOr;
// "ou" logique
begin
  if AtLeast(2) then
    Push(Pop or Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntRandom;
// nombre au hasard
begin
  Push(Random(Pop));
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntRandomRange;
// nombre au hard dans un intervalle
begin
  if AtLeast(2) then
  begin
    Swap;
    Push(RandomRange(Pop, Pop));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntSign;
// signe sur la pile ( -1, 0 ou +1)
begin
  Push(Sign(Pop));
end;

(* ********************************************************************* *)

procedure TGVIntStack.IntSub;
// soustraction
begin
  if AtLeast(2) then
  begin
    Swap;
    Push(Pop - Pop);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVIntStack.Peek: Integer;
// renvoie le sommet de la pile
begin
{$IFNDEF Delphi}
  Result := 0;
{$ENDIF}
  if AtLeast(1) then
    Result := Integer(inherited Peek^)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVIntStack.Pop: Integer;
// retire un entier de la pile
var
  PInt: PInteger;
begin
{$IFNDEF Delphi}
  Result := 0;
{$ENDIF}
  if AtLeast(1) then
  begin
    Result := Integer(inherited Peek^);
    PInt := inherited Pop;
    Dispose(PInt);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVIntStack.Push(const Value: Integer);
// place un entier sur la pile
var
  PInt: PInteger;
begin
  New(PInt);
  PInt^ := Value;
  inherited Push(PInt);
end;

(* ********************************************************************* *)

procedure TGVIntStack.ZeroDup;
// duplique le sommet si 0
begin
  if Peek = 0 then
    Dup;
end;

(* ********************************************************************* *)

{ TGVStringStack }

procedure TGVStringStack.Dup;
// duplication de la chaîne au sommet
begin
  Push(Peek);
end;

(* ********************************************************************* *)

procedure TGVStringStack.Over;
// avant-dernier élément dupliqué au sommet de la pile
var
  St: string;
begin
  if AtLeast(2) then
  begin
    St := Pop;
    Dup;
    Push(St);
    Swap;
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVStringStack.Peek: string;
// renvoie le sommet de la pile
begin
  if AtLeast(1) then
    Result := string(inherited Peek^)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVStringStack.Pop: string;
// retire un entier de la pile
var
  PStr: PString;
begin
  if AtLeast(1) then
  begin
    Result := string(inherited Peek^);
    PStr := inherited Pop;
    Dispose(PStr);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVStringStack.Push(const Value: string);
// place une chaîne sur la pile
var
  PStr: PString;
begin
  New(PStr);
  PStr^ := Value;
  inherited Push(PStr);
end;

(* ********************************************************************* *)

{ TGVRealStack }

procedure TGVRealStack.DAbs;
// valeur absolue
begin
  Push(Abs(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DArcCos;
// ArcCos
begin
  Push(ArcCos(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DArcSin;
// ArcSin
begin
  Push(ArcSin(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DCos;
// Cosinus
begin
  Push(Cos(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DCoTan;
// Cotangente
begin
  Push(Cotan(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSub;
// différence
begin
  if AtLeast(2) then
  begin
    Swap;
    Push(Pop - Pop);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DDiv;
// division
begin
  if AtLeast(2) then
  begin
    if not IsZero(Peek) then
    begin
      Swap;
      Push(Pop / Pop);
    end
    else
      raise EGVStackException.Create(ME_C_ZeroDiv);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.Dup;
// duplication du réel au sommet
begin
  Push(Peek);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DEqual;
// égalité ?
begin
  if AtLeast(2) then
  begin
    if SameValue(Pop, Pop) then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DExp;
// exponentielle
begin
  Push(Exp(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DFrac;
// partie fractionnelle
begin
  Push(Frac(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DHypot;
// hypothénuse
begin
  if AtLeast(2) then
  begin
    Swap;
    Push(Hypot(Pop, Pop));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DInf;
// infériorité stricte sur la pile
begin
  if AtLeast(2) then
  begin
    Swap;
    if Pop < Pop then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DInt;
// partie entière
begin
  Push(Int(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DLn;
// log népérien
begin
  Push(Ln(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DLog10;
// log base 10
begin
  Push(Log10(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DLog2;
// log base 2
begin
  Push(Log2(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DMax;
// maximum sur la pile
begin
  if AtLeast(2) then
    Push(Max(Pop, Pop))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DMin;
// minimum sur la pile
begin
  if AtLeast(2) then
    Push(Min(Pop, Pop))
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DMinus;
// nombre négatif sur la pile ?
begin
  if AtLeast(2) then
  begin
    if Pop < 0 then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DMul;
// multiplication
begin
  if AtLeast(2) then
    Push(Pop * Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DNegate;
// opposé
begin
  Push(-Pop);
end;

(* ********************************************************************* *)

procedure TGVRealStack.Over;
// avant-dernier élément dupliqué au sommet de la pile
var
  Num: Real;
begin
  if AtLeast(2) then
  begin
    Num := Pop;
    Dup;
    Push(Num);
    Swap;
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVRealStack.Peek: Real;
// renvoie le sommet de la pile
begin
  if AtLeast(1) then
    Result := Real(inherited Peek^)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DPi;
// nombre PI
begin
  Push(Pi);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DPlus;
// nombre positif sur la pile
begin
  if AtLeast(2) then
  begin
    if Pop >= 0 then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSum;
// addition
begin
  if AtLeast(2) then
    Push(Pop + Pop)
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

function TGVRealStack.Pop: Real;
// retire un réel de la pile
var
  Num: PReal;
begin
  if AtLeast(1) then
  begin
    Result := Real(inherited Peek^);
    Num := inherited Pop;
    Dispose(Num);
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DPower;
// puissance
begin
  if AtLeast(2) then
  begin
    Swap;
    Push(Power(Pop, Pop));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.Push(const Value: Real);
// place un réel sur la pile
var
  Num: PReal;
begin
  New(Num);
  Num^ := Value;
  inherited Push(Num);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DRandom;
// nombre au hasard
begin
  Push(Random(Round(Pop)));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DRound;
// arrondi
begin
  Push(Round(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSign;
// signe sur la pile (-1, 0, +1)
begin
  Push(Sign(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSin;
// sinus
begin
  Push(Sin(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSqr;
// au carré
begin
  Push(Sqr(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSqrt;
// racine carrée
begin
  Push(Sqrt(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DSup;
// supériorité stricte sur la pile
begin
  if AtLeast(2) then
  begin
    Swap;
    if Pop > Pop then
      Push(Ord(True))
    else
      Push(Ord(False));
  end
  else
    raise EGVStackException.Create(ME_C_LowStack);
end;

(* ********************************************************************* *)

procedure TGVRealStack.DTan;
// tangente
begin
  Push(Tan(Pop));
end;

(* ********************************************************************* *)

procedure TGVRealStack.DTrunc;
// réel tronqué
begin
  Push(Trunc(Pop));
end;

end.
