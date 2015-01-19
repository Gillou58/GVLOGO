{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Evaluation d'une expression             |
  |                  Unité : GVEval.pas                                    |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVEVAL - part of GVLOGO
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

unit GVEval;
// Evaluation d'une expression mathématique
//
// L'unité accepte les variables (nom précédé de ":" et traitement extérieur)
// et un ensemble de fonctions mathématiques.
//

interface

uses
  SysUtils, Classes,
  GVConsts, // constantes communes
  GVStacks, // les piles
  GVErrConsts, // constantes des erreurs
  GVPrimConsts, // constantes des primitives
  GVErrors, // les erreurs
  GVLocVars, // variables locales
  GVKernel; // noyau (variables)

type
  // tableau des éléments
  TGVItems = array of TGVBaseItem;

  // *** TGVEvalEnumerator
  TGVEvalEnumerator = class(TObject) // énumération
  private
    fLst: TGVItems;
    fIndex: Integer;
  protected
    function GetCurrent: TGVBaseItem; virtual; // recherche de l'élément courant
  public
    constructor Create(const AValue: TGVItems); // création
    function MoveNext: Boolean; // recherche de l'élément suivant
    property Current: TGVBaseItem read GetCurrent;  // élément courant
  end;

  // *** TGVEval ***

  { TGVEval }

  TGVEval = class(TObject)
  private
    fEvalState: TGVEvalState; // état de l'évaluation
    fKernel: TGVLogoKernel; // variables dans le noyau
    fLocVar: TGVLocVars;// variables locales
    fOnChange: TNotifyEvent; // gestionnaire de changements
    fOnStateChange: TNotifyEvent; // notification de changement d'état
    fError: TGVErrors; // gestionnaire des erreurs
    fResult: Double; // résultat de l'évaluation
    fText: string; // texte à analyser
    fActualItem: string; // élément en cours
    fIndx: Integer; // index dans la lecture
    fStartIndx: Integer; // index de départ dans la chaîne de travail
    fItemList: TGVItems; // éléments de la chaîne de travail
    fScan: TGVItems; // résultat du scan
    fScanStack: TGVEvalStack; // pile pour l'évaluation
    fDStack: TGVDoubleStack; // pile pour les calculs
    function GetCount: Integer; // nombre d'éléments
    function GetItem(N: Integer): TGVBaseItem; // accès aux éléments
    function GetScanCount: Integer; // nombre d'éléments scannés
    function GetScanItem(N: Integer): TGVBaseItem; // élément de scan
    procedure SetStartIndx(AValue: Integer); // index de départ fixé
    procedure SetState(AValue: TGVEvalState); // état fixé
    procedure SetText(const AValue: string); // texte en cours fixé
    procedure GetDelimGreater; // plus grand ou >=
    procedure GetDelimLower; // plus petit ou <=
    procedure GetDelimNot; // non ou !=
    procedure WipeItems; inline; // nettoyage du tableau des éléments
    procedure WipeScan; inline; // nettoyage du scan
    // cherche une fonction
    function WhichFunction(const St: string): TGVFunctions;
    // *** fonctions intégrées ***
    procedure DoAbs; // valeur absolue
    procedure DoCos; // cosinus
    procedure DoSin; // sinus
    procedure DoTan; // tangente
    procedure DoSqrt; // racine carrée
    procedure DoTrunc;  // nombre tronqué
    procedure DoRound;  // nombre arrondi
    procedure DoSqr; // nombre au carré
    procedure DoExp; // exponentielle
    procedure DoFrac; // partie fractionnelle
    procedure DoInt; // partie entière
    procedure DoLn; // log népérien
    procedure DoLog2; // log base 2
    procedure DoLog10; // log base 1à
    procedure DoCoTan; // cotangente
    procedure DoArcCos; // arc cosinus
    procedure DoArcSin; // arc sinus
    procedure DoMinus;// négatif ?
    procedure DoPlus; // positif ?
    procedure DoNegate; // signe inversé
    procedure DoSign; // signe
    procedure DoRandom; // entier au hasard
    procedure DoNumber(const St: string); // traite un nombre
    procedure DoAdd; // addition
    procedure DoSub; // soustracton
    procedure DoMul; // multiplication
    procedure DoDiv; // division
    procedure DoPower; // puissance
    procedure DoGreater; // plus grand
    procedure DoLower; // plus petit
    procedure DoGreaterOrEqual; // plus grand ou égal
    procedure DoLowerOrEqual; // plus petit ou égal
    procedure DoEqual; // égalité
    procedure DoNotEqual; // inégalité
    procedure DoMod; // mod
    procedure DoNot; // non
    procedure DoOr; // ou
    procedure DoAnd; // et
    procedure DoUnaryMinus; // moins unaire
    procedure DoUnaryPlus; // plus unaire
  protected
    // ajoute un élément au tableau des éléments
    procedure AddItem(const AItem: string; AKind: CTokensEnum); virtual;
    // ajoute un élément à la liste de scan
    procedure AddScan(const AItem: TGVBaseItem); virtual;
    procedure GetVar; virtual; // traitement des variables
    procedure GetFunction; virtual; // traitement des fonctions
    procedure GetNumber; virtual; // traitement des nombres
    procedure Change; // notification de changement
    procedure StateChange; // notification de changement de l'état
    procedure Tokenize; // répartition en éléments
    procedure DoScan; // on analyse les  éléments
    procedure DoEvaluate; // on évalue
  public
    constructor Create; overload; // constructeur simple
    // constructeur avec initialisation
    constructor Create(const AText: string); overload;
    destructor Destroy; override; // destructeur
    procedure Clear; // nettoyage
    function GetEnumerator: TGVEvalEnumerator; // énumération
    procedure Scan; // étudie la chaîne entrée
    function Association(AValue: CTokensEnum): Integer; // associativité
    function Precedence(AValue: CTokensEnum): Integer; // priorité
    property Text: string read fText write SetText; // expression à analyser
    property ActualItem: string read fActualItem; // élément en cours
    property Res: Double read fResult; // résultat de l'évaluation
    // index de départ
    property StartIndx: Integer read fStartIndx write SetStartIndx default 1;
    property Indx: Integer read fIndx default -1; // index en cours dans la chaîne
    property Count: Integer read GetCount; // nombre d'éléments
    property ScanCount: Integer read GetScanCount; // nombre d'éléments après scan
    property Item[N: Integer]: TGVBaseItem read GetItem; default; // liste des éléments
    property ScanItem[N: Integer]: TGVBaseItem read GetScanItem; // liste pour évaluation
    // état de l'évaluateur
    property State: TGVEvalState read fEvalState write SetState default esNoInit;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
    // événement lié à la recherche d'une variable globale
    property Kernel: TGVLogoKernel read fKernel write fKernel;
    // événement lié à la recherche d'une variable locale
    property LocVars: TGVLocVars read fLocVar write fLocVar;
    // événement lié à un changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    // événement lié à un changement
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
  end;


implementation

uses Math;

{ TGVEval }

procedure TGVEval.SetText(const AValue: string);
// *** fixe l'expression à analyser ***
begin
  if (fText = AValue) or (not (State in [esWaiting, esNoInit])) then
    Exit; // sortie si aucun changement ou évaluateur au travail
  fText := AValue; // nouvelle valeur
  State := esWaiting; // état d'attente
  WipeItems; // on nettoie l'évaluateur
end;

procedure TGVEval.WipeItems;
// *** nettoyage du tableau des éléments ***
begin
  SetLength(fItemList, 0);
  fActualItem := EmptyStr; // on nettoie l'élément en cours
  fDStack.Clear; // nettoyage de la pile de calcul
  fScanStack.Clear; // pile de scan nettoyée
  Error.Clear; // gestionnaire d'erreurs nettoyé
end;

procedure TGVEval.WipeScan;
// *** nettoyage du scan ***
begin
  SetLength(fScan, 0); // nettoyage du tableau de sortie
  fActualItem := EmptyStr; // on nettoie l'élément en cours
  fScanStack.Clear; // nettoyage de la pile
end;

function TGVEval.WhichFunction(const St: string): TGVFunctions;
// *** recherche d'une fonction ***
var
  Li: TGVFunctions;
begin
  Result := C_Unknown; // valeur si non trouvée
  for Li := Low(TGVFunctions) to High(TGVFunctions) do // on balaie les fonctions
 begin
   if AnsiSameText(St, GVFunctionName[Li]) then // égalité des chaînes ?
   begin
     Result := Li; // sauvegarde de l'indice
     Break; // on sort de la boucle
   end;
 end;
end;

procedure TGVEval.DoAbs;
// *** valeur absolue ***
begin
  fDStack.Push(Abs(fDStack.Pop));
end;

procedure TGVEval.DoCos;
// *** cosinus ***
begin
  fDStack.Push(Cos(DegToRad(fDStack.Pop)));
end;

procedure TGVEval.DoSin;
// *** sinus ***
begin
  fDStack.Push(Sin(DegToRad(fDStack.Pop)));
end;

procedure TGVEval.DoTan;
// *** tangente ***
var
  LDbl: Double;
begin
  LDbl := DegToRad(fDStack.Pop);
  if not IsZero(Cos(LDbl)) then // pas de cosinus nul
    fDStack.Push(Tan(LDbl))
  else
    // [### Erreur: tangente inexistante ###]
    Error.SetError(CE_Tan, Text);
end;

procedure TGVEval.DoSqrt;
// *** racine carrée ***
begin
  if (fDStack.Peek >= 0) then // pas de nombre négatif
    fDStack.Push(Sqrt(fDStack.Pop))
  else
    // [### Erreur: nombre négatif interdit ###]
    Error.SetError(CE_NoNegNumber, Text);
end;

procedure TGVEval.DoTrunc;
// *** nombre tronqué ***
begin
  fDStack.Push(Trunc(fDStack.Pop));
end;

procedure TGVEval.DoRound;
// *** nombre arrondi ***
begin
  fDStack.Push(Round(fDStack.Pop));
end;

procedure TGVEval.DoSqr;
// *** nombre au carré ***
begin
  fDStack.Push(fDStack.Peek * fDStack.Pop);
end;

procedure TGVEval.DoExp;
// *** exponentielle ***
begin
  fDStack.Push(Exp(fDStack.Pop));
end;

procedure TGVEval.DoFrac;
// *** partie fractionnelle ***
begin
  fDStack.Push(Frac(fDStack.Pop));
end;

procedure TGVEval.DoInt;
// *** partie entière ***
begin
  fDStack.Push(Int(fDStack.Pop));
end;

procedure TGVEval.DoLn;
// *** log népérien ***
begin
if (fDStack.Peek > 0) then // pas de nombre négatif ou nul
  fDStack.Push(Ln(fDStack.Pop))
else
  // [### Erreur: log non défini ###]
  Error.SetError(CE_Ln, Text);
end;

procedure TGVEval.DoLog2;
/// *** log base 2 ***
begin
  if (fDStack.Peek > 0) then // pas de nombre négatif ou nul
    fDStack.Push(Log2(fDStack.Pop))
  else
    // [### Erreur: log non défini ###]
    Error.SetError(CE_Ln, Text);
end;

procedure TGVEval.DoLog10;
// *** log base 10 ***
begin
  if (fDStack.Peek > 0) then // pas de nombre négatif ou nul
    fDStack.Push(Log10(fDStack.Pop))
  else
    // [### Erreur: log non défini ###]
    Error.SetError(CE_Ln, Text);
end;

procedure TGVEval.DoCoTan;
// *** cotangente ***
var
  LDbl: Double;
begin
  LDbl := DegToRad(fDStack.Pop);
  if not IsZero(Sin(LDbl)) then // pas de sinus nul
    fDStack.Push(Tan(LDbl))
  else
    // [### Erreur: cotangente non définie ###]
    Error.SetError(CE_CoTan, Text);
end;

procedure TGVEval.DoArcCos;
// *** arc cosinus ***
var
  LDbl: Double;
begin
 LDbl := fDStack.Pop;
 if (LDbl >= -1.0) and (LDbl <= 1) then // compris entre -1 et 1 ?
   fDStack.Push(RadToDeg(ArcCos(LDbl)))
 else
   // [### Erreur: arc cosinus non défini ###]
   Error.SetError(CE_Arc, Text);
end;

procedure TGVEval.DoArcSin;
// *** arc sinus ***
var
  LDbl: Double;
begin
 LDbl := fDStack.Pop;
 if (LDbl >= -1.0) and (LDbl <= 1) then // compris entre -1 et 1 ?
   fDStack.Push(RadToDeg(ArcSin(LDbl)))
 else
   // [### Erreur: arc sinus non défini ###]
   Error.SetError(CE_Arc, Text);
end;

procedure TGVEval.DoMinus;
// *** négatif ? ***
begin
 if (fDStack.Pop < 0) then
   fDStack.Push(CRTrue)
 else
   fDStack.Push(CRFalse);
end;

procedure TGVEval.DoPlus;
// *** positif ? ***
begin
if (fDStack.Pop >= 0) then
  fDStack.Push(CRTrue)
else
  fDStack.Push(CRFalse);
end;

procedure TGVEval.DoNegate;
// *** signe inversé ***
begin
  fDStack.Push(-fDStack.Pop);
end;

procedure TGVEval.DoSign;
// *** signe ***
begin
  fDStack.Push(Sign(fDStack.Pop));
end;

procedure TGVEval.DoRandom;
// *** nombre entier au hasard ***
var
  LDbl: Double;
begin
  LDbl := fDStack.Pop;
  if (Trunc(LDbl) = LDbl) then  // un entier ?
    fDStack.Push(Random(Trunc(LDbl)))
  else
    // [### Erreur: entier exigé ###]
    Error.SetError(CE_NeedsInteger, FloatToStr(LDbl));
end;

procedure TGVEval.DoNumber(const St: string);
// *** traite un nombre ***
var
  LDbl: Double;
begin
  if TryStrToFloat(St, LDbl) then // correct ?
    fDStack.Push(LDbl) // on l'empile
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, St);
end;

procedure TGVEval.DoAdd;
// *** addition ***
begin
  if fDStack.Needed(2) then // deux éléments sur la pile ?
    fDStack.Push(fDStack.Pop + fDStack.Pop)
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CPLus);
end;

procedure TGVEval.DoSub;
// *** soustraction ***
begin
  if fDStack.Needed(2) then // deux éléments sur la pile ?
  begin
    fDStack.Swap; // inversion sur la pile
    fDStack.Push(fDStack.Pop - fDStack.Pop);
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CMinus);
end;

procedure TGVEval.DoMul;
// *** multiplication ***
begin
  if fDStack.Needed(2) then // deux éléments sur la pile ?
    fDStack.Push(fDStack.Pop * fDStack.Pop)
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CMul);
end;

procedure TGVEval.DoDiv;
// *** division ***
begin
  if fDStack.Needed(2) then // deux éléments sur la pile ?
  begin
    if fDStack.Peek <> 0 then // division par zéro ?
    begin
      fDStack.Swap; // inversion sur la pile
      fDStack.Push(fDStack.Pop / fDStack.Pop);
    end
    else
      // [### Erreur: division par zéro ###]
      Error.SetError(CE_ZeroDiv, Text);
   end
   else
     // [### Erreur: pas assez d'arguments ###]
     Error.SetError(CE_NoArg, CDiv);
end;

procedure TGVEval.DoPower;
// *** puissance ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    fDSTack.Swap;
    fDSTack.Push(Power(fDSTack.Pop, fDSTack.Pop));
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CPower);
end;

procedure TGVEval.DoGreater;
// *** > ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    if fDSTack.Pop <= fDSTack.Pop then
      fDSTack.Push(CRTrue)
    else
      fDSTack.Push(CrFalse);
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CGreater);
end;

procedure TGVEval.DoLower;
// *** < ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    if fDSTack.Pop >= fDSTack.Pop then
      fDSTack.Push(CRTrue)
    else
      fDSTack.Push(CrFalse);
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CLower);
end;

procedure TGVEval.DoGreaterOrEqual;
// *** >= ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
    begin
      if fDSTack.Pop < fDSTack.Pop then
        fDSTack.Push(CRTrue)
      else
        fDSTack.Push(CrFalse);
    end
    else
      // [### Erreur: pas assez d'arguments ###]
      Error.SetError(CE_NoArg, CGreaterOrEqual);
end;

procedure TGVEval.DoLowerOrEqual;
// *** <= ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
    begin
      if fDSTack.Pop > fDSTack.Pop then
        fDSTack.Push(CRTrue)
      else
        fDSTack.Push(CrFalse);
    end
    else
      // [### Erreur: pas assez d'arguments ###]
      Error.SetError(CE_NoArg, CLowerOrEqual);
end;

procedure TGVEval.DoEqual;
// *** = ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
    begin
      if fDSTack.Pop = fDSTack.Pop then
        fDSTack.Push(CRTrue)
      else
        fDSTack.Push(CrFalse);
    end
    else
      // [### Erreur: pas assez d'arguments ###]
      Error.SetError(CE_NoArg, CEqual);
end;

procedure TGVEval.DoNotEqual;
// *** != ou <> ***
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
    begin
      if fDSTack.Pop <> fDSTack.Pop then
        fDSTack.Push(CRTrue)
      else
        fDSTack.Push(CrFalse);
    end
    else
      // [### Erreur: pas assez d'arguments ###]
      Error.SetError(CE_NoArg, CNotEqual);
end;

procedure TGVEval.DoMod;
// *** mod ***
var
  LDbl1, LDBl2: Double;
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    LDbl1 := fDSTack.Pop;
    LDbl2 := fDSTack.Pop;
    // on teste les deux nombres
    if (Trunc(LDbl2) <> LDbl2) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl2))
    else
    if (Trunc(LDbl1) <> LDbl1) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl1))
    else
      fDSTack.Push(Trunc(LDbl2) mod Trunc(LDbl1))
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, MF_Mod);
end;

procedure TGVEval.DoNot;
// *** non ***
var
  LDbl: Double;
begin
  if fDSTack.Needed(1) then // un élément sur la pile ?
  begin
    LDbl := fDSTack.Pop;
    if (Trunc(LDbl) = LDbl) then // un entier ?
      fDSTack.Push(not Trunc(LDbl))
    else
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl));
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, MF_Not);
end;

procedure TGVEval.DoOr;
var
  LDbl1, LDBl2: Double;
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    LDbl2 := fDSTack.Pop;
    LDbl1 := fDSTack.Pop;
    // on teste les deux nombres
    if (Trunc(LDbl1) <> LDbl1) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl1))
    else
    if (Trunc(LDbl2) <> LDbl2) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl2))
    else
      fDSTack.Push(Trunc(LDbl2) or Trunc(LDbl1))
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, MF_Or);
end;

procedure TGVEval.DoAnd;
var
  LDbl1, LDBl2: Double;
begin
  if fDSTack.Needed(2) then // deux éléments sur la pile ?
  begin
    LDbl2 := fDSTack.Pop;
    LDbl1 := fDSTack.Pop;
    // on teste les deux nombres
    if (Trunc(LDbl1) <> LDbl1) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl1))
    else
    if (Trunc(LDbl2) <> LDbl2) then
      // [### Erreur: entier exigé ###]
      Error.SetError(CE_NeedsInteger, FloatToStr(LDbl1))
    else
      fDSTack.Push(Trunc(LDbl2) and Trunc(LDbl1))
  end
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, MF_And);
end;

procedure TGVEval.DoUnaryMinus;
// *** moins unaire ***
begin
  if fDSTack.Needed(1) then // un élément sur la pile ?
    fDSTack.Push(-fDSTack.Pop)
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CMinus);
end;

procedure TGVEval.DoUnaryPlus;
// *** plus unaire ***
begin
  if fDSTack.Needed(1) then // un élément sur la pile ?
    fDSTack.Pop
  else
    // [### Erreur: pas assez d'arguments ###]
    Error.SetError(CE_NoArg, CPlus);
end;

function TGVEval.Association(AValue: CTokensEnum): Integer;
// *** associativité d'un opérateur ***
begin
  Result := CTokenAssociation[AValue];
end;

function TGVEval.Precedence(AValue: CTokensEnum): Integer;
// *** priorité d'un opérateur ***
begin
  Result := CTokenPrecedence[AValue];
end;

procedure TGVEval.AddItem(const AItem: string; AKind: CTokensEnum);
// *** ajoute un élément ***
begin
  SetLength(fItemList, Length(fItemList) + 1); // adapte la longueur du tableau
  with fItemList[Length(fItemList) - 1] do
  begin
    Token := AItem; // valeur de l'élément
    Kind := AKind; // catégorie de l'élément
  end;
  fActualItem := AItem; // élément en cours
  Change; // changement notifié
  Inc(fIndx); // caractère suivant
end;

procedure TGVEval.AddScan(const AItem: TGVBaseItem);
// *** ajoute un élément à la liste de scan ***
begin
  SetLength(fScan, Length(fScan) + 1); // adapte la longueur du tableau
  fScan[Length(fScan) - 1] := AItem; // affecte le nouvel élément
  fActualItem := AItem.Token; // élément en cours
  Change; // notification de changement
end;

procedure TGVEval.SetStartIndx(AValue: Integer);
// *** fixe l'index de départ dans l'expression ***
begin
  if fStartIndx = AValue then
    Exit; // on sort si aucun changement
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    // [### Erreur: hors bornes ###]
    Error.SetError(CIE_OutOfRange, Text, AValue);
  fStartIndx := AValue; // nouvelle valeur de l'index
end;

procedure TGVEval.SetState(AValue: TGVEvalState);
// *** état de l'évaluateur ***
begin
  if fEvalState = AValue then
    Exit; // on sort si aucun changement
  fEvalState := AValue; // nouvelle valeur
  StateChange; // notification de changement
end;

function TGVEval.GetCount: Integer;
// *** nombre d'éléments ***
begin
  Result := Length(fItemList);
end;

function TGVEval.GetItem(N: Integer): TGVBaseItem;
// *** renvoie d'un élément ***
begin
  if (N < 1) or (N > Count) then // hors limites ?
    // [### Erreur: hors bornes ###]
    Error.SetError(CIE_OutOfRange, Text, N); // erreur
  Result := fItemList[N-1]; // base 1
end;

function TGVEval.GetScanCount: Integer;
// *** éléments après scan ***
begin
  Result := Length(fScan);
end;

function TGVEval.GetScanItem(N: Integer): TGVBaseItem;
// *** élément après scan pour évaluation ***
begin
  if (N < 1) or (N > ScanCount) then // hors limites ?
    // [### Erreur: hors bornes ###]
    Error.SetError(CIE_OutOfRange, Text, N); // erreur
  Result := fScan[N-1]; // base 1
end;

constructor TGVEval.Create;
// *** constructeur simple ***
begin
  inherited Create; // on hérite
  fScanStack := TGVEvalStack.Create; // pile de scan
  fDStack := TGVDoubleStack.Create; // pile pour les calculs
  fError := TGVErrors.Create; // gestionnaire d'erreurs
  randomize; // nombres pseudo-aléatoires
  OnChange := nil; // gestionnaires non affectés
  OnStateChange := nil;
  Clear; // nettoyage
end;

constructor TGVEval.Create(const AText: string);
// *** constructeur avec initialisation ***
begin
  Create; // appel du constructeur simple
  Text := AText; // initialisation du texte de travail
  Clear; // on nettoie
end;

destructor TGVEval.Destroy;
// *** destructeur ***
begin
  fScanStack.Free; // pile de scan libérée
  fDStack.Free; // pile de calcul libérée
  Error.Free; // gestionnaire d'erreurs libéré
  OnChange := nil;
  Kernel := nil; // noyau déconnecté
  OnStateChange := nil;
  inherited Destroy; // on hérite
end;

procedure TGVEval.Clear;
// *** nettoyage de l'évaluateur ***
begin
  fEvalState := esNoInit; // état de l'évaluation (non initialisée)
  fStartIndx := 1; // index par défaut de départ
  fIndx := -1; // index de travail
  fText := EmptyStr; // chaîne de travail vide
  fResult := 0; // résultat par défaut
  Error.Clear; // pas d'erreur
  WipeItems; // tableau vide
end;

function TGVEval.GetEnumerator: TGVEvalEnumerator;
// *** énumération des éléments ***
begin
  Result := TGVEvalEnumerator.Create(fItemList);
end;

procedure TGVEval.Scan;
// *** analyse de la chaîne entrée ***
begin
  if (State = esNoInit) then // erreur si rien à évaluer
  begin
    // [### Erreur: non initialisation ###]
    Error.SetError(CIE_NoInit, CE_GVLOGO);
    Exit; // on sort
  end;
  try
    Change; // notification de changement
    Tokenize; // répartition en éléments
    if Error.OK then // pas d'erreur ?
      DoScan; // on analyse
    if Error.Ok then // toujours pas d'erreur ?
      DoEvaluate; // on évalue
  finally
    State := esWaiting; // état d'attente
  end;
end;

procedure TGVEval.Tokenize;
// *** répartit en éléments ***
var
  LCh: Char;
begin
  State := esTokenizing; // état mis à jour
  WipeItems; // liste interne nettoyée
  fIndx := StartIndx; // départ initialisé
  // on balaie l'expression tant qu'il n'y a pas d'erreur
  while Error.Ok and (fIndx <= Length(Text)) do
  begin
   LCh := fText[fIndx]; // caractère en cours
   case LCh of
     CBlank: Inc(fIndx); // on ignore les blancs
     '0'..'9': GetNumber; // c'est un nombre
     CColon: GetVar; // c'est une variable
     CPlus: if (Count = 0) or (Item[Count].Kind = cteBeginExp) then
               AddItem(CPlus, cteUnaryPlus) // plus unaire
             else
               AddItem(CPlus, ctePlus); // addition ou plus unaire
     CMinus: if (Count = 0) or (Item[Count].Kind = cteBeginExp) then
               AddItem(CMinus, cteUnaryMinus) // moins unaire
             else
               AddItem(CMinus, cteMinus); // soustraction ou moins unaire
     CMul: AddItem(CMul, cteMul); // multiplication
     CDiv: AddItem(CDiv, cteDiv); // division
     CPower: AddItem(CPower, ctePower); // puissance
     CGreater: GetDelimGreater; // plus grand ou >=
     CLower: GetDelimLower; // plus petit ou <= ou <>
     CEqual: AddItem(CEqual, cteEqual); // égal
     CNot: GetDelimNot; // négation ou !=
     COrB: AddItem(COrB, cteOrB); // ou logique |
     CAndB: AddItem(CAndB, cteAndB); // et logique &
     CBeginPar: AddItem(CBeginPar, cteBeginExp); // parenthèse ouvrante
     CEndPar: AddItem(CEndPar, cteEndExp); // parenthèse fermante
     'a'..'z', 'A'..'Z': GetFunction; // fonction
   else
     AddItem(LCh, cteUnknown); // enregistre le caractère interdit
     // [### Erreur: caractère interdit ###]
     Error.SetError(CE_BadChar, Text, fIndx - 1);
   end;
  end;
end;

procedure TGVEval.DoScan;
// *** analyse des éléments (algorithme shunting-yard) ***
var
  Li: Integer;
  LBI1, LBI2: TGVBaseItem;
begin
  State := esScanning; // état mis à jour
  WipeScan; // nettoyage de la sortie
  for Li := 1 to Count do // on balaie la liste
  begin
    LBI1 := Item[Li]; // élément en cours
    case LBI1.Kind of // on analyse sa nature
      cteReal, cteInteger, cteVar, cteBoolean: // <==== une constante
        AddScan(LBI1); // élément pour la sortie
      cteFunction: // <===== une fonction
        fScanStack.Push(LBI1);
      ctePlus, cteMinus, cteMul, cteDiv, ctePower, cteGreater, cteLower,
      cteEqual, cteNotEqual, cteGreaterOrEqual, cteLowerOrEqual, cteMod,
      cteNot, cteAnd, cteOr, cteOrB, cteAndB, cteUnaryMinus,
      cteUnaryPlus: // <==== un opérateur
        begin
          while (not fScanStack.IsEmpty) // tant que la pile n'est pas vide
            and (((Association(LBI1.Kind) = 0) and (Precedence(LBI1.Kind) >=
            Precedence(fScanStack.Peek.Kind))) or
            ((Association(LBI1.Kind) = 1) and (Precedence(LBI1.Kind) <
            Precedence(fScanStack.Peek.Kind)))) do
              AddScan(fScanStack.Pop); // on stocke le sommet de la pile
          fScanStack.Push(LBI1); // on empile le nouvel opérateur
        end;
      cteBeginExp: // <==== une parenthèse ouvrante
        fScanStack.Push(LBI1); // parenthèse ouvrante empilée
      cteEndExp: // <==== une parenthèse fermante
        begin
           // tant que pile non vide et sommet <> (
          while (not fScanStack.IsEmpty) and
            (fScanStack.Peek.Kind <> cteBeginExp) do
              AddScan(fScanStack.Pop); // on stocke le sommet de la pile
          // parenthèse ouvrante trouvée ?
          if (not fScanStack.IsEmpty) and
            (fScanStack.Peek.Kind = cteBeginExp) then
          begin
            fScanStack.Pop; // on retire la parenthèse
            if (not fScanStack.IsEmpty) and // pile non vide ?
              // et fonction au sommet ?
              (fScanStack.Peek.Kind = cteFunction) then
                AddScan(fScanStack.Pop); // on stocke le sommet de la pile
          end
          else
          begin
            // [### Erreur: parenthèses ###]
            Error.SetError(CE_ParMismatch, Text, Li);
            Break; // on quitte la boucle
          end;
        end;
    end;
  end;
  if Error.OK then // pas d'erreur ?
  begin
    while (not fScanStack.IsEmpty) do
    begin
      LBI2 := fScanStack.Pop; // on récupère le sommet
      if (LBI2.Kind in [cteBeginExp, cteEndExp])then
      begin
        // [### Erreur: parenthèses ###]
        Error.SetError(CE_ParMismatch, Text);
        Exit; // on quitte la procédure
      end
      else
        AddScan(LBI2); // on stocke
    end;
  end;
end;

procedure TGVEval.DoEvaluate;
// *** évaluation ***
var
  Li: Integer;
  LWhich: TGVBaseItem;

  procedure DoFunction;
  // traite une fonction
  var
    Lfunc: TGVFunctions;
  begin
    LFunc := WhichFunction(LWhich.Token); // numéro de fonction
    if fDStack.Needed(1) then // il faut un élément sur la pile
    begin
      case LFunc of // choix de la fonction
        C_DAbs, C_DAbs2: DoAbs; // valeur absolue
        C_DCos, C_DCos2: DoCos; // cosinus
        C_DSin, C_DSin2: DoSin; // sinus
        C_DTan, C_DTan2: DoTan; // tangente
        C_DSqrt, C_DSqrt2: DoSqrt; // racine carrée
        C_DTrunc: DoTrunc;  // nombre tronqué
        C_DRound: DoRound;  // nombre arrondi
        C_DSqr: DoSqr; // nombre au carré
        C_DExp: DoExp; // exponentielle
        C_DFrac: DoFrac; // partie fractionnelle
        C_DInt, C_DInt2: DoInt; // partie entière
        C_DLn: DoLn; // log népérien
        C_DLog2:  DoLog2; // log base 2
        C_DLog10: DoLog10; // log base 10
        C_DCoTan, C_DCoTan2: DoCoTan; // cotangente
        C_DArcCos, C_DArcCos2: DoArcCos; // arc cosinus
        C_DArcSin, C_DArcSin2: DoArcSin; // arc sinus
        C_Minus: DoMinus;// négatif ?
        C_Plus: DoPlus; // positif ?
        C_DNegate: DoNegate; // signe inversé
        C_DSign: DoSign; // signe
        C_DRandom: DoRandom; // entier au hasard
      end;
    end
    else
      // [### Erreur: pas assez d'arguments ###]
      Error.SetError(CE_NoArg, Text, Li);
    end;

begin
  State := esComputing; // état mis à jour
  fResult := 0; // résultat à zéro
  for Li := 1 to ScanCount do // on balaie les valeurs
  begin
    if not Error.OK then
      Break; // on sort en cas d'erreur
    LWhich := ScanItem[Li]; // élément en cours
    fActualItem := LWhich.Token; // élément en cours stocké
    Change; // notification de changement
    with fDStack do
      case LWhich.Kind of // répartition suivant la nature de l'élément
        cteReal, cteInteger, cteVar,
          cteBoolean: DoNumber(LWhich.Token); // nombre empilé
        ctePlus: DoAdd; // addition ?
        cteMinus: DoSub; // soustraction
        cteMul: DoMul; // multiplication
        cteDiv: DoDiv; // divivsion
        ctePower: DoPower; // puissance
        cteGreater: DoGreater; // >
        cteLower: DoLower; // <
        cteGreaterOrEqual: DoGreaterOrEqual; // >=
        cteLowerOrEqual: DoLowerOrEqual; // <=
        cteEqual: DoEqual; // =
        cteNotEqual: DoNotEqual; // != ou <>
        cteMod: DoMod; // mod
        cteNot: DoNot; // non
        cteOr, cteOrB: DoOr; // ou
        cteAnd, cteAndB: DoAnd; // et
        cteFunction: DoFunction; // une fonction
        cteUnaryMinus: DoUnaryMinus; // moins unaire
        cteUnaryPlus: DoUnaryPlus; // plus unaire
      end;
  end;
  // récupération du résultat si disponible
  if Error.OK then // pas d'erreur ?
  begin
    if fDStack.Count = 1 then // un élément attendu pour le résultat
    begin
      fResult := fDStack.Pop;  // c'est celui qui est sur la pile
      State := esOk; // résultat trouvé
      Change; // notification de changement
    end
    else
      // [### Erreur: des éléments restent ###]
      Error.SetError(CE_BadExp, Text);
  end;
end;

procedure TGVEval.GetVar;
// *** recherche d'une variable ***
var
  LSt: string;
  LRes: string;
  Li: Integer;
begin
  LSt := EmptyStr; // initialisation de la chaîne de travail
  LRes := EmptyStr; // résultat par défaut
  Inc(fIndx); // on passe au caractère suivant
  Li := Indx - 1; // on sauvegarde la position du pointeur si erreur
  // on recherche le nom de la variable (":" déjà traité)
  // le premier caractère doit être une lettre
  if (Text[Indx] in CAlpha) and (Indx <= Length(Text)) then
  begin
    LSt := LSt + Text[Indx]; // on conserve ce caractère
    Inc(fIndx); // on passe au suivant
  end
  else
  begin
    LSt := CColon;
    if Indx <= Length(Text) then
      LSt := LSt + Text[Indx]; // on traite le caractère fautif
    AddItem(LSt, cteForbidden); // élément en cours sauvegardé
    fIndx := Li; // on retrouve le début de la variable
    // [### Erreur: variable incorrecte ###]
    Error.SetError(CE_BadVar, LSt, Indx); // variable incorrecte
    Exit;
  end;
  // la suite peut être un caractère alphanumérique
  while (Indx <= Length(Text)) and (Text[Indx] in CAlphaNum) do
  begin
    LSt := LSt + Text[Indx]; // on stocke le caractère
    Inc(fIndx); // au suivant
  end;
  // les gestionnaires de variables sont-il en fonction? (erreur interne ?)
  if Assigned(fLocVar) and Assigned(fKernel) then
  begin
    Dec(fIndx); // réajustement du pointeur
    // variable locale ?
    if fLocVar.IsLocVar(LSt) and Error.OK then // variables locale
    begin
      // recherche de la variable et de sa valeur
      if fLocVar.ValLocVar(LSt, LRes) then
        // on enregistre sa valeur et sa catégorie
        AddItem(LRes, cteVar)
    end
    else
    // variable globale ?
    if fKernel.ValVar(LSt, LRes) then
        // on enregistre sa valeur et sa catégorie
        AddItem(LRes, cteVar)
    else
    begin
      LSt := CColon + LSt; // on replace les deux points
      AddItem(LSt, cteUnknown); // enregistre l'élément fautif
      fIndx := Li ; // on retrouve le début de la variable
      // [### Erreur: recherche infructueuse ###]
      Error.SetError(fKernel.Error.Error.Code, LSt, Indx);
    end;
  end
  else
    // [### Erreur: gestionnaire de variables introuvable ###]
    Error.SetError(CIE_NoGetVar, Text);
end;

procedure TGVEval.GetFunction;
// *** recherche d'une fonction ***
var
  LS: string;
  Li: Integer;
  LWhich: TGVFunctions;
begin
 LS := EmptyStr; // initialisation
 Li := Indx - 1; // pointeur conservé
 LWhich := C_Unknown; // fonction inconnue
 // la fonction est composée de caractères alphanumériques
 while (Indx <= Length(Text)) and (Text[Indx] in CAlphaNum) do
 begin
   LS := LS + Text[Indx]; // on stocke le caractère
   Inc(fIndx); // au suivant
 end;
 Dec(fIndx); // on réajuste le pointeur
 LWhich := WhichFunction(LS); // on cherche une fonction
 if (LWhich <> C_Unknown) and (LWhich < C_DMax) then // trouvée ?
 begin
   case LWhich of
     // élément ajouté
     C_DAbs..C_DRandom: AddItem(AnsiUpperCase(LS), cteFunction);
     C_Not: AddItem(MF_Not, cteNot); // non logique
     C_DPi: AddItem(FloatToStr(Pi), cteReal); // nombre PI
     C_True: AddItem(IntToStr(CRTrue), cteBoolean); // valeur VRAI
     C_False: AddItem(IntToStr(CRFalse), cteBoolean); // valeur FAUX
     // fonctions infixées
     C_Or: AddItem(MF_Or, cteOr); // ou logique
     C_And: AddItem(MF_And, cteAnd); // et logique
     C_Mod: AddItem(MF_Mod, cteMod); // modulo
     C_DPower: AddItem(MF_DPower, ctePower); // puissance
   end;
 end
 else
 begin
   fIndx := Li; // on retrouve le début du mot
   if (LWhich <> C_Unknown) then
   begin
     AddItem(AnsiUppercase(LS), cteNotSupported);
     // [### Erreur: fonction non supportée ###]
     Error.SetError(CE_FunctionNotSupported, LS, Indx);
   end
   else
   begin
     AddItem(AnsiUppercase(LS), cteUnknown); // on enregistre l'élément fautif
     // [### Erreur: fonction inconnue ###]
     Error.SetError(CE_UnKnownFunction, LS, Indx); // on signale l'erreur
   end;
 end;
end;

procedure TGVEval.GetNumber;
// *** recherche d'un nombre ***
var
  LS: string;
begin
  LS := EmptyStr; // on initialise la chaîne de travail
  // on recherche la partie entière
  while (Indx <= Length(Text)) and (Text[Indx] in CDigit) do
  begin
    LS := LS + Text[Indx]; // on stocke le caractère
    Inc(fIndx); // au suivant
  end;
  // une virgule ou un point ?
  if (Text[Indx] in [CDot, CComma]) then
  begin
    LS := LS + CComma; // si oui, on ajoute le signe [, en France]
    Inc(fIndx); // caractère suivant
    // on cherche la partie décimale
    while (Indx <= Length(Text)) and (Text[Indx] in CDigit) do
    begin
      LS := LS + Text[Indx]; // on stocke le caractère
      Inc(fIndx); // au suivant
    end;
    // on enregistre le réel
    AddItem(LS, cteReal);
  end
  else
    // on enregistre l'entier
    AddItem(LS, cteInteger);
  Dec(fIndx); // on revient sur le dernier caractère
end;

procedure TGVEval.GetDelimGreater;
// *** > ou >= ***
begin
  if (Indx < Length(Text)) and (Text[Indx + 1] = CEqual) then // >= ?
  begin
    AddItem(CGreaterOrEqual, cteGreaterOrEqual); // on enregistre >=
    Inc(fIndx); // caractère suivant
  end
  else
    AddItem(CGreater, cteGreater); // >
end;

procedure TGVEval.GetDelimLower;
// *** < ou <= ou <> ***
begin
  // <= ou <> ?
  if (Indx < Length(Text)) and (Text[Indx + 1] in [CEqual, CGreater]) then
  begin
    Inc(fIndx); // caractère suivant
    if Text[Indx] = CEqual then
      AddItem(CLowerOrEqual, cteLowerOrEqual) // on enregistre <=
    else
      AddItem(CNotEqual, cteNotEqual); // on enregistre <>
  end
  else
    AddItem(CLower, cteLower); // <
end;

procedure TGVEval.GetDelimNot;
// *** négation ou != (différent) ***
begin
  if (Indx < Length(Text)) and (Text[Indx + 1] = CEqual) then // != ?
  begin
    AddItem(CNotEqual2, cteNotEqual); // on enregistre !=
    Inc(fIndx); // caractère suivant
  end
  else
    AddItem(CNot, cteNot); // >
end;

procedure TGVEval.Change;
// *** notification de changement ***
begin
  if Assigned(fOnChange) then // le gestionnaire existe-t-il ?
    fOnChange(Self); // on l'exécute
end;

procedure TGVEval.StateChange;
// *** changement de l'état de l'évaluateur ***
begin
  if Assigned(fOnStateChange) then // gestionnaire assigné ?
    fOnStateChange(Self); // on l'exécute
end;

{ TGVEvalEnumerator }

function TGVEvalEnumerator.GetCurrent: TGVBaseItem;
// *** retourne l'élément courant ***
begin
  Result := fLst[fIndex];
end;

constructor TGVEvalEnumerator.Create(const AValue: TGVItems);
// *** création de l'énumérateur ***
begin
  inherited Create;
  fIndex := -1;
  fLst := AValue;
end;

function TGVEvalEnumerator.MoveNext: Boolean;
// *** passe à l'élément suivant ***
begin
  Result := fIndex < High(fLst);
  if Result then
    Inc(fIndex);
end;

end.
