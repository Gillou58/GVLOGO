{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Evaluation d'une expression             |
  |                  Unité : GVEval.pas                                    |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    27-11-2014 10:47:20                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVEval - part of GVLOGO
// Copyright (C) 2014 Gilles VASSEUR
//
// This program is free software: you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
// If not, see <http://www.gnu.org/licenses/>.

{$I GVDefines.inc}

{$IFNDEF Delphi}
{$mode objfpc}{$H+}
{$ENDIF}

unit GVEval;
// Evaluation d'une expression mathématique

interface

uses
  GVConsts, SysUtils, Classes, GVStacks;

type
  // *** classes pour l'évaluation d'une expression mathématique infixée
  EEvalException = class(Exception); // exception

  // événement de recherche d'une variable
  TGVGetVarEvent = procedure(Sender: TObject; VarName: string; var
    Value: Double; var Error: TGVError) of object;

  // tableau des éléments
  TGVItems = array of TGVBaseItem;

  { TGVEvalEnumerator }

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

  { TGVEval }

  TGVEval = class(TObject)
  private
    fEvalState: TGVEvalState; // état de l'évaluation
    fOnChange: TNotifyEvent; // gestionnaire de changements
    fOnError: TNotifyEvent; // gestionnaire d'erreurs
    fOnGetVar: TGVGetVarEvent; // événement concernant les variables
    fResult: Double; // résultat de l'évaluation
    fText: string; // texte à analyser
    fActualItem: string; // élément en cours
    fIndx: Integer; // index dans la lecture
    fStartIndx: Integer; // index de départ dans la chaîne de travail
    fItemList: TGVItems; // éléments de la chaîne de travail
    fError: TGVError; // erreur en cours
    fScan: TGVItems; // résultat du scan
    fScanStack: TGVEvalStack; // pile pour l'évaluation
    function GetCount: Integer; // nombre d'éléments
    function GetItem(N: Integer): TGVBaseItem; // accès aux éléments
    function GetScanCount: Integer; // nombre d'éléments scannés
    function GetScanItem(N: Integer): TGVBaseItem; // élément de scan
    procedure SetStartIndx(AValue: Integer); // index de départ fixé
    procedure SetState(AValue: TGVEvalState); // état fixé
    procedure SetText(const AValue: string); // texte en cours fixé
    procedure SetError(const Err: TGVError); // erreur fixée
    procedure GetDelimGreater; // plus grand ou >=
    procedure GetDelimLower; // plus petit ou <=
    procedure GetDelimNot; // non ou !=
    procedure WipeItems; inline; // nettoyage du tableau des éléments
    procedure WipeScan; inline; // nettoyage du scan
    function WhichFunction(const St: string): TGVFunctions; // cherche une fonction
  protected
    // ajoute un élément au tableau des éléments
    procedure AddItem(const AItem: string; AKind: CTokensEnum); virtual;
    // ajoute un élément à la liste de scan
    procedure AddScan(const AItem: TGVBaseItem); virtual;
    procedure GetVar; virtual; // traitement des variables
    procedure GetFunction; virtual; // traitement des fonctions
    procedure GetNumber; virtual; // traitement des nombres
    procedure Change; // notification de changement
    procedure Tokenize; // répartition en éléments
    procedure DoScan; // on analyse les  éléments
    function DoEvaluate: Double; // on évalue
  public
    constructor Create; overload; // constructeur simple
    // constructeur avec initialisation
    constructor Create(const AText: string); overload;
    destructor Destroy; override; // destructeur
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
    property Error: TGVError read fError default C_None; // erreur en cours
    // état de l'évaluateur
    property State: TGVEvalState read fEvalState write SetState default esNoInit;
    // événement lié à une erreur
    property OnError: TNotifyEvent read fOnError write fOnError;
    // événement lié à la recherche d'une variable
    property OnGetVar: TGVGetVarEvent read fOnGetVar write fOnGetVar;
    // événement lié à un changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
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
  SetError(C_None); // pas d'erreur
  State := esWaiting; // état d'attente
end;

procedure TGVEval.SetError(const Err: TGVError);
// *** fixe l'erreur en cours ***
begin
  fError := Err; // erreur stockée
  if fError <> C_None then
  begin  // erreur réelle ?
    Dec(fIndx); // ajustement du pointeur
    if Assigned(OnError) then // événement erreur
      OnError(Self);
    WipeItems; // on nettoie le tableau
  end;
end;

procedure TGVEval.WipeItems;
// *** nettoyage du tableau des éléments ***
begin
  SetLength(fItemList, 0);
  fActualItem := EmptyStr; // on nettoie l'élément en cours
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
  I: TGVFunctions;
begin
  Result := C_Unknown; // valeur si non trouvée
  for I := Low(TGVFunctions) to High(TGVFunctions) do // on balaie les fonctions
 begin
   if AnsiCompareText(St, GVFunctionName[I]) = 0 then // égalité des chaînes ?
   begin
     Result := I; // sauvegarde de l'indice
     Break; // on sort de la boucle
   end;
 end;
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
  Inc(fIndx); // caractère suivant
  Change; // changement notifié
end;

procedure TGVEval.AddScan(const AItem: TGVBaseItem);
// *** ajoute un élément à la liste de scan ***
begin
  SetLength(fScan, Length(fScan) + 1); // adapte la longueur du tableau
  fScan[Length(fScan) - 1] := AItem; // affecte le nouvel élément
  fActualItem := AItem.Token; // élément en cours
end;

procedure TGVEval.SetStartIndx(AValue: Integer);
// *** fixe l'index de départ dans l'expression ***
begin
  if fStartIndx = AValue then
    Exit; // on sort si aucun changement
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    raise EEvalException.CreateFmt(ME_OutOfRange, [AValue, Text]);
  fStartIndx := AValue; // nouvelle valeur de l'index
end;

procedure TGVEval.SetState(AValue: TGVEvalState);
// *** état de l'évaluateur ***
begin
  if fEvalState = AValue then
    Exit; // on sort si aucun changement
  fEvalState := AValue; // nouvelle valeur
  Change; // on notifie le changement
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
    raise EEvalException.CreateFmt(ME_OutOfRange, [N, Text]); // erreur
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
    raise EEvalException.CreateFmt(ME_OutOfRange, [N, Text]); // erreur
  Result := fScan[N-1]; // base 1
end;

constructor TGVEval.Create;
// *** constructeur simple ***
begin
  inherited Create; // on hérite
  fEvalState := esNoInit; // état de l'évaluation (non initialisée)
  fError := C_None; // pas d'erreur
  fStartIndx := 1; // index par défaut de départ
  fIndx := -1; // index de travail
  fScanStack := TGVEvalStack.Create; // pile de scan
  fText := EmptyStr; // chaîne de travail vide
  fResult := 0; // résultat par défaut
  WipeItems; // tableau vide
  randomize; // nombres pseudo-aléatoires
end;

constructor TGVEval.Create(const AText: string);
// *** constructeur avec initialisation ***
begin
  Create; // appel du constructeur simple
  Text := AText; // initialisation du texte de travail
end;

destructor TGVEval.Destroy;
// *** destructeur ***
begin
  fScanStack.Free; // pile de scan libérée
  inherited Destroy; // on hérite
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
    raise EEvalException.Create(ME_NoInit);
  try
  SetError(C_None); // pas d'erreur
  Tokenize; // répartition en éléments
  if Error = C_None then // pas d'erreur ?
    DoScan; // on analyse
  if Error = C_None then // toujours pas d'erreur ?
    fResult := DoEvaluate; // on évalue
  finally
    State := esWaiting; // état d'attente
  end;
end;

{ TGVEval }

procedure TGVEval.Tokenize;
// *** répartit en éléments ***
var
  Ch: Char;
begin
  State := esTokenizing; // état mis à jour
  WipeItems; // liste interne nettoyée
  fIndx := StartIndx; // départ initialisé
  while (Error = C_None) and (fIndx <= Length(Text)) do // on balaie l'expression
  begin
   Ch := fText[fIndx]; // caractère en cours
   case Ch of
     CBlank: Inc(fIndx); // on ignore les blancs
     '0'..'9': GetNumber; // c'est un nombre
     CColon: GetVar; // c'est une variable
     CPlus: AddItem(CPlus, ctePlus); // addition
     CMinus: AddItem(CMinus, cteMinus); // soustraction
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
     AddItem(Ch, cteUnknown); // enregistre le caractère interdit
     SetError(C_BadChar); // caractère interdit
   end;
  end;
  if (Error = C_None) then  // une erreur ?
    AddItem(EmptyStr, cteEnd); // marque de fin
end;

procedure TGVEval.DoScan;
// *** analyse des éléments (algorithme shunting-yard)***
var
  I: Integer;
  Which, Which2: TGVBaseItem;
begin
  State := esScanning; // état mis à jour
  WipeScan; // nettoyage de la sortie
  for I := 1 to Count-1 do // on balaie la liste (sauf marque de fin
  begin
    Which := Item[I]; // élément en cours
    case Which.Kind of // on analyse sa nature
      cteReal, cteInteger, cteVar, cteBoolean:
        AddScan(Which); // élément pour la sortie
      cteFunction:
        fScanStack.Push(Which); // fonction empilée
      ctePlus, cteMinus, cteMul, cteDiv, ctePower, cteGreater, cteLower, cteEqual,
      cteNotEqual, cteGreaterOrEqual, cteLowerOrEqual, cteMod, cteNot, cteAnd,
      cteOr, cteEnd, cteOrB, cteAndB: // un opérateur ?
        begin
          while (not fScanStack.IsEmpty) // tant que la pile n'est pas vide
            and (((Association(Which.Kind) = 0) and (Precedence(Which.Kind) >=
            Precedence(fScanStack.Peek.Kind))) or
            ((Association(Which.Kind) = 1) and (Precedence(Which.Kind) <
            Precedence(fScanStack.Peek.Kind)))) do
          begin
            Which2 := fScanStack.Pop; // on récupère l'opérateur
            AddScan(Which2); // on le stocke
          end;
          fScanStack.Push(Which); // on empile le nouvel opérateur
        end;
      cteBeginExp: // traitement d'une parenthèse ouvrante
        fScanStack.Push(Which); // parenthèse ouvrante empilée
      cteEndExp: // traitement d'une parenthèse fermante
        begin
           // tant que pile non vide et sommet <> (
          while (not fScanStack.IsEmpty) and
            (fScanStack.Peek.Kind <> cteBeginExp) do
          begin
            Which2 := fScanStack.Pop; // on récupère l'opérateur
            AddScan(Which2); // on le stocke
          end;
          // parenthèse ouvrante trouvée ?
          if (not fScanStack.IsEmpty) and (fScanStack.Peek.Kind = cteBeginExp) then
          begin
            fScanStack.Pop; // on retire la parenthèse
            if (not fScanStack.IsEmpty) and // pile non vide ?
              (fScanStack.Peek.Kind = cteFunction) then // et fonction au sommet ?
            begin
              Which2 := fScanStack.Pop; // on la récupère
              AddScan(Which2); // on la stocke
            end;
          end
          else
          begin
            SetError(C_ParMismatch); // erreur de parenthèses
            Exit; // on quitte la procédure
          end;
        end;
    end;
  end;
  if Error = C_None then // pas d'erreur ?
  begin
    while (not fScanStack.IsEmpty) do
    begin
      Which2 := fScanStack.Pop; // on récupère le sommet
      if (Which2.Kind in [cteBeginExp, cteEndExp])then
      begin
        SetError(C_ParMismatch); // erreur de parenthèses
        Exit; // on quitte la procédure
      end
      else
        AddScan(Which2); // on stocke
    end;
  end;
end;

function TGVEval.DoEvaluate: Double;
// *** évaluation ***
var
  I: Integer;
  Which: TGVBaseItem;
  ValStack: TGVDoubleStack;
  Dbl, Dbl2: Double;

  procedure DoFunction;
  // une fonction
  var
    Afunc: TGVFunctions;
    Dbl: Double;
  begin
    AFunc := WhichFunction(Which.Token); // numéro de fonction
    with ValStack do
    begin
      if Needed(1) then // il faut un élément sur la pile
      begin
        case AFunc of
          C_DAbs, C_DAbs2: Push(Abs(Pop)); // *** valeur absolue
          C_DCos, C_DCos2: Push(Cos(DegToRad(Pop))); // *** cosinus
          C_DSin, C_DSin2: Push(Sin(DegToRad(Pop))); // *** sinus
          C_DTan, C_DTan2: begin // *** tangente
                             Dbl := DegToRad(Pop);
                             if not IsZero(Cos(Dbl)) then // pas de cosinus nul
                               Push(Tan(Dbl))
                             else
                               SetError(C_Tan);
                           end;
          C_DSqrt, C_DSqrt2: begin // *** racine carrée
                             if (Peek >= 0) then // pas de nombre négatif
                               Push(Sqrt(Pop))
                             else
                               SetError(C_NegNumber);
                           end;
          C_DTrunc: Push(Trunc(Pop));  // *** nombre tronqué
          C_DRound: Push(Round(Pop));  // *** nombre arrondi
          C_DSqr: Push(Peek * Pop); // *** nombre au carré
          C_DExp: Push(Exp(Pop)); // *** exponentielle
          C_DFrac: Push(Frac(Pop)); // *** partie fractionnelle
          C_DInt, C_DInt2: Push(Int(Pop)); // *** partie entière
          C_DLn: begin // *** log népérien
                   if (Peek > 0) then // pas de nombre négatif ou nul
                               Push(Ln(Pop))
                             else
                               SetError(C_Ln);
                 end;
          C_DLog2:  begin // *** log base 2
                      if (Peek > 0) then // pas de nombre négatif ou nul
                        Push(Log2(Pop))
                      else
                        SetError(C_Ln);
                    end;
          C_DLog10: begin // *** log népérien
                      if (Peek > 0) then // pas de nombre négatif ou nul
                        Push(Log10(Pop))
                      else
                        SetError(C_Ln);
                 end;
          C_DCoTan, C_DCoTan2: begin // *** cotangente
                                 Dbl := DegToRad(Pop);
                                 if not IsZero(Sin(Dbl)) then // pas de sinus nul
                                   Push(Tan(Dbl))
                                 else
                                   SetError(C_CoTan);
                               end;
          C_DArcCos, C_DArcCos2: begin // *** arc cosinus
                                   Dbl := Pop;
                                   if (Dbl >= -1.0) and (Dbl <= 1) then
                                     Push(RadToDeg(ArcCos(Dbl)))
                                   else
                                     SetError(C_Arc);
                                  end;
          C_DArcSin, C_DArcSin2: begin // *** arc sinus
                                   Dbl := Pop;
                                   if (Dbl >= -1.0) and (Dbl <= 1) then
                                     Push(RadToDeg(ArcSin(Dbl)))
                                   else
                                     SetError(C_Arc);
                                  end;
          C_Minus: begin // *** négatif ?
                      Dbl := Pop;
                      if (Dbl <= 0) then
                        Push(CRTrue)
                      else
                        Push(CRFalse);
                   end;
          C_Plus: begin // *** positif ?
                      Dbl := Pop;
                      if (Dbl >= 0) then
                        Push(CRTrue)
                      else
                        Push(CRFalse);
                   end;
          C_DNegate: Push(-Pop); // *** signe inversé
          C_DSign: Push(Sign(Pop)); // *** signe
          C_DRandom: begin // *** nombre au hasard
                       Dbl := Pop;
                       if (Trunc(Dbl) = Dbl) then
                         Push(Random(Trunc(Dbl)))
                       else
                         SetError(C_NeedsInteger); // erreur : entier exigé
                     end;
        end;
      end
      else
        SetError(C_NoArg); // pas assez d'arguments
    end;
  end;

begin
  State := esComputing; // état mis à jour
  Result := 0;
  ValStack := TGVDoubleStack.Create; // création de la pile
  try
    for I := 1 to ScanCount do // on balaie les valeurs
    begin
      if Error <> C_None then
        exit; // on sort en cas d'erreur
      Which := ScanItem[I]; // élément en cours
      fActualItem := Which.Token; // élément en cours stocké
      with ValStack do
        case Which.Kind of
          cteReal, cteInteger, cteVar, cteBoolean: // *** nombre? => empilé
            begin
              if TryStrToFloat(Which.Token, Dbl) then // correct ?
                Push(Dbl) // on l'empile
              else
                SetError(C_BadNumber);
            end;
          ctePlus: if Needed(2) then // *** addition ?
              Push(Pop + Pop)
            else
              SetError(C_NoArg); // pas assez d'arguments
         cteMinus: if Needed(2) then // *** soustraction ?
           begin
             Swap; // inversion sur la pile
             Push(Pop - Pop);
           end
           else
             SetError(C_NoArg);
         cteMul: if Needed(2) then // *** multiplication ?
              Push(Pop * Pop)
            else
              SetError(C_NoArg);
         cteDiv: if Needed(2) then // *** division ?
           begin
             if Peek <> 0 then // division par zéro ?
             begin
               Swap; // inversion sur la pile
               Push(Pop / Pop);
             end
             else
               SetError(C_Zero); // erreur division par zéro
           end
           else
             SetError(C_NoArg);
         ctePower: if Needed(2) then // *** puissance
           begin
            Swap;
            Push(Power(Pop, Pop));
           end
           else
             SetError(C_NoArg);
         cteGreater: if Needed(2) then // *** >
           begin
             if Pop <= Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteLower: if Needed(2) then // *** <
           begin
             if Pop >= Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteGreaterOrEqual: if Needed(2) then // *** >=
           begin
             if Pop < Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteLowerOrEqual: if Needed(2) then // *** <=
           begin
             if Pop > Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteEqual: if Needed(2) then // *** =
           begin
             if Pop = Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteNotEqual: if Needed(2) then // *** <> ou !=
           begin
             if Pop <> Pop then
               Push(CRTrue)
             else
               Push(CrFalse);
           end
           else
             SetError(C_NoArg);
         cteMod: if Needed(2) then // *** mod
           begin
               Dbl := Pop;
               Dbl2 := Pop;
               if (Trunc(Dbl) = Dbl) and (Trunc(Dbl2) = Dbl2) then
                 Push(Trunc(Dbl2) mod Trunc(Dbl))
               else
                 SetError(C_NeedsInteger); // erreur : entiers exigés
           end
           else
             SetError(C_NoArg);
         cteNot: if Needed(1) then // *** non
           begin
             Dbl := Pop;
             if (Trunc(Dbl) = Dbl) then
                 Push(not Trunc(Dbl))
               else
                 SetError(C_NeedsInteger); // erreur : entiers exigés
           end
           else
             SetError(C_NoArg);
         cteOr, cteOrB: if Needed(2) then // *** ou
           begin
             Dbl := Pop;
             Dbl2 := Pop;
             if (Trunc(Dbl) = Dbl) and (Trunc(Dbl2) = Dbl2) then
                 Push(Trunc(Dbl) or Trunc(Dbl2))
               else
                 SetError(C_NeedsInteger); // erreur : entiers exigés
           end
           else
             SetError(C_NoArg);
         cteAnd, cteAndB: if Needed(2) then // *** et
           begin
             Dbl := Pop;
             Dbl2 := Pop;
             if (Trunc(Dbl) = Dbl) and (Trunc(Dbl2) = Dbl2) then
                 Push(Trunc(Dbl) and Trunc(Dbl2))
               else
                 SetError(C_NeedsInteger); // erreur : entiers exigés
           end
           else
             SetError(C_NoArg);
         cteFunction: DoFunction; // *** une fonction
      end;
    end;
    if Error = C_None then // pas d'erreur ?
    begin
      if ValStack.Count = 1 then // un élément attendu
        Result := ValStack.Pop  // c'est lui...
      else
        SetError(C_BadExp); // il reste des éléments inutilisés
    end;
  finally
    ValStack.Free; // libération de la pile
  end;
end;

procedure TGVEval.GetVar;
// *** recherche d'une variable ***
var
  St: string;
  OutRes: Double;
  Err: TGVError;
  IndxTmp: Integer;
begin
  St := EmptyStr; // initialisation de la chaîne de travail
  OutRes := 0; // résultat par défaut
  Err := C_None; // pas d'erreur par défaut
  Inc(fIndx); // on passe au caractère suivant
  IndxTmp := Indx; // on sauvegarde la position du pointeur si erreur
  // on recherche le nom de la variable (: déjà traité)
  // le premier caractère doit être une lettre
  {$IFDEF Delphi}
  if CharInSet(Text[Indx], CAlpha)  and (Indx <= Length(Text)) then
  {$ELSE}
  if (Text[Indx] in CAlpha) and (Indx <= Length(Text)) then
  {$ENDIF}
  begin
    St := St + Text[Indx]; // on conserve ce caractère
    Inc(fIndx); // on passe au suivant
  end
  else
  begin
    St := CColon;
    if Indx <= Length(Text) then
      St := St + Text[Indx]; // on traite le caractère fautif
    AddItem(St, cteForbidden); // élément en cours sauvegardé
    fIndx := IndxTmp; // on retrouve le début de la variable
    SetError(C_BadVar); // variable incorrecte
    Exit;
  end;
  // la suite peut être un caractère alphanumérique
  {$IFDEF Delphi}
  while (Indx <= Length(Text)) and CharInSet(Text[Indx], CAlphaNum) do
  {$ELSE}
  while (Indx <= Length(Text)) and (Text[Indx] in CAlphaNum) do
  {$ENDIF}
  begin
    St := St + Text[Indx]; // on stocke le caractère
    Inc(fIndx); // au suivant
  end;
  // le gestionnaire est-il en fonction? (erreur interne ?)
  if Assigned(OnGetVar) then
  begin
    if (Error = C_None) then // pas d'erreur ?
    begin
      // recherche de la variable et de sa valeur
      OnGetVar(Self, St, OutRes, Err);
      Dec(fIndx); // réajustement du pointeur
      if (Err = C_None) then // pas d'erreur ?
        AddItem(FloatToStr(OutRes), cteVar) // on enregistre sa valeur et sa catégorie
      else
      begin
        St := CColon + St; // on replace les deux points
        AddItem(St, cteUnknown); // enregistre l'élément fautif
        fIndx := IndxTmp; // on retrouve le début de la variable
        SetError(Err); // on signale l'erreur
      end;
    end;
  end
  else
    // erreur interne !
    raise EEvalException.Create(ME_InternalError);
end;

procedure TGVEval.GetFunction;
// *** recherche d'une fonction ***
var
  St: string;
  IndxTmp: Integer;
  Where: TGVFunctions;
begin
 St := EmptyStr; // initialisation
 IndxTmp := Indx; // pointeur conservé
 Where := C_Unknown; // fonction inconnue
 // la fonction est composée de caractères alphanumériques
 {$IFDEF Delphi}
 while (Indx <= Length(Text)) and CharInSet(Text[Indx], CAlphaNum) do
 {$ELSE}
 while (Indx <= Length(Text)) and (Text[Indx] in CAlphaNum) do
 {$ENDIF}
 begin
   St := St + Text[Indx]; // on stocke le caractère
   Inc(fIndx); // au suivant
 end;
 Dec(fIndx); // on réajuste le pointeur
 Where := WhichFunction(St); // on cherche une fonction
 if (Where <> C_Unknown) and (Where < C_DMax) then // trouvée ?
 begin
   case Where of
     // élément ajouté
     C_DAbs..C_DRandom: AddItem(AnsiUpperCase(St), cteFunction);
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
   fIndx := IndxTmp; // on retrouve le début du mot
   if (Where <> C_Unknown) then
   begin
     AddItem(AnsiUppercase(St), cteNotSupported); // non supporté dans une expression
     SetError(C_NotSupported); // on signale l'erreur
   end
   else
   begin
     AddItem(AnsiUppercase(St), cteUnknown); // on enregistre l'élément fautif
     SetError(C_BadFunction); // on signale l'erreur
   end;
 end;
end;

procedure TGVEval.GetNumber;
// *** recherche d'un nombre ***
var
  St: string;
begin
  St := EmptyStr; // on initialise la chaîne de travail
  // on recherche la partie entière
  {$IFDEF Delphi}
  while (Indx <= Length(Text)) and (CharInSet(Text[Indx], CDigit)) do
  {$ELSE}
  while (Indx <= Length(Text)) and (Text[Indx] in CDigit) do
  {$ENDIF}
  begin
    St := St + Text[Indx]; // on stocke le caractère
    Inc(fIndx); // au suivant
  end;
  // une virgule ou un point ?
  {$IFDEF Delphi}
  if CharInSet(Text[Indx], [CDot, CComma]) then
  {$ELSE}
  if (Text[Indx] in [CDot, CComma]) then
  {$ENDIF}
  begin
    St := St + CComma; // si oui, on ajoute le signe [, en France]
    Inc(fIndx); // caractère suivant
    // on cherche la partie décimale
    {$IFDEF Delphi}
    while (Indx <= Length(Text)) and CharInSet(Text[Indx], CDigit) do
    {$ELSE}
    while (Indx <= Length(Text)) and (Text[Indx] in CDigit) do
    {$ENDIF}
    begin
      St := St + Text[Indx]; // on stocke le caractère
      Inc(fIndx); // au suivant
    end;
    // on enregistre le réel
    AddItem(St, cteReal);
  end
  else
    // on enregistre l'entier
    AddItem(St, cteInteger);
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
  {$IFDEF Delphi}
  if (Indx < Length(Text)) and CharInSet(Text[Indx + 1], [CEqual, CGreater]) then
  {$ELSE}
  if (Indx < Length(Text)) and (Text[Indx + 1] in [CEqual, CGreater]) then
  {$ENDIF}
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
