{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Eléments d'une expression (2)           |
  |                  Unité : GVTokens2.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    13-11-2014 17:40:20                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVTokens2 - part of GVLOGO
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

unit GVTokens2;
// recherche de tokens - version avec reconnaissance de la nature de l'élément

interface

uses
  GVConsts, SysUtils, Classes;

type
  // *** classes pour l'évaluation d'une expression mathématique infixée
  EEvalException = class(Exception); // exception

  // événement de recherche d'une variable
  TGVGetVarEvent = procedure(Sender: TObject; VarName: string; var
    Value: Double; var Error: TGVError) of object;

  // élément de base de l'expression
  TGVBaseItem = record
    Token: string; // élément
    Kind: CTokensEnum; // type d'élément
  end;

  // tableau des éléments
  TGVItems = array of TGVBaseItem;

  { TGVTokensEnumerator }

  TGVTokensEnumerator = class(TObject) // énumération
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

  { TGVTokens2 }

  TGVTokens2 = class(TObject)
  private
    fOnChange: TNotifyEvent; // gestionnaire de changements
    fOnError: TNotifyEvent; // gestionnaire d'erreurs
    fOnGetVar: TGVGetVarEvent; // événement concernant les variables
    fText: string; // texte à analyser
    fIndx: Integer; // index dans la lecture
    fStartIndx: Integer; // index de départ dans la chaîne de travail
    fItemList: TGVItems; // éléments de la chaîne de travail
    fError: TGVError; // erreur en cours
    function GetCount: Integer;
    function GetItem(N: Integer): TGVBaseItem;
    procedure SetStartIndx(AValue: Integer); // index de départ fixé
    procedure SetText(const AValue: string); // texte en cours fixé
    procedure SetError(const Err: TGVError); // erreur fixée
    procedure WipeItems; inline; // nettoyage du tableau des éléments
  protected
    // ajoute un élément
    procedure AddItem(const AItem: string; AKind: CTokensEnum); virtual;
    procedure GetVar; virtual; // traitement des variables
    procedure GetFunction; virtual; // traitement des fonctions
    procedure GetNumber; virtual; // traitement des nombres
    procedure GetDelimGreater; // plus grand ou >=
    procedure GetDelimLower; // plus petit ou <=
    procedure GetDelimNot; // non ou !=
    procedure Change; // notification de changement
  public
    constructor Create; overload; // constructeur simple
    // constructeur avec initialisation
    constructor Create(const AText: string); overload;
    destructor Destroy; override; // destructeur
    function GetEnumerator: TGVTokensEnumerator; // énumération
    procedure Tokenize; // répartition en éléments
    property Text: string read fText write SetText; // expression à analyser
    // index de départ
    property StartIndx: Integer read fStartIndx write SetStartIndx default 1;
    property Indx: Integer read fIndx default -1; // index en cours dans la chaîne
    property Count: Integer read GetCount; // nombre d'éléments
    property Item[N: Integer]: TGVBaseItem read GetItem; default; // liste des éléments
    property Error: TGVError read fError default C_NoInit; // erreur en cours
    // événement lié à une erreur
    property OnError: TNotifyEvent read fOnError write fOnError;
    // événement lié à la recherche d'une variable
    property OnGetVar: TGVGetVarEvent read fOnGetVar write fOnGetVar;
    // événement lié à un changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

{ TGVTokens2 }

procedure TGVTokens2.SetText(const AValue: string);
// *** fixe l'expression à analyser ***
begin
  if fText = AValue then
    Exit; // sortie si aucun changement
  fText := AValue; // nouvelle valeur
  SetError(C_None); // pas d'erreur
end;

procedure TGVTokens2.SetError(const Err: TGVError);
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

procedure TGVTokens2.WipeItems;
// *** nettoyage du tableau des éléments ***
begin
  SetLength(fItemList, 0);
end;

procedure TGVTokens2.AddItem(const AItem: string; AKind: CTokensEnum);
// *** ajoute un élément ***
begin
  SetLength(fItemList, Length(fItemList) + 1); // adapte la longueur du tableau
  with fItemList[Length(fItemList) - 1] do
  begin
    Token := AItem; // valeur de l'élément
    Kind := AKind; // catégorie de l'élément
  end;
  Inc(fIndx); // caractère suivant
  Change; // changement notifié
end;

procedure TGVTokens2.SetStartIndx(AValue: Integer);
// *** fixe l'index de départ dans l'expression ***
begin
  if fStartIndx = AValue then
    Exit; // on sort si aucun changement
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    raise EEvalException.CreateFmt(ME_OutOfRange, [AValue, Text]);
  fStartIndx := AValue; // nouvelle valeur de l'index
end;

function TGVTokens2.GetCount: Integer;
// *** nombre d'éléments ***
begin
  Result := Length(fItemList);
end;

function TGVTokens2.GetItem(N: Integer): TGVBaseItem;
// *** renvoie d'un élément ***
begin
  if (N < 1) or (N > Count) then // hors limites ?
    raise EEvalException.CreateFmt(ME_OutOfRange, [N, Text]); // erreur
  Result := fItemList[N-1]; // base 1
end;

constructor TGVTokens2.Create;
// *** constructeur simple ***
begin
  inherited Create; // on hérite
  fError := C_NoInit; // erreur par défaut (chaîne non initialisée)
  fStartIndx := 1; // index par défaut de départ
  fIndx := -1; // index de travail
  fText := EmptyStr; // chaîne de travail vide
  WipeItems; // tableau vide
end;

constructor TGVTokens2.Create(const AText: string);
// *** constructeur avec initialisation ***
begin
  Create; // appel du constructeur simple
  Text := AText; // initialisation du texte de travail
end;

destructor TGVTokens2.Destroy;
// *** destructeur ***
begin
  inherited Destroy; // on hérite
end;

function TGVTokens2.GetEnumerator: TGVTokensEnumerator;
// *** énumération des éléments ***
begin
  Result := TGVTokensEnumerator.Create(fItemList);
end;

{ TGVTokens2 }

procedure TGVTokens2.Tokenize;
// *** répartit en éléments ***
var
  Ch: Char;
begin
 // erreur si rien à évaluer
 if (Error = C_NoInit) then
   raise EEvalException.Create(ME_NoInit);
 SetError(C_None); // pas d'erreur
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
end;

procedure TGVTokens2.GetVar;
// *** recherche d'une variable ***
var
  St: string;
  Res: Double;
  Err: TGVError;
  IndxTmp: Integer;
begin
  St := EmptyStr; // initialisation de la chaîne de travail
  Res := 0; // résultat par défaut
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
      OnGetVar(Self, St, Res, Err);
      Dec(fIndx); // réajustement du pointeur
      if (Err = C_None) then // pas d'erreur ?
        AddItem(FloatToStr(Res), cteVar) // on enregistre sa valeur et sa catégorie
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

procedure TGVTokens2.GetFunction;
// *** recherche d'une fonction ***  #### PROVISOIRE ####
var
  St: string;
  IndxTmp: Integer;
  I,Where: TGVFunctions;
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
 for I := Low(TGVFunctions) to High(TGVFunctions) do // on balaie les fonctions
 begin
   if AnsiCompareText(St, GVFunctionName[I]) = 0 then // égalité des chaînes ?
   begin
     Where := I; // sauvegarde de l'indice
     Break; // on sort de la boucle
   end;
 end;
 if (Where <> C_Unknown) then // trouvée ?
 begin
   case Ord(Where) of
     0..37: AddItem(AnsiUpperCase(St), cteFunction); // élément ajouté
     38: AddItem(FloatToStr(Pi), cteNumber); // nombre PI
     39: AddItem(IntToStr(CRTrue), cteNumber); // valeur VRAI
     40: AddItem(IntToStr(CRFalse), cteNumber); // valeur FAUX
     // fonctions infixées
     41: AddItem(MF_Or, cteOr); // ou logique
     42: AddItem(MF_And, cteAnd); // et logique
     43: AddItem(MF_Mod, cteMod); // modulo
   end;
 end
 else
 begin
   fIndx := IndxTmp; // on retrouve le début du mot
   AddItem(AnsiUppercase(St), cteUnknown); // on enregistre l'élément fautif
   SetError(C_BadFunction); // on signale l'erreur
 end;
end;

procedure TGVTokens2.GetNumber;
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
    St := St + Text[Indx]; // si oui, on ajoute le signe
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
  end;
  Dec(fIndx); // on revient sur le dernier caractère
  // on enregistre le résultat
  AddItem(St, cteNumber);
end;

procedure TGVTokens2.GetDelimGreater;
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

procedure TGVTokens2.GetDelimLower;
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

procedure TGVTokens2.GetDelimNot;
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

procedure TGVTokens2.Change;
// *** notification de changement ***
begin
  if Assigned(fOnChange) then // le gestionnaire existe-t-il ?
    fOnChange(Self); // on l'exécute
end;

{ TGVTokensEnumerator }

function TGVTokensEnumerator.GetCurrent: TGVBaseItem;
// *** retourne l'élément courant ***
begin
  Result := fLst[fIndex];
end;

constructor TGVTokensEnumerator.Create(const AValue: TGVItems);
// *** création de l'énumérateur ***
begin
  inherited Create;
  fIndex := -1;
  fLst := AValue;
end;

function TGVTokensEnumerator.MoveNext: Boolean;
// *** passe à l'élément suivant ***
begin
  Result := fIndex < High(fLst);
  if Result then
    Inc(fIndex);
end;

end.
