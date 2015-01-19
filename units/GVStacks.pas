{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des piles                    |
  |                  Unité : GVStacks.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVSTACKS - part of GVLOGO
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
  
unit GVStacks;
// Unité pour le traitement des piles
//
// L'unité comprend une pile générique et un ensemble de piles
// spécialisées.
//

interface

uses
  Classes, SysUtils, Math,
  GVConsts, // constantes communes
  GVErrConsts, // cosntantes d'erreurs
  GVErrors; // pour la gestion des erreurs

type
  // *** événement de la pile ***
  TGVStackEvent = procedure(Sender: TObject; Action: TGVStackNotification)
    of object;

  // *** pile générique ***
  generic TGVStack<T> = class(TObject)
  private
    fItems: array of T;
    fError: TGVErrors; // gestionnaire des erreurs
    fCount: Integer; // nombre d'éléments
    fCapacity: Integer; // capacité actuelle
    fOnNotify: TGVStackEvent; // notification
    procedure Expand; // expansion si nécessaire
    function GetCapacity: Integer; // capacité actuelle
    function GetItem(N: Integer): T;  // accès à un élément
    procedure SetCapacity(const Value: Integer); // fixe la capacité
    procedure SetItem(N: Integer; AValue: T);
  protected
    procedure Notify(Action: TGVStackNotification); virtual; // notification
    procedure DoPush(const Value: T); // empilement
    function DoPop: T; // dépilement
  public
    constructor Create; overload; // création
    destructor Destroy; override; // destruction
    procedure Clear; // nettoyage
    function IsEmpty: Boolean; inline; // pile vide ?
    procedure Push(const Value: T); // empilement avec notification
    function Pop: T; // dépilement avec notification
    function Peek: T; // sommet de la pile
    procedure Drop; // sommet de la pile éjecté
    procedure Dup; // duplication au sommet de la pile
    procedure Swap; // inversion au sommet de la pile
    procedure Over; // duplication de l'avant-dernier
    procedure Rot; // rotation au sommet de la pile
    procedure Shrink; // contraction de la pile
    function Needed(Nb: Integer): Boolean; // nombre d'éléments désirés
    property Count: Integer read fCount default 0; // compte des éléments
    // capacité de la pile
    property Capacity: Integer read GetCapacity write SetCapacity
      default CMinStack;
    // accès direct à un élément
    property Item[N: Integer]: T read GetItem write SetItem; default;
    // notification d'un changement
    property OnNotify: TGVStackEvent read fOnNotify write fOnNotify;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
end;

  // *** piles spécialisées ***
  TGVIntegerStack = specialize TGVStack<Integer>;
  TGVRealStack = specialize TGVStack<Real>;
  TGVStringStack = specialize TGVStack<string>;
  TGVDoubleStack = specialize TGVStack<Double>;
  TGVExtendedStack = specialize TGVStack<Extended>;
  TGVEvalStack = specialize TGVStack<TGVBaseItem>;

implementation

{ TGVStack }

procedure TGVStack.Expand;
// *** expansion de la pile ***
var
  LCount: Integer;
begin
  LCount := Length(fItems) shl 1; // double la capacité
  if LCount < 0 then // débordement !
    // [### Erreur: mémoire insuffisante ###]
    Error.SetError(CIE_OutOfMemory, ToString) //
  else
    SetLength(fItems, Max(LCount, CMinStack)); // ajuste la taille de la pile
end;

function TGVStack.GetCapacity: Integer;
// *** renvoi de la capacité de la pile ***
begin
  Result := Length(fItems);
end;

function TGVStack.GetItem(N: Integer): T;
// accès direct à un élément
begin
  if (N >= Count) or (N < 0) then
    // [### Erreur: pile insuffisante ###]
    Error.SetError(CIE_LowStack, ToString, N)
  else
    Result := fItems[N];
end;

procedure TGVStack.SetCapacity(const Value: Integer);
// *** mise à jour de la capacité de la pile ***
begin
  if Value > Count then
    SetLength(fItems, Value); // ajustement de la valeur
end;

procedure TGVStack.SetItem(N: Integer; AValue: T);
// *** mise à jour directe d'un élément ***
begin
  if (N >= Count) or (N < 0) then
    // [### Erreur: pile insuffisante ###]
    Error.SetError(CIE_LowStack, ToString, N)
  else
    fItems[N] := AValue;
end;

procedure TGVStack.Notify(Action: TGVStackNotification);
// *** exécution de la procédure de notification si elle existe ***
begin
  if Assigned(fOnNotify) then
    fOnNotify(Self, Action);
end;

procedure TGVStack.DoPush(const Value: T);
// *** effectue l'empilement sans notification ***
begin
  if Count = Length(fItems) then // espace rempli ?
    Expand;
  fItems[Count] := Value; // nouvelle valeur stockée
  Inc(fCount); // pointe vers l'élément suivant
end;

function TGVStack.DoPop: T;
// *** effectue le dépilement sans notification ***
begin
  Result := fItems[Count - 1]; // on renvoie la valeur
  Dec(fCount); // on ajuste le pointeur
end;

constructor TGVStack.Create;
// *** création de la pile ***
begin
  inherited Create; // on hérite
  fCount := 0; // pas d'éléments
  fCapacity := CMinStack; // capacité au minimum
  SetLength(fItems, CMinStack); // espace au minimum
  fError := TGVErrors.Create; // on crée le gestionnaire d'erreurs
end;

destructor TGVStack.Destroy;
// *** destruction de la pile ***
begin
  Clear; // nettoyage
  Error.Free; // on libère le gestionnaire d'erreurs
  inherited Destroy; // on hérite
end;

procedure TGVStack.Clear;
// *** nettoyage de la pile ***
begin
  while Count > 0 do // tant qu'il y a des éléments...
    DoPop; // le haut de la pile est enlevé
  SetLength(fItems, CMinStack); // espace au minimum
  Error.Clear; // pas d'erreur
  Notify(stCleared); // notification de l'état
end;

function TGVStack.IsEmpty: Boolean;
// *** pile vide ? ***
begin
  Result := (Count = 0);
end;

procedure TGVStack.Push(const Value: T);
// *** empilement d'un nouvel élément ***
begin
  DoPush(Value); // empilement
  Notify(stAdded); // on notifie le changement
end;

function TGVStack.Pop: T;
// *** renvoi du sommet de la pile en le détruisant ***
begin
  if Count = 0 then
    // [### Erreur: pile vide ###]
    Error.SetError(CIE_EmptyStack, ToString)
  else
  begin
    Result := DoPop; // effectue le dépilement
    Notify(stRemoved); // on notifie le changement
  end;
end;

function TGVStack.Peek: T;
// *** renvoi du sommet de la pile avec conservation ***
begin
  if Count = 0 then
    // [### Erreur: pile vide ###]
    Error.SetError(CIE_EmptyStack, ToString)
  else
    Result := fItems[Count - 1];
end;

procedure TGVStack.Drop;
// *** retrait du sommet de la pile sans affectation ***
begin
  Pop;
end;

procedure TGVStack.Dup;
// *** duplication du sommet de la pile ***
begin
  if Count = 0 then
    // [### Erreur: pile vide ###]
    Error.SetError(CIE_EmptyStack, ToString)
  else
    Push(Peek);
end;

procedure TGVStack.Swap;
// *** inversion des deux derniers éléments au sommet de la pile ***
var
  LItem1, LItem2: T;
begin
  if Count < 2 then
    // [### Erreur: pile insuffisante ###]
    Error.SetError(CIE_LowStack, ToString, 2)
  else
  begin
    LItem1 := DoPop; // on retire deux éléments
    LItem2 := DoPop;
    DoPush(LItem1); // qu'on rempile dans le sens inverse
    DoPush(LItem2);
    Notify(stChanged); // changement notifié
  end;
end;

procedure TGVStack.Over;
// *** duplication de l'avant-dernier élément sur la pile ***
var
  LItem1, LItem2: T;
begin
  if Count < 2 then
    // [### Erreur: pile insuffisante ###]
    Error.SetError(CIE_LowStack, ToString, 2)
  else
  begin
    LItem1 := DoPop; // on retire le premier élément
    LItem2 := Peek; // on mémorise le second
    DoPush(LItem1); // on rempile le premier
    DoPush(LItem2); // on rempile le second
    Notify(stAdded); // changement notifié
  end;
end;

procedure TGVStack.Rot;
// *** rotation des trois éléments au sommet de la pile ***
var
  LItem1, LItem2, LItem3: T;
begin
  if Count < 3 then
    // [### Erreur: pile insuffisante ###]
    Error.SetError(CIE_LowStack, ToString, 3)
  else
  begin
    LItem1 := DoPop; // on dépile trois éléments
    LItem2 := DoPop;
    LItem3 := DoPop;
    DoPush(LItem2); // on les rempile dans le bon ordre
    DoPush(LItem1);
    DoPush(LItem3);
    Notify(stChanged); // changement notifié
  end;
end;

procedure TGVStack.Shrink;
// *** réduction de la taille de la pile ***
begin
  SetLength(fItems, Max(Count, CMinStack)); // longueur ajustée
end;

function TGVStack.Needed(Nb: Integer): Boolean;
// *** nombre d'éléments désirés ***
begin
  Result := (Count >= Nb);
end;

end.
