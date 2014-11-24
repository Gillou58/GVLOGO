{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Piles                                   |
  |                  Unité : GVStacks.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    24-11-2014 09:08:57                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


// GVStacks - part of GVLOGO
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
//  If not, see <http://www.gnu.org/licenses/>.

{$I GVDefines.inc}
  
unit GVStacks;

//
// Unité pour le traitement des piles
//
// ##############################################################
//

interface

uses
  Classes, SysUtils, Math, GVConsts
{$IFDEF Delphi}
    , System.Generics.Collections
{$ENDIF};

type

  { TStack }

  EGVStackException = class(Exception); // erreur
  // événement de la pile
  TGVStackEvent = procedure(Sender: TObject; Action: TGVStackNotification)
    of object;

  // pile générique

  { TGVStack }

{$IFNDEF Delphi}generic{$ENDIF}TGVStack<T> = class
   private
    fItems: array of T;
    fCount: Integer; // nombre d'éléments
    fCapacity: Integer; // capacité actuelle
    fOnNotify: TGVStackEvent; // notification
    procedure Expand; // expansion si nécessaire
    function GetCapacity: Integer; // capacité actuelle
    function GetItem(N: Integer): T;  // accès à un élément
    procedure SetCapacity(const Value: Integer); // fixe la capacité
  protected
    procedure Notify(Action: TGVStackNotification); virtual; // notification
    procedure DoPush(const Value: T); // empilement
    function DoPop: T; // dépilement
  public
    constructor Create; overload; // création
    destructor Destroy; override; // destruction
    procedure Clear; // nettoyage
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
    // notification d'un changement
    property OnNotify: TGVStackEvent read fOnNotify write fOnNotify;
    // accès direct à un élément
    property Item[N: Integer]: T read GetItem; default;
end;

  // piles spécialisées
  TGVIntegerStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<Integer>;
  TGVRealStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<Real>;
  TGVStringStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<string>;
  TGVDoubleStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<Double>;
  TGVExtendedStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<Extended>;
  TGVEvalStack = {$IFNDEF Delphi}specialize{$ENDIF} TGVStack<TGVBaseItem>;

implementation

{ TGVStack }

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Expand;
// *** expansion de la pile ***
var
  LCount: Integer;
begin
  LCount := Length(fItems) shl 1; // double la capacité
  if LCount < 0 then // débordement !
    raise EGVStackException.Create(ME_OutOfMemory) // erreur
  else
    SetLength(fItems, Max(LCount, CMinStack)); // ajuste la taille de la pile
end;

function TGVStack{$IFDEF Delphi}<T>{$ENDIF}.GetCapacity: Integer;
// *** renvoi de la capacité de la pile ***
begin
  Result := Length(fItems);
end;

function TGVStack{$IFDEF Delphi}<T>{$ENDIF}.GetItem(N: Integer): T;
// accès direct à un élément
begin
  if (N >= Count) or (N < 0) then
    raise EGVStackException.CreateFmt(ME_LowStack, [N + 1, Count]);
  Result := fItems[N];
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.SetCapacity(const Value: Integer);
// *** mise à jour de la capacité de la pile ***
begin
  if Value > Count then
    SetLength(fItems, Value); // ajustement de la valeur
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Notify
  (Action: TGVStackNotification);
// *** exécution de la procédure de notification si elle existe ***
begin
  if Assigned(fOnNotify) then
    fOnNotify(Self, Action);
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.DoPush(const Value: T);
// *** effectue l'empilement sans notification ***
begin
  if Count = Length(fItems) then // espace rempli ?
    Expand;
  fItems[Count] := Value; // nouvelle valeur stockée
  Inc(fCount); // pointe vers l'élément suivant
end;

function TGVStack{$IFDEF Delphi}<T>{$ENDIF}.DoPop: T;
// *** effectue le dépilement sans notification ***
begin
  if Count = 0 then
    raise EGVStackException.Create(ME_EmptyStack); // pile vide
  Result := fItems[Count - 1]; // on renvoie la valeur
  Dec(fCount); // on ajuste le pointeur
end;

constructor TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Create;
// *** création de la pile ***
begin
  inherited Create; // on hérite
  fCount := 0; // pas d'éléments
  fCapacity := CMinStack; // capacité au minimum
  SetLength(fItems, CMinStack); // espace au minimum
end;

destructor TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Destroy;
// *** destruction de la pile ***
begin
  Clear; // nettoyage
  inherited Destroy; // on hérite
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Clear;
// *** nettoyage de la pile ***
begin
  while Count > 0 do // tant qu'il y a des éléments...
    DoPop; // le haut de la pile est enlevé
  SetLength(fItems, CMinStack); // espace au minimum
  Notify(stCleared);
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Push(const Value: T);
// *** empilement d'un nouvel élément ***
begin
  DoPush(Value); // empilement
  Notify(stAdded); // on notifie le changement
end;

function TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Pop: T;
// *** renvoi du sommet de la pile en le détruisant ***
begin
  Result := DoPop; // effectue le dépilement
  Notify(stRemoved); // on notifie le changement
end;

function TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Peek: T;
// *** renvoi du sommet de la pile avec conservation ***
begin
  if Count = 0 then
    raise EGVStackException.Create(ME_EmptyStack); // pile vide
  Result := fItems[Count - 1];
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Drop;
// *** retrait du sommet de la pile sans affectation ***
begin
  Pop;
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Dup;
// *** duplication du sommet de la pile ***
begin
  Push(Peek);
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Swap;
// *** inversion des deux derniers éléments au sommet de la pile ***
var
  LItem1, LItem2: T;
begin
  if Count < 2 then
    raise EGVStackException.CreateFmt(ME_LowStack, [Count, 2]);
  // pile insuffisante
  LItem1 := DoPop; // on retire deux éléments
  LItem2 := DoPop;
  DoPush(LItem1); // qu'on rempile dans le sens inverse
  DoPush(LItem2);
  Notify(stChanged); // changement notifié
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Over;
// *** duplication de l'avant-dernier élément sur la pile ***
var
  LItem1, LItem2: T;
begin
  if Count < 2 then
    raise EGVStackException.CreateFmt(ME_LowStack, [Count, 2]);
  // pile insuffisante
  LItem1 := DoPop; // on retire le premier élément
  LItem2 := Peek; // on mémorise le second
  DoPush(LItem1); // on rempile le premier
  DoPush(LItem2); // on rempile le second
  Notify(stAdded); // changement notifié
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Rot;
// *** rotation des trois éléments au sommet de la pile ***
var
  LItem1, LItem2, LItem3: T;
begin
  if Count < 3 then
    raise EGVStackException.CreateFmt(ME_LowStack, [Count, 3]);
  // pile insuffisante
  LItem1 := DoPop; // on dépile trois éléments
  LItem2 := DoPop;
  LItem3 := DoPop;
  DoPush(LItem2); // on les rempile dans le bon ordre
  DoPush(LItem1);
  DoPush(LItem3);
  Notify(stChanged); // changement notifié
end;

procedure TGVStack{$IFDEF Delphi}<T>{$ENDIF}.Shrink;
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
