{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : gestion des variables locales           |
  |                  Unité : GVLocVars.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVLOCVARS - part of GVLOGO
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

unit GVLocVars;
// Unité de traitement des variables locales
//
// L'unité traite les variables définies par les procédures,
// qu'elles soient paramètres ou définitions locales.
//

interface

uses
  Classes, SysUtils,
  GVConsts, // constantes communes
  GVStacks, // piles
  GVErrConsts, // constantes des erreurs
  GVErrors; // erreurs

type
  // *** classe pour les variables locales ***
  TGVLocVars = class(TObject)
  strict private
    fCount: Integer; // compteur de variables attendues
    fWhere: Integer; // résultat dernière recherche
    fError: TGVErrors; // enregistrement d'une erreur
    fNames: TGVStringStack; // liste des noms de variables locales
    fValues: TGVStringStack; // liste des valeurs des variables locales
    fStack: TGVIntegerStack; // pile des groupes de variables locales
    fOnChange: TNotifyEvent; // gestionnaire de changement
    procedure SetCount(AValue: Integer); // compte des variables attendues
    // localisation d'une variable locale
    function Where(const Name: string): Integer;
  protected
    procedure Change; // changement notifié
  public
    // création
    constructor Create;
    // destruction
    destructor Destroy; override;
    // nettoyage
    procedure Clear;
    // réservation pour N variables locales
    procedure AddLocNumber(N: Integer);
    // réservation pour une nouvelle variable locale
    procedure AddNewLocNumber;
    // est-ce une variable locale ?
    function IsLocVar(const Name: string): Boolean;
    // nombre de variables locales
    function LocVarsCount: Integer;
    // affectation à une variable locale
    function AddLocVar(const Name, Value: string): Boolean;
    // mise à jour d'une variable locale
    function UpdateLocVar(const Name, Value: string): Boolean;
    // mise à jour directe d'une variable locale (existence préétablie)
    procedure DirectUpdateLocVar(const Value: string);
    // valeur d'une variable locale
    function ValLocVar(const Name: string; out Value: string): Boolean;
      overload;
    function ValLocVar(const Name: string): string; overload;
    // valeur directe d'une variable (existence préétablie)
    function DirectValLocVar: string;
    // destruction du dernier groupe de variables locales
    procedure DelLastGroup;
    // liste des variables locales
    function LocVarsToList: string;
    // événement si changement
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
    // compteur de variables attendues
    property Count: Integer read fCount write SetCount default 0;
  end;

implementation

uses
  GVWords, // mots
  GVPrimConsts; // constantes des primitives

{ TGVLocVars }

function TGVLocVars.Where(const Name: string): Integer;
// *** localisation d'une variable locale ***
var
  Li: Integer;
  LW: TGVWord;
begin
  Result := -1; // non trouvée par défaut
  LW := TGVWord.Create; // création du mot de travail
  try
    LW.Text := Name; // mot normalisé
    LW.Text := LW.WithoutColon; // sans les deux points
    LW.Text := LW.WithoutQuote; // sans les guillemets
    // on balaie les variables locales s'il y en a
    if  (not fStack.IsEmpty) and (fStack.Peek <> 0) then
    begin
      for Li :=  1 to fStack.Peek do
        if AnsiSameText(fNames[fNames.Count - Li], LW.Text) then // trouvé ?
        begin
          fWhere := fNames.Count - Li; // emplacement trouvé et mémorisé
          Result := fWhere; // résultat
          Break; // on a fini
        end;
    end;
  finally
    LW.Free; // libération du mot de travail
  end;
end;

procedure TGVLocVars.SetCount(AValue: Integer);
// *** compteur de variables attendues ***
begin
  if fCount = AValue then // aucun changement ?
    Exit; // on sort
  fCount := AValue; // nouvelle valeur
end;

procedure TGVLocVars.Change;
// *** changement dans le noyau ***
begin
  if Assigned(fOnChange) then // si le gestionnaire existe
    fOnChange(Self); // on l'exécute
end;

constructor TGVLocVars.Create;
// *** création ***
begin
  fError := TGVErrors.Create;
  // on crée la zone des variables locales
  fNames := TGVStringStack.Create; // création des piles
  fValues := TGVStringStack.Create;
  fStack := TGVIntegerStack.Create;
  fCount := 0; // compteur à zéro
end;

destructor TGVLocVars.Destroy;
// *** destruction ***
begin
  fNames.Free; // libération des piles des variables locales
  fValues.Free;
  fStack.Free;
  fError.Free; // libération des erreurs
  inherited Destroy; // on hérite
end;

procedure TGVLocVars.AddLocNumber(N: Integer);
// *** réservation pour N variables locales ***
begin
  if Count = 0 then // compteur à zéro ?
  begin
    fStack.Push(N); // on empile le nombre de nouvelles variables
    Count := N; // compteur à jour
  end
  else
    // [### Erreur: mauvais compteur ###]
    Error.SetError(CIE_MemLocVar, ToString);
end;

procedure TGVLocVars.AddNewLocNumber;
// *** nouvelle place pour une variable locale ***
begin
  if fStack.IsEmpty then // pas de variables locales en cours ?
    // [### Erreur: allocation interdite ###]
    Error.SetError(CE_LocVarForbidden, P_Loc)
  else
  begin
    Count := 1; // place allouée
    fStack.Push(fStack.Pop + 1); // sommet de pile incrémenté
  end;
end;

function TGVLocVars.IsLocVar(const Name: string): Boolean;
// *** l'objet est-il une variable locale ? ***
begin
   Result := (Where(Name) <> -1);
end;

function TGVLocVars.LocVarsCount: Integer;
// *** nombre de variables locales ***
begin
  if fStack.Count <> 0 then
    Result := fStack.Peek
  else
    Result := 0;
end;

function TGVLocVars.AddLocVar(const Name, Value: string): Boolean;
// *** ajout d'une variable locale ***
var
  LW: TGVWord;
begin
  Result := False; // erreur présumée
  if Count = 0 then // plus de variables attendues ?
  begin
    // [### Erreur: mauvais compteur ###]
    Error.SetError(CIE_MemLocVar, ToString);
    Exit;
  end
  else
    Count := Count - 1; // on décompte la variable
  LW := TGVWord.Create; // mot de travail créé
  try
    LW.Text := Name; // mot normalisé
    LW.Text := LW.WithoutColon; // on supprime les deux points
    LW.Text := LW.WithoutQuote; // et les éventuels guillemets
    if LW.IsValidIdent then // le nom est-il correct ?
    begin
      // on crée la variable locale
      fNames.Push(LW.Text); // on empile le nom
      fValues.Push(Value); // puis la valeur
      Result := True;
      Change; // notifie le changement si effectif
    end
    else
      // [### Erreur: nom incorrect ###]
      Error.SetError(CE_BadName, Name);
  finally
    LW.Free; // libération du mot de travail
  end;
end;

function TGVLocVars.UpdateLocVar(const Name, Value: string): Boolean;
// *** mise à jour d'une variable locale ***
begin
  Result := False; // erreur présumée
  if IsLocVar(Name) then // est-ce une variable locale ?
  begin
    fValues[fWhere] := Value;
    Result := True;
    Change; // changement notifié
  end
  else
    // [### Erreur: ce n'est pas une variable locale ###]
    Error.SetError(CE_UnknownVar, Name);
end;

procedure TGVLocVars.DirectUpdateLocVar(const Value: string);
// *** mise à jour directe d'une variable locale ***
// => suppose un test préalable de son existence
begin
  fValues[fWhere] := Value; // nouvelle valeur
  Change; // changement notifié
end;

function TGVLocVars.ValLocVar(const Name: string; out Value: string): Boolean;
// *** valeur d'une variable locale ***
begin
  Result := False;
  if IsLocVar(Name) then // est-ce une variable locale ?
  begin
    // on rend sa valeur
    Value := fValues[fWhere];
    Result := True;
  end
  else
    // [### Erreur: ce n'est pas une variable locale ###]
    Error.SetError(CE_UnknownVar, Name);
end;

function TGVLocVars.ValLocVar(const Name: string): string;
// *** valeur d'une variable locale ***
begin
  if IsLocVar(Name) then // est-ce une variable locale ?
    // on rend sa valeur
    Result := fValues[fWhere]
  else
    // [### Erreur: ce n'est pas une variable locale ###]
    Error.SetError(CE_UnknownVar, Name);
end;

function TGVLocVars.DirectValLocVar: string;
// *** valeur directe d'une variable ***
// => suppose un test préalable de son existence
begin
  Result := fValues[fWhere];
end;

procedure TGVLocVars.DelLastGroup;
// *** suppression des dernières variables locales ***
var
  Li: Integer;
begin
  if fStack.IsEmpty or (fNames.Count < fStack.Peek) then
    // [### Erreur: pas assez de variables internes ###]
    Error.SetError(CIE_LowStack, ToString)
  else
  if fStack.Peek = 0 then // pas de variables locales ?
    fStack.Pop // on dépile l'indicateur
  else
    for Li := 1 to fStack.Pop do  // on supprime les variables
    begin
      fNames.Pop; // nom dépilé
      fValues.Pop; // valeur dépilée
    end;
  Change; // changement notifié
end;

procedure TGVLocVars.Clear;
// *** destruction de toutes les variables locales ***
begin
  fNames.Clear; // piles à zéro
  fValues.Clear;
  fStack.Clear;
  Error.Clear; // pas d'erreur
  Change; // changement notifié
end;

function TGVLocVars.LocVarsToList: string;
// *** liste des variables locales ***
var
  Li: Integer;
begin
  Result := CBeginList;
  try
    if LocVarsCount <> 0 then // des variables ?
      for Li := 1 to LocVarsCount do // on balaie
        Result := Result + fNames[fNames.Count - Li] + CBlank; // liste
  finally
    Result := TrimRight(Result) + CEndList; // liste fermée
  end;
end;

end.

