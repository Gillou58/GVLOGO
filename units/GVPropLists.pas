{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Listes de propriétés                    |
  |                  Unité : GVPropLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 21:39:46                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVPropLists - part of GVLOGO
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

{$IFNDEF Delphi}
{$mode objfpc}{$H+}
{$ENDIF}

unit GVPropLists;

// Unité pour le traitement des listes de propriétés
//
// ##############################################################
//
// Une liste de propriétés associe des valeurs à des caractéristiques
// choisies pour un objet.
// Ainsi, un chien pourra être défini par les propriétés : race, âge, sexe.
// Chacune de ces propriétés aura une valeur particulière.
//
// Les listes de propriétés sont elles-mêmes à la base du noyau de
// l'interpréteur GVLOGO.
//

interface

uses
  Classes,
  SysUtils,
  GVConsts, // pour les constantes
  GVWords, // pour les mots
  GVLists; // pour les listes

type
  // *** classe des listes de propriétés pour GVLOGO
  EPropException = class(Exception); // exception

  // classe pour énumération

  { TGVPropListEnumerator }

  TGVPropListEnumerator = class(TObject)
  private
    fLst: TStringList;
    fIndex: Integer;
  protected
    function GetCurrent: string; virtual;
  public
    constructor Create(const Value: TStrings);
    destructor Destroy; override;
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;

  // classe principale
  TGVPropList = class(TObject)
  private
    fNames: TStringList; // listes
    fOnchange: TNotifyEvent; // notification de changement
    function GetLPByNum(N: Integer): string; // liste par numéro
    function GetLPByName(const Name: string): string; // liste par nom
    procedure SetLPByName(const Name, AValue: string); // écriture par nom
  protected
    procedure Change; dynamic; // changement
  public
    // constructeur de la classe
    constructor Create;
    // destructeur de la classe
    destructor Destroy; override; // destructeur
    // énumération
    function GetEnumerator: TGVPropListEnumerator;
    // nettoie les listes de propriétés
    procedure Clear;

    // *** listes de propriétés ***

    // renvoie la liste des listes de propriétés
    function ListP: string;
    // la liste existe-t-elle ?
    function IsListP(const Name: string): Boolean;
    // renvoie le numéro d'une liste de propriétés
    function NumListP(const Name: string): Integer;
    // crée ou met à jour la liste de propriétés
    function UpDateListP(const Name, Prop, Value: string): Boolean;
    // renvoie la valeur d'une liste
    function ValListP(const Name: string): string;
    // destruction d'une liste de propriétés
    function RemoveListP(const Name: string): Boolean;
    // valeur d'une liste de propriétés par numéro
    function ValNumListP(N: Integer; out Name, Value: string): Boolean;
    // la propriété N existe-t-elle?
    function IsListPByNum(N: Integer): Boolean;
    // renvoie le nombre de listes de propriétés
    function CountListP: Integer;
    // chargement des listes
    procedure LoadFromFile(const FileName: string);
    // sauvegarde des listes
    procedure SaveToFile(const FileName: string);

    // *** propriétés

    // la propriété existe-t-elle ?
    function IsProp(const Name, Prop: string): Boolean;
    // renvoie le numéro d'une propriété
    function NumProp(const Name, Prop: string): Integer;
    // valeur d'une propriété
    function ValProp(const Name, Prop: string): string; overload;
    function ValProp(const Name, Prop: string; out Value: string)
      : Boolean; overload;
    // destruction d'une propriété
    function RemoveProp(const Name, Prop: string): Boolean;
    // renvoie le nombre de propriétés attachées à une liste
    function CountProps(const Name: string): Integer;
    // valeur d'une propriété par numéro
    function ValNumProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    function ValNumProp(const Name: string; N: Integer): string; overload;
    // liste des propriétés d'une liste
    function ListOfProps(const Name: string): string;
    // nom d'une propriété par numéro
    function NameOfProp(const Name: string; N: Integer): string; overload;
    function NameOfProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    // changement dans la liste de propriétés
    property OnChange: TNotifyEvent read fOnchange write fOnchange;
    // liste de propriétés par numéro
    property ListPByNum[N: Integer]: string read GetLPByNum; default;
    // liste de propriétés par nom
    property LPByName[const Name: string]: string read GetLPByName write SetLPByName;
  end;

implementation

uses
  StrUtils
  {$IFNDEF Delphi}
  ,lazutf8
  {$ENDIF};

{ TGVPropListEnumerator }

function TGVPropListEnumerator.GetCurrent: string;
// *** retourne l'élément courant ***
begin
  Result := fLst.Names[fIndex];
end;

constructor TGVPropListEnumerator.Create(const Value: TStrings);
// *** création de l'énumérateur ***
begin
  fIndex := -1;
  fLst := TStringList.Create;
  fLst.NameValueSeparator := CSep; // définit le caractère séparateur
  fLst.AddStrings(Value);
end;

destructor TGVPropListEnumerator.Destroy;
// *** destruction de l'énumérateur ***
begin
  fLst.Free;
  inherited Destroy;
end;

function TGVPropListEnumerator.MoveNext: Boolean;
// *** passe à l'élément suivant ***
begin
  Result := fIndex < (fLst.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ ========================================================== }

{ TGVPropList }

function TGVPropList.GetLPByNum(N: Integer): string;
// *** renvoie la liste complète ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (N > 0) and (N <= fNames.Count) then // intervalle possible ?
    Result := fNames[N - 1]
  else
    raise EPropException.CreateFmt(ME_BadListP, [N]);
end;

procedure TGVPropList.Change;
// *** changement dans la liste ***
begin
  if Assigned(fOnchange) then
    fOnchange(Self);
end;

procedure TGVPropList.Clear;
// *** nettoie les listes de propriétés ***
begin
  fNames.Clear; // pas de noms
  Change; // changement notifié
end;

function TGVPropList.CountListP: Integer;
// *** nombre de listes de propriétés ***
begin
  Result := fNames.Count; // = nombre de noms enregistrés
end;

function TGVPropList.CountProps(const Name: string): Integer;
// *** nombre de propriétés attachées à une liste ***
var
  Lst: TGVList;
begin
  Lst := TGVList.Create;
  try
    Lst.Text := fNames.Values[Name]; // création
    Result := Lst.Count div 2; // compte les éléments
  finally
    Lst.Free; // libère la liste de travail
  end;
end;

constructor TGVPropList.Create;
// *** constructeur de la classe ***
begin
  inherited Create; // on hérite !
  fNames := TStringList.Create; // crée la liste interne
  fNames.NameValueSeparator := CSep; // définit le caractère séparateur
  fOnchange := nil;
end;

destructor TGVPropList.Destroy;
// *** destructeur de la classe ***
begin
  fOnchange := nil; // gestionnaire d'événements annulé
  fNames.Free; // libère les noms
  inherited Destroy; // on hérite !
end;

function TGVPropList.GetEnumerator: TGVPropListEnumerator;
// *** mise en place de l'énumération ***
begin
  Result := TGVPropListEnumerator.Create(fNames);
end;

function TGVPropList.GetLPByName(const Name: string): string;
// *** liste de propriétés par nom ***
var
  I: Integer;
begin
  Result := EmptyStr; // chaîne vide par défaut
  I := fNames.IndexOfName(Name); // recherche de l'existence
  if I <> -1 then // si trouvée
    Result := fNames[I]; // on renvoie sa valeur
end;

procedure TGVPropList.SetLPByName(const Name, AValue: string);
// *** définit une liste de propriétés directement ***
begin
  fNames.Values[Name] := AValue;
end;

function TGVPropList.IsListPByNum(N: Integer): Boolean;
// *** la propriété N existe-t-elle ? ***
begin
  Result := (N > 0) and (N <= fNames.Count); // dans les bornes ?
end;

function TGVPropList.IsListP(const Name: string): Boolean;
// *** la liste existe-t-elle ? ***
begin
  Result := (fNames.IndexOfName(Name) <> -1);
end;

function TGVPropList.IsProp(const Name, Prop: string): Boolean;
// *** la propriété existe-t-elle ? ***
begin
  Result := (NumProp(Name, Prop) <> -1); // test
end;

function TGVPropList.ListOfProps(const Name: string): string;
// *** liste des propriétés d'une liste ***
var
  I: Integer;
  Lst: TGVList;
begin
  Result := CBeginList; // début de la liste
  try
    Lst := TGVList.Create; // création de la liste de travail
    try
      Lst.Text := ValListP(Name); // récupère la liste
      for I := 0 to Lst.Count - 1 do // on balaie les propriétés
        if not Odd(I) then // propriétés par paires
          Result := Result + Lst[I] + CBlank; // on ajoute le nom
    finally
      Lst.Free; // libération de la liste de travail
    end;
  finally
    Result := TrimRight(Result) + CEndList; // fin de la liste
  end;
end;

function TGVPropList.ListP: string;
// *** renvoie la liste des listes de propriétés ***
var
  S: string;
begin
  Result := CBeginList;
  try
    for S in fNames do // on balaie la liste
      if (Result <> CBeginList) then
        Result := Result + CBlank + S // pour construire la chaîne
      else
        Result := Result + S;
    // on remplace le caractère séparateur
    Result := AnsiReplaceText(Result, CSep, CBlank);
  finally
    Result := Result + CEndList;
  end;
end;

procedure TGVPropList.LoadFromFile(const FileName: string);
// ***  chargement depuis un fichier ***
var
  L: TStringList;
  Lst: TGVList;
  S, GVName: string;
  I, J: Integer;
begin
  L := TStringList.Create; // liste de travail
  L.NameValueSeparator := CSep; // séparateur pour les propriétés
  try
    // change l'extension si nécessaire
    if PosEx(CDot, FileName) = 0 then // si aucune extension
      S := FileName + CExtPl // on en ajoute une (.GPL)
    else
      S := FileName;
    L.LoadFromFile(S); // charge le fichier
    if L[0] <> CHeader then
      // si fichier dans un mauvais format
      raise EPropException.CreateFmt(ME_BadFormat, [S, L[0]]);
    L.Delete(0); // on élimine l'entête
    Lst := TGVList.Create;
    try
      for I := 0 to L.Count - 1 do
      begin
        GVName := L.Names[I]; // on a le nom
        Lst.Text := L.Values[GVName]; // et les propriétés
        for J := 0 to Lst.Count - 1 do
          if not Odd(J) then
            UpDateListP(GVName, Lst[J], Lst[J + 1]); // on met à jour
      end;
    finally
      Lst.Free;
    end;
  finally
    L.Free; // libère la liste de travail
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer): string;
// *** nom d'une propriété par numéro ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // chaîne vide par défaut
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Result := Lst[(N - 1) * 2]; // propriétés par paires
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** nom d'une propriété par numéro (avec contrôle) ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Prop := Lst[(N - 1) * 2]; // propriétés par paires
      Result := True; // tout est Ok
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

function TGVPropList.NumListP(const Name: string): Integer;
// *** recherche d'une liste de propriétés ***
begin
  Result := fNames.IndexOfName(Name) + 1;
end;

function TGVPropList.NumProp(const Name, Prop: string): Integer;
// *** recherche du numéro d'une propriété ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := -1; // on suppose une erreur
  Lst := TGVList.Create;
  try
    Lst.Text := ValListP(Name); // recherche de la valeur de la liste
    for I := 0 to Lst.Count - 1 do // analyse de la liste
      // cherche les éléments pairs et teste la propriété
{$IFDEF Delphi}
      if not Odd(I) and AnsiSameText(Lst[I], Prop) then
{$ELSE}
      if not Odd(I) and (UTF8CompareText(Lst[I], Prop) = 0) then
{$ENDIF}
      begin
        Result := (I div 2) + 1;
        break;
      end;
  finally
    Lst.Free; // libération de la liste de travail
  end;
end;

function TGVPropList.RemoveListP(const Name: string): Boolean;
// *** détruit la liste de propriétés ***
var
  N: Integer;
begin
  Result := False; // suppose une erreur
  N := fNames.IndexOfName(Name); // on cherche la liste
  if (N <> -1) then
  begin
    fNames.Delete(N); // on la détruit
    Result := True;
    Change; // on notifie les changements
  end;
end;

function TGVPropList.RemoveProp(const Name, Prop: string): Boolean;
// *** détruit une propriété ***
var
  Lst: TGVList;
  fUtil: TGVListUtils;
begin
  Result := False; // suppose une erreur
  fUtil := TGVListUtils.Create;
  try
    Lst := TGVList.Create;
    try
      if IsListP(Name) then // est-ce une liste existante ?
      begin
        Lst.Text := ValListP(Name); // liste temporaire
        // on retire deux éléments si possible
        if IsProp(Name, Prop) then
        begin
          fNames.Values[Name] := Lst.TwoDelete(NumProp(Name, Prop) * 2 - 1);
          if (ValListP(Name) = fUtil.EmptyList) then
            // on détruit la liste si vide
            Result := RemoveListP(Name);
          Result := True;
          Change; // on notifie les changements
        end;
      end;
    finally
      Lst.Free; // on libère la liste de travail
    end;
  finally
    fUtil.Free;
  end;
end;

procedure TGVPropList.SaveToFile(const FileName: string);
// *** sauvegarde des listes de propriétés ***
var
  L: TStringList;
  S: string;
begin
  L := TStringList.Create; // liste de travail
  try
    // change l'extension si nécessaire
    if PosEx(CDot, FileName) = 0 then
      S := FileName + CExtPl
    else
      S := FileName;
    L.Add(CHeader); // ajoute l'entête
    L.AddStrings(fNames); // ajoute les listes
    L.SaveToFile(S); // sauve la liste
  finally
    L.Free; // libère la liste de travail
  end;
end;

function TGVPropList.UpDateListP(const Name, Prop, Value: string): Boolean;
// *** crée ou met à jour la liste de propriétés ***
var
  Lst: TGVList;
  N: Integer;
  fWord: TGVWord;
  fUtil: TGVListUtils;
begin
  fUtil := TGVListUtils.Create;
  try
    fWord := TGVWord.Create;
    try
      // test des valeurs
      Result := (Name <> EmptyStr) and (Prop <> EmptyStr) and
        (Value <> EmptyStr) and fWord.IsValid(Name) and fWord.IsValid(Prop) and
        fUtil.IsValidValue(Value);
      if Result then // si tout est Ok
        if IsListP(Name) then // la liste existe-t-elle déjà ?
        begin // oui
          Lst := TGVList.Create;
          try
            Lst.Text := ValListP(Name); // cherche les propriétés
            N := NumProp(Name, Prop) - 1;
            if N = -2 then // propriété trouvée ?
              fNames.Values[Name] := Lst.TwoAdd(Prop, Value) // on ajoute
            else
              // ou on remplace
              fNames.Values[Name] := Lst.ReplaceItem(2 * (N + 1), Value);
            Change; // notifie le changement
          finally
            Lst.Free; // on libère la liste de travail
          end;
        end
        else // création de la nouvelle liste
        begin
          fNames.Add(Name + fNames.NameValueSeparator + CBeginList + Prop +
            CBlank + Value + CEndList); // on crée le nom
          Change; // notifie le changement
        end;
    finally
      fWord.Free;
    end;
  finally
    fUtil.Free;
  end;
end;

function TGVPropList.ValListP(const Name: string): string;
// *** valeur d'une liste de propriétés ***
begin
  Result := fNames.Values[Name]; // retourne les propriétés
end;

function TGVPropList.ValNumListP(N: Integer; out Name, Value: string): Boolean;
// *** valeur d'une liste de propriétés par numéro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= fNames.Count) then
  begin
    Name := fNames.Names[N - 1]; // d'abord le nom
    Value := fNames.ValueFromIndex[N - 1]; // puis les propriétés
    Result := True; // tout va bien
  end;
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer): string;
// *** valeur d'une propriété par numéro ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // chaîne vide par défaut
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Result := Lst[(N - 1) * 2 + 1]; // propriétés par paires
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

function TGVPropList.ValProp(const Name, Prop: string;
  out Value: string): Boolean;
// *** valeur d'une propriété (version avec contrôle) ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := False; // échec par défaut
  Lst := TGVList.Create;
  try
    if IsProp(Name, Prop) then // est-ce une propriété valide ?
    begin
      Lst.Text := ValListP(Name); // liste de travail
      for I := 0 to Lst.Count - 1 do // on examine la liste
        // si c'est une propriété et qu'elle correspond à celle cherchée
{$IFDEF Delphi}
        if (not Odd(I)) and AnsiSameText(Lst[I], Prop) then
{$ELSE}
        if (not Odd(I)) and (UTF8CompareText(Lst[I], Prop) = 0) then
{$ENDIF}
        begin
          Value := Lst[I + 1]; // élément trouvé
          Result := True; // on a trouvé
          break; // on sort de la boucle
        end;
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** valeur d'une propriété par numéro ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Prop := Lst[(N - 1) * 2 + 1]; // propriétés par paires
      Result := True; // tout est Ok
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

function TGVPropList.ValProp(const Name, Prop: string): string;
// *** valeur d'une propriété ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := EmptyStr; // chaîne vide par défaut
  Lst := TGVList.Create;
  try
    if IsProp(Name, Prop) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      for I := 0 to Lst.Count - 1 do // on examine la liste
        // si c'est une propriété et qu'elle correspond à celle cherchée
{$IFDEF Delphi}
        if (not Odd(I)) and AnsiSameText(Lst[I], Prop) then
{$ELSE}
        if (not Odd(I)) and (UTF8CompareText(Lst[I], Prop) = 0) then
{$ENDIF}
        begin
          Result := Lst[I + 1]; // élément trouvé
          break; // on sort de la boucle
        end;
    end;
  finally
    Lst.Free; // on libère la liste
  end;
end;

end.
