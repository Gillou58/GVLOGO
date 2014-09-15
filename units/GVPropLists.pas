{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Listes de propri�t�s                    |
  |                  Unit� : GVPropLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
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

// Unit� pour le traitement des listes de propri�t�s
//
// ##############################################################
//
// Une liste de propri�t�s associe des valeurs � des caract�ristiques
// choisies pour un objet.
// Ainsi, un chien pourra �tre d�fini par les propri�t�s : race, �ge, sexe.
// Chacune de ces propri�t�s aura une valeur particuli�re.
//
// Les listes de propri�t�s sont elles-m�mes � la base du noyau de
// l'interpr�teur GVLOGO.
//

interface

uses
  Classes,
  SysUtils,
  GVConsts, // pour les constantes
  GVWords, // pour les mots
  GVLists; // pour les listes

type
  // *** classe des listes de propri�t�s pour GVLOGO
  EPropException = class(Exception); // exception

  // classe pour �num�ration

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
    function GetLPByNum(N: Integer): string; // liste par num�ro
    function GetLPByName(const Name: string): string; // liste par nom
    procedure SetLPByName(const Name, AValue: string); // �criture par nom
  protected
    procedure Change; dynamic; // changement
  public
    // constructeur de la classe
    constructor Create;
    // destructeur de la classe
    destructor Destroy; override; // destructeur
    // �num�ration
    function GetEnumerator: TGVPropListEnumerator;
    // nettoie les listes de propri�t�s
    procedure Clear;

    // *** listes de propri�t�s ***

    // renvoie la liste des listes de propri�t�s
    function ListP: string;
    // la liste existe-t-elle ?
    function IsListP(const Name: string): Boolean;
    // renvoie le num�ro d'une liste de propri�t�s
    function NumListP(const Name: string): Integer;
    // cr�e ou met � jour la liste de propri�t�s
    function UpDateListP(const Name, Prop, Value: string): Boolean;
    // renvoie la valeur d'une liste
    function ValListP(const Name: string): string;
    // destruction d'une liste de propri�t�s
    function RemoveListP(const Name: string): Boolean;
    // valeur d'une liste de propri�t�s par num�ro
    function ValNumListP(N: Integer; out Name, Value: string): Boolean;
    // la propri�t� N existe-t-elle?
    function IsListPByNum(N: Integer): Boolean;
    // renvoie le nombre de listes de propri�t�s
    function CountListP: Integer;
    // chargement des listes
    procedure LoadFromFile(const FileName: string);
    // sauvegarde des listes
    procedure SaveToFile(const FileName: string);

    // *** propri�t�s

    // la propri�t� existe-t-elle ?
    function IsProp(const Name, Prop: string): Boolean;
    // renvoie le num�ro d'une propri�t�
    function NumProp(const Name, Prop: string): Integer;
    // valeur d'une propri�t�
    function ValProp(const Name, Prop: string): string; overload;
    function ValProp(const Name, Prop: string; out Value: string)
      : Boolean; overload;
    // destruction d'une propri�t�
    function RemoveProp(const Name, Prop: string): Boolean;
    // renvoie le nombre de propri�t�s attach�es � une liste
    function CountProps(const Name: string): Integer;
    // valeur d'une propri�t� par num�ro
    function ValNumProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    function ValNumProp(const Name: string; N: Integer): string; overload;
    // liste des propri�t�s d'une liste
    function ListOfProps(const Name: string): string;
    // nom d'une propri�t� par num�ro
    function NameOfProp(const Name: string; N: Integer): string; overload;
    function NameOfProp(const Name: string; N: Integer; out Prop: string)
      : Boolean; overload;
    // changement dans la liste de propri�t�s
    property OnChange: TNotifyEvent read fOnchange write fOnchange;
    // liste de propri�t�s par num�ro
    property ListPByNum[N: Integer]: string read GetLPByNum; default;
    // liste de propri�t�s par nom
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
// *** retourne l'�l�ment courant ***
begin
  Result := fLst.Names[fIndex];
end;

constructor TGVPropListEnumerator.Create(const Value: TStrings);
// *** cr�ation de l'�num�rateur ***
begin
  fIndex := -1;
  fLst := TStringList.Create;
  fLst.NameValueSeparator := CSep; // d�finit le caract�re s�parateur
  fLst.AddStrings(Value);
end;

destructor TGVPropListEnumerator.Destroy;
// *** destruction de l'�num�rateur ***
begin
  fLst.Free;
  inherited Destroy;
end;

function TGVPropListEnumerator.MoveNext: Boolean;
// *** passe � l'�l�ment suivant ***
begin
  Result := fIndex < (fLst.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ ========================================================== }

{ TGVPropList }

function TGVPropList.GetLPByNum(N: Integer): string;
// *** renvoie la liste compl�te ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
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
// *** nettoie les listes de propri�t�s ***
begin
  fNames.Clear; // pas de noms
  Change; // changement notifi�
end;

function TGVPropList.CountListP: Integer;
// *** nombre de listes de propri�t�s ***
begin
  Result := fNames.Count; // = nombre de noms enregistr�s
end;

function TGVPropList.CountProps(const Name: string): Integer;
// *** nombre de propri�t�s attach�es � une liste ***
var
  Lst: TGVList;
begin
  Lst := TGVList.Create;
  try
    Lst.Text := fNames.Values[Name]; // cr�ation
    Result := Lst.Count div 2; // compte les �l�ments
  finally
    Lst.Free; // lib�re la liste de travail
  end;
end;

constructor TGVPropList.Create;
// *** constructeur de la classe ***
begin
  inherited Create; // on h�rite !
  fNames := TStringList.Create; // cr�e la liste interne
  fNames.NameValueSeparator := CSep; // d�finit le caract�re s�parateur
  fOnchange := nil;
end;

destructor TGVPropList.Destroy;
// *** destructeur de la classe ***
begin
  fOnchange := nil; // gestionnaire d'�v�nements annul�
  fNames.Free; // lib�re les noms
  inherited Destroy; // on h�rite !
end;

function TGVPropList.GetEnumerator: TGVPropListEnumerator;
// *** mise en place de l'�num�ration ***
begin
  Result := TGVPropListEnumerator.Create(fNames);
end;

function TGVPropList.GetLPByName(const Name: string): string;
// *** liste de propri�t�s par nom ***
var
  I: Integer;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  I := fNames.IndexOfName(Name); // recherche de l'existence
  if I <> -1 then // si trouv�e
    Result := fNames[I]; // on renvoie sa valeur
end;

procedure TGVPropList.SetLPByName(const Name, AValue: string);
// *** d�finit une liste de propri�t�s directement ***
begin
  fNames.Values[Name] := AValue;
end;

function TGVPropList.IsListPByNum(N: Integer): Boolean;
// *** la propri�t� N existe-t-elle ? ***
begin
  Result := (N > 0) and (N <= fNames.Count); // dans les bornes ?
end;

function TGVPropList.IsListP(const Name: string): Boolean;
// *** la liste existe-t-elle ? ***
begin
  Result := (fNames.IndexOfName(Name) <> -1);
end;

function TGVPropList.IsProp(const Name, Prop: string): Boolean;
// *** la propri�t� existe-t-elle ? ***
begin
  Result := (NumProp(Name, Prop) <> -1); // test
end;

function TGVPropList.ListOfProps(const Name: string): string;
// *** liste des propri�t�s d'une liste ***
var
  I: Integer;
  Lst: TGVList;
begin
  Result := CBeginList; // d�but de la liste
  try
    Lst := TGVList.Create; // cr�ation de la liste de travail
    try
      Lst.Text := ValListP(Name); // r�cup�re la liste
      for I := 0 to Lst.Count - 1 do // on balaie les propri�t�s
        if not Odd(I) then // propri�t�s par paires
          Result := Result + Lst[I] + CBlank; // on ajoute le nom
    finally
      Lst.Free; // lib�ration de la liste de travail
    end;
  finally
    Result := TrimRight(Result) + CEndList; // fin de la liste
  end;
end;

function TGVPropList.ListP: string;
// *** renvoie la liste des listes de propri�t�s ***
var
  S: string;
begin
  Result := CBeginList;
  try
    for S in fNames do // on balaie la liste
      if (Result <> CBeginList) then
        Result := Result + CBlank + S // pour construire la cha�ne
      else
        Result := Result + S;
    // on remplace le caract�re s�parateur
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
  L.NameValueSeparator := CSep; // s�parateur pour les propri�t�s
  try
    // change l'extension si n�cessaire
    if PosEx(CDot, FileName) = 0 then // si aucune extension
      S := FileName + CExtPl // on en ajoute une (.GPL)
    else
      S := FileName;
    L.LoadFromFile(S); // charge le fichier
    if L[0] <> CHeader then
      // si fichier dans un mauvais format
      raise EPropException.CreateFmt(ME_BadFormat, [S, L[0]]);
    L.Delete(0); // on �limine l'ent�te
    Lst := TGVList.Create;
    try
      for I := 0 to L.Count - 1 do
      begin
        GVName := L.Names[I]; // on a le nom
        Lst.Text := L.Values[GVName]; // et les propri�t�s
        for J := 0 to Lst.Count - 1 do
          if not Odd(J) then
            UpDateListP(GVName, Lst[J], Lst[J + 1]); // on met � jour
      end;
    finally
      Lst.Free;
    end;
  finally
    L.Free; // lib�re la liste de travail
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer): string;
// *** nom d'une propri�t� par num�ro ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Result := Lst[(N - 1) * 2]; // propri�t�s par paires
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

function TGVPropList.NameOfProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** nom d'une propri�t� par num�ro (avec contr�le) ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Prop := Lst[(N - 1) * 2]; // propri�t�s par paires
      Result := True; // tout est Ok
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

function TGVPropList.NumListP(const Name: string): Integer;
// *** recherche d'une liste de propri�t�s ***
begin
  Result := fNames.IndexOfName(Name) + 1;
end;

function TGVPropList.NumProp(const Name, Prop: string): Integer;
// *** recherche du num�ro d'une propri�t� ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := -1; // on suppose une erreur
  Lst := TGVList.Create;
  try
    Lst.Text := ValListP(Name); // recherche de la valeur de la liste
    for I := 0 to Lst.Count - 1 do // analyse de la liste
      // cherche les �l�ments pairs et teste la propri�t�
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
    Lst.Free; // lib�ration de la liste de travail
  end;
end;

function TGVPropList.RemoveListP(const Name: string): Boolean;
// *** d�truit la liste de propri�t�s ***
var
  N: Integer;
begin
  Result := False; // suppose une erreur
  N := fNames.IndexOfName(Name); // on cherche la liste
  if (N <> -1) then
  begin
    fNames.Delete(N); // on la d�truit
    Result := True;
    Change; // on notifie les changements
  end;
end;

function TGVPropList.RemoveProp(const Name, Prop: string): Boolean;
// *** d�truit une propri�t� ***
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
        // on retire deux �l�ments si possible
        if IsProp(Name, Prop) then
        begin
          fNames.Values[Name] := Lst.TwoDelete(NumProp(Name, Prop) * 2 - 1);
          if (ValListP(Name) = fUtil.EmptyList) then
            // on d�truit la liste si vide
            Result := RemoveListP(Name);
          Result := True;
          Change; // on notifie les changements
        end;
      end;
    finally
      Lst.Free; // on lib�re la liste de travail
    end;
  finally
    fUtil.Free;
  end;
end;

procedure TGVPropList.SaveToFile(const FileName: string);
// *** sauvegarde des listes de propri�t�s ***
var
  L: TStringList;
  S: string;
begin
  L := TStringList.Create; // liste de travail
  try
    // change l'extension si n�cessaire
    if PosEx(CDot, FileName) = 0 then
      S := FileName + CExtPl
    else
      S := FileName;
    L.Add(CHeader); // ajoute l'ent�te
    L.AddStrings(fNames); // ajoute les listes
    L.SaveToFile(S); // sauve la liste
  finally
    L.Free; // lib�re la liste de travail
  end;
end;

function TGVPropList.UpDateListP(const Name, Prop, Value: string): Boolean;
// *** cr�e ou met � jour la liste de propri�t�s ***
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
        if IsListP(Name) then // la liste existe-t-elle d�j� ?
        begin // oui
          Lst := TGVList.Create;
          try
            Lst.Text := ValListP(Name); // cherche les propri�t�s
            N := NumProp(Name, Prop) - 1;
            if N = -2 then // propri�t� trouv�e ?
              fNames.Values[Name] := Lst.TwoAdd(Prop, Value) // on ajoute
            else
              // ou on remplace
              fNames.Values[Name] := Lst.ReplaceItem(2 * (N + 1), Value);
            Change; // notifie le changement
          finally
            Lst.Free; // on lib�re la liste de travail
          end;
        end
        else // cr�ation de la nouvelle liste
        begin
          fNames.Add(Name + fNames.NameValueSeparator + CBeginList + Prop +
            CBlank + Value + CEndList); // on cr�e le nom
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
// *** valeur d'une liste de propri�t�s ***
begin
  Result := fNames.Values[Name]; // retourne les propri�t�s
end;

function TGVPropList.ValNumListP(N: Integer; out Name, Value: string): Boolean;
// *** valeur d'une liste de propri�t�s par num�ro ***
begin
  Result := False; // suppose une erreur
  if (N > 0) and (N <= fNames.Count) then
  begin
    Name := fNames.Names[N - 1]; // d'abord le nom
    Value := fNames.ValueFromIndex[N - 1]; // puis les propri�t�s
    Result := True; // tout va bien
  end;
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer): string;
// *** valeur d'une propri�t� par num�ro ***
var
  Lst: TGVList;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Result := Lst[(N - 1) * 2 + 1]; // propri�t�s par paires
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

function TGVPropList.ValProp(const Name, Prop: string;
  out Value: string): Boolean;
// *** valeur d'une propri�t� (version avec contr�le) ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := False; // �chec par d�faut
  Lst := TGVList.Create;
  try
    if IsProp(Name, Prop) then // est-ce une propri�t� valide ?
    begin
      Lst.Text := ValListP(Name); // liste de travail
      for I := 0 to Lst.Count - 1 do // on examine la liste
        // si c'est une propri�t� et qu'elle correspond � celle cherch�e
{$IFDEF Delphi}
        if (not Odd(I)) and AnsiSameText(Lst[I], Prop) then
{$ELSE}
        if (not Odd(I)) and (UTF8CompareText(Lst[I], Prop) = 0) then
{$ENDIF}
        begin
          Value := Lst[I + 1]; // �l�ment trouv�
          Result := True; // on a trouv�
          break; // on sort de la boucle
        end;
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

function TGVPropList.ValNumProp(const Name: string; N: Integer;
  out Prop: string): Boolean;
// *** valeur d'une propri�t� par num�ro ***
var
  Lst: TGVList;
begin
  Result := False; // suppose une erreur
  Lst := TGVList.Create;
  try
    if (N > 0) and (N <= CountProps(Name)) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      Prop := Lst[(N - 1) * 2 + 1]; // propri�t�s par paires
      Result := True; // tout est Ok
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

function TGVPropList.ValProp(const Name, Prop: string): string;
// *** valeur d'une propri�t� ***
var
  Lst: TGVList;
  I: Integer;
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  Lst := TGVList.Create;
  try
    if IsProp(Name, Prop) then
    begin
      Lst.Text := ValListP(Name); // liste de travail
      for I := 0 to Lst.Count - 1 do // on examine la liste
        // si c'est une propri�t� et qu'elle correspond � celle cherch�e
{$IFDEF Delphi}
        if (not Odd(I)) and AnsiSameText(Lst[I], Prop) then
{$ELSE}
        if (not Odd(I)) and (UTF8CompareText(Lst[I], Prop) = 0) then
{$ENDIF}
        begin
          Result := Lst[I + 1]; // �l�ment trouv�
          break; // on sort de la boucle
        end;
    end;
  finally
    Lst.Free; // on lib�re la liste
  end;
end;

end.
