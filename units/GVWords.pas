{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des mots                     |
  |                  Unité : GVWords.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 18:05:08                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVWords - part of GVLOGO
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

unit GVWords;

//
// classe TGVWord pour GVLOGO
//
// ##############################################################
//
// L'unité regroupe la classe chargée de traiter les mots du
// projet GVLOGO.
//
// MOTS
//
// Les mots en GVLOGO sont définis comme une suite de caractères ne comprenant
// pas d'espace.
//
// CARACTERE D'ECHAPPEMENT
//
// Le caractère d'échappement ("$" par défaut) permet d'inclure un blanc
// dans un mot, le caractère d'échappement lui-même ou tout caractère
// interdit car utilisé par l'interpréteur : "[", "]", "(", ")" et " ".
// Ce caractère permet donc de ne plus considérer le caractère suivant comme un
// séparateur.
//

interface

uses
  Classes, SysUtils,
  GVConsts; // constantes

type
  { classe des chaînes }

  { TGVString }

  EGVStringException = class(Exception);

  TGVString = class
  strict private
    fRawStr: string; // chaîne brute interne
    fStr: string; // chaîne formatée interne
    function GetRawStr: string; // renvoie la chaîne brute
    function GetStr: string; // renvoie la chaîne formatée
    procedure SetStr(const St: string);
  protected
    function WithEsc(const St: string): string; // avec échappement
    function WithoutEsc(const St: string): string; // sans échappement
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // remise à zéro
    property Str: string read GetStr write SetStr; // chaîne formatée
    property RawStr: string read GetRawStr; // chaîne brute
  end;

  { classe des nombres }

  { TGVNumber }

  EGVNumberException = class(Exception);

  TGVNumber = class
  strict private
    fNum: Double; // nombre de travail
    fSt: string; // chaîne brute d'entrée
    fValid: Boolean; // drapeau de validité
    function GetInt: Integer; // renvoie un entier
    function GetDouble: Double; // renvoie un réel
    function GetStr: string; // acquiert une chaîne à convertir en nombre
    procedure SetStr(const St: string); // forme une chaîne à partir d'un nombre
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // remise à zéro
    function IsValid: Boolean; // est-ce un nombre ?
    function IsInt: Boolean; // est-ce un entier ?
    function IsZero: Boolean; // nombre 0 ?
    property AsString: string read GetStr write SetStr; // une chaîne de nombre
    property AsDouble: Double read GetDouble; // un réel
    property AsInt: Integer read GetInt; // un entier
  end;

  { classe des mots }

  { TGVWord }

  EGVWordException = class(Exception);

  TGVWord = class
  strict private
    fWord, fWord2: TGVString; // mots de travail
    fNum, fNum2: TGVNumber; // nombre de travail
  protected
    // comparaison de deux mots
    function Compare(const StFirst, StTwo: string): Integer;
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    // premier caractère d'un mot
    function First(const St: string): string;
    // dernier caractère d'un mot
    function Last(const St: string): string;
    // sauf le premier caractère d'un mot
    function ButFirst(const St: string): string;
    // sauf le dernier caractère d'un mot
    function ButLast(const St: string): string;
    // concatène les deux mots, le second en premier
    function PutFirst(const StOne, StTwo: string): string;
    // concatène les deux mots, le premier d'abord
    function PutLast(const StOne, StTwo: string): string;
    // supprime si nécessaire le " initial d'un mot
    function WithoutQuote(const St: string): string;
    // supprime si nécessaire le : initial d'un mot
    function WithoutColon(const St: string): string;
    // est-ce un identificateur valide ?
    function IsValidIdent(const St: string): Boolean;
    // les deux mots sont-ils égaux ?
    function EqualP(const StFirst, StTwo: string): Boolean;
    // le premier mot est-il à placer avant le second par ordre alphabétique ?
    function LowerP(const StFirst, StTwo: string): Boolean;
    // renvoie le mot qui vient avant par ordre alphabétique
    function Lowest(const StFirst, StTwo: string): string;
    // le premier mot est-il à placer après le second par ordre alphabétique ?
    function GreaterP(const StFirst, StTwo: string): Boolean;
    // renvoie le mot qui vient après par ordre alphabétique
    function Greatest(const StFirst, StTwo: string): string;
    // le mot est-il vide ?
    function EmptyWordP(const St: string): Boolean;
    // le mot est-il compris dans un autre ?
    function MemberP(const St, SubSt: string): Boolean;
    // longueur du mot
    function Count(const St: string): Integer;
    // longueur de mot en chaîne
    function StrCount(const St: string): string;
    // élément N d'un mot
    function Item(const St: string; N: Integer): string;
    // remplacement de l'élément N d'un mot
    function Replace(const St, SubSt: string; N: Integer): string;
    // mot inversé
    function Reverse(const St: string): string;
    // mot mélangé
    function Shuffle(const St: string): string;
    // lettre au hasard
    function AtRandom(const St: string): string;
    // mot en majuscules
    function Uppercase(const St: string): string;
    // mot en minuscules
    function Lowercase(const St: string): string;
    // insertion en position N
    function Insert(const St, SubSt: string; N: Integer): string;
    // tri des lettres du mot
    function Sort(const St: string): string;
    // est-ce un nombre ?
    function NumberP(const St: string): Boolean;
    // chaîne formatée
    function WithEsc(const St: string): string;
    // chaîne brute
    function WithoutEsc(const St: string): string;
    // le mot est-il valide sans traitement ?
    function IsValid(const St: string): Boolean;
    // rotation des caractères d'un mot
    function Rotate(const St: string): string;
  end;

implementation

uses StrUtils, Math
  {$IFNDEF Delphi}
  , lazutf8 // traitement des chaînes UTF8
  {$ENDIF};

{ ========================================================== }

{ TGVWord }

function TGVWord.Compare(const StFirst, StTwo: string): Integer;
// *** comparaison de deux mots ***
begin
  fNum.AsString := StFirst; // essai de conversion en nombre
  fNum2.AsString := StTwo;
  // ce sont des nombres ?
  if fNum.IsValid and fNum2.IsValid then
    // si oui, on les compare
    Result := CompareValue(fNum.AsDouble, fNum2.AsDouble)
  else
  begin // les autres mots
    fWord.Str := StFirst; // mots normalisés
    fWord2.Str := StTwo;
    {$IFDEF Delphi}
    Result := AnsiCompareText(fWord.Str, fWord2.Str); // comparaison
    {$ELSE}
    Result := UTF8CompareText(fWord.Str, fWord2.Str);
    {$ENDIF}
  end;
end;

function TGVWord.Count(const St: string): Integer;
// *** compte les caractères d'un mot ***
begin
  with fWord do
  begin
    Str := St; // normalisation du mot
    {$IFDEF Delphi}
    Result := Length(RawStr); // renvoie sa longueur sans l'échappement
    {$ELSE}
    Result := UTF8Length(RawStr);
    {$ENDIF}
  end;
end;

function TGVWord.StrCount(const St: string): string;
// *** compte les caractères d'un mot (chaîne) ***
begin
  Result := IntToStr(Count(St));
end;

constructor TGVWord.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fWord := TGVString.Create; // création des outils
  fWord2 := TGVString.Create;
  fNum := TGVNumber.Create;
  fNum2 := TGVNumber.Create;
end;

destructor TGVWord.Destroy;
// *** destruction ***
begin
  fWord.Free; // libération des outils
  fWord2.Free;
  fNum.Free;
  fNum2.Free;
  inherited Destroy; // on hérite
end;

function TGVWord.EmptyWordP(const St: string): Boolean;
// *** le mot est-il vide ? ***
begin
  with fWord do
  begin
    Str := St; // on normalise le mot
    Result := (Str = EmptyStr); // vide ?
  end;
end;

function TGVWord.EqualP(const StFirst, StTwo: string): Boolean;
// *** les mots sont-ils égaux ? ***
begin
  Result := (Compare(StFirst, StTwo) = 0);
end;

function TGVWord.First(const St: string): string;
// *** premier caractère d'un mot ***
begin
  with fWord do
  begin
    Str := St; // on traite la chaîne
    if Str <> EmptyStr then
    {$IFDEF Delphi}
      Result := AnsiLeftStr(RawStr, 1) // premier caractère seulement
    {$ELSE}
      Result := UTF8LeftStr(RawStr, 1)
    {$ENDIF}
    else
      raise EGVStringException.CreateFmt(ME_EmptyStr, [P_First]);
    Str := Result; // chaîne formatée
    Result := Str; // on la renvoie
  end;
end;

function TGVWord.GreaterP(const StFirst, StTwo: string): Boolean;
// *** le premier mot vint-il après le second ? ***
begin
  Result := (Compare(StFirst, StTwo) > 0);
  // ni plus petit ni égal
end;

function TGVWord.Greatest(const StFirst, StTwo: string): string;
 // *** renvoie le mot qui vient en dernier ***
begin
  fWord.Str := StFirst; // on normalise les mots
  fWord2.Str := StTwo;
  Result := IfThen((Compare(StFirst, StTwo) > 0), fWord.Str,
    fWord2.Str); // on renvoie le plus grand
end;

function TGVWord.Insert(const St, SubSt: string; N: Integer): string;
// *** insertion à l'emplacement N ***
begin
  with fWord do
  begin
    Str := St; // mot normalisé
    if (RawStr = EmptyStr) or (N <= 0) or // mot vide ou N hors bornes ?
      {$IFDEF Delphi}
      (Length(RawStr) < (N - 1)) // vérifie la longueur du mot
      {$ELSE}
      (UTF8Length(RawStr) < (N - 1))
      {$ENDIF}
    then
      raise EGVWordException.CreateFmt(ME_BadChar, [St, N]); // erreur !
    {$IFDEF Delphi}
    Str := AnsiLeftStr(RawStr, N - 1) + SubSt + AnsiMidStr(RawStr, N,
      Length(RawStr) - N + 1);
    {$ELSE}
    Str := UTF8LeftStr(RawStr, N - 1) + SubSt + UTF8Copy(RawStr, N,
      UTF8Length(RawStr) - N + 1);
    {$ENDIF}
    Result := Str; // mot normalisé
  end;
end;

function TGVWord.IsValid(const St: string): Boolean;
// *** le mot est-il valide sans traitement ? ***
begin
  with fWord do
  begin
    Str := St; // mot normalisé
    Result := (Str = RawStr); // correct si n'a pas changé
  end;
end;

function TGVWord.Rotate(const St: string): string;
// *** rotation des caractères d'un mot
begin
  Result := EmptyStr; // chaîne vide par défaut
  if St <> EmptyStr then
    {$IFDEF Delphi}
    Result := AnsiMidStr(St,2,Length(St)-1) + AnsiLeftStr(St,1); // premier à la fin
    {$ELSE}
    Result := UTF8Copy(St,2,UTF8Length(St)-1) + UTF8Copy(St,1,1);
    {$ENDIF}
end;

function TGVWord.IsValidIdent(const St: string): Boolean;
// *** le mot est-il un identificateur correct ? ***
var
  I: Integer;
begin
  Result := False; // suppose une erreur
  for I := 1 to Length(St) do
  begin
    // on accepte les minuscules et les majuscules non accentuées, le "_",
    // les chiffres non placés en première position
    // le "." en première position et le "?" en dernière.
    {$IFDEF Delphi}
    if I = 1 then
      Result := CharInSet(St[I], [CUnderline, CDot] + CAlpha)
    else if I = Length(St) then
      Result := CharInSet(St[I], [CAsk] + CAlphaNum)
    else
      Result := CharInSet(St[I], CAlphaNum);
    {$ELSE}
    if I = 1 then
      Result := St[I] in [CUnderline, CDot] + CAlpha
    else if I = Length(St) then
      Result := St[I] in [CAsk] + CAlphaNum
    else
      Result := St[I] in CAlphaNum;
    {$ENDIF}
    if not Result then
      Break;
  end;
end;

function TGVWord.Item(const St: string; N: Integer): string;
// *** élément N d'un mot ***
begin
  with fWord do
  begin
    Str := St; // chaîne normalisée
    if (RawStr = EmptyStr) or (N <= 0) or // mot vide ou N hors bornes ?
      {$IFDEF Delphi}
      (Length(RawStr) < N) // vérifie la longueur du mot
      {$ELSE}
      (UTF8Length(RawStr) < N)
      {$ENDIF}
    then
      raise EGVWordException.CreateFmt(ME_BadChar, [RawStr, N]); // erreur !
    {$IFDEF Delphi}
    Str := AnsiMidStr(RawStr, N, 1); // on cherche l'élément
    {$ELSE}
    Str := UTF8Copy(RawStr, N, 1);
    {$ENDIF}
    Result := Str; // chaîne normalisée
  end;
end;

function TGVWord.Last(const St: string): string;
// *** dernier caractère d'un mot ***
begin
  with fWord do
  begin Str := St; // on traite la chaîne
    if Str <> EmptyStr then
      {$IFDEF Delphi}
      Result := AnsiRightStr(RawStr, 1) // dernier caractère seulement
      {$ELSE}
      Result := UTF8RightStr(RawStr, 1)
      {$ENDIF}
    else
      raise EGVStringException.CreateFmt(ME_EmptyStr, [P_Last]);
    Str := Result; // chaîne formatée
    Result := Str; // on la renvoie
  end;
end;

function TGVWord.Lowercase(const St: string): string;
// *** mot en minuscules ***
begin
  fWord.Str := St; // on normalise le mot
  {$IFDEF Delphi}
  Result := AnsiLowerCase(fWord.Str);
  {$ELSE}
  Result := UTF8LowerCase(fWord.Str);
  {$ENDIF}
end;

 function TGVWord.LowerP(const StFirst, StTwo: string): Boolean;
 // *** le premier mot vient-il avant le second ? ***
begin
  Result := (Compare(StFirst, StTwo) < 0);
end;

 function TGVWord.Lowest(const StFirst, StTwo: string): string;
 // *** renvoie le mot qui vient en premier ***
begin
  fWord.Str := StFirst; // on normalise les mots
  fWord2.Str := StTwo;
  Result := IfThen((Compare(StFirst, StTwo) < 0), fWord.Str,
    fWord2.Str); // on renvoie le plus petit
 end;

function TGVWord.MemberP(const St, SubSt: string): Boolean;
// *** Un sous-mot est-il présent ? ***
begin
  fWord.Str := St; // mots normalisés
  fWord2.Str := SubSt;
  {$IFDEF Delphi}
  Result := (AnsiPos(fWord2.RawStr, fWord.RawStr) <> 0); // position <> 0 ?
  {$ELSE}
  Result := (UTF8Pos(fWord2.RawStr, fWord.RawStr) <> 0);
  {$ENDIF}
end;

function TGVWord.NumberP(const St: string): Boolean;
// *** le mot est-il un nombre ? ***
begin
  fNum.AsString := St; // on tente de convertir
  Result := fNum.IsValid; // résultat de la conversion
end;

function TGVWord.PutFirst(const StOne, StTwo: string): string;
// *** concatène les deux mots, le second en premier ***
begin
  with fWord do
  begin
    Str := StTwo + StOne; // on normalise la concaténation
    Result := Str; // on la renvoie
  end;
end;

function TGVWord.PutLast(const StOne, StTwo: string): string;
// *** concatène les deux mots, le premier d'abord ***
begin
  with fWord do
  begin
    Str := StOne + StTwo; // on normalise la concaténation
    Result := Str; // on la renvoie
  end;
end;

function TGVWord.Replace(const St, SubSt: string; N: Integer): string;
// *** remplacement de l'élément N  ***
begin
  with fWord do
  begin
    Str := St; // chaîne normalisée
    if (RawStr = EmptyStr) or (N <= 0) or // mot vide ou N hors bornes ?
      {$IFDEF Delphi}
      (Length(RawStr) < N) // vérifie la longueur du mot
      {$ELSE}
      (UTF8Length(RawStr) < N)
      {$ENDIF}
    then
      raise EGVWordException.CreateFmt(ME_BadChar, [St, N]); // erreur !
    {$IFDEF Delphi}
    if (N = 1) then // premier caractère ?
      Str := SubSt + AnsiRightStr(RawStr, Length(RawStr) - 1)
    else
    if N = Length(RawStr) then // dernier caractère ?
      Str := AnsiLeftStr(RawStr, Length(RawStr) - 1) + SubSt
    else
      Str := AnsiLeftStr(RawStr, N - 1) + SubSt + AnsiMidStr(RawStr, N + 1,
        Length(RawStr) - N + 1); // autres cas
    {$ELSE}
    if N = 1 then // premier caractère ?
      Str := SubSt + UTF8RightStr(RawStr, UTF8Length(RawStr) - 1)
    else
    if N = UTF8Length(RawStr) then // dernier caractère ?
      Str := UTF8LeftStr(RawStr, UTF8Length(RawStr) - 1) + SubSt
    else
      Str := UTF8LeftStr(RawStr, N - 1) + SubSt + UTF8Copy(RawStr, N + 1,
        UTF8Length(RawStr) - N + 1); // autres cas
    {$ENDIF}
    Result := Str; // chaîne normalisée en retour
  end;
end;

function TGVWord.Reverse(const St: string): string;
// *** mot inversé ***
var
  I: Integer;
begin
  with fWord do
  begin
    Str := St;
    // on traite la chaîne
    Result := EmptyStr;
    {$IFDEF Delphi}
    for I := 1 to Length(RawStr) do // on balaie la chaîne
      Result := RawStr[I] + Result; // on ajoute le caractère devant la chaîne
    {$ELSE}
    for I := 1 to UTF8Length(RawStr) do // on balaie la chaîne
      Result := UTF8Copy(RawStr, I, 1) + Result; // idem
    {$ENDIF}
    Str := Result; // on normalise la chaîne
    Result := Str; // puis on la renvoie
  end;
end;

function TGVWord.Shuffle(const St: string): string;
// *** mot mélangé ***
var
  N: Integer;
  S: string;
begin
  with fWord do
  begin Str := St;
    // chaîne normalisée
    S := RawStr; // chaîne brute
    Result := EmptyStr;
    while (S <> EmptyStr) do
    begin
      {$IFDEF Delphi}
      N := Random(Length(S)) + 1; // emplacement au hasard
      Result := Result + S[N]; // caractère enregistré
      Delete(S, N, 1); // caractère utilisé retiré du mot de travail
      {$ELSE}
      N := Random(UTF8Length(S)) + 1;
      Result := Result + UTF8Copy(S, N, 1);
      UTF8Delete(S, N, 1);
      {$ENDIF}
    end; Str := Result; // on normalise la chaîne
    Result := Str; // puis on la renvoie
  end;
end;

function TGVWord.Sort(const St: string): string;
// *** tri des lettres d'un mot (tri à bulles) ***
var
  I: Integer;
  Exchanged: Boolean;
  {$IFDEF Delphi}
  Ch: Char;
  {$ELSE}
  S: string;
  {$ENDIF}
begin
  with fWord do
  begin
    Str := St; // on normalise le mot
    Result := RawStr; // mot brut de sortie
    if Result = EmptyStr then // mot vide ?
      Exit; // rien à trier : on sort
    repeat
       Exchanged := False; // pas d'échange
      {$IFDEF Delphi}
      for I := 1 to Length(Result) - 1 do // on balaie le mot
        // placé après ?
        if AnsiCompareText(Result[I], Result[I + 1]) > 0 then
        begin
          Ch := Result[I]; // échange
          Result[I] := Result[I + 1];
          Result[I + 1] := Ch;
          Exchanged := True; // un échange a eu lieu
        end;
      {$ELSE}
      for I := 1 to UTF8Length(Result) - 1 do
         if UTF8CompareText(UTF8Copy(Result, I, 1), UTF8Copy(Result, I + 1, 1))
           > 0 then
        begin
          S := UTF8Copy(Result, I, 1);
          UTF8Delete(Result, I, 1);
          UTF8Insert(S, Result, I + 1);
          Exchanged := True; // un échange a eu lieu
        end;
      {$ENDIF}
    until not Exchanged; // sortie si pas d'échange (liste triée)
    Str := Result; // on normalise le mot
    Result := Str; // et on sort avec le mot trié
  end;
end;

function TGVWord.Uppercase(const St: string): string;
// *** mot en majuscules ***
begin
  fWord.Str := St; // on normalise le mot
  {$IFDEF Delphi}
  Result := AnsiUpperCase(fWord.Str); // renvoie la chaîne en majuscules
  {$ELSE}
  Result := UTF8UpperCase(fWord.Str);
  {$ENDIF}
end;

function TGVWord.WithEsc(const St: string): string;
// *** renvoie une chaîne formatée ***
begin
  fWord.Str := St; // on transforme la chaîne en mot
  Result := fWord.Str; // on renvoie le résultat
end;

function TGVWord.WithoutColon(const St: string): string;
// *** renvoie le mot sans les deux-points initiaux ***
begin
  fWord.Str := St; // on normalise le mot
  Result := fWord.Str;
  if (fWord.Str <> EmptyStr) and (fWord.Str[1] = CColon) then  // s'ils existent
    Delete(Result, 1, 1); // on enlève les deux-points
end;

function TGVWord.WithoutEsc(const St: string): string;
// *** renvoie une chaîne brute ***
begin
  fWord.Str := St; // on transforme la chaîne en mot
  Result := fWord.RawStr; // on renvoie le résultat
end;

function TGVWord.WithoutQuote(const St: string): string;
// *** renvoie le mot sans le guillemet anglais initial ***
begin
   fWord.Str := St; // on normalise le mot
   Result := fWord.Str;
   if (fWord.Str <> EmptyStr) and (fWord.Str[1] = CQuote) then // s'il existe
     Delete(Result, 1, 1); // on enlève le guillemet
end;

function TGVWord.AtRandom(const St: string): string;
// *** élément du mot tiré au hasard ***
begin
  with fWord do
  begin
    Str := St; // mot normalisé
    {$IFDEF Delphi}
    // recherche un élément au hasard
    Str := Item(RawStr, Random(Length(RawStr)) + 1);
    {$ELSE}
    Str := Item(RawStr, Random(UTF8Length(RawStr)) + 1);
    {$ENDIF}
    Result := Str; // mot normalisé
  end;
end;

function TGVWord.ButFirst(const St: string): string;
// *** sauf le premier caractère d'un mot ***
begin
  with fWord do
  begin Str := St; // on traite la chaîne
    if Str <> EmptyStr then // pas de chaîne vide
      {$IFDEF Delphi}
      Str := AnsiRightStr(RawStr, Length(RawStr) - 1) // premier caractère supprimé
      {$ELSE}
      Str := UTF8RightStr(RawStr, UTF8Length(RawStr) - 1)
      {$ENDIF}
    else
      raise EGVStringException.CreateFmt(ME_EmptyStr, [P_ButFirst]); // erreur
    Result := Str; // résultat normalisé
  end;
end;

function TGVWord.ButLast(const St: string): string;
// *** sauf le dernier caractère d'un mot ***
begin
  with fWord do
  begin
    Str := St; // on traite la chaîne
    if Str <> EmptyStr then // pas de chaîne vide
      {$IFDEF Delphi}
      Str := AnsiLeftStr(RawStr, Length(RawStr) - 1) // dernier caractère supprimé
      {$ELSE}
      Str := UTF8LeftStr(RawStr, UTF8Length(RawStr) - 1)
      {$ENDIF}
    else
      raise EGVStringException.CreateFmt(ME_EmptyStr, [P_ButLast]); // erreur
    Result := Str; // résultat normalisé
  end;
end;

{ ========================================================== }

{ TGVNumber }

function TGVNumber.GetInt: Integer;
// *** renvoie un entier si possible ***
begin
  if not TryStrToInt(fSt, Result) then // transformation possible ?
    raise EGVNumberException.CreateFmt(ME_BadNumber, [fSt]); // signale l'erreur
end;

function TGVNumber.GetDouble: Double;
// *** renvoie un réel si possible ***
begin
  if IsValid then
    Result := fNum
  else
    raise EGVNumberException.CreateFmt(ME_BadNumber, [fSt]);
end;

function TGVNumber.GetStr: string;
// *** renvoie une chaîne représentant un nombre si possible ***
begin
  if IsValid then
    Result := fSt
  else
    raise EGVNumberException.CreateFmt(ME_BadNumber, [fSt]);
end;

procedure TGVNumber.SetStr(const St: string);
// *** forme un nombre si possible ***
begin
  fSt := St; // chaîne brute affectée
  fValid := TryStrToFloat(St, fNum); // transformation possible ?
end;

constructor TGVNumber.Create;
// *** création ***
begin
  inherited Create; // on hérite
  Clear; // mise à zéro
end;

destructor TGVNumber.Destroy;
// *** destruction ***
begin
  inherited Destroy; // on hérite
end;

procedure TGVNumber.Clear;
// *** remise à zéro ***
begin
  fNum := 0; // nombre à O
  fValid := True; // et correct
end;

function TGVNumber.IsValid: Boolean;
// *** est-ce un nombre correct ? ***
begin
  Result := fValid;
end;

function TGVNumber.IsInt: Boolean;
// *** est-ce un entier ? ***
var
  Value: Integer;
begin
  if IsValid then
    Result := TryStrToInt(fSt, Value) // essai de conversion
  else
    raise EGVNumberException.CreateFmt(ME_BadInt, [fSt]); // signale l'erreur
end;

function TGVNumber.IsZero: Boolean;
// *** nombre 0 ? ***
begin
  if IsValid then
    Result := Math.IsZero(fNum)
  else
    raise EGVNumberException.CreateFmt(ME_BadNumber, [fSt]); // signale l'erreur
end;

{ ========================================================== }

{ TGVString }

function TGVString.GetRawStr: string;
// *** renvoie la chaîne brute ***
begin
  Result := fRawStr;
end;

function TGVString.GetStr: string;
// *** renvoie la chaîne formatée ***
begin
  Result := fStr;
end;

procedure TGVString.SetStr(const St: string);
// *** forme une chaîne formatée ***
begin
  fRawStr := WithoutEsc(St); // pas de caractères d'échappement
  fStr := WithEsc(fRawStr); // caractères d'échappement corrects
end;

function TGVString.WithEsc(const St: string): string;
// *** normalise un mot en tenant compte du caractère d'échappement ***
var
  C: Char;
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (St <> EmptyStr) then
    for C in St do
    // balaie une chaîne non vide
    begin
      {$IFDEF Delphi}
      if CharInSet(C, CSeparators + [CLink]) then
      {$ELSE}
      if C in CSeparators + [CLink] then
      {$ENDIF}
        // si séparateur ou échappement suit, on insère un échappement
        Result := Result + CLink;
      Result := Result + C; // caractère en cours traité
    end;
end;

function TGVString.WithoutEsc(const St: string): string;
// *** sans caractère d'échappement ***
var
  C: Char;
  Flag: Boolean;
begin
  Result := EmptyStr;
  // chaîne vide par défaut
  Flag := False; // on n'a pas eu affaire à un caractère d'échappement
  for C in St do // on balaie la chaîne
    if (C <> CLink) or Flag then // caractère d'échappement initial ?
    begin
      Result := Result + C; // non : on ajoute simplement
      Flag := False; // on indique ce cas
    end
    else
      Flag := True; // échappement initial qu'on ignore
end;

constructor TGVString.Create;
// *** création ***
begin
  inherited Create; // on hérite
  Clear; // remise à zéro
end;

destructor TGVString.Destroy;
// *** destruction ***
begin
  inherited Destroy; // on hérite
end;

procedure TGVString.Clear;
// *** remise à zéro de la chaîne ***
begin
  fStr := EmptyStr;
  fRawStr := EmptyStr;
end;

end.
