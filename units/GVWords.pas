{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des mots                     |
  |                  Unité : GVWords.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle
// 27/02/2015 - 1.0.1 - refonte BOOLEENS -> tranfert dans TGVNumber
//                    - acceptation de VRAI et FAUX comme valeurs booléennes
// 07/03/2015 - 1.0.2 - acceptation des caractères accentués pour les noms
// d'objets

// GVWORDS - part of GVLOGO
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

unit GVWords;
// L'unité GVWORDS regroupe les classes chargées de traiter les mots
// du projet GVLOGO.
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
  GVErrConsts, // constantes pour les erreurs
  GVPrimConsts, // constantes des primitives
  GVErrors; // traitement des erreurs

type
  // *** objet chaîne ***
  TGVString = object
  strict private
    fRawStr: string; // chaîne brute entrée
    fIsValid: Boolean; // validité de la chaîne d'entrée
    fStr: string; // chaîne brute interne
    fFmtStr: string; // chaîne formatée interne
    function GetFmtStr: string; // renvoie la chaîne formatée
    function GetStr: string; // renvoie la chaîne brute
    procedure SetStr(const St: string); // établit une nouvelle chaîne brute
    function WithEsc(const St: string): string; // avec échappement
    function WithoutEsc(const St: string): string; // sans échappement
  public
    procedure Clear; // remise à zéro
    property IsValid: Boolean read fIsValid; // validité de la chaîne d'entrée
    property Str: string read GetStr write SetStr; // chaîne brute d'entrée
    property FmtStr: string read GetFmtStr; // chaîne formatée
    property RawStr: string read fRawStr; // chaîne brute
  end;

  // *** classe des nombres ***
  TGVNumber = class(TObject)
  strict private
    fError: TGVErrors; // traitement des erreurs
    fNum: Double; // nombre de travail
    fSt: string; // chaîne brute d'entrée
    fValid: Boolean; // drapeau de validité
    function GetBoolean: Boolean;
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
    function IsBoolean: Boolean; // booléen ?
    function IsNegate: Boolean; // nombre négatif ?
    property Text: string read GetStr write SetStr; // une chaîne de nombre
    property AsDouble: Double read GetDouble; // un réel
    property AsInt: Integer read GetInt; // un entier
    property AsBoolean: Boolean read GetBoolean; // un booléen
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

  // *** classe des mots ***
  TGVWord = class(TObject)
  strict private
    fError: TGVErrors; // traitement des erreurs
    fText: TGVString; // mot à traiter
    fNum, fNum2: TGVNumber; // nombres de travail
    function GetFmtText: string; // texte formaté
    function GetText: string; // texte brut
    procedure SetText(const St: string); // établit le texte
  protected
    function Compare(const St: string): Integer; // comparaison de deux mots
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // nettoyage
    function First: string; // premier caractère d'un mot
    function Last: string; // dernier caractère d'un mot
    function ButFirst: string; // sauf le premier caractère d'un mot
    function ButLast: string; // sauf le dernier caractère d'un mot
    // concatène les deux mots, le second en premier
    function PutFirst(const St: string): string;
    // concatène les deux mots, le premier d'abord
    function PutLast(const St: string): string;
    // supprime si nécessaire le " initial d'un mot
    function WithoutQuote: string;
    // supprime si nécessaire le : initial d'un mot
    function WithoutColon: string;
    function IsValidIdent: Boolean; // est-ce un identificateur valide ?
    // les deux mots sont-ils égaux ?
    function IsEqual(const St: string): Boolean;
    // le premier mot est-il à placer avant le second par ordre alphabétique ?
    function IsLower(const St: string): Boolean;
    // renvoie le mot qui vient avant par ordre alphabétique
    function Lowest(const St: string): string;
    // le premier mot est-il à placer après le second par ordre alphabétique ?
    function IsGreater(const St: string): Boolean;
    // renvoie le mot qui vient après par ordre alphabétique
    function Greatest(const St: string): string;
    function IsEmptyWord: Boolean; // le mot est-il vide ?
    // le mot est-il compris dans un autre ?
    function IsMember(const St: string): Boolean;
    function Count: Integer; // longueur du mot
    function StrCount: string; // longueur de mot en chaîne
    function GetItem(const N: Integer): string; // élément N d'un mot
    // remplacement de l'élément N d'un mot
    function Replace(const N: Integer; const St: string): string;
    // suppression de l'élément N d'un mot
    function DelItem(const N: Integer): string;
    function Reverse: string; // mot inversé
    function Shuffle: string; // mot mélangé
    function AtRandom: string; // lettre au hasard
    function Uppercase: string; // mot en majuscules
    function Lowercase: string; // mot en minuscules
    // insertion en position N
    function Insert(const N: Integer; const St: string): string;
    function Sort: string; // tri des lettres du mot
    function IsNumber: Boolean; // est-ce un nombre ?
    function IsInt: Boolean; // est-ce un entier ?
    function IsBoolean: Boolean; // est-ce un booléen ?
    function AsNumber: Double; // renvoie un nombre
    function AsInt: Integer; // renvoie un entier
    function AsBoolean: Boolean; // renvoie un booléen
    function IsNegate: Boolean; // nombre négatif ?
    function WithEsc: string; // chaîne formatée
    function WithoutEsc: string; // chaîne brute
    function IsValid: Boolean; // le mot est-il valide sans traitement ?
    function Rotate: string; // rotation des caractères d'un mot
    property Item[N: Integer]: string read GetItem; default; // élément N
    property Text: string read GetText write SetText; // mot à traiter
    property FmtText: string read GetFmtText; // mot à traiter formaté
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses {%H-}StrUtils,
  // *** Bogue Lazarus : un conseil ("hint") indique que cette unité n'est pas
  // utilisée alors que la compilation échouera si elle est omise.
  // En effet, elle est indispensable pour le traitement de IFTHEN
  // avec des chaînes.
  // [Il semblerait que le compilateur soit trompé par le IFTHEN présent
  // dans l'unité "math" utilisée ci-après].
  Math, // pour les calculs
  lazutf8, // traitement des chaînes UTF8
  GVConsts; // constantes

{ ========================================================== }

{ TGVWord }

procedure TGVWord.SetText(const St: string);
// *** établit le texte de travail ***
begin
  if (fText.Str = St) then
    Exit; // on sort si aucun changement
  fText.Str := St; // mot normalisé conservé
end;

function TGVWord.GetText: string;
// *** récupère le texte de travail ***
begin
  Result := fText.Str; // renvoi du mot brut
end;

function TGVWord.GetFmtText: string;
// *** récupère le texte formaté de travail ***
begin
 Result := fText.FmtStr; // renvoi du mot formaté
end;

function TGVWord.Compare(const St: string): Integer;
// *** comparaison de deux mots ***
begin
  // essai de conversion en nombre des deux mots
  fNum.Text := Text;
  fNum2.Text := St;
  // ce sont des nombres ?
  if fNum.IsValid and fNum2.IsValid then
    // si oui, on les compare
    Result := CompareValue(fNum2.AsDouble, fNum.AsDouble)
  else
    // on compare les  mots ordinaires
    Result := UTF8CompareText(St, Text); // comparaison UTF8
end;

function TGVWord.Count: Integer;
// *** compte les caractères d'un mot ***
begin
  Result := UTF8Length(Text); // longueur UTF8
end;

function TGVWord.StrCount: string;
// *** compte les caractères d'un mot (renvoi en chaîne) ***
begin
  Result := IntToStr(Count);
end;

constructor TGVWord.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fNum := TGVNumber.Create;
  fNum2 := TGVNumber.Create;
  Error := TGVErrors.Create; // on crée le gestionnaire d'erreurs
end;

destructor TGVWord.Destroy;
// *** destruction ***
begin
  Error.Free; // libération du gestionnaire d'erreurs
  fNum.Free;
  fNum2.Free;
  inherited Destroy; // on hérite
end;

procedure TGVWord.Clear;
// *** nettoyage ***
begin
  Error.Clear; // pas d'erreur
  Text := EmptyStr; // texte réinitialisé
end;

function TGVWord.IsEmptyWord: Boolean;
// *** le mot est-il vide ? ***
begin
  Result := (Text = EmptyStr); // vide ?
end;

function TGVWord.IsEqual(const St: string): Boolean;
// *** les mots sont-ils égaux ? ***
begin
  Result := (Compare(St) = 0);
end;

function TGVWord.First: string;
// *** premier caractère d'un mot ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Text <> EmptyStr then // mot non vide ?
    Result := UTF8LeftStr(Text, 1) // premier caractère UTF8
  else
    // [### Erreur : mot vide ###]
    Error.SetError(CE_EmptyWord, P_First);
end;

function TGVWord.IsGreater(const St: string): Boolean;
// *** le premier mot vient-il après le second ? ***
begin
  // ni plus petit ni égal
  Result := (Compare(St) < 0);
end;

function TGVWord.Greatest(const St: string): string;
// *** renvoie le mot qui vient en dernier ***
begin
  Result := IfThen((Compare(St) > 0), St, Text);
end;

function TGVWord.Insert(const N: Integer; const St: string): string;
// *** insertion à l'emplacement N ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (Text = EmptyStr) or (N <= 0) or // mot vide ou N hors bornes ?
    (Count < (N - 1)) then
      // [### Erreur : hors bornes ###]
      Error.SetError(CE_BadItem, Text, N)
  else
    // on insère dans la chaîne à l'emplacement prévu
    Result := UTF8LeftStr(Text, N - 1) + St + UTF8Copy(Text, N,
        UTF8Length(Text) - N + 1);
end;

function TGVWord.IsValid: Boolean;
// *** le mot est-il valide sans traitement ? ***
begin
  Result := fText.IsValid;
end;

function TGVWord.Rotate: string;
// *** rotation des caractères d'un mot
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Text <> EmptyStr then
    // premier à la fin
    Result := UTF8Copy(Text, 2, Count - 1) + First;
end;

function TGVWord.IsValidIdent: Boolean;
// *** le mot est-il un identificateur correct ? *** # 1.0.1
var
  Li: Integer;
  LPredDot: Boolean;
begin
  Result := False; // suppose une erreur
  LPredDot := False; // pas de point rencontré juste avant
  for Li := 1 to Length(Text) do
  begin
    // on accepte les minuscules et les majuscules même accentuées (# 1.0.1),
    // le "_", les chiffres non placés en première position,
    // le "." mais pas en dernière position et doublé, et le "?" à la fin.
    if Li = 1 then // premier caractère
    begin
      Result := not (Text[Li] in [CAsk] + CSpecialChar + CNonPrintable + CDigit);
      LPredDot := (Text[Li] = CDot); // point enregistré
    end
    else
    if Li = Length(Text) then // dernier caractère
      Result := not (Text[Li] in CSpecialChar + CNonPrintable)
    else
    begin // les autres positions
      Result := not (Text[Li] in [CAsk] + CSpecialChar + CNonPrintable);
      if Text[Li] = CDot then // un point ?
      begin
        Result := not LPredDot; // pas deux fois d'affilée !
        LPredDot := True; // point mémorisé
      end
      else
        LPredDot := False; // pas de point en cours
    end;
    if not Result then // on sort si un caractère fautif
      Break;
  end;
end;

function TGVWord.GetItem(const N: Integer): string;
// *** élément N d'un mot ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  // mot vide ou N hors bornes ?
  if (Text = EmptyStr) or (N <= 0) or (Count < N) then
    // [### Erreur : élément inexistant ###]
    Error.SetError(CE_BadItem, Text, N)
  else
    Result := UTF8Copy(Text, N, 1); // on renvoie l'élément cherché
end;

function TGVWord.Last: string;
// *** dernier caractère d'un mot ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Text <> EmptyStr then // mot non vide ?
    Result := UTF8RightStr(Text, 1) // dernier élément seulement
  else
    // [### Erreur : mot vide ###]
    Error.SetError(CE_EmptyWord, P_Last);
end;

function TGVWord.Lowercase: string;
// *** mot en minuscules ***
begin
  Result := UTF8LowerCase(Text); // minuscules UTF8
end;

function TGVWord.IsLower(const St: string): Boolean;
// *** le premier mot vient-il avant le second ? ***
begin
  Result := (Compare(St) > 0);
end;

function TGVWord.Lowest(const St: string): string;
// *** renvoie le mot qui vient en premier ***
begin
  Result := IfThen((Compare(St) < 0), St, Text);
end;

function TGVWord.IsMember(const St: string): Boolean;
// *** un sous-mot est-il présent ? ***
begin
  Result := (UTF8Pos(St, Text) <> 0); // position UTF8
end;

function TGVWord.IsNumber: Boolean;
// *** le mot est-il un nombre ? ***
begin
  fNum.Text := Text; // on tente de convertir
  Result := fNum.IsValid; // résultat de la conversion
end;

function TGVWord.IsInt: Boolean;
// *** le mot est-il un entier ? ***
begin
  fNum.Text := Text; // on tente de convertir
  Result := fNum.IsValid and fNum.IsInt; // résultat de la conversion
end;

function TGVWord.IsBoolean: Boolean;
// *** est-ce un booléen ? ***
begin
  // entier = -1 ou 0, VRAI ou FAUX ?
  fNum.Text := Text; // on tente de convertir
  Result := fNum.IsValid and fNum.IsBoolean; // résultat de conversion
end;

function TGVWord.AsNumber: Double;
// *** renvoie un nombre ***
begin
  Result := -1;
  if IsNumber then // un nombre ?
    Result :=  fNum.AsDouble // on retrouve le résultat
  else
    // [### Erreur: pas un nombre ###]
   Error.SetError(CE_BadNumber, Text);
end;

function TGVWord.AsInt: Integer;
// *** renvoie un entier ***
begin
  Result := -1;
  if IsInt then // un entier ?
    Result :=  fNum.AsInt // on retrouve le résultat
  else
    // [### Erreur: pas un entier ###]
    Error.SetError(CE_BadInt, Text);
end;

function TGVWord.AsBoolean: Boolean;
// *** renvoie un booléen ***
begin
  Result := False;
  if IsBoolean then // un booléen
    Result := fNum.AsBoolean
  else
    // [### Erreur: pas un booléen ###]
    Error.SetError(CE_BadBool, Text);
end;

function TGVWord.IsNegate: Boolean;
// *** nombre négatif ? ***
begin
  Result := False;
  if IsNumber then // un nombre ?
    Result := (Sign(fNum.AsDouble) =  NegativeValue) // on retrouve le résultat
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, Text);
end;

function TGVWord.PutFirst(const St: string): string;
// *** concatène les deux mots, le second en premier ***
begin
  Result := St + Text;
end;

function TGVWord.PutLast(const St: string): string;
// *** concatène les deux mots, le premier d'abord ***
begin
  Result := Text + St;
end;

function TGVWord.Replace(const N: Integer; const St: string): string;
// *** remplacement de l'élément N  ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  // mot vide ou N hors bornes ?
  if (Text = EmptyStr) or (N <= 0) or (Count < N) then
    // [### Erreur : élément inexistant ###]
    Error.SetError(CE_BadItem, St, N)
  else
  begin
    // on remplace
    if N = 1 then // premier caractère ?
      Result := St + ButFirst
    else
    if N = Count then // dernier caractère ?
      Result := ButLast + St
    else
      Result := UTF8LeftStr(Text, N - 1) + St + UTF8Copy(Text, N + 1,
        Count - N + 1); // autres cas
  end;
end;

function TGVWord.DelItem(const N: Integer): string;
// *** suppression de l'élément N  ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  // mot vide ou N hors bornes ?
  if (Text = EmptyStr) or (N <= 0) or (Count < N) then
    // [### Erreur : élément inexistant ###]
    Error.SetError(CE_BadItem, Text, N)
  else
  begin
    // on supprime
    if N = 1 then // premier caractère ?
      Result := ButFirst
    else
    if N = Count then // dernier caractère ?
      Result := ButLast
    else
      Result := UTF8LeftStr(Text, N - 1) + UTF8Copy(Text, N + 1,
        Count - N + 1); // autres cas
  end;
end;

function TGVWord.Reverse: string;
// *** mot inversé ***
var
  Li: Integer;
begin
  Result := EmptyStr; // chaîne vide par défaut
  for Li := 1 to Count do // on balaie la chaîne
    Result := UTF8Copy(Text, Li, 1) + Result; // on reconstruit à l'envers
end;

function TGVWord.Shuffle: string;
// *** mot mélangé ***
var
  Li: Integer;
  LSt: string;
begin
  Result := EmptyStr; // chaîne vide par défaut
  Lst := Text; // chaîne de travail prête
  while (LSt <> EmptyStr) do // tant qu'il y a des caractères à traiter
  begin
    Li := Random(UTF8Length(LSt)) + 1; // emplacement au hasard
    Result := Result + UTF8Copy(LSt, Li, 1); // on construit le résultat
    UTF8Delete(LSt, Li, 1); // on retire le caractère utilisé
  end;
end;

function TGVWord.Sort: string;
// *** tri des lettres d'un mot (tri à bulles) ***
var
  Li: Integer;
  LExchanged: Boolean;
  LSt: string;
begin
  Result := Text; // mot brut de sortie
  if Result = EmptyStr then // mot vide ?
    Exit; // rien à trier : on sort
  repeat
    LExchanged := False; // pas d'échange
    // on balaie la chaîne
    for Li := 1 to Count - 1 do
      // comparaison de deux éléments contigus
      if UTF8CompareText(UTF8Copy(Result,Li,1),
        UTF8Copy(Result,Li + 1,1)) > 0 then
      begin
        // on inverse le éléments
        LSt := UTF8Copy(Result, Li, 1);
        UTF8Delete(Result, Li, 1);
        UTF8Insert(LSt, Result, Li + 1);
        LExchanged := True; // un échange a eu lieu
      end;
  until not LExchanged; // sortie si pas d'échange (liste triée)
end;

function TGVWord.Uppercase: string;
// *** mot en majuscules ***
begin
  Result := UTF8UpperCase(Text);
end;

function TGVWord.WithEsc: string;
// *** renvoie une chaîne formatée ***
begin
  Result := FmtText; // on renvoie le résultat
end;

function TGVWord.WithoutColon: string;
// *** renvoie le mot sans les deux-points initiaux ***
begin
  Result := Text;
  if (Text <> EmptyStr) and (Text[1] = CColon) then // s'ils existent
    Delete(Result, 1, 1); // on enlève les deux-points
end;

function TGVWord.WithoutEsc: string;
// *** renvoie une chaîne brute ***
begin
  Result := Text; // on renvoie le résultat
end;

function TGVWord.WithoutQuote: string;
// *** renvoie le mot sans le guillemet anglais initial ***
begin
  Result := Text;
  if (Text <> EmptyStr) and (Text[1] = CQuote) then // s'il existe
    Delete(Result, 1, 1); // on enlève le guillemet
end;

function TGVWord.AtRandom: string;
// *** élément du mot tiré au hasard ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if (Text = EmptyStr) then // mot vide interdit
    // [### Erreur : mot vide ###]
    Error.SetError(CE_EmptyWord, MF_DRandom)
  else
    Result := Item[Random(Count) + 1]; // caractère UTF8
end;

function TGVWord.ButFirst: string;
// *** sauf le premier caractère d'un mot ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Text <> EmptyStr then // pas de chaîne vide
    // tout sauf le premier caractère UTF8
    Result := UTF8RightStr(Text, Count - 1)
  else
    // [### Erreur : mot vide ###]
    Error.SetError(CE_EmptyWord, P_ButFirst);
end;

function TGVWord.ButLast: string;
// *** sauf le dernier caractère d'un mot ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if Text <> EmptyStr then // pas de chaîne vide
    // dernier caractère UTF8 supprimé
    Result := UTF8LeftStr(Text, Count - 1)
  else
    // [### Erreur : mot vide ###]
    Error.SetError(CE_EmptyWord, P_ButLast);
end;

{ ========================================================== }

{ TGVNumber }

function TGVNumber.GetInt: Integer;
// *** renvoie un entier si possible ***
begin
  if IsBoolean then // un booléen ?
  begin
    if AsBoolean then
      Result := CRTrue // on renvoie sa valeur sous forme d'entier
    else
      Result := CRFalse;
  end
  else
  if not TryStrToInt(fSt, Result) then // transformation en entier possible ?
    // [### Erreur: pas un entier ###]
    Error.SetError(CE_BadInt, fSt);
end;

function TGVNumber.GetBoolean: Boolean;
// *** renvoie un booléen si possible ***
begin
  if IsBoolean then
    Result := (fNum = CRTrue) // vrai si valeur VRAI
  else
    // [### Erreur: pas un booléen ###]
    Error.SetError(CE_BadBool, fSt);
end;

function TGVNumber.GetDouble: Double;
// *** renvoie un réel si possible ***
begin
  if IsValid then // nombre valide ?
    Result := fNum // on le renvoie
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt);
end;

function TGVNumber.GetStr: string;
// *** renvoie une chaîne représentant un nombre si possible ***
begin
  if IsValid then // nombre valide ?
    Result := fSt // on le renvoie
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt);
end;

procedure TGVNumber.SetStr(const St: string);
// *** forme un nombre si possible ***
begin
  fSt := St; // chaîne brute affectée
  if AnsiSameText(Trim(St), MF_True) then // primitive VRAI ?
  begin
    fNum := CRTrue; // valeur interne de VRAI
    fValid := True;
  end
  else
  if AnsiSameText(Trim(St), MF_False) then // primitive FALSE ?
  begin
    fNum := CRFalse; // valeur interne de FAUX
    fValid := True;
  end
  else
    fValid := TryStrToFloat(St, fNum); // transformation possible ?
end;

constructor TGVNumber.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fError := TGVErrors.Create; // création du gestionnaire d'erreurs
  Clear; // mise à zéro
end;

destructor TGVNumber.Destroy;
// *** destruction ***
begin
  fError.Free; // libération du gestionnaire d'erreurs
  inherited Destroy; // on hérite
end;

procedure TGVNumber.Clear;
// *** remise à zéro ***
begin
  fNum := 0; // nombre à O
  fValid := True; // et correct
  Error.Clear; // pas d'erreur
end;

function TGVNumber.IsValid: Boolean;
// *** est-ce un nombre correct ? ***
begin
  Result := fValid;
end;

function TGVNumber.IsInt: Boolean;
// *** est-ce un entier ? ***
var
  Li: Integer;
begin
  if IsValid then // nombre valide ?
    Result := TryStrToInt(fSt, Li) or IsBoolean // essai de conversion
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt);
end;

function TGVNumber.IsZero: Boolean;
// *** nombre 0 ? ***
begin
  if IsValid then // nombre valide ?
    Result := Math.IsZero(fNum) // vaut 0 ?
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt); // erreur signalée
end;

function TGVNumber.IsBoolean: Boolean;
// *** valeur booléenne ? ***
begin
  if IsValid then // nombre valide ?
    Result := (fNum = CRTrue) or (fNum = CRFalse)
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt); // erreur signalée
end;

function TGVNumber.IsNegate: Boolean;
// *** nombre négatif ? ***
begin
  if IsValid then // nombre valide ?
    Result := (Math.Sign(fNum) = NegativeValue) // vaut 0 ?
  else
    // [### Erreur: pas un nombre ###]
    Error.SetError(CE_BadNumber, fSt); // erreur signalée
end;

{ ========================================================== }

{ TGVString }

function TGVString.GetStr: string;
// *** renvoie la chaîne brute ***
begin
  Result := fStr;
end;

function TGVString.GetFmtStr: string;
// *** renvoie la chaîne formatée ***
begin
  Result := fFmtStr;
end;

procedure TGVString.SetStr(const St: string);
// *** forme une chaîne formatée ***
begin
  fRawStr := St; // on sauvegarde la chaîne d'origine
  fStr := WithoutEsc(St); // pas de caractères d'échappement
  fFmtStr := WithEsc(fStr); // caractères d'échappement corrects
  fIsValid := (fRawStr = fFmtStr); // valide telle quelle ?
end;

function TGVString.WithEsc(const St: string): string;
// *** normalise un mot en tenant compte du caractère d'échappement ***
var
  Lch: Char;
begin
  Result := EmptyStr; // chaîne vide par défaut
  for Lch in St do // balaie la chaîne de départ
  begin
    if Lch in CSeparators + [CLink] then
      // si séparateur ou échappement suit, on insère un échappement
      Result := Result + CLink;
    Result := Result + Lch; // caractère en cours ajouté
  end;
end;

function TGVString.WithoutEsc(const St: string): string;
// *** sans caractère d'échappement ***
var
  LCh: Char;
  LFlag: Boolean;
begin
  Result := EmptyStr;
  // chaîne vide par défaut
  LFlag := False; // on n'a pas eu affaire à un caractère d'échappement
  for LCh in St do // on balaie la chaîne
    if (LCh <> CLink) or LFlag then // caractère d'échappement initial ?
    begin
      Result := Result + LCh; // non : on ajoute simplement
      LFlag := False; // on indique ce cas
    end
    else
      LFlag := True; // échappement initial qu'on ignore
end;

procedure TGVString.Clear;
// *** remise à zéro de la chaîne ***
begin
  fStr := EmptyStr;
  fFmtStr := EmptyStr;
  fIsValid := True; // chaîne valide
end;

end.
