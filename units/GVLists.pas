{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des listes                   |
  |                  Unité : GVLists.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVLISTS - part of GVLOGO
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

unit GVLists;
// L'unité GVLISTS regroupe les classes chargées de traiter les listes
// du projet GVLOGO.
//
// LISTES SIMPLES
//
// 1. Un mot est un ensemble de caractères autres que le caractère blanc.
// 2. Une liste est un ensemble de mots placés entre crochets : [ et ].
// 3. Une liste peut comprendre d'autres listes.
// 4. Un élément d'une liste est soit un mot soit une liste.
// 5. Il existe une liste qui ne comprend aucun élément et qu'on appelle
// la liste vide.
//
// Exemples :
//
// [coucou] est une liste valide.
// [coucou [encore coucou]] est une liste valide.
// [coucou coucou2] est une liste valide.
// [[coucou]] est une liste valide.
// [[coucou] n'est pas une liste valide (il manque un crochet fermant).
//
// Les listes sont à la base du langage GVLogo.
// Elles permettent un traitement des chaînes de caractères à partir de
// primitives simples.
// Les lignes d'un programme en GVLogo sont considérées comme des listes.
//
// On notera que les éléments sont numérotés de 1 à Count.
// Cette façon de compter est différente de celle des listes en Pascal.
//

interface

uses
  Classes, SysUtils,
  GVConsts, // constantes
  GVWords, // mots
  GVErrConsts, // constantes d'erreurs
  GVPrimConsts, // constantes des primitives
  GVErrors; // traitement des erreurs

type
  // *** utilitaires pour les listes pour définition ultérieure ***
  TGVListUtils = class;

  // *** classe des listes ***
  TGVList = class(TStringList)
  strict private
    fRawStr: string; // chaîne brute en entrée
    fError: TGVErrors; // traitement des erreurs
    fIsValid: Boolean; // validité de la liste
    fPos: Integer; // position de travail
    fLoading: Boolean; // chargement en cours ?
    fNumLastItem: Integer; // dernier élément trouvé
    fWord: TGVWord; // mot de travail
    fUtil: TGVListUtils; // utilitaire pour liste
  protected
    procedure Put(Index: Integer; const St: string); override; // assignation
    function Get(Index: Integer): string; override; // élément choisi
    function GetTextStr: string; override; // récupération du texte
    procedure SetTextStr(const AValue: string); override; // validation du texte
  public
    constructor Create; overload; // création
    destructor Destroy; override; // destruction
    procedure Clear; override; // nettoyage
    function Add(const St: string): Integer; override; // ajout
    procedure LoadFromStream(Stream: TStream); overload; override; // chargement
    procedure Assign(Source: TPersistent); override; // assignation
    procedure Insert(Index: Integer; const St: string); override; // insertion
    // nouvelles méthodes (*** ne modifient pas la liste interne ***)
    // renvoie la liste sous forme de chaîne
    function ToStr: string;
    // renvoie la liste sous forme de chaîne sans crochets
    function ToWBStr: string;
    // la liste est-elle la liste vide ?
    function IsEmptyList: Boolean;
    // renvoie le premier élément de la liste
    function First: string;
    // renvoie le dernier élément de la liste
    function Last: string;
    // sauf le premier de la liste
    function ButFirst: string;
    // sauf le dernier de la liste
    function ButLast: string;
    // supprime l'élément N
    function DeleteItem(N: Integer): string;
    // insertion d'un élément en position N
    function InsertAItem(N: Integer; const St: string): string;
    // remplacement de l'élément N
    function ReplaceItem(N: Integer; const St: string): string;
    // met en premier
    function PutFirst(const St: string): string;
    // met en dernier
    function PutLast(const St: string): string;
    // phrase à droite
    function SentenceRight(const St: string): string;
    // phrase à gauche
    function SentenceLeft(const St: string): string;
    // tri des éléments
    function SortItems: string;
    // inversion des éléments
    function ReverseItems: string;
    // mélange des éléments
    function ShuffleItems: string;
    // membre présent ?
    function IsItem(const St: string): Boolean;
    // ajout d'une paire
    function TwoAdd(const St1, St2: string): string;
    // suppression d'une paire
    function TwoDelete(const N: Integer): string;
    // liste en majuscules
    function UpperCase: string;
    // liste en minuscules
    function LowerCase: string;
    // rotation de la liste
    function Rotate: string;
    // élément de la liste choisi au hasard
    function AtRandom: string;
    // premier élément de chaque élément d'une liste
    function Firsts: string;
    // tout sauf le premier élément de chaque élément d'une liste
    function ButFirsts: string;
    // dernier élément traité
    property LastItem: Integer read fNumLastItem default -1;
    // chaîne brute en entrée
    property RawStr: string read fRawStr; // chaîne brute en entrée
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
    // validité de la liste
    property IsValid: Boolean read fIsValid default False;
    // acquisition du texte
    property Text: string read GetTextStr write SetTextStr;
  end;

  // *** classe des utilitaires pour les listes ***
  TGVListUtils = class
  strict private
    fError: TGVErrors; // traitement des erreurs
    fWord: TGVWord; // mot de travail
    fPos: Integer; // position de travail
    fSt: TGVString; // chaîne de travail
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // nettoyage
    // conversion d'une liste en mot
    function ListToWord(const St: string): string;
    // conversion d'un mot en liste
    function WordToList(const St: string): string;
    // conversion d'une liste en chaîne
    function ListToStr(const St: string): string; overload;
    function ListToStr(const St: string; out ASt: string): Boolean; overload;
    // conversion d'une chaîne en liste
    function StrToList(const St: string): string;
    // retourne la liste vide
    function EmptyList: string;
    // vérifie la validité d'une liste
    function IsValid(const St: string): Boolean;
    // teste la validité d'une valeur (mot ou liste)
    function IsValidValue(const St: string): Boolean;
    // liste simple ?
    function IsSimpleList(const St: string): Boolean;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses
  lazutf8; // traitement des chaînes UTF8

{ TGVList }

function TGVList.Add(const St: string): Integer;
// *** ajout d'une chaîne ***
var
  LSt: string;
  LStrList: TStringList; // liste de travail

  procedure Parse(const LSt2: string);
  // *** découpe la liste de travail ***
  var
    LW: Integer; // place du caractère examiné
    Lev: Integer; // niveau de travail
    LItem: string; // élément en cours
    LStCh: string; // caractère en cours (1 ou 2 places)

    procedure NextChar;
    // vers le caractère suivant
    begin
      Inc(LW); // pointe sur le caractère suivant
      LStCh := UTF8Copy(LSt2, LW, 1); // nouveau caractère ne cours
    end;

    procedure DoBlank;
    // traite un blanc
    begin
      NextChar; // caractère suivant
      // saute les blancs
      while (LStCh = CBlank) or (LStCh = CTab) and (LW <= UTF8Length(LSt2)) do
        NextChar; // caractère suivant
      if LItem <> EmptyStr then
      begin
        LStrList.Add(LItem); // ajoute l'élément si non vide
        LItem := EmptyStr; // élément à zéro
      end;
    end;

    procedure DoBeginList;
    // traite un début de liste
    begin
        if LItem <> EmptyStr then // ajoute l'élément si non vide
          LStrList.Add(LItem);
        LItem := CBeginList; // élément = [
        NextChar; // caractère suivant
        Lev := 1; // niveau 1
        while (Lev <> 0) and (LW <= UTF8Length(LSt2)) do // autres niveaux ?
        begin
          if LStCh = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'élément
            NextChar; // caractère suivant
          end
          else
          if LStCh = CBeginList then
            Inc(Lev) // niveau suivant
          else
          if LStCh = CEndList then
            Dec(Lev); // niveau précédent
          if LW <= UTF8Length(LSt2) then // si possible
                LItem := LItem + LStCh; // sauve l'élément suivant
          NextChar; // caractère suivant
       end; // fin des niveaux
       if Lev = 0 then // normalement niveau = 0 à la sortie
       begin // c'est-à-dire autant de [ que de ]
         LStrList.Add(LItem); // sauve le dernier élément
         LItem := EmptyStr; // et le remet à zéro
       end
       else // si niveaux incorrects
         fPos := LW; // note la position de l'erreur
    end;

    procedure DoBeginPar;
    // début de parenthèse
    begin
        if LItem <> EmptyStr then // sauve l'élément si non vide
          LStrList.Add(LItem);
        LItem := CBeginPar; // élément = (
        NextChar; // caractère suivant
        Lev := 1; // premier niveau
        while (Lev <> 0) and (LW <= UTF8Length(LSt2)) do // autres niveaux ?
        begin
          if LStCh = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'élément
            NextChar; // caractère suivant
          end
          else
          if (LStCh = CBeginList) or (LStCh = CEndList) then
            fPos := LW // pas de liste dans une expression
          else
          if LStCh = CBeginPar then
            Inc(Lev) // niveau suivant
          else
          if LStCh = CEndPar then
            Dec(Lev); // niveau précédent
          if LW <= UTF8Length(LSt2) then // si possible
            LItem := LItem + LStCh; // dans l'élément
          NextChar; // caractère suivant
        end; // fin des autres niveaux
        if (Lev = 0) and (fPos = 0) then // nombre correct de niveaux ?
        begin
          LStrList.Add(LItem); // sauve le dernier élément
          LItem := EmptyStr; // remis à zéro
        end
        else // si erreur
          if fPos = 0 then
            fPos := LW; // note la position de l'erreur si non connue
    end;

    procedure DoLink;
    // début d'un lien
    begin
        LItem := LItem + CLink; // dans l'élément en cours
        NextChar; // caractère suivant
        if LW <= UTF8Length(LSt2) then // si possible
        begin
          LItem := LItem + LStCh; // dans l'élément en cours
          NextChar; // caractère suivant
        end;
    end;

  begin { début de PARSE }
    fPos := 0; // pas d'erreur
    LStrList.Clear; // on nettoie la liste de travail
    if LSt2 = EmptyStr then // si chaîne vide
      Exit; // on sort
    LItem := EmptyStr; // élément en cours vide
    LW := 0; // pointe sur le premier caractère
    NextChar; // caractère suivant
    while (fPos = 0) and (LW <= UTF8Length(LSt2)) do // on boucle si possible
    begin
      if (LStCh = CBlank) or (LStCh = CTab) then
      // *** blanc ? ***
        DoBlank
      else
      if LStCh = CBeginList then
      // *** début d'une sous-liste ? ***
        DoBeginList
      else
      if LStCh = CBeginPar then
      // *** début d'une expression ? ***
        DoBeginPar
      else
      if LStCh = CLink then
      // *** un lien ? ***
        DoLink
      else
      // *** fin de liste ou parenthèse ? => erreur ***
      if (LStCh = CEndList) or (LStCh = CEndPar) then
        fPos := LW // position de l'erreur
      else
      // *** autres caractères à ajouter simplement ***
      begin
        LItem := LItem + LStCh; // dans l'élément en cours
        NextChar; // caractère suivant
      end;
    end; // fin de la boucle
    if (fPos = 0) and (LItem <> EmptyStr) then // pas d'erreur ?
      LStrList.Add(LItem); // on sauve le dernier élément éventuel
  end; { fin de PARSE }

begin { début de ADD}
  BeginUpdate; // on marque le changement en cours
  Result := -1;
  fPos := 0; // pas d'erreur
  LStrList := TStringList.Create; // on crée la liste de travail
  try
    // est-ce une liste ?
    if (Length(St) > 1) and (St[1] = CBeginList)
      and (St[Length(St)] = CEndList) then
    begin
      // on l'analyse
      Parse(UTF8Copy(St, 2, UTF8Length(St) - 2));
      if fPos = 0 then // pas d'erreur pour le parsing
      begin
        for LSt in LStrList do // on ajoute tous les éléments de la liste
          Result := inherited Add(LSt);
      end
    end
    else
      fPos := 1; // erreur dès le départ
  finally
    LStrList.Free;
    fIsValid := (fPos = 0); // validité de la liste
    if not fIsValid then // une erreur ?
      // [### Erreur: mauvaise liste ###]
      Error.SetError(CE_BadList, St, fPos);
    EndUpdate; // fin de tout changement
  end;
end; { fin de ADD }

function TGVList.ButFirst: string;
// *** renvoie tout sauf le premier élément de la liste ***
// BF [mot1 mot2] => [mot2]
// BF [[sous-liste1] mot2 mot3 [sous-liste2]] => [mot2 mot3 [sous-liste2]]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not IsValid then // pas d'analyse si liste erronée
      Exit; // on sort
    if IsEmptyList then // l'élément existe-t-il ?
      // [### Erreur : liste vide ###]
      Error.SetError(CE_EmptyList, P_ButFirst)
    else
    begin
      for Li := 1 to (Count-1) do // on reconstruit la liste sans l'élément 1
         Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.ButLast: string;
// *** renvoie tout sauf le dernier élément de la liste ***
// BL [mot1 mot2] => [mot1]
// BL [[sous-liste1] mot2 mot3 [sous-liste2]] => [[sous-liste1] mot2 mot3]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not IsValid then // pas d'analyse si liste erronée
      Exit; // on sort
    if IsEmptyList then // l'élément existe-t-il ?
      // [### Erreur : liste vide ###]
      Error.SetError(CE_EmptyList, P_ButLast)
    else
    begin
      // on reconstruit la liste sans le dernier élément
      for Li := 0 to (Count - 2) do
         Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

constructor TGVList.Create;
// *** création de la liste ***
begin
  inherited Create; // on hérite
  fNumLastItem := -1; // dernier élément recherché
  fWord := TGVWord.Create; // création du mot de travail
  fUtil := TGVListUtils.Create; // utilitaires de travail
  fError := TGVErrors.Create; // on crée le gestionnaire d'erreurs
  fIsValid := False; // liste non valide par défaut
end;

destructor TGVList.Destroy;
// *** destruction de la liste ***
begin
  Error.Free; // on détruit le gestionnaire d'erreurs
  fWord.Free; // libération du mot de travail
  fUtil.Free; // idem pour les utilitaires
  inherited Destroy; // on hérite
end;

procedure TGVList.Clear;
// *** nettoyage ***
begin
  Error.Clear; // pas d'erreur
  inherited Clear; // on hérite
end;

function TGVList.DeleteItem(N: Integer): string;
// *** détruit l'élément N de la liste ***
// DL 1 [mot1 mot2]
// => [mot2]
// DL 3 [[sous-liste1] mot2 mot3 [sous-liste2]]
// => [[sous-liste1] mot2 [sous-liste2]]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    // les éléments existent-ils ?
    if (N > Count) or (N < 1) then
      // [### Erreur : élément inexistant ###]
      Error.SetError(CE_BadItem, RawStr, N)
    else
    begin
      for Li := 0 to (Count - 1) do // on reconstruit la liste
        if (Li <> (N - 1)) then // sans l'élément N
          Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.First: string;
// *** premier élément de la liste ***
// FL [mot1 mot2] => mot1
// CL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste1]
begin
  Result := CEmptyList; // liste vide par défaut
  if not IsValid then // pas d'analyse si liste erronée
    Exit; // on sort
  if IsEmptyList then // l'élément existe-t-il ?
    // [### Erreur : liste vide ###]
    Error.SetError(CE_EmptyList, P_First)
  else
    Result := Get(0); // on renvoie le premier élément
end;

function TGVList.InsertAItem(N: Integer; const St: string): string;
// *** insère un élément à la position N ***
// II 2 mot0 [mot1 mot2] => [mot1 mot0 mot2]
// II 3 mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot0 mot3 [sous-liste2]]
var
  Li: Integer;
  LSt1, LSt2: string;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not fUtil.IsValidValue(St) then // liste ou mot valides ?
    begin
      // [### Erreur :  valeur incorrecte ###]
      Error.SetError(CE_UnknownListWord, St, N);
      Exit; // on arrête !
    end;
    // les éléments existent-ils ?
    if (N > (Count + 1)) or (N < 1) then
      // [### Erreur : hors bornes ###]
      Error.SetError(CE_BadItem, RawStr, N)
    else
    for Li := 0 to Count do // on reconstruit la liste
    begin
      if (Li < Count) then
        LSt1 := Get(Li)
      else
        LSt1 := EmptyStr;
      if Li = (N - 1) then // c'est le point d'insertion ?
      begin
        LSt2 := St;
        if ((Result <> CBeginList) or (LSt2 <> EmptyStr)) and (LSt1 <> EmptyStr)
        then
          LSt1 := CBlank + LSt1;
        if (Result <> CBeginList) and (LSt2 <> EmptyStr) then
          LSt2 := CBlank + LSt2;
        Result := Result + LSt2 + LSt1; // on insère...
      end
      else
      begin
        if (Result <> CBeginList) and (LSt1 <> EmptyStr) then
          LSt1 := CBlank + LSt1;
        Result := Result + LSt1; // on ajoute l'élément suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

procedure TGVList.Insert(Index: Integer; const St: string);
// *** insertion d'un objet ***
begin
  BeginUpdate; // début de changement
  try
    if fUtil.IsValidValue(St) then // mot ou liste ?
      inherited Insert(Index, St) // on insère la valeur
    else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    EndUpdate; // fin de changement
  end;
end;

function TGVList.IsEmptyList: Boolean;
// *** la liste est-elle la liste vide ? ***
begin
  Result := (Count = 0);
end;

function TGVList.IsItem(const St: string): Boolean;
// *** l'élément donné est-il dans la liste ? ***
// II? mot0 [mot1 mot2] => faux
// II? mot2 [[sous-liste1] mot2 mot3 [sous-liste2]] => vrai
var
  Li: integer;
begin
  fNumLastItem := -1; // non trouvé
  for Li := 0 to (Count - 1) do
  begin
    if (St = Get(Li)) then // on balaie la liste
    begin
      fNumLastItem := Li + 1; // trouvé
      Break; // on sort de la boucle
    end;
  end;
  Result := (fNumLastItem <> -1);
end;

function TGVList.Last: string;
// *** renvoie le dernier élément de la liste ***
// LL [mot1 mot2] => mot2
// LL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste2]
begin
  Result := CEmptyList; // liste vide par défaut
  if not IsValid then // pas d'analyse si liste erronée
    Exit; // on sort
  if IsEmptyList then // l'élément existe-t-il ?
    // [### Erreur : liste vide ###]
    Error.SetError(CE_EmptyList, P_Last)
  else
    Result := Get(Count - 1);
end;

procedure TGVList.LoadFromStream(Stream: TStream);
// *** chargement de la liste ***
begin
  fLoading := True; // on indique un chargement
  try
    inherited LoadFromStream(Stream); // on charge
  finally
    fLoading := False; // fin du chargement
  end;
end;

procedure TGVList.Assign(Source: TPersistent);
// *** assign surchargée ***
begin
  if Source is TGVList then // est-ce une liste ?
  begin
    BeginUpdate;
    try
      Text := CBeginList + ToWBStr + CBlank + TGVList(Source).ToWBStr +
        CEndList; // on ajoute les nouvelles données
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TGVList.LowerCase: string;
// *** liste en minuscules ***
begin
  Result := UTF8LowerCase(RawStr);
end;

function TGVList.Rotate: string;
// *** rotation de la liste ***
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    for Li := 1 to (Count - 1) do // on reconstruit la liste
      Result := Result + Get(Li) + CBlank; // on ajoute l'élément suivant
    if Count <> 0 then // au moins un élément ?
      Result := Result + Get(0); // premier élément à la fin
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.AtRandom: string;
// *** renvoi d'un élément de la liste au hasard ***
begin
  Result := CEmptyList; // liste vide par défaut
  if Count > 0 then // si des éléments
    Result := Get(Random(Count)); // un au hasard
end;

function TGVList.Firsts: string;
// *** premier élément de chaque élément d'une liste ***
var
  LS: string;
  LL: TGVList;
  LW: TGVWord;
begin
  LL := TGVList.Create; // création de la liste de travail
  Result := CBeginList; // début de liste
  try
    LW := TGVWord.Create; // création du mot de travail
    try
    for LS in Self do  // on boucle
      if Error.Ok then // si pas d'erreur
      begin
        if LS[1] = CBeginList then // une liste ?
        begin
          LL.Text := LS; // liste récupérée
          if LL.IsValid then // correcte ?
            Result := Result + LL.First + CBlank // on ajoute le premier élément
          else
            // [### Erreur: mauvaise liste ###]
            Error.SetError(CE_BadList, LS);
        end
        else
        begin
          LW.Text := LS; // mot récupéré
          if LW.IsValid then  // correct ?
            // on récupère son dernier élément
            Result := Result + LW.First + CBlank
          else
            // [### Erreur: mauvais mot ###]
            Error.SetError(CE_BadWord, LS);
        end;
      end;
    finally
      LW.Free; // libération du mot de travail
    end;
  finally
    Result := TrimRight(Result) + CEndList; // fin de liste
    LL.Free; // libération de la liste de travail
  end;
end;

function TGVList.ButFirsts: string;
// *** tout sauf le premier élément de chaque élément d'une liste ***
var
  LS: string;
  LL: TGVList;
  LW: TGVWord;
begin
  LL := TGVList.Create; // création de la liste de travail
  Result := CBeginList; // début de liste
  try
    LW := TGVWord.Create; // création du mot de travail
    try
    for LS in Self do  // on boucle
      if Error.Ok then // si pas d'erreur
      begin
        if LS[1] = CBeginList then // une liste ?
        begin
          LL.Text := LS; // liste récupérée
          if LL.IsValid then // correcte ?
            // on ajoute le dernier élément
            Result := Result + LL.ButFirst + CBlank
          else
            // [### Erreur: mauvaise liste ###]
            Error.SetError(CE_BadList, LS);
        end
        else
        begin
          LW.Text := LS; // mot récupéré
          if LW.IsValid then  // correct ?
            // on récupère le dernier élément
            Result := Result + LW.ButFirst + CBlank
          else
            // [### Erreur: mauvais mot ###]
            Error.SetError(CE_BadWord, LS);
        end;
      end;
    finally
      LW.Free; // libération du mot de travail
    end;
  finally
    Result := TrimRight(Result) + CEndList; // fin de liste
    LL.Free; // libération de la liste de travail
  end;
end;

function TGVList.GetTextStr: string;
// *** récupération du texte ***
begin
  Result := inherited GetTextStr;
end;

procedure TGVList.SetTextStr(const AValue: string);
// *** initialisation du texte ***
begin
  fRawStr := AValue; // chaîne brute conservée
  if AValue <> EmptyStr then
    inherited SetTextStr(AValue)
  else
    // [### Erreur : liste vide ###]
    Error.SetError(CIE_ListInit, CE_GVLogo);
end;

procedure TGVList.Put(Index: Integer; const St: string);
// *** changement direct d'un élément ***
begin
  BeginUpdate;
  try
    if fUtil.IsValidValue(St) then // mot ou liste ?
      inherited Put(Index, St) // on change la valeur
    else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    EndUpdate;
  end;
end;

function TGVList.Get(Index: Integer): string;
// *** choix d'un élément ***
begin
  Result := CEmptyList; // liste vide par défaut
  BeginUpdate;
  try
    // dans les bornes ?
    if (Index >= 0) and (Index < Count) then
      Result:= inherited Get(Index) // on charge la valeur
    else
      // [### Erreur: élément invalide ###]
      Error.SetError(CE_BadItem, RawStr, Index);
  finally
    EndUpdate;
  end;
end;

function TGVList.PutFirst(const St: string): string;
// *** St comme premier élément de la liste ***
// PF [mot0] [mot1 mot2] => [[mot0] mot1 mot2]
// PF mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [mot0 [sous-liste1] mot2 mot3 [sous-liste2]]
var
  LSt: string;
begin
  Result := CBeginList;
  try
    LSt := ToWBStr; // liste en chaîne
    if (LSt <> EmptyStr) and (St <> EmptyStr) then
      LSt := CBlank + LSt;
    // on construit la liste
    Result := Result + St + LSt;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.PutLast(const St: string): string;
// *** St comme dernier élément de la liste ***
// PL [mot0] [mot1 mot2] => [mot1 mot2 [mot0]]
// PL mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot3 [sous-liste2] mot0]
var
  LSt: string;
begin
  Result := CBeginList;
  try
    LSt := ToWBStr; // liste en chaîne
    if (LSt <> EmptyStr) and (St <> EmptyStr) then
      LSt := LSt + CBlank;
    // on construit la liste
    Result := Result + LSt + St;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.ReplaceItem(N: Integer; const St: string): string;
// *** remplace l'élément N dans la liste par St ***
// RI 1 mot0 [mot1 mot2] => [mot1 mot0]
// RI 2 mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot0 [sous-liste2]]
var
  Li: Integer;
  LSt1, LSt2: string;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not fUtil.IsValidValue(St) then  // liste ou mot valides ?
    begin
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
      Exit; // on arrête !
    end;
    // les éléments existent-ils ?
    if (N > Count) or (N < 1) then
      // [### Erreur: hors bornes ###]
      Error.SetError(CE_BadItem, RawStr, N)
    else
    for Li := 0 to (Count - 1) do // on reconstruit la liste
    begin
      if Li = (N - 1) then // c'est le point de remplacement ?
      begin
        LSt1 := St;
        if (Result <> CBeginList) and (LSt1 <> EmptyStr) then
          LSt1 := CBlank + LSt1;
        Result := Result + LSt1; // on remplace
      end
      else
      begin
        LSt2 := Get(Li);
        if (Result <> CBeginList) and (LSt2 <> EmptyStr) then
          LSt2 := CBlank + LSt2;
        Result := Result + LSt2; // on ajoute l'élément suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.SentenceLeft(const St: string): string;
// *** phrase avec valeur à gauche ***
// SL [mot0] [mot1 mot2] => [mot0 mot1 mot2]
// SL mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [mot0 [sous-liste1] mot2 mot3 [sous-liste2]]
var
  LSt: string;
begin
  Result := CBeginList;
  try
    LSt := ToWBStr;
    if (LSt <> EmptyStr) and (St <> EmptyStr) and (St <> CEmptyList) then
      LSt := CBlank + LSt;
    fWord.Text := St;
    if fWord.IsValid then // mot valide ?
      Result := Result + St + LSt
    else
    if fUtil.IsValid(St) then // ou liste valide ?
      Result := Result + fUtil.ListToStr(St) + LSt
    else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.SentenceRight(const St: string): string;
// *** phrase avec valeur à droite ***
// SR [mot0] [mot1 mot2] => [mot1 mot2 mot0]
// SR mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot3 [sous-liste2] mot0]
var
  LSt1, LSt2: string;
begin
  Result := CBeginList;
  try
    LSt1 := ToWBStr;
    if (LSt1 <> EmptyStr) and (St <> EmptyStr) and (St <> CEmptyList) then
      LSt1 := LSt1 + CBlank;
    fWord.Text := St;
    if fWord.IsValid then // mot valide ?
      Result := Result + LSt1 + St
    else
    if fUtil.ListToStr(St, LSt2) then // liste valide ?
      Result := Result + LSt1 + LSt2
    else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.ShuffleItems: string;
// *** mélange des éléments ***
var
  Li: Integer;
  LL: TGVList;
begin
  LL := TGVList.Create; // création de la liste provisoire
  try
    LL.Text := RawStr; // on affecte la liste en cours
    for Li := 0 to Random(LL.Count * 4) do
      LL.Exchange(Random(LL.Count - 1), Random(LL.Count - 1));// on échange
    Result := LL.ToStr; // on renvoie le résultat
  finally
    LL.Free;
  end;
end;

function TGVList.SortItems: string;
// *** tri de la liste ***
var
  LL: TGVList;
begin
  LL := TGVList.Create; // création de la liste provisoire
  try
    LL.Text := RawStr; // on affecte la liste en cours
    LL.Sort; // on trie
    Result := LL.ToStr; // on renvoie le résultat
  finally
    LL.Free; // libération de la liste provisoire
  end;
end;

function TGVList.ReverseItems: string;
// *** inversion de la liste ***
var
  Li: Integer;
begin
  Result := EmptyStr; // chaîne vide
  try
    for Li := 0 to (Count - 1) do
      Result := Get(Li) + CBlank + Result; // liste à l'envers
  finally
    Result := CBeginList + Trim(Result) + CEndList; // on termine par le début
  end;
end;

function TGVList.ToWBStr: string;
// *** renvoie la liste sous forme de chaîne sans crochets ***
var
  Li: Integer;
begin
  BeginUpdate;
  Result := EmptyStr; // chaîne vide par défaut
  try
    for Li := 0 to (Count - 1) do // on construit la liste sans les crochets
      Result := Result + Get(Li) + CBlank; // élément par élément
  finally
    Result := TrimRight(Result); // on nettoie les blancs superflus
    EndUpdate;
  end;
end;

function TGVList.ToStr: string;
// *** renvoie la liste sous forme de chaîne ***
begin
  Result := CBeginList;
  try
    Result := Result + ToWBStr; // on construit la liste
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoAdd(const St1, St2: string): string;
// *** ajoute deux valeurs à la liste ***
// => utile pour les listes de propriétés
var
  LSt: string;
begin
  Result := CBeginList;
  try
    if not fUtil.IsValidValue(St1) then // liste ou mot valides 1
    begin
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St1);
      Exit; // on arrête !
    end;
    if not fUtil.IsValidValue(St2) then // liste ou mot valides 2
    begin
      // [### Erreur: mot incorrect ###]
      Error.SetError(CE_UnknownListWord, St2);
      Exit; // on arrête !
    end;
    LSt := ToWBStr;
    if LSt = EmptyStr then
      Result := Result + St1 + CBlank + St2
    else
      // on ajoute à la fin
      Result := Result + LSt + CBlank + St1 + CBlank + St2;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoDelete(const N: Integer): string;
// *** enlève deux valeurs à la liste à partir de N ***
// => utile pour les listes de propriétés
var
  Li: Integer;
begin
  Result := CBeginList; // début de la liste
  try
    // les éléments existent-ils ?
    if ((N > (Count - 1)) or (N < 1)) then
      // [### Erreur: liste trop courte ###]
      Error.SetError(CIE_TwoDelete, RawStr, N)
    else
    begin
      for Li := 0 to (Count - 1) do // on reconstruit la liste
        if (Li <> (N - 1)) and (Li <> N) then // sans les deux éléments
          Result := Result + Get(Li) + CBlank; // ajout
    end;
  finally
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.UpperCase: string;
// *** liste en majuscules ***
begin
  Result := UTF8UpperCase(RawStr);
end;

{ ========================================================== }

{ TGVListUtils }

constructor TGVListUtils.Create;
// *** création ***
begin
  inherited Create; // on hérite
  fWord := TGVWord.Create;  // mot de travail
  fSt := TGVString.Create; // chaîne de travail
  fError := TGVErrors.Create; // gestionnaire d'erreurs créé
end;

destructor TGVListUtils.Destroy;
// *** destruction ***
begin
  Error.Free; // on détruit le gestionnaire d'erreurs
  fWord.Free; // on libère le mot de travail
  fSt.Free; // idem pour la chaîne de travail
  inherited Destroy; // on hérite
end;

procedure TGVListUtils.Clear;
// *** nettoyage ***
begin
  Error.Clear; // pas d'erreur
  fWord.Clear; // mot nettoyé
  fSt.Clear; // idem chaîne de travail
end;

function TGVListUtils.EmptyList: string;
// *** renvoie la liste vide ***
begin
  Result := CEmptyList;
end;

function TGVListUtils.IsSimpleList(const St: string): Boolean;
// *** est-ce une liste simple ? ***
begin
  Result := (St <> EmptyStr) and (St[1] = CBeginList) and
    (St[Length(St)] = CEndList);
end;

function TGVListUtils.IsValid(const St: string): Boolean;
// *** teste la validité d'une liste ***
var
  LSt: string; // liste de travail
  LLevel: Integer; // niveau interne
begin
  // on cherche les crochets d'une chaîne non vide
  Result := IsSimpleList(St);
  fPos := 1; // pointe sur le début de la chaîne
  if not Result then
    Exit; // on sort si déjà une erreur
  LSt := Copy(St, 2, Length(St) - 2); // on retire les crochets
  // on boucle tant qu'il n'y a pas d'erreur et qu'il reste des caractères
  while Result and (fPos <= Length(LSt)) do
  begin
    case LSt[fPos] of
      CBeginList: // *** début d'une sous-liste ? ***
        begin
          Inc(fPos); // caractère suivant
          LLevel := 1; // premier niveau
          while (LLevel <> 0) and (fPos <= Length(LSt)) do // autres niveaux ?
          begin
            case LSt[fPos] of
              CLink: // un lien ?
                Inc(fPos); // on le saute
              CBeginList:
                Inc(LLevel); // niveau suivant
              CEndList:
                Dec(LLevel); // niveau précédent
            end;
            Inc(fPos); // caractère suivant
          end; // fin des autres niveaux
          Result := (LLevel = 0); // OK si niveau = 0
        end;
      CBeginPar: // *** début d'une expression ? ***
        begin
          Inc(fPos); // prochaine caractère
          LLevel := 1; // premier niveau
          while Result and (LLevel <> 0) and (fPos <= Length(LSt)) do
          // autres niveaux sans erreur ?
          begin
            case LSt[fPos] of
              CLink: // un lien ?
                Inc(fPos); // caractère suivant
              CBeginList, CEndList:
                Result := False; // pas de liste dans une expression
              CBeginPar:
                Inc(LLevel); // niveau suivant
              CEndPar:
                Dec(LLevel); // niveau précédent
            end;
            Inc(fPos); // caractère suivant
          end; // fin des autres niveaux
          if Result then
            Result := (LLevel = 0); // OK si niveau = 0 et pas d'erreur en amont
        end;
      CLink: // *** un lien ? ***
        Inc(fPos, 2); // on saute un caractère
      CEndList, CEndPar: // fin de liste ou de parenthèse ? => erreur
        Result := False; // mauvaise liste
    else // autres caractères
      Inc(fPos); // on les ignore
    end;
  end; // fin de la boucle
end;


function TGVListUtils.IsValidValue(const St: string): Boolean;
// *** vérifie que la valeur est soit une liste soit un mot corrects ***
begin
  fWord.Text := St;
  Result := fWord.IsValid or IsValid(St);
end;

function TGVListUtils.ListToStr(const St: string): string;
// *** change une liste en chaîne ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  if IsValid(St) then // liste valide ?
    Result := Copy(St, 2, Length(St) - 2) // on enlève les crochets
  else
    // [### Erreur: liste invalide ###]
    Error.SetError(CE_BadList, St, fPos);
end;

function TGVListUtils.ListToStr(const St: string; out ASt: string): Boolean;
// *** change une liste en chaîne ***
begin
  Result := IsValid(St); // liste valide ?
  if  Result then // liste valide ?
    ASt := Copy(St, 2, Length(St) - 2) // on enlève les crochets
  else
    ASt := EmptyStr; // vide si erreur
end;

function TGVListUtils.ListToWord(const St: string): string;
// *** change une liste en un mot normalisé ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  fSt.Str := ListToStr(St); // normalise la conversion de la liste
  if Error.OK then // pas d'erreur ?
    Result := fSt.Str; // résultat récupéré;
end;

function TGVListUtils.StrToList(const St: string): string;
// *** transforme une chaîne en liste ***
begin
  Result := CBeginList + St + CEndList; // on ajoute les crochets
  if not IsValid(Result) then // liste non valide ?
    // [### Erreur: liste invalide ###]
    Error.SetError(CE_BadList, St, fPos);
end;

function TGVListUtils.WordToList(const St: string): string;
// *** change un mot en liste ***
begin
  Result := EmptyStr; // chaîne vide par défaut
  fSt.Str := St; // normalise la conversion de la liste
  Result := StrToList(fSt.RawStr); // résultat récupéré sans formatage
end;

end.
