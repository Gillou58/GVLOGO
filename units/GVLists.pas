{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des listes                   |
  |                  Unit� : GVLists.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - premi�re version op�rationnelle

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

{$I GVDefines.inc} // fichier des d�finitions pr�alables

unit GVLists;
// L'unit� GVLISTS regroupe les classes charg�es de traiter les listes
// du projet GVLOGO.
//
// LISTES SIMPLES
//
// 1. Un mot est un ensemble de caract�res autres que le caract�re blanc.
// 2. Une liste est un ensemble de mots plac�s entre crochets : [ et ].
// 3. Une liste peut comprendre d'autres listes.
// 4. Un �l�ment d'une liste est soit un mot soit une liste.
// 5. Il existe une liste qui ne comprend aucun �l�ment et qu'on appelle
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
// Les listes sont � la base du langage GVLogo.
// Elles permettent un traitement des cha�nes de caract�res � partir de
// primitives simples.
// Les lignes d'un programme en GVLogo sont consid�r�es comme des listes.
//
// On notera que les �l�ments sont num�rot�s de 1 � Count.
// Cette fa�on de compter est diff�rente de celle des listes en Pascal.
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
  // *** utilitaires pour les listes pour d�finition ult�rieure ***
  TGVListUtils = class;

  // *** classe des listes ***
  TGVList = class(TStringList)
  strict private
    fRawStr: string; // cha�ne brute en entr�e
    fError: TGVErrors; // traitement des erreurs
    fIsValid: Boolean; // validit� de la liste
    fPos: Integer; // position de travail
    fLoading: Boolean; // chargement en cours ?
    fNumLastItem: Integer; // dernier �l�ment trouv�
    fWord: TGVWord; // mot de travail
    fUtil: TGVListUtils; // utilitaire pour liste
  protected
    procedure Put(Index: Integer; const St: string); override; // assignation
    function Get(Index: Integer): string; override; // �l�ment choisi
    function GetTextStr: string; override; // r�cup�ration du texte
    procedure SetTextStr(const AValue: string); override; // validation du texte
  public
    constructor Create; overload; // cr�ation
    destructor Destroy; override; // destruction
    procedure Clear; override; // nettoyage
    function Add(const St: string): Integer; override; // ajout
    procedure LoadFromStream(Stream: TStream); overload; override; // chargement
    procedure Assign(Source: TPersistent); override; // assignation
    procedure Insert(Index: Integer; const St: string); override; // insertion
    // nouvelles m�thodes (*** ne modifient pas la liste interne ***)
    // renvoie la liste sous forme de cha�ne
    function ToStr: string;
    // renvoie la liste sous forme de cha�ne sans crochets
    function ToWBStr: string;
    // la liste est-elle la liste vide ?
    function IsEmptyList: Boolean;
    // renvoie le premier �l�ment de la liste
    function First: string;
    // renvoie le dernier �l�ment de la liste
    function Last: string;
    // sauf le premier de la liste
    function ButFirst: string;
    // sauf le dernier de la liste
    function ButLast: string;
    // supprime l'�l�ment N
    function DeleteItem(N: Integer): string;
    // insertion d'un �l�ment en position N
    function InsertAItem(N: Integer; const St: string): string;
    // remplacement de l'�l�ment N
    function ReplaceItem(N: Integer; const St: string): string;
    // met en premier
    function PutFirst(const St: string): string;
    // met en dernier
    function PutLast(const St: string): string;
    // phrase � droite
    function SentenceRight(const St: string): string;
    // phrase � gauche
    function SentenceLeft(const St: string): string;
    // tri des �l�ments
    function SortItems: string;
    // inversion des �l�ments
    function ReverseItems: string;
    // m�lange des �l�ments
    function ShuffleItems: string;
    // membre pr�sent ?
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
    // �l�ment de la liste choisi au hasard
    function AtRandom: string;
    // dernier �l�ment trait�
    property LastItem: Integer read fNumLastItem default -1;
    // cha�ne brute en entr�e
    property RawStr: string read fRawStr; // cha�ne brute en entr�e
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
    // validit� de la liste
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
    fSt: TGVString; // cha�ne de travail
  public
    constructor Create; // constructeur
    destructor Destroy; override; // destructeur
    procedure Clear; // nettoyage
    // conversion d'une liste en mot
    function ListToWord(const St: string): string;
    // conversion d'un mot en liste
    function WordToList(const St: string): string;
    // conversion d'une liste en cha�ne
    function ListToStr(const St: string): string; overload;
    function ListToStr(const St: string; out ASt: string): Boolean; overload;
    // conversion d'une cha�ne en liste
    function StrToList(const St: string): string;
    // retourne la liste vide
    function EmptyList: string;
    // v�rifie la validit� d'une liste
    function IsValid(const St: string): Boolean;
    // teste la validit� d'une valeur (mot ou liste)
    function IsValidValue(const St: string): Boolean;
    // liste simple ?
    function IsSimpleList(const St: string): Boolean;
    // notification d'une erreur
    property Error: TGVErrors read fError write fError;
  end;

implementation

uses
  lazutf8; // traitement des cha�nes UTF8

{ TGVList }

function TGVList.Add(const St: string): Integer;
// *** ajout d'une cha�ne ***
var
  LSt: string;
  LStrList: TStringList; // liste de travail

  procedure Parse(const LSt2: string);
  // *** d�coupe la liste de travail ***
  var
    LW: Integer; // place du caract�re examin�
    Lev: Integer; // niveau de travail
    LItem: string; // �l�ment en cours
    LStCh: string; // caract�re en cours (1 ou 2 places)

    procedure NextChar;
    // vers le caract�re suivant
    begin
      Inc(LW); // pointe sur le caract�re suivant
      LStCh := UTF8Copy(LSt2, LW, 1); // nouveau caract�re ne cours
    end;

    procedure DoBlank;
    // traite un blanc
    begin
      NextChar; // caract�re suivant
      // saute les blancs
      while (LStCh = CBlank) and (LW <= UTF8Length(LSt2)) do
        NextChar; // caract�re suivant
      if LItem <> EmptyStr then
      begin
        LStrList.Add(LItem); // ajoute l'�l�ment si non vide
        LItem := EmptyStr; // �l�ment � z�ro
      end;
    end;

    procedure DoBeginList;
    // traite un d�but de liste
    begin
        if LItem <> EmptyStr then // ajoute l'�l�ment si non vide
          LStrList.Add(LItem);
        LItem := CBeginList; // �l�ment = [
        NextChar; // caract�re suivant
        Lev := 1; // niveau 1
        while (Lev <> 0) and (LW <= UTF8Length(LSt2)) do // autres niveaux ?
        begin
          if LStCh = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'�l�ment
            NextChar; // caract�re suivant
          end
          else
          if LStCh = CBeginList then
            Inc(Lev) // niveau suivant
          else
          if LStCh = CEndList then
            Dec(Lev); // niveau pr�c�dent
          if LW <= UTF8Length(LSt2) then // si possible
                LItem := LItem + LStCh; // sauve l'�l�ment suivant
          NextChar; // caract�re suivant
       end; // fin des niveaux
       if Lev = 0 then // normalement niveau = 0 � la sortie
       begin // c'est-�-dire autant de [ que de ]
         LStrList.Add(LItem); // sauve le dernier �l�ment
         LItem := EmptyStr; // et le remet � z�ro
       end
       else // si niveaux incorrects
         fPos := LW; // note la position de l'erreur
    end;

    procedure DoBeginPar;
    // d�but de parenth�se
    begin
        if LItem <> EmptyStr then // sauve l'�l�ment si non vide
          LStrList.Add(LItem);
        LItem := CBeginPar; // �l�ment = (
        NextChar; // caract�re suivant
        Lev := 1; // premier niveau
        while (Lev <> 0) and (LW <= UTF8Length(LSt2)) do // autres niveaux ?
        begin
          if LStCh = CLink then // un lien ?
          begin
            LItem := LItem + CLink; // dans l'�l�ment
            NextChar; // caract�re suivant
          end
          else
          if (LStCh = CBeginList) or (LStCh = CEndList) then
            fPos := LW // pas de liste dans une expression
          else
          if LStCh = CBeginPar then
            Inc(Lev) // niveau suivant
          else
          if LStCh = CEndPar then
            Dec(Lev); // niveau pr�c�dent
          if LW <= UTF8Length(LSt2) then // si possible
            LItem := LItem + LStCh; // dans l'�l�ment
          NextChar; // caract�re suivant
        end; // fin des autres niveaux
        if (Lev = 0) and (fPos = 0) then // nombre correct de niveaux ?
        begin
          LStrList.Add(LItem); // sauve le dernier �l�ment
          LItem := EmptyStr; // remis � z�ro
        end
        else // si erreur
          if fPos = 0 then
            fPos := LW; // note la position de l'erreur si non connue
    end;

    procedure DoLink;
    // d�but d'un lien
    begin
        LItem := LItem + CLink; // dans l'�l�ment en cours
        NextChar; // caract�re suivant
        if LW <= UTF8Length(LSt2) then // si possible
        begin
          LItem := LItem + LStCh; // dans l'�l�ment en cours
          NextChar; // caract�re suivant
        end;
    end;

  begin { d�but de PARSE }
    fPos := 0; // pas d'erreur
    LStrList.Clear; // on nettoie la liste de travail
    if LSt2 = EmptyStr then // si cha�ne vide
      Exit; // on sort
    LItem := EmptyStr; // �l�ment en cours vide
    LW := 0; // pointe sur le premier caract�re
    NextChar; // caract�re suivant
    while (fPos = 0) and (LW <= UTF8Length(LSt2)) do // on boucle si possible
    begin
      if LStCh = CBlank then
      // *** blanc ? ***
        DoBlank
      else
      if LStCh = CBeginList then
      // *** d�but d'une sous-liste ? ***
        DoBeginList
      else
      if LStCh = CBeginPar then
      // *** d�but d'une expression ? ***
        DoBeginPar
      else
      if LStCh = CLink then
      // *** un lien ? ***
        DoLink
      else
      // *** fin de liste ou parenth�se ? => erreur ***
      if (LStCh = CEndList) or (LStCh = CEndPar) then
        fPos := LW // position de l'erreur
      else
      // *** autres caract�res � ajouter simplement ***
      begin
        LItem := LItem + LStCh; // dans l'�l�ment en cours
        NextChar; // caract�re suivant
      end;
    end; // fin de la boucle
    if (fPos = 0) and (LItem <> EmptyStr) then // pas d'erreur ?
      LStrList.Add(LItem); // on sauve le dernier �l�ment �ventuel
  end; { fin de PARSE }

begin { d�but de ADD}
  BeginUpdate; // on marque le changement en cours
  Result := -1;
  fPos := 0; // pas d'erreur
  LStrList := TStringList.Create; // on cr�e la liste de travail
  try
    // est-ce une liste ?
    if (Length(St) > 1) and (St[1] = CBeginList)
      and (St[Length(St)] = CEndList) then
    begin
      // on l'analyse
      Parse(UTF8Copy(St, 2, UTF8Length(St) - 2));
      if fPos = 0 then // pas d'erreur pour le parsing
      begin
        for LSt in LStrList do // on ajoute tous les �l�ments de la liste
          Result := inherited Add(LSt);
      end
    end
    else
      fPos := 1; // erreur d�s le d�part
  finally
    LStrList.Free;
    fIsValid := (fPos = 0); // validit� de la liste
    if not fIsValid then // une erreur ?
      // [### Erreur: mauvaise liste ###]
      Error.SetError(CE_BadList, St, fPos);
    EndUpdate; // fin de tout changement
  end;
end; { fin de ADD }

function TGVList.ButFirst: string;
// *** renvoie tout sauf le premier �l�ment de la liste ***
// BF [mot1 mot2] => [mot2]
// BF [[sous-liste1] mot2 mot3 [sous-liste2]] => [mot2 mot3 [sous-liste2]]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not IsValid then // pas d'analyse si liste erron�e
      Exit; // on sort
    if IsEmptyList then // l'�l�ment existe-t-il ?
      // [### Erreur : liste vide ###]
      Error.SetError(CE_EmptyList, P_ButFirst)
    else
    begin
      for Li := 1 to (Count-1) do // on reconstruit la liste sans l'�l�ment 1
         Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.ButLast: string;
// *** renvoie tout sauf le dernier �l�ment de la liste ***
// BL [mot1 mot2] => [mot1]
// BL [[sous-liste1] mot2 mot3 [sous-liste2]] => [[sous-liste1] mot2 mot3]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    if not IsValid then // pas d'analyse si liste erron�e
      Exit; // on sort
    if IsEmptyList then // l'�l�ment existe-t-il ?
      // [### Erreur : liste vide ###]
      Error.SetError(CE_EmptyList, P_ButLast)
    else
    begin
      // on reconstruit la liste sans le dernier �l�ment
      for Li := 0 to (Count - 2) do
         Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

constructor TGVList.Create;
// *** cr�ation de la liste ***
begin
  inherited Create; // on h�rite
  fNumLastItem := -1; // dernier �l�ment recherch�
  fWord := TGVWord.Create; // cr�ation du mot de travail
  fUtil := TGVListUtils.Create; // utilitaires de travail
  fError := TGVErrors.Create; // on cr�e le gestionnaire d'erreurs
  fIsValid := False; // liste non valide par d�faut
end;

destructor TGVList.Destroy;
// *** destruction de la liste ***
begin
  Error.Free; // on d�truit le gestionnaire d'erreurs
  fWord.Free; // lib�ration du mot de travail
  fUtil.Free; // idem pour les utilitaires
  inherited Destroy; // on h�rite
end;

procedure TGVList.Clear;
// *** nettoyage ***
begin
  Error.Clear; // pas d'erreur
  inherited Clear; // on h�rite
end;

function TGVList.DeleteItem(N: Integer): string;
// *** d�truit l'�l�ment N de la liste ***
// DL 1 [mot1 mot2]
// => [mot2]
// DL 3 [[sous-liste1] mot2 mot3 [sous-liste2]]
// => [[sous-liste1] mot2 [sous-liste2]]
var
  Li: Integer;
begin
  Result := CBeginList; // crochet ouvrant
  try
    // les �l�ments existent-ils ?
    if (N > Count) or (N < 1) then
      // [### Erreur : �l�ment inexistant ###]
      Error.SetError(CE_BadItem, RawStr, N)
    else
    begin
      for Li := 0 to (Count - 1) do // on reconstruit la liste
        if (Li <> (N - 1)) then // sans l'�l�ment N
          Result := Result + Get(Li) + CBlank;
    end;
  finally
    // on termine par le crochet fermant
    Result := TrimRight(Result) + CEndList;
  end;
end;

function TGVList.First: string;
// *** premier �l�ment de la liste ***
// FL [mot1 mot2] => mot1
// CL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste1]
begin
  Result := CEmptyList; // liste vide par d�faut
  if not IsValid then // pas d'analyse si liste erron�e
    Exit; // on sort
  if IsEmptyList then // l'�l�ment existe-t-il ?
    // [### Erreur : liste vide ###]
    Error.SetError(CE_EmptyList, P_First)
  else
    Result := Get(0); // on renvoie le premier �l�ment
end;

function TGVList.InsertAItem(N: Integer; const St: string): string;
// *** ins�re un �l�ment � la position N ***
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
      Exit; // on arr�te !
    end;
    // les �l�ments existent-ils ?
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
        Result := Result + LSt2 + LSt1; // on ins�re...
      end
      else
      begin
        if (Result <> CBeginList) and (LSt1 <> EmptyStr) then
          LSt1 := CBlank + LSt1;
        Result := Result + LSt1; // on ajoute l'�l�ment suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

procedure TGVList.Insert(Index: Integer; const St: string);
// *** insertion d'un objet ***
begin
  BeginUpdate; // d�but de changement
  try
    if fUtil.IsValidValue(St) then // mot ou liste ?
      inherited Insert(Index, St) // on ins�re la valeur
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
// *** l'�l�ment donn� est-il dans la liste ? ***
// II? mot0 [mot1 mot2] => faux
// II? mot2 [[sous-liste1] mot2 mot3 [sous-liste2]] => vrai
var
  Li: integer;
begin
  fNumLastItem := -1; // non trouv�
  for Li := 0 to (Count - 1) do
  begin
    if (St = Get(Li)) then // on balaie la liste
    begin
      fNumLastItem := Li + 1; // trouv�
      Break; // on sort de la boucle
    end;
  end;
  Result := (fNumLastItem <> -1);
end;

function TGVList.Last: string;
// *** renvoie le dernier �l�ment de la liste ***
// LL [mot1 mot2] => mot2
// LL [[sous-liste1] mot2 mot3 [sous-liste2]] => [sous-liste2]
begin
  Result := CEmptyList; // liste vide par d�faut
  if not IsValid then // pas d'analyse si liste erron�e
    Exit; // on sort
  if IsEmptyList then // l'�l�ment existe-t-il ?
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
// *** assign surcharg�e ***
begin
  if Source is TGVList then // est-ce une liste ?
  begin
    BeginUpdate;
    try
      Text := CBeginList + ToWBStr + CBlank + TGVList(Source).ToWBStr +
        CEndList; // on ajoute les nouvelles donn�es
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
      Result := Result + Get(Li) + CBlank; // on ajoute l'�l�ment suivant
    if Count <> 0 then // au moins un �l�ment ?
      Result := Result + Get(0); // premier �l�ment � la fin
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.AtRandom: string;
// *** renvoi d'un �l�ment de la liste au hasard ***
begin
  Result := CEmptyList; // liste vide par d�faut
  if Count > 0 then // si des �l�ments
    Result := Get(Random(Count)); // un au hasard
end;

function TGVList.GetTextStr: string;
// *** r�cup�ration du texte ***
begin
  Result := inherited GetTextStr;
end;

procedure TGVList.SetTextStr(const AValue: string);
// *** initialisation du texte ***
begin
  fRawStr := AValue; // cha�ne brute conserv�e
  if AValue <> EmptyStr then
    inherited SetTextStr(AValue)
  else
    // [### Erreur : liste vide ###]
    Error.SetError(CIE_ListInit, CE_GVLogo);
end;

procedure TGVList.Put(Index: Integer; const St: string);
// *** changement direct d'un �l�ment ***
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
// *** choix d'un �l�ment ***
begin
  Result := CEmptyList; // liste vide par d�faut
  BeginUpdate;
  try
    // dans les bornes ?
    if (Index >= 0) and (Index < Count) then
      Result:= inherited Get(Index) // on charge la valeur
    else
      // [### Erreur: �l�ment invalide ###]
      Error.SetError(CE_BadItem, RawStr, Index);
  finally
    EndUpdate;
  end;
end;

function TGVList.PutFirst(const St: string): string;
// *** St comme premier �l�ment de la liste ***
// PF [mot0] [mot1 mot2] => [[mot0] mot1 mot2]
// PF mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [mot0 [sous-liste1] mot2 mot3 [sous-liste2]]
var
  LSt: string;
begin
  Result := CBeginList;
  try
    if fUtil.IsValidValue(St) then // liste ou mot valides
    begin
      LSt := ToWBStr; // liste en cha�ne
      if (LSt <> EmptyStr) and (St <> EmptyStr) then
        LSt := CBlank + LSt;
      // on construit la liste
      Result := Result + St + LSt;
    end
    else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.PutLast(const St: string): string;
// *** St comme dernier �l�ment de la liste ***
// PL [mot0] [mot1 mot2] => [mot1 mot2 [mot0]]
// PL mot0 [[sous-liste1] mot2 mot3 [sous-liste2]] =>
// [[sous-liste1] mot2 mot3 [sous-liste2] mot0]
var
  LSt: string;
begin
  Result := CBeginList;
  try
    if fUtil.IsValidValue(St) then // liste ou mot valides
    begin
      LSt := ToWBStr; // liste en cha�ne
      if (LSt <> EmptyStr) and (St <> EmptyStr) then
        LSt := LSt + CBlank;
      // on construit la liste
      Result := Result + LSt + St;
    end
     else
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St);
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.ReplaceItem(N: Integer; const St: string): string;
// *** remplace l'�l�ment N dans la liste par St ***
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
      Exit; // on arr�te !
    end;
    // les �l�ments existent-ils ?
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
        Result := Result + LSt2; // on ajoute l'�l�ment suivant
      end;
    end;
  finally
    Result := Result + CEndList; // et on termine par le crochet fermant
  end;
end;

function TGVList.SentenceLeft(const St: string): string;
// *** phrase avec valeur � gauche ***
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
// *** phrase avec valeur � droite ***
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
// *** m�lange des �l�ments ***
var
  Li: Integer;
  LL: TGVList;
begin
  LL := TGVList.Create; // cr�ation de la liste provisoire
  try
    LL.Text := RawStr; // on affecte la liste en cours
    for Li := 0 to Random(LL.Count * 4) do
      LL.Exchange(Random(LL.Count - 1), Random(LL.Count - 1));// on �change
    Result := LL.ToStr; // on renvoie le r�sultat
  finally
    LL.Free;
  end;
end;

function TGVList.SortItems: string;
// *** tri de la liste ***
var
  LL: TGVList;
begin
  LL := TGVList.Create; // cr�ation de la liste provisoire
  try
    LL.Text := RawStr; // on affecte la liste en cours
    LL.Sort; // on trie
    Result := LL.ToStr; // on renvoie le r�sultat
  finally
    LL.Free; // lib�ration de la liste provisoire
  end;
end;

function TGVList.ReverseItems: string;
// *** inversion de la liste ***
var
  Li: Integer;
begin
  Result := EmptyStr; // cha�ne vide
  try
    for Li := 0 to (Count - 1) do
      Result := Get(Li) + CBlank + Result; // liste � l'envers
  finally
    Result := CBeginList + Trim(Result) + CEndList; // on termine par le d�but
  end;
end;

function TGVList.ToWBStr: string;
// *** renvoie la liste sous forme de cha�ne sans crochets ***
var
  Li: Integer;
begin
  BeginUpdate;
  Result := EmptyStr; // cha�ne vide par d�faut
  try
    for Li := 0 to (Count - 1) do // on construit la liste sans les crochets
      Result := Result + Get(Li) + CBlank; // �l�ment par �l�ment
  finally
    Result := TrimRight(Result); // on nettoie les blancs superflus
    EndUpdate;
  end;
end;

function TGVList.ToStr: string;
// *** renvoie la liste sous forme de cha�ne ***
begin
  Result := CBeginList;
  try
    Result := Result + ToWBStr; // on construit la liste
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoAdd(const St1, St2: string): string;
// *** ajoute deux valeurs � la liste ***
// => utile pour les listes de propri�t�s
var
  LSt: string;
begin
  Result := CBeginList;
  try
    if not fUtil.IsValidValue(St1) then // liste ou mot valides 1
    begin
      // [### Erreur: ni une liste ni un mot ###]
      Error.SetError(CE_UnknownListWord, St1);
      Exit; // on arr�te !
    end;
    if not fUtil.IsValidValue(St2) then // liste ou mot valides 2
    begin
      // [### Erreur: mot incorrect ###]
      Error.SetError(CE_UnknownListWord, St2);
      Exit; // on arr�te !
    end;
    LSt := ToWBStr;
    if LSt = EmptyStr then
      Result := Result + St1 + CBlank + St2
    else
      // on ajoute � la fin
      Result := Result + LSt + CBlank + St1 + CBlank + St2;
  finally
    Result := Result + CEndList;
  end;
end;

function TGVList.TwoDelete(const N: Integer): string;
// *** enl�ve deux valeurs � la liste � partir de N ***
// => utile pour les listes de propri�t�s
var
  Li: Integer;
begin
  Result := CBeginList; // d�but de la liste
  try
    // les �l�ments existent-ils ?
    if ((N > (Count - 1)) or (N < 1)) then
      // [### Erreur: liste trop courte ###]
      Error.SetError(CIE_TwoDelete, RawStr, N)
    else
    begin
      for Li := 0 to (Count - 1) do // on reconstruit la liste
        if (Li <> (N - 1)) and (Li <> N) then // sans les deux �l�ments
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
// *** cr�ation ***
begin
  inherited Create; // on h�rite
  fWord := TGVWord.Create;  // mot de travail
  fSt := TGVString.Create; // cha�ne de travail
  fError := TGVErrors.Create; // gestionnaire d'erreurs cr��
end;

destructor TGVListUtils.Destroy;
// *** destruction ***
begin
  Error.Free; // on d�truit le gestionnaire d'erreurs
  fWord.Free; // on lib�re le mot de travail
  fSt.Free; // idem pour la cha�ne de travail
  inherited Destroy; // on h�rite
end;

procedure TGVListUtils.Clear;
// *** nettoyage ***
begin
  Error.Clear; // pas d'erreur
  fWord.Clear; // mot nettoy�
  fSt.Clear; // idem cha�ne de travail
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
// *** teste la validit� d'une liste ***
var
  LSt: string; // liste de travail
  LLevel: Integer; // niveau interne
begin
  // on cherche les crochets d'une cha�ne non vide
  Result := IsSimpleList(St);
  fPos := 1; // pointe sur le d�but de la cha�ne
  if not Result then
    Exit; // on sort si d�j� une erreur
  LSt := Copy(St, 2, Length(St) - 2); // on retire les crochets
  // on boucle tant qu'il n'y a pas d'erreur et qu'il reste des caract�res
  while Result and (fPos <= Length(LSt)) do
  begin
    case LSt[fPos] of
      CBeginList: // *** d�but d'une sous-liste ? ***
        begin
          Inc(fPos); // caract�re suivant
          LLevel := 1; // premier niveau
          while (LLevel <> 0) and (fPos <= Length(LSt)) do // autres niveaux ?
          begin
            case LSt[fPos] of
              CLink: // un lien ?
                Inc(fPos); // on le saute
              CBeginList:
                Inc(LLevel); // niveau suivant
              CEndList:
                Dec(LLevel); // niveau pr�c�dent
            end;
            Inc(fPos); // caract�re suivant
          end; // fin des autres niveaux
          Result := (LLevel = 0); // OK si niveau = 0
        end;
      CBeginPar: // *** d�but d'une expression ? ***
        begin
          Inc(fPos); // prochaine caract�re
          LLevel := 1; // premier niveau
          while Result and (LLevel <> 0) and (fPos <= Length(LSt)) do
          // autres niveaux sans erreur ?
          begin
            case LSt[fPos] of
              CLink: // un lien ?
                Inc(fPos); // caract�re suivant
              CBeginList, CEndList:
                Result := False; // pas de liste dans une expression
              CBeginPar:
                Inc(LLevel); // niveau suivant
              CEndPar:
                Dec(LLevel); // niveau pr�c�dent
            end;
            Inc(fPos); // caract�re suivant
          end; // fin des autres niveaux
          if Result then
            Result := (LLevel = 0); // OK si niveau = 0 et pas d'erreur en amont
        end;
      CLink: // *** un lien ? ***
        Inc(fPos, 2); // on saute un caract�re
      CEndList, CEndPar: // fin de liste ou de parenth�se ? => erreur
        Result := False; // mauvaise liste
    else // autres caract�res
      Inc(fPos); // on les ignore
    end;
  end; // fin de la boucle
end;


function TGVListUtils.IsValidValue(const St: string): Boolean;
// *** v�rifie que la valeur est soit une liste soit un mot corrects ***
begin
  fWord.Text := St;
  Result := fWord.IsValid or IsValid(St);
end;

function TGVListUtils.ListToStr(const St: string): string;
// *** change une liste en cha�ne ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  if IsValid(St) then // liste valide ?
    Result := Copy(St, 2, Length(St) - 2) // on enl�ve les crochets
  else
    // [### Erreur: liste invalide ###]
    Error.SetError(CE_BadList, St, fPos);
end;

function TGVListUtils.ListToStr(const St: string; out ASt: string): Boolean;
// *** change une liste en cha�ne ***
begin
  Result := IsValid(St); // liste valide ?
  if  Result then // liste valide ?
    ASt := Copy(St, 2, Length(St) - 2) // on enl�ve les crochets
  else
    ASt := EmptyStr; // vide si erreur
end;

function TGVListUtils.ListToWord(const St: string): string;
// *** change une liste en un mot normalis� ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  fSt.Str := ListToStr(St); // normalise la conversion de la liste
  if Error.OK then // pas d'erreur ?
    Result := fSt.Str; // r�sultat r�cup�r�;
end;

function TGVListUtils.StrToList(const St: string): string;
// *** transforme une cha�ne en liste ***
begin
  Result := CBeginList + St + CEndList; // on ajoute les crochets
  if not IsValid(Result) then // liste non valide ?
    // [### Erreur: liste invalide ###]
    Error.SetError(CE_BadList, St, fPos);
end;

function TGVListUtils.WordToList(const St: string): string;
// *** change un mot en liste ***
begin
  Result := EmptyStr; // cha�ne vide par d�faut
  fSt.Str := St; // normalise la conversion de la liste
  Result := StrToList(fSt.RawStr); // r�sultat r�cup�r� sans formatage
end;

end.
