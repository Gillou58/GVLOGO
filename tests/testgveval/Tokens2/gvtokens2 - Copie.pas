{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Eléments d'une expression (2)           |
  |                  Unité : GVTokens2.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    06-11-2014 16:15:20                          |
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

  // élément de base de l'expression
  TGVBaseItem = record
    Token: String; // élément
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
    fErrorPos: Integer; // position d'une erreur
    fOnError: TNotifyEvent; // gestionnaire d'erreurs
    fText: string; // texte à analyser
    fIndx: Integer; // index de départ dans la chaîne de travail
    fItemList: TGVItems; // éléments de la chaîne de travail
    fError: TGVError; // erreur en cours
    procedure SetIndx(AValue: Integer); // index de départ fixé
    procedure SetText(const AValue: string); // texte en cours fixé
    procedure SetError(const Err: TGVError); // erreur fixée
    procedure WipeItems; inline; // nettoyage du tableau des éléments
  protected
    function GetKind(const AItem: string): CTokensEnum; // type d'un élément
    procedure AddItem(const AItem: string); virtual; // ajoute un élément
    procedure Tokenize; virtual; // répartition en éléments
  public
    constructor Create; overload; // constructeur simple
    // constructeur avec initialisation
    constructor Create(const AText: string); overload;
    destructor Destroy; override; // destructeur
    function GetEnumerator: TGVTokensEnumerator; // énumération
    property Text: string read fText write SetText; // expression à analyser
    property Indx: Integer read fIndx write SetIndx default 1; // index de départ
    property Error: TGVError read fError default C_NoInit; // erreur en cours
    property ErrorPos: Integer read fErrorPos default -1; // position de l'erreur
    // événement lié à une erreur
    property OnError: TNotifyEvent read fOnError write fOnError;
  end;

implementation

{ TGVTokens2 }

procedure TGVTokens2.SetText(const AValue: string);
// *** fixe l'expression à analyser ***
begin
  if fText = AValue then
    Exit; // sortie si aucun changement
  fText := AValue; // nouvelle valeur
  Tokenize; // répartit en éléments
  if Length(fItemList) > 0 then // des éléments ?
  begin
    SetError(C_None); // pas d'erreur
    // on calculera ici !
  end;
end;

procedure TGVTokens2.SetError(const Err: TGVError);
// *** fixe l'erreur en cours ***
begin
  fError := Err; // erreur stockée
  // événement erreur
  if Assigned(fOnError) then
    fOnError(Self);
end;

procedure TGVTokens2.WipeItems;
// *** nettoyage du tableau des éléments
begin
  SetLength(fItemList, 0);
end;

function TGVTokens2.GetKind(const AItem: string): CTokensEnum;
// *** détermine le type d'un élément ***
begin
  if AItem = EmptyStr then // chaîne vide ?
    Result := cteEnd // fin du traitement
  else
  if AItem[1] = CColon then // est-ce une variable ?
  begin
    if ValidVar(AItem) then // nom valide ?
      Result := cteVar // variable enregistrée
    else
    begin
      Result := cteUnKnown; // signale l'erreur
      SetError(C_UnKnownVar); // variable inconnue
    end;
  end
  else
  if AItem[1] in CDigit then // est-ce un nombre ?
  begin
    if ValidNumber(AItem) then // nombre valide ?
      Result := cteNumber // nombre enregistré
    else
    begin
      Result := cteUnknown; // signale l'erreur
      SetError(C_BadNumber); // nombre incorrect
    end;
  end
  else
  if Length(AItem) = 1 then
  begin
    Ch := AItem[1];
    with fItemList[Length(fItemList) - 1] do
      case Ch of
        CPlus: Kind := ctePlus; // addition
        CMinus: Kind := cteMinus; // soustraction
        CMul: Kind := cteMul; // multiplication
        CDiv: Kind := cteDiv; // division
        CPower: Kind := ctePower; // puisssance
        CGreater: Kind := cteGreater; // plus grand
        CLower: Kind := cteLower; // plus petit
        CEqual: Kind := cteEqual; // égal
        CBeginPar: Kind := cteBeginExp; // parenthèse ouvrante
        CEndPar: Kind := cteEndExp; // parenthèse fermante
      end;
  end
  else
  begin
    with fItemList[Length(fItemList) - 1] do
    begin
      if AItem = CNotEqual then // inégalité
        Kind := cteNotEqual
      else
      if AItem = CGreaterOrEqual then  // >=
        Kind := cteGreaterOrEqual
      else
      if AItem = CLowerOrEqual then  // <=
        Kind := cteLowerOrEqual
      else
      if AnsiUppercase(AItem) = MF_Or then // ou
        Kind := cteOr
      if AnsiUppercase(AItem) = MF_And then // et
        Kind := cteAnd
      if AnsiUppercase(AItem) = MF_Mod then // mod
        Kind := cteMod
      if AnsiUppercase(AItem) = MF_Not then // not
        Kind := cteNOT
      // else
      // if GetFunction(AItem) then
      // Kind := cteFunction
      // else
      // begin
      //   Kind := cteUnKnown;
      //   SetError(C_BadFunction); // sortir de l'analyse
      // end;
    end;

    {cteEnd, cteNumber}
  end;
end;

procedure TGVTokens2.AddItem(const AItem: string);
// *** ajoute un élément ***
var
  Ch: Char;
begin
  SetLength(fItemList, Length(fItemList) + 1); // adapte la longueur du tableau
  with fItemList[Length(fItemList) - 1] do
    begin
      Token := AItem; // valeur de l'élément
      Kind := GetKind(AItem);
    end;
end;

procedure TGVTokens2.SetIndx(AValue: Integer);
// *** fixe l'index de départ dans l'expression ***
begin
  if fIndx = AValue then
    Exit; // on sort si aucun changement
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    raise EEvalException.CreateFmt(ME_OutOfRange, [Indx, Text]);
  fIndx := AValue; // nouvelle valeur de l'index
end;

constructor TGVTokens2.Create;
// *** constructeur simple ***
begin
  inherited Create; // on hérite
  fError := C_NoInit; // erreur par défaut (chaîne non initialisée)
  fErrorPos := -1; // position d'une erreur
  fIndx := 1; // index par défaut dans la chaîne de travail
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
  St: String;
  I: Integer;
begin
  WipeItems; // liste interne nettoyée
  St := EmptyStr; // chaîne vide par défaut
  for I := Indx to Length(fText) do // on balaie l'expression
  begin
    Ch := fText[I]; // caractère en cours
    if Ch in CSpecialChar then  // est-ce un délimiteur ?
    begin
      if St <> EmptyStr then  // chaîne en cours non vide ?
        AddItem(St); // on la stocke
      St := EmptyStr; // on vide la chaîne de travail
      if Ch <> CBlank then // les blancs et autres caractères sont ignorés
      begin
        // recherche de <>, >= et <=
        if (I > 1) then // au moins un caractère ?
        begin
         if (Ch = CEqual) and (fText[I - 1] in [CGreater, CLower]) then
         begin
           fItemList[High(fItemList) - 1].Token := fText[I-1] + CEqual;  // >= ou <=
           St := EmptyStr;
         end
         else
         if (Ch = CGreater) and (fText[I-1] = CLower) then
         begin
           fItemList[High(fItemList) - 1].Token := fText[I-1] + CGreater;  // <>
           St := EmptyStr;
         end
         else
           AddItem(Ch); // délimiteur stocké
        end;
      end;
    end
    else
      if Ch in CAllValidChars then // caractère valide ?
        St := St + Ch // on stocke le caractère en cours
      else
      begin
        SetError(C_BadChar2); // caractère interdit
        fErrorPos := I; // position de l'erreur
        WipeItems; // on remet à zéro la liste interne
        St := EmptyStr; // nettoyage de la chaîne provisoire
        Exit; // on arrête la recherche
      end;
  end;
  if St <> EmptyStr then // nettoyage de fin si nécessaire
    AddItem(St);
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
