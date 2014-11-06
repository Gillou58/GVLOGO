{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Eléments d'une expression               |
  |                  Unité : GVTokens.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    11-09-2014 09:14:46                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVTokens - part of GVLOGO
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

unit GVTokens;
// recherche de tokens

interface

uses
  Classes, SysUtils, GVConsts;

type

  { TGVTokensEnumerator }

  TGVTokensEnumerator = class(TObject) // énumération
  private
    fLst: TStringList;
    fIndex: Integer;
  protected
    function GetCurrent: string; virtual;
  public
    constructor Create(const Value: TStrings); // création
    destructor Destroy; override; // destruction
    function MoveNext: Boolean; // recherche de l'élément suivant
    property Current: string read GetCurrent;  // élément courant
  end;

  { TGVTokens }

  TGVTokens = class(TObject)
    private
      fSource: string;
      fIndx: Integer;
      fStrList: TStringList;
      function GetCount: Integer;
      function GetToken(N: Integer): string;
      procedure SetIndx(AValue: Integer);
      procedure SetSource(const AValue: string);
    public
      constructor Create; // création
      destructor Destroy; override; // destruction
      function GetEnumerator: TGVTokensEnumerator; // énumération
      procedure Tokenize; // liste les éléments
      property Source: string read fSource write SetSource; // source
      property Indx: Integer read fIndx write SetIndx; // index
      property Token[N: Integer]: string read GetToken; default; // éléments
      property Count: Integer read GetCount; // décompte
  end;

implementation

uses Math;

{ TGVTokensEnumerator }

function TGVTokensEnumerator.GetCurrent: string;
// *** retourne l'élément courant ***
begin
  Result := fLst[fIndex];
end;

constructor TGVTokensEnumerator.Create(const Value: TStrings);
// *** création de l'énumérateur ***
begin
  fIndex := -1;
  fLst := TStringList.Create;
  fLst.AddStrings(Value);
end;

destructor TGVTokensEnumerator.Destroy;
// *** destruction de l'énumérateur ***
begin
  fLst.Free;
  inherited Destroy;
end;

function TGVTokensEnumerator.MoveNext: Boolean;
// *** passe à l'élément suivant ***
begin
  Result := fIndex < (fLst.Count - 1);
  if Result then
    Inc(fIndex);
end;

{ TGVTokens }

function TGVTokens.GetToken(N: Integer): string;
// *** renvoie l'élément cherché ***
begin
  Result := fStrList[N];
end;

function TGVTokens.GetCount: Integer;
// *** nombre d'éléments trouvés ***
begin
  Result := fStrList.Count;
end;

(* ********************************************************************* *)

procedure TGVTokens.SetIndx(AValue: Integer);
// *** index de lecture ***
begin
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    raise EEvalException.CreateFmt(ME_OutOfRange, [Indx, Text]);
  if fIndx <> AValue then	
    fIndx := AValue; // nouvelle valeur de l'index
end;

procedure TGVTokens.SetSource(const AValue: string);
// *** texte à analyser ***
begin
  if fSource = AValue then
    Exit;
  fSource := AValue;
  fStrList.Clear; // nettoyage de la liste interne
  Indx := 1; // pointe sur le premier caractère
end;

constructor TGVTokens.Create;
// *** constructeur ***
begin
  inherited Create;
  fStrList := TStringList.Create;
end;

destructor TGVTokens.Destroy;
// *** destructeur ***
begin
  fStrList.Free;
  inherited Destroy;
end;

function TGVTokens.GetEnumerator: TGVTokensEnumerator;
// *** mise en place de l'énumération ***
begin
  Result := TGVTokensEnumerator.Create(fStrList);
end;

procedure TGVTokens.Tokenize;
// *** répartit en éléments ***
var
  Ch: Char;
  St: String;
  I: Integer;
begin
  fStrlist.Clear; // liste interne nettoyée
  St := EmptyStr; // chaîne vide par défaut
  for I := Indx to Length(Source) do // on balaie la source
  begin
    Ch := Source[I]; // caractère en cours
    if Ch in CSpecialChar then  // est-ce un délimiteur ?
    begin
      if St <> EmptyStr then  // chaîne en cours non vide ?
        fStrList.Add(St); // on la stocke
      St := EmptyStr; // on vide la chaîne de travail
      if Ch <> CBlank then // les blancs sont ignorés
      begin
        // recherche de >+ et <=
        if (I > 1) and  (Ch = CEqual) and (Source[I - 1] in [CGreater, CLower]) then
          fStrList[fStrList.Count - 1] := Source[I-1] + CEqual  // stockage spécifique
        else
          fStrList.Add(Ch); // délimiteur stocké
      end;
    end
    else
      St := St + Ch; // sinon on stocke le caractère en cours
  end;
  if St <> EmptyStr then //
    fStrList.Add(St);
end;

end.

