{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Coloration syntaxique de l'éditeur      |
  |                  Unité : GVHighligter.pas                              |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVHIGHLIGHTER - part of GVLOGO
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

unit GVHighlighter;

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase;

type
  // *** TGVHighlighter ***
  TGVHighlighter = class(TSynCustomFoldHighlighter)
  private
    fPrims: TStringList; // liste des primitives
    // attributs
    fStringAttri: TSynHighlighterAttributes; // chaîne de caractères
    fCommentAttri: TSynHighlighterAttributes; // commentaire
    fKeyWordAttri: TSynHighlighterAttributes; // primitives
    fVarAttri: TSynHighlighterAttributes; // variables
    fNumberAttri: TSynHighlighterAttributes; // nombres
    fIdentifierAttri: TSynHighlighterAttributes; // identificateurs
    fSpaceAttri: TSynHighlighterAttributes; // espaces
    fSymbolAttri: TSynHighlighterAttributes; // symboles spéciaux
    fTokenPos, fTokenEnd: Integer; // positions de travail
    fLineText: string; // ligne de texte
    // méthodes pour les attributs
    procedure SetCommentAttri(AValue: TSynHighlighterAttributes);
    procedure SetIdentifierAttri(AValue: TSynHighlighterAttributes);
    procedure SetKeyWordAttri(AValue: TSynHighlighterAttributes);
    procedure SetNumberAttri(AValue: TSynHighlighterAttributes);
    procedure SetSpaceAttri(AValue: TSynHighlighterAttributes);
    procedure SetStringAttri(AValue: TSynHighlighterAttributes);
    procedure SetSymbolAttri(AValue: TSynHighlighterAttributes);
    procedure SetVarAttri(AValue: TSynHighlighterAttributes);
  public
    constructor Create(AOwner: TComponent); override; // constructeur
    destructor Destroy; override; // destruction de l'objet
    // affectation de la ligne
    procedure SetLine(const NewValue: string; LineNumber: Integer); override;
    procedure Next; override; // élément suivant
    function  GetEol: Boolean; override; // fin de ligne ?
    // élément en cours
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: Integer);
      override;
    // attributs de l'élément
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    function GetToken: string; override; // élément
    function GetTokenPos: Integer; override; // position de l'élément
    function GetTokenKind: Integer; override; // type de l'élément
    // attributs par défaut
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
  published
    // valeurs des attributs
    property VarAttri: TSynHighlighterAttributes read fVarAttri
      write SetVarAttri;
    property KeyWordAttri: TSynHighlighterAttributes read fKeyWordAttri
      write SetKeyWordAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write SetNumberAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write SetIdentifierAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write SetSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write SetSymbolAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write SetCommentAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write SetStringAttri;
  end;

implementation

uses
  GVConsts, // constantes générales
  GVPrimConsts; // constantes des primitives

{ TGVHighlighter }

constructor TGVHighlighter.Create(AOwner: TComponent);
// *** création de l'objet ***
begin
  inherited Create(AOwner);
  // initialisation des primitives
  fPrims := TStringList.Create; // création de la liste des primitives
  fPrims.Delimiter := CBlank; // délimiteur de primitives
  // toutes les primitives par ordre alphabétique
  fPrims.DelimitedText := CPrimsAll;
  // création des attributs
  fVarAttri := TSynHighlighterAttributes.Create('var', 'var');
  AddAttribute(fVarAttri);
  fVarAttri.Style := [fsBold];
  // primitives
  fKeyWordAttri := TSynHighlighterAttributes.Create('prim', 'prim');
  AddAttribute(fKeyWordAttri);
  fKeyWordAttri.Style := [fsBold];
  fKeyWordAttri.Foreground := clBlue;
  // identificateurs
  fIdentifierAttri := TSynHighlighterAttributes.Create('ident',
    'ident');
  AddAttribute(fIdentifierAttri);
  // espace
  fSpaceAttri := TSynHighlighterAttributes.Create('space', 'space');
  AddAttribute(fSpaceAttri);
  fSpaceAttri.FrameEdges := sfeAround;
  // nombres
  fNumberAttri := TSynHighlighterAttributes.Create('number', 'number');
  AddAttribute(fNumberAttri);
  fNumberAttri.Foreground := clRed;
  // symboles
  fSymbolAttri := TSynHighlighterAttributes.Create('symbol', 'symbol');
  AddAttribute(fSymbolAttri);
  fSymbolAttri.Foreground := clFuchsia;
  // commentaire
  fCommentAttri := TSynHighlighterAttributes.Create('comment', 'comment');
  AddAttribute(fCommentAttri);
  fCommentAttri.Foreground := clGreen;
  fCommentAttri.Style := [fsItalic];
  // chaîne
  fStringAttri := TSynHighlighterAttributes.Create('string', 'string');
  AddAttribute(fStringAttri);
  fStringAttri.Foreground := clMaroon;
end;

procedure TGVHighlighter.SetIdentifierAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'un identificateur ***
begin
  fIdentifierAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetCommentAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'un commentaire ***
begin
  fCommentAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetKeyWordAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'une primitive ***
begin
  fKeyWordAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetNumberAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'un nombre ***
begin
  fNumberAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetSpaceAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'un espace ***
begin
  fSpaceAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetStringAttri(AValue: TSynHighlighterAttributes);
// *** atributs d'une chaîne de caractères ***
begin
  fStringAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetSymbolAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'un symbole spécial ***
begin
  fSymbolAttri.Assign(AValue);
end;

procedure TGVHighlighter.SetVarAttri(AValue: TSynHighlighterAttributes);
// *** attributs d'une variable ***
begin
  fVarAttri.Assign(AValue);
end;

destructor TGVHighlighter.Destroy;
// *** destruction de l'objet ***
begin
  fPrims.Free; // destruction de la liste des primitives
  inherited Destroy; // on hérite
end;

procedure TGVHighlighter.SetLine(const NewValue: string; LineNumber: Integer);
// *** nouvelle ligne de travail ***
begin
  inherited; // on hérite
  fLineText := NewValue; // nouvelle chaîne
  fTokenEnd := 1; // position
  Next; // on pointe sur le caractère suivant
end;

procedure TGVHighlighter.Next;
// *** caractère suivant ***
var
  Li: Integer;
  LB: Boolean;
begin
  fTokenPos := fTokenEnd; // position en cours
  Li := Length(FLineText); // longueur de la ligne en cours
  if fTokenPos > Li then // fin de ligne ?
    Exit // on sort
  else
  // une chaîne ?
  if fLineText[fTokenEnd] = CQuote then
  begin
    LB := True; // drapeau de caractère d'échappement
    while (fTokenEnd <= Li) and (not (fLineText[fTokenEnd]
      in [CTab, CBlank]) or LB) do
        begin
          LB := fLineText[fTokenEnd] = '$';
          Inc(fTokenEnd); // on compte jusqu'à la fin de l'élément
        end;
  end
  else
  // un symbole ?
  if fLineText[fTokenEnd] in CBraces then
    Inc(fTokenEnd)
  else
  // un commentaire ? (seulement si commence la ligne)
  if (Length(TrimLeft(fLineText)) > 1) and (TrimLeft(fLineText)[1] = CSlash)
    and (TrimLeft(fLineText)[2] = CSlash) then
      fTokenEnd := Length(fLineText) + 1
  else
  // un espace ?
  if fLineText[fTokenEnd] in CBlanks then
    while (fTokenEnd <= Li) and (fLineText[fTokenEnd] in CNonPrintable) do
      Inc(fTokenEnd) // on saute les espaces
  else
  // autres cas
  while (fTokenEnd <= Li) and not(fLineText[fTokenEnd]
    in CSeparators) do
      Inc(fTokenEnd); // on compte jusqu'à la fin de l'élément
  // POUR ou FIN ? => pliage possible
  if SameText(TrimLeft(Copy(fLineText, 1, fTokenEnd - 1)), P_To) then
    StartCodeFoldBlock(nil)
  else
  if SameText(TrimLeft(Copy(fLineText, 1, fTokenEnd - 1)), P_End) then
    EndCodeFoldBlock
end;

function TGVHighlighter.GetEol: Boolean;
// *** fin de ligne ? ***
begin
  Result := fTokenPos > Length(fLineText);
end;

procedure TGVHighlighter.GetTokenEx(out TokenStart: PChar; out TokenLength: Integer
  );
// *** renvoi de l'élément en cours ***
begin
  TokenStart := @fLineText[fTokenPos];
  TokenLength := fTokenEnd - fTokenPos;
end;

function TGVHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
// *** attributs associés à un élément ***
var
  Lpos: Integer;
begin
  // est-ce un commentaire ?
  if Copy(TrimLeft(fLineText), 1, 2) = CComment then
    Result := CommentAttri
  else
  // est-ce une chaîne ?
  if fLineText[fTokenPos] = CQuote then
    Result := StringAttri
  else
  // est-ce un espace ?
  if fLineText[fTokenPos] in CBlanks then
    Result := SpaceAttri
  else
  // un nombre ?
  if fLineText[fTokenPos] in CDigitPlus then
    Result := NumberAttri
  else
  // est-ce une liste ?
  if fLineText[fTokenPos] in CBraces then
    Result := SymbolAttri
  else
  // est-ce une variable ?
  if fLineText[fTokenPos] = CColon then
    Result := VarAttri
  else
  // est-ce une primitive ?
  if fPrims.Find(Copy(fLineText, fTokenPos, fTokenEnd - fTokenPos), LPos) then
    Result := KeyWordAttri
  else
    Result := IdentifierAttri; // un identificateur
end;

function TGVHighlighter.GetToken: string;
// *** renvoi de l'élément en cours ***
begin
  Result := Copy(fLineText, fTokenPos, fTokenEnd - fTokenPos);
end;

function TGVHighlighter.GetTokenPos: Integer;
// *** renvoi de la position de l'élément en cours ***
begin
  Result := fTokenPos - 1;
end;

function TGVHighlighter.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
// *** attributs par défaut ***
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_KEYWORD: Result := fKeyWordAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    else Result := nil;
  end;
end;

function TGVHighlighter.GetTokenKind: Integer;
// *** type d'élément ***
var
  LHlAt: TSynHighlighterAttributes;
begin
  // numéro unique pour chaque élément
  LHlAt := GetTokenAttribute; // attribut de l'élément
  Result := 0;
  if LHlAt = fSpaceAttri then
    Result := 1
  else
  if LHlAt = fVarAttri then
    Result := 2
  else
  if LHlAt = fIdentifierAttri then
    Result := 3
  else
  if LHlAt = fKeyWordAttri then
    Result := 4
  else
  if LHlAt = fSymbolAttri then
    Result := 5
  else
  if lHlAt = fCommentAttri then
    Result := 6
  else
  if lHlAt = fStringAttri then
    Result := 7;
end;

end.

