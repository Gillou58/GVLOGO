{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Evaluation d'une expression             |
  |                                infixée parenthésée                     |
  |                  Unité : GVEval.pas                                    |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    06-10-2014 16:15:20                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVEval - part of GVLOGO
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

unit GVEval;

// évaluation d'une expression infixée parenthésée
//
// ##############################################################
//
// L'unité GVEval définit la classe capable d'évaluer une expression
// mathématique incluant des variables.
//
// La récursivité permet d'imbriquer les expressions parenthésées.
//

interface

uses
  GVConsts, SysUtils, Classes;

type
  // *** classe de l'évaluation d'une expression pour GVLOGO

  EEvalException = class(Exception); // exception

  // événement de recherche d'une variable
  TGetVarEvent = procedure(Sender: TObject; VarName: string; var
    Value: Double; var Error: TGVError) of object;
  // événement de recherche d'une fonction
  TGetFunctionEvent = procedure(Sender: TObject; FunctionName: string;
    var Found: Boolean) of object;
  // événement de valeur de retour d'une fonction
  TGetFunctionValEvent = procedure(Sender: TObject; FunctionName: string;
    Param: Double; var Value: Double; var Error: TGVError) of Object;

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

  { TGVEvaluation }

  TGVEvaluation = class(TObject)
  private
    fValue: Double; // valeur de l'expression
    fError: TGVError; // erreur en cours
    fText: string; // texte de l'expression
    fIndx: Integer; // index dans la chaîne
    fItemList: TStringList; // éléments de l'expression
    fNumItem: Integer; // numéro de l'élément en cours de traitement
    fOnVariable: TGetVarEvent; // événement de variable
    fOnGetFunction: TGetFunctionEvent; // existence d'une fonction
    fOnGetFunctionVal: TGetFunctionValEvent; // valeur d'une fonction
    fOnError: TNotiFyEvent; // événement d'erreur
    procedure SetText(const AValue: string);
    procedure SetIndx(AValue: Integer);
    function GetValue: Double;
    function GetToken(N: Integer): string;
    function GetCount: Integer;
    procedure PushItem(N: Double); // empilement d'une valeur
  protected
    procedure EvalConst; // évaluation d'une constante
    procedure EvalVar; // évaluation d'une variable
    procedure EvalFunction; // évaluation d'une fonction
    procedure EvalUnaryMinus; // moins unaire
    procedure EvalNon; // non logique
    function GetVar(out Value: Double): Boolean; // recherche d'une variable
    procedure SetError(const Err: TGVError); // erreur
    procedure Tokenize; // on répartit en éléments
    procedure Calculate; // calcul de l'expression
  public
    constructor Create; overload; // création de l'objet
    constructor Create(const AText: string); overload; // création avec texte
    destructor Destroy; override; // destructeur
    function GetEnumerator: TGVTokensEnumerator; // énumération
    function ActualToken: string; // token en cours
    procedure NextToken; // passe au token suivant
    property Value: Double read GetValue; // valeur calculée
    property Error: TGVError read fError default C_NoInit; // erreur en cours
    property Text: string read fText write SetText; // expression à analyser
    property Indx: Integer read fIndx write SetIndx default 1; // index dans l'expression
    property Token[N:Integer]: string read GetToken; default; // accès direct aux éléments
    property NumItem: Integer read fNumItem; // élément en cours
    property Count: Integer read GetCount; // nombre d'éléments dans l'expression
    // événement lié à une variable
    property OnVariable: TGetVarEvent read fOnVariable write fOnVariable;
    // événement lié à une erreur
    property OnError: TNotifyEvent read fOnError write fOnError;
  end;

implementation

{ TGVEvaluation }

constructor TGVEvaluation.Create;
// *** création de l'objet ***
begin
  inherited Create; // on hérite
  fError := C_NoInit; // valeur non initialisée
  fValue :=  0; // valeur à zéro
  fIndx := 1; // pointeur sur la chaîne de travail
  fText := EmptyStr; // chaîne vide
  fItemList := TStringList.Create; // liste des éléments
end;

procedure TGVEvaluation.Calculate;
// *** calcul de l'expression ***

  procedure EvalExpression;
  // calcul d'une expression

    procedure EvalTerm;
    // on évalue un terme

      procedure EvalFactor;
      // on évalue un facteur
      begin

      end; // fin facteur

    begin // début terme

    end; // fin terme

  begin // début expression

  end; // fin expression

begin // début calcul

end; // fin calcul

constructor TGVEvaluation.Create(const AText: string);
// *** création avec initialisation du texte ***
begin
  Create; // on crée
  Text := AText; // on fixe le texte
end;

destructor TGVEvaluation.Destroy;
// *** destructeur ***
begin
  fItemList.Free; // liste des éléments libérée
  inherited Destroy; // on hérite
end;

procedure TGVEvaluation.EvalConst;
// *** évaluation d'une constante ***
var
  Num: Double;
begin
  // si l'élément en cours est un nombre correct
  if TryStrToFloat(ActualToken, Num) then
  begin
    PushItem(Num); // il faut l'empiler
    NextItem; // au suivant
  end
  else
    // nombre mauvais
    SetError(C_BadNumber);
end;

procedure TGVEvaluation.EvalVar;
// *** évaluation d'une variable ***
var
  Num: Double;
begin
  // recherche variable
  GetVar(Num);
  // ok ?
  if Error = C_None then
  begin
    PushItem(Num);  // on empile sa valeur
    NextItem; // au suivant
  end;
end;

function TGVEvaluation.GetCount: Integer;
// *** nombre d'éléments de l'expression en cours ***
begin
  Result := fItemList.Count;
end;

function TGVEvaluation.GetEnumerator: TGVTokensEnumerator;
// *** énumération des éléments ***
begin
  Result := TGVTokensEnumerator.Create(fItemList);
end;

function TGVEvaluation.ActualToken: string;
// token en cours
begin
  Result := Token[NumItem];
end;

procedure TGVEvaluation.NextToken;
// passe au token suivant
begin
  if (fNumItem < fItemList.Count) then // limites atteintes ?
    inc(fNumItem);  // suivant
end;

function TGVEvaluation.GetToken(N: Integer): string;
// *** accès direct aux éléments ***
begin
  // hors limites ? (base 1)
  if (N < 1) or (N > fItemList.Count) then
    raise EEvalException.CreateFmt(ME_OutOfRange2, [N, Text]);
  Result := fItemList[N - 1]; // renvoi de l'élément
end;

function TGVEvaluation.GetValue: Double;
// *** récupère la valeur de l'expression ***
begin
  // erreur ?
  if Error <> C_None then
    raise EEvalException.CreateFmt(ME_BadExp, [Text]);
  Result := fValue; // valeur renvoyée
end;

procedure TGVEvaluation.GetVar(out Value: Double);
// *** recherche d'une variable ***
begin
  // événement lié à l'acquisition d'une variable
  if Assigned(fOnVariable) then
    fOnVariable(Self, ActualToken, Value, fError);
end;

procedure TGVEvaluation.SetError(const Err: TGVError);
// *** erreur ***
begin
  fError := Err; // erreur stockée
  // événement erreur
  if Assigned(fOnError) then
    fOnError(Self);
end;

procedure TGVEvaluation.SetIndx(AValue: Integer);
// *** index dans l'expression ***
begin
  // hors limites ?
  if (AValue < 1) or (AValue > Length(fText)) then
    raise EEvalException.CreateFmt(ME_OutOfRange, [Indx, Text]);
  if fIndx <> AValue then	
    fIndx := AValue; // nouvelle valeur de l'index
end;

procedure TGVEvaluation.SetText(const AValue: string);
// *** nouvelle expression à analyser ***
begin
  if (AValue <> fText) then
  begin
    fText := AValue; // texte fixé
    Tokenize; // on construit la liste des éléments
  end;
  if fItemList.Count > 0 then // s'il y a des éléments
  begin
    fError := C_None; // pas d'erreur
    Calculate; // on calcule l'expression
  end;
end;

procedure TGVEvaluation.Tokenize;
// *** répartit en éléments ***
var
  Ch: Char;
  St: String;
  I: Integer;
begin
  fItemList.Clear; // liste interne nettoyée
  St := EmptyStr; // chaîne vide par défaut
  for I := Indx to Length(fText) do // on balaie l'expression
  begin
    Ch := fText[I]; // caractère en cours
    if Ch in CSpecialChar then  // est-ce un délimiteur ?
    begin
      if St <> EmptyStr then  // chaîne en cours non vide ?
        fItemList.Add(St); // on la stocke
      St := EmptyStr; // on vide la chaîne de travail
      if Ch <> CBlank then // les blancs et autres caractères sont ignorés
      begin
        // recherche de <>, >= et <=
        if (I > 1) then // au moins un caractère ?
        begin
         if (Ch = CEqual) and (fText[I - 1] in [CGreater, CLower]) then
           fItemList[fItemList.Count - 1] := fText[I-1] + CEqual  // >= ou <=
         else
         if (Ch = CGreater) and (fText[I-1] = CLower) then
           fItemList[fItemList.Count - 1] := fText[I-1] + CGreater  // <>
         else
           fItemList.Add(Ch); // délimiteur stocké
        end;
      end;
    end
    else
      if Ch in CAllValidChars then // caractère valide ?
        St := St + Ch // on stocke le caractère en cours
      else
        SetError(C_BadChar2); // caractère interdit
  end;
  if St <> EmptyStr then // nettoyage de fin si nécessaire
    fItemList.Add(St);
end;

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

end.
