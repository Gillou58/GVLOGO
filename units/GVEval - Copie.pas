{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Evaluation d'une expression             |
  |                                infix�e parenth�s�e                     |
  |                  Unit� : GVEval.pas                                    |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
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

// �valuation d'une expression infix�e parenth�s�e
//
// ##############################################################
//
// L'unit� GVEval d�finit la classe capable d'�valuer une expression
// math�matique incluant des variables.
//
// La r�cursivit� permet d'imbriquer les expressions parenth�s�es.
//

interface

uses
  GVConsts, SysUtils, Classes;

type
  // *** classe de l'�valuation d'une expression pour GVLOGO

  EEvalException = class(Exception); // exception

  // �v�nement de recherche d'une variable
  TGetVarEvent = procedure(Sender: TObject; VarName: string; var
    Value: Double; var Error: TGVError) of object;
  // �v�nement de recherche d'une fonction
  TGetFunctionEvent = procedure(Sender: TObject; FunctionName: string;
    var Found: Boolean) of object;
  // �v�nement de valeur de retour d'une fonction
  TGetFunctionValEvent = procedure(Sender: TObject; FunctionName: string;
    Param: Double; var Value: Double; var Error: TGVError) of Object;

  { TGVTokensEnumerator }

  TGVTokensEnumerator = class(TObject) // �num�ration
  private
    fLst: TStringList;
    fIndex: Integer;
  protected
    function GetCurrent: string; virtual;
  public
    constructor Create(const Value: TStrings); // cr�ation
    destructor Destroy; override; // destruction
    function MoveNext: Boolean; // recherche de l'�l�ment suivant
    property Current: string read GetCurrent;  // �l�ment courant
  end;

  { TGVEvaluation }

  TGVEvaluation = class(TObject)
  private
    fValue: Double; // valeur de l'expression
    fError: TGVError; // erreur en cours
    fText: string; // texte de l'expression
    fIndx: Integer; // index dans la cha�ne
    fItemList: TStringList; // �l�ments de l'expression
    fNumItem: Integer; // num�ro de l'�l�ment en cours de traitement
    fOnVariable: TGetVarEvent; // �v�nement de variable
    fOnGetFunction: TGetFunctionEvent; // existence d'une fonction
    fOnGetFunctionVal: TGetFunctionValEvent; // valeur d'une fonction
    fOnError: TNotiFyEvent; // �v�nement d'erreur
    procedure SetText(const AValue: string);
    procedure SetIndx(AValue: Integer);
    function GetValue: Double;
    function GetToken(N: Integer): string;
    function GetCount: Integer;
    procedure PushItem(N: Double); // empilement d'une valeur
  protected
    procedure EvalConst; // �valuation d'une constante
    procedure EvalVar; // �valuation d'une variable
    procedure EvalFunction; // �valuation d'une fonction
    procedure EvalUnaryMinus; // moins unaire
    procedure EvalNon; // non logique
    function GetVar(out Value: Double): Boolean; // recherche d'une variable
    procedure SetError(const Err: TGVError); // erreur
    procedure Tokenize; // on r�partit en �l�ments
    procedure Calculate; // calcul de l'expression
  public
    constructor Create; overload; // cr�ation de l'objet
    constructor Create(const AText: string); overload; // cr�ation avec texte
    destructor Destroy; override; // destructeur
    function GetEnumerator: TGVTokensEnumerator; // �num�ration
    function ActualToken: string; // token en cours
    procedure NextToken; // passe au token suivant
    property Value: Double read GetValue; // valeur calcul�e
    property Error: TGVError read fError default C_NoInit; // erreur en cours
    property Text: string read fText write SetText; // expression � analyser
    property Indx: Integer read fIndx write SetIndx default 1; // index dans l'expression
    property Token[N:Integer]: string read GetToken; default; // acc�s direct aux �l�ments
    property NumItem: Integer read fNumItem; // �l�ment en cours
    property Count: Integer read GetCount; // nombre d'�l�ments dans l'expression
    // �v�nement li� � une variable
    property OnVariable: TGetVarEvent read fOnVariable write fOnVariable;
    // �v�nement li� � une erreur
    property OnError: TNotifyEvent read fOnError write fOnError;
  end;

implementation

{ TGVEvaluation }

constructor TGVEvaluation.Create;
// *** cr�ation de l'objet ***
begin
  inherited Create; // on h�rite
  fError := C_NoInit; // valeur non initialis�e
  fValue :=  0; // valeur � z�ro
  fIndx := 1; // pointeur sur la cha�ne de travail
  fText := EmptyStr; // cha�ne vide
  fItemList := TStringList.Create; // liste des �l�ments
end;

procedure TGVEvaluation.Calculate;
// *** calcul de l'expression ***

  procedure EvalExpression;
  // calcul d'une expression

    procedure EvalTerm;
    // on �value un terme

      procedure EvalFactor;
      // on �value un facteur
      begin

      end; // fin facteur

    begin // d�but terme

    end; // fin terme

  begin // d�but expression

  end; // fin expression

begin // d�but calcul

end; // fin calcul

constructor TGVEvaluation.Create(const AText: string);
// *** cr�ation avec initialisation du texte ***
begin
  Create; // on cr�e
  Text := AText; // on fixe le texte
end;

destructor TGVEvaluation.Destroy;
// *** destructeur ***
begin
  fItemList.Free; // liste des �l�ments lib�r�e
  inherited Destroy; // on h�rite
end;

procedure TGVEvaluation.EvalConst;
// *** �valuation d'une constante ***
var
  Num: Double;
begin
  // si l'�l�ment en cours est un nombre correct
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
// *** �valuation d'une variable ***
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
// *** nombre d'�l�ments de l'expression en cours ***
begin
  Result := fItemList.Count;
end;

function TGVEvaluation.GetEnumerator: TGVTokensEnumerator;
// *** �num�ration des �l�ments ***
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
// *** acc�s direct aux �l�ments ***
begin
  // hors limites ? (base 1)
  if (N < 1) or (N > fItemList.Count) then
    raise EEvalException.CreateFmt(ME_OutOfRange2, [N, Text]);
  Result := fItemList[N - 1]; // renvoi de l'�l�ment
end;

function TGVEvaluation.GetValue: Double;
// *** r�cup�re la valeur de l'expression ***
begin
  // erreur ?
  if Error <> C_None then
    raise EEvalException.CreateFmt(ME_BadExp, [Text]);
  Result := fValue; // valeur renvoy�e
end;

procedure TGVEvaluation.GetVar(out Value: Double);
// *** recherche d'une variable ***
begin
  // �v�nement li� � l'acquisition d'une variable
  if Assigned(fOnVariable) then
    fOnVariable(Self, ActualToken, Value, fError);
end;

procedure TGVEvaluation.SetError(const Err: TGVError);
// *** erreur ***
begin
  fError := Err; // erreur stock�e
  // �v�nement erreur
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
// *** nouvelle expression � analyser ***
begin
  if (AValue <> fText) then
  begin
    fText := AValue; // texte fix�
    Tokenize; // on construit la liste des �l�ments
  end;
  if fItemList.Count > 0 then // s'il y a des �l�ments
  begin
    fError := C_None; // pas d'erreur
    Calculate; // on calcule l'expression
  end;
end;

procedure TGVEvaluation.Tokenize;
// *** r�partit en �l�ments ***
var
  Ch: Char;
  St: String;
  I: Integer;
begin
  fItemList.Clear; // liste interne nettoy�e
  St := EmptyStr; // cha�ne vide par d�faut
  for I := Indx to Length(fText) do // on balaie l'expression
  begin
    Ch := fText[I]; // caract�re en cours
    if Ch in CSpecialChar then  // est-ce un d�limiteur ?
    begin
      if St <> EmptyStr then  // cha�ne en cours non vide ?
        fItemList.Add(St); // on la stocke
      St := EmptyStr; // on vide la cha�ne de travail
      if Ch <> CBlank then // les blancs et autres caract�res sont ignor�s
      begin
        // recherche de <>, >= et <=
        if (I > 1) then // au moins un caract�re ?
        begin
         if (Ch = CEqual) and (fText[I - 1] in [CGreater, CLower]) then
           fItemList[fItemList.Count - 1] := fText[I-1] + CEqual  // >= ou <=
         else
         if (Ch = CGreater) and (fText[I-1] = CLower) then
           fItemList[fItemList.Count - 1] := fText[I-1] + CGreater  // <>
         else
           fItemList.Add(Ch); // d�limiteur stock�
        end;
      end;
    end
    else
      if Ch in CAllValidChars then // caract�re valide ?
        St := St + Ch // on stocke le caract�re en cours
      else
        SetError(C_BadChar2); // caract�re interdit
  end;
  if St <> EmptyStr then // nettoyage de fin si n�cessaire
    fItemList.Add(St);
end;

{ TGVTokensEnumerator }

function TGVTokensEnumerator.GetCurrent: string;
// *** retourne l'�l�ment courant ***
begin
  Result := fLst[fIndex];
end;

constructor TGVTokensEnumerator.Create(const Value: TStrings);
// *** cr�ation de l'�num�rateur ***
begin
  fIndex := -1;
  fLst := TStringList.Create;
  fLst.AddStrings(Value);
end;

destructor TGVTokensEnumerator.Destroy;
// *** destruction de l'�num�rateur ***
begin
  fLst.Free;
  inherited Destroy;
end;

function TGVTokensEnumerator.MoveNext: Boolean;
// *** passe � l'�l�ment suivant ***
begin
  Result := fIndex < (fLst.Count - 1);
  if Result then
    Inc(fIndex);
end;

end.
