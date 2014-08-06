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
  |                  Date:    07-05-2014 22:23:20                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

{$I GVDefines.inc}

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GVUtils, GVStacks;

type
  TGetVarEvent = procedure(Sender : TObject; VarName : string; var
    Value : Real; var Found : Boolean) of object;

  TGVEvalErrorEvent = procedure(Sender : TObject; EvalError: TGVError; Indx : Integer)
    of object;

  TGVEvaluation = class
  private
    { Déclarations privées }
    fSource : string; // ligne à évaluer
    fFunctions: TStringList; // liste des fonctions
    fResult : Real; // résultat
    fIndx : Integer; // index dans ligne à évaluer
    fChar : Char; // caractère en cours
    fSymbol : string; // symbole en cours
    fStack : TGVRealStack; // pile de travail
    fError : Boolean; // erreur?
    fNumError : TGVError; // numéro d'erreur
    FOnVariable : TGetVarEvent;
    FOnError : TGVEvalErrorEvent;
    procedure SetError(Num : TGVError);
    procedure SetIndx(const Value: Integer); // génère une erreur
  protected
    { Déclarations protégées }
    procedure ReadChar; // extrait le caractère en cours
    procedure ReadIdent; // extrait un identificateur
    procedure ReadSymbol; // extrait un symbole
    function GetVar(out Value : Real) : Boolean; // cherche une variable
    procedure EvalConst; // évalue une constante
    procedure EvalVariable; // évalue une variable
    procedure EvalFunction; // évalue une fonction
    procedure SetSource(const Value : string); // nouvelle source à évaluer
    function SubExp : Boolean; // cherche une sous-expression de fonction
  public
    { Déclarations publiques }
    constructor Create;
    destructor Destroy; override;
    procedure EvalExpression; // évalue une expression
    property Result : Real read fResult;
    property Error : Boolean read fError;
    property NumError : TGVError read fNumError;
  published
    { Déclarations publiées }
    property Source : string read fSource write SetSource;
    property Indx : Integer read fIndx write SetIndx default 1;
    property OnGetVar : TGetVarEvent read FOnVariable write FOnVariable;
    property OnEvalError : TGVEvalErrorEvent read FOnError write FOnError;
  end;

implementation

uses Math;

(* ********************************************************************* *)

constructor TGVEvaluation.Create;
// construction
var
  I: TGVFunctions;
begin
  inherited Create;
  fSource := EmptyStr;
  Indx := 1;
  fStack := TGVRealStack.Create;
  fFunctions := TStringList.Create;
  for I := Low(TGVFunctions) to High(TGVFunctions) do
    fFunctions.Append(GVFunctionName[I]);
end;

(* ********************************************************************* *)

destructor TGVEvaluation.Destroy;
// destruction
begin
  fStack.Free;
  fFunctions.Free;
  inherited Destroy;
end;

(* ********************************************************************* *)

procedure TGVEvaluation.SetSource(const Value : string);
// nouvelle source à évaluer
begin
  if (Value <> fSource) then
    fSource := Value;
  fStack.Clear;
  fError := False;
  Indx := 1;
end;

(* ********************************************************************* *)

procedure TGVEvaluation.ReadChar;
// lit un caractère
begin
  if Indx <= Length(fSource) then
  begin
    fChar := fSource[Indx];
    Inc(fIndx);
  end
  else
    fChar := CBlank;
end;

 (* ********************************************************************* *)

procedure TGVEvaluation.ReadIdent;
// lit un identificateur
begin
  fSymbol := EmptyStr;
  repeat
    fSymbol := fSymbol + fChar;
    ReadChar;
  {$IFNDEF Delphi}
  until fChar in SpecialChar;
  {$ELSE}
  until CharInSet(fChar, SpecialChar);
  {$ENDIF}
end;

(* ********************************************************************* *)

procedure TGVEvaluation.ReadSymbol;
// lit un symbole
begin
  while (fChar = CBlank) and (Indx <= Length(Source)) do
    ReadChar;
  {$IFNDEF Delphi}
  if fChar in CharAlphaNum then
  {$ELSE}
  if CharInSet(fChar, CharAlphaNum) then
  {$ENDIF}
    ReadIdent
  else
  begin
    fSymbol := fChar;
    ReadChar;
  end;
end;

(* ********************************************************************* *)

function TGVEvaluation.GetVar(out Value : Real) : Boolean;
// cherche si fSymbol est une variable correcte et renvoie sa valeur
begin
  Result := False;
  if Assigned(FOnVariable) then
    FOnVariable(Self,fSymbol,Value,Result);
end;

(* ********************************************************************* *)

procedure TGVEvaluation.SetError(Num : TGVError);
// génère une erreur
begin
  fError := True;
  fNumError := Num;
  if Assigned(FOnError) then
    FOnError(Self, fNumError, Indx);
end;

(* ********************************************************************* *)

procedure TGVEvaluation.SetIndx(const Value: Integer);
// index dans la ligne à analyser
begin
  if (Value <> fIndx) then
    // ne peut dépasser la longueur de la source
    fIndx := Min(Value,Length(fSource));
end;

(* ********************************************************************* *)

procedure TGVEvaluation.EvalConst;
// évaluation d'une constante
begin
  try
    fStack.Push(StrToFloat(fSymbol)); // on l'empile
    ReadSymbol;
  except
    SetError(C_BadNumber); // sauf si c'est un nombre incorrect
  end;
end;

(* ********************************************************************* *)

procedure TGVEvaluation.EvalVariable;
// évaluation d'une variable
var
  R : Real;
begin
  if GetVar(R) then  // on cherche la variable
  begin
    fStack.Push(R); // on l'empile
    ReadSymbol;
  end
  else
    SetError(C_BadVar); // sauf si elle est incorrecte
end;

(* ********************************************************************* *)

function TGVEvaluation.SubExp : Boolean;
// cherche une sous-expression de fonction
begin
  Result := False;
  ReadSymbol;
  if fSymbol = CBeginPar then // début de parenthèse ?
  begin
    dec(fIndx);
    EvalExpression;  // on évalue l'expression
    if (fSymbol = CEndPar) and (not fError) then  // fin de parenthèse ?
    begin
      fStack.Push(fResult); // on empile le résultat
      ReadSymbol;
      Result := True;
    end
    else
      SetError(C_ClosePar);  // pas de fin de parenthèse
  end;
end;

(* ********************************************************************* *)

procedure TGVEvaluation.EvalFunction;
// évalue une fonction
var
  Num: Integer;
begin
  Num := fFunctions.IndexOf(AnsiUpperCase(fSymbol)); // recherche de la fonction
  case TGVFunctions(Num) of   // répartition
  C_DAbs, // valeur absolue
  C_DAbs2: if SubExp then
             fStack.DAbs;
  C_DCos, // cosinus
  C_DCos2: if SubExp then
             fStack.DCos;
  C_DSin, // sinus
  C_DSin2: if SubExp then
             fStack.DSin;
  C_DTan, // tangente
  C_DTan2: if SubExp then
             fStack.DTan;
  C_DSqrt, // racine carrée
  C_DSqrt2: if SubExp then
              fStack.DSqrt;
  C_DTrunc: if SubExp then // nombre tronqué
              fStack.DTrunc;
  C_DRound: if SubExp then // nombre arrondi
              fStack.DRound;
  C_DSqr: if SubExp then // nombre au carré
            fStack.DSqr;
  C_DExp: if SubExp then // exponentielle
            fStack.DExp;
  C_DFrac: if SubExp then // partie fractionnelle
             fStack.DFrac;
  C_DInt, // partie entière
  C_DInt2: if SubExp then
             fStack.DInt;
  C_DLn: if SubExp then // log népérien
           fStack.DLn;
  C_DLog2: if SubExp then // log base 2
             fStack.DLog2;
  C_DLog10: if SubExp then // log base 10
              fStack.DLog10;
  C_DCoTan, // cotangente
  C_DCoTan2: if SubExp then
               fStack.DCoTan;
  C_DArcCos, // arc cosinus
  C_DArcCos2: if SubExp then
                fStack.DArcCos;
  C_DArcSin, // arc sinus
  C_DArcSin2: if SubExp then
                fStack.DArcSin;
  C_Minus: if SubExp then // nombre négatif
             fStack.DMinus;
  C_Plus: if SubExp then // nombre positif
            fStack.DPlus;
  C_DNegate: if SubExp then // signe inversé
               fStack.DNegate;
  C_DPi: // PI sur la pile
    begin
      fStack.DPi;
      ReadSymbol;
    end;
  C_DSign: if SubExp then // signe
             fStack.DSign;
  C_DRandom: if SubExp then
               fStack.DRandom;
  else
    SetError(C_BadFunction);  // fonction inconnue
  end;
end;

(* ********************************************************************* *)

procedure TGVEvaluation.EvalExpression;
// évalue une expression
var
  Ch : Char;

  procedure EvalTerm;
  // évalue un terme
  var
    Ch : Char;

    procedure EvalFactor;
    // évalue un facteur
    begin
      {$IFNDEF Delphi}
      if fSymbol[1] in CharNum then    // une constante ?
      {$ELSE}
      if CharInSet(fSymbol[1],CharNum) then
      {$ENDIF}
        EvalConst
      else
      if fSymbol[1] = CColon then  // une variable ?
        EvalVariable
      else
      if fSymbol[1] = CBeginPar then   // une expression ?
      begin
        dec(fIndx);
        EvalExpression;
        if (fSymbol = CEndPar) and (not fError) then
        begin
          fStack.Push(fResult);
          ReadSymbol;
        end
        else
          SetError(C_ClosePar);
      end
      else
      {$IFNDEF Delphi}
      if fSymbol[1] in CharAlpha then  // une fonction ?
      {$ELSE}
      if CharInSet(fSymbol[1],CharAlpha) then
      {$ENDIF}
        EvalFunction
      else
        SetError(C_BadChar);
    end; {fin Facteur }

  begin
    EvalFactor;
    while (not fError) and
    {$IFNDEF Delphi}
      fSymbol[1] in [CMul,CDiv, CPower] do  // *, / ou ^ ?
      {$ELSE}
      CharInSet(fSymbol[1],[CMul,CDiv, CPower]) do
      {$ENDIF}
    begin
      Ch := fSymbol[1];
      ReadSymbol;
      EvalFactor;
      if not fError then
        case Ch of
          CMul: fStack.DMul; // on multiplie
          CPower: fStack.DPower;  // on élève à la puissance
          CDiv:  begin // on divise
                   try
                     fStack.DDiv;
                   except
                     SetError(C_Zero);
                   end;
                 end;
        end;
    end;
  end; { fin Terme }

begin
  try
    ReadChar;
    ReadSymbol;
    {$IFNDEF Delphi}
    if fSymbol[1] in [CPLus,CMinus] then
    {$ELSE}
    if CharInSet(fSymbol[1], [CPLus,CMinus]) then
  {$ENDIF}
    begin
      Ch := fSymbol[1];
      ReadSymbol;
      EvalTerm;
      if (not fError) and (Ch = CMinus) then
        fStack.DNegate;
    end
    else
      EvalTerm;
    while (not fError) and
    {$IFNDEF Delphi}
     (fSymbol[1] in [CPLus,CMinus]) do
    {$ELSE}
      CharInSet(fSymbol[1],[CPLus,CMinus]) do
    {$ENDIF}
    begin
      Ch := fSymbol[1];
      ReadSymbol;
      EvalTerm;
      if not fError then
        if (Ch = CPLus) then
          fStack.DSum
        else
          fStack.DSub;
    end;
    if not fError then
      fResult := fStack.Pop;
  except
    SetError(C_BadExp);
  end;
end; { fin Expression }

end.
