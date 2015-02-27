{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des mots             |
  |                  Unité : mainform.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

  
// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVWORDS - part of GVLOGO
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
  
unit MainForm;

{$I GVDefines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Buttons, ComCtrls, StrUtils,
  GVWords, // mots
  GVErrConsts; // constantes pour les erreurs

const
  P_True = 'vrai';
  P_False = 'faux';

type
  // *** TGVMainForm ***
  TGVMainForm = class(TForm)
    btnClear: TBitBtn;
    btnExit: TBitBtn;
    btnFirst: TButton;
    btnWithoutColon: TButton;
    btnWithEsc: TButton;
    btnCount: TButton;
    btnReverse: TButton;
    btnAtRandom: TButton;
    btnShuffle: TButton;
    btnSort: TButton;
    btnNumberP: TButton;
    btnItem: TButton;
    btnLast: TButton;
    btnButFirst: TButton;
    btnButLast: TButton;
    btnLowerCase: TButton;
    btnUpperCase: TButton;
    btnEmptyWordP: TButton;
    btnWithoutEsc: TButton;
    btnWithoutQuote: TButton;
    btnEqualP: TButton;
    btnInsert: TButton;
    btnGreaterP: TButton;
    btnLowerP: TButton;
    btnMemberP: TButton;
    btnGreatest: TButton;
    btnLowest: TButton;
    btnPutFirst: TButton;
    btnPutLast: TButton;
    btnReplace: TButton;
    btnIsValid: TButton;
    btnIsValidIdent: TButton;
    btnRotate: TButton;
    btnIsInt: TButton;
    btnIsBoolean: TButton;
    btnDelItem: TButton;
    btnAsNumber: TButton;
    btnAsInt: TButton;
    btnAsBoolean: TButton;
    GBEdit: TGroupBox;
    GBOneWord: TGroupBox;
    GBTwoWords: TGroupBox;
    GBMisc: TGroupBox;
    GBErrors: TGroupBox;
    LabEdtFirst: TLabeledEdit;
    LabEdtSecond: TLabeledEdit;
    lblResult: TLabel;
    mmoErrors: TMemo;
    sedtItem: TSpinEdit;
    sedtReplace: TSpinEdit;
    sedtInsert: TSpinEdit;
    sbMain: TStatusBar;
    sedtDelItem: TSpinEdit;
    procedure btnAsBooleanClick(Sender: TObject);
    procedure btnAsIntClick(Sender: TObject);
    procedure btnAsNumberClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnAtRandomClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnDelItemClick(Sender: TObject);
    procedure btnEmptyWordPClick(Sender: TObject);
    procedure btnEqualPClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnGreaterPClick(Sender: TObject);
    procedure btnGreatestClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnIsBooleanClick(Sender: TObject);
    procedure btnIsIntClick(Sender: TObject);
    procedure btnIsValidClick(Sender: TObject);
    procedure btnIsValidIdentClick(Sender: TObject);
    procedure btnItemClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnLowerCaseClick(Sender: TObject);
    procedure btnLowerPClick(Sender: TObject);
    procedure btnLowestClick(Sender: TObject);
    procedure btnMemberPClick(Sender: TObject);
    procedure btnNumberPClick(Sender: TObject);
    procedure btnPutFirstClick(Sender: TObject);
    procedure btnPutLastClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
    procedure btnShuffleClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnUpperCaseClick(Sender: TObject);
    procedure btnWithEscClick(Sender: TObject);
    procedure btnWithoutColonClick(Sender: TObject);
    procedure btnWithoutEscClick(Sender: TObject);
    procedure btnWithoutQuoteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fWord: TGVWord; // mot de travail
    // traitement des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
  public
  end;

var
  GVMainForm: TGVMainForm;

implementation

{$R *.lfm}

{ TGVMainForm }

procedure TGVMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  inherited; // on hérite
  fWord := TGVWord.Create; // mot de travail créé
  // gestionnaire de recherche d'erreur actif
  fWord.Error.OnError := @GetError;
end;

procedure TGVMainForm.btnFirstClick(Sender: TObject);
// test de FIRST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text; // on affecte
  LS := fWord.First; // on effectue l'opération
  if fWord.Error.OK then // pas d'erreur ?
    lblResult.Caption := LS; // on affiche
end;

procedure TGVMainForm.btnGreaterPClick(Sender: TObject);
// test de ISGREATER
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsGreater(LabEdtSecond.Text), P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnGreatestClick(Sender: TObject);
// test de GREATEST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Greatest(LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnInsertClick(Sender: TObject);
// test de INSERT
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Insert(sedtInsert.Value, LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnIsBooleanClick(Sender: TObject);
// test de ISBOOLEAN
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsBoolean, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnIsIntClick(Sender: TObject);
// test de ISINT
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsInt, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnIsValidClick(Sender: TObject);
// test de ISVALID
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsValid, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnIsValidIdentClick(Sender: TObject);
// test de ISVALIDIDENT
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsValidIdent, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnItemClick(Sender: TObject);
// test de ITEM
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Item[sedtItem.Value];
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.ButFirst;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnAtRandomClick(Sender: TObject);
// test de ATRANDOM
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.AtRandom;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnClearClick(Sender: TObject);
// bouton effacer
begin
  mmoErrors.Clear;
  fWord.Error.Ok := True; // erreur annulée
end;

procedure TGVMainForm.btnAsNumberClick(Sender: TObject);
// test de ASNUMBER
var
  LDble: Double;
begin
  fWord.Text := LabEdtFirst.Text;
  LDble := fWord.AsNumber;
  if fWord.Error.Ok then
    lblResult.Caption := FloatToStr(LDble);
end;

procedure TGVMainForm.btnAsIntClick(Sender: TObject);
// test de ASINT
var
  Li: Integer;
begin
  fWord.Text := LabEdtFirst.Text;
  Li := fWord.AsInt;
  if fWord.Error.Ok then
    lblResult.Caption := IntToStr(Li);
end;

procedure TGVMainForm.btnAsBooleanClick(Sender: TObject);
// test de ASBOOLEAN
var
  LB: Boolean;
begin
  fWord.Text := LabEdtFirst.Text;
  LB := fWord.AsBoolean;
  if fWord.Error.Ok then
    lblResult.Caption := IfThen(LB, P_True, P_False);
end;

procedure TGVMainForm.btnButLastClick(Sender: TObject);
// test de BUTLAST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.ButLast;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnCountClick(Sender: TObject);
// test de COUNT
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.StrCount;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnDelItemClick(Sender: TObject);
// test de DELITEM
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.DelItem(sedtDelItem.Value);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnEmptyWordPClick(Sender: TObject);
// test de ISEMPTYWORD
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsEmptyWord, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnEqualPClick(Sender: TObject);
// test de ISEQUAL
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsEqual(LabEdtSecond.Text), P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnLastClick(Sender: TObject);
// test de LAST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Last;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.LowerCase;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnLowerPClick(Sender: TObject);
// test de ISLOWER
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsLower(LabEdtSecond.Text), P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnLowestClick(Sender: TObject);
// test de LOWEST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Lowest(LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnMemberPClick(Sender: TObject);
// test de MEMBERP
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsMember(LabEdtSecond.Text), P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnNumberPClick(Sender: TObject);
// test de ISNUMBER
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := IfThen(fWord.IsNumber, P_True, P_False);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
var
 LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.PutFirst(LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnPutLastClick(Sender: TObject);
// test de PUTLAST
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.PutLast(LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnReplaceClick(Sender: TObject);
// test de REPLACE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Replace(sedtReplace.Value, LabEdtSecond.Text);
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnReverseClick(Sender: TObject);
// test de REVERSE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Reverse;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnRotateClick(Sender: TObject);
// test de ROTATE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Rotate;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnShuffleClick(Sender: TObject);
// test de SHUFFLE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Shuffle;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnSortClick(Sender: TObject);
// test de SORT
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.Sort;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnUpperCaseClick(Sender: TObject);
// test de UPPERCASE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.UpperCase;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnWithEscClick(Sender: TObject);
// test de WITHESC
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.WithEsc;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnWithoutColonClick(Sender: TObject);
// test de WITHOUTCOLON
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.WithoutColon;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnWithoutEscClick(Sender: TObject);
// test de WITHOUTESC
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.WithoutEsc;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.btnWithoutQuoteClick(Sender: TObject);
// test de WITHOUTQUOTE
var
  LS: string;
begin
  fWord.Text := LabEdtFirst.Text;
  LS := fWord.WithoutQuote;
  if fWord.Error.OK then
    lblResult.Caption := LS;
end;

procedure TGVMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fWord.Free; // libération du mot de travail
  inherited Destroy; // on hérite
end;

procedure TGVMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// recherche d'une erreur
begin
  if ErrorRec.Code = CE_None then
    Exit; // on sort si pas d'erreur
  // message en toutes lettres
  mmoErrors.Lines.Add('>>> ' + fWord.Error.ErrorMessage);
  with mmoErrors.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
end;

end.

