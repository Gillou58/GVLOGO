{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des mots             |
  |                  Unité : TestGVWords.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 18:18:46                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// TestGVWords - part of GVLOGO
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
  
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Buttons, StrUtils, GVConsts, GVWords;

type

  { TGVMainForm }

  TGVMainForm = class(TForm)
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
    GBEdit: TGroupBox;
    GBOneWord: TGroupBox;
    GBTwoWords: TGroupBox;
    GBMisc: TGroupBox;
    LabEdtFirst: TLabeledEdit;
    LabEdtSecond: TLabeledEdit;
    lblResult: TLabel;
    sedtItem: TSpinEdit;
    sedtReplace: TSpinEdit;
    sedtInsert: TSpinEdit;
    procedure btnAtRandomClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnEmptyWordPClick(Sender: TObject);
    procedure btnEqualPClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnGreaterPClick(Sender: TObject);
    procedure btnGreatestClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
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
    { private declarations }
    fWord: TGVWord; // mot de travail
  public
    { public declarations }
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
end;

procedure TGVMainForm.btnFirstClick(Sender: TObject);
// test de FIRST
begin
  lblResult.Caption := fWord.First(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnGreaterPClick(Sender: TObject);
// test de GREATERP
begin
  lblResult.Caption := IfThen(fWord.GreaterP(LabEdtFirst.Text, LabEdtSecond.Text),
     P_True, P_False);
end;

procedure TGVMainForm.btnGreatestClick(Sender: TObject);
// test de GREATEST
begin
  lblResult.Caption := fWord.Greatest(LabEdtFirst.Text, LabEdtSecond.Text);
end;

procedure TGVMainForm.btnInsertClick(Sender: TObject);
// test de INSERT
begin
   lblResult.Caption := fWord.Insert(LabEdtFirst.Text, LabEdtSecond.Text,
    sedtInsert.Value);
end;

procedure TGVMainForm.btnIsValidClick(Sender: TObject);
// test de ISVALID
begin
  lblResult.Caption := IfThen(fWord.IsValid(LabEdtFirst.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnIsValidIdentClick(Sender: TObject);
// test de ISVALIDIDENT
begin
  lblResult.Caption := IfThen(fWord.IsValidIdent(LabEdtFirst.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnItemClick(Sender: TObject);
// test de ITEM
begin
  lblResult.Caption := fWord.Item(LabEdtFirst.Text, sedtItem.Value);
end;

procedure TGVMainForm.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
begin
  lblResult.Caption := fWord.ButFirst(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnAtRandomClick(Sender: TObject);
// test de ATRANDOM
begin
  lblResult.Caption := fWord.AtRandom(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnButLastClick(Sender: TObject);
// test de BUTLAST
begin
  lblResult.Caption := fWord.ButLast(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnCountClick(Sender: TObject);
// test de COUNT
begin
  lblResult.Caption := fWord.StrCount(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnEmptyWordPClick(Sender: TObject);
// test de EMPTYWORDP
begin
  lblResult.Caption := IfThen(fWord.EmptyWordP(LabEdtFirst.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnEqualPClick(Sender: TObject);
// test de EQUALP
begin
  lblResult.Caption := IfThen(fWord.EqualP(LabEdtFirst.Text, LabEdtSecond.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnLastClick(Sender: TObject);
// test de LAST
begin
  lblResult.Caption := fWord.Last(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
begin
  lblResult.Caption := fWord.LowerCase(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnLowerPClick(Sender: TObject);
// test de LOWERP
begin
  lblResult.Caption := IfThen(fWord.LowerP(LabEdtFirst.Text, LabEdtSecond.Text),
     P_True, P_False);
end;

procedure TGVMainForm.btnLowestClick(Sender: TObject);
// test de LOWEST
begin
  lblResult.Caption := fWord.Lowest(LabEdtFirst.Text, LabEdtSecond.Text);
end;

procedure TGVMainForm.btnMemberPClick(Sender: TObject);
// test de MEMBERP
begin
  lblResult.Caption := IfThen(fWord.MemberP(LabEdtFirst.Text, LabEdtSecond.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnNumberPClick(Sender: TObject);
// test de NUMBERP
begin
  lblResult.Caption := IfThen(fWord.NumberP(LabEdtFirst.Text),
    P_True, P_False);
end;

procedure TGVMainForm.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
begin
  lblResult.Caption := fWord.PutFirst(LabEdtFirst.Text, LabEdtSecond.Text);
end;

procedure TGVMainForm.btnPutLastClick(Sender: TObject);
// test de PUTLAST
begin
  lblResult.Caption := fWord.PutLast(LabEdtFirst.Text, LabEdtSecond.Text);
end;

procedure TGVMainForm.btnReplaceClick(Sender: TObject);
// test de REPLACE
begin
  lblResult.Caption := fWord.Replace(LabEdtFirst.Text, LabEdtSecond.Text,
    sedtReplace.Value);
end;

procedure TGVMainForm.btnReverseClick(Sender: TObject);
// test de REVERSE
begin
  lblResult.Caption := fWord.Reverse(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnRotateClick(Sender: TObject);
// test de ROTATE
begin
  lblResult.Caption := fWord.Rotate(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnShuffleClick(Sender: TObject);
// test de SHUFFLE
begin
  lblResult.Caption := fWord.Shuffle(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnSortClick(Sender: TObject);
// test de SORT
begin
  lblResult.Caption := fWord.Sort(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnUpperCaseClick(Sender: TObject);
// test de UPPERCASE
begin
  lblResult.Caption := fWord.UpperCase(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnWithEscClick(Sender: TObject);
// test de WITHESC
begin
  lblResult.Caption := fWord.WithEsc(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnWithoutColonClick(Sender: TObject);
// test de WITHOUTCOLON
begin
  lblResult.Caption := fWord.WithoutColon(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnWithoutEscClick(Sender: TObject);
// test de WITHOUTESC
begin
  lblResult.Caption := fWord.WithoutEsc(LabEdtFirst.Text);
end;

procedure TGVMainForm.btnWithoutQuoteClick(Sender: TObject);
// test de WITHOUTQUOTE
begin
  lblResult.Caption := fWord.WithoutQuote(LabEdtFirst.Text);
end;

procedure TGVMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fWord.Free; // libération du mot de travail
  inherited Destroy; // on hérite
end;

end.

