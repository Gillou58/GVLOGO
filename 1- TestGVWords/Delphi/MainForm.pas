{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des mots             |
  |                  Unité : TestGVWords.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    02-08-2014 18:18:46                          |
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
{$DEFINE Delphi}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  GVWords, Vcl.ComCtrls, Vcl.Samples.Spin, Vcl.Buttons;

type
  TFormGVWords = class(TForm)
    grpEdit: TGroupBox;
    statMain: TStatusBar;
    grpMisc: TGroupBox;
    grpOne: TGroupBox;
    grpTwo: TGroupBox;
    lblOne: TLabel;
    lblTwo: TLabel;
    edtOne: TEdit;
    edtTwo: TEdit;
    lblResult: TLabel;
    btnFirst: TButton;
    btnLast: TButton;
    btnButFirst: TButton;
    btnButLast: TButton;
    btnLowerCase: TButton;
    btnUppercase: TButton;
    btnEmptyWordP: TButton;
    btnWithoutEsc: TButton;
    btnWithoutQuote: TButton;
    btnWithoutColon: TButton;
    btnWithEsc: TButton;
    btnCount: TButton;
    btnReverse: TButton;
    btnAtRandom: TButton;
    btnShuffle: TButton;
    btnSort: TButton;
    btnNumberP: TButton;
    seItem: TSpinEdit;
    btnItem: TButton;
    btnIsValidWord: TButton;
    btnIsValidIdentWord: TButton;
    btnEqualP: TButton;
    btnGreaterP: TButton;
    btnLowerP: TButton;
    btnMemberP: TButton;
    btnGreatest: TButton;
    btnLowest: TButton;
    btnPutFirst: TButton;
    btnPutLast: TButton;
    btnReplace: TButton;
    seReplace: TSpinEdit;
    btnInsert: TButton;
    seInsert: TSpinEdit;
    btnRotate: TButton;
    procedure btnFirstClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnLowerCaseClick(Sender: TObject);
    procedure btnUppercaseClick(Sender: TObject);
    procedure btnWithoutQuoteClick(Sender: TObject);
    procedure btnWithoutColonClick(Sender: TObject);
    procedure btnWithoutEscClick(Sender: TObject);
    procedure btnReverseClick(Sender: TObject);
    procedure btnAtRandomClick(Sender: TObject);
    procedure btnShuffleClick(Sender: TObject);
    procedure btnSortClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnEmptyWordPClick(Sender: TObject);
    procedure btnNumberPClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnItemClick(Sender: TObject);
    procedure btnEqualPClick(Sender: TObject);
    procedure btnGreaterPClick(Sender: TObject);
    procedure btnLowerPClick(Sender: TObject);
    procedure btnMemberPClick(Sender: TObject);
    procedure btnLowestClick(Sender: TObject);
    procedure btnGreatestClick(Sender: TObject);
    procedure btnPutFirstClick(Sender: TObject);
    procedure btnPutLastClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure btnIsValidWordClick(Sender: TObject);
    procedure btnIsValidIdentWordClick(Sender: TObject);
    procedure btnWithEscClick(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
  private
    { Déclarations privées }
    fWord: TGVWord; // mot de travail
  public
    { Déclarations publiques }
  end;

var
  FormGVWords: TFormGVWords;

implementation

{$R *.dfm}

uses
  StrUtils, GVConsts;

procedure TFormGVWords.btnAtRandomClick(Sender: TObject);
// test de ATRANDOM
begin
  lblResult.Caption := fWord.AtRandom(edtOne.Text);
end;

procedure TFormGVWords.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
begin
  lblResult.Caption := fWord.ButFirst(edtOne.Text);
end;

procedure TFormGVWords.btnButLastClick(Sender: TObject);
// test de BUTLAST
begin
  lblResult.Caption := fWord.ButLast(edtOne.Text);
end;

procedure TFormGVWords.btnCountClick(Sender: TObject);
// test de COUNT
begin
  lblResult.Caption := fWord.StrCount(edtOne.Text);
end;

procedure TFormGVWords.btnEmptyWordPClick(Sender: TObject);
// test de EMPTYWORDP
begin
  lblResult.Caption := IfThen(fWord.EmptyWordP(edtOne.Text),
      P_True,P_False);
end;

procedure TFormGVWords.btnEqualPClick(Sender: TObject);
// test de EQUALP
begin
  lblResult.Caption := IfThen(fWord.EqualP(edtOne.Text, edtTwo.Text),
      P_True,P_False);
end;

procedure TFormGVWords.btnFirstClick(Sender: TObject);
// test de FIRST
begin
  lblResult.Caption := fWord.First(edtOne.Text);
end;

procedure TFormGVWords.btnGreaterPClick(Sender: TObject);
// test de GREATERP
begin
  lblResult.Caption := IfThen(fWord.GreaterP(edtOne.Text, edtTwo.Text),
      P_True,P_False);
end;

procedure TFormGVWords.btnGreatestClick(Sender: TObject);
// test de GREATEST
begin
  lblResult.Caption := fWord.Greatest(edtOne.Text, edtTwo.Text);
end;

procedure TFormGVWords.btnInsertClick(Sender: TObject);
// test de INSERT
begin
  lblResult.Caption := fWord.Insert(edtOne.Text, edtTwo.Text, seInsert.Value);
end;

procedure TFormGVWords.btnIsValidIdentWordClick(Sender: TObject);
// test de ISVALIDIDENTWORD
begin
  lblResult.Caption := IfThen(fWord.IsValidIdent(edtOne.Text),
    P_True,P_False);
end;

procedure TFormGVWords.btnIsValidWordClick(Sender: TObject);
// test de ISVALID
begin
  lblResult.Caption := IfThen(fWord.IsValid(edtOne.Text), P_True,P_False);
end;

procedure TFormGVWords.btnItemClick(Sender: TObject);
// test de ITEM
begin
  lblResult.Caption := fWord.Item(edtOne.Text, seItem.Value);
end;

procedure TFormGVWords.btnLastClick(Sender: TObject);
// test de LAST
begin
  lblResult.Caption := fWord.Last(edtOne.Text);
end;

procedure TFormGVWords.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
begin
  lblResult.Caption := fWord.LowerCase(edtOne.Text);
end;

procedure TFormGVWords.btnLowerPClick(Sender: TObject);
// test de LOWERP
begin
  lblResult.Caption := IfThen(fWord.LowerP(edtOne.Text, edtTwo.Text),
      P_True,P_False);
end;

procedure TFormGVWords.btnLowestClick(Sender: TObject);
// test de LOWEST
begin
  lblResult.Caption := fWord.Lowest(edtOne.Text, edtTwo.Text);
end;

procedure TFormGVWords.btnMemberPClick(Sender: TObject);
// test de MEMBERP
begin
  lblResult.Caption := IfThen(fWord.MemberP(edtOne.Text, edtTwo.Text),
      P_True,P_False);
end;

procedure TFormGVWords.btnNumberPClick(Sender: TObject);
// test de NUMBERP
begin
  lblResult.Caption := IfThen(fWord.NumberP(edtOne.Text), P_True,P_False);
end;

procedure TFormGVWords.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
begin
  lblResult.Caption := fWord.PutFirst(edtOne.Text, edtTwo.Text);
end;

procedure TFormGVWords.btnPutLastClick(Sender: TObject);
// test de PUTLAST
begin
  lblResult.Caption := fWord.PutLast(edtOne.Text, edtTwo.Text);
end;

procedure TFormGVWords.btnReplaceClick(Sender: TObject);
// test de REPLACE
begin
  lblResult.Caption := fWord.Replace(edtOne.Text, edtTwo.Text,
      seReplace.Value);
end;

procedure TFormGVWords.btnReverseClick(Sender: TObject);
// test de REVERSE
begin
  lblResult.Caption := fWord.Reverse(edtOne.Text);
end;

procedure TFormGVWords.btnRotateClick(Sender: TObject);
// test de ROTATE
begin
  lblResult.Caption := fWord.Rotate(edtOne.Text);
end;

procedure TFormGVWords.btnShuffleClick(Sender: TObject);
// test de SHUFFLE
begin
  lblResult.Caption := fWord.Shuffle(edtOne.Text);
end;

procedure TFormGVWords.btnSortClick(Sender: TObject);
// test de SORT
begin
  lblResult.Caption := fWord.Sort(edtOne.Text);
end;

procedure TFormGVWords.btnUppercaseClick(Sender: TObject);
// test de UPPERCASE
begin
  lblResult.Caption := fWord.Uppercase(edtOne.Text);
end;

procedure TFormGVWords.btnWithEscClick(Sender: TObject);
// test de WITHESC
begin
  lblResult.Caption := fWord.WithEsc(edtOne.Text);
end;

procedure TFormGVWords.btnWithoutColonClick(Sender: TObject);
// test de WITHOUTCOLON
begin
  lblResult.Caption := fWord.WithoutColon(edtOne.Text);
end;

procedure TFormGVWords.btnWithoutEscClick(Sender: TObject);
// test de WITHOUTESC
begin
  lblResult.Caption := fWord.WithoutEsc(edtOne.Text);
end;

procedure TFormGVWords.btnWithoutQuoteClick(Sender: TObject);
// test de WITHOUTQUOTE
begin
  lblResult.Caption := fWord.WithoutQuote(edtOne.Text);
end;

procedure TFormGVWords.FormCreate(Sender: TObject);
// création de la fiche
begin
  fWord := TGVWord.Create;
end;

procedure TFormGVWords.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fWord.Free;
end;

end.
