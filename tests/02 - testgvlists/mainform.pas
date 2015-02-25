{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des listes           |
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

// TESTGVLISTS - part of GVLOGO
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

{$I GVDefines.inc}

unit MainForm;

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls, Spin, ComCtrls,
  GVWords, // mots
  GVLists, // listes
  GVErrConsts; // constantes pour erreurs

const
  P_True = 'vrai';
  P_False = 'faux';

type

  { TMainFormGVLists }

  TMainFormGVLists = class(TForm)
    btnClear: TBitBtn;
    btnClose: TBitBtn;
    btnListToStr: TButton;
    btnFirst: TButton;
    btnLast: TButton;
    btnButFirst: TButton;
    btnButLast: TButton;
    btnPutFirst: TButton;
    btnPutLast: TButton;
    btnSentenceLeft: TButton;
    btnSentenceRight: TButton;
    btnUpperCase: TButton;
    btnLowerCase: TButton;
    btnStrToList: TButton;
    btnTwoAdd: TButton;
    btnTwoDelete: TButton;
    btnReverseItems: TButton;
    btnSortItems: TButton;
    btnShuffleItems: TButton;
    btnToStr: TButton;
    btnToWBStr: TButton;
    btnDeleteItem: TButton;
    btnInsertAItem: TButton;
    btnReplaceItem: TButton;
    btnListToWord: TButton;
    btnIsItem: TButton;
    btnIsEmptyList: TButton;
    btnN: TButton;
    btnCount: TButton;
    btnLastItem: TButton;
    btnWordToList: TButton;
    btnEmptyList: TButton;
    btnIsSimpleList: TButton;
    btnIsValid: TButton;
    btnIsValidValue: TButton;
    btnRotate: TButton;
    btnAtRandom: TButton;
    gbEdit: TGroupBox;
    gbListUtils: TGroupBox;
    gbTGVList: TGroupBox;
    edtList: TLabeledEdit;
    edtStr: TLabeledEdit;
    lblNum: TLabel;
    seNum: TSpinEdit;
    stbMain: TStatusBar;
    SynMemo: TSynMemo;
    procedure btnClearClick(Sender: TObject);
    procedure btnAtRandomClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnEmptyListClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnInsertAItemClick(Sender: TObject);
    procedure btnIsItemClick(Sender: TObject);
    procedure btnIsSimpleListClick(Sender: TObject);
    procedure btnIsValidClick(Sender: TObject);
    procedure btnIsValidValueClick(Sender: TObject);
    procedure btnLastItemClick(Sender: TObject);
    procedure btnListToStrClick(Sender: TObject);
    procedure btnListToWordClick(Sender: TObject);
    procedure btnLowerCaseClick(Sender: TObject);
    procedure btnNClick(Sender: TObject);
    procedure btnPutFirstClick(Sender: TObject);
    procedure btnPutLastClick(Sender: TObject);
    procedure btnReplaceItemClick(Sender: TObject);
    procedure btnReverseItemsClick(Sender: TObject);
    procedure btnRotateClick(Sender: TObject);
    procedure btnSentenceLeftClick(Sender: TObject);
    procedure btnSentenceRightClick(Sender: TObject);
    procedure btnShuffleItemsClick(Sender: TObject);
    procedure btnSortItemsClick(Sender: TObject);
    procedure btnStrToListClick(Sender: TObject);
    procedure btnToStrClick(Sender: TObject);
    procedure btnToWBStrClick(Sender: TObject);
    procedure btnTwoAddClick(Sender: TObject);
    procedure btnTwoDeleteClick(Sender: TObject);
    procedure btnUpperCaseClick(Sender: TObject);
    procedure btnWordToListClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnIsEmptyListClick(Sender: TObject);
  private
    fList: TGVList; // liste de travail
    fWord: TGVWord; // mot de travail
    fUtil: TGVListUtils; // utilitaire de travail
  public
    // recherche des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
end;

var
  MainFormGVLists: TMainFormGVLists;

implementation

{$R *.lfm}
 uses
   StrUtils;

{ TMainFormGVLists }

procedure TMainFormGVLists.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  SynMemo.Lines.Clear;
end;

procedure TMainFormGVLists.btnAtRandomClick(Sender: TObject);
// test de ATRANDOM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.AtRandom;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ButFirst;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnButLastClick(Sender: TObject);
// test de BUTLAST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ButLast;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnCountClick(Sender: TObject);
// test de COUNT
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := IntToStr(fList.Count);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnDeleteItemClick(Sender: TObject);
// test de DELETEITEM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.DeleteItem(seNum.value);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnEmptyListClick(Sender: TObject);
// test de EMPTYLIST
begin
  SynMemo.Lines.Add(fUtil.EmptyList);
end;

procedure TMainFormGVLists.btnFirstClick(Sender: TObject);
// test de FIRST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.First;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnInsertAItemClick(Sender: TObject);
// test de INSERTAITEM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.InsertAItem(seNum.Value, edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnIsItemClick(Sender: TObject);
// test de ISITEM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := IfThen(fList.IsItem(edtStr.Text), P_True, P_False);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnIsSimpleListClick(Sender: TObject);
// test de ISSIMPLELIST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := IfThen(fUtil.IsSimpleList(edtList.Text), P_True, P_False);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnIsValidClick(Sender: TObject);
// test de ISVALID
begin
  SynMemo.Lines.Add(IfThen(fUtil.IsValid(edtList.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnIsValidValueClick(Sender: TObject);
// test de ISVALIDVALUE
begin
  SynMemo.Lines.Add(IfThen(fUtil.IsValidValue(edtList.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnLastItemClick(Sender: TObject);
// test de LASTITEM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := IntToStr(fList.LastItem);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnListToStrClick(Sender: TObject);
// test de LISTTOSTR
var
  LS: string;
begin
  if fUtil.ListToStr(edtList.Text, LS) then
    SynMemo.Lines.Add(LS)
  else
    SynMemo.Lines.Add('Une erreur discrète...');
end;

procedure TMainFormGVLists.btnListToWordClick(Sender: TObject);
// test de LISTTOWORD
begin
  SynMemo.Lines.Add(fUtil.ListToWord(edtList.Text));
end;

procedure TMainFormGVLists.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.LowerCase;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnNClick(Sender: TObject);
// test de [N]
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList[seNum.Value - 1];
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.PutFirst(edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnPutLastClick(Sender: TObject);
// test de PUTLAST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.PutLast(edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnReplaceItemClick(Sender: TObject);
// test de REPLACEITEM
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ReplaceItem(seNum.Value, edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnReverseItemsClick(Sender: TObject);
// test de REVERSEITEMS
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ReverseItems;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnRotateClick(Sender: TObject);
// test de ROTATE
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.Rotate;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnSentenceLeftClick(Sender: TObject);
// test de SENTENCELEFT
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.SentenceLeft(edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnSentenceRightClick(Sender: TObject);
// test de SENTENCERIGHT
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.SentenceRight(edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnShuffleItemsClick(Sender: TObject);
// test de SHUFFLEITEMS
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ShuffleItems;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnSortItemsClick(Sender: TObject);
// test de SORTITEMS
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.SortItems;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnStrToListClick(Sender: TObject);
// test de STRTOLIST
begin
  SynMemo.Lines.Add(fUtil.StrToList(edtStr.Text));
end;

procedure TMainFormGVLists.btnToStrClick(Sender: TObject);
// test de TOSTR
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ToStr;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnToWBStrClick(Sender: TObject);
// test de TOWBSTR
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.ToWBStr;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnTwoAddClick(Sender: TObject);
// test de TWOADD
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.TwoAdd(edtStr.Text, edtStr.Text);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnTwoDeleteClick(Sender: TObject);
// test de TWODELETE
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.TwoDelete(seNum.Value);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnUpperCaseClick(Sender: TObject);
// test de UPPERCASE
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.UpperCase;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.btnWordToListClick(Sender: TObject);
// test de WORDTOLIST
begin
  SynMemo.Lines.Add(fUtil.WordToList(edtStr.Text));
end;

procedure TMainFormGVLists.btnLastClick(Sender: TObject);
// test de LAST
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := fList.Last;
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.FormCreate(Sender: TObject);
// création de la fiche
begin
  fList := TGVList.Create;  // création des outils
  fWord := TGVWord.Create;
  fUtil := TGVListUtils.Create;
  // gestionnaire de recherche d'erreur actif
  fList.Error.OnError := @GetError;
end;

procedure TMainFormGVLists.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fList.Free; // libération des outils
  fWord.Free;
  fUtil.Free;
end;

procedure TMainFormGVLists.btnIsEmptyListClick(Sender: TObject);
// test de btnIsEmptyList
var
  LS: string;
begin
  fList.Text:= edtList.Text;
  LS := IfThen(fList.IsEmptyList, P_True, P_False);
  if fList.Error.OK then
    SynMemo.Lines.Add(LS);
end;

procedure TMainFormGVLists.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// recherche d'une erreur
begin
  with SynMemo.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
  // message en toutes lettres
  SynMemo.Lines.Add('>>> ' + fList.Error.ErrorMessage);
end;

end.

