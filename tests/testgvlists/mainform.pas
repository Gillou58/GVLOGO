{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du traitement des listes           |
  |                  Unité : TestGVLists.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 16:28:55                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// TestGVLists - part of GVLOGO
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
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls, Spin, ComCtrls,
  GVConsts, GVWords, GVLists;

type

  { TMainFormGVLists }

  TMainFormGVLists = class(TForm)
    bbtnClear: TBitBtn;
    BitBtn1: TBitBtn;
    btnLastErrorPos: TButton;
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
    btnIsEmpty: TButton;
    btnIsEmptyList: TButton;
    btnN: TButton;
    btnCount: TButton;
    btnLastItem: TButton;
    btnWordToList: TButton;
    btnEmptyList: TButton;
    btnIsSimpleList: TButton;
    btnIsValid: TButton;
    btnIsValidValue: TButton;
    btnTestValue: TButton;
    btnRotate: TButton;
    gbEdit: TGroupBox;
    gbListUtils: TGroupBox;
    gbTGVList: TGroupBox;
    edtList: TLabeledEdit;
    edtStr: TLabeledEdit;
    lblNum: TLabel;
    seNum: TSpinEdit;
    stbMain: TStatusBar;
    SynMemo: TSynMemo;
    procedure bbtnClearClick(Sender: TObject);
    procedure btnButFirstClick(Sender: TObject);
    procedure btnButLastClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnDeleteItemClick(Sender: TObject);
    procedure btnEmptyListClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnInsertAItemClick(Sender: TObject);
    procedure btnIsEmptyClick(Sender: TObject);
    procedure btnIsItemClick(Sender: TObject);
    procedure btnIsSimpleListClick(Sender: TObject);
    procedure btnIsValidClick(Sender: TObject);
    procedure btnIsValidValueClick(Sender: TObject);
    procedure btnLastErrorPosClick(Sender: TObject);
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
    procedure btnTestValueClick(Sender: TObject);
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
    { private declarations }
    fList: TGVList;
    fWord: TGVWord;
    fUtil: TGVListUtils;
  public
    { public declarations }
  end;

var
  MainFormGVLists: TMainFormGVLists;

implementation

{$R *.lfm}
 uses
   StrUtils;

{ TMainFormGVLists }

procedure TMainFormGVLists.bbtnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  SynMemo.Lines.Clear;
end;

procedure TMainFormGVLists.btnButFirstClick(Sender: TObject);
// test de BUTFIRST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ButFirst);
end;

procedure TMainFormGVLists.btnButLastClick(Sender: TObject);
// test de BUTLAST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ButLast);
end;

procedure TMainFormGVLists.btnCountClick(Sender: TObject);
// test de COUNT
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IntToStr(fList.Count));
end;

procedure TMainFormGVLists.btnDeleteItemClick(Sender: TObject);
// test de DELETEITEM
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.DeleteItem(seNum.value));
end;

procedure TMainFormGVLists.btnEmptyListClick(Sender: TObject);
// test de EMPTYLIST
begin
  SynMemo.Lines.Add(fUtil.EmptyList);
end;

procedure TMainFormGVLists.btnFirstClick(Sender: TObject);
// test de FIRST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.First);
end;

procedure TMainFormGVLists.btnInsertAItemClick(Sender: TObject);
// test de INSERTAITEM
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.InsertAItem(seNum.Value, edtStr.Text));
end;

procedure TMainFormGVLists.btnIsEmptyClick(Sender: TObject);
// test de ISEMPTY
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IfThen(fList.IsEmpty, P_True, P_False));
end;

procedure TMainFormGVLists.btnIsItemClick(Sender: TObject);
// test de ISITEM
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IfThen(fList.IsItem(edtStr.Text), P_True, P_False));
end;

procedure TMainFormGVLists.btnIsSimpleListClick(Sender: TObject);
// test de ISSIMPLELIST
begin
  SynMemo.Lines.Add(IfThen(fUtil.IsSimpleList(edtList.Text), P_True, P_False));
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

procedure TMainFormGVLists.btnLastErrorPosClick(Sender: TObject);
// test de LASTERRORPOS
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IntToStr(fList.LastErrorPos));
end;

procedure TMainFormGVLists.btnLastItemClick(Sender: TObject);
// test de LASTITEM
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IntToStr(fList.LastItem));
end;

procedure TMainFormGVLists.btnListToStrClick(Sender: TObject);
// test de LISTTOSTR
begin
  SynMemo.Lines.Add(fUtil.ListToStr(edtList.Text));
end;

procedure TMainFormGVLists.btnListToWordClick(Sender: TObject);
// test de LISTTOWORD
begin
  SynMemo.Lines.Add(fUtil.ListToWord(edtList.Text));
end;

procedure TMainFormGVLists.btnLowerCaseClick(Sender: TObject);
// test de LOWERCASE
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.LowerCase);
end;

procedure TMainFormGVLists.btnNClick(Sender: TObject);
// test de [N]
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList[seNum.Value]);
end;

procedure TMainFormGVLists.btnPutFirstClick(Sender: TObject);
// test de PUTFIRST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.PutFirst(edtStr.Text));
end;

procedure TMainFormGVLists.btnPutLastClick(Sender: TObject);
// test de PUTLAST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.PutLast(edtStr.Text));
end;

procedure TMainFormGVLists.btnReplaceItemClick(Sender: TObject);
// test de REPLACEITEM
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ReplaceItem(seNum.Value, edtStr.Text));
end;

procedure TMainFormGVLists.btnReverseItemsClick(Sender: TObject);
// test de REVERSEITEMS
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ReverseItems);
end;

procedure TMainFormGVLists.btnRotateClick(Sender: TObject);
// test de ROTATE
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.Rotate);
end;

procedure TMainFormGVLists.btnSentenceLeftClick(Sender: TObject);
// test de SENTENCELEFT
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.SentenceLeft(edtStr.Text));
end;

procedure TMainFormGVLists.btnSentenceRightClick(Sender: TObject);
// test de SENTENCERIGHT
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.SentenceRight(edtStr.Text));
end;

procedure TMainFormGVLists.btnShuffleItemsClick(Sender: TObject);
// test de SHUFFLEITEMS
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ShuffleItems);
end;

procedure TMainFormGVLists.btnSortItemsClick(Sender: TObject);
// test de SORTITEMS
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.SortItems);
end;

procedure TMainFormGVLists.btnStrToListClick(Sender: TObject);
// test de STRTOLIST
begin
  SynMemo.Lines.Add(fUtil.StrToList(edtStr.Text));
end;

procedure TMainFormGVLists.btnTestValueClick(Sender: TObject);
// test de TESTVALUE
begin
  fUtil.TestValue(edtList.Text);
  SynMemo.Lines.Add('Pas d''erreur à signaler!');
end;

procedure TMainFormGVLists.btnToStrClick(Sender: TObject);
// test de ToStr
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ToStr);
end;

procedure TMainFormGVLists.btnToWBStrClick(Sender: TObject);
// test de TOWBSTR
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.ToWBStr);
end;

procedure TMainFormGVLists.btnTwoAddClick(Sender: TObject);
// test de TWOADD
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.TwoAdd(edtStr.Text, edtStr.Text));
end;

procedure TMainFormGVLists.btnTwoDeleteClick(Sender: TObject);
// test de TWODELETE
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.TwoDelete(seNum.Value));
end;

procedure TMainFormGVLists.btnUpperCaseClick(Sender: TObject);
// test de UPPERCASE
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.UpperCase);
end;

procedure TMainFormGVLists.btnWordToListClick(Sender: TObject);
// test de WORDTOLIST
begin
  SynMemo.Lines.Add(fUtil.WordToList(edtStr.Text));
end;

procedure TMainFormGVLists.btnLastClick(Sender: TObject);
// test de LAST
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(fList.Last);
end;

procedure TMainFormGVLists.FormCreate(Sender: TObject);
// création de la fiche
begin
  fList := TGVList.Create;  // création des outils
  fWord := TGVWord.Create;
  fUtil := TGVListUtils.Create;
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
begin
  fList.Text:= edtList.Text;
  SynMemo.Lines.Add(IfThen(fList.IsEmptyList, P_True, P_False));
end;

end.

