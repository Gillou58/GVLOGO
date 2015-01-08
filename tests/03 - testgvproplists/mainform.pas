{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unité GVPropLists             |
  |                  Unité : TestGVPropLists.pas                           |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

  
// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVPROPLISTS - part of GVLOGO
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

unit MainForm;

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, Spin, ComCtrls,
  GVPropLists, // listes de propriétés
  GVErrConsts; // constantes d'erreurs

const
  P_True = 'vrai';
  P_False = 'faux';

type

  { TMainFormGVPropLists }

  TMainFormGVPropLists = class(TForm)
    btnExit: TBitBtn;
    bitbtnClear: TBitBtn;
    btnClear: TButton;
    btnN: TButton;
    btnLoadFromFile: TButton;
    btnSaveToFile: TButton;
    btnIsListPByNum: TButton;
    btnListP: TButton;
    btnEnumerator: TButton;
    btnIsProp: TButton;
    btnNumProp: TButton;
    btnValProp: TButton;
    btnRemoveProp: TButton;
    btnUpdateListP: TButton;
    btnCountProps: TButton;
    btnValNumProp: TButton;
    btnListOfProps: TButton;
    btnNameOfProp: TButton;
    btnRemoveListP: TButton;
    btnCountListP: TButton;
    btnIsListP: TButton;
    btnValListP: TButton;
    btnNumListP: TButton;
    btnValNumListP: TButton;
    btnListPByNum: TButton;
    btnLPByName: TButton;
    gbEdit: TGroupBox;
    edtName: TLabeledEdit;
    edtProp: TLabeledEdit;
    edtValue: TLabeledEdit;
    gbPropList: TGroupBox;
    gbProps: TGroupBox;
    Label1: TLabel;
    seNum: TSpinEdit;
    mmoMain: TSynMemo;
    StatusBar: TStatusBar;
    procedure bitbtnClearClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCountListPClick(Sender: TObject);
    procedure btnCountPropsClick(Sender: TObject);
    procedure btnEnumeratorClick(Sender: TObject);
    procedure btnIsListPByNumClick(Sender: TObject);
    procedure btnIsListPClick(Sender: TObject);
    procedure btnIsPropClick(Sender: TObject);
    procedure btnListOfPropsClick(Sender: TObject);
    procedure btnListPByNumClick(Sender: TObject);
    procedure btnListPClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure btnLPByNameClick(Sender: TObject);
    procedure btnNameOfPropClick(Sender: TObject);
    procedure btnNClick(Sender: TObject);
    procedure btnNumListPClick(Sender: TObject);
    procedure btnNumPropClick(Sender: TObject);
    procedure btnRemoveListPClick(Sender: TObject);
    procedure btnRemovePropClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnUpdateListPClick(Sender: TObject);
    procedure btnValListPClick(Sender: TObject);
    procedure btnValNumListPClick(Sender: TObject);
    procedure btnValNumPropClick(Sender: TObject);
    procedure btnValPropClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fPropList: TGVPropList;
    // "s" final du pluriel
    function Plural(N: Integer): string;
  public
    // traitement des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
end;

var
  MainFormGVPropLists: TMainFormGVPropLists;

implementation

{$R *.lfm}

uses
  StrUtils;

{ TMainFormGVPropLists }

procedure TMainFormGVPropLists.bitbtnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoMain.Clear;
end;

procedure TMainFormGVPropLists.btnClearClick(Sender: TObject);
// test de CLEAR
begin
  fPropList.Clear;
  mmoMain.Lines.Add('Les listes de propriétés ont été remises à zéro.');
end;

procedure TMainFormGVPropLists.btnCountListPClick(Sender: TObject);
// test de COUNTLISTP
begin
  mmoMain.Lines.Add('Il y a ' +
  IntToStr(fPropList.CountListP) + ' liste' + Plural(fPropList.CountListP) +
    ' de propriétés.');
end;

procedure TMainFormGVPropLists.btnCountPropsClick(Sender: TObject);
// test de COUNTPROPS
var
  LS: string;
begin
  LS := IntToStr(fPropList.CountProps(edtName.Text));
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' comporte ' +
      LS + ' propriété' + Plural(StrToInt(LS)) + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnEnumeratorClick(Sender: TObject);
// test de ENUMERATOR
var
  LS: string;
begin
  mmoMain.Lines.Add('<<< Enumération :');
  for LS in fPropList do
    mmoMain.Lines.Add(LS);
  mmoMain.Lines.Add('Fin de l''énumération. >>>');
end;

procedure TMainFormGVPropLists.btnIsListPByNumClick(Sender: TObject);
// test de ISLISTPBYNUM
begin
  mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' existe : ' +
    IfThen(fPropList.IsListPByNum(seNum.Value), P_True,
    P_False));
end;

procedure TMainFormGVPropLists.btnIsListPClick(Sender: TObject);
// test de ISLISP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' existe : ' +
    IfThen(fPropList.IsListP(edtName.Text), P_True, P_False));
end;

procedure TMainFormGVPropLists.btnIsPropClick(Sender: TObject);
// test de ISPROP
var
  LB: Boolean;
begin
  LB := fPropList.IsProp(edtName.Text, edtProp.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' a pour propriété ' +
      edtProp.Text + ' : ' +
        IfThen(LB, P_True, P_False))
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnListOfPropsClick(Sender: TObject);
// test de LISTOFPROPS
var
  LS: string;
begin
  LS := fPropList.ListOfProps(edtName.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('Propriétés de ' + edtName.Text + ' : ' + LS)
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnListPByNumClick(Sender: TObject);
// test de LISTPBYNUM
var
  LS: string;
begin
  LS := fPropList.ListPByNum[seNum.Value];
  if fPropList.Error.OK then
    mmoMain.Lines.Add(LS)
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnListPClick(Sender: TObject);
// test de LISP
begin
  mmoMain.Lines.Add('Listes de propriétés enregistrées : ' +
    fPropList.ListP);
end;

procedure TMainFormGVPropLists.btnLoadFromFileClick(Sender: TObject);
// test de LOADFROMFILE
begin
  fPropList.LoadFromFile('ListPs');
  if fPropList.Error.OK then
    mmoMain.Lines.Add('Les listes de propriétés ont été chargées.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnLPByNameClick(Sender: TObject);
// test de LPBYNAME
var
  LS: string;
begin
  LS := fPropList.LPByName[edtName.Text];
  if fPropList.Error.OK then
    mmoMain.Lines.Add(LS)
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnNameOfPropClick(Sender: TObject);
// test de NAMEOFPROP
var
  LS: string;
begin
  LS := fPropList.NameOfProp(edtName.Text, seNum.Value);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La propriété ' + IntToStr(seNum.Value) + ' de la liste ' +
      edtName.Text + ' est ' + LS + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnNClick(Sender: TObject);
// test de [N]
var
  LS: string;
begin
  LS := fPropList[seNum.Value];
  if fPropList.Error.OK then
    mmoMain.Lines.Add(LS)
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnNumListPClick(Sender: TObject);
// test de NUMLISTP
var
  LS: string;
begin
  LS := IntToStr(fPropList.NumListP(edtName.Text));
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' porte le numéro ' +
      LS + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnNumPropClick(Sender: TObject);
// test de NUMPROP
var
  LS: string;
begin
  LS := IntToStr(fPropList.NumProp(edtName.Text, edtProp.Text));
  if fPropList.Error.OK then
  mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
    ' a pour numéro ' + LS + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnRemoveListPClick(Sender: TObject);
// test de REMOVELISTP
begin
  fPropList.RemoveListP(edtName.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' a été supprimée.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnRemovePropClick(Sender: TObject);
// test de REMOVEPROP
begin
  fPropList.RemoveProp(edtName.Text, edtProp.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
      ' a été supprimée.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnSaveToFileClick(Sender: TObject);
// test de SAVETOFILE
begin
  fPropList.SaveToFile('ListPs');
  if fPropList.Error.OK then
    mmoMain.Lines.Add('Le fichier a été sauvegardé.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnUpdateListPClick(Sender: TObject);
// test de UPDATELISTP
begin
  fPropList.UpDateListP(edtName.Text, edtProp.Text, edtValue.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' ayant pour valeur ' +
      edtValue.Text + ' a été ajoutée à ' + edtName.Text + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnValListPClick(Sender: TObject);
// test de VALLISP
var
  LS: string;
begin
  LS := fPropList.ValListP(edtName.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' vaut ' +
      LS + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnValNumListPClick(Sender: TObject);
// test de VALNUMLISTP
var
  LName, LValue: string;
begin
  fPropList.ValNumListP(seNum.Value, LName, LValue);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' a pour nom ' +
      LName + ' et valeurs ' + LValue + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnValNumPropClick(Sender: TObject);
// test de VALNUMPROP
var
  LS: string;
begin
  fPropList.ValNumProp(edtName.Text, seNum.Value, LS);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La valeur de la propriété ' + IntToStr(seNum.Value) +
      ' de ' + edtName.Text + ' est ' + LS + '.')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.btnValPropClick(Sender: TObject);
// test de VALPROP
var
  LS: string;
begin
  LS := fPropList.ValProp(edtName.Text, edtProp.Text);
  if fPropList.Error.OK then
    mmoMain.Lines.Add('La valeur de la propriété ' + edtProp.Text +
      ' de la liste ' + edtName.Text + ' est "' + LS + '".')
  else
    fPropList.Error.Clear; // erreurs nettoyées
end;

procedure TMainFormGVPropLists.FormCreate(Sender: TObject);
// création de la fiche
begin
  // listes de propriétés
  fPropList := TGVPropList.Create;
  // gestionnaire de recherche d'erreur actif
  fPropList.Error.OnError := @GetError;
end;

procedure TMainFormGVPropLists.FormDestroy(Sender: TObject);
// destruction de la liste
begin
  fPropList.Free;
end;

function TMainFormGVPropLists.Plural(N: Integer): string;
// "s" final du pluriel
begin
  Result := Ifthen(N > 1, 's', EmptyStr);
end;

procedure TMainFormGVPropLists.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// recherche d'une erreur
begin
  // message en toutes lettres
  mmoMain.Lines.Add('>>> ' + fPropList.Error.ErrorMessage);
  with mmoMain.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
end;

end.

