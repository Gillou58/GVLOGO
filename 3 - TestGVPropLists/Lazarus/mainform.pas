{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unité GVPropLists             |
  |                  Unité : TestGVPropLists.pas                           |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    25-06-2014 21:17:49                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ExtCtrls, Spin, ComCtrls, GVConsts, GVPropLists;

type

  { TMainFormGVPropLists }

  TMainFormGVPropLists = class(TForm)
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
    gbEdit: TGroupBox;
    edtName: TLabeledEdit;
    edtProp: TLabeledEdit;
    edtValue: TLabeledEdit;
    gbPropList: TGroupBox;
    gbProps: TGroupBox;
    Label1: TLabel;
    seNum: TSpinEdit;
    mmoMain: TSynMemo;
    StatusBar1: TStatusBar;
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
    { private declarations }
    fPropList: TGVPropList;
  public
    { public declarations }
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
  mmoMain.Lines.Add('Nombre de listes de propriétés : ' +
  IntToStr(fPropList.CountListP) + '.');
end;

procedure TMainFormGVPropLists.btnCountPropsClick(Sender: TObject);
// test de COUNTPROPS
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' comporte ' +
    IntToStr(fPropList.CountProps(edtName.Text)) + ' propriété(s).');
end;

procedure TMainFormGVPropLists.btnEnumeratorClick(Sender: TObject);
// test de ENUMERATOR
var
  S: string;
begin
  mmoMain.Lines.Add('Enumération :');
  for S in fPropList do
    mmoMain.Lines.Add(S);
  mmoMain.Lines.Add('Fin de l''énumération.');
end;

procedure TMainFormGVPropLists.btnIsListPByNumClick(Sender: TObject);
// test de ISLISTPBYNUM
begin
  mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' existe :');
  mmoMain.Lines.Add(IfThen(fPropList.IsListPByNum(seNum.Value), P_True,
    P_False));
end;

procedure TMainFormGVPropLists.btnIsListPClick(Sender: TObject);
// test de ISLISP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' existe :');
  mmoMain.Lines.Add(IfThen(fPropList.IsListP(edtName.Text), P_True, P_False));
end;

procedure TMainFormGVPropLists.btnIsPropClick(Sender: TObject);
// test de ISPROP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' a pour propriété ' +
    edtProp.Text + ':');
  mmoMain.Lines.Add(IfThen(fPropList.IsProp(edtName.Text, edtProp.Text), P_True,
    P_False));
end;

procedure TMainFormGVPropLists.btnListOfPropsClick(Sender: TObject);
// test de LISTOFPROPS
begin
  mmoMain.Lines.Add('Propriétés de ' + edtName.Text + ' : ' +
    fPropList.ListOfProps(edtName.Text));
end;

procedure TMainFormGVPropLists.btnListPByNumClick(Sender: TObject);
// test de LISTPBYNUM
begin
  mmoMain.Lines.Add(fPropList.ListPByNum[seNum.Value]);
end;

procedure TMainFormGVPropLists.btnListPClick(Sender: TObject);
// test de LISP
begin
  mmoMain.Lines.Add('Listes de propriétés enregistrées :');
  mmoMain.Lines.Add(fPropList.ListP);
end;

procedure TMainFormGVPropLists.btnLoadFromFileClick(Sender: TObject);
// test de LOADFROMFILE
begin
  fPropList.LoadFromFile('ListPs');
  mmoMain.Lines.Add('Les listes de propriétés ont été chargées.');
end;

procedure TMainFormGVPropLists.btnNameOfPropClick(Sender: TObject);
// test de NAMEOFPROP
var
  S: string;
begin
  if not fPropList.NameOfProp(edtName.Text, seNum.Value, S) then
    S := 'introuvable';
  mmoMain.Lines.Add('La propriété ' + IntToStr(seNum.Value) + ' de la liste ' +
    edtName.Text + ' est ' + S + '.');
end;

procedure TMainFormGVPropLists.btnNClick(Sender: TObject);
// test de [N]
begin
  mmoMain.Lines.Add(fPropList[seNum.Value]);
end;

procedure TMainFormGVPropLists.btnNumListPClick(Sender: TObject);
// test de NUMLISTP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' porte le numéro ' +
    IntToStr(fPropList.NumListP(edtName.Text)) + '.');
end;

procedure TMainFormGVPropLists.btnNumPropClick(Sender: TObject);
// test de NUMPROP
begin
  mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
    ' a pour numéro ' + IntToStr(fPropList.NumProp(edtName.Text,
    edtProp.Text)) + '.');
end;

procedure TMainFormGVPropLists.btnRemoveListPClick(Sender: TObject);
// test de REMOVELISTP
begin
  if fPropList.RemoveListP(edtName.Text) then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' a été supprimée.')
  else
    mmoMain.Lines.Add('Impossible de supprimer la liste ' + edtName.Text + '.');
end;

procedure TMainFormGVPropLists.btnRemovePropClick(Sender: TObject);
// test de REMOVEPROP
begin
  if fPropList.RemoveProp(edtName.Text, edtProp.Text) then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
      ' a été supprimée.')
  else
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
      ' est introuvable.');
end;

procedure TMainFormGVPropLists.btnSaveToFileClick(Sender: TObject);
// test de SAVETOFILE
begin
  fPropList.SaveToFile('ListPs');
  mmoMain.Lines.Add('Le fichier a été sauvegardé.');
end;

procedure TMainFormGVPropLists.btnUpdateListPClick(Sender: TObject);
begin
   if fPropList.UpDateListP(edtName.Text, edtProp.Text, edtValue.Text) then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' ayant pour valeur ' +
      edtValue.Text + ' a été ajoutée à ' + edtName.Text + '.')
  else
    mmoMain.Lines.Add('Une ou plusieurs des valeurs entrées sont erronées.');
end;

procedure TMainFormGVPropLists.btnValListPClick(Sender: TObject);
// test de VALLISP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' vaut ' +
    fPropList.ValListP(edtName.Text) + '.');
end;

procedure TMainFormGVPropLists.btnValNumListPClick(Sender: TObject);
// test de VALNUMLISTP
var
  Nme, Value: string;
begin
  if fPropList.ValNumListP(seNum.Value, Nme, Value) then
    mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' a pour nom ' +
      Nme + ' et valeurs ' + Value + '.')
  else
    mmoMain.Lines.Add('Impossible d''accéder à la liste ' +
      IntToStr(seNum.Value) + '.');
end;

procedure TMainFormGVPropLists.btnValNumPropClick(Sender: TObject);
// test de VALNUMPROP
var
  S: string;
begin
  if fPropList.ValNumProp(edtName.Text, seNum.Value, S) then
    mmoMain.Lines.Add('La valeur de la propriété ' + IntToStr(seNum.Value) +
      ' de ' + edtName.Text + ' est ' + S + '.')
  else
    mmoMain.Lines.Add('Impossible d''accéder à la propriété ' +
      IntToStr(seNum.Value) + ' de ' + edtName.Text + '.');
end;

procedure TMainFormGVPropLists.btnValPropClick(Sender: TObject);
// test de VALPROP
var
  S: string;
begin
  if fPropList.ValProp(edtName.Text, edtProp.Text, S) then
    mmoMain.Lines.Add('La valeur de la propriété ' + edtProp.Text +
      ' de la liste ' + edtName.Text + ' est ' + S + '.')
  else
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de la liste ' +
      edtName.Text + ' est introuvable.');
end;

procedure TMainFormGVPropLists.FormCreate(Sender: TObject);
// création de la fiche
begin
  fPropList := TGVPropList.Create;
end;

procedure TMainFormGVPropLists.FormDestroy(Sender: TObject);
// destruction de la liste
begin
  fPropList.Free;
end;

end.

