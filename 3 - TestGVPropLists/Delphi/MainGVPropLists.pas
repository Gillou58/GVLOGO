{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test des listes de propriétés           |
  |                  Unité : MainGVPropLists.pas                           |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    25-06-2014 21:39:46                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

unit MainGVPropLists;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  Vcl.Samples.Spin, GVConsts, GVPropLists;

type
  TMainForm = class(TForm)
    mmoMain: TMemo;
    statmain: TStatusBar;
    btnClear: TBitBtn;
    grpEdit: TGroupBox;
    lblName: TLabel;
    edtName: TEdit;
    lblProp: TLabel;
    edtProp: TEdit;
    lblValue: TLabel;
    edtValue: TEdit;
    seNum: TSpinEdit;
    lblNUm: TLabel;
    grpPropLists: TGroupBox;
    grpProps: TGroupBox;
    btnClearP: TButton;
    btnUpdateListP: TButton;
    btnRemoveListP: TButton;
    btnCountListP: TButton;
    btnIsListP: TButton;
    btnValListP: TButton;
    btnNumListP: TButton;
    btnValNumListP: TButton;
    btnListPByNum: TButton;
    btnIsListPByNum: TButton;
    btnLoadFromFile: TButton;
    btnSaveToFile: TButton;
    btnN: TButton;
    btnListP: TButton;
    btnEnumerator: TButton;
    btnIsProp: TButton;
    btnNumProp: TButton;
    btnValProp: TButton;
    btnRemoveProp: TButton;
    btnCountProps: TButton;
    btnValNumProp: TButton;
    btnListOfProps: TButton;
    btnNameOfProp: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearPClick(Sender: TObject);
    procedure btnUpdateListPClick(Sender: TObject);
    procedure btnCountListPClick(Sender: TObject);
    procedure btnListPClick(Sender: TObject);
    procedure btnRemoveListPClick(Sender: TObject);
    procedure btnValListPClick(Sender: TObject);
    procedure btnNumListPClick(Sender: TObject);
    procedure btnValNumListPClick(Sender: TObject);
    procedure btnListPByNumClick(Sender: TObject);
    procedure btnEnumeratorClick(Sender: TObject);
    procedure btnIsListPClick(Sender: TObject);
    procedure btnIsListPByNumClick(Sender: TObject);
    procedure btnNClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
    procedure btnIsPropClick(Sender: TObject);
    procedure btnNumPropClick(Sender: TObject);
    procedure btnValPropClick(Sender: TObject);
    procedure btnCountPropsClick(Sender: TObject);
    procedure btnValNumPropClick(Sender: TObject);
    procedure btnListOfPropsClick(Sender: TObject);
    procedure btnNameOfPropClick(Sender: TObject);
    procedure btnRemovePropClick(Sender: TObject);
  private
    { Déclarations privées }
    fPropList: TGVPropList;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  StrUtils;

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoMain.Lines.Clear;
end;

procedure TMainForm.btnClearPClick(Sender: TObject);
// test de CLEAR
begin
  fPropList.Clear;
  mmoMain.Lines.Add('Les listes de propriétés ont été remises à zéro.');
end;

procedure TMainForm.btnCountListPClick(Sender: TObject);
// test de COUNTLISTP
begin
  mmoMain.Lines.Add('Nombre de listes de propriétés : ' +
    IntToStr(fPropList.CountListP) + '.');
end;

procedure TMainForm.btnCountPropsClick(Sender: TObject);
// test de COUNTPROPS
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' comporte ' +
    IntToStr(fPropList.CountProps(edtName.Text)) + ' propriété(s).');
end;

procedure TMainForm.btnEnumeratorClick(Sender: TObject);
// test de ENUMERATOR
var
  S: string;
begin
  mmoMain.Lines.Add('Enumération :');
  for S in fPropList do
    mmoMain.Lines.Add(S);
  mmoMain.Lines.Add('Fin de l''énumération.');
end;

procedure TMainForm.btnIsListPByNumClick(Sender: TObject);
// test de ISLISTPBYNUM
begin
  mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' existe :');
  mmoMain.Lines.Add(IfThen(fPropList.IsListPByNum(seNum.Value), P_True,
    P_False));
end;

procedure TMainForm.btnIsListPClick(Sender: TObject);
// test de ISLISP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' existe :');
  mmoMain.Lines.Add(IfThen(fPropList.IsListP(edtName.Text), P_True, P_False));
end;

procedure TMainForm.btnIsPropClick(Sender: TObject);
// test de ISPROP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' a pour propriété ' +
    edtProp.Text + ':');
  mmoMain.Lines.Add(IfThen(fPropList.IsProp(edtName.Text, edtProp.Text), P_True,
    P_False));
end;

procedure TMainForm.btnListOfPropsClick(Sender: TObject);
// test de LISTOFPROPS
begin
  mmoMain.Lines.Add('Propriétés de ' + edtName.Text + ' : ' +
    fPropList.ListOfProps(edtName.Text));
end;

procedure TMainForm.btnListPByNumClick(Sender: TObject);
// test de LISTPBYNUM
begin
  mmoMain.Lines.Add(fPropList.ListPByNum[seNum.Value]);
end;

procedure TMainForm.btnListPClick(Sender: TObject);
// test de LISP
begin
  mmoMain.Lines.Add('Listes de propriétés enregistrées :');
  mmoMain.Lines.Add(fPropList.ListP);
end;

procedure TMainForm.btnLoadFromFileClick(Sender: TObject);
// test de LOADFROMFILE
begin
  fPropList.LoadFromFile('ListPs');
  mmoMain.Lines.Add('Les listes de propriétés ont été chargées.');
end;

procedure TMainForm.btnNameOfPropClick(Sender: TObject);
// test de NAMEOFPROP
var
  S: string;
begin
  if not fPropList.NameOfProp(edtName.Text, seNum.Value, S) then
    S := 'introuvable';
  mmoMain.Lines.Add('La propriété ' + IntToStr(seNum.Value) + ' de la liste ' +
    edtName.Text + ' est ' + S + '.');
end;

procedure TMainForm.btnNClick(Sender: TObject);
// test de [N]
begin
  mmoMain.Lines.Add(fPropList[seNum.Value]);
end;

procedure TMainForm.btnNumListPClick(Sender: TObject);
// test de NUMLISTP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' porte le numéro ' +
    IntToStr(fPropList.NumListP(edtName.Text)) + '.');
end;

procedure TMainForm.btnNumPropClick(Sender: TObject);
// test de NUMPROP
begin
  mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
    ' a pour numéro ' + IntToStr(fPropList.NumProp(edtName.Text,
    edtProp.Text)) + '.');
end;

procedure TMainForm.btnRemoveListPClick(Sender: TObject);
// ets de REMOVELISTP
begin
  if fPropList.RemoveListP(edtName.Text) then
    mmoMain.Lines.Add('La liste ' + edtName.Text + ' a été supprimée.')
  else
    mmoMain.Lines.Add('Impossible de supprimer la liste ' + edtName.Text + '.');
end;

procedure TMainForm.btnRemovePropClick(Sender: TObject);
// test de REMOVEPROP
begin
  if fPropList.RemoveProp(edtName.Text, edtProp.Text) then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
      ' a été supprimée.')
  else
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' de ' + edtName.Text +
      ' est introuvable.');
end;

procedure TMainForm.btnSaveToFileClick(Sender: TObject);
// test de SAVETOFILE
begin
  fPropList.SaveToFile('ListPs');
  mmoMain.Lines.Add('Le fichier a été sauvegardé.');
end;

procedure TMainForm.btnUpdateListPClick(Sender: TObject);
// test de UPDATELISTP
begin
  if fPropList.UpDateListP(edtName.Text, edtProp.Text, edtValue.Text) then
    mmoMain.Lines.Add('La propriété ' + edtProp.Text + ' ayant pour valeur ' +
      edtValue.Text + ' a été ajoutée à ' + edtName.Text + '.')
  else
    mmoMain.Lines.Add('Une ou plusieurs des valeurs entrées sont erronées.');
end;

procedure TMainForm.btnValListPClick(Sender: TObject);
// test de VALLISP
begin
  mmoMain.Lines.Add('La liste ' + edtName.Text + ' vaut ' +
    fPropList.ValListP(edtName.Text) + '.');
end;

procedure TMainForm.btnValNumListPClick(Sender: TObject);
// test de VALNUMLISTP
var
  Name, Value: string;
begin
  if fPropList.ValNumListP(seNum.Value, Name, Value) then
    mmoMain.Lines.Add('La liste ' + IntToStr(seNum.Value) + ' a pour nom ' +
      Name + ' et valeurs ' + Value + '.')
  else
    mmoMain.Lines.Add('Impossible d''accéder à la liste ' +
      IntToStr(seNum.Value) + '.');
end;

procedure TMainForm.btnValNumPropClick(Sender: TObject);
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

procedure TMainForm.btnValPropClick(Sender: TObject);
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

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  fPropList := TGVPropList.Create; // création des listes
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fPropList.Free; // libération des listes
end;

end.
