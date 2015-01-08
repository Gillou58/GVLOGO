{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test des varaibles locales              |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVLOCVARS - part of GVLOGO
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

unit main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, Spin, ComCtrls,
  GVConsts, // constantes
  GVErrConsts, // constantes des erreurs
  GVPrimConsts, // constantes primitives
  GVLocVars; // noyau

const
  P_True = 'vrai';
  P_False = 'faux';

type

  { TMainForm }

  TMainForm = class(TForm)
    btnClear: TBitBtn;
    btnClose: TBitBtn;
    btnAddLocNumber: TButton;
    btnIsLocVar: TButton;
    btnLocVarsCount: TButton;
    btnAddLocVar: TButton;
    btnUpdateLocVar: TButton;
    btnValLocVar: TButton;
    btnDelLastGroup: TButton;
    btnRemoveAllLocVars: TButton;
    btnLocVarsToList: TButton;
    edtName: TLabeledEdit;
    edtValue: TLabeledEdit;
    gbMethods: TGroupBox;
    lblNumber: TLabel;
    mmoGVKer: TMemo;
    seNumber: TSpinEdit;
    sbMain: TStatusBar;
    procedure btnAddLocNumberClick(Sender: TObject);
    procedure btnAddLocVarClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnDelLastGroupClick(Sender: TObject);
    procedure btnIsLocVarClick(Sender: TObject);
    procedure btnLocVarsCountClick(Sender: TObject);
    procedure btnLocVarsToListClick(Sender: TObject);
    procedure btnRemoveAllLocVarsClick(Sender: TObject);
    procedure btnUpdateLocVarClick(Sender: TObject);
    procedure btnValLocVarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    GVLocVr: TGVLocVars;
  public
    // recherche des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
  end;

var
  MainForm: TMainForm;

implementation

uses
  StrUtils;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  GVLocVr := TGVLocVars.Create;
  // gestionnaire de recherche d'erreur actif
  GVLocVr.Error.OnError := @GetError;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoGVKer.Lines.Clear;
end;

procedure TMainForm.btnDelLastGroupClick(Sender: TObject);
// test de DelLastGroup
begin
  GVLocVr.DelLastGroup;
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add(
      'Le dernier groupe de variables locales a été supprimé.')
  else
    GVLocVr.Error.Ok:= True; // on annule l'erreur
end;

procedure TMainForm.btnIsLocVarClick(Sender: TObject);
// test de IsLocVar
var
  LS: string;
begin
  if GVLocVr.IsLocVar(edtName.Text) then
    LS := P_True
  else
    LS := P_False;
  if GVLocVr.Error.Ok then
      mmoGVKer.Lines.Add(Format('"%s" est une variable locale : %s.',
        [edtName.Text, LS]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnLocVarsCountClick(Sender: TObject);
// test de LocVarsCount
var
  Li: Integer;
begin
  Li := GVLocVr.LocVarsCount;
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add(Format('Il y a %d variable%s enregistrée%s.',
      [Li, IfThen(Li > 1, 's', ''), IfThen(Li > 1, 's', '')]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnLocVarsToListClick(Sender: TObject);
// test de LocVarsToList
var
  Ls: string;
begin
  LS := GVLocVr.LocVarsToList;
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add('Liste des variables locales enregistrées : '+ LS)
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnRemoveAllLocVarsClick(Sender: TObject);
// test de Clear
begin
  GVLocVr.Clear;
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add('Toutes les variables locales ont été supprimées.')
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnUpdateLocVarClick(Sender: TObject);
// test de UpdateLocVar
begin
  if GVLocVr.UpdateLocVar(edtName.Text, edtValue.Text) then
    mmoGVKer.Lines.Add(Format(
          'La variable "%s" a maintenant pour valeur %s".',
            [edtName.Text, edtValue.Text]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnValLocVarClick(Sender: TObject);
// test de ValLocVar
var
  LS: string;
begin
  LS := GVLocVr.ValLocVar(edtName.Text);
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add(Format('La variable "%s" a pour valeur : %s.',
      [edtName.Text, LS]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnAddLocNumberClick(Sender: TObject);
// test de AddLocNumber
begin
  GVLocVr.AddLocNumber(seNumber.Value);
  if GVLocVr.Error.Ok then
    mmoGVKer.Lines.Add(Format('De la place a été réservée pour %d variable%s.',
      [seNumber.Value, IfThen(seNumber.Value > 1, 's', '')]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.btnAddLocVarClick(Sender: TObject);
// test de AddLocVar
begin
  if GVLocVr.AddLocVar(edtName.Text, edtValue.Text) then
    mmoGVKer.Lines.Add(Format(
      'La variable "%s" ayant pour valeur %s" a été ajoutée.',
        [edtName.Text, edtValue.Text]))
  else
    GVLocVr.Error.Clear; // on annule l'erreur
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  GVLocVr.Error := nil;
  GVLocVr.Free;
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// gestion des erreurs
begin
  with mmoGVKer.Lines, ErrorRec do
  begin
    Add(CComment + CBlank + '******************');
    // message en toutes lettres
    Add(CComment + CBlank + '>>> ' + GVLocVr.Error.ErrorMessage + ' <<< ');
    // code de l'erreur
    Add(CComment + CBlank + 'Code : ' + IntToStr(Ord(Code)));
    // élément fautif dans la ligne de travail
    Add(CComment + CBlank + 'Elément : ' + ErrItem);
    if ErrPos <> CE_NoErr then // position pertinente ?
      // position de l'erreur
      Add(CComment + CBlank + 'Position : ' + IntToStr(ErrPos));
    Add(CComment + CBlank + '******************');
  end;
end;

end.

