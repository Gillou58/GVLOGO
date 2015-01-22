{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche d'erreur de GVLOGO                |
  |                  Unité : frmError.pas                                  |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMERROR - part of GVLOGO
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
unit FrmError;

interface

uses
  Classes, SysUtils, FileUtil, BCButton, BCLabel, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, GVErrConsts; // constantes d'erreurs

type

  { TErrorForm }

  TErrorForm = class(TForm)
    btnClose: TBCButton;
    blblErrItem: TBCLabel;
    blblLine: TBCLabel;
    blblData: TBCLabel;
    blblPrim: TBCLabel;
    blblProc: TBCLabel;
    blblNum: TBCLabel;
    blblLevel: TBCLabel;
    blblPos: TBCLabel;
    blblMess: TBCLabel;
    imgErrTurtle: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure btnCloseClick(Sender: TObject);
  private
  public
    procedure SetError(const Err, ErrItem, Line, Data, Prim, Proc: string;
      Num, Level: Integer; Pos: Integer = CE_NoErr);
  end;

var
  ErrorForm: TErrorForm;

implementation

{$R *.lfm}

{ TErrorForm }

procedure TErrorForm.btnCloseClick(Sender: TObject);
// *** fermeture de la fiche ***
begin
  Close; // on ferme la fiche
end;

procedure TErrorForm.SetError(const Err, ErrItem, Line, Data, Prim,
  Proc: string; Num, Level: Integer; Pos: Integer);
// *** affichage de l'erreur ***
begin
  blblMess.Caption := Err; // message en toutes lettres
  blblErrItem.Caption := blblErrItem.Caption + ErrItem;
  blblLine.Caption := Line;
  blblData.Caption := Data;
  blblPrim.Caption := Prim;
  blblProc.Caption := Proc;
  blblNum.Caption := IntToStr(Num);
  blblLevel.Caption := IntToStr(Level);
  if Pos <> CE_NoErr then
    blblPos.Caption := IntToStr(Pos)
  else
    blblPos.Caption := EmptyStr;
end;

end.

