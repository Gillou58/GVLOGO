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
  Classes, SysUtils, FileUtil, SynMemo,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, StdCtrls,
  GVErrConsts; // constantes d'erreurs

type
  // *** TErrorForm ***
  TErrorForm = class(TForm)
    btnClose: TBitBtn;
    imgErrTurtle: TImage;
    lblMess: TLabel;
    SynMemoErr: TSynMemo;
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

uses
  GVLOGOConsts; // constantes du projet

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
  lblMess.Caption := Err; // message en toutes lettres
  with SynMemoErr.Lines do
  begin
    Clear; // on nettoie l'affichage
    Add(CrsErrItem + ' : ' + ErrItem);
    if (Line <> EmptyStr) then // une ligne pertinente ?
      Add(CrsErrLine + ' : ' + Line);
    if (Data <> EmptyStr) then // donnée ?
      Add(CrsErrData + ' : ' + Data);
    if (Prim <> EmptyStr) then // primitive ?
      Add(CrsErrPrim + ' : ' + Prim);
    if (Proc <> EmptyStr) then // procédure ?
      Add(CrsErrProc + ' : ' + Proc);
    if (Num > 0) then // numéro ?
      Add(CrsErrNum + ' : ' + IntToStr(Num));
    Add(CrsErrLevel + ' : ' + IntToStr(Level)); // niveau
    if Pos <> CE_NoErr then
      Add(CrsErrPos + ' : ' + IntToStr(Pos)); // position
  end;
end;

end.

