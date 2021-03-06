{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fenêtre d'information                   |
  |                  Unité : FrmInfo.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMINFO - part of GVLOGO
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

unit FrmInfo;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  // *** TInfoForm ***
  TInfoForm = class(TForm)
    btnQuit: TBitBtn;
    btnNo: TBitBtn;
    btnYes: TBitBtn;
    imgInfo: TImage;
    mmoInfo: TMemo;
  private
    fMessage: string;
    procedure SetMessage(const AValue: string);
  public
    // message à afficher
    property Mess: string read fMessage write SetMessage;
  end;

  function ShowInfoForm(const St: string): TModalResult; // information
  function ShowConfirmForm(const St: string): TModalResult; // confirmation

implementation

uses
  GVLogoConsts; // constantes GVLOGO

function ShowInfoForm(const St: string): TModalResult;
// *** affichage d'une information ***
var
  LGVInfoForm: TInfoForm;
begin
  LGVInfoForm := TInfoForm.Create(nil); // fiche créée
  try
    LGVInfoForm.Caption := CrsInfo; // titre de la fenêtre
    LGVInfoForm.btnYes.Visible := False; // boutons superflus désactivés
    LGVInfoForm.btnNo.Visible := False;
    LGVInfoForm.btnQuit.Kind := bkClose; // bouton de fermeture
    LGVInfoForm.btnQuit.Hint := CrsCloseHint;
    LGVInfoForm.btnQuit.Caption := CrsClose; // bouton marqué
    LGVInfoForm.Mess := St; // message affecté
    Result := LGVInfoForm.ShowModal; // fiche affichée
  finally
    LGVInfoForm.Free; // fiche libérée
  end;
end;

function ShowConfirmForm(const St: string): TModalResult;
// *** affichage d'une confirmation ***
var
  LGVConfirmForm: TInfoForm;
begin
  LGVConfirmForm := TInfoForm.Create(nil); // fiche créée
  try
    LGVConfirmForm.Caption := CrsConfirm; // titre de la fenêtre
    LGVConfirmForm.btnNo.Visible := True; // boutons visibles
    LGVConfirmForm.btnYes.Visible := True;
    LGVConfirmForm.Mess := St; // message affecté
    Result := LGVConfirmForm.ShowModal; // fiche affichée
  finally
    LGVConfirmForm.Free; // fiche libérée
  end;
end;


{$R *.lfm}

{ TInfoForm }

procedure TInfoForm.SetMessage(const AValue: string);
// *** changement de message ***
begin
  if fMessage = AValue then // pas de changement ?
    Exit; // on sort
  fMessage := AValue;
  mmoInfo.Lines.Add(fMessage); // mise à jour du mémo
end;

end.

