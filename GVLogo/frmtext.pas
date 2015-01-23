{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche du texte de GVLOGO                |
  |                  Unité : frmText.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMTEXT - part of GVLOGO
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

unit FrmText;

interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls;

type

  { TTextForm }

  TTextForm = class(TForm)
    rmmoText: TRichMemo;
    sbText: TStatusBar;
    procedure FormDeactivate(Sender: TObject);
  private
  public
  end;

var
  TextForm: TTextForm;

implementation

{$R *.lfm}

{ TTextForm }

procedure TTextForm.FormDeactivate(Sender: TObject);
// *** la fenêtre est désactivée ***
begin
  sbText.SimpleText := EmptyStr; // barre nettoyée
end;

end.

