{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de la tortue graphique de GVLOGO  |
  |                  Unité : frmTurtle.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    05-03-2015 18:00:00                          |
  |                  Version : 1.0.2                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle
// 04/03/2015 - 1.0.1 - ajout de barres de défilement
// 05/03/2015 - 1.0.2 - ajout d'un menu surgissant

// FRMTURTLE - part of GVLOGO
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

unit FrmTurtle;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus,
  GVTurtles, types; // unité de la tortue

type
  // *** TTurtleForm ***
  TTurtleForm = class(TForm)
    ilTurtle: TImageList;
    imgTurtle: TImage;
    MenuItemTurtleState: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItemClear: TMenuItem;
    PopupMenuTurtle: TPopupMenu;
    sbTurtle: TStatusBar;
    ScrollBox: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgTurtleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
    procedure MenuItemClearClick(Sender: TObject); // # 1.0.2
  private
    // gestionnaire de tortue
    procedure TurtleState(Sender: TObject);
    // dessin de la tortue
    procedure TurtleBeforePaint(Sender: TObject; cHeading: Integer);
  public
    GVTurtle: TGVTurtle;
  end;

var
  TurtleForm: TTurtleForm;

implementation

{$R *.lfm}
uses
  StrUtils, // utilitaires pour les chaînes de caractères
  GVConsts, // constantes générales
  GVPrimConsts, // constantes pour VRAI et FAUX
  GVLOGOCOnsts, // constantes du projet
  FrmTurtleShow, // affichage des donneés de la tortue
  Main; // fiche principale

{ TTurtleForm }

procedure TTurtleForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  // création de la tortue aux dimensions de l'image d'accueil
  GVTurtle := TGVTurtle.Create(imgTurtle.Width, imgTurtle.Height);
  GVTurtle.OnChange := @TurtleState;  // gestionnaire de changement
  GVTurtle.OnBeforeChange := @TurtleBeforePaint; // idem avant de dessiner
  GVTurtle.Error.OnError := @MainForm.GetError; // gestionnaire d'erreurs
  GVTurtle.ReInit; // initialisation
  GVTurtle.Kind := tkPng; // tortue image
end;

procedure TTurtleForm.FormDeactivate(Sender: TObject);
// *** désactivation de la fiche ***
begin
  sbTurtle.Panels[0].Text := CrsTurtleForm; // barre de statut à jour
end;

procedure TTurtleForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  GVTurtle.Free; // tortue libérée
end;

procedure TTurtleForm.imgTurtleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then  // touche Ctrl appuyée ?
  begin
     // déplacement horizontal
     if WheelDelta < 0 then
       Scrollbox.HorzScrollBar.Position :=
         Scrollbox.HorzScrollBar.Position + Scrollbox.HorzScrollBar.Increment
     else
       Scrollbox.HorzScrollBar.Position :=
         Scrollbox.HorzScrollBar.Position - Scrollbox.HorzScrollBar.Increment;
  end
  else
  // déplacement vertical
  if WheelDelta < 0 then
   Scrollbox.VertScrollBar.Position :=
     Scrollbox.VertScrollBar.Position + Scrollbox.VertScrollBar.Increment
  else
   Scrollbox.VertScrollBar.Position :=
     Scrollbox.VertScrollBar.Position - Scrollbox.VertScrollBar.Increment;
end;

procedure TTurtleForm.MenuItemClearClick(Sender: TObject);
// *** tortue réinitialisée *** # 1.0.2
begin
  GVTurtle.ReInit;
end;

procedure TTurtleForm.TurtleState(Sender: TObject);
// *** état de la tortue ***
begin
  GVTurtle.TurtleBitmap.Draw(imgTurtle.Canvas,0,0);
  imgTurtle.Invalidate;
  // données de la tortue
  with GVTurtle do
    sbTurtle.Panels[1].Text := Format('X: %.3d Y: %.3d Cap: %.3d',
      [Round(CoordX), Round(CoordY), Round(Heading)]) +
      ' Visible: ' + IfThen(TurtleVisible, MF_True, MF_False) +
      ' Baissé: ' + IfThen(PenDown, MF_True, MF_False);
  // état de la tortue affiché ?
  if Assigned(TurtleShowForm) and not TurtleShowForm.Changing then
    TurtleShowForm.GetTurtleState; // on le réactualise
end;

procedure TTurtleForm.TurtleBeforePaint(Sender: TObject; cHeading: Integer);
// *** image associée à la tortue ***
var
  BitM: TBitmap;
begin
  // charge l'image de la tortue
  BitM := TBitmap.Create;
  try
    // les images de la tortue sont proposées tous les 5 degrés
    ilTurtle.GetBitmap(Round(cHeading) div 5, BitM);
    // celle qui correspond est assignée au bitmap
    GVTurtle.PNGTurtle.Assign(BitM);
  finally
    BitM.Free;
  end;
end;

end.

