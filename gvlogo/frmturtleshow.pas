{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de l'état de la tortue            |
  |                  Unité : FrmTurtleShow.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMTURTLESHOW - part of GVLOGO
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

unit FrmTurtleShow;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, Spin, ColorBox,
  GVConsts; // constantes générales

type
  // *** TTurtleShowForm ***
  TTurtleShowForm = class(TForm)
    btnReInit: TBitBtn;
    btnClose: TBitBtn;
    btnCancel: TBitBtn;
    btnSave: TBitBtn;
    cbVisible: TCheckBox;
    cbRubber: TCheckBox;
    cbFilled: TCheckBox;
    cbPenDown: TCheckBox;
    cbgDraw: TCheckGroup;
    ColorBoxBckGround: TColorBox;
    ColorBoxPen: TColorBox;
    gbPen: TGroupBox;
    gbScale: TGroupBox;
    gbScreen: TGroupBox;
    gbSize: TGroupBox;
    gvSpeed: TGroupBox;
    Label1: TLabel;
    lblSize: TLabel;
    lblBckGrd: TLabel;
    lblScaleX: TLabel;
    Label3: TLabel;
    lblPenColor: TLabel;
    rbTriangle: TRadioButton;
    rbPng: TRadioButton;
    rgKind: TRadioGroup;
    gbCoords: TGroupBox;
    lblX: TLabel;
    lblY: TLabel;
    lblHeading: TLabel;
    pnlTurtleShow: TPanel;
    sbTurtleShow: TStatusBar;
    spedtWidth: TSpinEdit;
    spedtScaleX: TSpinEdit;
    spedtScaleY: TSpinEdit;
    spedtX: TSpinEdit;
    spedtHeading: TSpinEdit;
    spedtY: TSpinEdit;
    spedtSize: TSpinEdit;
    tbarSpeed: TTrackBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnReInitClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure rbTriangleChange(Sender: TObject);
  private
    fChanging: Boolean; // drapeau de changement
  public
    procedure GetTurtleState; // récupération de l'état de la tortue
    procedure SetTurtleState; // mise àjour des données
    property Changing: Boolean read fChanging write fChanging;
  end;

var
  TurtleShowForm: TTurtleShowForm;

implementation

uses
  FrmTurtle; // écran de la tortue

{$R *.lfm}

{ TTurtleShowForm }

procedure TTurtleShowForm.FormActivate(Sender: TObject);
// *** fenêtre visible ***
begin
  GetTurtleState; // récupération et affichage des données
end;

procedure TTurtleShowForm.rbTriangleChange(Sender: TObject);
// *** forme triangulaire de la tortue ***
begin
  gbSize.Enabled := rbTriangle.Checked; // activation de la tail
end;

procedure TTurtleShowForm.btnSaveClick(Sender: TObject);
// *** enregistrement des données ***
begin
  SetTurtleState; // mise à jour de la tortue
end;

procedure TTurtleShowForm.btnCancelClick(Sender: TObject);
// *** annulation des changements ***
begin
  GetTurtleState; // valeurs récupérées
end;

procedure TTurtleShowForm.btnReInitClick(Sender: TObject);
// *** réinitialisation de la tortue ***
begin
  TurtleForm.GVTurtle.ReInit; // tortue réinitialisée
  GetTurtleState; // valeurs récupérées
end;

procedure TTurtleShowForm.GetTurtleState;
// *** récupération de l'état de la tortue ***
begin
  if not Changing then // pas de mise à jour si changement en cours
    with TurtleForm.GVTurtle do // on sauvegarde la tortue en cours
    begin
      spedtX.Value := CoordX; // abscisse
      spedtY.Value := CoordY; // ordonnée
      rbPng.Checked := (Kind = tkPng); // type de tortue dessinée
      rbTriangle.Checked := (Kind = tkTriangle); // type de tortue triangulaire
      cbVisible.Checked := TurtleVisible; // drapeau de visibilité
      spedtHeading.Value := Heading; // direction
      cbPenDown.Checked := PenDown; // drapeau de crayon baissé
      cbRubber.Checked := PenRubber; // drapeau d'effacement
      spedtScaleX.Value := ScaleX; // échelle des X
      spedtScaleY.Value := ScaleY; // échelle des Y
      cbFilled.Checked := Filled; // remplissage
      spedtWidth.Value := PenWidth; // largeur du crayon
      ColorBoxPen.Selected := PenColor; // couleur du crayon
      ColorBoxBckGround.Selected := ScreenColor; // couleur de fond
      tbarSpeed.Position := Speed; // vitesse de la tortue
      spedtSize.Value := Size; // taille de la tortue triangulaire
    end;
end;

procedure TTurtleShowForm.SetTurtleState;
// *** mise à jour des données ***
begin
  Changing := True; // on indique le changement
  with TurtleForm.GVTurtle do // on sauvegarde la tortue en cours
  begin
    CoordX := spedtX.Value; // abscisse
    CoordY := spedtY.Value; // ordonnée
    if rbPng.Checked then
      Kind := tkPng // type de tortue dessinée
    else
      Kind := tkTriangle; // type de tortue triangulaire
    TurtleVisible := cbVisible.Checked; // drapeau de visibilité
    Heading := spedtHeading.Value; // direction
    PenDown := cbPenDown.Checked; // drapeau de crayon baissé
    PenRubber := cbRubber.Checked; // drapeau d'effacement
    ScaleX := spedtScaleX.Value; // échelle des X
    ScaleY := spedtScaleY.Value; // échelle des Y
    Filled := cbFilled.Checked ; // remplissage
    PenWidth := spedtWidth.Value; // largeur du crayon
    PenColor := ColorBoxPen.Selected; // couleur du crayon
    ScreenColor := ColorBoxBckGround.Selected; // couleur de fond
    Speed := tbarSpeed.Position; // vitesse de la tortue
    Size := spedtSize.Value; // taille de la tortue triangulaire
  end;
  Changing := False; // on a fini
end;

end.

