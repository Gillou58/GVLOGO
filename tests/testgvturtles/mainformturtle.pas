{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Tortue graphique                        |
  |                  Unité : TestGVTurtles.pas                             |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    08-08-2014 12:29:48                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// TestGVTurtles - part of GVLOGO
// Copyright (C) 2014 Gilles VASSEUR
//
// This program is free software: you can redistribute it and/or modify it under the terms of
// the GNU General Public License as published by the Free Software Foundation,
// either version 3 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
// without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
//  If not, see <http://www.gnu.org/licenses/>.

unit MainFormTurtle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Spin, ColorBox, Buttons, GVConsts, GVTurtles;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnClear: TBitBtn;
    btnExit: TBitBtn;
    btnSetPos: TButton;
    btnMove: TButton;
    btnScaleX: TButton;
    btnScaleY: TButton;
    btnTurn: TButton;
    btnTowards: TButton;
    btnHeading: TButton;
    btnSize: TButton;
    btnReInit: TButton;
    btnWipe: TButton;
    btnHome: TButton;
    btnTurtleVisible: TButton;
    btnPenDown: TButton;
    btnScreenColor: TButton;
    btnPenColor: TButton;
    btnPenInverse: TButton;
    btnRubber: TButton;
    btnSaveTurtle: TButton;
    btnReloadTurtle: TButton;
    btnRectangle: TButton;
    btnCircle2: TButton;
    btnArc: TButton;
    btnArc2: TButton;
    btnPie: TButton;
    btnPie2: TButton;
    btnChord: TButton;
    btnChord2: TButton;
    btnFilled: TButton;
    btnRectangle2: TButton;
    btnDessin: TButton;
    btnSquare: TButton;
    btnSquare2: TButton;
    btnRoundRect: TButton;
    btnRoundRect2: TButton;
    btnEllipse: TButton;
    btnEllipse2: TButton;
    btnCircle: TButton;
    cbScreenColor: TColorBox;
    cbPenColor: TColorBox;
    gbShapes: TGroupBox;
    Kind: TButton;
    GroupBox1: TGroupBox;
    iTurtle: TImageList;
    pnlTurtle: TPanel;
    seSetPosX: TSpinEdit;
    seSetPosY: TSpinEdit;
    seMove: TSpinEdit;
    seScaleX: TSpinEdit;
    seScaleY: TSpinEdit;
    seTurn: TSpinEdit;
    seTowardsX: TSpinEdit;
    seTowardsY: TSpinEdit;
    seHeading: TSpinEdit;
    seSize: TSpinEdit;
    seX1: TSpinEdit;
    seY1: TSpinEdit;
    seX2: TSpinEdit;
    seY2: TSpinEdit;
    seX3: TSpinEdit;
    seY3: TSpinEdit;
    seX4: TSpinEdit;
    seY4: TSpinEdit;
    seLen: TSpinEdit;
    StatusBar: TStatusBar;
    mmoTurtle: TSynMemo;
    tbExample: TTrackBar;
    procedure btnClearClick(Sender: TObject);
    procedure btnDessinClick(Sender: TObject);
    procedure btnFilledClick(Sender: TObject);
    procedure btnHeadingClick(Sender: TObject);
    procedure btnHomeClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btnPenColorClick(Sender: TObject);
    procedure btnPenDownClick(Sender: TObject);
    procedure btnPenInverseClick(Sender: TObject);
    procedure btnRectangleClick(Sender: TObject);
    procedure btnReInitClick(Sender: TObject);
    procedure btnReloadTurtleClick(Sender: TObject);
    procedure btnRubberClick(Sender: TObject);
    procedure btnSaveTurtleClick(Sender: TObject);
    procedure btnScaleXClick(Sender: TObject);
    procedure btnScaleYClick(Sender: TObject);
    procedure btnScreenColorClick(Sender: TObject);
    procedure btnSetPosClick(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure btnTowardsClick(Sender: TObject);
    procedure btnTurnClick(Sender: TObject);
    procedure btnTurtleVisibleClick(Sender: TObject);
    procedure btnWipeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure KindClick(Sender: TObject);
    procedure tbExampleChange(Sender: TObject);
  private
    { private declarations }
    GVTurtle: TGVTurtle;
  public
    { public declarations }
     // changement de fond
    procedure BkgChanged(Obj: TObject; BckColor: TColor);
    // message de la tortue
    procedure TurtleState(Sender: TObject; cX, cY, cHeading: Integer;
      cVisible, cDown: Boolean; cColor: TColor);
    // pour peindre la tortue
    procedure TurtleBeforePaint(Sender: TObject; cHeading: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  StrUtils;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // on crée la tortue
  GVTurtle := TGVTurtle.Create(pnlTurtle);
  GVTurtle.Parent := pnlTurtle;  // dans le panel
  GVTurtle.OnChange := @TurtleState;  // gestionnaire de changement
  GVTurtle.OnBeforeChange := @TurtleBeforePaint; // idem avant de dessiner
  // on initialise le fond d'écran (gestionnaire de fond inutile avec Lazarus !)
  GVTurtle.OnBackGroundChange := @BkgChanged;
  // on recherche le centre pour les éditeurs
  seSetPosX.Value := GVTurtle.Width shr 1;
  seSetPosY.Value := GVTurtle.Height shr 1;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  GVTurtle.Free;
end;

procedure TMainForm.KindClick(Sender: TObject);
// test de KIND
var
  S: string;
begin
  with GVTurtle do
    if Kind = tkTriangle then
    begin
      Kind := tkPng;
      S := 'Image PNG';
    end
    else
    begin
      Kind := tkTriangle;
      S := 'Triangle';
    end;
  mmoTurtle.Lines.Add('KIND - ' + S);
end;

procedure TMainForm.tbExampleChange(Sender: TObject);
// changement de vitesse de la tortue
begin
  GVTurtle.Speed := tbExample.Position;
  mmoTurtle.Lines.Add('SPEED - ' + IntToStr(GVTurtle.Speed));
end;

procedure TMainForm.btnSetPosClick(Sender: TObject);
// test de SETPOS
begin
  GVTurtle.SetPos(seSetPosX.Value, seSetPosY.Value);
  mmoTurtle.Lines.Add(Format('SETPOS - X: %d - Y: %d', [seSetPosX.Value,
    seSetPosY.Value]));
end;

procedure TMainForm.btnSizeClick(Sender: TObject);
// test de SIZE
begin
  GVTurtle.Size := seSize.Value;
  mmoTurtle.Lines.Add(Format('SIZE %d', [seSize.Value]));
end;

procedure TMainForm.btnTowardsClick(Sender: TObject);
// test de TOWARDS
begin
  mmoTurtle.Lines.Add(Format('TOWARDS - X: %d - Y: %d - Résultat: %d',
    [seTowardsX.Value, seTowardsY.Value,
    Round(GVTurtle.Towards(seTowardsX.Value, seTowardsY.Value))]));
end;

procedure TMainForm.btnTurnClick(Sender: TObject);
// test de TURN
begin
  GVTurtle.Turn(seTurn.Value);
  mmoTurtle.Lines.Add(Format('TURN - valeur en degrés: %d', [seTurn.Value]));
end;

procedure TMainForm.btnTurtleVisibleClick(Sender: TObject);
// test de TURTLEVISIBLE
begin
  with GVTurtle do
    TurtleVisible := not TurtleVisible;
  mmoTurtle.Lines.Add('TURTLEVISIBLE - ' + IfThen(GVTurtle.TurtleVisible,
    P_True, P_False));
end;

procedure TMainForm.btnWipeClick(Sender: TObject);
// test de WIPE
begin
  GVTurtle.Wipe;
  mmoTurtle.Lines.Add('WIPE');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  gvTurtle.Square(50);
end;

procedure TMainForm.btnMoveClick(Sender: TObject);
// test de MOVE
begin
  GVTurtle.Move(seMove.Value);
  mmoTurtle.Lines.Add(Format('MOVE - valeur en points: %d', [seMove.Value]));
end;

procedure TMainForm.btnPenColorClick(Sender: TObject);
// test de PENCOLOR
begin
  GVTurtle.PenColor := cbPenColor.Selected;;
  mmoTurtle.Lines.Add('PENCOLOR ');
end;

procedure TMainForm.btnPenDownClick(Sender: TObject);
// test de PENDOWN
begin
  with GVTurtle do
    PenDown := not PenDown;
  mmoTurtle.Lines.Add('PENDOWN - ' + IfThen(GVTurtle.PenDown, P_True, P_False));
end;

procedure TMainForm.btnPenInverseClick(Sender: TObject);
// test de PENINVERSE
begin
  with GVTurtle do
    PenReverse := not PenReverse;
  mmoTurtle.Lines.Add('PENREVERSE - ' + IfThen(GVTurtle.PenReverse, P_True,
    P_False));
end;

procedure TMainForm.btnRectangleClick(Sender: TObject);
// dessin de formes
begin
  with GVTurtle do
  case (Sender as TButton).Tag of
    1: Rectangle(seX1.Value,seY1.Value,seX2.Value,seY2.Value);
    2: Rectangle(seX2.Value,seY2.Value);
    3: Square(seX1.Value,seY1.Value,seLen.Value);
    4: Square(seLen.Value);
    5: RoundRect(seX1.Value,seY1.Value,seX2.Value,seY2.Value);
    6: RoundRect(seX2.Value,seY2.Value);
    7: Ellipse(seX1.Value,seY1.Value,seX2.Value,seY2.Value);
    8: Ellipse(seX2.Value,seY2.Value);
    9: Circle(seX1.Value,seY1.Value,seLen.Value);
    10: Circle(seLen.Value);
    11: Arc(seX1.Value,seY1.Value,seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
    12: Arc(seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
    13: Pie(seX1.Value,seY1.Value,seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
    14: Pie(seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
    15: Chord(seX1.Value,seY1.Value,seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
    16: Chord(seX2.Value,seY2.Value,seX3.Value,seY3.Value,seX4.Value,seY4.Value);
  end;
  mmoTurtle.Lines.Add('FORME DESSINEE');
end;

procedure TMainForm.btnReInitClick(Sender: TObject);
// test de REINIT
begin
  GVTurtle.ReInit;
  mmoTurtle.Lines.Add('REINIT');
end;

procedure TMainForm.btnReloadTurtleClick(Sender: TObject);
// test de RELOADTURTLE
begin
  GVTurtle.ReloadTurtle(False);
  mmoTurtle.Lines.Add('RELOADTURTLE');
end;

procedure TMainForm.btnRubberClick(Sender: TObject);
// test de PENRUBBER
begin
  with GVTurtle do
    PenRubber := not PenRubber;
  mmoTurtle.Lines.Add('PENRUBBER - ' + IfThen(GVTurtle.PenRubber, P_True,
    P_False));
end;

procedure TMainForm.btnSaveTurtleClick(Sender: TObject);
// test de SAVETURTLE
begin
  GVTurtle.SaveTurtle;
  mmoTurtle.Lines.Add('SAVETURTLE');
end;

procedure TMainForm.btnHeadingClick(Sender: TObject);
// test de HEADING
begin
  GVTurtle.Heading := seHeading.Value;
  mmoTurtle.Lines.Add(Format('HEADING - valeur en degrés: %d',
    [seHeading.Value]));
end;

procedure TMainForm.btnDessinClick(Sender: TObject);
// test d'une suite d'ordres pour GVTurtle
var
  I: Integer;

  procedure Wait;
  // permet le dessin pas à pas
  begin
    Application.ProcessMessages;
  end;

begin
  with GVTurtle do
    for I := 1 to 36 do
    begin
      PenColor := RGBToColor(random(255),random(255),random(255));
      Move(80);
      Wait;
      Turn(90);
      Wait;
      Move(80);
      Wait;
      Turn(90);
      Wait;
      Move(80);
      Wait;
      Turn(90);
      Wait;
      Move(80);
      Wait;
      Turn(90);
      Wait;
      Turn(10);
      Wait;
    end;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoTurtle.Clear;
end;

procedure TMainForm.btnFilledClick(Sender: TObject);
// test de FILLED
begin
  with GVTurtle do
    Filled := not Filled;
  mmoTurtle.Lines.Add('FILLED - ' + IfThen(GVTurtle.Filled, P_True,
    P_False));
end;

procedure TMainForm.btnHomeClick(Sender: TObject);
// test de HOME
begin
  GVTurtle.Home;
  mmoTurtle.Lines.Add('HOME');
end;

procedure TMainForm.btnScaleXClick(Sender: TObject);
// test de SCALEX
begin
  GVTurtle.ScaleX := seScaleX.Value;
  mmoTurtle.Lines.Add(Format('SCALEX - %d', [GVTurtle.ScaleX]));
end;

procedure TMainForm.btnScaleYClick(Sender: TObject);
// test de SCALEY
begin
  GVTurtle.ScaleY := seScaleY.Value;
  mmoTurtle.Lines.Add(Format('SCALEY - %d', [GVTurtle.ScaleY]));
end;

procedure TMainForm.btnScreenColorClick(Sender: TObject);
// test de SCREENCOLOR
begin
  GVTurtle.ScreenColor := cbScreenColor.Selected;
  mmoTurtle.Lines.Add('SCREENCOLOR - ');
end;

procedure TMainForm.BkgChanged(Obj: TObject; BckColor: TColor);
// changement de fond
begin
  pnlTurtle.Color := BckColor;
end;

procedure TMainForm.TurtleState(Sender: TObject; cX, cY, cHeading: Integer;
  cVisible, cDown: Boolean; cColor: TColor);
// état de la tortue
begin
  // données de la tortue
  statusbar.Panels[1].Text := Format('X: %.3d Y: %.3d Cap: %.3d', [cX, cY,
    cHeading]) + ' Visible: ' + IfThen(cVisible, P_True, P_False) +
    ' Baissé: ' + IfThen(cDown, P_True, P_False);
end;

procedure TMainForm.TurtleBeforePaint(Sender: TObject; cHeading: Integer);
// image associée à la tortue
var
  BitM: TBitmap;
begin
  // charge l'image de la tortue
  BitM := TBitmap.Create;
  try
    // les images de la tortue sont proposées tous les 5 degrés
    iTurtle.GetBitmap(Round(GVTurtle.Heading) div 5, BitM);
    // celle qui correspond est assignée au bitmap
    GVTurtle.TurtleImg.Assign(BitM);
  finally
    BitM.Free;
  end;
end;

end.

