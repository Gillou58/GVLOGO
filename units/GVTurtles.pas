{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Gestion de la tortue graphique          |
  |                  Unité : GVTurtles.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVTurtles - part of GVLOGO
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

unit GVTurtles;

//
// Unité de la tortue graphique de GVLOGO
//
// La tortue graphique permet de dessiner sur une surface
// en fonction d'ordres simples.
//

interface

uses
  Classes, SysUtils, Graphics,
  BGRABitmap, BGRABitmapTypes, // bibliothèque graphique
  GVConsts;

type
  // changement de la tortue
  TTurtleEvent = TNotifyEvent;
  // avant le changement de la tortue
  TTurtleBeforeEvent = procedure(Sender: TObject; cHeading: Integer) of object;

  // *** définition d'une tortue ***
  TTurtle = record
    rSaved: Boolean; // drapeau de sauvegarde
    rX: Extended; // abscisse
    rY: Extended; // ordonnée
    rKind: TTurtleKind; // type de tortue
    rSize: Integer; // taille de la tortue
    rVisible: Boolean; // drapeau de visibilité
    rHeading: Extended; // direction
    rPenDown: Boolean; // drapeau de crayon baissé
    rPenRubber: Boolean; // drapeau d'effacement
    rScaleX: Integer; // échelle des X
    rScaleY: Integer; // échelle des Y
    rFilled: Boolean; // remplissage
    rPenWidth: Integer; // largeur de crayon
    rBrush: TBrush; // type de brosse
    rPen: TPen; // type de crayon
    rFont: TFont; // type de fonte
  end;

  // *** TGVTurtle - la tortue ***

  { TGVTurtle }

  TGVTurtle = class(TObject)
    strict private
      fHeight: Integer; // hauteur de la surface d'origine
      fPNGTurtle: TBGRABitmap; // tortue PNG en cours
      fWidth: Integer; // largeur de la surface d'origine
      BckImg: TBGRABitmap; // fond
      DrwImg: TBGRABitmap; // surface dessinée
      TtlImg: TBGRABitmap; // dessin de la tortue
      ActualImg: TBGRABitmap; // surface réelle
      fX: Double; // abscisse de la tortue
      fY: Double; // ordonnée de la tortue
      fOnchange: TTurtleEvent; // changement notifié
      fScreen: TScreenTurtle; // mode de l'écran
      fTurtleKind: TTurtleKind; // type de tortue
      fTurtleVisible: Boolean; // visibilité de la tortue
      fHeading: Double; // cap de la tortue
      fSize: Integer; // taille de la tortue
      fPenDown: Boolean; // crayon levé/baissé
      fScaleX: Integer; // échelle des X
      fScaleY: Integer; // échelle des Y
      fSavedTurtle: TTurtle; // sauvegarde d'une tortue
      fScreenColor: TColor; // couleur de l'écran
      fFilled: Boolean; // drapeau de remplissage
      fTempColor: TColor; // sauvegarde temporaire de la couleur d'écriture
      fPenRubber: Boolean; // gomme du crayon
      fOnBeforeChange: TTurtleBeforeEvent; // notification avant cap changé
      fPenColor: TColor; // couleur du crayon
      fPenWidth: Integer; // largeur du crayon
      fSpeed: Integer; // vitesse de la tortue
      function GetCoordX: Double;
      function GetCoordY: Double;
      function GetImg: TBGRABitmap;
      procedure SetCoordX(AValue: Double);
      procedure SetCoordY(AValue: Double);
      procedure SetFilled(AValue: Boolean);
      procedure SetHeading(AValue: Double);
      procedure SetPenColor(AValue: TColor);
      procedure SetPenDown(AValue: Boolean);
      procedure SetPenWidth(AValue: Integer);
      procedure SetScreen(AValue: TScreenTurtle);
      procedure SetScreenColor(AValue: TColor);
      procedure SetSize(AValue: Integer);
      procedure SetSpeed(AValue: Integer);
      procedure SetTurtleKind(AValue: TTurtleKind);
      procedure SetTurtleVisible(AValue: Boolean);
      procedure SetRubberPen(AValue: Boolean); // la tortue efface
      // coordonnées dans limites ?
      function IsWithinLimits(X, Y: Double): Boolean;
      // effectue un déplacement
      procedure DoGo(X, Y: Double);
      // déplacement en écrivant
      procedure LineTo(X, Y: Double);
      // déplacement sans écrire
      procedure MoveTo(X, Y: Double);
      // arc d'ellipse (b : début, e : fin - en degrés)
      procedure ArcAntialias(x, y, rx, ry: single; b, e: word;
          c: TBGRAPixel; w: single);
      // arc d'ellipse rempli (b : début, e : fin - en degrés)
      procedure FillArcAntiAlias(x, y, rx, ry: single; b, e: word;
        c: TBGRAPixel);
      // portion d'ellipse (b : début, e : fin - en degrés)
      procedure PieAntialias(x, y, rx, ry: single; b, e: word;
          c: TBGRAPixel; w: single);
      // portion d'ellipse remplie (b : début, e : fin - en degrés)
      procedure FillPieAntiAlias(x, y, rx, ry: single; b, e: word;
        c: TBGRAPixel);
    protected
      // change l'ordonnée pour le nouveau repère
      function cY(Y: Integer): Integer; virtual;
      // on dessine la tortue triangulaire
      procedure DrawTriangleTurtle; virtual;
      // tortue PNG
      procedure DrawPNGTurtle; virtual;
      // gestion du changement
      procedure Change; dynamic;
      // avant le changement
      procedure BeforeChange; dynamic;
    public
      constructor Create(Width, Height: Integer); // création
      destructor Destroy; override; // destructeur
      procedure ReInit; // réinitialisation de l'objet
      // inversion de l'écriture
      procedure PenReverse;
      // la tortue se déplace
      procedure Move(Value: Double);
      // fixe les coordonnées de la tortue
      procedure SetPos(X, Y: Double);
      // la tortue tourne
      procedure Turn(Value: Double);
      // tortue à l'origine
      procedure Home;
      // nettoyage de l'écran
      procedure Wipe;
      // sauvegarde de la tortue
      procedure SaveTurtle;
      // récupère une tortue sauvée
      procedure ReloadTurtle(Clean: Boolean);
      // renvoie le cap vers un point
      function Towards(X, Y: Integer): Double;
      // renvoie la distance de la tortue à un point donné
      function Distance(X, Y: Integer): Double;
      // dessine un rectangle
      procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
      // dessine un rectangle à l'emplacement de la tortue
      procedure Rectangle(X2, Y2: Integer); overload;
      // dessine un carré
      procedure Square(X1, Y1, L: Integer); overload;
      // dessine un carré à l'emplacement de la tortue
      procedure Square(L: Integer); overload;
      // dessine un rectangle arrondi
      procedure RoundRect(X1, Y1, X2, Y2: Integer); overload;
      // dessine un rectangle arrondi à l'emplacement de la tortue
      procedure RoundRect(X2, Y2: Integer); overload;
      // dessine une ellipse
      procedure Ellipse(X1, Y1, X2, Y2: Integer); overload;
      // dessine une ellipse à l'emplacement de la tortue
      procedure Ellipse(X2, Y2: Integer); overload;
      // dessine un cercle
      procedure Circle(X1, Y1, R: Integer); overload;
      // dessine un cercle à l'emplacement de la tortue
      procedure Circle(R: Integer); overload;
      // dessine un arc d'ellipse
      procedure Arc(X1, Y1, X2, Y2, X3, Y3: Integer); overload;
      // dessine un arc d'ellipse à l'emplacement de la tortue
      procedure Arc(X2, Y2, X3, Y3: Integer); overload;
      // dessine une section d'ellipse
      procedure Pie(X1, Y1, X2, Y2, X3, Y3: Integer); overload;
      // dessine une section d'ellipse à l'emplacement de la tortue
      procedure Pie(X2, Y2, X3, Y3: Integer); overload;
      // texte affiché sur l'écran de la tortue
      procedure Text(const St: string; X,Y, Angle: Integer); overload;
      // texte affiché à l'emplacement de la tortue
      procedure Text(const St: string); overload;
      // état de la tortue
      function TurtleState: string;
      // abscisse de la tortue
      property CoordX: Double read GetCoordX write SetCoordX;
      // ordonnée de la tortue
      property CoordY: Double read GetCoordY write SetCoordY;
      // type de tortue
      property Kind: TTurtleKind read fTurtleKind write SetTurtleKind
        default tkTriangle;
      // visibilité de la tortue
      property TurtleVisible: Boolean read fTurtleVisible write SetTurtleVisible
        default True;
      // direction de la tortue
      property Heading: Double read fHeading write SetHeading;
      // taille de la tortue
      property Size: Integer read fSize write SetSize default CDefaultSize;
      // drapeau d'écriture
      property PenDown: Boolean read fPenDown write SetPenDown default True;
      // type de zone de déplacement
      property Screen: TScreenTurtle read fScreen write SetScreen
        default teWin;
      // échelle des X
      property ScaleX: Integer read fScaleX write fScaleX default CDefaultScale;
      // échelle des Y
      property ScaleY: Integer read fScaleY write fScaleY default CDefaultScale;
      // état de la gomme
      property PenRubber: Boolean read fPenRubber write SetRubberPen
        default False;
     // couleur du crayon
      property PenColor: TColor read fPenColor write SetPenColor
        default CDefaultPenColor;
      // largeur du crayon
      property PenWidth: Integer read fPenWidth write SetPenWidth default
        CDefaultPenWidth;
      // état du remplissage
      property Filled: Boolean read fFilled write SetFilled default True;
      // vitesse de dessin de la tortue
      property Speed: Integer read fSpeed write SetSpeed default CMaxSpeed;
      // couleur du fond d'écran
      property ScreenColor: TColor read fScreenColor write SetScreenColor
        default CDefaultBackColor;
      // événement après le changement de la tortue
      property OnChange: TTurtleEvent read fOnchange write fOnchange;
      // événement avant le changement de la tortue
      property OnBeforeChange: TTurtleBeforeEvent read fOnBeforeChange
        write fOnBeforeChange;
      // surface de dessin de la tortue
      property TurtleBitmap: TBGRABitmap read GetImg;
      // tortue PNG
      property PNGTurtle: TBGRABitmap read fPNGTurtle write fPNGTurtle;
  end;

  // couleur en couleur locale
  function RGBToIntColor(N: TColor): Integer;
  // couleur locale en couleur
  function IntColorToRGB(N: Integer): TColor;

implementation

uses Math, StrUtils, BGRAPen;

function RGBToIntColor(N: TColor): Integer;
// *** couleur en couleur locale ***
begin
  case N of
    clBlack: Result := 0; // noir
    clAqua: Result := 1; // Aqua
    clBlue: Result := 2; // Bleu
    clCream: Result := 3; // Crème
    clFuchsia: Result := 4; // Fuchsia
    clGray: Result := 5; // Gris
    clGreen: Result := 6; // Vert
    clLime: Result := 7; // Vert citron
    clMaroon: Result := 8; // Marron
    clMedGray: Result := 9; // Gris moyen
    clMoneyGreen: Result := 10; // Vert menthe
    clNavy: Result := 11; // Bleu marine
    clOlive: Result := 12; // Vert olive
    clPurple: Result := 13; // Violet
    clRed: Result := 14; // Rouge
    clSilver: Result := 15; // Argent
    clSkyBlue: Result := 16; // Bleu ciel
    clTeal: Result := 17; // Sarcelle
    clWhite: Result := 18; // Blanc
    clYellow: Result := 19; // Jaune
  else
    Result := 0; // couleur noire par défaut
  end;
end;

function IntColorToRGB(N: Integer): TColor;
// *** couleur locale en couleur ***
const
  CColors: array[0..19] of TColor = (clBlack, clAqua, clBlue, clCream,
    clFuchsia, clGray, clGreen, clLime, clMaroon, clMedGray,
    clMoneyGreen, clNavy, clOlive, clPurple, clRed, clSilver, clSkyBlue,
    clTeal, clWhite, clYellow);
begin
  if (N < 0) or (N > 19) then
    N := 0; // noire si hors bornes
  Result := CColors[N];
end;

{ TGVTurtle }

procedure TGVTurtle.SetScreen(AValue: TScreenTurtle);
// *** changement du mode d'écran ***
begin
  if fScreen = AValue then // la valeur doit être nouvelle
    Exit; // sinon sortie
  fScreen := AValue; // nouvelle valeur pour le mode d'écran
  Change; // changement notifié
end;

function TGVTurtle.GetCoordX: Double;
// *** renvoie l'abscisse (X) ***
begin
  Result := fX;
end;

function TGVTurtle.GetCoordY: Double;
// *** renvoie l'ordonnée (Y) ***
begin
  Result := fY;
end;

function TGVTurtle.GetImg: TBGRABitmap;
// *** récupération de l'image ***
begin
  with ActualImg do // on procède par couches
  begin
    PutImage(0,0,BckImg,dmDrawWithTransparency); // le fond
    PutImage(0,0,DrwImg,dmDrawWithTransparency); // le dessin
    if TurtleVisible then // tortue visible ?
    begin
      case Kind of
        tkTriangle: DrawTriangleTurtle; // on dessine la tortue triangulaire
        tkPNG: DrawPNGTurtle; // tortue PNG
      end;
      PutImage(0,0,TtlImg,dmDrawWithTransparency); // on l'affiche
    end;
  end;
  Result := ActualImg; // on renvoie l'image
end;

procedure TGVTurtle.SetCoordX(AValue: Double);
// *** fixe l'abscisse de la tortue ***
begin
  DoGo(AValue, CoordY); // déplace la tortue sans changer l'ordonnée
end;

procedure TGVTurtle.SetCoordY(AValue: Double);
// *** fixe l'ordonnée de la tortue ***
begin
  DoGo(CoordX, AValue); // déplace la tortue sans changer l'abscisse
end;

procedure TGVTurtle.SetFilled(AValue: Boolean);
// *** remplissage des formes ***
begin
  if fFilled = AValue then  // valeur inchangée ?
    Exit; // on sort
  fFilled := AValue; // nouvelle valeur
  with DrwImg.Canvas.Brush do // on modifie la brosse
  begin
    if fFilled then  // remplissage ?
      Style := bsSolid // brosse solide
    else
      Style := bsClear; // brosse transparente
  end;
  Change; // changement notifié
end;

procedure TGVTurtle.SetHeading(AValue: Double);
// *** fixe le cap de la tortue ***
begin
  if fHeading = AValue then // si valeur inchangée
    Exit; // on sort
  fHeading := Frac(AValue / 360) * 360; // change le cap
  if fHeading < 0 then // on normalise la valeur de l'orientation [0..360]
    fHeading := fHeading + 360;
  BeforeChange; // pour le dessin correct de la tortue
  Change; // changement notifié
end;

procedure TGVTurtle.SetPenColor(AValue: TColor);
// *** couleur du crayon ***
begin
  if fPenColor = AValue then // valeur inchangée ?
    Exit; // on sort
  fPenColor := AValue; // nouvelle valeur de la couleur du crayon
  DrwImg.CanvasBGRA.Pen.Color := fPenColor; // couleur affectée
  DrwImg.Canvas.Pen.Color := fPenColor;
  Change; // changement notifié
end;

procedure TGVTurtle.SetPenDown(AValue: Boolean);
// *** crayon baissé ou levé ***
begin
  if fPenDown = AValue then // valeur inchangée ?
    Exit; // on sort
  fPenDown := AValue; // nouvelle valeur du crayon
  Change; // changement notifié
end;

procedure TGVTurtle.SetPenWidth(AValue: Integer);
// *** largeur du crayon ***
begin
  if fPenWidth = AValue then // valeur inchangée ?
    Exit; // on sort
  fPenWidth := AValue;
  DrwImg.CanvasBGRA.Pen.Width := fPenWidth; // taille changée
end;

procedure TGVTurtle.SetScreenColor(AValue: TColor);
// *** couleur du fond de l'écran ***
begin
  if fScreenColor = AValue then // valeur inchangée ?
    Exit; // on sort
  fScreenColor := AValue; // nouvelle couleur de fond
  BckImg.FillRect(0,0,fWidth, fHeight, ColorToBGRA(ColorToRGB(fScreenColor)),
    dmSet);
  Change; // on signale le changement
end;

procedure TGVTurtle.SetSize(AValue: Integer);
// *** taille de la tortue ***
begin
  // valeur inchangée
  if (fSize = AValue) then
    Exit; // on sort
  fSize := Min(Abs(AValue), CMaxSize); // on normalise la taille
  Change; // changement notifié
end;

procedure TGVTurtle.SetSpeed(AValue: Integer);
// *** vitesse de dessin ***
begin
  if fSpeed = AValue then // valeur inchangée ?
    Exit; // on sort
  fSpeed := Min(AValue, CMaxSpeed); // nouvelle vitesse (maximum = 100)
  Change; // changement notifié
end;

procedure TGVTurtle.SetTurtleKind(AValue: TTurtleKind);
// *** type de tortue ***
begin
  if fTurtleKind = AValue then // valeur inchangée ?
    Exit; // on sort
  fTurtleKind := AValue; // nouveau type de tortue
  Change; // changement notifié
end;

procedure TGVTurtle.SetTurtleVisible(AValue: Boolean);
// *** visibilité de la tortue ***
begin
  if fTurtleVisible = AValue then // valeur inchangée ?
    Exit; // on sort
  fTurtleVisible := AValue; // nouvelle visibilité de la tortue
  Change; // changement notifié
end;

function TGVTurtle.IsWithinLimits(X, Y: Double): Boolean;
// *** coordonnées dans limites ? ***
begin
  Result := (X >= 0) and (Y >= 0) and (X <= fWidth) and (Y <= fHeight);
end;

procedure TGVTurtle.DoGo(X, Y: Double);
// *** effectue un déplacement de la tortue ***
begin
    // si champ clos et hors limites => erreur
    if (Screen <> teGate) or IsWithinLimits(X, Y) then
    begin
      fX := X; // nouvelle abscisse
      fY := Y; // nouvelle ordonnée
      // ralentit le dessin
      Sleep(CMaxSpeed - Speed);
      // dessine
      if PenDown then  // crayon baissé ?
        LineTo(X, Y) // en écrivant
      else
        MoveTo(X, Y); // sans écrire
      Change; // changement notifié
    end;
    // on continue si l'écran s'enroule
    if (Screen = teRoll) and not IsWithinLimits(X,Y) then
    begin
    // mise à jour des coordonnées après enroulement
    // débordement à droite ?
    if (X > fWidth) then
    begin
      MoveTo(0, Y);
      X := X - fWidth;
    end;
    // débordement en bas ?
    if (Y > fHeight) then
    begin
      MoveTo(X, 0);
      Y := Y - fHeight;
    end;
    // débordement à gauche ?
    if (X < 0) then
    begin
      MoveTo(fWidth, Y);
      X := fWidth + X;
    end;
    // débordement en haut ?
    if (Y < 0) then
    begin
      MoveTo(X, fHeight);
      Y := fHeight + Y;
    end;
    fX := X; // nouvelle abscisse
    fY := Y; // nouvelle ordonnée
    // ralentit le dessin
    Sleep(CMaxSpeed - Speed);
    // dessine ou déplace suivant l'état du crayon
    if PenDown then
      LineTo(X, Y)
    else
      MoveTo(X, Y);
  end;
end;

procedure TGVTurtle.LineTo(X, Y: Double);
// *** déplacement en écrivant ***
begin
  DrwImg.CanvasBGRA.LineTo(Round(X), cY(Round(Y))); // écriture effective
end;

procedure TGVTurtle.MoveTo(X, Y: Double);
// *** déplacement sans écrire ***
begin
  DrwImg.CanvasBGRA.MoveTo(Round(X), cY(Round(Y))); // déplacement effectif
end;

procedure TGVTurtle.ArcAntialias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel; w: single);
// *** arc d'ellipse ***
begin
  DrwImg.DrawPolygonAntialias(DrwImg.ComputeArcRad(x,y,rx,ry,
    b * DgToRad,e * DgToRad), c,w);
end;

procedure TGVTurtle.FillArcAntiAlias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel);
// *** arc d'ellipse rempli ***
begin
  DrwImg.FillPolyAntialias(DrwImg.ComputeArcRad(x,y,rx,ry,b * DgToRad,
    e * DgToRad), c);
end;

procedure TGVTurtle.PieAntialias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel; w: single);
// *** portion d'ellipse ***
begin
  DrwImg.DrawPolygonAntialias(DrwImg.ComputePieRad(x,y,rx,ry,b * DgToRad,
    e * DgToRad), c,w);
end;

procedure TGVTurtle.FillPieAntiAlias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel);
// *** portion d'ellipse remplie ***
begin
  DrwImg.FillPolyAntialias(DrwImg.ComputePieRad(x,y,rx,ry,b * DgToRad,
    e * DgToRad), c);
end;

function TGVTurtle.cY(Y: Integer): Integer;
// *** change l'ordonnée pour le nouveau repère ***
begin
  Result := fHeight - Y; // inversion des ordonnées
end;

procedure TGVTurtle.DrawTriangleTurtle;
// *** dessin de la tortue triangulaire ***
var
  LCosT, LSinT: Extended;
  LX1, LX2, LX3, LY1, LY2, LY3: Integer;
begin
  // on efface la surface
  TtlImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent, dmSet);
  // calcul des coordonnées des points de la tortue
  SinCos((90 + Heading) * DgToRad, LSinT, LCosT);
  LX1 := Round(CoordX + Size * LCosT - LSinT);
  LY1 := cY(Round(CoordY + Size * LSinT + LCosT));
  LX2 := Round(CoordX - Size * LCosT - LSinT);
  LY2 := cY(Round(CoordY - Size * LSinT + LCosT));
  LX3 := Round(CoordX - LCosT + (Size shl 1) * LSinT);
  LY3 := cY(Round(CoordY - LSinT - (Size shl 1) * LCosT));
  with TtlImg.CanvasBGRA do
  begin
    if PenColor <> BGRAToColor(BGRAPixelTransparent) then
      Pen.Color := PenColor
    else
      Pen.Color := CDefaultPenColor;
    MoveTo(LX1, LY1); // dessin de la tortue
    Pen.Width := 2;
    LineTo(LX2, LY2);
    Pen.Width := 1;
    LineTo(LX3, LY3);
    LineTo(LX1, LY1);
  end;
end;

procedure TGVTurtle.DrawPNGTurtle;
// *** dessin de la tortue PNG ***
var
  LCosT, LSinT: Extended;
  LX, LY: Integer;
begin
  // on efface la surface
  TtlImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent, dmSet);
  BeforeChange; // récupère la bonne image
  // calcul des coordonnées de la tortue
  SinCos((90 + Heading) * DgToRad, LSinT, LCosT);
  LX := Round(CoordX + LCosT - LSinT);
  LY := Round(CoordY + LSinT + LCosT);
  // copie de la tortue .png
  TtlImg.CanvasBGRA.Draw(LX - (fPNGTurtle.Width shr 1),
        cY(LY) - (fPNGTurtle.Height shr 1), fPNGTurtle);
end;

procedure TGVTurtle.Change;
// *** notification de changement ***
begin
  if Assigned(fOnchange) then // on exécute le gestionnaire s'il existe
    fOnchange(Self);
end;

procedure TGVTurtle.BeforeChange;
// *** gestion avant le changement ***
// (permet de mettre à jour une image pour la tortue avant de la dessiner)
begin
  if Assigned(fOnBeforeChange) then  // si le gestionnaire existe
    fOnBeforeChange(Self, Round(Heading)); // on l'exécute
end;

constructor TGVTurtle.Create(Width, Height: Integer);
// *** création de l'objet ***
begin
  inherited Create; // on hérite de l'ancêtre
  // on mémorise les dimensions
  fHeight := Height; // la hauteur
  fWidth := Width; // la largeur
  // on crée les images de travail
  // le fond
  BckImg := TBGRABitmap.Create(Width, Height,
    ColorToBGRA(ColorToRGB(CDefaultBackColor)));
  // la surface de dessin
  DrwImg := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  // la tortue
  TtlImg := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  fPNGTurtle := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  // surface réelle
  ActualImg := TBGRABitmap.Create(Width, Height, BGRAPixelTransparent);
  // initialisations
  ReInit;
end;

destructor TGVTurtle.Destroy;
// *** destruction de l'objet ***
begin
  BckImg.Free; // on libère les images créées
  DrwImg.Free;
  TtlImg.Free;
  fPNGTurtle.Free;
  ActualImg.Free;
  inherited Destroy; // on hérite
end;

procedure TGVTurtle.ReInit;
// *** réinitialisation de l'objet ***
begin
  Screen := teWin; // mode fenêtre étendue
  Kind := tkTriangle; // forme triangulaire pour la tortue
  PenRubber := False; // on ne gomme pas
  ScreenColor := CDefaultBackColor; // couleur de fond par défaut
  PenColor := CDefaultPenColor; // couleur de crayon par défaut
  fTempColor := PenColor; // on se souvient de cette couleur
  PenWidth := CDefaultPenWidth; // largeur du crayon par défaut
  Heading := CDefaultHeading; // orientation
  Size := CDefaultSize; // taille de la tortue
  PenDown := True; // drapeau d'écriture
  ScaleX := CDefaultScale; // échelle des X
  ScaleY := CDefaultScale; // échelle des Y
  Filled := True;  // remplissage
  Speed := CMaxSpeed; // vitesse de dessin de la tortue
  DoGo(fWidth shr 1, fHeight shr 1); // au centre
  DrwImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent,
    dmSet); // on efface la surface
  TurtleVisible := True; // tortue visible
  Change; // changement notifié
end;

procedure TGVTurtle.Move(Value: Double);
// *** la tortue se déplace ***
var
  LSinT, LCosT: Extended;
  LTX, LTY: Double;
begin
  // calcul du cosinus et du sinus du cap
  SinCos((Heading - 90) * DgToRad, LSinT, LCosT);
  // calcul des nouvelles coordonnées
  LTX := fX - Value * LSinT * (ScaleX / CDefaultScale);
  LTY := fY + Value * LCosT * (ScaleY / CDefaultScale);
  SetPos(LTX, LTY); // déplacement si possible
end;

procedure TGVTurtle.SetPos(X, Y: Double);
// *** fixe les coordonnées de la tortue ***
begin
  DoGo(X, Y); // déplacement de la tortue
end;

procedure TGVTurtle.Turn(Value: Double);
// *** la tortue tourne ***
begin
  if Value <> 0 then // si valeur effective
    SetHeading(Heading + Value); // tourne vers la gauche
end;

procedure TGVTurtle.Home;
// *** tortue à l'origine ***
begin
  DoGo(fWidth shr 1, fHeight shr 1); // au centre
  Heading := CDefaultHeading; // tête vers le haut de l'écran
end;

procedure TGVTurtle.Wipe;
// *** on nettoie l'écran sans bouger la tortue ***
begin
  DrwImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent, dmSet);
  Change; // on notifie le changement
end;

procedure TGVTurtle.SaveTurtle;
// *** sauvegarde de l'état de la tortue ***
begin
  with fSavedTurtle do // on sauvegarde la tortue en cours
  begin
    rX := fX; // abscisse
    rY := fY; // ordonnée
    rKind := fTurtleKind; // type de tortue
    rSize := fSize; // taille de la tortue
    rVisible := fTurtleVisible; // drapeau de visibilité
    rHeading := fHeading; // direction
    rPenDown := fPenDown; // drapeau de crayon baissé
    rPenRubber := fPenRubber; // drapeau d'effacement
    rScaleX := fScaleX; // échelle des X
    rScaleY := fScaleY; // échelle des Y
    rFilled := Filled; // remplissage
    rPenWidth := PenWidth; // largeur du crayon
    if not rSaved then // création si nécessaire
    begin
      rBrush := TBrush.Create; // type de brosse
      rPen := TPen.Create; // type de crayon
      rFont := TFont.Create; // type de fonte
    end;
    with DrwImg.Canvas do
    begin
      rBrush.Assign(Brush); // sauvegarde brosse
      rPen.Assign(Pen); // sauvegarde crayon
      rFont.Assign(Font); // sauvegarde fonte
    end;
    rSaved := True; // drapeau de sauvegarde activé
  end;
end;

procedure TGVTurtle.ReloadTurtle(Clean: Boolean);
// *** récupère une tortue ***
begin
  if fSavedTurtle.rSaved then // seulement si une tortue a été sauvegardée
    try
      TurtleVisible := False; // on cache la tortue
      PenDown := False; // on n'écrit pas !
      PenRubber := False; // on n'efface pas !
      with fSavedTurtle do // on recharge la tortue
      begin
        fX := rX; // abscisse
        fY := rY; // ordonnée
        MoveTo(Round(fX), Round(fY)); // on déplace la tortue
        fTurtleKind := rKind; // type de tortue
        Size := rSize; // taille de la tortue
        Heading := rHeading; // direction
        PenRubber := rPenRubber; // drapeau d'effacement
	ScaleX := rScaleX; // échelle des X
        ScaleY := rScaleY; // échelle des Y
        Filled := rFilled; // remplissage
        PenWidth := rPenWidth; // largeur du crayon
        with DrwImg.Canvas do
        begin
          Brush.Assign(rBrush); // type de brosse
          Pen.Assign(rPen); // type de crayon
          Font.Assign(rFont);
        end;
        TurtleVisible := rVisible; // drapeau de visibilité
        PenDown := rPenDown; // drapeau de crayon baissé
      end;
    finally
      if Clean then
        with fSavedTurtle do
        begin
          rBrush.Free; // on libère la brosse
          rPen.Free; // on libère le crayon
          rFont.Free; // on libère la fonte
          rSaved := False; // libère la sauvegarde
        end;
    end;
end;

function TGVTurtle.Towards(X, Y: Integer): Double;
// *** renvoie le cap vers un point ***
var
  LPX, LPY: Integer;
begin
  LPX := Round(CoordX) - X; // calcul des différences entre les points
  LPY := Y - Round(CoordY);
  Result := 0; // suppose 0
  // évalue suivant les calculs
  if ((LPX = 0) and (LPY < 0)) then
    Result := 270
  else if ((LPX = 0) and (LPY > 0)) then
    Result := 90
  else if ((LPX > 0) and (LPY >= 0)) then
    Result := 180 - ArcTan(LPY / LPX) * RadToDg
  else if ((LPX < 0) and (LPY > 0)) then
    Result := (ArcTan(LPY / Abs(LPX)) * RadToDg)
  else if ((LPX < 0) and (LPY <= 0)) then
    Result := 360 - (ArcTan(LPY / LPX) * RadToDg)
  else if ((LPX > 0) and (LPY < 0)) then
    Result := 180 + (ArcTan(Abs(LPY) / LPX) * RadToDg);
end;

function TGVTurtle.Distance(X, Y: Integer): Double;
// *** renvoie la distance de la tortue à un point donné ***
begin
  Result := Sqrt(Sqr(X - CoordX) + Sqr(Y - CoordY));
end;

procedure TGVTurtle.Rectangle(X1, Y1, X2, Y2: Integer);
// *** rectangle absolu ***
begin
  if Filled then
    DrwImg.FillRectAntialias(X1, cY(Y1), X2, cY(Y2),
      ColorToBGRA(ColorToRGB(PenColor)))
  else
    DrwImg.RectangleAntialias(X1, cY(Y1), X2, cY(Y2),
      ColorToBGRA(ColorToRGB(PenColor)), PenWidth);
  Change; // on notifie le changement
end;

procedure TGVTurtle.Rectangle(X2, Y2: Integer);
// *** rectangle à l'emplacement de la souris ***
begin
  Rectangle(Round(CoordX), Round(CoordY), X2, Y2);
end;

procedure TGVTurtle.Square(X1, Y1, L: Integer);
// *** carré absolu ***
begin
  Rectangle(X1, Y1, X1 + L, Y1 - L);
end;

procedure TGVTurtle.Square(L: Integer);
// *** carré à l'emplacement de la tortue ***
begin
  Rectangle(Round(CoordX), Round(CoordY), Round(CoordX) + L, Round(CoordY) - L);
end;

procedure TGVTurtle.RoundRect(X1, Y1, X2, Y2: Integer);
// *** dessine un rectangle arrondi ***
begin
  if Filled then
    DrwImg.FillRoundRectAntialias(X1, cY(Y1), X2, cY(Y2), 15, 15,
      ColorToBGRA(ColorToRGB(PenColor)))
  else
    DrwImg.RoundRectAntialias(X1, cY(Y1), X2, cY(Y2), 15, 15,
      ColorToBGRA(ColorToRGB(PenColor)), PenWidth);
  Change; // on notifie le changement
end;

procedure TGVTurtle.RoundRect(X2, Y2: Integer);
// *** dessine un rectangle arrondi à l'emplacement de la tortue ***
begin
  RoundRect(Round(CoordX), Round(CoordY), X2, Y2);
end;

procedure TGVTurtle.Ellipse(X1, Y1, X2, Y2: Integer);
// *** dessine une ellipse ***
begin
  if Filled then
    DrwImg.FillEllipseAntialias(X1, cY(Y1), X2, Y2,
      ColorToBGRA(ColorToRGB(PenColor)))
  else
    DrwImg.EllipseAntialias(X1, cY(Y1), X2, Y2,
      ColorToBGRA(ColorToRGB(PenColor)), PenWidth);
  Change; // on notifie le changement
end;

procedure TGVTurtle.Ellipse(X2, Y2: Integer);
// *** dessine une ellipse à l'emplacement de la tortue ***
begin
  Ellipse(Round(CoordX), Round(CoordY), X2, Y2);
end;

procedure TGVTurtle.Circle(X1, Y1, R: Integer);
// *** dessine un cercle ***
begin
  Ellipse(X1, Y1, R, R);
end;

procedure TGVTurtle.Circle(R: Integer);
// *** dessine un cercle à l'emplacement de la tortue ***
begin
  Circle(Round(CoordX), Round(CoordY), R);
end;

procedure TGVTurtle.Arc(X1, Y1, X2, Y2, X3, Y3: Integer);
// *** dessine un arc d'ellipse ***
begin
  if Filled then
    FillArcAntialias(X1, cY(Y1), X2, Y2, X3, Y3,
      ColorToBGRA(ColorToRGB(PenColor)))
  else
    ArcAntiAlias(X1,cY(Y1),X2,Y2,X3,Y3,
      ColorToBGRA(ColorToRGB(PenColor)),PenWidth);
  Change; // on notifie le changement
end;

procedure TGVTurtle.Arc(X2, Y2, X3, Y3: Integer);
// *** dessine un arc d'ellipse à l'emplacement de la tortue ***
begin
  Arc(Round(CoordX), Round(CoordY), X2, Y2, X3, Y3);
end;

procedure TGVTurtle.Pie(X1, Y1, X2, Y2, X3, Y3: Integer);
// *** dessine une section d'ellipse ***
begin
   if Filled then
    FillPieAntialias(X1, cY(Y1), X2, Y2, X3, Y3,
      ColorToBGRA(ColorToRGB(PenColor)))
  else
    PieAntiAlias(X1,cY(Y1),X2,Y2,X3,Y3,
      ColorToBGRA(ColorToRGB(PenColor)),PenWidth);
  Change; // on notifie le changement
end;

procedure TGVTurtle.Pie(X2, Y2, X3, Y3: Integer);
// *** dessine une section d'ellipse à l'emplacement de la tortue ***
begin
  Pie(Round(CoordX), Round(CoordY), X2, Y2, X3, Y3);
end;

procedure TGVTurtle.Text(const St: string; X, Y, Angle: Integer);
// *** affiche un texte sur l'écran de la tortue ***
begin
  DrwImg.CanvasBGRA.Font.Color := PenColor; // couleur d'écriture de la tortue
  DrwImg.TextOutAngle(X, cY(Y), Angle * 10, St,
    ColorToBGRA(ColorToRGB(PenColor)), taLeftJustify);
  Change; // on signifie le changement
end;

procedure TGVTurtle.Text(const St: string);
// *** affiche un texte à l'emplacement de la tortue ***
begin
  Text(St, Round(CoordX), Round(CoordY), Round(Heading));
end;

function TGVTurtle.TurtleState: string;
// *** état de la tortue ***
begin
  Result := CBeginList + FloatToStr(CoordX) + CBlank + FloatToStr(CoordY) +
    CBlank + FloatToStr(Heading) + CBlank + IntToStr(Size) + CBlank +
    IntToStr(Speed) + CBlank + Ifthen(TurtleVisible, IntToStr(CRTrue),
    IntToStr(CRFalse)) + CBlank + Ifthen((Kind = tkTriangle), IntToStr(CRTrue),
    IntToStr(CRFalse)) + CEndList;
end;

procedure TGVTurtle.PenReverse;
// *** inversion du crayon ***
begin
  PenColor := not PenColor; // couleur inversée
end;

procedure TGVTurtle.SetRubberPen(AValue: Boolean);
// *** le crayon gomme ***
begin
  if AValue = fPenRubber then
    Exit;
  fPenRubber := AValue; // nouvelle valeur
  if fPenRubber then
  begin
    fTempColor := PenColor; // on se souvient de la couleur en cours
    // on dessine avec la couleur transparente
    PenColor := BGRAToColor(BGRAPixelTransparent);
  end
  else
  if PenColor = BGRAToColor(BGRAPixelTransparent) then
    PenColor := fTempColor; // on restitue la couleur d'origine
end;

end.
