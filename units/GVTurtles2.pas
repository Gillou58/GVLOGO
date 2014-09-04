{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Tortue graphique (version BGRABitmap)   |
  |                  Unité : GVTurtles2.pas                                |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    04-09-2014 15:14:48                          |
  |                  Version : 1.1.2                                       |
  |                                                                        |
  |========================================================================| }

// GVTurtles2 - part of GVLOGO
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
//
// 04/09/2014 1.1.2 ajout enroulement, correction PenRubber et corrections mineures
// 03/09/2014 1.1.1 ajout de PenRubber et PenReverse
// 08/08/2014 1.1.0 version initiale (dérivée de GVTURTLES

{$I GVDefines.inc}

{$IFDEF Delphi}
  // Cette unité ne fonctionne qu'avec LAZARUS et la bibliothèque BGRABitmap.
{$ENDIF}
  
{$mode objfpc}{$H+}

unit GVTurtles2;

// Unité de la tortue graphique de GVLOGO
//
// ##############################################################
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
  TTurtleEvent = procedure(Sender: TObject) of object;
  // avant le changement de la tortue
  TTurtleBeforeEvent = procedure(Sender: TObject; cHeading: Integer) of object;

  // définition d'une tortue
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

  { TGVTurtle - la tortue }

  TGVTurtle = class
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
    published
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
      property Speed: Integer read fSpeed write SetSpeed default CMaxSpeed div 2;
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

implementation

uses math, BGRAPen;

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
// ***largeur du crayon ***
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
  BckImg.FillRect(0,0,fWidth, fHeight, ColorToBGRA(ColorToRGB(fScreenColor)), dmSet);
  Change; // on signale le changement
end;

procedure TGVTurtle.SetSize(AValue: Integer);
// *** taille de la tortue ***
begin
  // valeur inchangée ou tortue non triangulaire ?
  if (fSize = AValue) or (Kind <> tkTriangle) then
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

procedure TGVTurtle.ArcAntialias(x, y, rx, ry: single; b, e: word; c: TBGRAPixel;
    w: single);
// *** arc d'ellipse ***
begin
  DrwImg.DrawPolygonAntialias(DrwImg.ComputeArcRad(x,y,rx,ry,b * DgToRad,e * DgToRad),
    c,w);
end;

procedure TGVTurtle.FillArcAntiAlias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel);
// *** arc d'ellipse rempli ***
begin
  DrwImg.FillPolyAntialias(DrwImg.ComputeArcRad(x,y,rx,ry,b * DgToRad,e * DgToRad),
    c);
end;

procedure TGVTurtle.PieAntialias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel; w: single);
// *** portion d'ellipse ***
begin
  DrwImg.DrawPolygonAntialias(DrwImg.ComputePieRad(x,y,rx,ry,b * DgToRad,e * DgToRad),
    c,w);
end;

procedure TGVTurtle.FillPieAntiAlias(x, y, rx, ry: single; b, e: word;
  c: TBGRAPixel);
// *** portion d'ellipse remplie ***
begin
  DrwImg.FillPolyAntialias(DrwImg.ComputePieRad(x,y,rx,ry,b * DgToRad,e * DgToRad),
    c);
end;

function TGVTurtle.cY(Y: Integer): Integer;
// *** change l'ordonnée pour le nouveau repère ***
begin
  Result := fHeight - Y; // inversion des ordonnées
end;

procedure TGVTurtle.DrawTriangleTurtle;
// *** dessin de la tortue triangulaire ***
var
  CosT, SinT: Extended;
  X1, X2, X3, Y1, Y2, Y3: Integer;
begin
  // on efface la surface
  TtlImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent, dmSet);
  // calcul des coordonnées des points de la tortue
  SinCos((90 + Heading) * DgToRad, SinT, CosT);
  X1 := Round(CoordX + Size * CosT - SinT);
  Y1 := cY(Round(CoordY + Size * SinT + CosT));
  X2 := Round(CoordX - Size * CosT - SinT);
  Y2 := cY(Round(CoordY - Size * SinT + CosT));
  X3 := Round(CoordX - CosT + (Size shl 1) * SinT);
  Y3 := cY(Round(CoordY - SinT - (Size shl 1) * CosT));
  with TtlImg.CanvasBGRA do
  begin
    if PenColor <> BGRAToColor(BGRAPixelTransparent) then
      Pen.Color := PenColor
    else
      Pen.Color := CDefaultPenColor;
    MoveTo(X1, Y1); // dessin de la tortue
    Pen.Width := 2;
    LineTo(X2, Y2);
    Pen.Width := 1;
    LineTo(X3, Y3);
    LineTo(X1, Y1);
  end;
end;

procedure TGVTurtle.DrawPNGTurtle;
// *** dessin de la tortue PNG ***
var
  CosT, SinT: Extended;
  X, Y: Integer;
begin
  // on efface la surface
  TtlImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent, dmSet);
  BeforeChange; // récupère la bonne image
  // calcul des coordonnées de la tortue
  SinCos((90 + Heading) * DgToRad, SinT, CosT);
  X := Round(CoordX + CosT - SinT);
  Y := Round(CoordY + SinT + CosT);
  // copie de la tortue .png
  TtlImg.CanvasBGRA.Draw(X - (fPNGTurtle.Width shr 1),
        cY(Y) - (fPNGTurtle.Height shr 1), fPNGTurtle);
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
  BckImg := TBGRABitmap.Create(Width, Height, ColorToBGRA(ColorToRGB(CDefaultBackColor)));
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
  Speed := CMaxSpeed div 2; // vitesse de dessin de la tortue
  DoGo(fWidth shr 1, fHeight shr 1); // au centre
  DrwImg.FillRect(0,0,fWidth, fHeight, BGRAPixelTransparent,
    dmSet); // on efface la surface
  TurtleVisible := True; // tortue visible
  Change; // changement notifié
end;

procedure TGVTurtle.Move(Value: Double);
// *** la tortue se déplace ***
var
  SinT, CosT: Extended;
  TX, TY: Double;
begin
  // calcul du cosinus et du sinus du cap
  SinCos((Heading - 90) * DgToRad, SinT, CosT);
  // calcul des nouvelles coordonnées
  TX := fX - Value * SinT * (ScaleX / CDefaultScale);
  TY := fY + Value * CosT * (ScaleY / CDefaultScale);
  SetPos(TX, TY); // déplacement si possible
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
  PX, PY: Integer;
begin
  PX := Round(CoordX) - X; // calcul des différences entre les points
  PY := Y - Round(CoordY);
  Result := 0; // suppose 0
  // évalue suivant les calculs
  if ((PX = 0) and (PY < 0)) then
    Result := 270
  else if ((PX = 0) and (PY > 0)) then
    Result := 90
  else if ((PX > 0) and (PY >= 0)) then
    Result := 180 - ArcTan(PY / PX) * RadToDg
  else if ((PX < 0) and (PY > 0)) then
    Result := (ArcTan(PY / Abs(PX)) * RadToDg)
  else if ((PX < 0) and (PY <= 0)) then
    Result := 360 - (ArcTan(PY / PX) * RadToDg)
  else if ((PX > 0) and (PY < 0)) then
    Result := 180 + (ArcTan(Abs(PY) / PX) * RadToDg);
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
    DrwImg.FillRectAntialias(X1, cY(Y1), X2, cY(Y2), ColorToBGRA(ColorToRGB(PenColor)))
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
    FillArcAntialias(X1, cY(Y1), X2, Y2, X3, Y3, ColorToBGRA(ColorToRGB(PenColor)))
  else
    ArcAntiAlias(X1,cY(Y1),X2,Y2,X3,Y3,ColorToBGRA(ColorToRGB(PenColor)),PenWidth);
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
    FillPieAntialias(X1, cY(Y1), X2, Y2, X3, Y3, ColorToBGRA(ColorToRGB(PenColor)))
  else
    PieAntiAlias(X1,cY(Y1),X2,Y2,X3,Y3,ColorToBGRA(ColorToRGB(PenColor)),PenWidth);
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
  DrwImg.CanvasBGRA.Font.Color := PenColor; // couleur d'écriture comme la tortue
  DrwImg.TextOutAngle(X, cY(Y), Angle * 10, St, ColorToBGRA(ColorToRGB(PenColor)),
    taLeftJustify);
  Change; // on signifie le changement
end;

procedure TGVTurtle.Text(const St: string);
// *** affiche un texte à l'emplacement de la tortue ***
begin
  Text(St, Round(CoordX), Round(CoordY), Round(Heading));
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
