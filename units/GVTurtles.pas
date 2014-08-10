{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Tortue graphique                        |
  |                  Unit� : GVTurtles.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
  |                  Date:    08-08-2014 12:29:48                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVTurtles - part of GVLOGO
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
  
{$I GVDefines.inc}

{$IFNDEF Delphi}
{$mode objfpc}{$H+}
{$ENDIF}

unit GVTurtles;

// Unit� de la tortue graphique de GVLOGO
//
// ##############################################################
//
// La tortue graphique permet de dessiner sur une surface
// en fonction d'ordres simples.
//

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls
{$IFDEF Delphi}
    , System.Types, System.UITypes, PNGImage
{$ENDIF}
    , GVConsts;

type
  // changement de la tortue
  TTurtleEvent = procedure(Sender: TObject; cX, cY, cHeading: Integer;
    cVisible, cDown: Boolean; cColor: TColor) of object;
  // avant le changement de la tortue
  TTurtleBeforeEvent = procedure(Sender: TObject; cHeading: Integer) of object;
  // changement du fond d'�cran
  TTurtleBackGroundEvent = procedure(Sender: TObject; bkColor: TColor)
    of object;

  // d�finition d'une tortue
  TTurtle = record
    rSaved: Boolean; // drapeau de sauvegarde
    rX: Extended; // abscisse
    rY: Extended; // ordonn�e
    rKind: TTurtleKind; // type de tortue
    rSize: Integer; // taille de la tortue
    rVisible: Boolean; // drapeau de visibilit�
    rHeading: Extended; // direction
    rPenDown: Boolean; // drapeau de crayon baiss�
    rPenRubber: Boolean; // drapeau d'effacement
    rPenReverse: Boolean; // drapeau d'inversion
    rScaleX: Integer; // �chelle des X
    rScaleY: Integer; // �chelle des Y
    rFilled: Boolean; // remplissage
    rBrush: TBrush; // type de brosse
    rPen: TPen; // type de crayon
    rFont: TFont; // type de fonte
  end;

  { TGVTurtle - la tortue }

  TGVTurtle = class(TImage)
  private
    fX: Real; // abscisse de la tortue
    fY: Real; // ordonn�e de la tortue
    fTurtleKind: TTurtleKind; // type de tortue
    fTurtleVisible: Boolean; // visibilit� de la tortue
    fHeading: Real; // cap de la tortue
    fSize: Integer; // taille de la tortue
    fPenDown: Boolean; // crayon lev�/baiss�
    fScreen: TScreenTurtle; // �cran de la tortue
    fScaleX: Integer; // �chelle des X
    fScaleY: Integer; // �chelle des Y
    fPenRubber: Boolean; // gomme du crayon
    fPenReverse: Boolean; // inversion du crayon
    fOnchange: TTurtleEvent; // changement notifi�
    fSavedTurtle: TTurtle; // sauvegarde d'une tortue
    fScreenColor: TColor; // couleur de l'�cran
    fFilled: Boolean; // drapeau de remplissage
    fTempColor: TColor; // sauvegarde provisoire de la couleur (PenRubber)
{$IFDEF Delphi}
    fTurtleImg: TPngImage; // image de la tortue
    fTempImg: TBitmap; // image temporaire
    fOldImg: TBitmap; // image conserv�e
{$ELSE}
    fTurtleImg: TCustomBitmap;
    fTempImg: TCustomBitmap; // image temporaire
    fOldImg: TCustomBitmap; // image conserv�e
{$ENDIF}
    fOnBeforeChange: TTurtleBeforeEvent; // notification avant cap chang�
    fOnBackGroundChange: TTurtleBackGroundEvent; // changement de fond
    fPenColor: TColor; // couleur du crayon
    fSpeed: Integer; // vitesse de la tortue
    function GetCoordX: Integer; // abscisse de la tortue
    function GetCoordY: Integer; // ordonn�e de la tortue
    procedure SetCoordX(const Value: Integer); // abscisse de la tortue
    procedure SetCoordY(const Value: Integer); // ordonn�e de la tortue
    procedure SetTurtleKind(const Value: TTurtleKind); // type de tortue
    procedure SetTurtleVisible(const Value: Boolean); // visibilit� de la tortue
    procedure SetHeading(const Value: Real); // cap
    procedure SetSize(const Value: Integer); // taille de la tortue
    procedure SetPenReverse(const Value: Boolean); // inversion de l'�criture
    procedure SetRubberPen(const Value: Boolean); // la tortue efface
    procedure SetScreenColor(const Value: TColor); // couleur d'�cran
    procedure SetPenDown(const Value: Boolean); // crayon baiss� ou lev�
    procedure SetFilled(const Value: Boolean); // remplissage
    procedure SetPenColor(const Value: TColor); // couleur du crayon
    procedure SetSpeed(const Value: Integer); // vitesse de dessin de la tortue
  protected
    // change l'ordonn�e pour le nouveau rep�re
    function cY(Y: Integer): Integer;
    // effectue un d�placement
    procedure DoGo(const X, Y: Integer);
    // coordonn�es dans limites ?
    function IsWithinLimits(const X, Y: Integer): Boolean;
    // montre/cache la tortue png
    procedure ToggleTurtlePNG;
    // montre/cache la tortue triangle
    procedure ToggleTurtleTriangle;
    // gestion du changement
    procedure Change; dynamic;
    // gestion de l'action avant le changement
    procedure BeforeChange; dynamic;
    // gestion du changement de fond
    procedure BackGroundChange; dynamic;
  public
    // cr�ation
    constructor Create(AOwner: Tcomponent); override;
    // destruction
    destructor Destroy; override;
    // d�placement en �crivant
    procedure LineTo(X, Y: Integer);
    // d�placement sans �crire
    procedure MoveTo(X, Y: Integer);
    // r�initialisation de la tortue
    procedure Reinit;
    // tortue � l'origine
    procedure Home;
    // nettoyage de l'�cran
    procedure Wipe;
    // la tortue se d�place
    procedure Move(const Value: Real);
    // la tortue tourne
    procedure Turn(const Value: Real);
    // fixe les coordonn�es de la tortue
    procedure SetPos(const X, Y: Integer);
    // renvoie le cap vers un point
    function Towards(const X, Y: Integer): Real;
    // renvoie la distance de la tortue � un point donn�
    function Distance(const X, Y: Integer): Real;
    // dessine un rectangle
    procedure Rectangle(const X1, Y1, X2, Y2: Integer); overload;
    // dessine un rectangle � l'emplacement de la tortue
    procedure Rectangle(const X2, Y2: Integer); overload;
    // dessine un carr�
    procedure Square(const X1, Y1, L: Integer); overload;
    // dessine un carr� � l'emplacement de la tortue
    procedure Square(const L: Integer); overload;
    // dessine un rectangle arrondi
    procedure RoundRect(const X1, Y1, X2, Y2: Integer); overload;
    // dessine un rectangle arrondi � l'emplacement de la tortue
    procedure RoundRect(const X2, Y2: Integer); overload;
    // dessine une ellipse
    procedure Ellipse(const X1, Y1, X2, Y2: Integer); overload;
    // dessine une ellipse � l'emplacement de la tortue
    procedure Ellipse(const X2, Y2: Integer); overload;
    // dessine un cercle
    procedure Circle(const X1, Y1, R: Integer); overload;
    // dessine un cercle � l'emplacement de la tortue
    procedure Circle(const R: Integer); overload;
    // dessine un arc d'ellipse
    procedure Arc(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine un arc d'ellipse � l'emplacement de la tortue
    procedure Arc(const X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine une corde d'ellipse
    procedure Chord(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine une corde d'ellipse � l'emplacement de la tortue
    procedure Chord(const X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine une section d'ellipse
    procedure Pie(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine une section d'ellipse � l'emplacement de la tortue
    procedure Pie(const X2, Y2, X3, Y3, X4, Y4: Integer); overload;
    // dessine un polygone
    procedure Polygon(Points: array of TPoint);
    // dessine un polygone non couvrant
    procedure PolyLine(Points: array of TPoint);
    // sauvegarde la tortue
    procedure SaveTurtle;
    // r�cup�re une tortue sauv�e
    procedure ReloadTurtle(const Clean: Boolean);
  published
    // abscisse de la tortue
    property CoordX: Integer read GetCoordX write SetCoordX;
    // ordonn�e de la tortue
    property CoordY: Integer read GetCoordY write SetCoordY;
    // type de tortue
    property Kind: TTurtleKind read fTurtleKind write SetTurtleKind
      default tkTriangle;
    // dessin alternatif de la tortue
{$IFDEF Delphi}  // image en cours de la tortue PNG
    property TurtleImg: TPngImage read fTurtleImg write fTurtleImg;
{$ELSE}
    property TurtleImg: TCustomBitmap read fTurtleImg write fTurtleImg;
{$ENDIF}
    // visibilit� de la tortue
    property TurtleVisible: Boolean read fTurtleVisible write SetTurtleVisible
      default True;
    // direction de la tortue
    property Heading: Real read fHeading write SetHeading;
    // taille de la tortue
    property Size: Integer read fSize write SetSize default TurtleDefaultSize;
    // drapeau d'�criture
    property PenDown: Boolean read fPenDown write SetPenDown default True;
    // type de zone de d�placement
    property Screen: TScreenTurtle read fScreen write fScreen default teWin;
    // �chelle des X
    property ScaleX: Integer read fScaleX write fScaleX default DefaultScale;
    // �chelle des Y
    property ScaleY: Integer read fScaleY write fScaleY default DefaultScale;
    // �tat de la gomme
    property PenRubber: Boolean read fPenRubber write SetRubberPen
      default False;
    // �tat de l'inversion d'�criture
    property PenReverse: Boolean read fPenReverse write SetPenReverse
      default False;
    // couleur du crayon
    property PenColor: TColor read fPenColor write SetPenColor default clRed;
    // �tat du remplissage
    property Filled: Boolean read fFilled write SetFilled default True;
    // vitesse de dessin de la tortue
    property Speed: Integer read fSpeed write SetSpeed default TurtleMaxSpeed div 2;
    // couleur du fond d'�cran
    property ScreenColor: TColor read fScreenColor write SetScreenColor;
    // �v�nement apr�s le changement de la tortue
    property OnChange: TTurtleEvent read fOnchange write fOnchange;
    // �v�nement avant le changement de la tortue
    property OnBeforeChange: TTurtleBeforeEvent read fOnBeforeChange
      write fOnBeforeChange;
    // �v�nement de changement du fond
    property OnBackGroundChange: TTurtleBackGroundEvent read fOnBackGroundChange
      write fOnBackGroundChange;
  end;

implementation

uses
  Math, Controls;

{ TGVTurtle }

procedure TGVTurtle.Arc(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine un arc de cercle ***
var
  TV: Boolean;
begin
  TV := TurtleVisible; // sauve l'�tat de la tortue
  try
    TurtleVisible := False; // tortue invisible
    Canvas.Arc(X1, cY(Y1), X2, cY(Y2), X3, cY(Y3), X4, cY(Y4)); // dessine
  finally
    TurtleVisible := TV; // r�tablit la tortue
  end;
end;

procedure TGVTurtle.Arc(const X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine un arc de cercle � l'emplacement de la tortue ***
begin
  Arc(CoordX, CoordY, CoordX + X2, CoordY - Y2, X3, Y3, X4, Y4);
end;

procedure TGVTurtle.BackGroundChange;
// *** changement de fond ***
begin
  if Assigned(fOnBackGroundChange) then
    fOnBackGroundChange(Self, ScreenColor);
end;

procedure TGVTurtle.BeforeChange;
// *** gestion avant le changement ***
// (permet de mettre � jour une image pour la tortue avant de la dessiner)
begin
  if Assigned(fOnBeforeChange) then
    fOnBeforeChange(Self, Round(Heading));
end;

procedure TGVTurtle.Change;
// *** gestion du changement ***
begin
  if Assigned(fOnchange) then // on ex�cute le gestionnaire s'il existe
    fOnchange(Self, CoordX, CoordY, Round(Heading), TurtleVisible, PenDown,
      Canvas.Pen.Color);
end;

procedure TGVTurtle.Chord(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine une corde ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.Chord(X1, cY(Y1), X2, cY(Y2), X3, cY(Y3), X4, cY(Y4));
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.Chord(const X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine une corde � l'emplacement de la tortue ***
begin
  Chord(CoordX, CoordY, CoordX + X2, CoordY - Y2, X3, Y3, X4, Y4);
end;

procedure TGVTurtle.Circle(const X1, Y1, R: Integer);
// *** dessine un cercle ***
begin
  Ellipse(X1, Y1, X1 + R, Y1 - R);
end;

procedure TGVTurtle.Circle(const R: Integer);
// *** dessine un cercle � l'emplacement de la tortue ***
begin
  Circle(CoordX, CoordY, R);
end;

constructor TGVTurtle.Create(AOwner: Tcomponent);
// *** cr�ation de l'objet ***
begin
  inherited Create(AOwner);
{$IFDEF Delphi}
  Transparent := True;
{$ENDIF}
  Height := (AOwner as TControl).Height;
  Width := (AOwner as TControl).Width;
  with Picture.Bitmap do
  begin
    Height := Self.Height;
    Width := Self.Width;
{$IFDEF Delphi}
    Transparent := True;
    TransparentColor := clFuchsia;
    Canvas.Brush.Color := TransparentColor;
    fScreenColor := TransparentColor; // fond d'�cran
{$ELSE}
    Canvas.Brush.Color := clBlack;
    fScreenColor := clBlack;
{$ENDIF}
    Canvas.FillRect(Rect(0, 0, Width, Height));
  end;
  fTurtleVisible := False; // tortue invisible
{$IFDEF Delphi}
  fTurtleImg := TPngImage.Create; // tortue PNG alternative
  fTurtleImg.Transparent := True;
{$ELSE}
  fTurtleImg := TPortableNetworkGraphic.Create;
{$ENDIF}
  // images de sauvegarde transparentes pour la tortue .png
  fOldImg := TBitmap.Create;
  with fOldImg do
  begin
    Transparent := True;
    TransparentMode := tmFixed;
    Height := Self.Height;
    Width := Self.Width;
  end;
  fTempImg := TBitmap.Create;
  with fTempImg do
  begin
    Transparent := True;
    TransparentMode := tmFixed;
    Height := Self.Height;
    Width := Self.Width;
  end;
  fSavedTurtle.rSaved := False; // pas de tortue sauv�e
  Reinit; // nettoyage de l'�cran
end;

function TGVTurtle.cY(Y: Integer): Integer;
// *** Y dans le nouveau rep�re ***
begin
  Result := Canvas.ClipRect.Bottom - Y; // inversion des ordonn�es
end;

destructor TGVTurtle.Destroy;
// *** destruction de l'objet ***
begin
  Canvas.OnChange := nil;
  Canvas.OnChanging := nil;
  ReloadTurtle(True); // pour lib�rer les objets sauvegard�s
  fTurtleImg.Free;
  fTempImg.Free;
  fOldImg.Free;
  inherited Destroy;
end;

function TGVTurtle.Distance(const X, Y: Integer): Real;
// *** renvoie la distance de la tortue � un point donn� ***
begin
  Result := Sqrt(Sqr(X - CoordX) + Sqr(Y - CoordY));
end;

procedure TGVTurtle.DoGo(const X, Y: Integer);
// *** effectue un d�placement de la tortue ***
var
  TV: Boolean;
begin
  TV := TurtleVisible; // sauvegarde la tortue
  try
    TurtleVisible := False; // la cache
    // si champ clos et hors limites => erreur
    if (Screen <> teGate) or IsWithinLimits(X, Y) then
    begin
      fX := X;
      fY := Y;
      // ralentit le dessin
      Sleep(TurtleMaxSpeed - Speed);
      // dessine
      if PenDown then
        LineTo(X, Y)
      else
        MoveTo(X, Y);
    end;
  finally
    TurtleVisible := TV; // restaure la tortue
  end;
end;

procedure TGVTurtle.Ellipse(const X1, Y1, X2, Y2: Integer);
// *** dessine une ellipse ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.Ellipse(X1, cY(Y1), X2, cY(Y2));
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.Ellipse(const X2, Y2: Integer);
// *** dessine une ellipse � l'emplacement de la tortue ***
begin
  Ellipse(CoordX, CoordY, CoordX + X2, CoordY - Y2);
end;

function TGVTurtle.GetCoordX: Integer;
// *** renvoie l'abscisse de la tortue ***
begin
  Result := Round(fX);
end;

function TGVTurtle.GetCoordY: Integer;
// *** renvoie l'ordonn�e de la tortue ***
begin
  Result := Round(fY);
end;

procedure TGVTurtle.Home;
// *** tortue � l'origine ***
begin
  with Canvas.ClipRect do
    DoGo(Right shr 1, Bottom shr 1); // au centre
  Heading := DefaultHeading; // t�te vers le haut de l'�cran
end;

procedure TGVTurtle.Reinit;
// *** initialisation ***
begin
  with Canvas.Pen do
  begin
    Style := psSolid; // le crayon �crit en trait continu
    Mode := pmCopy; // copie normale
    Width := 1; // �paisseur de 1
    Color := clAqua; // couleur rouge
    fPenColor := clAqua; // qu'on m�morise
  end;
  fTurtleKind := tkTriangle; // en forme de triangle
  fScaleX := DefaultScale; // �chelle des X
  fScaleY := DefaultScale; // �chelle des Y
  fScreen := teWin; // type de champ
  fTurtleVisible := False; // invisible provisoirement
  fSize := TurtleDefaultSize; // taille par d�faut
  fFilled := True; // remplissage par d�faut
  fSpeed := TurtleMaxSpeed shr 1; // vitesse par d�faut
  Home;
  fTempImg.Canvas.Draw(0, 0, Picture.Bitmap); // vide l'image temporaire
  PenDown := True; // �crit
  Canvas.FillRect(Canvas.ClipRect); // vide l'�cran
  TurtleVisible := True; // tortue visible
end;

function TGVTurtle.IsWithinLimits(const X, Y: Integer): Boolean;
// *** coordonn�es dans limites ? ***
begin
  with Canvas.ClipRect do
    Result := (X >= Left) and (Y >= Top) and (X <= Right) and (Y <= Bottom);
end;

procedure TGVTurtle.LineTo(X, Y: Integer);
// *** d�placement en �crivant ***
begin
  Canvas.LineTo(X, cY(Y));
end;

procedure TGVTurtle.Move(const Value: Real);
// *** la tortue se d�place ***
var
  SinT, CosT: Extended;
  TX, TY: Real;
begin
  // calcul du cosinus et du sinus du cap
  SinCos((fHeading - 90) * DgToRad, SinT, CosT);
  // calcul des nouvelles coordonn�es
  TX := fX - Value * SinT * (fScaleX / DefaultScale);
  TY := fY + Value * CosT * (fScaleY / DefaultScale);
  SetPos(Round(TX), Round(TY)); // d�placement si possible
end;

procedure TGVTurtle.MoveTo(X, Y: Integer);
// *** d�placement sans �crire ***
begin
  Canvas.MoveTo(X, cY(Y));
end;

procedure TGVTurtle.Pie(const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine une section d'ellipse ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.Pie(X1, cY(Y1), X2, cY(Y2), X3, cY(Y3), X4, cY(Y4));
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.Pie(const X2, Y2, X3, Y3, X4, Y4: Integer);
// *** dessine une section d'ellipse � l'emplacement de la tortue ***
begin
  Pie(CoordX, CoordY, CoordX + X2, CoordY - Y2, X3, Y3, X4, Y4);
end;

procedure TGVTurtle.Polygon(Points: array of TPoint);
// *** dessine un polygone ***
var
  I: Integer;
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    for I := Low(Points) to High(Points) do // inverse les ordonn�es
      Points[I].Y := cY(Points[I].Y);
    Canvas.Polygon(Points);
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.PolyLine(Points: array of TPoint);
// *** dessine un polygone non couvrant ***
var
  I: Integer;
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    for I := Low(Points) to High(Points) do // inverse les ordonn�es
      Points[I].Y := cY(Points[I].Y);
    Canvas.PolyLine(Points);
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.Rectangle(const X1, Y1, X2, Y2: Integer);
// *** dessine un rectangle ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.Rectangle(X1, cY(Y1), X2, cY(Y2));
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.Rectangle(const X2, Y2: Integer);
// *** dessine un rectangle � l'emplacement de la tortue ***
begin
  Rectangle(CoordX, CoordY, CoordX + X2, CoordY - Y2);
end;

procedure TGVTurtle.ReloadTurtle(const Clean: Boolean);
// *** r�cup�re une tortue ***
begin
  if fSavedTurtle.rSaved then // seulement si une tortue a �t� sauvegard�e
    try
      TurtleVisible := False; // on cache la tortue
      PenDown := False; // on n'�crit pas !
      PenRubber := False; // on n'efface pas !
      PenReverse := False; // on n'inverse pas !
      with fSavedTurtle do // on recharge la tortue
      begin
        fX := rX; // abscisse
        fY := rY; // ordonn�e
        MoveTo(Round(fX), Round(fY)); // on d�place la tortue
        fTurtleKind := rKind; // type de tortue
        Size := rSize; // taille de la tortue
        Heading := rHeading; // direction
        PenRubber := rPenRubber; // drapeau d'effacement
        PenReverse := rPenReverse; // drapeau d'inversion
        ScaleX := rScaleX; // �chelle des X
        ScaleY := rScaleY; // �chelle des Y
        Filled := rFilled; // remplissage
        Canvas.Brush.Assign(rBrush); // type de brosse
        Canvas.Pen.Assign(rPen); // type de crayon
        Canvas.Font.Assign(rFont);
        TurtleVisible := rVisible; // drapeau de visibilit�
        PenDown := rPenDown; // drapeau de crayon baiss�
      end;
    finally
      if Clean then
        with fSavedTurtle do
        begin
          rBrush.Free; // on lib�re la brosse
          rPen.Free; // on lib�re le crayon
          rFont.Free; // on lib�re la fonte
          rSaved := False; // lib�re la sauvegarde
        end;
    end;
end;

procedure TGVTurtle.ToggleTurtlePNG;
// *** montre/cache la tortue png ***
var
  CosT, SinT: Extended;
  X, Y: Integer;
begin
  if fTurtleVisible then // visible ?
  begin
    BeforeChange;
    // calcul des coordonn�es de la tortue
    SinCos((90 + Heading) * DgToRad, SinT, CosT);
    X := Round(CoordX + CosT - SinT);
    Y := Round(CoordY + SinT + CosT);
    // copie de l'�cran
    fTempImg.Assign(Picture.Bitmap);
    // sauvegarde de l'ancienne image
    fOldImg.Assign(Picture.Bitmap);
{$IFDEF Delphi}
    fOldImg.TransparentColor := clWhite;
{$ENDIF}
    // copie de la tortue .png
    with fTempImg do
    begin
{$IFDEF Delphi}
      TransparentColor := clWhite;
{$ENDIF}
      Canvas.Draw(X - (fTurtleImg.Width shr 1),
        cY(Y) - (fTurtleImg.Height shr 1), fTurtleImg);
    end;
    // copie vers l'�cran
    Canvas.Draw(0, 0, fTempImg);
  end
  else
    // on r�tablit l'image sans la tortue
    Canvas.Draw(0, 0, fOldImg);
end;

procedure TGVTurtle.ToggleTurtleTriangle;
// *** montre/cache la tortue triangle ***
var
  CosT, SinT: Extended;
  X1, X2, X3, Y1, Y2, Y3: Integer;
  PenSave: TPen;
begin
  // calcul des coordonn�es des points de la tortue
  SinCos((90 + Heading) * DgToRad, SinT, CosT);
  X1 := Round(CoordX + Size * CosT - SinT);
  Y1 := Round(CoordY + Size * SinT + CosT);
  X2 := Round(CoordX - Size * CosT - SinT);
  Y2 := Round(CoordY - Size * SinT + CosT);
  X3 := Round(CoordX - CosT + (Size shl 1) * SinT);
  Y3 := Round(CoordY - SinT - (Size shl 1) * CosT);
  PenSave := TPen.Create; // sauvegarde et modification du crayon
  try
    PenSave.Assign(Canvas.Pen);
    with Canvas.Pen do
    begin
      Style := psSolid;
      Mode := pmXor; // tortue visible en mode ou exclusif
      MoveTo(X1, Y1); // dessin de la tortue en mode ou exclusif
      Width := 2;
      LineTo(X2, Y2);
      Width := 1;
      LineTo(X3, Y3);
      LineTo(X1, Y1);
      MoveTo(CoordX, CoordY);
      Assign(PenSave); // r�cup�ration du crayon
    end;
  finally
    PenSave.Free; // lib�ration du crayon provisoire
  end;
end;

procedure TGVTurtle.RoundRect(const X1, Y1, X2, Y2: Integer);
// *** dessine un rectangle arrondi ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.RoundRect(X1, cY(Y1), X2, cY(Y2), 15, 15);
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.RoundRect(const X2, Y2: Integer);
// *** dessine un rectangle arrondi � l'emplacement de la tortue ***
begin
  RoundRect(CoordX, CoordY, CoordX + X2, CoordY - Y2);
end;

procedure TGVTurtle.SaveTurtle;
// *** sauvegarde de l'�tat de la tortue ***
begin
  with fSavedTurtle do // on sauvegarde la tortue en cours
  begin
    rX := fX; // abscisse
    rY := fY; // ordonn�e
    rKind := fTurtleKind; // type de tortue
    rSize := fSize; // taille de la tortue
    rVisible := fTurtleVisible; // drapeau de visibilit�
    rHeading := fHeading; // direction
    rPenDown := fPenDown; // drapeau de crayon baiss�
    rPenRubber := fPenRubber; // drapeau d'effacement
    rPenReverse := fPenReverse; // drapeau d'inversion
    rScaleX := fScaleX; // �chelle des X
    rScaleY := fScaleY; // �chelle des Y
    rFilled := Filled; // remplissage
    if not rSaved then // cr�ation si n�cessaire
    begin
      rBrush := TBrush.Create; // type de brosse
      rPen := TPen.Create; // type de crayon
      rFont := TFont.Create; // type de fonte
    end;
    rBrush.Assign(Canvas.Brush); // sauvegarde brosse
    rPen.Assign(Canvas.Pen); // sauvegarde crayon
    rFont.Assign(Canvas.Font); // sauvegarde fonte
    rSaved := True; // drapeau de sauvegarde activ�
  end;
end;

procedure TGVTurtle.SetHeading(const Value: Real);
// *** fixe le cap de la tortue ***
var
  TV: Boolean;
begin
  if (Value <> fHeading) then
  begin
    TV := TurtleVisible; // sauvegarde la tortue
    try
      TurtleVisible := False; // la cache
      fHeading := Frac(Value / 360) * 360; // change le cap
      if fHeading < 0 then
        fHeading := fHeading + 360;
      BeforeChange; // pour le dessin de la tortue
    finally
      TurtleVisible := TV; // restaure la tortue
    end;
  end;
end;

procedure TGVTurtle.SetCoordX(const Value: Integer);
// *** fixe l'abscisse de la tortue ***
begin
  DoGo(Value, CoordY); // d�place la tortue
end;

procedure TGVTurtle.SetCoordY(const Value: Integer);
/// *** fixe l'ordonn�e de la tortue ***
begin
  DoGo(CoordX, Value); // d�place la tortue
end;

procedure TGVTurtle.SetFilled(const Value: Boolean);
// *** fixe le remplissage ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    if Value <> fFilled then
    begin
      fFilled := Value;
      if fFilled then
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := ScreenColor;
      end
      else
        Canvas.Brush.Style := bsClear;
    end;
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.SetTurtleKind(const Value: TTurtleKind);
// *** fixe le type de tortue ***
var
  TV: Boolean;
begin
  if (Value <> fTurtleKind) then
  begin
    TV := TurtleVisible;
    try
      TurtleVisible := False;
      fTurtleKind := Value;
    finally
      TurtleVisible := TV;
    end;
  end;
end;

procedure TGVTurtle.SetPenColor(const Value: TColor);
// *** on change la couleur du crayon ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    fPenColor := Value;
    Canvas.Pen.Color := fPenColor;
  finally
    TurtleVisible := TV;
  end;
end;

procedure TGVTurtle.SetPenDown(const Value: Boolean);
// *** g�re l'�criture du crayon ***
begin
  if (Value <> fPenDown) then
  begin
    fPenDown := Value;
    Change;
  end;
end;

procedure TGVTurtle.SetPenReverse(const Value: Boolean);
// *** inversion du crayon ***
var
  TV: Boolean;
begin
  if (Value <> fPenReverse) then
  begin
    TV := TurtleVisible;
    try
      TurtleVisible := False;
      if Value then
        Canvas.Pen.Mode := pmNot // en mode inversion
      else
        Canvas.Pen.Mode := pmCopy; // en mode copie
      Change; // on signale le changement
    finally
      TurtleVisible := TV;
    end;
  end;
end;

procedure TGVTurtle.SetPos(const X, Y: Integer);
// *** fixe les coordonn�es de la tortue ***
begin
  DoGo(X, Y); // d�placement de la tortue
end;

procedure TGVTurtle.SetRubberPen(const Value: Boolean);
// *** le crayon gomme ***
var
  TV: Boolean;
begin
  if (Value <> fPenRubber) then
  begin
    fPenRubber := Value;
    TV := TurtleVisible;
    try
      TurtleVisible := False;
      with Canvas.Pen do
        if Value then
        begin
          fTempColor := Color; // on se souvient de la couleur
          Color := fScreenColor; // on dessine avec la couleur de fond
        end
        else
          Color := fTempColor; // on restitue la couleur d'origine
      Change; // on signale le changement
    finally
      TurtleVisible := TV;
    end;
  end;
end;

procedure TGVTurtle.SetScreenColor(const Value: TColor);
// *** change  la couleur de fond ***
begin
  if (Value <> fScreenColor) then
  begin
    fScreenColor := Value;
{$IFNDEF Delphi}
    Canvas.Brush.Color := Value;
    Reinit;
{$ENDIF}
    BackGroundChange;
  end;
end;

procedure TGVTurtle.SetSize(const Value: Integer);
// *** la taille de la tortue change   ***
var
  TV: Boolean;
begin
  if (Value <> fSize) and (Kind <> tkPng) then // seulement tortue triangulaire
  begin
    TV := TurtleVisible;
    try
      TurtleVisible := False;
      fSize := Min(Abs(Value), TurtleMaxSize); // normalise sa taille
    finally
      TurtleVisible := TV;
    end;
  end;
end;

procedure TGVTurtle.SetSpeed(const Value: Integer);
// *** vitesse de la tortue ***
begin
  if fSpeed <> Value then
  begin
    fSpeed := Min(Value, TurtleMaxSpeed); // nouvelle vitesse  (maximum = 100)
  end;
end;

procedure TGVTurtle.SetTurtleVisible(const Value: Boolean);
// *** change la visibilit� de la tortue ***
begin
  if (Value <> fTurtleVisible) then
  begin
    fTurtleVisible := Value;
    case fTurtleKind of
      tkPng:
        ToggleTurtlePNG; // tortue PNG
      tkTriangle:
        ToggleTurtleTriangle; // tortue triangulaire
    end;
  end;
  Change; // signale tout changement
end;

procedure TGVTurtle.Square(const L: Integer);
// *** dessine un carr� � l'emplacement de la tortue ***
begin
  Rectangle(CoordX, CoordY, CoordX + L, CoordY - L);
end;

procedure TGVTurtle.Square(const X1, Y1, L: Integer);
// *** dessine un carr� ***
begin
  Rectangle(X1, Y1, X1 + L, Y1 - L);
end;

function TGVTurtle.Towards(const X, Y: Integer): Real;
// *** renvoie le cap vers un point ***
var
  PX, PY: Integer;
begin
  PX := CoordX - X; // calcul des diff�rences entre les points
  PY := Y - CoordY;
  Result := 0; // suppose 0
  // �value suivant les calculs
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

procedure TGVTurtle.Turn(const Value: Real);
// *** la tortue tourne ***
begin
  if Value <> 0 then
    SetHeading(Heading + Value); // tourne vers la gauche
end;

procedure TGVTurtle.Wipe;
// *** nettoyage de l'�cran ***
var
  TV: Boolean;
begin
  TV := TurtleVisible;
  try
    TurtleVisible := False;
    Canvas.FillRect(Canvas.ClipRect); // vide l'�cran
  finally
    TurtleVisible := TV;
  end;
end;

end.