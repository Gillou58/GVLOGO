{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'automate d'interprétation     |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVAUTOMAT - part of GVLOGO
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

unit main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, Buttons, ExtCtrls,
  GVErrConsts, // erreurs
  GVConsts, // constantes
  GVPrimConsts, // primitives
  GVTurtles, // tortue
  GVAutomat; // automate

type

  { TMainForm }

  TMainForm = class(TForm)
    btnStop: TBitBtn;
    btnToProc: TBitBtn;
    btnDeepTrace: TBitBtn;
    btnTrace: TBitBtn;
    btnGo: TBitBtn;
    btnClear: TBitBtn;
    btnClose: TBitBtn;
    iTurtle: TImageList;
    imgTurtle: TImage;
    lbledtMain: TLabeledEdit;
    mmoMain: TMemo;
    pnlTurtle: TPanel;
    sbMain: TStatusBar;
    procedure btnClearClick(Sender: TObject);
    procedure btnDeepTraceClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnToProcClick(Sender: TObject);
    procedure btnTraceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fTrace: Boolean; // drapeau de trace
    fDeepTrace: Boolean; // trace approfondie
  public
    Automat: TGVAutomat; // automate
    GVTurtle: TGVTurtle; // tortue
    // gestionnaire de changement d'état
    procedure GetStateChange(Sender: TObject);
    // gestionnaire des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
    // gestionnaire des messages
    procedure GetMessage(Sender: TObject);
    // gestionnaire de tortue
    procedure TurtleState(Sender: TObject);
    // dessin de la tortue
    procedure TurtleBeforePaint(Sender: TObject; cHeading: Integer);
  end;

var
  MainForm: TMainForm;

implementation

uses
  StrUtils,
  GVErrors; // erreurs

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoMain.Lines.Clear; // éditeur nettoyé
  lbledtMain.Text := EmptyStr; // ligne d'édition nettoyée
  GVTurtle.ReInit; // tortue réinitialisée
  GVTurtle.Kind := tkPng; // tortue image
  btnGo.Enabled := True; // bouton go actif
end;

procedure TMainForm.btnDeepTraceClick(Sender: TObject);
// trace approfondie
begin
  fDeepTrace := not fDeepTrace;
  fTrace := fDeepTrace;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
// interprétation
begin
  Automat.Clear; // nettoyage
  Automat.Process(CBeginList + lbledtMain.Text + CEndList); // ligne à exécuter
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  Automat.Stop := True; // arrêt demandé
end;

procedure TMainForm.btnToProcClick(Sender: TObject);
// vers les procédures
var
  Err: Integer;
begin
  if Automat.Kernel.EditToProc(mmoMain.Lines, 0, 0, Err) then
  begin
    mmoMain.Lines.Add('// Editeur analysé...');
    mmoMain.Lines.Add('// A présent, je connais : ' + Automat.Kernel.ProcsToList);
  end;
end;

procedure TMainForm.btnTraceClick(Sender: TObject);
// trace active/inactive
begin
  fTrace := not fTrace;
  if not fTrace then
    fDeepTrace := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  Automat := TGVAutomat.Create; // automate créé
  Automat.OnStateChange := @GetStateChange; // état changé
  Automat.Error.OnError := @GetError; // gestionnaire d'erreurs
  Automat.OnNewLine := @GetMessage; // gestionnaire de messages
  fTrace := False; // pas de trace par défaut
  fDeepTrace := False;
  // on crée la tortue
  GVTurtle := TGVTurtle.Create(imgTurtle.Width, imgTurtle.Height);
  Automat.Turtle := GVTurtle; // tortue liée à l'automate
  GVTurtle.OnChange := @TurtleState;  // gestionnaire de changement
  GVTurtle.OnBeforeChange := @TurtleBeforePaint; // idem avant de dessiner
  GVTurtle.Error.OnError := @GetError; // gestionnaire d'erreurs
  GVTurtle.ReInit; // initialisation
  GVTurtle.Kind := tkPng; // tortue image
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  Automat.Free; // automate libéré
  GVTurtle.Free;
end;

procedure TMainForm.GetStateChange(Sender: TObject);
// gestion du changement d'état
const
  StateArray: array[TGVAutomatState] of string =  ('Attente %s',
    'On termine', 'Au travail', 'On a rencontré l''erreur : %s',
    'On traite le mot %s', 'On traite la liste %s',
    'On traite la variable %s', 'On traite le nombre %s',
    'On traite l''expression %s', 'On traite la procédure %s',
    'On traite la primitive %s', 'On empile %s',
    'On s''est arrêté %s', 'On exécute la primitive %s',
    'On exécute la procédure %s',
    'On se prépare %s', 'La procédure %s a été exécutée ',
    'La primitive %s a été exécutée', 'STOP demandé %s',
    'Le résultat %s a été obtenu');
var
  LS: string;
  Li: Integer;
begin
  btnGo.Enabled:= False; // bouton pendant le travail
  case Automat.State of
    asWord, asList, asVar, asNumber, asEval, asPushing, asProc,
    asPrim, asPrimValue: LS := Automat.Datas.fItem;
    asExePrim, asPrimDone: LS := Automat.Datas.fPrim;
    asExeProc, asProcDone: LS := Automat.Datas.fProc;
    asWaiting: btnGo.Enabled := True;
  else
    LS := EmptyStr;
  end;
  if fTrace then
    with mmoMain.Lines do
    begin
      for Li := 1 to Automat.Datas.fLevel do
        Add(Format('// [%d]'+ StateArray[Automat.State] +
          '...', [Li, LS])); // état affiché
    end;
   if fDeepTrace and (not (Automat.State in [asWaiting, asPreparing,
        asEnding]))then  // trace ?
     with mmoMain.Lines do
       begin
         Add('// Ligne : ' + Automat.Datas.fLine);
         Add('// Donnée : ' + Automat.Datas.fItem);
         Add('// Numéro : ' + IntToStr(Automat.Datas.fNum));
         Add('// Primitive : ' + Automat.Datas.fPrim);
         Add('// Procédure : ' + Automat.Datas.fProc);
         Add('// Niveau : ' + IntToStr(Automat.Datas.fLevel));
         Add('');
       end;
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// gestionnaire d'erreurs
begin
  // message en toutes lettres
  mmoMain.Lines.Add('// >>> ' + (Sender as TGVErrors).ErrorMessage);
  with mmoMain.Lines, ErrorRec do
  begin
    Add('// Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('// Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('// Position : ' + IntToStr(ErrPos)); // position de l'erreur
    Add('');
  end;
end;

procedure TMainForm.GetMessage(Sender: TObject);
// gestionnaire des messages
begin
  case Automat.Message.fCommand of
    acWrite: mmoMain.Lines.Add(Automat.Message.fMessage);
    acClear: mmoMain.Lines.Clear;
  end;
end;

procedure TMainForm.TurtleState(Sender: TObject);
// état de la tortue
begin
  GVTurtle.TurtleBitmap.Draw(imgTurtle.Canvas,0,0);
  imgTurtle.Invalidate;
  // données de la tortue
  with GVTurtle do
    sbMain.Panels[1].Text := Format('X: %.3d Y: %.3d Cap: %.3d',
      [Round(CoordX), Round(CoordY), Round(Heading)]) +
      ' Visible: ' + IfThen(TurtleVisible, MF_True, MF_False) +
      ' Baissé: ' + IfThen(PenDown, MF_True, MF_False);
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
    iTurtle.GetBitmap(Round(cHeading) div 5, BitM);
    // celle qui correspond est assignée au bitmap
    GVTurtle.PNGTurtle.Assign(BitM);
  finally
    BitM.Free;
  end;
end;

end.

