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
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    fDeepTrace: Boolean; // trace approfondie
    fWaitForKey: Boolean; // attente d'une touche
    function GetColor: string; // couleur caractères
    function GetBackColor: string; // couleur fond caractères
    function SetAColor(const St: string): TColor; // couleur convertie
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
end;

procedure TMainForm.btnDeepTraceClick(Sender: TObject);
// trace approfondie
begin
  fDeepTrace := not fDeepTrace;
  Automat.Follow := fDeepTrace;
end;

procedure TMainForm.btnGoClick(Sender: TObject);
// interprétation
begin
  Automat.Clear; // nettoyage
  btnGo.Enabled := False; // bouton go inactif
  try
    Automat.Process(CBeginList + lbledtMain.Text + CEndList); // ligne à exécuter
  finally
    btnGo.Enabled := True; // bouton go actif
  end;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
// bouton stop
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
    mmoMain.Lines.Add('// A présent, je connais : ' +
      Automat.Kernel.ProcsToList);
  end;
end;

procedure TMainForm.btnTraceClick(Sender: TObject);
// trace active/inactive
begin
  Automat.Follow := not Automat.Follow;
  if not Automat.Follow then
    fDeepTrace := False;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  Automat := TGVAutomat.Create; // automate créé
  Automat.OnStateChange := @GetStateChange; // état changé
  Automat.Error.OnError := @GetError; // gestionnaire d'erreurs
  Automat.Message.OnMessageChange := @GetMessage; // gestionnaire de messages
  Automat.Follow := False; // pas de trace par défaut
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

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
// frappe d'une touche
begin
  if fWaitForKey then
  begin
    Automat.Message.Message := Key;
    fWaitForKey := False;
  end;
end;

function TMainForm.GetColor: string;
// couleur caractères
var
  Li: Integer;
  Lr: Integer;
begin
  Lr := -1;
  for Li := 0 to 19 do
    if CColors[Li] = mmoMain.Font.Color then // trouvée ?
    begin
      Lr := Li; // couleur enregistrée
      Break; // on arrête de boucler
    end;
  Result := IntToStr(Lr);
end;

function TMainForm.GetBackColor: string;
// couleur de fond du texte
var
  Li: Integer;
  Lr: Integer;
begin
  Lr := -1;
  for Li := 0 to 19 do
    if CColors[Li] = mmoMain.Color then // trouvée ?
    begin
      Lr := Li; // couleur enregistrée
      Break; // on arrête de boucler
    end;
  Result := IntToStr(Lr);
end;

function TMainForm.SetAColor(const St: string): TColor;
// conversion d'une couleur
var
  N: Integer;
begin
  N := StrToInt(St);
  if (N < 0) or (N > 19) then
    Result := clBlack // couleur noire par défaut
  else
    Result := CColors[N]; // renvoi de la couleur
end;

procedure TMainForm.GetStateChange(Sender: TObject);
// gestion du changement d'état
var
  LS: string;
  Li: Integer;
begin
  case Automat.State of
    asWord, asList, asVar, asNumber, asEval, asPushing, asProc,
    asPrim, asPrimValue: LS := Automat.Datas.fItem;
    asExePrim, asPrimDone: LS := Automat.Datas.fPrim;
    asExeProc, asProcDone: LS := Automat.Datas.fProc;
    asWaiting: btnGo.Enabled := True;
  else
    LS := EmptyStr;
  end;
  if Automat.Follow then
    with mmoMain.Lines do
    begin
      for Li := 1 to Automat.Datas.fLevel do
        Add(Format('// [%d]'+ CStatesArray[Automat.State] +
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
  case Automat.Message.Cmd of
    // écriture
    acWrite: mmoMain.Lines.Add(Automat.Message.Message);
    // nettoyage
    acClear: mmoMain.Lines.Clear;
    // lecture d'une liste
    acReadList: Automat.Message.Message := InputBox('TestGVAutomat',
    'Entrez la valeur demandée ici :', EmptyStr);
    // demande de confirmation
    acConfirm: if MessageDlg(Automat.Message.Message , mtConfirmation,
      mbYesNo, 0) = mrYes then
        Automat.Message.Message := CStTrue // vrai en retour
      else
        Automat.Message.Message := CStFalse; // faux en retour
    // écriture sans retour chariot
    acType: with mmoMain do
      Lines[Lines.Count-1] := Lines[Lines.Count-1] + Automat.Message.Message;
    // lecture d'un caractère
    acReadChar: begin
                  fWaitForKey := True;
                  while fWaitForKey do
                    Application.ProcessMessages;
    end;
    acBold: mmoMain.Font.Style := mmoMain.Font.Style + [fsBold];
    acUnderline: mmoMain.Font.Style := mmoMain.Font.Style + [fsUnderline];
    acItalic: mmoMain.Font.Style := mmoMain.Font.Style + [fsItalic];
    acNoBold: mmoMain.Font.Style := mmoMain.Font.Style - [fsBold];
    acNoUnderline: mmoMain.Font.Style := mmoMain.Font.Style - [fsUnderline];
    acNoItalic: mmoMain.Font.Style := mmoMain.Font.Style - [fsItalic];
    acColor: Automat.Message.Message := GetColor;
    acBackColor: Automat.Message.Message := GetBackColor;
    acSetColor: mmoMain.Font.Color := SetAColor(Automat.Message.Message);
    acSetBackColor: mmoMain.Color := SetAColor(Automat.Message.Message);
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

