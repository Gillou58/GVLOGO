{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de l'unité GVEval                  |
  |                  Unité : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVEVAL - part of GVLOGO
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

unit Main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons,
  GVConsts, // constantes communes
  GVEval, // évaluation
  GVKernel, // noyau (variables)
  GVErrConsts; // constantes des erreurs

type

  { TMainForm }

  TMainForm = class(TForm)
    btnClose: TBitBtn;
    btnGo: TBitBtn;
    btnClear: TBitBtn;
    edtExp: TEdit;
    mmoEval: TMemo;
    mmoState: TMemo;
    sbMain: TStatusBar;
    procedure btnClearClick(Sender: TObject);
    procedure btnTokenizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Compute: TGVEval; // évaluation
    Kernel: TGVLogoKernel; // noyau (variables)
    // traitement des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
    procedure GetChange(Sender: TObject); // gestionnaire de changement
    procedure GetStateChange(Sender: TObject); // gestionnaire de changement d'état
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  StateArray: array[TGVEvalState] of string = ('Attente', 'Découpage',
    'Analyse', 'Calcul', 'Non initialisé', 'Calcul effectué');

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  Kernel := TGVLogoKernel.Create; // noyau créé
  // on crée des variables
  Kernel.AddVar('var1', '123');
  Kernel.AddVar('var2', '45,56');
  Kernel.AddVar('var3', 'plouf');
  Compute := TGVEval.Create; // objet créé
  Compute.Kernel := Kernel;
  Compute.Error.OnError := @GetError; // gestionnaire d'erreurs
  Compute.OnChange := @GetChange; // gestionnaire de changement
  Compute.OnStateChange := @GetStateChange; // gestionnaire de changement d'état
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  Kernel.Free; // noyau libéré
  Compute.Free; // évaluateur aussi
end;

procedure TMainForm.btnTokenizeClick(Sender: TObject);
// éléments de la chaîne de l'éditeur
var
  LItem: TGVBaseItem;
  Li: Integer;
begin
  mmoEval.Clear;
  mmoState.Lines.Add('*********');
  Compute.Text := edtExp.Text; // texte en place
  mmoEval.Lines.Add(Format('<<<<< EVALUATION de : %s >>>>>', [edtExp.Text]));
  Compute.Scan;  // valeur évaluée
  if Compute.Error.OK then // pas d'erreur ?
  begin
    mmoEval.Lines.Add('<<< LISTE des éléments >>>');
    for LItem in Compute do  // on énumère
      mmoEval.Lines.Add(LItem.Token); // éléments dans le mémo
    mmoEval.Lines.Add('<<< LISTE des éléments après scan >>>');
    for Li := 1 to Compute.ScanCount do
      mmoEval.Lines.Add(Compute.ScanItem[Li].Token);
    mmoEval.Lines.Add(Format('<<< Résultat : %2f >>>', [Compute.Res]));
  end;
  mmoEval.Lines.Add('___________________________________');
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// nettoyage de l'éditeur
begin
  mmoEval.Lines.Clear;
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// gestionnaire d'erreurs
begin
  // message en toutes lettres
  mmoEval.Lines.Add('>>> ' + Compute.Error.ErrorMessage);
  with mmoEval.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    Add('Elément : ' + ErrItem); // élément fautif dans la ligne de travail
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
end;

procedure TMainForm.GetChange(Sender: TObject);
// gestionnaire de changement
const
  TypArray: array[CTokensEnum] of string = ('Integer', 'Real', 'Var',
    'Function', 'BeginExp', 'EndExp', 'Plus', 'Minus', 'Mul',
    'Div', 'Power', 'Greater', 'Lower', 'Equal', 'NotEqual',
    'GreaterOrEqual', 'LowerOrEqual', 'Mod', 'Not', 'And', 'Or',
    'OrB', 'AndB', 'Boolean', 'UnKnown',
    'Forbidden', 'NotSupported', 'UnaryMinus', 'UnaryPlus');

begin
  with Compute do // la base est de 1
  begin
    case State of
      esWaiting:
        mmoEval.Lines.Add('Expression à évaluer: ' + Text);
      esTokenizing: if (Count <> 0) then
        begin
          mmoEval.Lines.Add(Format('< %s: %s ---> %s',[StateArray[State],
            Item[Count].Token, TypArray[Item[Count].Kind]]));
          mmoEval.Lines.Add(' Priorité: ' +
            IntToStr(CTokenPrecedence[Item[Count].Kind]) +
              ' --- Association : ' +
                IntToStr(CTokenAssociation[Item[Count].Kind]));
        end;
      esScanning, esComputing: mmoEval.Lines.Add(Format('%s de l''élément : %s',
        [StateArray[State], ActualItem]));
      esOk: mmoEval.Lines.Add(Format('Résultat : %f', [Res]));
   end;
  end;
end;

procedure TMainForm.GetStateChange(Sender: TObject);
// gestionnaire de changement d'état
begin
  mmoState.Lines.Add(StateArray[Compute.State]);
end;

end.

