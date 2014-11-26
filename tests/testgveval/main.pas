{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de GVEval                          |
  |                  Unité : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    28-11-2014 10:47:57                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


// GVEval - part of GVLOGO
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

unit Main;

{$IFNDEF Delphi}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GVConsts, GVEval;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Compute: TGVEval;
    procedure GetVar(Sender: TObject; VarName: string; var
    Value: Double; var Error: TGVError); // gestionnaire de variables
    procedure GetError(Sender: TObject); // gestionnaire d'erreurs
    procedure GetChange(Sender: TObject); // gestionnaire de changement
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fenêtre
begin
  Compute := TGVEval.Create; // objet créé
  Compute.OnGetVar:= @GetVar; // gestionnaire d'événement pour les variables
  Compute.OnError := @GetError; // gestionnaire d'erreurs
  Compute.OnChange := @GetChange; // gestionnaire de changement
end;

procedure TMainForm.Button1Click(Sender: TObject);
// éléments de la chaîne de l'éditeur
var
  Item: TGVBaseItem;
  I: Integer;
begin
  Memo1.Clear;
  Compute.Text := Edit1.Text; // texte en place
  Memo1.Lines.Add(Format('*** ANALYSE de : %s', [Edit1.Text]));
  Memo1.Lines.Add('___________________________________');
  Memo1.Lines.Add(EmptyStr);
  Compute.Scan;  // valeur évaluée
  if Compute.Error = C_None then // pas d'erreur ?
  begin
    Memo1.Lines.Add(EmptyStr);
    Memo1.Lines.Add('*** LISTE des éléments ***');
    Memo1.Lines.Add('__________________________');
    Memo1.Lines.Add(EmptyStr);
    for Item in Compute do  // on énumère
      Memo1.Lines.Add(Item.Token); // éléments dans le mémo
  end;
  Memo1.Lines.Add(EmptyStr);
  Memo1.Lines.Add('*** LISTE des éléments après scan ***');
  Memo1.Lines.Add('_____________________________________');
  Memo1.Lines.Add(EmptyStr);
  for I := 1 to Compute.ScanCount do
  begin
    Memo1.Lines.Add(Compute.ScanItem[I].Token);
  end;
 Memo1.Lines.Add(EmptyStr);
 Memo1.Lines.Add(Format('*** Résultat : %2f', [Compute.Res]));
 Memo1.Lines.Add('___________________________________');
 Memo1.Lines.Add(EmptyStr);
end;

procedure TMainForm.GetVar(Sender: TObject; VarName: string; var Value: Double;
  var Error: TGVError);
// événement concernant les variables *** essai 3 variables seulement ***
begin
  Value := 100* random(5); // valeur au hasard
  if (VarName = 'var1') or  (VarName = 'var2') or (VarName = 'var3') then
    Error := C_None
  else
    Error := C_UnKnownVar;
end;

procedure TMainForm.GetError(Sender: TObject);
// gestionnaire d'erreurs
begin
  with Compute do
  begin
    Memo1.Lines.Add('< *** ERREUR *** :   ' + ActualItem);
    Memo1.Lines.Add('Message : ' + Format(GVErrorName[Error],
     [ActualItem]));
  end;
end;

procedure TMainForm.GetChange(Sender: TObject);
// gestionnaire de changement
const
  TypArray: array[CTokensEnum] of string = ('Integer', 'Real', 'Var',
    'Function', 'BeginExp', 'EndExp', 'Plus', 'Minus', 'Mul',
    'Div', 'Power', 'Greater', 'Lower', 'Equal', 'NotEqual',
    'GreaterOrEqual', 'LowerOrEqual', 'Mod', 'Not', 'And', 'Or',
    'End', 'OrB', 'AndB', 'Boolean', 'UnKnown',
    'Forbidden', 'NotSupported', 'UnaryMinus', 'UnaryPlus');
  StateArray: array[TGVEvalState] of string = ('Waiting', 'Tokenizing',
    'Scanning', 'Computing', 'NoInit');
begin
  with Compute do // la base est de 1
  begin
    if (Count <> 0) and (State = esTokenizing) then
    begin
      Memo1.Lines.Add('< AJOUT : ' + Item[Count].Token);
      Memo1.Lines.Add(' --> ' + TypArray[Item[Count].Kind]);
      Memo1.Lines.Add(' Priorité: ' + IntToStr(CTokenPrecedence[Item[Count].Kind]) +
        ' Association : ' + IntToStr(CTokenAssociation[Item[Count].Kind]));
    end
    else
      Memo1.Lines.Add('Etat : ' + StateArray[State]);
    Memo1.Lines.Add(EmptyStr);
  end;
end;

end.

