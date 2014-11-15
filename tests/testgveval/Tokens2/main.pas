{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Eléments d'une expression               |
  |                  Unité : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    06-11-2014 09:14:46                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// GVTokens2 - part of GVLOGO
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

unit Main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  GVConsts, GVTokens2;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Toks: TGVTokens2;
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

procedure TMainForm.Button1Click(Sender: TObject);
// éléments de la chaîne de l'éditeur
var
  Item: TGVBaseItem;
begin
  Memo1.Clear; // on nettoie le mémo
  Toks.Text := Edit1.Text; // texte en place
  Toks.Tokenize;  // valeur évaluée
  if Toks.Error = C_None then // pas d'erreur ?
  begin
    Memo1.Lines.Add(EmptyStr);
    Memo1.Lines.Add('*** LISTE des ELEMENTS ***');
    Memo1.Lines.Add(EmptyStr);
    for Item in Toks do  // on énumère
      Memo1.Lines.Add(Item.Token); // éléments dans le mémo
  end;
  Label3.Caption := IntToStr(Ord(Toks.Indx)); // position en fin d'analyse
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fenêtre
begin
  Toks := TGVTokens2.Create; // objet créé
  Toks.OnGetVar:= @GetVar; // gestionnaire d'événement pour les variables
  Toks.OnError := @GetError; // gestionnaire d'erreurs
  Toks.OnChange := @GetChange; // gestionnaire de changement
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fenêtre
begin
  Toks.Free; // libération de l'objet
end;

procedure TMainForm.GetVar(Sender: TObject; VarName: string; var Value: Double;
  var Error: TGVError);
// événement concernant les variables *** essai ***
begin
  Value := 100; // une seule valeur
  if VarName = 'var1' then
    Error := C_None
  else
    Error := C_UnKnownVar;
end;

procedure TMainForm.GetError(Sender: TObject);
// gestionnaire d'erreurs
begin
  with Toks do
  begin
    Memo1.Lines.Add('< *** ERREUR *** :   ' + Item[Count].Token + ' > Position : '
   + IntToStr(Indx)); // la base est de 1
    Memo1.Lines.Add('Message : ' + Format(GVErrorName[Error],
     [Item[Count].Token]));
  end;
end;

procedure TMainForm.GetChange(Sender: TObject);
// gestionnaire de changement
const
  TypArray: array[CTokensEnum] of string = ('Integer', 'Real', 'Var',
    'Function', 'BeginExp', 'EndExp', 'Plus', 'Minus', 'Mul',
    'Div', 'Power', 'Greater', 'Lower', 'Equal', 'NotEqual',
    'GreaterOrEqual', 'LowerOrEqual', 'Mod', 'And', 'Or',
    'Not', 'End', 'OrB', 'AndB', 'Boolean', 'UnKnown',
    'Forbidden');
begin
  with Toks do // la base est de 1
    Memo1.Lines.Add('< AJOUT : ' + Format('%-15s',[Item[Count].Token]) +
      ' --> ' + Format('%15s',[TypArray[Item[Count].Kind]]) + '  <--');
end;

end.

