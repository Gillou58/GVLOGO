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
    for Item in Toks do  // on énumère
      Memo1.Lines.Add(Item.Token); // éléments dans le mémo
  Label3.Caption := IntToStr(Ord(Toks.Indx)); // position en fin d'analyse
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fenêtre
begin
  Toks := TGVTokens2.Create; // objet créé
  Toks.OnGetVar:= @GetVar; // gestionnaire d'événement pour les variables
  Toks.OnError := @GetError; // gestionnaire d'erreurs
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fenêtre
begin
  Toks.Free;
end;

procedure TMainForm.GetVar(Sender: TObject; VarName: string; var Value: Double;
  var Error: TGVError);
// événement concernant les variables *** essai ***
begin
  Value := 100;
  Error := C_None;
end;

procedure TMainForm.GetError(Sender: TObject);
// gestionnaire d'erreurs
begin
  Memo1.Lines.Add('< ERREUR : ' + Toks.Item + '>');
end;

end.

