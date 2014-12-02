{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : test de l'intepréteur                   |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    01-12-2014 07:40:23                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// testgvinterpreter - part of GVLOGO
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

unit main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, ExtCtrls, StdCtrls,
  GVInterpreter, GVConsts
  ;

type
  { TMainForm }
  TMainForm = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    procedure Change(Sender: TObject); // gestionnaire de changement
    procedure Error(Sender: TObject); // gestionnaire de changement
  public
    { public declarations }
    Interpreter: TGVInterpreter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  Interpreter := TGVInterpreter.Create; // création de l'interpréteur
  Interpreter.OnChange := @Change; // gestionnaire affecté;
  Interpreter.OnError := @Error; // erreur;
end;

procedure TMainForm.Button1Click(Sender: TObject);
// demande d'interprétation
begin
  Memo1.Lines.Add(EmptyStr);
  Interpreter.Error := C_None;
  Interpreter.Lines := Memo1.Lines;
  Interpreter.ComputeLine(Edit1.Text);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Interpreter.Free; // libération de l'interpréteur
end;

procedure TMainForm.Change(Sender: TObject);
// gestionnaire de changement
begin
  //with Interpreter do
  //  Memo1.Lines.Add(Format('>> Elément:  %s Erreur: %d',
  //    [ActualItem, Ord(Error)]));
end;

procedure TMainForm.Error(Sender: TObject);
// gestionnaire d'erreur
begin
  with Interpreter do
  begin
    //Memo1.Lines.Add(Format('>>> ERREUR <<< Elément:  %s', [ActualItem]));
    Memo1.Lines.Add(Format('>>> ERREUR <<< ' + GVErrorName[Error], [ActualItem]));
  end;

end;

end.

