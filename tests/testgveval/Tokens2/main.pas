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
  private
    { private declarations }
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
  Toks: TGVTokens2;
  Item: TGVBaseItem;
begin
  Memo1.Clear; // on nettoie le mémo
  Toks := TGVTokens2.Create(Edit1.Text); // objet créé
  try
    if Toks.Error = C_None then // pas d'erreur ?
      for Item in Toks do  // on énumère
        Memo1.Lines.Add(Item.Token); // éléments dans le mémo
    Label3.Caption := IntToStr(Toks.ErrorPos); // position d'une erreur
  finally
    Toks.Free;  // libération de l'objet
  end;
end;

end.

