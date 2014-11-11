{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : El�ments d'une expression               |
  |                  Unit� : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : � G. VASSEUR                              |
  |                  Date:    11-11-2014 09:14:46                          |
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

unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, GVConsts, GVTokens2, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
// �l�ments de la cha�ne de l'�diteur
var
  Toks: TGVTokens2;
  Item: TGVBaseItem;
begin
  Memo1.Clear; // on nettoie le m�mo
  Toks := TGVTokens2.Create(Edit1.Text); // objet cr��
  try
    if Toks.Error = C_None then // pas d'erreur ?
      for Item in Toks do  // on �num�re
        Memo1.Lines.Add(Item.Token); // �l�ments dans le m�mo
    Label1.Caption := IntToStr(Ord(Toks.Error)); // position d'une erreur
  finally
    Toks.Free;  // lib�ration de l'objet
  end;
end;

end.
