{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo micro-logiciel EASYTURTLE             |
  |                  Description : fiche des préférences                   |
  |                  Unité : GVTools.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    02-08-2014 12:29:48                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// EasyTurtle - part of GVLOGO
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

unit GVTools;

{$mode objfpc}{$H+}

// Cette unité permet de modifier les propriétés suivantes :
//property pForward: Integer read fForward write fForward default C_Forward;
//property pBackward: Integer read fBackward write fBackward default C_Backward;
//property pLeft: Integer read fLeft write fLeft default C_Left;
//property pRight: Integer read fRight write fRight default C_Right;
//property pLength: Integer read fLength write fLength default C_Length;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, Spin;

type

  { TFormTools }

  TFormTools = class(TForm)
    btnReInit: TBitBtn;
    btnCancel: TBitBtn;
    btnClose: TBitBtn;
    gbTurtle: TGroupBox;
    lblForward: TLabel;
    lblBackward: TLabel;
    lblLeft: TLabel;
    lblRight: TLabel;
    lblLength: TLabel;
    seForward: TSpinEdit;
    seBackward: TSpinEdit;
    seLeft: TSpinEdit;
    seRight: TSpinEdit;
    seLength: TSpinEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnReInitClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormTools: TFormTools;

implementation

{$R *.lfm}

uses Main;

{ TFormTools }

procedure TFormTools.btnReInitClick(Sender: TObject);
// réinitialisation
begin
  FormActivate(Sender);
end;

procedure TFormTools.FormActivate(Sender: TObject);
// mise à jour des données
begin
  with MainForm do
  begin
    seForward.Value := pForward;
    seBackward.Value := pBackward;
    seLeft.Value := pLeft;
    seRight.Value := pRight;
    seLength.Value := pLength;
  end;
end;

procedure TFormTools.btnCloseClick(Sender: TObject);
// on enregistre les nouvelles données
begin
  with MainForm do
  begin
    pForward := seForward.Value;
    pBackward := seBackward.Value;
    pLeft := seLeft.Value;
    pRight := seRight.Value;
    pLength := seLength.Value;
  end;
end;

end.

