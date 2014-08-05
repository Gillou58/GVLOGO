{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo micro-logiciel EASYTURTLE             |
  |                  Description : fiche d'aide                            |
  |                  Unité : Help.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    02-08-2014 12:29:48                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// TestGVWords - part of GVLOGO
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
  
unit Help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, ComCtrls, StdCtrls;

type

  { THelpForm }

  THelpForm = class(TForm)
    btnClose: TBitBtn;
    Image1: TImage;
    ImageCmds: TImage;
    ImageCmds2: TImage;
    ImageTurtle: TImage;
    ImageScreen: TImage;
    ImageState: TImage;
    ImageKind: TImage;
    ImageColors: TImage;
    ImageShapes: TImage;
    ImageStatements: TImage;
    ImageTools: TImage;
    ImageSize: TImage;
    ImageArrows: TImage;
    ImageToolBar: TImage;
    ImageHelp: TImage;
    lblAbout: TLabel;
    lblWTools: TLabel;
    lblUndo: TLabel;
    lblHelp: TLabel;
    lblQuit: TLabel;
    lblReplay: TLabel;
    lblLoad: TLabel;
    lblSave: TLabel;
    lblReset: TLabel;
    lblCKind: TLabel;
    lblCPenColor: TLabel;
    lblCBackGround: TLabel;
    lblCSquare: TLabel;
    lblCCircle: TLabel;
    lblTForward: TLabel;
    lblTSeeSaw: TLabel;
    lblTBacward: TLabel;
    lblTLeft: TLabel;
    lblTRight: TLabel;
    lblTBigger: TLabel;
    lblTSmaller: TLabel;
    lblTErase: TLabel;
    lblTHome: TLabel;
    lblTUpDown: TLabel;
    lblScreen: TLabel;
    lblState: TLabel;
    lblKind: TLabel;
    lblColors: TLabel;
    lblShapes: TLabel;
    lblStatements: TLabel;
    lblTools: TLabel;
    lblSize: TLabel;
    lblArrows: TLabel;
    PageControlHelp: TPageControl;
    TabSheetToolBar: TTabSheet;
    TabSheetTurtle: TTabSheet;
    TabSheetColors: TTabSheet;
    TabSheetStatements: TTabSheet;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ImageArrowsMouseEnter(Sender: TObject);
    procedure lblTForwardMouseEnter(Sender: TObject);
    procedure lblTForwardMouseLeave(Sender: TObject);
  private
    { private declarations }
    procedure TagChange(tg: Integer); // changement suivant tag
  public
    { public declarations }
  end;

var
  HelpForm: THelpForm;

implementation

{$R *.lfm}

{ THelpForm }

procedure THelpForm.ImageArrowsMouseEnter(Sender: TObject);
// taille change pendant passage
begin
  with (Sender as TImage) do
  begin
    Stretch := not Stretch; // on étire ou non
    TagChange(Tag); // changement suivant le tag
  end;
end;

procedure THelpForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
// le dessin suit la souris...
begin
  ImageHelp.Left := X;
end;

procedure THelpForm.lblTForwardMouseEnter(Sender: TObject);
// couleur change avec souris
begin
  with (Sender as TLabel).Font do
    Style := Style + [fsBold];
end;

procedure THelpForm.lblTForwardMouseLeave(Sender: TObject);
// couleur revient avec souris
begin
  with (Sender as TLabel).Font do
    Style := Style - [fsBold];
end;

procedure THelpForm.TagChange(tg: Integer);
// changement sur le tag
begin
  case Tg of
      0: if fsBold in lblArrows.Font.Style then
           lblArrows.Font.Style := lblArrows.Font.Style - [fsBold]
         else
           lblArrows.Font.Style := lblArrows.Font.Style + [fsBold];
      1: if fsBold in lblSize.Font.Style then
           lblSize.Font.Style := lblSize.Font.Style - [fsBold]
         else
           lblSize.Font.Style := lblSize.Font.Style + [fsBold];
      2: if fsBold in lblScreen.Font.Style then
           lblScreen.Font.Style := lblScreen.Font.Style - [fsBold]
         else
           lblScreen.Font.Style := lblScreen.Font.Style + [fsBold];
      3: if fsBold in lblState.Font.Style then
           lblState.Font.Style := lblState.Font.Style - [fsBold]
         else
           lblState.Font.Style := lblState.Font.Style + [fsBold];
      4: if fsBold in lblKind.Font.Style then
           lblKind.Font.Style := lblKind.Font.Style - [fsBold]
         else
           lblKind.Font.Style := lblKind.Font.Style + [fsBold];
      5: if fsBold in lblColors.Font.Style then
           lblColors.Font.Style := lblColors.Font.Style - [fsBold]
         else
           lblColors.Font.Style := lblColors.Font.Style + [fsBold];
      6: if fsBold in lblShapes.Font.Style then
           lblShapes.Font.Style := lblShapes.Font.Style - [fsBold]
         else
           lblShapes.Font.Style := lblShapes.Font.Style + [fsBold];
      7: if fsBold in lblStatements.Font.Style then
           lblStatements.Font.Style := lblStatements.Font.Style - [fsBold]
         else
           lblStatements.Font.Style := lblStatements.Font.Style + [fsBold];
      8: if fsBold in lblTools.Font.Style then
           lblTools.Font.Style := lblTools.Font.Style - [fsBold]
         else
           lblTools.Font.Style := lblTools.Font.Style + [fsBold];
      end;
end;

end.

