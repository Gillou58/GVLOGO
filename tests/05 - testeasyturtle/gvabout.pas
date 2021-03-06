{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : boîte "à propos" pour EasyTurtle        |
  |                  Unité : GVAbout.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// GVABOUT - part of GVLOGO
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

{$I GVDefines.inc}
  
unit GVAbout;

//
// Cette unité fournit la boîte "à propos" du logiciel EasyTurtle.
//

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    btnExit: TBitBtn;
    ImageLazarus: TImage;
    ImageList: TImageList;
    ImageTurtle: TImage;
    lblCopyRight: TLabel;
    lblName: TLabel;
    mmoGNU: TMemo;
    mmoPresent: TMemo;
    PageControlAbout: TPageControl;
    TabSheetGNU: TTabSheet;
    TabSheetPresent: TTabSheet;
    procedure ImageTurtleMouseEnter(Sender: TObject);
    procedure ImageTurtleMouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.ImageTurtleMouseEnter(Sender: TObject);
// image changée
begin
  ImageList.GetBitmap(random(ImageList.Count - 1) + 1,ImageTurtle.Picture.Bitmap);
end;

procedure TAboutForm.ImageTurtleMouseLeave(Sender: TObject);
// image par défaut
begin
  ImageList.GetBitmap(0,ImageTurtle.Picture.Bitmap);
end;

end.

