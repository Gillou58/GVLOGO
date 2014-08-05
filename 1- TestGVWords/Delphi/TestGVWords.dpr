{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Traitement des mots                     |
  |                  Unité : TestGVWords.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    02-08-2014 10:23:54                          |
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

{$DEFINE Delphi}
program TestGVWords;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormGVWords} ,
  GVConsts in 'GVConsts.pas',
  GVWords in 'GVWords.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Aqua Graphite');
  Application.Title := 'Test de GVWords';
  Application.CreateForm(TFormGVWords, FormGVWords);
  Application.Run;

end.
