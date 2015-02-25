program EasyTurtle;

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

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, bgrabitmappack, Main, GVAbout, Help, GVTools, GVConsts, GVTurtles,
  SysUtils, GetText, Translations; // traduction française de la LCL

{$R *.res}

procedure TranslateLCL;
// *** traduction ***
var
  Lang, DefLang: string;
begin
  Lang := EmptyStr;
  Deflang := EmptyStr;
  GetLanguageIDs({%H-}Lang, {%H-}DefLang);
  // utilisation du fichier corrigé
  TranslateUnitResourceStrings('LCLStrConsts',
      '..\..\3rdparty\lclstrconsts.fr.po', Lang, DefLang);
end;

begin
  RequireDerivedFormResource := True;
  TranslateLCL; // traduction
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

