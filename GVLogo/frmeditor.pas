{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de l'éditeur de GVLOGO            |
  |                  Unité : FrmEditor.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMEDITOR - part of GVLOGO
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

{$I GVDefines.inc} // fichier des définitions préalables

unit FrmEditor;

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynPluginSyncroEdit, Forms, Controls,
  Graphics, Dialogs, ComCtrls,
  GVHighlighter; // colorisation de la syntaxe

type
  // *** TEditorForm ***

  { TEditorForm }

  TEditorForm = class(TForm)
    sbEdit: TStatusBar;
    SynEditEditor: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditEditorChange(Sender: TObject);
    procedure SynEditEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    fGVHighlighter: TGVHighlighter;
  public
  end;

var
  EditorForm: TEditorForm;

implementation

uses
  StrUtils,
  GVLogoConsts; // constantes de GVLOGO

{$R *.lfm}

{ TEditorForm }

procedure TEditorForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  fGVHighlighter := TGVHighlighter.Create(Self); // création de la colorisation
  SynEditEditor.Highlighter := fGVHighlighter; // éditeur affecté
end;

procedure TEditorForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  fGVHighlighter.Free; // libération de la colorisation
end;

procedure TEditorForm.SynEditEditorChange(Sender: TObject);
// *** changement de l'éditeur ***
begin
  with SynEditEditor do
    sbEdit.Panels[1].Text := Format(CrsLine + '%.3d - ' + CrsCol + '%.3d - %s',
      [CaretY, CaretX, IfThen(Modified, CrsModified, CrsOk)]);
end;

procedure TEditorForm.SynEditEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
// *** touche pressée ***
begin
  SynEditEditorChange(nil);
end;

end.

