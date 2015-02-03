{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fenêtre de suivi                        |
  |                  Unité : FrmFollow.pas                                 |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMFOLLOW - part of GVLOGO
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

unit FrmFollow;

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons, SynHighlighterPosition, SynEditHighlighter;

type
  // *** TFollowForm ***
  TFollowForm = class(TForm)
    btnClose: TBitBtn;
    btnClear: TBitBtn;
    pnlFollow: TPanel;
    sbFollow: TStatusBar;
    SynMemoFollow: TSynMemo;
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Hlighter: TSynPositionHighlighter;
    Attr, Attr2: TtkTokenKind;
  public
    procedure Write(const St: string; Kind: Integer); // affichage
  end;

var
  FollowForm: TFollowForm;

implementation

{$R *.lfm}

{ TFollowForm }

procedure TFollowForm.btnClearClick(Sender: TObject);
// *** nettoyage de la fenêtre ***
begin
  SynMemoFollow.Lines.Clear;
end;

procedure TFollowForm.FormCreate(Sender: TObject);
// *** création de la fiche ***
begin
  HLighter := TSynPositionHighlighter.Create(Self); // syntaxe colorée
  Attr := HLighter.CreateTokenID('Attr',clBlue,clNone,[fsBold]); // attribut
  Attr2 := HLighter.CreateTokenID('Attr2',clNone,clAqua,[fsBold]); // attribut 2
  SynMemoFollow.Highlighter := HLighter; // éditeur relié
end;

procedure TFollowForm.FormDestroy(Sender: TObject);
// *** destruction de la fiche ***
begin
  HLighter.Free; // libération de la colorisation
end;

procedure TFollowForm.Write(const St: string; Kind: Integer);
// *** affichage ***
begin
  // état affiché
  SynMemoFollow.Lines.Add(St);
  // couleur du niveau
  case Kind of
    1: HLighter.AddToken(SynMemoFollow.Lines.Count - 1, 5, Attr);
    2: HLighter.AddToken(SynMemoFollow.Lines.Count - 1, Length(
         SynMemoFollow.Lines[SynMemoFollow.Lines.Count - 1]), Attr2);
  end;
end;

end.
