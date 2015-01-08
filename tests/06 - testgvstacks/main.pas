{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test de GVStacks                        |
  |                  Unité : Main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVSTACKS - part of GVLOGO
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

unit Main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Grids, ComCtrls,
  GVStacks, // piles
  GVConsts, // constantes communes
  GVErrConsts; // constantes d'erreurs

type

  { TMainForm }

  TMainForm = class(TForm)
    btnError: TBitBtn;
    btnPush: TBitBtn;
    btnPop: TBitBtn;
    btnPeek: TBitBtn;
    btnSwap: TBitBtn;
    btnDup: TBitBtn;
    btnRot: TBitBtn;
    btnOver: TBitBtn;
    btnClear: TBitBtn;
    BitBtnClose: TBitBtn;
    lblCapacity: TLabel;
    lblDepth: TLabel;
    mmoActions: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioGroup1: TRadioGroup;
    sbStack: TStatusBar;
    sgStack: TStringGrid;
    procedure btnErrorClick(Sender: TObject);
    procedure btnPushClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fStackStr : TGVStringStack; // pile de travail
    fErr: Boolean; // erreur active/inactive
    procedure UpdateButtons; // mise à jour des boutons
    procedure StackChanged(Sender: TObject;
      Act: TGVStackNotification); // changement de la pile
  public
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
// fermeture de la fiche
begin
  CloseAction := caFree; // on libère la fenêtre
end;

procedure TMainForm.btnPushClick(Sender: TObject);
// actions
var
  LS: string;
begin
  case (Sender as TBitBtn).TabOrder of
    0 : begin
          LS := 'Item : ' + IntToStr(fStackStr.Count);
          fStackStr.Push(LS);
        end;
    1 : LS := 'Pop : ' + fStackStr.Pop;
    2 : LS := 'Peek : ' + fStackStr.Peek;
    3 : begin
          LS := 'Swap';
          fStackStr.Swap;
        end;
    4 : begin
          LS := 'Dup';
          fStackStr.Dup;
        end;
    5 : begin
          LS := 'Rot';
          fStackStr.Rot;
        end;
    6 : begin
          LS := 'Over';
          fStackStr.Over;
        end;
    7 : begin
          fStackStr.Clear;
          fStackStr.Shrink;
          mmoActions.Clear; // on nettoie l'affichage
          LS := '*** CLEAR ***';
        end;
  end;
  lblCapacity.Caption := 'Capacité de la pile : ' +
    IntToStr(fStackStr.Capacity);
  lblDepth.Caption := 'Profondeur de la pile : ' +
    IntToStr(fStackStr.Count);
  if fStackStr.Error.OK then  // si pas d'erreur
    MmoActions.Lines.Append(LS) // on affiche
  else
    fStackStr.Error.Clear; // erreur annulée
end;

procedure TMainForm.btnErrorClick(Sender: TObject);
// active/ désactive les erreurs
begin
  fErr := not fErr; // on inverse le drapeau
  if fErr then // erreur active
  begin
    btnPop.Enabled := True;
    btnPeek.Enabled := True;
    btnDup.Enabled := True;
    btnSwap.Enabled := True;
    btnOver.Enabled := True;
    btnRot.Enabled := True;
  end
  else
    UpdateButtons; // les boutons sont mis à jour
end;

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  fStackStr := TGVStringStack.Create; // on crée la pile de travail
  fStackStr.OnNotify := @StackChanged;
  // gestionnaire de recherche d'erreur actif
  fStackStr.Error.OnError := @GetError;
  Randomize; // initialisation des nombres aléatoires
  fErr := False; // pas d'erreur par défaut
  UpdateButtons; // boutons à jour
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  fStackStr.Free; // on libère la pile
end;

procedure TMainForm.UpdateButtons;
// mise à jour des boutons
var
  LStckCount : Integer;
  LStckEnabled : Boolean;
begin
  LStckCount := fStackStr.Count;
  LStckEnabled := (LStckCount <> 0);
  btnPop.Enabled := LStckEnabled;
  btnPeek.Enabled := LStckEnabled;
  btnDup.Enabled := LStckEnabled;
  LStckEnabled := (LStckCount > 1);
  btnSwap.Enabled := LStckEnabled;
  btnOver.Enabled := LStckEnabled;
  LStckEnabled := (LStckCount > 2);
  btnRot.Enabled := LStckEnabled;
end;

procedure TMainForm.StackChanged(Sender: TObject; Act: TGVStackNotification);
// changement de la pile
var
  Li: Integer;
begin
  for Li := 0 to fStackStr.Count - 1 do
    sgStack.Cells[0,Li] := fStackStr[Li];
  case Act of
    stAdded : begin
      mmoActions.Lines.Add('>>> Un élément a été ajouté à la pile.');
      sgStack.RowCount := fStackStr.Count + 4;
    end;
    stRemoved : begin
      mmoActions.Lines.Add('<<< Un élément a été retiré de la pile.');
      sgStack.Cells[0,fStackStr.Count] := EmptyStr;
    end;
    stChanged : mmoActions.Lines.Add('<<>> Le sommet de la pile a été modifié.');
    stCleared: begin
      mmoActions.Lines.Add('000 La pile est vide.');
      for Li := 0 to sgStack.RowCount -1 do
        sgStack.Cells[0,Li] := EmptyStr;
    end;
  end;
  if not fErr then // si erreur inactive
    UpdateButtons;
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// recherche d'une erreur
begin
  // message en toutes lettres
  mmoActions.Lines.Add('>>> ' + fStackStr.Error.ErrorMessage);
  with mmoActions.Lines, ErrorRec do
  begin
    Add('Code: ' + IntToStr(Ord(Code))); // code de l'erreur
    if ErrPos <> CE_NoErr then // position pertinente ?
      Add('Position : ' + IntToStr(ErrPos)); // position de l'erreur
  end;
end;

end.

