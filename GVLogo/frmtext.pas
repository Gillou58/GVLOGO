{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche du texte de GVLOGO                |
  |                  Unité : frmText.pas                                   |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMTEXT - part of GVLOGO
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

unit FrmText;

interface

uses
  Classes, SysUtils, FileUtil, RichMemo, Forms, Controls, Graphics, Dialogs,
  ComCtrls;

type

  { TTextForm }

  TTextForm = class(TForm)
    rmmoText: TRichMemo;
    sbText: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    fBackColor: TColor;
    fFParams: TFontParams;
    fBold: Boolean;
    fItalic: Boolean;
    fUnderline: Boolean;
    fFontColor: TColor;
    fFontSize: Integer;
    function GetSelStart(const Len: Integer): Integer;
    procedure SetBackColor(AValue: TColor);
  public
    procedure Clear;
    procedure WriteText(const St: string);
    procedure WriteTextLN(const St: string);
    property Bold: Boolean read fBold write fBold default False;
    property Italic: Boolean read fItalic write fItalic default False;
    property UnderLine: Boolean read fUnderline write fUnderline default False;
    property FontColor: TColor read fFontColor write fFontColor default clBlack;
    property FontSize: Integer read fFontSize write fFontSize default 12;
    property BackColor: TColor read fBackColor write SetBackColor
      default clWhite;
  end;

var
  TextForm: TTextForm;

implementation

{$R *.lfm}

{ TTextForm }

procedure TTextForm.FormDeactivate(Sender: TObject);
// *** la fenêtre est désactivée ***
begin
  sbText.SimpleText := EmptyStr; // barre nettoyée
end;

function TTextForm.GetSelStart(const Len: Integer): Integer;
// *** calcul de la position de départ ***
var
  Li: Integer;
begin
  Result := 0;
  for Li := 0 to rmmoText.Lines.Count - 2 do
    Result := Result + Length(rmmoText.Lines[Li]) + 1;
  Result := Result + Length(rmmoText.Lines[rmmoText.Lines.Count - 1]) - Len;
end;

procedure TTextForm.SetBackColor(AValue: TColor);
// *** couleur du fond du texte ***
begin
  if fBackColor = AValue then // pas de changement ?
    Exit; // on sort ?
  fBackColor := AValue; // nouvelle valeur
  rmmoText.Color := fBackColor; // éditeur en accord
end;

procedure TTextForm.Clear;
// *** nettoyage ***
begin
 Bold := False;
   Italic := False;
   Underline := False;
   FontSize := 12;
   FontColor := clBlack;
   BackColor := clWhite;
   rmmoText.Lines.Clear;
end;

procedure TTextForm.FormCreate(Sender: TObject);
// *** création de la fenêtre ***
begin
  Clear;
end;

procedure TTextForm.WriteText(const St: string);
// *** écriture d'un texte ***
begin
  WriteTextLN(St); // ### TODO ###
end;

procedure TTextForm.WriteTextLN(const St: string);
// *** écriture d'un texte (avec retour chariot) ***
var
  LStart: Integer;
begin
  rmmoText.Lines.Add(St);
  with rmmoText do
    LStart := GetSelStart(Length(St));
  rmmoText.GetTextAttributes(LStart, fFParams);
  if Bold then
    fFParams.Style:= fFParams.Style + [fsBold]
  else
    fFParams.Style:= fFParams.Style - [fsBold];
  if Italic then
    fFParams.Style:= fFParams.Style + [fsItalic]
  else
    fFParams.Style:= fFParams.Style - [fsItalic];
 if Underline then
    fFParams.Style:= fFParams.Style + [fsUnderline]
  else
    fFParams.Style:= fFParams.Style - [fsUnderline];
  fFParams.Color := FontColor;
  fFParams.Size := FontSize;
  rmmoText.SetTextAttributes(LStart, Length(St), fFParams);
end;

end.

