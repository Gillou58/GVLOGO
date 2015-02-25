{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Fiche de l'état du texte                |
  |                  Unité : FrmTextShow.pas                               |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR 2014-2015                    |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// FRMTEXTSHOW - part of GVLOGO
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

unit FrmTextShow;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, StdCtrls, ColorBox, Spin;

type
  // *** TTextShowForm ***
  TTextShowForm = class(TForm)
    BitBtn1: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    CheckBoxBold: TCheckBox;
    CheckBoxItalic: TCheckBox;
    CheckBoxUnderline: TCheckBox;
    CheckGroupStyle: TCheckGroup;
    ColorBoxBckGround: TColorBox;
    ColorBoxFont: TColorBox;
    cbBoxFonts: TComboBox;
    gbScreen: TGroupBox;
    gbFont: TGroupBox;
    gbSize: TGroupBox;
    gbFonts: TGroupBox;
    lblBckGrd: TLabel;
    lblFont: TLabel;
    lblSize: TLabel;
    mmoSample: TMemo;
    pnlTextShow: TPanel;
    sbTextShow: TStatusBar;
    spedtSize: TSpinEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure cbBoxFontsChange(Sender: TObject);
    procedure CheckBoxBoldChange(Sender: TObject);
    procedure CheckBoxItalicChange(Sender: TObject);
    procedure CheckBoxUnderlineChange(Sender: TObject);
    procedure ColorBoxBckGroundChange(Sender: TObject);
    procedure ColorBoxFontChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure spedtSizeChange(Sender: TObject);
  private
    fChanging: Boolean; // drapeau de changement en cours
  public
    procedure GetTextState; // récupération de l'état du texte
    procedure SetTextState; // mise àjour des données
    property Changing: Boolean read fChanging write fChanging;
  end;

var
  TextShowForm: TTextShowForm;

implementation

uses
  FrmText; // fiche du texte

{$R *.lfm}

{ TTextShowForm }

procedure TTextShowForm.FormCreate(Sender: TObject);
// *** création de la fenêtre ***
begin
  cbBoxFonts.Items.Assign(Screen.Fonts); // polices dans la combobox
end;

procedure TTextShowForm.spedtSizeChange(Sender: TObject);
// *** changement de taille ***
begin
  mmoSample.Font.Size := spedtSize.Value;
end;

procedure TTextShowForm.GetTextState;
// *** recherche de l'état du texte ***
begin
  if not Changing then
    with FrmText.TextForm.rmmoText do
    begin
      CheckBoxBold.Checked := (fsBold in Font.Style);
      CheckBoxItalic.Checked := (fsItalic in Font.Style);
      CheckBoxUnderline.Checked := (fsUnderline in Font.Style);
      ColorBoxBckGround.Selected := Color;
      ColorBoxFont.Selected := Font.Color;
      cbBoxFonts.Text := Font.Name;
      spedtSize.Value := Font.Size;
    end;
end;

procedure TTextShowForm.SetTextState;
// *** enregistrement de l'état du texte désiré ***
begin
  Changing := True; // changement signalé
  try
    with FrmText.TextForm.rmmoText do
    begin
      if CheckBoxBold.Checked then
        Font.Style := Font.Style + [fsBold]
      else
        Font.Style := Font.Style - [fsBold];
      if CheckBoxItalic.Checked then
        Font.Style := Font.Style + [fsItalic]
      else
        Font.Style := Font.Style - [fsItalic];
      if CheckBoxUnderline.Checked then
        Font.Style := Font.Style + [fsUnderline]
      else
        Font.Style := Font.Style - [fsUnderline];
      Color := ColorBoxBckGround.Selected;
      Font.Color := ColorBoxFont.Selected;
      Font.Name := cbBoxFonts.Text;
      Font.Size := spedtSize.Value;
    end;
  finally
    Changing := False;
  end;
end;

procedure TTextShowForm.cbBoxFontsChange(Sender: TObject);
// *** changement de police ***
begin
  mmoSample.Font.Name := cbBoxFonts.Items[cbBoxFOnts.ItemIndex];
end;

procedure TTextShowForm.btnSaveClick(Sender: TObject);
// enregistrement des modifications ***
begin
  SetTextState;
end;

procedure TTextShowForm.CheckBoxBoldChange(Sender: TObject);
// *** police en gras ***
begin
  if CheckBoxBold.Checked then
    mmoSample.Font.Style := mmoSample.Font.Style + [fsBold]
  else
    mmoSample.Font.Style := mmoSample.Font.Style - [fsBold];
end;

procedure TTextShowForm.CheckBoxItalicChange(Sender: TObject);
// *** police en italiques ***
begin
  if CheckBoxItalic.Checked then
    mmoSample.Font.Style := mmoSample.Font.Style + [fsItalic]
  else
    mmoSample.Font.Style := mmoSample.Font.Style - [fsItalic];
end;

procedure TTextShowForm.CheckBoxUnderlineChange(Sender: TObject);
// *** police soulignée ***
begin
  if CheckBoxUnderline.Checked then
    mmoSample.Font.Style := mmoSample.Font.Style + [fsUnderline]
  else
    mmoSample.Font.Style := mmoSample.Font.Style - [fsUnderline];
end;

procedure TTextShowForm.ColorBoxBckGroundChange(Sender: TObject);
// *** changement de couleur de fond ***
begin
  mmoSample.Color := ColorBoxBckGround.Selected;
end;

procedure TTextShowForm.ColorBoxFontChange(Sender: TObject);
// *** changement de la couleur des caractères ***
begin
  mmoSample.Font.Color := ColorBoxFont.Selected;
end;

procedure TTextShowForm.FormActivate(Sender: TObject);
// *** activation de la fiche ***
begin
  GetTextState; // on récupère les données actuelles
end;

end.

