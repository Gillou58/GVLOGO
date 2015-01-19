{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du noyau de GVLOGO                 |
  |                  Unité : main.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    23-12-2014 18:00:00                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }


// HISTORIQUE
// 23/12/2014 - 1.0.0 - première version opérationnelle

// TESTGVKERNEL - part of GVLOGO
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

unit main;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Buttons,
  GVConsts, // constantes communes
  GVPrimConsts, // constantes des primitives
  GVKernel,// noyau
  GVErrConsts; // constantes d'erreurs

type

  { TMainForm }

  TMainForm = class(TForm)
    btnClose: TBitBtn;
    BitBtnKerCancel: TBitBtn;
    btnAddVar: TButton;
    btnLoadProcs: TButton;
    btnSaveVars: TButton;
    btnIsProcedure: TButton;
    btnProcsCount: TButton;
    btnProcsToList: TButton;
    btnAddProc: TButton;
    btnRemoveProc: TButton;
    btnRemoveAllProcs: TButton;
    btnParamsCount: TButton;
    btnParamLine: TButton;
    btnParamNum: TButton;
    btnVarCounts: TButton;
    btnDump: TButton;
    btnProcLinesCount: TButton;
    btnProcLine: TButton;
    btnProcListDef: TButton;
    btnProcToEdit: TButton;
    btnEditToProc: TButton;
    btnSaveProcs: TButton;
    btnIsPrim: TButton;
    btnPrimsToList: TButton;
    btnIsVar: TButton;
    btnPrimsCount: TButton;
    btnNumPrim: TButton;
    btnNumParamPrim: TButton;
    btnPrimByNum: TButton;
    btnIsPackage: TButton;
    btnPackagesCount: TButton;
    btnIsBurriedPackage: TButton;
    btnBurryPackage: TButton;
    btnValVar: TButton;
    btnUnBurryPackage: TButton;
    btnToPackage: TButton;
    btnListToPackage: TButton;
    btnPackageToList: TButton;
    btnCountItemsPackage: TButton;
    btnPackagesToList: TButton;
    btnCreatePackage: TButton;
    btnRemovePackage: TButton;
    btnSavePackage: TButton;
    btnRemoveVar: TButton;
    btnPackageToEdit: TButton;
    btnIsValidDef: TButton;
    btnDProp: TButton;
    btnProcsToEdit: TButton;
    btnUnPackObj: TButton;
    btnIsInPackage: TButton;
    btnIsBurried: TButton;
    btnBelongTo: TButton;
    btnClear: TButton;
    btnRProp: TButton;
    btnAnProp: TButton;
    btnPListe: TButton;
    btnIsEmptyPList: TButton;
    btnSaveAllVars: TButton;
    btnSaveAllProcs: TButton;
    btnRemoveAllVars: TButton;
    btnExists: TButton;
    btnIsProtected: TButton;
    btnCount: TButton;
    btnToList: TButton;
    btnLoadAll: TButton;
    btnSaveAll: TButton;
    btnKError: TButton;
    btnVarsToList: TButton;
    btnKernelResult: TButton;
    btnLoadVars: TButton;
    btnAllProcsToEdit: TButton;
    btnProtected: TButton;
    btnCopyDef: TButton;
    GroupBoxPropLists: TGroupBox;
    GroupBoxGeneral: TGroupBox;
    GroupBoxProperties: TGroupBox;
    GroupBoxPackages: TGroupBox;
    GroupBoxPrims: TGroupBox;
    GroupBoxVars: TGroupBox;
    GroupBoxProcs: TGroupBox;
    lblIndice: TLabel;
    LabEditKerName: TLabeledEdit;
    LabEditKerValue: TLabeledEdit;
    LabEditKerList: TLabeledEdit;
    mmoGVKer: TMemo;
    MainPageControl: TPageControl;
    SpinEditKer: TSpinEdit;
    StatusBar: TStatusBar;
    TabSheetKernel: TTabSheet;
    procedure BitBtnKerCancelClick(Sender: TObject);
    procedure btnAddProcClick(Sender: TObject);
    procedure btnAddVarClick(Sender: TObject);
    procedure btnAllProcsToEditClick(Sender: TObject);
    procedure btnAnPropClick(Sender: TObject);
    procedure btnBelongToClick(Sender: TObject);
    procedure btnBurryPackageClick(Sender: TObject);
    procedure btnCopyDefClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnCountItemsPackageClick(Sender: TObject);
    procedure btnCreatePackageClick(Sender: TObject);
    procedure btnDPropClick(Sender: TObject);
    procedure btnDumpClick(Sender: TObject);
    procedure btnEditToProcClick(Sender: TObject);
    procedure btnExistsClick(Sender: TObject);
    procedure btnIsBurriedClick(Sender: TObject);
    procedure btnIsBurriedPackageClick(Sender: TObject);
    procedure btnIsEmptyPListClick(Sender: TObject);
    procedure btnIsInPackageClick(Sender: TObject);
    procedure btnIsPackageClick(Sender: TObject);
    procedure btnIsPrimClick(Sender: TObject);
    procedure btnIsProcedureClick(Sender: TObject);
    procedure btnIsProtectedClick(Sender: TObject);
    procedure btnIsValidDefClick(Sender: TObject);
    procedure btnIsVarClick(Sender: TObject);
    procedure btnKernelResultClick(Sender: TObject);
    procedure btnKErrorClick(Sender: TObject);
    procedure btnListToPackageClick(Sender: TObject);
    procedure btnLoadAllClick(Sender: TObject);
    procedure btnLoadProcsClick(Sender: TObject);
    procedure btnLoadVarsClick(Sender: TObject);
    procedure btnNumParamPrimClick(Sender: TObject);
    procedure btnNumPrimClick(Sender: TObject);
    procedure btnPackagesCountClick(Sender: TObject);
    procedure btnPackagesToListClick(Sender: TObject);
    procedure btnPackageToEditClick(Sender: TObject);
    procedure btnPackageToListClick(Sender: TObject);
    procedure btnParamLineClick(Sender: TObject);
    procedure btnParamNumClick(Sender: TObject);
    procedure btnParamsCountClick(Sender: TObject);
    procedure btnPListeClick(Sender: TObject);
    procedure btnPrimByNumClick(Sender: TObject);
    procedure btnPrimsCountClick(Sender: TObject);
    procedure btnPrimsToListClick(Sender: TObject);
    procedure btnProcLineClick(Sender: TObject);
    procedure btnProcLinesCountClick(Sender: TObject);
    procedure btnProcListDefClick(Sender: TObject);
    procedure btnProcsCountClick(Sender: TObject);
    procedure btnProcsToEditClick(Sender: TObject);
    procedure btnProcsToListClick(Sender: TObject);
    procedure btnProcToEditClick(Sender: TObject);
    procedure btnProtectedClick(Sender: TObject);
    procedure btnRemoveAllProcsClick(Sender: TObject);
    procedure btnRemoveAllVarsClick(Sender: TObject);
    procedure btnRemovePackageClick(Sender: TObject);
    procedure btnRemoveProcClick(Sender: TObject);
    procedure btnRemoveVarClick(Sender: TObject);
    procedure btnRPropClick(Sender: TObject);
    procedure btnSaveAllClick(Sender: TObject);
    procedure btnSaveAllProcsClick(Sender: TObject);
    procedure btnSaveAllVarsClick(Sender: TObject);
    procedure btnSavePackageClick(Sender: TObject);
    procedure btnSaveProcsClick(Sender: TObject);
    procedure btnSaveVarsClick(Sender: TObject);
    procedure btnToListClick(Sender: TObject);
    procedure btnToPackageClick(Sender: TObject);
    procedure btnUnBurryPackageClick(Sender: TObject);
    procedure btnUnPackObjClick(Sender: TObject);
    procedure btnValVarClick(Sender: TObject);
    procedure btnVarCountsClick(Sender: TObject);
    procedure btnVarsToListClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
  private
    GVKer: TGVLogoKernel; // noyau
  public
    // formatage d'un message
    function FmtMess(const Value: string) : string;
    // recherche des erreurs
    procedure GetError(Sender: TObject; ErrorRec: TGVErrorRec);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  StrUtils;

resourcestring
  RS_Result = '// Résultat de %s : ';
  RS_Exists = '"%s" existe.';
  RS_NoExists = '"%s" n''existe pas.';

const
  C_AllFile = 'AllFile';
  C_VarsFile = 'VarsFile';
  C_Procs = 'Procedures';

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  GVKer := TGVLogoKernel.Create; // noyau de l'interpréteur
  // gestionnaire de recherche d'erreur actif
  GVKer.Error.OnError := @GetError;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  GVKer.Free; // libération du noyau
end;

procedure TMainForm.MainPageControlChange(Sender: TObject);
// mise à jour de la barre d'état
begin
   StatusBar.SimpleText:= MainPageControl.ActivePage.Caption;
end;

function TMainForm.FmtMess(const Value: string): string;
// formatage d'un message
begin
  Result := Format(RS_Result,[Value]);
end;

procedure TMainForm.GetError(Sender: TObject; ErrorRec: TGVErrorRec);
// recherche d'une erreur
begin
  with mmoGVKer.Lines, ErrorRec do
  begin
    Add(CComment + CBlank + '******************');
    // message en toutes lettres
    Add(CComment + CBlank + '>>> ' + GVKer.Error.ErrorMessage + ' <<< ');
    // code de l'erreur
    Add(CComment + CBlank + 'Code : ' + IntToStr(Ord(Code)));
    // élément fautif dans la ligne de travail
    Add(CComment + CBlank + 'Elément : ' + ErrItem);
    if ErrPos <> CE_NoErr then // position pertinente ?
      // position de l'erreur
      Add(CComment + CBlank + 'Position : ' + IntToStr(ErrPos));
    Add(CComment + CBlank + '******************');
  end;
end;

procedure TMainForm.btnExistsClick(Sender: TObject);
// test de Exists
begin
  if GVKer.Exists(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('EXISTS') +
      Format(RS_Exists,[LabEditKerName.Text]))
  else
    mmoGVKer.Lines.Add(fmtMess('EXISTS') +
      Format(RS_NoExists,[LabEditKerName.Text]));
end;

procedure TMainForm.btnIsBurriedClick(Sender: TObject);
// test de IsBurried
var
  LS: string;
begin
  if GVKer.IsBurried(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISBURRIED') + Format('"%s" %s enfoui.',
   [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsBurriedPackageClick(Sender: TObject);
// test de IsBurriedPck
var
  LS: string;
begin
  if GVKer.IsBurriedPck(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISBURRIEDPCK') +
    Format('"%s" %s un paquet enfoui.', [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsEmptyPListClick(Sender: TObject);
// test de IsEmptyList
var
  LS: string;
begin
  if GVKer.IsEmptyPList(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISEMPTYLIST') + Format('"%s" %s vide.',
   [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsInPackageClick(Sender: TObject);
// test de IsInPackage
var
  LS: string;
begin
  if GVKer.IsInPck(LabEditKerName.Text) then
    LS := 'est'
   else
     LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISINPCK') + Format('"%s" %s dans un paquet.',
   [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsPackageClick(Sender: TObject);
// test de IsPackage
var
  LS: string;
begin
  if GVKer.IsPck(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISPCK') + Format('"%s" %s un paquet.',
    [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsPrimClick(Sender: TObject);
// test de IsPrim
var
  LS: string;
begin
  if GVKer.IsPrim(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISPRIM') + Format('"%s" %s une primitive.',
    [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsProcedureClick(Sender: TObject);
//test de IsProc
var
  LS: string;
begin
  if GVKer.IsProc(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISPROC') + Format('"%s" %s une procédure.',
    [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsProtectedClick(Sender: TObject);
var
  LS: string;
begin
  if GVKer.IsProtected(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISPROTECTED') + Format('"%s" %s protégé.',
    [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsValidDefClick(Sender: TObject);
// test de IsValidDef
var
  LS: string;
begin
  if GVKer.IsValidDef(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISVALIDDEF') +
    Format('"%s" %s une  définition correcte.', [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnIsVarClick(Sender: TObject);
// test de IsVar
var
  LS: string;
begin
  if GVKer.IsVar(LabEditKerName.Text) then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('ISVAR') + Format('"%s" %s une variable.',
   [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnKernelResultClick(Sender: TObject);
// test de ERRORMESSAGE
begin
  mmoGVKer.Lines.Add(FmtMess('ERRORMESSAGE') +
    GVKer.Error.ErrorMessage);
end;

procedure TMainForm.btnKErrorClick(Sender: TObject);
// test de OK
var
  LS: string;
begin
  if GVKer.Error.OK then
    LS := 'est'
  else
    LS := 'n''est pas';
  mmoGVKer.Lines.Add(FmtMess('OK') +
    Format('Le drapeau de bon déroulement %s levé.', [LS]));
  end;

procedure TMainForm.btnListToPackageClick(Sender: TObject);
// test de ListToPackage
begin
  if GVKer.ListToPck(LabEditKerName.Text,LabEditKerList.Text) then
    mmoGVKer.Lines.Add(fmtMess('LISTTOPCK') +
      Format('%s a été ajouté au paquet %s.',
        [LabEditKerList.Text, LabEditKerName.Text]));
end;

procedure TMainForm.BitBtnKerCancelClick(Sender: TObject);
// nettoyage de l'éditeur du noyau
begin
  mmoGVKer.Clear;
end;

procedure TMainForm.btnAddProcClick(Sender: TObject);
// test de AddProc
begin
  if GVKer.AddProc(LabEditKerName.Text,LabEditKerList.Text) then
    mmoGVKer.Lines.Add(fmtMess('ADDPROC') + 'La procédure ' +
      LabEditKerName.Text + ' a été ajoutée.');
end;

procedure TMainForm.btnAddVarClick(Sender: TObject);
// test de AddVar
begin
  if GVKer.AddVar(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('ADDVAR') + 'La variable '+
     LabEditKerName.Text + ' a été créée.');
end;

procedure TMainForm.btnAllProcsToEditClick(Sender: TObject);
// Test de AllProcsToEdit
begin
  if GVKer.AllProcsToEdit(mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('ALLPROCSTOEDIT') +
      'Toutes les procédures ont été éditées.');
end;

procedure TMainForm.btnAnPropClick(Sender: TObject);
// test de AnProp
begin
  if GVKer.AnProp(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('ANPROP') + LabEditKerName.Text + ' supprimée.');
end;

procedure TMainForm.btnBelongToClick(Sender: TObject);
// test de BelongTo
var
  LS: string;
begin
  LS := GVKer.BelongsTo(LabEditKerName.Text);
  if LS <> EmptyStr then
    mmoGVKer.Lines.Add(fmtMess('BELONGSTO') +
      Format('L''objet "%s" appartient au paquet "%s".',
        [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnBurryPackageClick(Sender: TObject);
// test de BurryPackage
begin
  if GVKer.BurryPck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('BURRYPCK') +
      Format('Le paquet %s est enfoui.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnCopyDefClick(Sender: TObject);
// test de CopyDef
begin
  if GVKer.CopyDef(LabEditKerName.Text, LabEditKerValue.Text) then
      mmoGVKer.Lines.Add(fmtMess('COPYDEF') +
        Format('La procédure "%s" a été créée à partir de "%s".',
          [LabEditKerValue.Text, LabEditKerName.Text]));
end;

procedure TMainForm.btnCountClick(Sender: TObject);
// test de Count
var
  Li: Integer;
begin
  Li := GVKer.Count;
  mmoGVKer.Lines.Add(fmtMess('COUNT') + 'Il y a ' +
    IntToStr(Li) + ' objet' + IfThen(Li>1, 's') + '.');
end;

procedure TMainForm.btnCountItemsPackageClick(Sender: TObject);
// test de CountItemsPck
var
  Li: Integer;
begin
  Li := GVKer.CountItemsPck(LabEditKerName.Text);
  if Li <> -1 then
    mmoGVKer.Lines.Add(fmtMess('COUNTITEMSPCK') + 'Il y a ' +
      IntToStr(Li) + ' objet' + IfThen(Li>1, 's') + ' dans le paquet ' +
        LabEditKerName.Text + '.');
end;

procedure TMainForm.btnCreatePackageClick(Sender: TObject);
// test de CreatePackage
begin
  if GVKer.CreatePck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('CREATEPCK') +
      Format('Le paquet %s a été créé.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnDPropClick(Sender: TObject);
// essai de Dprop
begin
  if GVKer.DProp(LabEditKerName.Text,LabEditKerValue.Text,
    LabEditKerList.Text) then
      mmoGVKer.Lines.Add(fmtMess('DPROP') + LabEditKerName.Text + ' ajoutée');
end;

procedure TMainForm.btnDumpClick(Sender: TObject);
// test de Dump
var
  Lst: TStringList;
  LS: string;
begin
  Lst := TStringList.Create;
  try
    GVKer.Dump(Lst);
    mmoGVKer.Lines.Add(fmtMess('DUMP'));
      if Lst.Count <> 0 then
      begin
        for LS in Lst do
          mmoGVKer.Lines.Append('// '+ LS);
      end
      else
        mmoGVKer.Lines.Append('// Aucun objet n''est enregistré !');
  finally
    Lst.Free;
  end;
end;

procedure TMainForm.btnEditToProcClick(Sender: TObject);
// test de EditToProc
var
  Li: Integer;
begin
  Li := 0;
  if GVKer.EditToProc(mmoGVKer.Lines, 0, 0, Li) then
    mmoGVKer.Lines.Add(fmtMess('EDITTOPROC') + 'Editeur interprété.')
  else
    mmoGVKer.Lines.Add(fmtMess('EDITTOPROC') +
      Format('Erreur à la ligne : %d.',[Li]));
end;

procedure TMainForm.btnLoadAllClick(Sender: TObject);
// test de LoadAll
begin
if GVKer.LoadAll(C_AllFile) then
    mmoGVKer.Lines.Add(fmtMess('LOADALL') + 'Fichier chargé.');
end;

procedure TMainForm.btnLoadProcsClick(Sender: TObject);
// test de LoadProcs
begin
  if GVKer.LoadProcs(C_Procs) then
    mmoGVKer.Lines.Add(fmtMess('LOADPROCS') + 'Procédures chargées.');
end;

procedure TMainForm.btnLoadVarsClick(Sender: TObject);
// test de LoadVars
begin
  if GVKer.LoadVars(C_VarsFile) then
    mmoGVKer.Lines.Add(fmtMess('LOADVARS') +
      'Le fichier de variables a été chargé.');
end;

procedure TMainForm.btnNumParamPrimClick(Sender: TObject);
// test de NumParamPrim
begin
  mmoGVKer.Lines.Add(fmtMess('NUMPARAMPRIM') +
    IntToStr(GVKer.NumParamsPrim(LabEditKerName.Text)));
end;

procedure TMainForm.btnNumPrimClick(Sender: TObject);
// test de NumPrim
var
  Li: Integer;
begin
  Li := GVKer.NumPrim(LabEditKerName.Text);
  if Li <> -1 then
    mmoGVKer.Lines.Add(fmtMess('NUMPRIMS') +
      Format('La primitive %s porte le numéro %d.', [LabEditKerName.Text, Li]))
  else
    mmoGVKer.Lines.Add(fmtMess('NUMPRIMS') +
      Format('La primitive "%s" n''existe pas.', [LabEditKerName.Text]));
end;

procedure TMainForm.btnPackagesCountClick(Sender: TObject);
// test de PcksCount
var
  Li: Integer;
begin
  Li := GVKer.PcksCount;
  mmoGVKer.Lines.Add(fmtMess('PCKSCOUNT') + 'Il y a ' +
    IntToStr(Li) + ' paquet' + IfThen(Li>1, 's') + '.');
end;

procedure TMainForm.btnPackagesToListClick(Sender: TObject);
// test de PcksToList
begin
  mmoGVKer.Lines.Add(fmtMess('PCKSTOLIST') + 'Liste des paquets : ' +
    GVKer.PcksToList);
end;

procedure TMainForm.btnPackageToEditClick(Sender: TObject);
// test de PckToEdit
begin
  if GVKer.PckToEdit(LabEditKerName.Text,mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('PCKTOEDIT') +
      Format('Le paquet "%s" a été envoyé vers l''éditeur.',
        [LabEditKerName.Text]));
end;

procedure TMainForm.btnPackageToListClick(Sender: TObject);
// test de PckToList
var
  LS: string;
begin
  LS := GVKer.PckToList(LabEditKerName.Text);
  if GVKer.Error.OK then
    mmoGVKer.Lines.Add(fmtMess('PCKTOLIST') +
      Format('Le paquet "%s" comprend les objets %s.',
        [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnParamLineClick(Sender: TObject);
// test de ParamLine
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.ParamsLine(LabEditKerName.Text, LS) then
    mmoGVKer.Lines.Add(fmtMess('PARAMLINE') +
      Format('La ligne de paramètres de la procédure "%s" est %s.',
        [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnParamNumClick(Sender: TObject);
// test de ParamNum
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.ParamNum(LabEditKerName.Text, SpinEditKer.Value, LS) then
    mmoGVKer.Lines.Add(fmtMess('PARAMNUM') +
      Format('Le paramètre %d de la procédure "%s" est %s.',
        [SpinEditKer.Value, LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnParamsCountClick(Sender: TObject);
// test de ParamsCount
var
  Li: Integer;
begin
  Li := GVKer.ParamsCount(LabEditKerName.Text);
  if Li <> -1 then
    mmoGVKer.Lines.Add(fmtMess('PARAMSCOUNT') +
      Format('La procédure "%s" compte %d paramètre', [LabEditKerName.Text, Li])
      + IfThen(Li>1,'s') +'.');
end;

procedure TMainForm.btnPListeClick(Sender: TObject);
// test de Pliste
var
  LS: string;
begin
  LS := EmptyStr; // initialisation
  if GVKer.PList(LabEditKerName.Text,LS) then
    mmoGVKer.Lines.Add(fmtMess('PLISTE') + LS);
end;

procedure TMainForm.btnPrimByNumClick(Sender: TObject);
// test de PrimByNum
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.PrimByNum(SpinEditKer.Value, LS) then
    mmoGVKer.Lines.Add(fmtMess('PRIMBYNUM') +
      Format('La primitive  %d est "%s".', [SpinEditKer.Value, LS]));
end;

procedure TMainForm.btnPrimsCountClick(Sender: TObject);
// test de PrimsCount
begin
  mmoGVKer.Lines.Add(fmtMess('PRIMSCOUNT') + 'Il y a ' +
    IntToStr(GVKer.PrimsCount) + ' primitives.');
end;

procedure TMainForm.btnPrimsToListClick(Sender: TObject);
// test de PrimsToList
begin
  mmoGVKer.Lines.Add(fmtMess('PRIMSTOLIST') + GVKer.PrimsToList);
end;

procedure TMainForm.btnProcLineClick(Sender: TObject);
// test de ProcLine
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.ProcLine(LabEditKerName.Text, SpinEditKer.Value, LS) then
    mmoGVKer.Lines.Add(fmtMess('PROCLINE') +
      Format('La ligne %d de la procédure "%s" est %s.',
        [SpinEditKer.Value,LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnProcLinesCountClick(Sender: TObject);
// test de ProcLinesCount
var
  Li: Integer;
begin
  Li := GVKer.ProcLinesCount(LabEditKerName.Text);
  if Li <> -1 then
    mmoGVKer.Lines.Add(fmtMess('PROCLINESCOUNT') +
      Format('La procédure "%s" comprend %d ligne', [LabEditKerName.Text, Li])
      + IfThen(Li>1,'s') + '.');
end;

procedure TMainForm.btnProcListDefClick(Sender: TObject);
// test de ProcListDef
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.ProcListDef(LabEditKerName.Text, LS) then
    mmoGVKer.Lines.Add(FmtMess('PROCLISTDEF') +
    Format('La définition de "%s" est %s.', [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnProcsCountClick(Sender: TObject);
// test de ProcsCount
var
  Li: Integer;
begin
  Li := GVKer.ProcsCount;
  mmoGVKer.Lines.Add(fmtMess('PROCSCOUNT') + 'Il y a ' +
    IntToStr(Li) + ' procédure' + IfThen(Li>1, 's') + '.');
end;

procedure TMainForm.btnProcsToEditClick(Sender: TObject);
// test de ProcsToEdit
begin
  if GVKer.ProcsToEdit(LabEditKerName.Text,mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('PROCSTOEDIT') +
      Format('Les procédures %s ont été éditées.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnProcsToListClick(Sender: TObject);
// test de ProcsToList
begin
  mmoGVKer.Lines.Add(fmtMess('PROCSTOLIST') +
  'Les procédures enregistrées sont : ' + GVKer.ProcsToList + '.');
end;

procedure TMainForm.btnProcToEditClick(Sender: TObject);
// test de ProcToEdit
begin
  if GVKer.ProcToEdit(LabEditKerName.Text, mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('PROCTOEDIT') + 'Procédure éditée.');
end;

procedure TMainForm.btnProtectedClick(Sender: TObject);
// test de Protect
begin
  GVKer.Protect := not GVKer.Protect;
  if GVKer.Protect then
    mmoGVKer.Lines.Add(fmtMess('PROTECT') +
      'Tous les objets sont protégés.')
  else
    mmoGVKer.Lines.Add(fmtMess('PROTECT') +
      'Les objets ne sont plus protégés.')
end;

procedure TMainForm.btnRemoveAllProcsClick(Sender: TObject);
// test de RemoveAllProcs
begin
  if GVKer.RemoveAllProcs then
    mmoGVKer.Lines.Add(fmtMess('REMOVEALLPROCS') +
      'Toutes les procédures ont été supprimées.');
end;

procedure TMainForm.btnRemoveAllVarsClick(Sender: TObject);
// test de RemoveAllVars
begin
  if GVKer.RemoveAllVars then
    mmoGVKer.Lines.Add(fmtMess('REMOVEALLVARS') +
      'Toutes les variables ont été supprimées.');
end;

procedure TMainForm.btnRemovePackageClick(Sender: TObject);
// test de RemovePackage
begin
  if GVKer.RemovePck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('REMOVEPCK') +
      Format('Le paquet %s a été supprimé.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnRemoveProcClick(Sender: TObject);
// test de RemoveProc
var
  LS: string;
begin
  LS := LabEditKerName.Text;
  if GVKer.RemoveProc(LS) then
    mmoGVKer.Lines.Add(fmtMess('REMOVEPROC') + LS + ' supprimée.');
end;

procedure TMainForm.btnRemoveVarClick(Sender: TObject);
// test de RemoveVar
begin
  if GVKer.RemoveVar(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('REMOVEVAR') +
      'La variable "' + LabEditKerName.Text + '" a été supprimée.');
end;

procedure TMainForm.btnRPropClick(Sender: TObject);
// test de RProp
var
  LS: string;
begin
  LS := EmptyStr; // initialisation
  if GVKer.RProp(LabEditKerName.Text,LabEditKerValue.text,LS) then
    mmoGVKer.Lines.Add(fmtMess('RPROP') + LS);
end;

procedure TMainForm.btnSaveAllClick(Sender: TObject);
// test de SaveAll
begin
  if GVKer.SaveAll(C_AllFile) then
    mmoGVKer.Lines.Add(fmtMess('SAVEALL') + 'Fichier sauvegardé.');
end;

procedure TMainForm.btnSaveAllProcsClick(Sender: TObject);
// test de SaveAllProcs
begin
  if GVKer.SaveAllProcs(C_Procs) then
      mmoGVKer.Lines.Add(fmtMess('SAVEALLPROCS') + 'Procédures sauvegardées.');
end;

procedure TMainForm.btnSaveAllVarsClick(Sender: TObject);
// test de SaveAllVars
begin
  if GVKer.SaveAllVars(C_VarsFile) then
    mmoGVKer.Lines.Add(fmtMess('SAVEALLVARS') +
      'Toutes les variables ont été sauvegardées.');
end;

procedure TMainForm.btnSavePackageClick(Sender: TObject);
// test de SavePackage
begin
  if GVKer.SavePck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('SAVEPCK') +
      Format('Le contenu du paquet %s a été sauvegardé.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnSaveProcsClick(Sender: TObject);
// test de SaveProcs
begin
  if GVKer.SaveProcs(C_Procs, LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('SAVEPROCS') +
      Format('Procédures %s sauvegardées.', [LabEditKerName.Text]));
end;

procedure TMainForm.btnSaveVarsClick(Sender: TObject);
// test de SaveVars
begin
  if GVKer.SaveVars(C_VarsFile,LabEditKerList.Text) then
    mmoGVKer.Lines.Add(fmtMess('SAVEVARS') +
      Format('Les variables %s ont été sauvegardées.', [LabEditKerList.Text]));
end;

procedure TMainForm.btnToListClick(Sender: TObject);
// test de ToList
begin
  mmoGVKer.Lines.Add(fmtMess('TOLIST') + GVKer.ToList);
end;

procedure TMainForm.btnToPackageClick(Sender: TObject);
// test de ToPackage
begin
  if GVKer.ToPck(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('TOPCK') +
      Format('L''objet "%s" a été empaqueté dans "%s".',[LabEditKerValue.Text,
        LabEditKerName.Text]));
end;

procedure TMainForm.btnUnBurryPackageClick(Sender: TObject);
// test de UnBurryPackage
begin
  if GVKer.UnBurryPck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('UNBURRYPCK') +
      Format('Le paquet "%s" est déterré.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnUnPackObjClick(Sender: TObject);
// test de UnPackObj
begin
  if GVKer.UnPackObj(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('UNPACKOBJ') +
      Format('L''objet "%s" a été ôté de son paquet.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnValVarClick(Sender: TObject);
// test de ValVar
var
  LS: string;
begin
  LS := EmptyStr;
  if GVKer.ValVar(LabEditKerName.Text, LS) then
    mmoGVKer.Lines.Add(fmtMess('VALVAR') + Format('La variable "%s" vaut "%s".',
      [LabEditKerName.Text, LS]));
end;

procedure TMainForm.btnVarCountsClick(Sender: TObject);
// test de VarsCount
var
  Li: Integer;
begin
  Li := GVKer.VarsCount;
  mmoGVKer.Lines.Add(fmtMess('VARSCOUNT') + 'Il y a ' +
    IntToStr(Li) + ' variable' + IfThen(Li>1, 's') + '.');
end;

procedure TMainForm.btnVarsToListClick(Sender: TObject);
// test de VarsToList
begin
  mmoGVKer.Lines.Add(fmtMess('VARSTOLIST') +
    'Voici la liste actuelle des variables : '+ GVKer.VarsToList);
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// test de Clear
begin
  GVKer.Clear;
  mmoGVKer.Lines.Add(FmTMess('CLEAR') + 'Espace de travail remis à zéro !');
end;

end.

