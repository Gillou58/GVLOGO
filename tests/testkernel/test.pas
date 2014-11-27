{ |========================================================================|
  |                                                                        |
  |                  G V S O F T                                           |
  |                  Projet : GVLogo                                       |
  |                  Description : Test du noyau de GVLOGO                 |
  |                  Unité : Test.pas                                      |
  |                  Ecrit par  : VASSEUR Gilles                           |
  |                  e-mail : g.vasseur58@laposte.net                      |
  |                  Copyright : © G. VASSEUR                              |
  |                  Date:    27-11-2014 11:14:40                          |
  |                  Version : 1.0.0                                       |
  |                                                                        |
  |========================================================================| }

{$I GVDefines.inc}

unit Test;

// GVKernel - part of GVLOGO
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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, ExtCtrls, Buttons, GVConsts, GVKernel;

type

  { TMainForm }

  TMainForm = class(TForm)
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
    btnPackageAll: TButton;
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
    btnIsValidParam: TButton;
    btnRemoveAllVars: TButton;
    btnExists: TButton;
    btnIsProtected: TButton;
    btnCount: TButton;
    btnToList: TButton;
    btnIsValid: TButton;
    btnLoadAll: TButton;
    btnSaveAll: TButton;
    btnKError: TButton;
    btnVarsToList: TButton;
    btnKernelResult: TButton;
    btnIsValidVar: TButton;
    btnLoadVars: TButton;
    btnAllProcsToEdit: TButton;
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
    STError: TStaticText;
    StatusBar: TStatusBar;
    TabSheetKernel: TTabSheet;
    procedure BitBtnKerCancelClick(Sender: TObject);
    procedure btnAddProcClick(Sender: TObject);
    procedure btnAddVarClick(Sender: TObject);
    procedure btnAllProcsToEditClick(Sender: TObject);
    procedure btnAnPropClick(Sender: TObject);
    procedure btnBelongToClick(Sender: TObject);
    procedure btnBurryPackageClick(Sender: TObject);
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
    procedure btnIsValidClick(Sender: TObject);
    procedure btnIsValidDefClick(Sender: TObject);
    procedure btnIsValidParamClick(Sender: TObject);
    procedure btnIsValidVarClick(Sender: TObject);
    procedure btnIsVarClick(Sender: TObject);
    procedure btnKernelResultClick(Sender: TObject);
    procedure btnKErrorClick(Sender: TObject);
    procedure btnListToPackageClick(Sender: TObject);
    procedure btnLoadAllClick(Sender: TObject);
    procedure btnLoadProcsClick(Sender: TObject);
    procedure btnLoadVarsClick(Sender: TObject);
    procedure btnNumParamPrimClick(Sender: TObject);
    procedure btnNumPrimClick(Sender: TObject);
    procedure btnPackageAllClick(Sender: TObject);
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
    { private declarations }
    GVKer: TGVLogoKernel;
  public
    { public declarations }
    // formatage d'un message
    function FmtMess(const Value: string) : string;
    // erreur dans le noyau
    procedure ErrorMessage(const Err: TGVError; Message: string);
  end;



var
  MainForm: TMainForm;

implementation

{$R *.lfm}

resourcestring
  RS_Result = '// Résultat de %s : ';
  RS_Exists = '%s existe.';
  RS_NoExists = '%s n''existe pas.';
  RS_Obj = 'Objet : %s - %s';

const
  C_AllFile = 'AllFile';
  C_VarsFile = 'VarsFile';
  C_Procs = 'Procedures';

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
// création de la fiche
begin
  GVKer := TGVLogoKernel.Create; // noyau de l'interpréteur
  GVKer.OnKernelError := @ErrorMessage; // événement erreur
end;

procedure TMainForm.FormDestroy(Sender: TObject);
// destruction de la fiche
begin
  GVKer.OnKernelError := nil;
  GVKer.Free;
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

procedure TMainForm.ErrorMessage(const Err: TGVError; Message: string);
// erreur dans le noyau
begin
  if Err = C_None then
    StError.Font.Color:= clLime
  else
    StError.Font.Color:= clRed;
  if Message = EmptyStr then
    Message := ME_C_Nothing;
  StError.Caption := Format(GVErrorName[Err], [Message]);
end;

procedure TMainForm.btnExistsClick(Sender: TObject);
// test de Exists
begin
  if GVKer.Exists(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('EXISTS') + Format(RS_Exists,[LabEditKerName.Text]))
  else
    mmoGVKer.Lines.Add(fmtMess('EXISTS') + Format(RS_NoExists,[LabEditKerName.Text]));
end;

procedure TMainForm.btnIsBurriedClick(Sender: TObject);
// test de IsBurried
var
  S: string;
begin
  if GVKer.IsBurried(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISBURRIED') + S);
end;

procedure TMainForm.btnIsBurriedPackageClick(Sender: TObject);
// test de IsBurriedPackage
var
  S: string;
begin
  if GVKer.IsBurriedPck(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISBURRIEDPCK') + S);
end;

procedure TMainForm.btnIsEmptyPListClick(Sender: TObject);
// test de IsEmptyList
var
  S: string;
begin
  if GVKer.IsEmptyPList(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISEMPTYLIST') + S);
end;

procedure TMainForm.btnIsInPackageClick(Sender: TObject);
// test de IsInPackage
var
  S: string;
begin
  if GVKer.IsInPck(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISINPCK') + S);
end;

procedure TMainForm.btnIsPackageClick(Sender: TObject);
// test de IsPackage
var
  S: string;
begin
  if GVKer.IsPck(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmtMess('ISPCK') + S);
end;

procedure TMainForm.btnIsPrimClick(Sender: TObject);
// test de IsPrim
var
  S: string;
begin
  if GVKer.IsPrim(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmtMess('ISPRIM') + S);
end;

procedure TMainForm.btnIsProcedureClick(Sender: TObject);
//test de IsProcedure
var
  S: string;
begin
  if GVKer.IsProc(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmtMess('ISPROC') +
    (Format(RS_Obj,[LabEditKerName.Text,S])));
end;

procedure TMainForm.btnIsProtectedClick(Sender: TObject);
var
  S: string;
begin
  if GVKer.IsProtected(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmtMess('ISPROTECTED') +
    (Format(RS_Obj,[LabEditKerName.Text,S])));
end;

procedure TMainForm.btnIsValidClick(Sender: TObject);
// test de IsValid
var
  S: string;
begin
  if GVKer.IsValid(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISVALID') + S);
end;

procedure TMainForm.btnIsValidDefClick(Sender: TObject);
// test de IsValidDef
begin
  if GVKer.IsValidDef(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('ISVALIDDEF') +
      Format('%s est une définition correcte.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnIsValidParamClick(Sender: TObject);
// test de IsValidParam
var
  S: string;
begin
  if GVKer.IsValidParam(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(fmtMess('ISVALIDPARAM') +
    Format('Le paramètre %s est correct : %s.',[LabEditKerName.Text,S]));
end;

procedure TMainForm.btnIsValidVarClick(Sender: TObject);
// test de IsValidVar
var
  S: string;
begin
  if GVKer.IsValidVar(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISVALIDVAR') + S);
end;

procedure TMainForm.btnIsVarClick(Sender: TObject);
// test de IsVar
var
  S: string;
begin
  if GVKer.IsVar(LabEditKerName.Text) then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ISVAR') + S);
end;

procedure TMainForm.btnKernelResultClick(Sender: TObject);
// test de KERNELRESULT
begin
  mmoGVKer.Lines.Add(FmtMess('KERNELRESULT ') +
    Format(GVErrorName[GVKer.KernelResult], [EmptyStr]));
end;

procedure TMainForm.btnKErrorClick(Sender: TObject);
// test de ERROR
var
  S: string;
begin
  if GVKer.Error then
    S := P_True
  else
    S := P_False;
  mmoGVKer.Lines.Add(FmTMess('ERROR') + S);
end;

procedure TMainForm.btnListToPackageClick(Sender: TObject);
// test de ListToPackage
begin
  if GVKer.ListToPck(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('LISTTOPCK') +
      Format('%s a été ajouté au paquet %s.',
        [LabEditKerName.Text,LabEditKerValue.Text]));
end;

procedure TMainForm.BitBtnKerCancelClick(Sender: TObject);
// nettoyage de l'éditeur du noyau
begin
  mmoGVKer.Clear;
end;

procedure TMainForm.btnAddProcClick(Sender: TObject);
// test de AddProc
begin
  if GVKer.AddProc(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('ADDPROC') + LabEditKerName.Text + ' ajoutée.');
end;

procedure TMainForm.btnAddVarClick(Sender: TObject);
// test de AddVar
begin
  if GVKer.AddVar(LabEditKerName.Text,LabEditKerValue.Text) then
    mmoGVKer.Lines.Add(fmtMess('ADDVAR') + LabEditKerName.Text + ' créée.');
end;

procedure TMainForm.btnAllProcsToEditClick(Sender: TObject);
// Test de AllProcsToEdit
begin
  if GVKer.AllProcsToEdit(mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('Toutes les procédures ont été éditées'));
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
  S: string;
begin
  S := GVKer.BelongsTo(LabEditKerName.Text);
  if S <> EmptyStr then
    mmoGVKer.Lines.Add(fmtMess('BELONGSTO') +S);
end;

procedure TMainForm.btnBurryPackageClick(Sender: TObject);
// test de BurryPackage
begin
  if GVKer.BurryPck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('BURRYPCK') +
      Format('Le paquet %s est enfoui.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnCountClick(Sender: TObject);
// test de Count
begin
  mmoGVKer.Lines.Add(fmtMess('COUNT') + IntToStr(GVKer.Count));
end;

procedure TMainForm.btnCountItemsPackageClick(Sender: TObject);
// test de CountItemsPackage
begin
  mmoGVKer.Lines.Add(fmtMess('COUNTITEMSPCK') +
    IntToStr(GVKer.CountItemsPck(LabEditKerName.Text)));
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
  S: string;
begin
  Lst := TStringList.Create;
  try
    GVKer.Dump(Lst);
    mmoGVKer.Lines.Add(fmtMess('DUMP'));
      if Lst.Count <> 0 then
      begin
        for S in Lst do
          mmoGVKer.Lines.Append('// '+ S);
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
  I: Integer;
begin
  I := 0;
  if GVKer.EditToProc(mmoGVKer.Lines,0,0,I) then
    mmoGVKer.Lines.Add(fmtMess('EDITTOPROC') + 'Editeur interprété.')
  else
    mmoGVKer.Lines.Add(fmtMess('EDITTOPROC') +
      Format('Erreur à la ligne : %d.',[I]));
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
    mmoGVKer.Lines.Add(fmtMess('LOADVARS') + 'Fichier de variables chargé.');
end;

procedure TMainForm.btnNumParamPrimClick(Sender: TObject);
// test de NumParamPrim
begin
  mmoGVKer.Lines.Add(fmtMess('NUMPARAMPRIM') +
    IntToStr(GVKer.NumParamsPrim(LabEditKerName.Text)));
end;

procedure TMainForm.btnNumPrimClick(Sender: TObject);
// test de NumPrim
begin
  mmoGVKer.Lines.Add(fmtMess('NUMPRIMS') +
    IntToStr(GVKer.NumPrim(LabEditKerName.Text)));
end;

procedure TMainForm.btnPackageAllClick(Sender: TObject);
// test de PackageAll
begin
  if GVKer.PckAll(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('PCKALL') +
      Format('Tout a été placé dans le paquet %s.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnPackagesCountClick(Sender: TObject);
// test de PackagesCount
begin
  mmoGVKer.Lines.Add(fmtMess('PCKSCOUNT') + IntToStr(GVKer.PcksCount));
end;

procedure TMainForm.btnPackagesToListClick(Sender: TObject);
// test de PackagesToList
begin
  mmoGVKer.Lines.Add(fmtMess('PCKSTOLIST') +
    GVKer.PcksToList);
end;

procedure TMainForm.btnPackageToEditClick(Sender: TObject);
// test de PackageToEdit
begin
  if GVKer.PckToEdit(LabEditKerName.Text,mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('PCKTOEDIT') +
      Format('Le paquet %s a été envoyé vers l''éditeur.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnPackageToListClick(Sender: TObject);
// test de PackageToList
begin
  mmoGVKer.Lines.Add(fmtMess('PCKTOLIST') +
    GVKer.PckToList(LabEditKerName.Text));
end;

procedure TMainForm.btnParamLineClick(Sender: TObject);
// test de ParamLine
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.ParamsLine(LabEditKerName.Text, S) then
    mmoGVKer.Lines.Add(fmtMess('PARAMLINE') + S);
end;

procedure TMainForm.btnParamNumClick(Sender: TObject);
// test de ParamNum
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.ParamNum(LabEditKerName.Text,SpinEditKer.Value,S) then
    mmoGVKer.Lines.Add(fmtMess('PARAMNUM') + S);
end;

procedure TMainForm.btnParamsCountClick(Sender: TObject);
// test de ParamsCount
begin
  mmoGVKer.Lines.Add(fmtMess('PARAMSCOUNT') +
    IntToStr(GVKer.ParamsCount(LabEditKerName.Text)));
end;

procedure TMainForm.btnPListeClick(Sender: TObject);
// test de Pliste
var
  S: string;
begin
  S := EmptyStr; // initialisation
  if GVKer.PListe(LabEditKerName.Text,S) then
    mmoGVKer.Lines.Add(fmtMess('PLISTE') + S);
end;

procedure TMainForm.btnPrimByNumClick(Sender: TObject);
// test de PrimByNum
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.PrimByNum(SpinEditKer.Value,S) then
    mmoGVKer.Lines.Add(fmtMess('PRIMBYNUM') + S);
end;

procedure TMainForm.btnPrimsCountClick(Sender: TObject);
// test de PrimsCount
begin
  mmoGVKer.Lines.Add(fmtMess('PRIMSCOUNT') + IntToStr(GVKer.PrimsCount));
end;

procedure TMainForm.btnPrimsToListClick(Sender: TObject);
// test de PrimsToList
begin
  mmoGVKer.Lines.Add(fmtMess('PRIMSTOLIST') + GVKer.PrimsToList);
end;

procedure TMainForm.btnProcLineClick(Sender: TObject);
// test de ProcLine
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.ProcLine(LabEditKerName.Text, SpinEditKer.Value, S) then
    mmoGVKer.Lines.Add(fmtMess('PROCLINE') + S);
end;

procedure TMainForm.btnProcLinesCountClick(Sender: TObject);
// test de ProcLinesCount
begin
  mmoGVKer.Lines.Add(fmtMess('PROCLINESCOUNT') +
    IntToStr(GVKer.ProcLinesCount(LabEditKerName.Text)));
end;

procedure TMainForm.btnProcListDefClick(Sender: TObject);
// test de ProcListDef
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.ProcListDef(LabEditKerName.Text, S) then
    mmoGVKer.Lines.Add(fmtMess('PROCLISTDEF') + S);
end;

procedure TMainForm.btnProcsCountClick(Sender: TObject);
// test de ProcsCount
begin
  mmoGVKer.Lines.Add(fmtMess('PROCSCOUNT') + IntToStr(GVKer.ProcsCount));
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
  mmoGVKer.Lines.Add(fmtMess('PROCSTOLIST') + GVKer.ProcsToList);
end;

procedure TMainForm.btnProcToEditClick(Sender: TObject);
// test de ProcToEdit
begin
  if GVKer.ProcToEdit(LabEditKerName.Text, mmoGVKer.Lines) then
    mmoGVKer.Lines.Add(fmtMess('PROCTOEDIT') + 'Procédure éditée.');
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
  S: string;
begin
  S := LabEditKerName.Text;
  if GVKer.RemoveProc(S) then
    mmoGVKer.Lines.Add(fmtMess('REMOVEPROC') + S + ' supprimée.');
end;

procedure TMainForm.btnRemoveVarClick(Sender: TObject);
// test de RemoveVar
begin
  if GVKer.RemoveVar(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('REMOVEVAR') + LabEditKerName.Text + ' supprimée');
end;

procedure TMainForm.btnRPropClick(Sender: TObject);
// test de RProp
var
  S: string;
begin
  S := EmptyStr; // initialisation
  if GVKer.RProp(LabEditKerName.Text,LabEditKerValue.text,S) then
    mmoGVKer.Lines.Add(fmtMess('RPROP') + S);
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
    mmoGVKer.Lines.Add(fmtMess('SAVEALLVARS') + 'Variables sauvegardées.');
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
  if GVKer.SaveVars(C_VarsFile,LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('SAVEVARS') +
      Format('Variables %s sauvegardées.',[LabEditKerName.Text]));
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
      Format('L''objet %s a été empaqueté.',[LabEditKerValue.Text]));
end;

procedure TMainForm.btnUnBurryPackageClick(Sender: TObject);
// test de UnBurryPackage
begin
  if GVKer.UnBurryPck(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('UNBURRYPCK') +
      Format('Le paquet %s est déterré.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnUnPackObjClick(Sender: TObject);
// test de UnPackObj
begin
  if GVKer.UnPackObj(LabEditKerName.Text) then
    mmoGVKer.Lines.Add(fmtMess('UNPACKOBJ') +
      Format('L''objet %s a été ôté de son paquet.',[LabEditKerName.Text]));
end;

procedure TMainForm.btnValVarClick(Sender: TObject);
// test de ValVar
var
  S: string;
begin
  S := EmptyStr;
  if GVKer.ValVar(LabEditKerName.Text,S) then
    mmoGVKer.Lines.Add(fmtMess('VALVAR') + S);
end;

procedure TMainForm.btnVarCountsClick(Sender: TObject);
// test de VarsCount
begin
  mmoGVKer.Lines.Add(fmtMess('VARSCOUNT') + IntToStr(GVKer.VarsCount));
end;

procedure TMainForm.btnVarsToListClick(Sender: TObject);
// test de VarsToList
begin
  mmoGVKer.Lines.Add(fmtMess('VARSTOLIST') + GVKer.VarsToList);
end;

procedure TMainForm.btnClearClick(Sender: TObject);
// test de Clear
begin
  GVKer.Clear;
  mmoGVKer.Lines.Add(FmTMess('CLEAR') + 'Espace remis à zéro !');
end;

end.

